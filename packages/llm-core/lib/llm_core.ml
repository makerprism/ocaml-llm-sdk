(** llm-core — runtime-agnostic, HTTP-client-agnostic interfaces for LLM
    provider API clients.

    The design mirrors {{:https://github.com/makerprism/ocaml-social-sdk}
    ocaml-social-sdk}: a pure core that depends on nothing but {!Yojson}, a
    CPS-style {!HTTP_CLIENT} interface any runtime can implement, and
    provider-neutral types so one agentic loop works across Anthropic, OpenAI,
    xAI Grok, and others.

    Concrete providers (e.g. [llm-anthropic], [llm-openai]) are functors over
    {!HTTP_CLIENT}; runtime adapters (e.g. [llm-lwt]) implement {!HTTP_CLIENT}
    and convert the CPS calls into their own async type. *)

(** {1 HTTP transport (the runtime-agnostic seam)} *)

(** A raw HTTP response. The provider layer parses [body] into typed values. *)
type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

(** CPS-style HTTP client. Implement it with any HTTP library (Cohttp, Curly,
    httpaf, ...) and any async runtime (Lwt, Eio, synchronous): the
    implementation calls [on_success] with the response or [on_error] with a
    transport-level message (timeout, DNS, connection reset). Non-2xx HTTP
    statuses are {b not} errors at this layer — they arrive as a [response] and
    the provider decides how to classify them. *)
module type HTTP_CLIENT = sig
  val get :
    ?headers:(string * string) list ->
    string ->
    (response -> unit) ->
    (string -> unit) ->
    unit

  val post :
    ?headers:(string * string) list ->
    ?body:string ->
    string ->
    (response -> unit) ->
    (string -> unit) ->
    unit
end

(** {1 Provider-neutral message types} *)

type role = System | User | Assistant | Tool

(** A model-emitted request to run one of the caller's tools. [arguments] is the
    parsed JSON object of tool inputs (the OpenAI wire format stringifies these;
    the OpenAI provider parses them back so this is always real JSON). *)
type tool_call = {
  id : string;
  name : string;
  arguments : Yojson.Safe.t;
}

type content_block =
  | Text of string
  | Tool_use of tool_call
  | Tool_result of {
      tool_call_id : string;
      content : string;
      is_error : bool;
    }

type message = {
  role : role;
  content : content_block list;
}

(** A tool the model may call. [input_schema] is JSON Schema — accepted natively
    by Anthropic, OpenAI, and Grok, so there is no per-provider schema dialect. *)
type tool_def = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
}

type usage = {
  input_tokens : int;
  output_tokens : int;
  cached_input_tokens : int;  (** prompt tokens served from cache, when reported *)
}

let zero_usage = { input_tokens = 0; output_tokens = 0; cached_input_tokens = 0 }

type stop_reason =
  | End_turn       (** final text reply; no tool calls *)
  | Tool_use_stop  (** model wants tool(s) run; execute and re-call *)
  | Max_tokens
  | Refusal
  | Other of string

type assistant_turn = {
  content : content_block list;  (** text and/or [Tool_use] blocks *)
  stop_reason : stop_reason;
  usage : usage;
}

(** {1 Errors} *)

(** Errors are passed to the CPS error continuation, never raised, so a provider
    hiccup degrades gracefully instead of taking down the caller. [Http_error]
    carries the raw status and body so the caller can classify (e.g. 429,
    provider error JSON) without string-matching. *)
type error =
  | Http_error of { status : int; body : string }
  | Network_error of string   (** timeout, DNS, connection failure *)
  | Invalid_response of string (** 2xx body that didn't parse as expected *)

let string_of_error = function
  | Http_error { status; body } ->
      Printf.sprintf "HTTP %d: %s" status body
  | Network_error msg -> Printf.sprintf "network error: %s" msg
  | Invalid_response msg -> Printf.sprintf "invalid response: %s" msg

(** True for a 429. *)
let is_rate_limited = function Http_error { status = 429; _ } -> true | _ -> false

(** Worth retrying with backoff: 429s, 5xx, and transport failures. A 4xx
    (other than 429) or an unparseable body is a caller/contract error and is
    not retried. *)
let is_retryable = function
  | Http_error { status; _ } -> status = 429 || status >= 500
  | Network_error _ -> true
  | Invalid_response _ -> false

(** {1 Safe JSON access}

    Decode helpers that {b never raise} — unlike {!Yojson.Safe.Util}, whose
    [member] raises on non-objects. Provider responses evolve and omit fields,
    so robust decoding matters more than strictness here. *)
module Json = struct
  let member key = function
    | `Assoc o -> ( try List.assoc key o with Not_found -> `Null )
    | _ -> `Null

  (** Nested lookup, e.g. [path [ "usage"; "prompt_tokens_details" ] json]. *)
  let path keys json = List.fold_left (fun j k -> member k j) json keys

  let to_int = function `Int n -> n | _ -> 0
  let to_string_opt = function `String s -> Some s | _ -> None
  let to_string ?(default = "") j = Option.value ~default (to_string_opt j)
  let to_list = function `List l -> l | _ -> []
  let to_bool = function `Bool b -> b | _ -> false
end

(** {1 Transport helper}

    Wire a JSON POST to the CPS continuations so every provider classifies
    responses identically: 2xx -> [decode] the body; non-2xx -> {!Http_error};
    transport failure -> {!Network_error}. [post] is an {!HTTP_CLIENT.post}. *)
let post_json
    (post :
      ?headers:(string * string) list ->
      ?body:string ->
      string ->
      (response -> unit) ->
      (string -> unit) ->
      unit) ~headers ~url ~body ~decode on_success on_error =
  post ~headers ~body url
    (fun resp ->
      if resp.status >= 200 && resp.status < 300 then
        match decode resp.body with
        | Ok v -> on_success v
        | Error e -> on_error e
      else on_error (Http_error { status = resp.status; body = resp.body }))
    (fun msg -> on_error (Network_error msg))

(** {1 The uniform provider shape} *)

(** Every provider satisfies this. [config] is provider-specific (API key,
    model, base URL); everything else is uniform, so the agentic loop above the
    seam is provider-agnostic. [complete] is one model round-trip in CPS. *)
module type PROVIDER = sig
  type config

  val name : string

  val complete :
    config ->
    system:string ->
    messages:message list ->
    tools:tool_def list ->
    ?max_tokens:int ->
    (assistant_turn -> unit) ->
    (error -> unit) ->
    unit
end
