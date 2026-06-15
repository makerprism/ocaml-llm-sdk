(** OpenAI Chat Completions client over {!Llm_core}.

    Also drives {b xAI Grok}: Grok's API is OpenAI-compatible, so point
    [base_url] at ["https://api.x.ai/v1"] (see {!grok_base_url}) and the same
    code path works.

    Functor over {!Llm_core.HTTP_CLIENT}. Encode/decode are pure and exposed for
    fixture testing. Note the OpenAI quirk that tool-call arguments travel as a
    {e JSON-encoded string}; we stringify on encode and parse back on decode so
    callers always see real {!Yojson.Safe.t}. *)

open Llm_core

let openai_base_url = "https://api.openai.com/v1"
let grok_base_url = "https://api.x.ai/v1"

type config = {
  api_key : string;
  model : string;
  base_url : string;        (** {!openai_base_url} or {!grok_base_url} *)
  max_tokens_field : string;
      (** ["max_tokens"] (default) or ["max_completion_tokens"] for o-series. *)
}

let make_config ?(base_url = openai_base_url) ?(max_tokens_field = "max_tokens")
    ~api_key ~model () =
  { api_key; model; base_url; max_tokens_field }

(** {1 Pure encode} *)

let text_of_blocks blocks =
  blocks
  |> List.filter_map (function Text t -> Some t | _ -> None)
  |> String.concat ""

let encode_tool_call { id; name; arguments } =
  `Assoc
    [ ("id", `String id);
      ("type", `String "function");
      ("function",
        `Assoc
          [ ("name", `String name);
            (* OpenAI requires arguments as a JSON-encoded string. *)
            ("arguments", `String (Yojson.Safe.to_string arguments)) ]) ]

(* One logical message can expand to several OpenAI messages (a [Tool] turn
   carrying N tool results becomes N role:"tool" messages). *)
let encode_message { role; content } : Yojson.Safe.t list =
  match role with
  | System -> [ `Assoc [ ("role", `String "system"); ("content", `String (text_of_blocks content)) ] ]
  | User -> [ `Assoc [ ("role", `String "user"); ("content", `String (text_of_blocks content)) ] ]
  | Assistant ->
      let text = text_of_blocks content in
      let tool_calls =
        List.filter_map
          (function Tool_use tc -> Some (encode_tool_call tc) | _ -> None)
          content
      in
      let fields =
        [ ("role", `String "assistant");
          ("content", if text = "" then `Null else `String text) ]
        @ (if tool_calls = [] then [] else [ ("tool_calls", `List tool_calls) ])
      in
      [ `Assoc fields ]
  | Tool ->
      List.filter_map
        (function
          | Tool_result { tool_call_id; content; _ } ->
              Some
                (`Assoc
                   [ ("role", `String "tool");
                     ("tool_call_id", `String tool_call_id);
                     ("content", `String content) ])
          | _ -> None)
        content

let encode_tool { name; description; input_schema } =
  `Assoc
    [ ("type", `String "function");
      ("function",
        `Assoc
          [ ("name", `String name);
            ("description", `String description);
            ("parameters", input_schema) ]) ]

let encode_request cfg ~system ~messages ~tools ~max_tokens : Yojson.Safe.t =
  let system_msg =
    `Assoc [ ("role", `String "system"); ("content", `String system) ]
  in
  let body_messages = system_msg :: List.concat_map encode_message messages in
  let base =
    [ ("model", `String cfg.model);
      (cfg.max_tokens_field, `Int max_tokens);
      ("messages", `List body_messages) ]
  in
  let with_tools =
    match tools with
    | [] -> base
    | _ -> base @ [ ("tools", `List (List.map encode_tool tools)) ]
  in
  `Assoc with_tools

(** {1 Pure decode} *)

let stop_reason_of_finish = function
  | "stop" -> End_turn
  | "tool_calls" | "function_call" -> Tool_use_stop
  | "length" -> Max_tokens
  | "content_filter" -> Refusal
  | other -> Other other

(* OpenAI tool-call arguments are a JSON string; parse back to real JSON. *)
let parse_arguments s =
  match Yojson.Safe.from_string s with
  | json -> json
  | exception _ -> `String s

let decode_tool_call json =
  let open Yojson.Safe.Util in
  match
    ( member "id" json |> to_string_option,
      member "function" json |> member "name" |> to_string_option )
  with
  | Some id, Some name ->
      let arguments =
        match member "function" json |> member "arguments" |> to_string_option with
        | Some s -> parse_arguments s
        | None -> `Assoc []
      in
      Some (Tool_use { id; name; arguments })
  | _ -> None

let decode_usage json =
  let open Yojson.Safe.Util in
  let int_field path j = match List.fold_left (fun acc k -> member k acc) j path with `Int n -> n | _ -> 0 in
  { input_tokens = int_field [ "prompt_tokens" ] json;
    output_tokens = int_field [ "completion_tokens" ] json;
    cached_input_tokens = int_field [ "prompt_tokens_details"; "cached_tokens" ] json }

let decode_response body : (assistant_turn, error) result =
  match Yojson.Safe.from_string body with
  | exception _ -> Error (Invalid_response "openai: body is not valid JSON")
  | json ->
      let open Yojson.Safe.Util in
      (match member "choices" json with
       | `List (choice :: _) ->
           let msg = member "message" choice in
           let text_blocks =
             match member "content" msg |> to_string_option with
             | Some s when s <> "" -> [ Text s ]
             | _ -> []
           in
           let tool_blocks =
             match member "tool_calls" msg with
             | `List tcs -> List.filter_map decode_tool_call tcs
             | _ -> []
           in
           let stop_reason =
             member "finish_reason" choice |> to_string_option
             |> Option.value ~default:"stop" |> stop_reason_of_finish
           in
           let usage = decode_usage (member "usage" json) in
           Ok { content = text_blocks @ tool_blocks; stop_reason; usage }
       | _ -> Error (Invalid_response "openai: no choices in response"))

(** {1 The provider} *)

module Make (Http : Llm_core.HTTP_CLIENT) :
  Llm_core.PROVIDER with type config = config = struct
  type nonrec config = config

  let name = "openai"

  let complete cfg ~system ~messages ~tools ?(max_tokens = 1024) on_success
      on_error =
    let body =
      Yojson.Safe.to_string (encode_request cfg ~system ~messages ~tools ~max_tokens)
    in
    let headers =
      [ ("content-type", "application/json");
        ("authorization", "Bearer " ^ cfg.api_key) ]
    in
    let url = cfg.base_url ^ "/chat/completions" in
    Http.post ~headers ~body url
      (fun (resp : Llm_core.response) ->
        if resp.status >= 200 && resp.status < 300 then
          match decode_response resp.body with
          | Ok turn -> on_success turn
          | Error e -> on_error e
        else on_error (Http_error { status = resp.status; body = resp.body }))
      (fun msg -> on_error (Network_error msg))
end
