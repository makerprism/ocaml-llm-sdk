(** Anthropic Messages API client over {!Llm_core}.

    Functor over {!Llm_core.HTTP_CLIENT}, so it is HTTP-client- and
    runtime-agnostic. The wire encode/decode are exposed as pure functions
    ({!encode_request} / {!decode_response}) so they can be unit-tested with
    fixtures — no network, no API key. *)

open Llm_core

type config = {
  api_key : string;
  model : string;
  base_url : string;          (** default ["https://api.anthropic.com"] *)
  anthropic_version : string; (** default ["2023-06-01"] *)
  cache_system : bool;        (** add [cache_control] to the system prompt *)
}

let make_config ?(base_url = "https://api.anthropic.com")
    ?(anthropic_version = "2023-06-01") ?(cache_system = true) ~api_key ~model () =
  { api_key; model; base_url; anthropic_version; cache_system }

(** {1 Pure encode (our types -> Anthropic request JSON)} *)

(* Anthropic has only user/assistant roles; tool results ride in a user turn as
   [tool_result] blocks, and the system prompt is a top-level field, so a
   [System]/[Tool] role in [messages] maps onto a user turn. *)
let role_to_string = function
  | User | Tool | System -> "user"
  | Assistant -> "assistant"

let encode_content_block = function
  | Text text -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Tool_use { id; name; arguments } ->
      `Assoc
        [ ("type", `String "tool_use");
          ("id", `String id);
          ("name", `String name);
          ("input", arguments) ]
  | Tool_result { tool_call_id; content; is_error } ->
      `Assoc
        [ ("type", `String "tool_result");
          ("tool_use_id", `String tool_call_id);
          ("content", `String content);
          ("is_error", `Bool is_error) ]

let encode_message { role; content } =
  `Assoc
    [ ("role", `String (role_to_string role));
      ("content", `List (List.map encode_content_block content)) ]

let encode_tool { name; description; input_schema } =
  `Assoc
    [ ("name", `String name);
      ("description", `String description);
      ("input_schema", input_schema) ]

let encode_system cfg system =
  if cfg.cache_system then
    `List
      [ `Assoc
          [ ("type", `String "text");
            ("text", `String system);
            ("cache_control", `Assoc [ ("type", `String "ephemeral") ]) ] ]
  else `String system

let encode_request cfg ~system ~messages ~tools ~max_tokens : Yojson.Safe.t =
  let base =
    [ ("model", `String cfg.model);
      ("max_tokens", `Int max_tokens);
      ("system", encode_system cfg system);
      ("messages", `List (List.map encode_message messages)) ]
  in
  let with_tools =
    match tools with
    | [] -> base
    | _ -> base @ [ ("tools", `List (List.map encode_tool tools)) ]
  in
  `Assoc with_tools

(** {1 Pure decode (Anthropic response JSON -> our types)} *)

let stop_reason_of_string = function
  | "end_turn" -> End_turn
  | "tool_use" -> Tool_use_stop
  | "max_tokens" -> Max_tokens
  | "refusal" -> Refusal
  | other -> Other other

let decode_block json =
  let open Yojson.Safe.Util in
  match member "type" json |> to_string_option with
  | Some "text" ->
      (match member "text" json |> to_string_option with
       | Some text -> Some (Text text)
       | None -> None)
  | Some "tool_use" ->
      (match
         ( member "id" json |> to_string_option,
           member "name" json |> to_string_option )
       with
       | Some id, Some name ->
           Some (Tool_use { id; name; arguments = member "input" json })
       | _ -> None)
  | _ -> None

let decode_usage json =
  let open Yojson.Safe.Util in
  let int_field k = match member k json with `Int n -> n | _ -> 0 in
  { input_tokens = int_field "input_tokens";
    output_tokens = int_field "output_tokens";
    cached_input_tokens = int_field "cache_read_input_tokens" }

(** Parse a 2xx Anthropic Messages response body. *)
let decode_response body : (assistant_turn, error) result =
  match Yojson.Safe.from_string body with
  | exception _ -> Error (Invalid_response "anthropic: body is not valid JSON")
  | json ->
      let open Yojson.Safe.Util in
      let content =
        match member "content" json with
        | `List items -> List.filter_map decode_block items
        | _ -> []
      in
      let stop_reason =
        member "stop_reason" json |> to_string_option
        |> Option.value ~default:"end_turn" |> stop_reason_of_string
      in
      let usage = decode_usage (member "usage" json) in
      Ok { content; stop_reason; usage }

(** {1 The provider (functor over the HTTP transport)} *)

module Make (Http : Llm_core.HTTP_CLIENT) :
  Llm_core.PROVIDER with type config = config = struct
  type nonrec config = config

  let name = "anthropic"

  let complete cfg ~system ~messages ~tools ?(max_tokens = 1024) on_success
      on_error =
    let body =
      Yojson.Safe.to_string (encode_request cfg ~system ~messages ~tools ~max_tokens)
    in
    let headers =
      [ ("content-type", "application/json");
        ("x-api-key", cfg.api_key);
        ("anthropic-version", cfg.anthropic_version) ]
    in
    let url = cfg.base_url ^ "/v1/messages" in
    Http.post ~headers ~body url
      (fun (resp : Llm_core.response) ->
        if resp.status >= 200 && resp.status < 300 then
          match decode_response resp.body with
          | Ok turn -> on_success turn
          | Error e -> on_error e
        else on_error (Http_error { status = resp.status; body = resp.body }))
      (fun msg -> on_error (Network_error msg))
end
