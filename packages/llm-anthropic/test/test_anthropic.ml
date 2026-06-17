(** Pure encode/decode tests — no network, no API key. This is the point of the
    sans-IO core: the wire mapping is testable with fixtures. *)

open Llm_core
open Yojson.Safe.Util

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else (incr failures; Printf.printf "FAIL - %s\n" name)

let () =
  (* --- encode --- *)
  let cfg = Llm_anthropic.make_config ~api_key:"k" ~model:"claude-x" () in
  let tools =
    [ { name = "lookup"; description = "find a listing";
        input_schema = `Assoc [ ("type", `String "object") ] } ]
  in
  let messages = [ { role = User; content = [ Text "karaoke at The Lamp?" ] } ] in
  let req =
    Llm_anthropic.encode_request cfg ~system:"SYS" ~messages ~tools ~max_tokens:512 ()
  in
  check "model encoded" (member "model" req |> to_string = "claude-x");
  check "max_tokens encoded" (member "max_tokens" req |> to_int = 512);
  check "temperature omitted by default"
    (match member "temperature" req with `Null -> true | _ -> false);
  check "temperature encoded when set"
    (let req_t =
       Llm_anthropic.encode_request cfg ~system:"SYS" ~messages ~tools
         ~max_tokens:512 ~temperature:0.2 ()
     in
     member "temperature" req_t |> to_float = 0.2);
  check "system carries cache_control"
    (match member "system" req with
     | `List [ (`Assoc _ as blk) ] ->
         member "cache_control" blk |> member "type" |> to_string = "ephemeral"
     | _ -> false);
  check "tools present"
    (match member "tools" req with `List [ _ ] -> true | _ -> false);
  check "tool_result rides a user turn"
    (let m = { role = Tool;
               content = [ Tool_result { tool_call_id = "tu_1"; content = "x"; is_error = false } ] } in
     match Llm_anthropic.encode_message m with
     | `Assoc fields -> List.assoc "role" fields = `String "user"
     | _ -> false);

  (* --- decode --- *)
  let body =
    {|{"content":[{"type":"text","text":"sure"},{"type":"tool_use","id":"tu_1","name":"lookup","input":{"name":"The Lamp"}}],"stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5,"cache_read_input_tokens":7}}|}
  in
  (match Llm_anthropic.decode_response body with
   | Ok turn ->
       check "stop_reason = tool_use" (turn.stop_reason = Tool_use_stop);
       check "cached usage parsed" (turn.usage.cached_input_tokens = 7);
       check "tool_use decoded with real JSON args"
         (List.exists
            (function
              | Tool_use { name = "lookup"; arguments; _ } ->
                  member "name" arguments |> to_string = "The Lamp"
              | _ -> false)
            turn.content)
   | Error e ->
       check "decode succeeds" false;
       Printf.printf "       error: %s\n" (string_of_error e));

  if !failures = 0 then print_endline "All llm-anthropic tests passed."
  else (Printf.printf "%d failure(s)\n" !failures; exit 1)
