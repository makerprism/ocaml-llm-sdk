(** Pure encode/decode tests — the riskiest part is OpenAI's stringified
    tool-call arguments, plus usage decoding when [prompt_tokens_details] is
    absent (which raised before the safe-accessor fix). *)

open Llm_core
open Yojson.Safe.Util

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else (incr failures; Printf.printf "FAIL - %s\n" name)

let () =
  (* encode: tool-call arguments must be a JSON-encoded STRING *)
  let assistant =
    { role = Assistant;
      content =
        [ Tool_use
            { id = "call_1"; name = "lookup";
              arguments = `Assoc [ ("city", `String "Bath") ] } ] }
  in
  (match Llm_openai.encode_message assistant with
   | [ `Assoc _ as m ] ->
       let args = member "tool_calls" m |> index 0 |> member "function" |> member "arguments" in
       check "tool-call arguments encoded as a string"
         (match args with `String _ -> true | _ -> false);
       check "stringified arguments are valid JSON round-trip"
         (match args with
          | `String s -> Yojson.Safe.from_string s |> member "city" |> to_string = "Bath"
          | _ -> false)
   | _ -> check "assistant encodes to one message" false);

  check "tool result becomes a role:tool message"
    (let m = { role = Tool;
               content = [ Tool_result { tool_call_id = "call_1"; content = "ok"; is_error = false } ] } in
     match Llm_openai.encode_message m with
     | [ `Assoc fields ] -> List.assoc "role" fields = `String "tool"
     | _ -> false);

  (* temperature: omitted by default, present in the request body when set *)
  let cfg = Llm_openai.make_config ~api_key:"k" ~model:"gpt-x" () in
  check "temperature omitted by default"
    (let req = Llm_openai.encode_request cfg ~system:"S" ~messages:[] ~tools:[] ~max_tokens:64 () in
     match member "temperature" req with `Null -> true | _ -> false);
  check "temperature encoded when set"
    (let req =
       Llm_openai.encode_request cfg ~system:"S" ~messages:[] ~tools:[] ~max_tokens:64
         ~temperature:0.2 ()
     in
     member "temperature" req |> to_float = 0.2);

  (* decode: tool_calls + finish_reason + usage WITHOUT prompt_tokens_details *)
  let body =
    {|{"choices":[{"message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"lookup","arguments":"{\"city\":\"Bath\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":12,"completion_tokens":3}}|}
  in
  (match Llm_openai.decode_response body with
   | Ok turn ->
       check "finish_reason tool_calls -> Tool_use_stop" (turn.stop_reason = Tool_use_stop);
       check "usage without prompt_tokens_details decodes (no raise)"
         (turn.usage.input_tokens = 12 && turn.usage.cached_input_tokens = 0);
       check "tool_call arguments parsed back to real JSON"
         (List.exists
            (function
              | Tool_use { name = "lookup"; arguments; _ } ->
                  member "city" arguments |> to_string = "Bath"
              | _ -> false)
            turn.content)
   | Error e ->
       check "decode succeeds" false;
       Printf.printf "       error: %s\n" (string_of_error e));

  if !failures = 0 then print_endline "All llm-openai tests passed."
  else (Printf.printf "%d failure(s)\n" !failures; exit 1)
