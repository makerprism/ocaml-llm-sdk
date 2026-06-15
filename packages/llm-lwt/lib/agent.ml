(** A bounded agentic tool-use loop — the thing every tool-use consumer
    otherwise writes by hand.

    {!run} drives one logical turn: call [complete] with the transcript; if the
    model asked for tools, run each via [run_tool], append the assistant turn
    and a [Tool] turn of results, and repeat — up to [max_rounds] model
    round-trips. The cap bounds cost/latency on a confused model.

    The provider-specific bits (which provider, how to execute a tool) are the
    caller's [complete] and [run_tool], so this loop is provider-agnostic and
    independent of where state lives. *)

open Llm_core
open Lwt.Syntax

let tool_uses_of turn =
  List.filter_map (function Tool_use tc -> Some tc | _ -> None) turn.content

(** Run the loop, starting from [messages].

    Returns the final {!Llm_core.assistant_turn}. On a normal finish its
    [stop_reason] is whatever the model returned ([End_turn], [Refusal], ...).
    If the round cap is hit while the model still wants tools, the last turn is
    returned with [stop_reason = Tool_use_stop] — the caller detects the cap by
    that, and typically replies "let me check that again" rather than looping.

    @param complete one model round-trip over the given transcript
    @param run_tool execute one tool call, returning its result string
    @param max_rounds hard cap on model round-trips (default 6) *)
let run ~(complete : message list -> (assistant_turn, error) result Lwt.t)
    ~(run_tool : tool_call -> string Lwt.t) ?(max_rounds = 6)
    (messages : message list) : (assistant_turn, error) result Lwt.t =
  let rec loop rounds messages =
    let* result = complete messages in
    match result with
    | Error _ as e -> Lwt.return e
    | Ok turn -> (
        match tool_uses_of turn with
        | [] -> Lwt.return (Ok turn)
        | _ when turn.stop_reason <> Tool_use_stop -> Lwt.return (Ok turn)
        | _ when rounds + 1 >= max_rounds ->
            (* Cap reached; return the unfulfilled turn so the caller can tell. *)
            Lwt.return (Ok turn)
        | tool_calls ->
            let* results =
              Lwt_list.map_s
                (fun tc ->
                  let* content = run_tool tc in
                  Lwt.return (tool_result ~tool_call_id:tc.id content))
                tool_calls
            in
            loop (rounds + 1)
              (messages
              @ [ { role = Assistant; content = turn.content };
                  tool_turn results ]))
  in
  loop 0 messages
