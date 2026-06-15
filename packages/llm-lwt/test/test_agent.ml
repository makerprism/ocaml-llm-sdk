(** Drives Llm_lwt.Agent.run with a scripted [complete] (no provider, no
    network) — exactly the deterministic test pattern the loop is meant to
    enable. *)

open Llm_core

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else (incr failures; Printf.printf "FAIL - %s\n" name)

let tool_use id name = Tool_use { id; name; arguments = `Null }

let turn ?(stop = End_turn) content = { content; stop_reason = stop; usage = zero_usage }

(* Scenario 1: one tool round, then a final text reply. *)
let normal_finish () =
  let calls = ref 0 and tool_runs = ref 0 in
  let complete _messages =
    incr calls;
    Lwt.return
      (Ok
         (if !calls = 1 then turn ~stop:Tool_use_stop [ tool_use "t1" "lookup" ]
          else turn [ Text "done" ]))
  in
  let run_tool _tc = incr tool_runs; Lwt.return "result" in
  let result =
    Lwt_main.run (Llm_lwt.Agent.run ~complete ~run_tool [ user_text "hi" ])
  in
  check "normal finish returns End_turn"
    (match result with Ok t -> t.stop_reason = End_turn | _ -> false);
  check "tool ran exactly once" (!tool_runs = 1);
  check "model called twice (tool round + final)" (!calls = 2)

(* Scenario 2: model never stops asking for tools — the cap fires. *)
let cap_reached () =
  let calls = ref 0 and tool_runs = ref 0 in
  let complete _messages =
    incr calls;
    Lwt.return (Ok (turn ~stop:Tool_use_stop [ tool_use "t" "loop" ]))
  in
  let run_tool _tc = incr tool_runs; Lwt.return "again" in
  let result =
    Lwt_main.run
      (Llm_lwt.Agent.run ~complete ~run_tool ~max_rounds:3 [ user_text "hi" ])
  in
  check "cap returns the still-Tool_use turn"
    (match result with Ok t -> t.stop_reason = Tool_use_stop | _ -> false);
  check "model calls bounded by max_rounds" (!calls <= 3);
  check "tool runs bounded" (!tool_runs <= 3)

let () =
  normal_finish ();
  cap_reached ();
  if !failures = 0 then print_endline "All llm-lwt agent tests passed."
  else (Printf.printf "%d failure(s)\n" !failures; exit 1)
