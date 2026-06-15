(** Turn a provider's CPS call into an Lwt promise.

    A provider's [complete cfg ~system ~messages ~tools] partially applied up to
    (but not including) the two continuations has type
    [(assistant_turn -> unit) -> (error -> unit) -> unit]. {!to_lwt} runs it and
    resolves an [(assistant_turn, error) result Lwt.t]:

    {[
      module A = Llm_anthropic.Make (Llm_lwt.Cohttp_client)
      let turn : (Llm_core.assistant_turn, Llm_core.error) result Lwt.t =
        Llm_lwt.Lwt_adapter.to_lwt
          (A.complete cfg ~system ~messages ~tools ~max_tokens:2048)
    ]} *)

(** Convert a CPS computation into a [result Lwt.t]. The promise is resolved
    exactly once; a synchronous exception in [f] becomes a [Network_error]. *)
let to_lwt (f : ('a -> unit) -> (Llm_core.error -> unit) -> unit) :
    ('a, Llm_core.error) result Lwt.t =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  (try f (fun ok -> resolve (Ok ok)) (fun err -> resolve (Error err))
   with exn -> resolve (Error (Llm_core.Network_error (Printexc.to_string exn))));
  promise
