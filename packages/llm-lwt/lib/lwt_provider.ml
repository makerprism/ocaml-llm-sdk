(** Wrap a CPS {!Llm_core.PROVIDER} as an Lwt-returning one, so callers never
    touch continuations:

    {[
      module Claude =
        Llm_lwt.Lwt_provider.Make (Llm_anthropic.Make (Llm_lwt.Cohttp_client))

      let turn = Claude.complete cfg ~system ~messages ~tools ~max_tokens:2048 ()
      (* : (Llm_core.assistant_turn, Llm_core.error) result Lwt.t *)
    ]} *)

module Make (P : Llm_core.PROVIDER) = struct
  type config = P.config

  let name = P.name

  let complete cfg ~system ~messages ~tools ?max_tokens () :
      (Llm_core.assistant_turn, Llm_core.error) result Lwt.t =
    Lwt_adapter.to_lwt (P.complete cfg ~system ~messages ~tools ?max_tokens)
end
