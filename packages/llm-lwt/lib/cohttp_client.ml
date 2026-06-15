(** A {!Llm_core.HTTP_CLIENT} implemented with Cohttp + Lwt.

    Pass this module to a provider functor, e.g.
    [module A = Llm_anthropic.Make (Llm_lwt.Cohttp_client)]. Requests are raced
    against {!timeout} so a hung provider fails the call instead of blocking
    forever; transport failures and timeouts arrive on the CPS [on_error]
    continuation. *)

open Lwt.Syntax

(** Per-request wall-clock timeout, seconds. Mutable so callers can tune it. *)
let timeout = ref 60.0

let to_response resp body_str : Llm_core.response =
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  let headers = Cohttp.Header.to_list (Cohttp.Response.headers resp) in
  { Llm_core.status; headers; body = body_str }

(* Run [perform] (which yields a response) racing a timeout, then dispatch to
   the CPS continuations. Never raises into the runtime. *)
let run ~perform on_success on_error =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let timed_out =
            let* () = Lwt_unix.sleep !timeout in
            Lwt.return
              (Error
                 (Printf.sprintf "request timed out after %.0fs" !timeout))
          in
          let succeeded =
            let* response = perform () in
            Lwt.return (Ok response)
          in
          let* outcome = Lwt.pick [ timed_out; succeeded ] in
          (match outcome with
           | Ok response -> on_success response
           | Error msg -> on_error msg);
          Lwt.return_unit)
        (fun exn ->
          on_error (Printexc.to_string exn);
          Lwt.return_unit))

let header_of = function Some h -> Cohttp.Header.of_list h | None -> Cohttp.Header.init ()

let get ?headers url on_success on_error =
  let headers = header_of headers in
  run on_success on_error
    ~perform:(fun () ->
      let* resp, body = Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url) in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (to_response resp body_str))

let post ?headers ?body url on_success on_error =
  let headers = header_of headers in
  let body = match body with Some b -> `String b | None -> `Empty in
  run on_success on_error
    ~perform:(fun () ->
      let* resp, resp_body =
        Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
      in
      let* body_str = Cohttp_lwt.Body.to_string resp_body in
      Lwt.return (to_response resp body_str))
