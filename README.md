# ocaml-llm-sdk

OCaml SDK for LLM provider APIs (Anthropic, OpenAI, xAI Grok). **Runtime-agnostic
and HTTP-client-agnostic** by design: the core is written in continuation-passing
style over a small `HTTP_CLIENT` interface, so the same provider code runs under
Lwt, Eio, or synchronous I/O, against any HTTP library.

Sibling to [`ocaml-social-sdk`](https://github.com/makerprism/ocaml-social-sdk)
and built the same way.

> **Warning: Experimental.** Early, LLM-assisted, under active development. Expect
> breaking changes. The pure wire mapping (encode/decode) is unit-tested; live
> provider calls have had limited validation.

## Why

There was no runtime-agnostic, multi-provider OCaml LLM client. The existing
bindings are single-provider and pin one async runtime (e.g. Eio). This SDK keeps
the provider logic pure and pushes I/O behind a CPS `HTTP_CLIENT`, so it composes
with whatever runtime your app already uses.

LLM tool-calling is uniform enough across vendors — tool inputs are plain **JSON
Schema** everywhere — that one set of provider-neutral types (`message`,
`tool_def`, `tool_call`, `assistant_turn`, `usage`) drives them all.

## Packages

| Package | Description |
|---------|-------------|
| `llm-core` | Runtime-agnostic core: CPS `HTTP_CLIENT` interface + provider-neutral types + the `PROVIDER` signature. Depends only on `yojson`. |
| `llm-anthropic` | Anthropic Messages API — tool use, prompt caching, refusal handling. |
| `llm-openai` | OpenAI Chat Completions — tool/function calling. **Also drives xAI Grok** via a `base_url` swap (Grok is OpenAI-compatible). |
| `llm-lwt` | Lwt adapter: a `Cohttp_lwt_unix` `HTTP_CLIENT` (with timeout) + `to_lwt` to turn a CPS call into a `result Lwt.t`. |

Providers are **functors over `Llm_core.HTTP_CLIENT`**; runtime adapters implement
that interface. To support Eio or sync I/O, implement `HTTP_CLIENT` for it — no
provider code changes.

## Install (Dune Package Management)

```scheme
; dune-project
(pin (url "git+https://github.com/makerprism/ocaml-llm-sdk") (package (name llm-core)))
(pin (url "git+https://github.com/makerprism/ocaml-llm-sdk") (package (name llm-anthropic)))
(pin (url "git+https://github.com/makerprism/ocaml-llm-sdk") (package (name llm-lwt)))
```

```bash
dune pkg lock && dune build
```

## Usage (Lwt)

```ocaml
open Llm_core

(* Pick a provider + the Lwt HTTP client; wrap as an Lwt-returning provider so
   you never touch continuations. For Grok: Llm_openai.Make with a config built
   from Llm_openai.grok_base_url. *)
module Claude =
  Llm_lwt.Lwt_provider.Make (Llm_anthropic.Make (Llm_lwt.Cohttp_client))

let cfg =
  Llm_anthropic.make_config
    ~api_key:(Sys.getenv "ANTHROPIC_API_KEY")
    ~model:"claude-sonnet-4-6" ()

let tools =
  [ { name = "lookup_listing";
      description = "Find a karaoke listing by name and city.";
      input_schema =
        `Assoc
          [ ("type", `String "object");
            ("properties",
              `Assoc [ ("name", `Assoc [ ("type", `String "string") ]);
                       ("city", `Assoc [ ("type", `String "string") ]) ]);
            ("required", `List [ `String "name"; `String "city" ]) ] } ]

let run () : (assistant_turn, error) result Lwt.t =
  let messages = [ { role = User; content = [ Text "karaoke at The Lamp in Bath?" ] } ] in
  Claude.complete cfg ~system:"You map listings to tools." ~messages ~tools ~max_tokens:1024 ()
```

`error` carries `is_retryable` / `is_rate_limited` helpers for backoff, and
`Llm_core.Json` provides non-raising response accessors. Need raw CPS (no Lwt)?
Use the provider's `Make` directly and pass `on_success` / `on_error`.

The agentic loop (call → run the requested tools → feed results back → re-call
until `stop_reason <> Tool_use_stop`) lives in *your* code, above the seam, and is
provider-agnostic.

## Testing without a provider

Because encode/decode are pure, the wire mapping is tested with fixtures (no
network, no key): `dune test`. For higher layers, implement a scripted
`HTTP_CLIENT` that returns canned response bodies in order — deterministic tests
for your agentic loop with zero provider cost.

## License

MIT
