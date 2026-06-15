(lang dune 3.16)
(name ocaml-llm-sdk)

(generate_opam_files true)

(source (github makerprism/ocaml-llm-sdk))
(license MIT)
(authors "Sabine Schmaltz")
(maintainers "sabine@makerprism.com")
(documentation "https://github.com/makerprism/ocaml-llm-sdk")
(bug_reports "https://github.com/makerprism/ocaml-llm-sdk/issues")

(package
 (name llm-core)
 (synopsis "Core interfaces for LLM provider API clients")
 (description
  "Runtime-agnostic, HTTP-client-agnostic core interfaces for building LLM \
   provider API clients (Anthropic, OpenAI, xAI Grok, ...). Provides a \
   CPS-style HTTP-client interface that can be implemented with any async \
   runtime (Lwt, Eio, synchronous) and any HTTP library, plus provider-neutral \
   message / tool-use / usage types so the same agentic loop works across \
   providers.")
 (depends
  (ocaml (>= 4.14))
  yojson))

(package
 (name llm-anthropic)
 (synopsis "Anthropic Messages API client")
 (description
  "Anthropic Messages API client over llm-core. Tool use (JSON Schema), prompt \
   caching, system prompts, and refusal handling. HTTP-client- and \
   runtime-agnostic via the llm-core CPS interface.")
 (depends
  (ocaml (>= 4.14))
  (llm-core (= :version))
  yojson))

(package
 (name llm-openai)
 (synopsis "OpenAI Chat Completions client (covers xAI Grok)")
 (description
  "OpenAI Chat Completions client over llm-core. Tool/function calling (JSON \
   Schema) and structured outputs. xAI Grok is supported via a base_url swap \
   (Grok's API is OpenAI-compatible). HTTP-client- and runtime-agnostic.")
 (depends
  (ocaml (>= 4.14))
  (llm-core (= :version))
  yojson))

(package
 (name llm-lwt)
 (synopsis "Lwt runtime adapter for llm-core")
 (description
  "Lwt runtime adapter for llm-core: a Cohttp_lwt_unix implementation of the \
   CPS HTTP_CLIENT interface (with timeout), and helpers to turn a provider's \
   CPS call into an ('a, error) result Lwt.t.")
 (depends
  (ocaml (>= 4.14))
  (llm-core (= :version))
  lwt
  (lwt_ppx (>= 2.0.0))
  cohttp-lwt-unix))
