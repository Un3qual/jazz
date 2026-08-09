# RFC 0003: Bootstrap interpreter profile

Status: Accepted
Date: 2026-07-31
Supersedes: Bootstrap and interpreter decisions dated 2026-03-03, 2026-03-18, 2026-07-10, 2026-07-11, and 2026-07-12.

## Decision

The Haskell compiler and interpreter under `src/` are Jazz stage 0. Stage 0 is
the executable semantic reference while compiler components are introduced as
ordinary Jazz modules under `jazz/compiler/`.

Hosted bootstrapping means stage 0 loads and runs Jazz-authored compiler code
and compares its structured results with the Haskell reference. It is not the
self-hosting fixed point. Jazz may claim that fixed point only after:

1. stage 0 runs the Jazz-authored compiler and emits native stage-1 artifacts;
2. the stage-1 compiler can compile the same compiler sources; and
3. stage 1 produces semantically equivalent stage-2 output and a conforming
   native compiler.

The committed long-term compiler and execution boundaries are:

```text
Jazz source
  -> hosted canonical core
      |-> reference interpreter
      |     -> RuntimeHost (reference execution boundary)
      `-> analysis and type inference
            -> post-inference typed core
            -> backend-neutral lowered IR
            -> LLVM IR
            -> object generation and linking
            -> native Jazz program
                  -> native runtime host ABI
```

Lexing, parsing, and surface lowering produce the hosted canonical-core
boundary. The reference interpreter branches from canonical core. The native
path instead runs analysis and type inference once, finalizes a separate
post-inference typed-core tree, and lowers that validated tree to lowered IR.
There is no bytecode format or virtual machine between lowered IR and LLVM.
The interpreter remains a permanent reference execution engine and development
surface beside the native pipeline; it does not consume typed core or lowered
IR, and its private evaluation machine is not an artifact format or backend IR.

The bootstrap profile keeps the trusted host boundary small. The host owns
immutable text storage, strict UTF-8 process I/O, and stack-safe interpreter
execution. Jazz code owns ordinary compiler data, `Maybe` and `Result`, source
positions, tokens, diagnostics, parser state, collection helpers, and compiler
logic. Host-backed implementations are permitted behind stable Jazz standard-
library APIs when their semantics and deterministic ordering are explicit.

The profile is rank-1 and backend-neutral: polymorphic schemes quantify type
variables only at their outermost level, and higher-rank function arguments or
results are outside the profile. It includes Unicode `Char` and `Text`, generic
named types, explicit-import library modules, deterministic host I/O, and
tail-safe hosted traversal. It does not authorize Haskell compiler callbacks
inside Jazz-authored compiler modules.

The current hosted boundary includes a Jazz-authored lexer, complete parser,
and canonical-core lowerer with differential parity evidence. Matching typed-
core and lowered-IR contracts also exist, and a bounded single-module scalar/
direct-call path can produce and lower typed core when explicitly requested.
Normal compile and run mode still use canonical core and the interpreter.
Closures, recursion, full control-flow lowering, multi-module typed-core
integration, LLVM, object/link production, and the native runtime remain
unpromoted design gates.

## Context

A minimal text-and-file bridge would have started a hosted lexer quickly but
left generic APIs and deep recursive traversal unsuitable for larger compiler
components. Completing the entire language and native backend before hosting
anything would have delayed useful parity evidence behind unrelated work.

The selected profile establishes reusable semantic boundaries early, then
ports one independently comparable compiler stage at a time. It keeps the
interpreter useful without confusing hosted execution with native self-hosting.

## Consequences

- Every hosted stage uses ordinary Jazz values and deterministic comparison
  contracts rather than ad hoc serialization.
- New host services must be general semantic runtime services behind Jazz APIs,
  not compiler-specific escape hatches.
- The reference interpreter and future native backend consume different
  permanent boundaries: canonical core and lowered IR respectively.
- Native-backend work requires separate accepted designs for LLVM lowering,
  the runtime ABI, allocation, object production, and linking.
- Documentation must distinguish hosted parity, opt-in stage-0 production, and
  the unachieved self-hosting fixed point.
