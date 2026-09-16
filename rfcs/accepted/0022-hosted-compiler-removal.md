# RFC 0022: Remove the hosted compiler

Status: Accepted
Date: 2026-09-15
Supersedes: Hosted compiler ownership in RFC 0002, the hosted compiler and conformance obligations of RFCs 0003 and 0004, hosted parser scale obligations of RFC 0008, hosted frontend retention and comparison obligations in RFCs 0016 and 0018, and hosted parity obligations or deferrals in RFCs 0017, 0019, and 0020.

## Decision

Develop and maintain one compiler implementation: the Haskell compiler and
analyzed-core interpreter. Remove the Jazz-authored lexer, parser, canonical-core
lowerer, and other compiler-only modules, together with exclusive comparison
encoders, adapters, fixtures, tests, scale workloads, and build/CI wiring.
Classify shared code by its actual consumers; preserve ordinary parsing,
canonical core, module compilation, runtime behavior, and useful language tests.

Keep the Jazz-authored standard library. Its collection, text, numeric, and I/O
facilities are ordinary language functionality regardless of why they were
originally introduced. Keep Haskell compiler benchmarks, profiling, program
corpus tests, and deterministic runtime observation that serve that compiler.

New language features require implementation and verification in the Haskell
compiler and matching public contracts. They do not require a second frontend,
mirrored schemas, or differential hosted parity. This also removes the hosted
parity requirement from the RFC 0021 proposal without deciding its language
semantics or importing its implementation.

Self-hosting remains a deferred future effort. Resuming it requires an explicit
execution goal and a fresh accepted design covering integration and verification.
Do not retain a dormant alternate frontend or prescribe replacement architecture.
The existing native-backend deferral remains unchanged.

## Context

Ordinary compile/run already uses the Haskell compiler. The hosted frontend ran
as ordinary Jazz programs under that compiler solely through separate comparison
and scale harnesses; it was not a production compilation stage. Maintaining it
required duplicating frontend changes without providing another execution path.
The maintainer approved removing it to focus development on the Haskell compiler.

RFC 0016 deliberately retained the hosted frontend while removing the optional
backend. This decision supersedes that retention. Earlier RFCs and completed
plans remain historical records, with explicit notices identifying retired
obligations; their old instructions do not authorize restoration.

## Consequences

There is one active compiler implementation and fewer comparison-only APIs and
verification jobs. Hosted parity and interpreter-scale evidence disappear with
the implementation they measured. Preserve meaningful production regressions
under their existing owners when the removed tests were their only coverage;
do not copy the entire parity corpus into a new harness.

The public language, Haskell frontend diagnostics, canonical lowering, module
pipeline, interpreter, CLI, and standard-library behavior remain supported.
This decision makes no self-hosting, native-code, ABI, or new language promise.
