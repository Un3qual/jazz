---
id: JN-HASKELL-POLYMORPHISM-EFFECTS-001
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/Parser/Lower.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
verification:
  - cabal test all --jobs=4 --test-show-details=failures
deliverable: "Simplify lowering metadata traversals, module discovery errors, and prepared benchmarks without changing semantics."
last_verified: 2026-09-09
---

# Polymorphism and effects simplification

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

The user authorized compiler architectural changes in this task when they
preserve functionality and improve code. The audit starts from `325220df`.
RFC 0016 retains analyzed core, phase indexing, nominal identities, runtime
plans, and hosted frontend conformance. No queued future implementation
supersedes these owners.

## Implementation

- [x] Share the two lowered-tree metadata walks using a local rank-n node
      operation and an Applicative traversal. Preserve strict source preorder,
      every embedded span, and all payloads. This traverses locations only;
      binding-sensitive semantic passes keep their own traversal.
- [x] Use ExceptT in module discovery to remove manual error propagation.
      Keep explicit state, dependency order, first-error selection, cycle
      detection, source-loading behavior, and all-or-error public results.
- [x] Replace corpus/generated prepared-benchmark sum types with one opaque
      existential package pairing an input, its NFData dictionary, and its
      runner. Keep selective analysis-input forcing, runtime rendering-only
      output forcing, stage markers, validation, and repeated execution.

## Audit decisions

Rank-n operations have two concrete metadata consumers; existential benchmark
inputs have eleven concrete preparation cases. Neither needs a new package.
Linear resource APIs do not remove work from the existing bracketed host I/O
boundary. Impredicative containers have no current consumer. A profunctor or
functional-dependency class would duplicate existing function/record boundaries.
Quantified constraints and visible/required type arguments do not eliminate a
current workaround. Keep those features available when a concrete use appears.

The resolver needs only ExceptT, not a whole-inference monad-stack rewrite.
Runtime continuation frames remain inspectable, stack-safe first-order data;
opaque higher-order continuations would make those properties harder to verify.

## Verification

Compile with the pinned GHC and development warnings. Check node identity/span
contracts, hosted lowering parity, resolver loading order and error stopping,
benchmark setup/runtime boundaries, and repeated runner execution. Run all 60
default suites, both profiling builds, formatting, package and queue checks.
Record measured line changes and commit evidence after verification.

Feature references: [GHC rank-n types](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html),
[GHC existentials](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html).

## Completion evidence

Implemented in `339183f4` (`Simplify lowering, resolution, and benchmark execution`).

- `cabal build all --enable-tests --enable-benchmarks --jobs=4` passed with
  development warnings enabled.
- `cabal test all --jobs=4 --test-show-details=failures` passed all 60 default
  suites. Focused module-resolution, core-normalization, canonical-core
  comparison, and benchmark-stage suites also passed during implementation.
- Both stage and hotspot profiling presets built successfully in their separate
  build directories with `--jobs=2` alongside the default test matrix.
- The real `jazz-bench --jazz-smoke` executable passed all six stage groups.
- `cabal check`, pinned Ormolu, `git diff --check`, and `scripts/check-docs.sh`
  passed. Queue completion is checked separately after recording this commit.

Regression coverage verifies source loading stops at the first lexical
dependency failure, preparation defers module work, and a prepared benchmark
reloads sources on a subsequent invocation. Existing tests cover metadata
traversal preorder, qualified spans, hosted conformance, and forcing boundaries.
The implementation removes 132 lines, or 83 lines including regression coverage.
No public language contract or package dependency changed.
