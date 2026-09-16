---
id: JN-HASKELL-FEATURE-SIMPLIFICATION-001
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Approved changes"
target_paths:
  - src/Jazz/Compiler/AST.hs
  - src/Jazz/Compiler/TypeInference/Types.hs
  - src/Jazz/Compiler/TypeInference/Analyzed.hs
  - src/Jazz/Compiler/ModuleGraph.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
verification:
  - cabal test all --jobs=4 --test-show-details=failures
deliverable: "Apply the ten approved Haskell simplifications while preserving compiler behavior and ownership."
last_verified: 2026-09-09
---

# Haskell feature simplification

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

Approved and implemented inline in this task on 2026-09-09. This is a bounded
cleanup of existing owners, not a new compiler representation or public contract.

## Approved changes

- [x] Parameterize type-scheme constraint payloads and derive Functor, Foldable,
      and Traversable for existing resolution, substitution, collection, and
      conditional-conversion callers. Keep metadata-specific cases explicit.
- [x] Replace repeated AST/module instance constraints with higher-kinded
      constraint synonyms.
- [x] Represent analyzed-fact attachment as success or nonempty accumulated
      failures. Preserve efficient sequence concatenation and diagnostic order.
- [x] Store checked import qualifiers inside the qualified exposure constructor.
      Retain raw invalid import shapes and the hosted canonical schema.
- [x] Derive NFData for benchmark data that is already fully forced. Preserve
      selective CompileInputs and runtime-output forcing.
- [x] Remove the capability parser's duplicate ParserFailure wrapper.
- [x] Use structured equality-visit keys with the existing rendered name identity.
- [x] Use Endo and newtype-derived composition for diagnostic builders.
- [x] Share fresh-variable allocation through strict State and replicateM,
      keeping the current explicit inference API and allocation order.
- [x] Add a rank-n RuntimeHost mapper shared by lifting and profiling.

The two additional candidates were approved in this task on 2026-09-09.

## Boundaries and verification

RFC 0016 retains the phase-indexed AST, analyzed interpreter, semantic facts,
runtime plans, and hosted frontend conformance. No accepted future queue child
supersedes this work. Preserve import visibility, source and diagnostic order,
constraint provenance, solver behavior, benchmark evaluation boundaries, and
canonical frontend schemas. No new dependency package is required; benchmark
components may declare the already-pinned deepseq dependency explicitly.

Use focused compiler/module/parser/profiling checks during implementation,
then the development-warning build, all default suites, profiling builds,
formatting, and repository checks. Commit verified milestones without pushing.

## Completion evidence

Implemented in `7255aef7` (`Simplify Haskell traversal, validation, and runtime adapters`).

- `cabal build all --enable-tests --enable-benchmarks --jobs=4` passed with the
  development warning policy.
- `cabal test all --jobs=4 --test-show-details=failures` passed all 60 default
  suites, including hosted frontend conformance, modules, inference, runtime,
  profiling, benchmarks, and repository checks.
- Both profiling presets built successfully in their separate build directories
  with `--jobs=2` while the default tests ran with four workers.
- `cabal check`, the pinned Ormolu check of changed Haskell files,
  `git diff --check`, and `scripts/check-docs.sh` passed.

Regression coverage checks accumulated fact-error order, fresh allocation
identities and retained constraints, and one wrapper application for every host
operation. The implementation removes 179 lines overall; including regression
coverage, the Haskell diff is 137 lines smaller. CompileInputs still uses shallow
forcing and runtime results still force only rendered output. No public language
contract changed.
