---
id: JN-COMPILER-PERFORMANCE-PROGRAM-001
status: active
priority: P1
size: XL
kind: coordination
autonomous_ready: no
depends_on:
  - JN-COMPILER-PERFORMANCE-VERIFICATION-FLOW-001
plan_section: "Task 2: Add generated compiler scale scenarios"
target_paths:
  - benchmark/Jazz/Benchmark/Stages.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
  - test/Jazz/Compiler/
  - jazz.cabal
verification:
  - cabal test benchmark-stage-spec --test-show-details=failures --jobs=1
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Add generated compiler scale fixtures and record controlled pre-optimization growth, allocation, and residency baselines."
last_verified: 2026-08-10
---

# Jazz Compiler Performance and Memory Program

**Goal:** Measure, prioritize, and remove real Jazz compiler/runtime CPU,
allocation, and peak-residency problems without changing public language
semantics, diagnostic ordering, binder identity, or artifact schemas.

**Architecture:** Use RFC 0008's existing separation between deterministic
correctness and physical evidence. Generated scale scenarios isolate one growth
curve at a time; ordinary tests own semantics and exact artifacts, while
compatible `jazz-bench`, RTS, stage-profile, and heap-profile receipts justify
each representation change. Compiler changes remain small and ordered so a
later lifetime reduction never hides an earlier asymptotic problem.

**Tech stack:** Haskell/GHC 9.14.1, Cabal, `tasty`, `tasty-bench`, GHC RTS and
cost-centre profiling, Bash, Python 3, Nix.

## Global constraints

- Work on `codex/compiler-performance-program`, based exactly on
  `codex/typed-core-closure-recursion` at
  `bf3f87dc1774e80e7ad9fc2eed5f9c5f989c2f9d`.
- Public language semantics, diagnostics and their order, binder identity,
  exact Typed Core and Lowered IR artifacts, hosted parity, and serialized
  schemas are invariant unless a separately approved RFC changes them.
- Exactly one Cabal, Jazz, benchmark, profiling, or Nix process may run at a
  time. Every local Cabal gate uses `--jobs=1` by default. Never replace a quiet
  or detached process with a duplicate; inspect and continue the original.
- Physical time, allocation, and residency are comparable evidence, never
  deterministic CI thresholds. Deterministic semantic and exact-artifact tests
  remain gating.
- Do not force compiler phases speculatively. Profiling-only forcing remains
  isolated until live-residency evidence proves a production strictness change.
- Generated benchmark and profile output stays ignored under
  `benchmark-results/` and `profile-results/`; record stable metadata and
  summarized measurements here or in the review record.
- Use focused tests during TDD, the affected subsystem/G1 gate once per batch,
  and one full closeout gate after the final source change in the program.

## Current-tree audit and deduplicated inventory

| Batch                                          | Original items                      | Confirmed current-tree shape                                                                                                                                                                                                                                                                                                       | Dependency and disposition                                                                                                                                                 |
| ---------------------------------------------- | ----------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A. Verification and scale evidence             | verification-flow requirements      | `main-functional.sh` runs an ordinary Cabal build and complete suite, then starts a fresh Nix package build; main, extended, and release commands do not bound Cabal/Nix jobs; the benchmark corpus does not isolate the required compiler growth curves                                                                           | Land bounded phase selection first, then generated scenarios before any optimization                                                                                       |
| B. Type-checker asymptotics                    | 1, 7 (constraint dedupe), 9, 10, 15 | `Solver.hs` follows substitution chains through `Map Int ExpressionType`; `Scope.hs` performs recursive previews and whole-environment free-variable scans; deferred deltas are list/length based; constraint dedupe uses ordered linear membership                                                                                | Add scale cases first; make buffers/cursors explicit, remove duplicate recursive inference, maintain environment free-variable summaries, then zonk/compress substitutions |
| C. Reusable scope and capture facts            | 12, 13                              | `RecursiveBindings.hs`, resolution, inference, analyzer, nested walks, and runtime planning recompute related dependency facts; lambda hints retain/search `Expr` bodies                                                                                                                                                           | Requires B's inference ownership to be stable; introduce reusable scope facts, then stable lambda IDs and capture plans                                                    |
| D. Module interfaces and artifact lifetime     | 2, 3, 11, 18                        | `ModuleCompiler.lookupDependency` linearly scans prior modules per import; interface imports deep-rebase schemes and merge maps/sets per module; `CompiledModule` retains `ResolvedModule` plus compiled `Expr`; `CompiledProgram` duplicates diagnostic list spines; ordinary inference retains final state through analyzer work | Index dependencies first, then cache canonical/rebased interfaces, compact compiled artifacts, and finalize ordinary inference state earlier                               |
| E. Parser and resolver passes                  | 4, 5, 6, 14, 17                     | operator lookup combines declared and builtin lists per use; alias collection scans remaining tokens; owned-prefix parsing reparses/scans consumed input; tokens are list-backed and retain payload plus lexeme; resolver lowers and separately walks `SurfaceExpr` for several fact sets                                          | Preindex cheap scope facts first, replace owned-prefix parsing with one cursor, then change token storage/ownership; fuse resolver facts after parser ownership is stable  |
| F. Checked handoffs and remaining accumulation | 7 (analyzer lists), 16              | analyzer diagnostics include append-heavy construction; Typed Core production validates and Lowered IR validates the same trusted artifact again                                                                                                                                                                                   | Safe after the larger ownership changes; add an opaque checked boundary and finish list-builder cleanup                                                                    |
| Global guard                                   | 8                                   | benchmark forcing is explicitly isolated in `benchmark/Jazz/Benchmark/Force.hs` and profiling tests                                                                                                                                                                                                                                | Not a standalone optimization; every batch must preserve this boundary unless heap evidence says otherwise                                                                 |

The clusters remove overlap without discarding any issue. Item 18 is sequenced
with compiled-artifact lifetime work rather than treated as a second analyzer
optimization. Item 7 is split between type-scheme constraint identity in B and
diagnostic accumulation in F. Item 12 owns reusable scope/dependency facts;
item 9 owns only the inference policy that consumes them.

## Controlled baseline receipt

The pre-change receipt was captured from a clean tree at `bf3f87dc` on an Apple
M1 Max, Darwin/aarch64, GHC 9.14, Cabal library 3.16.1.0, one RTS capability,
and profiling build mode. The exact benchmark leaf was
`All.jazz.module-preparation.identifier-classifier`.

| Evidence                    | Pre-change observation                                                                                                              | Artifact                                                                                   |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| `tasty-bench` CPU result    | 37.518875 ms mean; 3.746531896 ms two standard deviations                                                                           | `benchmark-results/compiler-performance-baseline/20260811T014048055686000000Z/results.csv` |
| Per-operation allocation    | 106,370,822 bytes allocated; 14,937,384 bytes copied                                                                                | same `results.csv`                                                                         |
| Per-operation peak memory   | 12,582,912 bytes                                                                                                                    | same `results.csv`                                                                         |
| Whole benchmark process RTS | 3,324,191,416 bytes cumulative allocation; 2,674,152 bytes maximum residency; 12 MiB total memory in use                            | `profile-results/compiler-performance-baseline/module-preparation.stats`                   |
| Stable stage profile        | JSON cost-centre profile with runtime-preparation, type-inference, constraint-solving, static-analysis, parser, and resolver frames | `profile-results/compiler-performance-baseline/module-preparation.prof`                    |
| Live heap                   | cost-centre-stack heap samples at 1 ms                                                                                              | `profile-results/compiler-performance-baseline/module-preparation.heap.hp`                 |

The first attempted measurement used the invalid Tasty pattern
`jazz/module-preparation` and executed no benchmark. Listing the registered
tree identified the required `All.` root; the corrected invocation above is
the only recorded physical result. These ignored files are local evidence, not
source-distribution inputs.

## Generated baseline matrix

Each scenario must generate valid source or internal input deterministically,
assert the exact semantic/artifact result at a small size, and expose at least
four increasing sizes. Initial sizes are starting points; reduce them if a
pre-change curve risks exhausting the machine rather than widening limits.

| Scenario                                 | Isolates                                                           | Initial sizes                              | Timed/profiled boundary              | Primary issue cluster |
| ---------------------------------------- | ------------------------------------------------------------------ | ------------------------------------------ | ------------------------------------ | --------------------- |
| Sequential polymorphic bindings          | repeated generalization and substitution application               | 64, 128, 256, 512 bindings                 | analysis and module preparation      | B: 1, 10, 15          |
| Interleaved recursive groups             | previews, future-body reinference, dependency/SCC facts            | 16, 32, 64, 128 groups                     | analysis and module preparation      | B/C: 9, 12            |
| Constrained signatures                   | constraint buffering, delta cursors, ordered dedupe                | 32, 64, 128, 256 signatures                | analysis                             | B/F: 7, 15            |
| Deep nested lambdas                      | repeated free-variable walks and AST-keyed hint lookup             | 16, 32, 64, 128 lambdas                    | analysis and runtime preparation     | C: 13                 |
| Wide module fanout with large interfaces | dependency lookup, rebasing, interface copying, compiled retention | 8, 16, 32, 64 imports with 16 exports each | module preparation and whole program | D: 2, 3, 11, 18       |
| Large operator tables                    | combined-list allocation and linear symbol lookup                  | 16, 32, 64, 128 declarations               | parse/lower                          | E: 4                  |
| Nested blocks                            | redundant alias scans and parser cursor ownership                  | 16, 32, 64, 128 blocks                     | parse/lower                          | E: 5, 17              |
| Long token streams                       | list cursor cost, duplicated lexeme storage, prefix rescans        | 1K, 4K, 16K, 64K tokens                    | parse/lower plus heap                | E: 6, 17              |

No scenario gains a wall-clock assertion. A deterministic test owns source,
diagnostic, binder, and artifact equality; `jazz-bench` and GHC profiling own
physical comparison artifacts.

## Batch gate ladder

1. Write the focused semantic regression or exact-artifact fixture and prove it
   fails for the intended missing behavior/probe.
2. Record the old growth curve and one RTS/stage/heap receipt with `--jobs=1`.
3. Implement one representation or pass change without changing forcing.
4. Run the focused test, then the affected subsystem/G1 gate once.
5. Record the compatible after curve and RTS/stage/heap receipt.
6. Stop on any semantic, diagnostic-order, artifact, allocation, or residency
   regression until it is understood; do not widen limits.
7. Commit the batch and update this plan's receipt and next priority.

## Task 1: Bound and phase heavyweight verification

**Files:** `scripts/ci/main-functional.sh`, `scripts/ci/extended.sh`,
`scripts/ci/release-candidate.sh`, `scripts/check-ci-policy.py`,
`scripts/test-check-ci-policy.py`, `PERFORMANCE.md`.

- [x] Add behavior tests proving main verification has explicit
      `all|compiler|repository|nix|low-memory` phases, defaults to authoritative
      `all`, rejects invalid phases/job values before executing work, and skips Nix
      only in the documented low-memory phase.
- [x] Add policy mutations requiring `--jobs="$JAZZ_CABAL_JOBS"` on every
      heavyweight Cabal build/test/bench command and bounded `--max-jobs`/`--cores`
      on internal Nix builds/checks.
- [x] Implement phase functions in `main-functional.sh`. Default Cabal jobs,
      Nix max jobs, and Nix cores to `1`; keep no-argument CI behavior authoritative.
- [x] Propagate the same bounded variables through extended and release
      verification. Preserve required experimental features and all current
      extended/release evidence.
- [x] Document low-memory local use and state that release publication still
      requires the full main, extended, Nix, packaging, and artifact gates.
- [x] Run the four frontmatter verification commands, commit this child, remove
      its completed queue row, and leave Task 2 as the next performance-program
      promotion candidate.

Task 1 landed in `c2bbded9` with 110 CI-policy behavior tests passing. The live
policy checker, execution-queue checker, shell syntax check, and diff check also
passed. This batch changed verification orchestration only, so it preserved the
controlled compiler baseline above and did not require a misleading physical
"after" profile. Task 2 is the next performance-program promotion candidate.

## Task 2: Add generated compiler scale scenarios

**Files:** create focused generator/fixture owners under `test/Jazz/Compiler/`
and `benchmark/Jazz/Benchmark/`; modify `benchmark/Jazz/Benchmark/Stages.hs`,
`benchmark/Jazz/Benchmark/StageInputs.hs`, `jazz.cabal`, and focused benchmark
tests.

- [ ] Add the eight scenarios from the matrix with exact small semantic
      results, stable identifiers, selected size metadata, and no physical
      thresholds.
- [ ] Keep production-shaped corpus cases unchanged; generated cases are an
      explicit compiler-scale benchmark family rather than fake corpus entries.
- [ ] Add CLI selection that can run one scenario and one size, plus list-tests
      coverage so profiling commands name exact leaves.
- [ ] Record all pre-optimization curves one process at a time and prioritize B
      and D using total allocation, maximum residency, and growth shape.

## Task 3: Remove type-checker asymptotic work

- [ ] Replace append/length delta tracking with append-efficient buffers and
      explicit cursors while preserving constraint order.
- [ ] Infer each recursive body once per necessary environment state and cache
      reusable group results.
- [ ] Maintain environment free-variable summaries or levels instead of
      rescanning the complete visible environment per generalization.
- [ ] Replace repeated substitution-chain resolution with an `IntMap`-backed
      zonk/compression boundary and avoid re-resolving child subtrees during
      recursive unification.
- [ ] Replace ordered linear constraint membership with stable-identity sets
      while emitting constraints in original order.

## Task 4: Reuse recursive scope and lambda capture facts

- [ ] Build declaration visibility, same-name indices, dependencies, and SCCs
      in one pass with append-efficient builders.
- [ ] Transport reusable immutable scope facts to resolution, inference,
      analyzer, free-variable, and runtime-scope consumers only where their
      semantics agree.
- [ ] Assign stable lambda IDs during owned lowering, compute capture plans
      once, and stop retaining/searching lambda `Expr` bodies as keys.

## Task 5: Index interfaces and compact compiled lifetime

- [ ] Thread a first-match-preserving module-path index through module
      compilation and keep the duplicate-path contract exact.
- [ ] Canonicalize interface names and cache ambient prelude/dependency rebases
      so module compilation does not deep-copy unchanged schemes/declarations.
- [ ] Split runtime/debug metadata from full resolved/compiled AST retention and
      remove the aggregate diagnostic spine where per-module order can be consumed
      directly.
- [ ] Finalize ordinary inference into the compact interface/solver result
      before analyzer work; retain the full state only for Typed Core production.

## Task 6: Reduce parser/resolver passes and checked-boundary cleanup

- [ ] Preindex scope-aware operator metadata and collect legal import aliases in
      the main module parse rather than rescanning nested token tails.
- [ ] Replace the owned-prefix/list adapter with one cursor-based parse and
      reuse compact-signature discrimination.
- [ ] Move tokens to indexed storage with source offsets/spans and own `Text`
      only where later semantics require it.
- [ ] Fuse lowering and module-fact collection into one `SurfaceModuleFacts`
      traversal or an equivalent returned lowering product.
- [ ] Introduce an opaque validated Typed Program handoff so trusted
      producer-to-lowerer transport validates once, while external artifacts remain
      checked.
- [ ] Finish reverse-builder/ordered-set cleanup for analyzer diagnostics.

## Full closeout

After the final source change, run exactly one complete ordinary closeout, one
extended/profile evidence closeout, and the mandatory release gate with bounded
jobs. Compare all eight generated scenarios to their compatible baselines,
retain semantic and exact-artifact results, summarize timing/allocation/maximum
residency without universal thresholds, and update `PERFORMANCE.md` only for
durable workflow changes.
