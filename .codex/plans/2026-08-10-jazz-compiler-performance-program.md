---
id: JN-COMPILER-PERFORMANCE-SCALE-MATRIX-002
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 2 follow-up: Complete the generated scale matrix"
target_paths:
  - benchmark/Jazz/Benchmark/ScaleCases.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
  - test/Jazz/Benchmark/StageSpec.hs
  - PERFORMANCE.md
verification:
  - cabal test benchmark-stage-spec --test-show-details=failures --jobs=1
  - cabal bench jazz-bench --benchmark-options='--jazz-scale-case=long-token-stream-01024 --list-tests' --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add the six remaining opt-in compiler scale families with exact semantic or parse-artifact tests, then record controlled CPU, allocation, and residency curves."
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

The first reviewable slice owns only sequential polymorphic bindings and wide
module fanout. The remaining six matrix scenarios stay in Task 2 follow-up
instead of inflating this child.

**Files:** create `benchmark/Jazz/Benchmark/ScaleCases.hs`; modify
`benchmark/Jazz/Benchmark/Stages.hs`, `benchmark/Jazz/Benchmark/StageInputs.hs`,
`test/Jazz/Benchmark/StageSpec.hs`, `jazz.cabal`, and `PERFORMANCE.md`.

- [x] In `StageSpec.hs`, add a failing registry test for the literal identifiers
      `sequential-polymorphic-bindings-{0064,0128,0256,0512}` and
      `wide-module-fanout-{0008,0016,0032,0064}x0016`, including scenario and
      size metadata. This catches missing sizes, unstable identities, or a
      generator silently changing the interface width.
- [x] Add failing real-pipeline tests that compile and run the smallest generated
      program from each scenario. Sequential polymorphism must produce
      `(42, True)`; wide fanout must produce `0` with nine virtual sources at the
      `8 x 16` size. Do not assert physical timing.
- [x] Implement `ScaleCases.hs` as a pure owner of `CompilerScaleCase`, scenario
      metadata, deterministic source maps, module resolution configuration, and
      source lookup. Generate virtual module paths in memory; do not create
      corpus entries, checked-in fixtures, or temporary files.
- [x] Extend `StageInputs.hs` with prepared analysis, module-preparation, and
      whole-program boundaries for compiler scale cases. Reuse the ordinary
      compiler driver, forcing helpers, diagnostic rendering, compiled-module
      lookup, and exact runtime-output checks; do not add production forcing.
- [x] Add failing command/selection tests for repeatable
      `--jazz-scale-case=IDENTIFIER`, missing/duplicate/unknown values, and
      rejection when mixed with `--jazz-case` or `--jazz-smoke`.
- [x] Extend `Stages.hs` with an opt-in `compiler-scale` benchmark tree. With no
      scale selector, preserve the existing corpus tree, smoke selection, and
      extended-gate workload exactly. Stable leaves must be addressable as
      `All.compiler-scale.<group>.<identifier>` and recorded metadata must keep
      the existing schema while listing the selected scale identifiers.
- [x] Add `ScaleCases.hs` to the benchmark and benchmark-stage test components;
      add `Stages.hs` and `Metadata.hs` to the focused test component only if the
      command parser tests require them. Document selector and exact leaf usage
      in `PERFORMANCE.md`.
- [x] Run the focused test, exact `--list-tests` selector, CI/queue/docs checks,
      and diff review serially; commit the implementation before physical runs.
- [x] Record CPU time, cumulative allocation, copied bytes, and peak memory for
      all eight generated cases using one optimized benchmark process. Capture
      RTS `-s`, stable stage, hotspot, and heap evidence for the largest case in
      each scenario one process at a time. Record compatible environment and
      command metadata without adding a timing threshold.
- [x] Update this receipt and leave the remaining six generated scenarios as the
      next performance-program promotion candidate. Use the measured growth
      curves to choose the first optimization batch rather than assuming B or D.

### First generated scale receipt

The implementation landed in `b7161746`. The canonical optimized run used
GHC 9.14 on Apple M1 Max / Darwin aarch64, one RTS capability, CPU time mode,
and `+RTS -T -RTS` from a clean tree. Its durable metadata and all 16 samples
are under
`benchmark-results/compiler-scale-baseline/20260811T025843036178000000Z/`.
Values below are per benchmark operation; peak memory is the CSV's reported
high-water value, not a deterministic limit.

| Sequential bindings | Boundary           | Mean ms | Allocated bytes | Copied bytes | Peak memory bytes |
| ------------------: | ------------------ | ------: | --------------: | -----------: | ----------------: |
|                  64 | analysis           |   1.223 |       4,767,015 |      149,355 |         7,340,032 |
|                 128 | analysis           |   3.159 |      10,393,534 |      922,303 |         8,388,608 |
|                 256 | analysis           |   7.796 |      24,937,278 |    2,624,971 |        12,582,912 |
|                 512 | analysis           |  20.265 |      66,626,398 |    6,597,910 |        18,874,368 |
|                  64 | module preparation |   4.990 |      24,070,226 |    2,243,971 |        18,874,368 |
|                 128 | module preparation |   8.317 |      34,224,309 |    3,994,919 |        18,874,368 |
|                 256 | module preparation |  15.021 |      58,020,831 |    7,750,299 |        18,874,368 |
|                 512 | module preparation |  31.122 |     118,413,538 |   15,672,641 |        19,922,944 |

| Fanout modules x width | Boundary           | Mean ms | Allocated bytes | Copied bytes | Peak memory bytes |
| ---------------------: | ------------------ | ------: | --------------: | -----------: | ----------------: |
|                 8 x 16 | module preparation |   4.521 |      21,041,027 |    2,027,642 |        19,922,944 |
|                16 x 16 | module preparation |   6.165 |      27,445,577 |    3,132,915 |        19,922,944 |
|                32 x 16 | module preparation |  10.966 |      40,279,722 |    6,658,760 |        19,922,944 |
|                64 x 16 | module preparation |  18.337 |      66,000,121 |   11,848,023 |        22,020,096 |
|                 8 x 16 | whole program      |   5.683 |      22,998,425 |    2,136,882 |        22,020,096 |
|                16 x 16 | whole program      |   7.604 |      29,938,337 |    3,248,899 |        22,020,096 |
|                32 x 16 | whole program      |  12.977 |      43,859,364 |    7,096,401 |        22,020,096 |
|                64 x 16 | whole program      |  20.814 |      71,788,044 |   13,075,681 |        23,068,672 |

The two largest module-preparation cases also have serial stable-stage JSON
profiles and eventlogs, late-cost-centre hotspot profiles, separate 1 ms live
heap profiles, and RTS summaries under
`profile-results/compiler-scale-baseline/`. The sequential process allocated
5,682,721,584 bytes cumulatively and reached 3,762,104 bytes maximum residency;
the wide-fanout process allocated 3,274,961,312 bytes and reached 4,846,752
bytes maximum residency. The separate heap profiles sampled peaks of about
3.82 MB and 3.51 MB respectively.

The evidence prioritizes environment free-variable maintenance as the first
optimization child after the complete scale matrix. From 64 to 512 sequential
bindings, analysis CPU grows 16.6x and allocation 14.0x for an 8x input. Its
stable stage profile assigns 423 of 1,315 ticks and 1,382,073,712 allocated
bytes to type inference. The hotspot profile assigns 131 ticks and 447,907,840
allocated bytes to `freeTypeVariablesInEnv`, plus another 77 ticks each to
`freeTypeVariables` and `freeTypeVariablesInScheme`. By contrast, 8x wider
fanout grows module-preparation CPU 4.1x and allocation 3.1x; parser work and
type inference dominate that profile, while `importSelectedInterface` is a
smaller 19-tick / 105,475,608-byte signal. These are prioritization facts, not
portable thresholds.

## Task 2 follow-up: Complete the generated scale matrix

This child adds the six missing families without changing the selector,
metadata schema, default corpus tree, smoke workload, or public language
behavior. It reuses the in-memory `CompilerScaleCase` registry and ordinary
compiler/parser boundaries from the first slice.

**Files:** `benchmark/Jazz/Benchmark/ScaleCases.hs`,
`benchmark/Jazz/Benchmark/StageInputs.hs`,
`test/Jazz/Benchmark/StageSpec.hs`, and `PERFORMANCE.md`.

- [ ] Register `interleaved-recursive-groups-{0016,0032,0064,0128}`. Each
      independent group must place a polymorphic use between mutually recursive
      members, participate in analysis and module-preparation, and preserve the
      exact runtime result `(1, True)` at the smallest size.
- [ ] Register `constrained-signatures-{0032,0064,0128,0256}`. Generate one
      visible unary class, concrete `Int` and `Bool` impl facts, and that many
      explicitly constrained polymorphic identities. Participate in analysis
      and preserve `(1, True)` at the smallest size.
- [ ] Register `deep-nested-lambdas-{0016,0032,0064,0128}`. Generate an explicit
      unary lambda chain whose result captures both the first and final
      parameters. Participate in analysis and module-preparation; the smallest
      case must return `(1, 16)`.
- [ ] Register `large-operator-tables-{0016,0032,0064,0128}`. Generate unique,
      valid non-built-in operator symbols, declare every symbol, and parse one
      use per declaration. Participate only in parse/lower so runtime operator
      implementation is not conflated with lookup cost.
- [ ] Register `nested-blocks-{0016,0032,0064,0128}`. Generate nested expression
      blocks with one local binding per level and participate only in
      parse/lower. The smallest case must parse and lower successfully.
- [ ] Register exact `long-token-stream-{01024,04096,16384,65536}` cases. Each
      source contains only four-token binding statements, so the identifier's
      size equals `length (tokenize source)` exactly. Participate only in
      parse/lower; the smallest test must assert exactly 1,024 tokens.
- [ ] Add prepared parse/lower support for generated cases by forcing the owned
      entry source during setup and reusing the existing lex/parse/lower
      boundary. Keep compiler-only groups unchanged and reject unsupported
      combinations.
- [ ] Write registry and smallest-case tests before implementation. Every test
      must exercise the real compiler or parser and assert literal outputs,
      source counts, or token counts rather than physical time.
- [ ] Run the focused suite, exact list-tree command, queue/docs checks, and
      diff review serially; commit before physical measurement.
- [ ] Record all 24 new cases in one optimized `+RTS -T` process. Profile the
      largest member of each family with serial RTS/stage/hotspot/heap commands,
      reducing a size rather than widening limits if the controlled machine
      cannot finish safely.
- [ ] Record the matrix, compare growth factors and dominant profiles, then
      promote the smallest evidence-backed optimization child. Do not use
      single-machine thresholds.

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
