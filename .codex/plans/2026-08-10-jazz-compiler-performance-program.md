---
id: JN-COMPILER-PERFORMANCE-INDEXED-TOKENS-018
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 6b: Remove parser and resolver repeat passes"
target_paths:
  - src/Jazz/Compiler/Parser/Lexer.hs
  - src/Jazz/Compiler/Parser.hs
  - src/Jazz/Compiler/Parser/Declaration.hs
  - src/Jazz/Compiler/Parser/Expression.hs
  - src/Jazz/Compiler/Parser/Pattern.hs
  - src/Jazz/Compiler/Parser/TokenParser.hs
  - test/Jazz/Compiler/Parser/DeclarationParserSpec.hs
  - test/Jazz/Compiler/Parser/ModuleImportParserSpec.hs
  - test/Jazz/Compiler/Parser/TokenParserSpec.hs
  - test/Jazz/Benchmark/StageSpec.hs
verification:
  - cabal test token-parser-spec expression-parser-spec declaration-parser-spec module-import-parser-spec benchmark-stage-spec --test-show-details=failures --jobs=1
  - cabal bench jazz-bench --benchmark-options='--environment-label=compiler-indexed-tokens --time-mode=cpu --jazz-scale-case=long-token-stream-01024 --jazz-scale-case=long-token-stream-04096 --jazz-scale-case=long-token-stream-16384 --jazz-scale-case=long-token-stream-65536 --pattern=parse-lower +RTS -T -RTS' --jobs=1
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Move parser input to indexed token storage with source-backed spans and a state-correct cursor, then eliminate the owned-prefix adapter's second consumed-prefix walk without retaining prior input; preserve token payloads required by semantics, exact diagnostics/spans, ASTs, and hosted parity."
last_verified: 2026-08-11
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

- [x] Register `interleaved-recursive-groups-{0016,0032,0064,0128}`. Each
      independent group must place a polymorphic use between mutually recursive
      members, participate in analysis and module-preparation, and preserve the
      exact runtime result `(1, True)` at the smallest size.
- [x] Register `constrained-signatures-{0032,0064,0128,0256}`. Generate one
      visible unary class, concrete `Int` and `Bool` impl facts, and that many
      explicitly constrained polymorphic identities. Participate in analysis
      and preserve `(1, True)` at the smallest size.
- [x] Register `deep-nested-lambdas-{0016,0032,0064,0128}`. Generate an explicit
      unary lambda chain whose result captures both the first and final
      parameters. Participate in analysis and module-preparation; the smallest
      case must return `(1, 16)`.
- [x] Register `large-operator-tables-{0016,0032,0064,0128}`. Generate unique,
      valid non-built-in operator symbols, declare every symbol, and parse one
      use per declaration. Participate only in parse/lower so runtime operator
      implementation is not conflated with lookup cost.
- [x] Register `nested-blocks-{0016,0032,0064,0128}`. Generate nested expression
      blocks with one local binding per level and participate only in
      parse/lower. The smallest case must parse and lower successfully.
- [x] Register exact `long-token-stream-{01024,04096,16384,65536}` cases. Each
      source contains only four-token binding statements, so the identifier's
      size equals `length (tokenize source)` exactly. Participate only in
      parse/lower; the smallest test must assert exactly 1,024 tokens.
- [x] Add prepared parse/lower support for generated cases by forcing the owned
      entry source during setup and reusing the existing lex/parse/lower
      boundary. Keep compiler-only groups unchanged and reject unsupported
      combinations.
- [x] Write registry and smallest-case tests before implementation. Every test
      must exercise the real compiler or parser and assert literal outputs,
      source counts, or token counts rather than physical time.
- [x] Run the focused suite, exact list-tree command, queue/docs checks, and
      diff review serially; commit before physical measurement.
- [x] Record all 24 new cases in one optimized `+RTS -T` process. Profile the
      largest member of each family with serial RTS/stage/hotspot/heap commands,
      reducing a size rather than widening limits if the controlled machine
      cannot finish safely.
- [x] Record the matrix, compare growth factors and dominant profiles, then
      promote the smallest evidence-backed optimization child. Do not use
      single-machine thresholds.

### Complete generated scale-matrix receipt

The implementation landed in `49073c43`. The canonical optimized run used the
same Apple M1 Max / Darwin aarch64 host, GHC 9.14, one RTS capability, CPU time
mode, and `+RTS -T -RTS` from a clean tree. Its 32 timed leaves and metadata are
under
`benchmark-results/compiler-scale-matrix-baseline/20260811T033036335127000000Z/`.
Peak memory below is the optimized process high-water at that leaf; it is not a
portable limit or an isolated per-case residency value.

| Family / size                | Boundary           | Mean ms | Allocated bytes | Copied bytes | Peak memory bytes |
| ---------------------------- | ------------------ | ------: | --------------: | -----------: | ----------------: |
| recursive groups / 16        | analysis           |   2.894 |      10,305,680 |      494,169 |        63,963,136 |
| recursive groups / 32        | analysis           |  12.812 |      57,403,636 |    1,371,256 |        63,963,136 |
| recursive groups / 64        | analysis           |  77.296 |     401,231,121 |    3,314,538 |        63,963,136 |
| recursive groups / 128       | analysis           | 574.666 |   3,055,875,427 |    8,963,052 |        63,963,136 |
| recursive groups / 16        | module preparation |   7.445 |      29,355,218 |    3,002,519 |        63,963,136 |
| recursive groups / 32        | module preparation |  18.263 |      80,855,598 |    4,520,340 |        63,963,136 |
| recursive groups / 64        | module preparation |  85.532 |     433,536,163 |    8,314,713 |        63,963,136 |
| recursive groups / 128       | module preparation | 599.103 |   3,106,018,222 |   18,311,522 |        63,963,136 |
| constrained signatures / 32  | analysis           |   0.602 |       2,418,732 |       37,860 |        63,963,136 |
| constrained signatures / 64  | analysis           |   1.289 |       4,882,866 |      141,755 |        63,963,136 |
| constrained signatures / 128 | analysis           |   3.395 |      10,867,771 |      933,117 |        63,963,136 |
| constrained signatures / 256 | analysis           |   8.375 |      27,095,142 |    2,651,298 |        63,963,136 |
| nested lambdas / 16          | analysis           |   0.144 |         616,235 |        3,739 |        63,963,136 |
| nested lambdas / 32          | analysis           |   0.225 |         904,590 |        9,313 |        63,963,136 |
| nested lambdas / 64          | analysis           |   0.502 |       1,720,405 |       41,571 |        63,963,136 |
| nested lambdas / 128         | analysis           |   1.481 |       4,273,202 |      295,726 |        63,963,136 |
| nested lambdas / 16          | module preparation |   3.382 |      15,872,136 |    1,461,757 |        63,963,136 |
| nested lambdas / 32          | module preparation |   3.590 |      16,758,530 |    1,525,205 |        63,963,136 |
| nested lambdas / 64          | module preparation |   4.111 |      18,774,494 |    1,745,394 |        63,963,136 |
| nested lambdas / 128         | module preparation |   5.830 |      23,836,726 |    2,857,681 |        63,963,136 |
| operator table / 16          | parse/lower        |   0.140 |       1,130,163 |        8,330 |         6,291,456 |
| operator table / 32          | parse/lower        |   0.291 |       2,273,446 |       32,796 |         6,291,456 |
| operator table / 64          | parse/lower        |   0.641 |       4,603,133 |      132,215 |         7,340,032 |
| operator table / 128         | parse/lower        |   1.639 |       9,432,882 |      667,116 |         8,388,608 |
| nested blocks / 16           | parse/lower        |   0.103 |         876,635 |        4,621 |         8,388,608 |
| nested blocks / 32           | parse/lower        |   0.252 |       1,941,774 |       20,572 |         8,388,608 |
| nested blocks / 64           | parse/lower        |   0.752 |       4,760,308 |      109,391 |         8,388,608 |
| nested blocks / 128          | parse/lower        |   2.706 |      13,154,168 |      672,519 |         8,388,608 |
| token stream / 1,024         | parse/lower        |   0.840 |       6,706,406 |      286,220 |         8,388,608 |
| token stream / 4,096         | parse/lower        |   4.719 |      26,815,808 |    3,174,848 |         9,437,184 |
| token stream / 16,384        | parse/lower        |  20.903 |     107,253,149 |   17,856,651 |        22,020,096 |
| token stream / 65,536        | parse/lower        |  88.379 |     429,003,568 |   82,010,695 |        63,963,136 |

Serial stable-stage, hotspot, and live-heap artifacts for the largest member of
all six families are under `profile-results/compiler-scale-matrix-baseline/`.
The standalone receipts separate cumulative allocation from live residency:

| Family                     | Profiled boundary | Stable per-op allocation | Process allocation | Maximum residency | Heap sampled peak | Dominant hotspot evidence                                                          |
| -------------------------- | ----------------- | -----------------------: | -----------------: | ----------------: | ----------------: | ---------------------------------------------------------------------------------- |
| Recursive groups / 128     | analysis          |                   3.4 GB |     14,693,851,184 |         3,052,648 |         3,064,480 | `inferScopeTypeInternal`; repeated name ordering and recursive free-variable walks |
| Constrained signatures/256 | analysis          |                    39 MB |      5,373,753,624 |         3,218,504 |         2,196,680 | `freeTypeVariablesInEnv`, `freeTypeVariables`, and concrete impl fact projection   |
| Nested lambdas / 128       | analysis          |                   6.3 MB |      3,422,709,304 |         1,223,608 |         1,147,768 | `applySubstitution` dominates both ticks and allocation                            |
| Operator table / 128       | parse/lower       |                    14 MB |      7,789,820,168 |           982,216 |           678,344 | lexer/Megaparsec dominates; operator lookup is visible but secondary               |
| Nested blocks / 128        | parse/lower       |                    22 MB |      5,922,440,160 |           761,896 |           568,152 | repeated list `span` dominates, matching owned-prefix rescans                      |
| Token stream / 65,536      | parse/lower       |                   656 MB |      4,854,211,168 |        31,814,848 |        20,335,856 | lexer/Megaparsec plus list forcing; this is the strongest residency signal         |

The new evidence changes the first priority. Eight times as many interleaved
groups cost 198.6x CPU and 296.5x allocation in analysis. `Scope.hs` currently
walks the complete deduplicated recursive-group set for every let before it can
discover that almost every group is irrelevant; the promoted child indexes only
groups whose declaration interval spans that let. Constrained signatures still
confirm the environment-free-variable work, deep lambdas isolate substitution
rebuilding, nested blocks isolate prefix/list rescans, and the exact token
stream establishes the token ownership/residency baseline. Operator-table
lookup remains real but lower priority because its allocation is close to
linear and parsing is dominated by lexer/Megaparsec work.

## Task 3a: Index recursive-group scheme exposure

This pure internal representation change preserves recursive preview inference,
diagnostics, declaration order, and generalized schemes. It only prevents lets
from considering recursive groups that cannot possibly be visible there.

**Files:** `src/Jazz/Compiler/TypeInference/Scope.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/RecursionTests.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Keep the existing smallest generated recursive case and binding-signature
      recursion suite as semantic red/green ownership. They already cover exact
      output, interleaved polymorphic use, intervening dependencies, preview
      diagnostics, deferred constraints, and inferred-constraint uniqueness.
- [x] Precompute the canonical recursive groups once, preserving their current
      `Set` order, and index each non-member statement strictly between a
      group's first and last declarations to only the groups spanning it.
- [x] Make `exposeVisibleRecursiveGroupSchemes` consume that index instead of
      scanning every recursive group for every let. Keep every existing signed,
      dependency, feed-forward, diagnostic, rollback, and latest-binding guard.
- [x] Run the two focused semantic suites serially, then record the four-case
      optimized analysis curve with `+RTS -T` from a clean implementation
      commit. Capture standalone stable-stage, hotspot, and heap after evidence
      for the 128-group case.
- [x] Stop and diagnose any semantic or physical regression; do not widen
      limits. Record before/after CPU, allocation, copied bytes, and residency,
      close the child, and promote environment free-variable maintenance next.

### Recursive-group exposure receipt

The 30-line source change landed in `cb7b3426`. It builds a declaration-interval
index once, preserves the former canonical group order, and routes each
intervening let only to groups spanning that declaration. The complete
binding-signature and generated-stage semantic suites passed before and after
the change.

The compatible optimized after run is
`benchmark-results/compiler-scale-matrix-baseline/20260811T035003834196000000Z/`.
After removing run ID, Git revision, and timestamp, every environment metadata
field matches the before receipt at
`20260811T033036335127000000Z`. Git dirtiness is false in both runs.

| Groups | Boundary           | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement |
| -----: | ------------------ | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: |
|     16 | analysis           |     2.894 |    1.055 |            2.7x |        10,305,680 |        3,888,174 |                   2.7x |
|     32 | analysis           |    12.812 |    2.697 |            4.8x |        57,403,636 |        8,592,402 |                   6.7x |
|     64 | analysis           |    77.296 |    7.128 |           10.8x |       401,231,121 |       21,271,350 |                  18.9x |
|    128 | analysis           |   574.666 |   18.466 |           31.1x |     3,055,875,427 |       59,346,191 |                  51.5x |
|     16 | module preparation |     7.445 |    5.959 |            1.2x |        29,355,218 |       22,909,742 |                   1.3x |
|     32 | module preparation |    18.263 |    9.191 |            2.0x |        80,855,598 |       32,015,061 |                   2.5x |
|     64 | module preparation |    85.532 |   16.641 |            5.1x |       433,536,163 |       53,573,944 |                   8.1x |
|    128 | module preparation |   599.103 |   32.492 |           18.4x |     3,106,018,222 |      109,525,953 |                  28.4x |

Analysis growth from 16 to 128 groups fell from 198.6x CPU / 296.5x allocation
to 17.5x / 15.3x. Module-preparation growth fell from 80.5x / 105.8x to 5.5x /
4.8x. The standalone stable-stage profile reports about 80 MB per operation,
down from 3.4 GB. Whole-process allocation is iteration-dependent and therefore
is not used as a per-operation improvement claim. Maximum residency was
3,052,648 bytes before and 3,416,288 after; the separate heap sampled peaks were
3,064,480 and 3,066,696 bytes, so this CPU/allocation change did not materially
alter live residency. The after hotspot now leads with
`freeTypeVariablesInEnv` at 232 ticks / 691,732,480 allocated bytes, which
promotes the next child without relying on a wall-clock threshold.

## Task 3b: Maintain environment free-variable summaries

Repeated generalization currently walks every visible binding, traverses each
type or scheme, and resolves its free variables again. Carry a summary of raw
unquantified variable identities beside the main scope environment, with
reference counts so rebinding and recursive-group deletion remain exact. At a
generalization point, resolve only the summary's distinct variable identities
through the current solver.

**Files:** `src/Jazz/Compiler/TypeInference/Capabilities.hs`,
`src/Jazz/Compiler/TypeInference/Scope.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/GeneralizationTests.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Add semantic coverage before implementation for shadowing, shared
      monomorphic variables, recursive-group deletion, constrained schemes, and
      constructor bindings. Preserve exact generalized variables, constraint
      order, diagnostics, and runtime outputs.
- [x] Introduce an internal environment free-variable summary with per-binding
      raw variable sets and per-variable reference counts. Derive raw variables
      once from every `TypeBinding`; resolve the distinct live identities only
      when the solver state is required.
- [x] Thread the summary through the stable source-order scope environment.
      Update it on binding insertion/rebinding, data-constructor registration,
      and final recursive-group generalization. Retain the exact full-environment
      fallback while temporary recursive schemes are exposed; do not change
      expression-level environment semantics.
- [x] Reuse the same resolved summary for ordinary and explicit-signature
      generalization at a statement. Keep arbitrary capability checks on the
      existing environment path unless the summary is already in scope and
      semantic ownership is clear.
- [x] Run the focused semantic suites, commit the implementation, and record
      compatible optimized curves for sequential polymorphism and constrained
      signatures. Capture stable-stage, hotspot, and heap evidence for the
      largest case of each family before closing the child.

### Environment free-variable receipt

The implementation landed in `13553527`. `TypeEnvFreeVariables` stores the raw
unquantified variables owned by each binding plus an `IntMap` reference count,
so rebinding removes an identity only after its last owner disappears. Ordinary
source-order generalization resolves each distinct live identity once. Scope
segments with temporary recursive scheme exposure deliberately retain the
former full-environment calculation. The complete binding-signature and
generated-stage suites passed, including new shared-variable, shadowing, and
constructor characterization cases.

The optimized sequential after run is
`benchmark-results/compiler-scale-baseline/20260811T040658274927000000Z/`; its
before run is `20260811T025843036178000000Z`. The constrained-signature after
run is
`benchmark-results/compiler-scale-matrix-baseline/20260811T040817811856000000Z/`;
its before run is `20260811T035003834196000000Z`. After excluding run ID, Git
revision, and timestamp, all environment metadata agrees within each pair and
all four receipts report a clean tree.

| Case / boundary                | Size | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement |
| ------------------------------ | ---: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: |
| Sequential analysis            |   64 |     1.223 |    1.226 |            1.0x |         4,767,015 |        3,965,058 |                   1.2x |
| Sequential analysis            |  128 |     3.159 |    2.594 |            1.2x |        10,393,534 |        7,747,812 |                   1.3x |
| Sequential analysis            |  256 |     7.796 |    6.514 |            1.2x |        24,937,278 |       15,558,535 |                   1.6x |
| Sequential analysis            |  512 |    20.265 |   14.039 |            1.4x |        66,626,398 |       31,624,618 |                   2.1x |
| Sequential module preparation  |  512 |    31.122 |   24.847 |            1.3x |       118,413,538 |       83,386,649 |                   1.4x |
| Constrained-signature analysis |   32 |     0.586 |    0.529 |            1.1x |         2,415,325 |        2,122,134 |                   1.1x |
| Constrained-signature analysis |   64 |     1.177 |    0.991 |            1.2x |         4,877,631 |        3,985,070 |                   1.2x |
| Constrained-signature analysis |  128 |     3.159 |    2.034 |            1.6x |        10,857,620 |        7,584,186 |                   1.4x |
| Constrained-signature analysis |  256 |     8.402 |    4.698 |            1.8x |        27,043,758 |       15,103,504 |                   1.8x |

Sequential analysis growth from 64 to 512 bindings fell from 16.6x CPU / 14.0x
allocation to 11.4x / 8.0x. Constrained-signature growth from 32 to 256 fell
from 14.3x / 11.2x to 8.9x / 7.1x. The standalone artifacts are under
`profile-results/compiler-env-fv-after/`. Maximum process residency changed
from 3,762,104 to 4,986,320 bytes for sequential module preparation and from
3,218,504 to 3,201,680 bytes for constrained analysis. Separate heap sampled
peaks were flat at 3,819,600 before / 3,819,856 after and 2,196,680 before /
2,182,280 after. Whole-process allocation is iteration-dependent and is not a
per-operation claim.

The old hotspot's `freeTypeVariablesInEnv` entry (447,907,840 allocated bytes
for sequential and 1,111,228,416 for constrained signatures) no longer appears
among the leading after cost centres. The existing deep-lambda curve remains
the cleanest next type-checker signal: `applySubstitution` dominates its prior
hotspot profile, while 128-lambda analysis still allocates about 4.1 MB.

## Task 3c: Compress type substitutions during unification

`Solver.hs` currently stores substitutions in `Map Int ExpressionType` and
fully resolves both compound operands at every recursive unification call.
Variable chains are followed repeatedly without compression, and already
resolved child subtrees are reconstructed before the recursive child call
resolves them again.

**Files:** `src/Jazz/Compiler/TypeInference/Solver.hs`,
`src/Jazz/Compiler/TypeInference/State.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Add direct solver characterization for long substitution chains,
      compound substitutions, occurs checks, rigid variables, numeric
      constraints, and exact final substitution behavior before implementation.
- [x] Move the integer-keyed substitution store to `IntMap` without changing
      solver rollback, equality, or debugging behavior.
- [x] Add an internal head-dereference operation that path-compresses traversed
      variable chains in the returned `InferState`. Make recursive unification
      descend compound operands once, re-resolving only after an earlier sibling
      can have added a substitution.
- [x] Keep the public pure `resolveType`/`applySubstitution` boundary fully
      zonking results for diagnostics, schemes, and exported inference data.
- [x] Run the focused semantic suites, commit the implementation, and record a
      metadata-compatible deep-lambda curve plus stable-stage, hotspot, and
      live-heap evidence for 128 lambdas.

### Substitution compression receipt

The implementation landed in `e929073c`. Substitutions now use `IntMap`; an
internal state-returning dereference operation compresses variable chains, and
recursive unification inspects one outer constructor at a time instead of
fully rebuilding both compound operands before descending into their children.
The pure `resolveType` boundary still fully zonks diagnostics, schemes, and
exported types. Direct tests cover long chains, compound replacements, occurs
checks, rigid variables, numeric constraints, compression, and final resolved
types; the complete binding-signature and generated-stage suites pass.

The compatible before and after runs are
`benchmark-results/compiler-substitution/20260811T042040386734000000Z/` and
`benchmark-results/compiler-substitution/20260811T042553213129000000Z/`.
After excluding run ID, Git revision, and timestamp, their environment metadata
is identical and both receipts report a clean tree.

| Boundary           | Lambdas | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement |
| ------------------ | ------: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: |
| Analysis           |      16 |     0.159 |    0.153 |            1.0x |           640,996 |          616,792 |                   1.0x |
| Analysis           |      32 |     0.237 |    0.214 |            1.1x |           929,348 |          855,501 |                   1.1x |
| Analysis           |      64 |     0.493 |    0.394 |            1.3x |         1,745,163 |        1,500,769 |                   1.2x |
| Analysis           |     128 |     1.468 |    1.004 |            1.5x |         4,297,942 |        3,434,859 |                   1.3x |
| Module preparation |      16 |     3.326 |    3.284 |            1.0x |        15,879,571 |       15,769,589 |                   1.0x |
| Module preparation |     128 |     6.075 |    5.256 |            1.2x |        23,843,602 |       22,889,275 |                   1.0x |

Analysis growth from 16 to 128 lambdas fell from 9.3x CPU / 6.7x allocation to
6.6x / 5.6x. The standalone stable-stage operation fell from 6.3 MB to 5.1 MB.
Maximum process residency changed from 1,215,992 to 1,350,656 bytes, while the
separate heap sampled peak fell from 1,148,408 to 1,085,912 bytes. Process
allocation is iteration-dependent and is not used as a per-operation claim.

`applySubstitution` remains the largest late-cost-centre entry, but its share of
profile allocation fell from about 51% to 43%. A measured follow-up that only
replaced one application inspection with a head resolver changed the largest
case by one allocated byte, so it was reverted in `12401167` rather than kept as
unearned API surface. Residual final-zonking work remains an end-of-program
recheck after higher-signal constraint, scope, parser, and lifetime batches.

## Task 3d: Make constraint storage and deduplication append-efficient

Deferred constraints are stored oldest-first and extended with `old ++ new`.
Both deferred and newest-first inferred constraint deltas repeatedly recover
cursors with `length`, `take`, and `drop`. `dedupeTypeSchemeConstraints` uses
linear `elem` inside a fold while preserving last-occurrence order.

**Files:** `src/Jazz/Compiler/TypeInference/State.hs`,
`src/Jazz/Compiler/TypeInference/Capabilities.hs`,
`src/Jazz/Compiler/TypeInference/Scope.hs`,
`src/Jazz/Compiler/TypeInference/TypeOps.hs`,
`src/Jazz/Compiler/TypeInference.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Characterize exact deferred/inferred insertion order, statement rollback,
      captured-constraint pruning, duplicate last-occurrence order, and error
      ordering before changing representation.
- [x] Store deferred constraints in an append-efficient sequence and carry
      explicit deferred/inferred counts in `InferenceOutput`. Use stored counts
      for statement deltas and rollback cursors instead of rescanning list
      spines.
- [x] Replace ordered quadratic scheme-constraint deduplication with an
      `Ord`-backed seen set while retaining the current last-occurrence order.
- [x] Keep chronological public accessors and newest-first inferred storage
      unchanged; preserve preview transactions, failed-application rollback,
      captured pruning, scheme constraints, and diagnostics exactly.
- [x] Run the focused semantic suites, commit the implementation, and record a
      compatible constrained-signature curve plus stable-stage, hotspot, and
      live-heap evidence for 256 declarations.

### Constraint-buffer receipt

The representation change landed in `2c5e6b92`. Deferred constraints now use
an append-efficient `Seq`; `InferenceOutput` carries explicit deferred and
inferred counts for statement cursors and rollback; and stable-last scheme
constraint deduplication uses an `Ord`-backed seen set. Chronological deferred
accessors, newest-first inferred storage, preview rollback, failed-application
rollback, captured pruning, and diagnostic order remain unchanged. A direct
inference-output test fixes the two public orders and both cursor values. The
complete binding-signature and generated-stage suites pass.

The original constrained-signature declaration curve is intentionally reported
as neutral: at 256 declarations it changed from 4.910 ms / 14,918,935 bytes to
5.154 ms / 14,966,566 bytes. The CPU distributions overlap and allocation grew
by 0.3%, because that source finalizes one constraint at a time and does not
exercise a growing deferred buffer. Stable-stage allocation remained 23 MB per
operation. Maximum residency changed from 3,218,672 to 3,201,248 bytes, and the
separate sampled heap peak changed from 2,140,320 to 2,167,408 bytes. The
compatible artifacts are under `benchmark-results/compiler-constraints/` and
`profile-results/compiler-constraints-{before,after}/`.

The focused deferred-use burst added in `912734f9` instantiates one constrained
scheme repeatedly inside a single expression, so restoring `old ++ new` makes
the curve quadratic. The clean compatible receipts are
`benchmark-results/compiler-constraint-burst-before-clean/20260811T045148544001000000Z/`
and
`benchmark-results/compiler-constraint-burst-after/20260811T045224214240000000Z/`.
After excluding run identity, revision, and the intentionally distinct label,
their environment metadata is identical and both trees are clean.

| Uses | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement | Before copied | After copied |
| ---: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: | ------------: | -----------: |
|  128 |     0.406 |    0.352 |            1.2x |         1,768,345 |        1,341,274 |                   1.3x |        60,258 |       35,002 |
|  256 |     0.912 |    0.602 |            1.5x |         4,007,321 |        2,237,210 |                   1.8x |       391,522 |      110,878 |
|  512 |     4.803 |    1.187 |            4.0x |        11,246,039 |        4,036,842 |                   2.8x |     4,945,818 |      394,267 |
| 1024 |    18.532 |    3.487 |            5.3x |        37,627,755 |        7,751,943 |                   4.9x |    24,152,763 |    2,460,237 |

From 128 to 1,024 uses, CPU/allocation growth fell from 45.7x / 21.3x to
9.9x / 5.8x. Peak memory at 1,024 uses fell from 47 MB to 14 MB. This is the
earned signal for the representation change; the neutral declaration curve is
retained to show that the optimization did not manufacture a gain outside its
target workload.

## Task 3e: Reuse recursive-group previews

At an intervening let, `exposeVisibleRecursiveGroupSchemes` transactionally
infers every later member of each spanning recursive group, discards its output,
and later repeats the same future bodies at subsequent intervening lets and in
the real source-order traversal. The interval index removed unrelated-group
scans, but it deliberately did not remove this duplicate body inference.

**Files:** `src/Jazz/Compiler/TypeInference/Scope.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/RecursionTests.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Characterize the exact environment facts, solver watermark, output
      rollback, diagnostics, signatures, rebindings, and dependency guards that
      make two recursive previews reusable or distinct.
- [x] Add a focused probe that counts future recursive-body inference across
      multiple safe intervening lets and demonstrates the old repeated-work
      growth without asserting wall-clock time.
- [x] Cache an immutable preview product at the narrowest semantically valid
      scope. Reuse it only when referenced bindings, capability facts, pending
      signatures, and solver identities are equivalent; keep the existing path
      for genuinely distinct environments.
- [x] Preserve the allocation watermark and discard all speculative diagnostics,
      runtime hints, deferred constraints, inferred constraints, and production
      artifacts exactly as today.
- [x] Run the complete binding-signature and generated-stage suites, then record
      a compatible interleaved-recursive-group curve plus stable-stage, hotspot,
      and live-heap evidence for the largest case.

### Recursive-preview reuse receipt

The implementation landed in `d74541d1`; the isolating generated fixture landed
in `0ac59993`. A narrow source-order cache keys preview products by recursive
group frontier and processed member, retains only generalized bindings plus the
next type-variable watermark, and is cleared across every statement kind that
can change the relevant environment. Advancing a real group member drops that
group's cached previews immediately. Actual source-order inference still owns
all diagnostics, output, constraints, runtime hints, and production artifacts.

The deterministic ownership probe inferred 13 bodies before the change and 11
after it: five binding seeds, five actual source bodies, and one reusable
preview. The complete binding-signature and generated-stage suites pass, fixing
diagnostic order, signatures, rebindings, deferred/inferred constraint order,
type-variable identity, exact runtime output `(1, True)`, and benchmark registry
ownership without a physical threshold.

The compatible optimized receipts are
`benchmark-results/compiler-recursive-preview-before/20260811T050916785367000000Z/`
and
`benchmark-results/compiler-recursive-preview-after/20260811T050953880070000000Z/`.
After excluding run identity, revision, timestamp, and the intentional label,
their environment metadata is identical and both source trees were clean.

| Groups | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement | Before copied | After copied |
| -----: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: | ------------: | -----------: |
|     16 |     2.056 |    1.831 |            1.1x |         6,126,500 |        5,710,508 |                   1.1x |       298,111 |      265,372 |
|     32 |     5.749 |    5.365 |            1.1x |        15,178,835 |       14,426,541 |                   1.1x |     1,605,148 |    1,589,327 |
|     64 |    14.824 |   13.643 |            1.1x |        42,740,438 |       41,324,377 |                   1.0x |     4,703,825 |    4,378,728 |
|    128 |    40.647 |   36.961 |            1.1x |       135,229,333 |      132,779,408 |                   1.0x |    11,930,251 |   11,573,524 |

The optimized high-water at 128 groups fell from 18 MB to 17 MB. In the
standalone stable-stage profile, elapsed time fell from 67.2 ms to 61.8 ms and
process allocation from 5,875,739,408 to 5,706,648,888 bytes. Its sampled
maximum residency moved from 4,769,264 to 5,783,552 bytes, while the independent
cost-centre heap census was effectively flat at 3,984,872 versus 3,985,160
bytes. Both results are retained: the cache earns a repeatable CPU/allocation
gain and bounded lifetime, but this batch does not claim a standalone residency
win. Stable-stage, eventlog, hotspot, and heap artifacts are under
`profile-results/compiler-recursive-preview-{before,after}/`.

## Task 3: Remove type-checker asymptotic work

- [x] Replace append/length delta tracking with append-efficient buffers and
      explicit cursors while preserving constraint order.
- [x] Infer each recursive body once per necessary environment state and cache
      reusable group results.
- [x] Maintain environment free-variable summaries or levels instead of
      rescanning the complete visible environment per generalization.
- [x] Replace repeated substitution-chain resolution with an `IntMap`-backed
      zonk/compression boundary and avoid re-resolving child subtrees during
      recursive unification.
- [x] Replace ordered linear constraint membership with stable-identity sets
      while emitting constraints in original order.

## Task 4: Reuse recursive scope and lambda capture facts

- [x] Build declaration visibility, same-name indices, dependencies, and SCCs
      in one pass with append-efficient builders.
- [x] Transport reusable immutable scope facts to resolution, inference,
      analyzer, free-variable, and runtime-scope consumers only where their
      semantics agree.
- [ ] Assign stable lambda IDs during owned lowering, compute capture plans
      once, and stop retaining/searching lambda `Expr` bodies as keys.

### Task 4a: Build recursive dependency facts in one pass

`inferRecursiveGroupsOrdered` currently builds each same-name declaration list
with `old ++ new` and reconstructs the complete visible-before name set by
scanning every declaration for every declaration. This is independent of the
already-fixed recursive-preview interval scan and remains on every resolver,
inference, analyzer, nested free-variable, and runtime-scope analysis path.

**Files:** `src/Jazz/Compiler/RecursiveBindings.hs`,
`test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`, and
`test/Jazz/Benchmark/StageSpec.hs`.

- [x] Preserve the existing generated interleaved-recursive curve as the full
      compiler baseline and add a direct deterministic fact-builder probe only
      if the existing corpus does not isolate same-name/visibility growth.
- [x] Build ascending same-name declaration indices with prepend-and-finalize
      or another append-efficient representation.
- [x] Thread the visible-before set through the source-order dependency fold so
      each declaration extends it once instead of rescanning all declarations.
- [x] Preserve nearest-prior, outer-shadowing, same-name self-cell, first-future,
      nested-scope, SCC, and source-order contracts exactly.
- [x] Run the recursive-binding and generated-stage suites, capture compatible
      before/after optimized plus stable-stage/hotspot/heap evidence, commit the
      batch, and then promote immutable fact transport.

#### Recursive dependency builder receipt

The focused generated same-name fixture landed in `be27c6f5`, and the 13-line
source-order fact-builder change landed in `e130feb5`. Same-name indices now
prepend in constant time and reverse once; the dependency fold carries the
visible-before name set forward once per declaration. The complete recursive
binding and generated-stage suites pass, preserving exact rebinding output,
outer shadowing, self-cell ownership, nested scope behavior, SCC membership,
and declaration order.

The compatible same-name receipts are
`benchmark-results/compiler-recursive-rebindings-before/compiler-recursive-rebindings-before/20260811T052510169170000000Z/`
and
`benchmark-results/compiler-recursive-rebindings-after/compiler-recursive-rebindings-after/20260811T052956208547000000Z/`.
After excluding run identity, revision, timestamp, and the intentional label,
their environment metadata is identical and both source trees were clean.

| Bindings | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement | Before copied | After copied |
| -------: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: | ------------: | -----------: |
|      128 |     1.160 |    0.970 |            1.2x |         5,312,580 |        4,420,694 |                   1.2x |       414,975 |      296,245 |
|      256 |     4.190 |    3.625 |            1.2x |        17,485,257 |       13,866,473 |                   1.3x |     2,475,124 |    2,090,238 |
|      512 |    12.632 |   10.974 |            1.2x |        62,409,024 |       47,831,117 |                   1.3x |     6,594,014 |    5,872,015 |
|    1,024 |    37.804 |   33.687 |            1.1x |       238,720,243 |      175,790,702 |                   1.4x |    11,604,551 |   11,126,921 |

At 1,024 bindings, the optimized high-water fell from 21 MB to 20 MB. In the
standalone stable-stage profile, elapsed time fell from 74.1 ms to 59.0 ms,
process allocation from 15,277,971,656 to 10,685,195,952 bytes, maximum
residency from 5,828,104 to 5,733,448 bytes, and the independent heap census
from 4,055,232 to 4,029,360 bytes. The pre-change hotspot attributed
1,910,204,336 bytes to list append; that cost centre disappears from the
post-change leaders. Artifacts are under
`profile-results/compiler-recursive-facts-{before,after}/`.

The compatible interleaved-group curve is intentionally reported as neutral:
at 128 groups it changed from 19.699 ms / 55,495,528 bytes to 19.762 ms /
55,577,449 bytes. This confirms that the win belongs to repeated same-name and
visible-prefix construction rather than the already-optimized preview path.

### Task 4b: Transport immutable recursive scope facts

Module resolution, type inference, the analyzer, nested free-variable walks,
and runtime scope planning still rebuild the same declaration names,
dependencies, and SCC projection independently. Outer visibility affects
forward-edge suppression, so facts may be shared only when the consumer's
projected outer-name set is equivalent; a global AST-keyed memo would retain
trees and is not acceptable.

**Files:** `src/Jazz/Compiler/RecursiveBindings.hs`,
`src/Jazz/Compiler/ModuleResolver.hs`,
`src/Jazz/Compiler/TypeInference/Scope.hs`, `src/Jazz/Compiler/Analyzer.hs`,
`src/Jazz/Compiler/Runtime/ScopePlan.hs`, and focused semantic/benchmark tests.

- [x] Instrument a deterministic pipeline probe that counts recursive fact
      construction per owned scope without asserting elapsed time.
- [x] Separate outer-independent declaration/free-name indices from the small
      outer-visibility projection that resolves local dependency edges.
- [x] Define an opaque immutable scope-fact product with exact statement-order
      ownership and no retained parent AST beyond the statements already owned
      by the active artifact.
- [x] Thread facts only through resolver/inference/analyzer/runtime boundaries
      whose projected outer visibility matches; preserve existing standalone
      entry points by building and validating facts locally.
- [x] Preserve diagnostics, rebinding selection, nested scope behavior, binder
      identity, runtime scope plans, and exact artifacts; capture before/after
      curves and residency before promoting lambda capture plans.

#### Recursive scope-fact transport receipt

The opaque fact product and top-level inference/analyzer reuse landed in
`33b45c3a`; resolver, nested free-variable, analyzer fallback, and runtime scope
planning were routed through the same product in `51f78abf`. The product owns
only binding names and integer group indices, not an AST key. Type inference and
the analyzer share one exact product because their imported-value and builtin
outer projections are identical. Resolver and runtime projections can differ,
so their standalone boundaries deliberately construct local products instead
of reusing an unsafe global cache.

The deterministic product test fixes binding-name and ordered-SCC ownership in
one value; the existing complete recursive-binding, binding-signature, and
generated-stage suites fix rebinding selection, nested scopes, diagnostics,
binder identity, runtime output, and standalone fallbacks. The compatible
optimized receipts are
`benchmark-results/compiler-recursive-rebindings-after/compiler-recursive-rebindings-after/20260811T052956208547000000Z/`
and
`benchmark-results/compiler-scope-facts-after/compiler-scope-facts-after/20260811T054234433661000000Z/`.
After excluding run identity, revision, timestamp, and the intentional label,
their environment metadata is identical and both source trees were clean.

| Bindings | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement | Before copied | After copied |
| -------: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: | ------------: | -----------: |
|      128 |     0.970 |    0.699 |            1.4x |         4,420,694 |        2,933,113 |                   1.5x |       296,245 |      156,018 |
|      256 |     3.625 |    2.748 |            1.3x |        13,866,473 |        8,389,785 |                   1.7x |     2,090,238 |    1,539,850 |
|      512 |    10.974 |    7.300 |            1.5x |        47,831,117 |       26,860,014 |                   1.8x |     5,872,015 |    3,889,136 |
|    1,024 |    33.687 |   21.867 |            1.5x |       175,790,702 |       93,955,245 |                   1.9x |    11,126,921 |    9,243,520 |

At 1,024 bindings, the optimized high-water fell from 20 MB to 18 MB. In the
standalone stable-stage profile, per-operation allocation fell from 310 MB to
171 MB and elapsed time from 59.0 ms to 40.1 ms. Exact maximum residency fell
from 5,733,448 to 5,273,056 bytes, and the independent heap census fell from
4,029,360 to 3,764,408 bytes. The total stable-profile process allocation is not
compared because the faster after run completed far more adaptive benchmark
iterations; the per-operation allocation counters are the compatible measure.
Artifacts are under `profile-results/compiler-recursive-facts-after/` and
`profile-results/compiler-scope-facts-after/`.

### Task 4c: Replace Expr-keyed lambda capture hints

Before `a94e825f`, `LambdaCaptureHints` stored each lambda's full body, recursively walked
nested bodies to build capture sets, linearly searches sibling hints, and uses
structural `Expr` equality to locate a runtime lambda. This retains duplicated
AST subtrees in closures and makes deep nesting increasingly expensive.

This is an internal representation change, not a public language or artifact
contract. The implementation must keep stable identity out of rendered/public
artifacts and preserve the existing `Expr` constructors where an ID can instead
live in owned lowering/runtime metadata. If exact lookup cannot be achieved
without changing a durable AST schema, stop implementation and write the
required design/RFC before changing that schema.

**Files:** `src/Jazz/Compiler/RecursiveBindings.hs`,
`src/Jazz/Compiler/Runtime.hs`, and focused lambda/runtime/benchmark tests. The
equivalent owned traversal key kept `Expr`, parser lowering, runtime value
rendering, and serialized artifacts unchanged.

- [x] Add whole-program deep-lambda scale ownership and record a clean runtime
      before curve plus stable-stage, hotspot, and heap evidence.
- [x] Characterize every AST construction path, runtime evaluation re-entry,
      closure serialization/rendering boundary, and exact-artifact test that
      constrains lambda identity.
- [x] Introduce stable internal lambda IDs or an equivalent owned traversal key
      without changing public syntax, diagnostics, binder identity, or rendered
      artifact schemas.
- [x] Precompute each capture set once, store plans without `Expr` bodies, and
      use indexed lookup rather than sibling scans/structural equality.
- [x] Preserve nested/curry capture boundaries, rebinding snapshots, recursion,
      host-cell reachability, hosted parity, and closure observation metrics.
- [x] Run focused lambda/runtime and generated-stage suites, capture comparable
      after evidence, and then promote module indexing/lifetime work.

The generated whole-program fixture landed in `427394c4`. The implementation
in `a94e825f` replaces body-retaining list hints with a sparse `IntMap` tree
whose keys are evaluator child positions. Capture sets and nested plans are
computed together in one bottom-up pass. Evaluator continuation frames carry
the exact child plan, and closures retain only the plan for their body. No AST
constructor, lowering result, binder identity, diagnostic, runtime rendering,
or serialized artifact changed. The focused lambda, recursive-binding, runtime,
and generated-stage suites all pass with `--jobs=1`.

The clean optimized curves are compatible after excluding run identity,
revision, timestamp, and the intentional environment label:

| Lambdas | Before ms | After ms | CPU improvement | Before allocation | After allocation | Allocation improvement | Before peak | After peak |
| ------: | --------: | -------: | --------------: | ----------------: | ---------------: | ---------------------: | ----------: | ---------: |
|      16 |     4.048 |    3.962 |            1.0x |        17,170,901 |       17,146,754 |                   1.0x |        7 MB |       7 MB |
|      32 |     4.237 |    4.126 |            1.0x |        18,166,041 |       18,040,897 |                   1.0x |        8 MB |       8 MB |
|      64 |     5.013 |    4.623 |            1.1x |        20,613,340 |       20,002,765 |                   1.0x |        8 MB |       8 MB |
|     128 |     7.567 |    5.785 |            1.3x |        27,571,679 |       24,675,789 |                   1.1x |        9 MB |       8 MB |

At 128 lambdas, elapsed CPU fell 23.5%, allocation fell 10.5%, copied bytes
fell 13.7%, and high-water fell 1 MB. The stable profiled stage fell from
16.1 ms / 41 MB allocated / 10 MB peak to 13.9 ms / 37 MB / 9 MB. Its exact
RTS maximum residency fell from 1,543,520 to 1,023,544 bytes; the independent
cost-centre heap census moved slightly upward from 1,148,880 to 1,203,968
bytes, so the receipt does not claim a universal live-heap reduction. Artifacts
are under
`benchmark-results/compiler-lambda-captures-{before,after}/` and
`profile-results/compiler-lambda-captures-{before,after}/`.

## Task 5: Index interfaces and compact compiled lifetime

### Task 5a: Index module dependencies

- [x] Record the clean `wide-module-fanout` module-preparation curve and
      stable-stage/hotspot/heap evidence before changing lookup representation.
- [x] Add a focused exact-order/duplicate-path regression for the dependency
      index boundary.
- [x] Carry an incrementally maintained `Map [Text] CompiledModule` beside the
      source-order module list and route every import lookup through it.
- [x] Preserve compiled module order, first-match behavior, diagnostics,
      interfaces, runtime parity, and all exact artifacts.
- [x] Run the module pipeline and generated-stage suites, capture compatible
      after evidence, and promote interface rebasing/caching.

The path-index implementation landed in `5ccd7564`; `a0ef3fe4` added a second
fanout family with one export per dependency to isolate lookup growth from
interface rebasing. `compileResolvedProgram` now carries a strict `Map [Text]
CompiledModule` beside the reversed source-order list. The public single-module
entry point constructs a first-wins index, preserving the historical contract
when a caller supplies duplicate paths. The focused module-pipeline and
generated-stage suites pass with `--jobs=1`.

The original width-16 curve remained dominated by interface rebasing and did
not show a physical win. On the isolating width-1 curve, the 512-module case
fell from 37.84 ms to 34.63 ms (8.5% CPU); allocation rose 0.28% and benchmark
peak stayed at 33 MB, so this receipt claims only lookup CPU improvement. The
stable profile fell from 77.1 ms / 174 MB allocated / 48 MB peak to 68.7 ms /
165 MB / 47 MB. Exact RTS total allocation fell from 5,679,515,472 to
5,389,995,368 bytes and maximum residency from 15,040,000 to 14,921,944 bytes;
the independent heap census moved slightly upward from 10,128,048 to
10,153,952 bytes. Artifacts are under
`benchmark-results/compiler-module-lookup-{before,after}/` and
`profile-results/compiler-module-lookup-{before,after}/`.

### Task 5b: Cache imported interfaces

- [x] Record a clean indexed `wide-module-fanout` width-16 curve plus
      stable-stage/hotspot/heap evidence before changing interface ownership.
- [x] Canonicalize interface names and cache ambient prelude/dependency rebases
      so module compilation does not deep-copy unchanged schemes/declarations.
- [x] Preserve selective and aliased imports, export inventories, diagnostics,
      source order, binder identity, hosted parity, and exact artifacts.
- [x] Run focused module-pipeline and generated-stage suites, capture compatible
      after evidence, and promote compiled-artifact lifetime reduction.

The shared-interface fixture landed in `f1686154`, and the cache implementation
landed in `6f68bc85`. Program compilation now creates the ambient-prelude import
once and stores a lazy canonical whole-import view beside each transient
dependency-index entry. Unqualified whole imports share that view; selective or
aliased imports retain the existing filtered rebase path. The cache is discarded
with compilation and does not change `CompiledModule` or serialized artifacts.

At 128 dependents importing the same 16-export interface, CPU fell from
13.90 ms to 9.51 ms (31.6%), allocation from 46,013,414 to 40,741,222 bytes
(11.5%), copied bytes from 8,096,367 to 3,992,719 (50.7%), and peak from 16 MB
to 10 MB (37.5%). On the original 64 x 16 graph, CPU fell from 19.03 ms to
17.44 ms, allocation from 65,862,020 to 63,681,229 bytes, copied bytes from
10,986,302 to 9,538,352, and peak from 17 MB to 16 MB. The stable profile's
exact maximum residency fell from 4,557,200 to 4,442,784 bytes, and the
independent heap census fell from 3,423,768 to 3,225,344 bytes. Artifacts are
under `benchmark-results/compiler-{interface-rebase,shared-interface}-{before,after}/`
and `profile-results/compiler-interface-rebase-{before,after}/`.

### Task 5c: Compact compiled lifetime

- [x] Split runtime/debug metadata from full resolved/compiled AST retention and
      remove the aggregate diagnostic spine where per-module order can be consumed
      directly.

The compact record landed in `52491d8a`. `CompiledModule` now retains only its
path, import descriptors, public export inventory, inferred interface,
diagnostics, and executable inferred expression. It no longer retains the
source path, declared header metadata, or the complete pre-inference Core AST
through `ResolvedModule`. `CompiledProgram` computes the canonical prelude-then-
module diagnostic stream on demand instead of owning a second list spine. The
analysis benchmark resolves its entry module separately during untimed setup,
so production ownership is not distorted for benchmarking convenience.

The width-16 curve remained physically flat, as expected: peak compilation
still includes the resolved tree while producing the compact result. At 64
modules, allocation changed from 63,681,229 to 63,681,742 bytes and peak stayed
at 16 MB. Stable exact maximum residency moved from 4,442,784 to 4,434,216
bytes, and the independent heap census from 3,225,344 to 3,219,688 bytes.
The receipt therefore claims a durable post-compilation ownership reduction,
not a compile-time CPU or peak-memory win. Artifacts are under
`benchmark-results/compiler-artifact-lifetime-after/` and
`profile-results/compiler-artifact-lifetime-after/`; the compatible before
receipt is the interface-cache after artifact above.

### Task 5d: Finalize ordinary inference before analysis

- [x] Capture a fresh sequential-polymorphic module-preparation curve plus
      stable-stage/hotspot/heap evidence showing final inference state live
      across the analyzer boundary.
- [x] Compact and container-force only the diagnostics, runtime hints, and
      module interface required by ordinary inference before invoking analysis.
- [x] Keep the stateful Typed Core producer path unchanged and preserve exact
      analyzer-first diagnostic ordering, inferred interfaces, runtime hints,
      and artifacts.
- [x] Run binding-signature, generated-stage, typed-core, and profiling-focused
      suites, capture compatible after evidence, and promote parser pass work.

The ordinary/stateful split landed in `2cd263e6`. Ordinary inference now
materializes only the type-diagnostic spine and module-interface/runtime-hint
container entries before tail-calling the analyzer helper, so the rest of
`InferState` can die before the analyzer walks the same AST. Values remain lazy;
this is not a blanket phase force. Typed Core production uses the same inference
work but deliberately retains the complete state through its existing
finalizer. Binding-signature, generated-stage, and exact Typed Core suites pass.

At 512 sequential bindings, CPU fell from 23.13 ms to 22.23 ms (3.9%), copied
bytes from 12,874,801 to 12,302,236 (4.4%), and allocation stayed flat at about
81.02 MB; benchmark peak stayed 14 MB. The stable profiled run's exact maximum
residency fell from 4,786,368 to 3,952,096 bytes (17.4%), and the independent
heap census fell from 3,652,424 to 2,845,344 bytes (22.1%). Stable per-operation
peak also fell from 17 MB to 16 MB. Artifacts are under
`benchmark-results/compiler-inference-lifetime-{before,after}/` and
`profile-results/compiler-inference-lifetime-{before,after}/`.

## Task 6: Reduce parser/resolver passes and checked-boundary cleanup

### Task 6a: Preindex operator scope

- [x] Record the large-operator-table parse/lower curve and
      stable-stage/hotspot/heap evidence before changing parser context.
- [x] Replace repeated `declaredOperators <> builtinOperatorInfos` allocation
      and scans with one incrementally updated, scope-local symbol index.
- [x] Preserve declaration-before-use visibility, duplicate/reserved checks,
      custom precedence and associativity, diagnostics, exact surface/Core ASTs,
      module isolation, and hosted parity.
- [x] Run expression/operator parser and generated-stage suites, capture
      comparable after evidence, and promote import-alias scan removal.

The indexed operator table landed in `75408fc1`. Parser context now carries one
scope-local `Map Text OperatorInfo` plus a declared-symbol `Set`, so builtin and
user fixity lookup is logarithmic and no use site constructs or scans a
declared-plus-builtin list. Incremental insertion preserves declaration-before-
use behavior, while the separate set preserves binding/signature eligibility
and duplicate diagnostics. Expression, fixity, invalid-syntax, section, and
generated-stage suites pass.

At 128 declarations, CPU fell from 1.550 ms to 1.479 ms (4.6%), allocation from
9,432,895 to 9,083,236 bytes (3.7%), and copied bytes from 620,284 to 580,884
(6.4%); the 7 MiB benchmark peak was unchanged. The stable profiled process
allocated 7,789,845,096 versus 7,497,572,296 bytes (3.8% less), copied 3.4% less,
and maximum residency moved from 982,344 to 970,928 bytes. The independent heap
census sampled 678,040 versus 687,216 bytes, a small non-win that is reported
rather than hidden. Artifacts are under
`benchmark-results/compiler-operator-table-{before,after}/` and
`profile-results/compiler-operator-table-{before,after}/`.

### Task 6b: Remove parser and resolver repeat passes

- [x] Collect legal import aliases in
      the main module parse rather than rescanning nested token tails.
- [x] Reuse one compact-signature parse for discrimination and output.
- [ ] Replace the owned-prefix/list adapter with one state-correct cursor as
      part of indexed token storage; do not retain earlier input in Megaparsec
      position state.
- [ ] Move tokens to indexed storage with source offsets/spans and own `Text`
      only where later semantics require it.
- [ ] Fuse lowering and module-fact collection into one `SurfaceModuleFacts`
      traversal or an equivalent returned lowering product.
- [ ] Introduce an opaque validated Typed Program handoff so trusted
      producer-to-lowerer transport validates once, while external artifacts remain
      checked.
- [ ] Finish reverse-builder/ordered-set cleanup for analyzer diagnostics.

The invalid-scope scan removal landed in `6b3dbfac`. Top-level programs and
module bodies retain one complete legal-scope prepass because lowercase aliases
remain visible before their import declaration. Nested blocks inherit that
alias set but no longer walk their remaining token tails: imports are rejected
there. A new exact-AST regression pins a nested qualified reference whose alias
is imported later in the enclosing legal scope; declaration, module-import, and
generated-stage suites pass.

At 128 nested blocks, CPU fell from 2.735 ms to 2.666 ms (2.5%), allocation from
13,154,137 to 13,142,836 bytes, and copied bytes from 625,859 to 618,755; the
7 MiB benchmark peak was unchanged. The stable profiled process allocated
5,922,502,272 versus 5,917,625,784 bytes, maximum residency fell from 761,896
to 751,968 bytes (1.3%), and the independent heap census fell from 570,120 to
563,800 bytes (1.1%). Artifacts are under
`benchmark-results/compiler-import-alias-scan-{before,after}/` and
`profile-results/compiler-import-alias-scan-{before,after}/`.

Compact-signature parse reuse landed in `97da3c48`, after `df61c708` added the
parse/lower boundary to the existing constrained-signature scale family. The
alias-versus-signature discriminator now returns the already parsed signature
instead of parsing the same token prefix again. Exact compact/non-compact,
constructor/lowercase, alias-shadowing, diagnostic-span, and generated-stage
tests pass. At 256 signatures, CPU fell from 7.852 ms to 7.700 ms (1.9%);
allocation, copied bytes, the 10 MiB benchmark peak, and exact 3.15 MB stable
maximum residency were flat. The independent heap census moved from 2,420,464
to 2,463,824 bytes, so no residency win is claimed. Artifacts are under
`benchmark-results/compiler-compact-signature-{before,after}/` and
`profile-results/compiler-compact-signature-{before,after}/`.

A direct list-remainder `setInput` experiment was deliberately rejected. It
cut long-stream CPU and allocation, but Megaparsec's parser/position state kept
earlier input live: stable maximum residency rose from 31,834,880 to 37,068,168
bytes (16.4%) and the heap census rose about 18%. The ignored exploratory
artifacts remain under `compiler-owned-prefix-cursor-*`. The cursor removal is
therefore coupled to indexed token storage rather than masking the regression.

## Full closeout

After the final source change, run exactly one complete ordinary closeout, one
extended/profile evidence closeout, and the mandatory release gate with bounded
jobs. Compare all eight generated scenarios to their compatible baselines,
retain semantic and exact-artifact results, summarize timing/allocation/maximum
residency without universal thresholds, and update `PERFORMANCE.md` only for
durable workflow changes.
