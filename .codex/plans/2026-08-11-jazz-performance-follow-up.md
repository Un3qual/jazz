---
id: JN-COMPILER-PERFORMANCE-FOLLOW-UP-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 1"
target_paths:
  - src/Jazz/Compiler/Force.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
  - test/Jazz/Compiler/ProfilingSpec.hs
  - test/Jazz/Benchmark/StageSpec.hs
verification:
  - JAZZ_MAIN_PHASE=compiler bash scripts/ci/main-functional.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Correct benchmark ownership, remove evidence-backed asymptotic/allocation paths, and publish comparable receipts without semantic changes."
last_verified: 2026-08-11
---

# Jazz Compiler Performance Follow-up

> **For agentic workers:** use test-driven development for each task. Subagents
> may inspect or edit independent files, but only the coordinator may run Cabal,
> Jazz, benchmark, profiling, or Nix commands, exactly one process at a time.

**Goal:** Measure and remove the remaining evidence-backed CPU, allocation, and
peak-residency problems discovered after the first compiler performance program.

**Architecture:** Preserve RFC 0008's split between deterministic correctness
and advisory physical evidence. Each optimization begins with a generated or
direct-artifact probe that exposes its growth mechanism, preserves exact public
behavior, and records a compatible before curve. Representation changes remain
local unless the probe proves a cross-phase cache is justified.

**Tech stack:** Haskell/GHC 9.14.1, Cabal, Tasty, tasty-bench, GHC RTS profiling,
Bash, Python 3, and Nix.

## Global constraints

- Work on `codex/compiler-performance-program`; commit every reviewable batch
  and push the existing pull-request branch after the closeout gate.
- Do not change public language semantics, diagnostics or ordering, binder
  identity, hosted parity, Typed Core or Lowered IR artifacts, or schemas.
- Exactly one heavyweight command at a time. Every Cabal command uses
  `--jobs=1`. Never replace a quiet command with a duplicate.
- Write semantic or exact-artifact tests first and observe the intended failure
  before editing production code.
- Physical timing, cumulative allocation, copied bytes, and maximum residency
  are recorded evidence, not deterministic assertions.
- Preserve source order wherever it affects diagnostics, candidate selection,
  exports, imports, evaluation, rendering, or ambiguity behavior.
- Do not add persistent caches or strictness without measured live-residency
  evidence. Prefer prepared indexes and reverse builders with bounded lifetime.
- Run focused tests during TDD, one affected compiler gate per batch, and
  exactly one full main closeout after the final source change.

## Prioritized inventory

| Order | Candidate | Growth mechanism | Disposition |
| --- | --- | --- | --- |
| 1 | Benchmark analysis forcing | lazy `ResolvedModule` fields cross the setup/timed boundary | correct before ranking analysis work |
| 2 | Evaluator continuation tracking | `length` of continuations on every transition plus disabled-observation state traffic | implement after runtime scale fixture |
| 3 | Runtime import preparation | selected inventories/class sets rebuilt for every export | implement after width fixture |
| 4 | Lowered IR temporary lookup | every operand scans the owning block instructions | implement after direct-artifact fixture |
| 5 | Lexer first-character dispatch | every token attempts several failed Megaparsec alternatives | implement against existing long-token curve |
| 6 | Typed Core recursive validation | all-pairs reachability and repeated same-name history scans | replace with ordered SCC facts after direct-artifact fixture |
| 7 | Wide constructors | append-built field and captured-argument lists plus repeated arity scans | implement after wide-arity fixture |
| 8 | Capability facts and candidates | rendered-text parsing, whole-catalog filtering, and append-built ordered candidates | fast-path empty facts, then structured/indexed storage only if scale evidence supports it |
| 9 | Host-free module evaluation | whole-program proof is discarded and scopes re-enter host machinery | profile-gated trusted pure path |
| 10 | Direct Typed Core suffix/export facts | per-function future-map scans and per-export function scans | generated producer fixture first |
| 11 | Parser/lowering pass reductions | case-arm suffix reparsing, post-lowering span rewrite, signature list/vector bounce | separate probes; implement only material curves |
| 12 | Recursive preview/scope-plan retention | repeated preview frontiers and runtime plan construction/retention | heap/counter evidence required before representation change |

Previously rejected list-backed token cursors, boxed token payloads, blanket
forcing, general persistent module indexes, broad resolver-fact fusion, and
substitution micro-tuning remain out of scope unless new evidence contradicts
their recorded regressions or neutral results.

## Batch gate ladder

1. Add the smallest deterministic fixture that owns semantic/artifact parity.
2. Run it against the old implementation and observe the missing invariant,
   probe count, or mutation failure required by TDD.
3. Record four-size CPU/allocation/residency baselines where physical behavior
   is the reason for the change.
4. Implement one local representation or traversal change.
5. Run the focused test and the affected subsystem gate serially.
6. Record compatible after results plus an RTS `-s` receipt and, for lifetime
   changes, a heap/stage profile.
7. Stop and diagnose regressions; never widen semantic budgets or timeouts.
8. Commit the batch and update this plan with exact evidence.

## Task 1: Correct analysis benchmark ownership

**Files:** `src/Jazz/Compiler/Force.hs`,
`benchmark/Jazz/Benchmark/StageInputs.hs`,
`test/Jazz/Compiler/ProfilingSpec.hs`, `test/Jazz/Benchmark/StageSpec.hs`.

- [x] Add a characterization proving prepared analysis forces every
      `ResolvedModule` field owned by setup while preserving intentionally lazy
      production behavior.
- [x] Add `forceResolvedModule` beside existing benchmark/profile forcing
      helpers and use it only in prepared benchmark `NFData` instances.
- [x] Run profiling/stage tests and re-record analysis baselines before using
      that stage to rank subsequent tasks.
- [x] Commit the measurement-boundary correction independently.

Task 1 landed in `0d88a34c`. Both benchmark variants now poison every nested
resolved-module field through their real `NFData` instance, while production
WHNF laziness remains locked. The corrected sequential analysis baseline at
64/128/256/512 bindings is 0.983/2.141/4.843/11.593 ms CPU,
3.77/7.36/14.62/29.65 MB allocated, and 7/10/13/20 MiB peak memory. Raw
receipt: `benchmark-results/compiler-performance-follow-up-analysis-boundary/20260811T171151853547000000Z`.

## Task 2: Make evaluator transition accounting constant-time

**Files:** `src/Jazz/Compiler/Runtime.hs`,
`src/Jazz/Compiler/Runtime/Types.hs`, runtime observation tests, compiler scale
cases and stage-input tests.

- [x] Add a generated nested-application runtime family at depths
      64/128/256/512 and lock result, transition/application counts, current and
      maximum continuation depth, and profile output.
- [x] Record observed and unobserved runtime baselines.
- [x] Store strict continuation depth in the evaluation machine and update it on
      push/pop instead of scanning the list.
- [x] Avoid observation-state mutation in the disabled path while preserving
      nested-machine accounting whenever observation is enabled.
- [x] Run focused observation/runtime tests, record after evidence, and commit.

Matched runtime measurements use the generated benchmark revision `7592b257`
for both sides. The before worktree restores only `Runtime.hs` and
`ModuleRuntime.hs` to `0d88a34c`; the after worktree retains the optimized
implementations. Both runs use the same eight selectors, CPU timing mode, and
`+RTS -T` statistics under the `compiler-performance-follow-up-runtime` label.

At depth 512, cached evaluator depth reduced CPU from 1.828 ms to 1.772 ms
(-3.1%), allocation from 4.976 MB to 4.641 MB (-6.7%), and copied bytes from
322,468 to 177,921 (-44.8%), with unchanged 10 MiB peak memory. The improvement
increases with depth. Commits: `8cffd08a` and evidence follow-up `7592b257`.
Before receipt: `20260811T170954604311000000Z`; after receipt:
`20260811T171050346693000000Z` under the common runtime label.

## Task 3: Prepare runtime imports once

**Files:** `src/Jazz/Compiler/ModuleRuntime.hs`, module-pipeline tests, compiler
scale cases and stage-input tests.

- [x] Generate interfaces at widths 64/128/256/512 across whole, selective,
      aliased, capability-method, and namespace-collision imports.
- [x] Lock real-pipeline runtime output while retaining the existing export,
      cell-identity, and diagnostic contract tests.
- [x] Compute import mode, visible inventory, and selected capability names once
      per import and pass the prepared selection to a leaf predicate.
- [x] Record runtime/whole-program before and after curves and commit.

Task 3's matched 64-512 width curve was physically neutral: at width 512,
runtime CPU changed from 1.514 ms to 1.540 ms (+1.7%) and whole-program CPU
from 11.583 ms to 11.719 ms (+1.2%), while allocation changed only -0.5% and
-0.05% respectively. The representation change and implementation-coupled test
were reverted in `83c03fc3`; the real-pipeline scale family is retained so a
future compiler change can reveal a material shift.

## Task 4: Index Lowered IR temporary representations

**Files:** `src/Jazz/Compiler/LoweredIR/Validate.hs`, Lowered IR contract tests,
compiler scale cases and stage-input tests.

- [x] Generate direct valid Lowered Programs with 64/256/1024/4096 chained
      instructions plus duplicate, use-before-definition, and cross-block cases.
- [x] Lock exact validation failures and ordering.
- [x] Add a per-function `(blockId, temporaryId)` representation index that
      preserves first-definition behavior.
- [x] Record validation/lowering before and after curves and commit.

The direct validator curve confirms the old instruction scan was quadratic.
At 4,096 chained temporaries, the function-local representation index reduced
CPU from 58.519 ms to 4.278 ms (-92.7%). Its bounded index costs 5.7% more
allocation (10.33 to 10.92 MB), 0.57 MB more copied data, and 1 MiB more peak
memory at that size; the CPU slope reduction is material and the retained map
dies with the validator context. Exact duplicate-block, duplicate-temporary,
use-before-definition, cross-block, and combined failure ordering are locked.
Implementation: `1bb64865`; direct fixtures: `8de32b4a`. Receipts:
`compiler-performance-follow-up-lowered-temporaries-{before,after}` runs
`20260811T174210907101000000Z` and `20260811T173931371976000000Z`.

## Task 5: Dispatch lexer tokens by their first character

**Files:** `src/Jazz/Compiler/Parser/Lexer.hs`, lexer/parser fixture tests,
compiler scale cases and profiling tests.

- [x] Extend the long-token family with nonliteral-heavy and literal-heavy
      controls. Lock exact token kinds, owned lexemes, spans, malformed-literal
      diagnostics, comment precedence, and signed-integer behavior.
- [x] Record the existing 1K/4K/16K/64K parse/lower curve and hotspot profile.
- [x] Inspect the next character once and dispatch to the owning token parser;
      reuse that decision in symbol handling.
- [x] Record compatible after evidence and commit.

The single-lookahead dispatch in `d6dfa3fe` preserves exact token and diagnostic
contracts. At 65,536 tokens, the identifier-only control fell from 82.183 ms,
443.03 MB allocated, 72.02 MB copied, and 54 MiB peak to 57.936 ms, 257.56 MB,
60.68 MB, and 51 MiB (-29.5% CPU, -41.9% allocation). The literal-heavy
control fell from 82.093 ms/386.93 MB to 63.384 ms/216.80 MB (-22.8% CPU,
-44.0% allocation), with copied bytes down 13.9%. The earlier long-token
hotspot receipt is `profile-results/compiler-scale-matrix-baseline/`
`token-stream-65536-hotspots.prof`; matched control receipts are
`compiler-performance-follow-up-lexer-controls-{before,after}` runs
`20260811T174322246358000000Z` and `20260811T174410946780000000Z`.

## Task 6: Replace Typed Core reachability with source-ordered SCC facts

**Files:** `src/Jazz/Compiler/TypedCore/Validate.hs`, Typed Core contract tests,
compiler scale cases and stage-input tests.

- [x] Generate valid typed statement chains, mutual SCCs, and repeated same-name
      histories at 128/512/1024/2048 statements; add invalid fixtures that lock
      failure ordering and forward-reference behavior.
- [x] Reuse latest-prior/first-future indexes and `stronglyConnComp` rather than
      materializing all-pairs reachability.
- [x] Preserve recursive eligibility and source-ordered group projection.
- [x] Record Typed Core validation before and after evidence and commit.

Commit `2bb78586` computes SCCs once, uses ordered predecessor/successor name
history lookup, and projects cyclic groups once in source order. At 2,048
statements, CPU fell from 448.452 ms to 12.672 ms (-97.2%), allocation from
1.072 GB to 29.05 MB (-97.3%), copied bytes from 206.81 MB to 3.39 MB (-98.4%),
and peak memory from 182 MiB to 13 MiB (-92.9%). The full curve improves at
every size and the direct semantic suite preserves nearest-prior rebinding,
future visibility, recursive eligibility, diagnostics, and source ordering.
Receipts: `compiler-performance-follow-up-typed-graph-{before,after}` runs
`20260811T173406299905000000Z` and `20260811T173914927802000000Z`.

## Task 7: Make wide constructor processing append-efficient

**Files:** `src/Jazz/Compiler/TypeInference/Scope.hs`,
`src/Jazz/Compiler/Runtime.hs`, `src/Jazz/Compiler/Runtime/Types.hs`,
`src/Jazz/Compiler/Runtime/Semantics.hs`, ADT semantic tests and scale cases.

- [x] Generate a constructor with 32/64/128/256 ordered fields, fully apply it,
      destructure it, and return a sentinel. Lock type, render, currying,
      saturation, field order, and pattern behavior.
- [x] Reverse-build constructor argument metadata and carry captured arity
      without repeated list lengths or prefix copies.
- [x] Record analysis/runtime before and after curves and commit.

Commit `e156c483` keeps the historical ordered-list `VConstructor` pattern but
stores curried arguments in an internal sequence with strict declared/captured
arity. Type inference reverse-builds field metadata once. At 256 fields,
runtime CPU fell from 1.898 ms to 0.829 ms (-56.3%), allocation from 5.86 MB to
2.29 MB (-60.8%), copied bytes from 284.8 KB to 52.7 KB (-81.5%), and peak
memory from 9 to 8 MiB. Analysis CPU fell 14.9% and allocation 20.5%; the
single-run 13-to-15 MiB analysis peak variation is not treated as deterministic
evidence. Receipts: `compiler-performance-follow-up-wide-constructor-`
`{before,after}` and `compiler-performance-follow-up-wide-constructor-analysis-`
`{before,after}` runs `20260811T175509976394000000Z`,
`20260811T180018555623000000Z`, `20260811T180231633434000000Z`, and
`20260811T180033831229000000Z`.

## Task 8: Remove capability-fact and candidate list scaling

**Files:** `src/Jazz/Compiler/CapabilityFacts.hs`, type-inference capability
modules/types, runtime capability registration/selection, capability semantic
tests and scale cases.

- [ ] Generate 16/32/64/128 classes/impls with declaration-only, exact-last,
      compatible, ambiguity, import-order, and repeated-call controls.
- [x] Add the exact empty-constraint fast path to referenced capability facts.
- [ ] Replace append-built ordered candidate construction with one reverse/final
      builder while preserving source/import order.
- [ ] If the recorded lookup curve remains material, add structured class/method
      indexes with an ordered compatibility fallback; otherwise stop without the
      larger representation change.
- [ ] Record before and after evidence and commit each independently justified
      representation change.

The empty-constraint fast path is independently material and requires no new
representation. A forcing characterization proves an unconstrained scheme no
longer traverses any capability catalogue. At 512 sequential polymorphic
bindings, analysis CPU fell from 11.442 ms to 9.055 ms (-20.9%), allocation
from 29.65 MB to 15.49 MB (-47.8%), and copied bytes by 2.4%, with unchanged
20 MiB peak memory. At 128 bindings peak memory also fell from 10 to 8 MiB.
Receipts: `compiler-performance-follow-up-empty-capabilities-{before,after}`
runs `20260811T175042277865000000Z` and `20260811T174826866747000000Z`.

## Task 9: Reuse trusted host-free provenance

**Files:** `src/Jazz/Compiler/ModuleRuntime.hs`,
`src/Jazz/Compiler/Runtime.hs`, runtime host/pure tests and scale cases.

- [x] Generate host-free module chains/fanout with nonempty imported/prelude
      environments and 64-4096 lazy lets, paired with early/late host builtin
      controls.
- [x] Profile the generic and trusted-pure paths; proceed only if host machinery
      is a material contributor.
- [x] Probe reuse of the existing opaque-environment host fact at the generic
      scope boundary and reject the change when the matched curve is neutral.
- [x] Preserve observation, effect, forcing, export identity, and diagnostic
      order; record the rejected probe and retain only its scale family.

The generated 64-4096-let family forces a nonempty imported environment while
keeping every local expression host-free. Reordering the generic branch test to
reuse the already-required environment host fact was neutral: at 4,096 lets,
CPU changed from 38.187 ms to 38.469 ms (+0.7%), allocation from 31.113 MB to
31.111 MB, copied bytes from 10.993 MB to 10.898 MB, and peak memory stayed
39 MiB. The production change was reverted rather than retained without a
physical benefit. The committed fixture remains as a gate for a future,
explicit known-pure evaluator lane. Receipts:
`compiler-performance-follow-up-host-free-scan-{before,after}` runs
`20260811T180427152233000000Z` and `20260811T180559408538000000Z`.

## Task 10: Probe remaining producer, parser, and lifetime candidates

**Files:** targeted owners from the inventory, their focused semantic suites,
compiler scale cases, and this plan.

- [ ] Measure direct-Typed-Core forward-function suffixes and wide exports;
      implement right-folded suffix facts and indexed export lookup only for
      demonstrated superlinear curves.
- [ ] Measure ambiguous case-arm pipes, module-owned span qualification, and
      signature payload ownership separately; do not combine grammar and
      lowering changes in one commit.
- [ ] Count recursive-preview frontier/free-variable queries and heap-profile
      repeated runtime scope-plan construction before changing ownership.
- [ ] Record rejected/neutral probes explicitly so they are not repeatedly
      reopened.

## Full closeout

- [ ] Run focused repository/docs checks after the final plan update.
- [ ] Request an independent whole-range code/performance review and resolve all
      critical or important findings.
- [ ] Run exactly one fresh full main gate after the final source change.
- [ ] Push the branch, refresh pull-request checks/review state, and report the
      per-batch before/after CPU, allocation, copied-byte, and residency results.
