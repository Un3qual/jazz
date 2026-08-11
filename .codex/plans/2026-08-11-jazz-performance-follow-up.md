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

- [ ] Add a characterization proving prepared analysis forces every
      `ResolvedModule` field owned by setup while preserving intentionally lazy
      production behavior.
- [ ] Add `forceResolvedModule` beside existing benchmark/profile forcing
      helpers and use it only in prepared benchmark `NFData` instances.
- [ ] Run profiling/stage tests and re-record analysis baselines before using
      that stage to rank subsequent tasks.
- [ ] Commit the measurement-boundary correction independently.

## Task 2: Make evaluator transition accounting constant-time

**Files:** `src/Jazz/Compiler/Runtime.hs`,
`src/Jazz/Compiler/Runtime/Types.hs`, runtime observation tests, compiler scale
cases and stage-input tests.

- [ ] Add a generated nested-application runtime family at depths
      64/128/256/512 and lock result, transition/application counts, current and
      maximum continuation depth, and profile output.
- [ ] Record observed and unobserved runtime baselines.
- [ ] Store strict continuation depth in the evaluation machine and update it on
      push/pop instead of scanning the list.
- [ ] Avoid observation-state mutation in the disabled path while preserving
      nested-machine accounting whenever observation is enabled.
- [ ] Run focused observation/runtime tests, record after evidence, and commit.

## Task 3: Prepare runtime imports once

**Files:** `src/Jazz/Compiler/ModuleRuntime.hs`, module-pipeline tests, compiler
scale cases and stage-input tests.

- [ ] Generate interfaces at widths 64/128/256/512 across whole, selective,
      aliased, capability-method, and namespace-collision imports.
- [ ] Lock exact runtime export maps, cell identities, output, and diagnostics.
- [ ] Compute import mode, visible inventory, and selected capability names once
      per import and pass the prepared selection to a leaf predicate.
- [ ] Record runtime/whole-program before and after curves and commit.

## Task 4: Index Lowered IR temporary representations

**Files:** `src/Jazz/Compiler/LoweredIR/Validate.hs`, Lowered IR contract tests,
compiler scale cases and stage-input tests.

- [ ] Generate direct valid Lowered Programs with 64/256/1024/4096 chained
      instructions plus duplicate, use-before-definition, and cross-block cases.
- [ ] Lock exact validation failures and ordering.
- [ ] Add a per-function `(blockId, temporaryId)` representation index that
      preserves first-definition behavior.
- [ ] Record validation/lowering before and after curves and commit.

## Task 5: Dispatch lexer tokens by their first character

**Files:** `src/Jazz/Compiler/Parser/Lexer.hs`, lexer/parser fixture tests,
compiler scale cases and profiling tests.

- [ ] Extend the long-token family with nonliteral-heavy and literal-heavy
      controls. Lock exact token kinds, owned lexemes, spans, malformed-literal
      diagnostics, comment precedence, and signed-integer behavior.
- [ ] Record the existing 1K/4K/16K/64K parse/lower curve and hotspot profile.
- [ ] Inspect the next character once and dispatch to the owning token parser;
      reuse that decision in symbol handling.
- [ ] Record compatible after evidence and commit.

## Task 6: Replace Typed Core reachability with source-ordered SCC facts

**Files:** `src/Jazz/Compiler/TypedCore/Validate.hs`, Typed Core contract tests,
compiler scale cases and stage-input tests.

- [ ] Generate valid typed statement chains, mutual SCCs, and repeated same-name
      histories at 128/512/1024/2048 statements; add invalid fixtures that lock
      failure ordering and forward-reference behavior.
- [ ] Reuse latest-prior/first-future indexes and `stronglyConnComp` rather than
      materializing all-pairs reachability.
- [ ] Preserve recursive eligibility and source-ordered group projection.
- [ ] Record Typed Core validation before and after evidence and commit.

## Task 7: Make wide constructor processing append-efficient

**Files:** `src/Jazz/Compiler/TypeInference/Scope.hs`,
`src/Jazz/Compiler/Runtime.hs`, `src/Jazz/Compiler/Runtime/Types.hs`,
`src/Jazz/Compiler/Runtime/Semantics.hs`, ADT semantic tests and scale cases.

- [ ] Generate a constructor with 32/64/128/256 ordered fields, fully apply it,
      destructure it, and return a sentinel. Lock type, render, currying,
      saturation, field order, and pattern behavior.
- [ ] Reverse-build constructor argument metadata and carry captured arity
      without repeated list lengths or prefix copies.
- [ ] Record analysis/runtime before and after curves and commit.

## Task 8: Remove capability-fact and candidate list scaling

**Files:** `src/Jazz/Compiler/CapabilityFacts.hs`, type-inference capability
modules/types, runtime capability registration/selection, capability semantic
tests and scale cases.

- [ ] Generate 16/32/64/128 classes/impls with declaration-only, exact-last,
      compatible, ambiguity, import-order, and repeated-call controls.
- [ ] Add the exact empty-constraint fast path to referenced capability facts.
- [ ] Replace append-built ordered candidate construction with one reverse/final
      builder while preserving source/import order.
- [ ] If the recorded lookup curve remains material, add structured class/method
      indexes with an ordered compatibility fallback; otherwise stop without the
      larger representation change.
- [ ] Record before and after evidence and commit each independently justified
      representation change.

## Task 9: Reuse trusted host-free provenance

**Files:** `src/Jazz/Compiler/ModuleRuntime.hs`,
`src/Jazz/Compiler/Runtime.hs`, runtime host/pure tests and scale cases.

- [ ] Generate host-free module chains/fanout with nonempty imported/prelude
      environments and 64-4096 lazy lets, paired with early/late host builtin
      controls.
- [ ] Profile the generic and trusted-pure paths; proceed only if host machinery
      is a material contributor.
- [ ] Propagate the existing conservative whole-program host-free proof through
      a dedicated known-pure scope entry point without extending compiled
      artifact lifetime.
- [ ] Preserve observation, effect, forcing, export identity, and diagnostic
      order; record before/after evidence and commit.

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
