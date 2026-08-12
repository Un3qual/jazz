# Jazz Compiler Performance Review Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct the five actionable findings from the final review of `codex/compiler-performance-program` without changing Jazz language semantics.

**Architecture:** Verification scripts own and propagate bounded Cabal concurrency at every process boundary. Benchmark groups state one truthful timed boundary and force freshly produced artifacts structurally. An opaque prepared-scope value keeps shared recursive facts inseparable from the statement list that produced them while avoiding long-lived duplicate ASTs.

**Tech Stack:** Bash, Python policy tests, Haskell 2010, Cabal, Nix, tasty-style repository tests.

## Global Constraints

- Preserve diagnostics, source order, binder identity, public language semantics, hosted parity, and existing compiled artifact schemas.
- Run exactly one Cabal, Jazz, profiling, or Nix process at a time and pass `--jobs=1` to local Cabal gates.
- Add deterministic semantic or policy tests before production changes; do not add wall-clock assertions.
- Keep changes in active root compiler paths plus repository-owned verification/docs state.
- Commit each independently reviewed task before moving to the next.

---

### Task 1: Close nested verification job escapes and stale dispatch state

**Files:**
- Modify: `scripts/check-examples.sh`
- Modify: `scripts/ci/determinism.sh`
- Modify: `scripts/ci/fast-compiler.sh`
- Modify: `scripts/ci/main-functional.sh`
- Modify: `scripts/ci/extended.sh`
- Modify: `scripts/check-ci-policy.py`
- Modify: `scripts/test-check-ci-policy.py`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**
- Consumes: `JAZZ_CABAL_JOBS`, defaulting to `1`.
- Produces: every script that invokes Cabal validates the value and passes `--jobs="$JAZZ_CABAL_JOBS"`; completed performance work is no longer promotable as a blocker.

- [x] **Step 1: Add failing policy and behavior tests**

  Require job defaulting, validation, and `--jobs` use in `fast-compiler.sh`, `determinism.sh`, and the build-owning path of `check-examples.sh`. Require parent scripts to export or explicitly forward the bound when invoking children. Add a queue-contract assertion that no completed constraint-buffer child remains under `Current Blockers`.

- [x] **Step 2: Run the policy suite and verify the new tests fail for the missing nested bounds/stale blocker**

  Run: `python3 scripts/test-check-ci-policy.py`

- [x] **Step 3: Implement bounded nested scripts and retire the completed blocker**

  Use one shared shell pattern per script: `JAZZ_CABAL_JOBS="${JAZZ_CABAL_JOBS-1}"`, reject empty/zero/non-digit values, and pass `--jobs="$JAZZ_CABAL_JOBS"` to every Cabal build/test. Preserve `--jazz-bin` as the no-build example-check path.

- [x] **Step 4: Run focused policy, queue, and diff checks**

  Run: `python3 scripts/test-check-ci-policy.py`

  Run: `bash scripts/check-execution-queue.sh`

  Run: `git diff --check`

- [x] **Step 5: Commit**

  Commit message: `fix: bound nested verification jobs`

### Task 2: Make benchmark boundaries truthful and fully forced

**Files:**
- Modify: `src/Jazz/Compiler/Profiling.hs`
- Modify: `src/Jazz/Compiler/Force.hs`
- Modify: `benchmark/Jazz/Benchmark/ScaleCases.hs`
- Modify: `benchmark/Jazz/Benchmark/StageInputs.hs`
- Modify: `benchmark/Jazz/Benchmark/Stages.hs`
- Modify: `test/Jazz/Benchmark/StageSpec.hs`
- Modify: `test/Jazz/Compiler/ProfilingSpec.hs`
- Modify: `PERFORMANCE.md`

**Interfaces:**
- Consumes: generated Typed Core and Lowered IR scale fixtures.
- Produces: distinct `typed-validation`, `lowered-validation`, and `typed-lowering` benchmark groups; a structural `forceLoweredProgram` helper used before timed lowering samples return.

- [x] **Step 1: Add failing metadata and strictness tests**

  Pin the exact group names/stage mappings, assign validator-only scenarios to their corresponding validation groups, keep genuine handoff/forward-function cases in `typed-lowering`, and add a poison-field characterization proving structural Lowered IR forcing reaches payloads that validation alone need not inspect.

- [x] **Step 2: Run the focused tests and verify the expected metadata/strictness failures**

  Run: `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test benchmark-stage-spec profiling-spec --test-show-details=direct --jobs=1`

- [x] **Step 3: Implement the group split and structural force**

  Add group constructors and stable names without changing compiler stages or language artifacts. Dispatch preparation and timed work by the selected group, not scenario exceptions. Implement `forceLoweredProgram` by structurally traversing Lowered IR without allocating rendered output; force successful lowering output inside the timed action.

- [x] **Step 4: Run focused benchmark/profiling tests and diff checks**

  Run the Step 2 command again, followed by `git diff --check`.

- [x] **Step 5: Commit**

  Commit message: `fix: separate benchmark validation boundaries`

### Task 3: Make shared recursive facts scope-owned

**Files:**
- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `src/Jazz/Compiler/Analyzer.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`
- Modify: relevant analyzer/inference semantic test owner discovered by focused search.

**Interfaces:**
- Produces: opaque `PreparedRecursiveScope`, built from outer visibility plus one statement list, with read-only access to its statements and facts.
- Consumes: analyzer and type-inference supplied-facts paths accept the prepared value and do not accept a second independent statement list/expression.

- [x] **Step 1: Add failing ownership and mismatched-scope regression tests**

  Characterize that prepared facts expose their original statement scope and that no analyzer/inference entry point permits pairing facts from one scope with another. The semantic regression must retain the forward-unbound diagnostic for `x = y; y = 1` even when a recursive scope was prepared elsewhere.

- [x] **Step 2: Run focused tests and verify failure against the independently pairable API**

  Run: `nix --extra-experimental-features 'nix-command flakes' develop -c cabal test recursive-bindings-spec binding-signature-coherence-spec --test-show-details=direct --jobs=1`

- [x] **Step 3: Implement the opaque prepared-scope boundary**

  Keep `RecursiveScopeFacts` lightweight for runtime/resolver consumers. Add an opaque wrapper containing the exact statement list and its facts; make supplied-facts analyzer and inference APIs consume that wrapper without a second AST parameter. Do not retain prepared scopes beyond ordinary inference/analyzer lifetime.

- [x] **Step 4: Run focused recursive/analyzer/type-inference tests and diff checks**

  Run the Step 2 command again, followed by `git diff --check`.

- [x] **Step 5: Commit**

  Commit message: `fix: bind recursive facts to their scope`

### Task 4: Integrated closeout and push

**Files:**
- Modify: this plan and the existing performance program plan/report only as needed to record the review-fix receipts.

- [x] **Step 1: Run one final source review over the fix range**

- [x] **Step 2: Run the scope-based closeout gate exactly once**

  Run focused repository policy checks plus the affected Haskell suites under Nix with `--jobs=1`. Reuse the still-valid full-gate receipt because the fixes alter verification orchestration, benchmark-only taxonomy/forcing, and an internal safety boundary already covered by focused semantic suites; do not rerun the entire release gate unless review finds compiled behavior outside that scope.

- [ ] **Step 3: Record receipts, commit plan updates, and push `codex/compiler-performance-program`**
