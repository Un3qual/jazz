---
id: JN-BOOTSTRAP-TYPED-CORE-CURRIED-APPLICATION-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 2"
target_paths:
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/TypedCore/Validate.hs
  - jazz/compiler/TypedCoreValidate.jz
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
verification:
  - cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce and lower staged curried applications, partial-application closure results, and ordered callable oversaturation while preserving exact source-diagnostic and malformed-artifact rejection boundaries."
last_verified: 2026-08-12
---

# Jazz Typed-Core Curried Application Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer with
unary staged currying, ordinary partial-application closure results, and ordered
oversaturation through callable intermediate results.

**Architecture:** Typed-core finalization walks every source application one
operand at a time, derives each intermediate type and recursively staged
representation recipe, and preserves the complete direct-call fast path only
for a statically known full leading-lambda chain. The lowerer consumes the
validated expression tree as written: direct complete chains remain one
multi-operand call, while closure-shaped stages emit one unary closure call at a
time and feed each callable result into the next stage without flattening or an
adapter ABI.

**Tech Stack:** Haskell, hosted Jazz, GHC 9.14.1, Cabal, backend-neutral
Lowered IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-CURRIED-APPLICATION-001` from
  accepted RFC 0009.
- Keep production and lowering opt-in through
  `finalizeTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Preserve one source application per unary closure stage. Evaluate the next
  argument only after the previous call returns its callable result.
- Retain the flattened multi-operand direct-call path only for a statically
  known complete leading-lambda chain whose intermediate applications cannot
  execute arbitrary computation.
- Make partial application the ordinary closure result of one source call; add
  no synthetic adapter function, mutable placeholder, alternate ABI, or new
  Typed Core or Lowered IR constructor/version.
- Keep source attempts to apply an argument after a non-callable result as
  ordinary type diagnostics. Keep independently constructed malformed typed
  oversaturation as a typed-core invariant failure that never reaches lowering.
- Preserve exact binder identity, lexical capture layouts, generated identity,
  validator parity, failure preorder, and every prior scalar/direct/closure
  fixture manifest.
- Retain structured rejection for recursion, control flow, patterns, nested
  blocks, imports, unsupported managed values, runtime services, and native
  behavior.
- Run Cabal only inside the checked-in Nix development shell and use
  `--jobs=1`.
- Commit after each green milestone using the commit message named below.

---

### Task 1: Promote the executor-ready curried-application child

**Files:**

- Create: `.codex/plans/2026-08-12-jazz-typed-core-curried-application.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote RFC 0009's fourth child with the exact ten required target paths and
  G1 gate from the ownership matrix.
- Remove curried application from `Next Curation Target` while its ready row is
  active.
- Keep both recursion children blocked and do not pre-promote direct recursion.

- [x] **Step 1: Add the matching plan and Ready Now row.** Use priority `P1`,
      size `L`, kind `impl`, `autonomous_ready: yes`, and the four exact
      verification commands in this plan's frontmatter.

- [x] **Step 2: Update the bootstrap blocker handoff.** State that execution of
      the validated curried-application row is the smallest unblocker and that
      no semantic decision remains.

- [x] **Step 3: Validate plan metadata and queue consistency.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero.

- [x] **Step 4: Commit the curation milestone.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-curried-application.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core curried application"
  ```

### Task 2: Produce staged curried applications

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Replace whole-spine equality checks in `finalizeApplicationSpine` with an
  ordered stage decision derived from the callee's concrete function type and
  recursively staged recipe.
- Continue producing one flattened `TypedApplyExpr` chain for a complete direct
  declaration call. Produce unary staged nodes for a closure-shaped named
  function, callable parameter, anonymous/nested lambda, partial application,
  or callable intermediate result.
- Use `scalarOrCallableInfo` for every intermediate result at its exact nested
  application path. Never infer a recipe from only the final result.
- Preserve argument descendant failures even when the outer application is
  unsupported, and keep existing source diagnostic precedence unchanged.

- [ ] **Step 1: Add exact producer success fixtures before implementation.**
      Add hand-derived typed programs for: a two-stage named function partially
      applied as the terminal result; that partial result passed to a
      higher-order consumer; an inline two-stage lambda; and a one-argument
      function returning a callable that is immediately oversaturated. Require
      nested unary recipes and exact binder references at every application.

- [ ] **Step 2: Add exact negative and ordering fixtures.** Preserve ordinary
      source diagnostics for applying after a scalar result. Add an
      underapplication whose supplied operand has an unsupported managed
      representation and assert its descendant failure ordering. Preserve all
      recursion and managed-capture rejections.

- [ ] **Step 3: Run the producer suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the new partial and oversaturated callable fixtures fail with the
  existing `TypedCoreCallArityUnsupported` boundary while all prior cases keep
  their previous outcomes.

- [ ] **Step 4: Finalize applications one source stage at a time.** Consume the
      concrete function type/recipe pair for each argument, emit the exact
      intermediate node info, and allow additional arguments only while the
      previous result is callable. Preserve the complete direct-call coalescing
      rule and fail closed on unresolved/non-concrete stage metadata.

- [ ] **Step 5: Run the focused producer suite twice and verify GREEN.** Run the
      Step 3 command twice. Expected: both runs pass with identical accepted and
      rejected manifests.

- [ ] **Step 6: Commit the producer milestone.**

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: produce typed curried applications"
  ```

### Task 3: Validate staged application invariants in Haskell and Jazz

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz/compiler/TypedCoreValidate.jz`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Require each `TypedApplyExpr` function node to expose exactly one applicable
  function stage, with argument type/recipe matching that stage and result
  type/recipe matching the remainder.
- Accept both flattened direct declaration recipes and recursively staged
  closure recipes only in the callable-shape contexts authorized by RFC 0009.
- Reject malformed typed oversaturation at the first node whose function value
  is non-callable. Mirror failure kind, detail, path, and order exactly in
  Haskell and hosted Jazz.

- [ ] **Step 1: Add valid contract parity fixtures.** Feed partial results,
      staged callable results, and ordered callable oversaturation through both
      validators twice with literal canonical expectations.

- [ ] **Step 2: Add malformed-artifact fixtures before validator changes.**
      Construct a typed tree that applies after a scalar result, plus mutations
      for wrong intermediate type, wrong intermediate recipe, and a flattened
      closure-stage recipe. Assert exact invariant failures and ensure these
      programs are absent from the valid lowerer manifest.

- [ ] **Step 3: Run typed-core contract verification and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: at least one new malformed staged fixture disagrees with the
  required exact failure set before the mirrored invariant is implemented.

- [ ] **Step 4: Implement the invariant once in each validator.** Keep the
      Haskell and Jazz branches structurally aligned, reuse the existing recipe
      and application compatibility helpers, and add no sidecar traversal or
      duplicate encoder/runner.

- [ ] **Step 5: Run the typed-core contract suite twice and verify GREEN.** Run
      the Step 3 command twice. Expected: stable ordered parity on both runs.

- [ ] **Step 6: Commit the validator milestone.**

  ```bash
  git add src/Jazz/Compiler/TypedCore/Validate.hs jazz/compiler/TypedCoreValidate.jz test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  git commit -m "feat: validate staged curried applications"
  ```

### Task 4: Lower partial and oversaturated callable stages in order

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`

**Interfaces:**

- Keep a complete direct declaration application as one `LoweredDirectCall`
  with all proven-safe operands.
- Lower every general staged application as one `LoweredClosureCall` with one
  argument. If the result is callable, retain its recursively nested
  `LoweredClosureRepresentation` as the operand for the next stage.
- Emit argument instructions only after the preceding call instruction so
  oversaturation preserves source staging.

- [ ] **Step 1: Add literal expected lowered programs before implementation.**
      Cover a terminal partial result, a partial result passed as a value, an
      inline two-stage lambda, and callable oversaturation. Assert function,
      environment, closure-construction, argument, and unary closure-call
      instruction order exactly.

- [ ] **Step 2: Run the expression/lowerer suites and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: accepted typed partial application still reaches the existing
  lowerer partial-call rejection or produces a non-matching flattened call.

- [ ] **Step 3: Separate the complete direct fast path from staged lowering.**
      Detect only a validated complete direct declaration spine for operand
      coalescing. For every other application, lower the callee, then its one
      argument, then emit one closure call and return that exact result operand.

- [ ] **Step 4: Add Lowered IR parity pressure.** Run every new valid lowered
      artifact through Haskell and hosted Jazz validators twice. Add realistic
      mutations for wrong closure result representation and call order without
      adding a second lowerer harness.

- [ ] **Step 5: Run G1 twice and verify GREEN.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: both complete repetitions pass with stable manifests and ordered
  artifacts.

- [ ] **Step 6: Commit the lowering/parity milestone.**

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
  git commit -m "feat: lower staged curried applications"
  ```

### Task 5: Close the child and verify the repository boundary

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-typed-core-curried-application.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`

**Interfaces:**

- Mark this plan complete only after fresh focused, full serialized, queue,
  docs, and diff gates.
- Remove the completed row from `Ready Now`, place only
  `JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001` in `Next Curation Target`, and
  update the blocker handoff without pre-promoting recursion.
- State publicly that the opt-in profile supports staged currying, partial
  application, and callable oversaturation while recursion remains absent and
  ordinary compile/run stays unchanged.

- [ ] **Step 1: Run fresh G1 verification.** Run the Task 4 Step 5 command once
      after the final source edit and read the complete exit status.

- [ ] **Step 2: Run the full serialized compiler suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every registered test suite passes without parallel timeout noise.

- [ ] **Step 3: Update public compiler-boundary pages.** Add staged curried
      applications, ordinary partial closure results, and ordered callable
      oversaturation to the supported opt-in profile; retain recursion and all
      unrelated exclusions plus the normal compile/run disclaimer.

- [ ] **Step 4: Synchronize plan, queue, blocker, and RFC state.** Check every
      task box, set frontmatter `status: complete` and `autonomous_ready: no`,
      use plan section `Full closeout`, remove the ready row, and add direct
      recursion as the sole next curation candidate using RFC 0009's exact G4
      ownership and gate.

- [ ] **Step 5: Run closeout gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [ ] **Step 6: Perform the anti-slop review.** Enumerate every new helper,
      branch, fallback, fixture builder, and validator clause in the diff.
      Remove anything without a concrete staged-production, invariant, ordered
      lowering, or parity responsibility.

- [ ] **Step 7: Commit closeout.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-curried-application.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0009-typed-core-closure-and-recursion.md
  git commit -m "docs: close typed-core curried application"
  ```
