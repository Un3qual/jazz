---
id: JN-BOOTSTRAP-TYPED-CORE-CONDITIONAL-CONTROL-FLOW-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - docs/compiler/bootstrapping.md
  - docs/compiler/pipeline.md
  - docs/project/status.md
  - rfcs/accepted/0010-typed-core-conditional-control-flow.md
  - .codex/execution/blocker-contracts.md
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce bounded value-returning if expressions as exact Typed Core and lower them into deterministic multi-block Lowered IR with explicit ambient edge transport and result joins."
last_verified: 2026-08-13
---

# Jazz Typed-Core Conditional Control Flow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer
with value-producing `if` expressions across the existing bounded expression
profile.

**Architecture:** The shared inference traversal retains a provisional
conditional and every provisional-tree analysis descends through its three
children. Finalization emits the existing `TypedIfExpr`. The lowerer becomes a
deterministic multi-block CFG builder that remaps every block-local ambient
operand through explicit branch and join parameters, with the selected result
as the join's final parameter.

**Tech Stack:** Haskell, hosted Jazz contract fixtures, GHC 9.14.1, Cabal,
backend-neutral Lowered IR v1, Nix

## Global Constraints

- Implement only accepted RFC 0010 and
  `JN-BOOTSTRAP-TYPED-CORE-CONDITIONAL-CONTROL-FLOW-001`.
- Keep production and lowering opt-in through
  `inferResolvedModuleTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept `if` at every expression path already admitted by the scalar,
  closure, capture, currying, and recursion profile.
- Require a source-valid `Bool` condition and one concrete unified branch
  result representation; do not repair type or representation disagreement in
  the lowerer.
- Evaluate the condition once and exactly one branch at runtime.
- Use the existing `TypedIfExpr`, `LoweredBranch`, `LoweredJump`, block
  parameter, and edge-argument schemas; add no Typed Core or Lowered IR
  constructor or version.
- Transport block-local local bindings in `TypedBinderId` order and block-local
  shared environments in `LoweredLayoutId` order. Do not depend on unordered
  map or set iteration.
- Do not leak temporary or block-parameter operands across a block edge without
  an explicit target parameter and edge argument.
- Generate block identifiers only from the typed statement path, expression
  path, and `then`, `else`, or `join` role using RFC 0010's counted grammar.
- Preserve condition, then, else failure order and return no partial Typed Core
  or Lowered IR on failure.
- Keep pattern cases, guards, pattern lambdas, managed values, local blocks,
  tail calls, imports, multi-module programs, runtime services, and native work
  closed.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Commit each green milestone with the commit message named below.

---

### Task 1: Promote the accepted conditional child

**Files:**

- Create: `.codex/plans/2026-08-13-jazz-typed-core-conditional-control-flow.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Point the bootstrap umbrella at accepted RFC 0010 and this plan.
- Keep `Next Curation Target` empty while this row is executable.

- [x] **Step 1: Record the approved design and durable decision.** Use
      `.codex/plans/2026-08-13-jazz-typed-core-conditional-control-flow-design.md`
      and `rfcs/accepted/0010-typed-core-conditional-control-flow.md` as the source
      contract.

- [x] **Step 2: Promote the queue row.** Replace the terminal-empty executor
      status with the single ready child and update the bootstrap blocker to name
      its execution.

- [x] **Step 3: Validate curation metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero and the queue row matches this plan's
  ordered frontmatter.

- [x] **Step 4: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-13-jazz-typed-core-conditional-control-flow.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core conditional control flow"
  ```

### Task 2: Retain exact provisional conditionals

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`

**Interfaces:**

- Add the private provisional form:

  ```haskell
  ProvisionalIfExpression
    ExpressionType
    ProvisionalTypedExpr
    ProvisionalTypedExpr
    ProvisionalTypedExpr
  ```

- Finalize it as:

  ```haskell
  TypedIfExpr info condition thenExpression elseExpression
  ```

- Preserve child paths `0`, `1`, and `2` and condition/then/else failure order.

- [x] **Step 1: Move the existing `conditional` source fixture from the
      rejected manifest to the accepted manifest.** Add its exact expected
      `TypedProgram` with a `TypedIfExpr` and add the exact expected four-block
      Lowered Program to the accepted-pipeline table.

- [x] **Step 2: Run the focused producer suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the fixture fails because production still returns
  `TypedCoreControlFlowUnsupported`.

- [x] **Step 3: Add `ProvisionalIfExpression` and construct it in
      `inferExprTypeDetailed`.** Specialize the condition against `TBoolType` and
      both branches against the unified conditional result type. Retain only child
      failures; do not emit the removed parent control-flow failure.

- [x] **Step 4: Finalize the provisional form.** Build exact node info from the
      resolved result type, recursively finalize child paths `0`, `1`, and `2`, and
      emit `TypedIfExpr` only when every failure list is empty.

- [x] **Step 5: Run the Step 2 command.** Expected: production reaches valid
      Typed Core; lowering still fails at `TypedIfExpr`, proving the producer half
      is green and the lowerer boundary remains red.

- [x] **Step 6: Commit the producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: produce typed-core conditionals"
  ```

### Task 3: Preserve analysis through conditional children

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`

**Interfaces:**

- Every provisional-tree traversal visits conditional children in the order
  condition, then branch, else branch.
- Expected-type propagation uses `Bool` for the condition and the selected
  conditional type for both branches.
- No traversal treats a conditional as a leaf.

- [x] **Step 1: Add source fixtures that fail under leaf treatment.** Add exact
      programs for a conditional function body using a parameter, a conditional
      whose branch captures an earlier scalar, and a conditional returning one of
      two closure values followed by application.

- [x] **Step 2: Run the focused producer suite and verify RED.** Expected:
      callable shape, free-name/capture, or scalar specialization differs from the
      literal expected Typed Core.

- [x] **Step 3: Update all provisional traversals.** Cover free-name discovery,
      callable-use classification, named-call specialization, call-profile
      collection, scalar-reference discovery, scalar-specialization discovery,
      recursive support analysis, parameter reference/application collection,
      provisional expression typing, expression specialization, and lambda counts.

- [x] **Step 4: Run the focused producer suite twice and verify GREEN.** Every
      new exact Typed Core value and every pre-existing manifest entry must match on
      both runs.

- [x] **Step 5: Commit the traversal milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "fix: preserve conditional analysis profiles"
  ```

### Task 4: Build deterministic scalar conditional CFGs

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Extend `LoweringState` with completed blocks, current block identity, and
  current block parameters.
- Add private helpers with these responsibilities:

  ```haskell
  finishCurrentBlock :: LoweredTerminator -> LoweringState -> LoweringState
  startBlock :: LoweredBlockId -> [LoweredParameter] -> LoweringState -> LoweringState
  finishFunctionBlocks :: LoweredOperand -> LoweringState -> [LoweredBlock]
  conditionalBlockId :: [Int] -> [Int] -> Text -> LoweredBlockId
  ```

- `emitFunction` and `emitEntry` use the block list produced by the CFG state
  rather than constructing one hard-coded entry block.

- [x] **Step 1: Change `testLowererStructuralBoundary` so
      `conditional-entry` expects `LoweredIRSucceeded` with exact entry, then,
      else, and join blocks.** The entry branches on `True`; each branch jumps with
      its literal; the join returns `LoweredBlockParameterOperand "result"`.

- [x] **Step 2: Run the focused suite and verify RED.** Expected: the existing
      lowerer reports `LoweredIRUnsupportedExpression` at the conditional path.

- [x] **Step 3: Generalize `LoweringState` into the internal CFG builder.** Keep
      instruction insertion and temporary numbering behavior unchanged within a
      block. Make all initial states start block `entry`, and make function/entry
      finalization terminate the current block with `LoweredReturn`.

- [x] **Step 4: Add `lowerConditional` for the no-ambient case.** Lower the
      condition once, finish with `LoweredBranch`, lower branches independently,
      jump to a join carrying the result, and return the join parameter operand.
      Generate exact counted block identifiers from normalized typed paths.

- [x] **Step 5: Run the focused suite twice and verify GREEN.** Confirm exact
      block order and identifiers, and confirm the complete output passes
      `validateLoweredProgram` through `LoweredIRSucceeded`.

- [x] **Step 6: Commit the CFG foundation.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower scalar conditional control flow"
  ```

### Task 5: Transport ambient values and nest conditional CFGs

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Introduce one ordered edge-scope representation that pairs source operands,
  target parameters, and remapped successor maps. Do not add separate local and
  shared-environment edge algorithms.
- Transport `LoweredTemporaryOperand` and `LoweredBlockParameterOperand` map
  values; leave immediates and function parameters unchanged.
- Order local bindings first by `TypedBinderId`, then shared environments by
  `LoweredLayoutId`.
- Then/else parameter lists are identical. Join parameters repeat them and add
  `LoweredParameter (LoweredParameterId "result") resultRepresentation`.

- [x] **Step 1: Add exact RED fixtures.** Cover an earlier scalar used inside
      both branches and after the join, a projected closure capture used in a
      branch, a closure-valued branch result, a conditional condition nested inside
      another conditional, and conditionals nested in then and else branches.

- [x] **Step 2: Run the focused suite and verify RED.** Expected failures are
      Lowered IR operand-scope or edge-shape failures, missing generated functions,
      or exact CFG mismatch—not parser or fixture errors.

- [x] **Step 3: Implement ordered ambient edge scopes.** Build deterministic
      `live<N>` parameters and arguments, remap each successor's local/shared maps,
      and make join resumption use the join's remapped ambient maps.

- [x] **Step 4: Merge nested branch block sequences structurally.** Preserve
      condition blocks, then subtree, else subtree, join order; reset temporary
      numbering per new block; never merge branch-local operand maps into the join.

- [x] **Step 5: Run the focused suite twice and verify GREEN.** Confirm every
      exact nested program succeeds and all old scalar, closure, capture, currying,
      and recursion lowering values remain unchanged.

- [x] **Step 6: Commit the complete lowering milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: transport conditional CFG values"
  ```

### Task 6: Close contract coverage and public compiler status

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0010-typed-core-conditional-control-flow.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-13-jazz-typed-core-conditional-control-flow.md`

**Interfaces:**

- Public compiler-boundary docs say bounded value-producing conditionals are
  included while pattern cases and full control flow remain excluded.
- RFC 0010 records the implemented child without claiming normal-pipeline
  cutover.
- Closeout removes the ready row and restores the explicit terminal-empty queue
  state with no invented successor.

- [x] **Step 1: Add descendant-failure regression expectations.** Update every
      existing conditional-containing rejection so the removed parent failure
      disappears and unsupported children retain exact paths and order. Keep
      pattern-case and managed-value rejections literal.

- [x] **Step 2: Run the focused contract gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: all three suites pass with no warnings or failures.

- [x] **Step 3: Update public and durable documentation.** Name conditional CFG
      coverage, explicit edge transport, and retained pattern/module/native
      exclusions. Do not describe repository files as language stages.

- [x] **Step 4: Close the queue and plan metadata.** Set this plan to
      `status: complete`, remove the ready row, record the verified completed child
      in the bootstrap blocker, and state that no source-backed successor is named.

- [x] **Step 5: Run structural gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [x] **Step 6: Commit the closeout milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-13-jazz-typed-core-conditional-control-flow.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0010-typed-core-conditional-control-flow.md test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "docs: close typed-core conditional control flow"
  ```

### Full closeout

- [x] **Step 1: Run the complete serialized suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every suite passes with zero failures.

- [x] **Step 2: Re-run final structural evidence against committed HEAD.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  git status --short
  ```

  Expected: all gates exit zero and the worktree has no uncommitted changes.
