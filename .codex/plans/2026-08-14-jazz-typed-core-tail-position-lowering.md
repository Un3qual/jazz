---
id: JN-BOOTSTRAP-TYPED-CORE-TAIL-POSITION-LOWERING-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 2"
target_paths:
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - docs/compiler/bootstrapping.md
  - docs/compiler/pipeline.md
  - docs/project/status.md
  - rfcs/accepted/0013-typed-core-tail-position-lowering.md
  - .codex/execution/blocker-contracts.md
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Lower exact local direct and closure calls in true named or lifted function-result position to existing Lowered IR tail terminators, including selected conditional branches and scalar-case bodies without result joins."
last_verified: 2026-08-14
---

# Jazz Typed-Core Tail-Position Lowering Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit existing Lowered IR direct and closure tail-call terminators for
exact calls in true named or lifted function-result position across the current
conditional and scalar-pattern-case profile.

**Architecture:** A dedicated function-result lowering boundary owns final
termination. Ordinary tail expressions reuse value lowering and locally turn
only their exact final call instruction into a tail terminator. Conditionals
and scalar pattern cases receive the function-result destination structurally,
so selected branches and arm bodies terminate directly without result joins.

**Tech Stack:** Haskell, Typed Core, backend-neutral Lowered IR v1, GHC 9.14.1,
Cabal, Nix

## Global Constraints

- Implement only accepted RFC 0013 and
  `JN-BOOTSTRAP-TYPED-CORE-TAIL-POSITION-LOWERING-001`.
- Change only named and lifted function-body lowering. Keep the synthetic
  module entry on ordinary value lowering followed by `LoweredReturn`.
- Classify tail position by control position and exact signature, not recursive
  group membership.
- Emit `LoweredDirectTailCall` for exact local direct calls and
  `LoweredClosureTailCall` for exact final unary closure-call stages.
- Preserve ordered earlier stages of oversaturated applications; partial
  applications return their closure normally.
- Propagate function-result position only into selected conditional branches
  and selected scalar-case bodies. Conditions, scrutinees, guards, arguments,
  and operands remain value positions.
- Remove conditional or case result joins only in function-result position.
  Value-position uses retain their existing joins and edge transport.
- Reuse the existing Typed Core and Lowered IR schemas, version, validators,
  failure kinds, block identities, ambient-slot order, and generated identity
  grammar.
- Preserve source diagnostics, producer failures, Typed Core validation,
  lowerer failures, and Lowered IR validation precedence.
- Add no native stack guarantee, runtime ABI, optimizer, module integration,
  managed pattern support, or ordinary compile/run cutover.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Follow strict red-green-refactor: every production change must be preceded by
  a focused expectation that fails for the missing tail behavior.
- Commit each green milestone with the commit message named below.

---

### Task 1: Promote the accepted tail-position child

**Files:**

- Create: `.codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Source contract:
  `.codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering-design.md`
  and `rfcs/accepted/0013-typed-core-tail-position-lowering.md`.
- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Keep `Next Curation Target` empty while this row is executable.

- [x] **Step 1: Add this implementation plan with ready frontmatter.** Keep the
      ordered target paths, verification commands, and deliverable identical to
      the queue row.

- [x] **Step 2: Promote the queue row and active blocker contract.** Replace the
      terminal-empty executor status, point the bootstrap umbrella at RFC 0013,
      and name this child as the current smallest unblocker.

- [x] **Step 3: Validate curation metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero and the queue row matches the plan
  frontmatter.

- [x] **Step 4: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core tail-position lowering"
  ```

### Task 2: Emit exact direct and closure tail terminators

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Add the internal result destination:

  ```haskell
  data ResultDestination
    = ProduceValue
    | FinishFunction LoweredRepresentation
  ```

- Add a function-result entry point with the existing failure ordering:

  ```haskell
  lowerFunctionResult ::
    [Text] ->
    [Int] ->
    [Int] ->
    FunctionIndex ->
    [FunctionParameterShape] ->
    LoweredRepresentation ->
    LoweringState ->
    TypedExpr ->
    ([LoweredIRLoweringFailure], LoweringState)
  ```

- Complete an ordinary produced operand with this exact decision:

  ```haskell
  finishFunctionResult expected operand state
    | loweredOperandRepresentation operand /= expected = Nothing
    | LoweredTemporaryOperand temporary representation <- operand,
      LoweredInstruction produced representation' operation : prior <- loweringInstructions state,
      produced == temporary,
      representation' == representation,
      Just terminator <- tailTerminator operation =
        Just (finishCurrentBlock terminator state {loweringInstructions = prior})
    | otherwise = Just (finishCurrentBlock (LoweredReturn operand) state)
  ```

- `tailTerminator` maps only `LoweredDirectCall` and `LoweredClosureCall`; no
  other instruction may be removed.

- [ ] **Step 1: Change exact recursive and helper-function expectations to tail
      terminators.** Cover direct self/mutual recursion, closure-shaped
      recursion, capturing recursion, and a non-recursive local helper. Leave
      the synthetic entry expectations as ordinary call instructions and
      returns.

- [ ] **Step 2: Run the focused suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: exact Lowered IR comparisons report call instructions plus
  `LoweredReturn` where tail terminators are now required.

- [ ] **Step 3: Add `lowerFunctionResult`, `finishFunctionResult`, and
      `tailTerminator`; route `emitFunction` through them.** Build a
      `LoweredFunction` from `reverse (loweringCompletedBlocks finalState)`;
      never append a second terminator.

- [ ] **Step 4: Run the Step 2 command and verify GREEN for direct and closure
      function bodies.** Confirm entry-function expectations remain unchanged.

- [ ] **Step 5: Add negative exact expectations.** Prove that a function body
      returning a literal, scalar operand, or partial application uses
      `LoweredReturn`, and that a call consumed by a binary expression remains a
      `LoweredDirectCall` or `LoweredClosureCall` instruction.

- [ ] **Step 6: Run the focused suite again.** Expected: all function-result
      and negative expectations pass with validated Lowered IR.

- [ ] **Step 7: Commit the call-termination milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower function-result tail calls"
  ```

### Task 3: Propagate function-result position through CFG alternatives

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Generalize the existing conditional and scalar-case helpers behind
  destination-aware internal functions while retaining their value wrappers:

  ```haskell
  lowerConditional = lowerConditionalTo ProduceValue
  lowerScalarPatternCase = lowerScalarPatternCaseTo ProduceValue
  ```

- `lowerFunctionResult` calls `lowerConditionalTo (FinishFunction expected)` or
  `lowerScalarPatternCaseTo (FinishFunction expected)` when the complete tail
  expression has that form.
- A `FinishFunction` conditional lowers both branches recursively and emits no
  join block.
- A `FinishFunction` scalar case lowers every selected body recursively,
  carries completed blocks into later failure alternatives, and emits no join
  block.

- [ ] **Step 1: Change the exact conditional function expectations.** For
      `conditional-function-parameter` and `conditional-captured-scalar`, make
      then/else blocks terminate directly with `LoweredReturn` and remove only
      their result join blocks. Keep entry-position conditional expectations
      unchanged.

- [ ] **Step 2: Add exact tail-call conditional coverage.** Use a source-valid
      recursive function whose base branch returns a scalar and whose recursive
      branch calls a local function. Expect one branch return and one direct or
      closure tail terminator, with no conditional result join.

- [ ] **Step 3: Run the focused suite and verify RED.** Expected: function-body
      conditionals still emit jumps to a result join and return its block
      parameter.

- [ ] **Step 4: Implement destination-aware conditional lowering.** Reuse the
      existing condition evaluation, deterministic block IDs, ambient slots,
      parameters, arguments, and remapping. In `FinishFunction`, recursively
      lower then and else bodies with the enclosing result representation and
      merge only their completed block lists.

- [ ] **Step 5: Run the focused suite and verify conditional GREEN.** Confirm
      nested entry/value conditionals still retain their joins.

- [ ] **Step 6: Add exact scalar-case function expectations.** Cover literal
      tests, a guarded variable arm, an unguarded catch-all, direct return, and
      a tail call. Expect no result join only for the function-body case;
      scrutinee and guard control flow must remain unchanged.

- [ ] **Step 7: Run the focused suite and verify case RED.** Expected: selected
      bodies still jump to a shared result join.

- [ ] **Step 8: Implement destination-aware scalar-case lowering.** Preserve
      profile checks, single scrutinee evaluation, `controlSlots`, arm-local
      binding, guard fallthrough, and later-arm continuation templates. In
      `FinishFunction`, terminate each body recursively and continue building
      only later alternative blocks; do not start the result join.

- [ ] **Step 9: Run the focused suite and verify case GREEN.** Confirm every
      produced program validates and value-position case expectations retain
      their joins.

- [ ] **Step 10: Commit the CFG milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: propagate tail position through control flow"
  ```

### Task 4: Close compiler documentation and execution state

**Files:**

- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0013-typed-core-tail-position-lowering.md`
- Modify: `.codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Public compiler documentation states that the opt-in profile records direct
  and closure tail intent for named/lifted function results, including
  selected conditional and bounded scalar-case bodies.
- Documentation explicitly says module entry remains ordinary and no native
  stack guarantee follows.
- Correct the stale bootstrapping statement that static exhaustiveness remains
  deferred; it shipped under RFC 0012.
- The queue returns to a checker-valid terminal-empty state with no invented
  successor.

- [ ] **Step 1: Update compiler and status documentation.** Describe compiler
      stages and semantic boundaries rather than implementation filenames.

- [ ] **Step 2: Mark RFC 0013 implemented.** Add the verified child and exact
      boundary to its implementation status.

- [ ] **Step 3: Close the plan and queue.** Set plan `status: complete`,
      `plan_section: "Full closeout"`, remove the ready row, update the
      bootstrap blocker, and retain the explicit no-source-backed/no-named-
      candidate terminal wording.

- [ ] **Step 4: Run documentation and queue checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: all commands exit zero.

- [ ] **Step 5: Commit documentation closeout.** Run:

  ```bash
  git add docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0013-typed-core-tail-position-lowering.md .codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: close typed-core tail-position lowering"
  ```

### Task 5: Run the complete verified gate

**Files:**

- Verify only; modify code or expectations only through a new red-green cycle
  if a real regression appears.

**Interfaces:**

- Focused suites prove exact producer/lowerer behavior and Haskell/Jazz
  contract parity.
- Full suite proves repository-wide compatibility under serialized execution.

- [ ] **Step 1: Run focused verification.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 2: Run the repository audit.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test repository-audit-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Run the full serialized suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

- [ ] **Step 4: Run final repository checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  git diff --check
  git status --short --branch
  ```

- [ ] **Step 5: Record fresh verification evidence in the plan's `Full
      closeout` section.** Include focused suite names, full suite count,
      documentation/queue/audit results, branch, and commit IDs.

## Full closeout

Pending implementation and fresh verification.
