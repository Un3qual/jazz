---
id: JN-BOOTSTRAP-TYPED-CORE-SCALAR-PATTERN-CASES-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Pattern.hs
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - docs/compiler/bootstrapping.md
  - docs/compiler/pipeline.md
  - docs/project/status.md
  - rfcs/accepted/0011-typed-core-scalar-pattern-cases.md
  - .codex/execution/blocker-contracts.md
  - scripts/check-docs.sh
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce ordered scalar literal, wildcard, and variable pattern cases with guarded fallthrough and a required final catch-all, then lower them to deterministic CFGs with explicit edge transport."
last_verified: 2026-08-14
---

# Jazz Typed-Core Scalar Pattern Cases Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer
with ordered scalar literal, wildcard, and variable pattern cases, guarded
fallthrough, arm-local binders, and a required final unguarded catch-all.

**Architecture:** The existing single inference traversal retains structured
provisional arms instead of flattening guard and body results. Typed-core
finalization validates the bounded profile and assigns exact arm-local binder
identities. The lowerer consumes validated `TypedPatternCaseExpr` values as an
ordered comparison/guard CFG chain, transporting the once-evaluated scrutinee,
ambient values, and in-flight operands explicitly and joining every selected
body through one result block.

**Tech Stack:** Haskell, hosted Jazz contract fixtures, GHC 9.14.1, Cabal,
backend-neutral Typed Core and Lowered IR v1, Nix

## Global Constraints

- Implement only accepted RFC 0011 and
  `JN-BOOTSTRAP-TYPED-CORE-SCALAR-PATTERN-CASES-001`.
- Keep production and lowering opt-in through
  `inferResolvedModuleTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept case expressions at every expression path already admitted by the
  scalar, closure, capture, currying, recursion, and conditional profile.
- Evaluate the scrutinee exactly once. Try arms in source order and preserve
  repeated literal arms because their guards can differ.
- Accept only immediate scalar scrutinees and `TypedLiteralPattern`,
  `TypedWildcardPattern`, or `TypedVariablePattern` arms. Require literal type
  and representation identity with the scrutinee.
- Require every non-final wildcard or variable arm to carry a guard. Require
  the final arm to be an unguarded wildcard or variable.
- Type guards as `Bool`. A false guard falls through to the next source arm.
  Arm bodies must unify to one concrete scalar or closure representation
  already supported by the profile.
- Give each variable pattern exactly one `TypedBinderId`, visible only in that
  arm's guard and body. Do not leak arm-local binders to later arms or the join.
- Use the existing `TypedPatternCaseExpr`, `TypedCaseArm`, `LoweredBranch`,
  `LoweredJump`, primitive equality, and block-parameter schemas. Add no public
  Typed Core or Lowered IR constructor or version.
- Lower an ordered arm chain directly. Do not normalize cases into
  `TypedIfExpr`, coalesce literal keys, or introduce a switch.
- Transport every non-global value crossing an arm-test, guard, body,
  continuation, or join edge. Keep existing deterministic local/shared
  ordering and append the scrutinee or result at a named position.
- Independently reject out-of-profile patterns and missing final catch-alls in
  the lowerer so arbitrary validated Typed Core cannot bypass the boundary.
- Preserve existing ordered descendant failures. Return no partial Typed Core
  or Lowered IR on any producer-profile or lowerer-profile failure.
- Do not add backend `E3022`, a runtime match-failure service, a trap
  terminator, or static exhaustiveness/unreachable-arm analysis.
- Keep constructor, list, tuple, text, as, and or-patterns; pattern lambdas;
  managed-value production; local blocks; tail calls; imports; multi-module
  programs; runtime services; native work; and normal-pipeline cutover closed.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Commit each green milestone with the commit message named below.

---

### Task 1: Promote the accepted scalar-pattern child

**Files:**

- Create: `.codex/plans/2026-08-14-jazz-typed-core-scalar-pattern-cases.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Point the bootstrap umbrella at accepted RFC 0011 and this plan.
- Keep `Next Curation Target` empty while the row is executable.

- [x] **Step 1: Record the approved durable decision.** Accept
      `rfcs/accepted/0011-typed-core-scalar-pattern-cases.md`, update the RFC
      index, and mark the design approved for implementation.

- [x] **Step 2: Commit the RFC acceptance.** The durable decision is recorded
      by commit `12abb066` (`docs: accept typed-core scalar pattern cases`).

- [x] **Step 3: Promote the queue row.** Replace the terminal-empty executor
      status with this single ready child and update the bootstrap blocker to
      name its execution contract and explicit non-goals.

- [x] **Step 4: Validate curation metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  git diff --check
  ```

  Expected: all commands exit zero; the queue row matches this plan's ordered
  frontmatter and RFC 0011 is indexed as accepted.

- [x] **Step 5: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-14-jazz-typed-core-scalar-pattern-cases.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core scalar pattern cases"
  ```

### Task 2: Retain exact provisional scalar cases

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`

**Interfaces:**

- Replace the flattened case inference payload with structured arms:

  ```haskell
  data InferredPatternCaseArm = InferredPatternCaseArm
    Pattern
    (Maybe InferredExpr)
    InferredExpr
  ```

- Retain the provisional tree as:

  ```haskell
  data ProvisionalPatternCaseArm = ProvisionalPatternCaseArm
    Pattern
    (Maybe ProvisionalTypedExpr)
    ProvisionalTypedExpr

  data ProvisionalTypedExpr
    = ...
    | ProvisionalPatternCaseExpression
        ExpressionType
        ProvisionalTypedExpr
        [ProvisionalPatternCaseArm]
  ```

- Finalize supported patterns without changing the public Typed Core schema:

  ```haskell
  finalizeScalarPattern
    :: Int
    -> [Int]
    -> ExpressionType
    -> Pattern
    -> Either TypedCoreProductionFailure
         (TypedPattern, Map.Map Name TypedBinderId)
  ```

- Use child path `0` for the scrutinee. Use case expression path plus
  `[armIndex]` for the arm pattern; retain each arm's existing guard/body child
  paths and source order.

- [x] **Step 1: Move the scalar pattern-case fixture into the accepted
      manifest.** Replace the old rejected fixture with this total form:

  ```jazz
  case True {
    | True -> 1
    | _ -> 2
  }.
  ```

  Add an exact expected `TypedProgram` using `TypedPatternCaseExpr`, two
  `TypedCaseArm` values, `TypedLiteralPattern`, and `TypedWildcardPattern`.
  Keep its Lowered IR expectation absent until Task 4.

- [x] **Step 2: Add exact producer boundary cases.** Assert profile failures
      for an empty arm list, a final guarded catch-all, a missing final
      catch-all, an unguarded non-final wildcard, an unguarded non-final
      variable, a managed scrutinee, and constructor/list/tuple/as/or patterns.
      Assert the existing inference diagnostic remains responsible for
      non-`Bool` guards and incompatible arm result types.

- [x] **Step 3: Run the focused producer suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the accepted fixture still returns
  `TypedCorePatternCaseUnsupported`; exact closed-boundary expectations remain
  stable.

- [x] **Step 4: Return structured inferred arms.** Change
      `inferPatternCaseTypeWithResults` to return
      `[InferredPatternCaseArm]`. Construct each record inside the same
      `inferPatternCaseTypeInternal` traversal that already owns pattern
      typing, arm environments, `Bool` guard checking, and body unification.
      Do not re-infer guards or bodies during elaboration.

- [x] **Step 5: Construct and finalize provisional cases.** Replace the root
      `EPatternCase` profile rejection in `inferExprTypeDetailed` with
      `ProvisionalPatternCaseExpression`. Recursively finalize the scrutinee,
      exact supported patterns, optional guards, and bodies. Emit
      `TypedPatternCaseExpr` only if every descendant and profile failure list
      is empty.

- [x] **Step 6: Enforce the bounded arm grammar in one producer helper.** Scan
      the source-ordered arms once. Reject unsupported pattern forms, missing
      or guarded final catch-alls, and unguarded earlier wildcard/variable
      arms. Verify literal patterns have the scrutinee's final type before
      calling the existing literal finalizer.

- [x] **Step 7: Run the Step 3 command.** Expected: exact Typed Core now
      matches; lowering still returns a structured lowerer-profile failure at
      the case path, proving the producer half is green without weakening the
      backend boundary.

- [x] **Step 8: Commit the producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Pattern.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: produce typed-core scalar pattern cases"
  ```

### Task 3: Preserve arm binders, guards, and callable analysis

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`

**Interfaces:**

- Assign a variable-pattern binder with:

  ```haskell
  binderAt statementIndex (caseExpressionPath <> [armIndex]) name
  ```

- Extend only that arm's lexical environment while finalizing its guard and
  body. The scrutinee and all later arms use the outer environment.
- Every provisional-tree traversal visits, in order: scrutinee, then each
  arm's optional guard followed by body.
- Apply the arm binder environment consistently to free-name/capture analysis,
  callable-use classification, parameter-use analysis, scalar specialization,
  recursive support checks, lambda counts, and finalization.

- [x] **Step 1: Add a guarded variable/catch-all fixture.** Lock the exact
      `TypedBinderId`, guard reference, body reference, and unified result:

  ```jazz
  case 2 {
    | item if item > 2 -> item
    | fallback -> fallback + 1
  }.
  ```

- [x] **Step 2: Add ordered guard fallthrough coverage.** Use repeated literal
      arms to prove source order is semantic:

  ```jazz
  case 2 {
    | 2 if False -> 10
    | 2 if True -> 20
    | _ -> 30
  }.
  ```

- [x] **Step 3: Add capture and callable-shape fixtures.** Add exact Typed Core
      for both programs:

  ```jazz
  seed = 40.
  choose = \(item) -> case item {
    | current if current > 0 -> current + seed
    | _ -> seed
  }.
  choose 2.
  ```

  ```jazz
  choose = \(flag) -> case flag {
    | True -> \(item) -> item
    | _ -> \(item) -> 0
  }.
  (choose True) 7.
  ```

  These fixtures must fail if the case is treated as a leaf, if an arm binder
  is counted as a capture, or if branch closure shape is discarded.

- [x] **Step 4: Run the focused suite and verify RED.** Expected failures are
      exact binder identity, capture layout, callable shape, or child-path
      mismatches—not parser failures.

- [x] **Step 5: Update every provisional traversal.** Add explicit
      `ProvisionalPatternCaseExpression` cases beside the existing
      `ProvisionalIfExpression` cases. Fold arms in source order and enter the
      arm-local binder scope only for that arm's guard and body. Do not place
      the binder in capture sets or recursive-group external dependencies.

- [x] **Step 6: Add the scalar case profile audit helper.** Return ordered
      `scalarPatternCaseProfileFailures` for the syntactic totality rule,
      immediate representation checks, and unsupported pattern forms. Combine
      those with existing descendant failures without masking earlier paths.

- [x] **Step 7: Run the focused suite twice and verify GREEN.** Every new exact
      Typed Core value and all pre-existing scalar, closure, capture, currying,
      recursion, and conditional fixtures must match identically on both runs.

- [x] **Step 8: Commit the analysis milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: preserve pattern-case binder profiles"
  ```

### Task 4: Lower the ordered scalar arm chain

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Add private profile failures, without changing the public IR:

  ```haskell
  data LoweredIRLoweringKind
    = ...
    | LoweredIRUnsupportedPattern
    | LoweredIRIncompletePatternCase
  ```

- Keep one lexical representation map for variable-pattern binders:

  ```haskell
  type LexicalRepresentations = Map.Map TypedBinderId LoweredRepresentation
  ```

- Add private helpers with these responsibilities:

  ```haskell
  patternCaseBlockId :: [Int] -> [Int] -> Int -> Text -> LoweredBlockId

  lowerScalarPatternCase
    :: [Int]
    -> [Int]
    -> TypedExpr
    -> [TypedCaseArm]
    -> LoweringState
    -> Either LoweredIRLoweringFailure (LoweredOperand, LoweringState)
  ```

- A literal arm emits the existing strict scalar equality primitive and a
  `LoweredBranch` to body-or-guard versus the next-arm test block.
- A wildcard/variable arm succeeds without comparison. A variable arm maps
  its binder to the transported scrutinee operand only inside the guard/body
  scope.
- A guarded match branches to its body when true and the next arm when false.
- Every body jumps to one result join. The final catch-all has no unmatched
  successor.

- [x] **Step 1: Make the root scalar fixture expect exact Lowered IR.** Add an
      entry block that computes the scrutinee once, a literal-test branch, one
      body block per arm, and one result join returning its block parameter.
      Assert exact block IDs, parameter IDs, instruction order, edge arguments,
      and terminators.

- [x] **Step 2: Add arbitrary Typed Core boundary fixtures.** Pass validated
      hand-built cases directly to the lowerer and require
      `LoweredIRUnsupportedPattern` for constructor/list/tuple/as/or patterns
      and `LoweredIRIncompletePatternCase` for every final-catch-all violation.
      Include literal/scrutinee representation disagreement even though the
      producer cannot emit it.

- [x] **Step 3: Run the focused suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the exact valid case still returns
  `LoweredIRUnsupportedExpression`; direct boundary fixtures do not yet receive
  the pattern-specific failures.

- [x] **Step 4: Validate the lowerer profile before emitting blocks.** Inspect
      every arm in source order, require an immediate scalar scrutinee, exact
      literal representation equality, guarded earlier catch-alls, and an
      unguarded final catch-all. Return one structured failure and no partial
      program when any check fails.

- [x] **Step 5: Implement the no-ambient arm chain.** Reuse `lowerLiteral` for
      literal operands, the existing primitive call form for strict equality,
      and the RFC 0010 CFG builder for branch/jump/join blocks. Lower the
      scrutinee once before the first test. Preserve repeated literals and reset
      block-local temporary numbering according to existing conventions.

- [x] **Step 6: Implement variable and guard scopes.** Extend
      `LexicalRepresentations` and the active local operand map for exactly one
      arm. Remove that extension before starting the continuation or join. On a
      false guard, transport the original scrutinee—not an arm-local alias—to
      the next arm.

- [x] **Step 7: Run the focused suite twice and verify GREEN.** Confirm exact
      CFG identity on both runs and successful `validateLoweredProgram` output.

- [x] **Step 8: Commit the CFG milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower scalar pattern case control flow"
  ```

### Task 5: Transport nested, captured, and in-flight case values

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`

**Interfaces:**

- Reuse RFC 0010's `ambientSlots`, `ambientParameters`, `ambientArguments`,
  `remapAmbient`, and `carryOperand` helpers; do not introduce a second edge
  transport algorithm for cases.
- Preserve ambient order: local binders by `TypedBinderId`, shared
  environments by `LoweredLayoutId`, then explicit carried operands.
- Carry the scrutinee through every test/guard continuation that needs it.
  Carry one selected body result as the final join parameter.
- Block sequences are deterministic: current test, matched guard/body subtree,
  next-arm subtree, then result join, with nested conditional/case blocks kept
  in their own structural order.

- [x] **Step 1: Add exact nested CFG fixtures.** Cover a case in a conditional
      branch, a conditional in a case guard, a case in a case body, and a case
      as the scrutinee of another case. Exact expectations must prove each
      scrutinee executes once and each join resumes with remapped operands.

- [x] **Step 2: Add ambient transport fixtures.** Cover an earlier scalar used
      in a guard and after the join, a captured scalar used from a closure body,
      a closure-valued case result followed by application, and an in-flight
      call operand whose argument is a case expression.

- [x] **Step 3: Run the focused suite and verify RED.** Expected failures are
      operand-scope, edge-shape, missing generated-function, or exact CFG
      mismatches—not parsing or type inference failures.

- [x] **Step 4: Thread case edges through the shared ambient helpers.** Build
      identical parameter/argument shapes for both successors of each literal
      or guard branch. Remap locals and shared environments on entry to every
      successor. Use `carryOperand` for the scrutinee and result rather than
      inserting them into unordered maps.

- [x] **Step 5: Merge nested block sequences without leaking arm scope.** Use
      the current CFG state APIs to finish/start blocks. Restore only the
      remapped outer maps at continuation and join blocks; never merge an arm's
      variable binder map into them.

- [x] **Step 6: Lock ordered failure behavior.** For source cases containing
      unsupported descendants, verify scrutinee failures precede arm failures,
      and within each arm the pattern/profile failure precedes guard then body
      descendant failures. Keep results deterministic on two runs.

- [x] **Step 7: Run the focused contract gate twice.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: all three suites pass twice with identical exact artifact values.

- [x] **Step 8: Commit the transport/boundary milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "test: lock scalar pattern case boundaries"
  ```

### Task 6: Close contract coverage and public compiler status

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0011-typed-core-scalar-pattern-cases.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-14-jazz-typed-core-scalar-pattern-cases.md`

**Interfaces:**

- Public compiler-boundary docs say bounded scalar cases with guarded
  fallthrough and a required final catch-all are included in the opt-in path.
- The same docs keep managed patterns, pattern lambdas, and exhaustiveness
  explicitly deferred for the dependency reasons recorded by RFC 0011.
- RFC 0011 records the implemented child without claiming normal-pipeline
  cutover or a public exhaustiveness rule.
- Closeout removes the ready row and restores an explicit terminal-empty queue
  state unless another independently accepted successor exists at that time.

- [x] **Step 1: Complete manifest and descendant-failure coverage.** Remove the
      obsolete parent `TypedCorePatternCaseUnsupported` expectations only for
      in-profile cases. Preserve exact failures for managed patterns, pattern
      lambdas, missing syntactic catch-alls, and unsupported descendants.

- [x] **Step 2: Run the focused contract gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: all three suites pass with no warnings or failures.

- [x] **Step 3: Update public and durable documentation.** Explain the shipped
      compiler stage in language terms: ordered scalar selection, arm-local
      binding, guard fallthrough, explicit CFG transport, and result joining.
      State that the final catch-all is an opt-in profile boundary, not static
      exhaustiveness analysis. Do not document repository files as compiler
      stages.

- [x] **Step 4: Close the queue and plan metadata.** Set this plan to
      `status: complete`, remove its ready row, record the completed child in
      the bootstrap blocker, and name no successor unless a separate accepted
      contract exists.

- [x] **Step 5: Run structural gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [x] **Step 6: Commit the closeout milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-14-jazz-typed-core-scalar-pattern-cases.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0011-typed-core-scalar-pattern-cases.md test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "docs: close typed-core scalar pattern cases"
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
  python3 scripts/check-rfcs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  git status --short
  ```

  Expected: all gates exit zero and the worktree has no uncommitted changes.
