---
id: JN-BOOTSTRAP-TYPED-CORE-SCALAR-BINDING-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full plan"
target_paths:
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
verification:
  - cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce concrete scalar bindings in typed core and lower their initializers once for ordered reuse by later entry expressions while retaining unsupported managed bindings."
last_verified: 2026-08-12
---

# Jazz Typed-Core Scalar Binding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer so
concrete scalar `let` bindings are produced with exact binder references,
evaluated once in source order, and reused by later entry expressions.

**Architecture:** The shared inference traversal emits a provisional scalar
binding only when the inferred value already fits the bounded scalar profile.
Typed-core finalization walks root statements in order, threads prior scalar
binder identities into later scalar initializers and terminal expressions, and
continues to finalize function bodies without those bindings so lexical capture
stays rejected. The lowerer distinguishes callable definitions from scalar
locals, lowers each scalar initializer into the entry block exactly once, and
stores its binder-indexed operand in entry-local lowering state for later reuse.

**Tech Stack:** Haskell, GHC 9.14.1, Cabal, backend-neutral Lowered IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-SCALAR-BINDING-001` from accepted
  RFC 0009.
- Keep production and lowering opt-in through
  `finalizeTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept concrete `Unit`, `Bool`, `Char`, `Int`, fixed numeric-width, and
  `Float` scalar bindings, including explicitly signed bindings when their
  inferred type is concrete.
- Evaluate each accepted initializer once in source order and resolve later
  references by `TypedBinderId`, never by textual-name lookup in the lowerer.
- Keep function-body scalar references classified as lexical capture; this
  child does not add inline/nested lambdas, capture layouts, or lambda lifting.
- Retain structured rejection for `Text`, lists, non-unit tuples, ADTs,
  unresolved or polymorphic values, callable aliases, control flow, nested
  blocks, imports, recursion, partial application, and oversaturation.
- Preserve every existing scalar-expression, direct-call, closure-call, typed
  validator, lowered validator, and ordered-failure fixture.
- Add no Typed Core or Lowered IR constructor, version, compatibility pattern,
  fallback ABI, cache, or parallel name-based local environment.
- Run Cabal only inside the checked-in Nix development shell and use
  `--jobs=1`.
- Commit after each green milestone using the commit message named below.

---

### Task 1: Promote the corrected executor-ready child

**Files:**

- Create: `.codex/plans/2026-08-12-jazz-typed-core-scalar-binding.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`

**Interfaces:**

- Add `src/Jazz/Compiler/TypeInference/Scope.hs` to the child ownership matrix;
  this is where non-callable `SLet` statements currently become provisional
  unsupported statements.
- Promote the candidate into `Ready Now` with frontmatter exactly matching this
  plan and remove it from `Next Curation Target`.
- Keep lexical capture as the next RFC-ordered child, but do not pre-promote it
  while scalar binding is active.

- [x] **Step 1: Correct RFC 0009 ownership and the blocker handoff.** Add
  `TypeInference/Scope.hs` to the scalar-binding required paths and make the
  blocker text point at execution of the validated ready child.

- [x] **Step 2: Promote the queue row.** Use priority `P1`, size `M`, kind
  `impl`, `autonomous_ready: yes`, the five exact implementation/test paths in
  this plan frontmatter, and the four exact verification commands above.

- [x] **Step 3: Validate plan metadata and queue consistency.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero.

- [x] **Step 4: Commit the ready milestone.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-scalar-binding.md .codex/execution/queue.md .codex/execution/blocker-contracts.md rfcs/accepted/0009-typed-core-closure-and-recursion.md
  git commit -m "docs: ready typed-core scalar binding"
  ```

### Task 2: Produce ordered scalar bindings with binder references

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Add `ProvisionalScalarBinding Int Name SourceSpan ExpressionType
  ProvisionalTypedExpr` to `ProvisionalTypedStatement`.
- Emit it for a non-callable root `SLet` only when inference retained a
  provisional expression and reported no profile failures.
- Replace independent statement mapping during finalization with an ordered
  fold that threads `Map Name TypedBinderId` for accepted root scalar bindings.
- Finalize scalar signatures with `valueInfo` and `Nothing` callable shape;
  keep callable signatures on `callableInfo`.
- Pass prior scalar binders into later scalar initializers and terminal
  expressions, but start every named function body with an empty scalar map so
  existing capture failures remain unchanged.

- [x] **Step 1: Add exact producer fixtures before implementation.** Add source
  fixtures and hand-derived `TypedProgram` expectations for:

  ```jazz
  seed = 40.
  seed + 2.
  ```

  ```jazz
  seed :: Int.
  seed = 40.
  answer = seed + 2.
  answer.
  ```

  ```jazz
  identity :: Bool -> Bool.
  identity = \(item) -> item.
  answer = identity True.
  answer.
  ```

  The expected artifacts must give each scalar declaration its statement-path
  binder, use `Nothing` callable shape in scalar schemes, put the prior binder
  on every scalar variable node, and retain exact source order.

- [x] **Step 2: Keep negative boundaries explicit.** Preserve the current
  capture rejection for a function body that reads a prior scalar and add a
  root managed-binding fixture whose `Text` initializer and later use remain
  rejected without producing a partial typed program.

- [x] **Step 3: Run the producer suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the new scalar-binding fixtures report
  `TypedCoreUnsupportedRootExpression`; all pre-existing cases retain their
  prior result.

- [x] **Step 4: Emit provisional scalar bindings.** In `Scope.hs`, select the
  new constructor only for a non-function `SLet` with a concrete retained
  expression and no production failures. Preserve the existing unsupported
  constructors for managed, structured, unresolved, callable-alias, and other
  rejected values.

- [x] **Step 5: Finalize statements in source order.** In `Elaboration.hs`,
  finalize scalar schemes and initializer expressions, add successful scalar
  binders to the ordered root-local map, and resolve later variable nodes to
  those binders. Do not make the map visible inside function binding bodies.

- [x] **Step 6: Run the focused suite twice and verify GREEN.** Run the Step 3
  command twice. Expected: both repetitions pass with identical ordered typed
  programs and exact rejection lists.

- [x] **Step 7: Commit the producer milestone.**

  ```bash
  git add src/Jazz/Compiler/TypeInference/Scope.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: produce typed scalar bindings"
  ```

### Task 3: Lower scalar locals once for ordered entry reuse

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Extend private `LoweringState` with
  `Map TypedBinderId LoweredOperand` entry-local bindings.
- Classify a `TypedLetStatement` as a function only when its scheme carries a
  callable type and shape; accept a concrete scalar scheme/expression as an
  entry-local statement rather than `LoweredIRInvalidFunctionShape`.
- In `emitEntry`, lower each scalar initializer at expression path `[0]`, then
  insert its result operand under the declaration binder before continuing.
- In `lowerExpression`, resolve an exact binder from the entry-local map before
  parameter and function lookup, and require representation equality with the
  variable node.
- Initialize function lowering with an empty local map so no scalar capture is
  introduced accidentally.

- [ ] **Step 1: Promote scalar lowerer boundaries to exact success fixtures.**
  Replace the current `invalid-function-shape` scalar program with exact
  expected lowered programs for a literal binding, derived ordered reuse, and
  a direct-call result binding. Assert instruction order and final return
  operands literally.

- [ ] **Step 2: Retain exact lowerer failures.** Keep a scalar initializer with
  unsupported control flow and a managed scalar binding as rejected cases;
  update only the failure kinds displaced by recognizing concrete scalar lets.

- [ ] **Step 3: Run the focused suite and verify RED.** Run the Task 2 Step 3
  command. Expected: scalar valid programs still report
  `LoweredIRInvalidFunctionShape` and do not match the expected entry blocks.

- [ ] **Step 4: Separate scalar locals from function shapes.** Change
  `collectFunctionShapes` so concrete non-callable `TypedLetStatement` values
  remain visible to profile validation without entering the function index or
  generated-identity checks.

- [ ] **Step 5: Thread binder-indexed entry locals.** Lower each scalar
  initializer once, update the local map only after a successful operand is
  produced, and reuse the exact operand for later variables, binary operations,
  direct-call arguments, and the terminal result.

- [ ] **Step 6: Run the focused suite twice and verify GREEN.** Run the Task 2
  Step 3 command twice. Expected: both repetitions pass with stable instruction
  ordering and all prior direct/closure fixtures unchanged.

- [ ] **Step 7: Commit the lowerer milestone.**

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: lower ordered scalar bindings"
  ```

### Task 4: Close the child and verify the repository boundary

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-typed-core-scalar-binding.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`

**Interfaces:**

- Mark this plan complete only after fresh focused and documentation gates.
- Remove the completed row from `Ready Now`, promote only lexical capture into
  `Next Curation Target`, and update the blocker handoff to name that child.
- State publicly that the opt-in profile supports concrete ordered scalar
  bindings while lexical capture and all later RFC 0009 children remain absent.

- [ ] **Step 1: Run focused compiler verification.** Run the Task 2 Step 3
  command twice after the final source edit.

- [ ] **Step 2: Update public compiler-boundary pages.** Add concrete scalar
  bindings and ordered entry reuse to the supported opt-in profile; retain the
  normal compile/run disclaimer and the exclusions for anonymous/nested
  closures, lexical capture, currying, partial application, oversaturation,
  and recursion.

- [ ] **Step 3: Synchronize plan, queue, and blocker state.** Check every task
  box, set frontmatter `status: complete` and `autonomous_ready: no`, use plan
  section `Full closeout`, remove the ready row, and add lexical capture as the
  sole next curation candidate using RFC 0009's exact target paths and G3 gate.

- [ ] **Step 4: Run closeout gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [ ] **Step 5: Perform the anti-slop review.** Enumerate every new constructor,
  helper, state field, branch, and fallback in the diff. Remove anything that
  lacks a concrete scalar-production or binder-indexed-lowering responsibility.

- [ ] **Step 6: Commit closeout.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-scalar-binding.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md
  git commit -m "docs: close typed-core scalar binding"
  ```
