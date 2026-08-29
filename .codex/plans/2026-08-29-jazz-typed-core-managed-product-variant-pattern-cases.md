---
id: JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 3"
target_paths:
  - src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs
  - src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Emit.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  - docs/compiler/bootstrapping.md
  - docs/compiler/pipeline.md
  - docs/project/status.md
  - rfcs/accepted/0015-typed-core-managed-products-and-variants.md
  - .codex/execution/queue.md
  - .codex/execution/blocker-contracts.md
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - python3 scripts/check-rfcs.py .
  - git diff --check
deliverable: "Produce constructor, tuple, as-, top-level or-, nested, literal, variable, and wildcard managed case patterns; independently prove backend totality; and lower source-ordered guarded decision trees with tag-before-field projections and arm-local binders without changing schemas, runtime services, or ordinary compile/run."
last_verified: 2026-08-29
---

# Jazz Typed-Core Managed Product and Variant Pattern Cases Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development to implement this plan task-by-task.
> Keep this plan's SDD `progress.md` ledger current after every
> implementation and review handoff. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Complete RFC 0015's second child by making the opt-in Typed Core
producer and Lowered IR lowerer support independently total, source-ordered
pattern cases over managed products and variants.

**Architecture:** Finalization resolves admitted source patterns against the
already-retained structured-value catalog and specializes every arm-local
binder. The centralized managed-layout catalog resolves constructor pattern
layouts. Shape analysis separately admits the bounded backend pattern profile
and proves totality over Typed Core without trusting source coverage. Emission
then lowers each source row into explicit tag tests, guarded field projections,
tuple projections, binder block parameters, guard fallthrough, and the existing
result or tail-result discipline.

**Tech Stack:** Haskell, Typed Core, Lowered IR v1, hosted Jazz contract
fixtures, GHC 9.14.1, Cabal, Nix

**Spec:**
`.codex/plans/2026-08-27-jazz-typed-core-managed-products-variants-design.md`
and `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`

## Global Constraints

- Work only in the opt-in
  `inferResolvedModuleTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall` path. Ordinary compile/run stays on
  canonical core and the reference interpreter.
- Reuse the existing Typed Core pattern nodes, Lowered IR projections,
  `LoweredSwitch`, block parameters, branches, joins, and tail terminators.
  Change no Typed Core schema, Lowered IR schema/version, or hosted validator.
- Admit wildcard, variable, immediate scalar literal, constructor, tuple,
  as-pattern, and top-level or-pattern forms. Constructor and tuple patterns
  may nest those forms except or-patterns.
- Reject list, cons, `Text` literal, nested or-pattern, and pattern-lambda
  behavior at the existing owning boundary. Do not add equality, list
  projection, match-failure services, traps, or unreachable values.
- Preserve source arm order. Test a constructor tag before projecting any of
  its fields. Project tuple fields from the exact product layout.
- Carry every projected binder as an explicit block argument into only its
  selected guard and body. An as-pattern also binds the complete current
  operand. Top-level or-pattern alternatives share the first alternative's
  canonical arm-binder contract.
- Run a guard only after its complete pattern succeeds. A false guard or any
  nested pattern mismatch resumes at the next source row.
- Independently prove totality from admitted Typed Core: unguarded rows cover,
  guarded rows do not; top-level alternatives contribute their union; local
  variants use their closed retained declaration; tuples have one product
  shape; scalar literal domains remain open.
- Fail incomplete otherwise-valid Typed Core with
  `LoweredIRIncompletePatternCase` before emission and return no partial
  artifact. Complete constructor sets need no synthetic wildcard.
- Preserve failure precedence: source diagnostics, producer profile, Typed
  Core validation, lowerer profile, then Lowered IR validation.
- Begin each behavior change with a focused failing exact expectation, observe
  the expected failure, implement the minimum behavior, and rerun the focused
  test before broader verification.
- Commit each green task with the message named below. Do not mix unrelated
  cleanup into the child.

---

### Task 1: Promote the accepted managed-pattern child

**Files:**

- Create: `.codex/plans/2026-08-29-jazz-typed-core-managed-product-variant-pattern-cases.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Empty `Next Curation Target` while this row is executable.
- Keep the bootstrap umbrella blocked on this child until verified closeout.

- [x] **Step 1: Reconfirm the accepted contract and live owners.** Inspect RFC
      0015, its checked-in design, the existing construction fixtures, and the
      producer/lowerer source owners named in frontmatter.

- [x] **Step 2: Prove the focused baseline before promotion.** Run the three
      focused suites serially. If the Nix daemon is unavailable, use the exact
      GHC 9.14.1/Cabal toolchain already present in `/nix/store`, with Cabal's
      build-summary redirected to `/private/tmp`, `--offline`, and `--jobs=1`.
      Expected: all three suites pass before any behavior edit.

- [x] **Step 3: Validate the dispatcher milestone.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  git diff --check
  ```

  Expected: each command exits zero and the queue row exactly matches this
  plan's frontmatter.

- [x] **Step 4: Commit the planning milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-29-jazz-typed-core-managed-product-variant-pattern-cases.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core managed pattern cases"
  ```

### Task 2: Produce exact Typed Core managed patterns and binders

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`

**Interfaces:**

- Extend `finalizePatternCaseArm` through a recursive helper shaped around:

  ```haskell
  finalizePattern
    :: StructuredValueCatalog
    -> InferState
    -> [Int]
    -> ExpressionType
    -> Pattern SourceSpan
    -> Either TypedCoreProductionFailure (TypedPattern, [PatternBinding])
  ```

- Constructor patterns resolve the current local constructor, derive concrete
  field node information and field expression types from the scrutinee's
  concrete variant result, and preserve source field order.
- Tuple patterns derive exact child types from the solved tuple type.
- As-patterns bind the current operand contract and then recurse at child path
  zero. Top-level or-pattern alternatives receive distinct binder IDs, while
  the selected arm expression specializes against the first alternative's
  binder names and expression types.
- Generalize arm specialization from a top-level variable to all canonical
  binders collected recursively. Pattern node infos carry no constructor
  instantiation/evidence payload beyond the exact existing schema contract.

- [x] **Step 1: Add RED exact producer fixtures.** Add complete expected
      `TypedProgram` values and assertions for:

  ```jazz
  data Maybe a = Nothing | Just a.
  subject = Just (41, True).
  case subject {
    | whole @ Just (item, True) -> item.
    | Nothing -> 0.
    | Just (_, False) -> 1.
  }.
  ```

  and:

  ```jazz
  data Choice a = Left a | Right a.
  subject = Right 7.
  case subject {
    | Left item | Right item -> item.
  }.
  ```

  Assert exact pattern paths, binder IDs, concrete recipes, source constructor
  order, and arm-body binder references. First run must fail with the existing
  `TypedCorePatternCaseUnsupported` profile result.

- [x] **Step 2: Implement recursive finalization minimally.** Reuse
      `structuredConstructorAtStatement`, `structuredNodeInfo`,
      `concreteConstructorContract`, and existing specialization helpers.
      Do not rerun inference or duplicate constructor resolution.

- [x] **Step 3: Lock exclusions.** Add focused producer-profile assertions for
      list/cons patterns, `Text` literal patterns, nested or-patterns, and
      pattern lambdas. Each must fail at its established source or producer
      boundary without producing partial Typed Core.

- [x] **Step 4: Verify and commit.** Run the focused managed producer tests,
      then the complete producer suite, and commit:

  ```bash
  git commit -m "feat: produce typed-core managed pattern cases"
  ```

### Task 3: Centralize constructor-pattern layouts and prove backend totality

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`

**Interfaces:**

- Add one catalog-owned constructor-pattern lookup:

  ```haskell
  constructorPatternLayoutFor
    :: ManagedLayoutCatalog
    -> TypedNodeInfo
    -> TypedCoreName
    -> Maybe ManagedConstructorLayout
  ```

  Resolve by current concrete variant identity plus constructor name, specialize
  the retained constructor template, and reuse `constructorLayoutFor`. Do not
  read catalog internals from `Shapes.hs` or `Emit.hs`.
- Replace the scalar-only profile check with recursive managed-pattern
  admission. Every scrutinee and projected child must have one exact admitted
  representation recipe and catalog layout.
- Implement a pure Typed Core coverage matrix local to the lowerer. Treat
  wildcard/variable/as-pattern as catch-alls at their node, expand top-level
  alternatives, decompose the one tuple product shape, enumerate every tag in
  a closed local variant layout, and never treat literal-only rows as closing
  an open scalar domain.
- Collect arm parameters recursively from the first or-pattern alternative;
  preserve deterministic binder order and exact representation recipes.
- `Requirements.hs` must continue to traverse pattern nodes and must add no
  runtime service. Change it only if a failing test exposes a missing existing
  dependency traversal.

- [ ] **Step 1: Add RED boundary fixtures.** Turn the established managed
      pattern boundary case into positive profile expectations. Add arbitrary
      structurally valid Typed Core cases proving:

  - every constructor of a closed variant is total without a wildcard;
  - one constructor missing is `LoweredIRIncompletePatternCase`;
  - a guarded constructor row contributes no coverage;
  - top-level or-pattern alternatives contribute their union;
  - a tuple row is total only when its nested open scalar fields are total;
  - unsupported nested/list/Text forms still fail the lowerer profile.

  First run must fail at the old scalar-only profile boundary.

- [ ] **Step 2: Add the centralized layout lookup and recursive profile.** Keep
      nominal variant identity module-qualified and reuse the deterministic
      catalog ordering already established by the construction child.

- [ ] **Step 3: Implement independent totality.** Do not import or call the
      source `PatternCoverage` proof. Add mutation-oriented assertions that
      would fail if guarded rows counted as coverage or a closed tag were
      omitted.

- [ ] **Step 4: Verify and commit.** Run the managed tests plus Typed Core and
      Lowered IR contract suites, then commit:

  ```bash
  git commit -m "feat: validate managed pattern lowering profile"
  ```

### Task 4: Emit source-ordered managed decision trees

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`

**Interfaces:**

- Generalize scalar-case lowering into a row compiler whose recursive matcher
  receives the current typed pattern, operand, success continuation, and row
  failure continuation.
- A constructor matcher emits `LoweredProjectVariantTag` and terminates with
  `LoweredSwitch`; only its matching block may emit
  `LoweredProjectVariantField`. A tuple matcher emits
  `LoweredProjectField` from the exact product layout.
- Every pattern step that crosses a block edge passes ambient values and matched
  binder operands explicitly. The success block exposes canonical arm binders
  as block parameters and extends `loweringLocalBindings` only while lowering
  that guard/body.
- Compile top-level or-pattern alternatives left to right. Alternative failure
  continues to the next alternative; final alternative failure continues to
  the next source row. Map each successful alternative's values to the first
  alternative's canonical binder IDs.
- Keep the existing scalar literal comparison primitive, result join, and
  function-result tail lowering. Do not synthesize a wildcard, trap, or runtime
  match failure after a total matrix.

- [ ] **Step 1: Add RED exact Lowered IR fixtures.** Assert complete
      `LoweredProgram` values for constructor selection, tuple selection,
      nested constructor/tuple matching, as-patterns, top-level alternatives,
      repeated guarded constructors, false-guard fallthrough, arm-local
      projected binders, a closed constructor set without wildcard, and
      tail-position arm bodies.

  Exact instruction order must prove:

  - the tag projection precedes its switch;
  - no variant field projection occurs before the matching tag block;
  - source rows and alternatives retain source order;
  - guard failure reaches the next source row;
  - projected values cross blocks only as parameters/arguments.

  First run must fail at the existing unsupported-pattern emission branch.

- [ ] **Step 2: Implement tuple and constructor matching.** Reuse the catalog
      lookup from Task 3 and existing block/ambient remapping helpers. Preserve
      deterministic block/temporary allocation.

- [ ] **Step 3: Implement binders, as-patterns, alternatives, and guards.** Add
      mutation-oriented exact assertions for wrong alternative binder mapping,
      field projection before tag selection, and nested failure accidentally
      entering a guard.

- [ ] **Step 4: Preserve result and tail paths.** Run existing scalar pattern
      case fixtures unchanged and add managed tail-body assertions. No managed
      row may introduce an unnecessary result join in true tail position.

- [ ] **Step 5: Verify and commit.** Run the focused managed suite and all
      producer/lowerer contract suites, then commit:

  ```bash
  git commit -m "feat: lower managed product and variant patterns"
  ```

### Task 5: Close the matrix, public status, and dispatcher

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`
- Modify: `.codex/plans/2026-08-29-jazz-typed-core-managed-product-variant-pattern-cases.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

- [ ] **Step 1: Audit the acceptance matrix.** Confirm every positive and
      negative RFC 0015 second-child fixture is assigned exactly once and that
      the existing scalar pattern and managed-construction fixture families
      remain unchanged.

- [ ] **Step 2: Run fresh full verification.** Prefer the checked-in Nix shell:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  git diff --check
  ```

  If the Nix daemon remains unavailable, use the same exact store-backed
  GHC/Cabal fallback as Task 1 for both focused and full suites, and run the
  documentation checks directly. Record that environmental substitution in
  the final handoff.

- [ ] **Step 3: Update durable status accurately.** State that the opt-in
      Typed Core/Lowered IR path now supports managed product/variant patterns,
      independent backend totality, and source-ordered decision trees. Keep
      lists, pattern lambdas, imported/multi-module data, runtime ABI/native
      execution, and ordinary compile/run cutover explicitly unshipped.

- [ ] **Step 4: Close plan and queue only after green evidence.** Set plan
      `status: complete` and `plan_section: "Full closeout"`; remove its Ready
      Now row; update the umbrella blocker with the verified commit and next
      source-backed state; leave no speculative replacement row.

- [ ] **Step 5: Commit closeout.** Run the dispatcher, RFC, docs, and diff
      checks again after the closure edits, then commit:

  ```bash
  git commit -m "docs: close managed product and variant patterns"
  ```

- [ ] **Step 6: Request whole-branch review and finish the branch.** Review the
      complete diff against RFC 0015, repair only validated findings through
      new RED/GREEN cycles, rerun affected focused tests and the full gate, and
      then use `superpowers:finishing-a-development-branch` to present the
      integration choices. Do not push or open a pull request without explicit
      authorization.
