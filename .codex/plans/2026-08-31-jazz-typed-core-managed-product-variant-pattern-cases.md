---
id: JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs
  - src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Types.hs
  - src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Emit.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  - jazz.cabal
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
  - git diff --check
deliverable: "Produce and lower source-ordered, independently total tuple and local-constructor pattern cases with nested projections, arm-local binders, guards, as-patterns, and top-level alternatives without changing Typed Core or Lowered IR schemas."
last_verified: 2026-08-31
---

# Jazz Typed-Core Managed Product and Variant Pattern Cases Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete RFC 0015 by retaining the approved managed pattern subset
in Typed Core and lowering it to source-ordered, independently total Lowered
IR decision trees with tag-before-field projection and arm-local binders.

**Architecture:** Typed Core finalization recursively converts already-typed
source patterns into the existing `TypedPattern` vocabulary, allocating one
canonical binder identity per arm. A focused lowerer-private module validates
the admitted subset, resolves exact managed layouts, and returns a non-empty
checked arm plan; the existing emitter consumes that plan to build ordered
success/failure continuations using existing projections, switches, branches,
and ambient edge transport. Ordinary compile/run stays on canonical core and
the reference interpreter.

**Tech Stack:** Haskell, GHC 9.14.1, `NonEmpty`, `Map`, `Set`, Typed Core,
Lowered IR v1, Cabal, Nix, and the existing Jazz test harness

**Spec:**
`.codex/plans/2026-08-27-jazz-typed-core-managed-products-variants-design.md`
and `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`

## Global Constraints

- Implement only the second ordered RFC 0015 child:
  `JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001`.
- Keep production and lowering opt-in through
  `inferResolvedModuleTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Reuse the existing `TypedPatternCaseExpr`, `TypedCaseArm`,
  `TypedConstructorPattern`, `TypedTuplePattern`, `TypedAsPattern`, and
  `TypedOrPattern` schemas. Do not change Typed Core, hosted Typed Core, Lowered
  IR, hosted Lowered IR, or either IR version.
- Admit wildcard, variable, immediate scalar literal, constructor, fixed-arity
  tuple, as-pattern, and top-level or-pattern forms. Nested constructor and
  tuple patterns are admitted; nested or-patterns are not.
- Keep list and cons patterns, Text literal patterns, pattern lambdas, imported
  data, multiple modules, product/variant equality, runtime services, ABI,
  native execution, and normal compile/run cutover out of scope.
- Evaluate the scrutinee exactly once. Preserve arm order. A nested pattern
  failure or false guard continues to the next source arm.
- Project a variant tag before any variant field. Project tuple fields from the
  exact structural product layout. Project and bind fields left-to-right.
- Allocate one binder identity per arm binder. Top-level or alternatives reuse
  those exact binder identities and pass the same ordered representations to
  one arm-success block.
- Keep projected binders visible only in the selected arm's guard and body.
  An as-pattern binds the complete current operand before matching its child.
- Prove totality independently from source `E2018`. Guarded rows do not cover.
  Complete closed constructor sets and the single tuple shape need no
  synthetic wildcard; scalar literal domains remain open.
- Reject incomplete arbitrary validated Typed Core with
  `LoweredIRIncompletePatternCase` before emitting any partial program.
- Preserve the existing failure precedence and deterministic artifact order.
- Centralize lowerer admission and totality in one pure module. Use
  `NonEmpty` after successful admission; do not add a GADT, singleton layer,
  recursion-scheme framework, or general pattern-pass abstraction.
- Begin each behavior change with a focused failing expectation. Do not add
  tests for facts already enforced by the Typed Core or Lowered IR validators.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Format every touched Haskell or Markdown file and commit each green task with
  the exact message named below.

---

### Task 1: Promote the accepted managed-pattern child

**Files:**

- Create: `.codex/plans/2026-08-31-jazz-typed-core-managed-product-variant-pattern-cases.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Point the bootstrap umbrella at accepted RFC 0015 and this plan.
- Keep the architecture simplification plan unqueued until this child closes.

- [x] **Step 1: Confirm the focused baseline.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: all three suites pass before queue promotion.

- [x] **Step 2: Promote the candidate.** Replace the empty `Ready Now` state
      with this plan's exact frontmatter row. Remove the candidate from `Next
Curation Target`; preserve the completed construction evidence.

- [x] **Step 3: Update the bootstrap blocker.** Name this managed-pattern child
      as executing and retain every explicit RFC 0015 exclusion.

- [x] **Step 4: Validate dispatcher structure.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  git diff --check
  ```

  Expected: all commands exit zero and the queue row matches this plan.

- [x] **Step 5: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-31-jazz-typed-core-managed-product-variant-pattern-cases.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core managed pattern cases"
  ```

### Task 2: Retain managed patterns with canonical arm binders

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ScalarTextTests.hs`

**Interfaces:**

- Add one private recursive finalizer shaped as:

  ```haskell
  finalizeManagedPattern
    :: StructuredValueCatalog
    -> InferState
    -> Int
    -> [Int]
    -> ExpressionType
    -> Map Name TypedBinderId
    -> Pattern
    -> ( [TypedCoreProductionFailure]
       , Maybe TypedPattern
       , Map Name ExpressionType
       )
  ```

- Reuse `concreteConstructorContract` and
  `concreteConstructorFieldTypes`; do not re-run inference or reconstruct
  constructor ownership from source spelling alone.
- Let the provisional producer profile pass only RFC 0015 tuple/data pattern
  roots to finalization; keep scalar cases and every existing exclusion at
  their current owners.
- Allocate every distinct arm binder with the arm pattern path and its resolved
  value name. Top-level or alternatives look up the same binder table.
- Count a `TypedOrPattern` as one logical definition set in global binder
  occurrence validation, using the first alternative exactly as
  `patternBinderNodes` does; retain `validateOrPattern`'s all-alternative
  contract-equality check.
- Specialize guard and body provisional references for every collected binder
  type, not only a root variable pattern.

- [x] **Step 1: Replace the two rejection assertions with RED production
      expectations.** Give `managed-tuple-pattern-failure` and
      `managed-constructor-pattern-failure` exact expected `TypedProgram`
      values containing `TypedTuplePattern` and `TypedConstructorPattern`.
      Keep their lowering expectations absent.

- [x] **Step 2: Add exact nested binder fixtures.** Add source and exact Typed
      Core for:

  ```jazz
  data Option a = None | Some a.
  case Some (1, "one") {
    | Some (number, label) -> number
    | None -> 0
  }.
  ```

  ```jazz
  data Option a = None | Some a.
  case Some 1 {
    | whole @ Some item -> item
    | None -> 0
  }.
  ```

  ```jazz
  data Choice = Left Int | Right Int.
  case Left 1 {
    | Left item | Right item -> item
  }.
  ```

  Assert exact child `TypedNodeInfo`, canonical binder IDs, constructor names,
  and identical binder contracts across or alternatives.

- [x] **Step 3: Add producer exclusions.** Assert the producer still returns
      `TypedCorePatternCaseUnsupported` at the exact nested pattern path for a
      Text literal inside an admitted managed pattern. Retain the established
      list/cons, nested-or grammar, and pattern-lambda rejection coverage at
      their earlier profile owners. Keep ordinary source diagnostics ahead of
      producer failures.

- [x] **Step 4: Run the focused suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: the newly admitted fixtures fail with the current
  `TypedCorePatternCaseUnsupported` boundary.

- [x] **Step 5: Finalize the admitted tree recursively.** For tuple nodes,
      derive each child type from the resolved `TTupleType`; for constructors,
      resolve the visible local `StructuredConstructor` at the case statement
      and specialize its field templates from the concrete scrutinee type.
      Emit child patterns in source order with exact child infos.

- [x] **Step 6: Bind variables, as-patterns, and alternatives once.** Build one
      arm binder table from first semantic occurrence order. Reuse it while
      finalizing every top-level or alternative. Collect each binder's resolved
      `ExpressionType`, then fold `specializeProvisionalParameterReferences`
      over the guard and body before their existing finalization.

- [x] **Step 7: Run the focused suite twice.** Expected: exact Typed Core and
      exclusions are stable on both runs; managed cases still fail closed at
      the lowerer profile.

- [x] **Step 8: Format and commit.** Run the repository formatter for the
      touched files, then:

  ```bash
  git add src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs src/Jazz/Compiler/TypedCore/Validate/Patterns.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ScalarTextTests.hs
  git commit -m "feat: produce typed-core managed patterns"
  ```

### Task 3: Admit layouts and prove managed-case totality

**Files:**

- Create: `src/Jazz/Compiler/LoweredIR/Lower/ManagedPatterns.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `jazz.cabal`

**Interfaces:**

- Add exact catalog pattern contracts:

  ```haskell
  data ManagedPatternConstructor = ManagedPatternConstructor
    { managedPatternConstructorLayout :: ManagedConstructorLayout
    , managedPatternConstructorName :: TypedCoreName
    , managedPatternConstructorFields :: [TypedNodeInfo]
    }

  managedPatternConstructorsFor
    :: ManagedLayoutCatalog
    -> TypedNodeInfo
    -> Maybe [ManagedPatternConstructor]

  managedPatternConstructorFor
    :: ManagedLayoutCatalog
    -> TypedCoreName
    -> TypedNodeInfo
    -> Maybe ManagedPatternConstructor
  ```

- Retain constructor source order in the catalog. Extend private constructor
  templates with field types as well as field recipes so concrete pattern
  field infos are specialized once from the scrutinee's data arguments.
- Introduce a checked lowerer-private plan:

  ```haskell
  data ManagedPattern
    = ManagedWildcard TypedNodeInfo
    | ManagedVariable TypedNodeInfo TypedBinderId
    | ManagedLiteral TypedNodeInfo TypedLiteral
    | ManagedConstructor ManagedPatternConstructor [ManagedPattern]
    | ManagedTuple TypedNodeInfo LoweredLayoutId [ManagedPattern]
    | ManagedAs TypedNodeInfo TypedBinderId ManagedPattern
    | ManagedOr TypedNodeInfo (NonEmpty ManagedPattern)

  data ManagedPatternArm = ManagedPatternArm
    { managedPatternArmPattern :: ManagedPattern
    , managedPatternArmGuard :: Maybe TypedExpr
    , managedPatternArmBody :: TypedExpr
    }

  analyzeManagedPatternCase
    :: ManagedLayoutCatalog
    -> [Text]
    -> [Int]
    -> [Int]
    -> TypedExpr
    -> [TypedCaseArm]
    -> Either LoweredIRLoweringFailure (NonEmpty ManagedPatternArm)
  ```

- `Shapes.inspectExpression` consumes the checked result for admission and
  recursively derives deterministic binder parameter shapes from it.

- [ ] **Step 1: Add RED arbitrary Typed Core boundaries.** Add valid hand-built
      programs for an incomplete constructor set, a guarded-only complete set,
      an incomplete nested tuple/constructor row, unsupported list/Text/nested
      or patterns, and a complete constructor set without a wildcard. Assert
      the exact `LoweredIRIncompletePatternCase` or
      `LoweredIRUnsupportedPattern` path for each rejected program.

- [ ] **Step 2: Add direct pure-analysis contracts.** Assert that successful
      analysis returns a `NonEmpty` source-ordered arm plan, exact constructor
      tags/layouts, left-to-right product children, and one binder contract for
      all top-level or alternatives.

- [ ] **Step 3: Run the focused suite and verify RED.** Expected: the current
      scalar-only checker rejects the valid managed cases and accepts only the
      old final-catch-all totality rule.

- [ ] **Step 4: Extend the catalog for pattern lookup.** Retain ordered
      constructor names, binders, type parameters, field types, and recipes.
      Specialize field infos using the node's concrete `TypedDataType`
      arguments, then reuse `constructorLayoutFor` for the layout and tag.
      Never select by map iteration order.

- [ ] **Step 5: Normalize only the approved grammar.** Convert validated raw
      patterns to `ManagedPattern`; convert top-level alternative lists with
      `NonEmpty.nonEmpty`; reject nested alternatives, lists, Text literals,
      mismatched recipes, missing layouts, and unknown constructors at their
      exact pattern paths.

- [ ] **Step 6: Implement a pure matrix totality check.** Treat wildcard,
      variable, and as-patterns as irrefutable at their current node; a product
      has one constructor; a variant has its catalog's complete ordered tag
      set; scalar literal domains are open. Expand top-level alternatives into
      rows. Add only unguarded rows to coverage. Return the expression-level
      incomplete failure before emission when a wildcard witness remains.

- [ ] **Step 7: Replace `scalarPatternCaseProfileFailures`.** Make Shapes call
      `analyzeManagedPatternCase`, recurse through the checked arm plan for
      exact binder representations, and preserve existing scalar-case behavior
      as a subset.

- [ ] **Step 8: Run the focused suite twice, format, and commit.** Expected:
      pure admission and boundary failures are exact and deterministic; valid
      managed cases proceed to the still-unimplemented emitter boundary.

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs src/Jazz/Compiler/LoweredIR/Lower/ManagedPatterns.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs jazz.cabal
  git commit -m "feat: analyze total managed pattern cases"
  ```

### Task 4: Lower tuple and constructor decision rows

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`

**Interfaces:**

- Replace the scalar-only dispatch with:

  ```haskell
  lowerManagedPatternCaseTo
    :: ResultDestination
    -> [Text]
    -> [Int]
    -> [Int]
    -> TypedCoreValidationPath
    -> TypedNodeInfo
    -> TypedExpr
    -> NonEmpty ManagedPatternArm
    -> FunctionIndex
    -> [FunctionParameterShape]
    -> LoweringState
    -> ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
  ```

- A recursive matcher receives the current operand, one success block, and one
  failure block. It returns projected binder operands in deterministic pattern
  order without mutating outer lexical scope.
- Constructor matching emits `LoweredProjectVariantTag`, then `LoweredSwitch`.
  Only the selected-tag successor emits `LoweredProjectVariantField`.
- Tuple matching emits `LoweredProjectField` in field order from the exact
  product layout. Nested failures jump to the next source row.

- [ ] **Step 1: Add exact RED tuple lowering.** Require one product
      construction, one left-to-right pair of field projections, arm-local
      binder block parameters, one selected body, and the existing result join.

- [ ] **Step 2: Add exact RED variant lowering.** Require the `Option` case to
      emit tag projection before `LoweredSwitch`, project `Some`'s field only
      in tag 1's successor, and cover `None` without a synthetic wildcard or
      default trap.

- [ ] **Step 3: Add nested-failure ordering.** Cover `Some (left, right)` before
      a later `Some fallback` row. Prove a nested tuple mismatch resumes at the
      later row and never evaluates the earlier body.

- [ ] **Step 4: Run the focused suite and verify RED.** Expected: admitted
      managed cases reach `lowerScalarPatternCaseTo` and fail with the existing
      unsupported-pattern emitter path.

- [ ] **Step 5: Generalize source-row continuations.** Keep the once-evaluated
      scrutinee in one carried slot. Give every row an entry block; recursively
      compile its pattern with the row success block and the next row entry as
      continuations. Reuse existing ambient slot, remapping, block completion,
      and join helpers.

- [ ] **Step 6: Emit tag and field operations in the required order.** The tag
      temporary dominates the switch. Start each matching tag block before
      emitting fields. Carry projected fields through nested tests to the
      selected arm entry; discard them on failure.

- [ ] **Step 7: Bind projected operands only at success.** Add the checked
      binder operands to `loweringLocalBindings` after the complete pattern
      succeeds. Do not add them to continuation or join states.

- [ ] **Step 8: Run the focused suite twice, format, and commit.** Expected:
      tuple, constructor, nested, complete-variant, and every existing scalar
      case lower to exact valid IR on both runs.

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower/Emit.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  git commit -m "feat: lower managed pattern decision rows"
  ```

### Task 5: Complete as-pattern, alternative, guard, and tail transport

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`

**Interfaces:**

- An as-pattern adds the current operand to the ordered binder payload before
  recursively matching its child.
- Every top-level alternative has the same success block parameters; all
  alternative failures continue to the next alternative, then to the next
  source arm.
- Guards run only from the complete-pattern success block. False guards enter
  the next source arm with outer ambient values and the original scrutinee.
- `FinishFunction` bodies keep existing direct/closure tail-result lowering;
  `ProduceValue` bodies keep the one-result join.

- [ ] **Step 1: Add RED exact as-pattern lowering.** Assert both the complete
      scrutinee and nested projected field arrive as distinct arm parameters
      and are usable in the guard/body only.

- [ ] **Step 2: Add RED exact top-level-or lowering.** Use alternatives sharing
      one binder. Assert each alternative reaches the same arm-success block
      with the same ordered parameter IDs and representations.

- [ ] **Step 3: Add guard and source-order regressions.** Cover repeated
      constructors whose earlier guard is false, a nested pattern failure, and
      a later catch-all. Assert only selected guards/bodies run and fallthrough
      preserves source order.

- [ ] **Step 4: Add ambient and tail fixtures.** Cover a captured scalar in a
      managed arm, a managed case nested inside another managed arm, a
      closure-valued result followed by application, and managed cases in
      direct and closure function-result position. Lock exact edge arguments
      and tail terminators.

- [ ] **Step 5: Run the focused suite and verify RED.** Expected failures are
      exact block-parameter, scoping, edge, or tail-CFG differences.

- [ ] **Step 6: Compile alternatives into one arm success contract.** Chain
      alternatives in authored order. Remap their agreed binder operands to
      the common success parameters. The final alternative failure enters the
      next source row.

- [ ] **Step 7: Restore outer scope on every failure edge.** Reuse the original
      control-slot parameter list for nested failure and false guard. Never
      transport an arm-local binder into the next row or result join.

- [ ] **Step 8: Preserve runtime requirement discovery.** Verify recursive
      pattern traversal continues to discover only existing managed layouts
      and runtime services; add no pattern-specific runtime requirement.

- [ ] **Step 9: Run the three-suite gate twice, format, and commit.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: all suites pass twice with exact deterministic artifacts.

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower/Emit.hs src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  git commit -m "test: lock managed pattern transport"
  ```

### Task 6: Close RFC 0015 and restore architecture execution

**Files:**

- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-31-jazz-typed-core-managed-product-variant-pattern-cases.md`
- Modify: `.codex/plans/2026-08-31-jazz-compiler-architecture-simplification.md`

**Interfaces:**

- Public compiler docs describe managed tuple and local-constructor selection
  as an opt-in backend stage, not a public language semantic change.
- RFC 0015 records both ordered children complete and retains every non-goal.
- This plan becomes `status: complete`; its ready row is removed.
- Promote only `JN-COMPILER-CORE-IDENTITY-TYPES-001`, the first milestone of
  the already-approved architecture simplification plan.

- [ ] **Step 1: Run the focused contract gate.** Run the Task 5 three-suite
      command once more. Expected: all suites pass.

- [ ] **Step 2: Update compiler and RFC status.** Document source-ordered
      matching, independent totality, tag-before-field projection, and
      arm-local guard/body binding. Keep lists, imported data, runtime ABI,
      native execution, and normal compile/run cutover explicitly excluded.

- [ ] **Step 3: Close the managed child.** Mark this plan complete, remove its
      ready row, and record its verified commit in the bootstrap blocker.

- [ ] **Step 4: Re-run architecture Task 1 baseline.** Run the architecture
      plan's eleven-suite behavioral matrix against the completed child.

- [ ] **Step 5: Promote the architecture's first milestone.** Add the exact
      `JN-COMPILER-CORE-IDENTITY-TYPES-001` row specified by architecture Task 1. Do not queue later architecture milestones concurrently.

- [ ] **Step 6: Run structural and documentation gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [ ] **Step 7: Format and commit closeout.** Run:

  ```bash
  git add docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0015-typed-core-managed-products-and-variants.md .codex/execution/queue.md .codex/execution/blocker-contracts.md .codex/plans/2026-08-31-jazz-typed-core-managed-product-variant-pattern-cases.md .codex/plans/2026-08-31-jazz-compiler-architecture-simplification.md
  git commit -m "docs: close typed-core managed pattern cases"
  ```

### Full closeout

- [ ] **Step 1: Run the complete serialized suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every suite passes with zero failures.

- [ ] **Step 2: Re-run final evidence against committed HEAD.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  git status --short
  ```

  Expected: all gates exit zero and the worktree is clean.

- [ ] **Step 3: Resume the architecture simplification plan.** Continue from
      Task 2 under subagent-driven execution; do not ask the user to reapprove
      the already-approved architecture design or execution mode.
