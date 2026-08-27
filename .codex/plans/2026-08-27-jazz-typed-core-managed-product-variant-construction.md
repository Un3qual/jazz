---
id: JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-CONSTRUCTION-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/TypeInference/Elaboration/Types.hs
  - src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs
  - src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs
  - src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Types.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
  - src/Jazz/Compiler/LoweredIR/Lower/Emit.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/Source.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/BoundaryTests.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/CallTests.hs
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
deliverable: "Produce and lower non-unit tuples plus exactly saturated local ADT constructors through deterministic product and variant layouts and every established managed-value transport boundary without adding managed pattern compilation."
last_verified: 2026-08-27
---

# Jazz Typed-Core Managed Product and Variant Construction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make non-unit tuples and exactly saturated local ADT constructors a
complete second managed-data family in the opt-in Typed Core and Lowered IR
path, including deterministic layouts and transport through all established
value boundaries.

**Architecture:** The single inference traversal retains tuple children and
resolved local data declarations. A focused structured-value finalization
module converts those contracts into existing Typed Core nodes and exact
constructor instantiations. A separate managed-layout catalog converts
concrete product and nominal variant recipes into deterministic Lowered IR
layouts, while the existing shape analysis and emitter transport and construct
their managed references. Ordinary compile/run remains on canonical core and
the reference interpreter.

**Tech Stack:** Haskell, Typed Core, Lowered IR v1, hosted Jazz contract
fixtures, GHC 9.14.1, Cabal, Nix

**Spec:**
`.codex/plans/2026-08-27-jazz-typed-core-managed-products-variants-design.md`
and `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`

## Global Constraints

- Implement only the first ordered child of accepted RFC 0015:
  `JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-CONSTRUCTION-001`.
- Keep production and lowering opt-in through
  `inferResolvedModuleTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains unchanged.
- Reuse `TypedDataStatement`, `TypedTupleExpr`, resolved constructor
  `TypedVariableExpr`/`TypedApplyExpr` spines,
  `TypedManagedProductRecipe`, and `TypedManagedVariantRecipe`.
- Reuse `LoweredProductLayout`, `LoweredVariantLayouts`,
  `LoweredConstructProduct`, and `LoweredConstructVariant`; do not change
  Typed Core, Lowered IR, or hosted Jazz schemas or validators.
- Support tuple expressions with two or more elements. Unit keeps
  `TypedUnitRecipe` and `LoweredUnitImmediate`.
- Support local nullary constructors and exactly saturated local constructor
  applications. Reject bare or partial non-nullary constructors at the
  producer boundary; retain ordinary source-diagnostic precedence for
  oversaturation.
- Support concrete generic, recursive, and mutually recursive local ADTs whose
  required fields contain only admitted scalars, Text, closures, products, or
  variants.
- Evaluate tuple elements and constructor fields exactly once from left to
  right before emitting one construction instruction.
- Product identity is structural over ordered concrete field recipes. Variant
  identity is nominal over current module path, resolved local type name, and
  ordered concrete type arguments. Both use versioned, length-prefixed semantic
  encodings without hashes, spans, absolute paths, target details, or process
  state.
- Emit runtime catalog layouts first, managed products/variants in first
  semantic discovery order second, and closure environments last. Reserve an
  identity before traversing recursive dependencies.
- Transport product and variant references through bindings, parameters,
  results, captures, direct/closure calls, recursive environments,
  conditionals, scalar-case results, joins, returns, and tail operands.
- Keep constructor/tuple pattern compilation, lists, list patterns, Text
  literal patterns, equality over products or variants, first-class
  constructors, pattern lambdas, imported data, multiple modules, runtime ABI,
  native execution, and normal compile/run cutover out of scope.
- Preserve failure order: source diagnostics, producer profile, Typed Core
  invariants, lowerer profile, Lowered IR invariants. Emit no partial artifact.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Begin each behavior change with a focused failing exact expectation and
  commit each green task with the message named below.

---

### Task 1: Accept and promote the first RFC 0015 child

**Files:**

- Create: `.codex/plans/2026-08-27-jazz-typed-core-managed-product-variant-construction.md`
- Move: `rfcs/proposed/0015-typed-core-managed-products-and-variants.md` to
  `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`
- Modify: `rfcs/README.md`
- Modify: `.codex/plans/2026-08-27-jazz-typed-core-managed-products-variants-design.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row.
- Keep `Next Curation Target` empty while this child is executable.
- Leave managed pattern compilation behind the completed-construction gate.

- [x] **Step 1: Record maintainer acceptance.** Set RFC 0015 to `Accepted`,
      move it into `rfcs/accepted/`, index it in `rfcs/README.md`, and mark the
      design approved for implementation planning.

- [x] **Step 2: Confirm the focused baseline.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: all three suites pass serially before the queue row is promoted.

- [x] **Step 3: Add this implementation plan and matching queue row.** Copy
      `id`, `priority`, `size`, `kind`, `autonomous_ready`, `depends_on`,
      `plan_section`, ordered `target_paths`, `deliverable`, `verification`, and
      `last_verified` exactly between frontmatter and `Ready Now`.

- [x] **Step 4: Update the umbrella blocker.** Name this accepted child as the
      current smallest unblocker and keep lists, managed patterns, imports,
      runtime ABI, and native execution explicitly excluded.

- [x] **Step 5: Validate and commit the planning milestone.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  python3 scripts/check-rfcs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  git diff --check
  git add .codex/plans/2026-08-27-jazz-typed-core-managed-product-variant-construction.md .codex/plans/2026-08-27-jazz-typed-core-managed-products-variants-design.md .codex/execution/queue.md .codex/execution/blocker-contracts.md rfcs/README.md rfcs/accepted/0015-typed-core-managed-products-and-variants.md
  git commit -m "docs: ready typed-core managed products and variants"
  ```

### Task 2: Retain resolved tuple and local-data structure once

**Files:**

- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/Source.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/Support.hs`
- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs`

**Interfaces:**

- Add these private inference contracts:

  ```haskell
  data ProvisionalConstructorDeclaration
    = ProvisionalConstructorDeclaration Name [ExpressionType]

  data ProvisionalDataDeclaration
    = ProvisionalDataDeclaration
        Int
        SourceSpan
        Name
        [Name]
        [ProvisionalConstructorDeclaration]

  data ProvisionalTypedExpr
    = ...
    | ProvisionalTupleExpression ExpressionType [ProvisionalTypedExpr]

  data ProvisionalTypedStatement
    = ...
    | ProvisionalDataStatement ProvisionalDataDeclaration
  ```

- `ProvisionalConstructorDeclaration` is built from the
  `ConstructorTypeBinding` registered by the existing inference pass. Scope
  converts each constructor field to an `ExpressionType` template while the
  declaration's parameter map is still available; finalization never sees or
  reparses `SignatureType`.
- Every traversal in `Specialize.hs` and `Profiles.hs` visits tuple children in
  source order and treats data statements as declaration metadata, not value
  expressions.

- [x] **Step 1: Add exact RED source cases.** Register these sources under
      stable fixture names and add them to the new focused test module:

  ```jazz
  (1, "two").
  ```

  ```jazz
  data Option a = None | Some a.
  Some 7.
  ```

  ```jazz
  data Tree a = Leaf a | Branch (Tree a) (Tree a).
  Branch (Leaf 1) (Leaf 2).
  ```

  The tests first assert repeatable inference compatibility and then compare
  `typedCoreProductionStatus` with complete expected `TypedProgram` values;
  keep the existing `non-unit-tuple` and `data-value` rejection entries until
  Tasks 2 and 3 turn them green.

- [x] **Step 2: Run the focused suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: the new tuple case reports `TypedCoreTupleValueDetail`; the data
  cases report `TypedCoreDataValueDetail` before finalization exists.

- [x] **Step 3: Retain tuple children in the shared traversal.** Replace the
      unconditional non-unit tuple failure with:

  ```haskell
  let (expressionType, finalState, elementResults) =
        inferTupleWithProduction state elements
      failures = concat (zipWith childFailures [0 ..] elementResults)
      provisional = do
        tupleType <- expressionType
        children <- traverse inferredProvisionalExpr elementResults
        pure (ProvisionalTupleExpression tupleType children)
   in InferredExpr expressionType provisional failures
  ```

  Keep `EList` unchanged. Preserve child paths and left-to-right state
  threading from `inferTupleWithProduction`.

- [x] **Step 4: Retain local data declarations from registered bindings.** In
      the `SData` branch, build `ProvisionalDataStatement` only after
      `registerDataConstructors` succeeds. Resolve each source constructor by
      name in `nextEnv`. Convert its registered `ConstructorArgumentType`
      values to `ExpressionType` templates with declaration parameters bound
      to fixed negative variables in source order; use
      `instantiateConstructorFieldType` for structured fields and reject
      `ConstructorArgumentFresh`. Remove the root block's synthetic data
      failure in `inferExprTypeWithMode`; nested blocks remain rejected by the
      existing nested-scope boundary.

- [x] **Step 5: Make private traversals total.** Add tuple recursion and data
      statement handling to specialization, free-name, application-type,
      parameter-reference, capture, and recursive-dependency traversals. Use
      `map child elements` or `foldMap child elements`; never flatten or reorder
      tuple children.

- [x] **Step 6: Run the focused suite to the expected intermediate boundary.**
      Expected: source inference and provisional retention are repeatable;
      finalization still rejects the new nodes through one explicit structured
      failure rather than losing their children.

- [x] **Step 7: Commit the green retention milestone.** Run:

  ```bash
  git add jazz.cabal src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Scope.hs src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/Source.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/Support.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  git commit -m "feat: retain provisional managed products and variants"
  ```

### Task 3: Finalize exact data, tuple, and constructor artifacts

**Files:**

- Create: `src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `jazz.cabal`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/Support.hs`

**Interfaces:**

- The new module owns this focused catalog:

  ```haskell
  data StructuredConstructor = StructuredConstructor
    { structuredConstructorSourceName :: Name
    , structuredConstructorBinder :: TypedBinderId
    , structuredConstructorName :: TypedCoreName
    , structuredConstructorDataName :: TypedCoreName
    , structuredConstructorParameters :: [TypedTypeParameterId]
    , structuredConstructorFieldTypes :: [TypedType]
    , structuredConstructorFieldRecipes :: [TypedRepresentationRecipe]
    , structuredConstructorTag :: Integer
    }

  data StructuredValueCatalog

  buildStructuredValueCatalog
    :: [Text]
    -> InferState
    -> [ProvisionalTypedStatement]
    -> Either [TypedCoreProductionFailure] StructuredValueCatalog

  structuredDataStatement
    :: StructuredValueCatalog -> Int -> Maybe TypedStatement

  structuredNodeInfo
    :: StructuredValueCatalog
    -> InferState
    -> ExpressionType
    -> Maybe TypedNodeInfo

  structuredConstructorBySourceName
    :: StructuredValueCatalog -> Name -> Maybe StructuredConstructor
  ```

- Type-parameter IDs are zero-based declaration order. Constructor binders use
  `[statementIndex, constructorIndex]` and the constructor namespace. Data
  names use the type namespace; constructor names use the constructor
  namespace.
- `structuredNodeInfo` maps concrete `TTupleType` to
  `TypedManagedProductRecipe` and concrete `TDataType` to
  `TypedManagedVariantRecipe`. Lists, unresolved variables, or recipes that
  mention excluded values return `Nothing`.

- [x] **Step 1: Complete exact expected Typed Core fixtures.** Spell out:

  ```haskell
  TypedDataStatement
    (TypedDataDeclaration span typeName [TypedTypeParameterId 0]
      [ TypedConstructorDeclaration noneBinder noneName [] []
      , TypedConstructorDeclaration someBinder someName
          [TypedTypeParameterType (TypedTypeParameterId 0)]
          [TypedRepresentationParameterRecipe (TypedTypeParameterId 0)]
      ])
  ```

  For `Some 7`, require a constructor callee with
  `TypedInstantiation someBinder [TypedTypeArgument parameter TypedIntType]
Nothing`, followed by one `TypedApplyExpr` whose result recipe is the exact
  nominal variant recipe. Add complete expectations for the recursive tree and
  the structural tuple.

- [x] **Step 2: Run the focused producer test and verify RED.** Expected: the
      exact new programs differ because `Finalize.hs` still accepts only unit,
      scalar, Text, closure, and existing statement forms.

- [x] **Step 3: Build the catalog without a second inference pass.** Convert
      retained `ExpressionType` templates using declaration parameter IDs and
      already-resolved inference structure. Reject any unresolved template or
      non-concrete required recipe with the original statement path. Reserve
      all declarations before resolving fields so recursive and mutually
      recursive data can refer to each other.

- [x] **Step 4: Emit data statements and interfaces in source order.** Add
      `ProvisionalDataStatement` to `finalizeStatement`; return the exact
      `TypedDataStatement` and do not add a value binding. Extend
      `finalizeExports` so public local types and constructors produce matching
      `TypedModuleExport` entries and one `TypedDataInterface`, while imported
      data remains blocked at the input profile.

- [x] **Step 5: Finalize tuple expressions.** Finalize children at
      `childPath <> [elementIndex]`, require the tuple node's type and recipe to
      match every finalized child, and emit:

  ```haskell
  TypedTupleExpr
    (TypedNodeInfo tupleType (TypedManagedProductRecipe childRecipes) [] [])
    typedChildren
  ```

- [x] **Step 6: Finalize exactly saturated constructors.** Before the generic
      named-call branches in `finalizeApplicationSpine`, detect a local
      `StructuredConstructor`. Require `actualArity == fieldCount`, specialize
      its parameter map from the concrete result `TDataType`, finalize fields
      left to right against the specialized field types, and build the existing
      staged `TypedVariableExpr`/`TypedApplyExpr` spine. A nullary constructor
      finalizes directly to a bound constructor `TypedVariableExpr` carrying
      the concrete variant node info.

- [x] **Step 7: Lock producer failures.** Add exact cases for a bare and a
      partial non-nullary constructor (`TypedCoreCallableValueUnsupported` and
      `TypedCoreCallArityUnsupported`), a list field, unresolved recipe, and an
      imported constructor. Keep oversaturation at
      `TypedCoreProductionBlockedByDiagnostics`.

- [x] **Step 8: Run producer and Typed Core contract suites.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: exact producer artifacts pass both Haskell and hosted Typed Core
  validation without any schema or mirror change.

- [x] **Step 9: Commit the green Typed Core milestone.** Run:

  ```bash
  git add jazz.cabal src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/Support.hs
  git commit -m "feat: produce typed-core managed products and variants"
  ```

### Task 4: Catalog deterministic managed layouts

**Files:**

- Create: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `jazz.cabal`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`

**Interfaces:**

- The new module exports only semantic layout operations:

  ```haskell
  data ManagedConstructorLayout = ManagedConstructorLayout
    { managedConstructorLayoutId :: LoweredLayoutId
    , managedConstructorTag :: Integer
    , managedConstructorFields :: [LoweredRepresentation]
    }

  data ManagedLayoutCatalog

  collectManagedLayoutCatalog
    :: TypedModule
    -> Either [LoweredIRLoweringFailure] ManagedLayoutCatalog

  orderedManagedLayouts :: ManagedLayoutCatalog -> [LoweredLayout]

  representationForRecipe
    :: ManagedLayoutCatalog
    -> TypedRepresentationRecipe
    -> Maybe LoweredRepresentation

  constructorLayoutFor
    :: ManagedLayoutCatalog
    -> TypedBinderId
    -> [TypedInstantiation]
    -> Maybe ManagedConstructorLayout
  ```

- Add `analyzedManagedLayoutCatalog :: ManagedLayoutCatalog` to
  `LoweringAnalysis`. `Shapes` uses `representationForRecipe`; `Emit` consumes
  the same catalog from the analysis. There is one identity owner.
- Product IDs start with `jazz.layout.product.v1`; variant IDs start with
  `jazz.layout.variant.v1`. Every count and segment is decimal and
  length-prefixed. Product encoding recursively includes ordered recipe
  encodings; variant encoding includes module segments, resolved type name,
  and ordered concrete typed-argument encodings.

- [x] **Step 1: Add exact catalog RED tests.** Construct validated Typed Core
      programs directly and assert exact `LoweredLayoutId` text plus complete
      `LoweredLayout` order for:

  - duplicate `(Bool, Text)` products sharing one layout;
  - two distinct local data names with identical fields remaining distinct;
  - `Option Bool` and `Option Text` remaining distinct;
  - a recursive `Tree Int` reserving one variant identity without expansion;
  - mutually recursive variants terminating; and
  - Text first, managed values in discovery order, closure environments last.

- [x] **Step 2: Run the focused test and verify RED.** Expected: product and
      variant recipes still fail `loweredRepresentation` and no managed layouts
      are emitted.

- [x] **Step 3: Implement the canonical encoder.** Use explicit encoders:

  ```haskell
  segment value = decimal (Text.length value) <> ":" <> value
  sequenceValue label values =
    label <> decimal (length values) <> foldMap (("$" <>) . segment) values
  ```

  Encode semantic constructors, not `show`; reject a current-module variant
  recipe if its declaration cannot be found. Keep product and closure domains
  distinct even when field representations match.

- [x] **Step 4: Collect first discovery deterministically.** Traverse module
      interface, statements, schemes, expressions, and patterns in stored
      order. Mark an identity before visiting its dependent field recipes.
      Deduplicate with `Set LoweredLayoutId` while retaining an ordered list;
      never iterate a `Map` to establish output order.

- [x] **Step 5: Build exact variant shapes.** Zip declaration parameters with
      concrete recipe arguments, substitute every constructor field type and
      recipe, and emit `LoweredVariantLayout tag fields` with zero-based
      declaration tags. Reject missing declarations, arity mismatch, unbound
      representation parameters, lists, or imported data without a partial
      catalog.

- [x] **Step 6: Thread one catalog through analysis and emission.** Replace
      scalar-only representation lookups in value schemes, callable shapes,
      captures, CFG values, result representations, and emitted operands with
      `representationForRecipe catalog`. Emit layouts as:

  ```haskell
  requiredRuntimeLayouts runtimeRequirements
    <> orderedManagedLayouts managedCatalog
    <> orderedClosureLayouts functionShapes
  ```

- [x] **Step 7: Run focused and contract verification.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: catalog tests pass; all existing exact Text/closure layouts retain
  their prior order; Haskell and hosted Lowered IR validation still match.

- [x] **Step 8: Commit the green catalog milestone.** Run:

  ```bash
  git add jazz.cabal src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs src/Jazz/Compiler/LoweredIR/Lower/Emit.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs
  git commit -m "feat: catalog managed product and variant layouts"
  ```

### Task 5: Lower construction exactly once and left to right

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`

**Interfaces:**

- Add one emitter helper that preserves state order:

  ```haskell
  lowerExpressionsLeftToRight
    :: ManagedLayoutCatalog
    -> [Text]
    -> [Int]
    -> [Int]
    -> FunctionIndex
    -> [FunctionParameterShape]
    -> LoweringState
    -> [TypedExpr]
    -> ([LoweredIRLoweringFailure], Maybe [LoweredOperand], LoweringState)
  ```

- A tuple emits one `LoweredConstructProduct layoutId operands` after every
  child succeeds. A constructor emits one
  `LoweredConstructVariant layoutId tag operands` after exact constructor
  lookup and arity verification. Nullary construction emits the variant
  instruction with `[]`.

- [x] **Step 1: Add exact Lowered IR RED expectations.** Cover `(1, "two")`,
      `None`, `Some 7`, nested `Branch (Leaf 1) (Leaf 2)`, a tuple containing a
      variant, and a variant containing Text, a closure, a product, and another
      variant. Spell out complete layouts, instructions, operands,
      temporaries, functions, and entry blocks.

- [x] **Step 2: Add arbitrary Typed Core lowerer boundaries.** Require bare or
      partially applied non-nullary constructors, wrong constructor binder,
      unsupported field recipe, and missing catalog declarations to return one
      ordered `LoweredIRUnsupported` result and no Lowered Program.

- [x] **Step 3: Run the focused suite and verify RED.** Expected: tuple nodes
      and constructor spines reach `LoweredIRUnsupportedExpression` after their
      Typed Core input validates.

- [x] **Step 4: Admit construction during shape inspection.** Allow non-empty
      `TypedTupleExpr` only when its node recipe resolves to the structural
      product layout and every child passes. Recognize a constructor variable
      or application only through binder-based `constructorLayoutFor`; do not
      match constructor spelling.

- [x] **Step 5: Emit tuple construction.** Lower each element with indexed
      child paths, stop on the first ordered failure while retaining failures
      already produced by earlier children, verify operand representations
      against the layout fields, then append one `LoweredConstructProduct`.

- [x] **Step 6: Emit variant construction.** Decompose the full application
      spine, lower every field left to right, require exact field count and
      representation, and append one `LoweredConstructVariant`. Do not emit a
      closure for the constructor callee or any partially built variant.

- [x] **Step 7: Run exact lowering and contract suites.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: every exact construction fixture passes twice; all arbitrary
  malformed Typed Core values fail at their owned boundary; contract parity is
  unchanged.

- [x] **Step 8: Commit the green construction milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs src/Jazz/Compiler/LoweredIR/Lower/Emit.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs
  git commit -m "feat: lower managed product and variant construction"
  ```

### Task 6: Close the complete managed-value transport matrix

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs`

**Interfaces:**

- Products and variants use ordinary
  `LoweredManagedReferenceRepresentation layoutId`; no transport-specific IR
  node is added.
- Existing `valueSchemeContract`, function parameter/result recipes, capture
  shapes, `ambientSlots`, carried operands, joins, returns, and tail calls must
  accept catalog-resolved managed references exactly as they accept Text.

- [x] **Step 1: Add exact RED transport fixtures.** Cover all of these source
      profiles with both exact Typed Core and exact Lowered IR expectations:

  ```jazz
  pair = (1, "two").
  pair.
  ```

  ```jazz
  identity :: (Int, Text) -> (Int, Text).
  identity = \(value) -> value.
  identity (1, "two").
  ```

  ```jazz
  data Box = Box (Int, Text).
  box = Box (1, "two").
  capture = \(ignored) -> box.
  capture True.
  ```

  Add direct and closure parameters/results, lexical and recursive captures,
  conditional joins, scalar-case result joins, direct and closure calls,
  returns, and direct/closure tail operands. Include nested construction on
  both conditional branches to prove evaluation order.

- [x] **Step 2: Run the focused suite and verify RED.** Expected: any remaining
      scalar-only helper reports `LoweredIRUnsupportedRepresentation` or a
      producer managed-value failure at the exact transport path.

- [x] **Step 3: Replace remaining scalar-only transport checks.** Route value
      schemes, scalar binding indexes, callable flattening, captures, result
      destinations, and operand comparison through the catalog. Preserve
      existing local/shared/carried sort order and block identities.

- [x] **Step 4: Lock exclusions and failure precedence.** Add repeatable tests
      for list fields and list construction, equality on products/variants,
      imported constructors, first-class non-nullary constructors, pattern
      cases that destructure tuples/constructors, pattern lambdas, and
      multi-module programs. Assert source diagnostics still precede producer
      and lowerer failures.

- [x] **Step 5: Run the complete focused matrix.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: construction, transport, failure, and mirrored contract tests all
  pass serially and twice-repeatable fixtures remain byte-for-byte equal.

- [x] **Step 6: Commit the green transport milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs src/Jazz/Compiler/LoweredIR/Lower/Emit.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/ManagedProductsVariants.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ManagedProductsVariantsTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/LowererBoundary.hs
  git commit -m "test: close managed product and variant transport"
  ```

### Task 7: Full closeout

**Files:**

- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0015-typed-core-managed-products-and-variants.md`
- Modify: `.codex/plans/2026-08-27-jazz-typed-core-managed-product-variant-construction.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Documentation must describe shipped construction/transport only. It must not
  claim managed constructor/tuple pattern compilation.
- On verified closure, remove this row from `Ready Now` and add only
  `JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001` to
  `Next Curation Target`; do not promote it without its own aligned plan.

- [x] **Step 1: Run focused verification from a clean milestone.** Run the
      three focused suites in the frontmatter command. Expected: pass.

- [x] **Step 2: Run the full serialized suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every registered suite passes with no parallel runner.

- [x] **Step 3: Update documentation and durable implementation status.** Add
      factual product/variant construction and transport capability to the
      compiler pipeline, bootstrapping guide, project status, and RFC 0015.
      Preserve explicit exclusions for managed patterns, lists, modules,
      runtime ABI, native execution, and ordinary compile/run cutover.

- [x] **Step 4: Close dispatcher state.** Set plan status to `complete`, remove
      the Ready row, update the bootstrap blocker with commit/test evidence,
      and register the managed-pattern child as a curation candidate behind a
      new plan and green baseline.

- [x] **Step 5: Run repository policy checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: documentation, RFC, link, authority, queue, regression, formatting,
  and whitespace checks all pass.

- [x] **Step 6: Audit repository scope.** Run:

  ```bash
  rg -n "ManagedProductsVariants|TypedManagedProductRecipe|TypedManagedVariantRecipe|LoweredConstructProduct|LoweredConstructVariant" src test jazz docs rfcs .codex
  git status --short
  ```

  Confirm that every active implementation is under root `src/`/`test/`, no
  hosted schema mirror changed, no implementation path exists outside the
  active roots, and only intended closeout files are uncommitted.

- [x] **Step 7: Commit the verified closeout.** Run:

  ```bash
  git add docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0015-typed-core-managed-products-and-variants.md .codex/plans/2026-08-27-jazz-typed-core-managed-product-variant-construction.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: close managed product and variant construction"
  ```
