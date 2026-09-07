---
id: JN-COMPILER-TYPED-CORE-BUILD-001
status: complete
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on: []
plan_section: "Task 15"
target_paths:
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Signature.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/TypeInference/Pattern.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/Runtime/Engine.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/ScalarTextTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
  - test/Jazz/Compiler/Semantics/BuiltinCatalogSpec.hs
  - test/Jazz/Repository/OpaqueCarrierContracts.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --jobs=1
deliverable: "Remove unused migration helpers and verify the completed compiler architecture. Normal execution remains analyzed-core based."
last_verified: 2026-09-04
---

# Jazz Compiler Architecture Simplification Implementation Plan

> Backend scope superseded on 2026-09-07 by
> [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md) and the
> [approved removal plan](2026-09-07-remove-optional-backend.md). Backend
> representations, mirrors, and future Typed Core interpreter migration below
> are historical proposals, not remaining tasks. Retained compiler and runtime
> simplifications remain in place.

Completed on 2026-09-04. Final implementation and verification revision:
`daea3d1a`. Tasks 1-15 are delivered; the original step commands below remain
as implementation history. Older unchecked step boxes are not outstanding work;
completion records describe the delivered milestones. Full development build
(including every enabled test component), all 63 default-enabled test suites,
Cabal metadata, executable examples, repository
checks, and the isolated Nix flake check pass. The CI phases were run separately
to avoid duplicating the workspace compiler test run. Task 14's benchmark-stage,
profiling, and runtime-observation checks also pass.

The final audit removed the unused pattern-arm and explicit-application result
carriers, their bridge assertion, obsolete runtime-template conversion, and
unused runtime declaration parameters. It corrected two stale resolved-phase
annotations in builtin runtime test helpers. The isolated gate also exposed an
earlier opacity test that assumed only Cabal v2 package databases; its lookup
now accepts the v1 layout used by Nix, preserving the same assertions. Public
language behavior and the hosted portable schema remain unchanged. Ordinary execution consumes analyzed
core; full Typed Core execution parity and a separately approved cutover remain
future work.

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Jazz's overlapping Haskell compiler representations with one
phase-indexed canonical program, four intentional type abstraction levels,
node-local semantic facts, and concrete compiler build outcomes while preserving
the existing interpreter and public language behavior.

**Architecture:** Introduce neutral identity and type modules first, then make
the parser produce fully located surface nodes and migrate the canonical AST to
DataKinds-indexed phases. Consolidate module carriers around that program,
attach analysis and runtime decisions directly to analyzed nodes, and move the
existing interpreter onto analyzed core before deleting hint maps or runtime
wrappers. Typed Core construction then moves onto analyzed facts with checked failures; the separate Typed Core interpreter remains a
future, parity-gated child.

**Tech Stack:** Haskell 2010 plus explicit GHC 9.14.1 extensions (`DataKinds`,
`TypeFamilies`, `RoleAnnotations`, deriving extensions, and GADTs only where a
constructor refines its result), Cabal, Nix, `containers`, `deepseq`, the
existing Jazz test harness, Typed Core, Lowered IR v1, and the existing
interpreter.

**Spec:**
`.codex/plans/2026-08-31-jazz-compiler-architecture-simplification-design.md`

## Global Constraints

- Preserve public Jazz syntax, type-system behavior, diagnostic content and
  ordering, interpreter behavior, module behavior, and deterministic artifacts.
- Keep ordinary `compile` and `run` interpreter-backed after every task.
- Do not add a Typed Core interpreter or delete raw-core interpretation in this
  plan. Those require full Typed Core parity and separate approval.
- Do not begin Task 1 until the accepted managed product/variant pattern child
  is completed or the user explicitly changes queue order.
- Promote only the currently executing architecture milestone in
  `.codex/execution/queue.md`; do not publish the whole umbrella as one
  autonomous queue row.
- Retain exactly four recursive type abstraction levels: authored
  `SignatureType`, inferred `SemanticType`, `TypedRepresentationRecipe`, and
  `LoweredRepresentation`.
- Keep `ValidatedTypedProgram` and `ValidatedLoweredProgram` constructors
  private.
- Keep hosted-Jazz data as an explicit portable projection; do not make hosted
  schemas production compiler types.
- Allocate `CoreNodeId` values deterministically in canonical pre-order within
  a source unit and preserve them, together with spans, through resolution and
  analysis.
- Use pattern synonyms only inside the task that removes a legacy constructor
  family; no migration-only pattern synonym may survive Task 15.
- Use `Traversable`/`Bitraversable`, `NonEmpty`, `Seq`, `Map`, `Semigroup`,
  `Monoid`, opaque newtypes, and deriving machinery where they replace custom
  code. Do not introduce recursion schemes, open type families, singleton
  reflection, a generic pass category, or a fully indexed `RuntimeValue`.
- Begin each behavior-sensitive task with a focused failing assertion or a
  named characterization gate. Do not add tests for facts already enforced by
  GHC's type checker.
- Delete temporary bridge tests with the bridge they cover.
- Run Cabal only inside the checked-in Nix development environment, with one
  job, and format every touched Haskell or Markdown file.
- Commit every green task with the exact message named in that task.

---

### Task 1: Establish the architecture queue child and baseline

**Files:**

- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-31-jazz-compiler-architecture-simplification.md`

**Interfaces:**

- Consumes: the completed
  `JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001` child or an
  explicit user decision to reorder it.
- Produces: one executable `JN-COMPILER-CORE-IDENTITY-TYPES-001` queue row for
  Tasks 2-5; later milestones replace this row rather than accumulating ready
  rows.

- [x] **Step 1: Verify the prerequisite and clean baseline.**

  Run:

  ```sh
  git status --short
  rg -n "JN-BOOTSTRAP-TYPED-CORE-MANAGED-PRODUCT-VARIANT-PATTERN-CASES-001" .codex/execution/queue.md .codex/execution/blocker-contracts.md
  ```

  Expected: the worktree is clean and the managed-pattern child is recorded as
  complete. Stop without editing if it is still the live curation target and no
  explicit reorder was approved.

- [x] **Step 2: Run the baseline behavioral matrix.**

  Run:

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test canonical-parser-comparison-spec core-normalization-spec name-semantics-spec binding-signature-coherence-spec module-resolution-spec module-pipeline-contract-spec loader-spec runtime-semantics-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: every listed suite passes before representation work starts.

- [x] **Step 3: Record the first child.** Add one `P1`, size `L`, kind `impl`,
      autonomous-ready row with ID `JN-COMPILER-CORE-IDENTITY-TYPES-001`, this plan,
      plan section `Task 5`, the Task 2-5 target paths, and the Task 5 verification
      command. Keep the managed-pattern child's closure evidence intact.

- [x] **Step 4: Validate dispatcher structure.**

  Run:

  ```sh
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands pass.

- [x] **Step 5: Commit.**

  ```sh
  git add .codex/execution/queue.md .codex/execution/blocker-contracts.md .codex/plans/2026-08-31-jazz-compiler-architecture-simplification.md
  git commit -m "docs: queue compiler core simplification"
  ```

### Task 2: Add nominal module and source identities

**Files:**

- Create: `src/Jazz/Compiler/ModuleIdentity.hs`
- Modify: `src/Jazz/Compiler/Name.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/ModuleGraph.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Modify: `jazz.cabal`
- Test: `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/NameSemanticsSpec.hs`

**Interfaces:**

- Consumes: `Identifier` and the current module-path validation rules.
- Produces:

  ```haskell
  newtype ModulePath = ModulePath (NonEmpty Identifier)
  newtype SourceFile = SourceFile FilePath
  newtype ModuleQualifier = ModuleQualifier Identifier

  data ModuleIdentity = ModuleIdentity
    { moduleIdentityPath :: ModulePath
    , moduleIdentitySource :: SourceFile
    }

  mkModulePath :: NonEmpty Identifier -> ModulePath
  mkSourceFile :: FilePath -> SourceFile
  sourceFilePath :: SourceFile -> FilePath
  moduleIdentity :: ModulePath -> SourceFile -> ModuleIdentity
  parseModulePathText :: Text -> Either Diagnostic ModulePath
  modulePathSegments :: ModulePath -> NonEmpty Identifier
  modulePathTextSegments :: ModulePath -> NonEmpty Text
  renderModulePath :: ModulePath -> Text
  modulePathRelativeFile :: String -> ModulePath -> FilePath
  ```

- [ ] **Step 1: Add failing identity contracts.** In
      `ModuleResolutionSpec.hs`, assert that `parseModulePathText "Foo::Bar"`
      produces two non-empty identifiers, rendering returns `Foo::Bar`, and empty,
      leading-separator, trailing-separator, and invalid-identifier inputs retain
      `E4016`. In `NameSemanticsSpec.hs`, assert imported origins round-trip a
      `ModulePath`, not `[Text]`.

- [ ] **Step 2: Run the contracts and verify the API is absent.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test module-resolution-spec name-semantics-spec --test-show-details=direct --jobs=1
  ```

  Expected: compilation fails because `Jazz.Compiler.ModuleIdentity` does not
  exist.

- [ ] **Step 3: Implement `ModuleIdentity`.** Keep constructors private, derive
      `Eq`, `Ord`, `Show`, `Generic`, and `NFData`, and implement path parsing once.
      `modulePathRelativeFile` must use `NonEmpty.toList`, `identifierText`, and
      `joinPath`; it must not accept an empty path.

- [ ] **Step 4: Replace path parsing ownership.** Make
      `ModuleResolver.parseModulePathText` delegate to the new module and migrate
      its internal lookup keys to `ModulePath`. Retain `[Text]` conversions only at
      CLI text and hosted/portable boundaries.

- [ ] **Step 5: Narrow name origins.** Change:

  ```haskell
  data ResolvedNameOrigin
    = CurrentModule
    | ImportedModule ModulePath
    | AmbientPrelude
  ```

  Update renderers and smart constructors. Do not split unresolved and resolved
  names yet; Task 7 performs that compiler-wide transition.

- [ ] **Step 6: Register and format the module.** Add
      `Jazz.Compiler.ModuleIdentity` to `jazz.cabal`, format touched Haskell, and
      run the two focused suites plus `loader-spec`.

- [ ] **Step 7: Commit.**

  ```sh
  git add src/Jazz/Compiler/ModuleIdentity.hs src/Jazz/Compiler/Name.hs src/Jazz/Compiler/ModuleResolver.hs src/Jazz/Compiler/ModuleGraph.hs src/Jazz/Compiler/Driver.hs test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs test/Jazz/Compiler/Semantics/NameSemanticsSpec.hs jazz.cabal
  git commit -m "refactor: add nominal module identities"
  ```

### Task 3: Consolidate signature syntax and numeric kinds

**Files:**

- Create: `src/Jazz/Compiler/TypeRepresentation.hs`
- Modify: `src/Jazz/Compiler/AST.hs`
- Modify: `src/Jazz/Compiler/Parser/AST.hs`
- Modify: `src/Jazz/Compiler/Parser/Signature.hs`
- Modify: `src/Jazz/Compiler/Parser/Expression.hs`
- Modify: `src/Jazz/Compiler/Parser/Lower.hs`
- Modify: `src/Jazz/Compiler/SignatureRendering.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Signature.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Expressions.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `jazz.cabal`
- Test: `test/Jazz/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Test: `test/Jazz/Compiler/Diagnostics/SignatureRenderingSpec.hs`
- Test: `test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs`

**Interfaces:**

- Consumes: `Identifier`, the current eleven numeric kinds, and existing
  signature constructors.
- Produces:

  ```haskell
  data NumericType
    = NumericInt8 | NumericInt16 | NumericInt32 | NumericInt64
    | NumericUInt8 | NumericUInt16 | NumericUInt32 | NumericUInt64
    | NumericFloat16 | NumericFloat32 | NumericFloat64

  data SignatureType typeName variable
    = TypeInt
    | TypeFloat
    | TypeNumeric NumericType
    | TypeBool
    | TypeChar
    | TypeText
    | TypeVariable variable
    | TypeName typeName
    | TypeApplication typeName [SignatureType typeName variable]
    | TypeList (SignatureType typeName variable)
    | TypeTuple [SignatureType typeName variable]
    | TypeFunction
        (SignatureType typeName variable)
        (SignatureType typeName variable)

  data SignatureConstraint typeName variable =
    SignatureConstraint typeName [SignatureType typeName variable]

  data SignatureToken name = ...
  data SignaturePayload typeName variable tokenName = ...
  ```

  `SignatureType` must have `Functor`, `Foldable`, `Traversable`, `Bifunctor`,
  `Bifoldable`, and `Bitraversable`; derive mechanical stock/anyclass instances.

- [ ] **Step 1: Add failing shared-type contracts.** Extend
      `HaskellTypeclassContractsSpec.hs` with a nested signature containing both a
      constructor name and variables. Assert `bimap Text.toUpper (+ 10)` changes
      every occurrence exactly once. Retain the existing exact numeric enumeration
      assertion in `CanonicalParserComparisonSpec.hs`.

- [ ] **Step 2: Run the focused suites.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test haskell-typeclass-contracts-spec canonical-parser-comparison-spec signature-rendering-spec --test-show-details=direct --jobs=1
  ```

  Expected: compilation fails because `TypeRepresentation` is absent.

- [ ] **Step 3: Implement the shared recursive syntax.** Move `NumericType` and
      the recursive signature families into `TypeRepresentation.hs`. Implement one
      structural `Bitraversable` instance and derive the one-parameter traversals
      over variables.

- [ ] **Step 4: Replace parser duplicates.** Define parser aliases using
      `Identifier`/`Text`, migrate parser constructors to the shared constructors,
      and delete `SurfaceNumericType`, `SurfaceSignatureType`, and their recursive
      conversion functions from `Parser.Lower`.

- [ ] **Step 5: Replace core duplicates.** Define core aliases using the
      current `Name` during this milestone, migrate signature payloads and tokens,
      and delete their old declarations from `AST.hs`.

- [ ] **Step 6: Share numeric kinds with Typed Core.** Replace
      `TypedNumericType` with `NumericType`, migrate recipe/validation/lowering
      matches, and keep the hosted-Jazz constructor spelling isolated in
      `CanonicalTypedCoreComparison.hs`; do not change the hosted schema in this
      task.

- [ ] **Step 7: Remove migration constructors.** Update all source and tests to
      the shared constructors and delete temporary surface/typed numeric pattern
      synonyms before the task closes.

- [ ] **Step 8: Run focused and dependent suites.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test canonical-parser-comparison-spec parser-foundation-spec signature-rendering-spec binding-signature-coherence-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec haskell-typeclass-contracts-spec --test-show-details=direct --jobs=1
  ```

  Expected: all suites pass and `rg -n 'data SurfaceNumericType|data SurfaceSignatureType|data TypedNumericType' src` returns no matches.

- [ ] **Step 9: Commit.**

  ```sh
  git add src/Jazz/Compiler/TypeRepresentation.hs src/Jazz/Compiler/AST.hs src/Jazz/Compiler/Parser src/Jazz/Compiler/SignatureRendering.hs src/Jazz/Compiler/TypeInference src/Jazz/Compiler/TypedCore.hs src/Jazz/Compiler/TypedCore/Validate src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs test/Jazz/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs test/Jazz/Compiler/Diagnostics/SignatureRenderingSpec.hs test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs jazz.cabal
  git commit -m "refactor: share signature and numeric types"
  ```

### Task 4: Move integer literal ranges into solver constraints

**Files:**

- Modify: `src/Jazz/Compiler/TypeRepresentation.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/State.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Solver.hs`
- Modify: `src/Jazz/Compiler/TypeInference/TypeOps.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Operator.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Test: `test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`
- Test: `test/Jazz/Compiler/Semantics/PrimitiveSemantics/NumericConversions.hs`
- Test: `test/Jazz/Compiler/Semantics/Runtime/NumericTests.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/CaptureRecursionTests.hs`

**Interfaces:**

- Consumes: existing `NumericConstraint` and inference-variable substitution.
- Produces:

  ```haskell
  newtype InferenceVariable = InferenceVariable Int

  freshIntegerLiteralType
    :: IntegerLiteralRange
    -> InferState
    -> (ExpressionType, InferState)

  integerLiteralRangeFor
    :: InferState
    -> ExpressionType
    -> Maybe IntegerLiteralRange
  ```

  Every integer literal is a fresh type variable carrying
  `IntegralLiteralNumericConstraint`; `TIntegerLiteralType` no longer exists.

- [ ] **Step 1: Characterize literal behavior.** Replace tests that construct
      `TIntegerLiteralType` directly with source-level assertions covering signed
      bounds, unsigned overflow, arithmetic range combination, comparison,
      recursion, default `Int`, explicit numeric signatures, and Typed Core literal
      constraints. Keep these as source-level behavior checks rather than exposing
      solver state for the test.

- [ ] **Step 2: Run the characterization matrix.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec primitive-semantics-spec runtime-semantics-spec jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: existing behavior passes. This is the named characterization gate;
  removing `TIntegerLiteralType` will then make the compiler fail until every
  solver-owned case has migrated.

- [ ] **Step 3: Introduce nominal variables and literal constraints.** Define
      `InferenceVariable` in neutral `TypeRepresentation` ownership. Make
      fresh literal inference allocate `TVarType (InferenceVariable n)` and record
      `IntegralLiteralNumericConstraint range` in `InferState`. Pattern literals use
      the same helper.

- [ ] **Step 4: Move range queries to solver state.** Rewrite unification,
      operator range propagation, defaulting, capability matching, diagnostics, and
      elaboration to call `integerLiteralRangeFor` rather than match a type
      constructor. Combining arithmetic ranges must update the result variable's
      numeric constraint.

- [ ] **Step 5: Delete the literal type variant.** Remove
      `TIntegerLiteralType`, its substitution/free-variable cases, and all tests
      that directly construct it.

- [ ] **Step 6: Verify behavior and absence.**

  ```sh
  rg -n "TIntegerLiteralType" src test
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec primitive-semantics-spec runtime-semantics-spec recursive-bindings-spec jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: the search has no matches and all suites pass.

- [ ] **Step 7: Commit.**

  ```sh
  git add src/Jazz/Compiler/TypeRepresentation.hs src/Jazz/Compiler/TypeInference test/Jazz/Compiler/Semantics test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/CaptureRecursionTests.hs
  git commit -m "refactor: model literal ranges as solver constraints"
  ```

### Task 5: Share the semantic type tree

**Files:**

- Modify: `src/Jazz/Compiler/TypeRepresentation.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Solver.hs`
- Modify: `src/Jazz/Compiler/TypeInference/TypeOps.hs`
- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Query.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Declarations.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Evidence.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Expressions.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Internal.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Program.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Modify: `test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs`

**Interfaces:**

- Consumes: Task 3's `NumericType`, Task 4's `InferenceVariable`, and current
  inferred/Typed Core shapes.
- Produces:

  ```haskell
  data SemanticType typeName variable
    = SemanticInt
    | SemanticFloat
    | SemanticNumeric NumericType
    | SemanticBool
    | SemanticChar
    | SemanticText
    | SemanticList (SemanticType typeName variable)
    | SemanticTuple [SemanticType typeName variable]
    | SemanticData typeName [SemanticType typeName variable]
    | SemanticFunction
        (SemanticType typeName variable)
        (SemanticType typeName variable)
    | SemanticVariable variable

  type ExpressionType = SemanticType Name InferenceVariable
  type TypedType = SemanticType TypedCoreName TypedTypeParameterId
  ```

- [ ] **Step 1: Add failing bitraversal laws.** Extend
      `HaskellTypeclassContractsSpec.hs` with a nested `SemanticData`/function type
      and assert identity, composition, and exact `bimap` behavior over names and
      variables.

- [ ] **Step 2: Run the focused test and confirm the type is missing.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test haskell-typeclass-contracts-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Add `SemanticType`.** Derive `Functor`, `Foldable`,
      `Traversable`, `Generic`, and `NFData`; implement `Bifunctor`, `Bifoldable`,
      and `Bitraversable` once. Keep representation recipes out of this tree.

- [ ] **Step 4: Migrate inference.** Alias `ExpressionType`, update solver and
      type-operation matches to semantic constructors, and remove the old recursive
      declaration. Keep `TypeScheme`, constraints, and substitutions
      inference-owned.

- [ ] **Step 5: Migrate Typed Core.** Alias `TypedType`, update validation,
      queries, lowering, fixtures, and canonical hosted-value projection. Keep
      binder IDs, type-parameter IDs, schemes, evidence, and recipes
      Typed-Core-owned.

- [ ] **Step 6: Remove migration patterns.** Update all remaining call sites to
      semantic constructors and delete temporary `T*`/`Typed*Type` patterns.
      Hosted Jazz retains its portable constructor vocabulary and is converted only
      in `CanonicalTypedCoreComparison.hs`.

- [ ] **Step 7: Verify the first architecture child.**

  ```sh
  rg -n "^data ExpressionType|^data TypedType" src
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test haskell-typeclass-contracts-spec binding-signature-coherence-spec primitive-semantics-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment --jobs=1
  ```

  Expected: no duplicate declarations and all commands pass.

- [ ] **Step 8: Close the queue child and commit.** Record verification for
      `JN-COMPILER-CORE-IDENTITY-TYPES-001`, leave `Ready Now` empty, name
      `JN-COMPILER-LOCATED-PHASED-CORE-001` as the next curation target, and commit:

  ```sh
  git add src test jazz.cabal .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "refactor: share compiler semantic types"
  ```

### Task 6: Give every surface expression and pattern a source location

**Files:**

- Modify: `src/Jazz/Compiler/Parser/AST.hs`
- Modify: `src/Jazz/Compiler/Parser/Expression.hs`
- Modify: `src/Jazz/Compiler/Parser/Pattern.hs`
- Modify: `src/Jazz/Compiler/Parser/Declaration.hs`
- Modify: `src/Jazz/Compiler/Parser/Lower.hs`
- Modify: `src/Jazz/Compiler/Parser.hs`
- Modify: `test/Jazz/Compiler/Parser/TestSupport.hs`
- Modify: `test/Jazz/Compiler/Parser/Foundation/ExpressionsTests.hs`
- Modify: `test/Jazz/Compiler/Parser/PatternParserSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalParserComparison.hs`

**Interfaces:**

- Consumes: lexer `Token.tokenSpan` and current surface forms.
- Produces:

  ```haskell
  data SurfaceExpr = SurfaceExpr
    { surfaceExprSpan :: SourceSpan
    , surfaceExprForm :: SurfaceExprForm
    }

  data SurfacePattern = SurfacePattern
    { surfacePatternSpan :: SourceSpan
    , surfacePatternForm :: SurfacePatternForm
    }
  ```

  The span is the first syntactic token of the node, matching existing
  statement and diagnostic conventions.

- [ ] **Step 1: Add failing location assertions.** Parse one nested expression
      containing application, lambda, tuple, list, `if`, `case`, binary operation,
      section, and type application. Assert the exact line/column for every node.
      Parse nested constructor/list/tuple/as/or patterns and assert each pattern's
      leading-token location.

- [ ] **Step 2: Run parser suites and confirm the accessors are absent.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test parser-foundation-spec expression-parser-spec pattern-parser-spec canonical-parser-comparison-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Wrap surface forms.** Split recursive surface expression and
      pattern declarations into located node plus form. Derive `Eq`, `Show`,
      `Generic`, and `NFData`; do not add a generic recursion framework.

- [ ] **Step 4: Capture spans in parsers.** Construct every node with the span
      of its first consumed token. Preserve the existing diagnostic span for
      failures and declarations.

- [ ] **Step 5: Update lowering and canonical comparison.** Lowering consumes
      locations; hosted parser parity continues comparing semantic forms and does
      not add spans to the portable Jazz schema.

- [ ] **Step 6: Run the parser matrix and format.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test parser-foundation-spec expression-parser-spec pattern-parser-spec declaration-parser-spec adt-pattern-parser-spec canonical-parser-comparison-spec jazz-parser-parity-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 7: Commit.**

  ```sh
  git add src/Jazz/Compiler/Parser test/Jazz/Compiler/Parser test/Jazz/Compiler/Bootstrap/CanonicalParserComparison.hs
  git commit -m "refactor: locate every surface syntax node"
  ```

### Task 7: Introduce the phase-indexed canonical AST and split names

**Files:**

- Create: `src/Jazz/Compiler/SemanticFacts.hs`
- Modify: `src/Jazz/Compiler/AST.hs`
- Modify: `src/Jazz/Compiler/Name.hs`
- Modify: `src/Jazz/Compiler/Parser/Lower.hs`
- Modify: `src/Jazz/Compiler/ModuleGraph.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/Analyzer.hs`
- Modify: `src/Jazz/Compiler/Analyzer/UnusedBindings.hs`
- Modify: `src/Jazz/Compiler/Pattern.hs`
- Modify: `src/Jazz/Compiler/PatternCoverage.hs`
- Modify: `src/Jazz/Compiler/Purity.hs`
- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Traversal.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `jazz.cabal`
- Test: `test/Jazz/Compiler/Semantics/CoreNormalizationSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/NameSemanticsSpec.hs`

**Interfaces:**

- Consumes: fully located surface nodes, `ModulePath`, and shared semantic
  types.
- Produces:

  ```haskell
  data CorePhase = Lowered | Resolved | Analyzed
  data CoreSort = ExpressionSort | PatternSort | StatementSort

  newtype CoreNodeId = CoreNodeId Int

  data Name user
    = UserName user
    | BuiltinName Identifier
    | GeneratedName GeneratedNameKind

  data SourceName
    = UnqualifiedSourceName Identifier
    | QualifiedSourceName Identifier Identifier

  data ResolvedUserName = ResolvedUserName
    ResolvedNameOrigin NameNamespace Identifier

  type UnresolvedName = Name SourceName
  type ResolvedName = Name ResolvedUserName

  type family CoreNameAt phase where
    CoreNameAt 'Lowered = UnresolvedName
    CoreNameAt 'Resolved = ResolvedName
    CoreNameAt 'Analyzed = ResolvedName

  type family FactsAt phase sort where
    FactsAt 'Lowered sort = ()
    FactsAt 'Resolved sort = ()
    FactsAt 'Analyzed 'ExpressionSort = ExpressionFacts
    FactsAt 'Analyzed 'PatternSort = PatternFacts
    FactsAt 'Analyzed 'StatementSort = StatementFacts

  data CoreNode phase sort = CoreNode
    { coreNodeId :: CoreNodeId
    , coreNodeSpan :: SourceSpan
    , coreNodeFacts :: FactsAt phase sort
    }

  type AnalyzedType =
    SemanticType ResolvedName InferenceVariable

  newtype CoreBinderId = CoreBinderId (ModulePath, CoreNodeId)
  newtype CapabilityId = CapabilityId ResolvedName
  newtype ImplId = ImplId (ModulePath, CoreNodeId)
  newtype MethodId = MethodId (ImplId, Identifier)

  data SemanticInstantiation = SemanticInstantiation
    { instantiatedBinder :: CoreBinderId
    , instantiatedTypes :: NonEmpty AnalyzedType
    }

  data EvidenceReference = EvidenceReference
    { evidenceCapability :: CapabilityId
    , evidenceImplementation :: ImplId
    , evidenceMethod :: Maybe MethodId
    , evidenceType :: AnalyzedType
    }

  data NumericTarget
    = DefaultIntegerTarget
    | ConcreteNumericTarget NumericType

  newtype RuntimePlan = RuntimePlan (Seq RuntimeObligation)
    deriving newtype (Semigroup, Monoid)

  data RuntimeObligation
    = InstantiateTypes (NonEmpty AnalyzedType)
    | SupplyEvidence (NonEmpty EvidenceReference)
    | SpecializeNumericLiteral NumericTarget
    | ConstrainResult AnalyzedType
  ```

  `Expr phase`, `Pattern phase`, `Statement phase`, `CaseArm phase`, and related
  declarations contain the matching `CoreNode` and `CoreNameAt phase`. Declare
  nominal roles for every phase-indexed carrier.

- [ ] **Step 1: Add failing core identity contracts.** Lower a nested source
      expression twice and assert identical pre-order IDs, complete spans on every
      expression/pattern/statement, and no duplicate IDs within the source unit.
      Add runtime assertions for source, imported, ambient, builtin, and generated
      name rendering. Phase/name separation itself is enforced by the signatures
      and does not receive a negative-compilation test.

- [ ] **Step 2: Run focused suites.** Expected: compilation fails because
      `CoreNodeId`, phase parameters, and the split name types do not exist.

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test core-normalization-spec name-semantics-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Add phase and fact foundations.** Declare analyzed fact
      records with the final field names (`expressionSemanticType`,
      `expressionInstantiations`, `expressionEvidence`, `expressionRuntimePlan`,
      pattern binding/constructor facts, and statement scheme/binder facts). Task 9
      populates them; no `Maybe` bag is allowed.

- [ ] **Step 4: Split names parametrically.** Implement `Name user`, source and
      resolved user-name payloads, smart constructors, renderers, and
      derived `Functor`/`Foldable`/`Traversable` mappings. Keep a private total adapter
      from each valid legacy constructor only while call sites migrate.

- [ ] **Step 5: Index the AST.** Add `CoreNode` to every recursive node and
      phase-index all core declarations. Use ordinary indexed ADTs; introduce a
      GADT constructor only if it narrows the result phase or sort.

- [ ] **Step 6: Make lowering allocate IDs.** Thread an internal strict
      `State CoreNodeId` through canonical lowering, allocate in source pre-order,
      copy surface spans, and expose the same pure public lowering functions by
      running the state internally.

- [ ] **Step 7: Migrate lowering-through-inference consumers.** Update analyzer,
      resolver, pattern, purity, recursive-binding, type-inference, and temporary
      runtime signatures to the exact phase they consume. The recursive name
      transformation is owned by:

  ```haskell
  resolveExprNames
    :: ResolutionContext
    -> Expr 'Lowered
    -> Either (NonEmpty Diagnostic) (Expr 'Resolved)
  ```

  Task 8 lifts this operation to the whole indexed program after that carrier
  exists.

- [ ] **Step 8: Delete legacy name and AST adapters.** Update test builders to
      require a real `CoreNode`; delete old `Name` mixed-state constructors and all
      migration patterns.

- [ ] **Step 9: Verify phase migration.**

  ```sh
  rg -n "SourceName|QualifiedName" src/Jazz/Compiler --glob '*.hs'
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test core-normalization-spec name-semantics-spec pattern-semantics-spec pattern-coverage-spec recursive-bindings-spec binding-signature-coherence-spec runtime-semantics-spec --test-show-details=direct --jobs=1
  ```

  Expected: unresolved constructors occur only in `SourceName` ownership and
  all commands pass.

- [ ] **Step 10: Commit.**

  ```sh
  git add src/Jazz/Compiler test/Jazz/Compiler jazz.cabal
  git commit -m "refactor: index canonical core by compiler phase"
  ```

### Task 8: Consolidate imports, modules, and resolved programs

**Files:**

- Modify: `src/Jazz/Compiler/ModuleGraph.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/Parser/Lower.hs`
- Modify: `src/Jazz/Compiler/ModuleExports.hs`
- Modify: `src/Jazz/Compiler/ModuleCompiler.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/BundledPrelude.hs`
- Modify: `src/Jazz/Compiler/Prelude.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Test: `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs`
- Test: `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`
- Test: `test/Jazz/Compiler/Modules/PreludeLoadingSpec.hs`
- Test: `test/Jazz/Compiler/Modules/Loader/VisibilityTests.hs`

**Interfaces:**

- Consumes: `CorePhase`, `ModulePath`, `ResolvedName`, and indexed statements.
- Produces:

  ```haskell
  type family ImportExposureAt phase where
    ImportExposureAt 'Lowered = DeclaredImportExposure
    ImportExposureAt 'Resolved = ImportExposure
    ImportExposureAt 'Analyzed = ImportExposure

  data DeclaredImportExposure
    = DeclaredImportAll
    | DeclaredImportOnly (NonEmpty Identifier)

  data ImportExposure
    = ImportAllUnqualified
    | ImportOnlyUnqualified (NonEmpty Identifier)
    | ImportQualifiedOnly

  data ModuleImport phase = ModuleImport
    { moduleImportNode :: CoreNode phase 'StatementSort
    , importedModule :: ModulePath
    , importAlias :: Maybe ModuleQualifier
    , importExposure :: ImportExposureAt phase
    }

  data CoreModule phase = CoreModule
    { coreModuleIdentity :: ModuleIdentity
    , coreModuleImports :: [ModuleImport phase]
    , coreModuleStatements :: [Statement phase]
    , coreModuleFacts :: ModuleFactsAt phase
    }

  type family ModuleFactsAt phase where
    ModuleFactsAt 'Lowered = DeclaredModuleFacts
    ModuleFactsAt 'Resolved = ResolvedModuleFacts
    ModuleFactsAt 'Analyzed = AnalyzedModuleFacts

  data DeclaredModuleFacts = DeclaredModuleFacts
    { declaredModuleExports :: Maybe DeclaredModuleExports
    }

  data ResolvedModuleFacts = ResolvedModuleFacts
    { resolvedModuleExports :: ModuleExportInventory
    }

  data AnalyzedModuleFacts = AnalyzedModuleFacts
    { analyzedModuleExports :: ModuleExportInventory
    , analyzedModuleInterface :: ModuleInterface
    , analyzedModuleDiagnostics :: [Diagnostic]
    }

  data PreludeArtifact phase = PreludeArtifact
    { preludeIdentity :: ModuleIdentity
    , preludeBuiltinMode :: BuiltinResolutionMode
    , preludeModule :: Maybe (CoreModule phase)
    }

  data CoreProgram phase = CoreProgram
    { coreProgramPrelude :: PreludeArtifact phase
    , coreProgramEntry :: ModulePath
    , coreProgramModules :: NonEmpty (CoreModule phase)
    }

  mkCoreProgram
    :: PreludeArtifact phase
    -> ModulePath
    -> NonEmpty (CoreModule phase)
    -> Either (NonEmpty ProgramInvariantFailure) (CoreProgram phase)
  ```

- [ ] **Step 1: Add failing invariant tests.** Assert `mkCoreProgram` rejects a
      missing entry, duplicate path, dependency after dependent, and unknown
      resolved import. Assert it accepts the prelude plus dependency-first modules
      and preserves that order.

- [ ] **Step 2: Run module suites and confirm the new program API is absent.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test module-resolution-spec module-pipeline-contract-spec prelude-loading-spec loader-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Add indexed imports and modules.** Move declared and resolved
      exposure into the closed family. Represent the prelude with an explicit
      `ModuleIdentity`, not `[]`.

- [ ] **Step 4: Make program construction opaque.** Implement all invariants in
      `mkCoreProgram`; expose folds/lookups rather than the constructor. Declare
      nominal phase roles.

- [ ] **Step 5: Simplify resolver traversal.** Replace `ParsedImport`,
      `CoreResolvedImport`, `ResolvedImport`, `ParsedModule`, `SurfaceModuleFacts`,
      `ResolvedModule`, and `ResolvedProgram` with `ModuleDiscoveryFacts`,
      `ReferenceInventory`, and the indexed program. Accumulate modules in `Seq`
      dependency order; delete reverse-list state and final reversal.

- [ ] **Step 6: Add the temporary compiler boundary.** Make `ModuleCompiler`
      consume `CoreProgram 'Resolved`; until Task 9 produces analyzed module facts,
      a single private adapter may project the current `CompiledProgram`. No other
      module may construct that legacy carrier.

- [ ] **Step 7: Verify module behavior and deletion.**

  ```sh
  rg -n "data (ParsedImport|CoreResolvedImport|ResolvedImport|ParsedModule|ResolvedModule|ResolvedProgram)" src
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test module-resolution-spec module-exports-spec module-pipeline-contract-spec prelude-loading-spec loader-spec --test-show-details=direct --jobs=1
  ```

  Expected: the old declarations are absent and all suites pass.

- [ ] **Step 8: Close the phase-core queue child and commit.** Promote
      `JN-COMPILER-ANALYZED-CORE-001` as the next architecture child and commit:

  ```sh
  git add src/Jazz/Compiler test/Jazz/Compiler .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "refactor: consolidate compiler module programs"
  ```

### Task 9: Populate analyzed facts and remove compiled syntax carriers

**Files:**

- Modify: `src/Jazz/Compiler/SemanticFacts.hs`
- Modify: `src/Jazz/Compiler/AST.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Result.hs`
- Modify: `src/Jazz/Compiler/TypeInference/State.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Traversal.hs`
- Modify: `src/Jazz/Compiler/ModuleCompiler.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Modify: `src/Jazz/Compiler/Force.hs`
- Test: `test/Jazz/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/PatternSemanticsSpec.hs`
- Test: `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`

**Interfaces:**

- Consumes: `CoreProgram 'Resolved` and existing inference decisions.
- Produces:

  ```haskell
  analyzeProgram
    :: InferenceInputs
    -> CoreProgram 'Resolved
    -> IO (InferenceResult, Maybe (CoreProgram 'Analyzed))

  data ExpressionFacts = ExpressionFacts
    { expressionSemanticType :: AnalyzedType
    , expressionInstantiations :: [SemanticInstantiation]
    , expressionEvidence :: [EvidenceReference]
    , expressionRuntimePlan :: RuntimePlan
    }
  ```

  `PatternFacts` contains resolved binder types, constructor identity, and
  refutability. `StatementFacts` contains binder identity, generalized scheme,
  and declaration facts. `ModuleFactsAt 'Analyzed` contains interface,
  diagnostics, export inventory, and module-level capability facts.

- [ ] **Step 1: Add failing fact-completeness tests.** Analyze a source covering
      literals, variables, lambda/application, explicit type application,
      capability dispatch, pattern bindings, recursive statements, imports, and
      exported declarations. Walk the analyzed tree and assert every node has its
      final facts with matching `CoreNodeId`/span.

- [ ] **Step 2: Run focused suites.** Expected: compilation fails because
      `analyzeProgram` and populated facts do not exist.

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec pattern-semantics-spec module-pipeline-contract-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Record facts during the existing traversal.** Add strict maps
      keyed by source-unit-local `CoreNodeId` to `InferState`, record the final
      resolved type/instantiation/evidence once per node, and reject duplicate or
      missing entries as internal invariant failures.

- [ ] **Step 4: Rebuild analyzed nodes once.** After successful analysis,
      traverse `CoreProgram 'Resolved` exactly once to attach facts and produce
      `CoreProgram 'Analyzed`. Error diagnostics produce `Nothing`; they never
      produce partially analyzed nodes.

- [ ] **Step 5: Move compiled fields into analyzed module facts.** Migrate
      interfaces, diagnostics, export inventories, and prelude artifacts, then
      delete `CompiledModule`, `CompiledProgram`, `CompiledDependency`, and
      `CompiledPrelude`. Rename the prelude owner to `PreludeArtifact 'Analyzed`.

- [ ] **Step 6: Update force and driver boundaries.** Force analyzed facts and
      expose diagnostics through `CoreProgram` queries. Do not expose constructors
      merely for tests.

- [ ] **Step 7: Verify analyzed completeness.**

  ```sh
  rg -n "data (CompiledModule|CompiledProgram|CompiledDependency|CompiledPrelude)" src
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec pattern-semantics-spec recursive-bindings-spec module-pipeline-contract-spec prelude-loading-spec loader-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 8: Commit.**

  ```sh
  git add src/Jazz/Compiler test/Jazz/Compiler
  git commit -m "refactor: attach semantic facts to analyzed core"
  ```

### Task 10: Replace inference runtime hints with node-local plans

**Files:**

- Modify: `src/Jazz/Compiler/SemanticFacts.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Result.hs`
- Modify: `src/Jazz/Compiler/TypeInference/State.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Signature.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/RuntimeHints.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Modify: `test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify: `test/Jazz/Compiler/ProfilingSpec.hs`

**Interfaces:**

- Consumes: analyzed expression facts and the old hint producer.
- Produces: completed analyzed `RuntimePlan` values plus the temporary
  projection below. The obligation constructors and their semantic payloads
  are the Task 7 types; this task replaces the legacy producer rather than
  defining another plan vocabulary.

  ```haskell
  projectRuntimeHints
    :: CoreProgram 'Analyzed
    -> Map BindingRuntimeHintKey SignatureType
  ```

  `projectRuntimeHints` is a private migration oracle removed in Task 11.

- [ ] **Step 1: Add failing dual-representation parity tests.** For existing
      module-scoped, nested, prelude, explicit generic ADT, class-method-result,
      numeric, and explicit-type-application cases, assert that projecting
      analyzed plans yields the exact current hint map.

- [ ] **Step 2: Run parity suites and verify projection is absent.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec loader-spec profiling-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 3: Populate runtime plans.** Translate final inference decisions,
      not source signatures, into ordered obligations. Accumulate with `Seq` and
      `(<>)`; no obligation may look itself up by span.

- [ ] **Step 4: Add the migration projection.** Implement one fold over analyzed
      core that reconstructs old hints solely for parity and the still-unmigrated
      interpreter. It must not be stored in `InferenceResult`, `ModuleInterface`, or
      prelude artifacts.

- [ ] **Step 5: Delete inference hint production.** Remove `outputRuntimeHints`,
      `inferredRuntimeTypeHints`, `finalizedRuntimeTypeHints`, module-interface hint
      fields, and the span-keyed writes in `Scope`/`Capabilities`.

- [ ] **Step 6: Run parity and dependent suites.**

  ```sh
  rg -n "outputRuntimeHints|inferredRuntimeTypeHints|finalizedRuntimeTypeHints|interfaceRuntimeHints|compiledPreludeRuntimeHints" src
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec runtime-semantics-spec loader-spec module-pipeline-contract-spec profiling-spec --test-show-details=direct --jobs=1
  ```

  Expected: the search has no matches and parity tests pass.

- [ ] **Step 7: Commit.**

  ```sh
  git add src/Jazz/Compiler test/Jazz/Compiler
  git commit -m "refactor: encode runtime decisions on analyzed nodes"
  ```

### Task 11: Move the existing interpreter onto analyzed core

**Files:**

- Modify: `src/Jazz/Compiler/Runtime.hs`
- Modify: `src/Jazz/Compiler/Runtime/Request.hs`
- Modify: `src/Jazz/Compiler/Runtime/Engine.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/Runtime/ScopePlan.hs`
- Modify: `src/Jazz/Compiler/Runtime/HostEvaluation.hs`
- Modify: `src/Jazz/Compiler/Runtime/Semantics.hs`
- Modify: `src/Jazz/Compiler/Runtime/Primitives.hs`
- Modify: `src/Jazz/Compiler/ModuleRuntime.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Delete: `src/Jazz/Compiler/RuntimeHints.hs`
- Modify: `jazz.cabal`
- Test: `test/Jazz/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Test: `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`
- Test: `test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Test: `test/Jazz/Compiler/ProfilingSpec.hs`

**Interfaces:**

- Consumes: `CoreProgram 'Analyzed` and node-local `RuntimePlan`.
- Produces:

  ```haskell
  interpretAnalyzedProgram
    :: Monad m
    => RuntimeObservationRequest
    -> RuntimeHost m
    -> CoreProgram 'Analyzed
    -> m (RuntimeObservationResult RuntimeProgram)
  ```

  Runtime closures store `ResolvedName` and `Expr 'Analyzed`. Runtime requests
  contain analyzed expressions/statements and no hint maps.

- [ ] **Step 1: Freeze interpreter behavior.** Run and retain exact assertions
      for CLI run, single/multi-module execution, recursion, closures, patterns,
      host cells, capabilities, numeric specialization, explicit type application,
      diagnostics, output, exit, and profiling frame balance.

- [ ] **Step 2: Add the new entry-point assertion.** Update one module-pipeline
      test to call `interpretAnalyzedProgram`; it must fail to compile before the
      entry point exists.

- [ ] **Step 3: Change evaluator inputs.** Remove hint maps from
      `RuntimeExpressionRequest`, `RuntimeScopeRequest`, `EvaluationContext`,
      deferred-host values, closure transfer, and public runtime wrappers. Read the
      current expression's obligations from `coreNodeFacts`.

- [ ] **Step 4: Preserve machine semantics.** Keep the existing first-order
      `EvaluationControl`, `EvaluationFrame`, heap-safe recursion, evaluation order,
      host forcing, and observation behavior. This task changes semantic input
      ownership, not the interpreter algorithm.

- [ ] **Step 5: Migrate module runtime.** Evaluate dependency-first analyzed
      modules directly from `CoreProgram`; use `Map ModulePath RuntimeModule` for
      lookup and preserve output selection from the explicit entry path.

- [ ] **Step 6: Delete the bridge.** Remove `projectRuntimeHints`,
      `BindingRuntimeHintKey`, and `RuntimeHints.hs`; delete bridge-only parity tests
      while retaining behavioral cases under their runtime/module names.

- [ ] **Step 7: Verify no hint protocol remains.**

  ```sh
  rg -n "BindingRuntimeHintKey|RuntimeHints|BindingTypeHints|bindingTypeHints" src test
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test cli-spec runtime-semantics-spec module-pipeline-contract-spec loader-spec profiling-spec runtime-observation-spec --test-show-details=direct --jobs=1
  ```

  Expected: the search has no matches and all suites pass.

- [ ] **Step 8: Run performance checks.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test benchmark-stage-spec profiling-spec --test-show-details=direct --jobs=1
  ```

- [ ] **Step 9: Close `JN-COMPILER-ANALYZED-CORE-001`, promote the checked-build
      child, and commit.**

  ```sh
  git add src/Jazz/Compiler test/Jazz/Compiler jazz.cabal .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "refactor: interpret analyzed core directly"
  ```

### Task 12: Consolidate Typed Core outcomes

Maintainer-approved revision (2026-09-04): use one concrete Typed Core result;
retain the concrete lowering result. No generic `CheckedBuild` or speculative
`Functor`/`Foldable`/`Traversable` API or tests.

**Files:** `src/Jazz/Compiler/TypeInference.hs`,
`src/Jazz/Compiler/TypeInference/Elaboration.hs`,
`src/Jazz/Compiler/TypeInference/Traversal.hs`,
`src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`,
`src/Jazz/Compiler/TypedCore.hs`, new `src/Jazz/Compiler/TypedCore/Portable.hs`,
`jazz/compiler/TypedCoreTypes.jz`, affected bootstrap contract tests, `jazz.cabal`.

- [x] Run the existing characterization suites before editing:

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test haskell-typeclass-contracts-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
  ```

- [x] Rename the concrete public sum to `TypedCoreBuildResult`, retaining its
      domain-specific constructors and validated success payload. Store it directly
      in the private `TypedCoreProductionResult`. Remove the duplicate private
      outcome, constructor adapters, and status conversion. Migrate consumers to
      direct construction and observation without compatibility aliases.
- [x] Move the existing portable schema to `TypedCore.Portable`, rename its type
      to `PortableTypedCoreOutcome` in Haskell and Jazz, and preserve encoded
      constructor names. Retain the existing canonical encoder; do not introduce
      an unused or lossy production-to-portable conversion.
- [x] Run the same characterization suites after editing, format touched files,
      and run `git diff --check`. Confirm no old production status/outcome type
      or adapter remains. Leave `LoweredIRLoweringResult` unchanged.
- [x] Commit with `refactor: consolidate typed core outcomes`.

Verified as `29cf6c6b` on `2026-09-04`: all four characterization suites passed
before and after the refactor; the producer suite also passed after final test
cleanup. `cabal build all -fdevelopment --jobs=1`, pinned Ormolu on all touched
Haskell files, and `git diff --check` passed. No generic outcome abstraction or
lowering-result migration was introduced.

### Task 13: Build Typed Core with checked construction from analyzed facts

Maintainer-approved revision (2026-09-04): move the producer onto analyzed core,
remove the provisional tree, and retain construction-dependent checks alongside
construction. Do not introduce `BackendEligibleProgram` or require a total,
single-pass builder. Reuse preliminary construction only when the final context
is identical; recursive capture specialization can change that context.

**Implementation scope:** `src/Jazz/Compiler/TypedCore/Build.hs`,
`src/Jazz/Compiler/TypeInference.hs`, `src/Jazz/Compiler/TypeInference/Scope.hs`,
`src/Jazz/Compiler/TypeInference/Analyzed.hs`, `src/Jazz/Compiler/SemanticFacts.hs`,
`src/Jazz/Compiler/TypeInference/Elaboration/`, `jazz.cabal`, and producer contract
fixtures in `test/Jazz/Compiler/Bootstrap/`.

- [x] Run the existing producer, Typed Core, Lowered IR, and recursive-binding
      characterization suites before editing.
- [x] Establish the analyzed input contract. Preserve semantic decisions needed
      by the producer without retaining `InferState` or a second expression tree.
- [x] Move checked construction onto analyzed expressions, statements, and
      semantic facts. Keep construction-dependent recursive support checks with
      the builder. Preserve catalog/module/statement failure precedence and all
      current profile boundaries.
- [x] Delete provisional expression and statement trees, inference-owned
      production failures, and migration-only adapters. Remove obsolete synthetic
      bridge tests only when their failure signal is covered at the new boundary.
- [x] Validate a successfully constructed raw `TypedProgram` with the existing
      independent validator. Return ordered unsupported failures or invariant
      failures without exposing a successful partial program.
- [x] Run focused suites, the development build, formatting, and whitespace
      checks; commit green milestones and the completed task.

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec recursive-bindings-spec --test-show-details=failures --jobs=1
```

Completion (2026-09-04): the live producer constructs directly from analyzed
expressions and statements (`ebec0326`), and the old finalizer/profile modules
are retired (`2065408c`). Inference no longer creates provisional expressions,
statements, or production failures. Removed the now-redundant inferred-result
wrapper, elaboration modules, and unused child-result bookkeeping. Inference
retains its two existing signature/forward-binding policies; construction reads
only analyzed facts. The development build, formatter, whitespace/queue checks,
and eight suites pass: producer, Typed Core, Lowered IR, recursive bindings,
binding signatures, module pipeline, loader, and runtime semantics.

The deletion audit also retired eager-capture checks that existed only for
injected provisional trees. The analyzer exposes forward names only within
signed function bodies; equivalent eager source uses already fail name analysis.
The builder therefore needs no eager-evaluation flag or availability map.
Declaration-level transitive capture checks remain covered by source fixtures.

Retrospective audit (2026-09-04): checked the recommendations and commits in
this task against Tasks 13-15 and the deferred interpreter cutover. The suggestion
to retain a private provisional tree is withdrawn: Task 13 already owns moving
its decisions into analyzed facts and construction-local specialization context.
The earlier warning about blanket runtime-wrapper deletion also overstated the
plan, which requires replacing behavior before deleting wrappers. Keep the
verified annotation consolidation, but reevaluate each carrier after Task 14's
semantic migration; the source-signature normalization is an intermediate step,
not a permanent runtime boundary. Concrete build outcomes, checked construction,
explicit method-parameter identity, and retained numeric/operator decisions remain
consistent with the remaining tasks. Before raising another design concern,
check whether a pending task supplies its replacement or removes the limitation.

Advanced Haskell features and language extensions are welcome when they reduce
code or improve maintainability. This applies to subsequent tasks as well;
concrete benefit takes precedence over the earlier blanket restrictions.

### Task 14: Simplify runtime annotations and remove source-type conversions

Maintainer-approved revision (2026-09-04): target redundant conversions and
repeated wrapper handling. Preserve value-associated metadata needed by partial
applications, stored callables, and deferred host execution. Eliminate a wrapper
only when its behavior has a simpler equivalent; deleting every wrapper is not
an acceptance criterion. This runtime cleanup is independent of Task 13's
builder migration and can be implemented while that migration remains active.

Method-identity refinement (2026-09-04): retain one explicit, signature-local
semantic class-parameter binder, including when unused. Checked analyzed
projection rejects unsupported method variables rather than dropping methods.
Do not add general method-variable ordering metadata.

**Files:**

- Modify: `src/Jazz/Compiler/SemanticFacts.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/Runtime/Engine.hs`
- Modify: `src/Jazz/Compiler/Runtime/Semantics.hs`
- Modify: `src/Jazz/Compiler/Runtime/Primitives.hs`
- Modify: `src/Jazz/Compiler/Runtime/HostEvaluation.hs`
- Modify: `src/Jazz/Compiler/ModuleRuntime.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/NumericTests.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/RenderingTests.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/HostIOTests.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Consumes: analyzed `RuntimePlan`, stable Typed Core-compatible semantic IDs,
  and the existing interpreter.
- Produces: runtime use of Task 7's `CapabilityId`, `ImplId`, `MethodId`, and
  `EvidenceReference`. `RuntimeValue`, `RuntimeClosure`, deferred host state,
  constructor shapes, method candidates, and runtime evidence contain no
  `SignatureType`.

- [x] **Step 1: Characterize every wrapper behavior.** Retain exact tests for
      typed numeric dispatch, explicit type applications, explicit result
      constraints, list element specialization, constructor fields, capability
      candidate filtering, imported method results, rendering, and deferred host
      bindings.

- [x] **Step 2: Run the runtime matrix before edits.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec loader-spec module-pipeline-contract-spec jazz-typed-core-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: existing behavior passes.

- [x] **Step 3: Replace runtime evidence identity.** Use the stable semantic
      IDs constructed during analysis and carry `EvidenceReference` through runtime plans.
      Remove textual capability/implementation identity and source signatures from
      `RuntimeEvidence`.

- [x] **Step 4: Consume semantic obligations at the correct lifetime.** Make application,
      primitive dispatch, numeric specialization, and result handling consume
      semantic obligations. Keep deferred obligations with the value until
      application, rather than only on the current evaluation stack. Preserve
      outermost-to-innermost source order with `Seq`.

- [x] **Step 5: Consolidate repeated annotation handling.** Retain the distinct
      semantics of type hints, explicit instantiation, and pending result hints.
      Share transparent reads and annotation-preserving transformations where
      they are identical. Replace source-type payloads with semantic data;
      remove wrappers only when a simpler representation preserves their behavior.
      Compare the resulting conversions, branches, and ownership with the baseline.

- [x] **Step 6: Keep runtime callables explicit.** Do not add a
      `RuntimeCallable` abstraction in this task; the
      existing callable constructors directly encode different runtime behavior,
      and the approved design requires measured duplication before consolidation.

- [x] **Step 7: Verify runtime source syntax is gone.**

  ```sh
  rg -n "SignatureType" src/Jazz/Compiler/Runtime src/Jazz/Compiler/ModuleRuntime.hs
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec loader-spec module-pipeline-contract-spec profiling-spec runtime-observation-spec --test-show-details=direct --jobs=1
  ```

  Expected: no runtime dependency on source-type syntax remains. Retained
  annotation carriers have a semantic purpose. All suites pass.

- [x] **Step 8: Run stage performance checks and commit.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test benchmark-stage-spec profiling-spec --test-show-details=direct --jobs=1
  git add src/Jazz/Compiler test/Jazz/Compiler
  git commit -m "refactor: simplify runtime annotations"
  ```

Verified completion (2026-09-04): runtime annotations, closure hints, constructor
fields, method signatures, and candidates use analyzed semantic types. Runtime
candidates carry `EvidenceReference` directly. Constructor fields come from
analyzed schemes; class and implementation nodes carry checked semantic
declarations. Source erasure after analysis leaves execution unchanged. Removed
runtime source conversions, numeric-name parsing, missing-method-signature cases,
and unused signature helpers. Value-associated explicit result obligations remain
ordered and deferred. Runtime, loader, module-pipeline, profiling, observation,
benchmark-stage, and binding-signature suites pass; formatting and whitespace
checks pass. No `SignatureType` reference remains in runtime owners.

### Task 15: Remove migration scaffolding and close the simplification pass

Audit (2026-09-04): Tasks 13 and 14 are committed at `81ae52e6` and
`43631af4`. Provisional trees, inference runtime-hint maps, and old outcome
carriers are absent. Parser `SurfaceSignatureType`/`SurfaceNumericType` aliases
name the shared types required by Task 3; they are not duplicate representations.
Hosted `TypedNumericType` strings belong to the preserved portable schema.
Neither should be deleted to satisfy a broad text search. Removed the unused
runtime-template conversion policy and its bridge tests. The remaining concrete
signature conversion serves inference capability rules and is named accordingly.
The final caller audit also removed unused pattern-arm and explicit-application
result carriers, the obsolete carrier test, and unused runtime declaration
parameters. The complete test-component build caught two builtin runtime helpers
with stale resolved-phase annotations; these now declare their analyzed inputs.
All test components compile with development warnings treated as errors.

**Files:**

- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-31-jazz-compiler-architecture-simplification-design.md`
- Modify: `.codex/plans/2026-08-31-jazz-compiler-architecture-simplification.md`
- Modify: `docs/compiler/pipeline.md` only if it names a removed active Haskell
  representation; do not document internal type names otherwise.
- Modify: `docs/project/status.md` only if its implementation description is no
  longer accurate; do not claim Typed Core execution parity.

**Interfaces:**

- Consumes: green Tasks 1-14.
- Produces: one compiler architecture with no temporary adapters, a preserved
  analyzed-core interpreter, and a terminal architecture queue state. The
  existing bootstrap blocker continues to own missing Typed Core feature parity
  and any future `interpretTypedProgram` child.

- [x] **Step 1: Run the deletion audit.** Each search must return no
      migration-owned declaration or consumer:

  ```sh
  rg -n "SurfaceNumericType|SurfaceSignatureType|TypedNumericType|TIntegerLiteralType" src test
  rg -n "data (ParsedImport|CoreResolvedImport|ResolvedImport|ParsedModule|ResolvedModule|ResolvedProgram|CompiledModule|CompiledProgram|CompiledDependency|CompiledPrelude)" src
  rg -n "BindingRuntimeHintKey|RuntimeHints|RuntimeTypeHints|ProvisionalTypedExpr|ProvisionalTypedStatement" src test
  rg -n "TypedCoreProductionStatus|TypedCoreProductionOutcome" src test
  rg -n "pattern (Surface|Typed|Legacy)|legacy.*(Name|Type|Expr|Module|Import|Program)" src test
  ```

- [x] **Step 2: Remove surviving migration scaffolding.** Delete only adapters
      identified by the searches. Preserve shared parser aliases and portable-schema
      constructors required by the design; record why those matches are permanent.

- [x] **Step 3: Format all touched sources.** Use the repository formatter from
      the Nix shell for Haskell and Prettier for Markdown. Run `git diff --check`.

- [x] **Step 4: Run the authoritative compiler gate.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal check
  ```

- [x] **Step 5: Run performance and repository gates.**

  ```sh
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test benchmark-stage-spec profiling-spec --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/ci/main-functional.sh
  ```

  Expected: all compiler, test, documentation, queue, example, repository, and
  Nix-flake checks pass.

- [x] **Step 6: Confirm the interpreter boundary.** Verify ordinary CLI and
      module execution call `interpretAnalyzedProgram`; verify no
      `interpretTypedProgram` implementation or raw-core deletion entered this
      pass. Record full Typed Core parity and a separately approved cutover as the
      only conditions for a future interpreter-input migration.

- [x] **Step 7: Close internal state.** Mark the architecture child complete,
      return `Ready Now` to the next accepted feature child or terminal-empty state,
      set the design and plan status to complete with the exact verification commit,
      and retain the bootstrap blocker for missing Typed Core profiles.

- [x] **Step 8: Commit closeout evidence.**

  ```sh
  git add .codex/execution .codex/plans docs/compiler/pipeline.md docs/project/status.md
  git commit -m "docs: close compiler architecture simplification"
  ```
