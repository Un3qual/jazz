# Jazz Haskell Maintainability Milestone 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give inference traversal, scope requests, provisional elaboration data, profile analysis, specialization, and finalization clear internal ownership with named call-site inputs.

**Architecture:** Extract shared inference contracts into cycle-free leaf modules, replace positional recursion arguments with immutable environment/location records, then split elaboration by data, analysis, specialization, and construction. Preserve the existing inference façade and exact Typed Core artifacts while pruning only repository-unused private wrappers.

**Tech Stack:** Haskell 2010 with existing local extensions, GHC 9.14.1, Cabal private library/test suites, `containers`, `text`, checked-in Nix development shell.

## Global Constraints

- Milestone 1 must be complete and green before this plan starts.
- Preserve public Jazz syntax and semantics.
- Preserve all source diagnostics, Typed Core production failures, child paths, ordering, and validation proofs exactly.
- Keep provisional constructors Cabal-private but available to the inference producer modules that construct them.
- Do not add a universal compiler context, a new inference monad, opaque smart constructors without an enforced invariant, or a generic traversal framework.
- Change only active root paths and internal `.codex/` execution state.
- Use the checked-in Nix development shell for authoritative verification.
- Format only touched Haskell and commit each task after focused verification.

**Design source:** `.codex/plans/2026-08-15-jazz-haskell-maintainability-design.md`

---

## File map

| File | Responsibility after this milestone |
| --- | --- |
| `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs` | Production outcomes/failures, provisional ADTs, profile/context records |
| `src/Jazz/Compiler/TypeInference/Traversal.hs` | `InferExprFn` and production-aware traversal callback aliases |
| `src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs` | Provisional-expression type/capture specialization |
| `src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs` | Callable, capture, recursion, and dependency analysis |
| `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs` | Final conversion to validated Typed Core |
| `src/Jazz/Compiler/TypeInference/Elaboration.hs` | Small compatibility façade re-exporting active internal contracts |
| `src/Jazz/Compiler/TypeInference/Scope.hs` | Scope traversal driven by one private named request and explicit forward policy |
| `src/Jazz/Compiler/TypeInference.hs` | Stable active inference conveniences over one private request function |
| `jazz.cabal` | Registers all new private modules |

### Task 1: Extract provisional elaboration contracts

**Files:**

- Create: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs:1-250`
- Modify imports in: `src/Jazz/Compiler/TypeInference.hs`, `src/Jazz/Compiler/TypeInference/Scope.hs`, `src/Jazz/Compiler/TypeInference/Pattern.hs`, `src/Jazz/Compiler/TypeInference/Diagnostics.hs`, focused tests
- Modify: `jazz.cabal`

**Interfaces:**

- Consumes: existing AST, diagnostic span, inference types, Typed Core failure/status, and `ValidatedTypedProgram` types.
- Produces the existing constructors unchanged: `TypedCoreProductionStatus`, private-constructor `TypedCoreProductionOutcome`, all production path/kind/detail/failure types, `TypedCoreProductionMode`, `InferredExpr`, `InferredProductionFailure`, all `Provisional*` ADTs, `FunctionProfile`, `ExpressionRole`, and `ExpressionEvaluation`.

- [ ] **Step 1: Capture the current producer contract behavior**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS. Save no generated artifacts; the checked-in exact fixtures are the characterization baseline.

- [ ] **Step 2: Move the ADTs and pure outcome helpers**

Create `Elaboration.Types` and move the declarations currently preceding
`expressionDependencyNames`, plus `FunctionProfile`, `ExpressionRole`, and
`ExpressionEvaluation`. Preserve constructor names and derivations exactly.
Export the production outcome type abstractly while exporting its existing
observer/constructor functions:

```haskell
module Jazz.Compiler.TypeInference.Elaboration.Types
  ( TypedCoreProductionStatus (..),
    TypedCoreProductionOutcome,
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    FunctionProfile (..),
    ExpressionRole (..),
    ExpressionEvaluation (..),
    blockedTypedCoreProductionOutcome,
    unsupportedTypedCoreProductionOutcome,
    typedCoreProductionOutcomeStatus,
    typedCoreProductionOutcomeValidatedProgram,
    blockProductionFailureKindAndDetail,
  ) where
```

`ProductionSucceeded` remains constructible only inside elaboration modules.
Export this exact internal helper instead of exporting the outcome constructors
broadly:

```haskell
succeededTypedCoreProductionOutcome :: ValidatedTypedProgram -> TypedCoreProductionOutcome
succeededTypedCoreProductionOutcome = ProductionSucceeded
```

- [ ] **Step 3: Turn `Elaboration` into the compatibility import boundary**

Import/re-export the moved names from `Elaboration`. Change producer modules to
import `Elaboration.Types` directly when they construct provisional values;
tests that intentionally verify provisional transport may do the same.
Ordinary consumers continue to work through the façade.

- [ ] **Step 4: Verify the move is behavior-neutral**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS with unchanged expected programs and failure lists.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Scope.hs src/Jazz/Compiler/TypeInference/Pattern.hs src/Jazz/Compiler/TypeInference/Diagnostics.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
git commit -m "refactor: extract elaboration contracts"
```

### Task 2: Move traversal callbacks out of diagnostics

**Files:**

- Create: `src/Jazz/Compiler/TypeInference/Traversal.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs:3-20,128-150`
- Modify imports in: `src/Jazz/Compiler/TypeInference/Scope.hs`, `Pattern.hs`, `Capabilities.hs`, `Operator.hs`, `Signature.hs`, `TypeInference.hs`, and focused tests returned by `rg`
- Modify: `jazz.cabal`

**Interfaces:**

- Produces unchanged aliases:

```haskell
type InferExprFn =
  BuiltinResolutionMode -> TypeEnv -> InferState -> Expr ->
  (Maybe ExpressionType, InferState)

type InferExprWithModeFn =
  TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> Expr ->
  (InferredExpr, InferState)
```

- `TypeInference.Diagnostics` produces diagnostics only and no longer imports elaboration just to own callback aliases.

- [ ] **Step 1: Inventory exact callback consumers**

Run:

```bash
rg -n '\bInferExprFn\b|\bInferExprWithModeFn\b' src test -g '*.hs'
```

Expected: definitions in `Diagnostics` and consumers across scope, pattern,
capabilities, inference helpers, and focused ownership tests.

- [ ] **Step 2: Create the neutral traversal module**

Implement the two aliases exactly as shown. The module imports only AST,
builtin mode, `Elaboration.Types`, inference state, and inference types. It
must not import `TypeInference.Diagnostics` or the `Elaboration` façade.

- [ ] **Step 3: Migrate all consumers and remove diagnostic ownership**

Delete both aliases from `Diagnostics` and its export list. Import
`TypeInference.Traversal` explicitly wherever either alias appears. Do not
replace the aliases with repeated long function types.

- [ ] **Step 4: Verify the dependency move**

Run:

```bash
rg -n '^type InferExpr' src/Jazz/Compiler
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec adt-pattern-type-spec -fdevelopment --test-show-details=direct
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: both aliases are defined only in `Traversal`; suites and build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypeInference/Traversal.hs src/Jazz/Compiler/TypeInference/Diagnostics.hs src/Jazz/Compiler/TypeInference/Scope.hs src/Jazz/Compiler/TypeInference/Pattern.hs src/Jazz/Compiler/TypeInference/Capabilities.hs src/Jazz/Compiler/TypeInference/Operator.hs src/Jazz/Compiler/TypeInference/Signature.hs src/Jazz/Compiler/TypeInference.hs test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
git commit -m "refactor: give inference traversal types a neutral owner"
```

### Task 3: Replace the scope positional API with a named request

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs:330-455`
- Characterization test: `test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`
- Characterization test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces private types and function:

```haskell
data ForwardSignedFunctionsPolicy
  = ForbidForwardSignedFunctions
  | PermitForwardSignedFunctions

data ScopeInferenceRequest = ScopeInferenceRequest
  { scopeForwardSignedFunctionsPolicy :: ForwardSignedFunctionsPolicy,
    scopePreludeStatementIndices :: Set Int,
    scopeInferExpression :: InferExprWithModeFn,
    scopeProductionMode :: TypedCoreProductionMode,
    scopeBuiltinMode :: BuiltinResolutionMode,
    scopeInitialEnv :: TypeEnv,
    scopeInitialState :: InferState,
    scopePreparedInference :: PreparedInferenceScope
  }

inferScopeTypeInternal ::
  ScopeInferenceRequest ->
  (InferredExpr, InferState, Map Int (Name, SourceSpan))
```

- Existing exported scope convenience signatures remain unchanged in this task.

- [ ] **Step 1: Run scope ownership and producer characterization**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Define the policy and request beside `PreparedInferenceScope`**

Add the exact private declarations above. Do not derive `Eq` or `Show` for the
request because it contains a function field. Replace every `True`/`False`
mode at the five current call sites with a named policy constructor.

- [ ] **Step 3: Rewrite the internal function to destructure one record**

Use this record pattern to bind the request fields at the start of the
equation:

```haskell
ScopeInferenceRequest
    { scopeForwardSignedFunctionsPolicy,
      scopePreludeStatementIndices,
      scopeInferExpression,
      scopeProductionMode,
      scopeBuiltinMode,
      scopeInitialEnv,
      scopeInitialState,
      scopePreparedInference = PreparedInferenceScope statements bindingNamesByStatement recursiveGroupsByStatement
    }
```

Enable `NamedFieldPuns` locally if needed. Replace the old Boolean guard with:

```haskell
forwardSignedFunctionsPermitted :: ForwardSignedFunctionsPolicy -> Bool
forwardSignedFunctionsPermitted policy =
  case policy of
    ForbidForwardSignedFunctions -> False
    PermitForwardSignedFunctions -> True
```

Do not derive `Eq` only to test this policy. Do not add a new state monad or
combine the environment and solver state.

- [ ] **Step 4: Verify exact scope and production behavior**

Run:

```bash
rg -n 'inferScopeTypeInternal (True|False)|inferScopeTypeInternal :: Bool' src/Jazz/Compiler/TypeInference/Scope.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec jazz-typed-core-expression-direct-call-spec pattern-coverage-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: `rg` has no matches; all suites PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/TypeInference/Scope.hs
git commit -m "refactor: name scope inference inputs"
```

### Task 4: Name finalization environment and location state

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs:325-1030`
- Characterization test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces:

```haskell
data FinalizationEnv = FinalizationEnv
  { finalizationInferState :: InferState,
    finalizationModulePath :: [Text],
    finalizationFunctions :: Map Name FunctionProfile,
    finalizationCallableShapes :: Map Name TypedCallableShape,
    finalizationScalarCaptureTypes :: Map TypedBinderId ExpressionType,
    finalizationEagerClosureCaptureStatements :: Set Int
  }

data FinalizationLocation = FinalizationLocation
  { finalizationStatementIndex :: Int,
    finalizationChildPath :: [Int],
    finalizationParameters :: Map Name TypedBinderId,
    finalizationScalarBindings :: Map Name TypedBinderId,
    finalizationExpressionEvaluation :: ExpressionEvaluation,
    finalizationExpressionRole :: ExpressionRole
  }

finalizeExpression ::
  FinalizationEnv -> FinalizationLocation -> ProvisionalTypedExpr ->
  ([TypedCoreProductionFailure], Maybe TypedExpr)
```

- [ ] **Step 1: Record exact child-path and failure-order baselines**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS, especially compound failure accumulation, same-statement
failure order, pattern paths, call spines, captures, and recursion fixtures.

- [ ] **Step 2: Add the two records to `Elaboration.Types`**

Add the exact declarations above and export their constructors only to
elaboration internals. This module may import `InferState`; `TypeInference.State`
does not import elaboration, so the dependency stays acyclic.

- [ ] **Step 3: Convert `finalizeExpression` and application-spine recursion**

Construct one `FinalizationEnv` after the profile analysis. At each statement,
construct a `FinalizationLocation`. Replace positional recursion with record
updates, for example:

```haskell
childLocation index role location =
  location
    { finalizationChildPath = finalizationChildPath location <> [index],
      finalizationExpressionRole = role
    }
```

For application spines, preserve the current repeated-zero callee path and
argument source order explicitly; do not derive paths from list length in a
new way. Lambda recursion updates only parameters, child path, evaluation, and
role. Statement traversal updates only statement index and scalar bindings.

- [ ] **Step 4: Verify the long positional form is gone**

Run:

```bash
rg -n 'finalizeExpression scalarCaptureTypes|finalizeExpression recursiveScalarCaptureTypes' src/Jazz/Compiler/TypeInference/Elaboration.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: `rg` has no matches; exact producer and contract suites PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration.hs
git commit -m "refactor: name typed-core finalization context"
```

### Task 5: Split elaboration analysis, specialization, and construction

**Files:**

- Create: `src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs`
- Create: `src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs`
- Create: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `jazz.cabal`

**Interfaces:**

- `Specialize` produces the current top-level specialization functions with unchanged signatures, including:

```haskell
specializeInferredExpression :: InferState -> ExpressionType -> InferredExpr -> InferredExpr
specializeProvisionalExpression :: InferState -> Maybe ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
specializeProvisionalCallableCapture :: InferState -> ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
provisionalExpressionType :: InferState -> ProvisionalTypedExpr -> Maybe ExpressionType
specializeExpressionType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
```

- `Profiles` produces:

```haskell
data FinalizationProfile = FinalizationProfile
  { profileFunctions :: Map Name FunctionProfile,
    profileCallableShapes :: Map Name TypedCallableShape,
    profileReboundFunctions :: Map Int Name,
    profileTypedRecursiveGroups :: [TypedRecursiveGroup],
    profileUnsupportedRecursiveBinders :: Set TypedBinderId,
    profileUnavailableClosureCaptureBinders :: Set TypedBinderId,
    profileRecursiveScalarCaptureTypes :: Map TypedBinderId ExpressionType,
    profileEagerClosureCaptureStatements :: Set Int
  }

analyzeFinalizationProfile ::
  InferState -> [Text] -> [ProvisionalTypedStatement] -> FinalizationProfile
```

- `Finalize` produces the existing `finalizeValidatedTypedCoreExpressionDirectCall` signature.
- `Elaboration` re-exports only active contracts and functions.

- [ ] **Step 1: Extract specialization as a mechanical move**

Move the top-level functions from `specializeInferredExpression` through
`defaultScalarLiterals` into `Specialize`. Include every private helper those
functions require. Import `Specialize` from current elaboration and run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS before profile/finalization extraction.

- [ ] **Step 2: Promote profile analysis behind one result record**

Move the current local helpers beginning with `functionTable`, callable shape
collection, rebinding/recursive-group analysis, dependency/capture walks, and
`supportedRecursiveProfile` to `Profiles`. Functions that construct Typed Core
nodes or failures remain in `Finalize`.

`analyzeFinalizationProfile` performs the existing sequence exactly:

```haskell
baseFunctions -> declarations -> callableShapes -> reboundFunctions
  -> typedRecursiveGroups -> supportedRecursiveProfile
  -> specializeFunctionProfiles -> FinalizationProfile
```

Do not add a fixed-point framework; preserve the current explicit convergence
loops and deterministic list/map ordering.

- [ ] **Step 3: Move final construction and leave a façade**

Move `finalizeValidatedTypedCoreExpressionDirectCall`, statement/expression
construction, export construction, node/type/recipe construction, literal
construction, and failure qualification to `Finalize`. It imports
`analyzeFinalizationProfile` and `Specialize` through their exact interfaces.

Reduce `Elaboration.hs` to explicit imports/re-exports plus
`expressionDependencyNames` and `isTypedCoreDirectCallOperator` only if those
remain natural façade helpers. The façade must contain no provisional ADT
definitions and no recursive finalizer body.

- [ ] **Step 4: Verify ownership and artifacts**

Run:

```bash
rg -n '^data Provisional|^data TypedCoreProduction|^data FinalizationProfile|^finalizeValidatedTypedCoreExpressionDirectCall' src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypeInference/Elaboration
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: each declaration has one focused owner; exact Typed Core and Lowered IR suites PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs
git commit -m "refactor: split typed-core elaboration responsibilities"
```

### Task 6: Collapse the inference wrapper matrix

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference.hs:6-28,200-335,640-655`
- Modify exact active consumers only if an export is removed
- Characterization tests: `test/Jazz/Compiler/ProfilingSpec.hs`, `CoreNormalizationSpec.hs`, `PatternCoverageSpec.hs`, runtime capabilities tests

**Interfaces:**

- Produces private request/function:

```haskell
data InferenceRequest = InferenceRequest
  { requestedInferenceInputs :: InferenceInputs,
    requestedHiddenStatementIndices :: Set Int,
    requestedPreludeStatementIndices :: Set Int
  }

inferExpressionWithRequest :: InferenceRequest -> Expr -> IO InferenceResult
```

- Retains active conveniences: `inferExpressionDefault`, `inferExpressionWithBuiltins`, `inferExpressionWithBuiltinsAndSourceUnitStatements`, `inferExpressionWithInputs`, and `inferExpressionWithInputsAndHiddenStatements`.
- Removes exports with no active consumer, initially `inferExpression` and `inferExpressionWithBuiltinsAndHiddenStatements`, after exact call-site confirmation.

- [ ] **Step 1: Confirm wrapper consumers by exact symbol**

Run:

```bash
rg -n '\binferExpression(Default|WithBuiltins|WithBuiltinsAndHiddenStatements|WithBuiltinsAndSourceUnitStatements|WithInputs|WithInputsAndHiddenStatements|WithInputsAndSourceUnitStatements)?\b' src app test benchmark -g '*.hs'
```

Expected: plain `inferExpression` and the builtins-plus-hidden wrapper have no
consumer outside `TypeInference`; other named conveniences have active compiler
or test consumers.

- [ ] **Step 2: Introduce one private request path**

Move the body of `inferExpressionWithInputsAndSourceUnitStatements` to
`inferExpressionWithRequest`. Make every retained convenience construct the
record directly. Preserve the existing rule that the hidden set is also the
prelude/source-unit set for `inferExpressionWithInputsAndHiddenStatements`.

- [ ] **Step 3: Remove dead exports and forwarding definitions**

Delete `inferExpression` and
`inferExpressionWithBuiltinsAndHiddenStatements` from the export list and
implementation after inlining their record construction into the retained
callers. Keep `InferenceRequest` private.

- [ ] **Step 4: Verify active callers and diagnostics**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test profiling-spec core-normalization-spec pattern-coverage-spec runtime-semantics-spec jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS with unchanged inference and production results.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/Driver.hs src/Jazz/Compiler/ModuleCompiler.hs test/Jazz/Compiler/ProfilingSpec.hs test/Jazz/Compiler/Semantics/CoreNormalizationSpec.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs
git commit -m "refactor: simplify inference entry points"
```

### Task 7: Close milestone 2

**Files:**

- Verify all milestone-2 files; modify only verified formatter output.

**Interfaces:**

- Consumes: tasks 1-6.
- Produces: a clean inference/elaboration dependency graph for lowering/runtime work.

- [ ] **Step 1: Format touched Haskell with the repository-compatible formatter**

Use the exact formatter/version established in milestone 1. Limit the command
to `TypeInference.hs`, the touched `TypeInference/` modules, and changed test
modules.

- [ ] **Step 2: Run all inference and bootstrap contracts serially**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test binding-signature-coherence-spec adt-pattern-type-spec pattern-coverage-spec profiling-spec core-normalization-spec jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 3: Run development build and package checks**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal check
git diff --check
```

Expected: PASS.

- [ ] **Step 4: Inspect final ownership and call shapes**

```bash
rg -n '^type InferExpr|^data Provisional|^data Finalization(Profile|Env|Location)|^data ScopeInferenceRequest' src/Jazz/Compiler/TypeInference
rg -n 'inferScopeTypeInternal :: Bool|finalizeExpression scalarCaptureTypes|finalizeExpression recursiveScalarCaptureTypes' src/Jazz/Compiler/TypeInference
git status --short
```

Expected: each contract has one focused owner; old positional signatures have no matches; only intended changes are present.

- [ ] **Step 5: Commit verified formatting changes if present**

```bash
git add jazz.cabal src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/TypeInference/Traversal.hs src/Jazz/Compiler/TypeInference/Diagnostics.hs src/Jazz/Compiler/TypeInference/Scope.hs src/Jazz/Compiler/TypeInference/Pattern.hs src/Jazz/Compiler/TypeInference/Capabilities.hs src/Jazz/Compiler/TypeInference/Operator.hs src/Jazz/Compiler/TypeInference/Signature.hs src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypeInference/Elaboration/Types.hs src/Jazz/Compiler/TypeInference/Elaboration/Specialize.hs src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
git commit -m "chore: close maintainability milestone 2"
```

Run this commit only when formatting changed tracked content; do not create an empty commit.
