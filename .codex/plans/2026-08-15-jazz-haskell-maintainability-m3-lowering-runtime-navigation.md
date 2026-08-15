# Jazz Haskell Maintainability Milestone 3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Separate Lowered IR analysis from emission, expose runtime execution through named requests, and turn `Runtime.hs` into a focused façade over the evaluator’s genuine mutually recursive engine.

**Architecture:** Lowering becomes an explicit `requirements -> shapes/profile -> emission` pipeline with immutable analysis results and isolated emission state. Runtime request and host-evaluation ownership are extracted first; the source-confirmed machine/scope/apply strongly connected component moves intact to `Runtime.Engine`, avoiding invented callbacks and module cycles while still separating public navigation from implementation.

**Tech Stack:** Haskell 2010 with existing local extensions, GHC 9.14.1, Cabal private library/test suites, `containers`, `transformers`, `text`, checked-in Nix development shell.

## Global Constraints

- Milestones 1 and 2 must be complete and green before this plan starts.
- Preserve public Jazz semantics, runtime observation, host-effect order, tail-call behavior, and exact Typed Core/Lowered IR artifacts.
- Preserve `ValidatedTypedProgram`; raw Typed Core must still cross validation before trusted lowering.
- Analysis modules must not allocate blocks or temporaries; emission must not rediscover semantic requirements or callable profiles.
- Runtime observation remains separate from runtime outcome, and pure execution remains separate from host execution.
- Do not add a generic lowering framework, visitor abstraction, evaluator callback record, global compiler context, or new module cycle.
- Cabal-private wrappers with no active source/test consumer may be removed.
- Use the checked-in Nix development shell, format only touched Haskell, and commit each task after focused verification.

**Design source:** `.codex/plans/2026-08-15-jazz-haskell-maintainability-design.md`

---

## File map

| File | Responsibility after this milestone |
| --- | --- |
| `src/Jazz/Compiler/LoweredIR/Lower/Types.hs` | Lowering failures/results, requirements, shape contracts, analysis result, emission state types |
| `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs` | Structural runtime-layout/service requirement collection |
| `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs` | Function/capture discovery and supported-profile validation |
| `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs` | CFG, block, operand, closure, call, tail-call, and runtime-service emission |
| `src/Jazz/Compiler/LoweredIR/Lower.hs` | Checked/raw entry points and small analysis-to-emission façade |
| `src/Jazz/Compiler/Runtime/Request.hs` | Named expression and scope execution requests |
| `src/Jazz/Compiler/Runtime/HostEvaluation.hs` | Runtime host-state runners and observation-state setup |
| `src/Jazz/Compiler/Runtime/Engine.hs` | Mutually recursive evaluator machine, scope, forcing, and application engine |
| `src/Jazz/Compiler/Runtime.hs` | Actively used compatibility façade and value re-exports |
| `src/Jazz/Compiler/Driver.hs` | Actively used compile/run entry points only |
| `jazz.cabal` | Registers private modules and focused test helpers |

### Task 1: Extract runtime-support requirement collection

**Files:**

- Create: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Create: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs:40-105,456-593`
- Modify: `jazz.cabal`
- Characterization tests: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`, `JazzLoweredIRContractSpec.hs`

**Interfaces:**

- `Lower.Types` initially owns unchanged public failure/result ADTs and:

```haskell
data RuntimeRequirements = RuntimeRequirements
  { runtimeRequiresTextLayout :: Bool,
    runtimeRequiredServices :: Set RuntimeServiceKey
  }
  deriving (Eq, Show)
```

- `Requirements` produces:

```haskell
collectRuntimeRequirements :: TypedModule -> RuntimeRequirements
requiredRuntimeLayouts :: RuntimeRequirements -> [LoweredLayout]
```

`requiredRuntimeLayouts` returns only semantic layouts owned by requirements;
closure-environment layouts remain shape-owned.

- [ ] **Step 1: Run exact layout/service characterization**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS, including managed Text transport, equality, runtime-service ordering, service omission, and closure layouts.

- [ ] **Step 2: Move shared ADTs to `Lower.Types`**

Move `LoweredIRLoweringKind`, `LoweredIRLoweringDetail`,
`LoweredIRLoweringFailure`, `LoweredIRLoweringResult`, and
`RuntimeRequirements` without changing constructors or derivations. Import and
re-export the public failure/result ADTs from `Lower`.

- [ ] **Step 3: Move the structural requirement walk**

Move `collectRuntimeRequirements`, the empty/merge helpers, all
`requirementsFor*` walkers, and runtime-service recognition to
`Requirements`. Keep traversal order and service-set semantics unchanged.

Implement:

```haskell
requiredRuntimeLayouts requirements =
  [textLayout | runtimeRequiresTextLayout requirements]
```

Do not import lowering state, function shapes, or emission functions.

- [ ] **Step 4: Verify the extraction and dependency boundary**

```bash
rg -n 'LoweringState|FunctionShape|emit|lowerExpression' src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: the first `rg` has no matches; suites and build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/LoweredIR/Lower.hs src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs
git commit -m "refactor: extract lowering requirements"
```

### Task 2: Extract callable-shape and profile analysis

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Create: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs:99-150,201-455,594-1722`
- Modify: `jazz.cabal`
- Characterization tests: direct-call and Lowered IR contract suites

**Interfaces:**

- `Lower.Types` owns unchanged `FunctionParameterShape`,
`FunctionDeclaration`, `CaptureShape`, `FunctionShape`, and `FunctionIndex`.
- It adds:

```haskell
data LoweringAnalysis = LoweringAnalysis
  { analyzedModulePath :: [Text],
    analyzedStatements :: [TypedStatement],
    analyzedFunctionShapes :: [FunctionShape],
    analyzedFunctionIndex :: FunctionIndex,
    analyzedResultRepresentation :: LoweredRepresentation,
    analyzedRuntimeRequirements :: RuntimeRequirements
  }
```

- `Shapes` produces:

```haskell
analyzeTypedModule ::
  TypedModule -> Either [LoweredIRLoweringFailure] LoweringAnalysis

orderedClosureLayouts :: [FunctionShape] -> [LoweredLayout]
```

- [ ] **Step 1: Capture profile-failure order before moving code**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS, including invalid function shape, duplicate identities,
captures, recursion, callable values, arity, non-local calls, and structural
failure ordering.

- [ ] **Step 2: Move shape contracts and pure discovery**

Move the shape/index records to `Lower.Types`. Move `collectFunctionShapes`,
recursive closure-group application, stable capture union, closure layouts,
root/generated function discovery, parameter/capture discovery, identity
helpers, and representation inspection to `Shapes`.

These functions may construct lowering failures but must not import
`LoweringState` or allocate `LoweredBlockId`/`LoweredTemporaryId` values for
emission.

- [ ] **Step 3: Move supported-profile inspection behind `analyzeTypedModule`**

Move `supportedModuleMetadata`, `validateStatementProfiles`, expression and
application inspection, recursive-group profile checks, and failure ordering.
`analyzeTypedModule` reproduces the old `lowerValidatedModule` analysis order:

```text
module metadata failures
  <> result representation failures
  <> statement-owned recursive, shape, and expression profile failures
```

On success it constructs `FunctionIndex` once and returns the exact
`LoweringAnalysis` record. It calls `collectRuntimeRequirements` but performs no
emission.

- [ ] **Step 4: Verify analysis has no emission state**

```bash
rg -n 'LoweringState|loweringInstructions|finishCurrentBlock|startBlock|emitFunction|emitEntry' src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: `rg` has no matches; suites PASS with exact failure order.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/LoweredIR/Lower.hs src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
git commit -m "refactor: separate lowering shape analysis"
```

### Task 3: Isolate CFG and instruction emission

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Create: `src/Jazz/Compiler/LoweredIR/Lower/Emit.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs:151-320,1723-3682`
- Modify: `jazz.cabal`

**Interfaces:**

- `Lower.Types` owns private emission records `LoweringState`,
`ResultDestination`, and `AmbientSlot`.
- `Emit` produces:

```haskell
emitAnalyzedModule ::
  LoweringAnalysis -> Either [LoweredIRLoweringFailure] LoweredProgram
```

- `Lower` retains:

```haskell
lowerTypedCoreExpressionDirectCall :: TypedProgram -> LoweredIRLoweringResult
lowerValidatedTypedCoreExpressionDirectCall :: ValidatedTypedProgram -> LoweredIRLoweringResult

selectSupportedEntryModule ::
  TypedProgram -> Either [LoweredIRLoweringFailure] TypedModule
```

- [ ] **Step 1: Move emission-only state types**

Move `LoweringState`, `ResultDestination`, and `AmbientSlot` to `Lower.Types`.
Keep constructors internal to `Lower.Emit`; other modules consume only shape
and analysis records.

- [ ] **Step 2: Move block, function, expression, and operation emission**

Move the functions beginning with `finishCurrentBlock` through
`unsupportedRepresentation`, plus `emitFunction`, `emitEntry`, closure and
recursive-environment construction, conditionals, scalar pattern cases,
applications, runtime-service instructions, and representation-at-path
failure construction to `Emit`.

`emitAnalyzedModule` combines:

```haskell
requiredRuntimeLayouts (analyzedRuntimeRequirements analysis)
  <> orderedClosureLayouts (analyzedFunctionShapes analysis)
```

and preserves the catalog-owned runtime-service ordering.

- [ ] **Step 3: Reduce `Lower` to validation and orchestration**

Implement the trusted path as:

```haskell
lowerValidatedProgram typedProgram = do
  typedModule <- selectSupportedEntryModule typedProgram
  analysis <- analyzeTypedModule typedModule
  emitAnalyzedModule analysis
```

Keep the existing program-shape failure construction and ordering inside
`selectSupportedEntryModule`; do not turn it into `head`, `fromJust`, or another
partial operation. Keep post-emission `validateLoweredProgram` in the façade.

- [ ] **Step 4: Verify exact emitted artifacts and ownership**

```bash
rg -n 'loweringNextTemporary|finishCurrentBlock|lowerExpression|emitRuntimeServiceInstruction' src/Jazz/Compiler/LoweredIR/Lower.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: façade `rg` has no matches; exact fixtures and build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/LoweredIR/Lower.hs src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs src/Jazz/Compiler/LoweredIR/Lower/Emit.hs
git commit -m "refactor: isolate lowered-ir emission"
```

### Task 4: Introduce named runtime requests

**Files:**

- Create: `src/Jazz/Compiler/Runtime/Request.hs`
- Modify: `src/Jazz/Compiler/Runtime.hs:345-565,637-835`
- Modify: `src/Jazz/Compiler/ModuleRuntime.hs` scope-evaluation call sites
- Modify: `jazz.cabal`
- Characterization tests: runtime semantics, observation, module pipeline

**Interfaces:**

- Produces:

```haskell
data RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpressionSourceUnitStatementIndices :: Set Int,
    runtimeExpressionBuiltinMode :: BuiltinResolutionMode,
    runtimeExpressionBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    runtimeExpression :: Expr
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeSourceUnitStatementIndices :: Set Int,
    runtimeScopeCurrentModulePath :: Maybe [Text],
    runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeBuiltinMode :: BuiltinResolutionMode,
    runtimeScopeBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScopeStatements :: [Statement]
  }
```

- Private request-driven evaluators take host and observation separately because those select execution strategy rather than describe the expression/scope.

- [ ] **Step 1: Run runtime behavior characterization**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec runtime-observation-spec module-pipeline-contract-spec cli-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Add the two request records**

Create `Runtime.Request` with the exact fields above. Do not derive `Show` for
`RuntimeScopeRequest` if the opaque runtime environment would make that output
misleading. The request module imports runtime types but not the evaluator
engine or façade.

- [ ] **Step 3: Convert long internal expression calls**

Add:

```haskell
evaluateRuntimeExpressionObserved :: Monad m =>
  RuntimeObservationRequest -> RuntimeHost m -> RuntimeExpressionRequest ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
```

Make the current public expression conveniences construct
`RuntimeExpressionRequest`. Preserve the optimized unobserved/pure branch and
the required-host branch exactly.

- [ ] **Step 4: Convert long internal scope calls**

Add request-driven internal forms for required host, evaluation host, ordinary
host, and pure evaluation with these exact signatures:

```haskell
evaluateRuntimeScopeWithHostRequest :: Monad m =>
  RuntimeHost m -> RuntimeScopeRequest -> m (Either Diagnostic ScopeResult)

evaluateRuntimeScopeWithRequiredHostRequest :: Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeScopeRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)

evaluateRuntimeScopeWithEvaluationHostRequest :: Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeScopeRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)

evaluateRuntimeScopePureRequest ::
  RuntimeScopeRequest -> Either Diagnostic ScopeResult
```

Each receives one `RuntimeScopeRequest`; it must not split the request back
into a new positional forwarding chain.

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec runtime-observation-spec module-pipeline-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/Runtime/Request.hs src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/ModuleRuntime.hs
git commit -m "refactor: name runtime execution requests"
```

### Task 5: Extract host evaluation and the runtime engine

**Files:**

- Create: `src/Jazz/Compiler/Runtime/HostEvaluation.hs`
- Create: `src/Jazz/Compiler/Runtime/Engine.hs`
- Modify: `src/Jazz/Compiler/Runtime.hs`
- Modify imports in exact internal consumers
- Modify: `jazz.cabal`

**Interfaces:**

- `HostEvaluation` produces existing runners unchanged:

```haskell
runRuntimeHostEvaluation :: Monad m =>
  RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m value

runRuntimeHostEvaluationWithObservation :: Monad m =>
  RuntimeObservationRequest -> RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m (value, RuntimeObservationState)
```

- `Engine` produces the request-driven expression/scope evaluators
  `evaluateRuntimeExpressionObserved`, `evaluateRuntimeScopeWithHostRequest`,
  `evaluateRuntimeScopeWithRequiredHostRequest`,
  `evaluateRuntimeScopeWithEvaluationHostRequest`, and
  `evaluateRuntimeScopePureRequest`, plus the currently exported semantic
  helpers `runtimeExprRequiresHost`, `runtimeValueExactlyMatchesConstraint`,
  `renderRuntimeValue`, and `untypedIntMetadata`.
- `Runtime` re-exports active value/types and defines thin compatibility conveniences only.

- [ ] **Step 1: Extract host-state lifecycle helpers**

Move `runRuntimeHostEvaluation`, observation setup, deferred scope-id/cache
updates, and host lifting from the pre-evaluator region to `HostEvaluation`.
Keep observation-state initialization/finalization order unchanged. Register
the module and run runtime observation tests.

- [ ] **Step 2: Confirm the evaluator strongly connected component**

Before moving code, run and retain the evidence:

```bash
sed -n '1815,2600p' src/Jazz/Compiler/Runtime.hs | rg -n 'evalScopeWithHost|forceRuntimeValueWithHost|applyRuntimeFunctionWithHost'
sed -n '2600,3415p' src/Jazz/Compiler/Runtime.hs | rg -n 'runEvaluationMachine|runCallableMachine|evalScopeWithHost|forceRuntimeValueWithHost'
```

Expected: machine stepping calls scope/forcing/application, and scope/forcing/application call machine entry points. This justifies one `Runtime.Engine` rather than cyclic `Machine`, `Scope`, and `Apply` modules.

- [ ] **Step 3: Move the evaluator implementation intact**

Move `EvaluationContext`, control/frame/continuation/machine types, public-scope
implementation, pure and host scope traversal, value evaluation, machine
stepping/resumption, forcing, application, builtin/host operations, pattern
continuation, and rendering/semantic helpers into `Runtime.Engine`.

Keep mutually recursive functions in the same module. Do not introduce a
service callback record. Import existing focused modules (`Runtime.Types`,
`Runtime.Semantics`, `Runtime.Primitives`, `Runtime.ScopePlan`,
`Runtime.Observation`, `Runtime.HostEvaluation`) directly.

- [ ] **Step 4: Rebuild `Runtime.hs` as the active façade**

The façade imports request-driven engine functions and defines the current
active conveniences, including these exact signatures:

```haskell
evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithHost :: Monad m => RuntimeHost m -> Expr -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateModuleScope :: Maybe [Text] -> ModuleEvaluationMode -> BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> [Statement] -> Either Diagnostic ScopeResult
```

Keep existing exports needed by `src`, `app`, and `test`; do not re-export
engine-only machine types or helpers.

- [ ] **Step 5: Verify the façade and engine**

```bash
rg -n '^data (EvaluationContext|EvaluationControl|EvaluationFrame|EvaluationMachine)|^runEvaluationMachine|^evalScopeWithHost|^applyRuntimeFunctionWithHost' src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/Runtime/Engine.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-semantics-spec runtime-observation-spec module-pipeline-contract-spec profiling-spec cli-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: implementation declarations exist only in `Runtime.Engine`; suites and build PASS.

- [ ] **Step 6: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/Runtime/Engine.hs src/Jazz/Compiler/Runtime/HostEvaluation.hs src/Jazz/Compiler/Runtime/Request.hs
git commit -m "refactor: separate runtime facade from evaluator engine"
```

### Task 6: Prune unused runtime and driver wrappers

**Files:**

- Modify: `src/Jazz/Compiler/Runtime.hs` export list and forwarding functions
- Modify: `src/Jazz/Compiler/Driver.hs:6-50,260-410,420-530`

**Interfaces:**

- Removes only zero-consumer Cabal-private exports/functions.
- Retains request-driven internal functions and actively used public conveniences.

- [ ] **Step 1: Re-run exact call-site counts after the engine move**

```bash
rg -n -w 'evaluateRuntimeExprWithHostObserved|evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements|evaluateModuleScopeWithHostAndSourceUnitStatements' src app test benchmark -g '*.hs'
rg -n -w 'runExpr|runExprObserved|runExprWithHost|runExprWithHostObserved|runSourceWithHost|runSourceWithHostObserved|runSourceWithResolvedPrelude|runSourceWithResolvedPreludeAndHost|runModuleGraphWithHost|runModuleGraphWithHostObserved|runModuleGraphWithResolvedPreludeAndHost' src app test benchmark -g '*.hs'
```

Expected from the audited baseline:

- the three long runtime wrappers are façade-internal only;
- the complete `runExpr*` family has no consumer outside `Driver`;
- `runSourceWithHost`, `runSourceWithResolvedPrelude`, and
  `runModuleGraphWithHost` are definition-only;
- `runSourceWithHostObserved`, `runSourceWithResolvedPreludeAndHost`,
  `runModuleGraphWithHostObserved`, and
  `runModuleGraphWithResolvedPreludeAndHost` are used only as private workers
  by active source or module-graph conveniences.

- [ ] **Step 2: Remove dead runtime exports**

Remove the three long/internal wrappers from the `Runtime` export list and
delete their forwarding definitions. Change active façade conveniences to
construct the request and call the request-driven engine functions directly.

- [ ] **Step 3: Remove dead driver functions and narrow exports**

Delete `runExpr`, `runExprObserved`, `runExprWithHost`, and
`runExprWithHostObserved`. Delete the definition-only unobserved wrappers
`runSourceWithHost`, `runSourceWithResolvedPrelude`, and
`runModuleGraphWithHost`. Keep their observed/host workers private when active
wrappers call them. Remove every deleted or now-private name from the export
list.

- [ ] **Step 4: Verify active CLI, source, and module execution**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test cli-spec runtime-semantics-spec loader-spec module-pipeline-contract-spec prelude-loading-spec program-corpus-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/Driver.hs
git commit -m "refactor: prune unused execution wrappers"
```

### Task 7: Close milestone 3

**Files:**

- Verify all milestone-3 files; modify only verified formatter output.

**Interfaces:**

- Consumes: tasks 1-6.
- Produces: clean lowering analysis/emission and runtime façade/engine boundaries.

- [ ] **Step 1: Format touched Haskell with the established compatible formatter**

Limit formatting to `LoweredIR/Lower*`, `Runtime*`, `Driver.hs`,
`ModuleRuntime.hs`, and changed focused tests.

- [ ] **Step 2: Run focused compiler/runtime suites serially**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec runtime-semantics-spec runtime-observation-spec loader-spec module-pipeline-contract-spec prelude-loading-spec profiling-spec cli-spec program-corpus-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 3: Run development build and package checks**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal check
git diff --check
```

Expected: PASS.

- [ ] **Step 4: Inspect dependency ownership**

```bash
rg -n 'LoweringState|finishCurrentBlock|lowerExpression' src/Jazz/Compiler/LoweredIR/Lower.hs src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs
rg -n '^data EvaluationMachine|^runEvaluationMachine|^evalScopeWithHost|^applyRuntimeFunctionWithHost' src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/Runtime/Engine.hs
git status --short
```

Expected: emission state appears only in `Lower.Emit`/`Lower.Types`; runtime engine declarations appear only in `Runtime.Engine`; only intentional changes are present.

- [ ] **Step 5: Commit verified formatting changes if present**

```bash
git add jazz.cabal src/Jazz/Compiler/LoweredIR/Lower.hs src/Jazz/Compiler/LoweredIR/Lower/Types.hs src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs src/Jazz/Compiler/LoweredIR/Lower/Emit.hs src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/Runtime/Request.hs src/Jazz/Compiler/Runtime/HostEvaluation.hs src/Jazz/Compiler/Runtime/Engine.hs src/Jazz/Compiler/ModuleRuntime.hs src/Jazz/Compiler/Driver.hs
git commit -m "chore: close maintainability milestone 3"
```

Run this commit only when formatting changed tracked content; do not create an empty commit.
