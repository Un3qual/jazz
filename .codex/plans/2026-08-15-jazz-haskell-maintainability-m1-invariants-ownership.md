# Jazz Haskell Maintainability Milestone 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the audited partial states and give runtime outcomes, inference results, diagnostic strictness, and resolved modules one implementation owner each.

**Architecture:** Narrow types first, then move shared contracts into neutral leaf modules before deleting compatibility duplicates. Preserve all public Jazz behavior and diagnostic ordering; Cabal-private Haskell APIs and tests migrate to the canonical owners.

**Tech Stack:** Haskell 2010 with the module-local extensions already in use, GHC 9.14.1, Cabal private library/test suites, `containers`, `text`, checked-in Nix development shell.

## Global Constraints

- Preserve public Jazz syntax and semantics.
- Preserve `ValidatedTypedProgram` and all existing phase-proof boundaries.
- Preserve diagnostic content, ordering, and failure precedence.
- Keep ordinary canonical-core execution separate from opt-in Typed Core and Lowered IR production.
- Do not introduce a universal AST, phase-wide typeclass hierarchy, generic visitor framework, global `CompilerContext`, or file-size CI budget.
- Change only active root paths: `src/`, `app/`, `test/`, `jazz.cabal`, and internal `.codex/` execution state.
- Use the checked-in Nix development shell for authoritative builds and tests.
- Format only touched Haskell; do not mass-format unrelated files.
- Commit each task after its focused verification passes.

**Design source:** `.codex/plans/2026-08-15-jazz-haskell-maintainability-design.md`

---

## File map

| File | Responsibility after this milestone |
| --- | --- |
| `src/Jazz/Compiler/TypeInference/Pattern.hs` | Pattern inference with a pattern-specific binder map; no general `TypeBinding` storage |
| `src/Jazz/Compiler/TypedCore/Validate.hs` | Existing validator with total recursive-group lookup until the milestone-4 split |
| `src/Jazz/Compiler/Runtime/Outcome.hs` | Sole owner of runtime control/outcome types and compatibility adapters |
| `src/Jazz/Compiler/Runtime/Types.hs` | Runtime values/state; imports and re-exports `RuntimeControl` during migration |
| `src/Jazz/Compiler/Runtime/Observation.hs` | Observation data; imports and re-exports `RuntimeOutcome` during migration |
| `src/Jazz/Compiler/Diagnostics/Strictness.hs` | Sole owner of structural diagnostic forcing |
| `src/Jazz/Compiler/TypeInference/Result.hs` | Sole owner of `InferenceResult` |
| `src/Jazz/Compiler/ModuleGraph.hs` | Sole production `ResolvedModule` representation |
| `src/Jazz/Compiler/ModuleResolver.hs` | Resolves directly into graph modules with one module accumulator |
| `test/Jazz/Compiler/Runtime/OutcomeTests.hs` | Exact adapter and legacy-exit contract tests |
| `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs` | Resolver tests using rich modules and local summary projections |
| `jazz.cabal` | Registers the new private library and test modules |

### Task 1: Make pattern inference bindings unrepresentably plain

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs:35-45,307-432`
- Characterization test: `test/Jazz/Compiler/Semantics/AdtPatternTypeSpec.hs`

**Interfaces:**

- Consumes: `Name`, `ExpressionType`, and `TypeEnv` from existing inference modules.
- Produces: private `PatternBindings`, `emptyPatternBindings`, `singletonPatternBinding`, `lookupPatternBinding`, `mergePatternBindings`, and `extendTypeEnvWithPatternBindings`.

- [ ] **Step 1: Run the existing binder characterization suite**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test adt-pattern-type-spec -fdevelopment --test-show-details=direct
```

Expected: PASS, including common or-pattern binders, incompatible binder types, duplicate binders, constructor payload binders, list binders, tuple binders, and as-pattern binders.

- [ ] **Step 2: Introduce the private dedicated binder type**

Add beside `PatternTyping`:

```haskell
newtype PatternBindings = PatternBindings (Map Name ExpressionType)
  deriving (Eq, Show)

emptyPatternBindings :: PatternBindings
emptyPatternBindings = PatternBindings Map.empty

singletonPatternBinding :: Name -> ExpressionType -> PatternBindings
singletonPatternBinding name expressionType =
  PatternBindings (Map.singleton name expressionType)

lookupPatternBinding :: Name -> PatternBindings -> Maybe ExpressionType
lookupPatternBinding name (PatternBindings bindings) = Map.lookup name bindings

extendTypeEnvWithPatternBindings :: PatternBindings -> TypeEnv -> TypeEnv
extendTypeEnvWithPatternBindings (PatternBindings bindings) env =
  Map.foldlWithKey'
    (\extended name expressionType -> Map.insert name (PlainTypeBinding expressionType) extended)
    env
    bindings
```

Change `PatternTyping` so its binding field is `PatternBindings`. Update variable/as/constructor/list/tuple/or-pattern production and merging to use `ExpressionType` directly. Preserve left-to-right duplicate diagnostics and solver-state threading.

- [ ] **Step 3: Delete the partial projection**

Remove:

```haskell
patternBindingExpressionType :: TypeBinding -> ExpressionType
patternBindingExpressionType binding =
  case binding of
    PlainTypeBinding expressionType -> expressionType
    _ -> error "internal type inference error: non-plain case pattern binding"
```

At the arm/lambda environment boundary, call `extendTypeEnvWithPatternBindings`; do not expose `PatternBindings` from the module.

- [ ] **Step 4: Verify inference behavior and the absence of the escape hatch**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test adt-pattern-type-spec pattern-coverage-spec lambda-semantics-spec -fdevelopment --test-show-details=direct
rg -n '\berror\s*(\(|")' src/Jazz/Compiler/TypeInference/Pattern.hs
```

Expected: all three suites PASS; `rg` returns no matches.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/TypeInference/Pattern.hs
git commit -m "refactor: narrow pattern inference bindings"
```

### Task 2: Totalize recursive-group ordering validation

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs:1520-1605`
- Characterization test: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`

**Interfaces:**

- Consumes: `callableByBinder :: Map TypedBinderId (Int, TypedStatement)` after existing basic validation.
- Produces: total local `callableStatementIndex :: TypedBinderId -> Maybe Int` and unchanged ordered validation failures.

- [ ] **Step 1: Run recursive-group characterization tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct
```

Expected: PASS, including unknown members, duplicate members, member ordering, group ordering, and recursive reachability fixtures.

- [ ] **Step 2: Replace partial member lookups with total traversal**

Inside `rootRecursiveGroupFailures`, introduce:

```haskell
callableStatementIndex :: TypedBinderId -> Maybe Int
callableStatementIndex binderId = fst <$> Map.lookup binderId callableByBinder

memberStatementIndices :: [TypedBinderId] -> Maybe [Int]
memberStatementIndices = traverse callableStatementIndex
```

Rewrite `memberOrderingFailures` to compare indices only for
`Just memberIndices`, and rewrite `indexedFirstMembers` with `mapMaybe`:

```haskell
indexedFirstMembers =
  mapMaybe
    (\(groupIndex, TypedRecursiveGroup members) ->
       (\indices -> (groupIndex, minimum indices)) <$> nonEmptyMemberIndices members)
    (zip [0 :: Int ..] declaredGroups)

nonEmptyMemberIndices members = do
  indices <- memberStatementIndices members
  case indices of
    [] -> Nothing
    _ -> Just indices
```

Missing and empty members remain owned by `basicFailures`; no new failure or
ordering is introduced.

- [ ] **Step 3: Delete every qualified partial lookup in active compiler source**

Run:

```bash
rg -n 'Map\.!' src -g '*.hs'
```

Expected before the edit: exactly the three validator sites. Expected after the edit: no matches.

- [ ] **Step 4: Re-run the contract and development build**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/TypedCore/Validate.hs
git commit -m "refactor: totalize typed-core recursive lookups"
```

### Task 3: Centralize runtime outcomes and adapters

**Files:**

- Create: `src/Jazz/Compiler/Runtime/Outcome.hs`
- Create: `test/Jazz/Compiler/Runtime/OutcomeTests.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs:6-45,120-136`
- Modify: `src/Jazz/Compiler/Runtime/Observation.hs:3-45,195-210`
- Modify: `src/Jazz/Compiler/Runtime.hs:1-120,345-540,1658-1692`
- Modify: `src/Jazz/Compiler/ModuleRuntime.hs:1-45,110-250,330-365`
- Modify: `test/Jazz/Compiler/Runtime/ObservationSpec.hs`
- Modify: `jazz.cabal` private-library module list and `runtime-observation-spec.other-modules`

**Interfaces:**

- Consumes: `Diagnostic`, `E3020`, and `RuntimeOrigin`.
- Produces:

```haskell
data RuntimeControl = RuntimeDiagnostic Diagnostic | RuntimeExitRequested Integer

data RuntimeOutcome value
  = RuntimeOutcomeCompleted value
  | RuntimeOutcomeExited Integer
  | RuntimeOutcomeFailed Diagnostic

runtimeControlOutcome :: Either RuntimeControl value -> RuntimeOutcome value
diagnosticResultOutcome :: Either Diagnostic value -> RuntimeOutcome value
runtimeControlAsDiagnosticResult :: Either RuntimeControl value -> Either Diagnostic value
runtimeOutcomeAsDiagnosticResult :: RuntimeOutcome value -> Either Diagnostic value
runtimeExitNotRepresentableDiagnostic :: Integer -> Diagnostic
```

- [ ] **Step 1: Add failing adapter contract tests**

Create `OutcomeTests` exporting `tests :: [NamedTest]`. Cover completed,
diagnostic, and exit controls; completed/failed/exited outcomes; diagnostic
conversion; and exact `E3020` rendering for status `17`:

```haskell
assertEqual
  "legacy exit diagnostic"
  (Left (runtimeExitNotRepresentableDiagnostic 17))
  (runtimeOutcomeAsDiagnosticResult (RuntimeOutcomeExited 17 :: RuntimeOutcome ()))
```

Append `OutcomeTests.tests` to `ObservationSpec.tests`, register the module in
the Cabal test suite, and run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-observation-spec -fdevelopment --test-show-details=direct
```

Expected: FAIL because `Jazz.Compiler.Runtime.Outcome` does not exist.

- [ ] **Step 2: Create the neutral outcome module**

Implement the exact interface above. Define the exit diagnostic once:

```haskell
runtimeExitNotRepresentableDiagnostic status =
  mkErrorDiagnostic
    E3020
    RuntimeOrigin
    ("runtime exit status " <> Text.pack (show status) <> " cannot be represented by this legacy evaluator result")
```

Register `Jazz.Compiler.Runtime.Outcome` in the private library.

- [ ] **Step 3: Migrate type ownership without adding cycles**

Remove `RuntimeControl` from `Runtime.Types` and import/re-export it from
`Runtime.Outcome`. Remove `RuntimeOutcome` from `Runtime.Observation` and
import/re-export it from `Runtime.Outcome`. Update `Runtime`, `ModuleRuntime`,
tests, and any direct consumers to import the canonical owner where practical.

The required direction is:

```text
Diagnostics <- Runtime.Outcome <- Runtime.Types
Diagnostics <- Runtime.Outcome <- Runtime.Observation
Runtime.Types <-> Runtime.Observation remains only through existing runtime data
```

`Runtime.Outcome` must not import `Runtime.Types` or `Runtime.Observation`.

- [ ] **Step 4: Delete local adapters and verify exact behavior**

Delete `runtimeControlOutcome`, `runtimeControlAsDiagnosticResult`,
`diagnosticResultOutcome`, `runtimeOutcomeAsDiagnosticResult`, and duplicate
exit-diagnostic construction from `Runtime.hs` and `ModuleRuntime.hs`. Import
the canonical functions instead.

Run:

```bash
rg -n '^(runtimeControlOutcome|diagnosticResultOutcome|runtimeControlAsDiagnosticResult|runtimeOutcomeAsDiagnosticResult|runtimeExitNotRepresentableDiagnostic) ::' src/Jazz/Compiler
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test runtime-observation-spec runtime-semantics-spec module-pipeline-contract-spec cli-spec -fdevelopment --test-show-details=direct
```

Expected: each signature appears only in `Runtime/Outcome.hs`; all suites PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/Runtime/Outcome.hs src/Jazz/Compiler/Runtime/Types.hs src/Jazz/Compiler/Runtime/Observation.hs src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/ModuleRuntime.hs test/Jazz/Compiler/Runtime/ObservationSpec.hs test/Jazz/Compiler/Runtime/OutcomeTests.hs
git commit -m "refactor: centralize runtime outcomes"
```

### Task 4: Break the inference/forcing ownership cycle

**Files:**

- Create: `src/Jazz/Compiler/Diagnostics/Strictness.hs`
- Create: `src/Jazz/Compiler/TypeInference/Result.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs:1-35,180-235,400-455`
- Modify: `src/Jazz/Compiler/Force.hs:1-140,620-655,935-960`
- Modify imports in: `src/Jazz/Compiler/Driver.hs`, `src/Jazz/Compiler/ModuleCompiler.hs`, `test/Jazz/Compiler/ProfilingSpec.hs`, and other exact `InferenceResult` consumers reported by `rg`
- Modify: `jazz.cabal`

**Interfaces:**

- `TypeInference.Result` produces the existing record unchanged:

```haskell
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredDiagnostics :: [Diagnostic],
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey SignatureType,
    inferredModuleInterface :: ModuleInterface
  }
  deriving (Eq, Show)
```

- `Diagnostics.Strictness` produces `forceDiagnostic :: Diagnostic -> ()`.
- `TypeInference` and `Force` re-export existing names only where active consumers require compatibility.

- [ ] **Step 1: Record the current import/call-site inventory**

Run:

```bash
rg -n '\bInferenceResult\b|\bforceDiagnostic\b' src app test benchmark -g '*.hs'
```

Expected: `InferenceResult` is defined in `TypeInference`, imported by `Force`,
and consumed by compiler/tests; diagnostic forcing is implemented both in
`TypeInference` and `Force`.

- [ ] **Step 2: Move `InferenceResult` to its leaf module**

Create `TypeInference.Result` with the exact record above. Import it from
`TypeInference` and `Force`. Keep `TypeInference` re-exporting
`InferenceResult (..)` so ordinary inference callers do not churn; direct
structural consumers such as `Force` and `ModuleCompiler` should import the
owner.

Register the new module and run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS before changing forcing ownership.

- [ ] **Step 3: Move diagnostic forcing to the neutral module**

Implement:

```haskell
forceDiagnostic :: Diagnostic -> ()
forceDiagnostic diagnostic =
  diagnosticSeverity diagnostic `seq`
    diagnosticCode diagnostic `seq`
      diagnosticWarningCategory diagnostic `seq`
        diagnosticOrigin diagnostic `seq`
          diagnosticSummary diagnostic `seq`
            forceMaybeWith forceDiagnosticLabel (diagnosticPrimaryLabel diagnostic) `seq`
              forceListWith forceDiagnosticLabel (diagnosticSecondaryLabels diagnostic) `seq`
                forceMaybeWith (\subject -> subject `seq` ()) (diagnosticSubject diagnostic) `seq`
                  forceListWith (\note -> note `seq` ()) (diagnosticNotes diagnostic) `seq`
                    forceMaybeWith (\helpText -> helpText `seq` ()) (diagnosticHelp diagnostic)
```

Keep its tiny `forceMaybeWith`, `forceListWith`, and private
`forceDiagnosticLabel` local so the neutral module does not import `Force`.
Replace `forceInferenceDiagnostic` calls with this canonical function. Remove
both duplicate label implementations. `Force` may re-export the imported
function for benchmark compatibility.

- [ ] **Step 4: Verify there is one owner and no cycle**

Run:

```bash
rg -n '^(forceInferenceDiagnostic|forceDiagnostic|forceDiagnosticLabel) ::' src/Jazz/Compiler
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test profiling-spec core-normalization-spec pattern-coverage-spec -fdevelopment --test-show-details=direct
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: only `Diagnostics/Strictness.hs` defines `forceDiagnostic`; all suites and build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/Diagnostics/Strictness.hs src/Jazz/Compiler/TypeInference/Result.hs src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/Force.hs src/Jazz/Compiler/Driver.hs src/Jazz/Compiler/ModuleCompiler.hs test/Jazz/Compiler/ProfilingSpec.hs
git add test/Jazz/Compiler/Semantics/CoreNormalizationSpec.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
git commit -m "refactor: centralize inference results and diagnostic forcing"
```

Inspect `git diff --cached --name-only` before committing; do not stage unrelated files.

### Task 5: Make graph modules the resolver's sole artifact

**Files:**

- Modify: `src/Jazz/Compiler/ModuleResolver.hs:7-16,130-200,240-280,318-435`
- Modify: `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify any exact consumers returned by the call-site inventory

**Interfaces:**

- Consumes: `ModuleGraph.ResolvedModule` and `ModuleGraph.ResolvedProgram`.
- Produces unchanged resolver entry-point names with canonical result types:

```haskell
resolveModuleGraph ::
  ModuleResolutionConfig -> Map FilePath Text -> [Text] ->
  Either Diagnostic [ModuleGraph.ResolvedModule]

resolveModuleGraphWithLookup :: Monad m =>
  ModuleResolutionConfig -> (FilePath -> m (Maybe Text)) -> [Text] ->
  m (Either Diagnostic [ModuleGraph.ResolvedModule])
```

`resolveModuleGraphWithLookupAndVisibleSymbols` has the analogous rich-module result.

- [ ] **Step 1: Make tests demand the canonical graph type**

In `ModuleResolutionSpec`, remove the resolver-local `ResolvedModule` import,
import `ModuleGraph.ResolvedModule`, and define the test-only projection:

```haskell
data ResolvedModuleSummary = ResolvedModuleSummary [Text] FilePath [[Text]]
  deriving (Eq, Show)

resolvedModuleSummary :: ModuleGraph.ResolvedModule -> ResolvedModuleSummary
resolvedModuleSummary resolvedModule =
  ResolvedModuleSummary
    (ModuleGraph.resolvedModulePath resolvedModule)
    (ModuleGraph.resolvedSourcePath resolvedModule)
    (map ModuleGraph.resolvedImportPath (ModuleGraph.resolvedModuleImports resolvedModule))
```

Change only summary-shaped expected lists to `ResolvedModuleSummary` and map
the projection over resolver results. Keep tests that inspect core/export data
on the rich value directly.

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test module-resolution-spec -fdevelopment --test-show-details=direct
```

Expected: FAIL to compile because resolver entry points still return the removed compatibility type.

- [ ] **Step 2: Remove the compatibility record and duplicate state**

Delete `ModuleResolver.ResolvedModule` and its export. Replace:

```haskell
resolvedModulesRevState :: [ResolvedModule]
resolvedGraphModulesRevState :: [ModuleGraph.ResolvedModule]
```

with:

```haskell
resolvedModulesRevState :: [ModuleGraph.ResolvedModule]
```

Construct only the graph module in `visitModule` and insert it into that one
list. Update `resolveProgramWithAmbientExports` and all graph-returning helpers
to reverse the same field.

- [ ] **Step 3: Re-run module resolution and pipeline contracts**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test module-resolution-spec loader-spec module-pipeline-contract-spec prelude-loading-spec -fdevelopment --test-show-details=direct
rg -n '^data ResolvedModule|resolvedGraphModulesRevState|Compatibility summary returned' src/Jazz/Compiler/ModuleResolver.hs
```

Expected: suites PASS; `rg` returns no matches.

- [ ] **Step 4: Confirm production still uses one graph in deterministic order**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test profiling-spec jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: PASS with no module-order or fixture changes.

- [ ] **Step 5: Commit**

```bash
git add src/Jazz/Compiler/ModuleResolver.hs test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs
git commit -m "refactor: use one resolved module artifact"
```

### Task 6: Close milestone 1

**Files:**

- Modify only if required by project workflow: `.codex/execution/queue.md`
- Verify: all milestone files

**Interfaces:**

- Consumes: the five completed task commits.
- Produces: a clean, compiling invariant/ownership baseline for milestone 2.

- [ ] **Step 1: Format only touched Haskell modules with the repository-compatible formatter**

First discover the formatter command/version from `flake.nix`, repository
scripts, or existing execution plans. Run it only on the files changed in this
milestone. If the pinned formatter cannot parse supported GHC syntax, record
that exact incompatibility and do not substitute a repository-wide rewrite.

- [ ] **Step 2: Run focused milestone tests serially**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test adt-pattern-type-spec jazz-typed-core-contract-spec runtime-observation-spec runtime-semantics-spec module-resolution-spec loader-spec module-pipeline-contract-spec profiling-spec core-normalization-spec pattern-coverage-spec jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 3: Run the development build and package checks**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal check
git diff --check
```

Expected: all commands PASS.

- [ ] **Step 4: Inspect source ownership and partial-operation invariants**

```bash
rg -n '\berror\s*(\(|")|Map\.!' src -g '*.hs'
rg -n '^data ResolvedModule|resolvedGraphModulesRevState' src/Jazz/Compiler/ModuleResolver.hs
rg -n '^(runtimeControlOutcome|diagnosticResultOutcome|runtimeOutcomeAsDiagnosticResult|forceDiagnostic) ::' src/Jazz/Compiler
git status --short
```

Expected: no `error`, `Map.!`, compatibility `ResolvedModule`, or duplicate
graph accumulator in active source; shared functions have exactly one owner;
only intentional milestone changes are present.

- [ ] **Step 5: Commit any verified formatting or execution-state update**

If Step 1 or project state changed files:

```bash
git add jazz.cabal src/Jazz/Compiler/Diagnostics/Strictness.hs src/Jazz/Compiler/TypeInference/Result.hs src/Jazz/Compiler/TypeInference/Pattern.hs src/Jazz/Compiler/TypeInference.hs src/Jazz/Compiler/Force.hs src/Jazz/Compiler/Driver.hs src/Jazz/Compiler/ModuleCompiler.hs src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/Runtime/Outcome.hs src/Jazz/Compiler/Runtime/Types.hs src/Jazz/Compiler/Runtime/Observation.hs src/Jazz/Compiler/Runtime.hs src/Jazz/Compiler/ModuleRuntime.hs src/Jazz/Compiler/ModuleResolver.hs test/Jazz/Compiler/Runtime/ObservationSpec.hs test/Jazz/Compiler/Runtime/OutcomeTests.hs test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs test/Jazz/Compiler/ProfilingSpec.hs test/Jazz/Compiler/Semantics/CoreNormalizationSpec.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
git commit -m "chore: close maintainability milestone 1"
```

Do not create an empty commit.
