# Jazz Haskell Quality Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remediate the approved whole-Haskell quality findings while preserving Jazz language behavior, ordered diagnostics, and runtime/module semantics.

**Architecture:** Replace serialized capability facts with one structured semantic type, remove unlawful general runtime equality, make validation and configuration collections match their mathematical domains, and simplify large internal state/API boundaries through explicit records and canonical entry points. Each task is independently testable and committed; behavior changes use a witnessed red-green cycle, while the approved behavior-preserving refactors use focused characterization suites before and after.

**Tech Stack:** Haskell 2010, GHC 9.14.1, Cabal, Nix, `containers`, `text`, `deepseq`, the repository test harness, Ormolu.

**Spec:** `.codex/plans/2026-08-30-jazz-haskell-quality-remediation-design.md`

## Global Constraints

- Active implementation paths are `src/`, `app/`, `test/`, `benchmark/`, and `program-support/` only.
- Preserve public Jazz syntax, typing, diagnostic text and order, module order, import precedence, runtime evaluation order, and bootstrap parity except for the confirmed punctuated-identifier import bug.
- The only other observable text change is the benchmark successful-empty-output diagnostic.
- Keep `RuntimeOutcome`, independent Typed Core/Lowered IR validators, managed-layout ownership, selective forcing, and order-sensitive sequences.
- Do not introduce a generic parser framework, a monad-polymorphic runtime engine, repository-wide formatting, or new package dependencies.
- Behavioral fixes must show a failing focused test before production changes and a passing focused test afterward.
- Behavior-preserving refactors must run their named characterization suites before and after.
- Run Cabal through `nix --extra-experimental-features 'nix-command flakes' develop --command` with `--jobs=1` unless an isolated build directory is explicitly provided.
- Format only touched Haskell files with `scripts/check-haskell-format.sh`.
- Make one focused commit per task.

---

### Task 1: Structure Capability Facts and Centralize Identifier Policy

**Files:**

- Modify: `src/Jazz/Compiler/AST.hs`
- Modify: `src/Jazz/Compiler/Name.hs`
- Modify: `src/Jazz/Compiler/Parser/Lexer.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/CapabilityFacts.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/ModuleCompiler.hs`
- Modify: `src/Jazz/Compiler/Analyzer.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Signature.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Test: `test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Test: `test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs`

**Interfaces:**

- Produces: `ConcreteImplFact`, `concreteImplFact`, `renderConcreteImplFact`, `concreteImplFactClassName`, `isIdentifierStartCharacter`, and `isIdentifierContinuationCharacter`.
- Replaces: every `Set Text` concrete-impl collection with `Set ConcreteImplFact`.
- Preserves: rendered missing-impl diagnostics and public module interfaces.

- [ ] **Step 1: Add the failing module-import regression**

Add the test to the `capabilitiesTests` manifest and use the existing real
module loader:

```haskell
testRunModuleGraphRebasesPunctuatedConcreteImplFacts :: IO ()
testRunModuleGraphRebasesPunctuatedConcreteImplFacts = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Marked (Marked!, Tagged', marked).
            marked.
            }
            """
          ),
          ( "src/Lib/Marked.jz",
            """
            module Lib::Marked {
            data Tagged' = Tagged'.
            class Marked!(a) {
            markedValue :: a -> Bool.
            }.
            impl Marked!(Tagged') {
            markedValue = \\(value) -> True.
            }.
            marked :: @{Marked!(Tagged')}: Bool.
            marked = Marked!::markedValue Tagged'.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
```

- [ ] **Step 2: Run the regression and witness the current failure**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test loader-spec --test-show-details=failures --jobs=1
```

Expected: FAIL because the imported punctuated concrete fact is not rebased,
with a missing-impl or related imported-capability diagnostic.

- [ ] **Step 3: Define the structured fact and shared identifier predicates**

In `CapabilityFacts.hs`, add the semantic representation and keep rendering
explicit:

```haskell
data ConcreteImplFact = ConcreteImplFact Name SignatureType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

concreteImplFact :: Name -> [SignatureType] -> Maybe ConcreteImplFact
concreteImplFact capabilityName arguments =
  case arguments of
    [argument]
      | concreteConstraintArgument argument ->
          Just (ConcreteImplFact capabilityName argument)
    _ -> Nothing

renderConcreteImplFact :: ConcreteImplFact -> Text
renderConcreteImplFact (ConcreteImplFact capabilityName argument) =
  renderName capabilityName <> "(" <> renderSignatureType argument <> ")"
```

Derive `Ord` for `SignatureType`. In `Name.hs`, export:

```haskell
isIdentifierStartCharacter :: Char -> Bool
isIdentifierStartCharacter character = isAlpha character || character == '_'

isIdentifierContinuationCharacter :: Char -> Bool
isIdentifierContinuationCharacter character =
  isAlphaNum character || character == '_' || character == '\'' || character == '!'
```

Use those predicates from both `Parser.Lexer` and `ModuleResolver`.

- [ ] **Step 4: Migrate fact storage, rebasing, filtering, and diagnostics**

Change `scopeConcreteImplFacts` and `interfaceConcreteImplFacts` to
`Set ConcreteImplFact`. Replace textual rebasing with recursive structural
rebasing:

```haskell
rebaseConcreteImplFact ::
  ResolvedNameOrigin ->
  Set Text ->
  Set Text ->
  ConcreteImplFact ->
  ConcreteImplFact
rebaseConcreteImplFact origin dataTypeNames classNames (ConcreteImplFact capabilityName argument) =
  ConcreteImplFact
    (rebaseKnownName CapabilityNamespace origin classNames capabilityName)
    (rebaseSignatureTypeNames origin dataTypeNames argument)
```

Implement `rebaseSignatureTypeNames` by exhaustively rebuilding every
`SignatureType` constructor and rebasing `TypeName`, `TypeApplication`, and
the `Name` inside `TypeVariable` only according to the existing type-name
rules. Delete `rebaseFact` and all string reconstruction for concrete facts.
Render through `renderConcreteImplFact` only at diagnostic sites.

- [ ] **Step 5: Run focused capability and module suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test loader-spec module-pipeline-contract-spec haskell-typeclass-contracts-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS, including the new punctuated-name regression.

- [ ] **Step 6: Commit the structured fact migration**

```sh
git add src test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs
git commit -m "refactor: structure concrete capability facts"
```

### Task 2: Remove Unlawful RuntimeValue Equality

**Files:**

- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/Runtime/Semantics.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Modify: runtime tests that compare `Either Diagnostic (Maybe RuntimeValue)` directly
- Test: `test/Jazz/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/Runtime/RenderingTests.hs`

**Interfaces:**

- Produces: `runtimeValueMatchesLiteral :: RuntimeValue -> Literal -> Bool`.
- Removes: `Eq RuntimeValue`, `Eq RuntimeMethodCandidate`, and equality derivations whose only unsupported field is an arbitrary runtime value.
- Preserves: source-level structural equality in `Runtime.Primitives`.

- [ ] **Step 1: Add direct literal-matching regression coverage**

Export the intended semantic helper from `Runtime.Semantics` and add a focused
test table that names the mutation it catches:

```haskell
assertEqual "typed integer literal matches" True
  (runtimeValueMatchesLiteral (VTyped TypeInt (VInt 7 (RuntimeIntMetadata Nothing))) (LInt 7))
assertEqual "different text literal does not match" False
  (runtimeValueMatchesLiteral (VText "Jazz") (LText "jazz"))
assertEqual "closure is never a literal match" False
  (runtimeValueMatchesLiteral closureValue (LBool True))
```

- [ ] **Step 2: Run the focused test and witness the missing-helper failure**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test runtime-semantics-spec --test-show-details=failures --jobs=1
```

Expected: FAIL to compile because `runtimeValueMatchesLiteral` is absent.

- [ ] **Step 3: Implement literal matching and route patterns through it**

Implement exhaustive wrapper stripping and literal comparison:

```haskell
runtimeValueMatchesLiteral :: RuntimeValue -> Literal -> Bool
runtimeValueMatchesLiteral runtimeValue literal =
  case runtimeValue of
    VTyped _ innerValue -> runtimeValueMatchesLiteral innerValue literal
    VExplicitTypeApplication _ innerValue -> runtimeValueMatchesLiteral innerValue literal
    VExplicitResultHints _ innerValue -> runtimeValueMatchesLiteral innerValue literal
    VInt actual _ -> case literal of LInt expected -> actual == expected; _ -> False
    VFloat actual _ -> case literal of LFloat expected _ _ -> actual == expected; _ -> False
    VBool actual -> case literal of LBool expected -> actual == expected; _ -> False
    VChar actual -> case literal of LChar expected -> actual == expected; _ -> False
    VText actual -> case literal of LText expected -> actual == expected; _ -> False
    _ -> False
```

Replace the `scrutineeValue == literalRuntimeValue literal` guard in
`matchPattern` with `runtimeValueMatchesLiteral scrutineeValue literal`.

- [ ] **Step 4: Remove the unlawful instance and repair test assertions**

Delete `Eq RuntimeValue`, `Eq RuntimeMethodCandidate`, and
`Eq RuntimeConstructorArguments`. Remove `Eq` from `RunResult`. Change direct
runtime-value assertions to compare `fmap (fmap renderRuntimeValue)` or to use
constructor-specific predicates; do not add another partial equality helper.

- [ ] **Step 5: Run runtime and ADT pattern suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test runtime-semantics-spec adt-pattern-runtime-spec primitive-semantics-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 6: Commit the equality boundary**

```sh
git add src/Jazz/Compiler/Runtime src/Jazz/Compiler/Driver.hs test/Jazz/Compiler/Semantics
git commit -m "refactor: narrow runtime literal equality"
```

### Task 3: Make Typed Core Validation Linear

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore/Validate/Internal.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Evidence.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Expressions.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Program.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Declarations.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/RegressionTests.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/Tests.hs`

**Interfaces:**

- Produces: private stable deduplication/failure-collection helpers in
  `Validate.Internal` only when at least two validator modules share them.
- Preserves: exact validation failure arrays and first-occurrence order.

- [ ] **Step 1: Add a high-cardinality duplicate characterization**

Construct 2,000 duplicate declarations or evidence entries with literal
expected first/last failure paths. Force the complete validation result:

```haskell
let failures = force (validateTypedProgram manyDuplicateProgram)
evaluate failures
assertEqual "duplicate failure count" 1999 (length failures)
assertEqual "first duplicate path" expectedFirst (head failures)
assertEqual "last duplicate path" expectedLast (last failures)
```

Expected values must be literal fixture values, not computed by validator
helpers.

- [ ] **Step 2: Run the characterization before implementation**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
```

Expected: PASS with 1,999 duplicate failures in the established order. This is
the approved characterization-before-refactor path; the task review verifies
that the implementation removes append-in-fold and list-membership complexity.

- [ ] **Step 3: Replace append-in-fold and stable nub paths**

Use the following shape for every left-to-right duplicate scan:

```haskell
collectDuplicateFailures makeFailure = reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, failuresRev) value
      | Set.member value seen = (seen, makeFailure value : failuresRev)
      | otherwise = (Set.insert value seen, failuresRev)
```

Retain local helpers where path/kind construction is module-specific. Replace
candidate membership lists with `Set` only when `Ord` already expresses the
same structural identity. Do not reorder concatenated child failures.

- [ ] **Step 4: Run exact contract and direct-call suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS with the established exact arrays and the new scale regression.

- [ ] **Step 5: Commit the Typed Core validator cleanup**

```sh
git add src/Jazz/Compiler/TypedCore/Validate test/Jazz/Compiler/Bootstrap/TypedCoreContract
git commit -m "perf: linearize Typed Core validation"
```

### Task 4: Make Lowered IR Validation and Elaboration Collection Linear

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Validate.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Profiles.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/CapabilityFacts.hs`
- Test: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`

**Interfaces:**

- Consumes: structured capability fact representation from Task 1.
- Preserves: exact Lowered IR failure ordering and Typed Core interface order.

- [ ] **Step 1: Add a Lowered IR duplicate characterization**

Create a `LoweredProgram` with 2,000 layouts sharing one identifier or 2,000
variant tags sharing one tag. Force validation and assert 1,999 ordered
duplicate failures with literal first and last expected paths.

- [ ] **Step 2: Run the Lowered IR characterization before implementation**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
```

Expected: PASS with the existing exact failure order.

- [ ] **Step 3: Use reversed accumulators and stable Set-backed dedupe**

Convert the identified duplicate scans in `LoweredIR.Validate` to strict
`Set` plus reverse-once folds. Convert append-in-loop group/interface
construction in `Profiles` and `Finalize` to reversed accumulation while
reversing exactly once at the owning boundary. Change capability signature
variable deduplication to a stable `Set`-backed fold.

- [ ] **Step 4: Run Lowered IR and Typed Core contracts**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-lowered-ir-contract-spec jazz-typed-core-contract-spec \
  jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
```

Expected: PASS with unchanged canonical ordering.

- [ ] **Step 5: Commit collection linearization**

```sh
git add src/Jazz/Compiler/LoweredIR/Validate.hs src/Jazz/Compiler/TypeInference/Elaboration src/Jazz/Compiler/CapabilityFacts.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
git commit -m "perf: linearize ordered compiler collections"
```

### Task 5: Introduce ScopeWalkState

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Test: existing `binding-signature-coherence-spec`
- Test: existing `recursive-bindings-spec`
- Test: existing `jazz-typed-core-expression-direct-call-spec`

**Interfaces:**

- Produces: private strict `ScopeWalkState` in `TypeInference.Scope`.
- Preserves: the public inference API and every diagnostic/production-failure order.

- [ ] **Step 1: Run characterization suites before editing**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test binding-signature-coherence-spec recursive-bindings-spec \
  jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Define the strict walk record**

Use named fields matching the current positional arguments:

```haskell
data ScopeWalkState = ScopeWalkState
  { scopeWalkEnv :: !TypeEnv,
    scopeWalkEnvFreeVariables :: !TypeEnvFreeVariables,
    scopeWalkLastExprType :: !(Maybe ExpressionType),
    scopeWalkPendingSignature :: !(Maybe PendingSignatureType),
    scopeWalkPendingSignaturesByStatement :: !(Map Int PendingSignatureType),
    scopeWalkRecursiveGroupStartStates :: !(Map Int InferState),
    scopeWalkRecursiveGroupPreviewCache :: !RecursiveGroupPreviewCache,
    scopeWalkModuleBaselineFacts :: !ScopeCapabilityFacts,
    scopeWalkInferState :: !InferState
  }
```

Keep remaining statements outside the record:
`go :: ScopeWalkState -> [(Int, Statement)] -> (Maybe ExpressionType, InferState, [ProvisionalTypedStatement], [InferredProductionFailure])`.

- [ ] **Step 3: Convert every recursive branch to explicit record updates**

Initialize one record at the call site. Replace positional calls with record
updates. Every branch that currently passes `Map.empty` for the preview cache
must visibly set `scopeWalkRecursiveGroupPreviewCache = Map.empty`. Do not
combine statement branches or alter failure concatenation in this task.

- [ ] **Step 4: Run the same characterization suites after editing**

Run the Step 1 command again. Expected: PASS with identical assertions.

- [ ] **Step 5: Commit the explicit state refactor**

```sh
git add src/Jazz/Compiler/TypeInference/Scope.hs
git commit -m "refactor: name scope inference walk state"
```

### Task 6: Retire InferExprFn Migration Adapters

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Traversal.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: call sites in `src/Jazz/Compiler/TypeInference.hs`
- Modify: `test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs`

**Interfaces:**

- Consumes: `InferExprWithModeFn` and `TypedCoreProductionMode`.
- Removes: `InferExprFn` and wrappers that erase/rebuild `InferredExpr`.
- Preserves: inference-only expected-type checking and preview rollback behavior.

- [ ] **Step 1: Run ownership and recursion characterization suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test binding-signature-coherence-spec recursive-bindings-spec runtime-semantics-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Migrate synthetic callbacks to the production callback shape**

Replace test callbacks of shape:

```haskell
BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (ExpressionType, InferState)
```

with:

```haskell
TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (InferredExpr, InferState)
```

The tests must assert `InferenceOnly` and return
`InferredExpr expressionType Nothing []` so preview ownership remains visible.

- [ ] **Step 3: Migrate internal consumers and delete adapters**

Thread `InferExprWithModeFn` directly through pattern, capability, and scope
helpers. At inference-only call sites, pass `InferenceOnly` explicitly. Delete
`InferExprFn` only after `rg -n "InferExprFn" src test` returns no consumers
outside the defining line, then remove the defining line.

- [ ] **Step 4: Run focused suites and compile the library**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test binding-signature-coherence-spec recursive-bindings-spec runtime-semantics-spec \
  --test-show-details=failures --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal build lib:jazz-internal -fdevelopment --jobs=1
```

Expected: both commands PASS.

- [ ] **Step 5: Commit callback consolidation**

```sh
git add src/Jazz/Compiler/TypeInference test/Jazz/Compiler/Semantics
git commit -m "refactor: consolidate inference callbacks"
```

### Task 7: Remove Resolver and Parser Compatibility Entry Points

**Files:**

- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/Parser/Declaration.hs`
- Modify: `test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: resolver fixture/contract callers returned by `rg`
- Modify: `test/Jazz/Compiler/Parser/DeclarationParserSpec.hs`

**Interfaces:**

- Canonical resolver: `resolveProgramWithAmbientExports`.
- Canonical parser: detailed `TokenStream` entry points.
- Removes: the compatibility functions named in the design and `Identity` support.

- [ ] **Step 1: Run resolver/parser characterization suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test module-resolution-spec module-pipeline-contract-spec declaration-parser-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Create test-local canonical resolver fixtures**

In `ModuleResolutionSpec`, replace compatibility calls with a helper that
builds `ModuleResolutionConfig`, passes a real `IO` or map lookup to
`resolveProgramWithAmbientExports`, and projects the rich
`ModuleGraph.ResolvedProgram` only inside the test. Do not add a new production
summary type.

- [ ] **Step 3: Migrate the injected parser failure test**

Use `parseCapabilityDeclarationTokensDetailed` with a `TokenStream` and the
existing test expression parser. Assert the same parse failure and consumed
cursor behavior, then remove `adaptListExpressionParser` and the list-returning
declaration helpers.

- [ ] **Step 4: Delete resolver wrappers and prove no callers remain**

Delete `resolveModuleGraph`, `resolveModuleGraphWithLookup`,
`resolveModuleGraphWithLookupAndVisibleSymbols`, and compatibility
`resolveProgram`. Remove `Identity` imports. Run:

```sh
rg -n "resolveModuleGraph|resolveProgram[^W]|adaptListExpressionParser|parseImportStatementTokens|parseDataStatementTokens|parseCapabilityDeclarationTokens" src test
```

Expected: no obsolete caller; detailed/canonical function names may remain.

- [ ] **Step 5: Run resolver/parser suites again**

Run the Step 1 command. Expected: PASS.

- [ ] **Step 6: Commit compatibility removal**

```sh
git add src/Jazz/Compiler/ModuleResolver.hs src/Jazz/Compiler/Parser/Declaration.hs test/Jazz/Compiler/Modules test/Jazz/Compiler/Parser
git commit -m "refactor: remove private compatibility entry points"
```

### Task 8: Share Pure Module Evaluation Steps and CLI Rendering

**Files:**

- Modify: `src/Jazz/Compiler/ModuleRuntime.hs`
- Modify: `src/Jazz/CLI/Main.hs`
- Test: `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`
- Test: `test/Jazz/CLI/CLISpec.hs` or the live CLI suite file

**Interfaces:**

- Produces: private pure module-preparation and module-publication helpers.
- Produces: private `renderCompileResult :: CompileResult -> CliOutput`.
- Preserves: two explicit runtime traversal loops and all public entry points.

- [ ] **Step 1: Add host-free path parity coverage**

Compile two semantically equivalent multi-module fixtures: one host-free, and
one containing a syntactically host-requiring call in an unselected branch.
Evaluate both through the public compiled-program APIs so the first selects the
pure loop and the second selects the host-capable loop. Assert identical
rendered module paths, export values, and program output. Keep the assertion on
observable projections rather than adding `Eq RuntimeProgram`.

- [ ] **Step 2: Run module and CLI characterization suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test module-pipeline-contract-spec cli-spec runtime-observation-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 3: Extract only the shared pure module decisions**

Create a private preparation record or tuple containing module path,
`ModuleEvaluationMode`, and imported environment. Create a pure completion
helper that publishes exports, accumulates the runtime module, and selects the
entry output. Both loops call these helpers but retain their current monads and
scope evaluators.

- [ ] **Step 4: Extract compile-result rendering**

Replace the two identical CLI branches with:

```haskell
renderCompileResult :: CompileResult -> CliOutput
renderCompileResult result =
  CliOutput
    { cliExitCode = if null (compileErrors result) then 0 else 1,
      cliStdout = "",
      cliStderr = renderLines (map renderDiagnostic (compileDiagnostics result))
    }
```

Retain the existing `renderLines` newline and quiet-output behavior.

- [ ] **Step 5: Run the Step 2 suites again and commit**

Expected: PASS.

```sh
git add src/Jazz/Compiler/ModuleRuntime.hs src/Jazz/CLI/Main.hs test/Jazz/Compiler/Modules test/Jazz/CLI
git commit -m "refactor: share module and CLI result steps"
```

### Task 9: Use Standard Sets, Newtypes, and Modern Imports

**Files:**

- Modify: `src/Jazz/Compiler/WarningConfig.hs`
- Modify: `src/Jazz/Compiler/PatternCoverage.hs`
- Modify: `src/Jazz/Compiler/Driver.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/StructuredValues.hs`
- Modify: nine HLint-confirmed files with unused `OverloadedStrings`
- Test: `test/Jazz/Compiler/Config/WarningConfigSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs`

**Interfaces:**

- Preserves: abstract `WarningSettings` API.
- Changes representation only: `Set WarningCategory`, three `newtype`s, and
  `Data.Functor.unzip`.

- [ ] **Step 1: Run warning and coverage characterization suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test warning-config-spec pattern-coverage-spec --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Replace boolean maps with sets**

Use:

```haskell
data WarningSettings = WarningSettings
  { enabledCategories :: Set WarningCategory,
    errorCategories :: Set WarningCategory,
    allEnabledAreErrors :: Bool
  }
```

Implement enable/promote with `Set.insert`, disable with `Set.delete`, none
with `Set.empty`, and queries with `Set.member`. Preserve
`allEnabledAreErrors` semantics.

- [ ] **Step 3: Replace ordered nub and single-field data declarations**

Use `Set.toAscList . Set.fromList` for the existing `sort . nub` pattern
coverage expression. Change only the declaration keyword for:

```haskell
newtype CompileResult = CompileResult {compileDiagnostics :: [Diagnostic]}
newtype RuntimeIntMetadata = RuntimeIntMetadata {runtimeIntTargetType :: Maybe NumericType}
newtype ImplMethodType = ImplMethodType SignatureType
```

Retain existing derived instances using explicit deriving strategies.

- [ ] **Step 4: Modernize the NonEmpty unzip and remove unused pragmas**

Import `Data.Functor (unzip)` in `StructuredValues.hs`. Rerun the filtered HLint
pragma check against the following files and remove `OverloadedStrings` from a
file exactly when HLint still reports it unused after Tasks 1-8:

```text
src/Jazz/Compiler/AST.hs
src/Jazz/Compiler/Driver.hs
src/Jazz/Compiler/LoweredIR.hs
src/Jazz/Compiler/ModuleGraph.hs
src/Jazz/Compiler/ModuleInterface.hs
src/Jazz/Compiler/RuntimeHints.hs
src/Jazz/Compiler/LoweredIR/Lower.hs
src/Jazz/Compiler/Parser/AST.hs
src/Jazz/Compiler/TypedCore/Validate/Patterns.hs
```

- [ ] **Step 5: Run focused suites and the development build**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test warning-config-spec pattern-coverage-spec --test-show-details=failures --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal build all -fdevelopment --jobs=1
```

Expected: both commands PASS.

- [ ] **Step 6: Commit the idiomatic representation cleanup**

```sh
git add src test/Jazz/Compiler/Config/WarningConfigSpec.hs test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs
git commit -m "refactor: use idiomatic Haskell representations"
```

### Task 10: Correct Benchmark Empty-Output Diagnostics

**Files:**

- Modify: `benchmark/Jazz/Benchmark/Metadata.hs`
- Test: `test/Jazz/Benchmark/MetadataSpec.hs`

**Interfaces:**

- Produces: a private or module-visible pure classifier for process exit/output.
- Changes: only the diagnostic for `ExitSuccess` plus disallowed empty output.

- [ ] **Step 1: Add the failing pure classification test**

Expose the classifier to the private benchmark component and assert:

```haskell
assertEqual
  "successful empty command output"
  (Left "command produced no output")
  (classifyProcessFact False ExitSuccess "" "")
```

Use the final exact message chosen in `Metadata.hs` on both sides. Also retain
cases for successful non-empty output and non-zero exit.

- [ ] **Step 2: Run the metadata suite and witness the current failure**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test benchmark-metadata-spec --test-show-details=failures --jobs=1
```

Expected: FAIL because successful empty output is classified as
`command failed (ExitSuccess)` or because the classifier is not yet defined.

- [ ] **Step 3: Extract and implement the pure classifier**

Order branches as: successful non-empty; successful allowed-empty; successful
disallowed-empty; non-success with stderr/stdout context. `captureProcessFact`
delegates to the helper after `readProcessWithExitCode`.

- [ ] **Step 4: Run benchmark and corpus suites**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test benchmark-metadata-spec benchmark-stage-spec program-corpus-spec profiling-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 5: Commit the benchmark diagnostic fix**

```sh
git add benchmark/Jazz/Benchmark/Metadata.hs test/Jazz/Benchmark/MetadataSpec.hs
git commit -m "fix: report empty benchmark command output"
```

### Task 11: Format, Verify, and Review the Aggregate Branch

**Files:**

- Modify: touched Haskell files only when Ormolu requires it.
- Verify: all commits since the design baseline.

**Interfaces:**

- Produces: a warnings-clean build, passing 67-suite matrix, and review-ready
  detached-head commit chain.

- [ ] **Step 1: Format every touched Haskell file**

Build the file list from the branch diff and pass only `*.hs` paths to:

```sh
git diff --name-only 7908fbc0..HEAD -- '*.hs' -z | xargs -0 scripts/check-haskell-format.sh
```

For files that fail, format the same exact list and rerun the check:

```sh
git diff --name-only 7908fbc0..HEAD -- '*.hs' -z | xargs -0 \
  nix --extra-experimental-features 'nix-command flakes' develop --command ormolu --mode inplace
git diff --name-only 7908fbc0..HEAD -- '*.hs' -z | xargs -0 scripts/check-haskell-format.sh
```

Do not format untouched files.

- [ ] **Step 2: Run repository whitespace and obsolete-symbol checks**

Run:

```sh
git diff --check
rg -n "InferExprFn|adaptListExpressionParser|resolveModuleGraphWithLookupAndVisibleSymbols" src test
```

Expected: no whitespace errors and no obsolete symbols.

- [ ] **Step 3: Run the complete development build**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal build all -fdevelopment --jobs=1
```

Expected: exit 0 with no warnings.

- [ ] **Step 4: Run the complete serialized test matrix**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test all --test-show-details=failures --jobs=1
```

Expected: all 67 test suites PASS.

- [ ] **Step 5: Request an aggregate code review**

Review the full range from the parent of the design commit through `HEAD`
against the spec and this plan. Resolve every Critical or Important finding,
rerun the covering tests, and obtain a scoped re-review.

- [ ] **Step 6: Commit any formatter-only residue**

If formatting changed files after their owning task commit:

```sh
git diff --name-only 7908fbc0..HEAD -- '*.hs' -z | xargs -0 git add
git commit -m "style: format Haskell remediation"
```

If the formatter made no changes, do not create an empty commit.
