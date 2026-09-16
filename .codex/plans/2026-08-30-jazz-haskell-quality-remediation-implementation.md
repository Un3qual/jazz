# Jazz Haskell Quality Remediation Implementation Plan

> **Retired compiler scope:** [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md)
> and [RFC 0022](../../rfcs/accepted/0022-hosted-compiler-removal.md) retired the
> optional backend, hosted compiler, and their exclusive verification. References
> to those paths, schemas, suites, and retention requirements in this record
> (including frontmatter) are historical and impose no current work or test obligations.

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

## Follow-up Pass 1: Semantic Deduplication

### Task 11: Share Type-Inference Numeric Policies

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/TypeOps.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/TypeInference.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Test: `test/Jazz/Compiler/Semantics/IfExpressionTypeSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/AdtPatternTypeSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/PrimitiveSemantics/NumericConversions.hs`

**Interfaces:**

- Produces: `mergedUnifiedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType` in `TypeOps`.
- Produces: `targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic` in `Diagnostics`.
- Removes: both private copies of each helper.
- Preserves: left-biased fallback, nested shape recursion, exact overflow diagnostics, and source-exact decimal checks.

- [ ] **Step 1: Establish focused characterization**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test if-expression-type-spec adt-pattern-type-spec primitive-semantics-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS before the move.

- [ ] **Step 2: Move the exact policies to their semantic owners**

Export the two signatures above. Move the existing recursive
`mergeIntegerLiteralRanges` implementation with `mergedUnifiedType` into
`TypeOps`; move the exact `numericTypeFloatMax`/finite/source-magnitude guard
into `Diagnostics`. Both call sites must import the shared definitions and
delete their local definitions. Do not change either decision table.

- [ ] **Step 3: Re-run focused characterization**

Run the Step 1 command again. Expected: PASS with identical observable results.

- [ ] **Step 4: Format, check, and commit**

```sh
scripts/check-haskell-format.sh \
  src/Jazz/Compiler/TypeInference/TypeOps.hs \
  src/Jazz/Compiler/TypeInference/Diagnostics.hs \
  src/Jazz/Compiler/TypeInference.hs \
  src/Jazz/Compiler/TypeInference/Pattern.hs \
  src/Jazz/Compiler/TypeInference/Scope.hs
git diff --check
git add src/Jazz/Compiler/TypeInference
git commit -m "refactor: share inference numeric policies"
```

### Task 12: Centralize Typed Core Structural Queries

**Files:**

- Create: `src/Jazz/Compiler/TypedCore/Query.hs`
- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/CaptureRecursionTests.hs`
- Test: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/RegressionTests.hs`

**Interfaces:**

- Produces: `typedExpressionReferencesAnyBinder :: Set TypedBinderId -> TypedExpr -> Bool`.
- Produces: `typedPatternInfo :: TypedPattern -> TypedNodeInfo`.
- Produces: `typedPatternChildren :: TypedPattern -> [TypedPattern]`.
- Preserves: exhaustive constructor matching and left-to-right traversal.

- [ ] **Step 1: Run the direct-call and contract characterization**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec \
  jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
```

Expected: PASS.

- [ ] **Step 2: Add the focused query module**

Implement the three signatures above by moving the exact existing exhaustive
case analyses. `typedExpressionReferencesAnyBinder` must inspect only explicit
`TypedVariableExpr` binder references and recurse through lambda bodies,
collections, applications, conditions, case guards/results, binary/section
operands, block initializers/results, and impl bodies. The pattern helpers must
retain source order and have no wildcard in `typedPatternInfo`.

- [ ] **Step 3: Migrate all exact copies and prove none remain**

Replace the two binder-reference copies and every exact pattern-info/children
copy with imports. Run:

```sh
rg -n '^expressionReferencesAnyBinder|^patternInfo|^patternChildren' \
  src/Jazz/Compiler/LoweredIR src/Jazz/Compiler/TypeInference src/Jazz/Compiler/TypedCore
```

Expected: only the shared exported definitions or stage-specific differently
named logic remain.

- [ ] **Step 4: Re-run the Step 1 suites, format, and commit**

```sh
scripts/check-haskell-format.sh \
  src/Jazz/Compiler/TypedCore/Query.hs \
  src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs \
  src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs \
  src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs \
  src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs \
  src/Jazz/Compiler/TypedCore/Validate/Patterns.hs
git diff --check
git add jazz.cabal src/Jazz/Compiler
git commit -m "refactor: centralize Typed Core structural queries"
```

### Task 13: Share Typed Numeric and Lowered Width Mappings

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/RegressionTests.hs`
- Test: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`

**Interfaces:**

- Produces: `typedNumericRepresentationRecipe :: TypedNumericType -> TypedRepresentationRecipe`.
- Produces: `loweredIntegerWidth :: Int -> Maybe LoweredIntegerWidth`.
- Produces: `loweredFloatWidth :: Int -> Maybe LoweredFloatWidth`.
- Preserves: independent invalid-width and recipe/type validation.

- [ ] **Step 1: Run numeric recipe and lowering characterization**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec \
  jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
```

- [ ] **Step 2: Define total mappings once**

Move the eleven-constructor numeric recipe table into `TypedCore`. Move the
8/16/32/64 integer and 16/32/64 float width decoders into `Lower.Types`.
Callers that formerly returned `Maybe` must wrap the total numeric mapping with
`Just`; malformed raw recipe widths must still be rejected locally.

- [ ] **Step 3: Re-run Step 1, format, and commit**

```sh
scripts/check-haskell-format.sh \
  src/Jazz/Compiler/TypedCore.hs \
  src/Jazz/Compiler/TypeInference/Elaboration/Finalize.hs \
  src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs \
  src/Jazz/Compiler/LoweredIR/Lower/ManagedLayouts.hs \
  src/Jazz/Compiler/LoweredIR/Lower/Shapes.hs \
  src/Jazz/Compiler/LoweredIR/Lower/Types.hs
git diff --check
git add src/Jazz/Compiler
git commit -m "refactor: share numeric representation mappings"
```

### Task 14: Share Canonical Runtime Value Codec Mechanics

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalValue.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Produces: shared checked list/text/integer/constructor/arity decoders and
  canonical list/maybe encoders in `CanonicalValue`.
- Preserves: separate Typed Core and Lowered IR schema tables and exact error labels.

- [ ] **Step 1: Run both canonical adapter characterizations**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec \
  --test-show-details=failures --jobs=1
```

- [ ] **Step 2: Move only representation-generic mechanics**

Move `decodeList`, `decodeText`, `decodeInteger`, checked host-`Int`
conversion, constructor extraction, named-constructor checks, arity/nullary
checks, `runtimeValueCategory`, and canonical list/maybe encoders to the
existing support module. Leave all domain constructor names, validation paths,
recipes, layouts, and failure-detail decoders in their owning adapters.

- [ ] **Step 3: Re-run Step 1, format, and commit**

```sh
scripts/check-haskell-format.sh \
  test/Jazz/Compiler/Bootstrap/CanonicalValue.hs \
  test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs \
  test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
git diff --check
git add test/Jazz/Compiler/Bootstrap
git commit -m "refactor: share canonical runtime value codecs"
```

## Follow-up Pass 2: Advanced Haskell and Data Structures

### Task 15: Preserve Validation Proofs with NonEmpty Failures

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration/Types.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: direct-call, Lowered IR, benchmark, and corpus consumers of these outcomes.

**Interfaces:**

- Changes: `validateTypedProgramOnce :: TypedProgram -> Either (NonEmpty TypedCoreValidationFailure) ValidatedTypedProgram`.
- Changes: checked Typed Core production failures to `NonEmpty` and success to `ValidatedTypedProgram`.
- Produces: opaque `ValidatedLoweredProgram` plus `validatedLoweredProgram`.
- Changes: lowering failure constructors to `NonEmpty`; success carries `ValidatedLoweredProgram`.
- Preserves: raw list-returning validators and malformed raw contract fixtures.

- [ ] **Step 1: Add checked-boundary invariant tests**

Add assertions to the existing Typed Core and Lowered IR contract suites that:

```haskell
NonEmpty.toList checkedFailures == rawFailures
```

for one invalid program, and that successful checked lowering is unwrapped only
through `validatedLoweredProgram`. Run the suites and witness compilation fail
until the new APIs exist.

- [ ] **Step 2: Implement proof-carrying checked outcomes**

Use `NonEmpty.nonEmpty` at list-to-checked boundaries. Keep
`validateTypedProgram` and `validateLoweredProgram` unchanged. Do not strengthen
raw hosted-Jazz contract values, which negative parity tests must construct.

- [ ] **Step 3: Run affected production and contract suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec \
  jazz-lowered-ir-contract-spec benchmark-stage-spec program-corpus-spec \
  --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Format every independently formattable touched file and the semantic regions in
legacy-layout consumer files. Do not normalize semantically untouched
surrounding hunks solely to make a legacy file whole-file clean; record that
whole-file mismatch for the aggregate Task 25 formatter pass. Run
`git diff --check`, then:

```sh
git add src benchmark program-support test
git commit -m "refactor: carry validated compiler outcomes"
```

### Task 16: Make Run Results Algebraic

**Files:**

- Modify: `src/Jazz/Compiler/Driver.hs`
- Modify: all explicit `RunResult (..)` import consumers under `src/`,
  `program-support/`, and `test/`; construction is already private to `Driver`.
- Test: `test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs`

**Interfaces:**

- Produces:

```haskell
data RunExecution
  = RunNotExecuted
  | RunRuntimeFailed
  | RunExited Integer
  | RunCompleted (Maybe RuntimeValue)
```

- Makes `RunResult` construction private and stores diagnostics, execution, and observation.
- Exposes `RunExecution (..)` and `runExecution` so callers can distinguish
  successful valueless completion from not-executed and runtime-failed states.
- Retains total `runOutput`, `runRuntimeValue`, and `runExitStatus` projections.

- [ ] **Step 1: Add projection invariant coverage**

Add a table built from real driver actions that checks not executed, runtime
failed, exited, completed with a value, and completed without a terminal value.
Assert the execution tag plus mutually exclusive value/exit projections and
preserved rendered output. Run
`module-pipeline-contract-spec`; expected compile failure before the new API.

- [ ] **Step 2: Refactor driver construction sites**

Map compile-not-run, runtime diagnostic failure, explicit exit, and normal
completion branches to the corresponding constructor. Derive output from the
execution value rather than storing a second independently constructible field.
Use one private adapter for the standalone and module runtime outcome mappings,
parameterized only by the successful result's runtime-value projection. Preserve
compile-first diagnostic ordering and the program-corpus classification of a
diagnostic-free explicit exit.

- [ ] **Step 3: Run driver, CLI, loader, runtime, corpus, and observation suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test module-pipeline-contract-spec cli-spec loader-spec runtime-semantics-spec \
  runtime-observation-spec program-corpus-spec --test-show-details=failures --jobs=1
```

Then run:

```sh
rg -n 'RunResult \(\.\.\)' src program-support test -g '*.hs'
```

Expected: no matches. The deferred Task 25 aggregate gate remains responsible
for compiling components outside the six focused suites.

- [ ] **Step 4: Format and commit**

Format `Driver` and the semantic test changes. For the mechanical
`RunResult (..)` import consumers, format only the modified import regions and
preserve unrelated legacy layout; Task 25 owns aggregate touched-file
normalization. Run `git diff --check`, then:

```sh
git add src program-support test
git commit -m "refactor: make run outcomes explicit"
```

### Task 17: Encapsulate Stable Ordered Sets

**Files:**

- Create: `src/Jazz/Compiler/StableSet.hs`
- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: TypeScheme construction and consumption sites in
  `TypeInference/Scope.hs`, `TypeInference/Operator.hs`,
  `TypeInference/Capabilities.hs`, and `ModuleCompiler.hs`.
- Test: `test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/BindingSignature/BasicsTests.hs`

**Interfaces:**

- Produces opaque `StableSet a` with empty, singleton, insert, delete,
  difference, from-set, from-preferred-order, membership-set, and ordered-list operations.
- Uses `Set a` for membership and `Seq a` for first-occurrence order.
- Produces opaque `QuantifiedVariables` wrapping `StableSet Int` inside
  `TypeScheme`, with deliberate set and ordered projections.

- [ ] **Step 1: Add StableSet law and normalization tests**

Cover set/list agreement, idempotent insertion, first-occurrence order,
duplicate preferred-order removal, deterministic remaining-set order, deletion,
difference, left-biased union order, and `Monoid` identity/associativity.
Include the literal normalization case `[3, 1, 3, 99]` over
`{1, 2, 3, 4}`, yielding `[3, 1, 2, 4]`. Run
`haskell-typeclass-contracts-spec`; expected compile failure before the module exists.

- [ ] **Step 2: Implement the opaque invariant and migrate owners**

`stableSetFromPreferred preferred members` must keep the first occurrence of
each preferred member, discard foreign entries, then append remaining members
in `Set` order. The representation derives the exact structural `Eq`, `Show`,
`Generic`, and `NFData` support required by its owners, but no `Foldable`,
`IsList`, mapping, or generic traversal API. Replace `OrderedNames` and the two
TypeScheme fields without exporting a constructor that can desynchronize
membership and order. Remove the now-redundant local preferred-variable
deduplicator in `Scope`; normalization belongs to `StableSet`.

At every consumer, use the set projection only for membership/subtraction and
the ordered projection for explicit type-application target selection, fresh
allocation, and runtime template naming. Preserve the existing first-variable
semantics rather than falling back to `Set.toList`.

- [ ] **Step 3: Run inference, recursion, runtime capture, and direct-call suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test haskell-typeclass-contracts-spec binding-signature-coherence-spec \
  recursive-bindings-spec lambda-semantics-spec runtime-semantics-spec \
  jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Format touched files, run `git diff --check`, then:

```sh
git add jazz.cabal src test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs \
  test/Jazz/Compiler/Semantics/BindingSignature/BasicsTests.hs
git commit -m "refactor: encode stable ordered sets"
```

### Task 18: Validate CLI State into Algebraic Options

**Files:**

- Modify: `src/Jazz/CLI/Main.hs`
- Test: `test/Jazz/CLI/CLISpec.hs`

**Interfaces:**

- Produces private permissive `RawCliOptions` for argument accumulation.
- Produces validated `CliInput`, `CliPreludeSelection`, and `CliExecutionMode`
  sums. The Prelude sum uses a default/automatic constructor because environment
  resolution can still select an explicit file after argument validation.
- Hides the validated `CliOptions` constructor while exposing deliberate projections.

```haskell
data CliInput
  = CliStdin
  | CliSourceFile FilePath
  | CliModuleGraph [Text] [FilePath]

data CliPreludeSelection
  = CliDefaultPrelude
  | CliExplicitPrelude FilePath
  | CliPreludeDisabled

data CliExecutionMode
  = CliCompile
  | CliRun
  | CliRunWithStatistics RuntimeStatisticsFormat
  | CliRunWithProfile FilePath
  | CliRunWithStatisticsAndProfile RuntimeStatisticsFormat FilePath
```

- [ ] **Step 1: Add algebraic parser-result assertions**

Extend CLI parser tests to assert stdin/source/module-graph input, default/
explicit/disabled Prelude, and compile/run observation modes through the new
constructors or projections. Treat absent Prelude flags as default/automatic,
not already bundled. Include the valid combined statistics-plus-profile run
mode. Run `cli-spec`; expected compile failure.

- [ ] **Step 2: Split accumulation from validation**

Keep the current one-pass flag parser over `RawCliOptions`; retain immediate
errors for missing/malformed/repeated/unknown individual arguments and convert
cross-field state once after all arguments are consumed. Preserve the current
final guard precedence: Prelude conflict, source/entry conflict, observation
without run, then module-root/entry relationship. The conversion must retain
every existing E5002 message and the resolver-owned E4016 invalid module-path
diagnostic.

Normalize implicit stdin and explicit `-` to one validated stdin constructor
only after raw parsing, so repeated-source precedence is unchanged. Normalize
module roots to encounter order and supply the existing `.` default in the
validated module-graph constructor. Store only the validated sums plus warning
flags/config in opaque `CliOptions`; derive runtime-observation and resolver
details through total private helpers.

- [ ] **Step 3: Run CLI and module entrypoint suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test cli-spec module-pipeline-contract-spec loader-spec \
  --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Record the base whole-file formatter result for both files. Format and check a
whole file only when its base layout is already clean; otherwise run focused
Ormolu checks over every Task 18 semantic region and preserve unrelated legacy
layout for Task 25. Record the exact focused commands in the task report. Then
run:

```sh
git diff --check
git add src/Jazz/CLI/Main.hs test/Jazz/CLI/CLISpec.hs
git commit -m "refactor: validate CLI options algebraically"
```

### Task 19: Make Resolved Import Exposure a Sum

**Files:**

- Modify: `src/Jazz/Compiler/ModuleGraph.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/Parser/Lower.hs`
- Modify: resolved-import consumers in `ModuleCompiler.hs`, `ModuleRuntime.hs`, and tests.
- Modify: raw Core canonical adapters and fixtures under
  `test/Jazz/Compiler/Bootstrap/CanonicalCoreComparison*.hs`.

**Interfaces:**

- Produces:

```haskell
data ImportExposure
  = ImportAll
  | ImportOnly (NonEmpty Text)
  | ImportQualified Text
```

- Renames the parser-lowered raw record to `CoreResolvedImport`; `CoreModule`
  stores this raw shape so canonical invalid fixtures remain constructible.
- Replaces independent alias/symbol optionals only in checked resolver output;
  `ResolvedImport` stores one `ImportExposure`.
- Preserves raw parser and Typed Core import shapes for negative validation.

- [ ] **Step 1: Add resolved-import shape characterization**

Add direct resolver-output assertions for all three valid syntax forms in
deliberately non-lexical declaration order, including selector order inside
`ImportOnly`. Do not project through `ResolvedModuleSummary`, which drops this
information. Retain public parser rejection tests for alias-plus-selectors and
empty selectors. Run
`module-resolution-spec module-import-parser-spec` and witness compile failure
until the sum exists.

- [ ] **Step 2: Convert at the resolver boundary**

Convert raw Core imports only after `validateImportBindings` succeeds. Construct
`ImportOnly` with `NonEmpty.nonEmpty`; fail closed with a private E4010
internal-invariant diagnostic for an impossible empty or alias-plus-selector
shape rather than using `error` or dropping data. Preserve declaration order
and duplicates. Update compiler/runtime consumers to one exhaustive case
analysis, keeping qualified imports' existing capability-method exclusion. Use
`NonEmpty.toList` only at the existing inventory boundary. Do not modify raw
`TypedResolvedImport` or its invalid fixtures.

- [ ] **Step 3: Run module and Typed Core boundary suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test module-resolution-spec module-exports-spec module-pipeline-contract-spec \
  loader-spec module-import-parser-spec jazz-typed-core-contract-spec \
  canonical-core-comparison-spec jazz-core-modules-corpus-closure-spec \
  --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Record base formatter status for the exact touched files. Whole-file format only
base-clean files; for legacy-layout files, format every Task 19 semantic region
and preserve unrelated layout for Task 25. Record the whole/scoped checks in the
task report, run `git diff --check`, then:

```sh
git add src/Jazz/Compiler test/Jazz/Compiler/Modules test/Jazz/Compiler/Parser \
  test/Jazz/Compiler/Bootstrap
git commit -m "refactor: encode resolved import exposure"
```

### Task 20: Use Append-Appropriate Runtime and Coverage Collections

**Files:**

- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/Runtime/Engine.hs`
- Modify: `src/Jazz/Compiler/Runtime/Semantics.hs`
- Modify: `src/Jazz/Compiler/Runtime/Primitives.hs`
- Modify: `src/Jazz/Compiler/PatternCoverage.hs`
- Modify: canonical/runtime tests that directly inspect qualified methods.
- Test: `test/Jazz/Compiler/Semantics/PatternCoverageSpec.hs`

**Interfaces:**

- Produces opaque `RuntimeMethodCandidates` backed by `Seq` for the proven
  repeated candidate snoc paths.
- Generalizes the existing opaque `RuntimeConstructorArguments` `Seq` wrapper
  into `RuntimeAppliedArguments` shared by constructor and qualified-method
  application; do not add a duplicate argument wrapper.
- Changes pattern-coverage accumulation to reversed rows/failures with one final reverse.
- Preserves candidate precedence, argument order, and diagnostic order.

- [ ] **Step 1: Add large ordered characterization**

Add a coverage case with 64 distinct integer arms followed by 1,024 repeats of
the first literal; assert exact unreachable indices 65 through 1088 and the
non-exhaustive wildcard failure last. Strengthen a qualified-method test to
assert exact candidate target order and add a noncommutative multi-argument
method result that fails if arguments reverse. Retain the host-backed selector
case for the separate host evaluator lane.
Run `pattern-coverage-spec runtime-semantics-spec`; expected PASS before refactor.

- [ ] **Step 2: Replace only repeated snoc operations**

Use `Seq.|>` for method candidates and the shared applied-argument wrapper;
preserve the historical list-facing `VQualifiedMethod` compatibility pattern
while internal engine/semantics patterns carry opaque collections. Expose only
the empty/append/filter/ordered-list or ordered-fold operations actually used.
For coverage, prepend normalized rows/failures and reverse exactly once before
the exhaustiveness check/result. Do not migrate fixed-arity runtime lists,
`VBuiltin`, `VOperator`, or coverage matrices themselves to `Seq`.

- [ ] **Step 3: Run coverage, capabilities, runtime, observation, and benchmark-stage suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test pattern-coverage-spec runtime-semantics-spec primitive-semantics-spec \
  runtime-observation-spec benchmark-stage-spec --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Record base formatter status for the exact touched files. Whole-file format only
base-clean files; for legacy-layout files, format every Task 20 semantic region
and preserve unrelated layout for Task 25. Record the whole/scoped checks in the
task report, run `git diff --check`, then:

```sh
git add src/Jazz/Compiler/Runtime src/Jazz/Compiler/PatternCoverage.hs test
git commit -m "perf: use append-appropriate compiler collections"
```

### Task 21: Replace Bespoke Helpers with Standard Library Abstractions

**Files:**

- Modify: `src/Jazz/Compiler/ModuleRuntime.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `src/Jazz/Compiler/ModuleResolver.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Validate.hs`
- Modify: `src/Jazz/Compiler/PatternCoverage.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/Parser.hs`
- Modify: `src/Jazz/Compiler/Parser/Lexer.hs`
- Modify: `src/Jazz/Compiler/Parser/Declaration.hs`
- Modify: `src/Jazz/Compiler/Parser/Signature.hs`
- Modify: `src/Jazz/Compiler/Parser/TokenParser.hs`
- Modify: `src/Jazz/Compiler/ModuleExports.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Modify: `src/Jazz/Compiler/Runtime/Outcome.hs`
- Modify: the exact test support consumers named in the standard-library audit.

**Interfaces:**

- Replaces hand-written first-match recursion with `Data.List.find`.
- Replaces bespoke head/optional adapters with `listToMaybe`, `maybeToList`,
  `find`, and ordered `lookup` where the behavior is identical.
- Replaces the total list index helper with `Data.List.!?` and the local
  reverse-application operator with `Data.Function.&`.
- Replaces four `firstJust` implementations with `Data.Foldable.asum`.
- Replaces four `mapLeft` implementations with `Data.Bifunctor.first`.
- Replaces two test-only stable-first deduplicators with `Data.List.nub`.
- Uses `deriving newtype` only for exact underlying `Set`, `Map`, and `Seq`
  composition; derives `Functor` for `RuntimeOutcome` to delete the manual test mapper.
- Preserves first-match, left-biased union, first-occurrence, and diagnostic order.

- [ ] **Step 1: Establish the focused behavior baseline**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test module-pipeline-contract-spec recursive-bindings-spec module-resolution-spec \
  jazz-lowered-ir-contract-spec token-parser-spec declaration-parser-spec \
  signature-rendering-spec pattern-coverage-spec module-exports-spec \
  haskell-typeclass-contracts-spec runtime-observation-spec repository-audit-spec \
  canonical-parser-comparison-spec jazz-core-modules-corpus-closure-spec \
  binding-signature-coherence-spec adt-pattern-type-spec runtime-semantics-spec \
  --test-show-details=failures --jobs=1
```

Expected: PASS before the mechanical replacements.

- [ ] **Step 2: Apply only exact standard-library equivalents**

Use `find` for the audited predicate-first recursions, including the complete
compiled-program diagnostic search; `listToMaybe` and `maybeToList` for the
audited adapters; `asum` for left-biased `[Maybe a]`; `first` for `Either` left
mapping; `!?` for the exact total indexing helper; `&` for the identical local
operator; and `nub` only in the two test-only stable-first `Eq` manifest paths.
Use Prelude `lookup` for ordered variant pairs rather than a `Map`, so duplicate
tags remain first-wins. Do not replace production `Set`-backed stable
deduplication, the Task 17 `StableSet`, or the last-occurrence-preserving
`AuditSpec.uniqueValues` helper.

- [ ] **Step 3: Derive only instances that delete identical manual code**

Enable the narrow deriving extensions needed for:

```haskell
deriving newtype (Semigroup, Monoid)
```

on `ModuleExportInventory` and `PatternBindings`, `Semigroup` on
`RuntimeExplicitResultHints`, and stock/derived `Functor` on `RuntimeOutcome`.
Replace `hasWitness` with `isJust` while editing `PatternCoverage`. Do not add
unused instances or expose constructors.

- [ ] **Step 4: Prove net reduction and run the focused suites**

Run the Step 1 command again. Inspect `git diff --stat` and the diff itself;
the production task must delete more nonblank bespoke implementation lines than
it adds, excluding import reflow. Each candidate must reduce bespoke code in
the repository as a whole; this deliberately admits derived `Functor` for
`RuntimeOutcome`, whose one production pragma deletes a larger manual test
mapper. If another candidate fails the repository-wide rule, revert that
candidate only.

- [ ] **Step 5: Format and commit**

Record base formatter status for the exact touched files. Whole-file format only
base-clean files; for legacy-layout files, format every Task 21 semantic region
and preserve unrelated layout for Task 25. Record the whole/scoped checks in the
task report, run `git diff --check`, then:

```sh
git add src test
git commit -m "refactor: prefer standard Haskell abstractions"
```

## Follow-up Pass 3: Test Pruning

### Task 22: Remove Vacuous Pure Repeatability Assertions

**Files:**

- Modify: direct-call test modules under `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/`.
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/RegressionTests.hs`.
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ManifestTests.hs`.
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`.
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`.
- Modify: `test/Jazz/Benchmark/MetadataSpec.hs`.
- Modify: `test/Jazz/Compiler/Runtime/Observation/StatisticsTests.hs`.
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`.

**Interfaces:**

- Removes: second calls to pure lowerers/validators/renderers and alias-to-self comparisons.
- Preserves: exact result/failure/order/schema/round-trip assertions and all repeated hosted-Jazz runs.

- [ ] **Step 1: Record the exact removable inventory**

Use the revalidated inventory: twenty direct-call lowerer repeatability sites,
including `testInvalidLowererTypedCoreBoundary` and
`testManagedConstructionLowererBoundaries`; three pure Typed Core validator
reruns; the vacuous canonical lexer self-alias test; the pure half of parser
corpus determinism; and four pure serializer/renderer self-equality checks.
These are twenty-nine redundant evaluation/assertion sites total. Confirm each
surrounding test retains a stronger independent assertion before editing.

- [ ] **Step 2: Remove only redundant evaluation and rename labels**

Keep one result binding and every exact program/failure/layout/schema assertion.
Delete `testParserFixtureDeterminism`; retain parser corpus category assertions
under a name that describes adaptation/category coverage. Rename the lifted
lambda direct-call case so it no longer claims to test failure preorder after
the duplicate call is removed. Retain every repeated `produceFixture` IO run,
all hosted Jazz reruns, and Task 15's raw-vs-checked failure-order assertion.

- [ ] **Step 3: Run every affected component**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec \
  canonical-lexer-comparison-spec canonical-parser-comparison-spec jazz-lexer-parity-spec \
  jazz-parser-parity-spec benchmark-metadata-spec runtime-observation-spec \
  jazz-lowered-ir-contract-spec --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Record base formatter status for the exact touched test files. Whole-file format
only base-clean files; for legacy-layout files, format every Task 22 semantic
region and preserve unrelated layout for Task 25. Record the whole/scoped checks
in the task report, run `git diff --check`, then:

```sh
git add test
git commit -m "test: remove vacuous pure repeatability checks"
```

### Task 23: Consolidate Redundant Manifest and Parser Assertions

**Files:**

- Modify: Typed Core, Lowered IR, direct-call, control-flow, and parser corpus manifest tests.
- Modify: `test/Jazz/Compiler/Parser/DeclarationParserSpec.hs`.
- Modify: `test/Jazz/Compiler/Parser/ModuleImportParserSpec.hs`.

**Interfaces:**

- Removes: numeric counts implied by exact ordered manifests and repeated combined-list equalities.
- Removes: two internal import diagnostic cases duplicated by public parsing.
- Preserves: exact manifests, uniqueness, disjointness, classifications, validation-kind coverage, and three legacy rejection inputs.

- [ ] **Step 1: Consolidate fixture ownership without deleting semantic checks**

Keep one exact ordered manifest per fixture family. Delete only derived `length`
literals and equalities implied by retained component manifests:

- remove the Typed Core 21/56/77 counts and repeated combined-manifest equality,
  while retaining exact names, uniqueness, disjointness, and validation-kind coverage;
- remove Lowered IR 31/51/20 counts and repeated combined equality, but retain the
  behavior/performance-sensitive 1,999-failure characterization;
- remove direct-call 36/14/50 counts and the repeated combined-set equality only
  where the exact accepted/rejected lists imply them; retain the independent
  uniqueness assertion and prior-inventory subset check, dropping only its
  redundant numeric size assertion;
- remove control-flow 15/12 counts only where exact ordered manifests imply them;
  retain hosted parity reruns and expected-output construction;
- remove canonical-parser 52/101/75 family sizes where the same tests retain exact
  ordered family-name lists. Do not delete size/boundary/classification checks for
  final families lacking an equivalent exact list in the same test.

Keep one authoritative 365/196/169 parser partition assertion in
`JazzCoreModulesCorpusClosureSpec`; remove only duplicated cardinality literals
from `CanonicalParserComparisonSpec`, retaining assignment membership, exact
family order, category coverage, and boundaries. Never remove high-cardinality
behavior or performance counts merely because they are numeric.

- [ ] **Step 2: Narrow parser cleanup**

Delete `testRejectsImportAliasWithSymbolList` and
`testRejectsImportSymbolListWithAlias` from `DeclarationParserSpec`; retain the
public `parseSurfaceProgram` cases. Bind repeated parse results once inside the
duplicate-export and trailing-comma tests. Replace the three legacy module
declaration functions with one explicit table of all three source forms.

- [ ] **Step 3: Run manifest and parser components**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec \
  jazz-typed-core-expression-direct-call-spec jazz-core-control-flow-patterns-spec \
  canonical-parser-comparison-spec jazz-core-modules-corpus-closure-spec \
  declaration-parser-spec module-import-parser-spec --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Format and commit**

Record base formatter status for the exact touched tests. Whole-file format only
base-clean files; for legacy-layout files, format every Task 23 semantic region
and preserve unrelated layout for Task 25. Record the whole/scoped checks in the
task report, run `git diff --check`, then:

```sh
git add test
git commit -m "test: consolidate redundant manifests and parser cases"
```

### Task 24: Remove the Dead Compatibility Resolution Mode

**Files:**

- Modify: `src/Jazz/Compiler/BuiltinCatalog.hs`
- Modify: `test/Jazz/Compiler/Semantics/Runtime/RecursionTests.hs`
- Modify: imports and case analyses exposed by constructor removal.

**Interfaces:**

- Removes: `ResolveCompatibility` and its test-only scope-plan assertion.
- Preserves: `ResolveKernelOnly`, Prelude aliases, public conversions, and the real runtime recursion assertion.

- [ ] **Step 1: Revalidate the live call-site proof**

```sh
rg -n 'ResolveCompatibility|ResolveKernelOnly' src app test -g '*.hs'
```

Expected before editing: `ResolveCompatibility` construction occurs only in
`Runtime/RecursionTests.hs`; production contains only its definition/case branches.

- [ ] **Step 2: Remove the dead constructor and paired test-only branch**

Delete compatibility-name/catalog selection branches. Simplify exhaustive
case expressions only as far as the single remaining mode requires. Retain
public builtin alias definitions and the subsequent `runSource` behavioral
assertion in the recursion test.

- [ ] **Step 3: Run builtin, runtime, Prelude, and loader suites**

```sh
nix --extra-experimental-features 'nix-command flakes' develop --command \
  cabal test builtin-catalog-spec runtime-semantics-spec prelude-loading-spec loader-spec \
  --test-show-details=failures --jobs=1
```

- [ ] **Step 4: Prove the symbol is gone, format, and commit**

Record base formatter status for the exact touched files. Whole-file format only
base-clean files; for legacy-layout files, format every Task 24 semantic region
and preserve unrelated layout for Task 25. Record the whole/scoped commands and
results in the task report. Then:

```sh
rg -n 'ResolveCompatibility' src app test -g '*.hs'
git diff --check
git add src/Jazz/Compiler/BuiltinCatalog.hs test/Jazz/Compiler/Semantics/Runtime/RecursionTests.hs
git commit -m "refactor: remove dead builtin compatibility mode"
```

Expected `rg`: no matches.

### Task 25: Format, Verify, and Review the Aggregate Branch

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
