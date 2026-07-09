# jazz-next Compiler Architecture Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace module replay with per-module compilation and evaluation while removing the seven approved code-quality findings without changing Jazz syntax or semantics.

**Architecture:** Parse and lower each source once into a `CoreModule`, resolve a dependency-ordered `ResolvedProgram`, compile modules against explicit `ModuleInterface` values, and evaluate them against explicit `RuntimeModule` exports. Canonical core names are structured data, `EIf` is the only boolean conditional, parser components share one Megaparsec error model, and type inference delegates to focused internal modules.

**Tech Stack:** Haskell 2010, GHC 9.4.8, Cabal 3.12, `containers`, `text`, `megaparsec`, the existing `NamedTest` harness, and the active `jazz-next` compiler only.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Preserve Jazz language syntax, compile outcomes, runtime outcomes, warning behavior, and diagnostic metadata unless a focused regression test proves an existing result is internally inconsistent.
- Breaking Haskell APIs are allowed.
- Keep `()` as Unit and `\()` as one Unit-pattern parameter; do not add true nullary functions.
- Do not introduce serialized module artifacts, incremental compilation, a linker, a stable public Haskell API, lenses, `StateT`, recursion schemes, a compiler-pass framework, Hspec, or Tasty.
- Use `apply_patch` for source edits and commit every completed task.
- Follow test-driven development for every behavioral or structural change.
- Keep the working tree free of unrelated edits and preserve the current branch history.

---

## Planned File Structure

### New production modules

- `jazz-next/src/JazzNext/Compiler/Name.hs` — structured core names and namespaces.
- `jazz-next/src/JazzNext/Compiler/Pattern.hs` — shared core-pattern binder/reference semantics.
- `jazz-next/src/JazzNext/Compiler/Parser/Context.hs` — parser context shared without module cycles.
- `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs` — `CoreModule`, `ResolvedImport`, `ResolvedModule`, and `ResolvedProgram`.
- `jazz-next/src/JazzNext/Compiler/Prelude.hs` — resolved/prepared prelude source and its parse-once metadata.
- `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs` — compile-time and runtime module export records.
- `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs` — dependency-ordered analysis and inference.
- `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs` — dependency-ordered runtime evaluation.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs` — internal type language and named scheme records.
- `jazz-next/src/JazzNext/Compiler/TypeInference/State.hs` — nested inference state records.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Solver.hs` — substitution, fresh variables, and unification.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs` — class/impl constraints and facts.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Pattern.hs` — pattern typing.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs` — scope, signatures, recursive groups, and exports.
- `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs` — type-specific diagnostics and rendering.

### New test/support modules

- `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PatternSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/CoreNormalizationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/NameSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/Loader/{Basic,Visibility,Capabilities,Operators,Diagnostics}Tests.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/{ControlFlow,Recursion,Numeric,Capabilities,Rendering}Tests.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/{Basics,Generalization,Constraints,Recursion,Diagnostics}Tests.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPattern/{Declarations,Patterns,InvalidSyntax}Tests.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/Foundation/{Expressions,Signatures,Modules,InvalidSyntax}Tests.hs`

### Modules deleted by the completed plan

- `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs`
- `jazz-next/src/JazzNext/Compiler/Desugar.hs`
- `jazz-next/src/JazzNext/Compiler/Identifier.hs` after all imports move to `Compiler.Name`.

---

### Task 1: Lock the module-pipeline behavior contract

**Files:**
- Create: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/jazz-next.cabal:88-107`

**Interfaces:**
- Consumes: existing `compileModuleGraphWithPrelude`, `runModuleGraphWithPrelude`, `ModuleResolutionConfig`, `CompileResult`, and `RunResult`.
- Produces: a focused parity suite named `module-pipeline-contract-spec` that both the replay and replacement pipelines must pass.

- [ ] **Step 1: Add the characterization suite**

Create a `Main` module using the existing harness. Include these tests with concrete source maps:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileModuleGraphWithPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModulePipelineContract" tests

tests :: [NamedTest]
tests =
  [ ("dependency expressions are checked but not executed", testDependencyExpressionContract),
    ("alias imports stay qualified", testAliasIsolationContract),
    ("transitive imports do not leak", testTransitiveVisibilityContract),
    ("module diagnostics retain source paths", testSourcePathContract)
  ]

testDependencyExpressionContract :: IO ()
testDependencyExpressionContract = do
  result <- runGraph dependencyExpressionSources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "entry output" (Just "1") (runOutput result)
  where
    dependencyExpressionSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. value. }"),
          ("src/Lib/Value.jz", "module Lib::Value { value = 1. 1 / 0. }")
        ]

testAliasIsolationContract :: IO ()
testAliasIsolationContract = do
  result <- runGraph sources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value as Value. Value::answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]

testTransitiveVisibilityContract :: IO ()
testTransitiveVisibilityContract = do
  result <- compileGraph sources
  assertEqual "warning count" 0 (length (compileWarnings result))
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "unbound code"
    "E1001"
    (renderDiagnostic (head (compileErrors result)))
  assertContains
    "alias-hidden transitive export"
    "unbound variable 'subtract'"
    (renderDiagnostic (head (compileErrors result)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nsubtract."),
          ("src/App/UsesMath.jz", "import Lib::Math as Math.\nuse = 0."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]

testSourcePathContract :: IO ()
testSourcePathContract = do
  result <- compileGraph sources
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "dependency source path"
    "src/Lib/Bad.jz:"
    (renderDiagnostic (head (compileErrors result)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Bad. bad. }"),
          ("src/Lib/Bad.jz", "module Lib::Bad { bad = missing. }")
        ]

runGraph :: Map.Map FilePath Text -> IO RunResult
runGraph sources =
  runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

compileGraph :: Map.Map FilePath Text -> IO CompileResult
compileGraph sources =
  compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
```

- [ ] **Step 2: Register the suite**

Add:

```cabal
test-suite module-pipeline-contract-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
```

- [ ] **Step 3: Run the characterization suite**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec --test-show-details=direct
```

Expected: PASS on the existing replay pipeline. If any proposed source is invalid under current Jazz syntax, replace only that source with the nearest existing `LoaderSpec` fixture that exercises the same contract; do not weaken the assertion.

- [ ] **Step 4: Run the adjacent loader and resolver suites**

Run:

```bash
cabal test --project-dir=jazz-next loader-spec module-resolution-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/jazz-next.cabal jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
git commit -m "test: lock module pipeline behavior"
```

---

### Task 2: Centralize core-pattern semantics

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/Pattern.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/PatternSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs:1108-1184`
- Modify: `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs:483-529`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs:755-808`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs:3162-3193`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs:5640-5686`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: `JazzNext.Compiler.AST.Pattern` and `Identifier`.
- Produces: `patternBinderNames`, `commonPatternBinderNames`, and `extendBoundWithPattern` as the only core implementations.

- [ ] **Step 1: Write the failing pattern-semantics test**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import JazzNext.Compiler.AST (Pattern (..))
import JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    patternBinderNames
  )
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "PatternSemantics" tests

tests :: [NamedTest]
tests =
  [ ("tuple and as-pattern binders are collected", testNestedBinders),
    ("or-patterns expose only common binders", testOrPatternBinders),
    ("Unit binds no names", testUnitBinders)
  ]

testNestedBinders :: IO ()
testNestedBinders =
  assertEqual
    "nested binders"
    (Set.fromList ["whole", "left", "right"])
    (patternBinderNames (PAs "whole" (PTuple [PVariable "left", PVariable "right"])))

testOrPatternBinders :: IO ()
testOrPatternBinders =
  assertEqual
    "common binders"
    (Set.singleton "x")
    (commonPatternBinderNames [PTuple [PVariable "x", PWildcard], PTuple [PVariable "x", PVariable "y"]])

testUnitBinders :: IO ()
testUnitBinders = assertEqual "Unit binders" Set.empty (patternBinderNames (PTuple []))
```

Register `pattern-semantics-spec` in the Cabal file.

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next pattern-semantics-spec --test-show-details=direct
```

Expected: FAIL because `JazzNext.Compiler.Pattern` does not exist.

- [ ] **Step 3: Add the shared implementation**

```haskell
module JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    extendBoundWithPattern,
    patternBinderNames
  ) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST (Pattern (..))
import JazzNext.Compiler.Identifier (identifierText)

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern patternValue bound =
  Set.union bound (patternBinderNames patternValue)

commonPatternBinderNames :: [Pattern] -> Set Text
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl' Set.intersection (patternBinderNames firstAlternative) (map patternBinderNames rest)

patternBinderNames :: Pattern -> Set Text
patternBinderNames patternValue =
  case patternValue of
    PVariable name -> Set.singleton (identifierText name)
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns -> Set.unions (map patternBinderNames patterns)
    PList patterns -> Set.unions (map patternBinderNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    PTuple patterns -> Set.unions (map patternBinderNames patterns)
    PAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinderNames nestedPattern)
    POr alternatives -> commonPatternBinderNames alternatives
```

- [ ] **Step 4: Replace all core duplicates with imports**

Delete the local copies in `Analyzer`, `RecursiveBindings`, `ModuleReplay`, `Runtime`, and `TypeInference`. Keep surface-pattern binder logic in `ModuleResolver` because it operates on `SurfacePattern`.

- [ ] **Step 5: Verify focused and adjacent suites**

Run:

```bash
cabal test --project-dir=jazz-next pattern-semantics-spec adt-pattern-type-spec adt-pattern-runtime-spec recursive-bindings-spec loader-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 6: Verify there is one core definition**

Run:

```bash
rg -n "^(patternBinderNames|commonPatternBinderNames) ::" jazz-next/src/JazzNext/Compiler
```

Expected: core definitions only in `Compiler/Pattern.hs`, plus distinctly named surface helpers if retained.

- [ ] **Step 7: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Pattern.hs jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs jazz-next/src/JazzNext/Compiler/ModuleReplay.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/test/JazzNext/Compiler/Semantics/PatternSemanticsSpec.hs jazz-next/jazz-next.cabal
git commit -m "refactor: centralize core pattern semantics"
```

---

### Task 3: Canonicalize the core AST around `EIf`

**Files:**
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/CoreNormalizationSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs:70-89`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs:55-111`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs:145-237`
- Modify: parser and semantic tests that construct `ECase`
- Delete: `jazz-next/src/JazzNext/Compiler/Desugar.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: surface `SEIf` and `SEBinary "$"`.
- Produces: core `EIf` for conditionals and `EApply` for `$`; `ECase` and `desugarExpr` cease to exist.

- [ ] **Step 1: Write RED normalization tests**

Add tests that assert both lowering and inference preserve the canonical forms:

```haskell
testIfRemainsCanonicalIf :: IO ()
testIfRemainsCanonicalIf =
  assertRight "parse if" (parseSurfaceProgram "if True { 1 } else { 2 }.") $ \surface -> do
    let lowered = lowerSurfaceExpr surface
    inference <- inferExpressionDefault lowered
    assertEqual "lowered equals inferred" lowered (inferredExpr inference)

testDollarLowersToApplication :: IO ()
testDollarLowersToApplication =
  assertRight "parse dollar" (parseSurfaceProgram "f $ x.") $ \surface ->
    assertEqual
      "canonical dollar"
      (EBlock [SExpr (SourceSpan 1 1) (EApply (EVar "f") (EVar "x"))])
      (lowerSurfaceExpr surface)
```

Register `core-normalization-spec`.

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next core-normalization-spec --test-show-details=direct
```

Expected: FAIL because inference rewrites `EIf` to `ECase`, and lowering preserves `$` as `EBinary`.

- [ ] **Step 3: Canonicalize during lowering**

Change the binary branch in `lowerSurfaceExpr`:

```haskell
    SEBinary "$" functionExpr argumentExpr ->
      EApply (lowerSurfaceExpr functionExpr) (lowerSurfaceExpr argumentExpr)
    SEBinary operatorSymbol leftExpr rightExpr ->
      EBinary operatorSymbol (lowerSurfaceExpr leftExpr) (lowerSurfaceExpr rightExpr)
```

Keep `SEIf -> EIf` unchanged.

- [ ] **Step 4: Remove `ECase` and downstream duplicate branches**

Delete `ECase` from `AST.Expr`. In every exhaustive AST walk, keep the existing `EIf` behavior and delete the `ECase` branch. Change runtime evaluation to evaluate `EIf` directly.

Delete `canonicalizeExpr`, `canonicalizeStatement`, and `canonicalizeImplMethod` from `TypeInference`. Set:

```haskell
  let canonicalExpr = expr
```

Delete `Desugar.hs`, remove its Cabal entry, and update its three test callers to assert `lowerSurfaceExpr` output directly.

- [ ] **Step 5: Verify focused and adjacent suites**

Run:

```bash
cabal test --project-dir=jazz-next core-normalization-spec if-expression-parser-spec if-expression-type-spec operator-section-spec runtime-semantics-spec recursive-bindings-spec loader-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 6: Verify structural deletion**

Run:

```bash
rg -n "\bECase\b|JazzNext\.Compiler\.Desugar|desugarExpr" jazz-next/src jazz-next/test jazz-next/jazz-next.cabal
```

Expected: no output.

- [ ] **Step 7: Commit**

```bash
git add jazz-next/src jazz-next/test jazz-next/jazz-next.cabal
git commit -m "refactor: canonicalize core conditionals and application"
```

---

### Task 4: Introduce structured core names

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/Name.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/NameSemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/{Analyzer,RecursiveBindings,ModuleReplay,Runtime,TypeInference}.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Identifier.hs` only to add `Ord` during the transition
- Modify: affected tests and `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: source-level `Identifier` values.
- Produces: `Name`, `NameNamespace`, `ResolvedNameOrigin`, `GeneratedNameKind`, and explicit constructors/helpers for source, qualified, resolved, builtin, and generated names.

- [ ] **Step 1: Write RED structured-name tests**

```haskell
testSourceAndResolvedNamesAreDistinct :: IO ()
testSourceAndResolvedNamesAreDistinct = do
  let source = sourceName (mkIdentifier "Lib::answer")
      imported = resolvedImportedName ["Lib"] ValueNamespace (mkIdentifier "answer")
  assertEqual "rendered source" "Lib::answer" (renderName source)
  assertEqual "rendered imported" "Lib::answer" (renderName imported)
  assertEqual "structured distinction" False (source == imported)

testGeneratedNamesDoNotAcquireUserPurity :: IO ()
testGeneratedNamesDoNotAcquireUserPurity =
  assertEqual
    "generated name"
    (GeneratedName (LambdaPatternArgument 1))
    (generatedName (LambdaPatternArgument 1))
```

Expected imports:

```haskell
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    generatedName,
    qualifiedName,
    renderName,
    resolvedImportedName,
    sourceName
  )
```

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next name-semantics-spec --test-show-details=direct
```

Expected: FAIL because `Compiler.Name` does not exist.

- [ ] **Step 3: Add the structured name model**

```haskell
module JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    builtinName,
    generatedName,
    namePurity,
    qualifiedName,
    renderName,
    resolvedAmbientName,
    resolvedImportedName,
    resolvedLocalName,
    sourceName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierPurity,
    identifierText
  )
import JazzNext.Compiler.Purity (Purity (..))

data NameNamespace
  = ValueNamespace
  | ConstructorNamespace
  | TypeNamespace
  | CapabilityNamespace
  deriving (Eq, Ord, Show)

data ResolvedNameOrigin
  = CurrentModule
  | ImportedModule [Text]
  | AmbientPrelude
  deriving (Eq, Ord, Show)

data GeneratedNameKind
  = LambdaPatternArgument Int
  | OperatorBinding Text
  | OperatorSectionFunction
  | OperatorSectionLeft
  | OperatorSectionRight
  | ModuleReplayBridge [Text] NameNamespace Text
  deriving (Eq, Ord, Show)

data Name
  = SourceName Identifier
  | QualifiedName Identifier Identifier
  | ResolvedName ResolvedNameOrigin NameNamespace Identifier
  | BuiltinName Identifier
  | GeneratedName GeneratedNameKind
  deriving (Eq, Ord, Show)

sourceName :: Identifier -> Name
sourceName = SourceName

qualifiedName :: Identifier -> Identifier -> Name
qualifiedName = QualifiedName

resolvedLocalName :: NameNamespace -> Identifier -> Name
resolvedLocalName = ResolvedName CurrentModule

resolvedImportedName :: [Text] -> NameNamespace -> Identifier -> Name
resolvedImportedName modulePath = ResolvedName (ImportedModule modulePath)

resolvedAmbientName :: NameNamespace -> Identifier -> Name
resolvedAmbientName = ResolvedName AmbientPrelude

builtinName :: Identifier -> Name
builtinName = BuiltinName

generatedName :: GeneratedNameKind -> Name
generatedName = GeneratedName

renderName :: Name -> Text
renderName name =
  case name of
    SourceName identifier -> identifierText identifier
    QualifiedName qualifier member -> identifierText qualifier <> "::" <> identifierText member
    ResolvedName CurrentModule _ member -> identifierText member
    ResolvedName (ImportedModule modulePath) _ member -> Text.intercalate "::" (modulePath ++ [identifierText member])
    ResolvedName AmbientPrelude _ member -> identifierText member
    BuiltinName identifier -> identifierText identifier
    GeneratedName generated -> "<generated:" <> Text.pack (show generated) <> ">"

namePurity :: Name -> Purity
namePurity name =
  case name of
    SourceName identifier -> identifierPurity identifier
    QualifiedName _ member -> identifierPurity member
    ResolvedName _ _ member -> identifierPurity member
    BuiltinName identifier -> identifierPurity identifier
    GeneratedName _ -> Pure
```

If `Purity` uses constructors other than `Pure`, use the existing pure constructor from `Purity.hs`; do not derive generated purity from rendered text.

Change the source `Identifier` derivation to `deriving (Eq, Ord, Show)` so it can participate in structured `Name` ordering.

- [ ] **Step 4: Move core name positions to `Name`**

Change core references and binding names to `Name`, including:

```haskell
data Expr
  = ELit Literal
  | EVar Name
  | ELambda Name Expr
  | EOperatorValue Text
  | EList [Expr]
  | ETuple [Expr]
  | EApply Expr Expr
  | ETypeApplication Expr SignatureType
  | EIf Expr Expr Expr
  | EPatternCase Expr [CaseArm]
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EBlock [Statement]

data Pattern
  = PWildcard
  | PVariable Name
  | PLiteral Literal
  | PConstructor Name [Pattern]
  | PList [Pattern]
  | PConsList Pattern Pattern
  | PTuple [Pattern]
  | PAs Name Pattern
  | POr [Pattern]
```

Change core statement, constructor, class, impl, constraint, and signature name positions consistently. Keep `Parser.AST` source-owned and lower each source identifier with `sourceName`; lower `SEQualifiedVar` with `qualifiedName`.

Use `GeneratedName` for lambda-pattern parameters and operator helpers. Use `GeneratedName (ModuleReplayBridge ...)` only as a temporary replay implementation detail; no downstream code may inspect rendered prefixes.

Update `Compiler.Pattern` from `Set Text` to `Set Name` at the same time, and update its characterization tests to expect `sourceName` values. From this task onward, compiler-owned binder/reference sets never project through `renderName` for lookup.

- [ ] **Step 5: Change semantic maps to structured keys**

Change analyzer binding maps, `TypeEnv`, runtime environments, recursive free-variable sets, and module replay reference maps from `Text` keys to `Name` keys wherever they represent core names. Convert to `renderName` only when building diagnostics.

Remove all `Text.isPrefixOf "__module::"`, `Text.splitOn "::"`, and `$...` classification logic from analyzer, inference, and runtime. Pattern match on `Name` instead.

- [ ] **Step 6: Verify focused and adjacent suites**

Run:

```bash
cabal test --project-dir=jazz-next name-semantics-spec lambda-parser-spec lambda-semantics-spec primitive-semantics-spec adt-pattern-type-spec adt-pattern-runtime-spec loader-spec --test-show-details=direct
```

Expected: PASS with unchanged Jazz-visible output.

- [ ] **Step 7: Commit**

```bash
git add jazz-next/src jazz-next/test jazz-next/jazz-next.cabal
git commit -m "refactor: represent compiler names structurally"
```

---

### Task 5: Unify the token parser and pattern grammar

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs`
- Rewrite: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`
- Modify: parser tests

**Interfaces:**
- Consumes: lexer `[Token]` streams.
- Produces: `runTokenParser`, `runTokenParserPrefix`, `failTokenParser`, and direct `Parser` grammar functions with one Megaparsec error channel.

- [ ] **Step 1: Add prefix-runner tests**

Extend `TokenParserSpec.hs` with:

```haskell
testRunTokenParserPrefixReturnsRemainder :: IO ()
testRunTokenParserPrefixReturnsRemainder =
  assertEqual
    "prefix result"
    (Right ("x", [Token TDot "." (SourceSpan 1 2)]))
    (runTokenParserPrefix "identifier prefix" parseIdentifier tokens)
  where
    tokens =
      [ Token (TIdentifier "x") "x" (SourceSpan 1 1),
        Token TDot "." (SourceSpan 1 2)
      ]
```

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next token-parser-spec --test-show-details=direct
```

Expected: FAIL because `runTokenParserPrefix` is missing.

- [ ] **Step 3: Add the prefix runner**

```haskell
runTokenParserPrefix :: Text -> Parser a -> [Token] -> Either Diagnostic (a, [Token])
runTokenParserPrefix label parser tokens =
  case MP.runParser ((,) <$> parser <*> MP.getInput) (Text.unpack label) tokens of
    Right value -> Right value
    Left bundle -> Left (parseDiagnostic (tokenParserErrorMessage bundle))

failTokenParser :: Text -> Parser a
failTokenParser = MP.customFailure . ParserError
```

Export `failTokenParser`; keep `ParserError` private so grammar modules can report domain diagnostics without owning a second error type.

- [ ] **Step 4: Rewrite `Parser.Pattern` as direct combinators**

Delete `PatternParser`, its `Functor`/`Applicative`/`Monad` instances, `getRemainingTokens`, `setRemainingTokens`, `throwDiagnostic`, and the input-clearing finalizer.

Expose direct parser values:

```haskell
parseCaseArmPatternParser :: Parser SurfacePattern
parseCasePatternParser :: Parser SurfacePattern
parseLambdaParameterParser :: Parser SurfaceLambdaParameter

parseCaseArmPatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCaseArmPatternTokens = runTokenParserPrefix "case arm pattern" parseCaseArmPatternParser
```

Implement token consumption through `parseAnyToken`, `parseToken`, `peekToken`, `MP.choice`, `MP.many`, and `MP.lookAhead`. Use `failTokenParser` for every current diagnostic message.

- [ ] **Step 5: Remove signature parser error swallowing**

Replace `parseTokenStreamMaybe` and `parseTokenStreamPrefixMaybe` call sites with explicit `Either Diagnostic` runners where failure is a real error. Keep `Maybe` only for intentional grammar alternatives implemented through `<|>` inside Megaparsec.

- [ ] **Step 6: Verify parser suites**

Run:

```bash
cabal test --project-dir=jazz-next token-parser-spec pattern-parser-spec adt-pattern-parser-spec lambda-parser-spec parser-foundation-spec --test-show-details=direct
```

Expected: PASS with unchanged diagnostics.

- [ ] **Step 7: Verify structural cleanup and commit**

Run:

```bash
rg -n "PatternParser|MP\.setInput|Parser \(Either Diagnostic" jazz-next/src/JazzNext/Compiler/Parser
```

Expected: no output.

```bash
git add jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser
git commit -m "refactor: unify token and pattern parsing"
```

---

### Task 6: Give expression and declaration grammars real ownership

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/Parser/Context.hs`
- Rewrite: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Rewrite: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Reduce: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: parser tests and `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: `ParserContext`, token streams, and parser callbacks only where recursive grammar requires them.
- Produces: owner-defined expression/declaration combinators and a small `parseSurfaceProgram` façade that ties their recursive callbacks together.

- [ ] **Step 1: Add parser ownership tests**

Update `ExpressionParserSpec` and `DeclarationParserSpec` to import direct parser entrypoints from their owner modules and run them through `runTokenParserPrefix`. The tests must no longer reach `parseSurfaceExpressionTokens` through `Parser.hs`.

Expected direct interface:

```haskell
data ParserContext = ParserContext
  { parserKnownAliases :: Set Text,
    parserDeclaredOperators :: [OperatorInfo],
    parserStatementContext :: StatementContext
  }

type ExpressionParser = ParserContext -> Parser SurfaceExpr
type StatementBlockParser = ParserContext -> Parser [SurfaceStatement]

parseExpressionParser :: StatementBlockParser -> ExpressionParser
parseStatementParser ::
  ExpressionParser ->
  StatementBlockParser ->
  ParserContext ->
  Parser ([SurfaceStatement], ParserContext)
```

The returned context carries operator declarations and aliases forward to the next statement. A declaration may return multiple surface statements because a braced module declaration retains its existing flattened `SSModule : body` representation during this parser-only refactor.

- [ ] **Step 2: Run ownership tests to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next expression-parser-spec declaration-parser-spec --test-show-details=direct
```

Expected: FAIL because the direct parser interfaces do not exist.

- [ ] **Step 3: Add `Parser.Context` and concrete recursive callback types**

Move `StatementContext`, `ParserContext`, `ExpressionParser`, and `StatementBlockParser` out of `Parser.hs`. Keep constructor names `TopLevelContext`, `ModuleBodyContext`, and `NestedBlockContext` so diagnostic logic remains recognizable. These are concrete aliases over `Parser`, not a generic grammar framework.

- [ ] **Step 4: Move the expression grammar**

Move `parseExpr` and every expression-only helper from `Parser.hs` into `Parser.Expression`. Blocks call the supplied `StatementBlockParser`; this is the only dependency on statement grammar. Replace the current façade:

```haskell
parseExpressionTokens = parseSurfaceExpressionTokens
```

with:

```haskell
parseExpressionTokens :: StatementBlockParser -> ParserContext -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens parseBlock context =
  runTokenParserPrefix "expression" (parseExpressionParser parseBlock context)
```

The implementation must consume tokens through Megaparsec primitives, not by copying the complete input into a separate hand parser.

- [ ] **Step 5: Move declaration grammar ownership**

Convert module, import, data, class, and impl declaration entrypoints to direct `Parser` values. `parseStatementParser` receives the concrete expression/block callbacks above and returns the next `ParserContext`, allowing operator declarations to update later parsing without mutable global state. Module bodies use a context with `ModuleBodyContext`; nested expression blocks use `NestedBlockContext`. Remove `runDeclarationParser` and `parseDeclarationWithRemainder`.

Preserve the current scope-level import-alias pre-scan used to classify forward alias-qualified references, but limit it to a read-only `lookAhead` inventory pass. It must not parse expressions, clear input, or maintain a second token cursor.

- [ ] **Step 6: Reduce `Parser.hs` to orchestration**

`Parser.hs` retains only:

```haskell
parseSurfaceProgram :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgram source = do
  tokens <- tokenize source
  runTokenParser "program" programParser tokens
  where
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser
    programParser = SEBlock <$> parseProgramStatements statementParser initialParserContext
```

`parseProgramStatements` and `parseStatementsUntilBrace` are short orchestration loops: call the owner-defined statement parser, thread its returned context, and stop at EOF or `}` respectively. They contain no expression or declaration branches. `Parser.hs` may also re-export stable entrypoints needed by existing tests, but it must not own expression/declaration implementations.

- [ ] **Step 7: Verify every parser suite**

Run:

```bash
cabal test --project-dir=jazz-next adt-pattern-parser-spec declaration-parser-spec expression-parser-spec if-expression-parser-spec lambda-parser-spec module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec operator-section-spec parser-foundation-spec pattern-parser-spec token-parser-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser.hs jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser jazz-next/jazz-next.cabal
git commit -m "refactor: assign parser grammar ownership"
```

---

### Task 7: Extract inference types, state, and solver

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs`
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/State.hs`
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Solver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: existing internal type definitions and solver functions.
- Produces: named `TypeScheme`, named `DeferredExplicitConstraint`, nested `InferState`, and solver operations with unchanged behavior.

- [ ] **Step 1: Add record-shape tests**

Extend `BindingSignatureCoherenceSpec` with a compile-only import of the internal records and a round-trip assertion:

```haskell
testTypeSchemeRecordPreservesFields :: IO ()
testTypeSchemeRecordPreservesFields =
  assertEqual
    "scheme result"
    (TFunctionType (TVarType 0) (TVarType 0))
    (schemeResultType scheme)
  where
    scheme =
      TypeScheme
        { schemeQuantifiedVariables = Set.singleton 0,
          schemeQuantifiedOrder = [0],
          schemeClassConstraints = [],
          schemePrimitiveConstraints = [],
          schemeDefiningCapabilities = emptyScopeCapabilityFacts,
          schemeResultType = TFunctionType (TVarType 0) (TVarType 0)
        }
```

This is an internal structural test; do not expose these types outside the package.

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next binding-signature-coherence-spec --test-show-details=direct
```

Expected: FAIL because the named fields/modules do not exist.

- [ ] **Step 3: Extract `TypeInference.Types`**

Move `ExpressionType`, `ConstructorArgumentType`, `IntegerLiteralRange`, `NumericConstraint`, `TypeBinding`, `TypeSchemePrimitiveConstraint`, `TypeSchemeConstraint`, `DataTypeBinding`, `ClassMethodType`, and `ImplMethodType` into `Types.hs`.

Move `ScopeCapabilityFacts` and `emptyScopeCapabilityFacts` into `Types.hs` as well because `TypeScheme` stores defining capability facts. Capability-specific merge/filter operations move later to `Capabilities.hs`.

Replace the positional scheme with:

```haskell
data TypeScheme = TypeScheme
  { schemeQuantifiedVariables :: Set Int,
    schemeQuantifiedOrder :: [Int],
    schemeClassConstraints :: [TypeSchemeConstraint],
    schemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint],
    schemeDefiningCapabilities :: ScopeCapabilityFacts,
    schemeResultType :: ExpressionType
  }
  deriving (Eq, Show)
```

Update every constructor and pattern match by field name.

- [ ] **Step 4: Extract nested state records**

Create:

```haskell
data SolverState = SolverState
  { solverNextTypeVar :: Int,
    solverSubstitution :: Map Int ExpressionType,
    solverStrictEqualityVars :: Set Int,
    solverNumericVars :: Map Int NumericConstraint
  }

data DeclarationState = DeclarationState
  { declarationDataTypes :: Map Name DataTypeBinding,
    declarationClassFacts :: Map Name Int,
    declarationConcreteImplFacts :: Set Text,
    declarationClassMethodSignatures :: Map Name ClassMethodType,
    declarationConcreteImplMethods :: Map Text [ImplMethodType]
  }

data ModuleInferenceState = ModuleInferenceState
  { inferenceModulePath :: Maybe [Text],
    inferenceLocalCapabilities :: ScopeCapabilityFacts,
    inferenceModuleCapabilities :: Map [Text] ScopeCapabilityFacts
  }

data InferenceOutput = InferenceOutput
  { outputRuntimeHints :: Map BindingRuntimeHintKey ConstraintSignatureType,
    outputDeferredConstraints :: [DeferredExplicitConstraint],
    outputInferredConstraints :: [TypeSchemeConstraint],
    outputErrorsRev :: [Diagnostic],
    outputErrorCount :: Int
  }

data InferState = InferState
  { inferSolver :: SolverState,
    inferDeclarations :: DeclarationState,
    inferModule :: ModuleInferenceState,
    inferOutput :: InferenceOutput
  }
```

Define `DeferredExplicitConstraint` as a record with fields `deferredConstraintName`, `deferredMethodKey`, `deferredWasInferred`, `deferredArgumentType`, `deferredVisibleFacts`, and `deferredStructuralFacts`.

- [ ] **Step 5: Extract solver operations**

Move `freshTypeVariable`, `freshTypeVar`, `resolveType`, `applySubstitution`, `unifyTypes`, `unifyTypeLists`, `bindTypeVar`, `occursInType`, and numeric/equality solver-state updates to `Solver.hs`.

Keep signatures explicit:

```haskell
freshTypeVar :: InferState -> (ExpressionType, InferState)
resolveType :: InferState -> ExpressionType -> ExpressionType
unifyTypes :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
```

- [ ] **Step 6: Verify type and runtime suites**

Run:

```bash
cabal test --project-dir=jazz-next binding-signature-coherence-spec primitive-semantics-spec adt-pattern-type-spec runtime-semantics-spec loader-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs jazz-next/jazz-next.cabal
git commit -m "refactor: extract inference types state and solver"
```

---

### Task 8: Extract inference capabilities, patterns, scope, and diagnostics

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Pattern.hs`
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Reduce: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: `Types`, `State`, `Solver`, AST, capability facts, recursive-binding facts.
- Produces: a façade `TypeInference.hs` exposing only `InferenceResult` and the four existing entrypoints, plus the future module-aware entrypoint.

- [ ] **Step 1: Add façade-only tests**

Change semantic tests so they import only public façade functions from `JazzNext.Compiler.TypeInference`. Keep the structural record test from Task 7 in a dedicated internal test module if necessary. Add a source scan assertion to `test-warning-config.sh`:

```bash
if rg -n '^data (ExpressionType|InferState|TypeScheme)\b' jazz-next/src/JazzNext/Compiler/TypeInference.hs; then
  echo "TypeInference facade still owns internal model types" >&2
  exit 1
fi
```

- [ ] **Step 2: Run the structural check to verify RED**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: FAIL while model types still live in the façade.

- [ ] **Step 3: Extract diagnostics**

Move every `mk*TypeError`, invalid-signature summary, and internal type renderer into `Diagnostics.hs`. Export only constructors called by the other inference modules.

Add the concrete callback type used to break Haskell module cycles without introducing a generic pass framework:

```haskell
type InferExprFn =
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)
```

- [ ] **Step 4: Extract pattern typing**

Move `PatternTyping`, `inferPatternCaseType`, `inferPatternType`, or-pattern agreement, constructor/list/tuple pattern typing, and pattern error rollback to `Pattern.hs`. Import binder semantics from `JazzNext.Compiler.Pattern`; do not recreate them. Functions that infer guards or arm bodies accept `InferExprFn` as their first argument.

- [ ] **Step 5: Extract capability handling**

Move capability-state seeding, impl method checks, explicit/inferred/deferred constraint handling, capability-fact merging, and qualified method type instantiation to `Capabilities.hs`. Impl-body checking accepts `InferExprFn` explicitly.

- [ ] **Step 6: Extract scope inference**

Move `inferScopeType`, signature adjacency/type application, ordinary and recursive binding schemes, data constructor registration, environment generalization, and runtime-hint publication to `Scope.hs`. Scope inference accepts `InferExprFn` explicitly so the façade can pass its concrete recursive expression dispatcher without an import cycle.

Expose:

```haskell
inferScopeType :: InferExprFn -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
```

- [ ] **Step 7: Reduce the façade**

`TypeInference.hs` should define `InferenceResult`, default entrypoints, and the top-level expression dispatcher. It must delegate scope, pattern, capability, solver, and diagnostic ownership to their modules.

- [ ] **Step 8: Verify the complete semantic suite**

Run:

```bash
cabal test --project-dir=jazz-next adt-pattern-type-spec binding-signature-coherence-spec builtin-catalog-spec if-expression-type-spec lambda-semantics-spec primitive-semantics-spec purity-semantics-spec rebinding-warning-spec recursive-bindings-spec runtime-semantics-spec loader-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 9: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/scripts/test-warning-config.sh jazz-next/test jazz-next/jazz-next.cabal
git commit -m "refactor: split inference by semantic ownership"
```

---

### Task 9: Retain parsed modules in a resolved graph

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs` as a temporary consumer
- Modify: module resolver/loader tests and `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: source lookup and `ModuleResolutionConfig`.
- Produces: `resolveProgram :: ModuleResolutionConfig -> BuiltinResolutionMode -> Set Name -> Set Name -> (FilePath -> IO (Maybe Text)) -> [Text] -> IO (Either Diagnostic ResolvedProgram)`.

- [ ] **Step 1: Write RED resolved-program tests**

Add to `ModuleResolutionSpec`:

```haskell
testResolvedProgramRetainsLoweredModules :: IO ()
testResolvedProgramRetainsLoweredModules = do
  result <- resolveProgram resolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "resolved program" result $ \program -> do
    assertEqual "module order" [["Lib", "Value"], ["App", "Main"]] (map resolvedModulePath (resolvedProgramModules program))
    assertEqual "entry path" ["App", "Main"] (resolvedProgramEntryPath program)
    assertEqual "module count" 2 (length (resolvedProgramModules program))
    assertEqual "unresolved core names" [] (concatMap unresolvedResolvedModuleNames (resolvedProgramModules program))
```

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next module-resolution-spec --test-show-details=direct
```

Expected: FAIL because `ResolvedProgram` and `resolveProgram` do not exist.

- [ ] **Step 3: Add graph records**

```haskell
data CoreModule = CoreModule
  { coreModuleDeclaredPath :: Maybe [Text],
    coreModuleImports :: [ResolvedImport],
    coreModuleExpr :: Expr
  }

data ResolvedImport = ResolvedImport
  { resolvedImportSpan :: SourceSpan,
    resolvedImportPath :: [Text],
    resolvedImportAlias :: Maybe Text,
    resolvedImportSymbols :: Maybe [Text]
  }

data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedModuleImports :: [ResolvedImport],
    resolvedModuleCore :: CoreModule
  }

data ResolvedProgram = ResolvedProgram
  { resolvedProgramEntryPath :: [Text],
    resolvedProgramModules :: [ResolvedModule]
  }
```

- [ ] **Step 4: Add module-aware lowering**

Implement:

```haskell
lowerSurfaceModule :: FilePath -> [Text] -> SurfaceExpr -> Either Diagnostic CoreModule
```

It validates/extracts the optional `SSModule`, extracts module-scope `SSImport` declarations into `coreModuleImports`, removes both statement forms from `coreModuleExpr`, qualifies spans with the source path during lowering, and lowers the remaining statements exactly once.

- [ ] **Step 5: Retain each core and resolve its names structurally**

Replace `parseModuleDetails` with a single parse/lower operation that returns both resolver inventories and `CoreModule`. After import validation, traverse the core once and rewrite names as follows:

- declaration binders and unqualified references to local inventory become `ResolvedName CurrentModule namespace identifier`;
- selected imported references become `ResolvedName (ImportedModule dependencyPath) namespace identifier`;
- ambient prelude references become `ResolvedName AmbientPrelude namespace identifier`;
- builtin references recognized under the supplied `BuiltinResolutionMode` become `BuiltinName identifier`; and
- alias-qualified references become the corresponding `ImportedModule` name.

An otherwise-unbound unqualified reference defaults to `CurrentModule`, allowing the existing analyzer/inference path to emit the same E1001 diagnostic. A successfully resolved module must contain no `SourceName` or `QualifiedName`; add the test-only `unresolvedResolvedModuleNames` traversal used above to enforce that invariant. Store the rewritten core in `ResolvedModule`. Keep deterministic graph ordering and all existing E4001-E4014 diagnostics.

- [ ] **Step 6: Make replay consume retained cores**

Temporarily change `loadLoweredModuleGraph` to call `resolveProgram` with the active builtin mode and pass `resolvedModuleCore` values to `buildModuleGraphExpr`. Delete `replayResolvedSources`, `parseAndLowerResolvedModule`, `qualifyExprSourceSpans`, and `loadModuleGraphSource`.

- [ ] **Step 7: Verify module suites and structural parse-once check**

Run:

```bash
cabal test --project-dir=jazz-next module-resolution-spec loader-spec module-pipeline-contract-spec --test-show-details=direct
rg -n "replayResolvedSources|parseAndLowerResolvedModule|loadModuleGraphSource" jazz-next/src
```

Expected: tests PASS; `rg` has no output.

- [ ] **Step 8: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleGraph.hs jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/src/JazzNext/Compiler/ModuleReplay.hs jazz-next/src/JazzNext/Compiler/Parser/Lower.hs jazz-next/test/JazzNext/Compiler/Modules jazz-next/jazz-next.cabal
git commit -m "refactor: retain parsed modules in resolved graph"
```

---

### Task 10: Publish compile-time module interfaces

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/Prelude.hs`
- Create: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Create: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/{State,Scope,Capabilities}.hs`
- Modify: module contract/loader tests and `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: `ResolvedProgram`, parse-once `PreparedPrelude`, warning settings, builtin mode.
- Produces: `ModuleInterface`, `CompiledPrelude`, `CompiledModule`, `CompiledProgram`, `compilePreparedPrelude`, and `compileResolvedProgram`.

- [ ] **Step 1: Write RED interface-isolation tests**

Add tests that compile a two-module graph and inspect the internal interfaces:

```haskell
testCompiledInterfacesExposeOnlyDeclaredExports :: IO ()
testCompiledInterfacesExposeOnlyDeclaredExports = do
  resolved <- resolveFixtureProgram sources
  compiled <- compileResolvedProgram (emptyCompileInputs defaultWarningSettings) resolved
  case lookupCompiledModule ["Lib", "Value"] compiled of
    Nothing -> failTest "missing compiled Lib::Value module"
    Just compiledModule ->
      assertEqual
        "exported values"
        (Set.fromList ["answer"])
        (Map.keysSet (interfaceValueTypes (compiledModuleInterface compiledModule)))
  assertEqual "no compile errors" [] (compiledProgramErrors compiled)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]

    resolveFixtureProgram sourceMap = do
      result <- resolveProgram resolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
      case result of
        Left diagnostic -> failTest ("resolution failed: " <> renderDiagnostic diagnostic)
        Right program -> pure program
      where
        lookupSource path = pure (Map.lookup path sourceMap)
```

Also retain the module-pipeline contract test for transitive visibility.

- [ ] **Step 2: Run the tests to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec loader-spec --test-show-details=direct
```

Expected: FAIL because module-interface APIs do not exist.

- [ ] **Step 3: Move prelude preparation behind an explicit boundary**

Move `ResolvedPrelude`, `LoweredResolvedPrelude`, `lowerResolvedPrelude`, visible-name collection, and builtin-mode selection from `Driver.hs` to `Prelude.hs`. Replace the lowered wrapper with:

```haskell
data PreparedPrelude = PreparedPrelude
  { preparedPreludeExpr :: Maybe Expr,
    preparedPreludeHiddenStatementIndices :: Set Int,
    preparedPreludeVisibleValues :: Set Name,
    preparedPreludeVisibleClasses :: Set Name,
    preparedPreludeBuiltinMode :: BuiltinResolutionMode
  }

preparePrelude :: ResolvedPrelude -> Either Diagnostic PreparedPrelude
```

`PreludeAbsent` produces `Nothing` plus empty sets. Bundled and explicit preludes are parsed and lowered once; bundled statement indices are hidden and explicit indices are visible. Re-export `ResolvedPrelude` from `Driver` until the driver cutover so existing callers continue to compile.

- [ ] **Step 4: Add interface and compiled-program records**

```haskell
data ModuleInterface = ModuleInterface
  { interfaceValueTypes :: Map Text TypeBinding,
    interfaceDataTypes :: Map Text DataTypeBinding,
    interfaceClassFacts :: Map Text Int,
    interfaceConcreteImplFacts :: Set Text,
    interfaceClassMethods :: Map Text ClassMethodType,
    interfaceConcreteImplMethods :: Map Text [ImplMethodType],
    interfaceRuntimeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
  }

emptyModuleInterface :: ModuleInterface

data CompiledPrelude = CompiledPrelude
  { compiledPreludeBuiltinMode :: BuiltinResolutionMode,
    compiledPreludeInterface :: ModuleInterface,
    compiledPreludeWarnings :: [WarningRecord],
    compiledPreludeErrors :: [Diagnostic],
    compiledPreludeExpr :: Maybe Expr,
    compiledPreludeRuntimeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
  }

emptyCompiledPrelude :: CompiledPrelude

data CompiledModule = CompiledModule
  { compiledResolvedModule :: ResolvedModule,
    compiledModuleInterface :: ModuleInterface,
    compiledModuleWarnings :: [WarningRecord],
    compiledModuleErrors :: [Diagnostic],
    compiledModuleExpr :: Expr
  }

data CompiledProgram = CompiledProgram
  { compiledProgramPrelude :: CompiledPrelude,
    compiledProgramEntryPath :: [Text],
    compiledProgramModules :: [CompiledModule],
    compiledProgramWarnings :: [WarningRecord],
    compiledProgramErrors :: [Diagnostic]
  }

data CompileInputs = CompileInputs
  { compileInputWarningSettings :: WarningSettings,
    compileInputBuiltinMode :: BuiltinResolutionMode,
    compileInputPrelude :: CompiledPrelude
  }

emptyCompileInputs :: WarningSettings -> CompileInputs
compileInputs :: WarningSettings -> CompiledPrelude -> CompileInputs
lookupCompiledModule :: [Text] -> CompiledProgram -> Maybe CompiledModule
```

`emptyCompileInputs` uses `ResolveKernelOnly` and `emptyCompiledPrelude`. `compileInputs` derives its builtin mode from `compiledPreludeBuiltinMode`.

- [ ] **Step 5: Add explicit analyzer/inference inputs**

Define:

```haskell
data AnalysisBinding = AnalysisBinding
  { analysisBindingSpan :: Maybe SourceSpan,
    analysisBindingIsHiddenPrelude :: Bool
  }

data AnalysisInputs = AnalysisInputs
  { analysisBuiltinMode :: BuiltinResolutionMode,
    analysisWarningSettings :: WarningSettings,
    analysisImportedValues :: Map Name AnalysisBinding,
    analysisImportedClasses :: Set Name,
    analysisModulePath :: Maybe [Text]
  }

data InferenceInputs = InferenceInputs
  { inferenceBuiltinMode :: BuiltinResolutionMode,
    inferenceWarningSettings :: WarningSettings,
    inferenceImportedTypes :: TypeEnv,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
    inferenceCurrentModulePath :: Maybe [Text]
  }
```

Keep standalone entrypoints by constructing empty/default inputs.

- [ ] **Step 6: Publish inferred exports**

Extend `InferenceResult` with:

```haskell
inferredModuleInterface :: ModuleInterface
```

Build the interface from final top-level binding schemes, constructor/data facts, class/impl facts, and runtime hints. Export only names declared by that module; imported names never become exports.

- [ ] **Step 7: Compile the prelude and modules exactly once**

Implement:

```haskell
compilePreparedPrelude :: WarningSettings -> PreparedPrelude -> IO CompiledPrelude
compileResolvedProgram :: CompileInputs -> ResolvedProgram -> IO CompiledProgram
```

`compilePreparedPrelude` first rewrites prelude binders/references to `ResolvedName AmbientPrelude` and recognized kernel references to `BuiltinName`, then analyzes the expression once. It retains the canonical expression, interface, raw warnings, raw errors, and runtime hints. Unknown names remain ambient-origin names so existing E1001 inference behavior is preserved. It returns `emptyCompiledPrelude` for `PreludeAbsent` except for the prepared builtin mode.

Fold over `resolvedProgramModules`. For each module, derive its structured imported-name type environment from `ResolvedImport` values and already-compiled dependency interfaces. Compile every module even when earlier diagnostics exist when sufficient partial facts are available, so deterministic diagnostic accumulation remains dependency ordered. Runtime remains disabled whenever `compiledProgramErrors` is non-empty.

Merge `compileInputPrelude` into every module's ambient inputs without treating it as a source import. Store that same value in `compiledProgramPrelude`; do not parse, infer, or canonicalize it again. Aggregate raw prelude warnings/errors first, then dependency modules, then the entry module. Warning promotion remains a driver concern so its existing ordering can be preserved.

- [ ] **Step 8: Verify compile-time module suites**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec loader-spec module-resolution-spec binding-signature-coherence-spec adt-pattern-type-spec primitive-semantics-spec --test-show-details=direct
```

Expected: PASS while the driver may still use replay.

- [ ] **Step 9: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Prelude.hs jazz-next/src/JazzNext/Compiler/ModuleInterface.hs jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/test/JazzNext/Compiler/Modules jazz-next/jazz-next.cabal
git commit -m "feat: compile modules against explicit interfaces"
```

---

### Task 11: Evaluate modules against runtime exports

**Files:**
- Create: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Modify: module contract/loader/runtime tests and `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: a successful `CompiledProgram`, including its already-compiled prelude.
- Produces: `RuntimeModule`, `RuntimeProgram`, and `evaluateCompiledProgram`.

- [ ] **Step 1: Write RED runtime-module tests**

Add tests that directly evaluate compiled modules and inspect export isolation:

```haskell
testRuntimeModulePublishesDeclaredExports :: IO ()
testRuntimeModulePublishesDeclaredExports = do
  compiled <- compileFixtureProgram sources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> failTest ("runtime program failed: " <> renderDiagnostic diagnostic)
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> failTest "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "export names"
            (Set.fromList ["answer"])
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]

testDependencyTerminalExpressionIsSkipped :: IO ()
testDependencyTerminalExpressionIsSkipped = do
  result <- runFixtureGraph dependencyExpressionSources
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "entry output" (Just "1") (runOutput result)
```

- [ ] **Step 2: Run the tests to verify RED**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec loader-spec --test-show-details=direct
```

Expected: FAIL because `RuntimeModule` and `evaluateCompiledProgram` do not exist.

- [ ] **Step 3: Return runtime environments from scope evaluation**

Introduce:

```haskell
data ScopeResult = ScopeResult
  { scopeResultEnvironment :: RuntimeEnv,
    scopeResultValue :: Maybe RuntimeValue
  }

data ModuleEvaluationMode
  = EvaluateDependencyModule
  | EvaluateEntryModule
```

Refactor `evalScopeWithModulePath` into an internal function returning `ScopeResult`. Existing standalone evaluators project `scopeResultValue`.

In dependency mode, skip runtime evaluation of `SExpr` statements; their semantic validation has already happened during compilation. In entry mode, preserve the existing terminal-expression behavior.

- [ ] **Step 4: Add runtime module records**

```haskell
data RuntimeModule = RuntimeModule
  { runtimeModulePath :: [Text],
    runtimeModuleExports :: Map Text RuntimeCell
  }

data RuntimeProgram = RuntimeProgram
  { runtimeProgramModules :: [RuntimeModule],
    runtimeProgramOutput :: Maybe RuntimeValue
  }

lookupRuntimeModule :: [Text] -> RuntimeProgram -> Maybe RuntimeModule
```

- [ ] **Step 5: Construct structured runtime imports**

For every resolved import, insert selected dependency cells under:

- `ResolvedName (ImportedModule dependencyPath) namespace name` keys for every imported reference, whether the source spelling was unqualified or alias-qualified.

Do not copy unselected exports. The resolver has already assigned source references to local, imported, ambient, or builtin origins, so runtime lookup does not need a second alias/unqualified compatibility map. Preserve the current local-rebinding outcome during resolution. Closures capture the dependency module's structured environment, so hidden dependencies remain available inside exported closures without becoming importer-visible.

- [ ] **Step 6: Evaluate the program in dependency order**

Implement:

```haskell
evaluateCompiledProgram :: CompiledProgram -> Either Diagnostic RuntimeProgram
```

Evaluate `compiledProgramPrelude` once to construct the ambient runtime environment. Then evaluate dependencies once, publish their declared exports, evaluate the entry module, and retain only its terminal output. The runtime must consume the canonical compiled prelude expression and hints; it must not parse, infer, or canonicalize prelude source again.

- [ ] **Step 7: Verify runtime and loader suites**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec loader-spec runtime-semantics-spec adt-pattern-runtime-spec lambda-semantics-spec primitive-semantics-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs jazz-next/src/JazzNext/Compiler/ModuleInterface.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Modules jazz-next/test/JazzNext/Compiler/Semantics jazz-next/jazz-next.cabal
git commit -m "feat: evaluate modules against runtime exports"
```

---

### Task 12: Cut the driver over and delete module replay

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: module tests and `jazz-next/jazz-next.cabal`
- Delete: `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs`
- Delete: temporary replay compatibility helpers from `Name`, `Runtime`, and `TypeInference`

**Interfaces:**
- Consumes: `resolveProgram`, `compileResolvedProgram`, `evaluateCompiledProgram`.
- Produces: unchanged public `compileModuleGraph*` and `runModuleGraph*` results with no replay implementation.

- [ ] **Step 1: Add a driver cutover test**

Add a structural assertion to `test-warning-config.sh`:

```bash
if rg -n 'ModuleReplay|moduleGraphValidationExpr|moduleGraphRuntimeExpr|__module::' jazz-next/src jazz-next/jazz-next.cabal; then
  echo "module replay architecture is still present" >&2
  exit 1
fi
```

- [ ] **Step 2: Run the structural test to verify RED**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: FAIL and print the replay references.

- [ ] **Step 3: Build one compiled program for both entrypoints**

Add one private helper so compile and run entrypoints cannot drift or compile the graph twice:

```haskell
buildCompiledProgram ::
  WarningSettings ->
  ResolvedPrelude ->
  ModuleResolutionConfig ->
  [Text] ->
  (FilePath -> IO (Maybe Text)) ->
  IO (Either Diagnostic CompiledProgram)
buildCompiledProgram settings resolvedPrelude config entryPath sourceLookup =
  case preparePrelude resolvedPrelude of
    Left diagnostic -> pure (Left diagnostic)
    Right preparedPrelude -> do
      resolvedResult <-
        resolveProgram
          config
          (preparedPreludeBuiltinMode preparedPrelude)
          (preparedPreludeVisibleValues preparedPrelude)
          (preparedPreludeVisibleClasses preparedPrelude)
          sourceLookup
          entryPath
      case resolvedResult of
        Left diagnostic -> pure (Left diagnostic)
        Right resolvedProgram -> do
          compiledPrelude <- compilePreparedPrelude settings preparedPrelude
          Right
            <$> compileResolvedProgram
              (compileInputs settings compiledPrelude)
              resolvedProgram
```

This preserves the existing phase order: prelude parse/lower errors precede resolution, resolution errors prevent semantic compilation, and prelude semantic diagnostics are collected only after successful resolution.

- [ ] **Step 4: Replace compile flow and preserve warning order**

Call `buildCompiledProgram` once. A `Left` becomes `CompileResult [] [diagnostic]`. For a compiled program, preserve the current ordering exactly:

```haskell
let warnings =
      filterWarningsForPromotion
        settings
        (compiledProgramWarnings compiledProgram)
    promotedWarningErrors =
      map warningToError (filter (isPromoted settings) warnings)
    errors = compiledProgramErrors compiledProgram ++ promotedWarningErrors
```

Return `warnings` and `errors`; do not promote warnings inside `ModuleCompiler`.

- [ ] **Step 5: Replace run flow**

Compile once. If compile errors are non-empty, return no runtime errors or output. Otherwise call `evaluateCompiledProgram`, render only the entry output, and retain existing `RunResult` separation.

Delete `runExprWithValidationAndRuntimeExprs` and all dual validation/runtime AST handling.

- [ ] **Step 6: Delete replay**

Delete `ModuleReplay.hs`, its Cabal entry, `ModuleGraphExpr`, source replay, export-closure analysis, alias bridge bindings, replay pruning, hidden capability rewriting, replay-specific operator helpers, and all temporary replay comparison code.

Remove `ModuleReplayBridge` from `GeneratedNameKind` and any storage rendering used only by replay.

- [ ] **Step 7: Run all module and CLI suites**

Run:

```bash
cabal test --project-dir=jazz-next module-pipeline-contract-spec loader-spec module-resolution-spec prelude-loading-spec cli-spec --test-show-details=direct
```

Expected: PASS using only the new pipeline.

- [ ] **Step 8: Run structural checks**

Run:

```bash
rg -n 'ModuleReplay|moduleGraphValidationExpr|moduleGraphRuntimeExpr|__module::|ModuleReplayBridge' jazz-next/src jazz-next/jazz-next.cabal
```

Expected: no output.

- [ ] **Step 9: Commit**

```bash
git add jazz-next/src jazz-next/test jazz-next/scripts/test-warning-config.sh jazz-next/jazz-next.cabal
git commit -m "refactor: replace module replay with module pipeline"
```

---

### Task 13: Make `Compiler.Name` the sole identifier owner

**Files:**
- Modify: all `jazz-next/src/**/*.hs` imports of `JazzNext.Compiler.Identifier`
- Modify: affected tests
- Delete: `jazz-next/src/JazzNext/Compiler/Identifier.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: final `Compiler.Name` API.
- Produces: one owner for source identifiers and structured core names.

- [ ] **Step 1: Move source identifier definitions into `Compiler.Name`**

Move `Identifier`, `identifierText`, `identifierPurity`, and `mkIdentifier` into `Name.hs`. Keep source operator spelling helpers only if the parser still requires them; compiler-generated operator bindings must remain `GeneratedName` values.

- [ ] **Step 2: Update imports and verify RED structural check**

Add to `test-warning-config.sh`:

```bash
if rg -n 'JazzNext\.Compiler\.Identifier' jazz-next/src jazz-next/test jazz-next/jazz-next.cabal; then
  echo "legacy Identifier module is still referenced" >&2
  exit 1
fi
```

Run it before deletion and confirm failure.

- [ ] **Step 3: Delete the façade and run semantic suites**

Run:

```bash
cabal test --project-dir=jazz-next name-semantics-spec lambda-semantics-spec primitive-semantics-spec loader-spec --test-show-details=direct
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add jazz-next/src jazz-next/test jazz-next/scripts/test-warning-config.sh jazz-next/jazz-next.cabal
git commit -m "refactor: make compiler name module authoritative"
```

---

### Task 14: Make the compiler library private

**Files:**
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/README.md`

**Interfaces:**
- Consumes: final production/test module inventory.
- Produces: private `jazz-next-internal` library used only by package components.

- [ ] **Step 1: Add a package-boundary check**

Add to `test-warning-config.sh`:

```bash
if rg -n '^library$' jazz-next/jazz-next.cabal; then
  echo "public compiler library remains exposed" >&2
  exit 1
fi
```

The anchored check permits `library jazz-next-internal` and its internal `exposed-modules` field; the failure condition is specifically an unnamed public library stanza.

- [ ] **Step 2: Run the check to verify RED**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: FAIL on the unnamed `library` stanza.

- [ ] **Step 3: Convert to a private named library**

Change the stanza header and dependencies:

```cabal
library jazz-next-internal
  visibility: private
  hs-source-dirs: src
  default-language: Haskell2010
  exposed-modules:
      -- complete internal module inventory

executable jazz-next
  hs-source-dirs: app
  main-is: Main.hs
  default-language: Haskell2010
  build-depends:
      base >= 4.17 && < 4.18,
      jazz-next:jazz-next-internal
```

Change `test-common` to depend on `jazz-next:jazz-next-internal`.

- [ ] **Step 4: Update README boundary wording**

State that `jazz-next` is currently a CLI/compiler package with a private Haskell implementation library and no supported embedding API.

- [ ] **Step 5: Verify package configuration**

Run:

```bash
cd jazz-next
cabal check
cabal build all
cabal test all --test-show-details=never
```

Expected: `cabal check` reports no errors/warnings; build and tests exit 0.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/jazz-next.cabal jazz-next/README.md jazz-next/scripts/test-warning-config.sh
git commit -m "build: keep jazz-next compiler internals private"
```

---

### Task 15: Split oversized test modules by concern

**Files:**
- Create: support modules listed in the Planned File Structure section
- Modify: `LoaderSpec.hs`, `RuntimeSemanticsSpec.hs`, `BindingSignatureCoherenceSpec.hs`, `AdtPatternParserSpec.hs`, and `ParserFoundationSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: existing `NamedTest` values and suite names.
- Produces: small suite entrypoints aggregating `NamedTest` groups; no behavioral changes and no new framework.

- [ ] **Step 1: Record the pre-split test inventory**

Run each affected suite with direct output and save the PASS-line count in the plan execution notes:

```bash
cabal test --project-dir=jazz-next loader-spec runtime-semantics-spec binding-signature-coherence-spec adt-pattern-parser-spec parser-foundation-spec --test-show-details=direct
```

Expected: PASS. The executor must record the number of `PASS:` lines for each suite before moving functions.

- [ ] **Step 2: Define the support-module contract**

Every support module exports one value:

```haskell
module JazzNext.Compiler.Semantics.Runtime.ControlFlowTests
  ( controlFlowTests
  ) where

import JazzNext.TestHarness (NamedTest)

controlFlowTests :: [NamedTest]
controlFlowTests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure)
    -- moved tests in original order
  ]
```

Keep test functions, fixtures, and imports with the group that owns them. Put genuinely shared helpers in a narrowly named sibling support module, not the suite `Main`.

- [ ] **Step 3: Split loader tests**

Move tests into `BasicTests`, `VisibilityTests`, `CapabilitiesTests`, `OperatorsTests`, and `DiagnosticsTests`. Preserve the original aggregate order in:

```haskell
tests = basicTests ++ visibilityTests ++ capabilitiesTests ++ operatorTests ++ diagnosticTests
```

- [ ] **Step 4: Split runtime and binding-signature tests**

Use the groups listed in Planned File Structure. Preserve names and order.

- [ ] **Step 5: Split parser tests**

Split `AdtPatternParserSpec` and `ParserFoundationSpec` into the listed support groups. Keep the Cabal suite names and `Main` paths unchanged.

- [ ] **Step 6: Register every imported support module**

Add the modules to the relevant test-suite `other-modules` fields or to a shared Cabal common stanza if every suite imports them. Do not expose test support through the compiler library.

- [ ] **Step 7: Verify inventory parity**

Run the five suites again. Expected: every suite exits 0 and has exactly the pre-split `PASS:` count.

Run:

```bash
find jazz-next/test -type f -name '*.hs' -print0 | xargs -0 wc -l | sort -nr | head -20
```

Expected: none of the five original suite `Main` files exceeds 250 lines; supporting modules have cohesive ownership and no copied fixtures.

- [ ] **Step 8: Commit**

```bash
git add jazz-next/test jazz-next/jazz-next.cabal
git commit -m "test: split oversized jazz-next suites"
```

---

### Task 16: Update architecture documentation and run the final audit

**Files:**
- Modify: `jazz-next/README.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/spec/modules/01-file-layout-and-package-roots.md`
- Modify: `docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- Modify: `docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- Modify: `docs/spec/modules/04-qualified-imports-and-binding.md`
- Modify: this plan's checkboxes as tasks complete

**Interfaces:**
- Consumes: final source tree and verified commands.
- Produces: current active-compiler documentation with no replay claims.

- [ ] **Step 1: Update active architecture descriptions**

Document:

- parse-once resolved module graph;
- per-module interfaces and runtime exports;
- structured names;
- canonical `EIf`/`EPatternCase` core;
- parser ownership;
- inference module boundaries;
- private Cabal library; and
- unchanged Jazz module/import semantics.

Remove references to source concatenation, replay bridges, `__module`, validation/runtime replay expressions, and replay pruning.

- [ ] **Step 2: Run structural completion searches**

Run:

```bash
rg -n 'ModuleReplay|moduleGraphValidationExpr|moduleGraphRuntimeExpr|__module::|\bECase\b|JazzNext\.Compiler\.Desugar|PatternParser|Parser \(Either Diagnostic' jazz-next/src jazz-next/test jazz-next/jazz-next.cabal
rg -n '^data (ExpressionType|InferState|TypeScheme)\b' jazz-next/src/JazzNext/Compiler/TypeInference.hs
```

Expected: no output.

- [ ] **Step 3: Run package verification**

Run:

```bash
cd jazz-next
cabal check
cabal build all
cabal test all --test-show-details=never
bash scripts/test-warning-config.sh
```

Expected: all commands exit 0; Cabal reports no errors or warnings.

- [ ] **Step 4: Run packaged CLI smoke tests**

Standalone Unit/lambda:

```bash
printf 'thunk :: () -> Int. thunk = \() -> 42. thunk ().' | cabal run --project-dir=jazz-next jazz-next -- --run -
```

Expected output: `42`.

Module graph: create sources only in a temporary directory and run the packaged CLI with `--module-root` and `--entry-module`. Expected output: the imported entry value, with no replay-specific diagnostics.

- [ ] **Step 5: Run repository documentation checks**

```bash
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
git diff --check
```

Expected: all checks exit 0. A documented prettier skip outside Nix is acceptable only if the script exits 0.

- [ ] **Step 6: Audit protected trees and worktree state**

Run:

```bash
git diff --name-only main...HEAD | rg '^(jazz-hs|jazz2)/'
git status --short
```

Expected: first command has no output; status is clean after the final commit.

- [ ] **Step 7: Commit documentation**

```bash
git add jazz-next/README.md docs/jazz-language-state.md docs/feature-status.md docs/spec/modules docs/superpowers/plans/2026-07-09-jazz-next-compiler-architecture-remediation.md
git commit -m "docs: describe jazz-next module pipeline architecture"
```

- [ ] **Step 8: Re-run the final verification after the commit**

Repeat Steps 2-6 against committed `HEAD`. Do not declare completion from pre-commit results.
