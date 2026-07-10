---
id: JN-MODULE-TYPED-EXPORT-INVENTORY-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-07-09
plan_section: "Implementation Batch: Typed Module Export Inventory"
target_paths:
  - jazz-next/src/JazzNext/Compiler/ModuleInterface.hs
  - jazz-next/src/JazzNext/Compiler/ModuleResolver.hs
  - jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs
  - jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs
  - jazz-next/jazz-next.cabal
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - jazz-next/scripts/test-warning-config.sh
  - docs/spec/modules/04-qualified-imports-and-binding.md
  - docs/feature-status.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - docs/superpowers/specs/2026-07-09-jazz-next-typed-module-export-inventory-design.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Introduce a shared typed module export inventory used by resolver, compiler interface, and runtime export selection, preserve existing value/constructor/type/class import behavior and E4007-E4014 diagnostics, and lock explicit class capability imports without adding syntax, re-exports, or new impl policy."
---

# Jazz-Next Typed Module Export Inventory Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** replace parallel module export bookkeeping with one namespace-aware
inventory shared by resolution, compiled-interface selection, and runtime
export selection while preserving current import behavior and diagnostics.

**Architecture:** add an opaque `ModuleExportInventory` beside the existing
`ModuleExport` record in a new focused module. `ModuleResolver` derives one
inventory from surface declarations and uses it for name visibility and import
validation; `ModuleInterface` derives the same inventory shape from compiled
payloads, and `ModuleCompiler` uses shared selection helpers to filter imports.
The inventory is derived at each boundary instead of stored as a second mutable
manifest.

**Tech Stack:** Haskell 2010, `containers` `Map`/`Set`, active `jazz-next`
parser/resolver/compiler/runtime pipeline, `runghc` test suites, Cabal package
metadata, Markdown module specifications, and repository queue/docs validators.

## Global Constraints

- Modify active compiler code only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only.
- Preserve bare, explicit symbol-list, and alias import syntax.
- Preserve `E4007` through `E4014` codes, subjects, paths, and span metadata.
- Keep type-only names ineligible as explicit import selectors.
- Keep alias imports capability-hidden and do not add `Alias::Class::method`.
- Keep imported classes non-transitive at public module boundaries.
- Keep impl facts and method bodies attached to their selected class; do not add
  separately selectable impl exports.
- Do not add export declarations, re-exports, default methods, superclasses,
  orphan/overlap policy, dictionaries, or dictionary optimization.
- Every task ends with focused verification and an intentional commit.

---

## Implementation Batch: Typed Module Export Inventory

The batch is split into four reviewable tasks. Task 1 establishes and tests the
shared API. Task 2 migrates compiled interfaces. Task 3 migrates the resolver
and removes the parallel map architecture. Task 4 aligns the public contract and
closes the queue row after full verification.

### Task 1: Shared Typed Export Inventory

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/scripts/test-warning-config.sh`

**Interfaces:**

- Consumes: `JazzNext.Compiler.Name.NameNamespace` and `Data.Set`.
- Produces: `ModuleExport`, opaque `ModuleExportInventory`,
  `ModuleImportMode`, `exportInventory`, `exportInventoryEntries`,
  `exportNamesInNamespace`, `exportNamesInNamespaces`,
  `selectorEligibleNames`, `selectExportNames`, `visibleImportInventory`,
  `inventoryHasExport`, and `firstExportNamespace`.

- [ ] **Step 1: Add the focused inventory test before the module exists**

Create `ModuleExportsSpec.hs` with this complete test program:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleImportMode (..),
    exportInventory,
    exportInventoryEntries,
    firstExportNamespace,
    selectExportNames,
    selectorEligibleNames,
    visibleImportInventory
  )
import JazzNext.Compiler.Name (NameNamespace (..))
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "ModuleExports" tests

tests :: [NamedTest]
tests =
  [ ("preserves same-text exports across namespaces", testPreservesNamespaces),
    ("excludes type-only names from selector eligibility", testSelectorEligibility),
    ("selects every same-text namespace entry", testSelectsSameTextEntries),
    ("filters alias imports to values and constructors", testAliasVisibility),
    ("keeps all namespaces for unqualified imports", testUnqualifiedVisibility),
    ("finds the first requested namespace deterministically", testFirstNamespace)
  ]

sampleInventory =
  exportInventory
    [ ModuleExport ValueNamespace "answer",
      ModuleExport ConstructorNamespace "Box",
      ModuleExport TypeNamespace "Box",
      ModuleExport TypeNamespace "HiddenType",
      ModuleExport CapabilityNamespace "Eq"
    ]

testPreservesNamespaces :: IO ()
testPreservesNamespaces =
  assertEqual
    "same-text entries"
    ( Set.fromList
        [ ModuleExport ConstructorNamespace "Box",
          ModuleExport TypeNamespace "Box"
        ]
    )
    ( Set.filter
        ((== "Box") . moduleExportName)
        (exportInventoryEntries sampleInventory)
    )

testSelectorEligibility :: IO ()
testSelectorEligibility =
  assertEqual
    "selector names"
    (Set.fromList ["answer", "Box", "Eq"])
    (selectorEligibleNames sampleInventory)

testSelectsSameTextEntries :: IO ()
testSelectsSameTextEntries =
  assertEqual
    "selected entries"
    ( Set.fromList
        [ ModuleExport ConstructorNamespace "Box",
          ModuleExport TypeNamespace "Box"
        ]
    )
    (exportInventoryEntries (selectExportNames (Just ["Box"]) sampleInventory))

testAliasVisibility :: IO ()
testAliasVisibility =
  assertEqual
    "alias entries"
    ( Set.fromList
        [ ModuleExport ValueNamespace "answer",
          ModuleExport ConstructorNamespace "Box"
        ]
    )
    ( exportInventoryEntries
        (visibleImportInventory QualifiedAliasImport Nothing sampleInventory)
    )

testUnqualifiedVisibility :: IO ()
testUnqualifiedVisibility =
  assertEqual
    "unqualified entries"
    (exportInventoryEntries sampleInventory)
    ( exportInventoryEntries
        (visibleImportInventory UnqualifiedImport Nothing sampleInventory)
    )

testFirstNamespace :: IO ()
testFirstNamespace =
  assertEqual
    "namespace precedence"
    (Just ConstructorNamespace)
    ( firstExportNamespace
        [ValueNamespace, ConstructorNamespace, CapabilityNamespace]
        "Box"
        sampleInventory
    )
```

- [ ] **Step 2: Run the test and verify the API is absent**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
```

Expected: compilation fails because
`JazzNext.Compiler.ModuleExports` does not exist.

- [ ] **Step 3: Implement the complete shared inventory API**

Create `ModuleExports.hs` with this implementation:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Shared typed inventory for source and compiled module exports.
module JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    selectorEligibleNames,
    selectExportNames,
    visibleImportInventory,
    inventoryHasExport,
    firstExportNamespace
  )
where

import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Name (NameNamespace (..))

data ModuleExport = ModuleExport
  { moduleExportNamespace :: NameNamespace,
    moduleExportName :: Text
  }
  deriving (Eq, Ord, Show)

newtype ModuleExportInventory = ModuleExportInventory (Set ModuleExport)
  deriving (Eq, Show)

data ModuleImportMode
  = UnqualifiedImport
  | QualifiedAliasImport
  deriving (Eq, Show)

exportInventory :: [ModuleExport] -> ModuleExportInventory
exportInventory = ModuleExportInventory . Set.fromList

exportInventoryEntries :: ModuleExportInventory -> Set ModuleExport
exportInventoryEntries (ModuleExportInventory entries) = entries

exportNamesInNamespace :: NameNamespace -> ModuleExportInventory -> Set Text
exportNamesInNamespace namespace =
  Set.map moduleExportName
    . Set.filter ((== namespace) . moduleExportNamespace)
    . exportInventoryEntries

exportNamesInNamespaces :: [NameNamespace] -> ModuleExportInventory -> Set Text
exportNamesInNamespaces namespaces inventory =
  Set.unions [exportNamesInNamespace namespace inventory | namespace <- namespaces]

selectorEligibleNames :: ModuleExportInventory -> Set Text
selectorEligibleNames =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace, CapabilityNamespace]

selectExportNames :: Maybe [Text] -> ModuleExportInventory -> ModuleExportInventory
selectExportNames maybeNames inventory =
  case maybeNames of
    Nothing -> inventory
    Just names ->
      let selectedNames = Set.fromList names
       in ModuleExportInventory
            ( Set.filter
                ((`Set.member` selectedNames) . moduleExportName)
                (exportInventoryEntries inventory)
            )

visibleImportInventory ::
  ModuleImportMode ->
  Maybe [Text] ->
  ModuleExportInventory ->
  ModuleExportInventory
visibleImportInventory mode maybeNames inventory =
  case mode of
    UnqualifiedImport -> selected
    QualifiedAliasImport ->
      ModuleExportInventory
        ( Set.filter
            ( (`elem` [ValueNamespace, ConstructorNamespace])
                . moduleExportNamespace
            )
            (exportInventoryEntries selected)
        )
  where
    selected = selectExportNames maybeNames inventory

inventoryHasExport :: ModuleExport -> ModuleExportInventory -> Bool
inventoryHasExport export = Set.member export . exportInventoryEntries

firstExportNamespace ::
  [NameNamespace] ->
  Text ->
  ModuleExportInventory ->
  Maybe NameNamespace
firstExportNamespace namespaces name inventory =
  find
    (\namespace -> inventoryHasExport (ModuleExport namespace name) inventory)
    namespaces
```

Register `JazzNext.Compiler.ModuleExports` beside `ModuleGraph` and
`ModuleInterface` in the library `exposed-modules`. Add this test suite beside
`module-resolution-spec`:

```cabal
test-suite module-exports-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Modules/ModuleExportsSpec.hs
```

Add the new test file immediately before `ModuleResolutionSpec.hs` in the
`TEST_FILES` array in `jazz-next/scripts/test-warning-config.sh`.

- [ ] **Step 4: Run the inventory suite and verify it passes**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
```

Expected: all six `ModuleExports` tests pass.

- [ ] **Step 5: Commit the shared API and focused test**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleExports.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs jazz-next/jazz-next.cabal jazz-next/scripts/test-warning-config.sh
git commit -m "refactor: add typed module export inventory"
```

### Task 2: Compiled Interface Selection

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify:
  `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`

**Interfaces:**

- Consumes: Task 1's `ModuleExportInventory`, `visibleImportInventory`,
  `inventoryHasExport`, and namespace queries.
- Produces: `moduleInterfaceExportInventory :: ModuleInterface ->
  ModuleExportInventory`; `ModuleCompiler.importSelectedInterface` delegates
  value and capability selection to the shared inventory; `ModuleRuntime`
  derives ordinary and class-method runtime export visibility from the same
  selected inventory.

- [ ] **Step 1: Add a non-transitive class-export characterization test**

Add this entry to `capabilitiesTests`:

```haskell
, ("compile module graph does not re-export imported classes", testCompileModuleGraphDoesNotReexportImportedClasses)
```

Add the complete test:

```haskell
testCompileModuleGraphDoesNotReexportImportedClasses :: IO ()
testCompileModuleGraphDoesNotReexportImportedClasses = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "non-transitive class code" "E4007" (renderDiagnostic diagnostic)
      assertContains
        "non-transitive class export"
        "import symbol 'Eq' is not exported by module 'Lib::Wrapper'"
        (renderDiagnostic diagnostic)
    diagnostics ->
      failTest
        ( "expected one non-transitive class export diagnostic, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Wrapper (Eq).\nx = 1.\n}"
          ),
          ( "src/Lib/Wrapper.jz",
            "module Lib::Wrapper {\nimport Lib::Facts (Eq).\nwrapper = 0.\n}"
          ),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) { }.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
```

- [ ] **Step 2: Run the loader suite as the pre-refactor characterization**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: the new non-transitive test and all existing capability-import tests
pass before the structural refactor.

- [ ] **Step 3: Move `ModuleExport` and derive interface inventory**

Remove the local `ModuleExport` declaration and export from
`ModuleInterface.hs`, then import it from `JazzNext.Compiler.ModuleExports`.
Add `moduleInterfaceExportInventory` to the module export list and implement it
exactly as follows:

```haskell
moduleInterfaceExportInventory :: ModuleInterface -> ModuleExportInventory
moduleInterfaceExportInventory interface =
  exportInventory
    ( Map.keys (interfaceValueTypes interface)
        <> [ ModuleExport TypeNamespace name
             | name <- Map.keys (interfaceDataTypes interface)
           ]
        <> [ ModuleExport CapabilityNamespace name
             | name <- Map.keys (interfaceClassFacts interface)
           ]
    )
```

Import `ModuleExportInventory` and `exportInventory` from
`JazzNext.Compiler.ModuleExports`. Keep `moduleExportForBinding` in
`ModuleInterface.hs` because it converts inference `TypeBinding` payloads into
interface export keys.

In `ModuleRuntime.hs` and `ModulePipelineContractSpec.hs`, remove
`ModuleExport (..)` from the `ModuleInterface` import and add:

```haskell
import JazzNext.Compiler.ModuleExports (ModuleExport (..))
```

- [ ] **Step 4: Replace compiler-local selection policy with inventory queries**

In `ModuleCompiler.hs`, import:

```haskell
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleImportMode (..),
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory
  )
```

Import `moduleInterfaceExportInventory` from `ModuleInterface`. Replace the
`selected`, `includeCapabilities`, and `selectedClassFacts` policy with:

```haskell
    importMode =
      case maybeAlias of
        Nothing -> UnqualifiedImport
        Just _ -> QualifiedAliasImport
    selectedInventory =
      visibleImportInventory
        importMode
        maybeSymbols
        (moduleInterfaceExportInventory moduleInterface)
    selectedValueTypes =
      Map.filterWithKey
        (\export _ -> inventoryHasExport export selectedInventory)
        (interfaceValueTypes moduleInterface)
    selectedClassNames =
      exportNamesInNamespace CapabilityNamespace selectedInventory
    selectedClassFacts =
      Map.restrictKeys
        (interfaceClassFacts moduleInterface)
        selectedClassNames
```

Leave `importedDataTypes` populated under qualified internal keys exactly as it
is today. Keep `selectedCapabilities`, `factUsesClass`, and `methodUsesClass`,
but feed them only `selectedClassNames` from the inventory.

- [ ] **Step 5: Route runtime export selection through the inventory**

In `ModuleRuntime.hs`, import `qualified Data.Set as Set`, import
`moduleInterfaceExportInventory`, and extend the `ModuleExports` import to:

```haskell
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleImportMode (..),
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory
  )
```

Replace `interfaceExports` with:

```haskell
interfaceExports :: ModuleInterface -> [ModuleExport]
interfaceExports moduleInterface =
  [ export
    | export <-
        Set.toList
          (exportInventoryEntries (moduleInterfaceExportInventory moduleInterface)),
      moduleExportNamespace export `elem` [ValueNamespace, ConstructorNamespace]
  ]
    <> map (ModuleExport ValueNamespace) (Map.keys (interfaceClassMethods moduleInterface))
```

Replace raw symbol-list checks in `runtimeExportSelected` with:

```haskell
runtimeExportSelected :: ResolvedImport -> ModuleInterface -> ModuleExport -> Bool
runtimeExportSelected importDecl moduleInterface moduleExport =
  case splitQualifiedMethodKey (moduleExportName moduleExport) of
    Just (className, _) ->
      resolvedImportAlias importDecl == Nothing
        && Set.member className selectedClassNames
    Nothing -> inventoryHasExport moduleExport selectedInventory
  where
    importMode =
      case resolvedImportAlias importDecl of
        Nothing -> UnqualifiedImport
        Just _ -> QualifiedAliasImport
    selectedInventory =
      visibleImportInventory
        importMode
        (resolvedImportSymbols importDecl)
        (moduleInterfaceExportInventory moduleInterface)
    selectedClassNames =
      exportNamesInNamespace CapabilityNamespace selectedInventory
```

This keeps class method runtime cells attached to selected capability exports
without presenting `Class::method` as a separately selectable import symbol.

- [ ] **Step 6: Run focused compiled-interface verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: both suites pass, including explicit class method dispatch, hidden
impl isolation, imported class origins, alias-hidden classes, and the new
non-transitive export test.

- [ ] **Step 7: Commit compiled-interface migration**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleInterface.hs jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
git commit -m "refactor: select module interfaces through export inventory"
```

### Task 3: Resolver Inventory Migration

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: `jazz-next/scripts/test-warning-config.sh`

**Interfaces:**

- Consumes: the shared inventory API from Task 1.
- Produces: one `parsedModuleInventory` per source module and one
  `resolvedExportInventoriesState` map for the graph; import validation and
  core-name resolution query these inventories.

- [ ] **Step 1: Add resolver characterization cases**

Add these entries to the `tests` list in `ModuleResolutionSpec.hs`:

```haskell
, ("accepts explicit class import symbols", testAcceptsExplicitClassImportSymbol)
, ("rejects type-only explicit import symbols", testRejectsTypeOnlyImportSymbol)
, ("reports class import collisions", testReportsClassImportCollision)
, ("keeps repeated class imports idempotent", testKeepsRepeatedClassImportsIdempotent)
```

Add this shared test configuration near the new tests:

```haskell
testResolverConfig :: ModuleResolutionConfig
testResolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }
```

Then add these complete tests:

```haskell
testAcceptsExplicitClassImportSymbol :: IO ()
testAcceptsExplicitClassImportSymbol = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "explicit class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Facts (Eq).\nx :: @{Eq(Int)}: Int.\nx = 1."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsTypeOnlyImportSymbol :: IO ()
testRejectsTypeOnlyImportSymbol = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "type-only import"
    "E4007"
    "import symbol 'Optional' is not exported"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Types (Optional).\nx = 1."),
          ("src/Lib/Types.jz", "data Optional a = Some a | None.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsClassImportCollision :: IO ()
testReportsClassImportCollision = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "class import collision"
    "E4008"
    "import binding collision for symbol 'Eq'"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import A::Facts.\nimport B::Facts.\nx = 1."),
          ("src/A/Facts.jz", "class Eq(a) { }."),
          ("src/B/Facts.jz", "class Eq(a) { }.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testKeepsRepeatedClassImportsIdempotent :: IO ()
testKeepsRepeatedClassImportsIdempotent = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "repeated class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Facts.\nimport Lib::Facts.\nx :: @{Eq(Int)}: Int.\nx = 1."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.")
        ]
    lookupSource path = pure (Map.lookup path sources)
```

Add `assertLeftDiagnosticCodeAndContains` to the existing `JazzNext.TestHarness`
import list.

- [ ] **Step 2: Run resolver characterization before migration**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: all existing and new resolver behavior tests pass.

- [ ] **Step 3: Add an architecture guard that fails on parallel state fields**

Append this check beside the existing module-architecture guards in
`jazz-next/scripts/test-warning-config.sh`:

```bash
if rg -n 'parsedModule(Exports|ValueNames|DataTypeNames|ConstructorNames|ClassNames)|resolved(ExportsState|ValueExportsState|DataTypeExportsState|ConstructorExportsState|ClassExportsState)' jazz-next/src/JazzNext/Compiler/ModuleResolver.hs; then
  echo "ModuleResolver still carries parallel namespace export inventories" >&2
  exit 1
fi
```

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: failure with `ModuleResolver still carries parallel namespace export
inventories` before the resolver refactor.

- [ ] **Step 4: Replace parsed and resolved parallel maps with one inventory**

Add `import Data.Maybe (fromMaybe)` and this Task 1 API import in
`ModuleResolver.hs`:

```haskell
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventory,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    firstExportNamespace,
    selectorEligibleNames,
    visibleImportInventory
  )
```

Replace the export fields in `ParsedModule` with:

```haskell
    parsedModuleInventory :: ModuleExportInventory,
```

Replace the five resolved export maps in `ResolvedState` with:

```haskell
    resolvedExportInventoriesState :: Map [Text] ModuleExportInventory
```

Initialize that field to `Map.empty`, pass it to import validation and
`resolveCoreModuleNames`, and insert `parsedModuleInventory` after each module
resolves.

Replace the four top-level namespace collectors with this single builder:

```haskell
collectModuleExportInventory :: SurfaceExpr -> ModuleExportInventory
collectModuleExportInventory surfaceExpr =
  exportInventory
    ( case surfaceExpr of
        SEBlock statements -> concatMap statementExports statements
        _ -> []
    )
  where
    statementExports statement =
      case statement of
        SSLet bindingName _ _
          | not (isOperatorBindingIdentifierText (identifierText bindingName)) ->
              [ModuleExport ValueNamespace (identifierText bindingName)]
        SSData _ typeName _ constructors ->
          ModuleExport TypeNamespace (identifierText typeName)
            : [ ModuleExport ConstructorNamespace (identifierText constructorName)
                | SurfaceDataConstructor constructorName _ <- constructors
              ]
        SSClass _ className _ _ ->
          [ModuleExport CapabilityNamespace (identifierText className)]
        _ -> []
```

Derive local sets where needed with `exportNamesInNamespace`. Derive the
top-level binding set used to subtract local references with:

```haskell
Set.union
  (exportNamesInNamespace ValueNamespace inventory)
  (exportNamesInNamespace ConstructorNamespace inventory)
```

- [ ] **Step 5: Route name resolution through inventories**

Change `resolveCoreModuleNames` to receive the current module inventory and
`Map [Text] ModuleExportInventory`. Use this helper for every non-aliased
dependency:

```haskell
visibleDependencyInventory importDecl =
  case Map.lookup (parsedImportModulePath importDecl) inventoriesByModule of
    Nothing -> exportInventory []
    Just inventory ->
      visibleImportInventory
        UnqualifiedImport
        (parsedImportSymbols importDecl)
        inventory
```

Build `visibleValueOrigins`, `visibleConstructorOrigins`,
`visibleTypeOrigins`, and `visibleClassOrigins` from
`exportNamesInNamespace` over that selected inventory. Keep separate origin
maps because each namespace can carry the same text, but remove separate export
inventories and selection policy.

Resolve a dependency member namespace with:

```haskell
fromMaybe fallbackNamespace
  ( firstExportNamespace
      [ValueNamespace, ConstructorNamespace, CapabilityNamespace]
      nameText
      dependencyInventory
  )
```

Use `exportNamesInNamespace` over the current inventory for local-name and
class-origin checks.

- [ ] **Step 6: Route import validation through the same inventory**

Change `validateImportBindings` to accept only
`Map [Text] ModuleExportInventory` for dependency exports. For each import:

```haskell
dependencyInventory importDecl =
  Map.lookup (parsedImportModulePath importDecl) inventoriesByModule

eligibleImportNames inventory = selectorEligibleNames inventory

visibleUnqualifiedInventory importDecl inventory =
  case parsedImportAlias importDecl of
    Just _ -> exportInventory []
    Nothing ->
      visibleImportInventory
        UnqualifiedImport
        (parsedImportSymbols importDecl)
        inventory

aliasMemberNames inventory =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace]
    (visibleImportInventory QualifiedAliasImport Nothing inventory)
```

Use `eligibleImportNames` for `E4007` and `E4008`. Use
`exportNamesInNamespace CapabilityNamespace` for visible class qualifiers. Use
`aliasMemberNames` for `E4014`. Use value-plus-constructor names, not capability
or type names, for existing `E4011` and `E4012` free-value reference checks.
Keep all existing diagnostic constructors and their span/origin data unchanged.

- [ ] **Step 7: Run resolver, loader, and architecture verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Expected: all focused suites and the full warning-config harness pass; the
parallel-state architecture guard prints nothing.

- [ ] **Step 8: Commit resolver migration**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs jazz-next/scripts/test-warning-config.sh
git commit -m "refactor: unify module resolver export inventory"
```

### Task 4: Contract Alignment and Queue Closeout

**Files:**

- Modify: `docs/spec/modules/04-qualified-imports-and-binding.md`
- Modify: `docs/feature-status.md`
- Modify:
  `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Modify:
  `docs/superpowers/specs/2026-07-09-jazz-next-typed-module-export-inventory-design.md`
- Modify: `docs/plans/2026-07-09-jazz-next-typed-module-export-inventory.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`

**Interfaces:**

- Consumes: verified behavior from Tasks 1-3.
- Produces: normative typed export documentation, current feature evidence,
  completed child metadata, and a terminal queue state with no stale candidate.

- [ ] **Step 1: Align the normative import inventory contract**

Replace the current value-only export inventory text in
`04-qualified-imports-and-binding.md` with a namespace table containing these
rows:

| declaration | inventory namespace | selector eligible | imported payload |
| --- | --- | --- | --- |
| top-level binding | value | yes | inferred type binding |
| data constructor | constructor | yes | constructor type binding |
| data declaration name | type | no by itself | data type identity metadata |
| class declaration | capability | yes | class fact, signatures, and matching impl payload |
| impl declaration | none | no | attached to its class capability export |

Document that bare and explicit non-alias imports expose selected classes,
alias imports expose no capabilities, and imports never re-export consumed
classes. Preserve the existing import truth table and add rows for explicit
class selection, hidden class selection, alias-hidden classes, and repeated
class imports.

- [ ] **Step 2: Refresh current architecture evidence**

Update the module/import row in `docs/feature-status.md` to name
`ModuleExports.hs` and `ModuleExportsSpec.hs`, and state that resolver and
compiled-interface selection share a typed export inventory.

Mark the accepted follow-up section in the runtime/module umbrella plan as
complete with the final verification date. Change the design spec status to
`Implemented and verified on 2026-07-09` only after every verification command
below passes.

- [ ] **Step 3: Close the execution metadata atomically**

After product and docs verification passes:

1. Change this plan's frontmatter to `status: done`, add
   `completed_on: 2026-07-09`, and keep `last_verified: 2026-07-09`.
2. Remove `JN-MODULE-TYPED-EXPORT-INVENTORY-001` from `Ready Now`.
3. Add one `done-archive.md` row describing the typed inventory, preserved
   import semantics, focused tests, and exact verification commands.
4. Restore the module blocker contract to `Candidate child: none currently`
   and record the child as landed evidence.
5. Restore the queue's terminal-empty status unless a separate accepted
   contract has appeared during execution.
6. Do not promote default methods, superclasses, alias-qualified classes,
   re-exports, orphan/overlap behavior, or any other follow-up from this child.

- [ ] **Step 4: Run the complete verification ladder**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all focused module suites, the full `jazz-next` harness, queue/docs
validators, and whitespace validation pass.

- [ ] **Step 5: Review the final diff against the design constraints**

Run:

```bash
git diff --stat HEAD~3..HEAD
git diff --check
rg -n 'Alias::[A-Z][A-Za-z0-9_]*::|re-export|default method|superclass' jazz-next/src/JazzNext/Compiler/ModuleExports.hs jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs
```

Expected: the diff is limited to the declared product/test/docs surfaces;
`git diff --check` is silent; the scope scan finds no new alias-qualified class
syntax, re-export implementation, default methods, or superclasses.

- [ ] **Step 6: Commit verified closeout metadata**

```bash
git add docs/spec/modules/04-qualified-imports-and-binding.md docs/feature-status.md docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md docs/superpowers/specs/2026-07-09-jazz-next-typed-module-export-inventory-design.md docs/plans/2026-07-09-jazz-next-typed-module-export-inventory.md docs/execution/blocker-contracts.md docs/execution/queue.md docs/execution/done-archive.md
git commit -m "docs: close typed module export inventory batch"
```
