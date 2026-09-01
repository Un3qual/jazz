{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExport (..),
    ModuleExportInventory,
    ModuleExportSelector (..),
    ModuleImportMode (..),
    ModuleTypeConstructorSelector (..),
    declarationExportNames,
    exportInventory,
    exportInventoryEntries,
    exportedConstructorOwners,
    firstExportNamespace,
    renderModuleExportSelector,
    selectExportNames,
    selectModuleExportSelectors,
    selectValidatedModuleExportSelectors,
    selectorEligibleNames,
    visibleImportInventory,
  )
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleInterfaceExportInventory,
  )
import Jazz.Compiler.Name (NameNamespace (..))
import Jazz.Compiler.TypeInference.Types
  ( SemanticType (..),
    TypeBinding (PlainTypeBinding),
  )
import Jazz.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "ModuleExports" tests

tests :: [NamedTest]
tests =
  [ ("preserves same-text exports across namespaces", testPreservesNamespaces),
    ("lists every declaration namespace as module export names", testDeclarationExportNames),
    ("excludes type-only names from selector eligibility", testSelectorEligibility),
    ("selects every same-text namespace entry", testSelectsSameTextEntries),
    ("selects exact module export namespaces", testSelectsExactModuleExportNamespaces),
    ("keeps same-text entries for bare module export selectors", testBareModuleExportSelectorKeepsSameTextEntries),
    ("renders grouped type export selectors", testRendersGroupedTypeExportSelectors),
    ("retains constructor ownership for grouped type selectors", testGroupedTypeSelectorOwnership),
    ("distinguishes standalone constructor selectors from owned selectors", testStandaloneConstructorSelectorOwnership),
    ("drops constructor ownership when filtering its constructor or type", testFilteredConstructorOwnership),
    ("combines conflicting constructor owners without bias", testConflictingConstructorOwnership),
    ("filters alias imports to values, constructors, and types", testAliasVisibility),
    ("keeps all namespaces for unqualified imports", testUnqualifiedVisibility),
    ("finds the first requested namespace deterministically", testFirstNamespace),
    ("derives compiled interface exports by namespace", testInterfaceInventory)
  ]

sampleInventory :: ModuleExportInventory
sampleInventory =
  exportInventory
    [ ModuleExport ValueNamespace "answer",
      ModuleExport ConstructorNamespace "Box",
      ModuleExport TypeNamespace "Box",
      ModuleExport TypeNamespace "HiddenType",
      ModuleExport CapabilityNamespace "Eq"
    ]

testDeclarationExportNames :: IO ()
testDeclarationExportNames =
  assertEqual
    "declaration export names include types"
    (Set.fromList ["answer", "Box", "HiddenType", "Eq"])
    (declarationExportNames sampleInventory)

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

testSelectsExactModuleExportNamespaces :: IO ()
testSelectsExactModuleExportNamespaces =
  assertEqual
    "exact typed exports"
    ( Set.fromList
        [ ModuleExport ValueNamespace "Shared",
          ModuleExport TypeNamespace "Shared",
          ModuleExport CapabilityNamespace "Shared"
        ]
    )
    ( exportInventoryEntries
        ( selectModuleExportSelectors
            [ ModuleExportSelector (Just ValueNamespace) "Shared",
              ModuleExportSelector (Just TypeNamespace) "Shared",
              ModuleExportSelector (Just CapabilityNamespace) "Shared"
            ]
            sharedInventory
        )
    )
  where
    sharedInventory =
      exportInventory
        [ ModuleExport ValueNamespace "Shared",
          ModuleExport ConstructorNamespace "Shared",
          ModuleExport TypeNamespace "Shared",
          ModuleExport CapabilityNamespace "Shared"
        ]

testBareModuleExportSelectorKeepsSameTextEntries :: IO ()
testBareModuleExportSelectorKeepsSameTextEntries =
  assertEqual
    "bare module export selector"
    ( Set.fromList
        [ ModuleExport ConstructorNamespace "Box",
          ModuleExport TypeNamespace "Box"
        ]
    )
    ( exportInventoryEntries
        ( selectModuleExportSelectors
            [ModuleExportSelector Nothing "Box"]
            sampleInventory
        )
    )

testRendersGroupedTypeExportSelectors :: IO ()
testRendersGroupedTypeExportSelectors = do
  assertEqual
    "abstract type selector rendering"
    "type 'Box'"
    (renderModuleExportSelector (ModuleTypeExportSelector "Box" (SourceSpan 1 1) AbstractType))
  assertEqual
    "all constructors selector rendering"
    "type 'Box(..)'"
    (renderModuleExportSelector (ModuleTypeExportSelector "Box" (SourceSpan 1 1) (AllTypeConstructors (SourceSpan 1 5))))
  assertEqual
    "selected constructors selector rendering"
    "type 'Choice(First, Second)'"
    ( renderModuleExportSelector
        ( ModuleTypeExportSelector
            "Choice"
            (SourceSpan 1 1)
            ( SelectedTypeConstructors
                ( LocatedModuleExportName "First" (SourceSpan 1 8)
                    :| [LocatedModuleExportName "Second" (SourceSpan 1 15)]
                )
            )
        )
    )

testGroupedTypeSelectorOwnership :: IO ()
testGroupedTypeSelectorOwnership =
  assertEqual
    "grouped type constructor owner"
    (Set.singleton "A")
    ( exportedConstructorOwners
        "C"
        ( selectValidatedModuleExportSelectors
            reboundConstructorOwners
            [ModuleTypeExportSelector "A" (SourceSpan 1 1) (AllTypeConstructors (SourceSpan 1 8))]
            reboundConstructorInventory
        )
    )

testStandaloneConstructorSelectorOwnership :: IO ()
testStandaloneConstructorSelectorOwnership =
  assertEqual
    "standalone constructor has no selected type owner"
    Set.empty
    ( exportedConstructorOwners
        "C"
        ( selectValidatedModuleExportSelectors
            reboundConstructorOwners
            [ModuleExportSelector (Just ConstructorNamespace) "C"]
            reboundConstructorInventory
        )
    )

testFilteredConstructorOwnership :: IO ()
testFilteredConstructorOwnership = do
  assertEqual
    "filtering constructor drops ownership"
    Set.empty
    (exportedConstructorOwners "C" (selectExportNames (Just ["A"]) ownedConstructorInventory))
  assertEqual
    "filtering owner type drops ownership"
    Set.empty
    (exportedConstructorOwners "C" (selectExportNames (Just ["C"]) ownedConstructorInventory))

testConflictingConstructorOwnership :: IO ()
testConflictingConstructorOwnership =
  assertEqual
    "conflicting selected owners remain explicit"
    (Set.fromList ["A", "B"])
    (exportedConstructorOwners "C" (ownedBy "A" <> ownedBy "B"))

ownedConstructorInventory :: ModuleExportInventory
ownedConstructorInventory = ownedBy "A"

ownedBy :: Text -> ModuleExportInventory
ownedBy typeName =
  selectValidatedModuleExportSelectors
    (Map.singleton typeName (Set.singleton "C"))
    [ModuleTypeExportSelector typeName (SourceSpan 1 1) (AllTypeConstructors (SourceSpan 1 8))]
    ( exportInventory
        [ ModuleExport TypeNamespace typeName,
          ModuleExport ConstructorNamespace "C"
        ]
    )

reboundConstructorOwners :: Map.Map Text (Set.Set Text)
reboundConstructorOwners =
  Map.fromList
    [ ("A", Set.singleton "C"),
      ("B", Set.singleton "C")
    ]

reboundConstructorInventory :: ModuleExportInventory
reboundConstructorInventory =
  exportInventory
    [ ModuleExport TypeNamespace "A",
      ModuleExport ConstructorNamespace "C",
      ModuleExport TypeNamespace "B"
    ]

testAliasVisibility :: IO ()
testAliasVisibility =
  assertEqual
    "alias entries"
    ( Set.fromList
        [ ModuleExport ValueNamespace "answer",
          ModuleExport ConstructorNamespace "Box",
          ModuleExport TypeNamespace "Box",
          ModuleExport TypeNamespace "HiddenType"
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

testInterfaceInventory :: IO ()
testInterfaceInventory =
  assertEqual
    "interface inventory"
    ( Set.fromList
        [ ModuleExport ValueNamespace "answer",
          ModuleExport CapabilityNamespace "Eq"
        ]
    )
    (exportInventoryEntries (moduleInterfaceExportInventory interface))
  where
    interface =
      emptyModuleInterface
        { interfaceValueTypes =
            Map.singleton
              (ModuleExport ValueNamespace "answer")
              (PlainTypeBinding SemanticInt),
          interfaceClassFacts = Map.singleton "Eq" 1
        }
