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
  )
import Jazz.Compiler.Name (NameNamespace (..))
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
    ("retains ownership only for selected constructors", testFilteredConstructorOwnership),
    ("combines conflicting constructor owners without bias", testConflictingConstructorOwnership),
    ("finds the first requested namespace deterministically", testFirstNamespace)
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
    "standalone constructor retains declaring owners"
    (Set.fromList ["A", "B"])
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
    "constructor-only imports retain nominal ownership"
    (Set.singleton "A")
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
