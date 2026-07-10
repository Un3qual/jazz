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
