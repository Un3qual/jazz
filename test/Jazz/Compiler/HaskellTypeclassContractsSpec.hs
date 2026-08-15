{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( SignaturePayload (SignatureType),
    SignatureType (TypeBool, TypeInt),
  )
import Jazz.Compiler.LoweredIR.Lower.Types
  ( RuntimeRequirements (..),
  )
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextAppendService, TextEqualService, TextLengthService),
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (ModuleExport),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
  )
import Jazz.Compiler.Name
  ( NameNamespace (TypeNamespace, ValueNamespace),
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (ClassMethodType),
    ImplMethodType (ImplMethodType),
    ScopeCapabilityFacts (..),
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "Haskell typeclass contracts" tests

tests :: [NamedTest]
tests =
  [ ("runtime requirements form their intended monoid", testRuntimeRequirements),
    ("scope capability facts preserve collision order", testScopeCapabilityFacts),
    ("module export inventories union without duplicates", testModuleExportInventory)
  ]

assertMonoidLaws :: (Eq value, Show value, Monoid value) => Text -> value -> value -> value -> IO ()
assertMonoidLaws label first second third = do
  assertEqual (label <> " left identity") first (mempty <> first)
  assertEqual (label <> " right identity") first (first <> mempty)
  assertEqual
    (label <> " associativity")
    ((first <> second) <> third)
    (first <> (second <> third))

testRuntimeRequirements :: IO ()
testRuntimeRequirements = do
  assertMonoidLaws "runtime requirements" first second third
  assertEqual
    "runtime requirements composition"
    (RuntimeRequirements True (Set.fromList [TextAppendService, TextLengthService]))
    (first <> second)
  where
    first = RuntimeRequirements False (Set.singleton TextLengthService)
    second = RuntimeRequirements True (Set.singleton TextAppendService)
    third = RuntimeRequirements False (Set.singleton TextEqualService)

testScopeCapabilityFacts :: IO ()
testScopeCapabilityFacts = do
  assertMonoidLaws "scope capability facts" first second third
  assertEqual
    "class facts remain left-biased"
    (Just 1)
    (Map.lookup "Comparable" (scopeClassFacts combined))
  assertEqual
    "method facts remain left-biased"
    (Just (ClassMethodType "Left" (SignatureType TypeInt)))
    (Map.lookup "compare" (scopeClassMethodSignatures combined))
  assertEqual
    "implementation methods preserve left-to-right order"
    (Just [ImplMethodType TypeInt, ImplMethodType TypeBool])
    (Map.lookup "Comparable" (scopeConcreteImplMethods combined))
  where
    combined = first <> second
    first =
      mempty
        { scopeClassFacts = Map.singleton "Comparable" 1,
          scopeClassMethodSignatures =
            Map.singleton "compare" (ClassMethodType "Left" (SignatureType TypeInt)),
          scopeConcreteImplMethods =
            Map.singleton "Comparable" [ImplMethodType TypeInt]
        }
    second =
      mempty
        { scopeClassFacts = Map.singleton "Comparable" 2,
          scopeClassMethodSignatures =
            Map.singleton "compare" (ClassMethodType "Right" (SignatureType TypeBool)),
          scopeConcreteImplMethods =
            Map.singleton "Comparable" [ImplMethodType TypeBool]
        }
    third =
      mempty
        { scopeGeneratedEqualityClassFacts = Set.singleton "Eq",
          scopeConcreteImplFacts = Set.singleton "Comparable"
        }

testModuleExportInventory :: IO ()
testModuleExportInventory = do
  assertMonoidLaws "module export inventory" first second third
  assertEqual
    "module export inventory union"
    ( Set.fromList
        [ ModuleExport ValueNamespace "answer",
          ModuleExport TypeNamespace "Answer"
        ]
    )
    (exportInventoryEntries (first <> second))
  where
    first = exportInventory [ModuleExport ValueNamespace "answer"]
    second =
      exportInventory
        [ ModuleExport ValueNamespace "answer",
          ModuleExport TypeNamespace "Answer"
        ]
    third :: ModuleExportInventory
    third = exportInventory [ModuleExport ValueNamespace "other"]
