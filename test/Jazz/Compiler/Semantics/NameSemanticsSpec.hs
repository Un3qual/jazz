{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Jazz.Compiler.ModuleIdentity (mkModulePath)
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    generatedName,
    mkIdentifier,
    namePurity,
    renderName,
    resolvedImportedName,
    sourceName,
  )
import Jazz.Compiler.Purity (Purity (..))
import Jazz.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "NameSemantics" tests

tests :: [NamedTest]
tests =
  [ ("source and resolved names are structurally distinct", testSourceAndResolvedNamesAreDistinct),
    ("generated names do not acquire user purity", testGeneratedNamesDoNotAcquireUserPurity)
  ]

testSourceAndResolvedNamesAreDistinct :: IO ()
testSourceAndResolvedNamesAreDistinct = do
  let modulePath = mkModulePath (mkIdentifier "Lib" :| [])
      source = sourceName (mkIdentifier "Lib::answer")
      imported = resolvedImportedName modulePath ValueNamespace (mkIdentifier "answer")
  assertEqual "rendered source" "Lib::answer" (renderName source)
  assertEqual "rendered imported" "Lib::answer" (renderName imported)
  assertEqual
    "imported origin retains nominal path"
    (ResolvedName (ImportedModule modulePath) ValueNamespace (mkIdentifier "answer"))
    imported
  assertEqual "structured distinction" False (source == imported)

testGeneratedNamesDoNotAcquireUserPurity :: IO ()
testGeneratedNamesDoNotAcquireUserPurity = do
  let generated = generatedName (OperatorBinding "!")
  assertEqual "generated constructor" (GeneratedName (OperatorBinding "!")) generated
  assertEqual "generated purity" Pure (namePurity generated)
