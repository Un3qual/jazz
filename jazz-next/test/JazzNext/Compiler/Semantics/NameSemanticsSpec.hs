{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Identifier (mkIdentifier)
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    generatedName,
    namePurity,
    renderName,
    resolvedImportedName,
    sourceName
  )
import JazzNext.Compiler.Purity (Purity (..))
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "NameSemantics" tests

tests :: [NamedTest]
tests =
  [ ("source and resolved names are structurally distinct", testSourceAndResolvedNamesAreDistinct),
    ("generated names do not acquire user purity", testGeneratedNamesDoNotAcquireUserPurity)
  ]

testSourceAndResolvedNamesAreDistinct :: IO ()
testSourceAndResolvedNamesAreDistinct = do
  let source = sourceName (mkIdentifier "Lib::answer")
      imported = resolvedImportedName ["Lib"] ValueNamespace (mkIdentifier "answer")
  assertEqual "rendered source" "Lib::answer" (renderName source)
  assertEqual "rendered imported" "Lib::answer" (renderName imported)
  assertEqual "structured distinction" False (source == imported)

testGeneratedNamesDoNotAcquireUserPurity :: IO ()
testGeneratedNamesDoNotAcquireUserPurity = do
  let generated = generatedName (OperatorBinding "!")
  assertEqual "generated constructor" (GeneratedName (OperatorBinding "!")) generated
  assertEqual "generated purity" Pure (namePurity generated)
