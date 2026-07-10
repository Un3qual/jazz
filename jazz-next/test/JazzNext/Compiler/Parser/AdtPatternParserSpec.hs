{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Parser.AdtPattern.DeclarationsTests (declarationTests)
import JazzNext.Compiler.Parser.AdtPattern.PatternsTests (patternTests)
import JazzNext.Compiler.Parser.AdtPattern.InvalidSyntaxTests (invalidSyntaxTests)
import JazzNext.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "AdtPatternParser" tests

tests :: [NamedTest]
tests = declarationTests ++ patternTests ++ invalidSyntaxTests
