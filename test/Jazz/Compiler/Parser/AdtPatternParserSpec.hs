{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Parser.AdtPattern.DeclarationsTests (declarationTests)
import Jazz.Compiler.Parser.AdtPattern.PatternsTests (patternTests)
import Jazz.Compiler.Parser.AdtPattern.InvalidSyntaxTests (invalidSyntaxTests)
import Jazz.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "AdtPatternParser" tests

tests :: [NamedTest]
tests = declarationTests ++ patternTests ++ invalidSyntaxTests
