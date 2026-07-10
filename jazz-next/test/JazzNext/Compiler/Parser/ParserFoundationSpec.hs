{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Parser.Foundation.ExpressionsTests (expressionTests)
import JazzNext.Compiler.Parser.Foundation.SignaturesTests (signatureTests)
import JazzNext.Compiler.Parser.Foundation.ModulesTests (moduleTests)
import JazzNext.Compiler.Parser.Foundation.InvalidSyntaxTests (invalidSyntaxTests)
import JazzNext.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "ParserFoundation" tests

tests :: [NamedTest]
tests = expressionTests ++ signatureTests ++ moduleTests ++ invalidSyntaxTests
