{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Parser.Foundation.ExpressionsTests (expressionTests)
import Jazz.Compiler.Parser.Foundation.SignaturesTests (signatureTests)
import Jazz.Compiler.Parser.Foundation.ModulesTests (moduleTests)
import Jazz.Compiler.Parser.Foundation.InvalidSyntaxTests (invalidSyntaxTests)
import Jazz.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "ParserFoundation" tests

tests :: [NamedTest]
tests = expressionTests ++ signatureTests ++ moduleTests ++ invalidSyntaxTests
