{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Semantics.BindingSignature.BasicsTests (basicTests)
import JazzNext.Compiler.Semantics.BindingSignature.GeneralizationTests (generalizationTests)
import JazzNext.Compiler.Semantics.BindingSignature.ConstraintsTests (constraintTests)
import JazzNext.Compiler.Semantics.BindingSignature.RecursionTests (recursionTests)
import JazzNext.Compiler.Semantics.BindingSignature.DiagnosticsTests (diagnosticTests)
import JazzNext.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "BindingSignatureCoherence" tests

tests :: [NamedTest]
tests = basicTests ++ generalizationTests ++ constraintTests ++ recursionTests ++ diagnosticTests
