{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Semantics.BindingSignature.BasicsTests (basicTests)
import Jazz.Compiler.Semantics.BindingSignature.GeneralizationTests (generalizationTests)
import Jazz.Compiler.Semantics.BindingSignature.ConstraintsTests (constraintTests)
import Jazz.Compiler.Semantics.BindingSignature.RecursionTests (recursionTests)
import Jazz.Compiler.Semantics.BindingSignature.DiagnosticsTests (diagnosticTests)
import Jazz.Compiler.Semantics.BindingSignature.InferenceOwnershipTests (inferenceOwnershipTests)
import Jazz.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "BindingSignatureCoherence" tests

tests :: [NamedTest]
tests = basicTests ++ generalizationTests ++ constraintTests ++ recursionTests ++ diagnosticTests ++ inferenceOwnershipTests
