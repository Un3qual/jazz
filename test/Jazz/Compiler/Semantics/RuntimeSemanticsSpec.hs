{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Semantics.Runtime.CapabilitiesTests (capabilityTests)
import Jazz.Compiler.Semantics.Runtime.ControlFlowTests (controlFlowTests)
import Jazz.Compiler.Semantics.Runtime.HostIOTests (hostIOTests, hostScaleTests)
import Jazz.Compiler.Semantics.Runtime.NumericTests (numericTests)
import Jazz.Compiler.Semantics.Runtime.RecursionTests (recursionScaleTests, recursionTests)
import Jazz.Compiler.Semantics.Runtime.RenderingTests (renderingTests)
import Jazz.Compiler.Semantics.Runtime.ScopeTests (scopeTests)
import Jazz.TestHarness (NamedTest, runTestSuite)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  runTestSuite "RuntimeSemantics" (if "--skip-performance" `elem` args then tests else scopeTests ++ recursionScaleTests ++ hostScaleTests ++ tests)

tests :: [NamedTest]
tests = controlFlowTests ++ recursionTests ++ numericTests ++ capabilityTests ++ renderingTests ++ hostIOTests
