{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Semantics.Runtime.ControlFlowTests (controlFlowTests)
import Jazz.Compiler.Semantics.Runtime.RecursionTests (recursionTests)
import Jazz.Compiler.Semantics.Runtime.NumericTests (numericTests)
import Jazz.Compiler.Semantics.Runtime.CapabilitiesTests (capabilityTests)
import Jazz.Compiler.Semantics.Runtime.RenderingTests (renderingTests)
import Jazz.Compiler.Semantics.Runtime.ScopeTests (scopeTests)
import Jazz.Compiler.Semantics.Runtime.HostIOTests (hostIOTests)
import Jazz.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "RuntimeSemantics" tests

tests :: [NamedTest]
tests = scopeTests ++ controlFlowTests ++ recursionTests ++ numericTests ++ capabilityTests ++ renderingTests ++ hostIOTests
