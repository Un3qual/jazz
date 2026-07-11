{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Semantics.Runtime.ControlFlowTests (controlFlowTests)
import JazzNext.Compiler.Semantics.Runtime.RecursionTests (recursionTests)
import JazzNext.Compiler.Semantics.Runtime.NumericTests (numericTests)
import JazzNext.Compiler.Semantics.Runtime.CapabilitiesTests (capabilityTests)
import JazzNext.Compiler.Semantics.Runtime.RenderingTests (renderingTests)
import JazzNext.Compiler.Semantics.Runtime.HostIOTests (hostIOTests)
import JazzNext.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "RuntimeSemantics" tests

tests :: [NamedTest]
tests = controlFlowTests ++ recursionTests ++ numericTests ++ capabilityTests ++ renderingTests ++ hostIOTests
