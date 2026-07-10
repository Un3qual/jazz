{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Modules.Loader.BasicTests (basicTests)
import JazzNext.Compiler.Modules.Loader.VisibilityTests (visibilityTests)
import JazzNext.Compiler.Modules.Loader.CapabilitiesTests (capabilitiesTests)
import JazzNext.Compiler.Modules.Loader.OperatorsTests (operatorTests)
import JazzNext.Compiler.Modules.Loader.DiagnosticsTests (diagnosticTests)
import JazzNext.TestHarness (NamedTest, runTestSuite)

main :: IO ()
main = runTestSuite "Loader" tests

tests :: [NamedTest]
tests = basicTests ++ visibilityTests ++ capabilitiesTests ++ operatorTests ++ diagnosticTests
