{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Driver
  ( compileSource,
    compileErrors
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "AdtPatternType" tests

tests :: [NamedTest]
tests =
  [ ( "source pipeline keeps variable pattern binders in scope while typing is deferred",
      testPatternVariableBinderDoesNotReportUnbound
    )
  ]

testPatternVariableBinderDoesNotReportUnbound :: IO ()
testPatternVariableBinderDoesNotReportUnbound = do
  result <- compileSource defaultWarningSettings "x = case 1 { | item -> item }."
  assertSingleDiagnosticCode
    "pattern case placeholder code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "pattern case placeholder text"
    "pattern case matching is not implemented yet"
    (compileErrors result)
