{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
  ( Text
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "AdtPatternRuntime" tests

tests :: [NamedTest]
tests =
  [ ("runtime selects the first matching literal arm", testRuntimeSelectsLiteralArm),
    ("runtime binds variable patterns in the selected arm", testRuntimeBindsVariablePattern),
    ("runtime uses wildcard fallback when literals do not match", testRuntimeUsesWildcardFallback),
    ("runtime binds nullary data constructor values", testRuntimeBindsNullaryDataConstructor),
    ("runtime applies data constructors", testRuntimeAppliesDataConstructor),
    ("runtime reports a deterministic error when no case arm matches", testRuntimeReportsNoMatchingArm)
  ]

testRuntimeSelectsLiteralArm :: IO ()
testRuntimeSelectsLiteralArm = do
  result <- runSource defaultWarningSettings "case 1 { | 0 -> 10 | 1 -> 20 | _ -> 30 }."
  assertSuccessfulRuntime "literal arm" (Just "20") result

testRuntimeBindsVariablePattern :: IO ()
testRuntimeBindsVariablePattern = do
  result <- runSource defaultWarningSettings "case 2 { | item -> item + 1 }."
  assertSuccessfulRuntime "variable binder" (Just "3") result

testRuntimeUsesWildcardFallback :: IO ()
testRuntimeUsesWildcardFallback = do
  result <- runSource defaultWarningSettings "case 2 { | 1 -> 10 | _ -> 20 }."
  assertSuccessfulRuntime "wildcard fallback" (Just "20") result

testRuntimeBindsNullaryDataConstructor :: IO ()
testRuntimeBindsNullaryDataConstructor = do
  result <- runSource defaultWarningSettings "data Maybe = Nothing. x = Nothing. x."
  assertSuccessfulRuntime "nullary constructor binding" (Just "Nothing") result

testRuntimeAppliesDataConstructor :: IO ()
testRuntimeAppliesDataConstructor = do
  result <- runSource defaultWarningSettings "data Maybe = Just value. x = Just 1. x."
  assertSuccessfulRuntime "constructor application" (Just "Just(1)") result

testRuntimeReportsNoMatchingArm :: IO ()
testRuntimeReportsNoMatchingArm = do
  result <- runSource defaultWarningSettings "case 2 { | 1 -> 10 }."
  assertEqual "no-match compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "no-match runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "no-match runtime text"
    "matched no arms"
    (runRuntimeErrors result)
  assertEqual "no-match runtime output" Nothing (runOutput result)

assertSuccessfulRuntime :: Text -> Maybe Text -> RunResult -> IO ()
assertSuccessfulRuntime label expectedOutput result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " runtime output") expectedOutput (runOutput result)
