{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.ControlFlowTests
  ( controlFlowTests
  ) where

import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftDiagnosticCodeAndContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest
  )
import System.Timeout
  ( timeout
  )
import JazzNext.Compiler.Semantics.Runtime.Shared

controlFlowTests :: [NamedTest]
controlFlowTests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure)
    , ("if with True condition skips else branch runtime failure", testIfTrueSkipsElseRuntimeFailure)
    , ("mixed wrapper with eager selected branch produces runtime unbound diagnostic", testMixedWrapperWithSelectedNonAliasSelfUseTerminates)
    , ("function-valued pattern guard uses prior rebinding", testFunctionPatternGuardUsesPriorRebinding)
    , ("pattern-case without a matching arm produces deterministic runtime diagnostic", testPatternCaseNoMatchRuntimeError)
  ]

testIfFalseSkipsThenRuntimeFailure :: IO ()
testIfFalseSkipsThenRuntimeFailure = do
  result <- runSource defaultWarningSettings "if False then (1 / 0) else 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testIfTrueSkipsElseRuntimeFailure :: IO ()
testIfTrueSkipsElseRuntimeFailure = do
  result <- runSource defaultWarningSettings "if True then 1 else (1 / 0)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testMixedWrapperWithSelectedNonAliasSelfUseTerminates :: IO ()
testMixedWrapperWithSelectedNonAliasSelfUseTerminates = do
  maybeResult <- timeout 1000000 (try (runSource defaultWarningSettings "f = if True then (f + 1) else f. 0.") :: IO (Either SomeException RunResult))
  case maybeResult of
    Nothing ->
      failTest "expected mixed wrapper with eager selected self use to terminate with a runtime diagnostic, but evaluation timed out"
    Just (Left err) ->
      failTest ("expected mixed wrapper with eager selected self use to report a runtime diagnostic, but evaluation raised " <> Text.pack (show err))
    Just (Right result) -> do
      assertEqual "compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticContains
        "mixed wrapper selected branch runtime code"
        "E3002"
        (runRuntimeErrors result)
      assertSingleDiagnosticContains
        "mixed wrapper selected branch runtime text"
        "unbound variable 'f'"
        (runRuntimeErrors result)
      assertEqual "runtime output is suppressed on failure" Nothing (runOutput result)

testFunctionPatternGuardUsesPriorRebinding :: IO ()
testFunctionPatternGuardUsesPriorRebinding = do
  result <- runSource defaultWarningSettings "f = \\(x) -> 0. f = case 1 { | 1 if f 0 == 0 -> \\(x) -> x | _ -> \\(x) -> x + 1 }. f 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testPatternCaseNoMatchRuntimeError :: IO ()
testPatternCaseNoMatchRuntimeError = do
  let result = evaluateRuntimeExpr patternCaseNoMatchExpr
  assertLeftDiagnosticCodeAndContains
    "pattern-case no-match runtime code"
    "E3022"
    "matched no arms"
    result
