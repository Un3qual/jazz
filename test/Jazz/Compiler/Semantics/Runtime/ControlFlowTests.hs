{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.ControlFlowTests
  ( controlFlowTests,
  )
where

import Control.Exception
  ( SomeException,
    try,
  )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource,
    withAnalyzedAttachment,
  )
import Jazz.Compiler.SemanticFacts
  ( CoreNodeId (..),
    SemanticFactInvariantFailure (MissingExpressionFacts),
  )
import Jazz.Compiler.Semantics.Runtime.ResolvedFixture
import Jazz.Compiler.Semantics.Runtime.Shared
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticCodeAndContains,
    assertSingleDiagnosticContains,
    failTest,
  )
import System.Timeout
  ( timeout,
  )

controlFlowTests :: [NamedTest]
controlFlowTests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure),
    ("if with True condition skips else branch runtime failure", testIfTrueSkipsElseRuntimeFailure),
    ("non-recursive missing bindings remain compile errors", testNonRecursiveMissingBindingRemainsCompileError),
    ("valid recursive branches receive complete analyzed plans", testValidRecursiveBranchesReceiveCompleteAnalyzedPlans),
    ("attachment invariant failures cannot enter runtime evaluation", testAttachmentFailureCannotEnterRuntime),
    ("mixed wrapper with eager selected branch produces runtime unbound diagnostic", testMixedWrapperWithSelectedNonAliasSelfUseTerminates),
    ("function-valued pattern guard uses prior rebinding", testFunctionPatternGuardUsesPriorRebinding),
    ("pattern-case without a matching arm produces deterministic runtime diagnostic", testPatternCaseNoMatchRuntimeError)
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

testNonRecursiveMissingBindingRemainsCompileError :: IO ()
testNonRecursiveMissingBindingRemainsCompileError = do
  result <- runSource defaultWarningSettings "missing."
  assertSingleDiagnosticContains
    "non-recursive missing binding compile diagnostic"
    "E1001"
    (runCompileErrors result)
  assertSingleDiagnosticContains
    "non-recursive missing binding diagnostic text"
    "unbound variable 'missing'"
    (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)

testValidRecursiveBranchesReceiveCompleteAnalyzedPlans :: IO ()
testValidRecursiveBranchesReceiveCompleteAnalyzedPlans = do
  result <-
    runSource
      defaultWarningSettings
      "if False then { f = if True then (f + 1) else f. 0. } else 0."
  assertEqual "recursive branch compile errors" [] (runCompileErrors result)
  assertEqual "recursive branch runtime errors" [] (runRuntimeErrors result)
  assertEqual "recursive branch runtime output" (Just "0") (runOutput result)

testAttachmentFailureCannotEnterRuntime :: IO ()
testAttachmentFailureCannotEnterRuntime = do
  executed <- newIORef False
  outcome <-
    try
      ( withAnalyzedAttachment
          (Left (MissingExpressionFacts (CoreNodeId 7) :| []))
          (\() -> writeIORef executed True)
      ) ::
      IO (Either SomeException ())
  assertEqual "runtime continuation was not entered" False =<< readIORef executed
  case outcome of
    Left err ->
      assertEqual
        "attachment failure remains the reported invariant"
        True
        ("MissingExpressionFacts" `Text.isInfixOf` Text.pack (show err))
    Right () -> failTest "attachment failure entered runtime evaluation"

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
  let result = evaluateFixture patternCaseNoMatchExpr
  assertLeftDiagnosticCodeAndContains
    "pattern-case no-match runtime code"
    "E3022"
    "matched no arms"
    result
