{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Runtime.OutcomeTests
  ( tests,
  )
where

import Jazz.Compiler.DiagnosticCatalog (ErrorCode (..))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (RuntimeOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    RuntimeOutcome (..),
    diagnosticResultOutcome,
    runtimeControlAsDiagnosticResult,
    runtimeControlOutcome,
    runtimeExitNotRepresentableDiagnostic,
    runtimeOutcomeAsDiagnosticResult,
  )
import Jazz.TestHarness (NamedTest, assertEqual)

tests :: [NamedTest]
tests =
  [ ("runtime controls preserve completed values", testCompletedControl),
    ("runtime controls preserve diagnostic failures", testDiagnosticControl),
    ("runtime controls preserve requested exits", testExitControl),
    ("diagnostic results preserve both branches", testDiagnosticResult),
    ("legacy control results preserve values and diagnostics", testLegacyControlResult),
    ("legacy outcome results use the canonical exit diagnostic", testLegacyOutcomeResult)
  ]

testCompletedControl :: IO ()
testCompletedControl =
  assertEqual
    "completed control"
    (RuntimeOutcomeCompleted (42 :: Int))
    (runtimeControlOutcome (Right 42))

testDiagnosticControl :: IO ()
testDiagnosticControl =
  assertEqual
    "diagnostic control"
    (RuntimeOutcomeFailed sampleDiagnostic :: RuntimeOutcome Int)
    (runtimeControlOutcome (Left (RuntimeDiagnostic sampleDiagnostic)))

testExitControl :: IO ()
testExitControl =
  assertEqual
    "exit control"
    (RuntimeOutcomeExited 17 :: RuntimeOutcome Int)
    (runtimeControlOutcome (Left (RuntimeExitRequested 17)))

testDiagnosticResult :: IO ()
testDiagnosticResult = do
  assertEqual
    "completed diagnostic result"
    (RuntimeOutcomeCompleted (42 :: Int))
    (diagnosticResultOutcome (Right 42))
  assertEqual
    "failed diagnostic result"
    (RuntimeOutcomeFailed sampleDiagnostic :: RuntimeOutcome Int)
    (diagnosticResultOutcome (Left sampleDiagnostic))

testLegacyControlResult :: IO ()
testLegacyControlResult = do
  assertEqual
    "completed legacy control"
    (Right (42 :: Int))
    (runtimeControlAsDiagnosticResult (Right 42))
  assertEqual
    "failed legacy control"
    (Left sampleDiagnostic :: Either Diagnostic Int)
    (runtimeControlAsDiagnosticResult (Left (RuntimeDiagnostic sampleDiagnostic)))
  assertEqual
    "exited legacy control"
    (Left expectedExitDiagnostic :: Either Diagnostic Int)
    (runtimeControlAsDiagnosticResult (Left (RuntimeExitRequested 17)))

testLegacyOutcomeResult :: IO ()
testLegacyOutcomeResult = do
  assertEqual
    "completed legacy outcome"
    (Right (42 :: Int))
    (runtimeOutcomeAsDiagnosticResult (RuntimeOutcomeCompleted 42))
  assertEqual
    "failed legacy outcome"
    (Left sampleDiagnostic :: Either Diagnostic Int)
    (runtimeOutcomeAsDiagnosticResult (RuntimeOutcomeFailed sampleDiagnostic))
  assertEqual
    "legacy exit diagnostic constructor"
    expectedExitDiagnostic
    (runtimeExitNotRepresentableDiagnostic 17)
  assertEqual
    "exited legacy outcome"
    (Left expectedExitDiagnostic :: Either Diagnostic Int)
    (runtimeOutcomeAsDiagnosticResult (RuntimeOutcomeExited 17))

sampleDiagnostic :: Diagnostic
sampleDiagnostic =
  mkErrorDiagnostic E3001 RuntimeOrigin "sample runtime failure"

expectedExitDiagnostic :: Diagnostic
expectedExitDiagnostic =
  mkErrorDiagnostic
    E3020
    RuntimeOrigin
    "runtime exit status 17 cannot be represented by this legacy evaluator result"
