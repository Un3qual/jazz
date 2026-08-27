{-# LANGUAGE OverloadedStrings #-}

-- | Runtime termination values and compatibility adapters shared by the
-- evaluator, module runtime, observation, and legacy result boundaries.
module Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    RuntimeOutcome (..),
    runtimeControlOutcome,
    diagnosticResultOutcome,
    runtimeControlAsDiagnosticResult,
    runtimeOutcomeAsDiagnosticResult,
    runtimeExitNotRepresentableDiagnostic,
  )
where

import qualified Data.Text as Text
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E3020))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (RuntimeOrigin),
    mkErrorDiagnostic,
  )

-- | Interpreter-internal non-local control. Runtime diagnostics and requested
-- process exits share the evaluator's unwind path without conflating exit with
-- an error visible to Jazz programs.
data RuntimeControl
  = RuntimeDiagnostic Diagnostic
  | RuntimeExitRequested Integer

-- | A runtime can complete with a value, fail with a diagnostic, or request
-- process termination. Exit is control flow rather than a diagnostic so hosts
-- can finalize observation artifacts before the CLI applies the status.
data RuntimeOutcome value
  = RuntimeOutcomeCompleted value
  | RuntimeOutcomeExited Integer
  | RuntimeOutcomeFailed Diagnostic
  deriving (Eq, Show)

runtimeControlOutcome :: Either RuntimeControl value -> RuntimeOutcome value
runtimeControlOutcome controlResult =
  case controlResult of
    Left (RuntimeDiagnostic diagnostic) -> RuntimeOutcomeFailed diagnostic
    Left (RuntimeExitRequested status) -> RuntimeOutcomeExited status
    Right value -> RuntimeOutcomeCompleted value

diagnosticResultOutcome :: Either Diagnostic value -> RuntimeOutcome value
diagnosticResultOutcome result =
  case result of
    Left diagnostic -> RuntimeOutcomeFailed diagnostic
    Right value -> RuntimeOutcomeCompleted value

runtimeControlAsDiagnosticResult :: Either RuntimeControl value -> Either Diagnostic value
runtimeControlAsDiagnosticResult controlResult =
  case controlResult of
    Left (RuntimeDiagnostic diagnostic) -> Left diagnostic
    Left (RuntimeExitRequested status) ->
      Left (runtimeExitNotRepresentableDiagnostic status)
    Right value -> Right value

runtimeOutcomeAsDiagnosticResult :: RuntimeOutcome value -> Either Diagnostic value
runtimeOutcomeAsDiagnosticResult outcome =
  case outcome of
    RuntimeOutcomeFailed diagnostic -> Left diagnostic
    RuntimeOutcomeExited status ->
      Left (runtimeExitNotRepresentableDiagnostic status)
    RuntimeOutcomeCompleted value -> Right value

runtimeExitNotRepresentableDiagnostic :: Integer -> Diagnostic
runtimeExitNotRepresentableDiagnostic status =
  mkErrorDiagnostic
    E3020
    RuntimeOrigin
    ("runtime exit status " <> Text.pack (show status) <> " cannot be represented by this legacy evaluator result")
