{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource,
    RunResult (..),
    runExpr,
    runSource
  ) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr
  )
import JazzNext.Compiler.Diagnostics
  ( WarningRecord (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr,
    renderRuntimeValue
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningError
  )

data CompileResult = CompileResult
  { compileWarnings :: [WarningRecord],
    compileErrors :: [Text],
    generatedJs :: Maybe Text
  }
  deriving (Eq, Show)

data RunResult = RunResult
  { runWarnings :: [WarningRecord],
    runCompileErrors :: [Text],
    runRuntimeErrors :: [Text],
    runOutput :: Maybe Text
  }
  deriving (Eq, Show)

-- Compiler driver flow for the current implementation slice:
-- analyze -> collect warnings/errors -> apply warning-as-error policy.
compileExpr :: WarningSettings -> Expr -> IO CompileResult
compileExpr settings expr = do
  (warnings, errors, _) <- analyzeWithWarnings settings expr
  let output =
        if null errors
          then
            -- This module is only responsible for warning/error control flow.
            -- JS generation remains a placeholder until codegen lands in jazz-next.
            Just "/* jazz-next codegen placeholder */"
          else Nothing
  pure
    CompileResult
      { compileWarnings = warnings,
        compileErrors = errors,
        generatedJs = output
      }

compileSource :: WarningSettings -> Text -> IO CompileResult
compileSource settings source =
  case parseSurfaceProgram source of
    Left parseError ->
      pure
        CompileResult
          { compileWarnings = [],
            compileErrors = ["E0001: parse error: " <> parseError],
            generatedJs = Nothing
          }
    Right surfaceProgram ->
      compileExpr settings (lowerSurfaceExpr surfaceProgram)

runExpr :: WarningSettings -> Expr -> IO RunResult
runExpr settings expr = do
  (warnings, compileErrors, canonicalExpr) <- analyzeWithWarnings settings expr
  if not (null compileErrors)
    then
      pure
        RunResult
          { runWarnings = warnings,
            runCompileErrors = compileErrors,
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    else
      case evaluateRuntimeExpr canonicalExpr of
        Left runtimeError ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [runtimeError],
                runOutput = Nothing
              }
        Right runtimeValue ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [],
                runOutput = fmap renderRuntimeValue runtimeValue
              }

runSource :: WarningSettings -> Text -> IO RunResult
runSource settings source =
  case parseSurfaceProgram source of
    Left parseError ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = ["E0001: parse error: " <> parseError],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right surfaceProgram ->
      runExpr settings (lowerSurfaceExpr surfaceProgram)

analyzeWithWarnings :: WarningSettings -> Expr -> IO ([WarningRecord], [Text], Expr)
analyzeWithWarnings settings expr = do
  inference <- inferExpression settings expr
  let warnings = filterWarningsForPromotion settings (inferredWarnings inference)
      promotedWarnings = filter (isPromoted settings) warnings
      promotedWarningErrors = map warningToError promotedWarnings
      errors = inferredErrors inference ++ promotedWarningErrors
  pure (warnings, errors, inferredExpr inference)

filterWarningsForPromotion :: WarningSettings -> [WarningRecord] -> [WarningRecord]
-- Placeholder hook for future category-level filtering.
filterWarningsForPromotion _ = id

isPromoted :: WarningSettings -> WarningRecord -> Bool
isPromoted settings warning = isWarningError settings (warningCategory warning)

warningToError :: WarningRecord -> Text
warningToError warning =
  warningCodeText warning
    <> ": "
    <> warningMessage warning
