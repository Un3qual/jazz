{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource
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

-- Compiler driver flow for the current implementation slice:
-- analyze -> collect warnings/errors -> apply warning-as-error policy.
compileExpr :: WarningSettings -> Expr -> IO CompileResult
compileExpr settings expr = do
  inference <- inferExpression settings expr
  let warnings = filterWarningsForPromotion settings (inferredWarnings inference)
      promotedWarnings = filter (isPromoted settings) warnings
      promotedWarningErrors = map warningToError promotedWarnings
      errors = inferredErrors inference ++ promotedWarningErrors
      output =
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
