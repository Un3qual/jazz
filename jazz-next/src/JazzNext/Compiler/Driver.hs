module JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr
  ) where

import JazzNext.Compiler.Analyzer
  ( Expr
  )
import JazzNext.Compiler.Diagnostics
  ( WarningRecord (..)
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
    compileErrors :: [String],
    generatedJs :: Maybe String
  }
  deriving (Eq, Show)

compileExpr :: WarningSettings -> Expr -> IO CompileResult
compileExpr settings expr = do
  inference <- inferExpression settings expr
  let warnings = filterWarningsForPromotion settings (inferredWarnings inference)
      promotedWarnings = filter (isPromoted settings) warnings
      errors = map warningToError promotedWarnings
      output =
        if null promotedWarnings
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

filterWarningsForPromotion :: WarningSettings -> [WarningRecord] -> [WarningRecord]
filterWarningsForPromotion _ = id

isPromoted :: WarningSettings -> WarningRecord -> Bool
isPromoted settings warning = isWarningError settings (warningCategory warning)

warningToError :: WarningRecord -> String
warningToError warning =
  warningCodeText warning
    ++ ": "
    ++ warningMessage warning
