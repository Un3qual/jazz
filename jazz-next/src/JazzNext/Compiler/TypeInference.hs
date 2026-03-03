module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression,
    inferExpressionDefault
  ) where

import JazzNext.Compiler.Analyzer
  ( AnalysisResult (..),
    Expr,
    analyzeProgram
  )
import JazzNext.Compiler.Diagnostics
  ( WarningRecord
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [String]
  }
  deriving (Eq, Show)

inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression settings expr = do
  AnalysisResult analyzed warnings errors <- analyzeProgram settings expr
  pure
    InferenceResult
      { inferredExpr = analyzed,
        inferredWarnings = warnings,
        inferredErrors = errors
      }

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings
