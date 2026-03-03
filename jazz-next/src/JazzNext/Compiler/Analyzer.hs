module JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..),
    AnalysisResult (..),
    analyzeProgram,
    analyzeRebindingWarnings
  ) where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Map.Strict (Map)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan,
    WarningRecord,
    mkSameScopeRebindingWarning,
    sortWarnings
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..)
  )

data Expr
  = EInt Int
  | EVar String
  | EScope [Statement]
  deriving (Eq, Show)

data Statement
  = SLet String SourceSpan Expr
  | SExpr Expr
  deriving (Eq, Show)

data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisWarnings :: [WarningRecord]
  }
  deriving (Eq, Show)

analyzeProgram :: WarningSettings -> Expr -> IO AnalysisResult
analyzeProgram settings expr =
  pure
    AnalysisResult
      { analyzedExpr = expr,
        analysisWarnings = sortWarnings (collectExprWarnings settings expr)
      }

analyzeRebindingWarnings :: WarningSettings -> Expr -> IO [WarningRecord]
analyzeRebindingWarnings settings expr = analysisWarnings <$> analyzeProgram settings expr

collectExprWarnings :: WarningSettings -> Expr -> [WarningRecord]
collectExprWarnings settings expr =
  case expr of
    EInt _ -> []
    EVar _ -> []
    EScope statements -> collectScopeWarnings settings Map.empty statements

collectScopeWarnings ::
  WarningSettings ->
  Map String SourceSpan ->
  [Statement] ->
  [WarningRecord]
collectScopeWarnings settings initialScope statements =
  reverse finalWarningsRev
  where
    (_, finalWarningsRev) = foldl' step (initialScope, []) statements

    step ::
      (Map String SourceSpan, [WarningRecord]) ->
      Statement ->
      (Map String SourceSpan, [WarningRecord])
    step (scopeBindings, warningsRev) statement =
      case statement of
        SExpr expr ->
          (scopeBindings, appendWarnings warningsRev (collectExprWarnings settings expr))
        SLet bindingName bindingSpan valueExpr ->
          let valueWarnings = collectExprWarnings settings valueExpr
              rebindingWarning =
                case Map.lookup bindingName scopeBindings of
                  Just previousSpan
                    | isWarningEnabled settings SameScopeRebinding ->
                        [mkSameScopeRebindingWarning bindingName bindingSpan previousSpan]
                  _ -> []
              nextScope = Map.insert bindingName bindingSpan scopeBindings
              warningsWithValue = appendWarnings warningsRev valueWarnings
           in (nextScope, appendWarnings warningsWithValue rebindingWarning)

    appendWarnings :: [WarningRecord] -> [WarningRecord] -> [WarningRecord]
    appendWarnings = foldl' (flip (:))
