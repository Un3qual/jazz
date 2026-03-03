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
    spanColumn,
    spanLine,
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
  | SSignature String SourceSpan String
  | SExpr Expr
  deriving (Eq, Show)

data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisWarnings :: [WarningRecord],
    analysisErrors :: [String]
  }
  deriving (Eq, Show)

analyzeProgram :: WarningSettings -> Expr -> IO AnalysisResult
analyzeProgram settings expr =
  let (warnings, errors) = collectExprDiagnostics settings Map.empty expr
   in
  pure
    AnalysisResult
      { analyzedExpr = expr,
        analysisWarnings = sortWarnings warnings,
        analysisErrors = errors
      }

analyzeRebindingWarnings :: WarningSettings -> Expr -> IO [WarningRecord]
analyzeRebindingWarnings settings expr = analysisWarnings <$> analyzeProgram settings expr

collectExprDiagnostics ::
  WarningSettings ->
  Map String SourceSpan ->
  Expr ->
  ([WarningRecord], [String])
collectExprDiagnostics settings visibleBindings expr =
  case expr of
    EInt _ -> ([], [])
    EVar name ->
      case Map.lookup name visibleBindings of
        Just _ -> ([], [])
        Nothing -> ([], [mkUnboundVariableError name])
    EScope statements -> collectScopeDiagnostics settings visibleBindings statements

collectScopeDiagnostics ::
  WarningSettings ->
  Map String SourceSpan ->
  [Statement] ->
  ([WarningRecord], [String])
collectScopeDiagnostics settings outerScope statements =
  (reverse finalWarningsRev, reverse errorsWithFinalPending)
  where
    (_, finalPendingSignature, finalWarningsRev, finalErrorsRev) =
      foldl' step (Map.empty, Nothing, [], []) statements
    errorsWithFinalPending = flushPendingSignature finalPendingSignature finalErrorsRev

    step ::
      (Map String SourceSpan, Maybe PendingSignature, [WarningRecord], [String]) ->
      Statement ->
      (Map String SourceSpan, Maybe PendingSignature, [WarningRecord], [String])
    step (scopeBindings, pendingSignature, warningsRev, errorsRev) statement =
      case statement of
        SExpr expr ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
              visible = currentVisibleBindings scopeBindings
              (exprWarnings, exprErrors) = collectExprDiagnostics settings visible expr
           in
            ( scopeBindings,
              Nothing,
              appendWarnings warningsRev exprWarnings,
              appendErrors errorsWithPending exprErrors
            )
        SSignature signatureName signatureSpan _signatureText ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Just (PendingSignature signatureName signatureSpan),
              warningsRev,
              errorsWithPending
            )
        SLet bindingName bindingSpan valueExpr ->
          let errorsFromSignature =
                case pendingSignature of
                  Nothing -> []
                  Just (PendingSignature signatureName signatureDeclSpan)
                    | signatureName == bindingName -> []
                    | otherwise ->
                        [ mkMismatchedSignatureError
                            signatureName
                            signatureDeclSpan
                            bindingName
                        ]
              rebindingWarning =
                case Map.lookup bindingName scopeBindings of
                  Just previousSpan
                    | isWarningEnabled settings SameScopeRebinding ->
                        [mkSameScopeRebindingWarning bindingName bindingSpan previousSpan]
                  _ -> []
              nextScope = Map.insert bindingName bindingSpan scopeBindings
              visible = currentVisibleBindings nextScope
              (valueWarnings, valueErrors) = collectExprDiagnostics settings visible valueExpr
              warningsWithValue = appendWarnings warningsRev valueWarnings
              errorsWithValue =
                appendErrors (appendErrors errorsRev errorsFromSignature) valueErrors
           in
            ( nextScope,
              Nothing,
              appendWarnings warningsWithValue rebindingWarning,
              errorsWithValue
            )

    currentVisibleBindings :: Map String SourceSpan -> Map String SourceSpan
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    appendWarnings :: [WarningRecord] -> [WarningRecord] -> [WarningRecord]
    appendWarnings = foldl' (flip (:))

    appendErrors :: [String] -> [String] -> [String]
    appendErrors = foldl' (flip (:))

data PendingSignature = PendingSignature
  { pendingSignatureName :: String,
    pendingSignatureSpan :: SourceSpan
  }

flushPendingSignature :: Maybe PendingSignature -> [String] -> [String]
flushPendingSignature pending errorsRev =
  case pending of
    Nothing -> errorsRev
    Just pendingSignature ->
      appendError errorsRev (mkMissingBindingForSignatureError pendingSignature)
  where
    appendError rev errorText = errorText : rev

mkUnboundVariableError :: String -> String
mkUnboundVariableError variableName =
  "E1001: unbound variable '" ++ variableName ++ "'"

mkMissingBindingForSignatureError :: PendingSignature -> String
mkMissingBindingForSignatureError pendingSignature =
  "E1002: signature for '"
    ++ pendingSignatureName pendingSignature
    ++ "' at "
    ++ renderSpan (pendingSignatureSpan pendingSignature)
    ++ " must be immediately followed by a matching binding"

mkMismatchedSignatureError :: String -> SourceSpan -> String -> String
mkMismatchedSignatureError signatureName signatureSpan bindingName =
  "E1003: signature for '"
    ++ signatureName
    ++ "' at "
    ++ renderSpan signatureSpan
    ++ " must annotate the next binding with the same name; found '"
    ++ bindingName
    ++ "'"

renderSpan :: SourceSpan -> String
renderSpan spanValue = show (spanLine spanValue) ++ ":" ++ show (spanColumn spanValue)
