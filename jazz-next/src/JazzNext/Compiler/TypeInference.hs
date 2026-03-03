module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression,
    inferExpressionDefault
  ) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import JazzNext.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgram
  )
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
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

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression settings expr = do
  AnalysisResult analyzed warnings errors <- analyzeProgram settings expr
  let typeErrors = collectIfTypeErrors analyzed
  pure
    InferenceResult
      { inferredExpr = analyzed,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors
      }

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

data ExpressionType
  = TIntType
  | TBoolType
  deriving (Eq, Show)

collectIfTypeErrors :: Expr -> [String]
collectIfTypeErrors expr = snd (inferExprType Map.empty expr)

inferExprType :: Map String ExpressionType -> Expr -> (Maybe ExpressionType, [String])
inferExprType env expr =
  case expr of
    EInt _ -> (Just TIntType, [])
    EBool _ -> (Just TBoolType, [])
    EVar name -> (Map.lookup name env, [])
    EIf conditionExpr thenExpr elseExpr ->
      let (conditionType, conditionErrors) = inferExprType env conditionExpr
          (thenType, thenErrors) = inferExprType env thenExpr
          (elseType, elseErrors) = inferExprType env elseExpr
          conditionTypeErrors =
            case conditionType of
              Just TBoolType -> []
              Just foundType -> [mkIfConditionTypeError foundType]
              Nothing -> []
          branchTypeErrors =
            case (thenType, elseType) of
              (Just leftType, Just rightType)
                | leftType /= rightType ->
                    [mkIfBranchTypeMismatchError leftType rightType]
              _ -> []
          resultType =
            case (thenType, elseType) of
              (Just leftType, Just rightType)
                | leftType == rightType -> Just leftType
              _ -> Nothing
       in
        ( resultType,
          conditionErrors
            ++ conditionTypeErrors
            ++ thenErrors
            ++ elseErrors
            ++ branchTypeErrors
        )
    EScope statements -> inferScopeType env statements

inferScopeType ::
  Map String ExpressionType ->
  [Statement] ->
  (Maybe ExpressionType, [String])
inferScopeType initialEnv statements = go initialEnv Nothing [] statements
  where
    go env lastExprType errorsSoFar remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, errorsSoFar)
        statement : rest ->
          case statement of
            SSignature name _ signatureText ->
              let nextEnv =
                    case parseSignatureType signatureText of
                      Just signatureType -> Map.insert name signatureType env
                      Nothing -> env
               in go nextEnv lastExprType errorsSoFar rest
            SLet name _ valueExpr ->
              let (valueType, valueErrors) = inferExprType env valueExpr
                  nextEnv =
                    case valueType of
                      Just inferredType -> Map.insert name inferredType env
                      Nothing -> env
               in go nextEnv lastExprType (errorsSoFar ++ valueErrors) rest
            SExpr _ expr ->
              let (exprType, exprErrors) = inferExprType env expr
               in go env exprType (errorsSoFar ++ exprErrors) rest

parseSignatureType :: String -> Maybe ExpressionType
parseSignatureType signatureText =
  case filter (not . isSpace) signatureText of
    "Int" -> Just TIntType
    "Bool" -> Just TBoolType
    _ -> Nothing

mkIfConditionTypeError :: ExpressionType -> String
mkIfConditionTypeError foundType =
  "E2001: if condition must have type Bool, found " ++ renderType foundType

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> String
mkIfBranchTypeMismatchError leftType rightType =
  "E2002: if branches must have matching types, found "
    ++ renderType leftType
    ++ " and "
    ++ renderType rightType

renderType :: ExpressionType -> String
renderType expressionType =
  case expressionType of
    TIntType -> "Int"
    TBoolType -> "Bool"
