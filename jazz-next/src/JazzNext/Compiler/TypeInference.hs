{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression,
    inferExpressionDefault
  ) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
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
import JazzNext.Compiler.Desugar
  ( desugarExpr
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [Text]
  }
  deriving (Eq, Show)

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression settings expr = do
  let canonicalExpr = desugarExpr expr
  AnalysisResult _ warnings errors <- analyzeProgram settings canonicalExpr
  let typeErrors = collectIfTypeErrors canonicalExpr
  pure
    InferenceResult
      { inferredExpr = canonicalExpr,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors
      }

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

data ExpressionType
  = TIntType
  | TBoolType
  deriving (Eq, Show)

collectIfTypeErrors :: Expr -> [Text]
collectIfTypeErrors expr = snd (inferExprType Map.empty expr)

inferExprType :: Map Text ExpressionType -> Expr -> (Maybe ExpressionType, [Text])
inferExprType env expr =
  case expr of
    EInt _ -> (Just TIntType, [])
    EBool _ -> (Just TBoolType, [])
    EVar name -> (Map.lookup name env, [])
    EIf conditionExpr thenExpr elseExpr ->
      inferExprType env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
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
    EBinary operatorSymbol leftExpr rightExpr ->
      let (leftType, leftErrors) = inferExprType env leftExpr
          (rightType, rightErrors) = inferExprType env rightExpr
          resultType = inferBinaryResultType operatorSymbol leftType rightType
          binaryErrors =
            case (leftType, rightType) of
              (Just leftOperandType, Just rightOperandType)
                -- Keep equality mismatch diagnostics specific so strict
                -- type-directed equality has a stable contract and error code.
                | operatorSymbol == "==" || operatorSymbol == "!=",
                  leftOperandType /= rightOperandType ->
                    [mkStrictEqualityTypeError operatorSymbol leftOperandType rightOperandType]
                | resultType == Nothing ->
                    [mkBinaryTypeError operatorSymbol leftOperandType rightOperandType]
              _ -> []
       in
        (resultType, leftErrors ++ rightErrors ++ binaryErrors)
    ESectionLeft leftExpr _ ->
      let (_, leftErrors) = inferExprType env leftExpr
       in (Nothing, leftErrors)
    ESectionRight _ rightExpr ->
      let (_, rightErrors) = inferExprType env rightExpr
       in (Nothing, rightErrors)
    EScope statements -> inferScopeType env statements

inferScopeType ::
  Map Text ExpressionType ->
  [Statement] ->
  (Maybe ExpressionType, [Text])
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

parseSignatureType :: Text -> Maybe ExpressionType
parseSignatureType signatureText =
  case Text.filter (not . isSpace) signatureText of
    "Int" -> Just TIntType
    "Bool" -> Just TBoolType
    _ -> Nothing

inferBinaryResultType ::
  Text ->
  Maybe ExpressionType ->
  Maybe ExpressionType ->
  Maybe ExpressionType
inferBinaryResultType operatorSymbol leftType rightType =
  case (operatorSymbol, leftType, rightType) of
    ("+", Just TIntType, Just TIntType) -> Just TIntType
    ("-", Just TIntType, Just TIntType) -> Just TIntType
    ("*", Just TIntType, Just TIntType) -> Just TIntType
    ("/", Just TIntType, Just TIntType) -> Just TIntType
    ("<", Just TIntType, Just TIntType) -> Just TBoolType
    ("<=", Just TIntType, Just TIntType) -> Just TBoolType
    (">", Just TIntType, Just TIntType) -> Just TBoolType
    (">=", Just TIntType, Just TIntType) -> Just TBoolType
    ("==", Just leftOperandType, Just rightOperandType)
      | leftOperandType == rightOperandType -> Just TBoolType
    ("!=", Just leftOperandType, Just rightOperandType)
      | leftOperandType == rightOperandType -> Just TBoolType
    _ -> Nothing

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Text
mkBinaryTypeError operatorSymbol leftType rightType =
  "E2003: cannot apply operator '"
    <> operatorSymbol
    <> "' to operands of type "
    <> renderType leftType
    <> " and "
    <> renderType rightType

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Text
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  "E2004: strict equality operator '"
    <> operatorSymbol
    <> "' requires operands of the same type, found "
    <> renderType leftType
    <> " and "
    <> renderType rightType

mkIfConditionTypeError :: ExpressionType -> Text
mkIfConditionTypeError foundType =
  "E2001: if condition must have type Bool, found " <> renderType foundType

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Text
mkIfBranchTypeMismatchError leftType rightType =
  "E2002: if branches must have matching types, found "
    <> renderType leftType
    <> " and "
    <> renderType rightType

renderType :: ExpressionType -> Text
renderType expressionType =
  case expressionType of
    TIntType -> "Int"
    TBoolType -> "Bool"
