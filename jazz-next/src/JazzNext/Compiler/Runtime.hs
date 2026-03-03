{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    renderRuntimeValue
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )

data RuntimeValue
  = VInt Int
  | VBool Bool
  deriving (Eq, Show)

evaluateRuntimeExpr :: Expr -> Either Text (Maybe RuntimeValue)
evaluateRuntimeExpr expr =
  case expr of
    EScope statements -> evalScope Map.empty statements
    _ -> Just <$> evalValue Map.empty expr

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue -> Text.pack (show intValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"

type RuntimeEnv = Map Text RuntimeValue

evalScope :: RuntimeEnv -> [Statement] -> Either Text (Maybe RuntimeValue)
evalScope initialEnv statements = go initialEnv Nothing statements
  where
    go :: RuntimeEnv -> Maybe RuntimeValue -> [Statement] -> Either Text (Maybe RuntimeValue)
    go env lastExprValue remainingStatements =
      case remainingStatements of
        [] -> Right lastExprValue
        statement : rest ->
          case statement of
            SSignature {} ->
              go env lastExprValue rest
            SLet name _ valueExpr -> do
              value <- evalValue env valueExpr
              go (Map.insert name value env) lastExprValue rest
            SExpr _ expr -> do
              value <- evalValue env expr
              go env (Just value) rest

evalValue :: RuntimeEnv -> Expr -> Either Text RuntimeValue
evalValue env expr =
  case expr of
    EInt value -> Right (VInt value)
    EBool value -> Right (VBool value)
    EVar name ->
      case Map.lookup name env of
        Just value -> Right value
        Nothing ->
          Left
            ( "E3002: runtime unbound variable '"
                <> name
                <> "'"
            )
    EIf conditionExpr thenExpr elseExpr ->
      evalValue env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValue env conditionExpr
      case conditionValue of
        VBool True -> evalValue env thenExpr
        VBool False -> evalValue env elseExpr
        other ->
          Left
            ( "E3003: runtime branch condition must be Bool, found "
                <> renderRuntimeType other
            )
    EBinary operatorSymbol leftExpr rightExpr -> do
      leftValue <- evalValue env leftExpr
      rightValue <- evalValue env rightExpr
      evalBinary operatorSymbol leftValue rightValue
    ESectionLeft {} ->
      Left "E3004: runtime does not yet support evaluating left operator sections"
    ESectionRight {} ->
      Left "E3005: runtime does not yet support evaluating right operator sections"
    EScope statements ->
      case evalScope env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            "E3006: scope expression has no terminal expression result at runtime"
        Right (Just value) -> Right value

evalBinary :: Text -> RuntimeValue -> RuntimeValue -> Either Text RuntimeValue
evalBinary operatorSymbol leftValue rightValue =
  case (operatorSymbol, leftValue, rightValue) of
    ("+", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt + rightInt))
    ("-", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt - rightInt))
    ("*", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt * rightInt))
    ("/", VInt _, VInt 0) ->
      Left "E3001: runtime primitive '/' failed: division by zero"
    ("/", VInt leftInt, VInt rightInt) ->
      Right (VInt (leftInt `div` rightInt))
    ("<", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt < rightInt))
    ("<=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt <= rightInt))
    (">", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt > rightInt))
    (">=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt >= rightInt))
    ("==", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt == rightInt))
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("!=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt /= rightInt))
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    _ ->
      Left
        ( "E3007: runtime primitive '"
            <> operatorSymbol
            <> "' cannot be applied to "
            <> renderRuntimeType leftValue
            <> " and "
            <> renderRuntimeType rightValue
        )

renderRuntimeType :: RuntimeValue -> Text
renderRuntimeType value =
  case value of
    VInt {} -> "Int"
    VBool {} -> "Bool"
