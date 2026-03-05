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
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbol
  )

data RuntimeValue
  = VInt Int
  | VBool Bool
  | VList [RuntimeValue]
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
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
    VList elements ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VBuiltin _ _ -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"

type RuntimeEnv = Map Text RuntimeValue

evalScope :: RuntimeEnv -> [Statement] -> Either Text (Maybe RuntimeValue)
evalScope initialEnv statements = go initialEnv Nothing statements
  where
    go :: RuntimeEnv -> Maybe RuntimeValue -> [Statement] -> Either Text (Maybe RuntimeValue)
    go env lastExprValue remainingStatements =
      case remainingStatements of
        [] ->
          -- Declaration-only scopes intentionally remain `Nothing` until a terminal `SExpr` sets a value.
          Right lastExprValue
        statement : rest ->
          case statement of
            SSignature {} ->
              go env Nothing rest
            SModule {} ->
              go env Nothing rest
            SImport {} ->
              go env Nothing rest
            SLet name _ valueExpr -> do
              value <- evalValue env valueExpr
              go (Map.insert name value env) Nothing rest
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
          case lookupBuiltinSymbol name of
            Just builtinFunction -> Right (VBuiltin builtinFunction [])
            Nothing ->
              Left
                ( "E3002: runtime unbound variable '"
                    <> name
                    <> "'"
                )
    EList elements ->
      VList <$> mapM (evalValue env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValue env functionExpr
      argumentValue <- evalValue env argumentExpr
      applyRuntimeFunction functionValue argumentValue
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
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValue env leftExpr
      Right (VSectionLeft operatorSymbol leftValue)
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValue env rightExpr
      Right (VSectionRight operatorSymbol rightValue)
    EScope statements ->
      case evalScope env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            "E3006: scope expression has no terminal expression result at runtime"
        Right (Just value) -> Right value

applyRuntimeFunction :: RuntimeValue -> RuntimeValue -> Either Text RuntimeValue
applyRuntimeFunction functionValue argumentValue =
  case functionValue of
    VSectionLeft operatorSymbol leftValue ->
      evalBinary operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary operatorSymbol argumentValue rightValue
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinFunction (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( "E3008: runtime cannot apply non-function value of type "
            <> renderRuntimeType functionValue
            <> ""
        )

applyBuiltin :: BuiltinSymbol -> [RuntimeValue] -> Either Text RuntimeValue
applyBuiltin builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinFunction arguments
  | otherwise =
      Left
        ( "E3014: runtime primitive '"
            <> builtinSymbolName builtinFunction
            <> "' received too many arguments"
        )

evalBuiltin :: BuiltinSymbol -> [RuntimeValue] -> Either Text RuntimeValue
evalBuiltin builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinHd, [VList []]) ->
      Left "E3009: runtime primitive 'hd' failed: empty list"
    (BuiltinHd, [VList (headValue : _)]) ->
      Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( "E3011: runtime primitive 'hd' expects a list argument, found "
            <> renderRuntimeType other
        )
    (BuiltinTl, [VList []]) ->
      Left "E3010: runtime primitive 'tl' failed: empty list"
    (BuiltinTl, [VList (_ : tailValues)]) ->
      Right (VList tailValues)
    (BuiltinTl, [other]) ->
      Left
        ( "E3012: runtime primitive 'tl' expects a list argument, found "
            <> renderRuntimeType other
        )
    (BuiltinMap, [mapper, collection])
      | not (isFunctionValue mapper) ->
          Left
            ( "E3015: runtime primitive 'map' expects a function as its first argument, found "
                <> renderRuntimeType mapper
            )
      | otherwise ->
          case collection of
            VList elements ->
              VList <$> mapM (applyRuntimeFunction mapper) elements
            other ->
              Left
                ( "E3013: runtime primitive 'map' expects a list as its second argument, found "
                    <> renderRuntimeType other
                )
    -- Stub-v1 keeps `print!` side effects out of runtime plumbing; it returns
    -- its evaluated argument so expression pipelines remain deterministic.
    (BuiltinPrint, [value]) ->
      Right value
    _ ->
      Left
        ( "E3016: runtime primitive '"
            <> builtinSymbolName builtinFunction
            <> "' received invalid arguments"
        )

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VBuiltin {} -> True
    _ -> False

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
    VList {} -> "List"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VBuiltin {} -> "Function"
