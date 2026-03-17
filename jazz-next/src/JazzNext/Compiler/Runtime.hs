{-# LANGUAGE OverloadedStrings #-}

-- | Small interpreter/runtime for the currently-supported core language. It is
-- intentionally simple and mirrors the same builtin/operator contracts enforced
-- by analysis and type inference.
module JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    renderRuntimeValue
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbolInMode
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
  )

-- | Runtime values produced by the interpreter, including partially applied
-- builtins/operators.
data RuntimeValue
  = VInt Int
  | VBool Bool
  | VList [RuntimeValue]
  | VClosure RuntimeEnv Identifier Expr
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  deriving (Eq, Show)

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = evaluateRuntimeExprWithBuiltins ResolveKernelOnly

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  case expr of
    EBlock statements -> evalScope builtinMode Map.empty statements
    _ -> Just <$> evalValue builtinMode Map.empty expr

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
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"

type RuntimeEnv = Map Text RuntimeValue

-- | Evaluate a block scope in order. Declarations clear `lastExprValue`, so
-- `evalScope` returns `Just` only when the final surviving statement is an
-- `SExpr`; otherwise the block yields `Nothing`.
evalScope :: BuiltinResolutionMode -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScope builtinMode initialEnv statements = go initialEnv Nothing statements
  where
    go :: RuntimeEnv -> Maybe RuntimeValue -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
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
              value <- evalValue builtinMode env valueExpr
              go (Map.insert (identifierText name) value env) Nothing rest
            SExpr _ expr -> do
              value <- evalValue builtinMode env expr
              go env (Just value) rest

evalValue builtinMode env expr =
  case expr of
    ELit literal -> Right (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup nameText env of
        Just value -> Right value
        Nothing ->
          case lookupBuiltinSymbolInMode builtinMode nameText of
            Just builtinFunction -> Right (VBuiltin builtinFunction [])
            Nothing ->
              Left
                ( runtimeDiagnostic
                    "E3002"
                    ("runtime unbound variable '" <> nameText <> "'")
                )
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      Right (VClosure env parameterName bodyExpr)
    EOperatorValue operatorSymbol ->
      Right (VOperator operatorSymbol [])
    EList elements ->
      VList <$> mapM (evalValue builtinMode env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValue builtinMode env functionExpr
      argumentValue <- evalValue builtinMode env argumentExpr
      applyRuntimeFunction builtinMode functionValue argumentValue
    EIf conditionExpr thenExpr elseExpr ->
      evalValue builtinMode env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValue builtinMode env conditionExpr
      case conditionValue of
        VBool True -> evalValue builtinMode env thenExpr
        VBool False -> evalValue builtinMode env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )
    EBinary operatorSymbol leftExpr rightExpr -> do
      leftValue <- evalValue builtinMode env leftExpr
      rightValue <- evalValue builtinMode env rightExpr
      evalBinary builtinMode operatorSymbol leftValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValue builtinMode env leftExpr
      Right (VSectionLeft operatorSymbol leftValue)
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValue builtinMode env rightExpr
      Right (VSectionRight operatorSymbol rightValue)
    EBlock statements ->
      case evalScope builtinMode env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
        Right (Just value) -> Right value

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value
    LBool value -> VBool value

-- | Apply any callable runtime value, including sections, builtin primitives,
-- and curried operator values.
applyRuntimeFunction :: BuiltinResolutionMode -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunction builtinMode functionValue argumentValue =
  case functionValue of
    VSectionLeft operatorSymbol leftValue ->
      evalBinary builtinMode operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary builtinMode operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr ->
      evalValue
        builtinMode
        (Map.insert (identifierText parameterName) argumentValue capturedEnv)
        bodyExpr
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinMode builtinFunction (capturedArgs ++ [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperator builtinMode operatorSymbol (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( runtimeDiagnostic
            "E3008"
            ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue)
        )

applyOperator :: BuiltinResolutionMode -> Text -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyOperator builtinMode operatorSymbol arguments =
  case arguments of
    [leftValue] ->
      Right (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinary builtinMode operatorSymbol leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments")
        )

-- | Builtin primitives are curried, so under-applied calls stay as function
-- values and only exact arity triggers evaluation.
applyBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyBuiltin builtinMode builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinMode builtinFunction arguments
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3014"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments")
        )

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltin builtinMode builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinHd, [VList []]) ->
      Left (runtimeDiagnostic "E3009" "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _)]) ->
      Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3011"
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList []]) ->
      Left (runtimeDiagnostic "E3010" "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues)]) ->
      Right (VList tailValues)
    (BuiltinTl, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3012"
            ("runtime primitive 'tl' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinMap, [mapper, collection])
      | not (isFunctionValue mapper) ->
          Left
            ( runtimeDiagnostic
                "E3015"
                ("runtime primitive 'map' expects a function as its first argument, found " <> renderRuntimeType mapper)
            )
      | otherwise ->
          case collection of
            VList elements ->
              VList <$> mapM (applyRuntimeFunction builtinMode mapper) elements
            other ->
              Left
                ( runtimeDiagnostic
                    "E3013"
                    ("runtime primitive 'map' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    (BuiltinFilter, [predicate, collection])
      | not (isFunctionValue predicate) ->
          Left
            ( runtimeDiagnostic
                "E3017"
                ("runtime primitive 'filter' expects a function as its first argument, found " <> renderRuntimeType predicate)
            )
      | otherwise ->
          case collection of
            VList elements ->
              VList <$> filterElements builtinMode predicate elements
            other ->
              Left
                ( runtimeDiagnostic
                    "E3018"
                    ("runtime primitive 'filter' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    -- Stub-v1 keeps `print!` side effects out of runtime plumbing; it returns
    -- its evaluated argument so expression pipelines remain deterministic.
    (BuiltinPrint, [value]) ->
      Right value
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received invalid arguments")
        )

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements :: BuiltinResolutionMode -> RuntimeValue -> [RuntimeValue] -> Either Diagnostic [RuntimeValue]
filterElements builtinMode predicate values = do
  results <- mapM applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate :: RuntimeValue -> Either Diagnostic (RuntimeValue, Bool)
    applyPredicate value = do
      predicateResult <- applyRuntimeFunction builtinMode predicate value
      case predicateResult of
        VBool shouldKeep -> Right (value, shouldKeep)
        other ->
          Left
            ( runtimeDiagnostic
                "E3019"
                ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
            )

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    _ -> False

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary :: BuiltinResolutionMode -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode operatorSymbol leftValue rightValue =
  case (operatorSymbol, leftValue, rightValue) of
    ("+", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt + rightInt))
    ("-", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt - rightInt))
    ("*", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt * rightInt))
    ("/", VInt _, VInt 0) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
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
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunction builtinMode functionValue argumentValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot be applied to "
                <> renderRuntimeType leftValue
                <> " and "
                <> renderRuntimeType rightValue
            )
        )

-- | Runtime-specific wrapper for mkDiagnostic.
-- This alias exists solely to improve readability and make it clear that
-- diagnostics are being created in a runtime evaluation context rather than
-- during parsing or type checking.
runtimeDiagnostic :: Text -> Text -> Diagnostic
runtimeDiagnostic = mkDiagnostic

-- | Render coarse runtime type names for diagnostics.
renderRuntimeType :: RuntimeValue -> Text
renderRuntimeType value =
  case value of
    VInt {} -> "Int"
    VBool {} -> "Bool"
    VList {} -> "List"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
