{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Builtin and operator dispatch over pure runtime values. The only evaluator
-- capability admitted here is explicit callable application for map, filter,
-- and dollar application.
module Jazz.Compiler.Runtime.Primitives
  ( RuntimeApplication,
    evalBuiltin,
    evalBinary
  ) where

import Control.Monad.Trans.Except
  ( ExceptT,
    throwE
  )
import Data.Char
  ( GeneralCategory (DecimalNumber),
    chr,
    generalCategory,
    isAlpha,
    isAlphaNum,
    isHexDigit,
    isLower,
    isSpace,
    isUpper,
    ord,
    toLower,
    toUpper
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Expr (..),
    NumericType (..),
    SignatureType (..)
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
    renderNumericTypeName
  )
import Jazz.Compiler.CapabilityFacts
  ( constraintSignatureTypesCompatible
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (..))
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.Runtime.Semantics
  ( applyRuntimeTypeHint,
    evalNumericConversion,
    convertIntegerToFloatTarget,
    exceedsFloatTarget,
    integerValueWithinBounds,
    integerValueMatchesTarget,
    isFunctionValue,
    renderRuntimeValue,
    renderRuntimeType,
    runtimeDiagnostic,
    runtimeIntMatchesTarget,
    runtimeValueMatchesConstraint,
    numericConversionFloatOverflowDiagnostic,
    roundFloatTarget,
    targetedFloatMetadata,
    targetedIntMetadata,
    untypedFloatMetadata,
    untypedIntMetadata
  )
import Jazz.Compiler.Runtime.Types
  ( RuntimeClosure (..),
    RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeValue (..),
    constructorIsSaturated,
    foldrRuntimeConstructorArguments,
    data VExplicitResultHints
  )

-- | The only evaluator capability needed by primitive value semantics.
type RuntimeApplication error m =
  RuntimeValue -> RuntimeValue -> ExceptT error m RuntimeValue

liftRuntimeResult :: Monad m => (Diagnostic -> error) -> Either Diagnostic value -> ExceptT error m value
liftRuntimeResult injectDiagnostic result =
  case result of
    Left diagnostic -> throwE (injectDiagnostic diagnostic)
    Right value -> pure value

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin ::
  Monad m =>
  (Diagnostic -> error) ->
  RuntimeApplication error m ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT error m RuntimeValue
evalBuiltin injectDiagnostic applyRuntimeValue builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinMap, [mapper, collection])
      | not (isFunctionValue mapper) ->
          throwE
            ( injectDiagnostic
                ( runtimeDiagnostic
                    E3015
                    ("runtime primitive 'map' expects a function as its first argument, found " <> renderRuntimeType mapper)
                )
            )
      | otherwise ->
          case collection of
            VList elements maybeCollectionTypeHint -> do
              mappedElements <- traverse (applyRuntimeValue mapper) elements
              let maybeMappedTypeHint = TypeList <$> runtimeMapResultElementType mapper maybeCollectionTypeHint
              pure (VList mappedElements maybeMappedTypeHint)
            other ->
              throwE
                ( injectDiagnostic
                    ( runtimeDiagnostic
                        E3013
                        ("runtime primitive 'map' expects a list as its second argument, found " <> renderRuntimeType other)
                    )
                )
    (BuiltinFilter, [predicate, collection])
      | not (isFunctionValue predicate) ->
          throwE
            ( injectDiagnostic
                ( runtimeDiagnostic
                    E3017
                    ("runtime primitive 'filter' expects a function as its first argument, found " <> renderRuntimeType predicate)
                )
            )
      | otherwise ->
          case collection of
            VList elements maybeTypeHint ->
              (`VList` maybeTypeHint) <$> filterElements injectDiagnostic applyRuntimeValue predicate elements
            other ->
              throwE
                ( injectDiagnostic
                    ( runtimeDiagnostic
                        E3018
                        ("runtime primitive 'filter' expects a list as its second argument, found " <> renderRuntimeType other)
                    )
                )
    _ -> liftRuntimeResult injectDiagnostic (evalBuiltinPure builtinFunction arguments)

evalBuiltinPure :: BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltinPure builtinFunction arguments =
  case (builtinFunction, arguments) of
    (_, [value])
      | Just targetType <- builtinSymbolNumericConversionTarget builtinFunction ->
          evalNumericConversion builtinFunction targetType value
    (BuiltinHd, [VList [] _]) ->
      Left (runtimeDiagnostic E3009 "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _) maybeTypeHint]) ->
      case maybeTypeHint of
        Just (TypeList elementType) ->
          applyRuntimeTypeHint elementType headValue
        _ ->
          Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            E3011
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList [] _]) ->
      Left (runtimeDiagnostic E3010 "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues) maybeTypeHint]) ->
      Right (VList tailValues maybeTypeHint)
    (BuiltinTl, [other]) ->
      Left
        ( runtimeDiagnostic
            E3012
            ("runtime primitive 'tl' expects a list argument, found " <> renderRuntimeType other)
        )
    -- Stub-v1 keeps `print!` side effects out of runtime plumbing; it returns
    -- its evaluated argument so expression pipelines remain deterministic.
    (BuiltinPrint, [value]) ->
      Right value
    (BuiltinListPrependRaw, [value, VList elements maybeTypeHint]) ->
      case maybeTypeHint of
        Just (TypeList elementType) -> do
          hintedValue <- applyRuntimeTypeHint elementType value
          Right (VList (hintedValue : elements) maybeTypeHint)
        _ ->
          Right (VList (value : elements) maybeTypeHint)
    (BuiltinListPrependRaw, [_, other]) ->
      Left
        ( runtimeDiagnostic
            E3032
            ("runtime primitive 'listPrependRaw' expects a list as its second argument, found " <> renderRuntimeType other)
        )
    (BuiltinListReverseRaw, [VList elements maybeTypeHint]) ->
      Right (VList (reverse elements) maybeTypeHint)
    (BuiltinListReverseRaw, [other]) ->
      Left
        ( runtimeDiagnostic
            E3038
            ("runtime primitive 'listReverseRaw' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinCharToUInt32, [VChar value]) ->
      Right (VInt (fromIntegral (ord value)) (targetedIntMetadata NumericUInt32))
    (BuiltinCharToUInt32, [other]) ->
      Left
        ( runtimeDiagnostic
            E3033
            ("runtime primitive 'charToUInt32' expects a Char argument, found " <> renderRuntimeType other)
        )
    (BuiltinCharFromUInt32Raw, [value@(VInt scalar _)])
      | runtimeIntMatchesTarget NumericUInt32 value ->
          let listTypeHint = Just (TypeList TypeChar)
           in
            if scalar <= 0x10FFFF && not (scalar >= 0xD800 && scalar <= 0xDFFF)
              then Right (VList [VChar (chr (fromInteger scalar))] listTypeHint)
              else Right (VList [] listTypeHint)
    (BuiltinCharFromUInt32Raw, [other]) ->
      Left
        ( runtimeDiagnostic
            E3034
            ("runtime primitive 'charFromUInt32Raw' expects a UInt32 argument, found " <> renderRuntimeType other)
        )
    (BuiltinCharIsAlpha, [VChar value]) -> Right (VBool (isAlpha value))
    (BuiltinCharIsAlphaNum, [VChar value]) -> Right (VBool (isAlphaNum value))
    (BuiltinCharIsDigit, [VChar value]) ->
      Right (VBool (generalCategory value == DecimalNumber))
    (BuiltinCharIsSpace, [VChar value]) -> Right (VBool (isSpace value))
    (BuiltinCharIsHexDigit, [VChar value]) -> Right (VBool (isHexDigit value))
    (BuiltinCharIsLower, [VChar value]) -> Right (VBool (isLower value))
    (BuiltinCharIsUpper, [VChar value]) -> Right (VBool (isUpper value))
    (BuiltinCharToLower, [VChar value]) -> Right (VChar (toLower value))
    (BuiltinCharToUpper, [VChar value]) -> Right (VChar (toUpper value))
    (builtin@BuiltinCharIsAlpha, [other]) -> invalidCharPredicate builtin other
    (builtin@BuiltinCharIsAlphaNum, [other]) -> invalidCharPredicate builtin other
    (builtin@BuiltinCharIsDigit, [other]) -> invalidCharPredicate builtin other
    (builtin@BuiltinCharIsSpace, [other]) -> invalidCharPredicate builtin other
    (builtin@BuiltinCharIsHexDigit, [other]) -> invalidCharPredicate builtin other
    (builtin@BuiltinCharIsLower, [other]) -> invalidCharPrimitive builtin other
    (builtin@BuiltinCharIsUpper, [other]) -> invalidCharPrimitive builtin other
    (builtin@BuiltinCharToLower, [other]) -> invalidCharPrimitive builtin other
    (builtin@BuiltinCharToUpper, [other]) -> invalidCharPrimitive builtin other
    (BuiltinTextLength, [VText textValue]) ->
      Right (VInt (fromIntegral (Text.length textValue)) untypedIntMetadata)
    (BuiltinTextLength, [other]) ->
      Left
        ( runtimeDiagnostic
            E3028
            ("runtime primitive 'textLength' expects a Text argument, found " <> renderRuntimeType other)
        )
    (BuiltinTextUnconsRaw, [VText textValue]) ->
      let listTypeHint = Just (TypeList (TypeTuple [TypeChar, TypeText]))
       in
        case Text.uncons textValue of
          Nothing ->
            Right (VList [] listTypeHint)
          Just (first, rest) ->
            Right (VList [VTuple [VChar first, VText rest]] listTypeHint)
    (BuiltinTextUnconsRaw, [other]) ->
      Left
        ( runtimeDiagnostic
            E3029
            ("runtime primitive 'textUnconsRaw' expects a Text argument, found " <> renderRuntimeType other)
        )
    (BuiltinTextAppend, [VText left, VText right]) ->
      Right (VText (left <> right))
    (BuiltinTextAppend, [left, right]) ->
      Left
        ( runtimeDiagnostic
            E3036
            ( "runtime primitive 'textAppend' expects Text arguments, found "
                <> renderRuntimeType left
                <> " and "
                <> renderRuntimeType right
            )
        )
    (BuiltinTextAppendChar, [VText textValue, VChar charValue]) ->
      Right (VText (Text.snoc textValue charValue))
    (BuiltinTextAppendChar, [textValue, charValue]) ->
      Left
        ( runtimeDiagnostic
            E3037
            ( "runtime primitive 'textAppendChar' expects Text then Char, found "
                <> renderRuntimeType textValue
                <> " and "
                <> renderRuntimeType charValue
            )
        )
    (BuiltinTextFromChars, [VList elements _]) ->
      case traverse runtimeChar elements of
        Just chars -> Right (VText (Text.pack chars))
        Nothing ->
          Left
            ( runtimeDiagnostic
                E3039
                "runtime primitive 'textFromChars' expects a list containing only Char values"
            )
    (BuiltinTextFromChars, [other]) ->
      Left
        ( runtimeDiagnostic
            E3039
            ("runtime primitive 'textFromChars' expects a list of Char, found " <> renderRuntimeType other)
        )
    (BuiltinTextConcat, [VList elements _]) ->
      case traverse runtimeText elements of
        Just fragments -> Right (VText (Text.concat fragments))
        Nothing ->
          Left
            ( runtimeDiagnostic
                E3040
                "runtime primitive 'textConcat' expects a list containing only Text values"
            )
    (BuiltinTextConcat, [other]) ->
      Left
        ( runtimeDiagnostic
            E3040
            ("runtime primitive 'textConcat' expects a list of Text, found " <> renderRuntimeType other)
        )
    (BuiltinRenderValue, [value]) ->
      Right (VText (renderRuntimeValue value))
    _ ->
      Left
        ( runtimeDiagnostic
            E3016
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received invalid arguments")
        )

invalidCharPredicate :: BuiltinSymbol -> RuntimeValue -> Either Diagnostic RuntimeValue
invalidCharPredicate = invalidCharPrimitive

invalidCharPrimitive :: BuiltinSymbol -> RuntimeValue -> Either Diagnostic RuntimeValue
invalidCharPrimitive builtin other =
  Left
    ( runtimeDiagnostic
        E3035
        ( "runtime primitive '"
            <> builtinSymbolName builtin
            <> "' expects a Char argument, found "
            <> renderRuntimeType other
        )
    )

runtimeChar :: RuntimeValue -> Maybe Char
runtimeChar runtimeValue =
  case runtimeValue of
    VChar value -> Just value
    VTyped _ innerValue -> runtimeChar innerValue
    VExplicitTypeApplication _ innerValue -> runtimeChar innerValue
    VExplicitResultHints _ innerValue -> runtimeChar innerValue
    _ -> Nothing

runtimeText :: RuntimeValue -> Maybe Text
runtimeText runtimeValue =
  case runtimeValue of
    VText value -> Just value
    VTyped _ innerValue -> runtimeText innerValue
    VExplicitTypeApplication _ innerValue -> runtimeText innerValue
    VExplicitResultHints _ innerValue -> runtimeText innerValue
    _ -> Nothing

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements ::
  Monad m =>
  (Diagnostic -> error) ->
  RuntimeApplication error m ->
  RuntimeValue ->
  [RuntimeValue] ->
  ExceptT error m [RuntimeValue]
filterElements injectDiagnostic applyRuntimeValue predicate values = do
  results <- traverse applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate value = do
      predicateResult <- applyRuntimeValue predicate value
      case predicateResult of
        VBool shouldKeep -> pure (value, shouldKeep)
        other ->
          throwE
            ( injectDiagnostic
                ( runtimeDiagnostic
                    E3019
                    ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
                )
            )

runtimeFunctionResultType :: RuntimeValue -> Maybe SignatureType
runtimeFunctionResultType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeFunctionResultType innerValue
    VExplicitResultHints _ innerValue ->
      runtimeFunctionResultType innerValue
    VTyped (TypeFunction _ resultType) _ ->
      Just resultType
    VClosure closure
      | Just (TypeFunction _ resultType) <- runtimeClosureTypeHint closure ->
          Just resultType
    _ ->
      Nothing

runtimeMapResultElementType :: RuntimeValue -> Maybe SignatureType -> Maybe SignatureType
runtimeMapResultElementType mapper maybeCollectionTypeHint =
  case runtimeFunctionResultType mapper of
    Just resultType ->
      Just resultType
    Nothing ->
      runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint

runtimeBuiltinMapResultElementType :: RuntimeValue -> Maybe SignatureType -> Maybe SignatureType
runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint =
  case (mapper, maybeCollectionTypeHint) of
    (VBuiltin BuiltinHd [], Just (TypeList (TypeList elementType))) ->
      Just elementType
    (VClosure closure, Just (TypeList elementType))
      | EVar resultName <- runtimeClosureBody closure,
        Nothing <- runtimeClosureTypeHint closure,
        resultName == runtimeClosureParameter closure ->
          Just elementType
    _ ->
      Nothing

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary ::
  Monad m =>
  (Diagnostic -> error) ->
  RuntimeApplication error m ->
  Text ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT error m RuntimeValue
evalBinary injectDiagnostic applyRuntimeValue operatorSymbol leftValue rightValue
  | operatorSymbol == "$" = applyRuntimeValue leftValue rightValue
  | otherwise = liftRuntimeResult injectDiagnostic (evalBinaryPure operatorSymbol leftValue rightValue)

evalBinaryPure :: Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinaryPure operatorSymbol leftValue rightValue
  | isStrictEqualityOperator operatorSymbol,
    isFunctionValue leftValue || isFunctionValue rightValue =
      Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
  | otherwise =
  case (operatorSymbol, leftValue, rightValue) of
    (_, VTyped leftTypeHint leftInnerValue, _)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality leftTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          preserveLeftTypedNumericOperatorResult operatorSymbol leftTypeHint
            =<< evalBinaryPure operatorSymbol leftInnerValue rightValue
    (_, _, VTyped rightTypeHint rightInnerValue)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality rightTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          preserveRightTypedNumericOperatorResult operatorSymbol leftValue rightTypeHint
            =<< evalBinaryPure operatorSymbol leftValue rightInnerValue
    ("+", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "+" leftMetadata rightMetadata (leftInt + rightInt)
    ("-", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "-" leftMetadata rightMetadata (leftInt - rightInt)
    ("*", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "*" leftMetadata rightMetadata (leftInt * rightInt)
    ("/", VInt _ _, VInt 0 _) ->
      Left (runtimeDiagnostic E3001 "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "/" leftMetadata rightMetadata (leftInt `div` rightInt)
    ("+", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "+" leftMetadata rightMetadata (leftFloat + rightFloat)
    ("-", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "-" leftMetadata rightMetadata (leftFloat - rightFloat)
    ("*", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "*" leftMetadata rightMetadata (leftFloat * rightFloat)
    ("/", VFloat _ _, VFloat rightFloat _)
      | floatIsZero rightFloat ->
          Left (runtimeDiagnostic E3001 "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "/" leftMetadata rightMetadata (leftFloat / rightFloat)
    ("+", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "+" rightMetadata leftInt rightFloat (+)
    ("+", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "+" leftMetadata leftFloat rightInt (+)
    ("-", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "-" rightMetadata leftInt rightFloat (-)
    ("-", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "-" leftMetadata leftFloat rightInt (-)
    ("*", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "*" rightMetadata leftInt rightFloat (*)
    ("*", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "*" leftMetadata leftFloat rightInt (*)
    ("/", VInt _ leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata,
        floatIsZero rightFloat ->
          Left (runtimeDiagnostic E3001 "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "/" rightMetadata leftInt rightFloat (/)
    ("/", VFloat _ leftMetadata, VInt 0 rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          Left (runtimeDiagnostic E3001 "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "/" leftMetadata leftFloat rightInt (/)
    ("<", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<" leftInt leftMetadata rightInt rightMetadata (leftInt < rightInt)
    ("<=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<=" leftInt leftMetadata rightInt rightMetadata (leftInt <= rightInt)
    (">", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">" leftInt leftMetadata rightInt rightMetadata (leftInt > rightInt)
    (">=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">=" leftInt leftMetadata rightInt rightMetadata (leftInt >= rightInt)
    ("<", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<" leftMetadata rightMetadata (leftFloat < rightFloat)
    ("<=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<=" leftMetadata rightMetadata (leftFloat <= rightFloat)
    (">", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">" leftMetadata rightMetadata (leftFloat > rightFloat)
    (">=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">=" leftMetadata rightMetadata (leftFloat >= rightFloat)
    ("<", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<" leftInt rightFloat (<)
    ("<", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<" leftFloat rightInt (<)
    ("<=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<=" leftInt rightFloat (<=)
    ("<=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<=" leftFloat rightInt (<=)
    (">", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">" leftInt rightFloat (>)
    (">", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">" leftFloat rightInt (>)
    (">=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">=" leftInt rightFloat (>=)
    (">=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">=" leftFloat rightInt (>=)
    ("==", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "==" leftInt leftMetadata rightInt rightMetadata
    ("==", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "==" leftMetadata rightMetadata (leftFloat == rightFloat)
    ("==", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "==" leftInt rightFloat
    ("==", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "==" leftFloat rightInt
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("==", VChar leftChar, VChar rightChar) -> Right (VBool (leftChar == rightChar))
    ("==", VText leftText, VText rightText) -> Right (VBool (leftText == rightText))
    ("==", VList {}, VList {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VTuple {}, VTuple {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VConstructorApplication {}, VConstructorApplication {}) -> evalStructuralEquality "==" leftValue rightValue
    ("!=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "!=" leftInt leftMetadata rightInt rightMetadata
    ("!=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "!=" leftMetadata rightMetadata (leftFloat /= rightFloat)
    ("!=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "!=" leftInt rightFloat
    ("!=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "!=" leftFloat rightInt
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    ("!=", VChar leftChar, VChar rightChar) -> Right (VBool (leftChar /= rightChar))
    ("!=", VText leftText, VText rightText) -> Right (VBool (leftText /= rightText))
    ("!=", VList {}, VList {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VTuple {}, VTuple {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VConstructorApplication {}, VConstructorApplication {}) -> evalStructuralEquality "!=" leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot be applied to "
                <> renderRuntimeType leftValue
                <> " and "
                <> renderRuntimeType rightValue
            )
        )

isStrictEqualityOperator :: Text -> Bool
isStrictEqualityOperator operatorSymbol =
  operatorSymbol == "==" || operatorSymbol == "!="

preserveLeftTypedNumericOperatorResult :: Text -> SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveLeftTypedNumericOperatorResult operatorSymbol typeHint runtimeValue
  | numericArithmeticOperator operatorSymbol,
    numericAliasTypeHint typeHint,
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

preserveRightTypedNumericOperatorResult :: Text -> RuntimeValue -> SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveRightTypedNumericOperatorResult operatorSymbol leftValue typeHint runtimeValue
  | numericArithmeticOperator operatorSymbol,
    numericAliasTypeHint typeHint,
    not (runtimeValueHasTargetedNumericMetadata leftValue),
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

numericArithmeticOperator :: Text -> Bool
numericArithmeticOperator operatorSymbol =
  operatorSymbol == "+" || operatorSymbol == "-" || operatorSymbol == "*" || operatorSymbol == "/"

numericAliasTypeHint :: SignatureType -> Bool
numericAliasTypeHint typeHint =
  case typeHint of
    TypeInt -> True
    TypeFloat -> True
    TypeName typeName ->
      identifierText typeName == "Int" || identifierText typeName == "Float"
    _ ->
      False

runtimeValueHasTargetedNumericMetadata :: RuntimeValue -> Bool
runtimeValueHasTargetedNumericMetadata runtimeValue =
  case runtimeValue of
    VInt _ metadata ->
      runtimeIntTargetType metadata /= Nothing
    VFloat _ metadata ->
      runtimeFloatTargetType metadata /= Nothing
    _ ->
      False

runtimeTypeHintRequiresStructuralEquality :: SignatureType -> Bool
runtimeTypeHintRequiresStructuralEquality signatureType =
  case signatureType of
    TypeApplication {} -> True
    TypeList {} -> True
    TypeTuple {} -> True
    _ -> False

runtimeCallableEqualityDiagnostic :: Text -> RuntimeValue -> RuntimeValue -> Diagnostic
runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot compare callable values; callable values are not equality-supported, found "
        <> renderRuntimeType leftValue
        <> " and "
        <> renderRuntimeType rightValue
    )

evalIntegerArithmetic ::
  Text ->
  RuntimeIntMetadata ->
  RuntimeIntMetadata ->
  Integer ->
  Either Diagnostic RuntimeValue
evalIntegerArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalIntegerBinary operatorSymbol targetType result

selectIntegerBinaryTarget :: Text -> RuntimeIntMetadata -> RuntimeIntMetadata -> Either Diagnostic (Maybe NumericType)
selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedIntegerArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just leftTarget, Nothing) -> Right (Just leftTarget)
    (Nothing, Just rightTarget) -> Right (Just rightTarget)
    _ -> Right Nothing

evalIntegerBinary :: Text -> Maybe NumericType -> Integer -> Either Diagnostic RuntimeValue
evalIntegerBinary operatorSymbol maybeTarget result =
  case maybeTarget of
    Just targetType ->
      case numericTypeIntegerBounds targetType of
        Just bounds
          | integerValueWithinBounds result bounds ->
              Right (VInt result (targetedIntMetadata targetType))
          | otherwise ->
              Left (runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result bounds)
        Nothing ->
          Right (VInt result (targetedIntMetadata targetType))
    Nothing ->
      Right (VInt result untypedIntMetadata)

mixedIntegerArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedIntegerArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderIntegerOperandTarget leftTarget
        <> " and "
        <> renderIntegerOperandTarget rightTarget
    )

renderIntegerOperandTarget :: Maybe NumericType -> Text
renderIntegerOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Int"

runtimeIntegerArithmeticOverflowDiagnostic :: Text -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result (lowerBound, upperBound) =
  runtimeDiagnostic
    E3025
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: integer value "
        <> Text.pack (show result)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

floatIsZero :: Double -> Bool
floatIsZero value =
  -- Jazz's finite runtime primitive subset treats both signed zeroes as
  -- division by zero rather than producing infinities.
  value == 0

evalFloatArithmetic ::
  Text ->
  RuntimeFloatMetadata ->
  RuntimeFloatMetadata ->
  Double ->
  Either Diagnostic RuntimeValue
evalFloatArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalFloatBinary operatorSymbol targetType result

selectFloatBinaryTarget :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Either Diagnostic (Maybe NumericType)
selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just NumericFloat64, Nothing) -> Right (Just NumericFloat64)
    (Nothing, Just NumericFloat64) -> Right (Just NumericFloat64)
    (Just targetType, Nothing) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just targetType) Nothing)
    (Nothing, Just targetType) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol Nothing (Just targetType))
    (Nothing, Nothing) -> Right Nothing

evalFloatBinary :: Text -> Maybe NumericType -> Double -> Either Diagnostic RuntimeValue
evalFloatBinary operatorSymbol targetType result
  | isNaN result || isInfinite result =
      Left
        ( runtimeDiagnostic
            E3025
            ("runtime primitive '" <> operatorSymbol <> "' failed: non-finite Float result")
        )
  | Just floatTarget <- targetType,
    exceedsFloatTarget floatTarget result =
      Left (runtimeFloatArithmeticOverflowDiagnostic operatorSymbol floatTarget)
  | Just floatTarget <- targetType =
      Right (VFloat (roundFloatTarget floatTarget result) (targetedFloatMetadata floatTarget))
  | otherwise = Right (VFloat result (untypedFloatMetadata Nothing))

runtimeIntFloat64PromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64PromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntFloat64ComparisonPromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64ComparisonPromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntMetadataIsIntegral :: RuntimeIntMetadata -> Bool
runtimeIntMetadataIsIntegral intMetadata =
  case runtimeIntTargetType intMetadata of
    Just numericType -> numericTypeIsIntegral numericType
    Nothing -> True

runtimeFloatMetadataIsFloat64Domain :: RuntimeFloatMetadata -> Bool
runtimeFloatMetadataIsFloat64Domain floatMetadata =
  case runtimeFloatTargetType floatMetadata of
    Just NumericFloat64 -> True
    Nothing -> True
    Just _ -> False

evalIntegerFloat64Arithmetic :: Text -> RuntimeFloatMetadata -> Integer -> Double -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Arithmetic operatorSymbol floatMetadata integerValue floatValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine integerFloat floatValue)

evalFloat64IntegerArithmetic :: Text -> RuntimeFloatMetadata -> Double -> Integer -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalFloat64IntegerArithmetic operatorSymbol floatMetadata floatValue integerValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine floatValue integerFloat)

evalIntegerFloat64Predicate :: Text -> Integer -> Double -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Predicate _ integerValue floatValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate integerFloat floatValue))

evalFloat64IntegerPredicate :: Text -> Double -> Integer -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalFloat64IntegerPredicate _ floatValue integerValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate floatValue integerFloat))

evalIntegerFloat64Equality :: Text -> Integer -> Double -> Either Diagnostic RuntimeValue
evalIntegerFloat64Equality operatorSymbol integerValue floatValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol integerFloat floatValue))

evalFloat64IntegerEquality :: Text -> Double -> Integer -> Either Diagnostic RuntimeValue
evalFloat64IntegerEquality operatorSymbol floatValue integerValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol floatValue integerFloat))

float64MixedEqualityResult :: Text -> Double -> Double -> Bool
float64MixedEqualityResult operatorSymbol leftValue rightValue =
  if operatorSymbol == "!="
    then leftValue /= rightValue
    else leftValue == rightValue

promotedIntegerFloat64Operand :: Integer -> Either Diagnostic Double
promotedIntegerFloat64Operand integerValue =
  case convertIntegerToFloatTarget BuiltinToFloat64 NumericFloat64 integerValue of
    Right (VFloat floatValue _) -> Right floatValue
    Right _ -> Left (numericConversionFloatOverflowDiagnostic BuiltinToFloat64 NumericFloat64)
    Left diagnostic -> Left diagnostic

mixedFloatArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedFloatArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderFloatOperandTarget leftTarget
        <> " and "
        <> renderFloatOperandTarget rightTarget
    )

renderFloatOperandTarget :: Maybe NumericType -> Text
renderFloatOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Float"

runtimeFloatArithmeticOverflowDiagnostic :: Text -> NumericType -> Diagnostic
runtimeFloatArithmeticOverflowDiagnostic operatorSymbol targetType =
  runtimeDiagnostic
    E3025
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

evalStructuralEquality :: Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalStructuralEquality operatorSymbol leftValue rightValue =
  if runtimeValueContainsFunction leftValue || runtimeValueContainsFunction rightValue
    then Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
    else
      case runtimeStructuralEquality leftValue rightValue of
        Just equalityResult ->
          Right
            ( VBool
                ( if operatorSymbol == "!="
                    then not equalityResult
                    else equalityResult
                )
            )
        Nothing ->
          Left
            ( runtimeDiagnostic
                E3007
                ( "runtime primitive '"
                    <> operatorSymbol
                    <> "' cannot be applied to "
                    <> renderRuntimeType leftValue
                    <> " and "
                    <> renderRuntimeType rightValue
                )
            )

runtimeValueContainsFunction :: RuntimeValue -> Bool
runtimeValueContainsFunction value =
  isFunctionValue value
    || runtimeContainerContainsFunction value
  where
    runtimeContainerContainsFunction runtimeValue =
      case runtimeValue of
        VList elements _ ->
          any runtimeValueContainsFunction elements
        VTuple elements ->
          any runtimeValueContainsFunction elements
        VConstructorApplication _ capturedArgs ->
          foldrRuntimeConstructorArguments
            (\argumentValue containsFunction -> runtimeValueContainsFunction argumentValue || containsFunction)
            False
            capturedArgs
        VTyped _ innerValue ->
          runtimeValueContainsFunction innerValue
        VExplicitTypeApplication _ innerValue ->
          runtimeValueContainsFunction innerValue
        VExplicitResultHints _ innerValue ->
          runtimeValueContainsFunction innerValue
        _ ->
          False

runtimeStructuralEquality :: RuntimeValue -> RuntimeValue -> Maybe Bool
runtimeStructuralEquality leftValue rightValue =
  case (leftValue, rightValue) of
    (VExplicitTypeApplication _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VExplicitTypeApplication _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VExplicitResultHints _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VExplicitResultHints _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VTyped leftTypeHint leftInnerValue, VTyped rightTypeHint rightInnerValue)
      | constraintSignatureTypesCompatible leftTypeHint rightTypeHint ->
          runtimeStructuralEquality leftInnerValue rightInnerValue
      | otherwise ->
          Just False
    (VTyped _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VTyped _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata
    (VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata
    (VBool leftBool, VBool rightBool) -> Just (leftBool == rightBool)
    (VChar leftChar, VChar rightChar) -> Just (leftChar == rightChar)
    (VText leftText, VText rightText) -> Just (leftText == rightText)
    (VList leftElements _, VList rightElements _) ->
      structuralElementEquality leftElements rightElements
    (VTuple leftElements, VTuple rightElements) ->
      structuralElementEquality leftElements rightElements
    ( VConstructor leftTypeName _ leftName leftConstructorArguments leftArgs,
      VConstructor rightTypeName _ rightName rightConstructorArguments rightArgs
      )
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs,
        leftTypeName == rightTypeName,
        leftName == rightName,
        leftConstructorArguments == rightConstructorArguments ->
          structuralElementEquality leftArgs rightArgs
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs ->
          Just False
    _ -> Nothing

structuralElementEquality :: [RuntimeValue] -> [RuntimeValue] -> Maybe Bool
structuralElementEquality leftElements rightElements
  | length leftElements /= length rightElements =
      Just False
  | otherwise =
      fmap and
        (traverse (uncurry runtimeStructuralEquality) (zip leftElements rightElements))

evalIntegerPredicate :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool -> Either Diagnostic RuntimeValue
evalIntegerPredicate operatorSymbol leftInt leftMetadata rightInt rightMetadata predicateResult =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right (VBool predicateResult)
    False ->
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalIntegerEquality :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Either Diagnostic RuntimeValue
evalIntegerEquality operatorSymbol leftInt leftMetadata rightInt rightMetadata =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right
        ( VBool
            ( if operatorSymbol == "!="
                then leftInt /= rightInt
                else leftInt == rightInt
            )
        )
    False ->
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalFloatPredicate :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool -> Either Diagnostic RuntimeValue
evalFloatPredicate operatorSymbol leftMetadata rightMetadata predicateResult =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Right (VBool predicateResult)
    else
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderFloatOperandTarget (runtimeFloatTargetType leftMetadata)
                <> " and "
                <> renderFloatOperandTarget (runtimeFloatTargetType rightMetadata)
            )
        )

runtimeIntegerStructuralEquality :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Maybe Bool
runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata =
  if runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata
    then Just (leftInt == rightInt)
    else Nothing

runtimeFloatStructuralEquality :: Double -> RuntimeFloatMetadata -> Double -> RuntimeFloatMetadata -> Maybe Bool
runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Just (leftFloat == rightFloat)
    else Nothing

runtimeIntegerMetadataCompatible :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool
runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just leftTarget, Nothing) ->
      integerValueMatchesTarget leftTarget rightInt
    (Nothing, Just rightTarget) ->
      integerValueMatchesTarget rightTarget leftInt
    (Nothing, Nothing) ->
      True

runtimeFloatMetadataCompatible :: RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool
runtimeFloatMetadataCompatible leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just NumericFloat64, Nothing) ->
      True
    (Nothing, Just NumericFloat64) ->
      True
    (Nothing, Nothing) ->
      True
    _ ->
      False
