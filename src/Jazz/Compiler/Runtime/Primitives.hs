{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Kernel primitives over runtime values. Collection primitives receive an
-- evaluator callback for applying their function arguments.
module Jazz.Compiler.Runtime.Primitives
  ( RuntimeApplication,
    evalBuiltin,
  )
where

import Control.Monad.Trans.Except
  ( ExceptT,
    throwE,
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
    toUpper,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Expr (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    numericTypeIntegerBounds,
    renderNumericTypeName,
  )
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (..))
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Runtime.Semantics
  ( applyRuntimeTypeHint,
    evalNumericConversion,
    exceedsFloatTarget,
    integerValueMatchesTarget,
    integerValueWithinBounds,
    isFunctionValue,
    renderRuntimeType,
    renderRuntimeValue,
    roundFloatTarget,
    runtimeDiagnostic,
    runtimeIntMatchesTarget,
    runtimeTypesCompatible,
    runtimeValueMatchesConstraint,
    targetedFloatMetadata,
    targetedIntMetadata,
    untypedFloatMetadata,
    untypedIntMetadata,
  )
import Jazz.Compiler.Runtime.Types
  ( RuntimeAnnotation (..),
    RuntimeClosure (..),
    RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeValue (..),
    constructorIsSaturated,
    foldrRuntimeAppliedArguments,
  )
import Jazz.Compiler.SemanticFacts (AnalyzedType)
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SemanticType (..),
  )

-- | The only evaluator capability needed by primitive value semantics.
type RuntimeApplication failure m =
  RuntimeValue -> RuntimeValue -> ExceptT failure m RuntimeValue

liftRuntimeResult :: (Monad m) => (Diagnostic -> failure) -> Either Diagnostic value -> ExceptT failure m value
liftRuntimeResult injectDiagnostic result =
  case result of
    Left diagnostic -> throwE (injectDiagnostic diagnostic)
    Right value -> pure value

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin ::
  (Monad m) =>
  (Diagnostic -> failure) ->
  RuntimeApplication failure m ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT failure m RuntimeValue
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
              let maybeMappedTypeHint = SemanticList <$> runtimeMapResultElementType mapper maybeCollectionTypeHint
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
    (BuiltinAdd, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinSubtract, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinMultiply, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinDivide, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinEquals, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinLessThan, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinGreaterThan, [left, right]) -> evalKernelBinary builtinFunction left right
    (BuiltinHd, [VList [] _]) ->
      Left (runtimeDiagnostic E3009 "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _) maybeTypeHint]) ->
      case maybeTypeHint of
        Just (SemanticList elementType) ->
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
        Just (SemanticList elementType) -> do
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
          let listTypeHint = Just (SemanticList SemanticChar)
           in if scalar <= 0x10FFFF && not (scalar >= 0xD800 && scalar <= 0xDFFF)
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
      let listTypeHint = Just (SemanticList (SemanticTuple [SemanticChar, SemanticText]))
       in case Text.uncons textValue of
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
    VAnnotated _ innerValue -> runtimeChar innerValue
    _ -> Nothing

runtimeText :: RuntimeValue -> Maybe Text
runtimeText runtimeValue =
  case runtimeValue of
    VText value -> Just value
    VAnnotated _ innerValue -> runtimeText innerValue
    _ -> Nothing

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements ::
  (Monad m) =>
  (Diagnostic -> failure) ->
  RuntimeApplication failure m ->
  RuntimeValue ->
  [RuntimeValue] ->
  ExceptT failure m [RuntimeValue]
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

runtimeFunctionResultType :: RuntimeValue -> Maybe AnalyzedType
runtimeFunctionResultType runtimeValue =
  case runtimeValue of
    VAnnotated (RuntimeMethodCall _) innerValue ->
      runtimeFunctionResultType innerValue
    VAnnotated (RuntimeTypeApplication _) innerValue ->
      runtimeFunctionResultType innerValue
    VAnnotated (RuntimeResultHints _) innerValue ->
      runtimeFunctionResultType innerValue
    VAnnotated (RuntimeTypeHint (SemanticFunction _ resultType)) _ ->
      Just resultType
    VClosure closure
      | Just (SemanticFunction _ resultType) <- runtimeClosureTypeHint closure ->
          Just resultType
    _ ->
      Nothing

runtimeMapResultElementType :: RuntimeValue -> Maybe AnalyzedType -> Maybe AnalyzedType
runtimeMapResultElementType mapper maybeCollectionTypeHint =
  case runtimeFunctionResultType mapper of
    Just resultType ->
      Just resultType
    Nothing ->
      runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint

runtimeBuiltinMapResultElementType :: RuntimeValue -> Maybe AnalyzedType -> Maybe AnalyzedType
runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint =
  case (mapper, maybeCollectionTypeHint) of
    (VBuiltin BuiltinHd [], Just (SemanticList (SemanticList elementType))) ->
      Just elementType
    (VClosure closure, Just (SemanticList elementType))
      | EVar _ resultName <- runtimeClosureBody closure,
        Nothing <- runtimeClosureTypeHint closure,
        resultName == runtimeClosureParameter closure ->
          Just elementType
    _ ->
      Nothing

-- | Arithmetic and comparison semantics shared by the binary kernel functions.
evalKernelBinary :: BuiltinSymbol -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalKernelBinary primitive leftValue rightValue
  | primitive == BuiltinEquals,
    isFunctionValue leftValue || isFunctionValue rightValue =
      Left (runtimeCallableEqualityDiagnostic leftValue rightValue)
  | otherwise =
      case (leftValue, rightValue) of
        (VAnnotated (RuntimeTypeHint leftTypeHint) leftInnerValue, _)
          | primitive == BuiltinEquals,
            runtimeTypeHintRequiresStructuralEquality leftTypeHint ->
              evalStructuralEquality leftValue rightValue
          | otherwise ->
              preserveLeftTypedNumericResult primitive leftTypeHint
                =<< evalKernelBinary primitive leftInnerValue rightValue
        (_, VAnnotated (RuntimeTypeHint rightTypeHint) rightInnerValue)
          | primitive == BuiltinEquals,
            runtimeTypeHintRequiresStructuralEquality rightTypeHint ->
              evalStructuralEquality leftValue rightValue
          | otherwise ->
              preserveRightTypedNumericResult primitive leftValue rightTypeHint
                =<< evalKernelBinary primitive leftValue rightInnerValue
        (VInt leftInt leftMetadata, VInt rightInt rightMetadata)
          | Just arithmetic <- arithmeticOperation div primitive ->
              if primitive == BuiltinDivide && rightInt == 0
                then divisionByZero
                else evalIntegerArithmetic primitive leftMetadata rightMetadata (arithmetic leftInt rightInt)
          | Just predicate <- comparisonOperation primitive ->
              evalIntegerPredicate primitive leftInt leftMetadata rightInt rightMetadata (predicate leftInt rightInt)
        (VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata)
          | Just arithmetic <- arithmeticOperation (/) primitive ->
              if primitive == BuiltinDivide && floatIsZero rightFloat
                then divisionByZero
                else evalFloatArithmetic primitive leftMetadata rightMetadata (arithmetic leftFloat rightFloat)
          | Just predicate <- comparisonOperation primitive ->
              evalFloatPredicate primitive leftMetadata rightMetadata (predicate leftFloat rightFloat)
        (VBool leftBool, VBool rightBool)
          | primitive == BuiltinEquals -> scalarEquality (leftBool == rightBool)
        (VChar leftChar, VChar rightChar)
          | primitive == BuiltinEquals -> scalarEquality (leftChar == rightChar)
        (VText leftText, VText rightText)
          | primitive == BuiltinEquals -> scalarEquality (leftText == rightText)
        (VList {}, VList {})
          | primitive == BuiltinEquals -> evalStructuralEquality leftValue rightValue
        (VTuple {}, VTuple {})
          | primitive == BuiltinEquals -> evalStructuralEquality leftValue rightValue
        (VConstructorApplication {}, VConstructorApplication {})
          | primitive == BuiltinEquals -> evalStructuralEquality leftValue rightValue
        _ -> Left (invalidBinaryOperands primitive leftValue rightValue)
  where
    divisionByZero = Left (runtimeDiagnostic E3001 "runtime primitive 'divide' failed: division by zero")
    scalarEquality = Right . VBool

arithmeticOperation :: (Num value) => (value -> value -> value) -> BuiltinSymbol -> Maybe (value -> value -> value)
arithmeticOperation divide primitive =
  case primitive of
    BuiltinAdd -> Just (+)
    BuiltinSubtract -> Just (-)
    BuiltinMultiply -> Just (*)
    BuiltinDivide -> Just divide
    _ -> Nothing

comparisonOperation :: (Ord value) => BuiltinSymbol -> Maybe (value -> value -> Bool)
comparisonOperation primitive =
  case primitive of
    BuiltinLessThan -> Just (<)
    BuiltinGreaterThan -> Just (>)
    BuiltinEquals -> Just (==)
    _ -> Nothing

invalidBinaryOperands :: BuiltinSymbol -> RuntimeValue -> RuntimeValue -> Diagnostic
invalidBinaryOperands primitive leftValue rightValue =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> builtinSymbolName primitive
        <> "' cannot be applied to "
        <> renderRuntimeType leftValue
        <> " and "
        <> renderRuntimeType rightValue
    )

preserveLeftTypedNumericResult :: BuiltinSymbol -> AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveLeftTypedNumericResult primitive typeHint runtimeValue
  | numericArithmeticPrimitive primitive,
    numericAliasTypeHint typeHint,
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

preserveRightTypedNumericResult :: BuiltinSymbol -> RuntimeValue -> AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
preserveRightTypedNumericResult primitive leftValue typeHint runtimeValue
  | numericArithmeticPrimitive primitive,
    numericAliasTypeHint typeHint,
    not (runtimeValueHasTargetedNumericMetadata leftValue),
    runtimeValueMatchesConstraint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

numericArithmeticPrimitive :: BuiltinSymbol -> Bool
numericArithmeticPrimitive primitive =
  primitive == BuiltinAdd || primitive == BuiltinSubtract || primitive == BuiltinMultiply || primitive == BuiltinDivide

numericAliasTypeHint :: AnalyzedType -> Bool
numericAliasTypeHint typeHint =
  case typeHint of
    SemanticInt -> True
    SemanticFloat -> True
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

runtimeTypeHintRequiresStructuralEquality :: AnalyzedType -> Bool
runtimeTypeHintRequiresStructuralEquality signatureType =
  case signatureType of
    SemanticData _ (_ : _) -> True
    SemanticList {} -> True
    SemanticTuple {} -> True
    _ -> False

runtimeCallableEqualityDiagnostic :: RuntimeValue -> RuntimeValue -> Diagnostic
runtimeCallableEqualityDiagnostic leftValue rightValue =
  runtimeDiagnostic
    E3007
    ( "runtime primitive 'equals' cannot compare callable values; callable values are not equality-supported, found "
        <> renderRuntimeType leftValue
        <> " and "
        <> renderRuntimeType rightValue
    )

evalIntegerArithmetic ::
  BuiltinSymbol ->
  RuntimeIntMetadata ->
  RuntimeIntMetadata ->
  Integer ->
  Either Diagnostic RuntimeValue
evalIntegerArithmetic primitive leftMetadata rightMetadata result = do
  targetType <- selectIntegerBinaryTarget primitive leftMetadata rightMetadata
  evalIntegerBinary primitive targetType result

selectIntegerBinaryTarget :: BuiltinSymbol -> RuntimeIntMetadata -> RuntimeIntMetadata -> Either Diagnostic (Maybe NumericType)
selectIntegerBinaryTarget primitive leftMetadata rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedIntegerArithmeticDiagnostic primitive (Just leftTarget) (Just rightTarget))
    (Just leftTarget, Nothing) -> Right (Just leftTarget)
    (Nothing, Just rightTarget) -> Right (Just rightTarget)
    _ -> Right Nothing

evalIntegerBinary :: BuiltinSymbol -> Maybe NumericType -> Integer -> Either Diagnostic RuntimeValue
evalIntegerBinary primitive maybeTarget result =
  case maybeTarget of
    Just targetType ->
      case numericTypeIntegerBounds targetType of
        Just bounds
          | integerValueWithinBounds result bounds ->
              Right (VInt result (targetedIntMetadata targetType))
          | otherwise ->
              Left (runtimeIntegerArithmeticOverflowDiagnostic primitive targetType result bounds)
        Nothing ->
          Right (VInt result (targetedIntMetadata targetType))
    Nothing ->
      Right (VInt result untypedIntMetadata)

mixedIntegerArithmeticDiagnostic :: BuiltinSymbol -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedIntegerArithmeticDiagnostic primitive leftTarget rightTarget =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> builtinSymbolName primitive
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

runtimeIntegerArithmeticOverflowDiagnostic :: BuiltinSymbol -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
runtimeIntegerArithmeticOverflowDiagnostic primitive targetType result (lowerBound, upperBound) =
  runtimeDiagnostic
    E3025
    ( "runtime primitive '"
        <> builtinSymbolName primitive
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
  BuiltinSymbol ->
  RuntimeFloatMetadata ->
  RuntimeFloatMetadata ->
  Double ->
  Either Diagnostic RuntimeValue
evalFloatArithmetic primitive leftMetadata rightMetadata result = do
  targetType <- selectFloatBinaryTarget primitive leftMetadata rightMetadata
  evalFloatBinary primitive targetType result

selectFloatBinaryTarget :: BuiltinSymbol -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Either Diagnostic (Maybe NumericType)
selectFloatBinaryTarget primitive leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedFloatArithmeticDiagnostic primitive (Just leftTarget) (Just rightTarget))
    (Just NumericFloat64, Nothing) -> Right (Just NumericFloat64)
    (Nothing, Just NumericFloat64) -> Right (Just NumericFloat64)
    (Just targetType, Nothing) -> Left (mixedFloatArithmeticDiagnostic primitive (Just targetType) Nothing)
    (Nothing, Just targetType) -> Left (mixedFloatArithmeticDiagnostic primitive Nothing (Just targetType))
    (Nothing, Nothing) -> Right Nothing

evalFloatBinary :: BuiltinSymbol -> Maybe NumericType -> Double -> Either Diagnostic RuntimeValue
evalFloatBinary primitive targetType result
  | isNaN result || isInfinite result =
      Left
        ( runtimeDiagnostic
            E3025
            ("runtime primitive '" <> builtinSymbolName primitive <> "' failed: non-finite Float result")
        )
  | Just floatTarget <- targetType,
    exceedsFloatTarget floatTarget result =
      Left (runtimeFloatArithmeticOverflowDiagnostic primitive floatTarget)
  | Just floatTarget <- targetType =
      Right (VFloat (roundFloatTarget floatTarget result) (targetedFloatMetadata floatTarget))
  | otherwise = Right (VFloat result (untypedFloatMetadata Nothing))

mixedFloatArithmeticDiagnostic :: BuiltinSymbol -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedFloatArithmeticDiagnostic primitive leftTarget rightTarget =
  runtimeDiagnostic
    E3007
    ( "runtime primitive '"
        <> builtinSymbolName primitive
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

runtimeFloatArithmeticOverflowDiagnostic :: BuiltinSymbol -> NumericType -> Diagnostic
runtimeFloatArithmeticOverflowDiagnostic primitive targetType =
  runtimeDiagnostic
    E3025
    ( "runtime primitive '"
        <> builtinSymbolName primitive
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

evalStructuralEquality :: RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalStructuralEquality leftValue rightValue =
  if runtimeValueContainsFunction leftValue || runtimeValueContainsFunction rightValue
    then Left (runtimeCallableEqualityDiagnostic leftValue rightValue)
    else case runtimeStructuralEquality leftValue rightValue of
      Just equalityResult -> Right (VBool equalityResult)
      Nothing -> Left (invalidBinaryOperands BuiltinEquals leftValue rightValue)

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
          foldrRuntimeAppliedArguments
            (\argumentValue containsFunction -> runtimeValueContainsFunction argumentValue || containsFunction)
            False
            capturedArgs
        VAnnotated _ innerValue ->
          runtimeValueContainsFunction innerValue
        _ ->
          False

runtimeStructuralEquality :: RuntimeValue -> RuntimeValue -> Maybe Bool
runtimeStructuralEquality leftValue rightValue =
  case (leftValue, rightValue) of
    (VAnnotated (RuntimeTypeApplication _) leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VAnnotated (RuntimeTypeApplication _) rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VAnnotated (RuntimeResultHints _) leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VAnnotated (RuntimeResultHints _) rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VAnnotated (RuntimeTypeHint leftTypeHint) leftInnerValue, VAnnotated (RuntimeTypeHint rightTypeHint) rightInnerValue)
      | runtimeTypesCompatible leftTypeHint rightTypeHint ->
          runtimeStructuralEquality leftInnerValue rightInnerValue
      | otherwise ->
          Just False
    (VAnnotated (RuntimeTypeHint _) leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VAnnotated (RuntimeTypeHint _) rightInnerValue) ->
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
      fmap
        and
        (traverse (uncurry runtimeStructuralEquality) (zip leftElements rightElements))

evalIntegerPredicate :: BuiltinSymbol -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool -> Either Diagnostic RuntimeValue
evalIntegerPredicate primitive leftInt leftMetadata rightInt rightMetadata predicateResult =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right (VBool predicateResult)
    False ->
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> builtinSymbolName primitive
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalFloatPredicate :: BuiltinSymbol -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool -> Either Diagnostic RuntimeValue
evalFloatPredicate primitive leftMetadata rightMetadata predicateResult =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Right (VBool predicateResult)
    else
      Left
        ( runtimeDiagnostic
            E3007
            ( "runtime primitive '"
                <> builtinSymbolName primitive
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
