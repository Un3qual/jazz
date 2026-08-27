{-# LANGUAGE OverloadedStrings #-}

-- | Type, representation-recipe, literal, and primitive-constraint validation.
module Jazz.Compiler.TypedCore.Validate.TypeRecipes
  ( callableRecipeCompatible,
    concreteImplTargetType,
    dataArgumentsUseParameter,
    dataParameterContributesToEquality,
    dataParameterContributesToEqualityFrom,
    directCallableRecipeArity,
    duplicateParameterFailures,
    expectedRecipe,
    expectedRecipeWithCallableStaging,
    expectedValueRecipe,
    fractionalLiteralFitsNumericType,
    hasUnboundRepresentationParameter,
    hasUnboundTypeParameter,
    identifierStartsUpper,
    integerLiteralFitsType,
    integralConstraint,
    integralLiteralConstraintAcceptsType,
    integralTypeBounds,
    invalidRecipeWidth,
    isFloatingNumericType,
    isFunctionType,
    isUnicodeScalar,
    literalMatchesType,
    literalType,
    nextTypeParameterOrdinal,
    nodeInfoHasCompatibleIntrinsicContract,
    numericConstraintAcceptsType,
    numericRecipe,
    numericTypeFromTyped,
    numericTypeIsIntegral,
    parseDecimalBound,
    parseDecimalMagnitude,
    recipeCompatibleWithCallableStaging,
    recipeContractFailures,
    recipeMentionsParameter,
    stagedClosureRecipe,
    stagedClosureRecipeCompatible,
    strictEqualityOperandTypeSupported,
    strictEqualityTypeSupported,
    strictEqualityTypeSupportedWith,
    substituteRepresentationParameters,
    substituteTypeParameters,
    typeMentionsParameter,
    typePositionUsesParameter,
    typePositionsUseParameter,
    typeRecipeCompatible,
    validOperatorBindingName,
    validIdentifierSpelling,
    validQualifiedIdentifier,
    validRecipeWidth,
    validResolvedIdentifier,
    validSourceIdentifier,
    validateCallableShape,
    validateCoreName,
    validateLiteral,
    validateNumericConstraintTarget,
    validateOrderedTypeParameters,
    validateOrderedTypeParametersFrom,
    validatePrimitiveConstraint,
    validateRecipe,
    validateStrictEqualityTarget,
    validateType,
    validateTypeRecipe,
  )
where

import Data.Char (isAlpha, isAlphaNum, isUpper, ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (NumericType (..))
import Jazz.Compiler.BuiltinCatalog (numericTypeFloatMax)
import Jazz.Compiler.Name (operatorBindingIdentifierText)
import Jazz.Compiler.Parser.Operator (isValidUserOperatorSymbol)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Internal

validateOrderedTypeParameters :: TypedCoreValidationPath -> [TypedTypeParameterId] -> [TypedCoreValidationFailure]
validateOrderedTypeParameters path = validateOrderedTypeParametersFrom path 0

validateOrderedTypeParametersFrom :: TypedCoreValidationPath -> Int -> [TypedTypeParameterId] -> [TypedCoreValidationFailure]
validateOrderedTypeParametersFrom path firstOrdinal parameters = duplicateFailures <> orderFailures
  where
    duplicateFailures = duplicateParameterFailures path TypedDuplicateTypeParameter TypedTypeParameterDetail parameters
    orderFailures =
      [ failure path TypedInvalidTypeParameterOrder (TypedIndexDetail expected)
      | (expected, TypedTypeParameterId actual) <- zip [firstOrdinal ..] parameters,
        actual /= expected
      ]

nextTypeParameterOrdinal :: Set TypedTypeParameterId -> Int
nextTypeParameterOrdinal =
  foldl'
    (\next (TypedTypeParameterId actual) -> max next (actual + 1))
    0
    . Set.toList

validatePrimitiveConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
validatePrimitiveConstraint context path scope constraint =
  case constraint of
    TypedNumericPrimitiveConstraint numericConstraint typeValue ->
      validateType path scope typeValue
        <> validateNumericConstraintTarget path numericConstraint typeValue
    TypedStrictEqualityPrimitiveConstraint typeValue ->
      validateType path scope typeValue
        <> validateStrictEqualityTarget context path typeValue

validateStrictEqualityTarget :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedCoreValidationFailure]
validateStrictEqualityTarget context path typeValue
  | strictEqualityTypeSupported context typeValue = []
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail TypedBoolType typeValue)]

strictEqualityTypeSupported :: ModuleContext -> TypedType -> Bool
strictEqualityTypeSupported context = strictEqualityTypeSupportedWith context (const True)

strictEqualityOperandTypeSupported :: ModuleContext -> TypedType -> Bool
strictEqualityOperandTypeSupported context typeValue =
  activeConstraint typeValue
    || strictEqualityTypeSupportedWith context activeConstraint typeValue
  where
    activeConstraint candidate =
      TypedStrictEqualityPrimitiveConstraint candidate
        `elem` moduleContextPrimitiveConstraints context

strictEqualityTypeSupportedWith :: ModuleContext -> (TypedType -> Bool) -> TypedType -> Bool
strictEqualityTypeSupportedWith context typeParameterSupported typeValue =
  supported Set.empty [(Set.empty, typeValue)]
  where
    supported :: Set TypedType -> [(Set ResolvedNameKey, TypedType)] -> Bool
    supported _ [] = True
    supported expanded ((seen, currentType) : remaining) =
      case currentType of
        TypedIntType -> supported expanded remaining
        TypedFloatType -> supported expanded remaining
        TypedNumericType _ -> supported expanded remaining
        TypedBoolType -> supported expanded remaining
        TypedCharType -> supported expanded remaining
        TypedTextType -> supported expanded remaining
        TypedListType elementType ->
          supported expanded ((seen, elementType) : remaining)
        TypedTupleType elementTypes ->
          supported expanded (map (\elementType -> (seen, elementType)) elementTypes <> remaining)
        TypedTypeParameterType _
          | typeParameterSupported currentType -> supported expanded remaining
          | otherwise -> False
        TypedFunctionType {} -> False
        TypedDataType name arguments
          | Set.member currentType expanded -> supported expanded remaining
          | otherwise ->
              case resolvedNameKey (moduleContextPath context) name of
                Nothing -> False
                Just dataKey ->
                  case Map.lookup dataKey (moduleContextDataContracts context) of
                    Nothing -> False
                    Just (DataContract parameters constructorFields)
                      | length parameters /= length arguments -> False
                      | Set.member dataKey seen ->
                          supported
                            expanded
                            ( map
                                (\argument -> (seen, argument))
                                [ argument
                                | (parameter, argument) <- zip parameters arguments,
                                  dataParameterContributesToEquality context Set.empty dataKey parameter
                                ]
                                <> remaining
                            )
                      | otherwise ->
                          let substitutions = Map.fromList (zip parameters arguments)
                              nextSeen = Set.insert dataKey seen
                              fields =
                                map
                                  (substituteTypeParameters substitutions)
                                  (concat constructorFields)
                           in supported
                                (Set.insert currentType expanded)
                                (map (\field -> (nextSeen, field)) fields <> remaining)

dataParameterContributesToEquality ::
  ModuleContext ->
  Set (ResolvedNameKey, TypedTypeParameterId) ->
  ResolvedNameKey ->
  TypedTypeParameterId ->
  Bool
dataParameterContributesToEquality context seen dataKey parameter =
  fst (dataParameterContributesToEqualityFrom context seen dataKey parameter)

dataParameterContributesToEqualityFrom ::
  ModuleContext ->
  Set (ResolvedNameKey, TypedTypeParameterId) ->
  ResolvedNameKey ->
  TypedTypeParameterId ->
  (Bool, Set (ResolvedNameKey, TypedTypeParameterId))
dataParameterContributesToEqualityFrom context seen dataKey parameter
  | Set.member parameterKey seen = (False, seen)
  | otherwise =
      case Map.lookup dataKey (moduleContextDataContracts context) of
        Nothing -> (True, nextSeen)
        Just (DataContract _ constructorFields) ->
          typePositionsUseParameter context nextSeen parameter (concat constructorFields)
  where
    parameterKey = (dataKey, parameter)
    nextSeen = Set.insert parameterKey seen

typePositionsUseParameter ::
  ModuleContext ->
  Set (ResolvedNameKey, TypedTypeParameterId) ->
  TypedTypeParameterId ->
  [TypedType] ->
  (Bool, Set (ResolvedNameKey, TypedTypeParameterId))
typePositionsUseParameter _ seen _ [] = (False, seen)
typePositionsUseParameter context seen parameter (typeValue : remaining) =
  case typePositionUsesParameter context seen parameter typeValue of
    (True, nextSeen) -> (True, nextSeen)
    (False, nextSeen) -> typePositionsUseParameter context nextSeen parameter remaining

typePositionUsesParameter ::
  ModuleContext ->
  Set (ResolvedNameKey, TypedTypeParameterId) ->
  TypedTypeParameterId ->
  TypedType ->
  (Bool, Set (ResolvedNameKey, TypedTypeParameterId))
typePositionUsesParameter context seen parameter typeValue =
  case typeValue of
    TypedListType elementType ->
      typePositionUsesParameter context seen parameter elementType
    TypedTupleType elementTypes ->
      typePositionsUseParameter context seen parameter elementTypes
    TypedDataType name arguments ->
      case resolvedNameKey (moduleContextPath context) name of
        Nothing -> (typeMentionsParameter parameter typeValue, seen)
        Just dataKey ->
          case Map.lookup dataKey (moduleContextDataContracts context) of
            Just (DataContract dataParameters _)
              | length dataParameters == length arguments ->
                  dataArgumentsUseParameter context seen parameter dataKey (zip dataParameters arguments)
            _ -> (typeMentionsParameter parameter typeValue, seen)
    TypedFunctionType argument result ->
      case typePositionUsesParameter context seen parameter argument of
        (True, nextSeen) -> (True, nextSeen)
        (False, nextSeen) -> typePositionUsesParameter context nextSeen parameter result
    TypedTypeParameterType candidate -> (candidate == parameter, seen)
    _ -> (False, seen)

dataArgumentsUseParameter ::
  ModuleContext ->
  Set (ResolvedNameKey, TypedTypeParameterId) ->
  TypedTypeParameterId ->
  ResolvedNameKey ->
  [(TypedTypeParameterId, TypedType)] ->
  (Bool, Set (ResolvedNameKey, TypedTypeParameterId))
dataArgumentsUseParameter _ seen _ _ [] = (False, seen)
dataArgumentsUseParameter context seen sourceParameter dataKey ((dataParameter, argument) : remaining)
  | not (typeMentionsParameter sourceParameter argument) =
      dataArgumentsUseParameter context seen sourceParameter dataKey remaining
  | otherwise =
      case dataParameterContributesToEqualityFrom context seen dataKey dataParameter of
        (True, nextSeen) -> (True, nextSeen)
        (False, nextSeen) -> dataArgumentsUseParameter context nextSeen sourceParameter dataKey remaining

validateNumericConstraintTarget :: TypedCoreValidationPath -> TypedNumericConstraint -> TypedType -> [TypedCoreValidationFailure]
validateNumericConstraintTarget path numericConstraint typeValue
  | numericConstraintAcceptsType numericConstraint typeValue = []
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail TypedIntType typeValue)]

numericConstraintAcceptsType :: TypedNumericConstraint -> TypedType -> Bool
numericConstraintAcceptsType numericConstraint typeValue =
  case numericConstraint of
    TypedIntegralLiteralNumericConstraint lower upper ->
      integralLiteralConstraintAcceptsType lower upper typeValue
    _ ->
      case typeValue of
        TypedTypeParameterType _ -> True
        TypedIntType -> True
        TypedFloatType -> not (integralConstraint numericConstraint)
        TypedNumericType numericType
          | integralConstraint numericConstraint -> numericTypeIsIntegral numericType
          | otherwise -> True
        _ -> False

integralLiteralConstraintAcceptsType :: Text -> Text -> TypedType -> Bool
integralLiteralConstraintAcceptsType lowerText upperText typeValue =
  case (parseDecimalBound lowerText, parseDecimalBound upperText) of
    (Just lower, Just upper)
      | lower <= upper ->
          case typeValue of
            TypedTypeParameterType _ -> True
            _ ->
              case integralTypeBounds typeValue of
                Just (minimumValue, maximumValue) ->
                  minimumValue <= lower
                    && upper <= maximumValue
                Nothing -> False
    _ -> False

parseDecimalBound :: Text -> Maybe Integer
parseDecimalBound value =
  case Text.uncons value of
    Just ('-', digits) -> negate <$> parseDecimalMagnitude digits
    _ -> parseDecimalMagnitude value

parseDecimalMagnitude :: Text -> Maybe Integer
parseDecimalMagnitude digits
  | Text.null digits || Text.any (not . asciiDigit) digits = Nothing
  | otherwise = Just (Text.foldl' accumulate 0 digits)
  where
    asciiDigit character = character >= '0' && character <= '9'
    accumulate result character =
      result * 10 + toInteger (fromEnum character - fromEnum '0')

integralTypeBounds :: TypedType -> Maybe (Integer, Integer)
integralTypeBounds typeValue =
  case typeValue of
    TypedIntType -> signedBounds 64
    TypedNumericType numericType ->
      case numericType of
        TypedInt8Type -> signedBounds 8
        TypedInt16Type -> signedBounds 16
        TypedInt32Type -> signedBounds 32
        TypedInt64Type -> signedBounds 64
        TypedUInt8Type -> unsignedBounds 8
        TypedUInt16Type -> unsignedBounds 16
        TypedUInt32Type -> unsignedBounds 32
        TypedUInt64Type -> unsignedBounds 64
        TypedFloat16Type -> Nothing
        TypedFloat32Type -> Nothing
        TypedFloat64Type -> Nothing
    _ -> Nothing
  where
    signedBounds :: Int -> Maybe (Integer, Integer)
    signedBounds width =
      let limit = 2 ^ (width - 1)
       in Just (-limit, limit - 1)
    unsignedBounds :: Int -> Maybe (Integer, Integer)
    unsignedBounds width = Just (0, 2 ^ width - 1)

integralConstraint :: TypedNumericConstraint -> Bool
integralConstraint numericConstraint =
  case numericConstraint of
    TypedIntegralNumericConstraint -> True
    TypedIntegralLiteralNumericConstraint {} -> True
    _ -> False

numericTypeIsIntegral :: TypedNumericType -> Bool
numericTypeIsIntegral numericType =
  case numericType of
    TypedInt8Type -> True
    TypedInt16Type -> True
    TypedInt32Type -> True
    TypedInt64Type -> True
    TypedUInt8Type -> True
    TypedUInt16Type -> True
    TypedUInt32Type -> True
    TypedUInt64Type -> True
    TypedFloat16Type -> False
    TypedFloat32Type -> False
    TypedFloat64Type -> False

validateLiteral :: TypedCoreValidationPath -> TypedNodeInfo -> TypedLiteral -> [TypedCoreValidationFailure]
validateLiteral path info literal
  | TypedCharacterLiteral character <- literal,
    not (isUnicodeScalar character) =
      [failure path TypedLiteralTypeMismatch (TypedTextDetail "non-scalar character")]
  | literalMatchesType literal (typedNodeType info) = []
  | otherwise = [failure path TypedLiteralTypeMismatch (TypedTypeDetail (literalType literal) (typedNodeType info))]

isUnicodeScalar :: Char -> Bool
isUnicodeScalar character =
  codePoint < 0xD800 || codePoint > 0xDFFF
  where
    codePoint = ord character

concreteImplTargetType :: TypedType -> Bool
concreteImplTargetType typeValue =
  case typeValue of
    TypedListType elementType -> concreteImplTargetType elementType
    TypedTupleType elementTypes -> all concreteImplTargetType elementTypes
    TypedDataType _ arguments -> all concreteImplTargetType arguments
    TypedFunctionType {} -> False
    TypedTypeParameterType {} -> False
    _ -> True

literalMatchesType :: TypedLiteral -> TypedType -> Bool
literalMatchesType literal typeValue =
  case (literal, typeValue) of
    (TypedIntegerLiteral value, TypedIntType) -> integerLiteralFitsType value typeValue
    (TypedIntegerLiteral value, TypedNumericType numericType) ->
      not (isFloatingNumericType numericType)
        && integerLiteralFitsType value typeValue
    (TypedFractionalLiteral whole fractional Nothing, TypedFloatType) ->
      fractionalLiteralFitsNumericType whole fractional NumericFloat64
    (TypedFractionalLiteral whole fractional Nothing, TypedNumericType numericType) ->
      isFloatingNumericType numericType
        && fractionalLiteralFitsNumericType whole fractional (numericTypeFromTyped numericType)
    (TypedFractionalLiteral whole fractional (Just expectedType), TypedNumericType actualType) ->
      expectedType == actualType
        && isFloatingNumericType actualType
        && fractionalLiteralFitsNumericType whole fractional (numericTypeFromTyped actualType)
    (TypedBooleanLiteral _, TypedBoolType) -> True
    (TypedCharacterLiteral _, TypedCharType) -> True
    (TypedTextLiteral _, TypedTextType) -> True
    _ -> False

integerLiteralFitsType :: Text -> TypedType -> Bool
integerLiteralFitsType value typeValue =
  case (parseDecimalBound value, integralTypeBounds typeValue) of
    (Just parsedValue, Just (minimumValue, maximumValue)) ->
      minimumValue <= parsedValue && parsedValue <= maximumValue
    _ -> False

fractionalLiteralFitsNumericType :: Text -> Text -> NumericType -> Bool
fractionalLiteralFitsNumericType whole fractional numericType =
  case (parseDecimalBound whole, parseDecimalMagnitude fractional, numericTypeFloatMax numericType) of
    (Just wholeValue, Just fractionalValue, Just maximumMagnitude) ->
      let scale = 10 ^ Text.length fractional
          magnitude = ((abs wholeValue * scale) + fractionalValue) % scale
       in magnitude <= toRational maximumMagnitude
    _ -> False

numericTypeFromTyped :: TypedNumericType -> NumericType
numericTypeFromTyped numericType =
  case numericType of
    TypedInt8Type -> NumericInt8
    TypedInt16Type -> NumericInt16
    TypedInt32Type -> NumericInt32
    TypedInt64Type -> NumericInt64
    TypedUInt8Type -> NumericUInt8
    TypedUInt16Type -> NumericUInt16
    TypedUInt32Type -> NumericUInt32
    TypedUInt64Type -> NumericUInt64
    TypedFloat16Type -> NumericFloat16
    TypedFloat32Type -> NumericFloat32
    TypedFloat64Type -> NumericFloat64

literalType :: TypedLiteral -> TypedType
literalType literal =
  case literal of
    TypedIntegerLiteral _ -> TypedIntType
    TypedFractionalLiteral _ _ Nothing -> TypedFloatType
    TypedFractionalLiteral _ _ (Just numericType) -> TypedNumericType numericType
    TypedBooleanLiteral _ -> TypedBoolType
    TypedCharacterLiteral _ -> TypedCharType
    TypedTextLiteral _ -> TypedTextType

isFloatingNumericType :: TypedNumericType -> Bool
isFloatingNumericType numericType = numericType `elem` [TypedFloat16Type, TypedFloat32Type, TypedFloat64Type]

validateType :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateType path scope typeValue =
  case typeValue of
    TypedIntType -> []
    TypedFloatType -> []
    TypedNumericType _ -> []
    TypedBoolType -> []
    TypedCharType -> []
    TypedTextType -> []
    TypedListType elementType -> validateType path scope elementType
    TypedTupleType elementTypes ->
      ( if length elementTypes == 1
          then [failure path TypedCollectionShapeMismatch (TypedArityDetail 2 1)]
          else []
      )
        <> concatMap (validateType path scope) elementTypes
    TypedDataType name arguments -> validateCoreName path name <> concatMap (validateType path scope) arguments
    TypedFunctionType argument result -> validateType path scope argument <> validateType path scope result
    TypedTypeParameterType parameterId
      | Set.member parameterId scope -> []
      | otherwise -> [failure path TypedUnboundTypeParameter (TypedTypeParameterDetail parameterId)]

validateRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateRecipe path scope recipe =
  widthFailures <> recipeFailures
  where
    widthFailures =
      case invalidRecipeWidth recipe of
        Just width -> [failure path TypedInvalidRepresentationWidth (TypedIndexDetail width)]
        Nothing -> []
    recipeFailures =
      case recipe of
        TypedManagedListRecipe elementRecipe -> validateRecipe path scope elementRecipe
        TypedManagedProductRecipe elementRecipes -> concatMap (validateRecipe path scope) elementRecipes
        TypedManagedVariantRecipe name arguments -> validateCoreName path name <> concatMap (validateType path scope) arguments
        TypedClosureRecipe parameters result -> concatMap (validateRecipe path scope) parameters <> validateRecipe path scope result
        TypedRepresentationParameterRecipe parameterId
          | Set.member parameterId scope -> []
          | otherwise -> [failure path TypedUnboundRepresentationParameter (TypedTypeParameterDetail parameterId)]
        _ -> []

validateTypeRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateTypeRecipe path scope typeValue recipe
  | not (validRecipeWidth recipe) = []
  | hasUnboundTypeParameter scope typeValue = []
  | hasUnboundRepresentationParameter scope recipe = []
  | otherwise =
      case expectedRecipe typeValue of
        Just expected
          | not (typeRecipeCompatible typeValue recipe) ->
              [ failure
                  path
                  (if isFunctionType typeValue then TypedCallableRecipeMismatch else TypedTypeRepresentationMismatch)
                  (TypedRecipeDetail expected recipe)
              ]
        _ -> []

validateCallableShape :: TypedCoreValidationPath -> TypedBinderId -> TypedType -> TypedRepresentationRecipe -> Maybe TypedCallableShape -> [TypedCoreValidationFailure]
validateCallableShape path owner typeValue recipe callableShape =
  case (typeValue, callableShape) of
    (TypedFunctionType {}, Nothing) -> mismatch
    (TypedFunctionType {}, Just TypedDirectCallableShape)
      | callableRecipeCompatible typeValue recipe,
        maybe True (<= 0) (directCallableRecipeArity recipe) ->
          mismatch
      | otherwise -> []
    (TypedFunctionType {}, Just TypedClosureCallableShape)
      | callableRecipeCompatible typeValue recipe,
        not (stagedClosureRecipeCompatible typeValue recipe) ->
          mismatch
      | otherwise -> []
    (_, Just _) -> mismatch
    (_, Nothing) -> []
  where
    mismatch = [failure path TypedCallableShapeMismatch (TypedBinderDetail owner)]

typeRecipeCompatible :: TypedType -> TypedRepresentationRecipe -> Bool
typeRecipeCompatible = recipeCompatibleWithCallableStaging False

callableRecipeCompatible :: TypedType -> TypedRepresentationRecipe -> Bool
callableRecipeCompatible typeValue =
  case typeValue of
    TypedFunctionType {} -> recipeCompatibleWithCallableStaging False typeValue
    _ -> const False

stagedClosureRecipeCompatible :: TypedType -> TypedRepresentationRecipe -> Bool
stagedClosureRecipeCompatible typeValue =
  case typeValue of
    TypedFunctionType {} -> recipeCompatibleWithCallableStaging True typeValue
    _ -> const False

recipeCompatibleWithCallableStaging :: Bool -> TypedType -> TypedRepresentationRecipe -> Bool
recipeCompatibleWithCallableStaging requireStagedCallable typeValue recipe =
  case typeValue of
    TypedFunctionType argumentType resultType ->
      case recipe of
        TypedClosureRecipe (argumentRecipe : remainingArguments) resultRecipe ->
          recipeCompatibleWithCallableStaging True argumentType argumentRecipe
            && if requireStagedCallable
              then
                null remainingArguments
                  && recipeCompatibleWithCallableStaging True resultType resultRecipe
              else
                recipeCompatibleWithCallableStaging
                  False
                  resultType
                  ( case remainingArguments of
                      [] -> resultRecipe
                      _ -> TypedClosureRecipe remainingArguments resultRecipe
                  )
        _ -> False
    _ -> expectedRecipe typeValue == Just recipe

stagedClosureRecipe :: TypedType -> Maybe TypedRepresentationRecipe
stagedClosureRecipe typeValue =
  case typeValue of
    TypedFunctionType {} -> expectedRecipeWithCallableStaging True typeValue
    _ -> Nothing

expectedRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedRecipe = expectedRecipeWithCallableStaging False

expectedValueRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedValueRecipe = expectedRecipeWithCallableStaging True

expectedRecipeWithCallableStaging :: Bool -> TypedType -> Maybe TypedRepresentationRecipe
expectedRecipeWithCallableStaging stageCallable typeValue =
  case typeValue of
    TypedIntType -> Just (TypedSignedIntegerRecipe 64)
    TypedFloatType -> Just (TypedFloatRecipe 64)
    TypedNumericType numericType -> Just (numericRecipe numericType)
    TypedBoolType -> Just TypedBoolRecipe
    TypedCharType -> Just TypedCharRecipe
    TypedTextType -> Just TypedManagedTextRecipe
    TypedListType elementType -> TypedManagedListRecipe <$> expectedRecipeWithCallableStaging True elementType
    TypedTupleType [] -> Just TypedUnitRecipe
    TypedTupleType elementTypes -> TypedManagedProductRecipe <$> traverse (expectedRecipeWithCallableStaging True) elementTypes
    TypedDataType name arguments -> Just (TypedManagedVariantRecipe name arguments)
    TypedFunctionType argumentType resultType -> do
      argumentRecipe <- expectedRecipeWithCallableStaging True argumentType
      resultRecipe <- expectedRecipeWithCallableStaging stageCallable resultType
      pure
        ( if stageCallable
            then TypedClosureRecipe [argumentRecipe] resultRecipe
            else case resultRecipe of
              TypedClosureRecipe remainingArguments finalResult ->
                TypedClosureRecipe (argumentRecipe : remainingArguments) finalResult
              _ -> TypedClosureRecipe [argumentRecipe] resultRecipe
        )
    TypedTypeParameterType parameterId -> Just (TypedRepresentationParameterRecipe parameterId)

numericRecipe :: TypedNumericType -> TypedRepresentationRecipe
numericRecipe numericType =
  case numericType of
    TypedInt8Type -> TypedSignedIntegerRecipe 8
    TypedInt16Type -> TypedSignedIntegerRecipe 16
    TypedInt32Type -> TypedSignedIntegerRecipe 32
    TypedInt64Type -> TypedSignedIntegerRecipe 64
    TypedUInt8Type -> TypedUnsignedIntegerRecipe 8
    TypedUInt16Type -> TypedUnsignedIntegerRecipe 16
    TypedUInt32Type -> TypedUnsignedIntegerRecipe 32
    TypedUInt64Type -> TypedUnsignedIntegerRecipe 64
    TypedFloat16Type -> TypedFloatRecipe 16
    TypedFloat32Type -> TypedFloatRecipe 32
    TypedFloat64Type -> TypedFloatRecipe 64

isFunctionType :: TypedType -> Bool
isFunctionType TypedFunctionType {} = True
isFunctionType _ = False

invalidRecipeWidth :: TypedRepresentationRecipe -> Maybe Int
invalidRecipeWidth recipe =
  case recipe of
    TypedSignedIntegerRecipe width
      | width `notElem` [8, 16, 32, 64] -> Just width
    TypedUnsignedIntegerRecipe width
      | width `notElem` [8, 16, 32, 64] -> Just width
    TypedFloatRecipe width
      | width `notElem` [16, 32, 64] -> Just width
    TypedManagedListRecipe elementRecipe -> invalidRecipeWidth elementRecipe
    TypedManagedProductRecipe elementRecipes -> firstJust (map invalidRecipeWidth elementRecipes)
    TypedClosureRecipe parameters result -> firstJust (map invalidRecipeWidth (parameters <> [result]))
    _ -> Nothing

validRecipeWidth :: TypedRepresentationRecipe -> Bool
validRecipeWidth = isNothing . invalidRecipeWidth

hasUnboundTypeParameter :: Set TypedTypeParameterId -> TypedType -> Bool
hasUnboundTypeParameter scope typeValue =
  case typeValue of
    TypedListType elementType -> hasUnboundTypeParameter scope elementType
    TypedTupleType elementTypes -> any (hasUnboundTypeParameter scope) elementTypes
    TypedDataType _ arguments -> any (hasUnboundTypeParameter scope) arguments
    TypedFunctionType argument result -> hasUnboundTypeParameter scope argument || hasUnboundTypeParameter scope result
    TypedTypeParameterType parameterId -> not (Set.member parameterId scope)
    _ -> False

hasUnboundRepresentationParameter :: Set TypedTypeParameterId -> TypedRepresentationRecipe -> Bool
hasUnboundRepresentationParameter scope recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> hasUnboundRepresentationParameter scope elementRecipe
    TypedManagedProductRecipe elementRecipes -> any (hasUnboundRepresentationParameter scope) elementRecipes
    TypedClosureRecipe parameters result -> any (hasUnboundRepresentationParameter scope) parameters || hasUnboundRepresentationParameter scope result
    TypedRepresentationParameterRecipe parameterId -> not (Set.member parameterId scope)
    _ -> False

validateCoreName :: TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateCoreName path name =
  case name of
    TypedUnresolvedSourceName _ -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedUnresolvedQualifiedName _ _ -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedResolvedName _ namespace identifier
      | not (validResolvedIdentifier namespace identifier) ->
          [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedGeneratedName (TypedLambdaPatternArgument index)
      | index < 1 -> [failure path TypedUnresolvedName (TypedNameDetail name)]
    TypedGeneratedName (TypedOperatorBinding bindingName)
      | not (validOperatorBindingName bindingName) ->
          [failure path TypedUnresolvedName (TypedNameDetail name)]
    _ -> []

validOperatorBindingName :: Text -> Bool
validOperatorBindingName bindingName =
  case Text.stripPrefix "$operator:" bindingName of
    Just suffix ->
      maybe False isValidUserOperatorSymbol (decodeOperatorBindingSuffix suffix)
    Nothing -> False
  where
    decodeOperatorBindingSuffix suffix =
      Text.pack <$> traverse (`lookup` canonicalOperatorEncodingPairs) (Text.chunksOf 3 suffix)

canonicalOperatorEncodingPairs :: [(Text, Char)]
canonicalOperatorEncodingPairs =
  [ (encoded, character)
  | character <- ("!%&*+-/<>?^|~" :: String),
    encoded <- maybeToList (Text.stripPrefix "$operator:" (operatorBindingIdentifierText (Text.singleton character)))
  ]

validResolvedIdentifier :: TypedNameNamespace -> Text -> Bool
validResolvedIdentifier namespace identifier =
  case namespace of
    TypedValueNamespace ->
      validSourceIdentifier identifier || validQualifiedIdentifier identifier
    _ ->
      validSourceIdentifier identifier && identifierStartsUpper identifier

identifierStartsUpper :: Text -> Bool
identifierStartsUpper identifier =
  case Text.uncons identifier of
    Just (first, _) -> isUpper first
    Nothing -> False

validQualifiedIdentifier :: Text -> Bool
validQualifiedIdentifier identifier =
  case Text.splitOn "::" identifier of
    segments@(_ : _ : _) -> all validSourceIdentifier segments
    _ -> False

validSourceIdentifier :: Text -> Bool
validSourceIdentifier identifier =
  identifier `notElem` reservedIdentifiers
    && validIdentifierSpelling identifier
  where
    reservedIdentifiers =
      [ "module",
        "import",
        "as",
        "data",
        "value",
        "if",
        "then",
        "else",
        "case",
        "True",
        "False"
      ]

nodeInfoHasCompatibleIntrinsicContract :: TypedNodeInfo -> Bool
nodeInfoHasCompatibleIntrinsicContract (TypedNodeInfo typeValue recipe _ _) =
  validRecipeWidth recipe && typeRecipeCompatible typeValue recipe

recipeContractFailures :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedRepresentationRecipe -> TypedNodeInfo -> [TypedCoreValidationFailure]
recipeContractFailures path kind expectedRecipeValue actualInfo
  | nodeInfoHasCompatibleIntrinsicContract actualInfo,
    expectedRecipeValue /= typedNodeRecipe actualInfo =
      [failure path kind (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe actualInfo))]
  | otherwise = []

validIdentifierSpelling :: Text -> Bool
validIdentifierSpelling identifier =
  case Text.uncons identifier of
    Just (first, rest) ->
      (isAlpha first || first == '_')
        && Text.all validContinuation rest
    Nothing -> False
  where
    validContinuation character =
      isAlphaNum character
        || character == '_'
        || character == '\''
        || character == '!'

typeMentionsParameter :: TypedTypeParameterId -> TypedType -> Bool
typeMentionsParameter parameter typeValue =
  case typeValue of
    TypedListType elementType -> typeMentionsParameter parameter elementType
    TypedTupleType elementTypes -> any (typeMentionsParameter parameter) elementTypes
    TypedDataType _ arguments -> any (typeMentionsParameter parameter) arguments
    TypedFunctionType argument result ->
      typeMentionsParameter parameter argument || typeMentionsParameter parameter result
    TypedTypeParameterType candidate -> candidate == parameter
    _ -> False

recipeMentionsParameter :: TypedTypeParameterId -> TypedRepresentationRecipe -> Bool
recipeMentionsParameter parameter recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> recipeMentionsParameter parameter elementRecipe
    TypedManagedProductRecipe elementRecipes -> any (recipeMentionsParameter parameter) elementRecipes
    TypedClosureRecipe parameters result ->
      any (recipeMentionsParameter parameter) parameters || recipeMentionsParameter parameter result
    TypedRepresentationParameterRecipe candidate -> candidate == parameter
    _ -> False

duplicateParameterFailures :: (Ord identifier) => TypedCoreValidationPath -> TypedCoreValidationKind -> (identifier -> TypedCoreValidationDetail) -> [identifier] -> [TypedCoreValidationFailure]
duplicateParameterFailures path kind detailOf = snd . foldl' step (Set.empty, [])
  where
    step (seen, failures) identifier
      | Set.member identifier seen =
          (seen, failures <> [failure path kind (detailOf identifier)])
      | otherwise = (Set.insert identifier seen, failures)

directCallableRecipeArity :: TypedRepresentationRecipe -> Maybe Int
directCallableRecipeArity recipe =
  case recipe of
    TypedClosureRecipe arguments _ -> Just (length arguments)
    _ -> Nothing

substituteTypeParameters :: Map TypedTypeParameterId TypedType -> TypedType -> TypedType
substituteTypeParameters substitutions typeValue =
  case typeValue of
    TypedListType elementType -> TypedListType (substituteTypeParameters substitutions elementType)
    TypedTupleType elementTypes -> TypedTupleType (map (substituteTypeParameters substitutions) elementTypes)
    TypedDataType name arguments -> TypedDataType name (map (substituteTypeParameters substitutions) arguments)
    TypedFunctionType argument result -> TypedFunctionType (substituteTypeParameters substitutions argument) (substituteTypeParameters substitutions result)
    TypedTypeParameterType parameterId -> Map.findWithDefault typeValue parameterId substitutions
    _ -> typeValue

substituteRepresentationParameters :: Map TypedTypeParameterId TypedType -> TypedRepresentationRecipe -> TypedRepresentationRecipe
substituteRepresentationParameters substitutions recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe (substituteRepresentationParameters substitutions elementRecipe)
    TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe (map (substituteRepresentationParameters substitutions) elementRecipes)
    TypedManagedVariantRecipe name arguments -> TypedManagedVariantRecipe name (map (substituteTypeParameters substitutions) arguments)
    TypedClosureRecipe parameters result ->
      TypedClosureRecipe
        (map (substituteRepresentationParameters substitutions) parameters)
        (substituteRepresentationParameters substitutions result)
    TypedRepresentationParameterRecipe parameterId ->
      case Map.lookup parameterId substitutions >>= expectedValueRecipe of
        Just substituted -> substituted
        Nothing -> recipe
    _ -> recipe
