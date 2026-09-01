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
import Jazz.Compiler.BuiltinCatalog (numericTypeFloatMax)
import Jazz.Compiler.Name (operatorBindingIdentifierText)
import Jazz.Compiler.Parser.Operator (isValidUserOperatorSymbol)
import Jazz.Compiler.TypeRepresentation (NumericType (..), SemanticType (..))
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
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail SemanticBool typeValue)]

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
        SemanticInt -> supported expanded remaining
        SemanticFloat -> supported expanded remaining
        SemanticNumeric _ -> supported expanded remaining
        SemanticBool -> supported expanded remaining
        SemanticChar -> supported expanded remaining
        SemanticText -> supported expanded remaining
        SemanticList elementType ->
          supported expanded ((seen, elementType) : remaining)
        SemanticTuple elementTypes ->
          supported expanded (map (\elementType -> (seen, elementType)) elementTypes <> remaining)
        SemanticVariable _
          | typeParameterSupported currentType -> supported expanded remaining
          | otherwise -> False
        SemanticFunction {} -> False
        SemanticData name arguments
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
    SemanticList elementType ->
      typePositionUsesParameter context seen parameter elementType
    SemanticTuple elementTypes ->
      typePositionsUseParameter context seen parameter elementTypes
    SemanticData name arguments ->
      case resolvedNameKey (moduleContextPath context) name of
        Nothing -> (typeMentionsParameter parameter typeValue, seen)
        Just dataKey ->
          case Map.lookup dataKey (moduleContextDataContracts context) of
            Just (DataContract dataParameters _)
              | length dataParameters == length arguments ->
                  dataArgumentsUseParameter context seen parameter dataKey (zip dataParameters arguments)
            _ -> (typeMentionsParameter parameter typeValue, seen)
    SemanticFunction argument result ->
      case typePositionUsesParameter context seen parameter argument of
        (True, nextSeen) -> (True, nextSeen)
        (False, nextSeen) -> typePositionUsesParameter context nextSeen parameter result
    SemanticVariable candidate -> (candidate == parameter, seen)
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
  | otherwise = [failure path TypedBindingValueMismatch (TypedTypeDetail SemanticInt typeValue)]

numericConstraintAcceptsType :: TypedNumericConstraint -> TypedType -> Bool
numericConstraintAcceptsType numericConstraint typeValue =
  case numericConstraint of
    TypedIntegralLiteralNumericConstraint lower upper ->
      integralLiteralConstraintAcceptsType lower upper typeValue
    _ ->
      case typeValue of
        SemanticVariable _ -> True
        SemanticInt -> True
        SemanticFloat -> not (integralConstraint numericConstraint)
        SemanticNumeric numericType
          | integralConstraint numericConstraint -> numericTypeIsIntegral numericType
          | otherwise -> True
        _ -> False

integralLiteralConstraintAcceptsType :: Text -> Text -> TypedType -> Bool
integralLiteralConstraintAcceptsType lowerText upperText typeValue =
  case (parseDecimalBound lowerText, parseDecimalBound upperText) of
    (Just lower, Just upper)
      | lower <= upper ->
          case typeValue of
            SemanticVariable _ -> True
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
    SemanticInt -> signedBounds 64
    SemanticNumeric numericType ->
      case numericType of
        NumericInt8 -> signedBounds 8
        NumericInt16 -> signedBounds 16
        NumericInt32 -> signedBounds 32
        NumericInt64 -> signedBounds 64
        NumericUInt8 -> unsignedBounds 8
        NumericUInt16 -> unsignedBounds 16
        NumericUInt32 -> unsignedBounds 32
        NumericUInt64 -> unsignedBounds 64
        NumericFloat16 -> Nothing
        NumericFloat32 -> Nothing
        NumericFloat64 -> Nothing
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

numericTypeIsIntegral :: NumericType -> Bool
numericTypeIsIntegral numericType =
  case numericType of
    NumericInt8 -> True
    NumericInt16 -> True
    NumericInt32 -> True
    NumericInt64 -> True
    NumericUInt8 -> True
    NumericUInt16 -> True
    NumericUInt32 -> True
    NumericUInt64 -> True
    NumericFloat16 -> False
    NumericFloat32 -> False
    NumericFloat64 -> False

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
    SemanticList elementType -> concreteImplTargetType elementType
    SemanticTuple elementTypes -> all concreteImplTargetType elementTypes
    SemanticData _ arguments -> all concreteImplTargetType arguments
    SemanticFunction {} -> False
    SemanticVariable {} -> False
    _ -> True

literalMatchesType :: TypedLiteral -> TypedType -> Bool
literalMatchesType literal typeValue =
  case (literal, typeValue) of
    (TypedIntegerLiteral value, SemanticInt) -> integerLiteralFitsType value typeValue
    (TypedIntegerLiteral value, SemanticNumeric numericType) ->
      not (isFloatingNumericType numericType)
        && integerLiteralFitsType value typeValue
    (TypedFractionalLiteral whole fractional Nothing, SemanticFloat) ->
      fractionalLiteralFitsNumericType whole fractional NumericFloat64
    (TypedFractionalLiteral whole fractional Nothing, SemanticNumeric numericType) ->
      isFloatingNumericType numericType
        && fractionalLiteralFitsNumericType whole fractional numericType
    (TypedFractionalLiteral whole fractional (Just expectedType), SemanticNumeric actualType) ->
      expectedType == actualType
        && isFloatingNumericType actualType
        && fractionalLiteralFitsNumericType whole fractional actualType
    (TypedBooleanLiteral _, SemanticBool) -> True
    (TypedCharacterLiteral _, SemanticChar) -> True
    (TypedTextLiteral _, SemanticText) -> True
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

literalType :: TypedLiteral -> TypedType
literalType literal =
  case literal of
    TypedIntegerLiteral _ -> SemanticInt
    TypedFractionalLiteral _ _ Nothing -> SemanticFloat
    TypedFractionalLiteral _ _ (Just numericType) -> SemanticNumeric numericType
    TypedBooleanLiteral _ -> SemanticBool
    TypedCharacterLiteral _ -> SemanticChar
    TypedTextLiteral _ -> SemanticText

isFloatingNumericType :: NumericType -> Bool
isFloatingNumericType numericType = numericType `elem` [NumericFloat16, NumericFloat32, NumericFloat64]

validateType :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateType path scope typeValue =
  case typeValue of
    SemanticInt -> []
    SemanticFloat -> []
    SemanticNumeric _ -> []
    SemanticBool -> []
    SemanticChar -> []
    SemanticText -> []
    SemanticList elementType -> validateType path scope elementType
    SemanticTuple elementTypes ->
      ( if length elementTypes == 1
          then [failure path TypedCollectionShapeMismatch (TypedArityDetail 2 1)]
          else []
      )
        <> concatMap (validateType path scope) elementTypes
    SemanticData name arguments -> validateCoreName path name <> concatMap (validateType path scope) arguments
    SemanticFunction argument result -> validateType path scope argument <> validateType path scope result
    SemanticVariable parameterId
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
    (SemanticFunction {}, Nothing) -> mismatch
    (SemanticFunction {}, Just TypedDirectCallableShape)
      | callableRecipeCompatible typeValue recipe,
        maybe True (<= 0) (directCallableRecipeArity recipe) ->
          mismatch
      | otherwise -> []
    (SemanticFunction {}, Just TypedClosureCallableShape)
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
    SemanticFunction {} -> recipeCompatibleWithCallableStaging False typeValue
    _ -> const False

stagedClosureRecipeCompatible :: TypedType -> TypedRepresentationRecipe -> Bool
stagedClosureRecipeCompatible typeValue =
  case typeValue of
    SemanticFunction {} -> recipeCompatibleWithCallableStaging True typeValue
    _ -> const False

recipeCompatibleWithCallableStaging :: Bool -> TypedType -> TypedRepresentationRecipe -> Bool
recipeCompatibleWithCallableStaging requireStagedCallable typeValue recipe =
  case typeValue of
    SemanticFunction argumentType resultType ->
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
    SemanticFunction {} -> expectedRecipeWithCallableStaging True typeValue
    _ -> Nothing

expectedRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedRecipe = expectedRecipeWithCallableStaging False

expectedValueRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedValueRecipe = expectedRecipeWithCallableStaging True

expectedRecipeWithCallableStaging :: Bool -> TypedType -> Maybe TypedRepresentationRecipe
expectedRecipeWithCallableStaging stageCallable typeValue =
  case typeValue of
    SemanticInt -> Just (TypedSignedIntegerRecipe 64)
    SemanticFloat -> Just (TypedFloatRecipe 64)
    SemanticNumeric numericType -> Just (typedNumericRepresentationRecipe numericType)
    SemanticBool -> Just TypedBoolRecipe
    SemanticChar -> Just TypedCharRecipe
    SemanticText -> Just TypedManagedTextRecipe
    SemanticList elementType -> TypedManagedListRecipe <$> expectedRecipeWithCallableStaging True elementType
    SemanticTuple [] -> Just TypedUnitRecipe
    SemanticTuple elementTypes -> TypedManagedProductRecipe <$> traverse (expectedRecipeWithCallableStaging True) elementTypes
    SemanticData name arguments -> Just (TypedManagedVariantRecipe name arguments)
    SemanticFunction argumentType resultType -> do
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
    SemanticVariable parameterId -> Just (TypedRepresentationParameterRecipe parameterId)

isFunctionType :: TypedType -> Bool
isFunctionType SemanticFunction {} = True
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
    SemanticList elementType -> hasUnboundTypeParameter scope elementType
    SemanticTuple elementTypes -> any (hasUnboundTypeParameter scope) elementTypes
    SemanticData _ arguments -> any (hasUnboundTypeParameter scope) arguments
    SemanticFunction argument result -> hasUnboundTypeParameter scope argument || hasUnboundTypeParameter scope result
    SemanticVariable parameterId -> not (Set.member parameterId scope)
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
typeMentionsParameter = elem

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
duplicateParameterFailures path kind detailOf =
  collectDuplicateFailuresBy id (\identifier -> failure path kind (detailOf identifier))

directCallableRecipeArity :: TypedRepresentationRecipe -> Maybe Int
directCallableRecipeArity recipe =
  case recipe of
    TypedClosureRecipe arguments _ -> Just (length arguments)
    _ -> Nothing

substituteTypeParameters :: Map TypedTypeParameterId TypedType -> TypedType -> TypedType
substituteTypeParameters substitutions typeValue =
  case typeValue of
    SemanticList elementType -> SemanticList (substituteTypeParameters substitutions elementType)
    SemanticTuple elementTypes -> SemanticTuple (map (substituteTypeParameters substitutions) elementTypes)
    SemanticData name arguments -> SemanticData name (map (substituteTypeParameters substitutions) arguments)
    SemanticFunction argument result -> SemanticFunction (substituteTypeParameters substitutions argument) (substituteTypeParameters substitutions result)
    SemanticVariable parameterId -> Map.findWithDefault typeValue parameterId substitutions
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
