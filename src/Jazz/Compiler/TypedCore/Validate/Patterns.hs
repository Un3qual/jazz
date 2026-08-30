{-# LANGUAGE OverloadedStrings #-}

-- | Pattern shape, binder, and constructor-contract validation.
module Jazz.Compiler.TypedCore.Validate.Patterns
  ( binderContractFromInfo,
    constructorPatternExpectedType,
    constructorPatternFieldContracts,
    constructorPatternFieldTypes,
    firstMismatchedBinder,
    nodeValueContract,
    patternBinderContract,
    patternBinderContractEqual,
    patternBinderContractsEqual,
    patternBinderOccurrences,
    patternBoundContracts,
    patternChildrenWithContracts,
    patternInfo,
    patternValueContract,
    validateConstructorPatternShape,
    validateListPatternShape,
    validateOrPattern,
    validatePattern,
    validatePatternLiteral,
    validatePatternMetadata,
    validateTuplePatternShape,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Evidence
import Jazz.Compiler.TypedCore.Validate.Internal
import Jazz.Compiler.TypedCore.Validate.TypeRecipes

patternBinderOccurrences :: [Text] -> [Int] -> [Int] -> TypedPattern -> [BinderOccurrence]
patternBinderOccurrences modulePath statementLocation patternPath patternValue =
  ownedOccurrences <> childOccurrences
  where
    patternValidationPath = TypedPatternPath modulePath statementLocation patternPath
    ownedOccurrences =
      case patternValue of
        TypedVariablePattern _ binderId _ -> [BinderOccurrence patternValidationPath binderId]
        TypedAsPattern _ binderId _ _ -> [BinderOccurrence patternValidationPath binderId]
        _ -> []
    childOccurrences =
      case patternValue of
        TypedConstructorPattern _ _ patterns -> indexedChildren patterns
        TypedListPattern _ patterns -> indexedChildren patterns
        TypedConsListPattern _ headPattern tailPattern -> indexedChildren [headPattern, tailPattern]
        TypedTuplePattern _ patterns -> indexedChildren patterns
        TypedAsPattern _ _ _ nested -> patternBinderOccurrences modulePath statementLocation (patternPath <> [0]) nested
        TypedOrPattern _ alternatives -> indexedChildren alternatives
        _ -> []
    indexedChildren patterns =
      concat
        [ patternBinderOccurrences modulePath statementLocation (patternPath <> [childIndex]) child
        | (childIndex, child) <- zip [0 ..] patterns
        ]

validatePattern :: ModuleContext -> [Int] -> [Int] -> ValueContract -> TypedPattern -> [TypedCoreValidationFailure]
validatePattern context statementLocation patternPath (ValueContract expectedType expectedRecipeValue) patternValue =
  validateNodeInfo context path (moduleContextTypeScope context) False Nothing Nothing (patternInfo patternValue)
    <> validatePatternMetadata path (patternInfo patternValue)
    <> scrutineeFailures
    <> patternOwnedFailures
    <> concatMap validateChild (patternChildrenWithContracts context patternValue)
  where
    path = TypedPatternPath (moduleContextPath context) statementLocation patternPath
    actualType = typedNodeType (patternInfo patternValue)
    scrutineeFailures
      | actualType /= expectedType = [failure path TypedPatternScrutineeMismatch (TypedTypeDetail expectedType actualType)]
      | otherwise = recipeContractFailures path TypedPatternScrutineeMismatch expectedRecipeValue (patternInfo patternValue)
    patternOwnedFailures =
      case patternValue of
        TypedVariablePattern _ binderId name -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name
        TypedLiteralPattern info literal -> validatePatternLiteral path info literal
        TypedConstructorPattern info name patterns ->
          validateVisibleNameInNamespaces [TypedConstructorNamespace] context path name
            <> validateConstructorPatternShape context path info name patterns
        TypedListPattern info _ -> validateListPatternShape path info
        TypedConsListPattern info _ _ -> validateListPatternShape path info
        TypedAsPattern _ binderId name _ -> validateLocalDefinitionName context [TypedValueNamespace] path name <> validateBinderDefinition context path binderId name
        TypedOrPattern _ alternatives -> validateOrPattern path alternatives
        TypedTuplePattern info patterns -> validateTuplePatternShape path info patterns
        _ -> []
    validateChild (childIndex, childContract, childPattern) =
      validatePattern context statementLocation (patternPath <> [childIndex]) childContract childPattern

validatePatternLiteral :: TypedCoreValidationPath -> TypedNodeInfo -> TypedLiteral -> [TypedCoreValidationFailure]
validatePatternLiteral path _ TypedFractionalLiteral {} =
  [failure path TypedPatternShapeMismatch TypedNoValidationDetail]
validatePatternLiteral path info literal =
  validateLiteral path info literal

validatePatternMetadata :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validatePatternMetadata path (TypedNodeInfo _ _ instantiations evidenceSelections)
  | null instantiations && null evidenceSelections = []
  | otherwise = [failure path TypedPatternShapeMismatch TypedNoValidationDetail]

validateTuplePatternShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedPattern] -> [TypedCoreValidationFailure]
validateTuplePatternShape path info patterns =
  case typedNodeType info of
    TypedTupleType types
      | length types == length patterns -> []
      | otherwise -> [failure path TypedPatternShapeMismatch (TypedArityDetail (length types) (length patterns))]
    actualType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail (TypedTupleType []) actualType)]

validateListPatternShape :: TypedCoreValidationPath -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateListPatternShape path info =
  case typedNodeType info of
    TypedListType _ -> []
    actualType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail (TypedListType actualType) actualType)]

validateConstructorPatternShape :: ModuleContext -> TypedCoreValidationPath -> TypedNodeInfo -> TypedCoreName -> [TypedPattern] -> [TypedCoreValidationFailure]
validateConstructorPatternShape context path info name patterns =
  case constructorPatternFieldTypes context info name of
    Just fieldTypes
      | length fieldTypes == length patterns -> []
      | otherwise -> [failure path TypedPatternShapeMismatch (TypedArityDetail (length fieldTypes) (length patterns))]
    Nothing ->
      case constructorPatternExpectedType context name of
        Just expectedType -> [failure path TypedPatternShapeMismatch (TypedTypeDetail expectedType (typedNodeType info))]
        Nothing -> []

constructorPatternExpectedType :: ModuleContext -> TypedCoreName -> Maybe TypedType
constructorPatternExpectedType context constructorName = do
  constructorKey <- resolvedNameKey (moduleContextPath context) constructorName
  ConstructorContract _ dataKey parameters _ <- Map.lookup constructorKey (moduleContextConstructorContracts context)
  pure (TypedDataType (resolvedNameFromKey context dataKey) (map TypedTypeParameterType parameters))

patternChildrenWithContracts :: ModuleContext -> TypedPattern -> [(Int, ValueContract, TypedPattern)]
patternChildrenWithContracts context patternValue =
  case patternValue of
    TypedConstructorPattern info name patterns ->
      case constructorPatternFieldContracts context info name of
        Just fieldContracts -> [(index, fieldContract, pattern') | (index, (fieldContract, pattern')) <- zip [0 ..] (zip fieldContracts patterns)]
        Nothing -> indexedPatternContracts patterns
    TypedListPattern info patterns ->
      case (typedNodeType info, typedNodeRecipe info) of
        (TypedListType elementType, TypedManagedListRecipe elementRecipe) ->
          [(index, ValueContract elementType elementRecipe, pattern') | (index, pattern') <- zip [0 ..] patterns]
        _ -> indexedPatternContracts patterns
    TypedConsListPattern info headPattern tailPattern ->
      case (typedNodeType info, typedNodeRecipe info) of
        (listType@(TypedListType elementType), listRecipe@(TypedManagedListRecipe elementRecipe)) ->
          [(0, ValueContract elementType elementRecipe, headPattern), (1, ValueContract listType listRecipe, tailPattern)]
        _ -> [(0, patternValueContract headPattern, headPattern), (1, patternValueContract tailPattern, tailPattern)]
    TypedTuplePattern info patterns ->
      case (typedNodeType info, typedNodeRecipe info) of
        (TypedTupleType types, TypedManagedProductRecipe recipes)
          | length types == length recipes ->
              [ (index, ValueContract typeValue recipe, pattern')
              | (index, (typeValue, recipe, pattern')) <- zip [0 ..] (zip3 types recipes patterns)
              ]
        _ -> indexedPatternContracts patterns
    TypedAsPattern info _ _ nested -> [(0, nodeValueContract info, nested)]
    TypedOrPattern info alternatives -> [(index, nodeValueContract info, alternative) | (index, alternative) <- zip [0 ..] alternatives]
    _ -> []
  where
    indexedPatternContracts patterns =
      [(index, patternValueContract pattern', pattern') | (index, pattern') <- zip [0 ..] patterns]

patternValueContract :: TypedPattern -> ValueContract
patternValueContract = nodeValueContract . patternInfo

nodeValueContract :: TypedNodeInfo -> ValueContract
nodeValueContract info = ValueContract (typedNodeType info) (typedNodeRecipe info)

constructorPatternFieldContracts :: ModuleContext -> TypedNodeInfo -> TypedCoreName -> Maybe [ValueContract]
constructorPatternFieldContracts context info constructorName = do
  fieldTypes <- constructorPatternFieldTypes context info constructorName
  fieldRecipes <- traverse expectedValueRecipe fieldTypes
  pure (zipWith ValueContract fieldTypes fieldRecipes)

constructorPatternFieldTypes :: ModuleContext -> TypedNodeInfo -> TypedCoreName -> Maybe [TypedType]
constructorPatternFieldTypes context info constructorName = do
  constructorKey <- resolvedNameKey (moduleContextPath context) constructorName
  ConstructorContract _ dataKey parameters fieldTypes <- Map.lookup constructorKey (moduleContextConstructorContracts context)
  case typedNodeType info of
    TypedDataType dataName arguments -> do
      actualDataKey <- resolvedNameKey (moduleContextPath context) dataName
      if actualDataKey == dataKey && length parameters == length arguments
        then pure (map (substituteTypeParameters (Map.fromList (zip parameters arguments))) fieldTypes)
        else Nothing
    _ -> Nothing

validateOrPattern :: TypedCoreValidationPath -> [TypedPattern] -> [TypedCoreValidationFailure]
validateOrPattern path [] = [failure path TypedPatternShapeMismatch (TypedArityDetail 2 0)]
validateOrPattern path [_] = [failure path TypedPatternShapeMismatch (TypedArityDetail 2 1)]
validateOrPattern path (firstAlternative : rest) = concatMap compareAlternative rest
  where
    expected = patternBinderContract firstAlternative
    compareAlternative alternative
      | patternBinderContractsEqual expected actual = []
      | otherwise =
          case firstMismatchedBinder expected actual of
            Just binderId -> [failure path TypedOrPatternBinderMismatch (TypedBinderDetail binderId)]
            Nothing -> [failure path TypedOrPatternBinderMismatch TypedNoValidationDetail]
      where
        actual = patternBinderContract alternative

patternBinderNodes ::
  (TypedBinderId -> TypedCoreName -> TypedNodeInfo -> contract) ->
  TypedPattern ->
  [contract]
patternBinderNodes build patternValue =
  case patternValue of
    TypedVariablePattern info binderId name ->
      [build binderId name info]
    TypedConstructorPattern _ _ patterns -> concatMap recurse patterns
    TypedListPattern _ patterns -> concatMap recurse patterns
    TypedConsListPattern _ headPattern tailPattern -> recurse headPattern <> recurse tailPattern
    TypedTuplePattern _ patterns -> concatMap recurse patterns
    TypedAsPattern info binderId name nested ->
      build binderId name info : recurse nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> recurse alternative
    _ -> []
  where
    recurse = patternBinderNodes build

patternBinderContract :: TypedPattern -> [PatternBinderContract]
patternBinderContract =
  patternBinderNodes
    (\binderId name info -> PatternBinderContract binderId name (typedNodeType info) (typedNodeRecipe info))

patternBoundContracts :: TypedPattern -> [BinderContract]
patternBoundContracts = patternBinderNodes binderContractFromInfo

binderContractFromInfo :: TypedBinderId -> TypedCoreName -> TypedNodeInfo -> BinderContract
binderContractFromInfo binderId name info =
  BinderContract binderId name (typedNodeType info) (typedNodeRecipe info)

patternBinderContractsEqual :: [PatternBinderContract] -> [PatternBinderContract] -> Bool
patternBinderContractsEqual expected actual =
  length expected == length actual
    && isNothing (firstMismatchedBinder expected actual)

firstMismatchedBinder :: [PatternBinderContract] -> [PatternBinderContract] -> Maybe TypedBinderId
firstMismatchedBinder expected actual = go expected actual
  where
    go [] [] = Nothing
    go (PatternBinderContract binderId _ _ _ : _) [] = Just binderId
    go remainingExpected (candidate@(PatternBinderContract binderId _ _ _) : remainingActual) =
      case removeMatching candidate remainingExpected of
        Just unmatchedExpected -> go unmatchedExpected remainingActual
        Nothing -> Just binderId

    removeMatching _ [] = Nothing
    removeMatching candidate (expectedContract : remainingExpected)
      | patternBinderContractEqual expectedContract candidate = Just remainingExpected
      | otherwise = (expectedContract :) <$> removeMatching candidate remainingExpected

patternBinderContractEqual :: PatternBinderContract -> PatternBinderContract -> Bool
patternBinderContractEqual
  (PatternBinderContract _ expectedName expectedType expectedRecipeValue)
  (PatternBinderContract _ actualName actualType actualRecipeValue) =
    expectedName == actualName
      && expectedType == actualType
      && expectedRecipeValue == actualRecipeValue

patternInfo :: TypedPattern -> TypedNodeInfo
patternInfo patternValue =
  case patternValue of
    TypedWildcardPattern info -> info
    TypedVariablePattern info _ _ -> info
    TypedLiteralPattern info _ -> info
    TypedConstructorPattern info _ _ -> info
    TypedListPattern info _ -> info
    TypedConsListPattern info _ _ -> info
    TypedTuplePattern info _ -> info
    TypedAsPattern info _ _ _ -> info
    TypedOrPattern info _ -> info
