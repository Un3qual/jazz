{-# LANGUAGE OverloadedStrings #-}

-- | Complete structural validation for the semantic typed-core boundary.
-- Validation is deliberately independent of inference, evaluation, and
-- lowering: it accepts an already-constructed contract value and reports all
-- invariant failures in stable structural order.
module JazzNext.Compiler.TypedCore.Validate
  ( validateTypedProgram,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.TypedCore

data ModuleContext = ModuleContext
  { moduleContextPath :: [Text],
    moduleContextVisibleModules :: Set [Text],
    moduleContextSchemes :: Map TypedBinderId TypedScheme
  }

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram (TypedProgram prelude modules entryModule) =
  duplicateModuleFailures allModules
    <> unknownEntryFailure
    <> maybe [] (validateModule allModulePaths True) prelude
    <> concatMap (validateModule allModulePaths False) modules
  where
    allModules = maybeToList prelude <> modules
    allModulePaths = Set.fromList (map typedModulePath allModules)
    unknownEntryFailure
      | Set.member entryModule allModulePaths = []
      | otherwise =
          [ failure
              TypedProgramPath
              TypedUnknownEntryModule
              (TypedTextDetail (renderModulePath entryModule))
          ]

validateModule :: Set [Text] -> Bool -> TypedModule -> [TypedCoreValidationFailure]
validateModule allModulePaths isPrelude moduleValue@(TypedModule modulePath sourcePath imports _ _ statements moduleInfo) =
  validateSourcePath modulePath sourcePath
    <> validateModuleInterface moduleValue
    <> duplicateBinderFailures modulePath statements
    <> concatMap (uncurry (validateStatement context)) (zip [0 ..] statements)
    <> validateNodeInfo context moduleValidationPath Set.empty moduleInfo
  where
    moduleValidationPath
      | isPrelude = TypedPreludePath
      | otherwise = TypedModulePath modulePath
    importedPaths = Set.fromList [path | TypedResolvedImport _ path _ _ <- imports]
    visibleModules = Set.insert modulePath (Set.insert ["Prelude"] (Set.union importedPaths allModulePaths))
    schemes =
      Map.fromList
        [ (binderId, scheme)
          | statement <- statements,
            (binderId, scheme) <- statementSchemes statement
        ]
    context =
      ModuleContext
        { moduleContextPath = modulePath,
          moduleContextVisibleModules = visibleModules,
          moduleContextSchemes = schemes
        }

validateSourcePath :: [Text] -> TypedSourcePath -> [TypedCoreValidationFailure]
validateSourcePath modulePath (TypedSourcePath sourcePath)
  | validRelativeSourcePath sourcePath = []
  | otherwise =
      [ failure
          (TypedModulePath modulePath)
          TypedInvalidSourcePath
          (TypedTextDetail sourcePath)
      ]

validRelativeSourcePath :: Text -> Bool
validRelativeSourcePath sourcePath =
  not (Text.null sourcePath)
    && not (Text.isPrefixOf "/" sourcePath)
    && not (Text.any (== '\\') sourcePath)
    && not (isDriveAbsolute sourcePath)
    && all validSegment (Text.splitOn "/" sourcePath)
  where
    validSegment segment = not (Text.null segment) && segment /= "." && segment /= ".."
    isDriveAbsolute path =
      case Text.unpack path of
        _ : ':' : _ -> True
        _ -> False

duplicateModuleFailures :: [TypedModule] -> [TypedCoreValidationFailure]
duplicateModuleFailures = snd . foldl' step (Set.empty, [])
  where
    step (seen, failures) moduleValue =
      let modulePath = typedModulePath moduleValue
       in if Set.member modulePath seen
            then
              ( seen,
                failures
                  <> [ failure
                         (TypedModulePath modulePath)
                         TypedDuplicateModule
                         (TypedTextDetail (renderModulePath modulePath))
                     ]
              )
            else (Set.insert modulePath seen, failures)

duplicateBinderFailures :: [Text] -> [TypedStatement] -> [TypedCoreValidationFailure]
duplicateBinderFailures modulePath statements = snd (foldl' step (Set.empty, []) indexedBinders)
  where
    indexedBinders =
      [ (statementIndex, binderId)
        | (statementIndex, statement) <- zip [0 ..] statements,
          binderId <- statementBinderDefinitions statement
      ]
    step (seen, failures) (statementIndex, binderId)
      | Set.member binderId seen =
          ( seen,
            failures
              <> [ failure
                     (TypedStatementPath modulePath statementIndex)
                     TypedDuplicateBinder
                     (TypedBinderDetail binderId)
                 ]
          )
      | otherwise = (Set.insert binderId seen, failures)

statementBinderDefinitions :: TypedStatement -> [TypedBinderId]
statementBinderDefinitions statement =
  case statement of
    TypedLetStatement binderId _ _ _ _ -> [binderId]
    TypedSignatureStatement binderId _ _ _ -> [binderId]
    TypedDataStatement (TypedDataDeclaration _ _ _ constructors) ->
      [binderId | TypedConstructorDeclaration binderId _ _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ _ _ methods) ->
      [binderId | TypedMethodSignature _ _ (TypedScheme binderId _ _ _ _ _) <- methods]
    TypedImplStatement (TypedImplDeclaration _ _ methods) ->
      [binderId | TypedMethodDefinition _ binderId _ _ _ <- methods]
    TypedExpressionStatement {} -> []

statementSchemes :: TypedStatement -> [(TypedBinderId, TypedScheme)]
statementSchemes statement =
  case statement of
    TypedLetStatement binderId _ _ scheme _ -> [(binderId, scheme)]
    TypedSignatureStatement binderId _ _ scheme -> [(binderId, scheme)]
    TypedClassStatement (TypedClassDeclaration _ _ _ methods) ->
      [(binderId, scheme) | TypedMethodSignature _ _ scheme@(TypedScheme binderId _ _ _ _ _) <- methods]
    _ -> []

validateStatement :: ModuleContext -> Int -> TypedStatement -> [TypedCoreValidationFailure]
validateStatement context statementIndex statement =
  case statement of
    TypedLetStatement binderId name _ scheme expression ->
      validateCoreName statementPath name
        <> validateScheme context statementPath binderId scheme
        <> validateExpression context statementIndex [0] expression
    TypedSignatureStatement binderId name _ scheme ->
      validateCoreName statementPath name
        <> validateScheme context statementPath binderId scheme
    TypedDataStatement declaration -> validateDataDeclaration context statementPath declaration
    TypedClassStatement declaration -> validateClassDeclaration context statementPath declaration
    TypedImplStatement declaration -> validateImplDeclaration context statementIndex statementPath declaration
    TypedExpressionStatement _ expression -> validateExpression context statementIndex [0] expression
  where
    statementPath = TypedStatementPath (moduleContextPath context) statementIndex

validateScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateScheme _ path owner (TypedScheme schemeOwner typeParameters evidenceParameters primitiveConstraints resultType resultRecipe) =
  ownerFailures
    <> validateOrderedTypeParameters path typeParameters
    <> validateOrderedEvidenceParameters path evidenceParameters
    <> concatMap (validateEvidenceParameter path parameterScope) evidenceParameters
    <> concatMap (validatePrimitiveConstraint path parameterScope) primitiveConstraints
    <> validateType path parameterScope resultType
    <> validateRecipe path parameterScope resultRecipe
    <> validateTypeRecipe path parameterScope resultType resultRecipe
  where
    ownerFailures
      | owner == schemeOwner = []
      | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail schemeOwner)]
    parameterScope = Set.fromList typeParameters

validateOrderedTypeParameters :: TypedCoreValidationPath -> [TypedTypeParameterId] -> [TypedCoreValidationFailure]
validateOrderedTypeParameters path parameters = duplicateFailures <> orderFailures
  where
    duplicateFailures = duplicateParameterFailures path TypedDuplicateTypeParameter TypedTypeParameterDetail parameters
    orderFailures =
      [ failure path TypedInvalidTypeParameterOrder (TypedIndexDetail index)
        | (index, TypedTypeParameterId actual) <- zip [0 ..] parameters,
          actual /= index
      ]

validateOrderedEvidenceParameters :: TypedCoreValidationPath -> [TypedEvidenceParameter] -> [TypedCoreValidationFailure]
validateOrderedEvidenceParameters path parameters = duplicateFailures <> orderFailures
  where
    parameterIds = [parameterId | TypedEvidenceParameter parameterId _ <- parameters]
    duplicateFailures = duplicateParameterFailures path TypedDuplicateEvidenceParameter TypedEvidenceParameterDetail parameterIds
    orderFailures =
      [ failure path TypedInvalidEvidenceParameterOrder (TypedIndexDetail index)
        | (index, TypedEvidenceParameter (TypedEvidenceParameterId actual) _) <- zip [0 ..] parameters,
          actual /= index
      ]

duplicateParameterFailures :: Ord identifier => TypedCoreValidationPath -> TypedCoreValidationKind -> (identifier -> TypedCoreValidationDetail) -> [identifier] -> [TypedCoreValidationFailure]
duplicateParameterFailures path kind detailOf = snd . foldl' step (Set.empty, [])
  where
    step (seen, failures) identifier
      | Set.member identifier seen =
          (seen, failures <> [failure path kind (detailOf identifier)])
      | otherwise = (Set.insert identifier seen, failures)

validateEvidenceParameter :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedEvidenceParameter -> [TypedCoreValidationFailure]
validateEvidenceParameter path scope (TypedEvidenceParameter _ constraint) =
  validateCapabilityConstraint path scope constraint

validatePrimitiveConstraint :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
validatePrimitiveConstraint path scope constraint =
  case constraint of
    TypedNumericPrimitiveConstraint _ typeValue -> validateType path scope typeValue
    TypedStrictEqualityPrimitiveConstraint typeValue -> validateType path scope typeValue

validateCapabilityConstraint :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateCapabilityConstraint path scope (TypedCapabilityConstraint _ _ targetType) = validateType path scope targetType

validateDataDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedDataDeclaration -> [TypedCoreValidationFailure]
validateDataDeclaration _ path (TypedDataDeclaration _ name parameters constructors) =
  validateCoreName path name
    <> validateOrderedTypeParameters path parameters
    <> concatMap validateConstructor constructors
  where
    scope = Set.fromList parameters
    validateConstructor (TypedConstructorDeclaration _ constructorName fields recipes) =
      validateCoreName path constructorName
        <> concatMap (validateType path scope) fields
        <> concatMap (validateRecipe path scope) recipes
        <> dataRecipeFailures fields recipes
    dataRecipeFailures fields recipes
      | length fields /= length recipes =
          [failure path TypedDataRecipeMismatch (TypedArityDetail (length fields) (length recipes))]
      | otherwise = concat (zipWith fieldFailure fields recipes)
    fieldFailure fieldType recipe =
      case expectedRecipe fieldType of
        Just expected
          | validRecipeWidth recipe && expected /= recipe ->
              [failure path TypedDataRecipeMismatch (TypedRecipeDetail expected recipe)]
        _ -> []

validateClassDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedClassDeclaration -> [TypedCoreValidationFailure]
validateClassDeclaration context path (TypedClassDeclaration _ name parameters methods) =
  validateCoreName path name
    <> validateOrderedTypeParameters path parameters
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodSignature methodName _ scheme@(TypedScheme binderId _ _ _ _ _)) =
      validateCoreName path methodName <> validateScheme context path binderId scheme

validateImplDeclaration :: ModuleContext -> Int -> TypedCoreValidationPath -> TypedImplDeclaration -> [TypedCoreValidationFailure]
validateImplDeclaration context statementIndex path (TypedImplDeclaration _ implId methods) =
  validateImplId path Set.empty implId
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodDefinition methodId _ name _ expression) =
      validateMethodId path Set.empty methodId
        <> validateCoreName path name
        <> validateExpression context statementIndex [0] expression

validateExpression :: ModuleContext -> Int -> [Int] -> TypedExpr -> [TypedCoreValidationFailure]
validateExpression context statementIndex expressionPath expression =
  validateNodeInfo context path Set.empty (expressionInfo expression)
    <> expressionOwnedFailures
    <> concatMap (uncurry validateChild) (zip [0 ..] (expressionChildren expression))
  where
    path = TypedExpressionPath (moduleContextPath context) statementIndex expressionPath
    validateChild childIndex child = validateExpression context statementIndex (expressionPath <> [childIndex]) child
    expressionOwnedFailures =
      case expression of
        TypedLiteralExpr _ _ -> []
        TypedVariableExpr _ name -> validateCoreName path name
        TypedLambdaExpr _ _ name _ -> validateCoreName path name
        TypedOperatorValueExpr _ operator -> validateOperatorRef path operator
        TypedListExpr {} -> []
        TypedTupleExpr {} -> []
        TypedApplyExpr info function argument -> validateApplication path info function argument
        TypedTypeApplicationExpr _ _ _ typeArgument -> validateType path Set.empty typeArgument
        TypedIfExpr info condition thenExpression elseExpression ->
          validateConditional path info condition thenExpression elseExpression
        TypedPatternCaseExpr info scrutinee arms -> validateCase context statementIndex expressionPath path info scrutinee arms
        TypedBinaryExpr _ operator _ _ -> validateOperatorRef path operator
        TypedLeftSectionExpr _ _ operator -> validateOperatorRef path operator
        TypedRightSectionExpr _ operator _ -> validateOperatorRef path operator
        TypedBlockExpr _ statements -> concatMap (uncurry (validateStatement context)) (zip [statementIndex ..] statements)

expressionChildren :: TypedExpr -> [TypedExpr]
expressionChildren expression =
  case expression of
    TypedLiteralExpr {} -> []
    TypedVariableExpr {} -> []
    TypedLambdaExpr _ _ _ body -> [body]
    TypedOperatorValueExpr {} -> []
    TypedListExpr _ expressions -> expressions
    TypedTupleExpr _ expressions -> expressions
    TypedApplyExpr _ function argument -> [function, argument]
    TypedTypeApplicationExpr _ function _ _ -> [function]
    TypedIfExpr _ condition thenExpression elseExpression -> [condition, thenExpression, elseExpression]
    TypedPatternCaseExpr _ scrutinee arms -> scrutinee : concatMap armExpressions arms
    TypedBinaryExpr _ _ left right -> [left, right]
    TypedLeftSectionExpr _ left _ -> [left]
    TypedRightSectionExpr _ _ right -> [right]
    TypedBlockExpr {} -> []
  where
    armExpressions (TypedCaseArm _ guard result) = maybeToList guard <> [result]

validateApplication :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateApplication path (TypedNodeInfo resultType _ _ _) function argument =
  case nodeType (expressionInfo function) of
    TypedFunctionType expectedArgument expectedResult ->
      argumentFailures expectedArgument <> resultFailures expectedResult
    actualFunctionType ->
      [ failure
          path
          TypedApplicationFunctionMismatch
          (TypedTypeDetail (TypedFunctionType (nodeType (expressionInfo argument)) resultType) actualFunctionType)
      ]
  where
    actualArgument = nodeType (expressionInfo argument)
    argumentFailures expected
      | expected == actualArgument = []
      | otherwise = [failure path TypedApplicationArgumentMismatch (TypedTypeDetail expected actualArgument)]
    resultFailures expected
      | expected == resultType = []
      | otherwise = [failure path TypedApplicationResultMismatch (TypedTypeDetail expected resultType)]

validateConditional :: TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> TypedExpr -> TypedExpr -> [TypedCoreValidationFailure]
validateConditional path (TypedNodeInfo resultType _ _ _) condition thenExpression elseExpression =
  conditionFailures <> branchFailures <> resultFailures
  where
    conditionType = nodeType (expressionInfo condition)
    thenType = nodeType (expressionInfo thenExpression)
    elseType = nodeType (expressionInfo elseExpression)
    conditionFailures
      | conditionType == TypedBoolType = []
      | otherwise = [failure path TypedConditionalConditionMismatch (TypedTypeDetail TypedBoolType conditionType)]
    branchFailures
      | thenType == elseType = []
      | otherwise = [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType elseType)]
    resultFailures
      | thenType /= elseType || resultType == thenType = []
      | otherwise = [failure path TypedConditionalBranchMismatch (TypedTypeDetail thenType resultType)]

validateCase :: ModuleContext -> Int -> [Int] -> TypedCoreValidationPath -> TypedNodeInfo -> TypedExpr -> [TypedCaseArm] -> [TypedCoreValidationFailure]
validateCase context statementIndex _ _ (TypedNodeInfo resultType _ _ _) scrutinee arms =
  concatMap (uncurry validateArm) (zip [0 ..] arms)
  where
    scrutineeType = nodeType (expressionInfo scrutinee)
    validateArm armIndex (TypedCaseArm patternValue maybeGuard resultExpression) =
      validatePattern context statementIndex [armIndex] scrutineeType patternValue
        <> guardFailures armIndex maybeGuard
        <> resultFailures armIndex resultExpression
    guardFailures _ Nothing = []
    guardFailures armIndex (Just guard)
      | nodeType (expressionInfo guard) == TypedBoolType = []
      | otherwise =
          [ failure
              (TypedPatternPath (moduleContextPath context) statementIndex [armIndex])
              TypedPatternGuardMismatch
              (TypedTypeDetail TypedBoolType (nodeType (expressionInfo guard)))
          ]
    resultFailures armIndex resultExpression
      | nodeType (expressionInfo resultExpression) == resultType = []
      | otherwise =
          [ failure
              (TypedPatternPath (moduleContextPath context) statementIndex [armIndex])
              TypedPatternArmResultMismatch
              (TypedTypeDetail resultType (nodeType (expressionInfo resultExpression)))
          ]

validatePattern :: ModuleContext -> Int -> [Int] -> TypedType -> TypedPattern -> [TypedCoreValidationFailure]
validatePattern context statementIndex patternPath expectedType patternValue =
  validateNodeInfo context path Set.empty (patternInfo patternValue)
    <> scrutineeFailures
    <> patternOwnedFailures
    <> concatMap validateChild (patternChildrenWithTypes patternValue)
  where
    path = TypedPatternPath (moduleContextPath context) statementIndex patternPath
    actualType = nodeType (patternInfo patternValue)
    scrutineeFailures
      | actualType == expectedType = []
      | otherwise = [failure path TypedPatternScrutineeMismatch (TypedTypeDetail expectedType actualType)]
    patternOwnedFailures =
      case patternValue of
        TypedVariablePattern _ _ name -> validateCoreName path name
        TypedLiteralPattern {} -> []
        TypedConstructorPattern _ name _ -> validateCoreName path name
        TypedAsPattern _ _ name _ -> validateCoreName path name
        TypedOrPattern _ alternatives -> validateOrPattern path alternatives
        _ -> []
    validateChild (childIndex, childType, childPattern) =
      validatePattern context statementIndex (patternPath <> [childIndex]) childType childPattern

patternChildrenWithTypes :: TypedPattern -> [(Int, TypedType, TypedPattern)]
patternChildrenWithTypes patternValue =
  case patternValue of
    TypedConstructorPattern _ _ patterns -> indexedPatternTypes patterns
    TypedListPattern info patterns ->
      case nodeType info of
        TypedListType elementType -> [(index, elementType, pattern') | (index, pattern') <- zip [0 ..] patterns]
        _ -> indexedPatternTypes patterns
    TypedConsListPattern info headPattern tailPattern ->
      case nodeType info of
        listType@(TypedListType elementType) -> [(0, elementType, headPattern), (1, listType, tailPattern)]
        otherType -> [(0, otherType, headPattern), (1, otherType, tailPattern)]
    TypedTuplePattern info patterns ->
      case nodeType info of
        TypedTupleType types ->
          [ (index, typeValue, pattern')
            | (index, (typeValue, pattern')) <- zip [0 ..] (zip types patterns)
          ]
        _ -> indexedPatternTypes patterns
    TypedAsPattern info _ _ nested -> [(0, nodeType info, nested)]
    TypedOrPattern info alternatives -> [(index, nodeType info, alternative) | (index, alternative) <- zip [0 ..] alternatives]
    _ -> []
  where
    indexedPatternTypes patterns =
      [(index, nodeType (patternInfo pattern'), pattern') | (index, pattern') <- zip [0 ..] patterns]

validateOrPattern :: TypedCoreValidationPath -> [TypedPattern] -> [TypedCoreValidationFailure]
validateOrPattern _ [] = []
validateOrPattern path (firstAlternative : rest) = concatMap compareAlternative rest
  where
    expected = patternBinderContract firstAlternative
    compareAlternative alternative
      | patternBinderContract alternative == expected = []
      | otherwise =
          case firstMismatchedBinder expected (patternBinderContract alternative) of
            Just binderId -> [failure path TypedOrPatternBinderMismatch (TypedBinderDetail binderId)]
            Nothing -> [failure path TypedOrPatternBinderMismatch TypedNoValidationDetail]

patternBinderContract :: TypedPattern -> [(TypedBinderId, TypedType, TypedRepresentationRecipe)]
patternBinderContract patternValue =
  case patternValue of
    TypedVariablePattern info binderId _ -> [(binderId, nodeType info, nodeRecipe info)]
    TypedConstructorPattern _ _ patterns -> concatMap patternBinderContract patterns
    TypedListPattern _ patterns -> concatMap patternBinderContract patterns
    TypedConsListPattern _ headPattern tailPattern -> patternBinderContract headPattern <> patternBinderContract tailPattern
    TypedTuplePattern _ patterns -> concatMap patternBinderContract patterns
    TypedAsPattern info binderId _ nested -> (binderId, nodeType info, nodeRecipe info) : patternBinderContract nested
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> patternBinderContract alternative
    _ -> []

firstMismatchedBinder :: [(TypedBinderId, TypedType, TypedRepresentationRecipe)] -> [(TypedBinderId, TypedType, TypedRepresentationRecipe)] -> Maybe TypedBinderId
firstMismatchedBinder expected actual =
  case dropWhile (uncurry (==)) (zip expected actual) of
    (_, (binderId, _, _)) : _ -> Just binderId
    [] ->
      case drop (length expected) actual of
        (binderId, _, _) : _ -> Just binderId
        [] -> Nothing

validateNodeInfo :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateNodeInfo context path parameterScope (TypedNodeInfo typeValue recipe instantiations evidenceSelections) =
  validateType path parameterScope typeValue
    <> validateRecipe path parameterScope recipe
    <> validateTypeRecipe path parameterScope typeValue recipe
    <> concatMap (validateInstantiation context path) instantiations
    <> validateEvidenceSelections context path typeValue evidenceSelections

validateInstantiation :: ModuleContext -> TypedCoreValidationPath -> TypedInstantiation -> [TypedCoreValidationFailure]
validateInstantiation context path (TypedInstantiation owner arguments _) =
  case Map.lookup owner (moduleContextSchemes context) of
    Nothing -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
    Just (TypedScheme _ parameters _ _ _ _) ->
      if map typeArgumentParameter arguments == parameters
        then concatMap (validateType path Set.empty . typeArgumentType) arguments
        else [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
  where
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId
    typeArgumentType (TypedTypeArgument _ typeValue) = typeValue

validateEvidenceSelections :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceSelections context path expressionType selections =
  concatMap validateSelection selections <> duplicateEvidenceUseFailures path selections
  where
    validateSelection selection =
      case selection of
        TypedSelectedEvidence evidenceUse -> validateEvidenceUse context path evidenceUse
        TypedEvidenceCandidates (TypedCapabilityConstraint capability _ targetType) candidates
          | null candidates -> [failure path TypedMissingEvidence (TypedTextDetail capability)]
          | length candidates > 1 && not (isFunctionType expressionType) ->
              [failure path TypedAmbiguousEvidence (TypedArityDetail 1 (length candidates))]
          | otherwise -> concatMap (validateEvidenceCandidate path targetType) candidates

duplicateEvidenceUseFailures :: TypedCoreValidationPath -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
duplicateEvidenceUseFailures path selections = snd (foldl' step (Set.empty, []) parameterIds)
  where
    parameterIds =
      [ parameterId
        | TypedSelectedEvidence (TypedEvidenceUse (Just parameterId) _ _ _) <- selections
      ]
    step (seen, failures) parameterId
      | Set.member parameterId seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedEvidenceParameterDetail parameterId)
                 ]
          )
      | otherwise = (Set.insert parameterId seen, failures)

validateEvidenceUse :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceUse -> [TypedCoreValidationFailure]
validateEvidenceUse context path (TypedEvidenceUse _ constraint implId maybeMethodId) =
  validateCapabilityConstraint path Set.empty constraint
    <> validateImplId path Set.empty implId
    <> visibilityFailures
    <> methodFailures
  where
    visibilityFailures
      | Set.member (implModulePath implId) (moduleContextVisibleModules context) = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    methodFailures =
      case maybeMethodId of
        Just methodId@(TypedMethodId methodImplId _)
          | methodImplId /= implId ->
              validateMethodId path Set.empty methodId
                <> [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)]
        Just methodId -> validateMethodId path Set.empty methodId
        Nothing -> []

validateEvidenceCandidate :: TypedCoreValidationPath -> TypedType -> TypedEvidenceCandidate -> [TypedCoreValidationFailure]
validateEvidenceCandidate path targetType (TypedEvidenceCandidate implId maybeMethodId) =
  validateImplId path Set.empty implId
    <> targetFailures
    <> maybe [] (validateMethodId path Set.empty) maybeMethodId
  where
    targetFailures =
      case implTargetTypes implId of
        target : _
          | target /= targetType -> [failure path TypedMethodSelectionMismatch (TypedTypeDetail targetType target)]
        _ -> []

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
    TypedTupleType elementTypes -> concatMap (validateType path scope) elementTypes
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
          | expected /= recipe ->
              [ failure
                  path
                  (if isFunctionType typeValue then TypedCallableRecipeMismatch else TypedTypeRepresentationMismatch)
                  (TypedRecipeDetail expected recipe)
              ]
        _ -> []

expectedRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedRecipe typeValue =
  case typeValue of
    TypedIntType -> Just (TypedSignedIntegerRecipe 64)
    TypedFloatType -> Just (TypedFloatRecipe 64)
    TypedNumericType numericType -> Just (numericRecipe numericType)
    TypedBoolType -> Just TypedBoolRecipe
    TypedCharType -> Just TypedCharRecipe
    TypedTextType -> Just TypedManagedTextRecipe
    TypedListType elementType -> TypedManagedListRecipe <$> expectedRecipe elementType
    TypedTupleType elementTypes -> TypedManagedProductRecipe <$> traverse expectedRecipe elementTypes
    TypedDataType name arguments -> Just (TypedManagedVariantRecipe name arguments)
    TypedFunctionType {} -> do
      let (parameters, result) = flattenFunctionType typeValue
      parameterRecipes <- traverse expectedRecipe parameters
      resultRecipe <- expectedRecipe result
      pure (TypedClosureRecipe parameterRecipes resultRecipe)
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

flattenFunctionType :: TypedType -> ([TypedType], TypedType)
flattenFunctionType typeValue =
  case typeValue of
    TypedFunctionType argument result ->
      let (arguments, finalResult) = flattenFunctionType result
       in (argument : arguments, finalResult)
    _ -> ([], typeValue)

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
validRecipeWidth = maybe True (const False) . invalidRecipeWidth

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
    _ -> []

validateOperatorRef :: TypedCoreValidationPath -> TypedOperatorRef -> [TypedCoreValidationFailure]
validateOperatorRef path operator =
  case operator of
    TypedBuiltinOperator _ -> []
    TypedResolvedOperator name _ -> validateCoreName path name

validateImplId :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
validateImplId path scope (TypedImplId _ capability arguments) =
  validateCoreName path capability <> concatMap (validateType path scope) arguments

validateMethodId :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedMethodId -> [TypedCoreValidationFailure]
validateMethodId path scope (TypedMethodId implId _) = validateImplId path scope implId

validateModuleInterface :: TypedModule -> [TypedCoreValidationFailure]
validateModuleInterface (TypedModule modulePath _ _ exports (TypedModuleInterface values datas classes impls) statements _) =
  concatMap validateValueInterface values
    <> concatMap validateDataInterface datas
    <> concatMap validateClassInterface classes
    <> concatMap validateImplInterface impls
    <> concatMap validateExport exports
  where
    path = TypedInterfacePath modulePath
    declaredValues =
      [ (name, scheme)
        | statement <- statements,
          (name, scheme) <- statementValueDeclarations statement
      ]
    declaredDatas = [declaration | TypedDataStatement declaration <- statements]
    declaredClasses = [declaration | TypedClassStatement declaration <- statements]
    declaredImpls = [implId | TypedImplStatement (TypedImplDeclaration _ implId _) <- statements]
    validateValueInterface (TypedValueInterface name scheme)
      | (name, scheme) `elem` declaredValues = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateDataInterface (TypedDataInterface declaration@(TypedDataDeclaration _ name _ _))
      | declaration `elem` declaredDatas = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateClassInterface (TypedClassInterface declaration@(TypedClassDeclaration _ name _ _))
      | declaration `elem` declaredClasses = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedNameDetail name)]
    validateImplInterface (TypedImplInterface implId)
      | implId `elem` declaredImpls = []
      | otherwise = [failure path TypedModuleInterfaceMismatch (TypedImplDetail implId)]
    validateExport (TypedModuleExport namespace exportedName) =
      case namespace of
        TypedValueNamespace
          | any (interfaceNameMatches exportedName) values -> []
        TypedTypeNamespace
          | any (dataInterfaceNameMatches exportedName) datas || any (classInterfaceNameMatches exportedName) classes -> []
        TypedConstructorNamespace
          | any (dataInterfaceConstructorMatches exportedName) datas -> []
        TypedCapabilityNamespace
          | any (classInterfaceNameMatches exportedName) classes -> []
        _ -> [failure path TypedModuleInterfaceMismatch (TypedNameDetail (TypedResolvedName TypedCurrentModule namespace exportedName))]

statementValueDeclarations :: TypedStatement -> [(TypedCoreName, TypedScheme)]
statementValueDeclarations statement =
  case statement of
    TypedLetStatement _ name _ scheme _ -> [(name, scheme)]
    TypedSignatureStatement _ name _ scheme -> [(name, scheme)]
    _ -> []

interfaceNameMatches :: Text -> TypedValueInterface -> Bool
interfaceNameMatches expected (TypedValueInterface name _) = coreNameIdentifier name == Just expected

dataInterfaceNameMatches :: Text -> TypedDataInterface -> Bool
dataInterfaceNameMatches expected (TypedDataInterface (TypedDataDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

classInterfaceNameMatches :: Text -> TypedClassInterface -> Bool
classInterfaceNameMatches expected (TypedClassInterface (TypedClassDeclaration _ name _ _)) = coreNameIdentifier name == Just expected

dataInterfaceConstructorMatches :: Text -> TypedDataInterface -> Bool
dataInterfaceConstructorMatches expected (TypedDataInterface (TypedDataDeclaration _ _ _ constructors)) =
  any constructorMatches constructors
  where
    constructorMatches (TypedConstructorDeclaration _ name _ _) = coreNameIdentifier name == Just expected

coreNameIdentifier :: TypedCoreName -> Maybe Text
coreNameIdentifier name =
  case name of
    TypedResolvedName _ _ identifier -> Just identifier
    TypedBuiltinName identifier -> Just identifier
    _ -> Nothing

expressionInfo :: TypedExpr -> TypedNodeInfo
expressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedOperatorValueExpr info _ -> info
    TypedListExpr info _ -> info
    TypedTupleExpr info _ -> info
    TypedApplyExpr info _ _ -> info
    TypedTypeApplicationExpr info _ _ _ -> info
    TypedIfExpr info _ _ _ -> info
    TypedPatternCaseExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedLeftSectionExpr info _ _ -> info
    TypedRightSectionExpr info _ _ -> info
    TypedBlockExpr info _ -> info

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

nodeType :: TypedNodeInfo -> TypedType
nodeType (TypedNodeInfo typeValue _ _ _) = typeValue

nodeRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
nodeRecipe (TypedNodeInfo _ recipe _ _) = recipe

typedModulePath :: TypedModule -> [Text]
typedModulePath (TypedModule modulePath _ _ _ _ _ _) = modulePath

implModulePath :: TypedImplId -> [Text]
implModulePath (TypedImplId modulePath _ _) = modulePath

implTargetTypes :: TypedImplId -> [TypedType]
implTargetTypes (TypedImplId _ _ targetTypes) = targetTypes

renderModulePath :: [Text] -> Text
renderModulePath = Text.intercalate "::"

failure :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
failure = TypedCoreValidationFailure

maybeToList :: Maybe value -> [value]
maybeToList maybeValue =
  case maybeValue of
    Nothing -> []
    Just value -> [value]

firstJust :: [Maybe value] -> Maybe value
firstJust values =
  case values of
    [] -> Nothing
    Nothing : rest -> firstJust rest
    Just value : _ -> Just value
