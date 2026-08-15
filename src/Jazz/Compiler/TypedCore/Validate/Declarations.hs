{-# LANGUAGE OverloadedStrings #-}

-- | Non-recursive declaration, scheme, and method-contract validation.
module Jazz.Compiler.TypedCore.Validate.Declarations
  ( capabilityEntry,
    duplicateImplMethodFailures,
    evidenceCapabilityEntries,
    implMethodContext,
    implMethodRequiresStagedLeadingLambdaRecipe,
    leadingLambdaCount,
    missingImplMethodFailures,
    schemeRequiresStagedLeadingLambdaRecipe,
    signatureBindingSchemeMismatch,
    statementCapabilityEntries,
    statementConstructorEntries,
    statementDataContractEntries,
    statementDataEntries,
    statementDefinedNameKeys,
    statementDefinedNames,
    statementImplEntries,
    statementSchemes,
    validateBindingValue,
    validateCallableBindingShape,
    validateClassDeclaration,
    validateDataDeclaration,
    validateImplMethodContract,
    validateInferredScheme,
    validateOrderedEvidenceParameters,
    validateScheme,
    validateSchemeWithOuterScope,
    validateSchemeWithOuterScopeUsing,
    validateSignatureBindingScheme,
    validateValueContract,
    visibleClassCollisionFailures,
    withSchemeScope,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Evidence
import Jazz.Compiler.TypedCore.Validate.Internal
import Jazz.Compiler.TypedCore.Validate.TypeRecipes

statementSchemes :: TypedStatement -> [(TypedBinderId, TypedScheme)]
statementSchemes statement =
  case statement of
    TypedLetStatement binderId _ _ scheme _ -> [(binderId, scheme)]
    TypedSignatureStatement {} -> []
    _ -> []

statementImplEntries :: TypedStatement -> [(TypedImplId, Set Text)]
statementImplEntries statement =
  case statement of
    TypedImplStatement (TypedImplDeclaration _ implId methods) ->
      [(implId, Set.fromList [methodKey | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods])]
    _ -> []

statementDefinedNameKeys :: [Text] -> TypedStatement -> [ResolvedNameKey]
statementDefinedNameKeys modulePath statement =
  [ key
  | name <- statementDefinedNames statement,
    key <- maybeToList (resolvedNameKey modulePath name)
  ]

statementDefinedNames :: TypedStatement -> [TypedCoreName]
statementDefinedNames statement =
  case statement of
    TypedLetStatement _ name _ _ _ -> [name]
    TypedSignatureStatement {} -> []
    TypedDataStatement (TypedDataDeclaration _ name _ constructors) ->
      name : [constructorName | TypedConstructorDeclaration _ constructorName _ _ <- constructors]
    TypedClassStatement (TypedClassDeclaration _ name _ _) -> [name]
    TypedImplStatement {} -> []
    TypedExpressionStatement {} -> []

statementDataEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, Int)]
statementDataEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ name parameters _) ->
      [(key, length parameters) | key <- maybeToList (definitionNameKey modulePath name)]
    _ -> []

statementDataContractEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, DataContract)]
statementDataContractEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ name parameters constructors) ->
      [ (key, DataContract parameters [fields | TypedConstructorDeclaration _ _ fields _ <- constructors])
      | key <- maybeToList (definitionNameKey modulePath name)
      ]
    _ -> []

statementConstructorEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, ConstructorContract)]
statementConstructorEntries modulePath statement =
  case statement of
    TypedDataStatement (TypedDataDeclaration _ dataName parameters constructors) ->
      [ (constructorKey, ConstructorContract binderId dataKey parameters fields)
      | dataKey <- maybeToList (definitionNameKey modulePath dataName),
        TypedConstructorDeclaration binderId constructorName fields _ <- constructors,
        constructorKey <- maybeToList (definitionNameKey modulePath constructorName)
      ]
    _ -> []

statementCapabilityEntries :: [Text] -> TypedStatement -> [(ResolvedNameKey, CapabilityContract)]
statementCapabilityEntries modulePath statement =
  case statement of
    TypedClassStatement declaration -> maybeToList (capabilityEntry modulePath declaration)
    _ -> []

evidenceCapabilityEntries ::
  Map ResolvedNameKey CapabilityContract ->
  Set ResolvedNameKey ->
  [(TypedBinderId, TypedScheme)] ->
  [(TypedEvidenceParameterRef, ResolvedNameKey)]
evidenceCapabilityEntries capabilityContracts eligibleCapabilities schemes =
  [ (TypedEvidenceParameterRef owner parameterId, capabilityKey)
  | (owner, TypedScheme _ _ parameters _ _ _ _) <- schemes,
    TypedEvidenceParameter parameterId (TypedCapabilityConstraint capability _ _) <- parameters,
    capabilityKey <- maybeToList (resolvedNameKey (binderModulePath owner) capability),
    Map.member capabilityKey capabilityContracts,
    Set.member capabilityKey eligibleCapabilities
  ]

capabilityEntry :: [Text] -> TypedClassDeclaration -> Maybe (ResolvedNameKey, CapabilityContract)
capabilityEntry modulePath (TypedClassDeclaration _ name parameters methods) = do
  key <- resolvedNameKey modulePath name
  pure
    ( key,
      CapabilityContract
        parameters
        ( Map.fromList
            [ (methodKey, scheme)
            | TypedMethodSignature methodName _ scheme <- methods,
              methodKey <- maybeToList (coreNameIdentifier methodName)
            ]
        )
    )

withSchemeScope :: TypedScheme -> ModuleContext -> ModuleContext
withSchemeScope (TypedScheme _ typeParameters _ primitiveConstraints _ _ _) context =
  context
    { moduleContextTypeScope = Set.union (Set.fromList typeParameters) (moduleContextTypeScope context),
      moduleContextPrimitiveConstraints = primitiveConstraints <> moduleContextPrimitiveConstraints context
    }

validateSignatureBindingScheme :: TypedCoreValidationPath -> TypedScheme -> TypedScheme -> [TypedCoreValidationFailure]
validateSignatureBindingScheme path signatureScheme bindingScheme =
  case signatureBindingSchemeMismatch signatureScheme bindingScheme of
    Nothing -> []
    Just (kind, detail) -> [failure path kind detail]

signatureBindingSchemeMismatch :: TypedScheme -> TypedScheme -> Maybe (TypedCoreValidationKind, TypedCoreValidationDetail)
signatureBindingSchemeMismatch
  (TypedScheme _ signatureParameters signatureEvidence signaturePrimitive signatureType signatureRecipe signatureShape)
  (TypedScheme bindingOwner bindingParameters bindingEvidence bindingPrimitive bindingType bindingRecipe bindingShape)
    | signatureParameters /= bindingParameters =
        bindingMismatch (TypedArityDetail (length signatureParameters) (length bindingParameters))
    | signatureEvidence /= bindingEvidence = bindingMismatch TypedNoValidationDetail
    | signaturePrimitive /= bindingPrimitive = bindingMismatch TypedNoValidationDetail
    | signatureType /= bindingType = bindingMismatch (TypedTypeDetail signatureType bindingType)
    | signatureRecipe /= bindingRecipe = bindingMismatch (TypedRecipeDetail signatureRecipe bindingRecipe)
    | signatureShape /= bindingShape =
        Just (TypedCallableShapeMismatch, TypedBinderDetail bindingOwner)
    | otherwise = Nothing
    where
      bindingMismatch detail = Just (TypedBindingValueMismatch, detail)

validateBindingValue :: TypedCoreValidationPath -> TypedScheme -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateBindingValue path (TypedScheme _ _ _ _ resultType resultRecipe _) info =
  valueFailures resultType resultRecipe
  where
    valueFailures expectedType expectedRecipeValue
      | typedNodeType info /= expectedType =
          [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (typedNodeType info))]
      | typedNodeRecipe info /= expectedRecipeValue =
          [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe info))]
      | otherwise = []

validateCallableBindingShape :: TypedCoreValidationPath -> TypedScheme -> TypedExpr -> [TypedCoreValidationFailure]
validateCallableBindingShape path (TypedScheme owner _ _ _ _ recipe callableShape) expression
  | callableShape == Just TypedDirectCallableShape,
    directCallableRecipeArity recipe /= Just (leadingLambdaCount expression) =
      [failure path TypedCallableShapeMismatch (TypedBinderDetail owner)]
  | otherwise = []

leadingLambdaCount :: TypedExpr -> Int
leadingLambdaCount expression =
  case expression of
    TypedLambdaExpr _ _ _ body -> 1 + leadingLambdaCount body
    _ -> 0

validateScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateScheme context path owner = validateSchemeWithOuterScope context path owner (moduleContextTypeScope context)

validateInferredScheme :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedScheme -> [TypedCoreValidationFailure]
validateInferredScheme context path owner =
  validateSchemeWithOuterScopeUsing
    validateRetainedCapabilityConstraint
    context
    path
    owner
    (moduleContextTypeScope context)

validateSchemeWithOuterScope :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> Set TypedTypeParameterId -> TypedScheme -> [TypedCoreValidationFailure]
validateSchemeWithOuterScope = validateSchemeWithOuterScopeUsing validateCapabilityConstraint

validateSchemeWithOuterScopeUsing ::
  (ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]) ->
  ModuleContext ->
  TypedCoreValidationPath ->
  TypedBinderId ->
  Set TypedTypeParameterId ->
  TypedScheme ->
  [TypedCoreValidationFailure]
validateSchemeWithOuterScopeUsing validateConstraint context path owner outerScope (TypedScheme schemeOwner typeParameters evidenceParameters primitiveConstraints resultType resultRecipe callableShape) =
  ownerFailures
    <> parameterShadowingFailures
    <> parameterOrderFailures
    <> validateOrderedEvidenceParameters path evidenceParameters
    <> concatMap validateEvidenceParameter evidenceParameters
    <> concatMap (validatePrimitiveConstraint context path parameterScope) primitiveConstraints
    <> validateType path parameterScope resultType
    <> concatMap (validateDataTypeApplications context path) (resultType : evidenceTypes <> primitiveTypes)
    <> validateRecipe path parameterScope resultRecipe
    <> validateTypeRecipe path parameterScope resultType resultRecipe
    <> validateCallableShape path schemeOwner resultType resultRecipe callableShape
  where
    ownerFailures
      | owner == schemeOwner = []
      | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail schemeOwner)]
    parameterShadowingFailures =
      [ failure path TypedDuplicateTypeParameter (TypedTypeParameterDetail parameter)
      | parameter <- typeParameters,
        Set.member parameter outerScope
      ]
    parameterOrderFailures
      | null parameterShadowingFailures =
          validateOrderedTypeParametersFrom path (nextTypeParameterOrdinal outerScope) typeParameters
      | otherwise = []
    parameterScope = Set.union outerScope (Set.fromList typeParameters)
    validateEvidenceParameter (TypedEvidenceParameter _ constraint) =
      validateConstraint context path parameterScope constraint
    evidenceTypes = [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidenceParameters]
    primitiveTypes =
      [ typeValue
      | constraint <- primitiveConstraints,
        typeValue <- case constraint of
          TypedNumericPrimitiveConstraint _ value -> [value]
          TypedStrictEqualityPrimitiveConstraint value -> [value]
      ]

validateOrderedEvidenceParameters :: TypedCoreValidationPath -> [TypedEvidenceParameter] -> [TypedCoreValidationFailure]
validateOrderedEvidenceParameters path parameters =
  duplicateFailures <> duplicateConstraintFailures <> orderFailures
  where
    parameterIds = [parameterId | TypedEvidenceParameter parameterId _ <- parameters]
    duplicateFailures = duplicateParameterFailures path TypedDuplicateEvidenceParameter TypedEvidenceParameterDetail parameterIds
    (_, _, duplicateConstraintFailures) =
      foldl' checkConstraint (Set.empty, Set.empty, []) parameters
    checkConstraint (seenIds, seenConstraints, failures) (TypedEvidenceParameter parameterId constraint)
      | Set.member parameterId seenIds =
          (seenIds, seenConstraints, failures)
      | Set.member constraint seenConstraints =
          ( Set.insert parameterId seenIds,
            seenConstraints,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidenceParameter
                     (TypedEvidenceParameterDetail parameterId)
                 ]
          )
      | otherwise =
          ( Set.insert parameterId seenIds,
            Set.insert constraint seenConstraints,
            failures
          )
    orderFailures =
      [ failure path TypedInvalidEvidenceParameterOrder (TypedIndexDetail index)
      | (index, TypedEvidenceParameter (TypedEvidenceParameterId actual) _) <- zip [0 ..] parameters,
        actual /= index
      ]

validateDataDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedDataDeclaration -> [TypedCoreValidationFailure]
validateDataDeclaration context path (TypedDataDeclaration spanValue name parameters constructors) =
  validateSpan path spanValue
    <> validateLocalDefinitionName context [TypedTypeNamespace] path name
    <> validateOrderedTypeParameters path parameters
    <> (if null constructors then [failure path TypedDataRecipeMismatch (TypedArityDetail 1 0)] else [])
    <> concatMap validateConstructor constructors
  where
    scope = Set.fromList parameters
    validateConstructor (TypedConstructorDeclaration binderId constructorName fields recipes) =
      validateLocalDefinitionName context [TypedConstructorNamespace] path constructorName
        <> validateBinderDefinition context path binderId constructorName
        <> concatMap (validateType path scope) fields
        <> concatMap (validateDataTypeApplications context path) fields
        <> concatMap (validateSourceDataTypeApplications context path) fields
        <> concatMap (validateRecipe path scope) recipes
        <> dataRecipeFailures fields recipes
    dataRecipeFailures fields recipes
      | length fields /= length recipes =
          [failure path TypedDataRecipeMismatch (TypedArityDetail (length fields) (length recipes))]
      | otherwise = concat (zipWith fieldFailure fields recipes)
    fieldFailure fieldType recipe =
      case expectedValueRecipe fieldType of
        Just expected
          | validRecipeWidth recipe && expected /= recipe ->
              [failure path TypedDataRecipeMismatch (TypedRecipeDetail expected recipe)]
        _ -> []

validateClassDeclaration :: ModuleContext -> TypedCoreValidationPath -> TypedClassDeclaration -> [TypedCoreValidationFailure]
validateClassDeclaration context path (TypedClassDeclaration spanValue name parameters methods) =
  validateSpan path spanValue
    <> validateLocalDefinitionName context [TypedCapabilityNamespace] path name
    <> visibleClassCollisionFailures context path name
    <> (if length parameters == 1 then [] else [failure path TypedMethodSelectionMismatch (TypedArityDetail 1 (length parameters))])
    <> validateOrderedTypeParameters path parameters
    <> duplicateParameterFailures
      path
      TypedDuplicateDeclaration
      TypedNameDetail
      [methodName | TypedMethodSignature methodName _ _ <- methods]
    <> concatMap validateMethod methods
  where
    validateMethod (TypedMethodSignature methodName methodSpan scheme@(TypedScheme binderId methodParameters evidenceParameters primitiveConstraints _ _ _)) =
      validateSpan path methodSpan
        <> validateLocalDefinitionName context [TypedValueNamespace] path methodName
        <> (case methodName of TypedGeneratedName {} -> [failure path TypedUnresolvedName (TypedNameDetail methodName)]; _ -> [])
        <> validateBinderDefinition context path binderId methodName
        <> classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints
        <> validateSchemeWithOuterScope context path binderId (Set.fromList parameters) scheme
        <> validateSourceSchemeDataTypes context path scheme
    classMethodSchemeShapeFailures methodParameters evidenceParameters primitiveConstraints =
      let obligationCount = length methodParameters + length evidenceParameters + length primitiveConstraints
       in if obligationCount == 0
            then []
            else [failure path TypedBindingValueMismatch (TypedArityDetail 0 obligationCount)]

visibleClassCollisionFailures :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
visibleClassCollisionFailures context path name =
  case localCapabilityIdentifier of
    Just identifier
      | any (externalCapabilityMatches identifier) (Set.toList (moduleContextSourceVisibleCapabilities context)) ->
          [failure path TypedDuplicateDeclaration (TypedNameDetail name)]
    _ -> []
  where
    localCapabilityIdentifier =
      case name of
        TypedResolvedName TypedCurrentModule TypedCapabilityNamespace identifier -> Just identifier
        TypedResolvedName TypedAmbientPrelude TypedCapabilityNamespace identifier
          | moduleContextPath context == ["Prelude"] -> Just identifier
        _ -> Nothing
    externalCapabilityMatches identifier key =
      case key of
        ResolvedNameKey modulePath TypedCapabilityNamespace candidate ->
          modulePath /= moduleContextPath context && candidate == identifier
        _ -> False

implMethodRequiresStagedLeadingLambdaRecipe :: ModuleContext -> TypedImplId -> Text -> Bool
implMethodRequiresStagedLeadingLambdaRecipe context implId methodKey =
  case lookupImplMethodScheme context implId methodKey of
    Right (Just (_, scheme)) -> schemeRequiresStagedLeadingLambdaRecipe scheme
    _ -> True

duplicateImplMethodFailures :: TypedCoreValidationPath -> [TypedMethodDefinition] -> [TypedCoreValidationFailure]
duplicateImplMethodFailures path methods = snd (foldl' step (Set.empty, []) methods)
  where
    step (seen, failures) (TypedMethodDefinition (TypedMethodId _ methodKey) _ name _ _)
      | Set.member methodKey seen =
          (seen, failures <> [failure path TypedDuplicateDeclaration (TypedNameDetail name)])
      | otherwise = (Set.insert methodKey seen, failures)

missingImplMethodFailures :: ModuleContext -> TypedCoreValidationPath -> TypedImplId -> [TypedMethodDefinition] -> [TypedCoreValidationFailure]
missingImplMethodFailures context path (TypedImplId _ capability targets) methods =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Nothing -> []
    Just (CapabilityContract parameters methodSchemes)
      | length targets /= length parameters -> []
      | otherwise ->
          [ failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)
          | methodKey <- Map.keys methodSchemes,
            Set.notMember methodKey providedMethodKeys
          ]
  where
    providedMethodKeys =
      Set.fromList
        [ methodKey
        | TypedMethodDefinition (TypedMethodId _ methodKey) _ _ _ _ <- methods
        ]

validateImplMethodContract :: ModuleContext -> TypedCoreValidationPath -> TypedImplId -> Text -> TypedExpr -> [TypedCoreValidationFailure]
validateImplMethodContract context path implId methodKey expression =
  case lookupImplMethodScheme context implId methodKey of
    Left () -> []
    Right Nothing -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodKey)]
    Right (Just (classParameters, scheme@(TypedScheme owner _ _ _ resultType resultRecipe _)))
      | length classParameters == length targets ->
          validateValueContract
            path
            (typedExpressionInfo expression)
            ( ValueContract
                (substituteTypeParameters substitutions qualifiedType)
                (substituteRepresentationParameters substitutions qualifiedRecipe)
            )
            <> validateCallableBindingShape path scheme expression
      | otherwise -> []
      where
        targets = implTargetTypes implId
        substitutions = Map.fromList (zip classParameters targets)
        ownerPath = binderModulePath owner
        (qualifiedType, qualifiedRecipe)
          | ownerPath == moduleContextPath context = (resultType, resultRecipe)
          | otherwise = (qualifyExternalType ownerPath resultType, qualifyExternalRecipe ownerPath resultRecipe)

implMethodContext :: ModuleContext -> TypedImplId -> Text -> ModuleContext
implMethodContext context implId methodKey =
  case lookupImplMethodScheme context implId methodKey of
    Right (Just (_, scheme)) ->
      withSchemeScope scheme context
    _ -> context

schemeRequiresStagedLeadingLambdaRecipe :: TypedScheme -> Bool
schemeRequiresStagedLeadingLambdaRecipe (TypedScheme _ _ _ _ _ _ callableShape) =
  case callableShape of
    Just TypedDirectCallableShape -> False
    _ -> True

validateValueContract :: TypedCoreValidationPath -> TypedNodeInfo -> ValueContract -> [TypedCoreValidationFailure]
validateValueContract path info (ValueContract expectedType expectedRecipeValue)
  | typedNodeType info /= expectedType =
      [failure path TypedBindingValueMismatch (TypedTypeDetail expectedType (typedNodeType info))]
  | typedNodeRecipe info /= expectedRecipeValue =
      [failure path TypedBindingValueMismatch (TypedRecipeDetail expectedRecipeValue (typedNodeRecipe info))]
  | otherwise = []
