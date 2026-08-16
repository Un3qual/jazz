{-# LANGUAGE OverloadedStrings #-}

-- | Capability, instantiation, evidence, and node-metadata validation.
module Jazz.Compiler.TypedCore.Validate.Evidence
  ( applicationArgumentCount,
    candidateConstraintCanDefer,
    capabilityArity,
    capabilityConstraintLabel,
    capabilityMethodQualifier,
    duplicateEvidenceCandidateFailures,
    duplicateEvidenceUseFailures,
    duplicateInstantiationFailures,
    evidenceParameterRefId,
    instantiationContractAcceptsArguments,
    lookupConstructorContractByOwner,
    lookupImplMethodScheme,
    lookupInstantiationContract,
    methodKeyMatches,
    methodKeyParts,
    nodeInfoEvidenceSelections,
    nodeInfoInstantiations,
    numericConstraintEntails,
    qualifiedMethodCandidateKey,
    qualifiedMethodExpressionKey,
    targetArgumentRemains,
    validateCapabilityConstraint,
    validateCapabilityConstraintTarget,
    validateCapabilityConstraintWith,
    validateCapabilityName,
    validateBinderDefinition,
    validateDataTypeApplications,
    validateEvidenceCandidate,
    validateEvidenceImplId,
    validateEvidenceMethodId,
    validateEvidenceParameterBindings,
    validateEvidenceSelectionDataTypes,
    validateEvidenceSelections,
    validateEvidenceUse,
    validateImplId,
    validateImplIdWith,
    validateInstantiatedPrimitiveConstraint,
    validateInstantiation,
    validateMethodId,
    validateLocalDefinitionName,
    validateNodeInfo,
    validateRetainedCapabilityConstraint,
    validateRetainedCapabilityName,
    validateSourceDataTypeApplications,
    validateSourceSchemeDataTypes,
    validateVisibleNameInNamespaces,
  )
where

import Data.List (find, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog (isBuiltinSymbolName, isKernelBuiltinSymbolName)
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate.Internal
import Jazz.Compiler.TypedCore.Validate.TypeRecipes

validateCapabilityConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateCapabilityConstraint = validateCapabilityConstraintWith validateCapabilityName

validateRetainedCapabilityConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateRetainedCapabilityConstraint = validateCapabilityConstraintWith validateRetainedCapabilityName

validateCapabilityConstraintWith ::
  (ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]) ->
  ModuleContext ->
  TypedCoreValidationPath ->
  Set TypedTypeParameterId ->
  TypedCapabilityConstraint ->
  [TypedCoreValidationFailure]
validateCapabilityConstraintWith validateCapability context path scope (TypedCapabilityConstraint capability maybeMethod targetType) =
  validateCapabilityConstraintTarget path scope targetType
    <> validateCapability context path capability
    <> methodFailures
  where
    capabilityContract = do
      key <- resolvedNameKey (moduleContextPath context) capability
      Map.lookup key (moduleContextCapabilityContracts context)
    methodFailures =
      case (maybeMethod, capabilityContract) of
        (Nothing, _) -> []
        (Just method, Just (CapabilityContract _ methods))
          | any (methodKeyMatches capability method) (Map.keys methods) -> []
        (Just method, Just _) ->
          [failure path TypedMethodSelectionMismatch (TypedTextDetail method)]
        (Just _, Nothing) -> []

validateCapabilityConstraintTarget :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateCapabilityConstraintTarget path scope = validateType path scope

validateNodeInfo :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> Bool -> Maybe Text -> Maybe Text -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateNodeInfo context path parameterScope requireSelectedConsumer selectedMethodKey candidateMethodKey (TypedNodeInfo typeValue recipe instantiations evidenceSelections) =
  validateType path parameterScope typeValue
    <> validateDataTypeApplications context path typeValue
    <> validateRecipe path parameterScope recipe
    <> validateTypeRecipe path parameterScope typeValue recipe
    <> concatMap (validateInstantiation context path parameterScope) instantiations
    <> duplicateInstantiationFailures path instantiations
    <> validateEvidenceSelections context path requireSelectedConsumer selectedMethodKey candidateMethodKey evidenceSelections
    <> validateEvidenceParameterBindings context path instantiations evidenceSelections
    <> concatMap (validateEvidenceSelectionDataTypes context path) evidenceSelections

duplicateInstantiationFailures :: TypedCoreValidationPath -> [TypedInstantiation] -> [TypedCoreValidationFailure]
duplicateInstantiationFailures path instantiations = snd (foldl' step (Set.empty, []) instantiations)
  where
    step (seen, failures) (TypedInstantiation owner _ _)
      | Set.member owner seen =
          (seen, failures <> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)])
      | otherwise = (Set.insert owner seen, failures)

validateEvidenceSelectionDataTypes :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceSelection -> [TypedCoreValidationFailure]
validateEvidenceSelectionDataTypes context path selection =
  case selection of
    TypedSelectedEvidence (TypedEvidenceUse _ (TypedCapabilityConstraint _ _ targetType) implId maybeMethodId) ->
      validateDataTypeApplications context path targetType
        <> concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
        <> maybe [] (concatMap (validateDataTypeApplications context path) . implTargetTypes . methodImplId) maybeMethodId
    TypedEvidenceCandidates (TypedCapabilityConstraint _ _ targetType) candidates ->
      validateDataTypeApplications context path targetType
        <> concat
          [ concatMap (validateDataTypeApplications context path) (implTargetTypes implId)
              <> maybe [] (concatMap (validateDataTypeApplications context path) . implTargetTypes . methodImplId) maybeMethodId
          | TypedEvidenceCandidate implId maybeMethodId <- candidates
          ]
  where
    methodImplId (TypedMethodId implId _) = implId

validateDataTypeApplications :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedCoreValidationFailure]
validateDataTypeApplications context path typeValue =
  case typeValue of
    TypedListType elementType -> validateDataTypeApplications context path elementType
    TypedTupleType elementTypes -> concatMap (validateDataTypeApplications context path) elementTypes
    TypedDataType name arguments ->
      concatMap (validateDataTypeApplications context path) arguments
        <> case resolvedNameKey (moduleContextPath context) name >>= (`Map.lookup` moduleContextDataArities context) of
          Nothing -> [failure path TypedDataTypeMismatch (TypedNameDetail name)]
          Just expectedArity
            | expectedArity /= length arguments -> [failure path TypedDataTypeMismatch (TypedArityDetail expectedArity (length arguments))]
            | otherwise -> []
    TypedFunctionType argument result -> validateDataTypeApplications context path argument <> validateDataTypeApplications context path result
    _ -> []

validateSourceSchemeDataTypes :: ModuleContext -> TypedCoreValidationPath -> TypedScheme -> [TypedCoreValidationFailure]
validateSourceSchemeDataTypes context path (TypedScheme _ _ evidenceParameters primitiveConstraints resultType _ _) =
  concatMap (validateSourceDataTypeApplications context path) sourceTypes
  where
    sourceTypes =
      resultType
        : [targetType | TypedEvidenceParameter _ (TypedCapabilityConstraint _ _ targetType) <- evidenceParameters]
          <> [ targetType
             | primitiveConstraint <- primitiveConstraints,
               targetType <-
                 case primitiveConstraint of
                   TypedNumericPrimitiveConstraint _ typeValue -> [typeValue]
                   TypedStrictEqualityPrimitiveConstraint typeValue -> [typeValue]
             ]

validateSourceDataTypeApplications :: ModuleContext -> TypedCoreValidationPath -> TypedType -> [TypedCoreValidationFailure]
validateSourceDataTypeApplications context path typeValue =
  case typeValue of
    TypedListType elementType -> validateSourceDataTypeApplications context path elementType
    TypedTupleType elementTypes -> concatMap (validateSourceDataTypeApplications context path) elementTypes
    TypedDataType name arguments ->
      concatMap (validateSourceDataTypeApplications context path) arguments
        <> case resolvedNameKey (moduleContextPath context) name of
          Just key
            | Map.member key (moduleContextDataArities context),
              Set.notMember key (moduleContextVisibleNames context) ->
                [failure path TypedInvisibleName (TypedNameDetail name)]
          _ -> []
    TypedFunctionType argument result ->
      validateSourceDataTypeApplications context path argument
        <> validateSourceDataTypeApplications context path result
    _ -> []

validateInstantiation :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedInstantiation -> [TypedCoreValidationFailure]
validateInstantiation context path parameterScope (TypedInstantiation owner arguments maybeSpan) =
  maybe [] (validateSpan path) maybeSpan
    <> case lookupInstantiationContract context owner of
      Nothing -> [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
      Just contract@(InstantiationContract contractOwner _ _ primitiveConstraints)
        | not (instantiationContractAcceptsArguments arguments contract) ->
            [failure path TypedInstantiationMismatch (TypedBinderDetail owner)]
        | otherwise ->
            concatMap
              (\argument -> validateType path parameterScope (typeArgumentType argument) <> validateDataTypeApplications context path (typeArgumentType argument))
              arguments
              <> concatMap
                (validateInstantiatedPrimitiveConstraint context path parameterScope . instantiatePrimitiveConstraint contractOwner substitutions)
                primitiveConstraints
  where
    substitutions =
      Map.fromList
        [ (parameterId, typeValue)
        | TypedTypeArgument parameterId typeValue <- arguments
        ]
    typeArgumentType (TypedTypeArgument _ typeValue) = typeValue
    instantiatePrimitiveConstraint schemeOwner typeSubstitutions primitiveConstraint =
      case primitiveConstraint of
        TypedNumericPrimitiveConstraint numericConstraint typeValue ->
          TypedNumericPrimitiveConstraint numericConstraint (instantiateType schemeOwner typeSubstitutions typeValue)
        TypedStrictEqualityPrimitiveConstraint typeValue ->
          TypedStrictEqualityPrimitiveConstraint (instantiateType schemeOwner typeSubstitutions typeValue)
    instantiateType schemeOwner typeSubstitutions typeValue =
      substituteTypeParameters typeSubstitutions qualifiedType
      where
        ownerPath = binderModulePath schemeOwner
        qualifiedType
          | ownerPath == moduleContextPath context = typeValue
          | otherwise = qualifyExternalType ownerPath typeValue

instantiationContractAcceptsArguments :: [TypedTypeArgument] -> InstantiationContract -> Bool
instantiationContractAcceptsArguments arguments (InstantiationContract _ parameters evidenceParameters _) =
  map typeArgumentParameter arguments == parameters
    && (not (null parameters) || not (null evidenceParameters))
  where
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

validateInstantiatedPrimitiveConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
validateInstantiatedPrimitiveConstraint context path scope constraint =
  case constraint of
    TypedNumericPrimitiveConstraint required typeValue ->
      validateType path scope typeValue
        <> case typeValue of
          TypedTypeParameterType _
            | any (numericConstraintProvides required typeValue) (moduleContextPrimitiveConstraints context) -> []
            | otherwise -> unsupportedNumericTarget typeValue
          _ -> validateNumericConstraintTarget path required typeValue
    TypedStrictEqualityPrimitiveConstraint typeValue ->
      validateType path scope typeValue
        <> if strictEqualityOperandTypeSupported context typeValue
          then []
          else [failure path TypedBindingValueMismatch (TypedTypeDetail TypedBoolType typeValue)]
  where
    numericConstraintProvides required targetType provided =
      case provided of
        TypedNumericPrimitiveConstraint candidate candidateType ->
          candidateType == targetType && numericConstraintEntails candidate required
        _ -> False
    unsupportedNumericTarget typeValue =
      [failure path TypedBindingValueMismatch (TypedTypeDetail TypedIntType typeValue)]

numericConstraintEntails :: TypedNumericConstraint -> TypedNumericConstraint -> Bool
numericConstraintEntails provided required =
  case (provided, required) of
    (TypedIntegralLiteralNumericConstraint providedLower providedUpper, TypedIntegralLiteralNumericConstraint requiredLower requiredUpper) ->
      literalConstraintEntails providedLower providedUpper requiredLower requiredUpper
    (TypedIntegralLiteralNumericConstraint lower upper, _) ->
      validLiteralConstraint lower upper
    (_, TypedIntegralLiteralNumericConstraint {}) -> False
    (TypedIntegralNumericConstraint, _) -> True
    (_, TypedIntegralNumericConstraint) -> False
    (TypedRuntimeArithmeticNumericConstraint, _) -> True
    (_, TypedRuntimeArithmeticNumericConstraint) -> False
    (TypedRuntimeComparisonNumericConstraint, TypedRuntimeComparisonNumericConstraint) -> True
    (TypedRuntimeComparisonNumericConstraint, TypedAnyNumericConstraint) -> True
    (TypedAnyNumericConstraint, TypedAnyNumericConstraint) -> True
    (TypedAnyNumericConstraint, _) -> False
  where
    validLiteralConstraint lower upper =
      case (parseDecimalBound lower, parseDecimalBound upper) of
        (Just minimumValue, Just maximumValue) -> minimumValue <= maximumValue
        _ -> False
    literalConstraintEntails providedLower providedUpper requiredLower requiredUpper =
      case ( parseDecimalBound providedLower,
             parseDecimalBound providedUpper,
             parseDecimalBound requiredLower,
             parseDecimalBound requiredUpper
           ) of
        (Just providedMinimum, Just providedMaximum, Just requiredMinimum, Just requiredMaximum) ->
          providedMinimum <= providedMaximum
            && requiredMinimum <= requiredMaximum
            && providedMinimum <= requiredMinimum
            && requiredMaximum <= providedMaximum
        _ -> False

lookupInstantiationContract :: ModuleContext -> TypedBinderId -> Maybe InstantiationContract
lookupInstantiationContract context owner =
  case Map.lookup owner (moduleContextSchemes context) of
    Just (TypedScheme schemeOwner parameters evidenceParameters primitiveConstraints _ _ _) ->
      Just (InstantiationContract schemeOwner parameters evidenceParameters primitiveConstraints)
    Nothing ->
      case lookupConstructorContractByOwner context owner of
        Just (ConstructorContract constructorOwner _ parameters _) ->
          Just (InstantiationContract constructorOwner parameters [] [])
        Nothing -> Nothing

lookupConstructorContractByOwner :: ModuleContext -> TypedBinderId -> Maybe ConstructorContract
lookupConstructorContractByOwner context owner =
  find matchesOwner (Map.elems (moduleContextConstructorContracts context))
  where
    matchesOwner (ConstructorContract candidateOwner _ _ _) = candidateOwner == owner

qualifiedMethodCandidateKey :: ModuleContext -> TypedExpr -> Maybe Text
qualifiedMethodCandidateKey context expression = do
  methodKey <- qualifiedMethodExpressionKey expression
  if all (candidateConstraintCanDefer context methodKey suppliedArgumentCount) candidateConstraints
    then Just methodKey
    else Nothing
  where
    suppliedArgumentCount = applicationArgumentCount expression
    candidateConstraints =
      [ constraint
      | TypedEvidenceCandidates constraint _ <- nodeInfoEvidenceSelections (typedExpressionInfo expression)
      ]

applicationArgumentCount :: TypedExpr -> Int
applicationArgumentCount expression =
  case expression of
    TypedApplyExpr _ function _ -> 1 + applicationArgumentCount function
    TypedTypeApplicationExpr _ function _ _ -> applicationArgumentCount function
    _ -> 0

candidateConstraintCanDefer :: ModuleContext -> Text -> Int -> TypedCapabilityConstraint -> Bool
candidateConstraintCanDefer context methodKey suppliedArgumentCount constraint =
  case matchingMethodContracts of
    [(classParameter, TypedScheme _ _ _ _ methodType _ _)] ->
      targetArgumentRemains classParameter suppliedArgumentCount methodType
    _ -> False
  where
    matchingMethodContracts =
      [ (classParameter, scheme)
      | TypedCapabilityConstraint capability (Just constraintMethod) _ <- [constraint],
        key <- maybeToList (resolvedNameKey (moduleContextPath context) capability),
        methodKeyMatches capability constraintMethod methodKey,
        CapabilityContract [classParameter] methods <-
          maybeToList (Map.lookup key (moduleContextCapabilityContracts context)),
        (contractMethod, scheme) <- Map.toList methods,
        methodKeyMatches capability contractMethod methodKey
      ]

targetArgumentRemains :: TypedTypeParameterId -> Int -> TypedType -> Bool
targetArgumentRemains parameter suppliedArgumentCount methodType =
  case methodType of
    TypedFunctionType argument result
      | suppliedArgumentCount > 0 ->
          not (typeMentionsParameter parameter argument)
            && targetArgumentRemains parameter (suppliedArgumentCount - 1) result
      | typeMentionsParameter parameter argument -> True
      | otherwise -> targetArgumentRemains parameter 0 result
    _ -> False

qualifiedMethodExpressionKey :: TypedExpr -> Maybe Text
qualifiedMethodExpressionKey expression =
  case expression of
    TypedVariableExpr _ (TypedBuiltinName identifier) _ -> Just identifier
    TypedApplyExpr _ function _ -> qualifiedMethodExpressionKey function
    TypedTypeApplicationExpr _ function _ _ -> qualifiedMethodExpressionKey function
    _ -> Nothing

validateEvidenceSelections :: ModuleContext -> TypedCoreValidationPath -> Bool -> Maybe Text -> Maybe Text -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceSelections context path requireSelectedConsumer selectedMethodKey candidateMethodKey selections =
  concatMap validateSelection selections <> duplicateEvidenceUseFailures path selections
  where
    validateSelection selection =
      case selection of
        TypedSelectedEvidence evidenceUse@(TypedEvidenceUse maybeParameter constraint _ _) ->
          case maybeParameter of
            Just _ -> evidenceUseFailures
            Nothing
              | null evidenceUseFailures,
                requireSelectedConsumer ->
                  selectedMethodFailures constraint
              | otherwise -> evidenceUseFailures
          where
            evidenceUseFailures = validateEvidenceUse context path evidenceUse
        TypedEvidenceCandidates constraint@(TypedCapabilityConstraint capability constraintMethod _) candidates
          | null candidates -> [failure path TypedMissingEvidence (TypedTextDetail (capabilityConstraintLabel constraint))]
          | not (qualifiedMethodCandidates capability constraintMethod candidateMethodKey) ->
              [failure path TypedAmbiguousEvidence (TypedArityDetail 1 (length candidates))]
          | otherwise ->
              duplicateEvidenceCandidateFailures path candidates
                <> concatMap (validateEvidenceCandidate context path constraint) candidates
    selectedMethodFailures constraint@(TypedCapabilityConstraint capability constraintMethod _)
      | qualifiedMethodCandidates capability constraintMethod selectedMethodKey = []
      | otherwise =
          [ failure
              path
              TypedMethodSelectionMismatch
              (TypedTextDetail (capabilityConstraintLabel constraint))
          ]
    qualifiedMethodCandidates capability constraintMethod expressionMethod =
      case (constraintMethod, expressionMethod) of
        (Just expectedMethod, Just actualMethod) -> methodKeyMatches capability expectedMethod actualMethod
        _ -> False

duplicateEvidenceCandidateFailures :: TypedCoreValidationPath -> [TypedEvidenceCandidate] -> [TypedCoreValidationFailure]
duplicateEvidenceCandidateFailures path candidates = snd (foldl' step ([], []) candidates)
  where
    step (seen, failures) candidate@(TypedEvidenceCandidate implId _)
      | candidate `elem` seen =
          (seen, failures <> [failure path TypedDuplicateEvidence (TypedImplDetail implId)])
      | otherwise = (candidate : seen, failures)

validateEvidenceParameterBindings :: ModuleContext -> TypedCoreValidationPath -> [TypedInstantiation] -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateEvidenceParameterBindings context path instantiations selections =
  missingBindingFailures <> orderedBindingFailures <> concatMap validateSelection selections
  where
    expectedBindings = concatMap expectedBindingsFor instantiations
    orderedExpectedBindings = nub expectedBindings
    suppliedBindings =
      [ (parameterRef, constraint)
      | TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) constraint _ _) <- selections
      ]
    missingBindingFailures =
      [ failure path TypedMissingEvidence (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
      | (parameterRef, constraint) <- nub expectedBindings,
        (parameterRef, constraint) `notElem` suppliedBindings
      ]
    orderedBindingFailures
      | sort orderedExpectedBindings == sort suppliedBindings =
          [ failure
              path
              TypedInstantiationMismatch
              (TypedEvidenceParameterDetail (evidenceParameterRefId actualParameterRef))
          | (expectedBinding, actualBinding@(actualParameterRef, _)) <-
              zip orderedExpectedBindings suppliedBindings,
            expectedBinding /= actualBinding
          ]
      | otherwise = []
    validateSelection selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) constraint _ _)
          | (parameterRef, constraint) `elem` expectedBindings -> []
          | otherwise ->
              [ failure
                  path
                  TypedInstantiationMismatch
                  (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
              ]
        _ -> []
    expectedBindingsFor (TypedInstantiation owner arguments _) =
      case Map.lookup owner (moduleContextSchemes context) of
        Just (TypedScheme _ parameters evidenceParameters _ _ _ _)
          | map typeArgumentParameter arguments == parameters ->
              [ ( TypedEvidenceParameterRef owner parameterId,
                  instantiateConstraint owner substitutions constraint
                )
              | TypedEvidenceParameter parameterId constraint <- evidenceParameters
              ]
          where
            substitutions = Map.fromList [(parameterId, typeValue) | TypedTypeArgument parameterId typeValue <- arguments]
        _ -> []
    instantiateConstraint owner substitutions (TypedCapabilityConstraint capability method targetType) =
      TypedCapabilityConstraint capability method (substituteTypeParameters substitutions qualifiedTarget)
      where
        ownerPath = binderModulePath owner
        qualifiedTarget
          | ownerPath == moduleContextPath context = targetType
          | otherwise = qualifyExternalType ownerPath targetType
    typeArgumentParameter (TypedTypeArgument parameterId _) = parameterId

evidenceParameterRefId :: TypedEvidenceParameterRef -> TypedEvidenceParameterId
evidenceParameterRefId (TypedEvidenceParameterRef _ parameterId) = parameterId

duplicateEvidenceUseFailures :: TypedCoreValidationPath -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
duplicateEvidenceUseFailures path selections =
  snd (foldl' parameterStep (Set.empty, []) parameterRefs)
    <> snd (foldl' constraintStep (Set.empty, []) unboundConstraints)
  where
    parameterRefs =
      [ parameterRef
      | TypedSelectedEvidence (TypedEvidenceUse (Just parameterRef) _ _ _) <- selections
      ]
    unboundConstraints =
      mapMaybe unboundConstraint selections
    unboundConstraint selection =
      case selection of
        TypedSelectedEvidence (TypedEvidenceUse Nothing constraint _ _) -> Just constraint
        TypedEvidenceCandidates constraint _ -> Just constraint
        _ -> Nothing
    parameterStep (seen, failures) parameterRef
      | Set.member parameterRef seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedEvidenceParameterDetail (evidenceParameterRefId parameterRef))
                 ]
          )
      | otherwise = (Set.insert parameterRef seen, failures)
    constraintStep (seen, failures) constraint
      | Set.member constraint seen =
          ( seen,
            failures
              <> [ failure
                     path
                     TypedDuplicateEvidence
                     (TypedTextDetail (capabilityConstraintLabel constraint))
                 ]
          )
      | otherwise = (Set.insert constraint seen, failures)

capabilityConstraintLabel :: TypedCapabilityConstraint -> Text
capabilityConstraintLabel (TypedCapabilityConstraint capability maybeMethod _) =
  case maybeMethod of
    Just method -> method
    Nothing -> fromMaybe "" (coreNameIdentifier capability)

validateEvidenceUse :: ModuleContext -> TypedCoreValidationPath -> TypedEvidenceUse -> [TypedCoreValidationFailure]
validateEvidenceUse context path (TypedEvidenceUse maybeParameterRef (TypedCapabilityConstraint capability constraintMethod targetType) implId maybeMethodId) =
  validateCapabilityConstraintTarget path scope targetType
    <> validateEvidenceCapability
    <> validateEvidenceImpl implId
    <> capabilityOriginFailures
    <> capabilityFailures
    <> targetFailures
    <> visibilityFailures
    <> methodFailures
  where
    scope = moduleContextTypeScope context
    validateEvidenceCapability
      | TypedImplId _ implCapability _ <- implId,
        implCapability == capability =
          []
      | isNothing maybeParameterRef =
          validateCapabilityName context path capability
      | otherwise =
          validateRetainedCapabilityName context path capability
    capabilityOriginFailures =
      case maybeParameterRef >>= (`Map.lookup` moduleContextEvidenceCapabilities context) of
        Nothing -> []
        Just expectedCapability ->
          case implId of
            TypedImplId _ capabilityName _
              | resolvedNameKey (moduleContextPath context) capabilityName
                  == Just expectedCapability ->
                  []
              | otherwise ->
                  [ failure
                      path
                      TypedMethodSelectionMismatch
                      (TypedNameDetail capabilityName)
                  ]
    capabilityFailures =
      case (maybeParameterRef, implId) of
        (Just _, _) -> []
        (Nothing, TypedImplId _ capabilityName _)
          | resolvedNameKey (moduleContextPath context) capabilityName
              == resolvedNameKey (moduleContextPath context) capability ->
              []
          | otherwise ->
              [failure path TypedMethodSelectionMismatch (TypedNameDetail capabilityName)]
    targetFailures =
      case implTargetTypes implId of
        [target]
          | target == targetType -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTypeDetail targetType target)]
        _ -> []
    visibilityFailures
      | Set.member implId (moduleContextVisibleImpls context) = []
      | otherwise = [failure path TypedInvisibleImpl (TypedImplDetail implId)]
    methodFailures =
      case (constraintMethod, maybeMethodId) of
        (Nothing, Nothing) -> []
        (Nothing, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateEvidenceMethod methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]
        (Just expectedMethod, Nothing) ->
          [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)]
        (Just expectedMethod, Just methodId@(TypedMethodId methodImplId methodName)) ->
          validateEvidenceMethod methodId
            <> (if methodImplId == implId then [] else [failure path TypedMethodSelectionMismatch (TypedImplDetail methodImplId)])
            <> ( if methodKeyMatches capability expectedMethod methodName
                   then []
                   else [failure path TypedMethodSelectionMismatch (TypedTextDetail expectedMethod)]
               )
            <> capabilityMethodFailures methodName
            <> implMethodFailures methodName
    validateEvidenceImpl
      | isNothing maybeParameterRef = validateImplId context path scope
      | otherwise = validateEvidenceImplId context path scope
    validateEvidenceMethod
      | isNothing maybeParameterRef = validateMethodId context path scope
      | otherwise = validateEvidenceMethodId context path scope
    capabilityMethodFailures methodName =
      case lookupImplMethodScheme context implId methodName of
        Left () -> []
        Right (Just _) -> []
        Right Nothing -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]
    implMethodFailures methodName =
      case Map.lookup implId (moduleContextImplMethods context) of
        Nothing -> []
        Just methods
          | Set.member methodName methods -> []
          | otherwise -> [failure path TypedMethodSelectionMismatch (TypedTextDetail methodName)]

methodKeyMatches :: TypedCoreName -> Text -> Text -> Bool
methodKeyMatches capability expected actual =
  case (methodKeyParts expected, methodKeyParts actual) of
    (Just (expectedQualifier, expectedMethod), Just (actualQualifier, actualMethod)) ->
      expectedMethod == actualMethod
        && maybe True qualifierMatchesCapability expectedQualifier
        && maybe True qualifierMatchesCapability actualQualifier
    _ -> False
  where
    qualifierMatchesCapability qualifier =
      capabilityMethodQualifier capability == Just qualifier

capabilityMethodQualifier :: TypedCoreName -> Maybe Text
capabilityMethodQualifier capability =
  case capability of
    TypedResolvedName origin TypedCapabilityNamespace identifier ->
      Just
        ( case origin of
            TypedImportedModule modulePath ->
              Text.intercalate "::" (modulePath <> [identifier])
            _ -> identifier
        )
    _ -> Nothing

methodKeyParts :: Text -> Maybe (Maybe Text, Text)
methodKeyParts methodKey
  | Text.null methodKey = Nothing
  | Just (qualifier, methodName) <- splitQualifiedMethodKey methodKey =
      Just (Just qualifier, methodName)
  | [qualifier, methodName] <- Text.splitOn "." methodKey,
    not (Text.null qualifier),
    not (Text.null methodName) =
      Just (Just qualifier, methodName)
  | Text.any (== '.') methodKey = Nothing
  | otherwise = Just (Nothing, methodKey)

validateEvidenceCandidate :: ModuleContext -> TypedCoreValidationPath -> TypedCapabilityConstraint -> TypedEvidenceCandidate -> [TypedCoreValidationFailure]
validateEvidenceCandidate context path constraint (TypedEvidenceCandidate implId maybeMethodId) =
  validateEvidenceUse context path (TypedEvidenceUse Nothing constraint implId maybeMethodId)

validateImplId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
validateImplId = validateImplIdWith validateCapabilityName

validateEvidenceImplId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
validateEvidenceImplId = validateImplIdWith validateRetainedCapabilityName

validateImplIdWith ::
  (ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]) ->
  ModuleContext ->
  TypedCoreValidationPath ->
  Set TypedTypeParameterId ->
  TypedImplId ->
  [TypedCoreValidationFailure]
validateImplIdWith validateCapability context path scope implId@(TypedImplId _ capability arguments) =
  validateCapability context path capability
    <> targetArityFailures
    <> concreteTargetFailures
    <> concatMap (validateType path scope) arguments
  where
    targetArityFailures =
      case capabilityArity context capability of
        Just expectedArity
          | expectedArity /= length arguments ->
              [failure path TypedMethodSelectionMismatch (TypedArityDetail expectedArity (length arguments))]
        _ -> []
    concreteTargetFailures
      | all concreteImplTargetType arguments = []
      | otherwise = [failure path TypedMethodSelectionMismatch (TypedImplDetail implId)]

capabilityArity :: ModuleContext -> TypedCoreName -> Maybe Int
capabilityArity context capability =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Just (CapabilityContract parameters _) -> Just (length parameters)
    Nothing -> Nothing

validateMethodId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedMethodId -> [TypedCoreValidationFailure]
validateMethodId context path scope (TypedMethodId implId _) = validateImplId context path scope implId

validateEvidenceMethodId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedMethodId -> [TypedCoreValidationFailure]
validateEvidenceMethodId context path scope (TypedMethodId implId _) = validateEvidenceImplId context path scope implId

validateCapabilityName :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateCapabilityName context path name =
  validateVisibleNameInNamespaces [TypedCapabilityNamespace] context path name

validateRetainedCapabilityName :: ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateRetainedCapabilityName context path name =
  validateCoreName path name
    <> case name of
      TypedResolvedName (TypedImportedModule importedPath) _ _
        | importedPath == moduleContextPath context
            || importedPath == ["Prelude"] ->
            [failure path TypedInvisibleName (TypedNameDetail name)]
      _ ->
        case resolvedNameKey (moduleContextPath context) name of
          Just key
            | Map.member key (moduleContextCapabilityContracts context) -> []
          _ -> [failure path TypedInvisibleName (TypedNameDetail name)]

lookupImplMethodScheme :: ModuleContext -> TypedImplId -> Text -> Either () (Maybe ([TypedTypeParameterId], TypedScheme))
lookupImplMethodScheme context (TypedImplId _ capability _) methodKey =
  case resolvedNameKey (moduleContextPath context) capability >>= (`Map.lookup` moduleContextCapabilityContracts context) of
    Nothing -> Left ()
    Just (CapabilityContract parameters methods) ->
      Right (fmap (\scheme -> (parameters, scheme)) (Map.lookup methodKey methods))

nodeInfoInstantiations :: TypedNodeInfo -> [TypedInstantiation]
nodeInfoInstantiations (TypedNodeInfo _ _ instantiations _) = instantiations

nodeInfoEvidenceSelections :: TypedNodeInfo -> [TypedEvidenceSelection]
nodeInfoEvidenceSelections (TypedNodeInfo _ _ _ evidenceSelections) = evidenceSelections

validateVisibleNameInNamespaces :: [TypedNameNamespace] -> ModuleContext -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateVisibleNameInNamespaces allowedNamespaces context path name =
  validateCoreName path name
    <> ( case name of
           TypedBuiltinName identifier
             | TypedValueNamespace `notElem` allowedNamespaces || not (knownBuiltinName identifier) ->
                 [failure path TypedInvisibleName (TypedNameDetail name)]
           TypedResolvedName (TypedImportedModule importedPath) _ _
             | importedPath == moduleContextPath context
                 || importedPath == ["Prelude"]
                 || Set.notMember importedPath (moduleContextVisibleModules context) ->
                 [failure path TypedInvisibleName (TypedNameDetail name)]
           _ ->
             case resolvedNameKey (moduleContextPath context) name of
               Just key
                 | not (nameUsesAllowedNamespace name) || Set.notMember key (moduleContextVisibleNames context) ->
                     [failure path TypedInvisibleName (TypedNameDetail name)]
               _ -> []
       )
  where
    nameUsesAllowedNamespace candidate =
      case candidate of
        TypedResolvedName _ namespace _ -> namespace `elem` allowedNamespaces
        TypedGeneratedName {} -> TypedValueNamespace `elem` allowedNamespaces
        _ -> True

knownBuiltinName :: Text -> Bool
knownBuiltinName identifier = isBuiltinSymbolName identifier || isKernelBuiltinSymbolName identifier

validateBinderDefinition :: ModuleContext -> TypedCoreValidationPath -> TypedBinderId -> TypedCoreName -> [TypedCoreValidationFailure]
validateBinderDefinition context path binderId@(TypedBinderId (modulePath, lexicalPath, embeddedName)) publishedName
  | modulePath == moduleContextPath context,
    all (>= 0) lexicalPath,
    embeddedName == publishedName =
      []
  | otherwise = [failure path TypedUnknownBinder (TypedBinderDetail binderId)]

validateLocalDefinitionName :: ModuleContext -> [TypedNameNamespace] -> TypedCoreValidationPath -> TypedCoreName -> [TypedCoreValidationFailure]
validateLocalDefinitionName context allowedNamespaces path name =
  validateCoreName path name
    <> localIdentifierFailures
    <> case name of
      TypedResolvedName TypedCurrentModule namespace _
        | namespace `elem` allowedNamespaces -> []
      TypedResolvedName TypedAmbientPrelude namespace _
        | moduleContextPath context == ["Prelude"], namespace `elem` allowedNamespaces -> []
      TypedGeneratedName {}
        | TypedValueNamespace `elem` allowedNamespaces -> []
      TypedUnresolvedSourceName {} -> []
      TypedUnresolvedQualifiedName {} -> []
      _ -> [failure path TypedInvisibleName (TypedNameDetail name)]
  where
    localIdentifierFailures =
      case name of
        TypedResolvedName _ namespace identifier
          | validResolvedIdentifier namespace identifier,
            not (validSourceIdentifier identifier) ->
              [failure path TypedUnresolvedName (TypedNameDetail name)]
        _ -> []
