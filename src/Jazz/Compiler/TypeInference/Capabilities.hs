{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jazz.Compiler.TypeInference.Capabilities
  ( TypeEnvFreeVariables,
    applyCapabilityFacts,
    addInferredEqualityClassConstraintIfVisible,
    addUnpreservedInferredMethodConstraintErrors,
    applyTypeSchemePrimitiveConstraints,
    builtinDollarOperatorExpr,
    capabilityFactsFromState,
    checkImplMethodBodies,
    classMethodPayloadToExpressionType,
    defaultBindingLiteralTypes,
    defaultLiteralTypes,
    deferExplicitConstraintsWithFacts,
    enterModuleCapabilityScope,
    finalizeDeferredExplicitConstraintsAt,
    finalizeDeferredExplicitConstraintsAtWithEntailments,
    flushCurrentModuleCapabilityFacts,
    freeTypeVariablesInEnv,
    freshTypeVars,
    importModuleCapabilityFacts,
    inferQualifiedMethodApplicationWithResults,
    instantiateQualifiedMethodType,
    instantiateQualifiedMethodTypeWithExpected,
    instantiateQualifiedMethodTypeWithExplicitTarget,
    deleteTypeEnvFreeVariables,
    insertTypeEnvFreeVariables,
    newInferredClassConstraints,
    qualifiedMethodClassIsVisible,
    resolveTypeEnvFreeVariables,
    resolveTypeSchemeConstraint,
    restoreCapabilityFacts,
    seedFacts,
    seedStatementCapabilityFact,
    typeSchemeDefiningFactsFromState,
    typeSchemeReferencedCapabilityFacts,
    structuralRuntimeEqualityType,
    typeEnvFreeVariables,
    updateRootModuleBaselineFacts,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (coreNodeSpan),
    CoreNodeId,
    CorePhase (..),
    Expr (..),
    ImplMethod (..),
    SignaturePayload,
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode,
    numericTypeFromName,
    numericTypeIsIntegral,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteConstraintArgument,
    concreteImplFact,
    concreteImplFactClassName,
    constraintFunctionArgumentTypes,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    renderConcreteImplFact,
    signaturePayloadConstraintType,
    splitQualifiedMethodKey,
    substituteClassMethodSignature,
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.Name
  ( NameNamespace (CapabilityNamespace),
    ResolvedName,
    identifierText,
    mkIdentifier,
    qualifiedMemberName,
    resolvedLocalName,
  )
import Jazz.Compiler.SemanticFacts
  ( CapabilityId (..),
    SemanticFactInvariantFailure (..),
  )
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithPrimarySpan,
    mkAmbiguousDeferredConstraintError,
    mkAmbiguousQualifiedMethodBodyError,
    mkAmbiguousQualifiedMethodBodyForArgumentsError,
    mkApplyTypeError,
    mkExplicitConstraintArityError,
    mkImplMethodMissingClassMethodError,
    mkImplMethodTypeMismatchError,
    mkInvalidQualifiedMethodSignatureError,
    mkMissingClassMethodError,
    mkMissingExplicitConstraintClassError,
    mkMissingExplicitConstraintImplFactError,
    mkMissingImplMethodBodyError,
    mkNoMatchingQualifiedMethodBodyError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError,
  )
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Solver
  ( addStrictEqualityTypeVarConstraint,
    constrainNumericOperatorType,
    freshTypeVar,
    integerLiteralRangeFitsNumericType,
    integerLiteralRangeFor,
    resolveType,
    supportsRuntimeEqualityType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    ExpressionEvidenceSeed (..),
    ImplementationEvidenceCandidate (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferClassFacts,
    inferClassMethodSignatures,
    inferConcreteImplFacts,
    inferConcreteImplMethods,
    inferCurrentModuleLocalCapabilityFacts,
    inferCurrentModulePath,
    inferDeferredExplicitConstraintCount,
    inferErrorCount,
    inferGeneratedEqualityClassFacts,
    inferImplementationEvidenceCandidates,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordExpressionEvidenceSeed,
  )
import Jazz.Compiler.TypeInference.Traversal (InferExprWithModeFn, InferenceMode)
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    ExpressionType,
    ImplMethodType (..),
    InferenceVariable,
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    quantifiedVariablesMembershipSet,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeFunction,
    pattern TypeInt,
    pattern TypeList,
    pattern TypeName,
    pattern TypeNumeric,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
  )

capabilityFactsFromState :: InferState -> ScopeCapabilityFacts
capabilityFactsFromState state =
  ScopeCapabilityFacts
    { scopeClassFacts = inferClassFacts state,
      scopeGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts state,
      scopeConcreteImplFacts = inferConcreteImplFacts state,
      scopeClassMethodSignatures = inferClassMethodSignatures state,
      scopeConcreteImplMethods = inferConcreteImplMethods state
    }

typeSchemeDefiningFactsFromState :: InferState -> [TypeSchemeConstraint] -> ScopeCapabilityFacts
typeSchemeDefiningFactsFromState state schemeConstraints =
  capturedFacts
    { scopeGeneratedEqualityClassFacts =
        Set.union
          inferredStructuralEqualityClasses
          (scopeGeneratedEqualityClassFacts capturedFacts)
    }
  where
    capturedFacts =
      case inferCurrentModulePath state of
        Just _ -> typeSchemeReferencedCapabilityFacts schemeConstraints (capabilityFactsFromState state)
        Nothing -> capabilityFactsFromState state
    inferredStructuralEqualityClasses =
      Set.fromList
        [ capabilityName
        | TypeSchemeInferredConstraint capabilityName _ <- schemeConstraints,
          activeEqualityClassName state == Just capabilityName
        ]

typeSchemeReferencedCapabilityFacts :: [TypeSchemeConstraint] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
typeSchemeReferencedCapabilityFacts [] _ = emptyScopeCapabilityFacts
typeSchemeReferencedCapabilityFacts schemeConstraints facts =
  facts
    { scopeClassFacts =
        Map.filterWithKey
          (\className _ -> Set.member className referencedCapabilityNames)
          (scopeClassFacts facts),
      scopeConcreteImplFacts =
        Set.filter
          (\implKey -> Set.member (concreteImplFactClassName implKey) referencedCapabilityNames)
          (scopeConcreteImplFacts facts),
      scopeGeneratedEqualityClassFacts =
        Set.filter
          (`Set.member` referencedCapabilityNames)
          (scopeGeneratedEqualityClassFacts facts),
      scopeClassMethodSignatures =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeClassMethodSignatures facts),
      scopeConcreteImplMethods =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeConcreteImplMethods facts)
    }
  where
    referencedCapabilityNames =
      Set.fromList
        [ constraintName
        | schemeConstraint <- schemeConstraints,
          let constraintName = typeSchemeConstraintCapabilityName schemeConstraint
        ]

    methodKeyReferencesCapturedCapability methodKey =
      case splitQualifiedMethodKey methodKey of
        Just (className, _) -> Set.member className referencedCapabilityNames
        Nothing -> False

typeSchemeConstraintCapabilityName :: TypeSchemeConstraint -> Text
typeSchemeConstraintCapabilityName constraint =
  case constraint of
    TypeSchemeConstraint constraintName _ -> constraintName
    TypeSchemeInferredConstraint constraintName _ -> constraintName
    TypeSchemeMethodConstraint constraintName _ _ -> constraintName

applyCapabilityFacts :: ScopeCapabilityFacts -> InferState -> InferState
applyCapabilityFacts facts state =
  modifyDeclarationState
    ( \declarations ->
        declarations
          { declarationClassFacts = scopeClassFacts facts,
            declarationGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts facts,
            declarationConcreteImplFacts = scopeConcreteImplFacts facts,
            declarationClassMethodSignatures = scopeClassMethodSignatures facts,
            declarationConcreteImplMethods = scopeConcreteImplMethods facts
          }
    )
    state

restoreCapabilityFacts :: InferState -> InferState -> InferState
restoreCapabilityFacts previousState nextState =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceLocalCapabilities =
              inferCurrentModuleLocalCapabilityFacts previousState
          }
    )
    ( modifyDeclarationState
        ( \declarations ->
            declarations
              { declarationClassFacts = inferClassFacts previousState,
                declarationGeneratedEqualityClassFacts = inferGeneratedEqualityClassFacts previousState,
                declarationConcreteImplFacts = inferConcreteImplFacts previousState,
                declarationClassMethodSignatures = inferClassMethodSignatures previousState,
                declarationConcreteImplMethods = inferConcreteImplMethods previousState
              }
        )
        nextState
    )

updateRootModuleBaselineFacts :: ScopeCapabilityFacts -> InferState -> InferState -> ScopeCapabilityFacts
updateRootModuleBaselineFacts moduleBaselineFacts previousState nextState =
  case inferCurrentModulePath previousState of
    Nothing -> capabilityFactsFromState nextState
    Just _ -> moduleBaselineFacts

flushCurrentModuleCapabilityFacts :: InferState -> InferState
flushCurrentModuleCapabilityFacts state =
  case inferCurrentModulePath state of
    Just modulePath ->
      modifyModuleInferenceState
        ( \moduleState ->
            moduleState
              { inferenceModuleCapabilities =
                  Map.insert
                    modulePath
                    (inferCurrentModuleLocalCapabilityFacts state)
                    (inferModuleCapabilityFacts state)
              }
        )
        state
    Nothing -> state

enterModuleCapabilityScope :: ScopeCapabilityFacts -> [Text] -> InferState -> InferState
enterModuleCapabilityScope baselineFacts modulePath state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceModulePath = Just modulePath,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts
          }
    )
    (applyCapabilityFacts baselineFacts (flushCurrentModuleCapabilityFacts state))

importModuleCapabilityFacts :: [Text] -> Maybe Text -> Maybe [Text] -> InferState -> InferState
importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state =
  applyCapabilityFacts
    ( capabilityFactsFromState state
        <> filterImportedCapabilityFacts maybeAlias maybeSymbolNames (Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state))
    )
    state

filterImportedCapabilityFacts :: Maybe Text -> Maybe [Text] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
filterImportedCapabilityFacts maybeAlias maybeSymbolNames facts =
  case maybeAlias of
    Just _ -> emptyScopeCapabilityFacts
    Nothing ->
      case maybeSymbolNames of
        Nothing -> facts
        Just symbolNames ->
          facts
            { scopeClassFacts =
                Map.filterWithKey
                  (\className _ -> Set.member className visibleSymbols)
                  (scopeClassFacts facts),
              scopeGeneratedEqualityClassFacts =
                Set.filter (`Set.member` visibleSymbols) (scopeGeneratedEqualityClassFacts facts),
              scopeConcreteImplFacts =
                Set.filter
                  (\implKey -> Set.member (concreteImplFactClassName implKey) visibleSymbols)
                  (scopeConcreteImplFacts facts),
              scopeClassMethodSignatures =
                Map.filterWithKey
                  (\methodKey _ -> importedMethodClassIsVisible methodKey)
                  (scopeClassMethodSignatures facts),
              scopeConcreteImplMethods =
                Map.filterWithKey
                  (\methodKey _ -> importedMethodClassIsVisible methodKey)
                  (scopeConcreteImplMethods facts)
            }
          where
            visibleSymbols = Set.fromList symbolNames
            importedMethodClassIsVisible methodKey =
              case splitQualifiedMethodKey methodKey of
                Just (className, _) -> Set.member className visibleSymbols
                Nothing -> False

seedStatementCapabilityFact :: InferState -> Statement 'Resolved -> InferState
seedStatementCapabilityFact state statement =
  let facts = seedFacts (capabilityFactsFromState state) (0, statement)
      stateWithVisibleFacts = applyCapabilityFacts facts state
   in case inferCurrentModulePath state of
        Just _ ->
          modifyModuleInferenceState
            ( \moduleState ->
                moduleState
                  { inferenceLocalCapabilities =
                      seedFacts (inferCurrentModuleLocalCapabilityFacts state) (0, statement)
                  }
            )
            stateWithVisibleFacts
        Nothing ->
          stateWithVisibleFacts

seedFacts :: ScopeCapabilityFacts -> (Int, Statement 'Resolved) -> ScopeCapabilityFacts
seedFacts facts (_, statement) =
  case statement of
    SClass _ capabilityName parameters methods ->
      seedClassMethodFacts
        capabilityName
        parameters
        methods
        facts
          { scopeClassFacts = Map.insert (identifierText capabilityName) (length parameters) (scopeClassFacts facts),
            scopeGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts facts
          }
    SImpl _ capabilityName arguments methods ->
      seedImplMethodFacts capabilityName arguments methods $
        case concreteImplFact capabilityName arguments of
          Just implFact ->
            facts {scopeConcreteImplFacts = Set.insert implFact (scopeConcreteImplFacts facts)}
          Nothing ->
            facts
    _ -> facts

seedClassMethodFacts ::
  ResolvedName ->
  [ResolvedName] ->
  [ClassMethodSignature 'Resolved] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedClassMethodFacts capabilityName parameters methods facts =
  case parameters of
    [classParameter] ->
      facts
        { scopeClassMethodSignatures =
            foldl'
              insertMethodSignature
              (scopeClassMethodSignatures facts)
              methods
        }
      where
        classParameterText = identifierText classParameter
        insertMethodSignature acc (ClassMethodSignature _ methodName methodSignature) =
          Map.insert
            (qualifiedMethodKey capabilityName methodName)
            (ClassMethodType classParameterText methodSignature)
            acc
    _ -> facts

seedImplMethodFacts ::
  ResolvedName ->
  [SignatureType 'Resolved] ->
  [ImplMethod 'Resolved] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedImplMethodFacts capabilityName arguments methods facts =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget ->
          facts
            { scopeConcreteImplMethods =
                foldl'
                  insertImplMethod
                  (scopeConcreteImplMethods facts)
                  methods
            }
      where
        insertImplMethod acc (ImplMethod _ methodName _) =
          Map.insertWith
            (\newMethods existingMethods -> existingMethods ++ newMethods)
            (qualifiedMethodKey capabilityName methodName)
            [ImplMethodType implTarget]
            acc
    _ -> facts

builtinDollarOperatorExpr :: TypeEnv -> Expr 'Resolved -> Bool
builtinDollarOperatorExpr env expr =
  case expr of
    EOperatorValue _ "$" -> True
    EVar _ name ->
      case Map.lookup name env of
        Just (BuiltinOperatorAliasTypeBinding "$") -> True
        Just (OperatorAliasSchemeTypeBinding "$" _) -> True
        _ -> False
    _ -> False

qualifiedMethodClassIsVisible :: Text -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  case splitQualifiedMethodKey methodKey of
    Just (capabilityName, _) -> Map.member capabilityName (inferClassFacts state)
    Nothing -> False

inferQualifiedMethodApplicationWithResults ::
  InferExprWithModeFn ->
  InferenceMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  CoreNodeId ->
  Text ->
  [Expr 'Resolved] ->
  (Maybe ExpressionType, InferState, [Maybe ExpressionType])
inferQualifiedMethodApplicationWithResults inferExpression mode builtinMode env state nodeId methodKey argumentExprs =
  let (reversedResults, stateAfterArguments) = foldl' step ([], state) argumentExprs
      results = reverse reversedResults
   in case sequenceA results of
        Nothing -> (Nothing, stateAfterArguments, results)
        Just typedArgumentTypes ->
          let (expressionType, finalState) =
                resolveQualifiedMethodApplicationType
                  nodeId
                  methodKey
                  env
                  stateAfterArguments
                  (zip argumentExprs typedArgumentTypes)
           in (expressionType, finalState, results)
  where
    step (resultsAcc, stateAcc) argumentExpr =
      let (result, stateAfterArgument) =
            inferExpression mode builtinMode env stateAcc argumentExpr
       in (result : resultsAcc, stateAfterArgument)

checkImplMethodBodies ::
  ( BuiltinResolutionMode ->
    TypeEnv ->
    InferState ->
    ExpressionType ->
    Expr 'Resolved ->
    (result, InferState)
  ) ->
  (result -> Maybe ExpressionType) ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ResolvedName ->
  [SignatureType 'Resolved] ->
  [ImplMethod 'Resolved] ->
  (InferState, [(Int, result)])
checkImplMethodBodies inferExpected resultType builtinMode env state capabilityName arguments methods =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget,
        not (implMethodNamesHaveDuplicates methods) ->
          let implMethodEnv stateForBindings =
                Map.union env (currentImplMethodBindings implTarget stateForBindings)
              checkMethod (stateAcc, resultsAcc) (methodIndex, ImplMethod methodNode methodName methodExpr) =
                let methodSpan = coreNodeSpan methodNode
                    methodKey = qualifiedMethodKey capabilityName methodName
                 in case Map.lookup methodKey (inferClassMethodSignatures stateAcc) of
                      Nothing ->
                        ( addTypeError
                            stateAcc
                            (mkImplMethodMissingClassMethodError methodKey methodSpan),
                          resultsAcc
                        )
                      Just classMethodType ->
                        let (maybeExpectedType, stateAfterExpectedType) =
                              qualifiedMethodSignatureType
                                methodKey
                                classMethodType
                                (ImplMethodType implTarget)
                                stateAcc
                         in case maybeExpectedType of
                              Nothing ->
                                (stateAfterExpectedType, resultsAcc)
                              Just expectedType ->
                                let (methodResult, rawStateAfterMethod) =
                                      inferExpected
                                        builtinMode
                                        (implMethodEnv stateAcc)
                                        stateAfterExpectedType
                                        expectedType
                                        methodExpr
                                    stateAfterMethod =
                                      annotateNewErrorsWithPrimarySpan methodSpan stateAfterExpectedType rawStateAfterMethod
                                    stateAfterMethodCheck =
                                      case resultType methodResult of
                                        Just methodType ->
                                          case unifyTypes expectedType methodType stateAfterMethod of
                                            Just unifiedState -> unifiedState
                                            Nothing ->
                                              addTypeError
                                                stateAfterMethod
                                                ( mkImplMethodTypeMismatchError
                                                    methodKey
                                                    methodSpan
                                                    (defaultLiteralTypes stateAfterMethod (resolveType stateAfterMethod expectedType))
                                                    (defaultLiteralTypes stateAfterMethod (resolveType stateAfterMethod methodType))
                                                )
                                        Nothing ->
                                          stateAfterMethod
                                    finalMethodState =
                                      finalizeDeferredExplicitConstraintsAt
                                        methodSpan
                                        stateAfterExpectedType
                                        stateAfterMethodCheck
                                 in (finalMethodState, (methodIndex, methodResult) : resultsAcc)
              (finalState, reversedResults) =
                foldl' checkMethod (state, []) (zip [0 ..] methods)
           in (finalState, reverse reversedResults)
    _ -> (state, [])
  where
    implMethodNamesHaveDuplicates :: [ImplMethod 'Resolved] -> Bool
    implMethodNamesHaveDuplicates implMethods =
      let methodNames = map (\(ImplMethod _ methodName _) -> identifierText methodName) implMethods
       in length methodNames /= Set.size (Set.fromList methodNames)

    currentImplMethodBindings :: SignatureType 'Resolved -> InferState -> TypeEnv
    currentImplMethodBindings implTarget stateForBindings =
      Map.fromList
        [ (qualifiedMemberName capabilityName methodName, PlainTypeBinding methodType)
        | ImplMethod _ methodName _ <- methods,
          let methodKey = qualifiedMethodKey capabilityName methodName,
          Just (ClassMethodType classParameter methodSignature) <- [Map.lookup methodKey (inferClassMethodSignatures stateForBindings)],
          Just methodType <- [classMethodPayloadToExpressionType stateForBindings classParameter implTarget methodSignature]
        ]

addUnpreservedInferredMethodConstraintErrors ::
  SourceSpan ->
  TypeEnv ->
  InferState ->
  InferState ->
  ExpressionType ->
  Set InferenceVariable ->
  InferState
addUnpreservedInferredMethodConstraintErrors spanValue env statementStartState state statementResultType schemeVariables =
  foldl'
    addUnpreservedClassConstraintError
    ( foldl'
        addUnpreservedMethodError
        (foldl' addUnpreservedConcreteMethodConstraintError state droppedConcreteMethodConstraints)
        droppedAmbiguousMethodKeys
    )
    droppedClassConstraints
  where
    droppedClassConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeInferredConstraint constraintName argumentType
        | TypeSchemeInferredConstraint constraintName argumentType <-
            newInferredClassConstraints statementStartState state,
          not (inferredConstraintTargetPreserved state schemeVariables argumentType),
          not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
          inferredConstraintTargetConcrete state argumentType
            || ( not statementIntroducedErrors
                   && inferredConstraintTargetEscapesResult state statementResultType argumentType
               )
        ]

    droppedMethodConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeMethodConstraint constraintName methodKey argumentType
        | TypeSchemeMethodConstraint constraintName methodKey argumentType <-
            newInferredClassConstraints statementStartState state,
          not (inferredConstraintTargetPreserved state schemeVariables argumentType),
          not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
          not (concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType)
        ]

    droppedConcreteMethodConstraints =
      [ methodConstraint
      | methodConstraint@(TypeSchemeMethodConstraint _ _ argumentType) <- droppedMethodConstraints,
        inferredConstraintTargetConcrete state argumentType
      ]

    droppedAmbiguousMethodKeys =
      Set.toList
        ( Set.fromList
            [ methodKey
            | TypeSchemeMethodConstraint _ methodKey argumentType <- droppedMethodConstraints,
              not (inferredConstraintTargetConcrete state argumentType)
            ]
        )

    addUnpreservedMethodError stateAcc methodKey =
      addTypeError
        stateAcc
        (setDiagnosticPrimarySpan spanValue (mkAmbiguousQualifiedMethodBodyError methodKey))

    addUnpreservedConcreteMethodConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    addUnpreservedClassConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    statementIntroducedErrors =
      inferErrorCount state > inferErrorCount statementStartState

newInferredClassConstraints :: InferState -> InferState -> [TypeSchemeConstraint]
newInferredClassConstraints previousState state =
  take newConstraintCount (inferInferredClassConstraints state)
  where
    previousConstraintCount = inferInferredClassConstraintCount previousState
    currentConstraintCount = inferInferredClassConstraintCount state
    newConstraintCount = max 0 (currentConstraintCount - previousConstraintCount)

inferredConstraintTargetPreserved :: InferState -> Set InferenceVariable -> ExpressionType -> Bool
inferredConstraintTargetPreserved state schemeVariables argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` schemeVariables

inferredConstraintTargetConcrete :: InferState -> ExpressionType -> Bool
inferredConstraintTargetConcrete state argumentType =
  let resolvedArgumentType = defaultLiteralTypes state (resolveType state argumentType)
   in Set.null (freeTypeVariables resolvedArgumentType)
        && case Signature.expressionTypeToConcreteSignature resolvedArgumentType of
          Just _ -> True
          Nothing -> False

inferredConstraintTargetStillVisibleInEnv :: InferState -> TypeEnv -> ExpressionType -> Bool
inferredConstraintTargetStillVisibleInEnv state env argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
      environmentVariables = freeTypeVariablesInEnv state env
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` environmentVariables

inferredConstraintTargetEscapesResult :: InferState -> ExpressionType -> ExpressionType -> Bool
inferredConstraintTargetEscapesResult state statementResultType argumentType =
  let targetVariables = freeTypeVariables (resolveType state argumentType)
      resultVariables = freeTypeVariables (resolveType state statementResultType)
   in not (Set.null targetVariables)
        && not (Set.null (Set.intersection targetVariables resultVariables))

concreteInferredMethodConstraintSatisfied :: InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType =
  let resolvedArgumentType = resolveType state argumentType
      facts = capabilityFactsFromState state
   in Set.null (freeTypeVariables resolvedArgumentType)
        && concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey resolvedArgumentType

concreteInferredMethodConstraintHasUniqueCandidate :: ScopeCapabilityFacts -> InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey argumentType =
  case satisfyingMethodHints of
    [] -> False
    [_] -> True
    _
      | expressionTypeContainsUncommittedIntegerLiteral state argumentType ->
          uniqueExactRuntimeCandidateHint state argumentType satisfyingMethodHints
      | otherwise -> True
  where
    satisfyingMethodHints =
      [ argumentHint
      | argumentHint <- inferredConstraintCandidateSignatures facts state (Just methodKey) argumentType,
        concreteImplFactExists constraintName argumentHint facts,
        concreteImplMethodBodyExists methodKey argumentHint facts
      ]

uniqueExactRuntimeCandidateHint :: InferState -> ExpressionType -> [SignatureType 'Resolved] -> Bool
uniqueExactRuntimeCandidateHint state argumentType candidateHints =
  case [ candidateHint
       | candidateHint <- candidateHints,
         constraintSignatureTypeExactlyMatchesExpressionType state candidateHint argumentType
       ] of
    [candidateHint] ->
      not (constraintSignatureTypeContainsList candidateHint)
    _ -> False

resolveTypeSchemeConstraint :: InferState -> TypeSchemeConstraint -> TypeSchemeConstraint
resolveTypeSchemeConstraint state constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (resolveType state argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (resolveType state argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (resolveType state argumentType)

freeTypeVariablesInEnv :: InferState -> TypeEnv -> Set InferenceVariable
freeTypeVariablesInEnv state =
  Set.unions . map (freeTypeVariablesInBinding state) . Map.elems

data TypeEnvFreeVariables = TypeEnvFreeVariables
  { typeEnvBindingFreeVariables :: Map ResolvedName (Set InferenceVariable),
    typeEnvFreeVariableReferenceCounts :: Map InferenceVariable Int
  }

typeEnvFreeVariables :: TypeEnv -> TypeEnvFreeVariables
typeEnvFreeVariables =
  Map.foldlWithKey' (\summary name binding -> insertTypeEnvFreeVariables name binding summary) emptyTypeEnvFreeVariables

emptyTypeEnvFreeVariables :: TypeEnvFreeVariables
emptyTypeEnvFreeVariables = TypeEnvFreeVariables Map.empty Map.empty

insertTypeEnvFreeVariables :: ResolvedName -> TypeBinding -> TypeEnvFreeVariables -> TypeEnvFreeVariables
insertTypeEnvFreeVariables name binding summary =
  TypeEnvFreeVariables
    { typeEnvBindingFreeVariables =
        Map.insert name newVariables (typeEnvBindingFreeVariables summary),
      typeEnvFreeVariableReferenceCounts =
        Set.foldl' incrementReference countsWithoutPriorBinding newVariables
    }
  where
    newVariables = freeTypeVariablesInBindingRaw binding
    priorVariables =
      Map.findWithDefault Set.empty name (typeEnvBindingFreeVariables summary)
    countsWithoutPriorBinding =
      Set.foldl' decrementTypeEnvFreeVariableReference (typeEnvFreeVariableReferenceCounts summary) priorVariables
    incrementReference counts typeVar = Map.insertWith (+) typeVar 1 counts

deleteTypeEnvFreeVariables :: ResolvedName -> TypeEnvFreeVariables -> TypeEnvFreeVariables
deleteTypeEnvFreeVariables name summary =
  TypeEnvFreeVariables
    { typeEnvBindingFreeVariables =
        Map.delete name (typeEnvBindingFreeVariables summary),
      typeEnvFreeVariableReferenceCounts =
        Set.foldl'
          decrementTypeEnvFreeVariableReference
          (typeEnvFreeVariableReferenceCounts summary)
          priorVariables
    }
  where
    priorVariables =
      Map.findWithDefault Set.empty name (typeEnvBindingFreeVariables summary)

decrementTypeEnvFreeVariableReference :: Map InferenceVariable Int -> InferenceVariable -> Map InferenceVariable Int
decrementTypeEnvFreeVariableReference counts typeVar =
  Map.update decrement typeVar counts
  where
    decrement count
      | count <= 1 = Nothing
      | otherwise = Just (count - 1)

resolveTypeEnvFreeVariables :: InferState -> TypeEnvFreeVariables -> Set InferenceVariable
resolveTypeEnvFreeVariables state summary =
  Set.unions
    [ freeTypeVariables (resolveType state (SemanticVariable typeVar))
    | typeVar <- Map.keys (typeEnvFreeVariableReferenceCounts summary)
    ]

freeTypeVariablesInBinding :: InferState -> TypeBinding -> Set InferenceVariable
freeTypeVariablesInBinding state binding =
  Set.unions
    [ freeTypeVariables (resolveType state (SemanticVariable typeVar))
    | typeVar <- Set.toList (freeTypeVariablesInBindingRaw binding)
    ]

freeTypeVariablesInBindingRaw :: TypeBinding -> Set InferenceVariable
freeTypeVariablesInBindingRaw binding =
  case binding of
    PlainTypeBinding expressionType ->
      freeTypeVariables expressionType
    SchemeTypeBinding typeScheme ->
      freeTypeVariablesInSchemeRaw typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      freeTypeVariablesInSchemeRaw typeScheme
    BuiltinAliasTypeBinding {} -> Set.empty
    BuiltinOperatorAliasTypeBinding {} -> Set.empty
    ConstructorTypeBinding _ _ argumentTypes ->
      Set.unions (map freeTypeVariablesInConstructorArgumentRaw argumentTypes)

freeTypeVariablesInSchemeRaw :: TypeScheme -> Set InferenceVariable
freeTypeVariablesInSchemeRaw typeScheme =
  Set.difference
    ( Set.unions
        [ freeTypeVariables (schemeResultType typeScheme),
          freeTypeVariablesInTypeSchemeConstraints (schemeClassConstraints typeScheme),
          freeTypeVariablesInTypeSchemePrimitiveConstraints (schemePrimitiveConstraints typeScheme)
        ]
    )
    (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme))

freeTypeVariablesInConstructorArgumentRaw :: ConstructorArgumentType -> Set InferenceVariable
freeTypeVariablesInConstructorArgumentRaw argumentType =
  case argumentType of
    ConstructorArgumentMonomorphic expressionType -> freeTypeVariables expressionType
    ConstructorArgumentParameter {} -> Set.empty
    ConstructorArgumentStructured {} -> Set.empty
    ConstructorArgumentFresh -> Set.empty

applyTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> InferState -> InferState
applyTypeSchemePrimitiveConstraints primitiveConstraints state =
  foldl' applyPrimitiveConstraint state primitiveConstraints
  where
    applyPrimitiveConstraint stateAcc primitiveConstraint =
      case primitiveConstraint of
        TypeSchemeNumericConstraint numericConstraint argumentType ->
          case constrainNumericOperatorType numericConstraint argumentType stateAcc of
            Just nextState -> nextState
            Nothing ->
              addTypeError
                stateAcc
                (mkTypeSchemeNumericConstraintError numericConstraint (resolveType stateAcc argumentType))
        TypeSchemeStrictEqualityConstraint argumentType ->
          case resolveType stateAcc argumentType of
            SemanticVariable typeVar ->
              addStrictEqualityTypeVarConstraint typeVar stateAcc
            resolvedType
              | supportsRuntimeEqualityType stateAcc resolvedType ->
                  stateAcc
              | otherwise ->
                  addTypeError stateAcc (mkTypeSchemeStrictEqualityConstraintError resolvedType)

deferExplicitConstraintsWithFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> [TypeSchemeConstraint] -> InferState -> InferState
deferExplicitConstraintsWithFacts facts structuralFacts explicitConstraints state
  | null explicitConstraints = state
  | otherwise =
      modifyInferenceOutput
        ( \output ->
            output
              { outputDeferredConstraints =
                  outputDeferredConstraints output
                    Seq.>< Seq.fromList (map (typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts) explicitConstraints)
              }
        )
        state

typeSchemeConstraintToDeferredExplicitConstraint :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> TypeSchemeConstraint -> DeferredExplicitConstraint
typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Nothing,
          deferredWasInferred = False,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }
    TypeSchemeInferredConstraint constraintName argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Nothing,
          deferredWasInferred = True,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      DeferredExplicitConstraint
        { deferredConstraintName = constraintName,
          deferredMethodKey = Just methodKey,
          deferredWasInferred = True,
          deferredArgumentType = argumentType,
          deferredVisibleFacts = facts,
          deferredStructuralFacts = structuralFacts
        }

finalizeDeferredExplicitConstraintsAt :: SourceSpan -> InferState -> InferState -> InferState
finalizeDeferredExplicitConstraintsAt spanValue statementStartState state =
  finalizeDeferredExplicitConstraintsAtWithEntailments spanValue [] statementStartState state

finalizeDeferredExplicitConstraintsAtWithEntailments :: SourceSpan -> [TypeSchemeConstraint] -> InferState -> InferState -> InferState
finalizeDeferredExplicitConstraintsAtWithEntailments spanValue entailingConstraints statementStartState state =
  annotateNewErrorsWithPrimarySpan
    spanValue
    state
    (resolveStatementDeferredExplicitConstraints entailingConstraints statementStartState state)

resolveStatementDeferredExplicitConstraints :: [TypeSchemeConstraint] -> InferState -> InferState -> InferState
resolveStatementDeferredExplicitConstraints entailingConstraints statementStartState state =
  foldl' resolveDeferredExplicitConstraint stateWithoutStatementConstraints statementConstraints
  where
    priorConstraintCount = inferDeferredExplicitConstraintCount statementStartState
    currentConstraints = outputDeferredConstraints (inferOutput state)
    priorConstraints = Seq.take priorConstraintCount currentConstraints
    statementConstraints =
      filter
        (not . deferredConstraintIsEntailed state entailingConstraints)
        (toList (Seq.drop priorConstraintCount currentConstraints))
    stateWithoutStatementConstraints =
      modifyInferenceOutput
        ( \output ->
            output
              { outputDeferredConstraints = priorConstraints
              }
        )
        state

deferredConstraintIsEntailed :: InferState -> [TypeSchemeConstraint] -> DeferredExplicitConstraint -> Bool
deferredConstraintIsEntailed state entailingConstraints deferredConstraint =
  any entails entailingConstraints
  where
    entails constraint =
      case constraint of
        TypeSchemeConstraint constraintName argumentType -> matches constraintName argumentType
        TypeSchemeInferredConstraint constraintName argumentType -> matches constraintName argumentType
        TypeSchemeMethodConstraint constraintName _ argumentType -> matches constraintName argumentType

    matches constraintName argumentType =
      constraintName == deferredConstraintName deferredConstraint
        && resolveType state argumentType
          == resolveType state (deferredArgumentType deferredConstraint)

resolveDeferredExplicitConstraint :: InferState -> DeferredExplicitConstraint -> InferState
resolveDeferredExplicitConstraint state deferredConstraint =
  let unresolvedArgumentType =
        resolveType state argumentType
      resolvedArgumentType =
        defaultLiteralTypes state unresolvedArgumentType
   in if not (Set.null (freeTypeVariables resolvedArgumentType))
        then addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
        else case Map.lookup constraintName (scopeClassFacts facts) of
          Nothing ->
            addTypeError state (mkMissingExplicitConstraintClassError constraintName)
          Just classArity
            | classArity /= 1 ->
                addTypeError state (mkExplicitConstraintArityError constraintName classArity)
            | otherwise ->
                case uncons (constraintCandidateSignaturesForDeferred facts state inferredConstraint constraintName maybeMethodKey unresolvedArgumentType) of
                  Nothing ->
                    addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
                  Just (firstArgumentHint, remainingArgumentHints) ->
                    let argumentHints = firstArgumentHint : remainingArgumentHints
                        implFactHints =
                          filter
                            (constraintImplFactExistsForDeferred facts inferredConstraint constraintName)
                            argumentHints
                        methodBodyHints methodKey =
                          filter
                            (\argumentHint -> concreteImplMethodBodyExists methodKey argumentHint facts)
                            implFactHints
                        ambiguousMethodBodyHints methodKey =
                          inferredConstraint
                            && expressionTypeContainsUncommittedIntegerLiteral state unresolvedArgumentType
                            && length (methodBodyHints methodKey) > 1
                            && not (uniqueExactRuntimeCandidateHint state unresolvedArgumentType (methodBodyHints methodKey))
                        renderedImplFactKey =
                          renderConcreteImplFact (concreteImplFactForRenderedName constraintName firstArgumentHint)
                     in case maybeMethodKey of
                          Nothing
                            | not (null implFactHints) ->
                                state
                            | inferredConstraint
                                && inferredEqualityConstraintCanUseStructuralRuntimeEquality state structuralFacts maybeMethodKey constraintName resolvedArgumentType ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                          Just methodKey
                            | null implFactHints ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                            | ambiguousMethodBodyHints methodKey ->
                                addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey)
                            | not (null (methodBodyHints methodKey)) ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingImplMethodBodyError methodKey)
  where
    constraintName = deferredConstraintName deferredConstraint
    maybeMethodKey = deferredMethodKey deferredConstraint
    inferredConstraint = deferredWasInferred deferredConstraint
    argumentType = deferredArgumentType deferredConstraint
    facts = deferredVisibleFacts deferredConstraint
    structuralFacts = deferredStructuralFacts deferredConstraint

expressionTypeContainsUncommittedIntegerLiteral :: InferState -> ExpressionType -> Bool
expressionTypeContainsUncommittedIntegerLiteral state expressionType
  | Just _ <- integerLiteralRangeFor state expressionType = True
  | otherwise =
      case expressionType of
        SemanticList elementType ->
          expressionTypeContainsUncommittedIntegerLiteral state elementType
        SemanticTuple elementTypes ->
          any (expressionTypeContainsUncommittedIntegerLiteral state) elementTypes
        SemanticData _ typeArguments ->
          any (expressionTypeContainsUncommittedIntegerLiteral state) typeArguments
        SemanticFunction argumentType resultType ->
          expressionTypeContainsUncommittedIntegerLiteral state argumentType
            || expressionTypeContainsUncommittedIntegerLiteral state resultType
        _ -> False

constraintCandidateSignaturesForDeferred ::
  ScopeCapabilityFacts ->
  InferState ->
  Bool ->
  Text ->
  Maybe Text ->
  ExpressionType ->
  [SignatureType 'Resolved]
constraintCandidateSignaturesForDeferred facts state inferredConstraint _ maybeMethodKey argumentType
  | inferredConstraint =
      inferredConstraintCandidateSignatures facts state maybeMethodKey argumentType
  | otherwise =
      case Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

constraintImplFactExistsForDeferred :: ScopeCapabilityFacts -> Bool -> Text -> SignatureType 'Resolved -> Bool
constraintImplFactExistsForDeferred facts inferredConstraint constraintName argumentHint =
  if inferredConstraint
    then concreteImplFactExists constraintName argumentHint facts
    else concreteImplFactExistsExactly constraintName argumentHint facts

inferredConstraintCandidateSignatures :: ScopeCapabilityFacts -> InferState -> Maybe Text -> ExpressionType -> [SignatureType 'Resolved]
inferredConstraintCandidateSignatures facts state maybeMethodKey argumentType =
  dedupeSignatureTypes (defaultHint ++ methodCandidateHints)
  where
    defaultHint =
      case Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

    methodCandidateHints =
      case maybeMethodKey of
        Nothing -> []
        Just methodKey ->
          [ implTarget
          | ImplMethodType implTarget <- Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts),
            constraintSignatureTypeMatchesExpressionType state implTarget argumentType
          ]

dedupeSignatureTypes :: [SignatureType 'Resolved] -> [SignatureType 'Resolved]
dedupeSignatureTypes =
  go Set.empty
  where
    go _ [] = []
    go seen (signatureType : rest)
      | Set.member rendered seen = go seen rest
      | otherwise = signatureType : go (Set.insert rendered seen) rest
      where
        rendered = renderSignatureType signatureType

constraintSignatureTypeMatchesExpressionType :: InferState -> SignatureType 'Resolved -> ExpressionType -> Bool
constraintSignatureTypeMatchesExpressionType state signatureType expressionType =
  case (signatureType, integerLiteralRangeFor state expressionType, resolveType state expressionType) of
    (TypeInt, Just literalRange, _) ->
      integerLiteralRangeFitsNumericType literalRange NumericInt64
    (TypeNumeric numericType, Just literalRange, _) ->
      numericTypeIsIntegral numericType
        && integerLiteralRangeFitsNumericType literalRange numericType
    (TypeName signatureName, Just literalRange, _) ->
      case numericTypeFromConstraintSignatureName (identifierText signatureName) of
        Just numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        Nothing ->
          False
    (TypeList signatureElementType, _, SemanticList elementType) ->
      constraintSignatureTypeMatchesExpressionType state signatureElementType elementType
    (TypeTuple signatureElementTypes, _, SemanticTuple elementTypes)
      | length signatureElementTypes == length elementTypes ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureElementTypes elementTypes)
    (TypeApplication signatureName signatureArguments, _, SemanticData typeName typeArguments)
      | normalizeConstraintSignatureName (identifierText signatureName)
          == normalizeConstraintSignatureName (identifierText typeName),
        length signatureArguments == length typeArguments ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureArguments typeArguments)
    (TypeFunction signatureArgument signatureResult, _, SemanticFunction argumentType resultType) ->
      constraintSignatureTypeMatchesExpressionType state signatureArgument argumentType
        && constraintSignatureTypeMatchesExpressionType state signatureResult resultType
    _ ->
      case Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state (resolveType state expressionType)) of
        Just argumentHint -> constraintSignatureTypesCompatible signatureType argumentHint
        Nothing -> False

numericTypeFromConstraintSignatureName :: Text -> Maybe NumericType
numericTypeFromConstraintSignatureName =
  numericTypeFromName . normalizeConstraintSignatureName

concreteImplFactExists :: Text -> SignatureType 'Resolved -> ScopeCapabilityFacts -> Bool
concreteImplFactExists constraintName argumentHint facts =
  any
    (\candidateHint -> concreteImplFactExistsExactly constraintName candidateHint facts)
    (constraintSignatureAliasVariants argumentHint)

concreteImplFactExistsExactly :: Text -> SignatureType 'Resolved -> ScopeCapabilityFacts -> Bool
concreteImplFactExistsExactly constraintName argumentHint facts =
  any matches (scopeConcreteImplFacts facts)
  where
    matches (ConcreteImplFact capabilityName candidateHint) =
      identifierText capabilityName == constraintName
        && candidateHint == argumentHint

concreteImplFactForRenderedName :: Text -> SignatureType 'Resolved -> ConcreteImplFact
concreteImplFactForRenderedName constraintName argumentHint =
  ConcreteImplFact (resolvedLocalName CapabilityNamespace (mkIdentifier constraintName)) argumentHint

concreteImplMethodBodyExists :: Text -> SignatureType 'Resolved -> ScopeCapabilityFacts -> Bool
concreteImplMethodBodyExists methodKey argumentHint facts =
  any
    (\(ImplMethodType implTarget) -> constraintSignatureTypesCompatible implTarget argumentHint)
    (Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts))

inferredEqualityConstraintCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Maybe Text -> Text -> ExpressionType -> Bool
inferredEqualityConstraintCanUseStructuralRuntimeEquality state facts maybeMethodKey constraintName argumentType =
  maybeMethodKey == Nothing
    && equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName
    && structuralRuntimeEqualityType state argumentType

equalityConstraintNameCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Text -> Bool
equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName =
  activeEqualityClassName state == Just constraintName
    || generatedHiddenEqualityClassFact constraintName facts

generatedHiddenEqualityClassFact :: Text -> ScopeCapabilityFacts -> Bool
generatedHiddenEqualityClassFact constraintName facts =
  Set.member constraintName (scopeGeneratedEqualityClassFacts facts)
    && Map.lookup constraintName (scopeClassFacts facts) == Just 1

structuralRuntimeEqualityType :: InferState -> ExpressionType -> Bool
structuralRuntimeEqualityType state argumentType =
  case resolveType state argumentType of
    SemanticList elementType ->
      supportsRuntimeEqualityType state elementType
    SemanticTuple elementTypes ->
      all (supportsRuntimeEqualityType state) elementTypes
    SemanticData typeName typeArguments ->
      supportsRuntimeEqualityType state (SemanticData typeName typeArguments)
    _ ->
      False

instantiateQualifiedMethodType :: CoreNodeId -> Text -> InferState -> Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodType nodeId nameText state =
  case splitQualifiedMethodKey nameText of
    Just (capabilityName, _)
      | Map.member capabilityName (inferClassFacts state) ->
          Just (resolveQualifiedMethodType nodeId nameText state)
    _ -> Nothing

instantiateQualifiedMethodTypeWithExpected ::
  CoreNodeId ->
  Text ->
  ExpressionType ->
  InferState ->
  Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodTypeWithExpected nodeId nameText expectedType state =
  case splitQualifiedMethodKey nameText of
    Just (capabilityName, _)
      | Map.member capabilityName (inferClassFacts state) ->
          Just (resolveQualifiedMethodTypeWithExpected nodeId nameText expectedType state)
    _ -> Nothing

resolveQualifiedMethodTypeWithExpected ::
  CoreNodeId ->
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
resolveQualifiedMethodTypeWithExpected nodeId methodKey expectedType state =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing ->
      (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case preferredCandidates of
        [] ->
          ( Nothing,
            addTypeError
              state
              (mkNoMatchingQualifiedMethodBodyError methodKey [defaultLiteralTypes state (resolveType state expectedType)])
          )
        [(implMethodType, matchedType, matchedState)] ->
          recordSelectedQualifiedMethodResult
            nodeId
            methodKey
            implMethodType
            (Just matchedType, matchedState)
        _ ->
          (Nothing, addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey))
      where
        preferredCandidates =
          case exactMatchingCandidates of
            [] -> matchingCandidates
            exactMatches -> exactMatches

        exactMatchingCandidates =
          filter candidateExactlyMatchesExpected matchingCandidates

        matchingCandidates =
          foldr collectMatchingCandidate [] (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))

        collectMatchingCandidate implMethodType matches =
          case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
            (Just methodType, stateAfterMethodType) ->
              case unifyTypes expectedType methodType stateAfterMethodType of
                Just unifiedState ->
                  (implMethodType, resolveType unifiedState methodType, unifiedState) : matches
                Nothing -> matches
            (Nothing, _) -> matches

        candidateExactlyMatchesExpected (ImplMethodType implTarget, _, _) =
          case classMethodType of
            ClassMethodType classParameter methodSignature ->
              case substituteClassMethodSignature classParameter implTarget methodSignature of
                Just candidateSignature ->
                  constraintSignatureTypeExactlyMatchesExpressionType state candidateSignature expectedType
                Nothing -> False

resolveQualifiedMethodType :: CoreNodeId -> Text -> InferState -> (Maybe ExpressionType, InferState)
resolveQualifiedMethodType nodeId methodKey state =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
        [] ->
          (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
        [implMethodType] ->
          recordSelectedQualifiedMethodResult
            nodeId
            methodKey
            implMethodType
            (qualifiedMethodSignatureType methodKey classMethodType implMethodType state)
        _ ->
          (Nothing, addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey))

instantiateQualifiedMethodTypeWithExplicitTarget ::
  CoreNodeId ->
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateQualifiedMethodTypeWithExplicitTarget nodeId methodKey explicitTarget state =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing ->
      (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case matchingImplMethods of
        [] ->
          (Nothing, addTypeError state (mkNoMatchingQualifiedMethodBodyError methodKey [explicitTarget]))
        [implMethodType] ->
          recordSelectedQualifiedMethodResult
            nodeId
            methodKey
            implMethodType
            (qualifiedMethodSignatureType methodKey classMethodType implMethodType state)
        _ ->
          (Nothing, addTypeError state (mkAmbiguousQualifiedMethodBodyForArgumentsError methodKey [explicitTarget]))
  where
    matchingImplMethods =
      filter
        (\(ImplMethodType implTarget) -> constraintSignatureTypeExactlyMatchesExpressionType state implTarget explicitTarget)
        (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))

resolveQualifiedMethodApplicationType ::
  CoreNodeId ->
  Text ->
  TypeEnv ->
  InferState ->
  [(Expr 'Resolved, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
resolveQualifiedMethodApplicationType nodeId methodKey env state typedArguments =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case inferQualifiedMethodRequirement methodKey classMethodType state argumentTypes of
        Just inferredRequirement ->
          inferredRequirement
        Nothing ->
          case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
            [] ->
              (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
            [implMethodType] ->
              recordSelectedQualifiedMethodResult
                nodeId
                methodKey
                implMethodType
                (applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes)
            implMethodTypes ->
              selectQualifiedMethodCandidate nodeId methodKey classMethodType implMethodTypes env state typedArguments
  where
    argumentTypes = map snd typedArguments

inferQualifiedMethodRequirement ::
  Text ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirement methodKey classMethodType state argumentTypes =
  {-# SCC "jazz-stage:capability-solving" #-}
  inferQualifiedMethodRequirementWithoutCostCentre methodKey classMethodType state argumentTypes

inferQualifiedMethodRequirementWithoutCostCentre ::
  Text ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirementWithoutCostCentre methodKey (ClassMethodType classParameter methodSignature) state argumentTypes = do
  (capabilityName, _) <- splitQualifiedMethodKey methodKey
  classArity <- Map.lookup capabilityName (inferClassFacts state)
  if classArity /= 1
    then Nothing
    else
      if not (classMethodSignatureHasTargetArgument classParameter methodSignature)
        then Nothing
        else
          let (classTarget, stateAfterClassTarget) = freshTypeVar state
           in do
                methodType <-
                  classMethodPayloadToGenericExpressionType
                    stateAfterClassTarget
                    classParameter
                    classTarget
                    methodSignature
                let (maybeResultType, stateAfterArguments) =
                      applyKnownFunctionArguments methodType argumentTypes stateAfterClassTarget
                    resolvedClassTarget = resolveType stateAfterArguments classTarget
                case maybeResultType of
                  Just resultType
                    | not
                        ( Set.null
                            (freeTypeVariables (defaultLiteralTypes stateAfterArguments resolvedClassTarget))
                        ) ->
                        Just
                          ( Just resultType,
                            addInferredMethodClassConstraint capabilityName methodKey resolvedClassTarget stateAfterArguments
                          )
                  _ ->
                    Nothing

classMethodSignatureHasTargetArgument :: Text -> SignaturePayload 'Resolved -> Bool
classMethodSignatureHasTargetArgument classParameter methodSignature =
  case signaturePayloadConstraintType methodSignature of
    Just signatureType ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes signatureType
       in any (constraintSignatureTypeContainsClassParameter classParameter) argumentTypes
    Nothing ->
      False

selectQualifiedMethodCandidate ::
  CoreNodeId ->
  Text ->
  ClassMethodType ->
  [ImplMethodType] ->
  TypeEnv ->
  InferState ->
  [(Expr 'Resolved, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
selectQualifiedMethodCandidate nodeId methodKey classMethodType implMethodTypes env state typedArguments =
  case preferredCandidates of
    [] ->
      ( Nothing,
        addTypeError state (mkNoMatchingQualifiedMethodBodyError methodKey (resolvedArgumentTypes state))
      )
    [(implMethodType, matchedType, matchedState)] ->
      (Just matchedType, recordSelectedQualifiedMethodEvidence nodeId methodKey implMethodType matchedState)
    _ ->
      ( Nothing,
        addTypeError state (mkAmbiguousQualifiedMethodBodyForArgumentsError methodKey (resolvedArgumentTypes state))
      )
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches

    exactMatchingCandidates =
      filterExactMatches matchingCandidatesWithTargets

    matchingCandidates = matchingCandidatesWithTargets

    matchingCandidatesWithTargets =
      foldr collectMatch [] implMethodTypes

    collectMatch implMethodType matches =
      case applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes of
        (Just matchedType, matchedState) -> (implMethodType, matchedType, matchedState) : matches
        (Nothing, _) -> matches

    filterExactMatches candidates =
      [ (implMethodType, matchedType, matchedState)
      | (implMethodType, matchedType, matchedState) <- candidates,
        qualifiedMethodCandidateExactlyMatchesArguments state env classMethodType implMethodType typedArguments
      ]

    resolvedArgumentTypes stateForRendering =
      map
        (defaultLiteralTypes stateForRendering . resolveType stateForRendering)
        argumentTypes

    argumentTypes = map snd typedArguments

recordSelectedQualifiedMethodEvidence :: CoreNodeId -> Text -> ImplMethodType -> InferState -> InferState
recordSelectedQualifiedMethodEvidence nodeId methodKey (ImplMethodType selectedTarget) state =
  case matchingCandidates of
    [candidate] ->
      case Signature.signatureTypeToExpressionType state Map.empty selectedTarget of
        Left _ -> recordInvariantFailure (MissingExpressionEvidence nodeId) state
        Right targetType ->
          recordExpressionEvidenceSeed
            nodeId
            ( ExpressionEvidenceSeed
                { evidenceSeedCapability = CapabilityId (implementationCandidateCapability candidate),
                  evidenceSeedImplementation = implementationCandidateId candidate,
                  evidenceSeedMethod = implementationCandidateMethodId candidate,
                  evidenceSeedType = targetType
                }
            )
            state
    [] -> recordInvariantFailure (MissingExpressionEvidence nodeId) state
    _ -> recordInvariantFailure (AmbiguousExpressionEvidence nodeId) state
  where
    matchingCandidates =
      filter
        ((== selectedTarget) . implementationCandidateTarget)
        (Map.findWithDefault [] methodKey (inferImplementationEvidenceCandidates state))

recordSelectedQualifiedMethodResult ::
  CoreNodeId ->
  Text ->
  ImplMethodType ->
  (Maybe ExpressionType, InferState) ->
  (Maybe ExpressionType, InferState)
recordSelectedQualifiedMethodResult nodeId methodKey implMethodType (maybeResultType, selectedState) =
  ( maybeResultType,
    case maybeResultType of
      Nothing -> selectedState
      Just _ -> recordSelectedQualifiedMethodEvidence nodeId methodKey implMethodType selectedState
  )

recordInvariantFailure :: SemanticFactInvariantFailure -> InferState -> InferState
recordInvariantFailure failure =
  modifyInferenceOutput
    ( \output ->
        output
          { outputFactInvariantFailures =
              outputFactInvariantFailures output Seq.|> failure
          }
    )

qualifiedMethodCandidateExactlyMatchesArguments ::
  InferState ->
  TypeEnv ->
  ClassMethodType ->
  ImplMethodType ->
  [(Expr 'Resolved, ExpressionType)] ->
  Bool
qualifiedMethodCandidateExactlyMatchesArguments state env (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) typedArguments =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (candidateArgumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length typedArguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedCandidateArgumentTypes = take suppliedArgumentCount candidateArgumentTypes
          targetArgumentPositions =
            map (constraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length candidateArgumentTypes
            && or targetArgumentPositions
            && and
              ( zipWith3
                  exactCandidateArgumentMatches
                  targetArgumentPositions
                  suppliedCandidateArgumentTypes
                  typedArguments
              )
    _ ->
      False
  where
    exactCandidateArgumentMatches targetArgumentPosition signatureType (argumentExpr, expressionType) =
      not targetArgumentPosition
        || case scalarApplicationRuntimeHint state env expressionType argumentExpr of
          Just runtimeHint -> runtimeHint == signatureType
          Nothing ->
            constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType
              && constraintSignatureExpressionHasExactEvidence state env signatureType argumentExpr

scalarApplicationRuntimeHint :: InferState -> TypeEnv -> ExpressionType -> Expr 'Resolved -> Maybe (SignatureType 'Resolved)
scalarApplicationRuntimeHint state env expressionType argumentExpr =
  case argumentExpr of
    EApply {} ->
      constraintSignatureExpressionRuntimeHint state env argumentExpr
        <|> inferredScalarHint
    _ -> Nothing
  where
    inferredScalarHint =
      Signature.expressionTypeToConcreteSignature
        =<< if integerLiteralRangeFor state resolvedType /= Nothing
          then Just (SemanticNumeric NumericInt64)
          else case resolvedType of
            SemanticInt -> Just (SemanticNumeric NumericInt64)
            scalarType@SemanticFloat -> Just scalarType
            scalarType@SemanticNumeric {} -> Just scalarType
            scalarType@SemanticBool -> Just scalarType
            scalarType@SemanticChar -> Just scalarType
            scalarType@SemanticText -> Just scalarType
            _ -> Nothing
    resolvedType = resolveType state expressionType

constraintSignatureExpressionHasExactEvidence :: InferState -> TypeEnv -> SignatureType 'Resolved -> Expr 'Resolved -> Bool
constraintSignatureExpressionHasExactEvidence state env signatureType argumentExpr =
  case (signatureType, argumentExpr) of
    (TypeList elementType, EList _ elements) ->
      not (null elements)
        && all (constraintSignatureExpressionHasExactEvidence state env elementType) elements
    (TypeTuple elementTypes, ETuple _ elements)
      | length elementTypes == length elements ->
          and (zipWith (constraintSignatureExpressionHasExactEvidence state env) elementTypes elements)
    (TypeApplication typeName typeArguments, EApply {}) ->
      constructorApplicationExpressionHasExactEvidence state env typeName typeArguments argumentExpr
        || constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (TypeFunction {}, _) ->
      constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EVar {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EApply {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EIf {}) ->
      constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EPatternCase {}) ->
      constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EBlock {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr
    _ -> True

constraintSignatureExpressionRuntimeHintMatches :: InferState -> TypeEnv -> SignatureType 'Resolved -> Expr 'Resolved -> Bool
constraintSignatureExpressionRuntimeHintMatches state env signatureType argumentExpr =
  case constraintSignatureExpressionRuntimeHint state env argumentExpr of
    Just runtimeHint -> runtimeHint == signatureType
    Nothing -> False

constraintSignatureExpressionRuntimeHint :: InferState -> TypeEnv -> Expr 'Resolved -> Maybe (SignatureType 'Resolved)
constraintSignatureExpressionRuntimeHint state env argumentExpr =
  constraintSignatureExpressionRuntimeHintWithLocalHints state env Map.empty argumentExpr

constraintSignatureExpressionRuntimeHintWithLocalHints ::
  InferState ->
  TypeEnv ->
  Map Text (SignatureType 'Resolved) ->
  Expr 'Resolved ->
  Maybe (SignatureType 'Resolved)
constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints argumentExpr =
  case argumentExpr of
    EVar _ referencedName ->
      Map.lookup (identifierText referencedName) localHints
        <|> (Map.lookup referencedName env >>= typeBindingRuntimeHint state)
    EApply _ (EApply _ dollarExpr functionExpr) _
      | builtinDollarOperatorExpr env dollarExpr ->
          case constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints functionExpr of
            Just (TypeFunction _ resultType) -> Just resultType
            _ -> Nothing
    EApply _ functionExpr _ ->
      case constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints functionExpr of
        Just (TypeFunction _ resultType) -> Just resultType
        _ -> Nothing
    EIf _ _ thenExpr elseExpr ->
      commonConstraintSignatureExpressionRuntimeHint state env localHints [thenExpr, elseExpr]
    EPatternCase _ _ caseArms ->
      commonConstraintSignatureExpressionRuntimeHint state env localHints [bodyExpr | CaseArm _ _ _ bodyExpr <- caseArms]
    EBlock _ statements ->
      constraintSignatureBlockRuntimeHint state env localHints statements
    _ -> Nothing

commonConstraintSignatureExpressionRuntimeHint ::
  InferState ->
  TypeEnv ->
  Map Text (SignatureType 'Resolved) ->
  [Expr 'Resolved] ->
  Maybe (SignatureType 'Resolved)
commonConstraintSignatureExpressionRuntimeHint _ _ _ [] = Nothing
commonConstraintSignatureExpressionRuntimeHint state env localHints (firstExpr : restExprs) = do
  firstHint <- constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints firstExpr
  if all
    (\expr -> constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints expr == Just firstHint)
    restExprs
    then Just firstHint
    else Nothing

constraintSignatureBlockRuntimeHint ::
  InferState ->
  TypeEnv ->
  Map Text (SignatureType 'Resolved) ->
  [Statement 'Resolved] ->
  Maybe (SignatureType 'Resolved)
constraintSignatureBlockRuntimeHint state env initialLocalHints statements =
  go initialLocalHints Map.empty statements
  where
    go _ _ [] =
      Nothing
    go localHints _ [SExpr _ expr] =
      constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints expr
    go localHints pendingHints (statement : rest) =
      case statement of
        SSignature _ name signaturePayload ->
          let nameText = identifierText name
              nextPendingHints =
                case signaturePayloadRuntimeHint signaturePayload of
                  Just runtimeHint -> Map.insert nameText runtimeHint pendingHints
                  Nothing -> Map.delete nameText pendingHints
           in go localHints nextPendingHints rest
        SLet _ name valueExpr ->
          let nameText = identifierText name
              bindingHint =
                Map.lookup nameText pendingHints
                  <|> constraintSignatureExpressionRuntimeHintWithLocalHints state env localHints valueExpr
              nextLocalHints =
                case bindingHint of
                  Just runtimeHint -> Map.insert nameText runtimeHint localHints
                  Nothing -> localHints
           in go nextLocalHints (Map.delete nameText pendingHints) rest
        _ ->
          go localHints pendingHints rest

signaturePayloadRuntimeHint :: SignaturePayload 'Resolved -> Maybe (SignatureType 'Resolved)
signaturePayloadRuntimeHint signaturePayload =
  case signaturePayload of
    SignatureType signatureType
      | null (constraintSignatureTypeVariableNamesInOrder signatureType) -> Just signatureType
    SignatureType {} -> Nothing
    ConstrainedSignature _ signatureType
      | null (constraintSignatureTypeVariableNamesInOrder signatureType) ->
          Just signatureType
    ConstrainedSignature _ signatureType ->
      Signature.constraintSignatureTypeToExpressionType signatureType >>= Signature.expressionTypeToConcreteSignature
    UnsupportedSignature {} ->
      Nothing

typeBindingRuntimeHint :: InferState -> TypeBinding -> Maybe (SignatureType 'Resolved)
typeBindingRuntimeHint state binding =
  case binding of
    PlainTypeBinding bindingType ->
      Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state bindingType)
    SchemeTypeBinding typeScheme
      | Set.null (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme)) ->
          Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state (schemeResultType typeScheme))
    OperatorAliasSchemeTypeBinding _ typeScheme
      | Set.null (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme)) ->
          Signature.expressionTypeToConcreteSignature (defaultLiteralTypes state (schemeResultType typeScheme))
    _ -> Nothing

constraintSignatureTypeContainsList :: SignatureType 'Resolved -> Bool
constraintSignatureTypeContainsList signatureType =
  case signatureType of
    TypeList {} -> True
    TypeTuple elementTypes ->
      any constraintSignatureTypeContainsList elementTypes
    TypeApplication _ typeArguments ->
      any constraintSignatureTypeContainsList typeArguments
    TypeFunction argumentType resultType ->
      constraintSignatureTypeContainsList argumentType
        || constraintSignatureTypeContainsList resultType
    TypeName {} -> False
    _ -> False

constructorApplicationExpressionHasExactEvidence :: InferState -> TypeEnv -> ResolvedName -> [SignatureType 'Resolved] -> Expr 'Resolved -> Bool
constructorApplicationExpressionHasExactEvidence state env typeName typeArguments argumentExpr =
  case constructorExpressionSpine argumentExpr of
    Just (constructorName, constructorArgumentExprs) ->
      case Map.lookup constructorName env of
        Just (ConstructorTypeBinding constructorTypeName typeParameters constructorArgumentTypes)
          | constructorTypeName == typeName,
            length typeParameters == length typeArguments,
            length constructorArgumentTypes == length constructorArgumentExprs ->
              let typeParameterBindings =
                    Map.fromList (zip (map identifierText typeParameters) typeArguments)
               in and
                    ( zipWith
                        (constructorArgumentExpressionHasExactEvidence state env typeParameterBindings)
                        constructorArgumentTypes
                        constructorArgumentExprs
                    )
        _ -> False
    Nothing -> False

constructorExpressionSpine :: Expr 'Resolved -> Maybe (ResolvedName, [Expr 'Resolved])
constructorExpressionSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply _ functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar _ constructorName ->
          Just (constructorName, argumentExprs)
        _ ->
          Nothing

constructorArgumentExpressionHasExactEvidence :: InferState -> TypeEnv -> Map Text (SignatureType 'Resolved) -> ConstructorArgumentType -> Expr 'Resolved -> Bool
constructorArgumentExpressionHasExactEvidence state env typeParameterBindings constructorArgument argumentExpr =
  case constructorArgument of
    ConstructorArgumentParameter parameterName ->
      case Map.lookup parameterName typeParameterBindings of
        Just concreteArgumentType ->
          constraintSignatureExpressionHasExactEvidence state env concreteArgumentType argumentExpr
        Nothing ->
          True
    ConstructorArgumentMonomorphic {} ->
      True
    ConstructorArgumentStructured fieldType ->
      constraintSignatureExpressionHasExactEvidence
        state
        env
        (substituteConstructorFieldSignatureType typeParameterBindings fieldType)
        argumentExpr
    ConstructorArgumentFresh ->
      True

substituteConstructorFieldSignatureType ::
  Map Text (SignatureType 'Resolved) ->
  SignatureType 'Resolved ->
  SignatureType 'Resolved
substituteConstructorFieldSignatureType typeParameterBindings fieldType =
  case fieldType of
    TypeVariable name ->
      Map.findWithDefault fieldType (identifierText name) typeParameterBindings
    TypeApplication name arguments ->
      TypeApplication
        name
        (map (substituteConstructorFieldSignatureType typeParameterBindings) arguments)
    TypeList elementType ->
      TypeList (substituteConstructorFieldSignatureType typeParameterBindings elementType)
    TypeTuple elementTypes ->
      TypeTuple (map (substituteConstructorFieldSignatureType typeParameterBindings) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (substituteConstructorFieldSignatureType typeParameterBindings argumentType)
        (substituteConstructorFieldSignatureType typeParameterBindings resultType)
    _ -> fieldType

constraintSignatureTypeExactlyMatchesExpressionType :: InferState -> SignatureType 'Resolved -> ExpressionType -> Bool
constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType =
  case Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty signatureType of
    Just signatureExpressionType ->
      resolveType state signatureExpressionType == defaultLiteralTypes state (resolveType state expressionType)
    Nothing ->
      False

applyQualifiedMethodCandidate ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArguments methodType argumentTypes stateAfterMethodType

applyQualifiedMethodCandidateWithErrors ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArgumentsWithErrors methodType argumentTypes stateAfterMethodType

applyKnownFunctionArguments ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArguments functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (SemanticFunction argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              (Nothing, stateAcc)

applyKnownFunctionArgumentsWithErrors ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArgumentsWithErrors functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (SemanticFunction argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  stateWithResultVar
                  ( mkApplyTypeError
                      (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar currentFunctionType))
                      (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar argumentType))
                  )
              )

qualifiedMethodSignatureType ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  (Maybe ExpressionType, InferState)
qualifiedMethodSignatureType methodKey (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) state =
  case classMethodPayloadToExpressionType state classParameter implTarget methodSignature of
    Just methodType -> (Just methodType, state)
    Nothing ->
      (Nothing, addTypeError state (mkInvalidQualifiedMethodSignatureError methodKey methodSignature))

classMethodPayloadToExpressionType ::
  InferState ->
  Text ->
  SignatureType 'Resolved ->
  SignaturePayload 'Resolved ->
  Maybe ExpressionType
classMethodPayloadToExpressionType state classParameter implTarget methodSignature =
  substituteClassMethodSignature classParameter implTarget methodSignature
    >>= Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty

classMethodPayloadToGenericExpressionType ::
  InferState ->
  Text ->
  ExpressionType ->
  SignaturePayload 'Resolved ->
  Maybe ExpressionType
classMethodPayloadToGenericExpressionType state classParameter classTarget methodSignature =
  signaturePayloadConstraintType methodSignature
    >>= Signature.constraintSignatureTypeToExpressionTypeWithState
      state
      (Map.singleton classParameter classTarget)

freshTypeVars :: Int -> InferState -> ([ExpressionType], InferState)
freshTypeVars count initialState =
  go count [] initialState
  where
    go remaining acc state
      | remaining <= 0 = (reverse acc, state)
      | otherwise =
          let (typeVar, nextState) = freshTypeVar state
           in go (remaining - 1) (typeVar : acc) nextState

defaultLiteralTypes :: InferState -> ExpressionType -> ExpressionType
defaultLiteralTypes state =
  defaultLiteralTypesWith state SemanticInt

defaultBindingLiteralTypes :: InferState -> ExpressionType -> ExpressionType
defaultBindingLiteralTypes state =
  defaultLiteralTypesWith state (SemanticNumeric NumericInt64)

defaultLiteralTypesWith :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
defaultLiteralTypesWith state integerLiteralDefault expressionType
  | Just _ <- integerLiteralRangeFor state expressionType = integerLiteralDefault
  | otherwise =
      case expressionType of
        SemanticList elementType ->
          SemanticList (defaultLiteralTypesWith state integerLiteralDefault elementType)
        SemanticTuple elementTypes ->
          SemanticTuple (map (defaultLiteralTypesWith state integerLiteralDefault) elementTypes)
        SemanticData typeName typeArguments ->
          SemanticData typeName (map (defaultLiteralTypesWith state integerLiteralDefault) typeArguments)
        SemanticFunction inputType outputType ->
          SemanticFunction
            (defaultLiteralTypesWith state integerLiteralDefault inputType)
            (defaultLiteralTypesWith state integerLiteralDefault outputType)
        _ -> expressionType

addInferredClassConstraint :: Text -> ExpressionType -> InferState -> InferState
addInferredClassConstraint constraintName argumentType state =
  modifyInferenceOutput
    ( \output ->
        output
          { outputInferredConstraints =
              TypeSchemeInferredConstraint constraintName argumentType : outputInferredConstraints output,
            outputInferredConstraintCount = outputInferredConstraintCount output + 1
          }
    )
    state

addInferredMethodClassConstraint :: Text -> Text -> ExpressionType -> InferState -> InferState
addInferredMethodClassConstraint constraintName methodKey argumentType state =
  modifyInferenceOutput
    ( \output ->
        output
          { outputInferredConstraints =
              TypeSchemeMethodConstraint constraintName methodKey argumentType : outputInferredConstraints output,
            outputInferredConstraintCount = outputInferredConstraintCount output + 1
          }
    )
    state

addInferredEqualityClassConstraintIfVisible :: ExpressionType -> InferState -> InferState
addInferredEqualityClassConstraintIfVisible argumentType state =
  case activeEqualityClassName state of
    Just equalityClassName -> addInferredClassConstraint equalityClassName argumentType state
    Nothing -> state

activeEqualityClassName :: InferState -> Maybe Text
activeEqualityClassName state =
  if classFactIsUnary "Eq"
    then Just "Eq"
    else case filter importedEqualityClass (Map.toList (inferClassFacts state)) of
      [(className, _)] -> Just className
      _ -> Nothing
  where
    classFactIsUnary className =
      Map.lookup className (inferClassFacts state) == Just 1
    importedEqualityClass (className, arity) =
      arity == 1 && "::Eq" `Text.isSuffixOf` className
