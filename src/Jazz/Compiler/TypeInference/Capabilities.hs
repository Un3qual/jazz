{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.TypeInference.Capabilities
  ( TypeEnvFreeVariables,
    applyCapabilityFacts,
    addInferredEqualityClassConstraintIfVisible,
    addUnpreservedInferredMethodConstraintErrors,
    applyTypeSchemePrimitiveConstraints,
    builtinDollarOperatorExpr,
    capabilityFactsFromState,
    instantiateClassMethodTarget,
    defaultBindingLiteralTypes,
    defaultLiteralTypes,
    deferExplicitConstraintsWithFacts,
    enterModuleCapabilityScope,
    finalizeDeferredExplicitConstraintsAt,
    finalizeDeferredExplicitConstraintsAtWithEntailments,
    flushCurrentModuleCapabilityFacts,
    freeTypeVariablesInEnv,
    importModuleCapabilityFacts,
    inferQualifiedMethodApplicationWithResults,
    instantiateQualifiedMethodType,
    instantiateQualifiedMethodTypeWithExpected,
    instantiateQualifiedMethodTypeWithExplicitTarget,
    deleteTypeEnvFreeVariables,
    insertTypeEnvFreeVariables,
    newInferredClassConstraints,
    qualifiedMethodClassIsVisible,
    qualifiedMethodSignatureType,
    resolveTypeEnvFreeVariables,
    resolveTypeSchemeConstraint,
    restoreCapabilityFacts,
    registerClassCapabilityFacts,
    registerImplementation,
    typeSchemeDefiningFactsFromState,
    typeSchemeReferencedCapabilityFacts,
    structuralRuntimeEqualityType,
    typeEnvFreeVariables,
    updateRootModuleBaselineFacts,
  )
where

import Control.Applicative
  ( (<|>),
  )
import Data.Foldable
  ( toList,
  )
import qualified Data.Foldable as Foldable
import Data.List
  ( uncons,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import Data.Void (Void, absurd)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (coreNodeFacts, coreNodeId),
    CoreNodeId,
    CorePhase (..),
    CoreSort (StatementSort),
    Expr (..),
    ImplMethod (..),
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeIsIntegral,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactCapability,
    qualifiedMethodKey,
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CapabilityMethodKey, ImplId (..), MethodId (..), ResolvedNodeFacts (..), ResolvedReference (..), capabilityMethodKeyFromReference, renderCapabilityId)
import Jazz.Compiler.Diagnostics
  ( DiagnosticContext (SatisfyingConstraint),
    SourceSpan,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name
  ( Name (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
  )
import Jazz.Compiler.SemanticDeclarations (concreteImplementationType, implementationTargetSignature, semanticFunctionArguments)
import Jazz.Compiler.SemanticFacts (StatementDeclarationFact (ImplementationDeclaration))
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithContext,
    annotateNewErrorsWithPrimarySpan,
    mkAmbiguousDeferredConstraintError,
    mkAmbiguousQualifiedMethodBodyError,
    mkAmbiguousQualifiedMethodBodyForArgumentsError,
    mkApplyTypeError,
    mkExplicitConstraintArityError,
    mkMissingClassMethodError,
    mkMissingExplicitConstraintClassError,
    mkMissingExplicitConstraintImplFactError,
    mkMissingImplMethodBodyError,
    mkNoMatchingQualifiedMethodBodyError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError,
  )
import Jazz.Compiler.TypeInference.Environment
  ( TypeEnvFreeVariables,
    deleteTypeEnvFreeVariables,
    freeTypeVariablesInEnv,
    insertTypeEnvFreeVariables,
    resolveTypeEnvFreeVariables,
    typeEnvFreeVariables,
  )
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
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferStatementFactSeeds,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordExpressionEvidenceSeed,
    recordStatementFactSeed,
  )
import Jazz.Compiler.TypeInference.Traversal
  ( InferExprWithModeFn,
    InferenceMode,
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    ExpressionType,
    ImplMethodType (..),
    InferenceVariable,
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    TypeEnvKey,
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
    emptyScopeCapabilityFacts,
    instantiateDeclarationType,
    quantifiedVariablesMembershipSet,
    typeEnvBindingKey,
    typeEnvReferenceKey,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))

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
          (\implKey -> Set.member (concreteImplFactCapability implKey) referencedCapabilityNames)
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
      Set.member (fst methodKey) referencedCapabilityNames

typeSchemeConstraintCapabilityName :: TypeSchemeConstraint -> CapabilityId
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
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceModuleCapabilities = Map.insert (inferCurrentModulePath state) (inferCurrentModuleLocalCapabilityFacts state) (inferModuleCapabilityFacts state)
          }
    )
    state

enterModuleCapabilityScope :: ScopeCapabilityFacts -> ModulePath -> InferState -> InferState
enterModuleCapabilityScope baselineFacts modulePath state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceModulePath = Just modulePath,
            inferenceLocalCapabilities = emptyScopeCapabilityFacts
          }
    )
    (applyCapabilityFacts baselineFacts (flushCurrentModuleCapabilityFacts state))

importModuleCapabilityFacts :: ModulePath -> Maybe Text -> Maybe [Text] -> InferState -> InferState
importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state =
  applyCapabilityFacts
    ( capabilityFactsFromState state
        <> filterImportedCapabilityFacts maybeAlias maybeSymbolNames (Map.findWithDefault emptyScopeCapabilityFacts (Just modulePath) (inferModuleCapabilityFacts state))
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
                  (\className _ -> Set.member (renderCapabilityId className) visibleSymbols)
                  (scopeClassFacts facts),
              scopeGeneratedEqualityClassFacts =
                Set.filter (\capability -> Set.member (renderCapabilityId capability) visibleSymbols) (scopeGeneratedEqualityClassFacts facts),
              scopeConcreteImplFacts =
                Set.filter
                  (\implKey -> Set.member (renderCapabilityId (concreteImplFactCapability implKey)) visibleSymbols)
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
              Set.member (renderCapabilityId (fst methodKey)) visibleSymbols

registerClassCapabilityFacts :: ResolvedName -> Int -> [(ResolvedName, ClassMethodType)] -> InferState -> InferState
registerClassCapabilityFacts capabilityName arity methods =
  modifyCapabilityFacts $ \facts ->
    facts
      { scopeClassFacts = Map.insert (CapabilityId capabilityName) arity (scopeClassFacts facts),
        scopeClassMethodSignatures = foldl' insertMethod (scopeClassMethodSignatures facts) methods
      }
  where
    insertMethod signatures (methodName, methodType) = Map.insert (qualifiedMethodKey capabilityName methodName) methodType signatures

modifyCapabilityFacts :: (ScopeCapabilityFacts -> ScopeCapabilityFacts) -> InferState -> InferState
modifyCapabilityFacts update state =
  let stateWithVisibleFacts = applyCapabilityFacts (update (capabilityFactsFromState state)) state
   in modifyModuleInferenceState (\moduleState -> moduleState {inferenceLocalCapabilities = update (inferCurrentModuleLocalCapabilityFacts state)}) stateWithVisibleFacts

registerImplementation :: CoreNode 'Resolved 'StatementSort -> ResolvedName -> [SemanticType ResolvedName Void] -> [ImplMethod 'Resolved] -> InferState -> InferState
registerImplementation node capabilityName targets methods =
  recordStatementFactSeed (coreNodeId node) ([], ImplementationDeclaration capabilityName (map (fmap absurd) targets)) . modifyCapabilityFacts seed
  where
    seed facts = seedImplMethodFacts node capabilityName targets methods $
      case targets of
        [target] | concreteImplementationType target -> facts {scopeConcreteImplFacts = Set.insert (ConcreteImplFact (CapabilityId capabilityName) target) (scopeConcreteImplFacts facts)}
        _ -> facts

seedImplMethodFacts ::
  CoreNode 'Resolved 'StatementSort ->
  ResolvedName ->
  [SemanticType ResolvedName Void] ->
  [ImplMethod 'Resolved] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedImplMethodFacts implementationNode capabilityName arguments methods facts =
  case arguments of
    [implTarget]
      | concreteImplementationType implTarget ->
          facts
            { scopeConcreteImplMethods =
                foldl'
                  insertImplMethod
                  (scopeConcreteImplMethods facts)
                  methods
            }
      where
        insertImplMethod acc (ImplMethod methodNode methodName _) =
          Map.insertWith
            (\newMethods existingMethods -> existingMethods ++ newMethods)
            (qualifiedMethodKey capabilityName methodName)
            [ImplMethodType implTarget capability identity]
            acc
          where
            (capability, member) = case resolvedNodeReference (coreNodeFacts methodNode) of
              Just (CapabilityMethodReference target name) -> (target, name)
              _ -> error "implementation method has no resolved capability target"
            identity = MethodId (ImplId (resolvedNodeOwner (coreNodeFacts implementationNode), coreNodeId implementationNode), member)
    _ -> facts

builtinDollarOperatorExpr :: TypeEnv -> Expr 'Resolved -> Bool
builtinDollarOperatorExpr env expr =
  case expr of
    EOperatorValue _ "$" -> True
    EVar node name ->
      case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) name) env of
        Just (BuiltinOperatorAliasTypeBinding "$") -> True
        Just (OperatorAliasSchemeTypeBinding "$" _) -> True
        _ -> False
    _ -> False

qualifiedMethodClassIsVisible :: CapabilityMethodKey -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  Map.member (fst methodKey) (inferClassFacts state)

inferQualifiedMethodApplicationWithResults ::
  InferExprWithModeFn ->
  InferenceMode ->
  TypeEnv ->
  InferState ->
  CoreNodeId ->
  CapabilityMethodKey ->
  [Expr 'Resolved] ->
  (Maybe ExpressionType, InferState, [Maybe ExpressionType])
inferQualifiedMethodApplicationWithResults inferExpression mode env state nodeId methodKey argumentExprs =
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
            inferExpression mode env stateAcc argumentExpr
       in (result : resultsAcc, stateAfterArgument)

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
        && case closedConstraintType resolvedArgumentType of
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

concreteInferredMethodConstraintSatisfied :: InferState -> CapabilityId -> CapabilityMethodKey -> ExpressionType -> Bool
concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType =
  let resolvedArgumentType = resolveType state argumentType
      facts = capabilityFactsFromState state
   in Set.null (freeTypeVariables resolvedArgumentType)
        && concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey resolvedArgumentType

concreteInferredMethodConstraintHasUniqueCandidate :: ScopeCapabilityFacts -> InferState -> CapabilityId -> CapabilityMethodKey -> ExpressionType -> Bool
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
      | argumentHint <- inferredConstraintCandidateTypes facts state (Just methodKey) argumentType,
        concreteImplFactExists constraintName argumentHint facts,
        concreteImplMethodBodyExists methodKey argumentHint facts
      ]

uniqueExactRuntimeCandidateHint :: InferState -> ExpressionType -> [SemanticType ResolvedName Void] -> Bool
uniqueExactRuntimeCandidateHint state argumentType candidateHints =
  case [ candidateHint
       | candidateHint <- candidateHints,
         constraintTypeExactlyMatchesExpressionType state candidateHint argumentType
       ] of
    [candidateHint] ->
      not (constraintTypeContainsList candidateHint)
    _ -> False

resolveTypeSchemeConstraint :: InferState -> TypeSchemeConstraint -> TypeSchemeConstraint
resolveTypeSchemeConstraint state = fmap (resolveType state)

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
    (resolveStatementDeferredExplicitConstraints spanValue entailingConstraints statementStartState state)

resolveStatementDeferredExplicitConstraints :: SourceSpan -> [TypeSchemeConstraint] -> InferState -> InferState -> InferState
resolveStatementDeferredExplicitConstraints spanValue entailingConstraints statementStartState state =
  foldl' resolveWithContext stateWithoutStatementConstraints statementConstraints
  where
    resolveWithContext before constraint =
      annotateNewErrorsWithContext
        (SatisfyingConstraint (renderCapabilityId (deferredConstraintName constraint)))
        spanValue
        before
        (resolveDeferredExplicitConstraint before constraint)
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
                case uncons (constraintCandidateTypesForDeferred facts state inferredConstraint constraintName maybeMethodKey unresolvedArgumentType) of
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
                          renderCapabilityId constraintName <> "(" <> renderSignatureType (implementationTargetSignature firstArgumentHint) <> ")"
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

constraintCandidateTypesForDeferred ::
  ScopeCapabilityFacts ->
  InferState ->
  Bool ->
  CapabilityId ->
  Maybe CapabilityMethodKey ->
  ExpressionType ->
  [SemanticType ResolvedName Void]
constraintCandidateTypesForDeferred facts state inferredConstraint _ maybeMethodKey argumentType
  | inferredConstraint =
      inferredConstraintCandidateTypes facts state maybeMethodKey argumentType
  | otherwise =
      case closedConstraintType (defaultLiteralTypes state argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

constraintImplFactExistsForDeferred :: ScopeCapabilityFacts -> Bool -> CapabilityId -> SemanticType ResolvedName Void -> Bool
constraintImplFactExistsForDeferred facts inferredConstraint constraintName argumentHint =
  if inferredConstraint
    then concreteImplFactExists constraintName argumentHint facts
    else concreteImplFactExistsExactly constraintName argumentHint facts

inferredConstraintCandidateTypes :: ScopeCapabilityFacts -> InferState -> Maybe CapabilityMethodKey -> ExpressionType -> [SemanticType ResolvedName Void]
inferredConstraintCandidateTypes facts state maybeMethodKey argumentType =
  dedupeConstraintTypes (defaultHint ++ methodCandidateHints)
  where
    defaultHint =
      case closedConstraintType (defaultLiteralTypes state argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

    methodCandidateHints =
      case maybeMethodKey of
        Nothing -> []
        Just methodKey ->
          [ implTarget
          | ImplMethodType implTarget _ _ <- Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts),
            constraintTypeMatchesExpressionType state implTarget argumentType
          ]

dedupeConstraintTypes :: [SemanticType ResolvedName Void] -> [SemanticType ResolvedName Void]
dedupeConstraintTypes =
  go Set.empty
  where
    go _ [] = []
    go seen (signatureType : rest)
      | Set.member signatureType seen = go seen rest
      | otherwise = signatureType : go (Set.insert signatureType seen) rest

constraintTypeMatchesExpressionType :: InferState -> SemanticType ResolvedName Void -> ExpressionType -> Bool
constraintTypeMatchesExpressionType state signatureType expressionType =
  case (signatureType, integerLiteralRangeFor state expressionType, resolveType state expressionType) of
    (SemanticInt, Just literalRange, _) ->
      integerLiteralRangeFitsNumericType literalRange NumericInt64
    (SemanticNumeric numericType, Just literalRange, _) ->
      numericTypeIsIntegral numericType
        && integerLiteralRangeFitsNumericType literalRange numericType
    (SemanticList signatureElementType, _, SemanticList elementType) ->
      constraintTypeMatchesExpressionType state signatureElementType elementType
    (SemanticTuple signatureElementTypes, _, SemanticTuple elementTypes)
      | length signatureElementTypes == length elementTypes ->
          and (zipWith (constraintTypeMatchesExpressionType state) signatureElementTypes elementTypes)
    (SemanticData signatureName signatureArguments, _, SemanticData typeName typeArguments)
      | signatureName == typeName,
        length signatureArguments == length typeArguments ->
          and (zipWith (constraintTypeMatchesExpressionType state) signatureArguments typeArguments)
    (SemanticFunction signatureArgument signatureResult, _, SemanticFunction argumentType resultType) ->
      constraintTypeMatchesExpressionType state signatureArgument argumentType
        && constraintTypeMatchesExpressionType state signatureResult resultType
    _ ->
      case closedConstraintType (defaultLiteralTypes state (resolveType state expressionType)) of
        Just argumentHint -> constraintTypesCompatible signatureType argumentHint
        Nothing -> False

concreteImplFactExists :: CapabilityId -> SemanticType ResolvedName Void -> ScopeCapabilityFacts -> Bool
concreteImplFactExists constraintName target facts =
  any (\(ConcreteImplFact capability candidate) -> capability == constraintName && constraintTypesCompatible candidate target) (scopeConcreteImplFacts facts)

concreteImplFactExistsExactly :: CapabilityId -> SemanticType ResolvedName Void -> ScopeCapabilityFacts -> Bool
concreteImplFactExistsExactly constraintName target facts =
  Set.member (ConcreteImplFact constraintName target) (scopeConcreteImplFacts facts)

concreteImplMethodBodyExists :: CapabilityMethodKey -> SemanticType ResolvedName Void -> ScopeCapabilityFacts -> Bool
concreteImplMethodBodyExists methodKey argumentHint facts =
  any
    (\(ImplMethodType implTarget _ _) -> constraintTypesCompatible implTarget argumentHint)
    (Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts))

inferredEqualityConstraintCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Maybe CapabilityMethodKey -> CapabilityId -> ExpressionType -> Bool
inferredEqualityConstraintCanUseStructuralRuntimeEquality state facts maybeMethodKey constraintName argumentType =
  maybeMethodKey == Nothing
    && equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName
    && structuralRuntimeEqualityType state argumentType

equalityConstraintNameCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> CapabilityId -> Bool
equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName =
  activeEqualityClassName state == Just constraintName
    || generatedHiddenEqualityClassFact constraintName facts

generatedHiddenEqualityClassFact :: CapabilityId -> ScopeCapabilityFacts -> Bool
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

instantiateQualifiedMethodType :: CoreNodeId -> ResolvedReference -> InferState -> Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodType nodeId reference state = do
  methodKey <- capabilityMethodKeyFromReference reference
  if qualifiedMethodClassIsVisible methodKey state
    then Just (resolveQualifiedMethodType nodeId methodKey state)
    else Nothing

instantiateQualifiedMethodTypeWithExpected ::
  CoreNodeId ->
  ResolvedReference ->
  ExpressionType ->
  InferState ->
  Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodTypeWithExpected nodeId reference expectedType state = do
  methodKey <- capabilityMethodKeyFromReference reference
  if qualifiedMethodClassIsVisible methodKey state
    then Just (resolveQualifiedMethodTypeWithExpected nodeId methodKey expectedType state)
    else Nothing

resolveQualifiedMethodTypeWithExpected ::
  CoreNodeId ->
  CapabilityMethodKey ->
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

        candidateExactlyMatchesExpected (ImplMethodType implTarget _ _, _, _) =
          case classMethodType of
            ClassMethodType classParameter methodSignature ->
              case instantiateClassMethodTarget classParameter implTarget methodSignature of
                Just candidateType ->
                  resolveType state candidateType == defaultLiteralTypes state (resolveType state expectedType)
                Nothing -> False

resolveQualifiedMethodType :: CoreNodeId -> CapabilityMethodKey -> InferState -> (Maybe ExpressionType, InferState)
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
  CapabilityMethodKey ->
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
        (\(ImplMethodType implTarget _ _) -> fmap absurd implTarget == defaultLiteralTypes state (resolveType state explicitTarget))
        (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))

resolveQualifiedMethodApplicationType ::
  CoreNodeId ->
  CapabilityMethodKey ->
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
  CapabilityMethodKey ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirement methodKey classMethodType state argumentTypes =
  {-# SCC "jazz-stage:capability-solving" #-}
  inferQualifiedMethodRequirementWithoutCostCentre methodKey classMethodType state argumentTypes

inferQualifiedMethodRequirementWithoutCostCentre ::
  CapabilityMethodKey ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirementWithoutCostCentre methodKey (ClassMethodType classParameter methodSignature) state argumentTypes = do
  let capabilityName = fst methodKey
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

classMethodSignatureHasTargetArgument :: Text -> SemanticType ResolvedName Text -> Bool
classMethodSignatureHasTargetArgument classParameter methodSignature =
  any (Foldable.elem classParameter) (fst (semanticFunctionArguments methodSignature))

selectQualifiedMethodCandidate ::
  CoreNodeId ->
  CapabilityMethodKey ->
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
      (Just matchedType, recordSelectedQualifiedMethodEvidence nodeId implMethodType matchedState)
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

recordSelectedQualifiedMethodEvidence :: CoreNodeId -> ImplMethodType -> InferState -> InferState
recordSelectedQualifiedMethodEvidence nodeId method =
  recordExpressionEvidenceSeed nodeId (ExpressionEvidenceSeed (implMethodCapability method) implementationId (implMethodIdentity method) (fmap absurd (implMethodTarget method)))
  where
    MethodId (implementationId, _) = implMethodIdentity method

recordSelectedQualifiedMethodResult ::
  CoreNodeId ->
  CapabilityMethodKey ->
  ImplMethodType ->
  (Maybe ExpressionType, InferState) ->
  (Maybe ExpressionType, InferState)
recordSelectedQualifiedMethodResult nodeId _ implMethodType (maybeResultType, selectedState) =
  ( maybeResultType,
    case maybeResultType of
      Nothing -> selectedState
      Just _ -> recordSelectedQualifiedMethodEvidence nodeId implMethodType selectedState
  )

qualifiedMethodCandidateExactlyMatchesArguments ::
  InferState ->
  TypeEnv ->
  ClassMethodType ->
  ImplMethodType ->
  [(Expr 'Resolved, ExpressionType)] ->
  Bool
qualifiedMethodCandidateExactlyMatchesArguments state env (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget _ _) typedArguments =
  case instantiateClassMethodTarget classParameter implTarget methodSignature of
    Just substitutedSignature ->
      let (genericArgumentTypes, _) = semanticFunctionArguments methodSignature
          (candidateArgumentTypes, _) = semanticFunctionArguments substitutedSignature
          suppliedArgumentCount = length typedArguments
          targetArgumentPositions = map (Foldable.elem classParameter) (take suppliedArgumentCount genericArgumentTypes)
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length candidateArgumentTypes
            && or targetArgumentPositions
            && and (zipWith3 exactCandidateArgumentMatches targetArgumentPositions (take suppliedArgumentCount candidateArgumentTypes) typedArguments)
    Nothing -> False
  where
    exactCandidateArgumentMatches targetArgumentPosition candidateType (argumentExpr, expressionType) =
      not targetArgumentPosition
        || case closedConstraintType candidateType of
          Nothing -> False
          Just signatureType -> case scalarApplicationRuntimeHint state env expressionType argumentExpr of
            Just runtimeHint -> runtimeHint == signatureType
            Nothing ->
              resolveType state candidateType == defaultLiteralTypes state (resolveType state expressionType)
                && constraintExpressionHasExactEvidence state env signatureType argumentExpr

scalarApplicationRuntimeHint :: InferState -> TypeEnv -> ExpressionType -> Expr 'Resolved -> Maybe (SemanticType ResolvedName Void)
scalarApplicationRuntimeHint state env expressionType argumentExpr =
  case argumentExpr of
    EApply {} ->
      constraintExpressionRuntimeHint state env argumentExpr
        <|> inferredScalarHint
    _ -> Nothing
  where
    inferredScalarHint =
      closedConstraintType
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

constraintExpressionHasExactEvidence :: InferState -> TypeEnv -> SemanticType ResolvedName Void -> Expr 'Resolved -> Bool
constraintExpressionHasExactEvidence state env signatureType argumentExpr =
  case (signatureType, argumentExpr) of
    (SemanticList elementType, EList _ elements) ->
      not (null elements)
        && all (constraintExpressionHasExactEvidence state env elementType) elements
    (SemanticTuple elementTypes, ETuple _ elements)
      | length elementTypes == length elements ->
          and (zipWith (constraintExpressionHasExactEvidence state env) elementTypes elements)
    (SemanticData typeName typeArguments, EApply {})
      | not (null typeArguments) ->
          constructorApplicationExpressionHasExactEvidence state env typeName typeArguments argumentExpr
            || constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (SemanticFunction {}, _) ->
      constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EVar {})
      | constraintTypeContainsList signatureType ->
          constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EApply {})
      | constraintTypeContainsList signatureType ->
          constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EIf {}) ->
      constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EPatternCase {}) ->
      constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    (_, EBlock {})
      | constraintTypeContainsList signatureType ->
          constraintExpressionRuntimeHintMatches state env signatureType argumentExpr
    _ -> True

constraintExpressionRuntimeHintMatches :: InferState -> TypeEnv -> SemanticType ResolvedName Void -> Expr 'Resolved -> Bool
constraintExpressionRuntimeHintMatches state env signatureType argumentExpr =
  case constraintExpressionRuntimeHint state env argumentExpr of
    Just runtimeHint -> runtimeHint == signatureType
    Nothing -> False

constraintExpressionRuntimeHint :: InferState -> TypeEnv -> Expr 'Resolved -> Maybe (SemanticType ResolvedName Void)
constraintExpressionRuntimeHint state env argumentExpr =
  constraintExpressionRuntimeHintWithLocalHints state env Map.empty argumentExpr

constraintExpressionRuntimeHintWithLocalHints ::
  InferState ->
  TypeEnv ->
  Map TypeEnvKey (SemanticType ResolvedName Void) ->
  Expr 'Resolved ->
  Maybe (SemanticType ResolvedName Void)
constraintExpressionRuntimeHintWithLocalHints state env localHints argumentExpr =
  case argumentExpr of
    EVar node referencedName ->
      Map.lookup (typeEnvReferenceKey (coreNodeFacts node) referencedName) localHints
        <|> (Map.lookup (typeEnvReferenceKey (coreNodeFacts node) referencedName) env >>= typeBindingRuntimeHint state)
    EApply _ (EApply _ dollarExpr functionExpr) _
      | builtinDollarOperatorExpr env dollarExpr ->
          case constraintExpressionRuntimeHintWithLocalHints state env localHints functionExpr of
            Just (SemanticFunction _ resultType) -> Just resultType
            _ -> Nothing
    EApply _ functionExpr _ ->
      case constraintExpressionRuntimeHintWithLocalHints state env localHints functionExpr of
        Just (SemanticFunction _ resultType) -> Just resultType
        _ -> Nothing
    EIf _ _ thenExpr elseExpr ->
      commonConstraintExpressionRuntimeHint state env localHints [thenExpr, elseExpr]
    EPatternCase _ _ caseArms ->
      commonConstraintExpressionRuntimeHint state env localHints [bodyExpr | CaseArm _ _ _ bodyExpr <- caseArms]
    EBlock _ statements ->
      constraintBlockRuntimeHint state env localHints statements
    _ -> Nothing

commonConstraintExpressionRuntimeHint ::
  InferState ->
  TypeEnv ->
  Map TypeEnvKey (SemanticType ResolvedName Void) ->
  [Expr 'Resolved] ->
  Maybe (SemanticType ResolvedName Void)
commonConstraintExpressionRuntimeHint _ _ _ [] = Nothing
commonConstraintExpressionRuntimeHint state env localHints (firstExpr : restExprs) = do
  firstHint <- constraintExpressionRuntimeHintWithLocalHints state env localHints firstExpr
  if all
    (\expr -> constraintExpressionRuntimeHintWithLocalHints state env localHints expr == Just firstHint)
    restExprs
    then Just firstHint
    else Nothing

constraintBlockRuntimeHint ::
  InferState ->
  TypeEnv ->
  Map TypeEnvKey (SemanticType ResolvedName Void) ->
  [Statement 'Resolved] ->
  Maybe (SemanticType ResolvedName Void)
constraintBlockRuntimeHint state env initialLocalHints statements =
  go initialLocalHints Map.empty statements
  where
    go _ _ [] =
      Nothing
    go localHints _ [SExpr _ expr] =
      constraintExpressionRuntimeHintWithLocalHints state env localHints expr
    go localHints pendingHints (statement : rest) =
      case statement of
        SSignature node name _ ->
          let key = typeEnvReferenceKey (coreNodeFacts node) name
              nextPendingHints =
                case checkedSignatureRuntimeHint (coreNodeId node) of
                  Just runtimeHint -> Map.insert key runtimeHint pendingHints
                  Nothing -> Map.delete key pendingHints
           in go localHints nextPendingHints rest
        SLet node name valueExpr ->
          let key = typeEnvBindingKey (coreNodeFacts node) name
              bindingHint =
                Map.lookup key pendingHints
                  <|> constraintExpressionRuntimeHintWithLocalHints state env localHints valueExpr
              nextLocalHints =
                case bindingHint of
                  Just runtimeHint -> Map.insert (typeEnvBindingKey (coreNodeFacts node) name) runtimeHint localHints
                  Nothing -> localHints
           in go nextLocalHints (Map.delete key pendingHints) rest
        _ ->
          go localHints pendingHints rest

    checkedSignatureRuntimeHint nodeId = do
      (bindings, _) <- Map.lookup nodeId (inferStatementFactSeeds state)
      case bindings of
        [(_, binding)] -> typeBindingRuntimeHint state binding
        _ -> Nothing

typeBindingRuntimeHint :: InferState -> TypeBinding -> Maybe (SemanticType ResolvedName Void)
typeBindingRuntimeHint state binding =
  case binding of
    PlainTypeBinding bindingType ->
      closedConstraintType (defaultLiteralTypes state bindingType)
    SchemeTypeBinding typeScheme
      | Set.null (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme)) ->
          closedConstraintType (defaultLiteralTypes state (schemeResultType typeScheme))
    OperatorAliasSchemeTypeBinding _ typeScheme
      | Set.null (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme)) ->
          closedConstraintType (defaultLiteralTypes state (schemeResultType typeScheme))
    _ -> Nothing

constraintTypeContainsList :: SemanticType ResolvedName Void -> Bool
constraintTypeContainsList signatureType =
  case signatureType of
    SemanticList {} -> True
    SemanticTuple elementTypes ->
      any constraintTypeContainsList elementTypes
    SemanticData _ typeArguments ->
      any constraintTypeContainsList typeArguments
    SemanticFunction argumentType resultType ->
      constraintTypeContainsList argumentType
        || constraintTypeContainsList resultType
    _ -> False

constructorApplicationExpressionHasExactEvidence :: InferState -> TypeEnv -> ResolvedName -> [SemanticType ResolvedName Void] -> Expr 'Resolved -> Bool
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

constructorExpressionSpine :: Expr 'Resolved -> Maybe (TypeEnvKey, [Expr 'Resolved])
constructorExpressionSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply _ functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar node constructorName ->
          Just (typeEnvReferenceKey (coreNodeFacts node) constructorName, argumentExprs)
        _ ->
          Nothing

constructorArgumentExpressionHasExactEvidence :: InferState -> TypeEnv -> Map Text (SemanticType ResolvedName Void) -> ConstructorArgumentType -> Expr 'Resolved -> Bool
constructorArgumentExpressionHasExactEvidence state env typeParameterBindings constructorArgument argumentExpr =
  case constructorArgument of
    ConstructorArgumentType fieldType ->
      case instantiateDeclarationType typeParameterBindings fieldType of
        Just concreteField -> constraintExpressionHasExactEvidence state env concreteField argumentExpr
        Nothing -> True
    ConstructorArgumentFresh -> True

constraintTypeExactlyMatchesExpressionType :: InferState -> SemanticType ResolvedName Void -> ExpressionType -> Bool
constraintTypeExactlyMatchesExpressionType state target expressionType =
  fmap absurd target == defaultLiteralTypes state (resolveType state expressionType)

closedConstraintType :: ExpressionType -> Maybe (SemanticType ResolvedName Void)
closedConstraintType = traverse (const Nothing)

constraintTypesCompatible :: SemanticType ResolvedName Void -> SemanticType ResolvedName Void -> Bool
constraintTypesCompatible left right = normalize left == normalize right
  where
    normalize target = case target of
      SemanticInt -> SemanticNumeric NumericInt64
      SemanticFloat -> SemanticNumeric NumericFloat64
      SemanticList element -> SemanticList (normalize element)
      SemanticTuple elements -> SemanticTuple (map normalize elements)
      SemanticData name arguments -> SemanticData name (map normalize arguments)
      SemanticFunction argument result -> SemanticFunction (normalize argument) (normalize result)
      _ -> target

applyQualifiedMethodCandidate ::
  CapabilityMethodKey ->
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
  CapabilityMethodKey ->
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
  CapabilityMethodKey ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  (Maybe ExpressionType, InferState)
qualifiedMethodSignatureType _ (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget _ _) state =
  (instantiateClassMethodTarget classParameter implTarget methodSignature, state)

instantiateClassMethodTarget :: Text -> SemanticType ResolvedName Void -> SemanticType ResolvedName Text -> Maybe ExpressionType
instantiateClassMethodTarget classParameter implTarget =
  instantiateDeclarationType (Map.singleton classParameter (fmap absurd implTarget))

classMethodPayloadToGenericExpressionType ::
  Text -> ExpressionType -> SemanticType ResolvedName Text -> Maybe ExpressionType
classMethodPayloadToGenericExpressionType classParameter classTarget =
  instantiateDeclarationType (Map.singleton classParameter classTarget)

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

addInferredClassConstraint :: CapabilityId -> ExpressionType -> InferState -> InferState
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

addInferredMethodClassConstraint :: CapabilityId -> CapabilityMethodKey -> ExpressionType -> InferState -> InferState
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

activeEqualityClassName :: InferState -> Maybe CapabilityId
activeEqualityClassName state =
  case filter (unqualifiedEqualityClass . fst) classes of
    (capability, 1) : _ -> Just capability
    _ -> case filter importedEqualityClass classes of
      [(capability, _)] -> Just capability
      _ -> Nothing
  where
    classes = Map.toList (inferClassFacts state)
    unqualifiedEqualityClass (CapabilityId name) = case name of
      UserName (ResolvedUserName ImportedModule {} _ _) -> False
      _ -> identifierText name == "Eq"
    importedEqualityClass (CapabilityId (UserName (ResolvedUserName ImportedModule {} _ member)), arity) =
      arity == 1 && identifierText member == "Eq"
    importedEqualityClass _ = False
