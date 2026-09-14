{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Jazz.Compiler.TypeInference.Capabilities
  ( TypeEnvFreeVariables,
    applyCapabilityFacts,
    addInferredEqualityClassConstraintIfVisible,
    addUnpreservedInferredMethodConstraintErrors,
    applyTypeSchemePrimitiveConstraints,
    typeSchemePrimitiveConstraints,
    checkMethodPrimitiveConstraints,
    capabilityFactsFromState,
    defaultBindingLiteralTypes,
    defaultLiteralTypes,
    deferExplicitConstraintsWithFacts,
    enterModuleCapabilityScope,
    finalizeDeferredExplicitConstraintsAt,
    finalizeDeferredExplicitConstraintsAtWithEntailments,
    flushCurrentModuleCapabilityFacts,
    freeTypeVariablesInEnv,
    importModuleCapabilityFacts,
    MethodSelection (..),
    reindexDeclarationScheme,
    instantiateQualifiedMethodType,
    instantiateQualifiedMethodTypeWithExplicitTarget,
    deleteTypeEnvFreeVariables,
    insertTypeEnvFreeVariables,
    newInferredClassConstraints,
    qualifiedMethodClassIsVisible,
    resolveTypeEnvFreeVariables,
    resolveTypeSchemeConstraint,
    restoreCapabilityFacts,
    registerClassCapabilityFacts,
    registerImplementation,
    implementationsOverlap,
    validateImplementationCoherence,
    newConstraintEvidence,
    addInferredConstraint,
    freshImplementation,
    superclassPath,
    resolveCapabilityEvidence,
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
import Control.Monad (guard)
import qualified Control.Monad.Trans.State.Strict as Trial
import Data.Foldable
  ( toList,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Sequence as Seq
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import Jazz.Compiler.CapabilityFacts
  ( qualifiedMethodKey,
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CapabilityMethodKey, ResolvedReference (..), capabilityMethodKeyFromReference, renderCapabilityId)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticContext (SatisfyingConstraint),
    SourceSpan (..),
  )
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name
  ( Identifier,
    Name (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
  )
import Jazz.Compiler.SignatureRendering
  ( renderSemanticTypeWith,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithContext,
    annotateNewErrorsWithPrimarySpan,
    mkAmbiguousDeferredConstraintError,
    mkAmbiguousQualifiedMethodBodyError,
    mkInvalidCapabilityDeclarationError,
    mkMissingClassMethodError,
    mkMissingExplicitConstraintImplFactError,
    mkMissingImplMethodBodyError,
    mkNoMatchingQualifiedMethodBodyError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError,
    mkUndeclaredSignatureConstraintError,
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
    freshTypeVars,
    integerLiteralRangeFor,
    resolveType,
    supportsRuntimeEqualityType,
    unifyTypes,
    unifyTypesExactly,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    DeferredExplicitConstraint (..),
    EvidenceReference (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferClassFacts,
    inferClassMethodSignatures,
    inferCurrentModuleLocalCapabilityFacts,
    inferCurrentModulePath,
    inferDeferredExplicitConstraintCount,
    inferErrorCount,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferModuleCapabilityFacts,
    inferNumericVars,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassDefinition (..),
    ClassMethodType (..),
    ExpressionType,
    ImplementationTemplate (..),
    InferenceVariable (..),
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeScheme,
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
    emptyScopeCapabilityFacts,
    implementationTarget,
    instantiateDeclarationType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesOrderedList,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..), semanticApplicationSpine, substituteSemanticVariables)

capabilityFactsFromState :: InferState -> ScopeCapabilityFacts
capabilityFactsFromState = declarationCapabilities . inferDeclarations

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
        Map.restrictKeys (scopeClassFacts facts) referencedCapabilityNames,
      scopeGeneratedEqualityClassFacts =
        Set.intersection (scopeGeneratedEqualityClassFacts facts) referencedCapabilityNames,
      scopeClassMethodSignatures =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeClassMethodSignatures facts)
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
applyCapabilityFacts facts =
  modifyDeclarationState (\declarations -> declarations {declarationCapabilities = facts})

restoreCapabilityFacts :: InferState -> InferState -> InferState
restoreCapabilityFacts previousState nextState =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceLocalCapabilities =
              inferCurrentModuleLocalCapabilityFacts previousState
          }
    )
    (applyCapabilityFacts (capabilityFactsFromState previousState) nextState)

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

importModuleCapabilityFacts :: ModulePath -> InferState -> InferState
importModuleCapabilityFacts modulePath state =
  applyCapabilityFacts
    ( capabilityFactsFromState state
        <> Map.findWithDefault emptyScopeCapabilityFacts (Just modulePath) (inferModuleCapabilityFacts state)
    )
    state

registerClassCapabilityFacts :: ResolvedName -> ClassDefinition -> [(ResolvedName, ClassMethodType)] -> InferState -> InferState
registerClassCapabilityFacts capabilityName definition methods =
  modifyCapabilityFacts $ \facts ->
    facts
      { scopeClassFacts = Map.insert (CapabilityId capabilityName) definition (scopeClassFacts facts),
        scopeClassMethodSignatures = foldl' insertMethod (scopeClassMethodSignatures facts) methods
      }
  where
    insertMethod signatures (methodName, methodType) = Map.insert (qualifiedMethodKey capabilityName methodName) methodType signatures

modifyCapabilityFacts :: (ScopeCapabilityFacts -> ScopeCapabilityFacts) -> InferState -> InferState
modifyCapabilityFacts update state =
  let stateWithVisibleFacts = applyCapabilityFacts (update (capabilityFactsFromState state)) state
   in modifyModuleInferenceState (\moduleState -> moduleState {inferenceLocalCapabilities = update (inferCurrentModuleLocalCapabilityFacts state)}) stateWithVisibleFacts

registerImplementation :: ImplementationTemplate -> InferState -> InferState
registerImplementation template = modifyCapabilityFacts $ \facts ->
  facts {scopeImplementations = Map.insert (implementationIdentity template) template (scopeImplementations facts)}

freshImplementation :: ImplementationTemplate -> Trial.StateT InferState Maybe (ExpressionType, Map Text ExpressionType, [TypeSchemeConstraint])
freshImplementation template = do
  let scheme = implementationScheme template
      parameters = quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme)
  variables <- Trial.state (freshTypeVars (length parameters))
  let bindings = Map.fromList (zip parameters variables)
  target <- Trial.StateT (\state -> (,state) <$> instantiateDeclarationType bindings (implementationTarget template))
  prerequisites <- Trial.StateT (\state -> (,state) <$> traverse (traverse (instantiateDeclarationType bindings)) (schemeClassConstraints scheme))
  pure (target, bindings, prerequisites)

implementationsOverlap :: InferState -> ImplementationTemplate -> ImplementationTemplate -> Bool
implementationsOverlap state left right =
  implementationCapability left == implementationCapability right
    && isJust (Trial.execStateT trial state)
  where
    trial = do
      (leftTarget, _, _) <- freshImplementation left
      (rightTarget, _, _) <- freshImplementation right
      Trial.StateT (fmap ((),) . unifyTypesExactly leftTarget rightTarget)

validateImplementationCoherence :: InferState -> InferState
validateImplementationCoherence state =
  foldl'
    ( \current capability ->
        addTypeError
          current
          (mkInvalidCapabilityDeclarationError (SourceSpan 1 1) ("overlapping impl declarations for '" <> renderCapabilityId capability <> "'"))
    )
    state
    collisions
  where
    implementations = Map.toList (scopeImplementations (capabilityFactsFromState state))
    collisions = Set.toList (Set.fromList [implementationCapability left | (leftId, left) <- implementations, (rightId, right) <- implementations, leftId < rightId, implementationsOverlap state left right])

qualifiedMethodClassIsVisible :: CapabilityMethodKey -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  Map.member (fst methodKey) (inferClassFacts state)

data MethodSelection = MethodSelection
  { selectedMethodType :: Maybe ExpressionType,
    selectedMethodEvidence :: [EvidenceReference]
  }

type SchemeInstantiation = TypeScheme -> InferState -> (Maybe ExpressionType, InferState)

newConstraintEvidence :: InferState -> InferState -> [EvidenceReference]
newConstraintEvidence before after =
  [ PendingEvidence (deferredConstraintName constraint) (snd <$> deferredMethodKey constraint) (deferredArgumentType constraint)
  | constraint <- toList (Seq.drop (inferDeferredExplicitConstraintCount before) (outputDeferredConstraints (inferOutput after))),
    not (deferredWasInferred constraint) || isJust (deferredMethodKey constraint)
  ]

reindexDeclarationScheme :: SemanticScheme Text -> Maybe TypeScheme
reindexDeclarationScheme scheme = do
  constraints <- traverse (traverse (traverse (`Map.lookup` parameters))) (schemeClassConstraints scheme)
  primitives <- traverse (traverse (traverse (`Map.lookup` parameters))) (schemePrimitiveConstraints scheme)
  resultType <- traverse (`Map.lookup` parameters) (schemeResultType scheme)
  pure (SemanticScheme (quantifiedVariablesFromPreferred order (Set.fromList order)) constraints primitives mempty resultType)
  where
    names = quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme)
    parameters = Map.fromList (zip names [0 ..])
    order = [variable | name <- names, Just variable <- [Map.lookup name parameters]]

instantiateMethod :: SchemeInstantiation -> CapabilityMethodKey -> InferState -> (MethodSelection, InferState)
instantiateMethod instantiate methodKey state = case Map.lookup methodKey (inferClassMethodSignatures state) >>= reindexDeclarationScheme . classMethodScheme of
  Nothing -> (MethodSelection Nothing [], addTypeError state (mkMissingClassMethodError methodKey))
  Just scheme ->
    let (result, next) = instantiate scheme state
     in (MethodSelection result (newConstraintEvidence state next), next)

instantiateQualifiedMethodType :: SchemeInstantiation -> ResolvedReference -> InferState -> Maybe (MethodSelection, InferState)
instantiateQualifiedMethodType instantiate reference state = do
  methodKey <- capabilityMethodKeyFromReference reference
  guard (qualifiedMethodClassIsVisible methodKey state)
  pure (instantiateMethod instantiate methodKey state)

instantiateQualifiedMethodTypeWithExplicitTarget :: SchemeInstantiation -> CapabilityMethodKey -> ExpressionType -> InferState -> (MethodSelection, InferState)
instantiateQualifiedMethodTypeWithExplicitTarget instantiate methodKey explicitTarget state =
  let (selection, next) = instantiateMethod instantiate methodKey state
   in case selectedMethodEvidence selection of
        PendingEvidence _ _ target : _ -> case unifyTypes target explicitTarget next of
          Just unified -> (selection {selectedMethodType = resolveType unified <$> selectedMethodType selection}, unified)
          Nothing -> rejected next
        _
          | isNothing (selectedMethodType selection) -> (selection, next)
          | otherwise -> rejected next
  where
    rejected next = (MethodSelection Nothing [], addTypeError next (mkNoMatchingQualifiedMethodBodyError methodKey [explicitTarget]))

addUnpreservedInferredMethodConstraintErrors ::
  SourceSpan ->
  Set InferenceVariable ->
  InferState ->
  InferState ->
  ExpressionType ->
  Set InferenceVariable ->
  InferState
addUnpreservedInferredMethodConstraintErrors spanValue environmentVariables statementStartState state statementResultType schemeVariables
  | inferErrorCount state > inferErrorCount statementStartState = state
  | otherwise = foldl' check state droppedConstraints
  where
    droppedConstraints =
      dedupeTypeSchemeConstraints
        [ constraint
        | constraint@(TypeSchemeInferredConstraint _ argumentType) <- newInferredClassConstraints statementStartState state,
          not (inferredConstraintTargetPreserved state schemeVariables argumentType),
          not (inferredConstraintTargetStillVisibleInEnv state environmentVariables argumentType),
          inferredConstraintTargetConcrete state argumentType
            || (inferErrorCount state == inferErrorCount statementStartState && inferredConstraintTargetEscapesResult state statementResultType argumentType)
        ]
    check current constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        current
        (resolveDeferredExplicitConstraint current (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint))

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

inferredConstraintTargetStillVisibleInEnv :: InferState -> Set InferenceVariable -> ExpressionType -> Bool
inferredConstraintTargetStillVisibleInEnv state environmentVariables argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` environmentVariables

inferredConstraintTargetEscapesResult :: InferState -> ExpressionType -> ExpressionType -> Bool
inferredConstraintTargetEscapesResult state statementResultType argumentType =
  let targetVariables = freeTypeVariables (resolveType state argumentType)
      resultVariables = freeTypeVariables (resolveType state statementResultType)
   in not (Set.null targetVariables)
        && not (Set.null (Set.intersection targetVariables resultVariables))

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
    resolveWithContext before constraint
      | inferErrorCount before > inferErrorCount statementStartState = before
      | otherwise =
          annotateNewErrorsWithContext
            (SatisfyingConstraint (renderCapabilityId (deferredConstraintName constraint)))
            spanValue
            before
            (resolveDeferredExplicitConstraintWithEntailments entailingConstraints before constraint)
    priorConstraintCount = inferDeferredExplicitConstraintCount statementStartState
    currentConstraints = outputDeferredConstraints (inferOutput state)
    priorConstraints = Seq.take priorConstraintCount currentConstraints
    statementConstraints = toList (Seq.drop priorConstraintCount currentConstraints)
    stateWithoutStatementConstraints =
      modifyInferenceOutput
        ( \output ->
            output
              { outputDeferredConstraints = priorConstraints
              }
        )
        state

superclassPath :: ScopeCapabilityFacts -> CapabilityId -> CapabilityId -> Maybe [CapabilityId]
superclassPath facts from to = visit Set.empty from
  where
    visit seen current
      | current == to = Just []
      | Set.member current seen = Nothing
      | otherwise =
          foldr
            (<|>)
            Nothing
            [ (parent :) <$> visit (Set.insert current seen) parent
            | definition <- maybe [] pure (Map.lookup current (scopeClassFacts facts)),
              parent <- classSuperclasses definition
            ]

constraintIdentity :: TypeSchemeConstraint -> (CapabilityId, ExpressionType)
constraintIdentity constraint = case constraint of
  TypeSchemeConstraint capability target -> (capability, target)
  TypeSchemeInferredConstraint capability target -> (capability, target)
  TypeSchemeMethodConstraint capability _ target -> (capability, target)

deferredConstraintIsEntailed :: InferState -> [TypeSchemeConstraint] -> DeferredExplicitConstraint -> Bool
deferredConstraintIsEntailed state assumptions constraint = any entails assumptions
  where
    entails assumption =
      let (capability, target) = constraintIdentity assumption
       in resolveType state target == resolveType state (deferredArgumentType constraint)
            && not (Set.null (freeTypeVariables (resolveType state target)))
            && isJust (superclassPath (deferredVisibleFacts constraint) capability (deferredConstraintName constraint))

resolveDeferredExplicitConstraint :: InferState -> DeferredExplicitConstraint -> InferState
resolveDeferredExplicitConstraint = resolveDeferredExplicitConstraintWithEntailments []

resolveDeferredExplicitConstraintWithEntailments :: [TypeSchemeConstraint] -> InferState -> DeferredExplicitConstraint -> InferState
resolveDeferredExplicitConstraintWithEntailments assumptions state constraint
  | Map.member request (outputEvidence (inferOutput state)) = state
  | otherwise = case resolution of
      Right (evidence, next) -> remember evidence next
      Left diagnostic
        | deferredWasInferred constraint && inferredEqualityConstraintCanUseStructuralRuntimeEquality state (deferredStructuralFacts constraint) (deferredMethodKey constraint) capability (resolveType state argument) -> state
        | otherwise -> remember request (addTypeError state (if isNothing member && deferredWasInferred constraint && not (Set.null (freeTypeVariables (resolveType state argument))) then mkAmbiguousDeferredConstraintError True capability (resolveType state argument) else diagnostic))
  where
    resolution
      | deferredConstraintIsEntailed state assumptions constraint = Right (PendingEvidence capability member (resolveType state argument), state)
      | otherwise = resolveCapabilityEvidence assumptions facts capability member argument state
    facts = deferredVisibleFacts constraint
    capability = deferredConstraintName constraint
    member = snd <$> deferredMethodKey constraint
    argument = deferredArgumentType constraint
    request = PendingEvidence capability member argument
    remember evidence = modifyInferenceOutput (\output -> output {outputEvidence = Map.insert request evidence (outputEvidence output)})

-- Every candidate starts from the same immutable state. Only the uniquely
-- selected head's substitutions and prerequisite checks may continue.
resolveCapabilityEvidence :: [TypeSchemeConstraint] -> ScopeCapabilityFacts -> CapabilityId -> Maybe Identifier -> ExpressionType -> InferState -> Either Diagnostic (EvidenceReference, InferState)
resolveCapabilityEvidence assumptions facts capability member argument state
  | any entails assumptions = Right (PendingEvidence capability member target, state)
  | SemanticVariable variable <- fst (semanticApplicationSpine target), isNothing (integerLiteralRangeFor state (SemanticVariable variable)) = Left ambiguous
  | otherwise = case preferred of
      [] | not (Set.null unresolvedTargetVariables) -> Left ambiguous
      [] -> Left (mkMissingExplicitConstraintImplFactError (renderCapabilityId capability <> "(" <> renderSemanticTypeWith (const "_") (defaultLiteralTypes state target) <> ")"))
      [(template, headTarget, substitution, prerequisites, matched)] -> do
        selectedMethod <- case member of
          Nothing -> Right Nothing
          Just method -> maybe (Left (mkMissingImplMethodBodyError (capability, method))) (Right . Just) (Map.lookup method (implementationMethods template))
        let superclasses = maybe [] classSuperclasses (Map.lookup capability (scopeClassFacts facts))
            requirements = prerequisites <> [TypeSchemeConstraint superclass headTarget | superclass <- superclasses]
        (arguments, checked) <- foldl' solvePrerequisite (Right ([], matched)) requirements
        Right (EvidenceReference capability (implementationIdentity template) selectedMethod (resolveType checked headTarget) (fmap (resolveType checked) substitution) arguments, checked)
      _ -> Left ambiguous
  where
    target = resolveType state argument
    unresolvedTargetVariables = Set.filter (\variable -> isNothing (integerLiteralRangeFor state (SemanticVariable variable))) (freeTypeVariables target)
    entails assumption =
      let (owner, entailedTarget) = constraintIdentity assumption
       in not (Set.null (freeTypeVariables target)) && resolveType state entailedTarget == target && isJust (superclassPath facts owner capability)
    ambiguous = case member of
      Just method -> mkAmbiguousQualifiedMethodBodyError (capability, method)
      Nothing -> mkAmbiguousDeferredConstraintError False capability target
    templates = filter ((== capability) . implementationCapability) (Map.elems (scopeImplementations facts))
    candidates candidateTarget exact =
      [ (template, headTarget, substitution, prerequisites, matched)
      | template <- templates,
        Just ((headTarget, substitution, prerequisites), matched) <- [Trial.runStateT (trial candidateTarget exact template) state]
      ]
    trial candidateTarget exact template = do
      Trial.modify' (\current -> current {inferSolver = (inferSolver current) {solverRigidTypeVars = inferRigidTypeVars current <> unresolvedTargetVariables}})
      (headTarget, substitution, prerequisites) <- freshImplementation template
      Trial.StateT (fmap ((),) . (if exact then unifyTypesExactly else unifyTypes) headTarget candidateTarget)
      Trial.StateT (fmap ((),) . unifyTypes headTarget target)
      Trial.modify' (\current -> current {inferSolver = (inferSolver current) {solverRigidTypeVars = inferRigidTypeVars state}})
      pure (headTarget, substitution, prerequisites)
    preferred = case candidates (defaultLiteralTypes state target) True of
      [] -> case candidates target True of [] -> candidates target False; exact -> exact
      exact -> exact
    solvePrerequisite previous constraint = do
      (evidence, before) <- previous
      let (owner, required) = constraintIdentity constraint
      (selected, after) <- resolveCapabilityEvidence assumptions facts owner Nothing required before
      Right (evidence <> [selected], after)

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
    && Map.member constraintName (scopeClassFacts facts)

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

defaultLiteralTypes :: InferState -> ExpressionType -> ExpressionType
defaultLiteralTypes state =
  defaultLiteralTypesWith state SemanticInt

defaultBindingLiteralTypes :: InferState -> ExpressionType -> ExpressionType
defaultBindingLiteralTypes state =
  defaultLiteralTypesWith state (SemanticNumeric NumericInt64)

defaultLiteralTypesWith :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
defaultLiteralTypesWith state integerLiteralDefault =
  substituteSemanticVariables defaultVariable
  where
    defaultVariable variable
      | isJust (integerLiteralRangeFor state (SemanticVariable variable)) = integerLiteralDefault
      | otherwise = SemanticVariable variable

addInferredConstraint :: TypeSchemeConstraint -> InferState -> InferState
addInferredConstraint constraint state =
  modifyInferenceOutput
    ( \output ->
        output
          { outputInferredConstraints =
              constraint : outputInferredConstraints output,
            outputInferredConstraintCount = outputInferredConstraintCount output + 1
          }
    )
    state

addInferredEqualityClassConstraintIfVisible :: ExpressionType -> InferState -> InferState
addInferredEqualityClassConstraintIfVisible argumentType state =
  case activeEqualityClassName state of
    Just equalityClassName -> addInferredConstraint (TypeSchemeInferredConstraint equalityClassName argumentType) state
    Nothing -> state

activeEqualityClassName :: InferState -> Maybe CapabilityId
activeEqualityClassName state =
  case filter (unqualifiedEqualityClass . fst) classes of
    (capability, _) : _ -> Just capability
    _ -> case filter importedEqualityClass classes of
      [(capability, _)] -> Just capability
      _ -> Nothing
  where
    classes = Map.toList (inferClassFacts state)
    unqualifiedEqualityClass (CapabilityId name) = case name of
      UserName (ResolvedUserName ImportedModule {} _ _) -> False
      _ -> identifierText name == "Equatable"
    importedEqualityClass (CapabilityId (UserName (ResolvedUserName ImportedModule {} _ member)), _) =
      identifierText member == "Equatable"
    importedEqualityClass _ = False

typeSchemePrimitiveConstraints :: InferState -> Set InferenceVariable -> [TypeSchemePrimitiveConstraint]
typeSchemePrimitiveConstraints state schemeVariables =
  numericConstraints ++ equalityConstraints
  where
    targetTypeFor typeVar =
      let targetType = resolveType state (SemanticVariable typeVar)
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

    numericConstraints =
      [ TypeSchemeNumericConstraint numericConstraint targetType
      | (typeVar, numericConstraint) <- Map.toList (inferNumericVars state),
        Just targetType <- [targetTypeFor typeVar]
      ]

    equalityConstraints =
      [ TypeSchemeStrictEqualityConstraint targetType
      | typeVar <- Set.toList (inferStrictEqualityVars state),
        Just targetType <- [targetTypeFor typeVar]
      ]

-- The shared implementation/default checker must not silently strengthen a
-- universally quantified method with numeric or structural-equality promises.
checkMethodPrimitiveConstraints :: Text -> SourceSpan -> Set InferenceVariable -> [TypeSchemeConstraint] -> InferState -> InferState -> InferState
checkMethodPrimitiveConstraints method spanValue variables assumptions before after
  | inferErrorCount after > inferErrorCount before = after
  | otherwise = foldl' check after (typeSchemePrimitiveConstraints after variables)
  where
    facts = capabilityFactsFromState after
    check current requirement =
      let (label, target) = case requirement of
            TypeSchemeNumericConstraint _ argument -> ("Num", argument)
            TypeSchemeStrictEqualityConstraint argument -> ("Equatable", argument)
          owners = [capability | capability@(CapabilityId name) <- Map.keys (scopeClassFacts facts), identifierText name == label]
          entailed capability = case resolveCapabilityEvidence assumptions facts capability Nothing target current of
            Right _ -> True
            Left _ -> False
       in if any entailed owners
            then current
            else addTypeError current (mkUndeclaredSignatureConstraintError method True label target spanValue)
