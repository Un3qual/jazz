{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Scope, binding, signature, and constructor inference. The resulting
-- semantic facts feed the analyzed nodes used by execution.
module Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferNestedScopeTypeWithMode,
    inferScopeType,
    inferScopeTypeWithMode,
    instantiateNonBuiltinTypeBinding,
  )
where

import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.List
  ( uncons,
    unsnoc,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
    isNothing,
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import Data.Void (Void, absurd)
import Jazz.Compiler.AST
  ( ClassMethodSignature (..),
    CoreNode (coreNodeFacts, coreNodeId, coreNodeSpan),
    CorePhase (..),
    CoreSort (StatementSort),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    SignatureType,
    Statement (..),
    expressionNode,
    statementNode,
  )
import Jazz.Compiler.BuiltinCatalog
  ( lookupKernelBuiltinSymbol,
    numericTypeFloatMax,
  )
import Jazz.Compiler.CapabilityFacts
  ( constraintSignatureTypeVariableNamesInOrder,
    signaturePayloadConstraintType,
  )
import Jazz.Compiler.CoreIdentity (CoreBinderId, ResolvedNodeFacts (..), ResolvedReference (..), ResolvedScopeFacts (..), renderCapabilityId, resolvedNodeImportTarget, resolvedValueReference)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticContext (CheckingBinding),
    SourceSpan,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.ModuleIdentity (sourceUnitOwnerModulePath)
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierText,
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    preparedRecursiveScopeBindingNames,
    preparedRecursiveScopeFacts,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeStatements,
    resolvedExpressionReferences,
  )
import Jazz.Compiler.SemanticDeclarations (normalizeSignatureType, prepareDataTypeKinds)
import Jazz.Compiler.SemanticFacts
  ( SemanticFactInvariantFailure (..),
    StatementDeclarationFact (..),
  )
import Jazz.Compiler.TypeInference.Analyzed (ExpressionDecision (..), constrainBindingRuntimeResult, draftDecidedExpressionNode, draftExpressionNode, draftStatementNode, noExpressionDecision, projectAnalyzedMethodSignature)
import Jazz.Compiler.TypeInference.Capabilities
  ( MethodSelection (..),
    TypeEnvFreeVariables,
    addUnpreservedInferredMethodConstraintErrors,
    capabilityFactsFromState,
    defaultBindingLiteralTypes,
    defaultLiteralTypes,
    deleteTypeEnvFreeVariables,
    enterModuleCapabilityScope,
    finalizeDeferredExplicitConstraintsAt,
    finalizeDeferredExplicitConstraintsAtWithEntailments,
    flushCurrentModuleCapabilityFacts,
    freeTypeVariablesInEnv,
    importModuleCapabilityFacts,
    insertTypeEnvFreeVariables,
    instantiateQualifiedMethodTypeWithExpected,
    newInferredClassConstraints,
    registerClassCapabilityFacts,
    registerImplementation,
    resolveTypeEnvFreeVariables,
    resolveTypeSchemeConstraint,
    restoreCapabilityFacts,
    typeEnvFreeVariables,
    typeSchemeDefiningFactsFromState,
    updateRootModuleBaselineFacts,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithContext,
    annotateNewErrorsWithPrimarySpan,
    mkBindingTypeMismatchError,
    mkDuplicateDataTypeDeclarationError,
    mkInvalidConstructorPayloadTypeError,
    mkInvalidImplTargetError,
    mkInvalidQualifiedMethodSignatureError,
    mkInvalidSignatureTypeError,
    mkMethodLocalTypeVariableError,
    mkSignatureTypeMismatchError,
    mkUndeclaredSignatureConstraintError,
    mkUnknownConstructorPayloadTypeError,
    targetedFloatLiteralDiagnostic,
  )
import Jazz.Compiler.TypeInference.Draft (CheckedExpr (..), CheckedScope (..), Draft, rejectedDraft)
import Jazz.Compiler.TypeInference.Environment (insertResolvedTypeBinding, insertResolvedTypeEnvFreeVariables)
import Jazz.Compiler.TypeInference.ImplChecking (checkImplMethodBodies)
import Jazz.Compiler.TypeInference.Instantiation
  ( inferExplicitTypeApplication,
    instantiateNonBuiltinTypeBinding,
    typeBindingScheme,
  )
import Jazz.Compiler.TypeInference.Operator (builtinOperatorSymbolExpr)
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVar,
    integerLiteralRangeFitsNumericType,
    integerLiteralRangeFor,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    SolverState (..),
    inferDataTypes,
    inferErrorCount,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferNumericVars,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    modifyDeclarationState,
    modifyInferenceOutput,
    previewInference,
  )
import Jazz.Compiler.TypeInference.Traversal
  ( InferExprWithModeFn,
    InferenceMode (..),
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    InferenceVariable (..),
    NumericConstraint,
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts,
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    TypeEnvKey (..),
    TypeScheme,
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    typeEnvBindingKey,
    typeEnvReferenceKey,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
    pattern SignatureType,
  )

inferExprTypeWithExpectedMode ::
  InferExprWithModeFn ->
  InferenceMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr 'Resolved ->
  (CheckedExpr, InferState)
inferExprTypeWithExpectedMode inferExpression mode env state expectedType expr =
  case (resolveType state expectedType, expr) of
    (_, EVar node name)
      | Map.notMember (typeEnvReferenceKey (coreNodeFacts node) name) env,
        Just (selection, checkedState) <- instantiateQualifiedMethodTypeWithExpected (resolvedValueReference (coreNodeFacts node) name) expectedType state ->
          let result = selectedMethodType selection
              decision = noExpressionDecision {decisionEvidence = selectedMethodEvidence selection}
           in (CheckedExpr result (EVar <$> draftDecidedExpressionNode decision result expr <*> pure name), checkedState)
    (SemanticFunction argumentType resultType, ELambda node parameterName bodyExpr) ->
      let extendedEnv = insertResolvedTypeBinding (coreNodeFacts node) parameterName (PlainTypeBinding argumentType) env
          (bodyCheck, afterBody) = inferExprTypeWithExpectedMode inferExpression mode extendedEnv state resultType bodyExpr
          bodyType = case mode of
            InferenceOnly -> checkedExprType bodyCheck
            InferConcreteFunctions -> Just (fromMaybe resultType (checkedExprType bodyCheck))
          result = SemanticFunction (resolveType afterBody argumentType) <$> bodyType
       in finish result afterBody (\facts -> ELambda <$> facts <*> pure parameterName <*> checkedExprTree bodyCheck)
    (SemanticNumeric _, literal@(ELit _ LInt {}))
      | mode == InferConcreteFunctions ->
          let (checked, next) = inferExpression mode env state literal
           in case checkedExprType checked of
                Just literalType
                  | Just unified <- unifyTypes expectedType literalType next ->
                      (checked {checkedExprType = Just (resolveType unified expectedType)}, unified)
                _ -> (checked, next)
    (SemanticNumeric numericType, ELit _ literal@(LFloat literalValue literalSource Nothing))
      | Just _ <- numericTypeFloatMax numericType ->
          let checkedState = maybe state (addTypeError state) (targetedFloatLiteralDiagnostic numericType literalValue literalSource)
           in finish (Just (SemanticNumeric numericType)) checkedState (\facts -> ELit <$> facts <*> pure literal)
    _ ->
      let (checked, next) = inferExpression mode env state expr
       in case checkedExprType checked of
            Just inferredType
              | mode == InferConcreteFunctions,
                Just unified <- unifyTypes expectedType inferredType next ->
                  (checked {checkedExprType = specializeExpectedType unified expectedType <$> checkedExprType checked}, unified)
            _ -> (checked, next)
  where
    finish result checkedState build =
      ( CheckedExpr result (build (draftExpressionNode result expr)),
        checkedState
      )

checkImplementationTargets :: InferState -> SourceSpan -> [SignatureType 'Resolved] -> Either Diagnostic [SemanticType ResolvedName Void]
checkImplementationTargets state implSpan =
  traverse (first (mkInvalidImplTargetError implSpan) . normalizeSignatureType (inferDataTypes state) Map.empty)

type CheckedClassMethod = (CoreNode 'Resolved 'StatementSort, ResolvedName, ClassMethodType)

checkClassMethods :: InferState -> ResolvedName -> [ResolvedName] -> [ClassMethodSignature 'Resolved] -> Either Diagnostic [CheckedClassMethod]
checkClassMethods state capabilityName parameters = traverse checkMethod
  where
    parameterNames = map identifierText parameters
    variables = Map.fromList [(parameter, SemanticVariable parameter) | parameter <- parameterNames]
    classParameter = case parameterNames of
      [parameter] -> parameter
      _ -> ""
    checkMethod (ClassMethodSignature node methodName payload) =
      let methodSpan = coreNodeSpan node
          methodKey = identifierText capabilityName <> "::" <> identifierText methodName
          methodVariables = maybe [] constraintSignatureTypeVariableNamesInOrder (signaturePayloadConstraintType payload)
          methodLocalVariables = filter (`Map.notMember` variables) methodVariables
          invalid = mkInvalidSignatureTypeError state methodKey methodSpan payload
          normalize signature = case normalizeSignatureType (inferDataTypes state) variables signature of
            Right methodType -> Right (node, methodName, ClassMethodType classParameter methodType)
            Left _ -> Left invalid
       in case payload of
            ConstrainedSignature (_ : _) _ -> Left (setDiagnosticPrimarySpan methodSpan (mkInvalidQualifiedMethodSignatureError methodKey payload))
            _ -> case methodLocalVariables of
              variable : _ -> Left (mkMethodLocalTypeVariableError methodKey variable methodSpan)
              [] -> case payload of
                SignatureType signature -> normalize signature
                ConstrainedSignature [] signature -> normalize signature
                _ -> Left invalid

registerClassDeclaration :: InferState -> ResolvedName -> [ResolvedName] -> [CheckedClassMethod] -> InferState
registerClassDeclaration state capabilityName parameters checkedMethods =
  registerClassCapabilityFacts capabilityName (length parameters) unaryMethods state
  where
    unaryMethods = if length parameters == 1 then [(name, methodType) | (_, name, methodType) <- checkedMethods] else []

publishVisibleTypes :: TypeEnv -> InferState -> InferState
publishVisibleTypes env state =
  state
    { inferModule =
        (inferModule state) {inferenceVisibleTypes = env}
    }

inferScopeTypeWithMode :: InferExprWithModeFn -> InferenceMode -> TypeEnv -> InferState -> PreparedRecursiveScope 'Resolved -> (CheckedScope, InferState)
inferScopeTypeWithMode inferExpression mode initialEnv initialState preparedScope =
  preparedScope `seq`
    inferScopeTypeInternal
      ScopeInferenceRequest
        { scopeForwardSignedFunctionsPolicy = PermitForwardSignedFunctions,
          scopeInferExpression = inferExpression,
          scopeInferenceMode = mode,
          scopeInitialEnv = initialEnv,
          scopeInitialState = initialState,
          scopePreparedInference = preparedScope
        }

inferNestedScopeTypeWithMode :: InferExprWithModeFn -> InferenceMode -> TypeEnv -> InferState -> PreparedRecursiveScope 'Resolved -> (CheckedScope, InferState)
inferNestedScopeTypeWithMode inferExpression mode initialEnv initialState preparedScope =
  inferScopeTypeInternal
    ScopeInferenceRequest
      { scopeForwardSignedFunctionsPolicy = ForbidForwardSignedFunctions,
        scopeInferExpression = inferExpression,
        scopeInferenceMode = mode,
        scopeInitialEnv = initialEnv,
        scopeInitialState = initialState,
        scopePreparedInference = preparedScope
      }

inferScopeType :: InferExprWithModeFn -> TypeEnv -> InferState -> PreparedRecursiveScope 'Resolved -> (CheckedScope, InferState)
inferScopeType inferExpression initialEnv initialState preparedScope =
  let (inferredResult, finalState) =
        inferNestedScopeTypeWithMode
          inferExpression
          InferenceOnly
          initialEnv
          initialState
          preparedScope
   in (inferredResult, finalState)

data ForwardSignedFunctionsPolicy
  = ForbidForwardSignedFunctions
  | PermitForwardSignedFunctions

forwardSignedFunctionsPermitted :: ForwardSignedFunctionsPolicy -> Bool
forwardSignedFunctionsPermitted policy =
  case policy of
    ForbidForwardSignedFunctions -> False
    PermitForwardSignedFunctions -> True

data ScopeInferenceRequest = ScopeInferenceRequest
  { scopeForwardSignedFunctionsPolicy :: ForwardSignedFunctionsPolicy,
    scopeInferExpression :: InferExprWithModeFn,
    scopeInferenceMode :: InferenceMode,
    scopeInitialEnv :: TypeEnv,
    scopeInitialState :: InferState,
    scopePreparedInference :: PreparedRecursiveScope 'Resolved
  }

data ScopeWalkState = ScopeWalkState
  { scopeWalkEnv :: !TypeEnv,
    scopeWalkEnvFreeVariables :: !TypeEnvFreeVariables,
    scopeWalkLastExprType :: !(Maybe ExpressionType),
    scopeWalkStatements :: !(Map Int (Draft (Statement 'Analyzed))),
    scopeWalkPendingValues :: !(Map Int CheckedExpr),
    scopeWalkPendingSignature :: !(Maybe PendingSignatureType),
    scopeWalkPendingSignaturesByStatement :: !(Map Int PendingSignatureType),
    scopeWalkRecursiveGroupStartStates :: !(Map Int InferState),
    scopeWalkRecursiveGroupPreviewCache :: !RecursiveGroupPreviewCache,
    scopeWalkModuleBaselineFacts :: !ScopeCapabilityFacts,
    scopeWalkInferState :: !InferState
  }

inferScopeTypeInternal :: ScopeInferenceRequest -> (CheckedScope, InferState)
inferScopeTypeInternal
  ScopeInferenceRequest
    { scopeForwardSignedFunctionsPolicy,
      scopeInferExpression,
      scopeInferenceMode,
      scopeInitialEnv,
      scopeInitialState,
      scopePreparedInference = preparedScope
    } =
    let initialWalkState =
          ScopeWalkState
            { scopeWalkEnv = initialEnv,
              scopeWalkEnvFreeVariables = typeEnvFreeVariables initialEnv,
              scopeWalkLastExprType = Nothing,
              scopeWalkStatements = Map.empty,
              scopeWalkPendingValues = Map.empty,
              scopeWalkPendingSignature = Nothing,
              scopeWalkPendingSignaturesByStatement = Map.empty,
              scopeWalkRecursiveGroupStartStates = Map.empty,
              scopeWalkRecursiveGroupPreviewCache = Map.empty,
              scopeWalkModuleBaselineFacts = initialModuleBaselineFacts,
              scopeWalkInferState = stateAfterBindingSeeds
            }
        (scopeType, finalState) =
          go initialWalkState indexedStatements
        stateWithPublishedModuleFacts = flushCurrentModuleCapabilityFacts finalState
     in ( scopeType,
          restoreCapabilityFacts initialState stateWithPublishedModuleFacts
        )
    where
      statements = preparedRecursiveScopeStatements preparedScope
      bindingNamesByStatement = preparedRecursiveScopeBindingNames preparedScope
      recursiveGroupsByStatement = preparedRecursiveScopeGroups preparedScope
      lexicalFacts = preparedRecursiveScopeFacts preparedScope
      bindingKeysByStatement = Map.fromList [(index, typeEnvBindingKey (coreNodeFacts node) name) | (index, SLet node name _) <- indexedStatements]
      inferExpression = scopeInferExpression
      mode = scopeInferenceMode
      initialEnv = scopeInitialEnv
      initialState = scopeInitialState

      statementFactVisibleTypes :: InferState -> Maybe ExpressionType -> TypeEnv -> Statement 'Resolved -> TypeEnv
      statementFactVisibleTypes state valueType visibleTypes statement =
        case statement of
          SLet node name _ ->
            Map.adjust (semanticFactBinding state valueType) (typeEnvBindingKey (coreNodeFacts node) name) visibleTypes
          _ -> visibleTypes

      semanticFactBinding :: InferState -> Maybe ExpressionType -> TypeBinding -> TypeBinding
      semanticFactBinding state valueType binding =
        case binding of
          BuiltinAliasTypeBinding {} -> generalizedAliasBinding
          BuiltinOperatorAliasTypeBinding {} -> generalizedAliasBinding
          _ -> binding
        where
          generalizedAliasBinding =
            case valueType of
              Nothing -> binding
              Just inferredType ->
                let resolvedType = resolveType state inferredType
                    schemeVariables = freeTypeVariables resolvedType
                    inferredClassConstraints = typeSchemeInferredClassConstraints state schemeVariables
                 in SchemeTypeBinding
                      (bindingTypeScheme state (expressionTypeVariableOrder resolvedType) schemeVariables inferredClassConstraints resolvedType)

      buildStatement :: Int -> TypeEnv -> Maybe CheckedExpr -> [(Int, CheckedExpr)] -> Statement 'Resolved -> Draft (Statement 'Analyzed)
      buildStatement index visibleTypes body methods statement = case statement of
        SLet node name value -> makeLet name <$> facts node (bindingFor node name) (ValueDeclaration name) <*> valueDraft value body
        SSignature node name signature -> SSignature <$> facts node (bindingAt (resolvedNodeReference (coreNodeFacts node)) name) (SignatureDeclaration name) <*> pure name <*> pure signature
        SData node name parameters constructors -> SData <$> facts node Nothing (DataDeclaration name [constructorName | DataConstructor _ constructorName _ <- constructors]) <*> pure name <*> pure parameters <*> traverse constructor constructors
        SClass node name parameters signatures ->
          SClass <$> facts node Nothing (CapabilityDeclaration name parameters) <*> pure name <*> pure parameters <*> case Map.lookup index (preparedDeclarations scopePreparation) of
            Just (Right (PreparedClassMethods checkedMethods)) -> zipWithM classMethod signatures checkedMethods
            _ -> rejectedDraft (MissingStatementFacts (coreNodeId node))
        SImpl node name arguments declarations -> SImpl <$> implementationNode node name <*> pure name <*> pure arguments <*> traverse implMethod (zip [0 ..] declarations)
        SModule node path -> SModule <$> facts node Nothing (ModuleDeclaration (sourceUnitOwnerModulePath (resolvedNodeOwner (coreNodeFacts node)))) <*> pure path
        SImport node path alias names -> case resolvedNodeImportTarget (coreNodeFacts node) of
          Just target -> SImport <$> facts node Nothing (ImportDeclaration target) <*> pure path <*> pure alias <*> pure names
          Nothing -> rejectedDraft (MissingStatementFacts (coreNodeId node))
        SExpr node value -> SExpr <$> facts node Nothing ExpressionDeclaration <*> valueDraft value body
        where
          facts = draftStatementNode
          bindingFor node name = bindingAt (LexicalReference <$> resolvedNodeBinder (coreNodeFacts node)) name
          bindingAt reference name = reference >>= (\target -> Map.lookup (TypeEnvKey target name) visibleTypes)
          valueDraft _ (Just value) = checkedExprTree value
          valueDraft value Nothing = rejectedDraft (MissingExpressionFacts (coreNodeId (expressionNode value)))
          makeLet name node value = SLet node name (constrainBindingRuntimeResult (coreNodeFacts node) value)
          constructor (DataConstructor node name arguments) = DataConstructor <$> facts node (bindingFor node name) (ValueDeclaration name) <*> pure name <*> pure arguments
          classMethod (ClassMethodSignature node name signature) (_, _, methodType) = case projectAnalyzedMethodSignature (identifierText name) methodType of
            Right analyzed -> ClassMethodSignature <$> facts node Nothing (MethodDeclaration name analyzed) <*> pure name <*> pure signature
            Left failure -> rejectedDraft failure
          implementationNode node name = case Map.lookup index (preparedDeclarations scopePreparation) of
            Just (Right (PreparedImplementationTargets targets)) -> facts node Nothing (ImplementationDeclaration name (map (fmap absurd) targets))
            _ -> rejectedDraft (MissingStatementFacts (coreNodeId node))
          implMethod (methodIndex, ImplMethod node name value) = ImplMethod <$> facts node (bindingFor node name) (ValueDeclaration name) <*> pure name <*> valueDraft value (lookup methodIndex methods)

      commitLetDrafts pendingSignatures checkedValues statementIndex visibleTypes state drafts =
        (foldl' retainDefinition drafts committedStatementIndices, committedStatementIndices)
        where
          committedStatementIndices =
            case Map.lookup statementIndex recursiveGroupsByStatement of
              Nothing -> [statementIndex]
              Just groupMembers
                | Just (_, lastMember) <- unsnoc groupMembers,
                  statementIndex == lastMember ->
                    groupMembers
                | otherwise -> []
          retainDefinition current definitionIndex =
            case Map.lookup definitionIndex statementsByIndex of
              Just definition@(SLet node name _) ->
                let key = typeEnvBindingKey (coreNodeFacts node) name
                    definitionVisibleTypes =
                      case Map.lookup definitionIndex pendingSignatures of
                        Nothing -> visibleTypes
                        Just pendingSignature ->
                          Map.insert
                            key
                            (generalizedExplicitSignatureBinding (freeTypeVariablesInEnv state (Map.delete key visibleTypes)) state pendingSignature)
                            visibleTypes
                    body = Map.lookup definitionIndex checkedValues
                    factTypes = statementFactVisibleTypes state (body >>= checkedExprType) definitionVisibleTypes definition
                    withDefinition = Map.insert definitionIndex (buildStatement definitionIndex factTypes body [] definition) current
                 in retainSignature definitionVisibleTypes withDefinition definitionIndex
              _ -> current
          retainSignature definitionVisibleTypes current definitionIndex
            | Map.member definitionIndex pendingSignatures =
                case Map.lookup (definitionIndex - 1) statementsByIndex of
                  Just signature@SSignature {} -> Map.insert (definitionIndex - 1) (buildStatement (definitionIndex - 1) definitionVisibleTypes Nothing [] signature) current
                  _ -> current
            | otherwise = current

      indexedStatements = zip [0 ..] statements
      recursiveGroups =
        Set.toList (Set.fromList (Map.elems recursiveGroupsByStatement))
      recursiveGroupIntervals =
        [ (firstMember, lastMember, Set.fromList groupMembers, groupMembers)
        | groupMembers <- recursiveGroups,
          Just (firstMember, _) <- [uncons groupMembers],
          Just (_, lastMember) <- [unsnoc groupMembers],
          firstMember < lastMember
        ]
      recursiveGroupsStartingAt =
        Map.fromList
          [ (firstMember, (groupMemberSet, groupMembers))
          | (firstMember, _, groupMemberSet, groupMembers) <- recursiveGroupIntervals
          ]
      recursiveGroupStartByLastMember =
        Map.fromList
          [ (lastMember, firstMember)
          | (firstMember, lastMember, _, _) <- recursiveGroupIntervals
          ]
      recursiveGroupSweepIndices =
        Set.toAscList
          ( Set.unions
              [ Map.keysSet bindingNamesByStatement,
                Map.keysSet recursiveGroupsStartingAt,
                Map.keysSet recursiveGroupStartByLastMember
              ]
          )
      recursiveGroupsByInterveningLet =
        snd
          ( foldl'
              indexInterveningRecursiveGroups
              (Map.empty, Map.empty)
              recursiveGroupSweepIndices
          )
      -- A group stops before its last member is observed and starts only after
      -- its first. Other group members are filtered while the interval is live.
      indexInterveningRecursiveGroups (activeGroups, groupsByStatement) statementIndex =
        (activeGroupsAfterStart, nextGroupsByStatement)
        where
          activeGroupsBeforeStart =
            case Map.lookup statementIndex recursiveGroupStartByLastMember of
              Just firstMember -> Map.delete firstMember activeGroups
              Nothing -> activeGroups
          interveningGroups =
            [ groupMembers
            | (groupMemberSet, groupMembers) <- Map.elems activeGroupsBeforeStart,
              Set.notMember statementIndex groupMemberSet
            ]
          nextGroupsByStatement
            | Map.member statementIndex bindingNamesByStatement,
              not (null interveningGroups) =
                Map.insert statementIndex interveningGroups groupsByStatement
            | otherwise = groupsByStatement
          activeGroupsAfterStart =
            case Map.lookup statementIndex recursiveGroupsStartingAt of
              Just groupInterval -> Map.insert statementIndex groupInterval activeGroupsBeforeStart
              Nothing -> activeGroupsBeforeStart
      previewGroupMemberIndices =
        Set.fromList
          [ memberIndex
          | groups <- Map.elems recursiveGroupsByInterveningLet,
            groupMembers <- groups,
            memberIndex <- groupMembers
          ]
      bindingIndexById = Map.fromList [(binder, index) | (index, binder) <- Map.toList (resolvedScopeBinderIds lexicalFacts)]
      previewGroupReferencesByStatement =
        Map.fromList
          [ (statementIndex, Map.keysSet (resolvedExpressionReferences valueExpr))
          | statementIndex <- Set.toList previewGroupMemberIndices,
            Just (SLet _ _ valueExpr) <- [Map.lookup statementIndex statementsByIndex]
          ]
      selfRecursiveFunctionStatements = resolvedScopeSelfRecursiveFunctions lexicalFacts
      selfRecursiveTypeStatements = resolvedScopeSelfReferences lexicalFacts
      signedBindingStatements = collectSignedBindingStatements indexedStatements
      statementsByIndex = Map.fromList indexedStatements
      predeclaredDataTypes = preparedDataTypes scopePreparation
      scopePreparation =
        prepareScope scopeForwardSignedFunctionsPolicy mode indexedStatements initialState
      bindingSeedsByStatement = preparedBindingSeeds scopePreparation
      preparedSignaturesByStatement = preparedSignatures scopePreparation
      forwardFunctionBindings = preparedForwardFunctions scopePreparation
      stateAfterBindingSeeds = preparedScopeState scopePreparation
      initialModuleBaselineFacts = capabilityFactsFromState initialState

      retainStatement index statement visibleTypes checked body methods walkState =
        walkState
          { scopeWalkStatements = Map.insert index (buildStatement index visibleTypes body methods statement) (scopeWalkStatements walkState),
            scopeWalkInferState = checked
          }

      go :: ScopeWalkState -> [(Int, Statement 'Resolved)] -> (CheckedScope, InferState)
      go walkState remainingStatements =
        case remainingStatements of
          [] ->
            let drafts = [Map.findWithDefault (rejectedDraft (MissingStatementFacts (coreNodeId (statementNode statement)))) index (scopeWalkStatements walkState) | (index, statement) <- indexedStatements]
             in (CheckedScope (scopeWalkLastExprType walkState) (sequenceA drafts), publishVisibleTypes (scopeWalkEnv walkState) (scopeWalkInferState walkState))
          (statementIndex, statement) : rest ->
            let env = scopeWalkEnv walkState
                envFreeVariables = scopeWalkEnvFreeVariables walkState
                pendingSignatureType = scopeWalkPendingSignature walkState
                pendingSignaturesByStatement = scopeWalkPendingSignaturesByStatement walkState
                recursiveGroupStartStates = scopeWalkRecursiveGroupStartStates walkState
                recursiveGroupPreviewCache = scopeWalkRecursiveGroupPreviewCache walkState
                moduleBaselineFacts = scopeWalkModuleBaselineFacts walkState
                state = scopeWalkInferState walkState
                stateForSource = state
             in case statement of
                  SModule moduleNode _ ->
                    let next = enterModuleCapabilityScope moduleBaselineFacts (sourceUnitOwnerModulePath (resolvedNodeOwner (coreNodeFacts moduleNode))) state
                     in go (retainStatement statementIndex statement env next Nothing [] walkState {scopeWalkRecursiveGroupPreviewCache = Map.empty}) rest
                  SImport importNode _ maybeAlias maybeSymbolNames ->
                    let next = maybe state (\target -> importModuleCapabilityFacts target maybeAlias maybeSymbolNames state) (resolvedNodeImportTarget (coreNodeFacts importNode))
                     in go (retainStatement statementIndex statement env next Nothing [] walkState {scopeWalkRecursiveGroupPreviewCache = Map.empty}) rest
                  SClass _ capabilityName parameters _ ->
                    let nextState = case Map.lookup statementIndex (preparedDeclarations scopePreparation) of
                          Just (Left diagnostic) -> addTypeError stateForSource diagnostic
                          Just (Right (PreparedClassMethods methods)) -> registerClassDeclaration stateForSource capabilityName parameters methods
                          _ -> stateForSource -- The owned declaration draft rejects missing preparation.
                        nextModuleBaselineFacts =
                          updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                        (scopeResultType, resultState) =
                          go
                            ( retainStatement
                                statementIndex
                                statement
                                env
                                nextState
                                Nothing
                                []
                                walkState
                                  { scopeWalkPendingSignature = Nothing,
                                    scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                    scopeWalkModuleBaselineFacts = nextModuleBaselineFacts
                                  }
                            )
                            rest
                     in (scopeResultType, resultState)
                  SImpl implNode capabilityName _ methods ->
                    let checkedTargets = case Map.lookup statementIndex (preparedDeclarations scopePreparation) of
                          Just (Left diagnostic) -> Just (Left diagnostic)
                          Just (Right (PreparedImplementationTargets targets)) -> Just (Right targets)
                          _ -> Nothing
                        (nextState, methodChecks) =
                          case checkedTargets of
                            Nothing -> (stateForSource, []) -- Rejected by the declaration draft.
                            Just (Left diagnostic) -> (addTypeError stateForSource diagnostic, [])
                            Just (Right targets) ->
                              let implSeededState = registerImplementation implNode capabilityName targets methods stateForSource
                               in checkImplMethodBodies
                                    (inferExprTypeWithExpectedMode inferExpression mode)
                                    checkedExprType
                                    env
                                    implSeededState
                                    capabilityName
                                    targets
                                    methods
                        nextModuleBaselineFacts =
                          updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                        (scopeResultType, resultState) =
                          go
                            ( retainStatement
                                statementIndex
                                statement
                                env
                                nextState
                                Nothing
                                methodChecks
                                walkState
                                  { scopeWalkPendingSignature = Nothing,
                                    scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                    scopeWalkModuleBaselineFacts = nextModuleBaselineFacts
                                  }
                            )
                            rest
                     in (scopeResultType, resultState)
                  SData dataNode typeName typeParameters constructors ->
                    let dataTypeAlreadyDeclared =
                          Map.member typeName (inferDataTypes state)
                        (nextEnv, nextState) = case Map.lookup statementIndex (preparedDeclarations scopePreparation) of
                          Just (Left diagnostic) -> (env, addTypeError state diagnostic)
                          Just (Right (PreparedDataType binding)) ->
                            registerDataConstructors (Map.insert typeName binding predeclaredDataTypes) (coreNodeSpan dataNode) typeName typeParameters constructors env state
                          _ -> registerDataConstructors predeclaredDataTypes (coreNodeSpan dataNode) typeName typeParameters constructors env state
                        nextEnvFreeVariables =
                          if dataTypeAlreadyDeclared
                            then envFreeVariables
                            else
                              foldl'
                                (insertRegisteredConstructorFreeVariables nextEnv)
                                envFreeVariables
                                constructors
                        (scopeResultType, resultState) =
                          go
                            ( retainStatement
                                statementIndex
                                statement
                                nextEnv
                                nextState
                                Nothing
                                []
                                walkState
                                  { scopeWalkEnv = nextEnv,
                                    scopeWalkEnvFreeVariables = nextEnvFreeVariables,
                                    scopeWalkPendingSignature = Nothing,
                                    scopeWalkRecursiveGroupPreviewCache = Map.empty
                                  }
                            )
                            rest
                     in (scopeResultType, resultState)
                  SSignature signatureNode name signaturePayload ->
                    let (nextPendingSignature, nextState) =
                          case Map.lookup statementIndex preparedSignaturesByStatement of
                            Just (PreparedSignature (Just pendingSignature) _) ->
                              (Just pendingSignature, state)
                            _ ->
                              ( Nothing,
                                addTypeError
                                  state
                                  (mkInvalidSignatureTypeError signatureState (identifierText name) (coreNodeSpan signatureNode) signaturePayload)
                              )
                        signatureState = state
                        (scopeResultType, resultState) =
                          go
                            walkState
                              { scopeWalkPendingSignature = nextPendingSignature,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkStatements = case nextPendingSignature of
                                  Nothing -> Map.insert statementIndex (buildStatement statementIndex env Nothing [] statement) (scopeWalkStatements walkState)
                                  Just _ -> scopeWalkStatements walkState,
                                scopeWalkInferState = nextState
                              }
                            rest
                     in (scopeResultType, resultState)
                  SLet bindingNode name valueExpr ->
                    let key = typeEnvBindingKey (coreNodeFacts bindingNode) name
                        nameText = identifierText name
                        bindingSpan = coreNodeSpan bindingNode
                        (envForStatement, stateForStatement, recursiveGroupPreviewCacheForStatement) =
                          exposeVisibleRecursiveGroupSchemes statementIndex env envFreeVariables stateForSource recursiveGroupPreviewCache
                        recursiveGroupStartStatesForStatement =
                          rememberRecursiveGroupStart statementIndex stateForStatement recursiveGroupStartStates
                        matchingPendingSignature =
                          case pendingSignatureType of
                            Just pendingSignature
                              | pendingSignatureName pendingSignature == nameText ->
                                  Just pendingSignature
                            _ -> Nothing
                        envWithRecursiveBindings =
                          recursiveBindingEnv
                            statementIndex
                            envForStatement
                            recursiveGroupsByStatement
                            bindingKeysByStatement
                            bindingSeedsByStatement
                        envWithBindingSeed =
                          case ( shouldSeedSelfRecursiveBinding statementIndex key envForStatement,
                                 Map.lookup statementIndex bindingSeedsByStatement
                               ) of
                            (True, Just bindingSeed) ->
                              Map.insert key (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                            _ -> envWithRecursiveBindings
                        envWithForwardSignedBindings =
                          case Map.lookup statementIndex forwardFunctionBindings of
                            Nothing -> envWithBindingSeed
                            Just _ ->
                              foldl'
                                ( \currentEnv (forwardStatementIndex, forwardBinding) ->
                                    case Map.lookup forwardStatementIndex bindingKeysByStatement of
                                      Just forwardKey
                                        | forwardStatementIndex > statementIndex ->
                                            Map.insertWith
                                              (\_ existing -> existing)
                                              forwardKey
                                              (PlainTypeBinding forwardBinding)
                                              currentEnv
                                      _ -> currentEnv
                                )
                                envWithBindingSeed
                                (Map.toAscList forwardFunctionBindings)
                        envWithPendingSignature =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              Map.insert
                                key
                                (PlainTypeBinding (pendingSignatureDeclaredType pendingSignature))
                                envWithForwardSignedBindings
                            Nothing -> envWithForwardSignedBindings
                        maybeExpectedValueType =
                          pendingSignatureDeclaredType <$> matchingPendingSignature
                        stateForSignatureCheck =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              setRigidTypeVariables
                                ( Set.union
                                    (inferRigidTypeVars stateForStatement)
                                    (Set.fromList (pendingSignatureVariableOrder pendingSignature))
                                )
                                stateForStatement
                            Nothing -> stateForStatement
                        (rawValueResult, rawStateAfterValue) =
                          case maybeExpectedValueType of
                            Just expectedValueType ->
                              inferExprTypeWithExpectedMode inferExpression mode envWithPendingSignature stateForSignatureCheck expectedValueType valueExpr
                            Nothing ->
                              inferExpression mode envWithPendingSignature stateForStatement valueExpr
                        rawValueType = checkedExprType rawValueResult
                        valueType =
                          targetedFractionalLiteralBindingType
                            nameText
                            matchingPendingSignature
                            valueExpr
                            rawValueType
                        stateAfterTargetedLiteralCheck =
                          case targetedFractionalLiteralDiagnostic nameText matchingPendingSignature valueExpr rawValueType of
                            Just diagnostic -> addTypeError rawStateAfterValue diagnostic
                            Nothing -> rawStateAfterValue
                        stateAfterValue =
                          annotateNewErrorsWithPrimarySpan bindingSpan stateForStatement stateAfterTargetedLiteralCheck
                        stateAfterBindingSeedCheck =
                          case (Map.lookup statementIndex bindingSeedsByStatement, valueType) of
                            (Just bindingSeed, Just inferredType) ->
                              case unifyTypes bindingSeed inferredType stateAfterValue of
                                Just unifiedState -> unifiedState
                                Nothing ->
                                  addTypeError
                                    stateAfterValue
                                    ( mkBindingTypeMismatchError
                                        nameText
                                        (resolveType stateAfterValue bindingSeed)
                                        bindingSpan
                                        (resolveType stateAfterValue inferredType)
                                    )
                            _ -> stateAfterValue
                        stateAfterSignatureCheck =
                          case (matchingPendingSignature, valueType) of
                            (Just pendingSignature, Just inferredType) ->
                              case unifyTypes
                                (pendingSignatureDeclaredType pendingSignature)
                                inferredType
                                stateAfterBindingSeedCheck of
                                Just unifiedState -> unifiedState
                                Nothing ->
                                  addTypeError
                                    stateAfterBindingSeedCheck
                                    ( mkSignatureTypeMismatchError
                                        nameText
                                        (pendingSignatureSpan pendingSignature)
                                        (resolveType stateAfterBindingSeedCheck (pendingSignatureDeclaredType pendingSignature))
                                        bindingSpan
                                        (defaultLiteralTypes stateAfterBindingSeedCheck (resolveType stateAfterBindingSeedCheck inferredType))
                                    )
                            _ -> stateAfterBindingSeedCheck
                        stateAfterExplicitConstraintCheck =
                          restoreRigidTypeVariables stateForStatement $
                            finalizeDeferredExplicitConstraintsAtWithEntailments
                              bindingSpan
                              (maybe [] pendingSignatureExplicitConstraints matchingPendingSignature)
                              stateForStatement
                              stateAfterSignatureCheck
                        stateAfterSignatureContractCheck =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              addUndeclaredSignatureConstraintErrors
                                nameText
                                stateForStatement
                                pendingSignature
                                stateAfterExplicitConstraintCheck
                            Nothing -> stateAfterExplicitConstraintCheck
                        nextBindingType =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              Just (resolveType stateAfterSignatureContractCheck (pendingSignatureDeclaredType pendingSignature))
                            _ ->
                              fmap
                                (bindingTypeForValue stateAfterSignatureContractCheck valueExpr)
                                (Map.lookup statementIndex bindingSeedsByStatement)
                        generalizationEnv =
                          generalizationEnvForStatement statementIndex envForStatement
                        generalizationEnvVariables =
                          if Map.notMember statementIndex recursiveGroupsByStatement
                            && Map.notMember statementIndex recursiveGroupsByInterveningLet
                            then resolveTypeEnvFreeVariables stateAfterSignatureContractCheck envFreeVariables
                            else freeTypeVariablesInEnv stateAfterSignatureContractCheck generalizationEnv
                        droppedInferredSchemeVariables =
                          case (matchingPendingSignature, nextBindingType) of
                            (Just pendingSignature, Just _)
                              | shouldGeneralizeExplicitSignatureBinding pendingSignature ->
                                  explicitBindingSchemeVariables generalizationEnvVariables stateAfterSignatureContractCheck pendingSignature
                            (_, Just inferredType)
                              | shouldGeneralizeOrdinaryBinding statementIndex generalizationEnv valueExpr matchingPendingSignature ->
                                  ordinaryBindingSchemeVariables generalizationEnvVariables stateAfterSignatureContractCheck valueExpr inferredType
                            _ -> Set.empty
                        stateAfterDroppedInferredMethodCheck =
                          case nextBindingType of
                            Just bindingType ->
                              addUnpreservedInferredMethodConstraintErrors
                                bindingSpan
                                generalizationEnvVariables
                                stateForStatement
                                stateAfterSignatureContractCheck
                                bindingType
                                droppedInferredSchemeVariables
                            Nothing -> stateAfterSignatureContractCheck
                        maybeNextBinding =
                          nextBindingForValue
                            statementIndex
                            envForStatement
                            generalizationEnvVariables
                            valueExpr
                            nextBindingType
                            matchingPendingSignature
                            stateAfterDroppedInferredMethodCheck
                        stateAfterCapturedConstraintPrune =
                          case maybeNextBinding of
                            Just binding ->
                              pruneCapturedInferredClassConstraints stateForStatement binding stateAfterDroppedInferredMethodCheck
                            Nothing -> stateAfterDroppedInferredMethodCheck
                        nextPendingSignaturesByStatement =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              Map.insert statementIndex pendingSignature pendingSignaturesByStatement
                            Nothing -> pendingSignaturesByStatement
                        nextEnvBeforeRecursiveGroupGeneralization =
                          case maybeNextBinding of
                            Just binding -> insertResolvedTypeBinding (coreNodeFacts bindingNode) name binding env
                            Nothing -> env
                        nextEnvFreeVariablesBeforeRecursiveGroupGeneralization =
                          case maybeNextBinding of
                            Just binding -> insertResolvedTypeEnvFreeVariables (coreNodeFacts bindingNode) name binding envFreeVariables
                            Nothing -> envFreeVariables
                        (nextEnv, stateAfterRecursiveGroupPrune) =
                          generalizeCompletedRecursiveGroup
                            nextPendingSignaturesByStatement
                            statementIndex
                            nextEnvBeforeRecursiveGroupGeneralization
                            recursiveGroupStartStatesForStatement
                            stateAfterCapturedConstraintPrune
                        nextEnvFreeVariables =
                          refreshCompletedRecursiveGroupFreeVariables
                            statementIndex
                            nextEnv
                            nextEnvFreeVariablesBeforeRecursiveGroupGeneralization
                        recursiveGroupPreviewCacheAfterStatement =
                          dropAdvancedRecursiveGroupPreview statementIndex recursiveGroupPreviewCacheForStatement
                        (committedDrafts, committedIndices) =
                          commitLetDrafts
                            nextPendingSignaturesByStatement
                            pendingValues
                            statementIndex
                            nextEnv
                            stateAfterRecursiveGroupPrune
                            (scopeWalkStatements walkState)
                        pendingValues = Map.insert statementIndex rawValueResult (scopeWalkPendingValues walkState)
                        (scopeResultType, resultState) =
                          go
                            walkState
                              { scopeWalkStatements = committedDrafts,
                                scopeWalkPendingValues = Map.withoutKeys pendingValues (Set.fromList committedIndices),
                                scopeWalkEnv = nextEnv,
                                scopeWalkEnvFreeVariables = nextEnvFreeVariables,
                                scopeWalkPendingSignature = Nothing,
                                scopeWalkPendingSignaturesByStatement = nextPendingSignaturesByStatement,
                                scopeWalkRecursiveGroupStartStates = recursiveGroupStartStatesForStatement,
                                scopeWalkRecursiveGroupPreviewCache = recursiveGroupPreviewCacheAfterStatement,
                                scopeWalkInferState =
                                  annotateNewErrorsWithContext (CheckingBinding nameText) bindingSpan stateForStatement stateAfterRecursiveGroupPrune
                              }
                            rest
                     in (scopeResultType, resultState)
                  SExpr exprNode expr ->
                    let exprSpan = coreNodeSpan exprNode
                        (envForStatement, stateForStatement, _) =
                          exposeVisibleRecursiveGroupSchemes statementIndex env envFreeVariables stateForSource recursiveGroupPreviewCache
                        (exprResult, rawStateAfterExpr) = inferExpression mode envForStatement stateForStatement expr
                        exprType = checkedExprType exprResult
                        stateAfterExpr =
                          annotateNewErrorsWithPrimarySpan exprSpan stateForStatement rawStateAfterExpr
                        stateAfterExplicitConstraintCheck =
                          finalizeDeferredExplicitConstraintsAt
                            exprSpan
                            stateForStatement
                            stateAfterExpr
                        stateAfterDroppedInferredMethodCheck =
                          case exprType of
                            Just resultType ->
                              addUnpreservedInferredMethodConstraintErrors
                                exprSpan
                                (freeTypeVariablesInEnv stateAfterExplicitConstraintCheck envForStatement)
                                stateForStatement
                                stateAfterExplicitConstraintCheck
                                resultType
                                Set.empty
                            Nothing -> stateAfterExplicitConstraintCheck
                        (scopeResultType, resultState) =
                          go
                            walkState
                              { scopeWalkStatements = Map.insert statementIndex (buildStatement statementIndex envForStatement (Just exprResult) [] statement) (scopeWalkStatements walkState),
                                scopeWalkLastExprType = exprType,
                                scopeWalkPendingSignature = Nothing,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkInferState = stateAfterDroppedInferredMethodCheck
                              }
                            rest
                     in (scopeResultType, resultState)

      builtinOperatorAliasSymbol :: Text -> Bool
      builtinOperatorAliasSymbol operatorSymbol =
        isBuiltinOperatorSymbol operatorSymbol && operatorSymbol /= "|"

      nextBindingForValue ::
        Int ->
        TypeEnv ->
        Set InferenceVariable ->
        Expr 'Resolved ->
        Maybe ExpressionType ->
        Maybe PendingSignatureType ->
        InferState ->
        Maybe TypeBinding
      nextBindingForValue statementIndex currentEnv environmentVariables valueExpr maybeInferredType maybePendingSignature state =
        let monomorphicBinding =
              if Set.member statementIndex (Map.keysSet recursiveGroupsByStatement)
                then PlainTypeBinding <$> maybeInferredType
                else ordinaryBindingForValue statementIndex currentEnv environmentVariables valueExpr maybeInferredType maybePendingSignature state
         in case valueExpr of
              EVar node _
                | Just (BuiltinOperatorReference operatorSymbol) <- resolvedNodeReference (coreNodeFacts node),
                  isNothing maybePendingSignature,
                  builtinOperatorAliasSymbol operatorSymbol ->
                    Just (operatorAliasBinding operatorSymbol monomorphicBinding)
              EApply _ _ _
                | isNothing maybePendingSignature,
                  Just (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr currentEnv valueExpr ->
                    Just (operatorAliasBinding operatorSymbol (SchemeTypeBinding <$> maybeAliasScheme))
              EVar node builtinName ->
                let referencedName = identifierText builtinName
                 in case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) builtinName) currentEnv of
                      Just (BuiltinAliasTypeBinding builtinSymbol) ->
                        Just (BuiltinAliasTypeBinding builtinSymbol)
                      Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                        | isNothing maybePendingSignature ->
                            Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                      Just binding@(OperatorAliasSchemeTypeBinding _ _)
                        | isNothing maybePendingSignature ->
                            Just binding
                      Just _ ->
                        monomorphicBinding
                      Nothing ->
                        case lookupKernelBuiltinSymbol referencedName of
                          Just builtinSymbol -> Just (BuiltinAliasTypeBinding builtinSymbol)
                          Nothing -> monomorphicBinding
              _ -> monomorphicBinding

      operatorAliasBinding :: Text -> Maybe TypeBinding -> TypeBinding
      operatorAliasBinding operatorSymbol maybeBinding =
        case maybeBinding of
          Just (SchemeTypeBinding typeScheme) ->
            OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
          Just (OperatorAliasSchemeTypeBinding _ typeScheme) ->
            OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
          _ ->
            BuiltinOperatorAliasTypeBinding operatorSymbol

      ordinaryBindingForValue ::
        Int ->
        TypeEnv ->
        Set InferenceVariable ->
        Expr 'Resolved ->
        Maybe ExpressionType ->
        Maybe PendingSignatureType ->
        InferState ->
        Maybe TypeBinding
      ordinaryBindingForValue statementIndex currentEnv environmentVariables valueExpr maybeInferredType maybePendingSignature state =
        case maybeInferredType of
          Just _
            | Just pendingSignature <- maybePendingSignature,
              shouldGeneralizeExplicitSignatureBinding pendingSignature ->
                Just (generalizedExplicitSignatureBinding environmentVariables state pendingSignature)
          Just inferredType
            | shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature ->
                Just (generalizedOrdinaryBinding environmentVariables state valueExpr inferredType)
          _ -> PlainTypeBinding <$> maybeInferredType

      shouldGeneralizeExplicitSignatureBinding :: PendingSignatureType -> Bool
      shouldGeneralizeExplicitSignatureBinding pendingSignature =
        ( not (null (pendingSignatureExplicitConstraints pendingSignature))
            || not (null (pendingSignatureVariableOrder pendingSignature))
        )

      setRigidTypeVariables rigidVariables state =
        state
          { inferSolver =
              (inferSolver state) {solverRigidTypeVars = rigidVariables}
          }

      restoreRigidTypeVariables originalState =
        setRigidTypeVariables (inferRigidTypeVars originalState)

      shouldGeneralizeOrdinaryBinding ::
        Int ->
        TypeEnv ->
        Expr 'Resolved ->
        Maybe PendingSignatureType ->
        Bool
      shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature =
        isNothing maybePendingSignature
          && Set.notMember statementIndex signedBindingStatements
          && not (isDirectConstructorAlias currentEnv valueExpr)

      generalizationEnvForStatement :: Int -> TypeEnv -> TypeEnv
      generalizationEnvForStatement statementIndex currentEnv =
        case Map.lookup statementIndex recursiveGroupsByStatement of
          Just groupMembers ->
            foldl' (flip Map.delete) currentEnv (recursiveGroupBindingNames groupMembers)
          Nothing ->
            currentEnv

      recursiveGroupBindingNames :: [Int] -> Set TypeEnvKey
      recursiveGroupBindingNames groupMembers =
        Set.fromList
          [ bindingName
          | memberIndex <- groupMembers,
            Just bindingName <- [Map.lookup memberIndex bindingKeysByStatement]
          ]

      rememberRecursiveGroupStart :: Int -> InferState -> Map Int InferState -> Map Int InferState
      rememberRecursiveGroupStart statementIndex state groupStartStates =
        case Map.lookup statementIndex recursiveGroupsByStatement of
          Just groupMembers
            | Just (firstMember, _) <- uncons groupMembers,
              statementIndex == firstMember ->
                Map.insert firstMember state groupStartStates
          _ -> groupStartStates

      generalizeCompletedRecursiveGroup :: Map Int PendingSignatureType -> Int -> TypeEnv -> Map Int InferState -> InferState -> (TypeEnv, InferState)
      generalizeCompletedRecursiveGroup pendingSignatures statementIndex currentEnv groupStartStates state =
        case Map.lookup statementIndex recursiveGroupsByStatement of
          Just groupMembers
            | Just (firstMember, _) <- uncons groupMembers,
              Just (_, lastMember) <- unsnoc groupMembers,
              statementIndex == lastMember ->
                let groupBindingNames =
                      Set.fromList
                        [ bindingName
                        | memberIndex <- groupMembers,
                          Just bindingName <- [Map.lookup memberIndex bindingKeysByStatement]
                        ]
                    envOutsideGroup =
                      foldl' (flip Map.delete) currentEnv groupBindingNames
                    nextEnv =
                      foldl'
                        (generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state)
                        currentEnv
                        groupMembers
                    groupStartState =
                      Map.findWithDefault state firstMember groupStartStates
                    groupBindings =
                      [ binding
                      | memberIndex <- groupMembers,
                        Just bindingName <- [Map.lookup memberIndex bindingKeysByStatement],
                        Just binding <- [Map.lookup bindingName nextEnv]
                      ]
                 in ( nextEnv,
                      pruneCapturedInferredClassConstraintsForBindings groupStartState groupBindings state
                    )
          _ -> (currentEnv, state)

      refreshCompletedRecursiveGroupFreeVariables statementIndex currentEnv currentSummary =
        case Map.lookup statementIndex recursiveGroupsByStatement of
          Just groupMembers
            | Just (_, lastMember) <- unsnoc groupMembers,
              statementIndex == lastMember ->
                foldl' refreshMember currentSummary groupMembers
          _ -> currentSummary
        where
          refreshMember summary memberIndex =
            case Map.lookup memberIndex bindingKeysByStatement of
              Just bindingName ->
                case Map.lookup bindingName currentEnv of
                  Just binding -> insertTypeEnvFreeVariables bindingName binding summary
                  Nothing -> summary
              Nothing -> summary

      dropAdvancedRecursiveGroupPreview statementIndex previewCache =
        case Map.lookup statementIndex recursiveGroupsByStatement >>= uncons of
          Just (firstMember, _) ->
            Map.filterWithKey
              (\(cachedFirstMember, _) _ -> cachedFirstMember /= firstMember)
              previewCache
          Nothing -> previewCache

      exposeVisibleRecursiveGroupSchemes :: Int -> TypeEnv -> TypeEnvFreeVariables -> InferState -> RecursiveGroupPreviewCache -> (TypeEnv, InferState, RecursiveGroupPreviewCache)
      exposeVisibleRecursiveGroupSchemes statementIndex currentEnv currentEnvFreeVariables state previewCache =
        let (nextEnv, _, nextState, nextCache) =
              foldl'
                exposeGroup
                (currentEnv, currentEnvFreeVariables, state, previewCache)
                (Map.findWithDefault [] statementIndex recursiveGroupsByInterveningLet)
         in (nextEnv, nextState, nextCache)
        where
          exposeGroup (envAcc, freeVariablesAcc, stateAcc, cacheAcc) groupMembers =
            case (uncons groupMembers, unsnoc groupMembers, unsnoc processedMembers) of
              (Just (firstMember, _), Just (_, lastMember), Just (_, processedLastMember))
                | statementIndex `elem` groupMembers ->
                    (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                | statementIndex > lastMember ->
                    (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                | any (`Set.member` signedBindingStatements) groupMembers ->
                    (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                | interleavedBindingFeedsLaterGroup statementIndex groupMembers ->
                    (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                | laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers ->
                    (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                | otherwise ->
                    let previewKey = (firstMember, processedLastMember)
                     in case Map.lookup previewKey cacheAcc of
                          Just cachedPreview
                            | recursiveGroupPreviewIsCurrent stateAcc cachedPreview ->
                                let (nextEnv, nextFreeVariables) =
                                      applyRecursiveGroupPreview statementIndex envAcc freeVariablesAcc cachedPreview
                                 in ( nextEnv,
                                      nextFreeVariables,
                                      reserveRecursiveGroupPreviewState stateAcc cachedPreview,
                                      cacheAcc
                                    )
                          _ ->
                            case previewRecursiveGroupState envAcc stateAcc statementIndex groupMembers of
                              (Nothing, nextState) ->
                                (envAcc, freeVariablesAcc, nextState, cacheAcc)
                              (Just previewState, nextState) ->
                                let groupBindingNames =
                                      Set.fromList
                                        [ bindingName
                                        | memberIndex <- groupMembers,
                                          Just bindingName <- [Map.lookup memberIndex bindingKeysByStatement]
                                        ]
                                    envOutsideGroup =
                                      foldl' (flip Map.delete) envAcc groupBindingNames
                                    freeVariablesOutsideGroup =
                                      Set.foldl'
                                        (flip deleteTypeEnvFreeVariables)
                                        freeVariablesAcc
                                        groupBindingNames
                                    environmentVariables =
                                      resolveTypeEnvFreeVariables previewState freeVariablesOutsideGroup
                                    (nextEnv, nextFreeVariables) =
                                      foldl'
                                        (exposePreviewRecursiveGroupMember statementIndex envOutsideGroup environmentVariables previewState)
                                        (envAcc, freeVariablesAcc)
                                        processedMembers
                                    previewBindings =
                                      Map.fromList
                                        [ (memberIndex, binding)
                                        | memberIndex <- processedMembers,
                                          bindingIsVisibleBefore statementIndex memberIndex,
                                          Just bindingName <- [Map.lookup memberIndex bindingKeysByStatement],
                                          Just binding <- [Map.lookup bindingName nextEnv]
                                        ]
                                    previewDependencies =
                                      recursiveGroupPreviewDependencyTypes stateAcc previewBindings
                                    previewDependencyVariables = Map.keysSet previewDependencies
                                    cachedPreview =
                                      RecursiveGroupPreview
                                        { recursiveGroupPreviewBindings = previewBindings,
                                          recursiveGroupPreviewNextTypeVar = solverNextTypeVar (inferSolver previewState),
                                          recursiveGroupPreviewDependencies = previewDependencies,
                                          recursiveGroupPreviewNumericConstraints =
                                            Map.restrictKeys (inferNumericVars stateAcc) previewDependencyVariables,
                                          recursiveGroupPreviewStrictEqualityVars =
                                            Set.intersection (inferStrictEqualityVars stateAcc) previewDependencyVariables
                                        }
                                 in (nextEnv, nextFreeVariables, nextState, Map.insert previewKey cachedPreview cacheAcc)
              _ ->
                (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
            where
              processedMembers = filter (< statementIndex) groupMembers

          applyRecursiveGroupPreview currentStatementIndex envAcc freeVariablesAcc cachedPreview =
            foldl'
              applyBinding
              (envAcc, freeVariablesAcc)
              (Map.toAscList (recursiveGroupPreviewBindings cachedPreview))
            where
              applyBinding (bindingEnv, freeVariables) (memberIndex, binding) =
                case Map.lookup memberIndex bindingKeysByStatement of
                  Just bindingName
                    | bindingIsVisibleBefore currentStatementIndex memberIndex ->
                        ( Map.insert bindingName binding bindingEnv,
                          insertTypeEnvFreeVariables bindingName binding freeVariables
                        )
                  _ -> (bindingEnv, freeVariables)

          reserveRecursiveGroupPreviewState stateAcc cachedPreview =
            stateAcc
              { inferSolver =
                  (inferSolver stateAcc)
                    { solverNextTypeVar =
                        max
                          (solverNextTypeVar (inferSolver stateAcc))
                          (recursiveGroupPreviewNextTypeVar cachedPreview)
                    }
              }

          recursiveGroupPreviewIsCurrent stateAcc cachedPreview =
            resolvedDependenciesAreCurrent
              && currentNumericConstraints == recursiveGroupPreviewNumericConstraints cachedPreview
              && currentStrictEqualityVars == recursiveGroupPreviewStrictEqualityVars cachedPreview
            where
              dependencies = recursiveGroupPreviewDependencies cachedPreview
              dependencyVariables = Map.keysSet dependencies
              resolvedDependenciesAreCurrent =
                Map.foldlWithKey'
                  (\isCurrent typeVar expectedType -> isCurrent && resolveType stateAcc (SemanticVariable typeVar) == expectedType)
                  True
                  dependencies
              currentNumericConstraints =
                Map.restrictKeys (inferNumericVars stateAcc) dependencyVariables
              currentStrictEqualityVars =
                Set.intersection (inferStrictEqualityVars stateAcc) dependencyVariables

          recursiveGroupPreviewDependencyTypes stateAcc bindings =
            Map.fromSet
              (resolveType stateAcc . SemanticVariable)
              (Set.unions (map recursiveGroupPreviewBindingFreeVariables (Map.elems bindings)))

          recursiveGroupPreviewBindingFreeVariables binding =
            case binding of
              PlainTypeBinding expressionType -> freeTypeVariables expressionType
              SchemeTypeBinding typeScheme -> recursiveGroupPreviewSchemeFreeVariables typeScheme
              OperatorAliasSchemeTypeBinding _ typeScheme -> recursiveGroupPreviewSchemeFreeVariables typeScheme
              _ -> Set.empty

          recursiveGroupPreviewSchemeFreeVariables typeScheme =
            Set.difference
              ( Set.unions
                  [ freeTypeVariables (schemeResultType typeScheme),
                    freeTypeVariablesInTypeSchemeConstraints (schemeClassConstraints typeScheme),
                    freeTypeVariablesInTypeSchemePrimitiveConstraints (schemePrimitiveConstraints typeScheme)
                  ]
              )
              (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme))

      interleavedBindingFeedsLaterGroup :: Int -> [Int] -> Bool
      interleavedBindingFeedsLaterGroup statementIndex groupMembers =
        case Map.lookup statementIndex (resolvedScopeBinderIds lexicalFacts) of
          Just binder -> any (laterGroupMemberReferences binder) (filter (> statementIndex) groupMembers)
          Nothing -> False

      laterGroupMemberReferences :: CoreBinderId -> Int -> Bool
      laterGroupMemberReferences binder memberIndex =
        maybe False (Set.member binder) (Map.lookup memberIndex previewGroupReferencesByStatement)

      laterGroupMemberDependsOnInterveningBinding :: Int -> [Int] -> Bool
      laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
        any memberDependsOnInterveningBinding (filter (> statementIndex) groupMembers)
        where
          groupMemberSet = Set.fromList groupMembers
          memberDependsOnInterveningBinding memberIndex =
            maybe
              False
              (any (interveningBindingIsReferenced memberIndex))
              (Map.lookup memberIndex previewGroupReferencesByStatement)
          interveningBindingIsReferenced memberIndex binder =
            case Map.lookup binder bindingIndexById of
              Just bindingIndex ->
                statementIndex < bindingIndex && bindingIndex < memberIndex && Set.notMember bindingIndex groupMemberSet
              Nothing -> False

      previewRecursiveGroupState :: TypeEnv -> InferState -> Int -> [Int] -> (Maybe InferState, InferState)
      previewRecursiveGroupState currentEnv state statementIndex groupMembers =
        previewInference (\initial -> foldl' previewMember initial (filter (> statementIndex) groupMembers)) state
        where
          previewMember stateAcc memberIndex =
            case Map.lookup memberIndex statementsByIndex of
              Just (SLet bindingNode bindingName valueExpr) ->
                let key = typeEnvBindingKey (coreNodeFacts bindingNode) bindingName
                    nameText = identifierText bindingName
                    bindingSpan = coreNodeSpan bindingNode
                    envWithRecursiveBindings =
                      recursiveBindingEnv
                        memberIndex
                        currentEnv
                        recursiveGroupsByStatement
                        bindingKeysByStatement
                        bindingSeedsByStatement
                    envWithBindingSeed =
                      case ( shouldSeedSelfRecursiveBinding memberIndex key currentEnv,
                             Map.lookup memberIndex bindingSeedsByStatement
                           ) of
                        (True, Just bindingSeed) ->
                          Map.insert key (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                        _ -> envWithRecursiveBindings
                    (valueResult, rawStateAfterValue) =
                      inferExpression InferenceOnly envWithBindingSeed stateAcc valueExpr
                    valueType = checkedExprType valueResult
                    stateAfterValue =
                      annotateNewErrorsWithPrimarySpan bindingSpan stateAcc rawStateAfterValue
                 in case (Map.lookup memberIndex bindingSeedsByStatement, valueType) of
                      (Just bindingSeed, Just inferredType) ->
                        case unifyTypes bindingSeed inferredType stateAfterValue of
                          Just unifiedState -> unifiedState
                          Nothing ->
                            addTypeError
                              stateAfterValue
                              ( mkBindingTypeMismatchError
                                  nameText
                                  (resolveType stateAfterValue bindingSeed)
                                  bindingSpan
                                  (resolveType stateAfterValue inferredType)
                              )
                      _ -> stateAfterValue
              _ -> stateAcc

      shouldSeedSelfRecursiveFunction :: Int -> TypeEnvKey -> TypeEnv -> Bool
      shouldSeedSelfRecursiveFunction statementIndex bindingName visibleEnv =
        Set.member statementIndex selfRecursiveFunctionStatements
          && Map.notMember bindingName visibleEnv

      shouldSeedSelfRecursiveBinding :: Int -> TypeEnvKey -> TypeEnv -> Bool
      shouldSeedSelfRecursiveBinding statementIndex bindingName visibleEnv =
        ( Set.member statementIndex selfRecursiveTypeStatements
            || shouldSeedSelfRecursiveFunction statementIndex bindingName visibleEnv
        )
          && Map.notMember bindingName visibleEnv

      exposePreviewRecursiveGroupMember :: Int -> TypeEnv -> Set InferenceVariable -> InferState -> (TypeEnv, TypeEnvFreeVariables) -> Int -> (TypeEnv, TypeEnvFreeVariables)
      exposePreviewRecursiveGroupMember statementIndex envOutsideGroup environmentVariables state (currentEnv, currentFreeVariables) memberIndex =
        case Map.lookup memberIndex bindingKeysByStatement of
          Just bindingName
            | bindingIsVisibleBefore statementIndex memberIndex ->
                let nextEnv =
                      generalizeRecursiveGroupMemberWithVariables
                        Map.empty
                        envOutsideGroup
                        environmentVariables
                        state
                        currentEnv
                        memberIndex
                 in case Map.lookup bindingName nextEnv of
                      Just binding ->
                        ( nextEnv,
                          insertTypeEnvFreeVariables bindingName binding currentFreeVariables
                        )
                      Nothing -> (nextEnv, currentFreeVariables)
          _ -> (currentEnv, currentFreeVariables)

      bindingIsVisibleBefore :: Int -> Int -> Bool
      bindingIsVisibleBefore statementIndex memberIndex =
        memberIndex < statementIndex
          && maybe True (>= statementIndex) (Map.lookup memberIndex (resolvedScopeBindingReplacements lexicalFacts))

      generalizeRecursiveGroupMember :: Map Int PendingSignatureType -> TypeEnv -> InferState -> TypeEnv -> Int -> TypeEnv
      generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state currentEnv memberIndex =
        generalizeRecursiveGroupMemberWithVariables
          pendingSignatures
          envOutsideGroup
          (freeTypeVariablesInEnv state envOutsideGroup)
          state
          currentEnv
          memberIndex

      generalizeRecursiveGroupMemberWithVariables :: Map Int PendingSignatureType -> TypeEnv -> Set InferenceVariable -> InferState -> TypeEnv -> Int -> TypeEnv
      generalizeRecursiveGroupMemberWithVariables pendingSignatures envOutsideGroup environmentVariables state currentEnv memberIndex =
        case (Map.lookup memberIndex statementsByIndex, Map.lookup memberIndex bindingKeysByStatement) of
          (Just (SLet _ _ _), Just bindingName)
            | Just pendingSignature <- Map.lookup memberIndex pendingSignatures,
              shouldGeneralizeExplicitSignatureBinding pendingSignature ->
                Map.insert
                  bindingName
                  (generalizedExplicitSignatureBinding environmentVariables state pendingSignature)
                  currentEnv
          (Just (SLet _ _ valueExpr), Just bindingName)
            | shouldGeneralizeOrdinaryBinding memberIndex envOutsideGroup valueExpr Nothing ->
                case Map.lookup memberIndex bindingSeedsByStatement of
                  Just bindingSeed ->
                    Map.insert
                      bindingName
                      (generalizedOrdinaryBinding environmentVariables state valueExpr bindingSeed)
                      currentEnv
                  _ -> currentEnv
          _ -> currentEnv

data RecursiveGroupPreview = RecursiveGroupPreview
  { recursiveGroupPreviewBindings :: Map Int TypeBinding,
    recursiveGroupPreviewNextTypeVar :: InferenceVariable,
    recursiveGroupPreviewDependencies :: Map InferenceVariable ExpressionType,
    recursiveGroupPreviewNumericConstraints :: Map InferenceVariable NumericConstraint,
    recursiveGroupPreviewStrictEqualityVars :: Set InferenceVariable
  }

type RecursiveGroupPreviewCache = Map (Int, Int) RecursiveGroupPreview

data PreparedSignature
  = PreparedSignature (Maybe PendingSignatureType) Bool

data PreparedDeclaration
  = PreparedClassMethods [CheckedClassMethod]
  | PreparedImplementationTargets [SemanticType ResolvedName Void]
  | PreparedDataType DataTypeBinding

data ScopePreparation = ScopePreparation
  { preparedBindingSeeds :: Map Int ExpressionType,
    preparedSignatures :: Map Int PreparedSignature,
    preparedForwardFunctions :: Map Int ExpressionType,
    preparedDeclarations :: Map Int (Either Diagnostic PreparedDeclaration),
    preparedDataTypes :: Map ResolvedName DataTypeBinding,
    preparedScopeState :: InferState
  }

prepareScope ::
  ForwardSignedFunctionsPolicy ->
  InferenceMode ->
  [(Int, Statement 'Resolved)] ->
  InferState ->
  ScopePreparation
prepareScope forwardSignedFunctionsPolicy mode indexedStatements initialState =
  let (bindingSeeds, signatures, forwardFunctions, declarations, _, _, finalPreparationState) =
        foldl'
          step
          ( Map.empty,
            Map.empty,
            Map.empty,
            Map.empty,
            Nothing,
            capabilityFactsFromState initialState,
            initialState
          )
          indexedStatements
   in ScopePreparation
        { preparedBindingSeeds = bindingSeeds,
          preparedSignatures = signatures,
          preparedForwardFunctions = forwardFunctions,
          preparedDeclarations = declarations,
          preparedDataTypes = scopeDataTypes,
          preparedScopeState =
            initialState
              { inferSolver =
                  (inferSolver initialState)
                    { solverNextTypeVar = solverNextTypeVar (inferSolver finalPreparationState)
                    }
              }
        }
  where
    preparedKinds = predeclareScopeDataTypes indexedStatements initialState
    scopeDataTypes = either (const fallbackDataTypes) id preparedKinds
    fallbackDataTypes = Map.fromList [(name, DataTypeBinding parameters []) | (_, SData _ name parameters _) <- indexedStatements]
    checkedDataType node name binding = case preparedKinds of
      Left (failedName, failure@Signature.SignatureKindMismatch {})
        | name == failedName ->
            Left (setDiagnosticPrimarySpan (coreNodeSpan node) (mkInvalidConstructorPayloadTypeError (Signature.renderSignatureTypeFailure failure)))
      _ -> Right (PreparedDataType binding)
    step
      (bindingSeeds, signatures, forwardFunctions, declarations, pendingSignature, moduleBaselineFacts, state)
      (statementIndex, statement) =
        case statement of
          SModule moduleNode _ ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
              declarations,
              Nothing,
              moduleBaselineFacts,
              enterModuleCapabilityScope moduleBaselineFacts (sourceUnitOwnerModulePath (resolvedNodeOwner (coreNodeFacts moduleNode))) state
            )
          SImport importNode _ maybeAlias maybeSymbolNames ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
              declarations,
              Nothing,
              moduleBaselineFacts,
              maybe state (\target -> importModuleCapabilityFacts target maybeAlias maybeSymbolNames state) (resolvedNodeImportTarget (coreNodeFacts importNode))
            )
          SClass _ capabilityName parameters methods ->
            let checked = checkClassMethods state capabilityName parameters methods
                nextState = either (const state) (registerClassDeclaration state capabilityName parameters) checked
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
                  Map.insert statementIndex (PreparedClassMethods <$> checked) declarations,
                  Nothing,
                  updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
                  nextState
                )
          SImpl implNode capabilityName arguments methods ->
            let checked = checkImplementationTargets state (coreNodeSpan implNode) arguments
                nextState = either (const state) (\targets -> registerImplementation implNode capabilityName targets methods state) checked
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
                  Map.insert statementIndex (PreparedImplementationTargets <$> checked) declarations,
                  Nothing,
                  updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
                  nextState
                )
          SData node typeName typeParameters _ ->
            let binding = Map.findWithDefault (DataTypeBinding typeParameters []) typeName scopeDataTypes
                checked = checkedDataType node typeName binding
                nextState = modifyDeclarationState (\current -> current {declarationDataTypes = Map.insertWith (\_ existing -> existing) typeName binding (inferDataTypes state)}) state
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
                  Map.insert statementIndex checked declarations,
                  Nothing,
                  moduleBaselineFacts,
                  nextState
                )
          SSignature signatureNode name signaturePayload ->
            let (maybeSignatureType, stateAfterSignature) =
                  Signature.signaturePayloadToSignatureType signaturePayload state
                maybePendingSignature =
                  fmap
                    ( \signatureType ->
                        PendingSignatureType
                          (identifierText name)
                          (coreNodeSpan signatureNode)
                          (Signature.signaturePayloadDeclaredType signatureType)
                          (Signature.signaturePayloadExplicitConstraints signatureType)
                          (Signature.signaturePayloadVariableOrder signatureType)
                    )
                    maybeSignatureType
                preparedSignature =
                  PreparedSignature
                    maybePendingSignature
                    ( maybe
                        False
                        (\signature -> unconstrainedSignaturePayload signaturePayload && eligibleForwardSignature signature)
                        maybePendingSignature
                    )
             in ( bindingSeeds,
                  Map.insert statementIndex preparedSignature signatures,
                  forwardFunctions,
                  declarations,
                  Just preparedSignature,
                  moduleBaselineFacts,
                  restoreCapabilityFacts state stateAfterSignature
                )
          SLet _ bindingName bindingExpression ->
            let (bindingSeed, nextState) = freshTypeVar state
                nextForwardFunctions =
                  case pendingSignature of
                    Just (PreparedSignature (Just signature) True)
                      | forwardSignedFunctionsPermitted forwardSignedFunctionsPolicy,
                        mode == InferConcreteFunctions,
                        pendingSignatureName signature == identifierText bindingName,
                        ELambda {} <- bindingExpression,
                        concreteForwardFunctionType (pendingSignatureDeclaredType signature) ->
                          Map.insert
                            statementIndex
                            (pendingSignatureDeclaredType signature)
                            forwardFunctions
                    _ -> forwardFunctions
             in ( Map.insert statementIndex bindingSeed bindingSeeds,
                  signatures,
                  nextForwardFunctions,
                  declarations,
                  Nothing,
                  moduleBaselineFacts,
                  nextState
                )
          _ ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
              declarations,
              Nothing,
              moduleBaselineFacts,
              state
            )

    unconstrainedSignaturePayload signaturePayload =
      case signaturePayload of
        ConstrainedSignature constraints _ -> null constraints
        _ -> True

    eligibleForwardSignature signature =
      null (pendingSignatureVariableOrder signature)
        && null (pendingSignatureExplicitConstraints signature)
        && concreteForwardFunctionType (pendingSignatureDeclaredType signature)

    concreteForwardFunctionType expressionType =
      case expressionType of
        SemanticFunction argumentType resultType ->
          concreteForwardScalarType argumentType
            && (concreteForwardScalarType resultType || concreteForwardFunctionType resultType)
        _ -> False

    concreteForwardScalarType expressionType =
      case expressionType of
        SemanticInt -> True
        SemanticFloat -> True
        SemanticNumeric {} -> True
        SemanticBool -> True
        SemanticChar -> True
        SemanticTuple [] -> True
        _ -> False

predeclareScopeDataTypes ::
  [(Int, Statement 'Resolved)] ->
  InferState ->
  Either (ResolvedName, Signature.SignatureTypeFailure) (Map ResolvedName DataTypeBinding)
predeclareScopeDataTypes indexedStatements initialState =
  prepareDataTypeKinds (inferDataTypes initialState) declarations
  where
    declarations =
      [ (typeName, parameters, concatMap (\(DataConstructor _ _ fields) -> fields) constructors)
      | (_, SData _ typeName parameters constructors) <- indexedStatements,
        Map.notMember typeName (inferDataTypes initialState)
      ]

recursiveBindingEnv ::
  Int ->
  TypeEnv ->
  Map Int [Int] ->
  Map Int TypeEnvKey ->
  Map Int ExpressionType ->
  TypeEnv
recursiveBindingEnv statementIndex env recursiveGroupsByStatement bindingNamesByStatement bindingSeedsByStatement =
  case Map.lookup statementIndex recursiveGroupsByStatement of
    Nothing -> env
    Just groupMembers ->
      foldl' insertBindingSeed env groupMembers
  where
    -- Preserve the declaration-time snapshot already visible in `env`; only
    -- missing peer names should be seeded into the recursive inference scope.
    insertBindingSeed envAcc memberIndex =
      case ( Map.lookup memberIndex bindingNamesByStatement,
             Map.lookup memberIndex bindingSeedsByStatement
           ) of
        (Just bindingNameText, Just bindingSeed)
          | Map.notMember bindingNameText env ->
              Map.insert bindingNameText (PlainTypeBinding bindingSeed) envAcc
        _ -> envAcc

collectSignedBindingStatements :: [(Int, Statement 'Resolved)] -> Set Int
collectSignedBindingStatements statements =
  case statements of
    (_, SSignature _ signatureName _) : (bindingIndex, SLet _ bindingName _) : rest
      | identifierText signatureName == identifierText bindingName ->
          Set.insert bindingIndex (collectSignedBindingStatements rest)
    _ : rest -> collectSignedBindingStatements rest
    [] -> Set.empty

isDirectConstructorAlias :: TypeEnv -> Expr 'Resolved -> Bool
isDirectConstructorAlias env expr =
  case expr of
    EVar node referencedName ->
      case Map.lookup (typeEnvReferenceKey (coreNodeFacts node) referencedName) env of
        Just ConstructorTypeBinding {} -> True
        _ -> False
    _ -> False

generalizedOrdinaryBinding :: Set InferenceVariable -> InferState -> Expr 'Resolved -> ExpressionType -> TypeBinding
generalizedOrdinaryBinding environmentVariables state valueExpr expressionType =
  let resolvedType = bindingTypeForValue state valueExpr expressionType
      schemeVariables = ordinaryBindingSchemeVariables environmentVariables state valueExpr expressionType
      inferredClassConstraints = typeSchemeInferredClassConstraints state schemeVariables
   in generalizedTypeBinding
        (bindingTypeScheme state (expressionTypeVariableOrder resolvedType) schemeVariables inferredClassConstraints resolvedType)

ordinaryBindingSchemeVariables :: Set InferenceVariable -> InferState -> Expr 'Resolved -> ExpressionType -> Set InferenceVariable
ordinaryBindingSchemeVariables environmentVariables state valueExpr expressionType =
  let resolvedType = bindingTypeForValue state valueExpr expressionType
      freeVariables = freeTypeVariables resolvedType
      quantifiedVariables = Set.difference freeVariables environmentVariables
   in Set.difference
        quantifiedVariables
        (numericConstrainedTypeVariables state)

bindingTypeForValue :: InferState -> Expr 'Resolved -> ExpressionType -> ExpressionType
bindingTypeForValue state valueExpr expressionType =
  case valueExpr of
    ESectionLeft {} -> resolvedType
    ESectionRight {} -> resolvedType
    _ -> defaultBindingLiteralTypes state resolvedType
  where
    resolvedType = resolveType state expressionType

generalizedExplicitSignatureBinding ::
  Set InferenceVariable ->
  InferState ->
  PendingSignatureType ->
  TypeBinding
generalizedExplicitSignatureBinding environmentVariables state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      schemeVariables = explicitBindingSchemeVariables environmentVariables state pendingSignature
      inferredClassConstraints =
        typeSchemeInferredClassConstraints state schemeVariables
      schemeConstraints =
        dedupeTypeSchemeConstraints (resolvedConstraints ++ inferredClassConstraints)
   in generalizedTypeBinding
        (bindingTypeScheme state (pendingSignatureVariableOrder pendingSignature) schemeVariables schemeConstraints resolvedType)

bindingTypeScheme :: InferState -> [InferenceVariable] -> Set InferenceVariable -> [TypeSchemeConstraint] -> ExpressionType -> TypeScheme
bindingTypeScheme state variableOrder variables constraints resultType =
  SemanticScheme
    { schemeQuantifiedVariables = quantifiedVariablesFromPreferred variableOrder variables,
      schemeClassConstraints = constraints,
      schemePrimitiveConstraints = typeSchemePrimitiveConstraints state variables,
      schemeDefiningCapabilities = typeSchemeDefiningFactsFromState state constraints,
      schemeResultType = resultType
    }

generalizedTypeBinding :: TypeScheme -> TypeBinding
generalizedTypeBinding scheme
  | Set.null (quantifiedVariablesMembershipSet (schemeQuantifiedVariables scheme)),
    null (schemeClassConstraints scheme),
    null (schemePrimitiveConstraints scheme) =
      PlainTypeBinding (schemeResultType scheme)
  | otherwise = SchemeTypeBinding scheme

addUndeclaredSignatureConstraintErrors :: Text -> InferState -> PendingSignatureType -> InferState -> InferState
addUndeclaredSignatureConstraintErrors bindingName statementStartState pendingSignature state
  | inferErrorCount state > inferErrorCount statementStartState = state
  | otherwise = foldl' addMissingConstraint state missingObligations
  where
    signatureVariables = Set.fromList (pendingSignatureVariableOrder pendingSignature)
    declaredConstraints =
      map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)

    inferredObligations =
      [ (False, Right constraintName, targetType)
      | constraint <- newInferredClassConstraints statementStartState state,
        Just (constraintName, targetType) <- [constraintIdentity (resolveTypeSchemeConstraint state constraint)],
        targetUsesSignatureVariables targetType
      ]

    primitiveObligations =
      [ case primitiveConstraint of
          TypeSchemeNumericConstraint _ targetType -> (True, Left "Num", targetType)
          TypeSchemeStrictEqualityConstraint targetType -> (True, Left "Eq", targetType)
      | primitiveConstraint <- typeSchemePrimitiveConstraints state signatureVariables
      ]

    missingObligations =
      dedupeObligations
        [ obligation
        | obligation@(_, constraintName, targetType) <- inferredObligations ++ primitiveObligations,
          not (declaredConstraintEntails constraintName targetType)
        ]

    targetUsesSignatureVariables targetType =
      let targetVariables = freeTypeVariables (resolveType state targetType)
       in not (Set.null targetVariables)
            && targetVariables `Set.isSubsetOf` signatureVariables

    declaredConstraintEntails requiredName requiredTarget =
      any matches declaredConstraints
      where
        matches declaredConstraint =
          case constraintIdentity declaredConstraint of
            Just (declaredName, declaredTarget) ->
              sameConstraintName (Right declaredName) requiredName
                && resolveType state declaredTarget == resolveType state requiredTarget
            Nothing -> False

    constraintIdentity constraint =
      case constraint of
        TypeSchemeConstraint constraintName targetType -> Just (constraintName, targetType)
        TypeSchemeInferredConstraint constraintName targetType -> Just (constraintName, targetType)
        TypeSchemeMethodConstraint constraintName _ targetType -> Just (constraintName, targetType)

    dedupeObligations =
      foldl' insertObligation []
      where
        insertObligation obligations obligation@(_, constraintName, targetType)
          | any (sameObligation constraintName targetType) obligations = obligations
          | otherwise = obligations ++ [obligation]
        sameObligation constraintName targetType (_, existingName, existingTarget) =
          sameConstraintName constraintName existingName
            && resolveType state targetType == resolveType state existingTarget

    addMissingConstraint stateAcc (primitive, constraintName, targetType) =
      addTypeError
        stateAcc
        ( mkUndeclaredSignatureConstraintError
            bindingName
            primitive
            (constraintLabel constraintName)
            (resolveType state targetType)
            (pendingSignatureSpan pendingSignature)
        )

    constraintLabel = either id renderCapabilityId

    sameConstraintName (Right left) (Right right) = left == right
    sameConstraintName left right = constraintLabel left == constraintLabel right

pruneCapturedInferredClassConstraints :: InferState -> TypeBinding -> InferState -> InferState
pruneCapturedInferredClassConstraints statementStartState binding =
  pruneCapturedInferredClassConstraintsForBindings statementStartState [binding]

pruneCapturedInferredClassConstraintsForBindings :: InferState -> [TypeBinding] -> InferState -> InferState
pruneCapturedInferredClassConstraintsForBindings statementStartState bindings state =
  if Set.null capturedConstraints
    then state
    else
      modifyInferenceOutput
        ( \output ->
            output
              { outputInferredConstraints =
                  retainedStatementConstraints ++ priorConstraints,
                outputInferredConstraintCount =
                  priorConstraintCount + length retainedStatementConstraints
              }
        )
        state
  where
    priorConstraintCount = inferInferredClassConstraintCount statementStartState
    currentConstraints = inferInferredClassConstraints state
    statementConstraintCount = max 0 (inferInferredClassConstraintCount state - priorConstraintCount)
    statementConstraints = take statementConstraintCount currentConstraints
    priorConstraints = drop statementConstraintCount currentConstraints
    retainedStatementConstraints =
      filter
        (not . capturedInScheme . resolveTypeSchemeConstraint state)
        statementConstraints
    capturedConstraints =
      Set.fromList
        [ resolveTypeSchemeConstraint state constraint
        | binding <- bindings,
          Just typeScheme <- [typeBindingScheme binding],
          constraint <- schemeClassConstraints typeScheme,
          typeSchemeConstraintIsInferred constraint
        ]
    capturedInScheme constraint =
      Set.member constraint capturedConstraints

typeSchemeConstraintIsInferred :: TypeSchemeConstraint -> Bool
typeSchemeConstraintIsInferred constraint =
  case constraint of
    TypeSchemeInferredConstraint {} -> True
    TypeSchemeMethodConstraint {} -> True
    TypeSchemeConstraint {} -> False

explicitBindingSchemeVariables :: Set InferenceVariable -> InferState -> PendingSignatureType -> Set InferenceVariable
explicitBindingSchemeVariables environmentVariables state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      freeVariables =
        Set.union
          (freeTypeVariables resolvedType)
          (freeTypeVariablesInTypeSchemeConstraints resolvedConstraints)
   in Set.difference freeVariables environmentVariables

expressionTypeVariableOrder :: ExpressionType -> [InferenceVariable]
expressionTypeVariableOrder = Foldable.toList

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

numericConstrainedTypeVariables :: InferState -> Set InferenceVariable
numericConstrainedTypeVariables =
  Map.keysSet . inferNumericVars

typeSchemeInferredClassConstraints :: InferState -> Set InferenceVariable -> [TypeSchemeConstraint]
typeSchemeInferredClassConstraints state schemeVariables =
  dedupeTypeSchemeConstraints qualifiedMethodConstraints
  where
    qualifiedMethodConstraints =
      [ resolvedConstraint
      | constraint <- reverse (inferInferredClassConstraints state),
        Just resolvedConstraint <- [constraintForScheme constraint]
      ]

    constraintForScheme = traverse targetTypeFor

    targetTypeFor argumentType =
      let targetType = resolveType state argumentType
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan,
    pendingSignatureDeclaredType :: ExpressionType,
    pendingSignatureExplicitConstraints :: [TypeSchemeConstraint],
    pendingSignatureVariableOrder :: [InferenceVariable]
  }

targetedFractionalLiteralBindingType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr 'Resolved ->
  Maybe ExpressionType ->
  Maybe ExpressionType
targetedFractionalLiteralBindingType bindingName maybePendingSignature valueExpr maybeInferredType =
  case targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType of
    Just targetType -> Just (SemanticNumeric targetType)
    Nothing -> maybeInferredType

targetedFractionalLiteralDiagnostic ::
  Text ->
  Maybe PendingSignatureType ->
  Expr 'Resolved ->
  Maybe ExpressionType ->
  Maybe Diagnostic
targetedFractionalLiteralDiagnostic bindingName maybePendingSignature valueExpr maybeInferredType =
  case (targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType, valueExpr) of
    (Just targetType, ELit _ (LFloat literalValue literalSource Nothing)) ->
      targetedFloatLiteralDiagnostic targetType literalValue literalSource
    _ -> Nothing

targetedFractionalLiteralType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr 'Resolved ->
  Maybe ExpressionType ->
  Maybe NumericType
targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType =
  case (maybePendingSignature, valueExpr, maybeInferredType) of
    (Just pendingSignature, ELit _ (LFloat _ _ Nothing), Just SemanticFloat)
      | pendingSignatureName pendingSignature == bindingName ->
          concreteFloatNumericType (pendingSignatureDeclaredType pendingSignature)
    _ -> Nothing

concreteFloatNumericType :: ExpressionType -> Maybe NumericType
concreteFloatNumericType expressionType =
  case expressionType of
    SemanticNumeric NumericFloat16 -> Just NumericFloat16
    SemanticNumeric NumericFloat32 -> Just NumericFloat32
    SemanticNumeric NumericFloat64 -> Just NumericFloat64
    _ -> Nothing

insertRegisteredConstructorFreeVariables :: TypeEnv -> TypeEnvFreeVariables -> DataConstructor 'Resolved -> TypeEnvFreeVariables
insertRegisteredConstructorFreeVariables env summary (DataConstructor node constructorName _) =
  case Map.lookup (typeEnvBindingKey (coreNodeFacts node) constructorName) env of
    Just binding -> insertResolvedTypeEnvFreeVariables (coreNodeFacts node) constructorName binding summary
    Nothing -> summary

registerDataConstructors :: Map ResolvedName DataTypeBinding -> SourceSpan -> ResolvedName -> [ResolvedName] -> [DataConstructor 'Resolved] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors predeclaredDataTypes spanValue typeName typeParameters constructors env initialState =
  case Map.lookup typeName (inferDataTypes initialState) of
    Just _ ->
      ( env,
        addTypeError
          initialState
          (mkDuplicateDataTypeDeclarationError typeNameText spanValue)
      )
    Nothing -> registerInto initialState
  where
    typeNameText = identifierText typeName

    registerInto stateBeforeConstructors =
      let (nextEnv, nextState, constructorPayloadsRev) =
            foldl' register (env, stateBeforeConstructors, []) constructors
       in ( nextEnv,
            modifyDeclarationState
              ( \declarations ->
                  declarations
                    { declarationDataTypes =
                        Map.insert
                          typeName
                          ((Map.findWithDefault (DataTypeBinding typeParameters []) typeName predeclaredDataTypes) {dataTypeConstructors = reverse constructorPayloadsRev})
                          (inferDataTypes nextState)
                    }
              )
              nextState
          )

    register (envAcc, stateAcc, constructorPayloadsAcc) (DataConstructor node constructorName constructorArguments) =
      let (argumentTypes, nextState) =
            constructorArgumentTypes predeclaredDataTypes typeParameters constructorArguments stateAcc
          binding = ConstructorTypeBinding typeName typeParameters argumentTypes
       in ( insertResolvedTypeBinding (coreNodeFacts node) constructorName binding envAcc,
            nextState,
            argumentTypes : constructorPayloadsAcc
          )

constructorArgumentTypes :: Map ResolvedName DataTypeBinding -> [ResolvedName] -> [SignatureType 'Resolved] -> InferState -> ([ConstructorArgumentType], InferState)
constructorArgumentTypes predeclaredDataTypes typeParameters fieldTypes initialState =
  let (argumentTypesRev, finalState) =
        foldl' collectField ([], initialState) fieldTypes
   in (reverse argumentTypesRev, finalState)
  where
    visibleDataTypes = Map.union (inferDataTypes initialState) predeclaredDataTypes
    signatureVariables =
      Map.fromList
        [ (identifierText parameterName, SemanticVariable (identifierText parameterName))
        | parameterName <- typeParameters
        ]

    collectField (argumentTypesRev, stateAcc) fieldType =
      case normalizeSignatureType visibleDataTypes signatureVariables fieldType of
        Right field ->
          ( ConstructorArgumentType field : argumentTypesRev,
            stateAcc
          )
        Left (Signature.UnknownNamedType payloadName) ->
          ( ConstructorArgumentFresh : argumentTypesRev,
            addTypeError stateAcc (mkUnknownConstructorPayloadTypeError payloadName)
          )
        Left failure ->
          ( ConstructorArgumentFresh : argumentTypesRev,
            addTypeError
              stateAcc
              (mkInvalidConstructorPayloadTypeError (Signature.renderSignatureTypeFailure failure))
          )

specializeExpectedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
specializeExpectedType state expectedType expressionType =
  let resolvedExpected = resolveType state expectedType
      resolvedExpression = resolveType state expressionType
   in case (integerLiteralRangeFor state expressionType, resolvedExpression, resolvedExpected) of
        (_, SemanticTuple expressionElements, SemanticTuple expectedElements)
          | length expressionElements == length expectedElements ->
              SemanticTuple (zipWith (specializeExpectedType state) expectedElements expressionElements)
        (_, SemanticData expressionName expressionArguments, SemanticData expectedName expectedArguments)
          | expressionName == expectedName,
            length expressionArguments == length expectedArguments ->
              SemanticData
                expressionName
                (zipWith (specializeExpectedType state) expectedArguments expressionArguments)
        (Just literalRange, _, SemanticInt)
          | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> SemanticInt
        (Just literalRange, _, numericType@(SemanticNumeric concreteType))
          | integerLiteralRangeFitsNumericType literalRange concreteType -> numericType
        (_, SemanticInt, SemanticNumeric NumericInt64) -> resolvedExpected
        (_, SemanticNumeric NumericInt64, SemanticInt) -> resolvedExpected
        (_, SemanticFloat, SemanticNumeric NumericFloat64) -> resolvedExpected
        (_, SemanticNumeric NumericFloat64, SemanticFloat) -> resolvedExpected
        _ -> resolvedExpression
