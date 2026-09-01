{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Scope, binding, signature, and constructor inference.  Typed-core
-- production selects and finalizes only the root scope after this traversal,
-- leaving ordinary scope inference and its runtime-hint ownership unchanged.
module Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplication,
    inferExplicitTypeApplicationWithResult,
    inferNestedScopeTypeWithMode,
    inferScopeType,
    inferScopeTypeWithMode,
    inferScopeTypeWithModeAndForwardBindings,
    inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope,
    instantiateNonBuiltinTypeBinding,
  )
where

import Data.List (uncons, unsnoc)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( ClassMethodSignature (..),
    CoreNode (coreNodeSpan),
    CorePhase (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax,
  )
import Jazz.Compiler.CapabilityFacts
  ( constraintSignatureTypeVariableNamesInOrder,
    signaturePayloadConstraintType,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.ModuleIdentity (modulePathTextSegments)
import Jazz.Compiler.Name
  ( NameNamespace (TypeNamespace, ValueNamespace),
    ResolvedName,
    identifierText,
    mkIdentifier,
    operatorBindingName,
    resolvedAmbientName,
  )
import Jazz.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    exprContainsFunctionBranch,
    freeVarsExprWithBound,
    inferSelfRecursiveBindings,
    prepareRecursiveScope,
    preparedRecursiveScopeFactsForOuterBindings,
    preparedRecursiveScopeStatements,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
  )
import Jazz.Compiler.RuntimeHints
  ( bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule,
  )
import Jazz.Compiler.TypeInference.Capabilities
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Elaboration
  ( specializeInferredExpression,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalConstructorDeclaration (..),
    ProvisionalDataDeclaration (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    blockProductionFailureKindAndDetail,
  )
import Jazz.Compiler.TypeInference.Pattern (instantiateConstructorBinding)
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVar,
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
    inferErrorsRev,
    inferInferredClassConstraintCount,
    inferInferredClassConstraints,
    inferNumericVars,
    inferRigidTypeVars,
    inferRuntimeHintPath,
    inferRuntimeTypeHints,
    inferStrictEqualityVars,
    modifyDeclarationState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
  )
import Jazz.Compiler.TypeInference.Traversal
  ( InferExprWithModeFn,
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    InferenceVariable (..),
    NumericConstraint,
    ScopeCapabilityFacts,
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    instantiateConstructorFieldType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    pattern ConstrainedSignature,
  )

inferExprTypeWithExpectedMode ::
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr 'Resolved ->
  (InferredExpr, InferState)
inferExprTypeWithExpectedMode inferExpression mode builtinMode env state expectedType expr =
  case mode of
    InferenceOnly -> inferInferenceOnly
    ProduceTypedCoreExpressionDirectCall -> inferProduction
  where
    inferInferenceOnly =
      case (resolveType state expectedType, expr) of
        (_, EVar _ name)
          | Map.notMember name env,
            Just (expressionType, nextState) <-
              instantiateQualifiedMethodTypeWithExpected
                (identifierText name)
                expectedType
                state ->
              (InferredExpr expressionType Nothing [], nextState)
        (SemanticFunction argumentType resultType, ELambda _ parameterName bodyExpr) ->
          let extendedEnv = Map.insert parameterName (PlainTypeBinding argumentType) env
              (bodyResult, stateAfterBody) =
                inferExprTypeWithExpectedMode
                  inferExpression
                  InferenceOnly
                  builtinMode
                  extendedEnv
                  state
                  resultType
                  bodyExpr
              expressionType =
                SemanticFunction
                  (resolveType stateAfterBody argumentType)
                  <$> inferredExpressionType bodyResult
           in (InferredExpr expressionType Nothing [], stateAfterBody)
        (SemanticNumeric numericType, ELit _ (LFloat literalValue literalSource Nothing))
          | Just _ <- numericTypeFloatMax numericType ->
              ( InferredExpr (Just (SemanticNumeric numericType)) Nothing [],
                maybe state (addTypeError state) (targetedFloatLiteralDiagnostic numericType literalValue literalSource)
              )
        _ -> inferExpression InferenceOnly builtinMode env state expr

    inferProduction =
      case (resolveType state expectedType, expr) of
        (_, EVar _ name)
          | Map.notMember name env,
            Just (expressionType, nextState) <-
              instantiateQualifiedMethodTypeWithExpected
                (identifierText name)
                expectedType
                state ->
              ( InferredExpr
                  expressionType
                  (if mode == ProduceTypedCoreExpressionDirectCall then ProvisionalVariableExpression name <$> expressionType else Nothing)
                  [],
                nextState
              )
        (SemanticFunction argumentType resultType, ELambda _ parameterName bodyExpr) ->
          let extendedEnv = Map.insert parameterName (PlainTypeBinding argumentType) env
              (bodyResult, stateAfterBody) =
                inferExprTypeWithExpectedMode inferExpression mode builtinMode extendedEnv state resultType bodyExpr
              functionType =
                SemanticFunction
                  (resolveType stateAfterBody argumentType)
                  (maybe resultType id (inferredExpressionType bodyResult))
              provisional =
                ProvisionalLambdaExpression parameterName functionType
                  <$> inferredProvisionalExpr bodyResult
              failures =
                [ InferredProductionFailure (0 : childPath) kind detail
                | InferredProductionFailure childPath kind detail <- inferredProductionFailures bodyResult
                ]
           in (InferredExpr (Just functionType) provisional failures, stateAfterBody)
        (SemanticNumeric _, literalExpr@(ELit _ literal@(LInt _))) ->
          let (literalResult, nextState) = inferExpression mode builtinMode env state literalExpr
           in case inferredExpressionType literalResult of
                Just literalType
                  | Just checkedState <- unifyTypes expectedType literalType nextState ->
                      let concreteType = resolveType checkedState expectedType
                       in ( InferredExpr
                              (Just concreteType)
                              (if mode == ProduceTypedCoreExpressionDirectCall then Just (ProvisionalLiteralExpression literal concreteType) else Nothing)
                              [],
                            checkedState
                          )
                _ -> (literalResult, nextState)
        (SemanticNumeric numericType, ELit _ literal@(LFloat literalValue literalSource Nothing))
          | Just _ <- numericTypeFloatMax numericType ->
              let nextState =
                    maybe state (addTypeError state) (targetedFloatLiteralDiagnostic numericType literalValue literalSource)
                  concreteType = SemanticNumeric numericType
               in ( InferredExpr
                      (Just concreteType)
                      (if mode == ProduceTypedCoreExpressionDirectCall then Just (ProvisionalLiteralExpression literal concreteType) else Nothing)
                      [],
                    nextState
                  )
        _ ->
          let (inferred, nextState) = inferExpression mode builtinMode env state expr
           in case inferredExpressionType inferred of
                Just expressionType
                  | Just checkedState <- unifyTypes expectedType expressionType nextState ->
                      (specializeInferredExpression checkedState expectedType inferred, checkedState)
                _ -> (inferred, nextState)

setStatementRuntimeHintPath :: Set Int -> Int -> InferState -> InferState
setStatementRuntimeHintPath preludeStatementIndices statementIndex state =
  modifyModuleInferenceState
    ( \moduleState ->
        moduleState
          { inferenceRuntimeHintPath =
              if Set.member statementIndex preludeStatementIndices
                then
                  Just
                    ( NonEmpty.toList
                        (modulePathTextSegments (moduleInferencePreludePath moduleState))
                    )
                else
                  if Set.null preludeStatementIndices
                    then inferenceRuntimeHintPath moduleState
                    else inferenceModulePath moduleState
          }
    )
    state

firstInvalidImplTarget :: InferState -> SourceSpan -> [SignatureType 'Resolved] -> Maybe Diagnostic
firstInvalidImplTarget state implSpan =
  go
  where
    go signatureTypes =
      case signatureTypes of
        [] -> Nothing
        signatureType : rest ->
          case mkInvalidImplTargetError state implSpan signatureType of
            Just diagnostic -> Just diagnostic
            Nothing -> go rest

firstInvalidClassMethodSignature :: InferState -> ResolvedName -> [ResolvedName] -> [ClassMethodSignature 'Resolved] -> Maybe Diagnostic
firstInvalidClassMethodSignature state capabilityName parameters =
  go
  where
    classParameterNames = Set.fromList (map identifierText parameters)

    go methods =
      case methods of
        [] -> Nothing
        ClassMethodSignature methodNode methodName methodPayload : rest ->
          let methodSpan = coreNodeSpan methodNode
              methodKey = identifierText capabilityName <> "::" <> identifierText methodName
              methodVariables =
                maybe [] constraintSignatureTypeVariableNamesInOrder (signaturePayloadConstraintType methodPayload)
              methodLocalVariables = filter (`Set.notMember` classParameterNames) methodVariables
              invalidMethodSignature =
                mkInvalidSignatureTypeError
                  state
                  methodKey
                  methodSpan
                  methodPayload
           in case methodPayload of
                ConstrainedSignature (_ : _) _ ->
                  Just
                    ( setDiagnosticPrimarySpan
                        methodSpan
                        (mkInvalidQualifiedMethodSignatureError methodKey methodPayload)
                    )
                _ ->
                  case methodLocalVariables of
                    variableName : _ ->
                      Just (mkMethodLocalTypeVariableError methodKey variableName methodSpan)
                    [] ->
                      case Signature.signaturePayloadToSignatureType methodPayload state of
                        (Just _, _) -> go rest
                        (Nothing, _) -> Just invalidMethodSignature

publishVisibleTypes :: TypeEnv -> InferState -> InferState
publishVisibleTypes env state =
  state
    { inferModule =
        (inferModule state) {inferenceVisibleTypes = env}
    }

inferScopeTypeWithMode :: Set Int -> InferExprWithModeFn -> TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement 'Resolved] -> (InferredExpr, InferState)
inferScopeTypeWithMode preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  let (inferredResult, finalState, _) =
        inferScopeTypeWithModeAndForwardBindings
          preludeStatementIndices
          inferExpression
          mode
          builtinMode
          initialEnv
          initialState
          statements
   in (inferredResult, finalState)

inferScopeTypeWithModeAndForwardBindings ::
  Set Int ->
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Statement 'Resolved] ->
  (InferredExpr, InferState, Map Int (ResolvedName, SourceSpan))
inferScopeTypeWithModeAndForwardBindings preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  inferScopeTypeInternal
    ScopeInferenceRequest
      { scopeForwardSignedFunctionsPolicy = PermitForwardSignedFunctions,
        scopePreludeStatementIndices = preludeStatementIndices,
        scopeInferExpression = inferExpression,
        scopeProductionMode = mode,
        scopeBuiltinMode = builtinMode,
        scopeInitialEnv = initialEnv,
        scopeInitialState = initialState,
        scopePreparedInference = prepareInferenceScope builtinMode initialEnv statements
      }

inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope ::
  PreparedRecursiveScope 'Resolved ->
  Set Int ->
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  (InferredExpr, InferState, Map Int (ResolvedName, SourceSpan))
inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope preparedScope preludeStatementIndices inferExpression mode builtinMode initialEnv initialState =
  let inferenceScope = preparedInferenceScope (inferenceOuterBindingNames builtinMode initialEnv) preparedScope
   in inferenceScope `seq`
        inferScopeTypeInternal
          ScopeInferenceRequest
            { scopeForwardSignedFunctionsPolicy = PermitForwardSignedFunctions,
              scopePreludeStatementIndices = preludeStatementIndices,
              scopeInferExpression = inferExpression,
              scopeProductionMode = mode,
              scopeBuiltinMode = builtinMode,
              scopeInitialEnv = initialEnv,
              scopeInitialState = initialState,
              scopePreparedInference = inferenceScope
            }

inferNestedScopeTypeWithMode :: Set Int -> InferExprWithModeFn -> TypedCoreProductionMode -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement 'Resolved] -> (InferredExpr, InferState)
inferNestedScopeTypeWithMode preludeStatementIndices inferExpression mode builtinMode initialEnv initialState statements =
  let (inferredResult, finalState, _) =
        inferScopeTypeInternal
          ScopeInferenceRequest
            { scopeForwardSignedFunctionsPolicy = ForbidForwardSignedFunctions,
              scopePreludeStatementIndices = preludeStatementIndices,
              scopeInferExpression = inferExpression,
              scopeProductionMode = mode,
              scopeBuiltinMode = builtinMode,
              scopeInitialEnv = initialEnv,
              scopeInitialState = initialState,
              scopePreparedInference = prepareInferenceScope builtinMode initialEnv statements
            }
   in (inferredResult, finalState)

inferScopeType :: Set Int -> InferExprWithModeFn -> BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement 'Resolved] -> (Maybe ExpressionType, InferState)
inferScopeType preludeStatementIndices inferExpression builtinMode initialEnv initialState statements =
  let (inferredResult, finalState) =
        inferNestedScopeTypeWithMode
          preludeStatementIndices
          inferExpression
          InferenceOnly
          builtinMode
          initialEnv
          initialState
          statements
   in (inferredExpressionType inferredResult, finalState)

prepareInferenceScope :: BuiltinResolutionMode -> TypeEnv -> [Statement 'Resolved] -> PreparedInferenceScope
prepareInferenceScope builtinMode initialEnv statements =
  preparedInferenceScope
    outerBindingNames
    (prepareRecursiveScope outerBindingNames statements)
  where
    outerBindingNames = inferenceOuterBindingNames builtinMode initialEnv

inferenceOuterBindingNames :: BuiltinResolutionMode -> TypeEnv -> Set ResolvedName
inferenceOuterBindingNames builtinMode initialEnv =
  Set.union
    (Map.keysSet initialEnv)
    (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) (builtinNamesInMode builtinMode))

data PreparedInferenceScope = PreparedInferenceScope ![Statement 'Resolved] !(Map Int ResolvedName) !(Map Int [Int])

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
    scopePreludeStatementIndices :: Set Int,
    scopeInferExpression :: InferExprWithModeFn,
    scopeProductionMode :: TypedCoreProductionMode,
    scopeBuiltinMode :: BuiltinResolutionMode,
    scopeInitialEnv :: TypeEnv,
    scopeInitialState :: InferState,
    scopePreparedInference :: PreparedInferenceScope
  }

data ScopeWalkState = ScopeWalkState
  { scopeWalkEnv :: !TypeEnv,
    scopeWalkEnvFreeVariables :: !TypeEnvFreeVariables,
    scopeWalkLastExprType :: !(Maybe ExpressionType),
    scopeWalkPendingSignature :: !(Maybe PendingSignatureType),
    scopeWalkPendingSignaturesByStatement :: !(Map Int PendingSignatureType),
    scopeWalkRecursiveGroupStartStates :: !(Map Int InferState),
    scopeWalkRecursiveGroupPreviewCache :: !RecursiveGroupPreviewCache,
    scopeWalkModuleBaselineFacts :: !ScopeCapabilityFacts,
    scopeWalkInferState :: !InferState
  }

preparedInferenceScope :: Set ResolvedName -> PreparedRecursiveScope 'Resolved -> PreparedInferenceScope
preparedInferenceScope expectedOuterBindingNames preparedScope =
  PreparedInferenceScope
    (preparedRecursiveScopeStatements preparedScope)
    (recursiveScopeBindingNames recursiveScopeFactsValue)
    (recursiveScopeGroups recursiveScopeFactsValue)
  where
    recursiveScopeFactsValue =
      preparedRecursiveScopeFactsForOuterBindings expectedOuterBindingNames preparedScope

inferScopeTypeInternal :: ScopeInferenceRequest -> (InferredExpr, InferState, Map Int (ResolvedName, SourceSpan))
inferScopeTypeInternal
  ScopeInferenceRequest
    { scopeForwardSignedFunctionsPolicy,
      scopePreludeStatementIndices,
      scopeInferExpression,
      scopeProductionMode,
      scopeBuiltinMode,
      scopeInitialEnv,
      scopeInitialState,
      scopePreparedInference = PreparedInferenceScope statements bindingNamesByStatement recursiveGroupsByStatement
    } =
    let initialWalkState =
          ScopeWalkState
            { scopeWalkEnv = initialEnv,
              scopeWalkEnvFreeVariables = typeEnvFreeVariables initialEnv,
              scopeWalkLastExprType = Nothing,
              scopeWalkPendingSignature = Nothing,
              scopeWalkPendingSignaturesByStatement = Map.empty,
              scopeWalkRecursiveGroupStartStates = Map.empty,
              scopeWalkRecursiveGroupPreviewCache = Map.empty,
              scopeWalkModuleBaselineFacts = initialModuleBaselineFacts,
              scopeWalkInferState = stateAfterBindingSeeds
            }
        (scopeType, finalState, provisionalStatements, productionFailures) =
          go initialWalkState indexedStatements
        stateWithPublishedModuleFacts = flushCurrentModuleCapabilityFacts finalState
        provisionalExpr =
          case scopeProductionMode of
            ProduceTypedCoreExpressionDirectCall -> Just (ProvisionalScopeStatements provisionalStatements)
            InferenceOnly -> Nothing
     in ( InferredExpr scopeType provisionalExpr productionFailures,
          restoreCapabilityFacts initialState stateWithPublishedModuleFacts,
          forwardAnalysisBindings
        )
    where
      preludeStatementIndices = scopePreludeStatementIndices
      inferExpression = scopeInferExpression
      mode = scopeProductionMode
      builtinMode = scopeBuiltinMode
      initialEnv = scopeInitialEnv
      initialState = scopeInitialState

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
      bindingIndicesByName =
        Map.foldlWithKey'
          ( \indicesByName statementIndex bindingName ->
              Map.insertWith
                Set.union
                bindingName
                (Set.singleton statementIndex)
                indicesByName
          )
          Map.empty
          bindingNamesByStatement
      previewGroupMemberIndices =
        Set.fromList
          [ memberIndex
          | groups <- Map.elems recursiveGroupsByInterveningLet,
            groupMembers <- groups,
            memberIndex <- groupMembers
          ]
      previewGroupFreeNamesByStatement =
        Map.fromList
          [ (statementIndex, freeVarsExprWithBound Set.empty valueExpr)
          | statementIndex <- Set.toList previewGroupMemberIndices,
            Just (SLet _ _ valueExpr) <- [Map.lookup statementIndex statementsByIndex]
          ]
      selfRecursiveFunctionStatements =
        inferSelfRecursiveBindings
          ( Set.union
              (Map.keysSet initialEnv)
              (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) (builtinNamesInMode builtinMode))
          )
          exprContainsFunctionBranch
          indexedStatements
      signedBindingStatements = collectSignedBindingStatements indexedStatements
      statementsByIndex = Map.fromList indexedStatements
      predeclaredDataTypes =
        predeclareScopeDataTypes indexedStatements initialState
      scopePreparation =
        prepareScope scopeForwardSignedFunctionsPolicy mode predeclaredDataTypes indexedStatements initialState
      bindingSeedsByStatement = preparedBindingSeeds scopePreparation
      preparedSignaturesByStatement = preparedSignatures scopePreparation
      forwardFunctionBindings = preparedForwardFunctions scopePreparation
      forwardAnalysisBindings =
        Map.fromList
          [ (statementIndex, (bindingName, coreNodeSpan bindingNode))
          | (statementIndex, SLet bindingNode bindingName _) <- indexedStatements,
            Map.member statementIndex forwardFunctionBindings
          ]
      stateAfterBindingSeeds = preparedScopeState scopePreparation
      initialModuleBaselineFacts = capabilityFactsFromState initialState

      go :: ScopeWalkState -> [(Int, Statement 'Resolved)] -> (Maybe ExpressionType, InferState, [ProvisionalTypedStatement], [InferredProductionFailure])
      go walkState remainingStatements =
        case remainingStatements of
          [] -> (scopeWalkLastExprType walkState, publishVisibleTypes (scopeWalkEnv walkState) (scopeWalkInferState walkState), [], [])
          (statementIndex, statement) : rest ->
            let env = scopeWalkEnv walkState
                envFreeVariables = scopeWalkEnvFreeVariables walkState
                pendingSignatureType = scopeWalkPendingSignature walkState
                pendingSignaturesByStatement = scopeWalkPendingSignaturesByStatement walkState
                recursiveGroupStartStates = scopeWalkRecursiveGroupStartStates walkState
                recursiveGroupPreviewCache = scopeWalkRecursiveGroupPreviewCache walkState
                moduleBaselineFacts = scopeWalkModuleBaselineFacts walkState
                state = scopeWalkInferState walkState
                stateForSource = setStatementRuntimeHintPath preludeStatementIndices statementIndex state
             in case statement of
                  SModule _ modulePath ->
                    go
                      walkState
                        { scopeWalkRecursiveGroupPreviewCache = Map.empty,
                          scopeWalkInferState = enterModuleCapabilityScope moduleBaselineFacts modulePath state
                        }
                      rest
                  SImport _ modulePath maybeAlias maybeSymbolNames ->
                    go
                      walkState
                        { scopeWalkRecursiveGroupPreviewCache = Map.empty,
                          scopeWalkInferState = importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state
                        }
                      rest
                  SClass classNode capabilityName parameters methods ->
                    let validationState =
                          seedStatementCapabilityFact
                            stateForSource
                            (SClass classNode capabilityName parameters [])
                        maybeInvalidMethod =
                          firstInvalidClassMethodSignature validationState capabilityName parameters methods
                        nextState =
                          case maybeInvalidMethod of
                            Just diagnostic -> addTypeError stateForSource diagnostic
                            Nothing -> seedStatementCapabilityFact stateForSource statement
                        nextModuleBaselineFacts =
                          updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                        (scopeResultType, resultState, provisionalRest, productionFailures) =
                          go
                            walkState
                              { scopeWalkPendingSignature = Nothing,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkModuleBaselineFacts = nextModuleBaselineFacts,
                                scopeWalkInferState = nextState
                              }
                            rest
                        provisional =
                          case mode of
                            ProduceTypedCoreExpressionDirectCall ->
                              ProvisionalUnsupportedStatement
                                statementIndex
                                TypedCoreUnsupportedRootExpression
                                TypedCoreUnsupportedRootDetail
                                []
                                : provisionalRest
                            InferenceOnly -> provisionalRest
                     in (scopeResultType, resultState, provisional, productionFailures)
                  SImpl implNode capabilityName arguments methods ->
                    let maybeInvalidTarget = firstInvalidImplTarget stateForSource (coreNodeSpan implNode) arguments
                        (nextState, implMethodResults) =
                          case maybeInvalidTarget of
                            Just diagnostic -> (addTypeError stateForSource diagnostic, [])
                            Nothing ->
                              let implSeededState = seedStatementCapabilityFact stateForSource statement
                               in checkImplMethodBodies
                                    (inferExprTypeWithExpectedMode inferExpression mode)
                                    inferredExpressionType
                                    builtinMode
                                    env
                                    implSeededState
                                    capabilityName
                                    arguments
                                    methods
                        implMethodFailures =
                          [ InferredProductionFailure (methodIndex : childPath) kind detail
                          | (methodIndex, methodResult) <- implMethodResults,
                            InferredProductionFailure childPath kind detail <- inferredProductionFailures methodResult
                          ]
                        nextModuleBaselineFacts =
                          updateRootModuleBaselineFacts moduleBaselineFacts state nextState
                        (scopeResultType, resultState, provisionalRest, restProductionFailures) =
                          go
                            walkState
                              { scopeWalkPendingSignature = Nothing,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkModuleBaselineFacts = nextModuleBaselineFacts,
                                scopeWalkInferState = nextState
                              }
                            rest
                        provisional =
                          case mode of
                            ProduceTypedCoreExpressionDirectCall ->
                              ProvisionalUnsupportedStatement
                                statementIndex
                                TypedCoreUnsupportedRootExpression
                                TypedCoreUnsupportedRootDetail
                                implMethodFailures
                                : provisionalRest
                            InferenceOnly -> provisionalRest
                        productionFailures =
                          qualifyStatementProductionFailures statementIndex implMethodFailures
                            <> restProductionFailures
                     in (scopeResultType, resultState, provisional, productionFailures)
                  SData dataNode typeName typeParameters constructors ->
                    let dataTypeAlreadyDeclared =
                          Map.member (identifierText typeName) (inferDataTypes state)
                        (nextEnv, nextState) =
                          registerDataConstructors predeclaredDataTypes (coreNodeSpan dataNode) typeName typeParameters constructors env state
                        nextEnvFreeVariables =
                          if dataTypeAlreadyDeclared
                            then envFreeVariables
                            else
                              foldl'
                                (insertRegisteredConstructorFreeVariables nextEnv)
                                envFreeVariables
                                constructors
                        (scopeResultType, resultState, provisionalRest, productionFailures) =
                          go
                            walkState
                              { scopeWalkEnv = nextEnv,
                                scopeWalkEnvFreeVariables = nextEnvFreeVariables,
                                scopeWalkPendingSignature = Nothing,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkInferState = nextState
                              }
                            rest
                        provisional =
                          case mode of
                            ProduceTypedCoreExpressionDirectCall ->
                              case retainedDataDeclaration statementIndex (coreNodeSpan dataNode) typeName typeParameters constructors nextEnv of
                                Just declaration
                                  | not dataTypeAlreadyDeclared -> ProvisionalDataStatement declaration : provisionalRest
                                _ ->
                                  ProvisionalUnsupportedStatement
                                    statementIndex
                                    TypedCoreStructuredValueUnsupported
                                    TypedCoreDataValueDetail
                                    []
                                    : provisionalRest
                            InferenceOnly -> provisionalRest
                     in (scopeResultType, resultState, provisional, productionFailures)
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
                        (scopeResultType, resultState, provisionalRest, productionFailures) =
                          go
                            walkState
                              { scopeWalkPendingSignature = nextPendingSignature,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkInferState = nextState
                              }
                            rest
                        provisional =
                          case (mode, nextPendingSignature) of
                            (ProduceTypedCoreExpressionDirectCall, Just pendingSignature)
                              | supportedTypedCoreSignatureType (pendingSignatureDeclaredType pendingSignature) ->
                                  [ProvisionalSignature statementIndex name (coreNodeSpan signatureNode) (pendingSignatureDeclaredType pendingSignature)]
                            _ -> []
                     in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)
                  SLet bindingNode name valueExpr ->
                    let nameText = identifierText name
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
                            bindingNamesByStatement
                            bindingSeedsByStatement
                        envWithBindingSeed =
                          case ( shouldSeedSelfRecursiveFunction statementIndex name envForStatement,
                                 Map.lookup statementIndex bindingSeedsByStatement
                               ) of
                            (True, Just bindingSeed) ->
                              Map.insert name (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                            _ -> envWithRecursiveBindings
                        envWithForwardSignedBindings =
                          case Map.lookup statementIndex forwardFunctionBindings of
                            Nothing -> envWithBindingSeed
                            Just _ ->
                              foldl'
                                ( \currentEnv (forwardStatementIndex, forwardBinding) ->
                                    if forwardStatementIndex > statementIndex
                                      then
                                        Map.insertWith
                                          (\_ existing -> existing)
                                          (forwardFunctionName forwardBinding)
                                          (PlainTypeBinding (forwardFunctionType forwardBinding))
                                          currentEnv
                                      else currentEnv
                                )
                                envWithBindingSeed
                                (Map.toAscList forwardFunctionBindings)
                        envWithPendingSignature =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              Map.insert
                                name
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
                              inferExprTypeWithExpectedMode inferExpression mode builtinMode envWithPendingSignature stateForSignatureCheck expectedValueType valueExpr
                            Nothing ->
                              inferExpression mode builtinMode envWithPendingSignature stateForStatement valueExpr
                        valueProductionFailures =
                          nestedBlockProductionFailures valueExpr rawValueResult
                        rawValueType = inferredExpressionType rawValueResult
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
                                generalizationEnv
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
                        stateAfterRuntimeHint =
                          case runtimeHintForBinding
                            stateAfterDroppedInferredMethodCheck
                            maybeNextBinding
                            nextBindingType of
                            Just runtimeHint ->
                              modifyInferenceOutput
                                ( \output ->
                                    output
                                      { outputRuntimeHints =
                                          Map.insert
                                            (bindingRuntimeHintKeyInModule (inferRuntimeHintPath stateAfterDroppedInferredMethodCheck) name bindingSpan)
                                            runtimeHint
                                            (inferRuntimeTypeHints stateAfterDroppedInferredMethodCheck)
                                      }
                                )
                                stateAfterDroppedInferredMethodCheck
                            Nothing -> stateAfterDroppedInferredMethodCheck
                        stateAfterCapturedConstraintPrune =
                          case maybeNextBinding of
                            Just binding ->
                              pruneCapturedInferredClassConstraints stateForStatement binding stateAfterRuntimeHint
                            Nothing -> stateAfterRuntimeHint
                        nextPendingSignaturesByStatement =
                          case matchingPendingSignature of
                            Just pendingSignature ->
                              Map.insert statementIndex pendingSignature pendingSignaturesByStatement
                            Nothing -> pendingSignaturesByStatement
                        nextEnvBeforeRecursiveGroupGeneralization =
                          case maybeNextBinding of
                            Just binding -> Map.insert name binding env
                            Nothing -> env
                        nextEnvFreeVariablesBeforeRecursiveGroupGeneralization =
                          case maybeNextBinding of
                            Just binding -> insertTypeEnvFreeVariables name binding envFreeVariables
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
                        (scopeResultType, resultState, provisionalRest, restProductionFailures) =
                          go
                            walkState
                              { scopeWalkEnv = nextEnv,
                                scopeWalkEnvFreeVariables = nextEnvFreeVariables,
                                scopeWalkPendingSignature = Nothing,
                                scopeWalkPendingSignaturesByStatement = nextPendingSignaturesByStatement,
                                scopeWalkRecursiveGroupStartStates = recursiveGroupStartStatesForStatement,
                                scopeWalkRecursiveGroupPreviewCache = recursiveGroupPreviewCacheAfterStatement,
                                scopeWalkInferState = stateAfterRecursiveGroupPrune
                              }
                            rest
                        canonicalRecursiveGroupMembers =
                          Map.lookup statementIndex recursiveGroupsByStatement
                        callableDeclaration =
                          case nextBindingType of
                            Just bindingType@SemanticFunction {} ->
                              Just
                                ( ProvisionalCallableDeclaration
                                    statementIndex
                                    name
                                    bindingSpan
                                    bindingType
                                    maybeNextBinding
                                    canonicalRecursiveGroupMembers
                                )
                            _ -> Nothing
                        productionValueResult =
                          case nextBindingType of
                            Just bindingType ->
                              specializeInferredExpression
                                stateAfterRecursiveGroupPrune
                                bindingType
                                rawValueResult
                            Nothing -> rawValueResult
                        provisional =
                          case (mode, valueProductionFailures, callableDeclaration, nextBindingType, inferredProvisionalExpr productionValueResult) of
                            (ProduceTypedCoreExpressionDirectCall, _, Just declaration, _, Just expression)
                              | ProvisionalLambdaExpression {} <- expression ->
                                  [ProvisionalFunctionBinding declaration expression]
                            (ProduceTypedCoreExpressionDirectCall, failures, Just declaration, _, _) ->
                              [ ProvisionalUnsupportedCallableBinding
                                  declaration
                                  TypedCoreUnsupportedRootExpression
                                  TypedCoreUnsupportedRootDetail
                                  [ InferredProductionFailure (0 : childPath) kind detail
                                  | InferredProductionFailure childPath kind detail <- failures
                                  ]
                              ]
                            (ProduceTypedCoreExpressionDirectCall, [], Nothing, Just bindingType, Just expression)
                              | not (isFunctionType bindingType) ->
                                  [ProvisionalScalarBinding statementIndex name bindingSpan bindingType expression]
                            (ProduceTypedCoreExpressionDirectCall, failures@(_ : _), _, _, _) ->
                              [ ProvisionalUnsupportedStatement
                                  statementIndex
                                  TypedCoreUnsupportedRootExpression
                                  TypedCoreUnsupportedRootDetail
                                  [ InferredProductionFailure (0 : childPath) kind detail
                                  | InferredProductionFailure childPath kind detail <- failures
                                  ]
                              ]
                            (ProduceTypedCoreExpressionDirectCall, [], _, _, _) ->
                              [ ProvisionalUnsupportedStatement
                                  statementIndex
                                  TypedCoreUnsupportedRootExpression
                                  TypedCoreUnsupportedRootDetail
                                  []
                              ]
                            _ -> []
                        productionFailures =
                          qualifyStatementProductionFailures statementIndex valueProductionFailures
                            <> restProductionFailures
                     in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)
                  SExpr exprNode expr ->
                    let exprSpan = coreNodeSpan exprNode
                        (envForStatement, stateForStatement, _) =
                          exposeVisibleRecursiveGroupSchemes statementIndex env envFreeVariables stateForSource recursiveGroupPreviewCache
                        (exprResult, rawStateAfterExpr) = inferExpression mode builtinMode envForStatement stateForStatement expr
                        expressionProductionFailures =
                          nestedBlockProductionFailures expr exprResult
                        exprType = inferredExpressionType exprResult
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
                                envForStatement
                                stateForStatement
                                stateAfterExplicitConstraintCheck
                                resultType
                                Set.empty
                            Nothing -> stateAfterExplicitConstraintCheck
                        (scopeResultType, resultState, provisionalRest, restProductionFailures) =
                          go
                            walkState
                              { scopeWalkLastExprType = exprType,
                                scopeWalkPendingSignature = Nothing,
                                scopeWalkRecursiveGroupPreviewCache = Map.empty,
                                scopeWalkInferState = stateAfterDroppedInferredMethodCheck
                              }
                            rest
                        provisional =
                          case (mode, expressionProductionFailures, inferredProvisionalExpr exprResult) of
                            (ProduceTypedCoreExpressionDirectCall, failures@(_ : _), Just ProvisionalScopeStatements {}) ->
                              [ ProvisionalTerminalExpression
                                  statementIndex
                                  exprSpan
                                  (ProvisionalRetainedFailures failures)
                              ]
                            (ProduceTypedCoreExpressionDirectCall, _, Just expression) ->
                              [ProvisionalTerminalExpression statementIndex exprSpan expression]
                            (ProduceTypedCoreExpressionDirectCall, _, Nothing) ->
                              [ ProvisionalUnsupportedStatement
                                  statementIndex
                                  TypedCoreUnsupportedRootExpression
                                  TypedCoreUnsupportedRootDetail
                                  []
                              ]
                            _ -> []
                        productionFailures =
                          qualifyStatementProductionFailures statementIndex expressionProductionFailures
                            <> restProductionFailures
                     in (scopeResultType, resultState, provisional <> provisionalRest, productionFailures)

      nestedBlockProductionFailures expression result =
        if mode /= ProduceTypedCoreExpressionDirectCall
          then inferredProductionFailures result
          else case expression of
            EBlock _ blockStatements ->
              case blockProductionFailureKindAndDetail blockStatements of
                (TypedCoreStructuredValueUnsupported, _) ->
                  inferredProductionFailures result
                (failureKind, failureDetail) ->
                  InferredProductionFailure
                    []
                    failureKind
                    failureDetail
                    : inferredProductionFailures result
            _ -> inferredProductionFailures result

      supportedTypedCoreSignatureType expressionType =
        case expressionType of
          SemanticFunction {} -> True
          SemanticInt -> True
          SemanticFloat -> True
          SemanticNumeric {} -> True
          SemanticBool -> True
          SemanticChar -> True
          SemanticTuple [] -> True
          _ -> False

      isFunctionType expressionType =
        case expressionType of
          SemanticFunction {} -> True
          _ -> False

      qualifyStatementProductionFailures statementIndex failures =
        [ InferredProductionFailure (statementIndex : childPath) kind detail
        | InferredProductionFailure childPath kind detail <- failures
        ]

      builtinOperatorSymbolExpr :: TypeEnv -> Expr 'Resolved -> Maybe (Text, Maybe TypeScheme)
      builtinOperatorSymbolExpr currentEnv expression =
        case expression of
          EOperatorValue _ operatorSymbol
            | isBuiltinOperatorSymbol operatorSymbol ->
                Just (operatorSymbol, Nothing)
          EApply _ dollarExpr operatorExpr
            | builtinDollarOperatorExpr currentEnv dollarExpr ->
                builtinOperatorSymbolExpr currentEnv operatorExpr
          EVar _ name ->
            case Map.lookup name currentEnv of
              Just (BuiltinOperatorAliasTypeBinding operatorSymbol) ->
                Just (operatorSymbol, Nothing)
              Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) ->
                Just (operatorSymbol, Just typeScheme)
              _ -> Nothing
          _ -> Nothing

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
              EOperatorValue _ operatorSymbol
                | isNothing maybePendingSignature,
                  builtinOperatorAliasSymbol operatorSymbol ->
                    Just (operatorAliasBinding operatorSymbol monomorphicBinding)
              EApply _ _ _
                | isNothing maybePendingSignature,
                  Just (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr currentEnv valueExpr ->
                    Just (operatorAliasBinding operatorSymbol (SchemeTypeBinding <$> maybeAliasScheme))
              EVar _ builtinName ->
                let referencedName = identifierText builtinName
                 in case Map.lookup builtinName currentEnv of
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
                        case lookupBuiltinSymbolInMode builtinMode referencedName of
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

      recursiveGroupBindingNames :: [Int] -> Set ResolvedName
      recursiveGroupBindingNames groupMembers =
        Set.fromList
          [ bindingName
          | memberIndex <- groupMembers,
            Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
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
                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
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
                        Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
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
            case Map.lookup memberIndex bindingNamesByStatement of
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
                              Nothing ->
                                (envAcc, freeVariablesAcc, stateAcc, cacheAcc)
                              Just previewState ->
                                let groupBindingNames =
                                      Set.fromList
                                        [ bindingName
                                        | memberIndex <- groupMembers,
                                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
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
                                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
                                          latestBindingIndexBefore statementIndex bindingName == Just memberIndex,
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
                                    nextState = rollbackPreviewState stateAcc previewState
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
                case Map.lookup memberIndex bindingNamesByStatement of
                  Just bindingName
                    | latestBindingIndexBefore currentStatementIndex bindingName == Just memberIndex ->
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
        case Map.lookup statementIndex statementsByIndex of
          Just (SLet _ bindingName _) ->
            any
              (laterGroupMemberReferences bindingName)
              (filter (> statementIndex) groupMembers)
          _ -> False

      laterGroupMemberReferences :: ResolvedName -> Int -> Bool
      laterGroupMemberReferences bindingName memberIndex =
        maybe
          False
          (Set.member bindingName)
          (Map.lookup memberIndex previewGroupFreeNamesByStatement)

      laterGroupMemberDependsOnInterveningBinding :: Int -> [Int] -> Bool
      laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
        any memberDependsOnInterveningBinding (filter (> statementIndex) groupMembers)
        where
          groupMemberSet = Set.fromList groupMembers

          memberDependsOnInterveningBinding memberIndex =
            maybe
              False
              (any (interveningBindingIsReferenced memberIndex) . Set.toList)
              (Map.lookup memberIndex previewGroupFreeNamesByStatement)

          interveningBindingIsReferenced memberIndex bindingName =
            case Map.lookup bindingName bindingIndicesByName of
              Nothing -> False
              Just bindingIndices ->
                hasInterveningBindingAfter statementIndex bindingIndices
            where
              hasInterveningBindingAfter lowerBound bindingIndices =
                case Set.lookupGT lowerBound bindingIndices of
                  Just bindingIndex
                    | bindingIndex < memberIndex ->
                        Set.notMember bindingIndex groupMemberSet
                          || hasInterveningBindingAfter bindingIndex bindingIndices
                  _ -> False

      previewRecursiveGroupState :: TypeEnv -> InferState -> Int -> [Int] -> Maybe InferState
      previewRecursiveGroupState currentEnv state statementIndex groupMembers =
        let previewState = foldl' previewMember state (filter (> statementIndex) groupMembers)
         in if previewIntroducedDiagnostics state previewState
              then Nothing
              else Just (discardPreviewOutput state previewState)
        where
          previewMember stateAcc memberIndex =
            case Map.lookup memberIndex statementsByIndex of
              Just (SLet bindingNode bindingName valueExpr) ->
                let nameText = identifierText bindingName
                    bindingSpan = coreNodeSpan bindingNode
                    envWithRecursiveBindings =
                      recursiveBindingEnv
                        memberIndex
                        currentEnv
                        recursiveGroupsByStatement
                        bindingNamesByStatement
                        bindingSeedsByStatement
                    envWithBindingSeed =
                      case ( shouldSeedSelfRecursiveFunction memberIndex bindingName currentEnv,
                             Map.lookup memberIndex bindingSeedsByStatement
                           ) of
                        (True, Just bindingSeed) ->
                          Map.insert bindingName (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                        _ -> envWithRecursiveBindings
                    (valueResult, rawStateAfterValue) =
                      inferExpression InferenceOnly builtinMode envWithBindingSeed stateAcc valueExpr
                    valueType = inferredExpressionType valueResult
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

          discardPreviewOutput originalState previewState =
            modifyInferenceOutput
              ( \output ->
                  output
                    { outputErrorsRev = inferErrorsRev originalState,
                      outputRuntimeHints = inferRuntimeTypeHints originalState,
                      outputDeferredConstraints = outputDeferredConstraints (inferOutput originalState),
                      outputInferredConstraints = inferInferredClassConstraints originalState,
                      outputInferredConstraintCount = inferInferredClassConstraintCount originalState
                    }
              )
              previewState

          previewIntroducedDiagnostics originalState previewState =
            length (inferErrorsRev previewState) /= length (inferErrorsRev originalState)

      -- Preview inference is a transaction: its resolved types may be used to
      -- expose a temporary scheme, but none of its semantic state belongs to
      -- the real traversal. Keep only the allocation watermark so type-variable
      -- identifiers embedded in that temporary scheme cannot be reused.
      rollbackPreviewState originalState previewState =
        originalState
          { inferSolver =
              (inferSolver originalState)
                { solverNextTypeVar = solverNextTypeVar (inferSolver previewState)
                }
          }

      shouldSeedSelfRecursiveFunction :: Int -> ResolvedName -> TypeEnv -> Bool
      shouldSeedSelfRecursiveFunction statementIndex bindingName visibleEnv =
        Set.member statementIndex selfRecursiveFunctionStatements
          && Map.notMember bindingName visibleEnv

      exposePreviewRecursiveGroupMember :: Int -> TypeEnv -> Set InferenceVariable -> InferState -> (TypeEnv, TypeEnvFreeVariables) -> Int -> (TypeEnv, TypeEnvFreeVariables)
      exposePreviewRecursiveGroupMember statementIndex envOutsideGroup environmentVariables state (currentEnv, currentFreeVariables) memberIndex =
        case Map.lookup memberIndex bindingNamesByStatement of
          Just bindingName
            | latestBindingIndexBefore statementIndex bindingName == Just memberIndex ->
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

      latestBindingIndexBefore :: Int -> ResolvedName -> Maybe Int
      latestBindingIndexBefore statementIndex bindingName =
        Map.lookup bindingName bindingIndicesByName
          >>= Set.lookupLT statementIndex

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
        case (Map.lookup memberIndex statementsByIndex, Map.lookup memberIndex bindingNamesByStatement) of
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

data ForwardFunctionBinding = ForwardFunctionBinding
  { forwardFunctionName :: ResolvedName,
    forwardFunctionType :: ExpressionType
  }

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

data ScopePreparation = ScopePreparation
  { preparedBindingSeeds :: Map Int ExpressionType,
    preparedSignatures :: Map Int PreparedSignature,
    preparedForwardFunctions :: Map Int ForwardFunctionBinding,
    preparedScopeState :: InferState
  }

prepareScope ::
  ForwardSignedFunctionsPolicy ->
  TypedCoreProductionMode ->
  Map Text DataTypeBinding ->
  [(Int, Statement 'Resolved)] ->
  InferState ->
  ScopePreparation
prepareScope forwardSignedFunctionsPolicy mode predeclaredDataTypes indexedStatements initialState =
  let (bindingSeeds, signatures, forwardFunctions, _, _, finalPreparationState) =
        foldl'
          step
          ( Map.empty,
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
          preparedScopeState =
            initialState
              { inferSolver =
                  (inferSolver initialState)
                    { solverNextTypeVar = solverNextTypeVar (inferSolver finalPreparationState)
                    }
              }
        }
  where
    step
      (bindingSeeds, signatures, forwardFunctions, pendingSignature, moduleBaselineFacts, state)
      (statementIndex, statement) =
        case statement of
          SModule _ modulePath ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
              Nothing,
              moduleBaselineFacts,
              enterModuleCapabilityScope moduleBaselineFacts modulePath state
            )
          SImport _ modulePath maybeAlias maybeSymbolNames ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
              Nothing,
              moduleBaselineFacts,
              importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state
            )
          SClass classNode capabilityName parameters methods ->
            let validationState =
                  seedStatementCapabilityFact
                    state
                    (SClass classNode capabilityName parameters [])
                nextState =
                  case firstInvalidClassMethodSignature validationState capabilityName parameters methods of
                    Just _ -> state
                    Nothing -> seedStatementCapabilityFact state statement
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
                  Nothing,
                  updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
                  nextState
                )
          SImpl implNode _capabilityName arguments _ ->
            let nextState =
                  case firstInvalidImplTarget state (coreNodeSpan implNode) arguments of
                    Just _ -> state
                    Nothing -> seedStatementCapabilityFact state statement
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
                  Nothing,
                  updateRootModuleBaselineFacts moduleBaselineFacts state nextState,
                  nextState
                )
          SData dataNode typeName typeParameters constructors ->
            let (_, nextState) =
                  registerDataConstructors
                    predeclaredDataTypes
                    (coreNodeSpan dataNode)
                    typeName
                    typeParameters
                    constructors
                    Map.empty
                    state
             in ( bindingSeeds,
                  signatures,
                  forwardFunctions,
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
                        mode == ProduceTypedCoreExpressionDirectCall,
                        pendingSignatureName signature == identifierText bindingName,
                        ELambda {} <- bindingExpression,
                        concreteForwardFunctionType (pendingSignatureDeclaredType signature) ->
                          Map.insert
                            statementIndex
                            (ForwardFunctionBinding bindingName (pendingSignatureDeclaredType signature))
                            forwardFunctions
                    _ -> forwardFunctions
             in ( Map.insert statementIndex bindingSeed bindingSeeds,
                  signatures,
                  nextForwardFunctions,
                  Nothing,
                  moduleBaselineFacts,
                  nextState
                )
          _ ->
            ( bindingSeeds,
              signatures,
              forwardFunctions,
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
  Map Text DataTypeBinding
predeclareScopeDataTypes indexedStatements initialState =
  foldl' step Map.empty indexedStatements
  where
    step predeclaredDataTypes (_, statement) =
      case statement of
        SData _ typeName typeParameters _
          | Map.notMember typeNameText (inferDataTypes initialState),
            Map.notMember typeNameText predeclaredDataTypes ->
              Map.insert
                typeNameText
                (DataTypeBinding typeParameters [])
                predeclaredDataTypes
          where
            typeNameText = identifierText typeName
        _ -> predeclaredDataTypes

recursiveBindingEnv ::
  Int ->
  TypeEnv ->
  Map Int [Int] ->
  Map Int ResolvedName ->
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
    EVar _ referencedName ->
      case Map.lookup referencedName env of
        Just ConstructorTypeBinding {} -> True
        _ -> False
    _ -> False

generalizedOrdinaryBinding :: Set InferenceVariable -> InferState -> Expr 'Resolved -> ExpressionType -> TypeBinding
generalizedOrdinaryBinding environmentVariables state valueExpr expressionType =
  let resolvedType = bindingTypeForValue state valueExpr expressionType
      schemeVariables = ordinaryBindingSchemeVariables environmentVariables state valueExpr expressionType
      inferredClassConstraints = typeSchemeInferredClassConstraints state schemeVariables
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in if Set.null schemeVariables
        && null inferredClassConstraints
        && null primitiveConstraints
        then PlainTypeBinding resolvedType
        else
          SchemeTypeBinding
            TypeScheme
              { schemeQuantifiedVariables = quantifiedVariablesFromPreferred (expressionTypeVariableOrder resolvedType) schemeVariables,
                schemeClassConstraints = inferredClassConstraints,
                schemePrimitiveConstraints = primitiveConstraints,
                schemeDefiningCapabilities = typeSchemeDefiningFactsFromState state inferredClassConstraints,
                schemeResultType = resolvedType
              }

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
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in if Set.null schemeVariables && null schemeConstraints && null primitiveConstraints
        then PlainTypeBinding resolvedType
        else
          SchemeTypeBinding
            TypeScheme
              { schemeQuantifiedVariables = quantifiedVariablesFromPreferred (pendingSignatureVariableOrder pendingSignature) schemeVariables,
                schemeClassConstraints = schemeConstraints,
                schemePrimitiveConstraints = primitiveConstraints,
                schemeDefiningCapabilities = typeSchemeDefiningFactsFromState state schemeConstraints,
                schemeResultType = resolvedType
              }

addUndeclaredSignatureConstraintErrors :: Text -> InferState -> PendingSignatureType -> InferState -> InferState
addUndeclaredSignatureConstraintErrors bindingName statementStartState pendingSignature state
  | inferErrorCount state > inferErrorCount statementStartState = state
  | otherwise = foldl' addMissingConstraint state missingObligations
  where
    signatureVariables = Set.fromList (pendingSignatureVariableOrder pendingSignature)
    declaredConstraints =
      map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)

    inferredObligations =
      [ (False, constraintName, targetType)
      | constraint <- newInferredClassConstraints statementStartState state,
        Just (constraintName, targetType) <- [constraintIdentity (resolveTypeSchemeConstraint state constraint)],
        targetUsesSignatureVariables targetType
      ]

    primitiveObligations =
      [ case primitiveConstraint of
          TypeSchemeNumericConstraint _ targetType -> (True, "Num", targetType)
          TypeSchemeStrictEqualityConstraint targetType -> (True, "Eq", targetType)
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
              declaredName == requiredName
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
          constraintName == existingName
            && resolveType state targetType == resolveType state existingTarget

    addMissingConstraint stateAcc (primitive, constraintName, targetType) =
      addTypeError
        stateAcc
        ( mkUndeclaredSignatureConstraintError
            bindingName
            primitive
            constraintName
            (resolveType state targetType)
            (pendingSignatureSpan pendingSignature)
        )

pruneCapturedInferredClassConstraints :: InferState -> TypeBinding -> InferState -> InferState
pruneCapturedInferredClassConstraints statementStartState binding =
  pruneCapturedInferredClassConstraintsForBindings statementStartState [binding]

pruneCapturedInferredClassConstraintsForBindings :: InferState -> [TypeBinding] -> InferState -> InferState
pruneCapturedInferredClassConstraintsForBindings statementStartState bindings state =
  if null capturedConstraints
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
      [ resolveTypeSchemeConstraint state constraint
      | binding <- bindings,
        Just typeScheme <- [typeBindingScheme binding],
        constraint <- schemeClassConstraints typeScheme,
        typeSchemeConstraintIsInferred constraint
      ]
    capturedInScheme constraint =
      constraint `elem` capturedConstraints

typeBindingScheme :: TypeBinding -> Maybe TypeScheme
typeBindingScheme binding =
  case binding of
    SchemeTypeBinding typeScheme -> Just typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme -> Just typeScheme
    _ -> Nothing

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
expressionTypeVariableOrder = go
  where
    go expressionType =
      case expressionType of
        SemanticInt -> []
        SemanticFloat -> []
        SemanticNumeric {} -> []
        SemanticBool -> []
        SemanticChar -> []
        SemanticText -> []
        SemanticList elementType ->
          go elementType
        SemanticTuple elementTypes ->
          concatMap go elementTypes
        SemanticData _ typeArguments ->
          concatMap go typeArguments
        SemanticFunction inputType outputType ->
          go inputType ++ go outputType
        SemanticVariable typeVar ->
          [typeVar]

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

    constraintForScheme constraint =
      case constraint of
        TypeSchemeConstraint constraintName argumentType ->
          TypeSchemeConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeInferredConstraint constraintName argumentType ->
          TypeSchemeInferredConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeMethodConstraint constraintName methodKey argumentType ->
          TypeSchemeMethodConstraint constraintName methodKey <$> targetTypeFor argumentType

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
insertRegisteredConstructorFreeVariables env summary (DataConstructor _ constructorName _) =
  case Map.lookup constructorName env of
    Just binding -> insertTypeEnvFreeVariables constructorName binding summary
    Nothing -> summary

registerDataConstructors :: Map Text DataTypeBinding -> SourceSpan -> ResolvedName -> [ResolvedName] -> [DataConstructor 'Resolved] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors predeclaredDataTypes spanValue typeName typeParameters constructors env initialState =
  case Map.lookup typeNameText (inferDataTypes initialState) of
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
                          typeNameText
                          (DataTypeBinding typeParameters (reverse constructorPayloadsRev))
                          (inferDataTypes nextState)
                    }
              )
              nextState
          )

    register (envAcc, stateAcc, constructorPayloadsAcc) (DataConstructor _ constructorName constructorArguments) =
      let (argumentTypes, nextState) =
            constructorArgumentTypes predeclaredDataTypes typeParameters constructorArguments stateAcc
          binding = ConstructorTypeBinding typeName typeParameters argumentTypes
       in ( Map.insert constructorName binding envAcc,
            nextState,
            argumentTypes : constructorPayloadsAcc
          )

retainedDataDeclaration :: Int -> SourceSpan -> ResolvedName -> [ResolvedName] -> [DataConstructor 'Resolved] -> TypeEnv -> Maybe ProvisionalDataDeclaration
retainedDataDeclaration statementIndex spanValue typeName typeParameters constructors env = do
  retainedConstructors <- traverse retainedConstructor constructors
  pure
    ( ProvisionalDataDeclaration
        statementIndex
        spanValue
        typeName
        typeParameters
        retainedConstructors
    )
  where
    parameterTypes =
      Map.fromList
        [ (identifierText parameterName, SemanticVariable (InferenceVariable (negate position - 1)))
        | (position, parameterName) <- zip [0 :: Int ..] typeParameters
        ]

    retainedConstructor (DataConstructor _ constructorName _) = do
      ConstructorTypeBinding registeredTypeName registeredParameters argumentTypes <- Map.lookup constructorName env
      if registeredTypeName == typeName && registeredParameters == typeParameters
        then ProvisionalConstructorDeclaration constructorName <$> traverse retainedFieldType argumentTypes
        else Nothing

    retainedFieldType argumentType =
      case argumentType of
        ConstructorArgumentMonomorphic expressionType -> Just expressionType
        ConstructorArgumentParameter parameterName -> Map.lookup parameterName parameterTypes
        ConstructorArgumentStructured signatureType -> instantiateConstructorFieldType parameterTypes signatureType
        ConstructorArgumentFresh -> Nothing

constructorArgumentTypes :: Map Text DataTypeBinding -> [ResolvedName] -> [SignatureType 'Resolved] -> InferState -> ([ConstructorArgumentType], InferState)
constructorArgumentTypes predeclaredDataTypes typeParameters fieldTypes initialState =
  let (argumentTypesRev, finalState) =
        foldl' collectField ([], initialState) fieldTypes
   in (reverse argumentTypesRev, finalState)
  where
    signatureVariables =
      Map.fromList
        [ (identifierText parameterName, SemanticVariable (InferenceVariable (negate position - 1)))
        | (position, parameterName) <- zip [0 :: Int ..] typeParameters
        ]

    collectField (argumentTypesRev, stateAcc) fieldType =
      case Signature.signatureTypeToExpressionType (stateWithPredeclaredDataTypes stateAcc) signatureVariables fieldType of
        Right _ ->
          ( ConstructorArgumentStructured fieldType : argumentTypesRev,
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

    stateWithPredeclaredDataTypes state =
      modifyDeclarationState
        ( \declarations ->
            declarations
              { declarationDataTypes =
                  Map.union
                    (inferDataTypes state)
                    predeclaredDataTypes
              }
        )
        state

-- | Instantiate non-builtin local bindings and constructors at use sites.
-- Builtin aliases stay with the top-level dispatcher because their rules share
-- the operator and primitive catalog owned there.
instantiateNonBuiltinTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateNonBuiltinTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    SchemeTypeBinding typeScheme ->
      instantiateTypeScheme typeScheme state
    BuiltinAliasTypeBinding {} -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding {} -> (Nothing, state)
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      instantiateTypeScheme typeScheme state
    ConstructorTypeBinding {} ->
      case instantiateConstructorBinding binding state of
        Just (constructorArgumentTypes', constructorResultType, nextState) ->
          ( Just
              (foldr SemanticFunction constructorResultType constructorArgumentTypes'),
            nextState
          )
        Nothing -> (Nothing, state)

instantiateTypeScheme :: TypeScheme -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeScheme typeScheme state =
  let (freshBindings, nextState) =
        foldl'
          allocateFreshBinding
          (Map.empty, state)
          (quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme))
      instantiatedType =
        replaceTypeVariables freshBindings expressionType
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
      stateWithDeferredConstraints =
        deferExplicitConstraintsWithFacts
          (definingFacts <> capabilityFactsFromState state)
          definingFacts
          instantiatedConstraints
          stateWithPrimitiveConstraints
   in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

inferExplicitTypeApplication ::
  InferExprWithModeFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  SourceSpan ->
  SignatureType 'Resolved ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication inferExpression builtinMode env state functionExpr typeArgumentSpan typeArgument =
  let (expressionType, finalState, _) =
        inferExplicitTypeApplicationInternal
          inferExpression
          InferenceOnly
          builtinMode
          env
          state
          functionExpr
          typeArgumentSpan
          typeArgument
   in (expressionType, finalState)

inferExplicitTypeApplicationWithResult ::
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  SourceSpan ->
  SignatureType 'Resolved ->
  (Maybe ExpressionType, InferState, Maybe InferredExpr)
inferExplicitTypeApplicationWithResult inferExpression mode builtinMode env state functionExpr typeArgumentSpan typeArgument =
  inferExplicitTypeApplicationInternal
    inferExpression
    mode
    builtinMode
    env
    state
    functionExpr
    typeArgumentSpan
    typeArgument

inferExplicitTypeApplicationInternal ::
  InferExprWithModeFn ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  SourceSpan ->
  SignatureType 'Resolved ->
  (Maybe ExpressionType, InferState, Maybe InferredExpr)
inferExplicitTypeApplicationInternal inferExpression mode builtinMode env state functionExpr typeArgumentSpan typeArgument =
  case (explicitTypeApplicationScheme env functionExpr, Signature.constraintSignatureTypeToExpressionTypeWithState state Map.empty typeArgument) of
    (_, Just explicitArgumentType)
      | Just methodKey <- explicitQualifiedMethodTypeApplicationKey env state functionExpr ->
          let (maybeInstantiatedType, nextState) =
                instantiateQualifiedMethodTypeWithExplicitTarget methodKey explicitArgumentType state
           in ( maybeInstantiatedType,
                recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeInstantiatedType nextState,
                Nothing
              )
    (Just typeScheme, Just explicitArgumentType) ->
      let (maybeInstantiatedType, nextState) =
            instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state
       in ( maybeInstantiatedType,
            recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeInstantiatedType nextState,
            Nothing
          )
    (Just _, Nothing) ->
      (Nothing, addTypeError state (mkInvalidExplicitTypeApplicationArgumentError state typeArgumentSpan typeArgument), Nothing)
    (Nothing, _) ->
      let (functionResult, stateAfterFunction) =
            inferExpression mode builtinMode env state functionExpr
       in case inferredExpressionType functionResult of
            Just _ ->
              (Nothing, addTypeError stateAfterFunction mkExplicitTypeApplicationTargetError, Just functionResult)
            Nothing -> (Nothing, stateAfterFunction, Just functionResult)

explicitQualifiedMethodTypeApplicationKey :: TypeEnv -> InferState -> Expr 'Resolved -> Maybe Text
explicitQualifiedMethodTypeApplicationKey env state functionExpr =
  case functionExpr of
    EVar _ name
      | Map.notMember name env,
        qualifiedMethodClassIsVisible methodKey state ->
          Just methodKey
      where
        methodKey = identifierText name
    _ -> Nothing

recordExplicitTypeApplicationRuntimeHint :: SourceSpan -> Maybe ExpressionType -> InferState -> InferState
recordExplicitTypeApplicationRuntimeHint typeArgumentSpan maybeExpressionType state =
  case maybeExpressionType >>= runtimeHintFromExpressionType state of
    Just runtimeHint ->
      modifyInferenceOutput
        ( \output ->
            output
              { outputRuntimeHints =
                  Map.insert
                    (explicitTypeApplicationRuntimeHintKeyInModule (inferRuntimeHintPath state) typeArgumentSpan)
                    runtimeHint
                    (inferRuntimeTypeHints state)
              }
        )
        state
    Nothing -> state

explicitTypeApplicationScheme :: TypeEnv -> Expr 'Resolved -> Maybe TypeScheme
explicitTypeApplicationScheme env functionExpr =
  case functionExpr of
    EVar _ name ->
      Map.lookup name env >>= typeBindingScheme
    EOperatorValue _ operatorSymbol ->
      Map.lookup (operatorBindingName operatorSymbol) env >>= typeBindingScheme
    _ -> Nothing

instantiateTypeSchemeWithExplicitArgument ::
  TypeScheme ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateTypeSchemeWithExplicitArgument typeScheme explicitArgumentType state =
  case quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme) of
    [] ->
      (Nothing, addTypeError state mkExplicitTypeApplicationTargetError)
    explicitTypeVar : remainingTypeVars ->
      let (freshBindings, nextState) =
            foldl'
              allocateFreshBinding
              (Map.singleton explicitTypeVar explicitArgumentType, state)
              remainingTypeVars
          instantiatedType =
            replaceTypeVariables freshBindings expressionType
          instantiatedConstraints =
            map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
          instantiatedPrimitiveConstraints =
            map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
          stateWithPrimitiveConstraints =
            applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
          stateWithDeferredConstraints =
            deferExplicitConstraintsWithFacts
              (definingFacts <> capabilityFactsFromState state)
              definingFacts
              instantiatedConstraints
              stateWithPrimitiveConstraints
       in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    explicitConstraints = schemeClassConstraints typeScheme
    primitiveConstraints = schemePrimitiveConstraints typeScheme
    definingFacts = schemeDefiningCapabilities typeScheme
    expressionType = schemeResultType typeScheme

    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

runtimeHintForBinding :: InferState -> Maybe TypeBinding -> Maybe ExpressionType -> Maybe (SignatureType 'Resolved)
runtimeHintForBinding state maybeBinding maybeExpressionType =
  case maybeBinding >>= runtimeHintForTypeBinding state of
    Just runtimeHint -> Just runtimeHint
    Nothing -> maybeExpressionType >>= runtimeHintFromExpressionType state

runtimeHintForTypeBinding :: InferState -> TypeBinding -> Maybe (SignatureType 'Resolved)
runtimeHintForTypeBinding state binding =
  case binding of
    PlainTypeBinding expressionType ->
      runtimeHintFromExpressionType state expressionType
    SchemeTypeBinding typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      typeSchemeRuntimeHint state typeScheme
    _ -> Nothing

typeSchemeRuntimeHint :: InferState -> TypeScheme -> Maybe (SignatureType 'Resolved)
typeSchemeRuntimeHint state typeScheme =
  case resolvedSchemeType of
    SemanticFunction {} ->
      Signature.expressionTypeToRuntimeTemplate runtimeTemplateVariables resolvedSchemeType
    _ -> Nothing
  where
    expressionType = schemeResultType typeScheme
    resolvedSchemeType =
      defaultLiteralTypes state (resolveType state expressionType)
    orderedVariables =
      quantifiedVariablesOrderedList (schemeQuantifiedVariables typeScheme)
    runtimeTemplateVariables =
      Map.fromList
        [ (typeVar, resolvedAmbientName TypeNamespace (mkIdentifier ("t" <> Text.pack (show position))))
        | (position, typeVar) <- zip [0 :: Int ..] orderedVariables
        ]

runtimeHintFromExpressionType :: InferState -> ExpressionType -> Maybe (SignatureType 'Resolved)
runtimeHintFromExpressionType state expressionType =
  Signature.expressionTypeToRuntimeHint (defaultLiteralTypes state (resolveType state expressionType))
