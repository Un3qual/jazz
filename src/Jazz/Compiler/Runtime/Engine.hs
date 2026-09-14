{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The mutually recursive evaluator machine, scope, forcing, and callable
-- application engine. These responsibilities stay together because each can
-- re-enter the others while evaluating Jazz code.
module Jazz.Compiler.Runtime.Engine
  ( evaluateRuntimeExpressionObserved,
    evaluateRuntimeScopeWithHostRequest,
    evaluateRuntimeScopeWithRequiredHostRequest,
    evaluateRuntimeScopeWithEvaluationHostRequest,
    evaluateRuntimeScopePureRequest,
    prepareRuntimeScope,
    runtimeExprRequiresHost,
    runtimeValueExactlyMatchesConstraint,
    renderRuntimeValue,
    untypedIntMetadata,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
    throwE,
  )
import Control.Monad.Trans.State.Strict
  ( get,
    modify',
    put,
  )
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (runIdentity)
import qualified Data.IntMap.Lazy as LazyIntMap
import Data.List (scanl')
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (coreNodeFacts, coreNodeId),
    CorePhase (..),
    CoreSort (StatementSort),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Statement (..),
    expressionNode,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupKernelBuiltinSymbol,
  )
import Jazz.Compiler.CapabilityFacts
  ( qualifiedMethodKey,
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CoreNodeId, ImplId (..), MethodId (..), ResolvedNodeFacts (resolvedNodeCaptures, resolvedNodeOwner, resolvedNodeReference, resolvedOperatorSpelling), ResolvedReference (..), renderCapabilityMethodKey, resolvedBinderReference, resolvedValueReference)
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity
  ( standaloneModulePath,
  )
import Jazz.Compiler.Name
  ( NameNamespace (..),
    ResolvedName,
    identifierText,
    mkIdentifier,
    renderName,
    resolvedLocalName,
  )
import Jazz.Compiler.Pattern
  ( patternBinderNames,
  )
import Jazz.Compiler.RecursiveBindings (PreparedRecursiveScope, prepareAnalyzedScope, preparedRecursiveScopeStatements, takePreparedScope)
import Jazz.Compiler.Runtime.HostEvaluation
  ( freshDeferredHostScopeId,
    modifyDeferredHostBindingCache,
    modifyRuntimeObservation,
    recordRuntimeProfileOpenWhen,
    recordRuntimeStatisticWhen,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeApplicationKind (..),
    RuntimeBuiltinKind (..),
    RuntimeCallableIdentity (..),
    RuntimeConstructionKind (..),
    RuntimeDeferredCacheKind (..),
    RuntimeHostOperationKind (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    finishRuntimeObservationResult,
    recordRuntimeApplication,
    recordRuntimeBuiltinCall,
    recordRuntimeClosureCreation,
    recordRuntimeConstruction,
    recordRuntimeDeferredCacheOutcome,
    recordRuntimeForcedValue,
    recordRuntimeHostOperation,
    recordRuntimePatternAttempt,
    recordRuntimePatternMatch,
    recordRuntimeProfileClose,
    recordRuntimeTransition,
    restoreRuntimeContinuationDepth,
    runtimeObservationEnabled,
    runtimeObservationProfileEnabled,
    runtimeObservationStatisticsEnabled,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    RuntimeOutcome (..),
    runtimeControlAsDiagnosticResult,
    runtimeControlOutcome,
  )
import Jazz.Compiler.Runtime.Primitives
  ( evalBinary,
    evalBuiltin,
  )
import Jazz.Compiler.Runtime.Request
  ( RuntimeScopeRequest (..),
  )
import Jazz.Compiler.Runtime.ScopePlan
  ( RuntimeScopePlan,
    buildRuntimeScopePlan,
    exprDefinitelyNotFunctionValue,
    runtimeExprRequiresHost,
    runtimeStatementRequiresHost,
    scopePlanBindingIndex,
    scopePlanBindingReferenceAt,
    scopePlanIndexedStatements,
    scopePlanIsHostRecursiveBinding,
    scopePlanIsRecursiveBinding,
    scopePlanIsSelfRecursiveFunction,
    scopePlanModulePathForStatement,
    scopePlanRecursiveGroupAt,
    scopePlanStatementAt,
  )
import Jazz.Compiler.Runtime.Semantics
  ( applyConstructor,
    applyExplicitTypeApplicationResultHint,
    applyRuntimeFunctionArgumentHint,
    applyRuntimeTypeHint,
    attachDefaultBindingIntegerTarget,
    evalNumericConversion,
    explicitTypeApplicationRuntimeFunctionHint,
    explicitTypeApplicationRuntimeValueHint,
    isFunctionValue,
    literalRuntimeValue,
    matchCaseArm,
    numericConversionBuiltinForTarget,
    qualifyRuntimeType,
    renderRuntimeType,
    renderRuntimeValue,
    runtimeDefinitionName,
    runtimeDefinitionNameIn,
    runtimeDiagnostic,
    runtimeFunctionArguments,
    runtimeValueExactlyMatchesConstraint,
    untypedIntMetadata,
  )
import Jazz.Compiler.Runtime.Types
  ( DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
    DeferredHostScopeId (..),
    ModuleEvaluationMode (..),
    RuntimeAnnotation (..),
    RuntimeCell,
    RuntimeClosure (..),
    RuntimeDictionary (..),
    RuntimeEnv,
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
    RuntimeMethodCandidate (..),
    RuntimeValue (..),
    ScopeResult (..),
    appendRuntimeAppliedArgument,
    appendRuntimeMethodCandidate,
    constructorApplicationIsSaturated,
    emptyRuntimeAppliedArguments,
    emptyRuntimeMethodCandidates,
    foldRuntimeExplicitResultHints,
    runtimeConstructorName,
    pattern VQualifiedMethodApplication,
  )
import Jazz.Compiler.RuntimeHost
  ( HostIOFailure (..),
    RuntimeHost (..),
    RuntimeHostExit (..),
    disabledRuntimeHost,
    hostIOCategoryToken,
    hostIOFailureMessage,
  )
import Jazz.Compiler.SemanticFacts
  ( AnalyzedMethodSignature (..),
    AnalyzedScheme (..),
    AnalyzedSchemeConstraint (..),
    AnalyzedType,
    EvidenceReference (..),
    ExpressionFacts (..),
    InstantiationTarget (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner (..))
import Jazz.Compiler.TypeRepresentation
  ( SemanticType (..),
  )

closeRuntimeProfileOnReturn :: Bool -> EvaluationMachine -> EvaluationMachine
closeRuntimeProfileOnReturn enabled machine =
  if enabled
    then appendRuntimeResultObligation CloseRuntimeProfileFrame machine
    else machine

evaluateRuntimeExpressionObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  Expr 'Analyzed ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExpressionObserved observationRequest host expr =
  {-# SCC "jazz-stage:evaluation" #-}
  case observationRequest of
    RuntimeObservationDisabled -> do
      outcome <-
        evaluateRuntimeExpressionUnobserved host expr
      pure (RuntimeObservationResult outcome Nothing)
    _ -> do
      (outcome, observationState) <-
        runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
          evaluateRuntimeExpressionWithRequiredEvaluationHost evaluationHost expr
      pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateRuntimeExpressionUnobserved ::
  (Monad m) =>
  RuntimeHost m ->
  Expr 'Analyzed ->
  m (RuntimeOutcome (Maybe RuntimeValue))
evaluateRuntimeExpressionUnobserved host expr =
  runtimeControlOutcome
    <$> runRuntimeHostEvaluation
      host
      ( \evaluationHost ->
          evaluateRuntimeExpressionWithEvaluationHost evaluationHost expr
      )

evaluateRuntimeExpressionWithRequiredEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Expr 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExpressionWithRequiredEvaluationHost host expr =
  case expr of
    EBlock {} -> case runtimeExpressionScopeRequest expr of
      Left diagnostic -> pure (Left (RuntimeDiagnostic diagnostic))
      Right scopeRequest -> fmap scopeResultValue <$> evaluateRuntimeScopeWithRequiredHostRequest host scopeRequest
    _ ->
      runExceptT
        (Just <$> evalValueWithHost host Nothing Map.empty expr)

evaluateRuntimeExpressionWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Expr 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExpressionWithEvaluationHost host expr =
  if runtimeExprRequiresHost expr
    then case expr of
      EBlock {} -> case runtimeExpressionScopeRequest expr of
        Left diagnostic -> pure (Left (RuntimeDiagnostic diagnostic))
        Right scopeRequest -> fmap scopeResultValue <$> evaluateRuntimeScopeWithRequiredHostRequest host scopeRequest
      _ ->
        runExceptT
          (Just <$> evalValueWithHost host Nothing Map.empty expr)
    else
      pure
        ( case evaluateRuntimeExpressionPure expr of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )

-- | Evaluate an expression, returning a terminal scope value when one exists.
evaluateRuntimeExpressionPure :: Expr 'Analyzed -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpressionPure expr =
  case expr of
    EBlock {} -> do
      scopeRequest <- runtimeExpressionScopeRequest expr
      scopeResultValue <$> evaluateRuntimeScopePureRequest scopeRequest
    _ -> Just <$> evalValue Map.empty expr

prepareRuntimeScope :: Expr 'Analyzed -> Either Diagnostic (PreparedRecursiveScope 'Analyzed)
prepareRuntimeScope = either (Left . runtimeDiagnostic E3020 . Text.pack . show) Right . prepareAnalyzedScope

runtimeExpressionScopeRequest :: Expr 'Analyzed -> Either Diagnostic RuntimeScopeRequest
runtimeExpressionScopeRequest expr = do
  prepared <- prepareRuntimeScope expr
  pure
    RuntimeScopeRequest
      { runtimeScopeEvaluationMode = EvaluateEntryModule,
        runtimeScopeInitialEnvironment = Map.empty,
        runtimeScope = prepared
      }

-- Public scope entry points receive an opaque map whose lazy cells may include
-- recursive blackholes. They cannot safely recover provenance by inspecting
-- values, so only the empty map is known not to contain imported host cells.
opaqueRuntimeEnvironmentMayReachHostCells :: RuntimeEnv -> Bool
opaqueRuntimeEnvironmentMayReachHostCells = not . Map.null

-- | Immutable expression-local inputs for the shared evaluator. Callable
-- transfer replaces the captured environment and module path.
data EvaluationContext = EvaluationContext
  { evaluationModulePath :: Maybe SourceUnitOwner,
    evaluationEnvironment :: RuntimeEnv,
    evaluationClosureBaseName :: Text,
    evaluationLambdaStage :: Int
  }

data RuntimeResultObligation
  = ApplyResultTypeHint AnalyzedType
  | ApplyExplicitResultHint AnalyzedType
  | ApplySelectedEvidence RuntimeEnv (NonEmpty.NonEmpty EvidenceReference)
  | AttachDefaultIntegerResult
  | CloseRuntimeProfileFrame
  deriving (Show)

newtype RuntimeReturnPolicy
  = RuntimeReturnPolicy [RuntimeResultObligation]

data EvaluationControl
  = EvaluateExpression EvaluationContext (Expr 'Analyzed)
  | ApplyCallable RuntimeValue RuntimeValue
  | ForceRuntimeValue RuntimeValue
  | ReturnRuntimeValue RuntimeValue

-- | First-order continuation frames make evaluation order inspectable and
-- keep Jazz recursion on the heap instead of the Haskell call stack.
data EvaluationFrame
  = EvaluateApplicationArgument EvaluationContext (Expr 'Analyzed)
  | ApplyEvaluatedFunction RuntimeValue
  | EvaluateListElement EvaluationContext [RuntimeValue] [Expr 'Analyzed]
  | EvaluateTupleElement EvaluationContext [RuntimeValue] [Expr 'Analyzed]
  | EvaluateIfBranch EvaluationContext (Expr 'Analyzed) (Expr 'Analyzed)
  | EvaluateCaseArms EvaluationContext [CaseArm 'Analyzed]
  | EvaluateCaseGuard EvaluationContext RuntimeValue RuntimeEnv (Expr 'Analyzed) [CaseArm 'Analyzed]
  | EvaluateBuiltinRightOperand EvaluationContext Text (Expr 'Analyzed)
  | ApplyBuiltinBinary Text RuntimeValue
  | EvaluateLeftSection Text
  | EvaluateRightSection Text
  | FinishTypeApplication RuntimeEnv (Maybe SourceUnitOwner) [SemanticInstantiation] [EvidenceReference]
  | ApplyRemainingArguments [RuntimeValue]

data EvaluationContinuation
  = EvaluationContinuation RuntimeReturnPolicy EvaluationFrame

-- | 'evaluationContinuationDepth' is the cached length of
-- 'evaluationContinuations'. All frame pushes and pops must update both fields
-- together so observation can read the depth in constant time.
data EvaluationMachine = EvaluationMachine
  { evaluationControl :: EvaluationControl,
    evaluationContinuations :: [EvaluationContinuation],
    evaluationContinuationDepth :: !Word64,
    -- Compact now, rather than retaining a thunk per tail call until return.
    evaluationReturnPolicy :: !RuntimeReturnPolicy
  }

data EvaluationProgress
  = EvaluationFinished RuntimeValue
  | EvaluationContinues EvaluationMachine

evaluateRuntimeScopeWithRequiredHostRequest ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeScopeRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateRuntimeScopeWithRequiredHostRequest host request =
  runExceptT
    ( evalScopeWithHost
        host
        evaluationMode
        initialEnv
        preparedScope
    )
  where
    evaluationMode = runtimeScopeEvaluationMode request
    initialEnv = runtimeScopeInitialEnvironment request
    preparedScope = runtimeScope request

evaluateRuntimeScopeWithHostRequest ::
  (Monad m) =>
  RuntimeHost m ->
  RuntimeScopeRequest ->
  m (Either Diagnostic ScopeResult)
evaluateRuntimeScopeWithHostRequest host request =
  runtimeControlAsDiagnosticResult
    <$> runRuntimeHostEvaluation
      host
      ( \evaluationHost ->
          evaluateRuntimeScopeWithEvaluationHostRequest evaluationHost request
      )

evaluateRuntimeScopeWithEvaluationHostRequest ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeScopeRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateRuntimeScopeWithEvaluationHostRequest host request =
  if any runtimeStatementRequiresHost statements
    || opaqueRuntimeEnvironmentMayReachHostCells initialEnv
    then
      runExceptT
        ( evalScopeWithHost
            host
            evaluationMode
            initialEnv
            preparedScope
        )
    else
      pure
        ( case evaluateRuntimeScopePureRequest request of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )
  where
    evaluationMode = runtimeScopeEvaluationMode request
    initialEnv = runtimeScopeInitialEnvironment request
    preparedScope = runtimeScope request
    statements = preparedRecursiveScopeStatements preparedScope

-- Storage differs at the forcing boundary; both modes use the same lexical
-- environments and source-order execution below.
data ScopeCellStorage = LazyScopeCells | DeferredScopeCells DeferredHostScopeId

data PreparedRuntimeCells = PreparedRuntimeCells
  { preparedCellPlan :: RuntimeScopePlan,
    preparedFinalEnvironment :: RuntimeEnv,
    preparedEnvironmentBefore :: Int -> RuntimeEnv,
    preparedBindingCellAt :: Int -> RuntimeCell
  }

evaluateRuntimeScopePureRequest :: RuntimeScopeRequest -> Either Diagnostic ScopeResult
evaluateRuntimeScopePureRequest =
  evaluateRuntimeScope LazyScopeCells evalValueWithModulePath id

evaluateRuntimeScope ::
  (Monad m) =>
  ScopeCellStorage ->
  (Maybe SourceUnitOwner -> RuntimeEnv -> Expr 'Analyzed -> m RuntimeValue) ->
  (RuntimeCell -> m RuntimeValue) ->
  RuntimeScopeRequest ->
  m ScopeResult
evaluateRuntimeScope storage evaluateValue forceCell request =
  go Nothing (scopePlanIndexedStatements scopePlan)
  where
    cells = prepareRuntimeCells storage (runtimeScopeInitialEnvironment request) (runtimeScope request)
    scopePlan = preparedCellPlan cells
    go lastValue remaining = case remaining of
      [] -> pure (ScopeResult (preparedFinalEnvironment cells) lastValue)
      (statementIndex, statement) : rest ->
        case (runtimeScopeEvaluationMode request, statement) of
          (EvaluateEntryModule, SLet {}) -> do
            _ <- forceCell (preparedBindingCellAt cells statementIndex)
            go Nothing rest
          (EvaluateEntryModule, SExpr _ expr) -> do
            value <- evaluateValue (scopePlanModulePathForStatement scopePlan statementIndex) (preparedEnvironmentBefore cells statementIndex) expr
            go (Just value) rest
          _ -> go Nothing rest

prepareRuntimeCells :: ScopeCellStorage -> RuntimeEnv -> PreparedRecursiveScope 'Analyzed -> PreparedRuntimeCells
-- Binding cells capture environments that themselves contain the cells.
-- The lazy maps tie this recursive knot without forcing a binding before its
-- environment exists; cycle detection remains in the cell evaluation paths.
prepareRuntimeCells storage initialEnv preparedScope =
  PreparedRuntimeCells scopePlan finalEnvironment envBefore bindingCellAt
  where
    scopePlan = buildRuntimeScopePlan preparedScope
    indexedStatements = scopePlanIndexedStatements scopePlan
    bindingCells =
      LazyIntMap.fromDistinctAscList
        [ (statementIndex, cellForBinding statementIndex node name valueExpr)
        | (statementIndex, SLet node name valueExpr) <- indexedStatements
        ]
    prefixEnvironments =
      LazyIntMap.fromDistinctAscList
        (zip [0 ..] (scanl' extendPrefixEnvironment initialEnv indexedStatements))
    finalEnvironment =
      LazyIntMap.findWithDefault initialEnv (length indexedStatements) prefixEnvironments

    extendPrefixEnvironment env (statementIndex, statement) =
      case statement of
        SLet node name _ ->
          LazyMap.insert (resolvedBinderReference (statementResolution (coreNodeFacts node)) name) (bindingCellAt statementIndex) env
        SData _ _ _ constructors ->
          insertDataConstructors (modulePathForStatement statementIndex) constructors env
        SClass _ capabilityName _ methods _ defaults ->
          insertClassMethods (modulePathForStatement statementIndex) capabilityName methods defaults env
        SImpl implementationNode capabilityName _ methods _ ->
          insertImplMethods (modulePathForStatement statementIndex) implementationNode capabilityName methods env
        _ -> env

    modulePathForStatement :: Int -> Maybe SourceUnitOwner
    modulePathForStatement = scopePlanModulePathForStatement scopePlan

    evalValueAt :: Int -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
    evalValueAt statementIndex =
      evalValueWithModulePath (modulePathForStatement statementIndex)

    bindingCellAt :: Int -> RuntimeCell
    bindingCellAt statementIndex =
      case LazyIntMap.lookup statementIndex bindingCells of
        Just cell -> cell
        Nothing ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: missing binding cell for statement")

    cellForBinding :: Int -> CoreNode 'Analyzed 'StatementSort -> ResolvedName -> Expr 'Analyzed -> RuntimeCell
    cellForBinding statementIndex bindingNode bindingName valueExpr
      | Just (_, scheme) <- statementBinding (coreNodeFacts bindingNode),
        not (null (analyzedSchemeVariables scheme)),
        Just cell <- constrainedCell storage bindingNode bindingName (modulePathForStatement statementIndex) valueExpr (LazyMap.insert (resolvedBinderReference (statementResolution (coreNodeFacts bindingNode)) bindingName) (bindingCellAt statementIndex) (bindingEnv statementIndex)) =
          cell
      | otherwise = case storage of
          LazyScopeCells -> bindingCell statementIndex bindingName valueExpr
          DeferredScopeCells scopeId ->
            Right
              ( VDeferredHostBinding
                  (DeferredHostBindingKey scopeId (coreNodeId bindingNode) bindingName)
                  (recursiveBindingDiagnostic statementIndex valueExpr)
                  (modulePathForStatement statementIndex)
                  valueExpr
                  (bindingEnv statementIndex)
              )

    -- Explicit cells detect cycles through their evaluating cache state. Lazy
    -- cells below resolve alias edges before tying a value thunk's knot.
    recursiveBindingDiagnostic statementIndex valueExpr
      | scopePlanIsHostRecursiveBinding scopePlan statementIndex =
          runtimeDiagnostic E3021 "runtime recursive host binding has no concrete value"
      | scopePlanIsRecursiveBinding scopePlan statementIndex,
        not (exprDefinitelyNotFunctionValue valueExpr) =
          runtimeDiagnostic E3021 "runtime recursive alias cycle has no concrete value"
      | otherwise =
          runtimeDiagnostic E3021 "runtime recursive binding has no concrete value"

    bindingCell :: Int -> ResolvedName -> Expr 'Analyzed -> RuntimeCell
    bindingCell statementIndex bindingName valueExpr =
      case selectedRecursiveAliasTarget statementIndex visibleEnv valueExpr of
        Left diagnostic ->
          Left diagnostic
        Right (Just targetIndex) ->
          case resolveRecursiveAliasTarget (Set.singleton statementIndex) targetIndex of
            Left diagnostic -> Left diagnostic
            Right resolvedTargetIndex -> bindingCellAt resolvedTargetIndex
        Right Nothing
          | scopePlanIsRecursiveBinding scopePlan statementIndex,
            exprDefinitelyNotFunctionValue valueExpr ->
              Left (runtimeDiagnostic E3021 "runtime recursive binding has no concrete value")
          | otherwise ->
              do
                evaluatedValue <- evalBindingValue statementIndex bindingName visibleEnv valueExpr
                Right (attachSelfRecursiveBinding statementIndex evaluatedValue)
      where
        visibleEnv = bindingEnv statementIndex

    evalBindingValue :: Int -> ResolvedName -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      nameRuntimeClosureBinding
        (modulePathForStatement statementIndex)
        bindingName
        <$> evalValueWithModulePath
          (modulePathForStatement statementIndex)
          env
          valueExpr

    -- Alias bridges can legitimately point across a recursive SCC, but pure
    -- alias loops need a deterministic diagnostic instead of infinite forcing.
    resolveRecursiveAliasTarget :: Set Int -> Int -> Either Diagnostic Int
    resolveRecursiveAliasTarget visited statementIndex
      | Set.member statementIndex visited =
          Left (runtimeDiagnostic E3021 "runtime recursive alias cycle has no concrete value")
      | otherwise =
          case scopePlanStatementAt scopePlan statementIndex of
            Just (SLet _ _ aliasExpr) ->
              case selectedRecursiveAliasTarget statementIndex (bindingEnv statementIndex) aliasExpr of
                Left diagnostic ->
                  Left diagnostic
                Right (Just nextTargetIndex) ->
                  resolveRecursiveAliasTarget (Set.insert statementIndex visited) nextTargetIndex
                Right Nothing ->
                  Right statementIndex
            Just _ ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: expected binding statement while resolving alias")
            Nothing ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: missing binding statement while resolving alias")

    bindingEnv :: Int -> RuntimeEnv
    bindingEnv statementIndex =
      case scopePlanBindingReferenceAt scopePlan statementIndex of
        Nothing -> peerVisibleEnv
        Just reference -> case functionSelfReferenceCell statementIndex of
          Just selfCell -> LazyMap.insert reference selfCell peerVisibleEnv
          Nothing
            | recursiveBindingNeedsSelf statementIndex ->
                LazyMap.insert reference (bindingCellAt statementIndex) peerVisibleEnv
            | otherwise -> peerVisibleEnv
      where
        peerVisibleEnv = recursivePeerEnv statementIndex (envBefore statementIndex)

    functionSelfReferenceCell :: Int -> Maybe RuntimeCell
    functionSelfReferenceCell statementIndex
      | LazyScopeCells <- storage,
        recursiveFunctionNeedsSelf statementIndex =
          Just (Left (runtimeDiagnostic E3021 "runtime recursive binding has no concrete value"))
      | otherwise =
          Nothing

    recursiveFunctionNeedsSelf :: Int -> Bool
    recursiveFunctionNeedsSelf statementIndex =
      scopePlanIsSelfRecursiveFunction scopePlan statementIndex
        && maybe False (`Map.notMember` envBefore statementIndex) (scopePlanBindingReferenceAt scopePlan statementIndex)

    recursiveBindingNeedsSelf :: Int -> Bool
    recursiveBindingNeedsSelf statementIndex =
      -- Function-valued self recursion gets stitched onto the resulting
      -- closure after wrapper evaluation. Pre-seeding `self` here is only
      -- needed for non-function recursive bindings; doing it eagerly for block
      -- alias wrappers can blackhole before the closure is returned.
      case storage of
        LazyScopeCells ->
          scopePlanIsRecursiveBinding scopePlan statementIndex
            && not (scopePlanIsSelfRecursiveFunction scopePlan statementIndex)
        DeferredScopeCells _ ->
          scopePlanIsRecursiveBinding scopePlan statementIndex
            || recursiveFunctionNeedsSelf statementIndex

    -- Wrapper expressions like `if` and `{ g = \(x) -> f x. g. }` should
    -- evaluate to their closure first, then get their own binding stitched
    -- into the captured env without forcing the whole wrapper through a
    -- self-referential scope during evaluation.
    attachSelfRecursiveBinding :: Int -> RuntimeValue -> RuntimeValue
    attachSelfRecursiveBinding statementIndex runtimeValue
      | recursiveFunctionNeedsSelf statementIndex,
        Just reference <- scopePlanBindingReferenceAt scopePlan statementIndex =
          case runtimeValue of
            VClosure closure ->
              VClosure
                closure
                  { runtimeClosureEnvironment =
                      LazyMap.insert
                        reference
                        (bindingCellAt statementIndex)
                        (runtimeClosureEnvironment closure)
                  }
            _ -> runtimeValue
      | otherwise =
          runtimeValue

    recursiveAliasTarget :: Int -> Expr 'Analyzed -> Maybe Int
    recursiveAliasTarget statementIndex valueExpr = do
      node <- case peelSingleExprBlock valueExpr of
        EVar referenceNode _ -> Just referenceNode
        _ -> Nothing
      LexicalReference binder <- resolvedNodeReference (expressionResolution (coreNodeFacts node))
      targetIndex <- scopePlanBindingIndex scopePlan binder
      groupMembers <- scopePlanRecursiveGroupAt scopePlan statementIndex
      if targetIndex `elem` groupMembers then Just targetIndex else Nothing

    -- Preserve wrapper runtime semantics by evaluating the branch condition
    -- first, then following alias resolution only through the selected branch.
    selectedRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTarget statementIndex _ _
      | not (scopePlanIsRecursiveBinding scopePlan statementIndex) = Right Nothing
    selectedRecursiveAliasTarget statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf _ conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget statementIndex env conditionExpr thenExpr elseExpr
        EPatternCase _ scrutineeExpr caseArms -> do
          scrutineeValue <- evalValueAt statementIndex env scrutineeExpr
          selectedArm <-
            selectMatchingCaseArmForAlias
              (modulePathForStatement statementIndex)
              (evalValueAt statementIndex)
              env
              scrutineeValue
              caseArms
          case selectedArm of
            Just (armEnv, bodyExpr) ->
              selectedRecursiveAliasTarget
                statementIndex
                armEnv
                bodyExpr
            Nothing ->
              Right Nothing
        peeledExpr ->
          Right (recursiveAliasTarget statementIndex peeledExpr)

    selectRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed -> Either Diagnostic (Maybe Int)
    selectRecursiveAliasTarget statementIndex env conditionExpr thenExpr elseExpr = do
      condition <- evalValueAt statementIndex env conditionExpr >>= runtimeBoolean "branch condition"
      selectedRecursiveAliasTarget statementIndex env (if condition then thenExpr else elseExpr)

    selectMatchingCaseArmForAlias ::
      Maybe SourceUnitOwner ->
      (RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue) ->
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm 'Analyzed] ->
      Either Diagnostic (Maybe (RuntimeEnv, Expr 'Analyzed))
    selectMatchingCaseArmForAlias patternModulePath evalGuard env scrutineeValue =
      chooseRemainingArm
      where
        chooseRemainingArm [] = Right Nothing
        chooseRemainingArm (caseArm : rest) =
          case matchCaseArm patternModulePath env scrutineeValue caseArm of
            Nothing -> chooseRemainingArm rest
            Just (armEnv, guardExpr, bodyExpr) -> do
              selected <- case guardExpr of
                Nothing -> Right True
                Just conditionExpr -> evalGuard armEnv conditionExpr >>= runtimeBoolean "case guard"
              if selected
                then Right (Just (armEnv, bodyExpr))
                else chooseRemainingArm rest

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr 'Analyzed -> Expr 'Analyzed
    peelSingleExprBlock expr =
      case expr of
        EBlock _ [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    envBefore :: Int -> RuntimeEnv
    envBefore statementIndex =
      LazyIntMap.findWithDefault initialEnv statementIndex prefixEnvironments

    recursivePeerEnv :: Int -> RuntimeEnv -> RuntimeEnv
    recursivePeerEnv statementIndex envBeforeValue =
      case scopePlanRecursiveGroupAt scopePlan statementIndex of
        Nothing -> envBeforeValue
        Just groupMembers ->
          foldl' insertPeer envBeforeValue groupMembers
      where
        insertPeer envAcc peerIndex
          | peerIndex == statementIndex = envAcc
          | otherwise =
              case scopePlanBindingReferenceAt scopePlan peerIndex of
                Just reference
                  | Map.notMember reference envBeforeValue ->
                      LazyMap.insert reference (bindingCellAt peerIndex) envAcc
                _ ->
                  envAcc

    insertDataConstructors :: Maybe SourceUnitOwner -> [DataConstructor 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors definitionModulePath constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor node constructorName _) =
          Map.insert (resolvedBinderReference (statementResolution (coreNodeFacts node)) constructorName) (constructorValue node constructorName) envAcc
        constructorValue node constructorName =
          case statementBinding (coreNodeFacts node) of
            Just (_, scheme)
              | (fields, SemanticData typeName arguments) <- runtimeFunctionArguments (analyzedSchemeType scheme),
                Just parameters <- traverse parameterVariable arguments ->
                  Right
                    ( VConstructor
                        (runtimeDefinitionNameIn TypeNamespace definitionModulePath typeName)
                        parameters
                        (runtimeDefinitionNameIn ConstructorNamespace definitionModulePath constructorName)
                        (map (qualifyRuntimeType definitionModulePath) fields)
                        []
                    )
            _ -> Left (runtimeDiagnostic E3021 "runtime constructor is missing its analyzed scheme")
        parameterVariable (SemanticVariable variable) = Just variable
        parameterVariable _ = Nothing

    insertClassMethods :: Maybe SourceUnitOwner -> ResolvedName -> [ClassMethodSignature 'Analyzed] -> [ImplMethod 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods definitionModulePath capabilityName methods defaults env = methodEnv
      where
        methodEnv = foldl' insertMethod defaultEnv methods
        defaultEnv = foldl' insertDefault env defaults
        insertDefault current (ImplMethod node name body) =
          Map.insert
            (DefaultMethodReference (CapabilityId capabilityName) (mkIdentifier (identifierText name)))
            ( case constrainedCell storage node name definitionModulePath body methodEnv of
                Just cell -> cell
                Nothing -> Left (runtimeDiagnostic E3021 "default method is missing its analyzed scheme")
            )
            current
        insertMethod envAcc (ClassMethodSignature node methodName _) =
          let methodKey = renderCapabilityMethodKey (qualifiedMethodKey capabilityName methodName)
              methodName' = resolvedValueReference (statementResolution (coreNodeFacts node)) methodName
              methodValue = case statementDeclarationFact (coreNodeFacts node) of
                MethodDeclaration _ signature ->
                  Right
                    ( VQualifiedMethodApplication
                        methodKey
                        (analyzedMethodClassParameter signature)
                        (qualifyRuntimeType definitionModulePath (analyzedMethodType signature))
                        emptyRuntimeMethodCandidates
                        emptyRuntimeAppliedArguments
                    )
                _ -> Left (runtimeDiagnostic E3021 "runtime method is missing its analyzed signature")
           in Map.insertWith (\_ existing -> existing) methodName' methodValue envAcc

    insertImplMethods :: Maybe SourceUnitOwner -> CoreNode 'Analyzed 'StatementSort -> ResolvedName -> [ImplMethod 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods methodModulePath implementationNode capabilityName methods env =
      case statementDeclarationFact (coreNodeFacts implementationNode) of
        ImplementationDeclaration _ [implTarget] ->
          methodEnv
          where
            runtimeImplTarget = qualifyRuntimeType methodModulePath implTarget
            methodEnv = foldl' insertCandidate directMethods methodCandidates
            directMethods = LazyMap.fromList [(ImplementationMethodReference identity, cell) | (_, _, RuntimeMethodCandidate EvidenceReference {evidenceMethod = Just identity} cell) <- methodCandidates] <> env
            methodCandidates = suppliedCandidates <> defaultCandidates
            defaultCandidates =
              [ ( CapabilityMethodReference capability member,
                  renderCapabilityMethodKey (capability, member),
                  RuntimeMethodCandidate (runtimeEvidence methodModulePath (coreNodeId implementationNode) capabilityName (resolvedLocalName ValueNamespace member) runtimeImplTarget) cell
                )
              | (DefaultMethodReference capability member, cell) <- Map.toList env,
                capability == CapabilityId capabilityName,
                all (\(ImplMethod _ name _) -> identifierText name /= identifierText member) methods
              ]
            suppliedCandidates =
              map
                ( \(ImplMethod methodNode methodName methodExpr) ->
                    let methodKey = renderCapabilityMethodKey (qualifiedMethodKey capabilityName methodName)
                        methodName' = resolvedValueReference (statementResolution (coreNodeFacts methodNode)) methodName
                        evidence = runtimeEvidence methodModulePath (coreNodeId implementationNode) capabilityName methodName runtimeImplTarget
                     in ( methodName',
                          methodKey,
                          RuntimeMethodCandidate evidence (methodCandidateCell methodNode methodName methodExpr)
                        )
                )
                methods
            methodCandidateCell methodNode methodIdentifier methodExpr =
              fromMaybe
                (Left (runtimeDiagnostic E3021 "implementation method is missing its analyzed scheme"))
                (constrainedCell storage methodNode methodIdentifier methodModulePath methodExpr methodEnv)
            insertCandidate envAcc (methodName, _, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc
        _ -> env
      where
        addMethodCandidate methodCandidate methodCell =
          case methodCell of
            Right (VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs) ->
              Right
                ( VQualifiedMethodApplication
                    methodKey
                    classParameter
                    methodSignature
                    (appendRuntimeMethodCandidate methodCandidate candidates)
                    capturedArgs
                )
            _ -> methodCell

runtimeBoolean :: Text -> RuntimeValue -> Either Diagnostic Bool
runtimeBoolean context runtimeValue =
  case runtimeValue of
    VBool condition -> Right condition
    other -> Left (runtimeDiagnostic E3003 ("runtime " <> context <> " must be Bool, found " <> renderRuntimeType other))

evalValue :: RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
evalValue =
  evalValueWithModulePath Nothing

evalValueWithModulePath :: Maybe SourceUnitOwner -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
evalValueWithModulePath currentModulePath env expr =
  runtimeControlAsDiagnosticResult
    ( runIdentity
        ( runRuntimeHostEvaluation disabledRuntimeHost $ \host ->
            runExceptT
              ( runEvaluationMachine
                  host
                  EvaluationContext
                    { evaluationModulePath = currentModulePath,
                      evaluationEnvironment = env,
                      evaluationClosureBaseName = "<entry>",
                      evaluationLambdaStage = 1
                    }
                  expr
              )
        )
    )

nameRuntimeClosureBinding :: Maybe SourceUnitOwner -> ResolvedName -> RuntimeValue -> RuntimeValue
nameRuntimeClosureBinding currentModulePath bindingName runtimeValue =
  case runtimeValue of
    VClosure closure ->
      VClosure
        closure
          { runtimeClosureCallableIdentity =
              ClosureCallable
                qualifiedBindingName
                1
                (renderName (runtimeClosureParameter closure))
          }
    VAnnotated annotation innerValue ->
      VAnnotated annotation (nameRuntimeClosureBinding currentModulePath bindingName innerValue)
    _ -> runtimeValue
  where
    qualifiedBindingName =
      renderName (runtimeDefinitionName currentModulePath bindingName)

nextClosureOrigin :: RuntimeCallableIdentity -> (Text, Int)
nextClosureOrigin callableIdentity =
  case callableIdentity of
    ClosureCallable baseName stage _ -> (baseName, stage + 1)
    GeneratedCallable name -> ("<" <> name <> ">", 2)
    _ -> ("<entry>", 1)

deferredHostBindingName :: DeferredHostBindingKey -> ResolvedName
deferredHostBindingName (DeferredHostBindingKey _ _ bindingName) = bindingName
deferredHostBindingName (DictionaryBindingKey _ _ bindingName _) = bindingName

throwRuntimeDiagnostic :: (Monad m) => Diagnostic -> ExceptT RuntimeControl m value
throwRuntimeDiagnostic = throwE . RuntimeDiagnostic

liftRuntimeControl :: (Monad m) => Either RuntimeControl value -> ExceptT RuntimeControl m value
liftRuntimeControl result =
  case result of
    Left control -> throwE control
    Right value -> pure value

liftRuntimeResult :: (Monad m) => Either Diagnostic value -> ExceptT RuntimeControl m value
liftRuntimeResult result =
  case result of
    Left diagnostic -> throwRuntimeDiagnostic diagnostic
    Right value -> pure value

runEvaluationMachine ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationContext ->
  Expr 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationMachine host context expression =
  runEvaluationControl
    host
    (EvaluateExpression context expression)

runCallableMachine ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runCallableMachine host functionValue argumentValue =
  runEvaluationControl
    host
    (ApplyCallable functionValue argumentValue)

runEvaluationControl ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationControl ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationControl host initialControl =
  ExceptT $ do
    initialState <- get
    let activeMachineCount = runtimeHostEvaluationActiveMachineCount initialState
        parentContinuationDepth = runtimeHostEvaluationContinuationDepth initialState
        continuationBaseDepth =
          if activeMachineCount == 0
            then 0
            else parentContinuationDepth + 1
        observationState = runtimeHostEvaluationObservation initialState
        observeTransitions = runtimeObservationEnabled observationState
        observeStatistics = runtimeObservationStatisticsEnabled observationState
        observeProfile = runtimeObservationProfileEnabled observationState
        advance machine = do
          let continuationDepth =
                continuationBaseDepth
                  + evaluationContinuationDepth machine
          if observeTransitions
            then
              lift
                ( modify'
                    ( \evaluationState ->
                        evaluationState
                          { runtimeHostEvaluationContinuationDepth = continuationDepth,
                            runtimeHostEvaluationObservation =
                              recordRuntimeTransition
                                continuationDepth
                                (runtimeHostEvaluationObservation evaluationState)
                          }
                    )
                )
            else pure ()
          progress <- stepEvaluationMachine observeStatistics observeProfile host machine
          case progress of
            EvaluationFinished value -> pure value
            EvaluationContinues nextMachine -> advance nextMachine
    if observeTransitions
      then
        put
          initialState
            { runtimeHostEvaluationActiveMachineCount = activeMachineCount + 1
            }
      else pure ()
    result <-
      runExceptT
        ( advance
            EvaluationMachine
              { evaluationControl = initialControl,
                evaluationContinuations = [],
                evaluationContinuationDepth = 0,
                evaluationReturnPolicy = RuntimeReturnPolicy []
              }
        )
    if observeTransitions
      then
        modify'
          ( \evaluationState ->
              evaluationState
                { runtimeHostEvaluationActiveMachineCount = activeMachineCount,
                  runtimeHostEvaluationContinuationDepth = parentContinuationDepth,
                  runtimeHostEvaluationObservation =
                    if activeMachineCount > 0
                      then
                        restoreRuntimeContinuationDepth
                          parentContinuationDepth
                          (runtimeHostEvaluationObservation evaluationState)
                      else runtimeHostEvaluationObservation evaluationState
                }
          )
      else pure ()
    pure result

stepEvaluationMachine ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationMachine ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
stepEvaluationMachine observeStatistics observeProfile host machine =
  case evaluationControl machine of
    EvaluateExpression context expression ->
      stepExpression context expression
    ApplyCallable functionValue argumentValue ->
      stepCallable functionValue argumentValue
    ForceRuntimeValue runtimeValue -> do
      if observeStatistics
        then lift (modifyRuntimeObservation recordRuntimeForcedValue)
        else pure ()
      forcedValue <-
        forceRuntimeValueWithHost host runtimeValue
      continueWith (ReturnRuntimeValue forcedValue) machine
    ReturnRuntimeValue runtimeValue -> do
      dischargedValue <-
        dischargeRuntimeReturnPolicy (evaluationReturnPolicy machine) runtimeValue
      case evaluationContinuations machine of
        [] -> pure (EvaluationFinished dischargedValue)
        EvaluationContinuation parentPolicy frame : rest ->
          resumeEvaluationFrame
            observeStatistics
            observeProfile
            host
            machine
              { evaluationContinuations = rest,
                evaluationContinuationDepth = evaluationContinuationDepth machine - 1,
                evaluationReturnPolicy = parentPolicy
              }
            frame
            dischargedValue
  where
    stepExpression context expression =
      case expression of
        ELit _ literal -> do
          value <- liftRuntimeResult (specializeAnalyzedLiteral facts literal)
          continueWith (ReturnRuntimeValue value) expressionMachine
        EVar node _
          | Just (BuiltinOperatorReference symbol) <- resolvedNodeReference (expressionResolution (coreNodeFacts node)) ->
              continueWith (ReturnRuntimeValue (VOperator symbol [])) expressionMachine
        EVar node name ->
          case Map.lookup (resolvedValueReference (expressionResolution (coreNodeFacts node)) name) (evaluationEnvironment context) of
            Just runtimeCell -> do
              runtimeValue <- liftRuntimeResult runtimeCell
              forceReference runtimeValue
            Nothing ->
              case resolvedNodeReference (expressionResolution (coreNodeFacts node)) >>= kernelReference of
                Just builtinFunction ->
                  continueWith (ReturnRuntimeValue (VBuiltin builtinFunction [])) expressionMachine
                Nothing
                  | Just symbol <- resolvedOperatorSpelling (expressionResolution (coreNodeFacts node)) ->
                      throwRuntimeDiagnostic (runtimeDiagnostic E3027 ("operator '" <> symbol <> "' has no executable binding"))
                Nothing ->
                  throwRuntimeDiagnostic
                    (runtimeDiagnostic E3002 ("runtime unbound variable '" <> identifierText name <> "'"))
        ELambda node parameterName bodyExpr ->
          do
            let capturedNames =
                  Set.fromList (map fst (resolvedNodeCaptures (expressionResolution (coreNodeFacts node))))
                    <> requiredEvidenceReferences (evaluationEnvironment context) bodyExpr
                capturedEnvironment =
                  Map.filterWithKey
                    (\reference _ -> Set.member reference capturedNames)
                    (evaluationEnvironment context)
            recordRuntimeStatisticWhen
              observeStatistics
              (recordRuntimeClosureCreation (Map.size capturedEnvironment))
            continueWith
              ( ReturnRuntimeValue
                  ( VClosure
                      RuntimeClosure
                        { runtimeClosureEnvironment = capturedEnvironment,
                          runtimeClosureParameter = parameterName,
                          runtimeClosureParameterReference = resolvedBinderReference (expressionResolution (coreNodeFacts node)) parameterName,
                          runtimeClosureBody = bodyExpr,
                          runtimeClosureTypeHint = Nothing,
                          runtimeClosureModulePath = evaluationModulePath context,
                          runtimeClosureCallableIdentity =
                            ClosureCallable
                              (evaluationClosureBaseName context)
                              (evaluationLambdaStage context)
                              (renderName parameterName)
                        }
                  )
              )
              expressionMachine
        EOperatorValue {} ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3021 "analyzed expression contains an unresolved operator value")
        EList _ [] ->
          continueWith (ReturnRuntimeValue (VList [] Nothing)) expressionMachine
        EList _ (element : rest) ->
          suspendEvaluation
            expressionMachine
            (EvaluateListElement context [] rest)
            (EvaluateExpression context element)
        ETuple _ [] ->
          do
            recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction TupleConstruction 1)
            continueWith (ReturnRuntimeValue (VTuple [])) expressionMachine
        ETuple _ (element : rest) ->
          suspendEvaluation
            expressionMachine
            (EvaluateTupleElement context [] rest)
            (EvaluateExpression context element)
        EApply _ functionExpr argumentExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateApplicationArgument context argumentExpr)
            (EvaluateExpression context functionExpr)
        ETypeApplication _ functionExpr _ _ ->
          case functionExpr of
            EVar node name ->
              case Map.lookup (resolvedValueReference (expressionResolution (coreNodeFacts node)) name) (evaluationEnvironment context) of
                Just runtimeCell -> do
                  unforcedValue <- liftRuntimeResult runtimeCell
                  case unforcedValue of
                    VQualifiedMethodApplication {} -> forceReference unforcedValue
                    _ -> evaluateTypeApplicationNormally resultMachine context facts functionExpr
                Nothing -> evaluateTypeApplicationNormally resultMachine context facts functionExpr
            _ -> evaluateTypeApplicationNormally resultMachine context facts functionExpr
        EIf _ conditionExpr thenExpr elseExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateIfBranch context thenExpr elseExpr)
            (EvaluateExpression context conditionExpr)
        EPatternCase _ scrutineeExpr caseArms ->
          suspendEvaluation
            expressionMachine
            (EvaluateCaseArms context caseArms)
            (EvaluateExpression context scrutineeExpr)
        EBinary _ operatorSymbol leftExpr rightExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateBuiltinRightOperand context operatorSymbol rightExpr)
            (EvaluateExpression context leftExpr)
        ESectionLeft _ leftExpr operatorSymbol ->
          suspendEvaluation expressionMachine (EvaluateLeftSection operatorSymbol) (EvaluateExpression context leftExpr)
        ESectionRight _ operatorSymbol rightExpr ->
          suspendEvaluation expressionMachine (EvaluateRightSection operatorSymbol) (EvaluateExpression context rightExpr)
        EBlock _ statements -> do
          prepared <- liftRuntimeResult (prepareRuntimeScope expression)
          stepBlock expressionMachine context prepared statements
      where
        modulePath = evaluationModulePath context
        facts = coreNodeFacts (expressionNode expression)
        resultMachine = appendCheckedResult modulePath (expressionResultRepresentation facts) machine
        expressionMachine = appendCheckedEvidence (evaluationEnvironment context) (expressionEvidence facts) resultMachine

        kernelReference (BuiltinReference identifier) = lookupKernelBuiltinSymbol (identifierText identifier)
        kernelReference _ = Nothing

        forceReference runtimeValue
          | null (expressionInstantiations facts),
            null (expressionEvidence facts) =
              continueWith (ForceRuntimeValue runtimeValue) resultMachine
          | isFunctionValue runtimeValue = do
              -- Select nullary methods before forcing can execute their body.
              prepared <- liftRuntimeResult (prepareCheckedCallable (evaluationEnvironment context) modulePath (expressionInstantiations facts) (expressionEvidence facts) runtimeValue)
              continueWith (ForceRuntimeValue prepared) resultMachine
          | otherwise =
              -- Deferred host cells must expose their value before preparation.
              suspendEvaluation
                resultMachine
                (FinishTypeApplication (evaluationEnvironment context) modulePath (expressionInstantiations facts) (expressionEvidence facts))
                (ForceRuntimeValue runtimeValue)

    evaluateTypeApplicationNormally resultMachine context facts functionExpr =
      suspendEvaluation
        resultMachine
        (FinishTypeApplication (evaluationEnvironment context) (evaluationModulePath context) (expressionInstantiations facts) (expressionEvidence facts))
        (EvaluateExpression context functionExpr)

    stepBlock expressionMachine context preparedScope statements =
      case reverse statements of
        SExpr _ terminalExpr : reversedPrefix -> do
          scopeResult <-
            evalScopeWithHost
              host
              EvaluateEntryModule
              (evaluationEnvironment context)
              (takePreparedScope (length reversedPrefix) preparedScope)
          let terminalContext =
                context
                  { evaluationModulePath =
                      Just (resolvedNodeOwner (expressionResolution (coreNodeFacts (expressionNode terminalExpr)))),
                    evaluationEnvironment = scopeResultEnvironment scopeResult
                  }
          continueWith (EvaluateExpression terminalContext terminalExpr) expressionMachine
        _ -> do
          _ <-
            evalScopeWithHost
              host
              EvaluateEntryModule
              (evaluationEnvironment context)
              preparedScope
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3006 "block expression has no terminal expression result at runtime")

    stepCallable functionValue argumentValue = do
      if observeStatistics
        then case runtimeApplicationKind functionValue of
          Nothing -> pure ()
          Just applicationKind ->
            lift (modifyRuntimeObservation (recordRuntimeApplication applicationKind))
        else pure ()
      let maybeCallableIdentity = runtimeCallableIdentity functionValue
      case maybeCallableIdentity of
        Just callableIdentity ->
          recordRuntimeProfileOpenWhen observeProfile callableIdentity
        Nothing -> pure ()
      let profiledMachine =
            case maybeCallableIdentity of
              Just _ -> closeRuntimeProfileOnReturn observeProfile machine
              Nothing -> machine
      case functionValue of
        VDeferredHostBinding {} -> do
          forcedFunctionValue <-
            forceRuntimeValueWithHost host functionValue
          continueWith (ApplyCallable forcedFunctionValue argumentValue) machine
        VAnnotated (RuntimeMethodCall _) innerFunctionValue ->
          continueWith (ApplyCallable innerFunctionValue argumentValue) profiledMachine
        VAnnotated (RuntimeTypeApplication typeHint) innerFunctionValue ->
          case explicitTypeApplicationRuntimeFunctionHint typeHint innerFunctionValue of
            Just instantiatedFunctionHint ->
              continueWith
                (ApplyCallable (VAnnotated (RuntimeTypeHint instantiatedFunctionHint) innerFunctionValue) argumentValue)
                machine
            Nothing ->
              continueWith
                (ApplyCallable innerFunctionValue argumentValue)
                (appendRuntimeResultObligation (ApplyExplicitResultHint typeHint) machine)
        VAnnotated (RuntimeResultHints hints) innerFunctionValue ->
          continueWith
            (ApplyCallable innerFunctionValue argumentValue)
            ( foldRuntimeExplicitResultHints
                ( \hintedMachine typeHint ->
                    appendRuntimeResultObligation
                      (ApplyExplicitResultHint typeHint)
                      hintedMachine
                )
                machine
                hints
            )
        VAnnotated (RuntimeTypeHint typeHint) innerFunctionValue -> do
          hintedArgumentValue <-
            liftRuntimeResult (applyRuntimeFunctionArgumentHint typeHint argumentValue)
          continueWith
            (ApplyCallable innerFunctionValue hintedArgumentValue)
            (appendFunctionResultHint typeHint machine)
        VSectionLeft operatorSymbol leftValue
          | operatorSymbol == "$" ->
              continueWith (ApplyCallable leftValue argumentValue) profiledMachine
          | otherwise -> do
              resultValue <-
                evalBinaryWithHost host operatorSymbol leftValue argumentValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VSectionRight operatorSymbol rightValue
          | operatorSymbol == "$" ->
              continueWith (ApplyCallable argumentValue rightValue) profiledMachine
          | otherwise -> do
              resultValue <-
                evalBinaryWithHost host operatorSymbol argumentValue rightValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VClosure closure -> do
          hintedArgumentValue <-
            case runtimeClosureTypeHint closure of
              Just typeHint ->
                liftRuntimeResult (applyRuntimeFunctionArgumentHint typeHint argumentValue)
              Nothing -> pure argumentValue
          let withResultHint =
                case runtimeClosureTypeHint closure of
                  Just typeHint -> appendFunctionResultHint typeHint
                  Nothing -> appendRuntimeResultObligation AttachDefaultIntegerResult
              (nextClosureBaseName, nextLambdaStage) =
                nextClosureOrigin (runtimeClosureCallableIdentity closure)
              closureContext =
                EvaluationContext
                  { evaluationModulePath = runtimeClosureModulePath closure,
                    evaluationEnvironment =
                      Map.insert
                        (runtimeClosureParameterReference closure)
                        (Right hintedArgumentValue)
                        (runtimeClosureEnvironment closure),
                    evaluationClosureBaseName = nextClosureBaseName,
                    evaluationLambdaStage = nextLambdaStage
                  }
          continueWith
            (EvaluateExpression closureContext (runtimeClosureBody closure))
            (withResultHint profiledMachine)
        VBuiltin builtinFunction capturedArgs -> do
          resultValue <-
            applyBuiltinWithHost
              observeStatistics
              observeProfile
              host
              builtinFunction
              (capturedArgs <> [argumentValue])
          continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VOperator operatorSymbol capturedArgs ->
          case capturedArgs <> [argumentValue] of
            [leftValue] ->
              continueWith
                (ReturnRuntimeValue (VOperator operatorSymbol [leftValue]))
                profiledMachine
            [leftValue, rightValue]
              | operatorSymbol == "$" ->
                  continueWith (ApplyCallable leftValue rightValue) profiledMachine
              | otherwise -> do
                  resultValue <-
                    evalBinaryWithHost
                      host
                      operatorSymbol
                      leftValue
                      rightValue
                  continueWith (ReturnRuntimeValue resultValue) profiledMachine
            _ ->
              throwRuntimeDiagnostic
                (runtimeDiagnostic E3016 ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments"))
        VConstructorApplication shape capturedArgs -> do
          let arguments = appendRuntimeAppliedArgument argumentValue capturedArgs
          resultValue <-
            liftRuntimeResult
              ( applyConstructor
                  shape
                  arguments
              )
          if constructorApplicationIsSaturated shape arguments
            then recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction SaturatedAdtConstruction 1)
            else pure ()
          continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VQualifiedMethodApplication {} -> throwRuntimeDiagnostic inconsistentEvidence
        _ ->
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3008 ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue))

runtimeApplicationKind :: RuntimeValue -> Maybe RuntimeApplicationKind
runtimeApplicationKind runtimeValue =
  case runtimeValue of
    VClosure {} -> Just ClosureApplication
    VBuiltin {} -> Just BuiltinApplication
    VOperator {} -> Just OperatorApplication
    VSectionLeft {} -> Just OperatorApplication
    VSectionRight {} -> Just OperatorApplication
    VConstructorApplication {} -> Just ConstructorApplication
    VAnnotated (RuntimeMethodCall _) _ -> Just MethodApplication
    VQualifiedMethodApplication {} -> Just MethodApplication
    _ -> Nothing

runtimeCallableIdentity :: RuntimeValue -> Maybe RuntimeCallableIdentity
runtimeCallableIdentity runtimeValue =
  case runtimeValue of
    VClosure closure -> Just (runtimeClosureCallableIdentity closure)
    VBuiltin builtinFunction _ ->
      Just (BuiltinCallable (builtinSymbolName builtinFunction))
    VOperator operatorSymbol _ -> Just (OperatorCallable operatorSymbol)
    VSectionLeft operatorSymbol _ -> Just (OperatorCallable operatorSymbol)
    VSectionRight operatorSymbol _ -> Just (OperatorCallable operatorSymbol)
    VConstructorApplication shape _ ->
      Just (ConstructorCallable (renderName (runtimeConstructorName shape)))
    VAnnotated (RuntimeMethodCall methodKey) _ -> Just (MethodCallable methodKey)
    VQualifiedMethodApplication methodKey _ _ _ _ -> Just (MethodCallable methodKey)
    _ -> Nothing

resumeEvaluationFrame ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationMachine ->
  EvaluationFrame ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
resumeEvaluationFrame observeStatistics observeProfile host machine frame runtimeValue =
  case frame of
    EvaluateApplicationArgument context argumentExpr ->
      suspendEvaluation
        machine
        (ApplyEvaluatedFunction runtimeValue)
        (EvaluateExpression context argumentExpr)
    ApplyEvaluatedFunction functionValue ->
      continueWith (ApplyCallable functionValue runtimeValue) machine
    EvaluateListElement context reversedElements remainingElements ->
      case remainingElements of
        [] -> do
          let elements = reverse (runtimeValue : reversedElements)
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction ListCellConstruction (fromIntegral (length elements)))
          continueWith
            (ReturnRuntimeValue (VList elements Nothing))
            machine
        nextElement : rest ->
          suspendEvaluation
            machine
            (EvaluateListElement context (runtimeValue : reversedElements) rest)
            (EvaluateExpression context nextElement)
    EvaluateTupleElement context reversedElements remainingElements ->
      case remainingElements of
        [] -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction TupleConstruction 1)
          continueWith
            (ReturnRuntimeValue (VTuple (reverse (runtimeValue : reversedElements))))
            machine
        nextElement : rest ->
          suspendEvaluation
            machine
            (EvaluateTupleElement context (runtimeValue : reversedElements) rest)
            (EvaluateExpression context nextElement)
    EvaluateIfBranch context thenExpr elseExpr -> do
      condition <- liftRuntimeResult (runtimeBoolean "branch condition" runtimeValue)
      continueWith (EvaluateExpression context (if condition then thenExpr else elseExpr)) machine
    EvaluateCaseArms context caseArms ->
      continueCaseEvaluation observeStatistics machine context runtimeValue caseArms
    EvaluateCaseGuard context scrutineeValue armEnv bodyExpr remainingArms -> do
      condition <- liftRuntimeResult (runtimeBoolean "case guard" runtimeValue)
      if condition
        then continueWith (EvaluateExpression context {evaluationEnvironment = armEnv} bodyExpr) machine
        else continueCaseEvaluation observeStatistics machine context scrutineeValue remainingArms
    EvaluateBuiltinRightOperand context operatorSymbol rightExpr ->
      suspendEvaluation
        machine
        (ApplyBuiltinBinary operatorSymbol runtimeValue)
        (EvaluateExpression context rightExpr)
    ApplyBuiltinBinary operatorSymbol leftValue
      | operatorSymbol == "$" ->
          continueWith (ApplyCallable leftValue runtimeValue) machine
      | otherwise -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeApplication OperatorApplication)
          recordRuntimeProfileOpenWhen observeProfile (OperatorCallable operatorSymbol)
          resultValue <-
            evalBinaryWithHost
              host
              operatorSymbol
              leftValue
              runtimeValue
          continueWith
            (ReturnRuntimeValue resultValue)
            (closeRuntimeProfileOnReturn observeProfile machine)
    EvaluateLeftSection operatorSymbol ->
      continueWith (ReturnRuntimeValue (VSectionLeft operatorSymbol runtimeValue)) machine
    EvaluateRightSection operatorSymbol ->
      continueWith (ReturnRuntimeValue (VSectionRight operatorSymbol runtimeValue)) machine
    FinishTypeApplication env modulePath instantiations evidence -> do
      prepared <- liftRuntimeResult (prepareCheckedCallable env modulePath instantiations evidence runtimeValue)
      continueWith (ReturnRuntimeValue prepared) machine
    ApplyRemainingArguments arguments ->
      applyRemainingArguments machine runtimeValue arguments

continueCaseEvaluation ::
  (Monad m) =>
  Bool ->
  EvaluationMachine ->
  EvaluationContext ->
  RuntimeValue ->
  [CaseArm 'Analyzed] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
continueCaseEvaluation observeStatistics machine context scrutineeValue =
  chooseArm
  where
    chooseArm remainingArms =
      case remainingArms of
        [] ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3022 "pattern case matched no arms")
        caseArm@(CaseArm _ casePattern _ _) : rest -> do
          recordRuntimeStatisticWhen observeStatistics recordRuntimePatternAttempt
          case matchCaseArm
            (evaluationModulePath context)
            (evaluationEnvironment context)
            scrutineeValue
            caseArm of
            Nothing -> chooseArm rest
            Just (armEnv, guardExpr, bodyExpr) -> do
              recordRuntimeStatisticWhen
                observeStatistics
                (recordRuntimePatternMatch (Set.size (patternBinderNames casePattern)))
              let armContext = context {evaluationEnvironment = armEnv}
              case guardExpr of
                Nothing -> continueWith (EvaluateExpression armContext bodyExpr) machine
                Just conditionExpr ->
                  suspendEvaluation
                    machine
                    (EvaluateCaseGuard context scrutineeValue armEnv bodyExpr rest)
                    (EvaluateExpression armContext conditionExpr)

applyRemainingArguments ::
  (Monad m) =>
  EvaluationMachine ->
  RuntimeValue ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
applyRemainingArguments machine functionValue arguments =
  case arguments of
    [] -> continueWith (ReturnRuntimeValue functionValue) machine
    [argumentValue] ->
      continueWith (ApplyCallable functionValue argumentValue) machine
    argumentValue : rest ->
      suspendEvaluation
        machine
        (ApplyRemainingArguments rest)
        (ApplyCallable functionValue argumentValue)

continueWith ::
  (Monad m) =>
  EvaluationControl ->
  EvaluationMachine ->
  ExceptT RuntimeControl m EvaluationProgress
continueWith control machine =
  pure
    ( EvaluationContinues
        machine {evaluationControl = control}
    )

suspendEvaluation ::
  (Monad m) =>
  EvaluationMachine ->
  EvaluationFrame ->
  EvaluationControl ->
  ExceptT RuntimeControl m EvaluationProgress
suspendEvaluation machine frame nestedControl =
  pure
    ( EvaluationContinues
        machine
          { evaluationControl = nestedControl,
            evaluationContinuations =
              EvaluationContinuation (evaluationReturnPolicy machine) frame
                : evaluationContinuations machine,
            evaluationContinuationDepth = evaluationContinuationDepth machine + 1,
            evaluationReturnPolicy = RuntimeReturnPolicy []
          }
    )

appendRuntimeResultObligation :: RuntimeResultObligation -> EvaluationMachine -> EvaluationMachine
appendRuntimeResultObligation obligation machine =
  machine
    { evaluationReturnPolicy =
        prependRuntimeResultObligation obligation (evaluationReturnPolicy machine)
    }

appendFunctionResultHint :: AnalyzedType -> EvaluationMachine -> EvaluationMachine
appendFunctionResultHint typeHint =
  case typeHint of
    SemanticFunction _ resultType -> appendRuntimeResultObligation (ApplyResultTypeHint resultType)
    _ -> id

prependRuntimeResultObligation :: RuntimeResultObligation -> RuntimeReturnPolicy -> RuntimeReturnPolicy
-- An Int result hint already performs Int64 conversion/defaulting. Keep that
-- stronger check when it meets an ordinary integer-defaulting obligation.
prependRuntimeResultObligation AttachDefaultIntegerResult policy@(RuntimeReturnPolicy (ApplyResultTypeHint SemanticInt : _)) = policy
prependRuntimeResultObligation obligation@(ApplyResultTypeHint SemanticInt) (RuntimeReturnPolicy (AttachDefaultIntegerResult : rest)) =
  prependRuntimeResultObligation obligation (RuntimeReturnPolicy rest)
prependRuntimeResultObligation obligation policy@(RuntimeReturnPolicy obligations) =
  case obligations of
    existing : _
      | equivalentIdempotentObligation obligation existing -> policy
    _ -> RuntimeReturnPolicy (obligation : obligations)

equivalentIdempotentObligation :: RuntimeResultObligation -> RuntimeResultObligation -> Bool
equivalentIdempotentObligation leftObligation rightObligation =
  case (leftObligation, rightObligation) of
    (AttachDefaultIntegerResult, AttachDefaultIntegerResult) -> True
    (ApplyResultTypeHint leftHint, ApplyResultTypeHint rightHint) ->
      leftHint == rightHint
    _ -> False

dischargeRuntimeReturnPolicy ::
  (Monad m) =>
  RuntimeReturnPolicy ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
dischargeRuntimeReturnPolicy (RuntimeReturnPolicy obligations) runtimeValue =
  foldM applyObligation runtimeValue obligations
  where
    applyObligation currentValue obligation =
      case obligation of
        ApplyResultTypeHint typeHint ->
          liftRuntimeResult (applyRuntimeTypeHint typeHint currentValue)
        ApplyExplicitResultHint typeHint ->
          liftRuntimeResult (applyExplicitTypeApplicationResultHint typeHint currentValue)
        ApplySelectedEvidence env evidence ->
          liftRuntimeResult (selectRuntimeEvidence env evidence currentValue)
        AttachDefaultIntegerResult ->
          liftRuntimeResult (attachDefaultBindingIntegerTarget currentValue)
        CloseRuntimeProfileFrame -> do
          lift (modifyRuntimeObservation recordRuntimeProfileClose)
          pure currentValue

appendCheckedResult :: Maybe SourceUnitOwner -> Maybe AnalyzedType -> EvaluationMachine -> EvaluationMachine
appendCheckedResult _ Nothing = id
appendCheckedResult _ (Just SemanticInt) = appendRuntimeResultObligation AttachDefaultIntegerResult
appendCheckedResult modulePath (Just semanticType)
  | Foldable.null semanticType = appendRuntimeResultObligation (ApplyResultTypeHint (qualifyRuntimeType modulePath semanticType))
  | otherwise = id

appendCheckedEvidence :: RuntimeEnv -> [EvidenceReference] -> EvaluationMachine -> EvaluationMachine
appendCheckedEvidence env evidence = case NonEmpty.nonEmpty evidence of
  Nothing -> id
  Just selected -> appendRuntimeResultObligation (ApplySelectedEvidence env selected)

specializeAnalyzedLiteral :: ExpressionFacts -> Literal -> Either Diagnostic RuntimeValue
specializeAnalyzedLiteral facts literal = case (literal, expressionSemanticType facts) of
  (LInt {}, SemanticNumeric target) -> convert target
  (LFloat {}, SemanticNumeric target) -> convert target
  _ -> Right value
  where
    value = literalRuntimeValue literal
    convert target = evalNumericConversion (numericConversionBuiltinForTarget target) target value

prepareCheckedCallable :: RuntimeEnv -> Maybe SourceUnitOwner -> [SemanticInstantiation] -> [EvidenceReference] -> RuntimeValue -> Either Diagnostic RuntimeValue
prepareCheckedCallable env modulePath instantiations evidence runtimeValue = do
  instantiated <- foldM applyInstantiation runtimeValue [argument | null evidence, instantiation <- instantiations, LexicalInstantiation {} <- [instantiatedTarget instantiation], argument <- NonEmpty.toList (instantiatedTypes instantiation)]
  maybe (Right instantiated) (\selected -> selectRuntimeEvidence env selected instantiated) (NonEmpty.nonEmpty evidence)
  where
    applyInstantiation value semanticType
      | not (Foldable.null semanticType) = Right value
      | otherwise = applyRuntimeInstantiation (qualifyRuntimeType modulePath semanticType) value

applyRuntimeInstantiation :: AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeInstantiation typeHint runtimeValue
  | isFunctionValue runtimeValue = Right (VAnnotated (RuntimeTypeApplication typeHint) runtimeValue)
  | otherwise =
      applyRuntimeTypeHint
        (fromMaybe typeHint (explicitTypeApplicationRuntimeValueHint typeHint runtimeValue))
        runtimeValue

-- Closure capture consumes the already-selected evidence on checked nodes.
-- A dictionary retains all methods of its selected implementation, including
-- methods used by a default, but unrelated implementations stay outside it.
requiredEvidenceReferences :: RuntimeEnv -> Expr 'Analyzed -> Set ResolvedReference
requiredEvidenceReferences env = expression
  where
    expression expr =
      foldMap evidence (expressionEvidence (coreNodeFacts (expressionNode expr))) <> case expr of
        ELambda _ _ body -> expression body
        EList _ values -> foldMap expression values
        ETuple _ values -> foldMap expression values
        EApply _ function argument -> expression function <> expression argument
        ETypeApplication _ function _ _ -> expression function
        EIf _ condition yes no -> foldMap expression [condition, yes, no]
        EPatternCase _ scrutinee arms -> expression scrutinee <> foldMap arm arms
        EBinary _ _ left right -> expression left <> expression right
        ESectionLeft _ left _ -> expression left
        ESectionRight _ _ right -> expression right
        EBlock _ statements -> foldMap statement statements
        _ -> Set.empty
    arm (CaseArm _ _ guard body) = foldMap expression guard <> expression body
    statement value = case value of
      SLet _ _ body -> expression body
      SExpr _ body -> expression body
      SClass _ _ _ _ _ defaults -> foldMap method defaults
      SImpl _ capability _ methods _ ->
        foldMap method methods <> Map.keysSet (Map.filterWithKey (defaultFor (CapabilityId capability)) env)
      _ -> Set.empty
    method (ImplMethod _ _ body) = expression body
    defaultFor capability (DefaultMethodReference owner _) _ = capability == owner
    defaultFor _ _ _ = False
    evidence reference = case reference of
      ParameterEvidence owner index _ _ _ _ -> Set.singleton (EvidenceParameterReference owner index)
      EvidenceReference {evidenceImplementation = implementation, evidencePrerequisites = prerequisites} ->
        Map.keysSet (Map.filterWithKey (methodFor implementation) env) <> foldMap evidence prerequisites
      PendingEvidence {} -> Set.empty
    methodFor implementation (ImplementationMethodReference (MethodId (owner, _))) _ = implementation == owner
    methodFor _ _ _ = False

resolveRuntimeEvidence :: RuntimeEnv -> EvidenceReference -> Either Diagnostic RuntimeDictionary
resolveRuntimeEvidence env reference = case reference of
  PendingEvidence {} -> Left inconsistentEvidence
  ParameterEvidence owner index path capability member _ -> do
    cell <- maybe (Left inconsistentEvidence) Right (Map.lookup (EvidenceParameterReference owner index) env)
    dictionary <- cell >>= \case VEvidence selected -> Right selected; _ -> Left inconsistentEvidence
    projected <- foldM superclass dictionary path
    case runtimeDictionaryEvidence projected of
      selected@EvidenceReference {evidenceCapability = actual, evidenceImplementation = implementation}
        | actual == capability -> Right projected {runtimeDictionaryEvidence = selected {evidenceMethod = (\method -> MethodId (implementation, method)) <$> member}}
      _ -> Left inconsistentEvidence
  EvidenceReference {evidenceImplementation = implementation, evidencePrerequisites = prerequisites} -> do
    required <- traverse (resolveRuntimeEvidence env) prerequisites
    let methods = Map.fromList [(method, cell) | (ImplementationMethodReference method@(MethodId (owner, _)), cell) <- Map.toList env, owner == implementation]
    pure (RuntimeDictionary reference methods required)
  where
    superclass dictionary capability = case [ prerequisite
                                            | prerequisite <- runtimeDictionaryPrerequisites dictionary,
                                              evidenceCapability (runtimeDictionaryEvidence prerequisite) == capability,
                                              evidenceType (runtimeDictionaryEvidence prerequisite) == evidenceType (runtimeDictionaryEvidence dictionary)
                                            ] of
      selected : _ -> Right selected
      [] -> Left inconsistentEvidence

inconsistentEvidence :: Diagnostic
inconsistentEvidence = runtimeDiagnostic E3026 "missing or inconsistent checked capability evidence"

selectRuntimeEvidence :: RuntimeEnv -> NonEmpty.NonEmpty EvidenceReference -> RuntimeValue -> Either Diagnostic RuntimeValue
selectRuntimeEvidence env references runtimeValue = do
  dictionaries <- traverse (resolveRuntimeEvidence env) (NonEmpty.toList references)
  applyDictionaries dictionaries runtimeValue

applyDictionaries :: [RuntimeDictionary] -> RuntimeValue -> Either Diagnostic RuntimeValue
applyDictionaries dictionaries runtimeValue = case runtimeValue of
  VAnnotated annotation inner -> VAnnotated annotation <$> applyDictionaries dictionaries inner
  VConstrained scopeId owner scheme name modulePath expression captured
    | let indices = runtimeEvidenceIndices scheme,
      length dictionaries >= length indices ->
        let arguments = take (length indices) dictionaries
            extended = Map.fromList [(EvidenceParameterReference owner index, Right (VEvidence dictionary)) | (index, dictionary) <- zip indices arguments] <> captured
         in Right (VDeferredHostBinding (DictionaryBindingKey scopeId owner name (map runtimeDictionaryEvidence arguments)) (runtimeDiagnostic E3021 "runtime recursive dictionary binding has no concrete value") modulePath expression extended)
    | otherwise -> Left inconsistentEvidence
  VQualifiedMethodApplication methodKey _ _ _ _ -> case dictionaries of
    selected : localRequirements -> case runtimeDictionaryEvidence selected of
      selectedEvidence@EvidenceReference {evidenceMethod = Just method} -> do
        cell <- maybe (Left inconsistentEvidence) Right (Map.lookup method (runtimeDictionaryMethods selected))
        body <- cell
        case body of
          VConstrained _ _ scheme _ modulePath _ _
            | AnalyzedMethodCapabilityConstraint _ _ target : _ <- analyzedSchemeConstraints scheme,
              not (Foldable.null target) || qualifyRuntimeType modulePath target == evidenceType selectedEvidence ->
                VAnnotated (RuntimeMethodCall methodKey) <$> applyDictionaries (selected : localRequirements <> runtimeDictionaryPrerequisites selected) body
          _ -> Left (runtimeDiagnostic E3026 "inconsistent selected method evidence")
      _ -> Left inconsistentEvidence
    [] -> Left inconsistentEvidence
  _ -> Right runtimeValue

runtimeEvidenceIndices :: AnalyzedScheme -> [Int]
runtimeEvidenceIndices scheme =
  [index | (index, constraint) <- zip [0 ..] (analyzedSchemeConstraints scheme), case constraint of AnalyzedInferredCapabilityConstraint {} -> False; _ -> True]

constrainedCell :: ScopeCellStorage -> CoreNode 'Analyzed 'StatementSort -> ResolvedName -> Maybe SourceUnitOwner -> Expr 'Analyzed -> RuntimeEnv -> Maybe RuntimeCell
constrainedCell storage node name modulePath expression env = do
  (owner, scheme) <- statementBinding (coreNodeFacts node)
  let indices = runtimeEvidenceIndices scheme
      scopeId = case storage of DeferredScopeCells identity -> identity; LazyScopeCells -> DeferredHostScopeId 0
  if null indices then Nothing else Just (Right (VConstrained scopeId owner scheme name modulePath expression env))

runtimeEvidence ::
  Maybe SourceUnitOwner ->
  CoreNodeId ->
  ResolvedName ->
  ResolvedName ->
  AnalyzedType ->
  EvidenceReference
runtimeEvidence modulePath implementationNodeId capabilityName methodName targetType =
  EvidenceReference
    (CapabilityId capabilityName)
    implementationId
    (Just (MethodId (implementationId, mkIdentifier (identifierText methodName))))
    targetType
    Map.empty
    []
  where
    implementationId = ImplId (fromMaybe (StandaloneSourceUnit standaloneModulePath) modulePath, implementationNodeId)

evalValueWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe SourceUnitOwner ->
  RuntimeEnv ->
  Expr 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalValueWithHost host currentModulePath env expr =
  runEvaluationMachine
    host
    EvaluationContext
      { evaluationModulePath = currentModulePath,
        evaluationEnvironment = env,
        evaluationClosureBaseName = "<entry>",
        evaluationLambdaStage = 1
      }
    expr

evalScopeWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  PreparedRecursiveScope 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHost host evaluationMode initialEnv preparedScope = do
  scopeId <- lift freshDeferredHostScopeId
  let forceCell cell = liftRuntimeResult cell >>= forceRuntimeValueWithHost host
  evaluateRuntimeScope
    (DeferredScopeCells scopeId)
    (evalValueWithHost host)
    forceCell
    RuntimeScopeRequest
      { runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScope = preparedScope
      }

evalHostBindingValue ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe SourceUnitOwner ->
  RuntimeEnv ->
  ResolvedName ->
  Expr 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalHostBindingValue host currentModulePath env bindingName valueExpr =
  nameRuntimeClosureBinding currentModulePath bindingName
    <$> evalValueWithHost host currentModulePath env valueExpr

forceRuntimeValueWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
forceRuntimeValueWithHost host runtimeValue =
  case runtimeValue of
    VDeferredHostBinding bindingKey recursionDiagnostic currentModulePath valueExpr env -> do
      evaluationState <- lift get
      let cache = runtimeHostEvaluationBindingCache evaluationState
          observeStatistics =
            runtimeObservationStatisticsEnabled
              (runtimeHostEvaluationObservation evaluationState)
      case Map.lookup bindingKey cache of
        Just (DeferredHostBindingEvaluated result) -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeDeferredCacheOutcome DeferredCacheHit)
          liftRuntimeControl result
        Just DeferredHostBindingEvaluating -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeDeferredCacheOutcome DeferredCacheRecursiveEvaluation)
          throwRuntimeDiagnostic recursionDiagnostic
        Nothing -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeDeferredCacheOutcome DeferredCacheMiss)
          lift
            ( modifyDeferredHostBindingCache
                (Map.insert bindingKey DeferredHostBindingEvaluating)
            )
          result <-
            lift
              ( runExceptT
                  ( evalHostBindingValue
                      host
                      currentModulePath
                      env
                      (deferredHostBindingName bindingKey)
                      valueExpr
                  )
              )
          lift
            ( modifyDeferredHostBindingCache
                (Map.insert bindingKey (DeferredHostBindingEvaluated result))
            )
          liftRuntimeControl result
    VAnnotated annotation innerValue -> do
      forcedValue <- forceRuntimeValueWithHost host innerValue
      case annotation of
        -- Nullary methods produce a value while being forced, so their pending
        -- type application must become a value hint rather than a callable tag.
        RuntimeTypeApplication typeHint -> liftRuntimeResult (applyRuntimeInstantiation typeHint forcedValue)
        RuntimeTypeHint typeHint -> liftRuntimeResult (applyRuntimeTypeHint typeHint forcedValue)
        RuntimeMethodCall _ | not (isFunctionValue forcedValue) -> pure forcedValue
        _ -> pure (VAnnotated annotation forcedValue)
    VQualifiedMethodApplication {} -> throwRuntimeDiagnostic inconsistentEvidence
    _ -> pure runtimeValue

applyRuntimeFunctionWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyRuntimeFunctionWithHost host functionValue argumentValue =
  runCallableMachine
    host
    functionValue
    argumentValue

applyBuiltinWithHost ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyBuiltinWithHost observeStatistics observeProfile host builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      pure (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction = do
      recordRuntimeStatisticWhen
        observeStatistics
        (recordRuntimeBuiltinCall (runtimeBuiltinKind builtinFunction))
      resultValue <-
        evalBuiltinWithHost
          observeStatistics
          observeProfile
          host
          builtinFunction
          arguments
      mapM_
        ( \(constructionKind, amount) ->
            recordRuntimeStatisticWhen
              observeStatistics
              (recordRuntimeConstruction constructionKind amount)
        )
        (builtinResultConstructions builtinFunction resultValue)
      pure resultValue
  | otherwise =
      throwRuntimeDiagnostic
        (runtimeDiagnostic E3014 ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments"))

evalBuiltinWithHost ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBuiltinWithHost observeStatistics observeProfile host builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinReadTextRaw, [VText path]) ->
      rawHostOutcome VText <$> observeHostOperation ReadTextHostOperation (runtimeHostReadText host path)
    (BuiltinWriteTextRaw, [VText path, VText contents]) ->
      rawHostOutcome (const (VText "")) <$> observeHostOperation WriteTextHostOperation (runtimeHostWriteText host path contents)
    (BuiltinReadStdinRaw, [VTuple []]) ->
      rawHostOutcome VText <$> observeHostOperation ReadStdinHostOperation (runtimeHostReadStdin host)
    (BuiltinWriteStdoutRaw, [VText contents]) ->
      rawHostOutcome (const (VText "")) <$> observeHostOperation WriteStdoutHostOperation (runtimeHostWriteStdout host contents)
    (BuiltinWriteStderrRaw, [VText contents]) ->
      rawHostOutcome (const (VText "")) <$> observeHostOperation WriteStderrHostOperation (runtimeHostWriteStderr host contents)
    (BuiltinArguments, [VTuple []]) -> do
      argumentsText <- observeHostOperation ArgumentsHostOperation (runtimeHostArguments host)
      pure (VList (map VText argumentsText) (Just (SemanticList SemanticText)))
    (BuiltinExit, [statusValue])
      | Just status <- runtimeHostExitStatus statusValue,
        status >= 0 && status <= 255 -> do
          exitResult <- observeHostOperation ExitHostOperation (runtimeHostExit host status)
          case exitResult of
            Right RuntimeHostExitReturned -> pure (VTuple [])
            Right RuntimeHostExitRequested ->
              throwE (RuntimeExitRequested status)
            Left failure ->
              throwRuntimeDiagnostic
                ( runtimeDiagnostic
                    E3031
                    ( "runtime host operation 'exit!' failed: "
                        <> hostIOFailureMessage (hostIOFailureCategory failure)
                    )
                )
      | Just status <- runtimeHostExitStatus statusValue ->
          throwRuntimeDiagnostic
            ( runtimeDiagnostic
                E3030
                ("runtime primitive 'exit!' expects a status in range 0..255, found " <> Text.pack (show status))
            )
    _ ->
      evalBuiltin
        RuntimeDiagnostic
        (applyRuntimeFunctionWithHost host)
        builtinFunction
        arguments
  where
    observeHostOperation operation action = do
      beginHostOperation observeStatistics observeProfile operation
      outcome <- lift action
      endHostOperation observeProfile
      pure outcome

beginHostOperation ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHostOperationKind ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
beginHostOperation observeStatistics observeProfile hostOperationKind = do
  recordRuntimeStatisticWhen
    observeStatistics
    (recordRuntimeHostOperation hostOperationKind)
  recordRuntimeProfileOpenWhen
    observeProfile
    (HostCallable (runtimeHostOperationName hostOperationKind))

endHostOperation ::
  (Monad m) =>
  Bool ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
endHostOperation observeProfile =
  if observeProfile
    then lift (modifyRuntimeObservation recordRuntimeProfileClose)
    else pure ()

runtimeHostOperationName :: RuntimeHostOperationKind -> Text
runtimeHostOperationName hostOperationKind =
  case hostOperationKind of
    ReadTextHostOperation -> "readText"
    WriteTextHostOperation -> "writeText"
    ReadStdinHostOperation -> "readStdin"
    WriteStdoutHostOperation -> "writeStdout"
    WriteStderrHostOperation -> "writeStderr"
    ArgumentsHostOperation -> "arguments"
    ExitHostOperation -> "exit"

runtimeBuiltinKind :: BuiltinSymbol -> RuntimeBuiltinKind
runtimeBuiltinKind builtinFunction =
  case builtinFunction of
    BuiltinMap -> CollectionBuiltinCall
    BuiltinFilter -> CollectionBuiltinCall
    BuiltinHd -> CollectionBuiltinCall
    BuiltinTl -> CollectionBuiltinCall
    BuiltinListPrependRaw -> CollectionBuiltinCall
    BuiltinListReverseRaw -> CollectionBuiltinCall
    BuiltinToInt8 -> NumericBuiltinCall
    BuiltinToInt16 -> NumericBuiltinCall
    BuiltinToInt32 -> NumericBuiltinCall
    BuiltinToInt64 -> NumericBuiltinCall
    BuiltinToUInt8 -> NumericBuiltinCall
    BuiltinToUInt16 -> NumericBuiltinCall
    BuiltinToUInt32 -> NumericBuiltinCall
    BuiltinToUInt64 -> NumericBuiltinCall
    BuiltinToFloat16 -> NumericBuiltinCall
    BuiltinToFloat32 -> NumericBuiltinCall
    BuiltinToFloat64 -> NumericBuiltinCall
    BuiltinCharToUInt32 -> CharacterBuiltinCall
    BuiltinCharFromUInt32Raw -> CharacterBuiltinCall
    BuiltinCharIsAlpha -> CharacterBuiltinCall
    BuiltinCharIsAlphaNum -> CharacterBuiltinCall
    BuiltinCharIsDigit -> CharacterBuiltinCall
    BuiltinCharIsSpace -> CharacterBuiltinCall
    BuiltinCharIsHexDigit -> CharacterBuiltinCall
    BuiltinCharIsLower -> CharacterBuiltinCall
    BuiltinCharIsUpper -> CharacterBuiltinCall
    BuiltinCharToLower -> CharacterBuiltinCall
    BuiltinCharToUpper -> CharacterBuiltinCall
    BuiltinTextLength -> TextBuiltinCall
    BuiltinTextUnconsRaw -> TextBuiltinCall
    BuiltinTextAppend -> TextBuiltinCall
    BuiltinTextAppendChar -> TextBuiltinCall
    BuiltinTextFromChars -> TextBuiltinCall
    BuiltinTextConcat -> TextBuiltinCall
    BuiltinRenderValue -> TextBuiltinCall
    BuiltinReadTextRaw -> HostBuiltinCall
    BuiltinWriteTextRaw -> HostBuiltinCall
    BuiltinReadStdinRaw -> HostBuiltinCall
    BuiltinWriteStdoutRaw -> HostBuiltinCall
    BuiltinWriteStderrRaw -> HostBuiltinCall
    BuiltinArguments -> HostBuiltinCall
    BuiltinExit -> HostBuiltinCall
    BuiltinPrint -> OtherBuiltinCall

builtinResultConstructions :: BuiltinSymbol -> RuntimeValue -> [(RuntimeConstructionKind, Word64)]
builtinResultConstructions builtinFunction resultValue =
  case builtinFunction of
    BuiltinMap -> listResult
    BuiltinFilter -> listResult
    BuiltinListPrependRaw -> [(ListCellConstruction, 1)]
    BuiltinListReverseRaw -> listResult
    BuiltinCharFromUInt32Raw -> listResult
    BuiltinTextUnconsRaw ->
      listResult
        <> [ (TupleConstruction, fromIntegral (length tupleValues))
           | VList elements _ <- [resultValue],
             let tupleValues = [() | VTuple {} <- elements],
             not (null tupleValues)
           ]
    BuiltinReadTextRaw -> tupleResult
    BuiltinWriteTextRaw -> tupleResult
    BuiltinReadStdinRaw -> tupleResult
    BuiltinWriteStdoutRaw -> tupleResult
    BuiltinWriteStderrRaw -> tupleResult
    BuiltinArguments -> listResult
    BuiltinExit -> tupleResult
    _ -> []
  where
    listResult =
      case resultValue of
        VList elements _ -> [(ListCellConstruction, fromIntegral (length elements))]
        _ -> []
    tupleResult =
      case resultValue of
        VTuple {} -> [(TupleConstruction, 1)]
        _ -> []

rawHostOutcome :: (success -> RuntimeValue) -> Either HostIOFailure success -> RuntimeValue
rawHostOutcome renderSuccess outcome =
  case outcome of
    Right value ->
      VTuple [VBool True, renderSuccess value, VText "", VText ""]
    Left failure ->
      let category = hostIOFailureCategory failure
       in VTuple
            [ VBool False,
              VText "",
              VText (hostIOCategoryToken category),
              VText (hostIOFailureMessage category)
            ]

runtimeHostExitStatus :: RuntimeValue -> Maybe Integer
runtimeHostExitStatus runtimeValue =
  case runtimeValue of
    VInt status _ -> Just status
    VAnnotated _ innerValue -> runtimeHostExitStatus innerValue
    _ -> Nothing

evalBinaryWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Text ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBinaryWithHost host operatorSymbol leftValue rightValue =
  evalBinary
    RuntimeDiagnostic
    (applyRuntimeFunctionWithHost host)
    operatorSymbol
    leftValue
    rightValue
