{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
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
import Data.Map.Strict (Map)
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
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbolInMode,
  )
import Jazz.Compiler.CapabilityFacts
  ( qualifiedMethodKey,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    mkModulePath,
    modulePathTextSegments,
    preludeModulePath,
    standaloneModulePath,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    mkIdentifier,
    operatorBindingName,
    qualifiedMemberName,
    renderName,
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.Pattern
  ( patternBinderNames,
  )
import Jazz.Compiler.RecursiveBindings
  ( LambdaCaptureHints,
    closureCaptureCandidatesWithBound,
    collectLambdaCaptureHints,
    lambdaCaptureHintsChild,
    lookupLambdaCapturedNames,
  )
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
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
import Jazz.Compiler.Runtime.ScopePlan
  ( buildRuntimeScopePlan,
    exprDefinitelyNotFunctionValue,
    runtimeExprRequiresHost,
    runtimeModulePathAfterStatements,
    runtimeStatementRequiresHost,
    scopePlanBindingNameAt,
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
    applyRuntimeFunctionResultHint,
    applyRuntimeTypeHint,
    attachDefaultBindingIntegerTarget,
    evalNumericConversion,
    explicitTypeApplicationRuntimeFunctionHint,
    explicitTypeApplicationRuntimeValueHint,
    isFunctionValue,
    literalRuntimeValue,
    matchCaseArm,
    numericConversionBuiltinForTarget,
    preferredRuntimeMethodCandidates,
    qualifyRuntimeType,
    renderRuntimeType,
    renderRuntimeValue,
    runtimeDefinitionName,
    runtimeDefinitionNameIn,
    runtimeDiagnostic,
    runtimeFunctionArguments,
    runtimeQualifiedMethodIsFullyApplied,
    runtimeValueExactlyMatchesConstraint,
    substituteRuntimeVariable,
    untypedIntMetadata,
  )
import Jazz.Compiler.Runtime.Types
  ( DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
    DeferredHostScopeId (..),
    ModuleEvaluationMode (..),
    RuntimeAnnotation (..),
    RuntimeAppliedArguments,
    RuntimeCell,
    RuntimeClosure (..),
    RuntimeEnv,
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
    RuntimeMethodCandidate (..),
    RuntimeMethodCandidates,
    RuntimeValue (..),
    ScopeResult (..),
    appendRuntimeAppliedArgument,
    appendRuntimeMethodCandidate,
    constructorApplicationIsSaturated,
    emptyRuntimeAppliedArguments,
    emptyRuntimeMethodCandidates,
    filterRuntimeMethodCandidates,
    foldRuntimeExplicitResultHints,
    runtimeAppliedArgumentsInOrder,
    runtimeConstructorName,
    runtimeMethodCandidatesInOrder,
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
    AnalyzedType,
    CapabilityId (..),
    CoreNodeId,
    EvidenceReference (..),
    ExpressionFacts (expressionRuntimePlan),
    ImplId (..),
    MethodId (..),
    NumericTarget (..),
    RuntimeObligation (..),
    RuntimePlan (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    SemanticType (..),
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
  RuntimeExpressionRequest ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExpressionObserved observationRequest host request =
  {-# SCC "jazz-stage:evaluation" #-}
  case observationRequest of
    RuntimeObservationDisabled -> do
      outcome <-
        evaluateRuntimeExpressionUnobserved host request
      pure (RuntimeObservationResult outcome Nothing)
    _ -> do
      (outcome, observationState) <-
        runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
          evaluateRuntimeExpressionWithRequiredEvaluationHost evaluationHost request
      pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateRuntimeExpressionUnobserved ::
  (Monad m) =>
  RuntimeHost m ->
  RuntimeExpressionRequest ->
  m (RuntimeOutcome (Maybe RuntimeValue))
evaluateRuntimeExpressionUnobserved host request =
  runtimeControlOutcome
    <$> runRuntimeHostEvaluation
      host
      ( \evaluationHost ->
          evaluateRuntimeExpressionWithEvaluationHost evaluationHost request
      )

evaluateRuntimeExpressionWithRequiredEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeExpressionRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExpressionWithRequiredEvaluationHost host request =
  case expr of
    EBlock _ statements ->
      fmap scopeResultValue
        <$> evaluateRuntimeScopeWithRequiredHostRequest
          host
          (runtimeExpressionScopeRequest request statements)
    _ ->
      runExceptT
        (Just <$> evalValueWithHost host Nothing builtinMode Map.empty False expr)
  where
    builtinMode = runtimeExpressionBuiltinMode request
    expr = runtimeExpression request

evaluateRuntimeExpressionWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  RuntimeExpressionRequest ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExpressionWithEvaluationHost host request =
  if runtimeExprRequiresHost expr
    then case expr of
      EBlock _ statements ->
        fmap scopeResultValue
          <$> evaluateRuntimeScopeWithEvaluationHostRequest
            host
            (runtimeExpressionScopeRequest request statements)
      _ ->
        runExceptT
          (Just <$> evalValueWithHost host Nothing builtinMode Map.empty False expr)
    else
      pure
        ( case evaluateRuntimeExpressionPure request of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )
  where
    builtinMode = runtimeExpressionBuiltinMode request
    expr = runtimeExpression request

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExpressionPure :: RuntimeExpressionRequest -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpressionPure request =
  case expr of
    EBlock _ statements ->
      scopeResultValue
        <$> evaluateRuntimeScopePureRequest
          (runtimeExpressionScopeRequest request statements)
    _ -> Just <$> evalValue builtinMode Map.empty expr
  where
    builtinMode = runtimeExpressionBuiltinMode request
    expr = runtimeExpression request

runtimeExpressionScopeRequest :: RuntimeExpressionRequest -> [Statement 'Analyzed] -> RuntimeScopeRequest
runtimeExpressionScopeRequest request statements =
  RuntimeScopeRequest
    { runtimeScopeSourceUnitStatementIndices = runtimeExpressionSourceUnitStatementIndices request,
      runtimeScopePreludeModulePath = runtimeExpressionPreludeModulePath request,
      runtimeScopeCurrentModulePath = Nothing,
      runtimeScopeEvaluationMode = EvaluateEntryModule,
      runtimeScopeBuiltinMode = runtimeExpressionBuiltinMode request,
      runtimeScopeInitialEnvironment = Map.empty,
      runtimeScopeStatements = statements
    }

-- Public scope entry points receive an opaque map whose lazy cells may include
-- recursive blackholes. They cannot safely recover provenance by inspecting
-- values, so only the empty map is known not to contain imported host cells.
opaqueRuntimeEnvironmentMayReachHostCells :: RuntimeEnv -> Bool
opaqueRuntimeEnvironmentMayReachHostCells = not . Map.null

-- | Immutable expression-local inputs for the shared evaluator. Callable
-- transfer replaces only the captured environment and module path; builtin
-- resolution remains stable for the whole machine run.
data EvaluationContext = EvaluationContext
  { evaluationModulePath :: Maybe [Text],
    evaluationBuiltinMode :: BuiltinResolutionMode,
    evaluationEnvironment :: RuntimeEnv,
    evaluationEnvironmentMayReachHostCells :: Bool,
    evaluationLambdaCaptureHints :: LambdaCaptureHints 'Analyzed,
    evaluationClosureBaseName :: Text,
    evaluationLambdaStage :: Int
  }

evaluationContextForLambdaChild :: Int -> EvaluationContext -> EvaluationContext
evaluationContextForLambdaChild childIndex context =
  context
    { evaluationLambdaCaptureHints =
        lambdaCaptureHintsChild childIndex (evaluationLambdaCaptureHints context)
    }

data RuntimeResultObligation
  = ApplyFunctionResultHint AnalyzedType
  | ApplyExplicitResultHint AnalyzedType
  | ApplyExpressionRuntimePlan (Maybe [Text]) RuntimePlan
  | AttachDefaultIntegerResult
  | CloseRuntimeProfileFrame
  deriving (Eq, Show)

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
  | EvaluateListElement EvaluationContext Int [RuntimeValue] [Expr 'Analyzed]
  | EvaluateTupleElement EvaluationContext Int [RuntimeValue] [Expr 'Analyzed]
  | EvaluateIfBranch EvaluationContext (Expr 'Analyzed) (Expr 'Analyzed)
  | EvaluateCaseArms EvaluationContext [(Int, CaseArm 'Analyzed)]
  | EvaluateCaseGuard EvaluationContext RuntimeValue RuntimeEnv Int (Expr 'Analyzed) [(Int, CaseArm 'Analyzed)]
  | EvaluateBuiltinRightOperand EvaluationContext Text (Expr 'Analyzed)
  | ApplyBuiltinBinary Text RuntimeValue
  | EvaluateDeclaredOperatorLeft EvaluationContext (Expr 'Analyzed) (Expr 'Analyzed)
  | ApplyDeclaredOperatorLeft EvaluationContext RuntimeValue (Expr 'Analyzed)
  | EvaluateDeclaredOperatorRight EvaluationContext (Expr 'Analyzed)
  | EvaluateLeftSection EvaluationContext Text
  | ApplyForcedCallable RuntimeValue
  | EvaluateRightSection EvaluationContext Text
  | BuildDeclaredRightSection EvaluationContext Text RuntimeValue
  | ApplyDeclaredRightSectionOperand RuntimeValue
  | FinishTypeApplication
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
    evaluationReturnPolicy :: RuntimeReturnPolicy
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
        (runtimeScopePreludeModulePath request)
        (runtimeScopeSourceUnitStatementIndices request)
        currentModulePath
        evaluationMode
        builtinMode
        (opaqueRuntimeEnvironmentMayReachHostCells initialEnv)
        initialEnv
        statements
    )
  where
    currentModulePath = runtimeScopeCurrentModulePath request
    evaluationMode = runtimeScopeEvaluationMode request
    builtinMode = runtimeScopeBuiltinMode request
    initialEnv = runtimeScopeInitialEnvironment request
    statements = runtimeScopeStatements request

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
            preludePath
            preludeStatementIndices
            currentModulePath
            evaluationMode
            builtinMode
            (opaqueRuntimeEnvironmentMayReachHostCells initialEnv)
            initialEnv
            statements
        )
    else
      pure
        ( case evaluateRuntimeScopePureRequest request of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )
  where
    preludePath = runtimeScopePreludeModulePath request
    preludeStatementIndices = runtimeScopeSourceUnitStatementIndices request
    currentModulePath = runtimeScopeCurrentModulePath request
    evaluationMode = runtimeScopeEvaluationMode request
    builtinMode = runtimeScopeBuiltinMode request
    initialEnv = runtimeScopeInitialEnvironment request
    statements = runtimeScopeStatements request

evaluateRuntimeScopePureRequest :: RuntimeScopeRequest -> Either Diagnostic ScopeResult
evaluateRuntimeScopePureRequest request = go Nothing indexedStatements
  where
    preludeStatementIndices = runtimeScopeSourceUnitStatementIndices request
    preludePath = runtimeScopePreludeModulePath request
    currentModulePath = runtimeScopeCurrentModulePath request
    evaluationMode = runtimeScopeEvaluationMode request
    builtinMode = runtimeScopeBuiltinMode request
    initialEnv = runtimeScopeInitialEnvironment request
    statements = runtimeScopeStatements request
    scopePlan =
      buildRuntimeScopePlan
        preludePath
        preludeStatementIndices
        currentModulePath
        builtinMode
        (Map.keysSet initialEnv)
        statements
    indexedStatements = scopePlanIndexedStatements scopePlan
    bindingCells =
      LazyIntMap.fromDistinctAscList
        [ (statementIndex, cellForStatement statementIndex statement)
        | (statementIndex, statement) <- indexedStatements
        ]
    prefixEnvironments =
      LazyIntMap.fromDistinctAscList
        (zip [0 ..] (scanl' extendPrefixEnvironment initialEnv indexedStatements))
    finalEnvironment =
      LazyIntMap.findWithDefault initialEnv (length indexedStatements) prefixEnvironments

    extendPrefixEnvironment env (statementIndex, statement) =
      case statement of
        SLet _ bindingName _ ->
          LazyMap.insert bindingName (bindingCellAt statementIndex) env
        SData _ _ _ constructors ->
          insertDataConstructors (modulePathForStatement statementIndex) constructors env
        SClass _ capabilityName _ methods ->
          insertClassMethods capabilityName methods env
        SImpl implementationNode capabilityName _ methods ->
          insertImplMethods (modulePathForStatement statementIndex) implementationNode capabilityName methods env
        _ -> env

    go :: Maybe RuntimeValue -> [(Int, Statement 'Analyzed)] -> Either Diagnostic ScopeResult
    go lastExprValue remainingStatements =
      case remainingStatements of
        [] ->
          -- Declaration-only scopes intentionally remain `Nothing` until a terminal `SExpr` sets a value.
          Right (ScopeResult finalEnvironment lastExprValue False)
        (statementIndex, statement) : rest ->
          case statement of
            SSignature {} ->
              go Nothing rest
            SModule {} ->
              go Nothing rest
            SImport {} ->
              go Nothing rest
            SClass {} ->
              go Nothing rest
            SImpl {} ->
              go Nothing rest
            SData {} ->
              go Nothing rest
            SLet {} ->
              case evaluationMode of
                EvaluateDependencyModule ->
                  go Nothing rest
                EvaluateEntryModule -> do
                  _ <- bindingCellAt statementIndex
                  go Nothing rest
            SExpr _ expr ->
              case evaluationMode of
                EvaluateDependencyModule -> go Nothing rest
                EvaluateEntryModule -> do
                  value <- evalValueAt statementIndex (envBefore statementIndex) expr
                  go (Just value) rest

    modulePathForStatement :: Int -> Maybe [Text]
    modulePathForStatement = scopePlanModulePathForStatement scopePlan

    evalValueAt :: Int -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
    evalValueAt statementIndex =
      evalValueWithModulePath (modulePathForStatement statementIndex) builtinMode

    bindingCellAt :: Int -> RuntimeCell
    bindingCellAt statementIndex =
      case LazyIntMap.lookup statementIndex bindingCells of
        Just cell -> cell
        Nothing ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: missing binding cell for statement")

    cellForStatement :: Int -> Statement 'Analyzed -> RuntimeCell
    cellForStatement statementIndex statement =
      case statement of
        SLet _ bindingName valueExpr ->
          bindingCell statementIndex bindingName valueExpr
        _ ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: expected binding statement")

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
                Right (attachSelfRecursiveBinding statementIndex bindingName evaluatedValue)
      where
        visibleEnv = bindingEnv statementIndex bindingName

    evalBindingValue :: Int -> ResolvedName -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      nameRuntimeClosureBinding
        (modulePathForStatement statementIndex)
        bindingName
        <$> evalValueWithModulePath
          (modulePathForStatement statementIndex)
          builtinMode
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
            Just (SLet _ bindingName aliasExpr) ->
              case selectedRecursiveAliasTarget statementIndex (bindingEnv statementIndex bindingName) aliasExpr of
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

    bindingEnv :: Int -> ResolvedName -> RuntimeEnv
    bindingEnv statementIndex bindingName =
      case functionSelfReferenceCell statementIndex bindingName of
        Just selfCell ->
          LazyMap.insert
            bindingName
            selfCell
            peerVisibleEnv
        Nothing
          | recursiveBindingNeedsSelf statementIndex ->
              LazyMap.insert
                bindingName
                (bindingCellAt statementIndex)
                peerVisibleEnv
          | otherwise -> peerVisibleEnv
      where
        peerVisibleEnv = recursivePeerEnv statementIndex (envBefore statementIndex)

    functionSelfReferenceCell :: Int -> ResolvedName -> Maybe RuntimeCell
    functionSelfReferenceCell statementIndex bindingName
      | recursiveFunctionNeedsSelf statementIndex bindingName =
          Just (Left (runtimeDiagnostic E3021 "runtime recursive binding has no concrete value"))
      | otherwise =
          Nothing

    recursiveFunctionNeedsSelf :: Int -> ResolvedName -> Bool
    recursiveFunctionNeedsSelf statementIndex bindingName =
      scopePlanIsSelfRecursiveFunction scopePlan statementIndex
        && Map.notMember bindingName (envBefore statementIndex)

    recursiveBindingNeedsSelf :: Int -> Bool
    recursiveBindingNeedsSelf statementIndex =
      -- Function-valued self recursion gets stitched onto the resulting
      -- closure after wrapper evaluation. Pre-seeding `self` here is only
      -- needed for non-function recursive bindings; doing it eagerly for block
      -- alias wrappers can blackhole before the closure is returned.
      scopePlanIsRecursiveBinding scopePlan statementIndex
        && not (scopePlanIsSelfRecursiveFunction scopePlan statementIndex)

    -- Wrapper expressions like `if` and `{ g = \(x) -> f x. g. }` should
    -- evaluate to their closure first, then get their own binding stitched
    -- into the captured env without forcing the whole wrapper through a
    -- self-referential scope during evaluation.
    attachSelfRecursiveBinding :: Int -> ResolvedName -> RuntimeValue -> RuntimeValue
    attachSelfRecursiveBinding statementIndex bindingName runtimeValue
      | recursiveFunctionNeedsSelf statementIndex bindingName =
          case runtimeValue of
            VClosure closure ->
              VClosure
                closure
                  { runtimeClosureEnvironment =
                      LazyMap.insert
                        bindingName
                        (bindingCellAt statementIndex)
                        (runtimeClosureEnvironment closure)
                  }
            _ -> runtimeValue
      | otherwise =
          runtimeValue

    recursiveAliasTarget :: Set ResolvedName -> Int -> Expr 'Analyzed -> Maybe Int
    recursiveAliasTarget locallyBoundNames statementIndex valueExpr =
      case peelSingleExprBlock valueExpr of
        EVar _ targetName ->
          if Set.member targetName locallyBoundNames
            then Nothing
            else case scopePlanRecursiveGroupAt scopePlan statementIndex of
              Just groupMembers ->
                lookupRecursivePeer targetName groupMembers
              Nothing -> Nothing
        EOperatorValue _ operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol) ->
              let targetName = operatorBindingName operatorSymbol
               in if Set.member targetName locallyBoundNames
                    then Nothing
                    else case scopePlanRecursiveGroupAt scopePlan statementIndex of
                      Just groupMembers ->
                        lookupRecursivePeer targetName groupMembers
                      Nothing -> Nothing
        _ -> Nothing

    -- Preserve wrapper runtime semantics by evaluating the branch condition
    -- first, then following alias resolution only through the selected branch.
    selectedRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTarget =
      selectedRecursiveAliasTargetWithBound Set.empty

    selectedRecursiveAliasTargetWithBound ::
      Set ResolvedName ->
      Int ->
      RuntimeEnv ->
      Expr 'Analyzed ->
      Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf _ conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
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
            Just (newLocallyBoundNames, armEnv, bodyExpr) ->
              selectedRecursiveAliasTargetWithBound
                (Set.union locallyBoundNames newLocallyBoundNames)
                statementIndex
                armEnv
                bodyExpr
            Nothing ->
              Right Nothing
        peeledExpr ->
          Right (recursiveAliasTarget locallyBoundNames statementIndex peeledExpr)

    selectRecursiveAliasTarget :: Set ResolvedName -> Int -> RuntimeEnv -> Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed -> Either Diagnostic (Maybe Int)
    selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueAt statementIndex env conditionExpr
      case conditionValue of
        VBool True ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env thenExpr
        VBool False ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                E3003
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

    selectMatchingCaseArmForAlias ::
      Maybe [Text] ->
      (RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue) ->
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm 'Analyzed] ->
      Either Diagnostic (Maybe (Set ResolvedName, RuntimeEnv, Expr 'Analyzed))
    selectMatchingCaseArmForAlias patternModulePath evalGuard env scrutineeValue =
      chooseRemainingArm
      where
        chooseRemainingArm remainingArms =
          case remainingArms of
            [] -> Right Nothing
            caseArm : rest ->
              chooseArm caseArm rest

        chooseArm caseArm rest =
          case matchCaseArm patternModulePath env scrutineeValue caseArm of
            Just (armEnv, guardExpr, bodyExpr) ->
              case guardExpr of
                Nothing ->
                  Right
                    ( Just
                        ( caseArmBoundNames caseArm,
                          armEnv,
                          bodyExpr
                        )
                    )
                Just conditionExpr -> do
                  guardValue <- evalGuard armEnv conditionExpr
                  case guardValue of
                    VBool True ->
                      Right
                        ( Just
                            ( caseArmBoundNames caseArm,
                              armEnv,
                              bodyExpr
                            )
                        )
                    VBool False ->
                      chooseRemainingArm rest
                    other ->
                      Left
                        ( runtimeDiagnostic
                            E3003
                            ("runtime case guard must be Bool, found " <> renderRuntimeType other)
                        )
            Nothing ->
              chooseRemainingArm rest

    caseArmBoundNames :: CaseArm 'Analyzed -> Set ResolvedName
    caseArmBoundNames (CaseArm _ casePattern _ _) =
      patternBinderNames casePattern

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr 'Analyzed -> Expr 'Analyzed
    peelSingleExprBlock expr =
      case expr of
        EBlock _ [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    terminalBlockLocalAliasExpr :: [Statement 'Analyzed] -> Maybe ([Statement 'Analyzed], Expr 'Analyzed)
    terminalBlockLocalAliasExpr blockStatements =
      case reverse blockStatements of
        SExpr _ (EVar _ aliasName) : precedingStatements ->
          let prefixStatements = reverse precedingStatements
           in fmap
                (\aliasExpr -> (prefixStatements, aliasExpr))
                (followLocalAlias Set.empty aliasName (localAliasBindings prefixStatements))
        _ -> Nothing

    localAliasBindings :: [Statement 'Analyzed] -> Map ResolvedName (Expr 'Analyzed)
    localAliasBindings =
      foldl' collectBinding Map.empty
      where
        collectBinding :: Map ResolvedName (Expr 'Analyzed) -> Statement 'Analyzed -> Map ResolvedName (Expr 'Analyzed)
        collectBinding bindings statement =
          case statement of
            SLet _ bindingName bindingExpr ->
              Map.insert bindingName bindingExpr bindings
            _ -> bindings

    followLocalAlias :: Set ResolvedName -> ResolvedName -> Map ResolvedName (Expr 'Analyzed) -> Maybe (Expr 'Analyzed)
    followLocalAlias visitedNames aliasName localBindings =
      if Set.member aliasName visitedNames
        then Nothing
        else case Map.lookup aliasName localBindings of
          Just aliasExpr ->
            case peelSingleExprBlock aliasExpr of
              EVar _ nextAliasName
                | Map.member nextAliasName localBindings ->
                    followLocalAlias (Set.insert aliasName visitedNames) nextAliasName localBindings
              _ -> Just aliasExpr
          Nothing ->
            Nothing

    blockLocalAliasEnv :: Maybe [Text] -> RuntimeEnv -> [Statement 'Analyzed] -> RuntimeEnv
    blockLocalAliasEnv blockModulePath blockInitialEnv blockStatements =
      case LazyIntMap.lookup (length indexedBlockStatements) blockPrefixEnvironments of
        Just env -> env
        Nothing -> blockInitialEnv
      where
        blockScopePlan =
          buildRuntimeScopePlan
            preludeModulePath
            Set.empty
            blockModulePath
            builtinMode
            (Map.keysSet blockInitialEnv)
            blockStatements
        indexedBlockStatements = scopePlanIndexedStatements blockScopePlan
        blockBindingCells =
          LazyIntMap.fromDistinctAscList
            [ (statementIndex, blockCellForStatement statementIndex statement)
            | (statementIndex, statement) <- indexedBlockStatements
            ]
        blockPrefixEnvironments =
          LazyIntMap.fromDistinctAscList
            (zip [0 ..] (scanl' extendBlockPrefixEnvironment blockInitialEnv indexedBlockStatements))

        blockEnvBefore statementIndex =
          LazyIntMap.findWithDefault blockInitialEnv statementIndex blockPrefixEnvironments

        extendBlockPrefixEnvironment env (statementIndex, statement) =
          case statement of
            SLet _ bindingName _ ->
              LazyMap.insert bindingName (blockBindingCellAt statementIndex) env
            SData _ _ _ constructors ->
              insertDataConstructors blockModulePath constructors env
            SClass _ capabilityName _ methods ->
              insertClassMethods capabilityName methods env
            SImpl implementationNode capabilityName _ methods ->
              insertImplMethods blockModulePath implementationNode capabilityName methods env
            _ -> env

        blockBindingCellAt statementIndex =
          case LazyIntMap.lookup statementIndex blockBindingCells of
            Just cell -> cell
            Nothing ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: missing block binding cell for alias selection")

        blockCellForStatement statementIndex statement =
          case statement of
            SLet _ _ valueExpr ->
              evalValueWithModulePath
                blockModulePath
                builtinMode
                (blockEnvBefore statementIndex)
                valueExpr
            _ ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: expected block binding statement for alias selection")

    lookupRecursivePeer :: ResolvedName -> [Int] -> Maybe Int
    lookupRecursivePeer targetName =
      foldl' chooseTarget Nothing
      where
        chooseTarget currentChoice peerIndex =
          case scopePlanBindingNameAt scopePlan peerIndex of
            Just peerName
              | peerName == targetName ->
                  Just peerIndex
            _ -> currentChoice

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
              case scopePlanBindingNameAt scopePlan peerIndex of
                Just peerName
                  | Map.notMember peerName envBeforeValue ->
                      LazyMap.insert peerName (bindingCellAt peerIndex) envAcc
                _ ->
                  envAcc

    insertDataConstructors :: Maybe [Text] -> [DataConstructor 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors definitionModulePath constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor node constructorName _) =
          Map.insert constructorName (constructorValue node constructorName) envAcc
        constructorValue node constructorName =
          case Map.elems (statementGeneralizedSchemes (coreNodeFacts node)) of
            [scheme]
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

    insertClassMethods :: ResolvedName -> [ClassMethodSignature 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods capabilityName methods env =
      foldl' insertMethod env methods
      where
        insertMethod envAcc (ClassMethodSignature node methodName _) =
          let methodKey = qualifiedMethodKey capabilityName methodName
              methodName' = qualifiedMemberName capabilityName methodName
              methodValue = case statementDeclarationFact (coreNodeFacts node) of
                MethodDeclaration _ signature ->
                  Right
                    ( VQualifiedMethodApplication
                        methodKey
                        (analyzedMethodClassParameter signature)
                        (analyzedMethodType signature)
                        emptyRuntimeMethodCandidates
                        emptyRuntimeAppliedArguments
                    )
                _ -> Left (runtimeDiagnostic E3021 "runtime method is missing its analyzed signature")
           in Map.insertWith (\_ existing -> existing) methodName' methodValue envAcc

    insertImplMethods :: Maybe [Text] -> CoreNode 'Analyzed 'StatementSort -> ResolvedName -> [ImplMethod 'Analyzed] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods methodModulePath implementationNode capabilityName methods env =
      case statementDeclarationFact (coreNodeFacts implementationNode) of
        ImplementationDeclaration _ [implTarget] ->
          methodEnv
          where
            runtimeImplTarget = qualifyRuntimeType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodExprsByKey =
              Map.fromList
                [ (qualifiedMethodKey capabilityName methodName, methodExpr)
                | ImplMethod _ methodName methodExpr <- methods
                ]
            methodCandidates =
              map
                ( \(ImplMethod _ methodName methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                        methodName' = qualifiedMemberName capabilityName methodName
                        evidence = runtimeEvidence methodModulePath (coreNodeId implementationNode) capabilityName methodName runtimeImplTarget
                     in ( methodName',
                          methodKey,
                          RuntimeMethodCandidate evidence (methodCandidateCell runtimeImplTarget methodName' methodKey methodExpr)
                        )
                )
                methods
            methodCandidateCell candidateImplTarget methodName methodKey methodExpr =
              case selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey Set.empty methodEnv methodKey methodExpr of
                Left diagnostic ->
                  Left diagnostic
                Right True ->
                  Left
                    ( runtimeDiagnostic
                        E3021
                        ("runtime recursive qualified method alias cycle '" <> methodKey <> "' has no concrete value")
                    )
                Right False ->
                  evalValueWithModulePath methodModulePath builtinMode methodEnv methodExpr
                    >>= attachRuntimeMethodSignature methodModulePath methodEnv candidateImplTarget methodName
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

    attachRuntimeMethodSignature ::
      Maybe [Text] ->
      RuntimeEnv ->
      AnalyzedType ->
      ResolvedName ->
      RuntimeValue ->
      Either Diagnostic RuntimeValue
    attachRuntimeMethodSignature methodModulePath env implTarget methodName methodValue =
      case Map.lookup methodName env of
        Just (Right (VQualifiedMethodApplication _ classParameter methodSignature _ _)) ->
          applyRuntimeTypeHint
            (qualifyRuntimeType signatureModulePath (substituteRuntimeVariable classParameter implTarget methodSignature))
            methodValue
        _ ->
          Right methodValue
      where
        signatureModulePath =
          case methodName of
            UserName (ResolvedUserName (ImportedModule classModulePath) _ _) ->
              Just (NonEmpty.toList (modulePathTextSegments classModulePath))
            _ -> methodModulePath

    selectedQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text (Expr 'Analyzed) -> Set Text -> RuntimeEnv -> Text -> Expr 'Analyzed -> Either Diagnostic Bool
    selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey expr
      | Set.member methodKey visitedMethodKeys =
          Right True
      | otherwise =
          case peelSingleExprBlock expr of
            EIf _ conditionExpr thenExpr elseExpr ->
              selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr
            EPatternCase _ scrutineeExpr caseArms -> do
              scrutineeValue <- evalValueWithModulePath methodModulePath builtinMode env scrutineeExpr
              selectedArm <-
                selectMatchingCaseArmForAlias
                  methodModulePath
                  (evalValueWithModulePath methodModulePath builtinMode)
                  env
                  scrutineeValue
                  caseArms
              case selectedArm of
                Just (_, armEnv, bodyExpr) ->
                  selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys armEnv methodKey bodyExpr
                Nothing ->
                  Right False
            EBlock _ blockStatements ->
              case terminalBlockLocalAliasExpr blockStatements of
                Just (prefixStatements, aliasExpr) ->
                  selectedQualifiedMethodAliasTarget
                    methodModulePath
                    methodExprsByKey
                    visitedMethodKeys
                    (blockLocalAliasEnv methodModulePath env prefixStatements)
                    methodKey
                    aliasExpr
                Nothing ->
                  Right False
            EVar _ aliasName ->
              let aliasNameText = identifierText aliasName
               in case Map.lookup aliasNameText methodExprsByKey of
                    Just aliasExpr ->
                      selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey nextVisitedMethodKeys env aliasNameText aliasExpr
                    Nothing ->
                      Right (aliasNameText == methodKey)
            _ ->
              Right False
      where
        nextVisitedMethodKeys = Set.insert methodKey visitedMethodKeys

    selectQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text (Expr 'Analyzed) -> Set Text -> RuntimeEnv -> Text -> Expr 'Analyzed -> Expr 'Analyzed -> Expr 'Analyzed -> Either Diagnostic Bool
    selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueWithModulePath methodModulePath builtinMode env conditionExpr
      case conditionValue of
        VBool True ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey thenExpr
        VBool False ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                E3003
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

evalValue :: BuiltinResolutionMode -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
evalValue =
  evalValueWithModulePath Nothing

evalValueWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic RuntimeValue
evalValueWithModulePath currentModulePath builtinMode env expr =
  runtimeControlAsDiagnosticResult
    ( runIdentity
        ( runRuntimeHostEvaluation disabledRuntimeHost $ \host ->
            runExceptT
              ( runEvaluationMachine
                  host
                  EvaluationContext
                    { evaluationModulePath = currentModulePath,
                      evaluationBuiltinMode = builtinMode,
                      evaluationEnvironment = env,
                      evaluationEnvironmentMayReachHostCells = False,
                      evaluationLambdaCaptureHints = collectLambdaCaptureHints expr,
                      evaluationClosureBaseName = "<entry>",
                      evaluationLambdaStage = 1
                    }
                  expr
              )
        )
    )

nameRuntimeClosureBinding :: Maybe [Text] -> ResolvedName -> RuntimeValue -> RuntimeValue
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
    (evaluationBuiltinMode context)
    (EvaluateExpression context expression)

runCallableMachine ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runCallableMachine host builtinMode functionValue argumentValue =
  runEvaluationControl
    host
    builtinMode
    (ApplyCallable functionValue argumentValue)

runEvaluationControl ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  EvaluationControl ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationControl host builtinMode initialControl =
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
          progress <- stepEvaluationMachine observeStatistics observeProfile host builtinMode machine
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
  BuiltinResolutionMode ->
  EvaluationMachine ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
stepEvaluationMachine observeStatistics observeProfile host builtinMode machine =
  case evaluationControl machine of
    EvaluateExpression context expression ->
      stepExpression
        ( appendRuntimeResultObligation
            (ApplyExpressionRuntimePlan (evaluationModulePath context) (expressionRuntimePlanOf expression))
            machine
        )
        context
        expression
    ApplyCallable functionValue argumentValue ->
      stepCallable functionValue argumentValue
    ForceRuntimeValue runtimeValue -> do
      if observeStatistics
        then lift (modifyRuntimeObservation recordRuntimeForcedValue)
        else pure ()
      forcedValue <-
        forceRuntimeValueWithHost host builtinMode runtimeValue
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
            builtinMode
            machine
              { evaluationContinuations = rest,
                evaluationContinuationDepth = evaluationContinuationDepth machine - 1,
                evaluationReturnPolicy = parentPolicy
              }
            frame
            dischargedValue
  where
    stepExpression expressionMachine context expression =
      case expression of
        ELit _ literal ->
          continueWith (ReturnRuntimeValue (literalRuntimeValue literal)) expressionMachine
        EVar _ name ->
          case Map.lookup name (evaluationEnvironment context) of
            Just runtimeCell -> do
              runtimeValue <- liftRuntimeResult runtimeCell
              continueWith
                (ForceRuntimeValue (prepareRuntimeEvidence (expressionRuntimePlanOf expression) runtimeValue))
                expressionMachine
            Nothing ->
              case lookupBuiltinSymbolInMode builtinMode (identifierText name) of
                Just builtinFunction ->
                  continueWith (ReturnRuntimeValue (VBuiltin builtinFunction [])) expressionMachine
                Nothing ->
                  throwRuntimeDiagnostic
                    (runtimeDiagnostic E3002 ("runtime unbound variable '" <> identifierText name <> "'"))
        ELambda _ parameterName bodyExpr ->
          do
            let (capturedNames, nestedCaptureHints) =
                  fromMaybe
                    ( closureCaptureCandidatesWithBound (Set.singleton parameterName) bodyExpr,
                      collectLambdaCaptureHints bodyExpr
                    )
                    (lookupLambdaCapturedNames (evaluationLambdaCaptureHints context))
                capturedEnvironment =
                  Map.restrictKeys
                    (evaluationEnvironment context)
                    capturedNames
                capturedEnvironmentMayReachHostCells =
                  not (Map.null capturedEnvironment)
                    && evaluationEnvironmentMayReachHostCells context
            recordRuntimeStatisticWhen
              observeStatistics
              (recordRuntimeClosureCreation (Map.size capturedEnvironment))
            continueWith
              ( ReturnRuntimeValue
                  ( VClosure
                      RuntimeClosure
                        { runtimeClosureEnvironment = capturedEnvironment,
                          runtimeClosureEnvironmentMayReachHostCells =
                            capturedEnvironmentMayReachHostCells,
                          runtimeClosureLambdaCaptureHints =
                            nestedCaptureHints,
                          runtimeClosureParameter = parameterName,
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
        EOperatorValue _ operatorSymbol
          | isBuiltinOperatorSymbol operatorSymbol ->
              continueWith (ReturnRuntimeValue (VOperator operatorSymbol [])) expressionMachine
          | otherwise -> do
              operatorValue <-
                liftRuntimeResult
                  (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
              continueWith
                (ForceRuntimeValue (prepareRuntimeEvidence (expressionRuntimePlanOf expression) operatorValue))
                expressionMachine
        EList _ [] ->
          continueWith (ReturnRuntimeValue (VList [] Nothing)) expressionMachine
        EList _ (element : rest) ->
          suspendEvaluation
            expressionMachine
            (EvaluateListElement context 1 [] rest)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) element)
        ETuple _ [] ->
          do
            recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction TupleConstruction 1)
            continueWith (ReturnRuntimeValue (VTuple [])) expressionMachine
        ETuple _ (element : rest) ->
          suspendEvaluation
            expressionMachine
            (EvaluateTupleElement context 1 [] rest)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) element)
        EApply _ functionExpr argumentExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateApplicationArgument context argumentExpr)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) functionExpr)
        ETypeApplication _ functionExpr _ _ ->
          case functionExpr of
            EVar _ name ->
              case Map.lookup name (evaluationEnvironment context) of
                Just runtimeCell -> do
                  unforcedValue <- liftRuntimeResult runtimeCell
                  case unforcedValue of
                    VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs -> do
                      let matchingValue =
                            prepareRuntimeEvidence
                              (expressionRuntimePlanOf expression)
                              (VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs)
                          matchingCandidates =
                            case matchingValue of
                              VQualifiedMethodApplication _ _ _ selectedCandidates _ -> selectedCandidates
                              _ -> candidates
                      selectedValue <-
                        applyQualifiedMethodWithHost
                          host
                          builtinMode
                          methodKey
                          classParameter
                          methodSignature
                          matchingCandidates
                          capturedArgs
                      continueWith (ReturnRuntimeValue selectedValue) expressionMachine
                    _ -> evaluateTypeApplicationNormally expressionMachine context functionExpr
                Nothing -> evaluateTypeApplicationNormally expressionMachine context functionExpr
            _ -> evaluateTypeApplicationNormally expressionMachine context functionExpr
        EIf _ conditionExpr thenExpr elseExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateIfBranch context thenExpr elseExpr)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) conditionExpr)
        EPatternCase _ scrutineeExpr caseArms ->
          suspendEvaluation
            expressionMachine
            (EvaluateCaseArms context (zip [0 ..] caseArms))
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) scrutineeExpr)
        EBinary _ operatorSymbol leftExpr rightExpr
          | isBuiltinOperatorSymbol operatorSymbol ->
              suspendEvaluation
                expressionMachine
                (EvaluateBuiltinRightOperand context operatorSymbol rightExpr)
                (EvaluateExpression (evaluationContextForLambdaChild 0 context) leftExpr)
          | otherwise -> do
              operatorValue <-
                liftRuntimeResult
                  (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
              suspendEvaluation
                expressionMachine
                (EvaluateDeclaredOperatorLeft context leftExpr rightExpr)
                (ForceRuntimeValue operatorValue)
        ESectionLeft _ leftExpr operatorSymbol ->
          suspendEvaluation
            expressionMachine
            (EvaluateLeftSection context operatorSymbol)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) leftExpr)
        ESectionRight _ operatorSymbol rightExpr ->
          suspendEvaluation
            expressionMachine
            (EvaluateRightSection context operatorSymbol)
            (EvaluateExpression (evaluationContextForLambdaChild 0 context) rightExpr)
        EBlock _ statements ->
          stepBlock expressionMachine context statements

    evaluateTypeApplicationNormally expressionMachine context functionExpr =
      suspendEvaluation
        expressionMachine
        FinishTypeApplication
        (EvaluateExpression (evaluationContextForLambdaChild 0 context) functionExpr)

    stepBlock expressionMachine context statements =
      case reverse statements of
        SExpr _ terminalExpr : reversedPrefix -> do
          let prefixStatements = reverse reversedPrefix
          scopeResult <-
            evalScopeWithHost
              host
              preludeModulePath
              Set.empty
              (evaluationModulePath context)
              EvaluateEntryModule
              builtinMode
              (evaluationEnvironmentMayReachHostCells context)
              (evaluationEnvironment context)
              prefixStatements
          let terminalContext =
                (evaluationContextForLambdaChild (length prefixStatements) context)
                  { evaluationModulePath =
                      runtimeModulePathAfterStatements
                        (evaluationModulePath context)
                        prefixStatements,
                    evaluationEnvironment = scopeResultEnvironment scopeResult,
                    evaluationEnvironmentMayReachHostCells =
                      scopeResultEnvironmentMayReachHostCells scopeResult
                  }
          continueWith (EvaluateExpression terminalContext terminalExpr) expressionMachine
        _ -> do
          _ <-
            evalScopeWithHost
              host
              preludeModulePath
              Set.empty
              (evaluationModulePath context)
              EvaluateEntryModule
              builtinMode
              (evaluationEnvironmentMayReachHostCells context)
              (evaluationEnvironment context)
              statements
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
            forceRuntimeValueWithHost host builtinMode functionValue
          continueWith (ApplyCallable forcedFunctionValue argumentValue) machine
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
            (appendRuntimeResultObligation (ApplyFunctionResultHint typeHint) machine)
        VSectionLeft operatorSymbol leftValue
          | operatorSymbol == "$" ->
              continueWith (ApplyCallable leftValue argumentValue) profiledMachine
          | otherwise -> do
              resultValue <-
                evalBinaryWithHost host builtinMode operatorSymbol leftValue argumentValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VSectionRight operatorSymbol rightValue
          | operatorSymbol == "$" ->
              continueWith (ApplyCallable argumentValue rightValue) profiledMachine
          | otherwise -> do
              resultValue <-
                evalBinaryWithHost host builtinMode operatorSymbol argumentValue rightValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VDeclaredOperatorRightSection _ operatorValue rightValue ->
          suspendEvaluation
            profiledMachine
            (ApplyDeclaredRightSectionOperand rightValue)
            (ApplyCallable operatorValue argumentValue)
        VClosure closure -> do
          hintedArgumentValue <-
            case runtimeClosureTypeHint closure of
              Just typeHint ->
                liftRuntimeResult (applyRuntimeFunctionArgumentHint typeHint argumentValue)
              Nothing -> pure argumentValue
          let resultObligation =
                case runtimeClosureTypeHint closure of
                  Just typeHint -> ApplyFunctionResultHint typeHint
                  Nothing -> AttachDefaultIntegerResult
              (nextClosureBaseName, nextLambdaStage) =
                nextClosureOrigin (runtimeClosureCallableIdentity closure)
              closureContext =
                EvaluationContext
                  { evaluationModulePath = runtimeClosureModulePath closure,
                    evaluationBuiltinMode = builtinMode,
                    evaluationEnvironment =
                      Map.insert
                        (runtimeClosureParameter closure)
                        (Right hintedArgumentValue)
                        (runtimeClosureEnvironment closure),
                    evaluationEnvironmentMayReachHostCells =
                      runtimeClosureEnvironmentMayReachHostCells closure,
                    evaluationLambdaCaptureHints =
                      runtimeClosureLambdaCaptureHints closure,
                    evaluationClosureBaseName = nextClosureBaseName,
                    evaluationLambdaStage = nextLambdaStage
                  }
          continueWith
            (EvaluateExpression closureContext (runtimeClosureBody closure))
            (appendRuntimeResultObligation resultObligation profiledMachine)
        VBuiltin builtinFunction capturedArgs -> do
          resultValue <-
            applyBuiltinWithHost
              observeStatistics
              observeProfile
              host
              builtinMode
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
                      builtinMode
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
        VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs ->
          let arguments = appendRuntimeAppliedArgument argumentValue capturedArgs
              preferredCandidates =
                preferredRuntimeMethodCandidates
                  classParameter
                  methodSignature
                  arguments
                  candidates
           in case runtimeMethodCandidatesInOrder preferredCandidates of
                [] ->
                  throwRuntimeDiagnostic
                    (runtimeDiagnostic E3026 ("no matching qualified method body '" <> methodKey <> "'"))
                [RuntimeMethodCandidate _ methodCell] -> do
                  methodValue <- liftRuntimeResult methodCell
                  suspendEvaluation
                    profiledMachine
                    (ApplyRemainingArguments (runtimeAppliedArgumentsInOrder arguments))
                    (ForceRuntimeValue methodValue)
                _
                  | runtimeQualifiedMethodIsFullyApplied
                      classParameter
                      methodSignature
                      arguments
                      preferredCandidates ->
                      throwRuntimeDiagnostic
                        (runtimeDiagnostic E3026 ("ambiguous qualified method body '" <> methodKey <> "'"))
                  | otherwise ->
                      continueWith
                        ( ReturnRuntimeValue
                            ( VQualifiedMethodApplication
                                methodKey
                                classParameter
                                methodSignature
                                preferredCandidates
                                arguments
                            )
                        )
                        profiledMachine
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
    VDeclaredOperatorRightSection {} -> Just ClosureApplication
    VConstructorApplication {} -> Just ConstructorApplication
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
    VDeclaredOperatorRightSection operatorSymbol _ _ ->
      Just (GeneratedCallable ("declared right section " <> operatorSymbol))
    VConstructorApplication shape _ ->
      Just (ConstructorCallable (renderName (runtimeConstructorName shape)))
    VQualifiedMethodApplication methodKey _ _ _ _ -> Just (MethodCallable methodKey)
    _ -> Nothing

resumeEvaluationFrame ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  EvaluationMachine ->
  EvaluationFrame ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
resumeEvaluationFrame observeStatistics observeProfile host builtinMode machine frame runtimeValue =
  case frame of
    EvaluateApplicationArgument context argumentExpr ->
      suspendEvaluation
        machine
        (ApplyEvaluatedFunction runtimeValue)
        (EvaluateExpression (evaluationContextForLambdaChild 1 context) argumentExpr)
    ApplyEvaluatedFunction functionValue ->
      continueWith (ApplyCallable functionValue runtimeValue) machine
    EvaluateListElement context nextChildIndex reversedElements remainingElements ->
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
            (EvaluateListElement context (nextChildIndex + 1) (runtimeValue : reversedElements) rest)
            (EvaluateExpression (evaluationContextForLambdaChild nextChildIndex context) nextElement)
    EvaluateTupleElement context nextChildIndex reversedElements remainingElements ->
      case remainingElements of
        [] -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction TupleConstruction 1)
          continueWith
            (ReturnRuntimeValue (VTuple (reverse (runtimeValue : reversedElements))))
            machine
        nextElement : rest ->
          suspendEvaluation
            machine
            (EvaluateTupleElement context (nextChildIndex + 1) (runtimeValue : reversedElements) rest)
            (EvaluateExpression (evaluationContextForLambdaChild nextChildIndex context) nextElement)
    EvaluateIfBranch context thenExpr elseExpr ->
      case runtimeValue of
        VBool True ->
          continueWith
            (EvaluateExpression (evaluationContextForLambdaChild 1 context) thenExpr)
            machine
        VBool False ->
          continueWith
            (EvaluateExpression (evaluationContextForLambdaChild 2 context) elseExpr)
            machine
        other ->
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3003 ("runtime branch condition must be Bool, found " <> renderRuntimeType other))
    EvaluateCaseArms context caseArms ->
      continueCaseEvaluation observeStatistics machine context runtimeValue caseArms
    EvaluateCaseGuard context scrutineeValue armEnv armIndex bodyExpr remainingArms ->
      case runtimeValue of
        VBool True ->
          continueWith
            ( EvaluateExpression
                ( (evaluationContextForLambdaChild (2 + (2 * armIndex)) context)
                    { evaluationEnvironment = armEnv
                    }
                )
                bodyExpr
            )
            machine
        VBool False ->
          continueCaseEvaluation observeStatistics machine context scrutineeValue remainingArms
        other ->
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3003 ("runtime case guard must be Bool, found " <> renderRuntimeType other))
    EvaluateBuiltinRightOperand context operatorSymbol rightExpr ->
      suspendEvaluation
        machine
        (ApplyBuiltinBinary operatorSymbol runtimeValue)
        (EvaluateExpression (evaluationContextForLambdaChild 1 context) rightExpr)
    ApplyBuiltinBinary operatorSymbol leftValue
      | operatorSymbol == "$" ->
          continueWith (ApplyCallable leftValue runtimeValue) machine
      | otherwise -> do
          recordRuntimeStatisticWhen observeStatistics (recordRuntimeApplication OperatorApplication)
          recordRuntimeProfileOpenWhen observeProfile (OperatorCallable operatorSymbol)
          resultValue <-
            evalBinaryWithHost
              host
              builtinMode
              operatorSymbol
              leftValue
              runtimeValue
          continueWith
            (ReturnRuntimeValue resultValue)
            (closeRuntimeProfileOnReturn observeProfile machine)
    EvaluateDeclaredOperatorLeft context leftExpr rightExpr ->
      suspendEvaluation
        machine
        (ApplyDeclaredOperatorLeft context runtimeValue rightExpr)
        (EvaluateExpression (evaluationContextForLambdaChild 0 context) leftExpr)
    ApplyDeclaredOperatorLeft context operatorValue rightExpr ->
      suspendEvaluation
        machine
        (EvaluateDeclaredOperatorRight context rightExpr)
        (ApplyCallable operatorValue runtimeValue)
    EvaluateDeclaredOperatorRight context rightExpr ->
      suspendEvaluation
        machine
        (ApplyEvaluatedFunction runtimeValue)
        (EvaluateExpression (evaluationContextForLambdaChild 1 context) rightExpr)
    EvaluateLeftSection context operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          continueWith
            (ReturnRuntimeValue (VSectionLeft operatorSymbol runtimeValue))
            machine
      | otherwise -> do
          operatorValue <-
            liftRuntimeResult
              (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
          suspendEvaluation
            machine
            (ApplyForcedCallable runtimeValue)
            (ForceRuntimeValue operatorValue)
    ApplyForcedCallable argumentValue ->
      continueWith (ApplyCallable runtimeValue argumentValue) machine
    EvaluateRightSection context operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          continueWith
            (ReturnRuntimeValue (VSectionRight operatorSymbol runtimeValue))
            machine
      | otherwise -> do
          operatorValue <-
            liftRuntimeResult
              (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
          suspendEvaluation
            machine
            (BuildDeclaredRightSection context operatorSymbol runtimeValue)
            (ForceRuntimeValue operatorValue)
    BuildDeclaredRightSection _ operatorSymbol rightValue ->
      do
        recordRuntimeStatisticWhen observeStatistics (recordRuntimeClosureCreation 2)
        continueWith
          ( ReturnRuntimeValue
              (VDeclaredOperatorRightSection operatorSymbol runtimeValue rightValue)
          )
          machine
    ApplyDeclaredRightSectionOperand rightValue ->
      continueWith (ApplyCallable runtimeValue rightValue) machine
    FinishTypeApplication ->
      continueWith (ReturnRuntimeValue runtimeValue) machine
    ApplyRemainingArguments arguments ->
      applyRemainingArguments machine runtimeValue arguments

continueCaseEvaluation ::
  (Monad m) =>
  Bool ->
  EvaluationMachine ->
  EvaluationContext ->
  RuntimeValue ->
  [(Int, CaseArm 'Analyzed)] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
continueCaseEvaluation observeStatistics machine context scrutineeValue =
  chooseArm
  where
    chooseArm remainingArms =
      case remainingArms of
        [] ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3022 "pattern case matched no arms")
        (armIndex, caseArm@(CaseArm _ casePattern _ _)) : rest -> do
          recordRuntimeStatisticWhen observeStatistics recordRuntimePatternAttempt
          case matchCaseArm
            (evaluationModulePath context)
            (evaluationEnvironment context)
            scrutineeValue
            caseArm of
            Nothing -> chooseArm rest
            Just (armEnv, Nothing, bodyExpr) -> do
              recordRuntimeStatisticWhen
                observeStatistics
                (recordRuntimePatternMatch (Set.size (patternBinderNames casePattern)))
              continueWith
                ( EvaluateExpression
                    ( (evaluationContextForLambdaChild (2 + (2 * armIndex)) context)
                        { evaluationEnvironment = armEnv
                        }
                    )
                    bodyExpr
                )
                machine
            Just (armEnv, Just guardExpr, bodyExpr) -> do
              recordRuntimeStatisticWhen
                observeStatistics
                (recordRuntimePatternMatch (Set.size (patternBinderNames casePattern)))
              suspendEvaluation
                machine
                (EvaluateCaseGuard context scrutineeValue armEnv armIndex bodyExpr rest)
                ( EvaluateExpression
                    ( (evaluationContextForLambdaChild (1 + (2 * armIndex)) context)
                        { evaluationEnvironment = armEnv
                        }
                    )
                    guardExpr
                )

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

prependRuntimeResultObligation :: RuntimeResultObligation -> RuntimeReturnPolicy -> RuntimeReturnPolicy
prependRuntimeResultObligation obligation policy@(RuntimeReturnPolicy obligations) =
  case obligations of
    existing : _
      | equivalentIdempotentObligation obligation existing -> policy
    _ -> RuntimeReturnPolicy (obligation : obligations)

equivalentIdempotentObligation :: RuntimeResultObligation -> RuntimeResultObligation -> Bool
equivalentIdempotentObligation leftObligation rightObligation =
  case (leftObligation, rightObligation) of
    (AttachDefaultIntegerResult, AttachDefaultIntegerResult) -> True
    (ApplyFunctionResultHint leftHint, ApplyFunctionResultHint rightHint) ->
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
        ApplyFunctionResultHint typeHint ->
          liftRuntimeResult (applyRuntimeFunctionResultHint typeHint currentValue)
        ApplyExplicitResultHint typeHint ->
          liftRuntimeResult (applyExplicitTypeApplicationResultHint typeHint currentValue)
        ApplyExpressionRuntimePlan modulePath runtimePlan ->
          liftRuntimeResult (applyExpressionRuntimePlan modulePath runtimePlan currentValue)
        AttachDefaultIntegerResult ->
          liftRuntimeResult (attachDefaultBindingIntegerTarget currentValue)
        CloseRuntimeProfileFrame -> do
          lift (modifyRuntimeObservation recordRuntimeProfileClose)
          pure currentValue

expressionRuntimePlanOf :: Expr 'Analyzed -> RuntimePlan
expressionRuntimePlanOf expression =
  case expression of
    ELit node _ -> plan node
    EVar node _ -> plan node
    ELambda node _ _ -> plan node
    EOperatorValue node _ -> plan node
    EList node _ -> plan node
    ETuple node _ -> plan node
    EApply node _ _ -> plan node
    ETypeApplication node _ _ _ -> plan node
    EIf node _ _ _ -> plan node
    EPatternCase node _ _ -> plan node
    EBinary node _ _ _ -> plan node
    ESectionLeft node _ _ -> plan node
    ESectionRight node _ _ -> plan node
    EBlock node _ -> plan node
  where
    plan = expressionRuntimePlan . coreNodeFacts

applyExpressionRuntimePlan :: Maybe [Text] -> RuntimePlan -> RuntimeValue -> Either Diagnostic RuntimeValue
applyExpressionRuntimePlan modulePath (RuntimePlan obligations) initialValue =
  foldM applyObligation initialValue obligations
  where
    applyObligation runtimeValue obligation =
      case obligation of
        InstantiateTypes instantiatedTypes ->
          foldM applyInstantiation runtimeValue instantiatedTypes
        SupplyEvidence evidenceReferences ->
          Right (selectRuntimeEvidence evidenceReferences runtimeValue)
        SpecializeNumericLiteral numericTarget ->
          specializeNumericLiteral numericTarget runtimeValue
        ConstrainResult semanticType ->
          constrainRuntimeResult semanticType runtimeValue

    applyInstantiation runtimeValue semanticType
      | not (Foldable.null semanticType) = Right runtimeValue
      | otherwise =
          let typeHint = qualifyRuntimeType modulePath semanticType
           in if isFunctionValue runtimeValue
                then Right (VAnnotated (RuntimeTypeApplication typeHint) runtimeValue)
                else
                  applyRuntimeTypeHint
                    (fromMaybe typeHint (explicitTypeApplicationRuntimeValueHint typeHint runtimeValue))
                    runtimeValue

    specializeNumericLiteral numericTarget runtimeValue =
      case numericTarget of
        -- Defaultable integral literals stay untyped until their enclosing
        -- analyzed result constraint decides whether contextual typing won or
        -- the ordinary Int64 binding default applies.
        DefaultIntegerTarget -> Right runtimeValue
        ConcreteNumericTarget targetType ->
          evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue

    constrainRuntimeResult semanticType runtimeValue = case semanticType of
      SemanticInt -> attachDefaultBindingIntegerTarget runtimeValue
      _ | Foldable.null semanticType -> applyRuntimeTypeHint (qualifyRuntimeType modulePath semanticType) runtimeValue
      _ -> Right runtimeValue

selectRuntimeEvidence :: NonEmpty.NonEmpty EvidenceReference -> RuntimeValue -> RuntimeValue
selectRuntimeEvidence evidenceReferences runtimeValue =
  case runtimeValue of
    VAnnotated annotation innerValue ->
      VAnnotated annotation (selectRuntimeEvidence evidenceReferences innerValue)
    VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs ->
      VQualifiedMethodApplication
        methodKey
        classParameter
        methodSignature
        (filterRuntimeMethodCandidates selected candidates)
        capturedArgs
    _ -> runtimeValue
  where
    selected (RuntimeMethodCandidate runtimeEvidenceValue _) =
      any (runtimeEvidenceMatches runtimeEvidenceValue) evidenceReferences

runtimeEvidenceMatches :: EvidenceReference -> EvidenceReference -> Bool
runtimeEvidenceMatches candidate reference =
  canonicalCapability (evidenceImplementation candidate) (evidenceCapability candidate)
    == canonicalCapability (evidenceImplementation reference) (evidenceCapability reference)
    && evidenceImplementation candidate == evidenceImplementation reference
    && evidenceMethod candidate == evidenceMethod reference

-- Evidence is owned by its implementation, regardless of the module evaluating
-- the reference. This also gives standalone references the same qualification
-- as their runtime candidates.
canonicalCapability :: ImplId -> CapabilityId -> CapabilityId
canonicalCapability (ImplId (modulePath, _)) (CapabilityId capabilityName) =
  CapabilityId
    (runtimeDefinitionNameIn CapabilityNamespace (Just (NonEmpty.toList (modulePathTextSegments modulePath))) capabilityName)

runtimeEvidence ::
  Maybe [Text] ->
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
  where
    implementationId = ImplId (runtimeModulePath modulePath, implementationNodeId)

runtimeModulePath :: Maybe [Text] -> ModulePath
runtimeModulePath modulePath =
  case modulePath >>= NonEmpty.nonEmpty of
    Nothing -> standaloneModulePath
    Just path -> mkModulePath (fmap mkIdentifier path)

prepareRuntimeEvidence :: RuntimePlan -> RuntimeValue -> RuntimeValue
prepareRuntimeEvidence (RuntimePlan obligations) initialValue =
  foldl' applyEvidence initialValue obligations
  where
    applyEvidence runtimeValue obligation =
      case obligation of
        SupplyEvidence evidenceReferences -> selectRuntimeEvidence evidenceReferences runtimeValue
        _ -> runtimeValue

lookupDeclaredOperatorCell :: Text -> RuntimeEnv -> Either Diagnostic RuntimeValue
lookupDeclaredOperatorCell operatorSymbol env =
  case Map.lookup (operatorBindingName operatorSymbol) env of
    Just runtimeCell -> runtimeCell
    Nothing ->
      Left
        ( runtimeDiagnostic
            E3027
            ("operator '" <> operatorSymbol <> "' has no executable binding")
        )

evalValueWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  Bool ->
  Expr 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalValueWithHost host currentModulePath builtinMode env envMayReachHostCells expr =
  runEvaluationMachine
    host
    EvaluationContext
      { evaluationModulePath = currentModulePath,
        evaluationBuiltinMode = builtinMode,
        evaluationEnvironment = env,
        evaluationEnvironmentMayReachHostCells = envMayReachHostCells,
        evaluationLambdaCaptureHints = collectLambdaCaptureHints expr,
        evaluationClosureBaseName = "<entry>",
        evaluationLambdaStage = 1
      }
    expr

evalScopeWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModulePath ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Bool ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHost host preludePath preludeStatementIndices currentModulePath evaluationMode builtinMode initialEnvMayReachHostCells initialEnv statements = do
  scopeId <- lift freshDeferredHostScopeId
  observationEnabled <-
    lift
      (runtimeObservationEnabled . runtimeHostEvaluationObservation <$> get)
  evalScopeWithHostInstance
    observationEnabled
    scopeId
    host
    preludePath
    preludeStatementIndices
    currentModulePath
    evaluationMode
    builtinMode
    initialEnvMayReachHostCells
    initialEnv
    statements

evalScopeWithHostInstance ::
  (Monad m) =>
  Bool ->
  DeferredHostScopeId ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModulePath ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Bool ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHostInstance observationEnabled scopeId host preludePath preludeStatementIndices currentModulePath evaluationMode builtinMode initialEnvMayReachHostCells initialEnv statements =
  go initialEnvMayReachHostCells initialEnv Nothing indexedStatements
  where
    scopePlan =
      buildRuntimeScopePlan
        preludePath
        preludeStatementIndices
        currentModulePath
        builtinMode
        (Map.keysSet initialEnv)
        statements
    indexedStatements = scopePlanIndexedStatements scopePlan
    modulePathForStatement = scopePlanModulePathForStatement scopePlan

    go hostCellsMayBeReachable env lastValue [] =
      pure (ScopeResult env lastValue hostCellsMayBeReachable)
    go hostCellsMayBeReachable env _ remaining@((statementIndex, statement) : rest)
      | statementMayUsePureChunk hostCellsMayBeReachable statementIndex statement = do
          let (pureChunk, remainingAfterChunk) =
                span
                  (\(index, chunkStatement) -> statementMayUsePureChunk hostCellsMayBeReachable index chunkStatement)
                  remaining
              chunkPreludeStatementIndices =
                Set.fromList
                  [ localIndex
                  | (localIndex, (globalIndex, _)) <- zip [0 ..] pureChunk,
                    Set.member globalIndex preludeStatementIndices
                  ]
          scopeResult <-
            liftRuntimeResult
              ( evaluateRuntimeScopePureRequest
                  RuntimeScopeRequest
                    { runtimeScopeSourceUnitStatementIndices = chunkPreludeStatementIndices,
                      runtimeScopePreludeModulePath = preludePath,
                      runtimeScopeCurrentModulePath = modulePathForStatement statementIndex,
                      runtimeScopeEvaluationMode = evaluationMode,
                      runtimeScopeBuiltinMode = builtinMode,
                      runtimeScopeInitialEnvironment = env,
                      runtimeScopeStatements = map snd pureChunk
                    }
              )
          go
            hostCellsMayBeReachable
            (scopeResultEnvironment scopeResult)
            (scopeResultValue scopeResult)
            remainingAfterChunk
      | otherwise =
          case statement of
            SLet _ name _ ->
              let bindingCell = hostBindingCell hostCellsMayBeReachable statementIndex env
               in case evaluationMode of
                    EvaluateDependencyModule ->
                      go
                        True
                        (LazyMap.insert name bindingCell env)
                        Nothing
                        rest
                    EvaluateEntryModule -> do
                      value <- forceRuntimeCellWithHost bindingCell
                      go True (Map.insert name (Right value) env) Nothing rest
            SImpl implementationNode capabilityName _ methods ->
              go
                True
                (insertImplMethodsWithHost (modulePathForStatement statementIndex) implementationNode capabilityName methods env)
                Nothing
                rest
            SExpr _ valueExpr ->
              case evaluationMode of
                EvaluateDependencyModule -> go hostCellsMayBeReachable env Nothing rest
                EvaluateEntryModule -> do
                  value <-
                    evalValueWithHost
                      host
                      (modulePathForStatement statementIndex)
                      builtinMode
                      env
                      hostCellsMayBeReachable
                      valueExpr
                  go hostCellsMayBeReachable env (Just value) rest
            _ ->
              throwRuntimeDiagnostic
                (runtimeDiagnostic E3020 "internal runtime error: unsupported direct host statement")

    statementMayUsePureChunk hostCellsMayBeReachable statementIndex statement
      | not observationEnabled =
          not (statementNeedsDirectHostEvaluation hostCellsMayBeReachable statementIndex statement)
      | otherwise =
          case statement of
            SLet {} -> False
            SImpl {} -> False
            SExpr {} -> False
            _ -> True

    -- Once direct host evaluation has introduced a deferred cell (or a value
    -- that can capture one), later bindings must stay on the same host lane.
    -- Sending them through the pure scope evaluator would install the disabled
    -- host inside their lazy cells and split cache/effect state when forced.
    statementNeedsDirectHostEvaluation hostCellsMayBeReachable statementIndex statement =
      case statement of
        SLet _ _ valueExpr ->
          hostCellsMayBeReachable
            || runtimeExprRequiresHost valueExpr
            || scopePlanIsHostRecursiveBinding scopePlan statementIndex
            || not (scopePlanIsRecursiveBinding scopePlan statementIndex)
        SImpl _ _ _ methods -> hostCellsMayBeReachable || any implMethodRequiresHost methods
        SExpr {} -> True
        _ -> False

    implMethodRequiresHost (ImplMethod _ _ methodExpr) = runtimeExprRequiresHost methodExpr

    hostBindingCell hostCellsMayBeReachable statementIndex baseEnv =
      case scopePlanRecursiveGroupAt scopePlan statementIndex of
        Just groupMembers ->
          makeHostBindingCell hostCellsMayBeReachable statementIndex recursiveEnv baseEnv
          where
            recursiveEnv = foldl' insertGroupMember baseEnv groupMembers

            insertGroupMember envAcc groupIndex =
              case scopePlanBindingNameAt scopePlan groupIndex of
                Just groupName
                  | Map.notMember groupName baseEnv ->
                      LazyMap.insert
                        groupName
                        (makeHostBindingCell hostCellsMayBeReachable groupIndex recursiveEnv baseEnv)
                        envAcc
                _ -> envAcc
        Nothing ->
          case scopePlanBindingNameAt scopePlan statementIndex of
            Just bindingName
              | scopePlanIsSelfRecursiveFunction scopePlan statementIndex,
                Map.notMember bindingName baseEnv ->
                  selfCell
              where
                selfCell = makeHostBindingCell hostCellsMayBeReachable statementIndex selfEnv baseEnv
                selfEnv = LazyMap.insert bindingName selfCell baseEnv
            _ -> makeHostBindingCell hostCellsMayBeReachable statementIndex baseEnv baseEnv

    makeHostBindingCell hostCellsMayBeReachable statementIndex capturedEnv diagnosticBaseEnv =
      case scopePlanStatementAt scopePlan statementIndex of
        Just (SLet bindingNode bindingName valueExpr) ->
          Right
            ( VDeferredHostBinding
                (DeferredHostBindingKey scopeId (coreNodeId bindingNode) bindingName)
                (recursiveBindingDiagnostic hostCellsMayBeReachable statementIndex diagnosticBaseEnv)
                (modulePathForStatement statementIndex)
                valueExpr
                capturedEnv
            )
        _ ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: expected host binding statement")

    recursiveBindingDiagnostic hostCellsMayBeReachable statementIndex diagnosticBaseEnv =
      case scopePlanRecursiveGroupAt scopePlan statementIndex of
        Just groupMembers
          | not hostCellsMayBeReachable,
            not (scopePlanIsHostRecursiveBinding scopePlan statementIndex) ->
              case evaluateRuntimeScopePureRequest
                RuntimeScopeRequest
                  { runtimeScopeSourceUnitStatementIndices = groupPreludeStatementIndices,
                    runtimeScopePreludeModulePath = preludePath,
                    runtimeScopeCurrentModulePath = modulePathForStatement statementIndex,
                    runtimeScopeEvaluationMode = EvaluateEntryModule,
                    runtimeScopeBuiltinMode = builtinMode,
                    runtimeScopeInitialEnvironment = diagnosticBaseEnv,
                    runtimeScopeStatements = groupStatements
                  } of
                Left diagnostic -> diagnostic
                Right _ -> recursiveBindingFallback
          where
            indexedGroupStatements =
              [ (groupIndex, groupStatement)
              | groupIndex <- groupMembers,
                Just groupStatement <- [scopePlanStatementAt scopePlan groupIndex]
              ]
            groupStatements = map snd indexedGroupStatements
            groupPreludeStatementIndices =
              Set.fromList
                [ localIndex
                | (localIndex, (globalIndex, _)) <- zip [0 ..] indexedGroupStatements,
                  Set.member globalIndex preludeStatementIndices
                ]
            recursiveBindingFallback =
              runtimeDiagnostic E3021 "runtime recursive binding has no concrete value"
        _ ->
          runtimeDiagnostic E3021 "runtime recursive host binding has no concrete value"

    forceRuntimeCellWithHost bindingCell =
      liftRuntimeResult bindingCell
        >>= forceRuntimeValueWithHost host builtinMode

    insertImplMethodsWithHost methodModulePath implementationNode capabilityName methods env =
      case statementDeclarationFact (coreNodeFacts implementationNode) of
        ImplementationDeclaration _ [implTarget] -> methodEnv
          where
            runtimeImplTarget = qualifyRuntimeType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodCandidates =
              map
                ( \(ImplMethod methodNode methodName methodExpr) ->
                    let qualifiedMethodName = qualifiedMemberName capabilityName methodName
                        evidence = runtimeEvidence methodModulePath (coreNodeId implementationNode) capabilityName methodName runtimeImplTarget
                     in ( qualifiedMethodName,
                          RuntimeMethodCandidate
                            evidence
                            ( Right
                                ( VDeferredHostBinding
                                    (DeferredHostBindingKey scopeId (coreNodeId methodNode) qualifiedMethodName)
                                    (runtimeDiagnostic E3021 "runtime recursive host binding has no concrete value")
                                    methodModulePath
                                    methodExpr
                                    methodEnv
                                )
                            )
                        )
                )
                methods

            insertCandidate envAcc (methodName, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc

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
        _ -> env

evalHostBindingValue ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  ResolvedName ->
  Expr 'Analyzed ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalHostBindingValue host currentModulePath builtinMode env bindingName valueExpr =
  nameRuntimeClosureBinding currentModulePath bindingName
    <$> evalValueWithHost host currentModulePath builtinMode env True valueExpr

forceQualifiedMethodValueWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
forceQualifiedMethodValueWithHost host builtinMode runtimeValue =
  case runtimeValue of
    VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethodWithHost host builtinMode methodKey classParameter methodSignature candidates capturedArgs
    _ -> pure runtimeValue

forceRuntimeValueWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
forceRuntimeValueWithHost host builtinMode runtimeValue =
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
                      builtinMode
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
    VAnnotated annotation innerValue ->
      VAnnotated annotation <$> forceRuntimeValueWithHost host builtinMode innerValue
    _ ->
      forceQualifiedMethodValueWithHost host builtinMode runtimeValue

applyRuntimeFunctionWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyRuntimeFunctionWithHost host builtinMode functionValue argumentValue =
  runCallableMachine
    host
    builtinMode
    functionValue
    argumentValue

applyQualifiedMethodWithHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Text ->
  InferenceVariable ->
  AnalyzedType ->
  RuntimeMethodCandidates ->
  RuntimeAppliedArguments ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyQualifiedMethodWithHost host builtinMode methodKey classParameter methodSignature candidates arguments =
  case runtimeMethodCandidatesInOrder preferredCandidates of
    [] -> throwRuntimeDiagnostic (runtimeDiagnostic E3026 ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] -> do
      methodValue <-
        liftRuntimeResult methodCell
          >>= forceRuntimeValueWithHost host builtinMode
      foldM
        (applyRuntimeFunctionWithHost host builtinMode)
        methodValue
        (runtimeAppliedArgumentsInOrder arguments)
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments preferredCandidates ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3026 ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          pure (VQualifiedMethodApplication methodKey classParameter methodSignature preferredCandidates arguments)
  where
    preferredCandidates =
      preferredRuntimeMethodCandidates
        classParameter
        methodSignature
        arguments
        candidates

applyBuiltinWithHost ::
  (Monad m) =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyBuiltinWithHost observeStatistics observeProfile host builtinMode builtinFunction arguments
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
          builtinMode
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
  BuiltinResolutionMode ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBuiltinWithHost observeStatistics observeProfile host builtinMode builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinReadTextRaw, [VText path]) -> do
      beginHostOperation observeStatistics observeProfile ReadTextHostOperation
      outcome <- lift (runtimeHostReadText host path)
      endHostOperation observeProfile
      pure (rawHostOutcome VText outcome)
    (BuiltinWriteTextRaw, [VText path, VText contents]) -> do
      beginHostOperation observeStatistics observeProfile WriteTextHostOperation
      outcome <- lift (runtimeHostWriteText host path contents)
      endHostOperation observeProfile
      pure (rawHostOutcome (const (VText "")) outcome)
    (BuiltinReadStdinRaw, [VTuple []]) -> do
      beginHostOperation observeStatistics observeProfile ReadStdinHostOperation
      outcome <- lift (runtimeHostReadStdin host)
      endHostOperation observeProfile
      pure (rawHostOutcome VText outcome)
    (BuiltinWriteStdoutRaw, [VText contents]) -> do
      beginHostOperation observeStatistics observeProfile WriteStdoutHostOperation
      outcome <- lift (runtimeHostWriteStdout host contents)
      endHostOperation observeProfile
      pure (rawHostOutcome (const (VText "")) outcome)
    (BuiltinWriteStderrRaw, [VText contents]) -> do
      beginHostOperation observeStatistics observeProfile WriteStderrHostOperation
      outcome <- lift (runtimeHostWriteStderr host contents)
      endHostOperation observeProfile
      pure (rawHostOutcome (const (VText "")) outcome)
    (BuiltinArguments, [VTuple []]) -> do
      beginHostOperation observeStatistics observeProfile ArgumentsHostOperation
      argumentsText <- lift (runtimeHostArguments host)
      endHostOperation observeProfile
      pure (VList (map VText argumentsText) (Just (SemanticList SemanticText)))
    (BuiltinExit, [statusValue])
      | Just status <- runtimeHostExitStatus statusValue,
        status >= 0 && status <= 255 -> do
          beginHostOperation observeStatistics observeProfile ExitHostOperation
          exitResult <- lift (runtimeHostExit host status)
          endHostOperation observeProfile
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
        (applyRuntimeFunctionWithHost host builtinMode)
        builtinFunction
        arguments

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
  BuiltinResolutionMode ->
  Text ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBinaryWithHost host builtinMode operatorSymbol leftValue rightValue =
  evalBinary
    RuntimeDiagnostic
    (applyRuntimeFunctionWithHost host builtinMode)
    operatorSymbol
    leftValue
    rightValue
