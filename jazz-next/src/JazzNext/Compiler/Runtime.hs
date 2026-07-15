{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Interpreter/runtime for the currently-supported core language. Its explicit
-- evaluation machine mirrors the builtin/operator contracts enforced by analysis
-- and type inference while keeping Jazz recursion off the Haskell call stack.
module JazzNext.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeControl (..),
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue (..),
    RuntimeExplicitResultHints,
    data VExplicitResultHints,
    prependRuntimeExplicitResultHint,
    runtimeExplicitResultHintsInOrder,
    ScopeResult (..),
    evaluateModuleScope,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements,
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    evaluateRuntimeExprObserved,
    evaluateRuntimeExprWithHost,
    evaluateRuntimeExprWithHostObserved,
    evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements,
    evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved,
    evaluateModuleScopeWithHost,
    evaluateModuleScopeWithHostAndSourceUnitStatements,
    evaluateModuleScopeWithRequiredHost,
    evaluateModuleScopeWithRequiredEvaluationHost,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost,
    runtimeValueExactlyMatchesConstraint,
    renderRuntimeValue,
    untypedIntMetadata
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
    throwE
  )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( get,
    modify',
    put,
    runStateT
  )
import Data.Functor.Identity (runIdentity)
import Data.List (scanl')
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Lazy as LazyIntMap
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbolInMode
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    qualifiedMethodKey,
    signaturePayloadConstraintType,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    ResolvedNameOrigin (..),
    generatedName,
    identifierText,
    operatorBindingName,
    qualifiedMemberName,
    renderName
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.Pattern
  ( patternBinderNames
  )
import JazzNext.Compiler.Runtime.Primitives
  ( evalBinary,
    evalBuiltin
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeApplicationKind (..),
    RuntimeBuiltinKind (..),
    RuntimeCallableIdentity (..),
    RuntimeConstructionKind (..),
    RuntimeDeferredCacheKind (..),
    RuntimeHostOperationKind (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
    RuntimeObservationState,
    finishRuntimeObservationResult,
    initialRuntimeObservationState,
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
    recordRuntimeProfileOpen,
    recordRuntimeTransition,
    restoreRuntimeContinuationDepth,
    runtimeObservationEnabled,
    runtimeObservationProfileEnabled,
    runtimeObservationStatisticsEnabled
  )
import JazzNext.Compiler.Runtime.ScopePlan
  ( buildRuntimeScopePlan,
    exprDefinitelyNotFunctionValue,
    runtimeExprRequiresHost,
    runtimeModulePathAfterStatements,
    runtimeSignatureNumericTarget,
    scopePlanBindingNameAt,
    scopePlanIndexedStatements,
    scopePlanIsHostRecursiveBinding,
    scopePlanIsRecursiveBinding,
    scopePlanIsSelfRecursiveFunction,
    scopePlanModulePathForStatement,
    scopePlanPreviousSignaturePayload,
    scopePlanRecursiveGroupAt,
    scopePlanStatementAt
  )
import JazzNext.Compiler.Runtime.Semantics
  ( applyConstructor,
    applyExplicitTypeApplicationResultHint,
    applyRuntimeFunctionArgumentHint,
    applyRuntimeFunctionResultHint,
    applyRuntimeTypeHint,
    attachDefaultBindingIntegerTarget,
    attachRuntimeTypeHint,
    convertFloatToNumericTarget,
    convertIntegerToNumericTarget,
    evalNumericConversion,
    explicitTypeApplicationRuntimeFunctionHint,
    explicitTypeApplicationRuntimeValueHint,
    isFunctionValue,
    literalRuntimeValue,
    matchCaseArm,
    numericConversionBuiltinForTarget,
    preferredRuntimeMethodCandidates,
    renderRuntimeType,
    renderRuntimeValue,
    runtimeConstraintType,
    runtimeConstructorArgument,
    runtimeDefinitionName,
    runtimeDiagnostic,
    runtimeQualifiedMethodIsFullyApplied,
    runtimeValueExactlyMatchesConstraint,
    untypedIntMetadata
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.Runtime.Types
  ( DeferredHostBindingKey (..),
    DeferredHostBindingState (..),
    DeferredHostScopeId (..),
    ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeClosure (..),
    RuntimeEnv,
    RuntimeEvidence (..),
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
    RuntimeControl (..),
    RuntimeMethodCandidate (..),
    RuntimeExplicitResultHints,
    RuntimeValue (..),
    ScopeResult (..),
    attachRuntimeExplicitResultHints,
    constructorIsSaturated,
    foldRuntimeExplicitResultHints,
    data VExplicitResultHints,
    prependRuntimeExplicitResultHint,
    runtimeExplicitResultHintsInOrder
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule
  )
import JazzNext.Compiler.RuntimeHost
  ( HostIOFailure (..),
    RuntimeHost (..),
    RuntimeHostExit (..),
    disabledRuntimeHost,
    hostIOCategoryToken,
    hostIOFailureMessage
  )

runRuntimeHostEvaluation ::
  Monad m =>
  RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m value
runRuntimeHostEvaluation host action =
  fst <$> runRuntimeHostEvaluationWithObservation RuntimeObservationDisabled host action

runRuntimeHostEvaluationWithObservation ::
  Monad m =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m (value, RuntimeObservationState)
runRuntimeHostEvaluationWithObservation observationRequest host action = do
  (value, finalState) <-
    runStateT
      (action (liftRuntimeHost host))
      RuntimeHostEvaluationState
        { runtimeHostEvaluationBindingCache = Map.empty,
          runtimeHostEvaluationNextScopeId = 0,
          runtimeHostEvaluationActiveMachineCount = 0,
          runtimeHostEvaluationContinuationDepth = 0,
          runtimeHostEvaluationObservation = initialRuntimeObservationState observationRequest
        }
  pure (value, runtimeHostEvaluationObservation finalState)

freshDeferredHostScopeId :: Monad m => RuntimeHostEvaluationT m DeferredHostScopeId
freshDeferredHostScopeId = do
  evaluationState <- get
  let scopeId = runtimeHostEvaluationNextScopeId evaluationState
  put
    evaluationState
      { runtimeHostEvaluationNextScopeId = scopeId + 1
      }
  pure (DeferredHostScopeId scopeId)

modifyDeferredHostBindingCache ::
  Monad m =>
  (Map DeferredHostBindingKey DeferredHostBindingState -> Map DeferredHostBindingKey DeferredHostBindingState) ->
  RuntimeHostEvaluationT m ()
modifyDeferredHostBindingCache updateCache =
  modify'
    ( \evaluationState ->
        evaluationState
          { runtimeHostEvaluationBindingCache =
              updateCache (runtimeHostEvaluationBindingCache evaluationState)
          }
    )

modifyRuntimeObservation ::
  Monad m =>
  (RuntimeObservationState -> RuntimeObservationState) ->
  RuntimeHostEvaluationT m ()
modifyRuntimeObservation updateObservation =
  modify'
    ( \evaluationState ->
        evaluationState
          { runtimeHostEvaluationObservation =
              updateObservation (runtimeHostEvaluationObservation evaluationState)
          }
    )

recordRuntimeStatisticWhen ::
  Monad m =>
  Bool ->
  (RuntimeObservationState -> RuntimeObservationState) ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
recordRuntimeStatisticWhen enabled updateObservation =
  if enabled
    then lift (modifyRuntimeObservation updateObservation)
    else pure ()

recordRuntimeProfileOpenWhen ::
  Monad m =>
  Bool ->
  RuntimeCallableIdentity ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
recordRuntimeProfileOpenWhen enabled callableIdentity =
  if enabled
    then lift (modifyRuntimeObservation (recordRuntimeProfileOpen callableIdentity))
    else pure ()

closeRuntimeProfileOnReturn :: Bool -> EvaluationMachine -> EvaluationMachine
closeRuntimeProfileOnReturn enabled machine =
  if enabled
    then appendRuntimeResultObligation CloseRuntimeProfileFrame machine
    else machine

liftRuntimeHost :: Monad m => RuntimeHost m -> RuntimeHost (RuntimeHostEvaluationT m)
liftRuntimeHost host =
  RuntimeHost
    { runtimeHostReadText = lift . runtimeHostReadText host,
      runtimeHostWriteText = \path contents -> lift (runtimeHostWriteText host path contents),
      runtimeHostReadStdin = lift (runtimeHostReadStdin host),
      runtimeHostWriteStdout = lift . runtimeHostWriteStdout host,
      runtimeHostWriteStderr = lift . runtimeHostWriteStderr host,
      runtimeHostArguments = lift (runtimeHostArguments host),
      runtimeHostExit = lift . runtimeHostExit host
    }

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome . evaluateRuntimeExprObserved RuntimeObservationDisabled

evaluateRuntimeExprObserved :: RuntimeObservationRequest -> Expr -> RuntimeObservationResult (Maybe RuntimeValue)
evaluateRuntimeExprObserved observationRequest expr =
  runIdentity
    (evaluateRuntimeExprWithHostObserved observationRequest disabledRuntimeHost expr)

evaluateRuntimeExprWithHost :: Monad m => RuntimeHost m -> Expr -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHost host =
  fmap (runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome)
    . evaluateRuntimeExprWithHostObserved RuntimeObservationDisabled host

evaluateRuntimeExprWithHostObserved ::
  Monad m =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  Expr ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExprWithHostObserved observationRequest host =
  evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved
    observationRequest
    host
    Set.empty
    ResolveKernelOnly
    Map.empty

evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host preludeStatementIndices builtinMode bindingTypeHints expr =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    <$> evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved
      RuntimeObservationDisabled
      host
      preludeStatementIndices
      builtinMode
      bindingTypeHints
      expr

evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved ::
  Monad m =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved observationRequest host preludeStatementIndices builtinMode bindingTypeHints expr =
  {-# SCC "jazz-stage:evaluation" #-}
  case observationRequest of
    RuntimeObservationDisabled -> do
      outcome <-
        evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsUnobserved
          host
          preludeStatementIndices
          builtinMode
          bindingTypeHints
          expr
      pure (RuntimeObservationResult outcome Nothing)
    _ -> do
      (outcome, observationState) <-
        runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
          evaluateRuntimeExprWithRequiredEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
            evaluationHost
            preludeStatementIndices
            builtinMode
            bindingTypeHints
            expr
      pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsUnobserved ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  m (RuntimeOutcome (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsUnobserved host preludeStatementIndices builtinMode bindingTypeHints expr =
  runtimeControlOutcome
    <$> runRuntimeHostEvaluation host (\evaluationHost ->
      evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
        evaluationHost
        preludeStatementIndices
        builtinMode
        bindingTypeHints
        expr)

evaluateRuntimeExprWithRequiredEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExprWithRequiredEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host preludeStatementIndices builtinMode bindingTypeHints expr =
  case expr of
    EBlock statements ->
      fmap scopeResultValue
        <$> runExceptT
          ( evalScopeWithHost
              host
              preludeStatementIndices
              Nothing
              EvaluateEntryModule
              builtinMode
              bindingTypeHints
              False
              Map.empty
              statements
          )
    _ ->
      runExceptT
        (Just <$> evalValueWithHost host Nothing builtinMode bindingTypeHints Map.empty False expr)

evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  RuntimeHostEvaluationT m (Either RuntimeControl (Maybe RuntimeValue))
evaluateRuntimeExprWithEvaluationHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host preludeStatementIndices builtinMode bindingTypeHints expr =
  if runtimeExprRequiresHost expr
    then
      case expr of
        EBlock statements ->
          fmap scopeResultValue
            <$> evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements
              host
              preludeStatementIndices
              Nothing
              EvaluateEntryModule
              builtinMode
              bindingTypeHints
              Map.empty
              statements
        _ ->
          runExceptT
            (Just <$> evalValueWithHost host Nothing builtinMode bindingTypeHints Map.empty False expr)
    else
      pure
        ( case
            evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements
              preludeStatementIndices
              builtinMode
              bindingTypeHints
              expr
            of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode Map.empty expr

evaluateRuntimeExprWithBuiltinsAndBindingHints ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode bindingTypeHints expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements Set.empty builtinMode bindingTypeHints expr

evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements preludeStatementIndices builtinMode bindingTypeHints expr =
  runIdentity
    ( evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
        disabledRuntimeHost
        preludeStatementIndices
        builtinMode
        bindingTypeHints
        expr
    )

evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprPureWithBuiltinsAndBindingHintsAndSourceUnitStatements preludeStatementIndices builtinMode bindingTypeHints expr =
  case expr of
    EBlock statements ->
      scopeResultValue
        <$> evaluateModuleScopeWithSourceUnitStatements
          preludeStatementIndices
          Nothing
          EvaluateEntryModule
          builtinMode
          bindingTypeHints
          Map.empty
          statements
    _ -> Just <$> evalValue builtinMode bindingTypeHints Map.empty expr


-- Public scope entry points receive an opaque map whose lazy cells may include
-- recursive blackholes. They cannot safely recover provenance by inspecting
-- values, so only the empty map is known not to contain imported host cells.
opaqueRuntimeEnvironmentMayReachHostCells :: RuntimeEnv -> Bool
opaqueRuntimeEnvironmentMayReachHostCells = not . Map.null

-- | Immutable expression-local inputs for the shared evaluator. Callable
-- transfer replaces only the captured environment and module path; builtin
-- resolution and runtime hints remain stable for the whole machine run.
data EvaluationContext = EvaluationContext
  { evaluationModulePath :: Maybe [Text],
    evaluationBuiltinMode :: BuiltinResolutionMode,
    evaluationBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    evaluationEnvironment :: RuntimeEnv,
    evaluationEnvironmentMayReachHostCells :: Bool,
    evaluationClosureBaseName :: Text,
    evaluationLambdaStage :: Int
  }

data RuntimeResultObligation
  = ApplyFunctionResultHint SignatureType
  | ApplyExplicitResultHint SignatureType
  | AttachDefaultIntegerResult
  | CloseRuntimeProfileFrame
  deriving (Eq, Show)

newtype RuntimeReturnPolicy =
  RuntimeReturnPolicy [RuntimeResultObligation]

data EvaluationControl
  = EvaluateExpression EvaluationContext Expr
  | ApplyCallable RuntimeValue RuntimeValue
  | ForceRuntimeValue RuntimeValue
  | ReturnRuntimeValue RuntimeValue

-- | First-order continuation frames make evaluation order inspectable and
-- keep Jazz recursion on the heap instead of the Haskell call stack.
data EvaluationFrame
  = EvaluateApplicationArgument EvaluationContext Expr
  | ApplyEvaluatedFunction RuntimeValue
  | EvaluateListElement EvaluationContext [RuntimeValue] [Expr]
  | EvaluateTupleElement EvaluationContext [RuntimeValue] [Expr]
  | EvaluateIfBranch EvaluationContext Expr Expr
  | EvaluateCaseArms EvaluationContext [CaseArm]
  | EvaluateCaseGuard EvaluationContext RuntimeValue RuntimeEnv Expr [CaseArm]
  | EvaluateBuiltinRightOperand EvaluationContext Text Expr
  | ApplyBuiltinBinary Text RuntimeValue
  | EvaluateDeclaredOperatorLeft EvaluationContext Expr Expr
  | ApplyDeclaredOperatorLeft EvaluationContext RuntimeValue Expr
  | EvaluateDeclaredOperatorRight EvaluationContext Expr
  | EvaluateLeftSection EvaluationContext Text
  | ApplyForcedCallable RuntimeValue
  | EvaluateRightSection EvaluationContext Text
  | BuildDeclaredRightSection EvaluationContext Text RuntimeValue
  | ApplyTypeApplicationHint EvaluationContext SourceSpan SignatureType
  | ApplyRemainingArguments [RuntimeValue]

data EvaluationContinuation =
  EvaluationContinuation RuntimeReturnPolicy EvaluationFrame

data EvaluationMachine = EvaluationMachine
  { evaluationControl :: EvaluationControl,
    evaluationContinuations :: [EvaluationContinuation],
    evaluationReturnPolicy :: RuntimeReturnPolicy
  }

data EvaluationProgress
  = EvaluationFinished RuntimeValue
  | EvaluationContinues EvaluationMachine

evaluateModuleScope ::
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScope = evaluateModuleScopeWithSourceUnitStatements Set.empty

evaluateModuleScopeWithHost ::
  Monad m =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHost host =
  evaluateModuleScopeWithHostAndSourceUnitStatements host Set.empty

evaluateModuleScopeWithRequiredHost ::
  Monad m =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredHost host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    evaluateModuleScopeWithRequiredEvaluationHost
      evaluationHost
      currentModulePath
      evaluationMode
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHost host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runtimeControlAsDiagnosticResult
    <$> evaluateModuleScopeWithRequiredEvaluationHostControl
      host
      currentModulePath
      evaluationMode
      builtinMode
      bindingTypeHints
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHostControl ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHostControl host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runExceptT
    ( evalScopeWithHost
        host
        Set.empty
        currentModulePath
        evaluationMode
        builtinMode
        bindingTypeHints
        (opaqueRuntimeEnvironmentMayReachHostCells initialEnv)
        initialEnv
        statements
    )

evaluateModuleScopeWithHostAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHostAndSourceUnitStatements host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runtimeControlAsDiagnosticResult
    <$> runRuntimeHostEvaluation host (\evaluationHost ->
      evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements
        evaluationHost
        preludeStatementIndices
        currentModulePath
        evaluationMode
        builtinMode
        bindingTypeHints
        initialEnv
        statements)

evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateModuleScopeWithEvaluationHostAndSourceUnitStatements host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  if
      runtimeExprRequiresHost (EBlock statements)
        || opaqueRuntimeEnvironmentMayReachHostCells initialEnv
    then
      runExceptT
        ( evalScopeWithHost
            host
            preludeStatementIndices
            currentModulePath
            evaluationMode
            builtinMode
            bindingTypeHints
            (opaqueRuntimeEnvironmentMayReachHostCells initialEnv)
            initialEnv
            statements
        )
    else
      pure
        ( case
            evaluateModuleScopePureWithSourceUnitStatements
              preludeStatementIndices
              currentModulePath
              evaluationMode
              builtinMode
              bindingTypeHints
              initialEnv
              statements
            of
            Left diagnostic -> Left (RuntimeDiagnostic diagnostic)
            Right value -> Right value
        )

evaluateModuleScopeWithSourceUnitStatements ::
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScopeWithSourceUnitStatements preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runIdentity
    ( evaluateModuleScopeWithHostAndSourceUnitStatements
        disabledRuntimeHost
        preludeStatementIndices
        currentModulePath
        evaluationMode
        builtinMode
        bindingTypeHints
        initialEnv
        statements
    )

evaluateModuleScopePureWithSourceUnitStatements ::
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScopePureWithSourceUnitStatements preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements = go Nothing indexedStatements
  where
    scopePlan =
      buildRuntimeScopePlan
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
        SLet bindingName _ _ ->
          LazyMap.insert bindingName (bindingCellAt statementIndex) env
        SData _ typeName typeParameters constructors ->
          insertDataConstructors (modulePathForStatement statementIndex) typeName typeParameters constructors env
        SClass _ capabilityName parameters methods ->
          insertClassMethods capabilityName parameters methods env
        SImpl _ capabilityName arguments methods ->
          insertImplMethods (modulePathForStatement statementIndex) capabilityName arguments methods env
        _ -> env

    go :: Maybe RuntimeValue -> [(Int, Statement)] -> Either Diagnostic ScopeResult
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

    evalValueAt :: Int -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalValueAt statementIndex =
      evalValueWithModulePath (modulePathForStatement statementIndex) builtinMode bindingTypeHints

    bindingCellAt :: Int -> RuntimeCell
    bindingCellAt statementIndex =
      case LazyIntMap.lookup statementIndex bindingCells of
        Just cell -> cell
        Nothing ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: missing binding cell for statement")
    
    cellForStatement :: Int -> Statement -> RuntimeCell
    cellForStatement statementIndex statement =
      case statement of
        SLet bindingName _ valueExpr ->
          bindingCell statementIndex bindingName valueExpr
        _ ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: expected binding statement")

    bindingCell :: Int -> Name -> Expr -> RuntimeCell
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

    evalBindingValue :: Int -> Name -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      nameRuntimeClosureBinding
        (modulePathForStatement statementIndex)
        bindingName
        <$> case previousSignatureNumericTarget statementIndex bindingName of
          Just targetType -> do
            runtimeValue <- evalNumericSignatureBinding statementIndex targetType env valueExpr
            attachRuntimeTypeHint (previousSignatureRuntimeTypeHint statementIndex bindingName) runtimeValue
              >>= attachDefaultBindingIntegerTarget
          Nothing -> do
            runtimeValue <- evalValueAt statementIndex env valueExpr
            attachRuntimeTypeHint (bindingRuntimeTypeHint statementIndex bindingName) runtimeValue
              >>= attachDefaultBindingIntegerTarget

    evalNumericSignatureBinding :: Int -> NumericType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalNumericSignatureBinding statementIndex targetType env valueExpr =
      case valueExpr of
        ELit (LInt literalValue) ->
          convertIntegerToNumericTarget conversionBuiltin targetType literalValue
        ELit (LFloat literalValue literalSource _) ->
          convertFloatToNumericTarget conversionBuiltin targetType literalValue (Just literalSource)
        _ -> do
          runtimeValue <- evalValueAt statementIndex env valueExpr
          evalNumericConversion conversionBuiltin targetType runtimeValue
      where
        conversionBuiltin = numericConversionBuiltinForTarget targetType

    previousSignatureNumericTarget :: Int -> Name -> Maybe NumericType
    previousSignatureNumericTarget statementIndex bindingName =
      scopePlanPreviousSignaturePayload scopePlan statementIndex bindingName
        >>= runtimeSignatureNumericTarget

    previousSignatureRuntimeTypeHint :: Int -> Name -> Maybe SignatureType
    previousSignatureRuntimeTypeHint statementIndex bindingName =
      scopePlanPreviousSignaturePayload scopePlan statementIndex bindingName
        >>= signaturePayloadConstraintType

    bindingRuntimeTypeHint :: Int -> Name -> Maybe SignatureType
    bindingRuntimeTypeHint statementIndex bindingName =
      runtimeConstraintType (modulePathForStatement statementIndex) <$> rawHint
      where
        rawHint =
          case previousSignatureRuntimeTypeHint statementIndex bindingName of
            Just signatureHint -> Just signatureHint
            Nothing ->
              case scopePlanStatementAt scopePlan statementIndex of
                Just (SLet _ bindingSpan _) ->
                  Map.lookup
                    (bindingRuntimeHintKeyInModule (modulePathForStatement statementIndex) bindingName bindingSpan)
                    bindingTypeHints
                _ -> Nothing

    -- Alias bridges can legitimately point across a recursive SCC, but pure
    -- alias loops need a deterministic diagnostic instead of infinite forcing.
    resolveRecursiveAliasTarget :: Set Int -> Int -> Either Diagnostic Int
    resolveRecursiveAliasTarget visited statementIndex
      | Set.member statementIndex visited =
          Left (runtimeDiagnostic E3021 "runtime recursive alias cycle has no concrete value")
      | otherwise =
          case scopePlanStatementAt scopePlan statementIndex of
            Just (SLet bindingName _ aliasExpr) ->
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

    bindingEnv :: Int -> Name -> RuntimeEnv
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

    functionSelfReferenceCell :: Int -> Name -> Maybe RuntimeCell
    functionSelfReferenceCell statementIndex bindingName
      | recursiveFunctionNeedsSelf statementIndex bindingName =
          Just (Left (runtimeDiagnostic E3021 "runtime recursive binding has no concrete value"))
      | otherwise =
          Nothing

    recursiveFunctionNeedsSelf :: Int -> Name -> Bool
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
    attachSelfRecursiveBinding :: Int -> Name -> RuntimeValue -> RuntimeValue
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

    recursiveAliasTarget :: Set Name -> Int -> Expr -> Maybe Int
    recursiveAliasTarget locallyBoundNames statementIndex valueExpr =
      case peelSingleExprBlock valueExpr of
        EVar targetName ->
          if Set.member targetName locallyBoundNames
            then Nothing
            else
              case scopePlanRecursiveGroupAt scopePlan statementIndex of
                Just groupMembers ->
                  lookupRecursivePeer targetName groupMembers
                Nothing -> Nothing
        EOperatorValue operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol) ->
              let targetName = operatorBindingName operatorSymbol
               in
                if Set.member targetName locallyBoundNames
                  then Nothing
                  else
                    case scopePlanRecursiveGroupAt scopePlan statementIndex of
                      Just groupMembers ->
                        lookupRecursivePeer targetName groupMembers
                      Nothing -> Nothing
        _ -> Nothing

    -- Preserve wrapper runtime semantics by evaluating the branch condition
    -- first, then following alias resolution only through the selected branch.
    selectedRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr -> Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTarget =
      selectedRecursiveAliasTargetWithBound Set.empty

    selectedRecursiveAliasTargetWithBound ::
      Set Name ->
      Int ->
      RuntimeEnv ->
      Expr ->
      Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        EPatternCase scrutineeExpr caseArms -> do
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

    selectRecursiveAliasTarget :: Set Name -> Int -> RuntimeEnv -> Expr -> Expr -> Expr -> Either Diagnostic (Maybe Int)
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
      (RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue) ->
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm] ->
      Either Diagnostic (Maybe (Set Name, RuntimeEnv, Expr))
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

    caseArmBoundNames :: CaseArm -> Set Name
    caseArmBoundNames (CaseArm casePattern _ _) =
      patternBinderNames casePattern

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr -> Expr
    peelSingleExprBlock expr =
      case expr of
        EBlock [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    terminalBlockLocalAliasExpr :: [Statement] -> Maybe ([Statement], Expr)
    terminalBlockLocalAliasExpr blockStatements =
      case reverse blockStatements of
        SExpr _ (EVar aliasName) : precedingStatements ->
          let prefixStatements = reverse precedingStatements
           in fmap
                (\aliasExpr -> (prefixStatements, aliasExpr))
                (followLocalAlias Set.empty aliasName (localAliasBindings prefixStatements))
        _ -> Nothing

    localAliasBindings :: [Statement] -> Map Name Expr
    localAliasBindings =
      foldl' collectBinding Map.empty
      where
        collectBinding bindings statement =
          case statement of
            SLet bindingName _ bindingExpr ->
              Map.insert bindingName bindingExpr bindings
            _ -> bindings

    followLocalAlias :: Set Name -> Name -> Map Name Expr -> Maybe Expr
    followLocalAlias visitedNames aliasName localBindings =
      if Set.member aliasName visitedNames
        then Nothing
        else
          case Map.lookup aliasName localBindings of
            Just aliasExpr ->
              case peelSingleExprBlock aliasExpr of
                EVar nextAliasName
                  | Map.member nextAliasName localBindings ->
                      followLocalAlias (Set.insert aliasName visitedNames) nextAliasName localBindings
                _ -> Just aliasExpr
            Nothing ->
              Nothing

    blockLocalAliasEnv :: Maybe [Text] -> RuntimeEnv -> [Statement] -> RuntimeEnv
    blockLocalAliasEnv blockModulePath blockInitialEnv blockStatements =
      case LazyIntMap.lookup (length indexedBlockStatements) blockPrefixEnvironments of
        Just env -> env
        Nothing -> blockInitialEnv
      where
        blockScopePlan =
          buildRuntimeScopePlan
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
            SLet bindingName _ _ ->
              LazyMap.insert bindingName (blockBindingCellAt statementIndex) env
            SData _ typeName typeParameters constructors ->
              insertDataConstructors blockModulePath typeName typeParameters constructors env
            SClass _ capabilityName parameters methods ->
              insertClassMethods capabilityName parameters methods env
            SImpl _ capabilityName arguments methods ->
              insertImplMethods blockModulePath capabilityName arguments methods env
            _ -> env

        blockBindingCellAt statementIndex =
          case LazyIntMap.lookup statementIndex blockBindingCells of
            Just cell -> cell
            Nothing ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: missing block binding cell for alias selection")

        blockCellForStatement statementIndex statement =
          case statement of
            SLet bindingName _ valueExpr ->
              evalValueWithModulePath blockModulePath builtinMode bindingTypeHints (blockEnvBefore statementIndex) valueExpr
                >>= attachRuntimeTypeHint (blockBindingRuntimeTypeHint statementIndex bindingName)
                >>= attachDefaultBindingIntegerTarget
            _ ->
              Left
                (runtimeDiagnostic E3020 "internal runtime error: expected block binding statement for alias selection")

        blockBindingRuntimeTypeHint statementIndex bindingName =
          runtimeConstraintType blockModulePath <$> rawHint
          where
            rawHint =
              case blockPreviousSignatureRuntimeTypeHint statementIndex bindingName of
                Just signatureHint -> Just signatureHint
                Nothing ->
                  case scopePlanStatementAt blockScopePlan statementIndex of
                    Just (SLet _ bindingSpan _) ->
                      Map.lookup
                        (bindingRuntimeHintKeyInModule blockModulePath bindingName bindingSpan)
                        bindingTypeHints
                    _ -> Nothing

        blockPreviousSignatureRuntimeTypeHint statementIndex bindingName =
          scopePlanPreviousSignaturePayload blockScopePlan statementIndex bindingName
            >>= signaturePayloadConstraintType

    lookupRecursivePeer :: Name -> [Int] -> Maybe Int
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
              case
                  scopePlanBindingNameAt scopePlan peerIndex of
                Just peerName
                  | Map.notMember peerName envBeforeValue ->
                      LazyMap.insert peerName (bindingCellAt peerIndex) envAcc
                _ ->
                  envAcc

    insertDataConstructors :: Maybe [Text] -> Name -> [Name] -> [DataConstructor] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors definitionModulePath typeName typeParameters constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor constructorName constructorArguments) =
          Map.insert
            constructorName
            ( Right
                ( VConstructor
                    (runtimeDefinitionName definitionModulePath typeName)
                    typeParameters
                    (runtimeDefinitionName definitionModulePath constructorName)
                    (map (runtimeConstructorArgument definitionModulePath) constructorArguments)
                    []
                )
            )
            envAcc

    insertClassMethods :: Name -> [Name] -> [ClassMethodSignature] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods capabilityName parameters methods env =
      case parameters of
        [classParameter] ->
          foldl' (insertMethod (identifierText classParameter)) env methods
        _ -> env
      where
        insertMethod classParameter envAcc (ClassMethodSignature methodName _ methodSignature) =
          let methodKey = qualifiedMethodKey capabilityName methodName
              methodName' = qualifiedMemberName capabilityName methodName
           in if Map.member methodName' envAcc
                then envAcc
                else Map.insert methodName' (Right (VQualifiedMethod methodKey classParameter methodSignature [] [])) envAcc

    insertImplMethods :: Maybe [Text] -> Name -> [SignatureType] -> [ImplMethod] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods methodModulePath capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget ->
              methodEnv
          where
            runtimeImplTarget = runtimeConstraintType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodExprsByKey =
              Map.fromList
                [ (qualifiedMethodKey capabilityName methodName, methodExpr)
                  | ImplMethod methodName _ methodExpr <- methods
                ]
            methodCandidates =
              map
                ( \(ImplMethod methodName _ methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                        methodName' = qualifiedMemberName capabilityName methodName
                        evidence = RuntimeEvidence (identifierText capabilityName) runtimeImplTarget (Just methodKey)
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
                  evalValueWithModulePath methodModulePath builtinMode bindingTypeHints methodEnv methodExpr
                    >>= attachRuntimeMethodSignature methodModulePath methodEnv candidateImplTarget methodName
            insertCandidate envAcc (methodName, _, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc
        _ -> env
      where
        addMethodCandidate methodCandidate methodCell =
          case methodCell of
            Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
              Right (VQualifiedMethod methodKey classParameter methodSignature (candidates ++ [methodCandidate]) capturedArgs)
            _ -> methodCell

    attachRuntimeMethodSignature ::
      Maybe [Text] ->
      RuntimeEnv ->
      SignatureType ->
      Name ->
      RuntimeValue ->
      Either Diagnostic RuntimeValue
    attachRuntimeMethodSignature methodModulePath env implTarget methodName methodValue =
      case Map.lookup methodName env of
        Just (Right (VQualifiedMethod _ classParameter methodSignature _ _)) ->
          attachRuntimeTypeHint
            ( runtimeConstraintType signatureModulePath
                <$> substituteClassMethodSignature classParameter implTarget methodSignature
            )
            methodValue
        _ ->
          Right methodValue
      where
        signatureModulePath =
          case methodName of
            ResolvedName (ImportedModule classModulePath) _ _ -> Just classModulePath
            _ -> methodModulePath

    selectedQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Either Diagnostic Bool
    selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey expr
      | Set.member methodKey visitedMethodKeys =
          Right True
      | otherwise =
          case peelSingleExprBlock expr of
            EIf conditionExpr thenExpr elseExpr ->
              selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr
            EPatternCase scrutineeExpr caseArms -> do
              scrutineeValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env scrutineeExpr
              selectedArm <-
                selectMatchingCaseArmForAlias
                  methodModulePath
                  (evalValueWithModulePath methodModulePath builtinMode bindingTypeHints)
                  env
                  scrutineeValue
                  caseArms
              case selectedArm of
                Just (_, armEnv, bodyExpr) ->
                  selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys armEnv methodKey bodyExpr
                Nothing ->
                  Right False
            EBlock blockStatements ->
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
            EVar aliasName ->
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

    selectQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Expr -> Expr -> Either Diagnostic Bool
    selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env conditionExpr
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

evalValue :: BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValue =
  evalValueWithModulePath Nothing

evalValueWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> Map BindingRuntimeHintKey SignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env expr =
  runtimeControlAsDiagnosticResult
    ( runIdentity
        ( runRuntimeHostEvaluation disabledRuntimeHost $ \host ->
            runExceptT
              ( runEvaluationMachine
                  host
                  EvaluationContext
                    { evaluationModulePath = currentModulePath,
                      evaluationBuiltinMode = builtinMode,
                      evaluationBindingTypeHints = bindingTypeHints,
                      evaluationEnvironment = env,
                      evaluationEnvironmentMayReachHostCells = False,
                      evaluationClosureBaseName = "<entry>",
                      evaluationLambdaStage = 1
                    }
                  expr
              )
        )
    )

declaredOperatorRightSectionClosure :: Maybe [Text] -> Text -> RuntimeValue -> RuntimeValue -> RuntimeEnv -> Bool -> RuntimeValue
declaredOperatorRightSectionClosure currentModulePath operatorSymbol operatorValue rightValue env envMayReachHostCells =
  VClosure
    RuntimeClosure
      { runtimeClosureEnvironment = capturedEnv,
        runtimeClosureEnvironmentMayReachHostCells = envMayReachHostCells,
        runtimeClosureParameter = leftParameter,
        runtimeClosureBody =
          EApply (EApply (EVar functionName) (EVar leftParameter)) (EVar rightParameter),
        runtimeClosureTypeHint = Nothing,
        runtimeClosureModulePath = currentModulePath,
        runtimeClosureCallableIdentity =
          GeneratedCallable ("declared right section " <> operatorSymbol)
      }
  where
    functionName = generatedName OperatorSectionFunction
    leftParameter = generatedName OperatorSectionLeft
    rightParameter = generatedName OperatorSectionRight
    capturedEnv =
      Map.insert functionName (Right operatorValue) $
        Map.insert rightParameter (Right rightValue) env

nameRuntimeClosureBinding :: Maybe [Text] -> Name -> RuntimeValue -> RuntimeValue
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
    VTyped typeHint innerValue ->
      VTyped typeHint (nameRuntimeClosureBinding currentModulePath bindingName innerValue)
    VExplicitTypeApplication typeHint innerValue ->
      VExplicitTypeApplication typeHint (nameRuntimeClosureBinding currentModulePath bindingName innerValue)
    VExplicitResultHints hints innerValue ->
      attachRuntimeExplicitResultHints
        hints
        (nameRuntimeClosureBinding currentModulePath bindingName innerValue)
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

deferredHostBindingName :: DeferredHostBindingKey -> Name
deferredHostBindingName (DeferredHostBindingKey _ _ _ bindingName) = bindingName


runtimeControlOutcome :: Either RuntimeControl value -> RuntimeOutcome value
runtimeControlOutcome controlResult =
  case controlResult of
    Left (RuntimeDiagnostic diagnostic) -> RuntimeOutcomeFailed diagnostic
    Left (RuntimeExitRequested status) -> RuntimeOutcomeExited status
    Right value -> RuntimeOutcomeCompleted value

runtimeControlAsDiagnosticResult :: Either RuntimeControl value -> Either Diagnostic value
runtimeControlAsDiagnosticResult controlResult =
  case controlResult of
    Left (RuntimeDiagnostic diagnostic) -> Left diagnostic
    Left (RuntimeExitRequested status) ->
      Left
        ( runtimeDiagnostic
            E3020
            ("runtime exit status " <> Text.pack (show status) <> " cannot be represented by this legacy evaluator result")
        )
    Right value -> Right value

runtimeOutcomeAsDiagnosticResult :: RuntimeOutcome value -> Either Diagnostic value
runtimeOutcomeAsDiagnosticResult outcome =
  case outcome of
    RuntimeOutcomeFailed diagnostic -> Left diagnostic
    RuntimeOutcomeExited status ->
      Left
        ( runtimeDiagnostic
            E3020
            ("runtime exit status " <> Text.pack (show status) <> " cannot be represented by this legacy evaluator result")
        )
    RuntimeOutcomeCompleted value -> Right value

throwRuntimeDiagnostic :: Monad m => Diagnostic -> ExceptT RuntimeControl m value
throwRuntimeDiagnostic = throwE . RuntimeDiagnostic

liftRuntimeControl :: Monad m => Either RuntimeControl value -> ExceptT RuntimeControl m value
liftRuntimeControl result =
  case result of
    Left control -> throwE control
    Right value -> pure value

liftRuntimeResult :: Monad m => Either Diagnostic value -> ExceptT RuntimeControl m value
liftRuntimeResult result =
  case result of
    Left diagnostic -> throwRuntimeDiagnostic diagnostic
    Right value -> pure value

runEvaluationMachine ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  EvaluationContext ->
  Expr ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationMachine host context expression =
  runEvaluationControl
    host
    (evaluationBuiltinMode context)
    (evaluationBindingTypeHints context)
    (EvaluateExpression context expression)

runCallableMachine ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runCallableMachine host builtinMode bindingTypeHints functionValue argumentValue =
  runEvaluationControl
    host
    builtinMode
    bindingTypeHints
    (ApplyCallable functionValue argumentValue)

runEvaluationControl ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  EvaluationControl ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
runEvaluationControl host builtinMode bindingTypeHints initialControl =
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
                  + fromIntegral (length (evaluationContinuations machine))
          lift
            ( modify'
                ( \evaluationState ->
                    evaluationState
                      { runtimeHostEvaluationContinuationDepth = continuationDepth,
                        runtimeHostEvaluationObservation =
                          if observeTransitions
                            then
                              recordRuntimeTransition
                                continuationDepth
                                (runtimeHostEvaluationObservation evaluationState)
                            else runtimeHostEvaluationObservation evaluationState
                      }
                )
            )
          progress <- stepEvaluationMachine observeStatistics observeProfile host builtinMode bindingTypeHints machine
          case progress of
            EvaluationFinished value -> pure value
            EvaluationContinues nextMachine -> advance nextMachine
    put
      initialState
        { runtimeHostEvaluationActiveMachineCount = activeMachineCount + 1
        }
    result <-
      runExceptT
        ( advance
            EvaluationMachine
              { evaluationControl = initialControl,
                evaluationContinuations = [],
                evaluationReturnPolicy = RuntimeReturnPolicy []
              }
        )
    modify'
      ( \evaluationState ->
          evaluationState
            { runtimeHostEvaluationActiveMachineCount = activeMachineCount,
              runtimeHostEvaluationContinuationDepth = parentContinuationDepth,
              runtimeHostEvaluationObservation =
                if observeTransitions && activeMachineCount > 0
                  then
                    restoreRuntimeContinuationDepth
                      parentContinuationDepth
                      (runtimeHostEvaluationObservation evaluationState)
                  else runtimeHostEvaluationObservation evaluationState
            }
      )
    pure result

stepEvaluationMachine ::
  Monad m =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  EvaluationMachine ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
stepEvaluationMachine observeStatistics observeProfile host builtinMode bindingTypeHints machine =
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
          forceRuntimeValueWithHost host builtinMode bindingTypeHints runtimeValue
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
              bindingTypeHints
              machine
                { evaluationContinuations = rest,
                  evaluationReturnPolicy = parentPolicy
                }
              frame
              dischargedValue
  where
    stepExpression context expression =
      case expression of
        ELit literal ->
          continueWith (ReturnRuntimeValue (literalRuntimeValue literal)) machine
        EVar name ->
          case Map.lookup name (evaluationEnvironment context) of
            Just runtimeCell -> do
              runtimeValue <- liftRuntimeResult runtimeCell
              continueWith (ForceRuntimeValue runtimeValue) machine
            Nothing ->
              case lookupBuiltinSymbolInMode builtinMode (identifierText name) of
                Just builtinFunction ->
                  continueWith (ReturnRuntimeValue (VBuiltin builtinFunction [])) machine
                Nothing ->
                  throwRuntimeDiagnostic
                    (runtimeDiagnostic E3002 ("runtime unbound variable '" <> identifierText name <> "'"))
        ELambda parameterName bodyExpr ->
          do
            recordRuntimeStatisticWhen
              observeStatistics
              (recordRuntimeClosureCreation (Map.size (evaluationEnvironment context)))
            continueWith
              ( ReturnRuntimeValue
                  ( VClosure
                      RuntimeClosure
                        { runtimeClosureEnvironment = evaluationEnvironment context,
                          runtimeClosureEnvironmentMayReachHostCells =
                            evaluationEnvironmentMayReachHostCells context,
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
              machine
        EOperatorValue operatorSymbol
          | isBuiltinOperatorSymbol operatorSymbol ->
              continueWith (ReturnRuntimeValue (VOperator operatorSymbol [])) machine
          | otherwise -> do
              operatorValue <-
                liftRuntimeResult
                  (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
              continueWith (ForceRuntimeValue operatorValue) machine
        EList [] ->
          continueWith (ReturnRuntimeValue (VList [] Nothing)) machine
        EList (element : rest) ->
          suspendEvaluation
            machine
            (EvaluateListElement context [] rest)
            (EvaluateExpression context element)
        ETuple [] ->
          do
            recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction TupleConstruction 1)
            continueWith (ReturnRuntimeValue (VTuple [])) machine
        ETuple (element : rest) ->
          suspendEvaluation
            machine
            (EvaluateTupleElement context [] rest)
            (EvaluateExpression context element)
        EApply functionExpr argumentExpr ->
          suspendEvaluation
            machine
            (EvaluateApplicationArgument context argumentExpr)
            (EvaluateExpression context functionExpr)
        ETypeApplication functionExpr typeArgumentSpan signatureType ->
          suspendEvaluation
            machine
            (ApplyTypeApplicationHint context typeArgumentSpan signatureType)
            (EvaluateExpression context functionExpr)
        EIf conditionExpr thenExpr elseExpr ->
          suspendEvaluation
            machine
            (EvaluateIfBranch context thenExpr elseExpr)
            (EvaluateExpression context conditionExpr)
        EPatternCase scrutineeExpr caseArms ->
          suspendEvaluation
            machine
            (EvaluateCaseArms context caseArms)
            (EvaluateExpression context scrutineeExpr)
        EBinary operatorSymbol leftExpr rightExpr
          | isBuiltinOperatorSymbol operatorSymbol ->
              suspendEvaluation
                machine
                (EvaluateBuiltinRightOperand context operatorSymbol rightExpr)
                (EvaluateExpression context leftExpr)
          | otherwise -> do
              operatorValue <-
                liftRuntimeResult
                  (lookupDeclaredOperatorCell operatorSymbol (evaluationEnvironment context))
              suspendEvaluation
                machine
                (EvaluateDeclaredOperatorLeft context leftExpr rightExpr)
                (ForceRuntimeValue operatorValue)
        ESectionLeft leftExpr operatorSymbol ->
          suspendEvaluation
            machine
            (EvaluateLeftSection context operatorSymbol)
            (EvaluateExpression context leftExpr)
        ESectionRight operatorSymbol rightExpr ->
          suspendEvaluation
            machine
            (EvaluateRightSection context operatorSymbol)
            (EvaluateExpression context rightExpr)
        EBlock statements ->
          stepBlock context statements

    stepBlock context statements =
      case reverse statements of
        SExpr _ terminalExpr : reversedPrefix -> do
          let prefixStatements = reverse reversedPrefix
          scopeResult <-
            evalScopeWithHost
              host
              Set.empty
              (evaluationModulePath context)
              EvaluateEntryModule
              builtinMode
              bindingTypeHints
              (evaluationEnvironmentMayReachHostCells context)
              (evaluationEnvironment context)
              prefixStatements
          let terminalContext =
                context
                  { evaluationModulePath =
                      runtimeModulePathAfterStatements
                        (evaluationModulePath context)
                        prefixStatements,
                    evaluationEnvironment = scopeResultEnvironment scopeResult,
                    evaluationEnvironmentMayReachHostCells =
                        scopeResultEnvironmentMayReachHostCells scopeResult
                  }
          continueWith (EvaluateExpression terminalContext terminalExpr) machine
        _ -> do
          _ <-
            evalScopeWithHost
              host
              Set.empty
              (evaluationModulePath context)
              EvaluateEntryModule
              builtinMode
              bindingTypeHints
              (evaluationEnvironmentMayReachHostCells context)
              (evaluationEnvironment context)
              statements
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3006 "block expression has no terminal expression result at runtime")

    stepCallable functionValue argumentValue = do
      if observeStatistics
        then
          case runtimeApplicationKind functionValue of
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
            forceRuntimeValueWithHost host builtinMode bindingTypeHints functionValue
          continueWith (ApplyCallable forcedFunctionValue argumentValue) machine
        VExplicitTypeApplication typeHint innerFunctionValue ->
          case explicitTypeApplicationRuntimeFunctionHint typeHint innerFunctionValue of
            Just instantiatedFunctionHint ->
              continueWith
                (ApplyCallable (VTyped instantiatedFunctionHint innerFunctionValue) argumentValue)
                machine
            Nothing ->
              continueWith
                (ApplyCallable innerFunctionValue argumentValue)
                (appendRuntimeResultObligation (ApplyExplicitResultHint typeHint) machine)
        VExplicitResultHints hints innerFunctionValue ->
          continueWith
            (ApplyCallable innerFunctionValue argumentValue)
            ( foldRuntimeExplicitResultHints
                (\hintedMachine typeHint ->
                    appendRuntimeResultObligation
                      (ApplyExplicitResultHint typeHint)
                      hintedMachine
                )
                machine
                hints
            )
        VTyped typeHint innerFunctionValue -> do
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
                evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue argumentValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VSectionRight operatorSymbol rightValue
          | operatorSymbol == "$" ->
              continueWith (ApplyCallable argumentValue rightValue) profiledMachine
          | otherwise -> do
              resultValue <-
                evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol argumentValue rightValue
              continueWith (ReturnRuntimeValue resultValue) profiledMachine
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
                    evaluationBindingTypeHints = bindingTypeHints,
                    evaluationEnvironment =
                      Map.insert
                        (runtimeClosureParameter closure)
                        (Right hintedArgumentValue)
                        (runtimeClosureEnvironment closure),
                    evaluationEnvironmentMayReachHostCells =
                      runtimeClosureEnvironmentMayReachHostCells closure,
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
              bindingTypeHints
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
                      bindingTypeHints
                      operatorSymbol
                      leftValue
                      rightValue
                  continueWith (ReturnRuntimeValue resultValue) profiledMachine
            _ ->
              throwRuntimeDiagnostic
                (runtimeDiagnostic E3016 ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments"))
        VConstructor typeName typeParameters constructorName constructorArguments capturedArgs -> do
          resultValue <-
            liftRuntimeResult
              ( applyConstructor
                  typeName
                  typeParameters
                  constructorName
                  constructorArguments
                  (capturedArgs <> [argumentValue])
              )
          if constructorIsSaturated constructorArguments (capturedArgs <> [argumentValue])
            then recordRuntimeStatisticWhen observeStatistics (recordRuntimeConstruction SaturatedAdtConstruction 1)
            else pure ()
          continueWith (ReturnRuntimeValue resultValue) profiledMachine
        VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
          let arguments = capturedArgs <> [argumentValue]
              preferredCandidates =
                preferredRuntimeMethodCandidates
                  classParameter
                  methodSignature
                  arguments
                  candidates
           in case preferredCandidates of
                [] ->
                  throwRuntimeDiagnostic
                    (runtimeDiagnostic E3026 ("no matching qualified method body '" <> methodKey <> "'"))
                [RuntimeMethodCandidate _ methodCell] -> do
                  methodValue <- liftRuntimeResult methodCell
                  suspendEvaluation
                    profiledMachine
                    (ApplyRemainingArguments arguments)
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
                            ( VQualifiedMethod
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
    VConstructor {} -> Just ConstructorApplication
    VQualifiedMethod {} -> Just MethodApplication
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
    VConstructor _ _ constructorName _ _ ->
      Just (ConstructorCallable (renderName constructorName))
    VQualifiedMethod methodKey _ _ _ _ -> Just (MethodCallable methodKey)
    _ -> Nothing

resumeEvaluationFrame ::
  Monad m =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  EvaluationMachine ->
  EvaluationFrame ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
resumeEvaluationFrame observeStatistics observeProfile host builtinMode bindingTypeHints machine frame runtimeValue =
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
    EvaluateIfBranch context thenExpr elseExpr ->
      case runtimeValue of
        VBool True -> continueWith (EvaluateExpression context thenExpr) machine
        VBool False -> continueWith (EvaluateExpression context elseExpr) machine
        other ->
          throwRuntimeDiagnostic
            (runtimeDiagnostic E3003 ("runtime branch condition must be Bool, found " <> renderRuntimeType other))
    EvaluateCaseArms context caseArms ->
      continueCaseEvaluation observeStatistics machine context runtimeValue caseArms
    EvaluateCaseGuard context scrutineeValue armEnv bodyExpr remainingArms ->
      case runtimeValue of
        VBool True ->
          continueWith
            (EvaluateExpression (context {evaluationEnvironment = armEnv}) bodyExpr)
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
              builtinMode
              bindingTypeHints
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
        (EvaluateExpression context leftExpr)
    ApplyDeclaredOperatorLeft context operatorValue rightExpr ->
      suspendEvaluation
        machine
        (EvaluateDeclaredOperatorRight context rightExpr)
        (ApplyCallable operatorValue runtimeValue)
    EvaluateDeclaredOperatorRight context rightExpr ->
      suspendEvaluation
        machine
        (ApplyEvaluatedFunction runtimeValue)
        (EvaluateExpression context rightExpr)
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
    BuildDeclaredRightSection context operatorSymbol rightValue ->
      do
        let captureWidth = Map.size (evaluationEnvironment context) + 2
        recordRuntimeStatisticWhen observeStatistics (recordRuntimeClosureCreation captureWidth)
        continueWith
          ( ReturnRuntimeValue
              ( declaredOperatorRightSectionClosure
                  (evaluationModulePath context)
                  operatorSymbol
                  runtimeValue
                  rightValue
                  (evaluationEnvironment context)
                  (evaluationEnvironmentMayReachHostCells context)
              )
          )
          machine
    ApplyTypeApplicationHint context typeArgumentSpan signatureType -> do
      let typeHint = runtimeConstraintType (evaluationModulePath context) signatureType
      hintedValue <-
        case
            Map.lookup
              ( explicitTypeApplicationRuntimeHintKeyInModule
                  (evaluationModulePath context)
                  typeArgumentSpan
              )
              (evaluationBindingTypeHints context)
          of
            Just concreteTypeHint ->
              liftRuntimeResult
                ( applyRuntimeTypeHint
                    (runtimeConstraintType (evaluationModulePath context) concreteTypeHint)
                    runtimeValue
                )
            Nothing ->
              if isFunctionValue runtimeValue
                then pure (VExplicitTypeApplication typeHint runtimeValue)
                else
                  liftRuntimeResult
                    ( applyRuntimeTypeHint
                        (fromMaybe typeHint (explicitTypeApplicationRuntimeValueHint typeHint runtimeValue))
                        runtimeValue
                    )
      continueWith (ReturnRuntimeValue hintedValue) machine
    ApplyRemainingArguments arguments ->
      applyRemainingArguments machine runtimeValue arguments

continueCaseEvaluation ::
  Monad m =>
  Bool ->
  EvaluationMachine ->
  EvaluationContext ->
  RuntimeValue ->
  [CaseArm] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) EvaluationProgress
continueCaseEvaluation observeStatistics machine context scrutineeValue =
  chooseArm
  where
    chooseArm remainingArms =
      case remainingArms of
        [] ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3022 "pattern case matched no arms")
        caseArm@(CaseArm casePattern _ _) : rest -> do
          recordRuntimeStatisticWhen observeStatistics recordRuntimePatternAttempt
          case
              matchCaseArm
                (evaluationModulePath context)
                (evaluationEnvironment context)
                scrutineeValue
                caseArm
            of
              Nothing -> chooseArm rest
              Just (armEnv, Nothing, bodyExpr) -> do
                recordRuntimeStatisticWhen
                  observeStatistics
                  (recordRuntimePatternMatch (Set.size (patternBinderNames casePattern)))
                continueWith
                  (EvaluateExpression (context {evaluationEnvironment = armEnv}) bodyExpr)
                  machine
              Just (armEnv, Just guardExpr, bodyExpr) -> do
                recordRuntimeStatisticWhen
                  observeStatistics
                  (recordRuntimePatternMatch (Set.size (patternBinderNames casePattern)))
                suspendEvaluation
                  machine
                  (EvaluateCaseGuard context scrutineeValue armEnv bodyExpr rest)
                  (EvaluateExpression (context {evaluationEnvironment = armEnv}) guardExpr)

applyRemainingArguments ::
  Monad m =>
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
  Monad m =>
  EvaluationControl ->
  EvaluationMachine ->
  ExceptT RuntimeControl m EvaluationProgress
continueWith control machine =
  pure
    ( EvaluationContinues
        machine {evaluationControl = control}
    )

suspendEvaluation ::
  Monad m =>
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
  Monad m =>
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
        AttachDefaultIntegerResult ->
          liftRuntimeResult (attachDefaultBindingIntegerTarget currentValue)
        CloseRuntimeProfileFrame -> do
          lift (modifyRuntimeObservation recordRuntimeProfileClose)
          pure currentValue

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
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  Bool ->
  Expr ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalValueWithHost host currentModulePath builtinMode bindingTypeHints env envMayReachHostCells expr =
  runEvaluationMachine
    host
    EvaluationContext
      { evaluationModulePath = currentModulePath,
        evaluationBuiltinMode = builtinMode,
        evaluationBindingTypeHints = bindingTypeHints,
        evaluationEnvironment = env,
        evaluationEnvironmentMayReachHostCells = envMayReachHostCells,
        evaluationClosureBaseName = "<entry>",
        evaluationLambdaStage = 1
      }
    expr

evalScopeWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Bool ->
  RuntimeEnv ->
  [Statement] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHost host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnvMayReachHostCells initialEnv statements = do
  scopeId <- lift freshDeferredHostScopeId
  observationEnabled <-
    lift
      (runtimeObservationEnabled . runtimeHostEvaluationObservation <$> get)
  evalScopeWithHostInstance
    observationEnabled
    scopeId
    host
    preludeStatementIndices
    currentModulePath
    evaluationMode
    builtinMode
    bindingTypeHints
    initialEnvMayReachHostCells
    initialEnv
    statements

evalScopeWithHostInstance ::
  Monad m =>
  Bool ->
  DeferredHostScopeId ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Set Int ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Bool ->
  RuntimeEnv ->
  [Statement] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ScopeResult
evalScopeWithHostInstance observationEnabled scopeId host preludeStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnvMayReachHostCells initialEnv statements =
  go initialEnvMayReachHostCells initialEnv Nothing indexedStatements
  where
    scopePlan =
      buildRuntimeScopePlan
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
              ( evaluateModuleScopePureWithSourceUnitStatements
                  chunkPreludeStatementIndices
                  (modulePathForStatement statementIndex)
                  evaluationMode
                  builtinMode
                  bindingTypeHints
                  env
                  (map snd pureChunk)
              )
          go
            hostCellsMayBeReachable
            (scopeResultEnvironment scopeResult)
            (scopeResultValue scopeResult)
            remainingAfterChunk
      | otherwise =
          case statement of
            SLet name _ _ ->
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
            SImpl _ capabilityName arguments methods ->
              go
                True
                (insertImplMethodsWithHost (modulePathForStatement statementIndex) capabilityName arguments methods env)
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
                      bindingTypeHints
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
        Just (SLet bindingName bindingSpan valueExpr) ->
          Right
            ( VDeferredHostBinding
                (DeferredHostBindingKey scopeId (modulePathForStatement statementIndex) bindingSpan bindingName)
                (recursiveBindingDiagnostic hostCellsMayBeReachable statementIndex diagnosticBaseEnv)
                (modulePathForStatement statementIndex)
                valueExpr
                capturedEnv
                bindingTypeHints
                (previousSignatureNumericTarget statementIndex bindingName)
                (bindingRuntimeTypeHint statementIndex bindingName)
            )
        _ ->
          Left
            (runtimeDiagnostic E3020 "internal runtime error: expected host binding statement")

    recursiveBindingDiagnostic hostCellsMayBeReachable statementIndex diagnosticBaseEnv =
      case scopePlanRecursiveGroupAt scopePlan statementIndex of
        Just groupMembers
          | not hostCellsMayBeReachable,
            not (scopePlanIsHostRecursiveBinding scopePlan statementIndex) ->
              case
                  evaluateModuleScopePureWithSourceUnitStatements
                    groupPreludeStatementIndices
                    (modulePathForStatement statementIndex)
                    EvaluateEntryModule
                    builtinMode
                    bindingTypeHints
                    diagnosticBaseEnv
                    groupStatements
                of
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
        >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints

    insertImplMethodsWithHost methodModulePath capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget -> methodEnv
          where
            runtimeImplTarget = runtimeConstraintType methodModulePath implTarget
            methodEnv = foldl' insertCandidate env methodCandidates
            methodCandidates =
              map
                ( \(ImplMethod methodName methodSpan methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                        qualifiedMethodName = qualifiedMemberName capabilityName methodName
                        evidence = RuntimeEvidence (identifierText capabilityName) runtimeImplTarget (Just methodKey)
                     in ( qualifiedMethodName,
                          RuntimeMethodCandidate
                            evidence
                            ( Right
                                ( VDeferredHostBinding
                                    (DeferredHostBindingKey scopeId methodModulePath methodSpan qualifiedMethodName)
                                    (runtimeDiagnostic E3021 "runtime recursive host binding has no concrete value")
                                    methodModulePath
                                    methodExpr
                                    methodEnv
                                    bindingTypeHints
                                    Nothing
                                    (methodRuntimeTypeHint runtimeImplTarget qualifiedMethodName)
                                )
                            )
                        )
                )
                methods

            insertCandidate envAcc (methodName, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodName envAcc

            addMethodCandidate methodCandidate methodCell =
              case methodCell of
                Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
                  Right (VQualifiedMethod methodKey classParameter methodSignature (candidates <> [methodCandidate]) capturedArgs)
                _ -> methodCell

            methodRuntimeTypeHint candidateImplTarget methodName =
              case Map.lookup methodName methodEnv of
                Just (Right (VQualifiedMethod _ classParameter methodSignature _ _)) ->
                  runtimeConstraintType signatureModulePath
                    <$> substituteClassMethodSignature classParameter candidateImplTarget methodSignature
                _ -> Nothing
              where
                signatureModulePath =
                  case methodName of
                    ResolvedName (ImportedModule classModulePath) _ _ -> Just classModulePath
                    _ -> methodModulePath
        _ -> env

    previousSignatureNumericTarget statementIndex bindingName =
      scopePlanPreviousSignaturePayload scopePlan statementIndex bindingName
        >>= runtimeSignatureNumericTarget

    previousSignatureRuntimeTypeHint statementIndex bindingName =
      scopePlanPreviousSignaturePayload scopePlan statementIndex bindingName
        >>= signaturePayloadConstraintType

    bindingRuntimeTypeHint statementIndex bindingName =
      runtimeConstraintType (modulePathForStatement statementIndex) <$> rawHint
      where
        rawHint =
          case previousSignatureRuntimeTypeHint statementIndex bindingName of
            Just signatureHint -> Just signatureHint
            Nothing ->
              case scopePlanStatementAt scopePlan statementIndex of
                Just (SLet _ bindingSpan _) ->
                  Map.lookup
                    (bindingRuntimeHintKeyInModule (modulePathForStatement statementIndex) bindingName bindingSpan)
                    bindingTypeHints
                _ -> Nothing

evalHostBindingValue ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  Name ->
  Expr ->
  Maybe NumericType ->
  Maybe SignatureType ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalHostBindingValue host currentModulePath builtinMode bindingTypeHints env bindingName valueExpr maybeNumericTarget maybeTypeHint = do
  value <-
    case maybeNumericTarget of
      Just targetType ->
        evalHostNumericSignatureBinding targetType
      Nothing ->
        evalValueWithHost host currentModulePath builtinMode bindingTypeHints env True valueExpr
  nameRuntimeClosureBinding currentModulePath bindingName
    <$> liftRuntimeResult
      ( attachRuntimeTypeHint maybeTypeHint value
          >>= attachDefaultBindingIntegerTarget
      )
  where
    evalHostNumericSignatureBinding targetType =
      case valueExpr of
        ELit (LInt literalValue) ->
          liftRuntimeResult
            (convertIntegerToNumericTarget conversionBuiltin targetType literalValue)
        ELit (LFloat literalValue literalSource _) ->
          liftRuntimeResult
            (convertFloatToNumericTarget conversionBuiltin targetType literalValue (Just literalSource))
        _ -> do
          runtimeValue <-
            evalValueWithHost host currentModulePath builtinMode bindingTypeHints env True valueExpr
          liftRuntimeResult (evalNumericConversion conversionBuiltin targetType runtimeValue)
      where
        conversionBuiltin = numericConversionBuiltinForTarget targetType

forceQualifiedMethodValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
forceQualifiedMethodValueWithHost host builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethodWithHost host builtinMode bindingTypeHints methodKey classParameter methodSignature candidates capturedArgs
    _ -> pure runtimeValue

forceRuntimeValueWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
forceRuntimeValueWithHost host builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VDeferredHostBinding bindingKey recursionDiagnostic currentModulePath valueExpr env capturedBindingTypeHints maybeNumericTarget maybeTypeHint -> do
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
                      capturedBindingTypeHints
                      env
                      (deferredHostBindingName bindingKey)
                      valueExpr
                      maybeNumericTarget
                      maybeTypeHint
                  )
              )
          lift
            ( modifyDeferredHostBindingCache
                (Map.insert bindingKey (DeferredHostBindingEvaluated result))
            )
          liftRuntimeControl result
    VTyped typeHint innerValue ->
      VTyped typeHint <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    VExplicitTypeApplication typeHint innerValue ->
      VExplicitTypeApplication typeHint <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    VExplicitResultHints hints innerValue ->
      attachRuntimeExplicitResultHints hints
        <$> forceRuntimeValueWithHost host builtinMode bindingTypeHints innerValue
    _ ->
      forceQualifiedMethodValueWithHost host builtinMode bindingTypeHints runtimeValue

applyRuntimeFunctionWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyRuntimeFunctionWithHost host builtinMode bindingTypeHints functionValue argumentValue =
  runCallableMachine
    host
    builtinMode
    bindingTypeHints
    functionValue
    argumentValue

applyQualifiedMethodWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  Text ->
  SignaturePayload ->
  [RuntimeMethodCandidate] ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyQualifiedMethodWithHost host builtinMode bindingTypeHints methodKey classParameter methodSignature candidates arguments =
  case preferredCandidates of
    [] -> throwRuntimeDiagnostic (runtimeDiagnostic E3026 ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] -> do
      methodValue <-
        liftRuntimeResult methodCell
          >>= forceRuntimeValueWithHost host builtinMode bindingTypeHints
      foldM (applyRuntimeFunctionWithHost host builtinMode bindingTypeHints) methodValue arguments
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments preferredCandidates ->
          throwRuntimeDiagnostic (runtimeDiagnostic E3026 ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          pure (VQualifiedMethod methodKey classParameter methodSignature preferredCandidates arguments)
  where
    preferredCandidates =
      preferredRuntimeMethodCandidates
        classParameter
        methodSignature
        arguments
        candidates

applyBuiltinWithHost ::
  Monad m =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
applyBuiltinWithHost observeStatistics observeProfile host builtinMode bindingTypeHints builtinFunction arguments
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
          bindingTypeHints
          builtinFunction
          arguments
      mapM_
        (\(constructionKind, amount) ->
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
  Monad m =>
  Bool ->
  Bool ->
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  BuiltinSymbol ->
  [RuntimeValue] ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBuiltinWithHost observeStatistics observeProfile host builtinMode bindingTypeHints builtinFunction arguments =
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
      pure (VList (map VText argumentsText) (Just (TypeList TypeText)))
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
        (applyRuntimeFunctionWithHost host builtinMode bindingTypeHints)
        builtinFunction
        arguments

beginHostOperation ::
  Monad m =>
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
  Monad m =>
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
    BuiltinTextLength -> TextBuiltinCall
    BuiltinTextUnconsRaw -> TextBuiltinCall
    BuiltinTextAppend -> TextBuiltinCall
    BuiltinTextAppendChar -> TextBuiltinCall
    BuiltinTextFromChars -> TextBuiltinCall
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
    VTyped _ innerValue -> runtimeHostExitStatus innerValue
    VExplicitTypeApplication _ innerValue -> runtimeHostExitStatus innerValue
    VExplicitResultHints _ innerValue -> runtimeHostExitStatus innerValue
    _ -> Nothing

evalBinaryWithHost ::
  Monad m =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Text ->
  RuntimeValue ->
  RuntimeValue ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) RuntimeValue
evalBinaryWithHost host builtinMode bindingTypeHints operatorSymbol leftValue rightValue =
  evalBinary
    RuntimeDiagnostic
    (applyRuntimeFunctionWithHost host builtinMode bindingTypeHints)
    operatorSymbol
    leftValue
    rightValue
