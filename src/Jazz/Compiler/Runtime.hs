{-# LANGUAGE DataKinds #-}

-- | Stable runtime façade. Evaluation implementation lives in
-- 'Jazz.Compiler.Runtime.Engine'; this module provides evaluation entry points
-- and value re-exports.
module Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeControl (..),
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue (..),
    RuntimeAnnotation (..),
    RuntimeExplicitResultHints,
    prependRuntimeExplicitResultHint,
    runtimeExplicitResultHintsInOrder,
    ScopeResult (..),
    evaluateModuleScopePure,
    evaluateRuntimeExpr,
    evaluateRuntimeExprObserved,
    evaluateRuntimeExprWithHost,
    evaluateModuleScopeWithHost,
    evaluateModuleScopeWithRequiredHost,
    evaluateModuleScopeWithRequiredEvaluationHost,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost,
    renderRuntimeValue,
    untypedIntMetadata,
  )
where

import Data.Functor.Identity (runIdentity)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr,
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Runtime.Engine
  ( evaluateRuntimeExpressionObserved,
    evaluateRuntimeScopePureRequest,
    evaluateRuntimeScopeWithHostRequest,
    evaluateRuntimeScopeWithRequiredHostRequest,
    prepareRuntimeScope,
    renderRuntimeValue,
    runtimeExprRequiresHost,
    untypedIntMetadata,
  )
import Jazz.Compiler.Runtime.HostEvaluation
  ( runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationDisabled),
    RuntimeObservationResult,
    runtimeObservationOutcome,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl (..),
    runtimeControlAsDiagnosticResult,
    runtimeOutcomeAsDiagnosticResult,
  )
import Jazz.Compiler.Runtime.Request
  ( RuntimeScopeRequest (..),
  )
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode (..),
    RuntimeAnnotation (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeExplicitResultHints,
    RuntimeHostEvaluationT,
    RuntimeValue (..),
    ScopeResult (..),
    prependRuntimeExplicitResultHint,
    runtimeExplicitResultHintsInOrder,
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )

evaluateRuntimeExpr :: Expr 'Analyzed -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr =
  runtimeOutcomeAsDiagnosticResult
    . runtimeObservationOutcome
    . evaluateRuntimeExprObserved RuntimeObservationDisabled

evaluateRuntimeExprObserved :: RuntimeObservationRequest -> Expr 'Analyzed -> RuntimeObservationResult (Maybe RuntimeValue)
evaluateRuntimeExprObserved observationRequest expr =
  runIdentity
    ( evaluateRuntimeExpressionObserved
        observationRequest
        disabledRuntimeHost
        expr
    )

evaluateRuntimeExprWithHost :: (Monad m) => RuntimeHost m -> Expr 'Analyzed -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHost host expr =
  fmap
    (runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome)
    ( evaluateRuntimeExpressionObserved
        RuntimeObservationDisabled
        host
        expr
    )

-- | The program coordinator uses this only after proving that all artifacts
-- are host-free, including every dependency supplying the initial environment.
evaluateModuleScopePure ::
  ModuleEvaluationMode -> RuntimeEnv -> Expr 'Analyzed -> Either Diagnostic ScopeResult
evaluateModuleScopePure mode env expression = do
  prepared <- prepareRuntimeScope expression
  evaluateRuntimeScopePureRequest
    RuntimeScopeRequest
      { runtimeScopeEvaluationMode = mode,
        runtimeScopeInitialEnvironment = env,
        runtimeScope = prepared
      }

evaluateModuleScopeWithHost ::
  (Monad m) =>
  RuntimeHost m ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  Expr 'Analyzed ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHost host evaluationMode initialEnv statements =
  case prepareRuntimeScope statements of
    Left diagnostic -> pure (Left diagnostic)
    Right prepared ->
      evaluateRuntimeScopeWithHostRequest
        host
        RuntimeScopeRequest
          { runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScope = prepared
          }

evaluateModuleScopeWithRequiredHost ::
  (Monad m) =>
  RuntimeHost m ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  Expr 'Analyzed ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredHost host evaluationMode initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    evaluateModuleScopeWithRequiredEvaluationHost evaluationHost evaluationMode initialEnv statements

evaluateModuleScopeWithRequiredEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  Expr 'Analyzed ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHost host evaluationMode initialEnv statements =
  runtimeControlAsDiagnosticResult
    <$> evaluateModuleScopeWithRequiredEvaluationHostControl
      host
      evaluationMode
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHostControl ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  Expr 'Analyzed ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHostControl host evaluationMode initialEnv statements =
  case prepareRuntimeScope statements of
    Left diagnostic -> pure (Left (RuntimeDiagnostic diagnostic))
    Right prepared ->
      evaluateRuntimeScopeWithRequiredHostRequest
        host
        RuntimeScopeRequest
          { runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScope = prepared
          }
