module Jazz.Compiler.Runtime.HostEvaluation
  ( runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    freshDeferredHostScopeId,
    modifyDeferredHostBindingCache,
    modifyRuntimeObservation,
    recordRuntimeStatisticWhen,
    recordRuntimeProfileOpenWhen,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict
  ( get,
    modify',
    put,
    runStateT,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity,
    RuntimeObservationRequest (RuntimeObservationDisabled),
    RuntimeObservationState,
    initialRuntimeObservationState,
    recordRuntimeProfileOpen,
  )
import Jazz.Compiler.Runtime.Outcome (RuntimeControl)
import Jazz.Compiler.Runtime.Types
  ( DeferredHostBindingKey,
    DeferredHostBindingState,
    DeferredHostScopeId (..),
    RuntimeHostEvaluationState (..),
    RuntimeHostEvaluationT,
  )
import Jazz.Compiler.RuntimeHost (RuntimeHost (..))

runRuntimeHostEvaluation ::
  (Monad m) =>
  RuntimeHost m ->
  (RuntimeHost (RuntimeHostEvaluationT m) -> RuntimeHostEvaluationT m value) ->
  m value
runRuntimeHostEvaluation host action =
  fst <$> runRuntimeHostEvaluationWithObservation RuntimeObservationDisabled host action

runRuntimeHostEvaluationWithObservation ::
  (Monad m) =>
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

freshDeferredHostScopeId :: (Monad m) => RuntimeHostEvaluationT m DeferredHostScopeId
freshDeferredHostScopeId = do
  evaluationState <- get
  let scopeId = runtimeHostEvaluationNextScopeId evaluationState
  put
    evaluationState
      { runtimeHostEvaluationNextScopeId = scopeId + 1
      }
  pure (DeferredHostScopeId scopeId)

modifyDeferredHostBindingCache ::
  (Monad m) =>
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
  (Monad m) =>
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
  (Monad m) =>
  Bool ->
  (RuntimeObservationState -> RuntimeObservationState) ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
recordRuntimeStatisticWhen enabled updateObservation =
  if enabled
    then lift (modifyRuntimeObservation updateObservation)
    else pure ()

recordRuntimeProfileOpenWhen ::
  (Monad m) =>
  Bool ->
  RuntimeCallableIdentity ->
  ExceptT RuntimeControl (RuntimeHostEvaluationT m) ()
recordRuntimeProfileOpenWhen enabled callableIdentity =
  if enabled
    then lift (modifyRuntimeObservation (recordRuntimeProfileOpen callableIdentity))
    else pure ()

liftRuntimeHost :: (Monad m) => RuntimeHost m -> RuntimeHost (RuntimeHostEvaluationT m)
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
