{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Stable runtime façade. Evaluation implementation lives in
-- 'Jazz.Compiler.Runtime.Engine'; this module retains the actively used
-- compatibility conveniences and value re-exports.
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
    evaluateModuleScope,
    evaluateRuntimeExprWithSourceUnitStatements,
    evaluateRuntimeExpr,
    evaluateRuntimeExprObserved,
    evaluateRuntimeExprWithHost,
    evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved,
    evaluateModuleScopeWithHost,
    evaluateModuleScopeWithRequiredHost,
    evaluateModuleScopeWithRequiredEvaluationHost,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost,
    runtimeValueExactlyMatchesConstraint,
    renderRuntimeValue,
    untypedIntMetadata,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr,
    Statement,
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleIdentity (ModulePath, preludeModulePath)
import Jazz.Compiler.Runtime.Engine
  ( evaluateRuntimeExpressionObserved,
    evaluateRuntimeScopeWithHostRequest,
    evaluateRuntimeScopeWithRequiredHostRequest,
    renderRuntimeValue,
    runtimeExprRequiresHost,
    runtimeValueExactlyMatchesConstraint,
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
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
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
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner)

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
        RuntimeExpressionRequest
          { runtimeExpressionSourceUnitStatementIndices = Set.empty,
            runtimeExpressionPreludeModulePath = preludeModulePath,
            runtimeExpression = expr
          }
    )

evaluateRuntimeExprWithHost :: (Monad m) => RuntimeHost m -> Expr 'Analyzed -> m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHost host expr =
  fmap
    (runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome)
    ( evaluateRuntimeExpressionObserved
        RuntimeObservationDisabled
        host
        RuntimeExpressionRequest
          { runtimeExpressionSourceUnitStatementIndices = Set.empty,
            runtimeExpressionPreludeModulePath = preludeModulePath,
            runtimeExpression = expr
          }
    )

evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  Set Int ->
  ModulePath ->
  Expr 'Analyzed ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved observationRequest host sourceUnitStatementIndices preludePath expr =
  evaluateRuntimeExpressionObserved
    observationRequest
    host
    RuntimeExpressionRequest
      { runtimeExpressionSourceUnitStatementIndices = sourceUnitStatementIndices,
        runtimeExpressionPreludeModulePath = preludePath,
        runtimeExpression = expr
      }

evaluateRuntimeExprWithSourceUnitStatements ::
  Set Int ->
  Expr 'Analyzed ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithSourceUnitStatements sourceUnitStatementIndices expr =
  runIdentity
    ( fmap
        (runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome)
        ( evaluateRuntimeExprWithHostAndSourceUnitStatementsObserved
            RuntimeObservationDisabled
            disabledRuntimeHost
            sourceUnitStatementIndices
            preludeModulePath
            expr
        )
    )

evaluateModuleScope ::
  Maybe SourceUnitOwner ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  Either Diagnostic ScopeResult
evaluateModuleScope currentModulePath evaluationMode initialEnv statements =
  runIdentity
    ( evaluateRuntimeScopeWithHostRequest
        disabledRuntimeHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopePreludeModulePath = preludeModulePath,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }
    )

evaluateModuleScopeWithHost ::
  (Monad m) =>
  RuntimeHost m ->
  Maybe SourceUnitOwner ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHost host currentModulePath evaluationMode initialEnv statements =
  evaluateRuntimeScopeWithHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = Set.empty,
        runtimeScopePreludeModulePath = preludeModulePath,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }

evaluateModuleScopeWithRequiredHost ::
  (Monad m) =>
  RuntimeHost m ->
  Maybe SourceUnitOwner ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredHost host currentModulePath evaluationMode initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    runtimeControlAsDiagnosticResult
      <$> evaluateRuntimeScopeWithRequiredHostRequest
        evaluationHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopePreludeModulePath = preludeModulePath,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }

evaluateModuleScopeWithRequiredEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe SourceUnitOwner ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHost host currentModulePath evaluationMode initialEnv statements =
  runtimeControlAsDiagnosticResult
    <$> evaluateModuleScopeWithRequiredEvaluationHostControl
      host
      currentModulePath
      evaluationMode
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHostControl ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe SourceUnitOwner ->
  ModuleEvaluationMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHostControl host currentModulePath evaluationMode initialEnv statements =
  evaluateRuntimeScopeWithRequiredHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = Set.empty,
        runtimeScopePreludeModulePath = preludeModulePath,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }
