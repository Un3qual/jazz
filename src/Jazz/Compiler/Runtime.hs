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
    evaluateRuntimeExprWithBuiltinsAndSourceUnitStatements,
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    evaluateRuntimeExprObserved,
    evaluateRuntimeExprWithHost,
    evaluateRuntimeExprWithHostAndBuiltinsAndSourceUnitStatementsObserved,
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
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr,
    Statement,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
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
            runtimeExpressionBuiltinMode = ResolveKernelOnly,
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
            runtimeExpressionBuiltinMode = ResolveKernelOnly,
            runtimeExpression = expr
          }
    )

evaluateRuntimeExprWithHostAndBuiltinsAndSourceUnitStatementsObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  Set Int ->
  ModulePath ->
  BuiltinResolutionMode ->
  Expr 'Analyzed ->
  m (RuntimeObservationResult (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndSourceUnitStatementsObserved observationRequest host sourceUnitStatementIndices preludePath builtinMode expr =
  evaluateRuntimeExpressionObserved
    observationRequest
    host
    RuntimeExpressionRequest
      { runtimeExpressionSourceUnitStatementIndices = sourceUnitStatementIndices,
        runtimeExpressionPreludeModulePath = preludePath,
        runtimeExpressionBuiltinMode = builtinMode,
        runtimeExpression = expr
      }

evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr 'Analyzed -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  evaluateRuntimeExprWithBuiltinsAndSourceUnitStatements
    Set.empty
    builtinMode
    expr

evaluateRuntimeExprWithBuiltinsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Expr 'Analyzed ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndSourceUnitStatements sourceUnitStatementIndices builtinMode expr =
  runIdentity
    ( fmap
        (runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome)
        ( evaluateRuntimeExprWithHostAndBuiltinsAndSourceUnitStatementsObserved
            RuntimeObservationDisabled
            disabledRuntimeHost
            sourceUnitStatementIndices
            preludeModulePath
            builtinMode
            expr
        )
    )

evaluateModuleScope ::
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  Either Diagnostic ScopeResult
evaluateModuleScope currentModulePath evaluationMode builtinMode initialEnv statements =
  runIdentity
    ( evaluateRuntimeScopeWithHostRequest
        disabledRuntimeHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopePreludeModulePath = preludeModulePath,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeBuiltinMode = builtinMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }
    )

evaluateModuleScopeWithHost ::
  (Monad m) =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithHost host currentModulePath evaluationMode builtinMode initialEnv statements =
  evaluateRuntimeScopeWithHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = Set.empty,
        runtimeScopePreludeModulePath = preludeModulePath,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeBuiltinMode = builtinMode,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }

evaluateModuleScopeWithRequiredHost ::
  (Monad m) =>
  RuntimeHost m ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredHost host currentModulePath evaluationMode builtinMode initialEnv statements =
  runRuntimeHostEvaluation host $ \evaluationHost ->
    runtimeControlAsDiagnosticResult
      <$> evaluateRuntimeScopeWithRequiredHostRequest
        evaluationHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopePreludeModulePath = preludeModulePath,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeBuiltinMode = builtinMode,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }

evaluateModuleScopeWithRequiredEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  RuntimeHostEvaluationT m (Either Diagnostic ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHost host currentModulePath evaluationMode builtinMode initialEnv statements =
  runtimeControlAsDiagnosticResult
    <$> evaluateModuleScopeWithRequiredEvaluationHostControl
      host
      currentModulePath
      evaluationMode
      builtinMode
      initialEnv
      statements

evaluateModuleScopeWithRequiredEvaluationHostControl ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  RuntimeEnv ->
  [Statement 'Analyzed] ->
  RuntimeHostEvaluationT m (Either RuntimeControl ScopeResult)
evaluateModuleScopeWithRequiredEvaluationHostControl host currentModulePath evaluationMode builtinMode initialEnv statements =
  evaluateRuntimeScopeWithRequiredHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = Set.empty,
        runtimeScopePreludeModulePath = preludeModulePath,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeBuiltinMode = builtinMode,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }
