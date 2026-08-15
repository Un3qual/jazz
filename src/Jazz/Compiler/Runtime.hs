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
    untypedIntMetadata,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( Expr,
    SignatureType,
    Statement,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
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
    RuntimeCell,
    RuntimeEnv,
    RuntimeExplicitResultHints,
    RuntimeHostEvaluationT,
    RuntimeValue (..),
    ScopeResult (..),
    data VExplicitResultHints,
    prependRuntimeExplicitResultHint,
    runtimeExplicitResultHintsInOrder,
  )
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr =
  runtimeOutcomeAsDiagnosticResult
    . runtimeObservationOutcome
    . evaluateRuntimeExprObserved RuntimeObservationDisabled

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
evaluateRuntimeExprWithHostObserved observationRequest host expr =
  evaluateRuntimeExpressionObserved
    observationRequest
    host
    RuntimeExpressionRequest
      { runtimeExpressionSourceUnitStatementIndices = Set.empty,
        runtimeExpressionBuiltinMode = ResolveKernelOnly,
        runtimeExpressionBindingTypeHints = Map.empty,
        runtimeExpression = expr
      }

evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Monad m =>
  RuntimeHost m ->
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  m (Either Diagnostic (Maybe RuntimeValue))
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements host sourceUnitStatementIndices builtinMode bindingTypeHints expr =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    <$> evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved
      RuntimeObservationDisabled
      host
      sourceUnitStatementIndices
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
evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatementsObserved observationRequest host sourceUnitStatementIndices builtinMode bindingTypeHints expr =
  evaluateRuntimeExpressionObserved
    observationRequest
    host
    RuntimeExpressionRequest
      { runtimeExpressionSourceUnitStatementIndices = sourceUnitStatementIndices,
        runtimeExpressionBuiltinMode = builtinMode,
        runtimeExpressionBindingTypeHints = bindingTypeHints,
        runtimeExpression = expr
      }

evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode Map.empty expr

evaluateRuntimeExprWithBuiltinsAndBindingHints ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode bindingTypeHints expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements
    Set.empty
    builtinMode
    bindingTypeHints
    expr

evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements ::
  Set Int ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHintsAndSourceUnitStatements sourceUnitStatementIndices builtinMode bindingTypeHints expr =
  runIdentity
    ( evaluateRuntimeExprWithHostAndBuiltinsAndBindingHintsAndSourceUnitStatements
        disabledRuntimeHost
        sourceUnitStatementIndices
        builtinMode
        bindingTypeHints
        expr
    )

evaluateModuleScope ::
  Maybe [Text] ->
  ModuleEvaluationMode ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey SignatureType ->
  RuntimeEnv ->
  [Statement] ->
  Either Diagnostic ScopeResult
evaluateModuleScope currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  runIdentity
    ( evaluateRuntimeScopeWithHostRequest
        disabledRuntimeHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeBuiltinMode = builtinMode,
            runtimeScopeBindingTypeHints = bindingTypeHints,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }
    )

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
evaluateModuleScopeWithHost host currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  evaluateModuleScopeWithHostAndSourceUnitStatements
    host
    Set.empty
    currentModulePath
    evaluationMode
    builtinMode
    bindingTypeHints
    initialEnv
    statements

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
evaluateModuleScopeWithHostAndSourceUnitStatements host sourceUnitStatementIndices currentModulePath evaluationMode builtinMode bindingTypeHints initialEnv statements =
  evaluateRuntimeScopeWithHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = sourceUnitStatementIndices,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeBuiltinMode = builtinMode,
        runtimeScopeBindingTypeHints = bindingTypeHints,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }

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
    runtimeControlAsDiagnosticResult
      <$> evaluateRuntimeScopeWithRequiredHostRequest
        evaluationHost
        RuntimeScopeRequest
          { runtimeScopeSourceUnitStatementIndices = Set.empty,
            runtimeScopeCurrentModulePath = currentModulePath,
            runtimeScopeEvaluationMode = evaluationMode,
            runtimeScopeBuiltinMode = builtinMode,
            runtimeScopeBindingTypeHints = bindingTypeHints,
            runtimeScopeInitialEnvironment = initialEnv,
            runtimeScopeStatements = statements
          }

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
  evaluateRuntimeScopeWithRequiredHostRequest
    host
    RuntimeScopeRequest
      { runtimeScopeSourceUnitStatementIndices = Set.empty,
        runtimeScopeCurrentModulePath = currentModulePath,
        runtimeScopeEvaluationMode = evaluationMode,
        runtimeScopeBuiltinMode = builtinMode,
        runtimeScopeBindingTypeHints = bindingTypeHints,
        runtimeScopeInitialEnvironment = initialEnv,
        runtimeScopeStatements = statements
      }
