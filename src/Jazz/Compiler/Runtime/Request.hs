{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.Runtime.Request
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
where

import Data.Set (Set)
import Jazz.Compiler.AST (CorePhase (..), Expr)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.RecursiveBindings (PreparedRecursiveScope)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner)

data RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpressionSourceUnitStatementIndices :: Set Int,
    runtimeExpressionPreludeModulePath :: ModulePath,
    runtimeExpression :: Expr 'Analyzed
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeSourceUnitStatementIndices :: Set Int,
    runtimeScopePreludeModulePath :: ModulePath,
    runtimeScopeCurrentModulePath :: Maybe SourceUnitOwner,
    runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScope :: PreparedRecursiveScope 'Analyzed
  }
