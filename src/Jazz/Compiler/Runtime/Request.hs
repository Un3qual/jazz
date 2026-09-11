{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.Runtime.Request
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
where

import Jazz.Compiler.AST (CorePhase (..), Expr)
import Jazz.Compiler.RecursiveBindings (PreparedRecursiveScope)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )

newtype RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpression :: Expr 'Analyzed
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScope :: PreparedRecursiveScope 'Analyzed
  }
