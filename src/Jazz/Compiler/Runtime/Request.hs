{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.Runtime.Request
  ( RuntimeScopeRequest (..),
  )
where

import Jazz.Compiler.AST (CorePhase (Analyzed))
import Jazz.Compiler.RecursiveBindings (PreparedRecursiveScope)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScope :: PreparedRecursiveScope 'Analyzed
  }
