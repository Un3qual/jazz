{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.Runtime.Request
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
where

import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST (CorePhase (..), Expr, Statement)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )

data RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpressionSourceUnitStatementIndices :: Set Int,
    runtimeExpressionPreludeModulePath :: ModulePath,
    runtimeExpressionBuiltinMode :: BuiltinResolutionMode,
    runtimeExpression :: Expr 'Analyzed
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeSourceUnitStatementIndices :: Set Int,
    runtimeScopePreludeModulePath :: ModulePath,
    runtimeScopeCurrentModulePath :: Maybe [Text],
    runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeBuiltinMode :: BuiltinResolutionMode,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScopeStatements :: [Statement 'Analyzed]
  }
