{-# LANGUAGE DataKinds #-}

module Jazz.Compiler.Runtime.Request
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST (CorePhase (..), Expr, SignatureType, Statement)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)

data RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpressionSourceUnitStatementIndices :: Set Int,
    runtimeExpressionPreludeModulePath :: ModulePath,
    runtimeExpressionBuiltinMode :: BuiltinResolutionMode,
    runtimeExpressionBindingTypeHints :: Map BindingRuntimeHintKey (SignatureType 'Resolved),
    runtimeExpression :: Expr 'Resolved
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeSourceUnitStatementIndices :: Set Int,
    runtimeScopePreludeModulePath :: ModulePath,
    runtimeScopeCurrentModulePath :: Maybe [Text],
    runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeBuiltinMode :: BuiltinResolutionMode,
    runtimeScopeBindingTypeHints :: Map BindingRuntimeHintKey (SignatureType 'Resolved),
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScopeStatements :: [Statement 'Resolved]
  }
