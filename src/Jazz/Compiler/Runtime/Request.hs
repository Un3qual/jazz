module Jazz.Compiler.Runtime.Request
  ( RuntimeExpressionRequest (..),
    RuntimeScopeRequest (..),
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST (Expr, SignatureType, Statement)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode)
import Jazz.Compiler.Runtime.Types
  ( ModuleEvaluationMode,
    RuntimeEnv,
  )
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)

data RuntimeExpressionRequest = RuntimeExpressionRequest
  { runtimeExpressionSourceUnitStatementIndices :: Set Int,
    runtimeExpressionBuiltinMode :: BuiltinResolutionMode,
    runtimeExpressionBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    runtimeExpression :: Expr
  }

data RuntimeScopeRequest = RuntimeScopeRequest
  { runtimeScopeSourceUnitStatementIndices :: Set Int,
    runtimeScopeCurrentModulePath :: Maybe [Text],
    runtimeScopeEvaluationMode :: ModuleEvaluationMode,
    runtimeScopeBuiltinMode :: BuiltinResolutionMode,
    runtimeScopeBindingTypeHints :: Map BindingRuntimeHintKey SignatureType,
    runtimeScopeInitialEnvironment :: RuntimeEnv,
    runtimeScopeStatements :: [Statement]
  }
