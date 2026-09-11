{-# LANGUAGE DataKinds #-}

-- | Shared callback contracts for inference traversal.
module Jazz.Compiler.TypeInference.Traversal (InferenceMode (..), InferExprFn, InferExprWithModeFn) where

import Jazz.Compiler.AST (CorePhase (Resolved), Expr)
import Jazz.Compiler.TypeInference.Draft (CheckedExpr)
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (TypeEnv)

-- Concrete function inference commits compatible declared numeric contexts
-- and permits signed forward references within eligible function bodies.
data InferenceMode = InferenceOnly | InferConcreteFunctions deriving (Eq, Show)

type InferExprWithModeFn = InferenceMode -> InferExprFn

type InferExprFn = TypeEnv -> InferState -> Expr 'Resolved -> (CheckedExpr, InferState)
