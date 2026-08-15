-- | Shared callback contracts for inference traversal.
module Jazz.Compiler.TypeInference.Traversal
  ( InferExprFn,
    InferExprWithModeFn,
  )
where

import Jazz.Compiler.AST (Expr)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode)
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr,
    TypedCoreProductionMode,
  )
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    TypeEnv,
  )

type InferExprFn =
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)

type InferExprWithModeFn =
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (InferredExpr, InferState)
