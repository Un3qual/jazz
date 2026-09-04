{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module Jazz.Compiler.TypeInference.Elaboration
  ( TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredProductionFailure (..),
    InferredExpr (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    blockProductionFailureKindAndDetail,
    specializeInferredExpression,
    isTypedCoreDirectCallOperator,
    typedCoreBuildValidatedProgram,
  )
where

import Data.Text (Text)
import Jazz.Compiler.TypeInference.Elaboration.Specialize
  ( specializeInferredExpression,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    TypedCoreProductionPath (..),
    blockProductionFailureKindAndDetail,
    typedCoreBuildValidatedProgram,
  )

isTypedCoreDirectCallOperator :: Text -> Bool
isTypedCoreDirectCallOperator symbol = symbol `elem` ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]
