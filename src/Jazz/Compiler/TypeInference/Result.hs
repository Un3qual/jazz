{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The materialized result of type inference.
--
-- This module is deliberately a leaf-level owner for the result record so
-- consumers that only inspect inference output do not depend on the inference
-- orchestration module.
module Jazz.Compiler.TypeInference.Result
  ( InferenceResult (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Jazz.Compiler.AST (Expr, SignatureType)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleInterface (ModuleInterface)
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)

-- | The canonicalized expression and the ordered diagnostics and metadata
-- produced while inferring it.
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredDiagnostics :: [Diagnostic],
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey SignatureType,
    inferredModuleInterface :: ModuleInterface
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
