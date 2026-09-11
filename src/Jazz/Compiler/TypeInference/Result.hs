{-# LANGUAGE DataKinds #-}
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
    inferredDiagnostics,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Jazz.Compiler.AST (CorePhase (Resolved), Expr)
import Jazz.Compiler.Diagnostics (CompilationDiagnostics, Diagnostic, compilationDiagnostics)
import Jazz.Compiler.ModuleInterface (ModuleInterface)

-- | The canonicalized expression and the ordered diagnostics and metadata
-- produced while inferring it.
data InferenceResult = InferenceResult
  { inferredExpr :: Expr 'Resolved,
    inferredDiagnosticGroups :: CompilationDiagnostics,
    inferredModuleInterface :: ModuleInterface
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

inferredDiagnostics :: InferenceResult -> [Diagnostic]
inferredDiagnostics = compilationDiagnostics . inferredDiagnosticGroups
