{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module JazzNext.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionProfile (..),
    TypedCoreProductionStatus (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedScope (..),
    rootProvisionalTypedScope,
    finalizeTypedCoreExpressionDirectCall,
  ) where

import Data.Text (Text)
import JazzNext.Compiler.AST (Expr (..), Statement (..))
import JazzNext.Compiler.ModuleGraph (ResolvedModule (..))
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
import JazzNext.Compiler.TypeInference.State (InferState)
import JazzNext.Compiler.TypeInference.Types (ExpressionType)

data TypedCoreProductionProfile
  = TypedCoreExpressionDirectCallProfile
  deriving (Eq, Show)

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

data TypedCoreProductionPath
  = TypedCoreProductionInputPath
  | TypedCoreProductionModulePath [Text]
  deriving (Eq, Show)

data TypedCoreProductionFailureKind
  = TypedCoreModulePathMismatch
  | TypedCoreInvalidPortableSourcePath
  | TypedCoreResolvedImportsUnsupported
  | TypedCoreImportedInputsUnsupported
  | TypedCoreAmbientPreludeInputUnsupported
  | TypedCoreUnsupportedRootExpression
  deriving (Eq, Show)

data TypedCoreProductionFailure
  = TypedCoreProductionFailure
      TypedCoreProductionPath
      TypedCoreProductionFailureKind
  deriving (Eq, Show)

data TypedCoreProductionMode
  = InferenceOnly
  | ProduceTypedCoreExpressionDirectCall
  deriving (Eq, Show)

-- | The private result threaded by production-aware inference.  Existing
-- inference-only helpers construct this with no retained node or failures.
data InferredExpr = InferredExpr
  { inferredExpressionType :: Maybe ExpressionType,
    inferredProvisionalExpr :: Maybe ProvisionalTypedExpr,
    inferredProductionFailures :: [TypedCoreProductionFailure]
  }
  deriving (Eq, Show)

data ProvisionalTypedExpr
  = ProvisionalUnitExpression
  deriving (Eq, Show)

newtype ProvisionalTypedScope = ProvisionalTypedScope ProvisionalTypedExpr
  deriving (Eq, Show)

-- | This first task intentionally retains only the root unit shape.  The
-- matcher is a producer-side selection, not another inference traversal.
rootProvisionalTypedScope :: Expr -> Maybe ProvisionalTypedScope
rootProvisionalTypedScope expression =
  case expression of
    EBlock statements ->
      case reverse statements of
        SExpr _ (ETuple []) : _ -> Just (ProvisionalTypedScope ProvisionalUnitExpression)
        _ -> Nothing
    _ -> Nothing

-- | Finalize the initial unit-only root against the permanent contract.
-- Future profile slices extend the provisional scope rather than changing the
-- typed-core constructors themselves.
finalizeTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ResolvedModule ->
  InferState ->
  ProvisionalTypedScope ->
  TypedCoreProductionStatus
finalizeTypedCoreExpressionDirectCall sourcePath resolvedModule _state provisionalScope =
  case provisionalScope of
    ProvisionalTypedScope ProvisionalUnitExpression ->
      case validateTypedProgram typedProgram of
        [] -> TypedCoreProductionSucceeded typedProgram
        failures -> TypedCoreProductionInvariantFailures failures
  where
    modulePath = resolvedModulePath resolvedModule
    unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []
    entryModule =
      TypedModule
        modulePath
        sourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedExpressionStatement (TypedSpan 1 1) (TypedTupleExpr unitInfo [])]
        unitInfo
    typedProgram = TypedProgram Nothing [entryModule] modulePath
