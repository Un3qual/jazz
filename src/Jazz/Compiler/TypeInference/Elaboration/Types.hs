{-# LANGUAGE DataKinds #-}

-- | Internal contracts shared by inference and Typed Core elaboration.
--
-- Successful production carries an opaque validated program; the enclosing
-- production result keeps that outcome tied to its inference result.
module Jazz.Compiler.TypeInference.Elaboration.Types
  ( TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    FunctionProfile (..),
    ExpressionRole (..),
    ExpressionEvaluation (..),
    FinalizationEnv (..),
    FinalizationLocation (..),
    typedCoreBuildValidatedProgram,
    blockProductionFailureKindAndDetail,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Literal,
    Pattern,
    Statement (..),
  )
import Jazz.Compiler.Diagnostics (SourceSpan)
import Jazz.Compiler.Name (ResolvedName)
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    TypeBinding,
  )
import Jazz.Compiler.TypedCore
  ( TypedBinderId,
    TypedCallableShape,
    TypedCoreValidationFailure,
  )
import Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
  )

data TypedCoreBuildResult
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported (NonEmpty TypedCoreProductionFailure)
  | TypedCoreProductionInvariantFailures (NonEmpty TypedCoreValidationFailure)
  | TypedCoreProductionSucceeded ValidatedTypedProgram
  deriving (Eq, Show)

typedCoreBuildValidatedProgram :: TypedCoreBuildResult -> Maybe ValidatedTypedProgram
typedCoreBuildValidatedProgram result =
  case result of
    TypedCoreProductionSucceeded validatedProgram -> Just validatedProgram
    _ -> Nothing

data TypedCoreProductionPath
  = TypedCoreProductionInputPath
  | TypedCoreProductionModulePath [Text]
  | TypedCoreProductionStatementPath [Text] Int
  | TypedCoreProductionExpressionPath [Text] Int [Int]
  deriving (Eq, Show)

data TypedCoreProductionFailureKind
  = TypedCoreModulePathMismatch
  | TypedCoreInvalidPortableSourcePath
  | TypedCoreResolvedImportsUnsupported
  | TypedCoreImportedInputsUnsupported
  | TypedCoreAmbientPreludeInputUnsupported
  | TypedCoreUnsupportedRootExpression
  | TypedCoreManagedValueUnsupported
  | TypedCoreStructuredValueUnsupported
  | TypedCoreControlFlowUnsupported
  | TypedCorePatternCaseUnsupported
  | TypedCoreNestedBlockUnsupported
  | TypedCoreUserDefinedOperatorUnsupported
  | TypedCoreCallableValueUnsupported
  | TypedCoreCallArityUnsupported
  | TypedCoreCaptureUnsupported
  | TypedCoreRecursiveFunctionUnsupported
  | TypedCoreFunctionRebindingUnsupported
  | TypedCoreDuplicateParameterUnsupported
  | TypedCoreNonMonomorphicFunctionUnsupported
  | TypedCoreNonLocalCallUnsupported
  | TypedCoreUnsupportedExport
  | TypedCoreUnresolvedExpressionType
  deriving (Eq, Show)

data TypedCoreProductionFailureDetail
  = TypedCoreNoFailureDetail
  | TypedCoreTextValueDetail
  | TypedCoreListValueDetail
  | TypedCoreTupleValueDetail
  | TypedCoreDataValueDetail
  | TypedCoreConditionalDetail
  | TypedCorePatternCaseDetail
  | TypedCoreLocalBlockDetail
  | TypedCoreUnsupportedRootDetail
  | TypedCoreNameDetail Text
  | TypedCoreArityDetail Int Int
  deriving (Eq, Show)

data TypedCoreProductionFailure
  = TypedCoreProductionFailure
      TypedCoreProductionPath
      TypedCoreProductionFailureKind
      TypedCoreProductionFailureDetail
  deriving (Eq, Show)

data TypedCoreProductionMode
  = InferenceOnly
  | ProduceTypedCoreExpressionDirectCall
  deriving (Eq, Show)

-- | Keep the unsupported block classification beside the failure contract so
-- root and nested production traversals cannot drift apart.
blockProductionFailureKindAndDetail ::
  [Statement 'Resolved] ->
  (TypedCoreProductionFailureKind, TypedCoreProductionFailureDetail)
blockProductionFailureKindAndDetail statements
  | any isDataStatement statements =
      (TypedCoreStructuredValueUnsupported, TypedCoreDataValueDetail)
  | otherwise =
      (TypedCoreNestedBlockUnsupported, TypedCoreLocalBlockDetail)
  where
    isDataStatement statement =
      case statement of
        SData {} -> True
        _ -> False

-- | The private result threaded by the shared inference traversal. Ordinary
-- inference projects the expression type; production also consumes the
-- provisional node and ordered profile failures.
data InferredExpr = InferredExpr
  { inferredExpressionType :: Maybe ExpressionType,
    inferredProvisionalExpr :: Maybe ProvisionalTypedExpr,
    inferredProductionFailures :: [InferredProductionFailure]
  }
  deriving (Eq, Show)

data InferredProductionFailure
  = InferredProductionFailure
      [Int]
      TypedCoreProductionFailureKind
      TypedCoreProductionFailureDetail
  deriving (Eq, Show)

data ProvisionalTypedExpr
  = ProvisionalUnitExpression
  | ProvisionalTupleExpression ExpressionType [ProvisionalTypedExpr]
  | ProvisionalLiteralExpression Literal ExpressionType
  | ProvisionalBinaryExpression Text ExpressionType ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalVariableExpression ResolvedName ExpressionType
  | ProvisionalLambdaExpression ResolvedName ExpressionType ProvisionalTypedExpr
  | ProvisionalApplyExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalIfExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalPatternCaseExpression ExpressionType ProvisionalTypedExpr [ProvisionalPatternCaseArm]
  | ProvisionalScopeStatements [ProvisionalTypedStatement]
  | ProvisionalUnsupportedExpression TypedCoreProductionFailureKind TypedCoreProductionFailureDetail
  | ProvisionalRetainedFailures [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalPatternCaseArm
  = ProvisionalPatternCaseArm
      (Pattern 'Resolved)
      (Maybe ProvisionalTypedExpr)
      ProvisionalTypedExpr
  deriving (Eq, Show)

data ProvisionalTypedStatement
  = ProvisionalSignature Int ResolvedName SourceSpan ExpressionType
  | ProvisionalFunctionBinding ProvisionalCallableDeclaration ProvisionalTypedExpr
  | ProvisionalScalarBinding Int ResolvedName SourceSpan ExpressionType ProvisionalTypedExpr
  | ProvisionalTerminalExpression Int SourceSpan ProvisionalTypedExpr
  | ProvisionalDataStatement Int
  | ProvisionalUnsupportedCallableBinding ProvisionalCallableDeclaration TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  | ProvisionalUnsupportedStatement Int TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalCallableDeclaration = ProvisionalCallableDeclaration
  { provisionalCallableStatementIndex :: Int,
    provisionalCallableName :: ResolvedName,
    provisionalCallableSpan :: SourceSpan,
    provisionalCallableType :: ExpressionType,
    provisionalCallableBinding :: Maybe TypeBinding,
    provisionalCallableRecursiveGroupMembers :: Maybe [Int]
  }
  deriving (Eq, Show)

data FunctionProfile = FunctionProfile
  { functionStatementIndex :: Int,
    functionType :: ExpressionType,
    functionArity :: Int
  }
  deriving (Eq)

data ExpressionRole
  = FunctionBindingExpression TypedCallableShape Int
  | CalleeExpression
  | ScalarExpression

data ExpressionEvaluation
  = EagerExpression
  | DeferredExpression

data FinalizationEnv = FinalizationEnv
  { finalizationInferState :: InferState,
    finalizationModulePath :: [Text],
    finalizationFunctions :: Map ResolvedName FunctionProfile,
    finalizationCallableShapes :: Map ResolvedName TypedCallableShape,
    finalizationScalarCaptureTypes :: Map TypedBinderId ExpressionType,
    finalizationEagerClosureCaptureStatements :: Map ResolvedName Int
  }

data FinalizationLocation = FinalizationLocation
  { finalizationStatementIndex :: Int,
    finalizationChildPath :: [Int],
    finalizationParameters :: Map ResolvedName TypedBinderId,
    finalizationScalarBindings :: Map ResolvedName TypedBinderId,
    finalizationExpressionEvaluation :: ExpressionEvaluation,
    finalizationExpressionRole :: ExpressionRole
  }
