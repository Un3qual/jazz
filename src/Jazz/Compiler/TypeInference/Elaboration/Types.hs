-- | Internal contracts shared by inference and Typed Core elaboration.
--
-- The production outcome stays abstract so its raw program and validation
-- proof cannot be separated accidentally.
module Jazz.Compiler.TypeInference.Elaboration.Types
  ( TypedCoreProductionStatus (..),
    TypedCoreProductionOutcome,
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
    blockedTypedCoreProductionOutcome,
    unsupportedTypedCoreProductionOutcome,
    invariantFailuresTypedCoreProductionOutcome,
    succeededTypedCoreProductionOutcome,
    typedCoreProductionOutcomeStatus,
    typedCoreProductionOutcomeValidatedProgram,
    blockProductionFailureKindAndDetail,
  )
where

import Data.Text (Text)
import Jazz.Compiler.AST
  ( Literal,
    Pattern,
    Statement (..),
  )
import Jazz.Compiler.Diagnostics (SourceSpan)
import Jazz.Compiler.Name (Name)
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    TypeBinding,
  )
import Jazz.Compiler.TypedCore
  ( TypedCallableShape,
    TypedCoreValidationFailure,
    TypedProgram,
  )
import Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validatedTypedProgram,
  )

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

data TypedCoreProductionOutcome
  = ProductionBlockedByDiagnostics
  | ProductionUnsupported [TypedCoreProductionFailure]
  | ProductionInvariantFailures [TypedCoreValidationFailure]
  | ProductionSucceeded ValidatedTypedProgram
  deriving (Eq, Show)

blockedTypedCoreProductionOutcome :: TypedCoreProductionOutcome
blockedTypedCoreProductionOutcome = ProductionBlockedByDiagnostics

unsupportedTypedCoreProductionOutcome :: [TypedCoreProductionFailure] -> TypedCoreProductionOutcome
unsupportedTypedCoreProductionOutcome = ProductionUnsupported

invariantFailuresTypedCoreProductionOutcome :: [TypedCoreValidationFailure] -> TypedCoreProductionOutcome
invariantFailuresTypedCoreProductionOutcome = ProductionInvariantFailures

succeededTypedCoreProductionOutcome :: ValidatedTypedProgram -> TypedCoreProductionOutcome
succeededTypedCoreProductionOutcome = ProductionSucceeded

typedCoreProductionOutcomeStatus :: TypedCoreProductionOutcome -> TypedCoreProductionStatus
typedCoreProductionOutcomeStatus outcome =
  case outcome of
    ProductionBlockedByDiagnostics -> TypedCoreProductionBlockedByDiagnostics
    ProductionUnsupported failures -> TypedCoreProductionUnsupported failures
    ProductionInvariantFailures failures -> TypedCoreProductionInvariantFailures failures
    ProductionSucceeded validatedProgram ->
      TypedCoreProductionSucceeded (validatedTypedProgram validatedProgram)

typedCoreProductionOutcomeValidatedProgram :: TypedCoreProductionOutcome -> Maybe ValidatedTypedProgram
typedCoreProductionOutcomeValidatedProgram outcome =
  case outcome of
    ProductionSucceeded validatedProgram -> Just validatedProgram
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
  [Statement] ->
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
  | ProvisionalLiteralExpression Literal ExpressionType
  | ProvisionalBinaryExpression Text ExpressionType ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalVariableExpression Name ExpressionType
  | ProvisionalLambdaExpression Name ExpressionType ProvisionalTypedExpr
  | ProvisionalApplyExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalIfExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalPatternCaseExpression ExpressionType ProvisionalTypedExpr [ProvisionalPatternCaseArm]
  | ProvisionalScopeStatements [ProvisionalTypedStatement]
  | ProvisionalUnsupportedExpression TypedCoreProductionFailureKind TypedCoreProductionFailureDetail
  | ProvisionalRetainedFailures [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalPatternCaseArm
  = ProvisionalPatternCaseArm
      Pattern
      (Maybe ProvisionalTypedExpr)
      ProvisionalTypedExpr
  deriving (Eq, Show)

data ProvisionalTypedStatement
  = ProvisionalSignature Int Name SourceSpan ExpressionType
  | ProvisionalFunctionBinding ProvisionalCallableDeclaration ProvisionalTypedExpr
  | ProvisionalScalarBinding Int Name SourceSpan ExpressionType ProvisionalTypedExpr
  | ProvisionalTerminalExpression Int SourceSpan ProvisionalTypedExpr
  | ProvisionalUnsupportedCallableBinding ProvisionalCallableDeclaration TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  | ProvisionalUnsupportedStatement Int TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalCallableDeclaration = ProvisionalCallableDeclaration
  { provisionalCallableStatementIndex :: Int,
    provisionalCallableName :: Name,
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
