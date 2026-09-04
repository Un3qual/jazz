-- | Checked Typed Core construction outcomes. Success retains the independent
-- validator's opaque proof; failed construction never exposes a partial program.
module Jazz.Compiler.TypedCore.Build.Result
  ( TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    typedCoreBuildValidatedProgram,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Jazz.Compiler.TypedCore (TypedCoreValidationFailure)
import Jazz.Compiler.TypedCore.Validate (ValidatedTypedProgram)

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
