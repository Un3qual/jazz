-- | Validated lowering from the first typed-core scalar profile into the
-- permanent backend-neutral lowered IR.
module Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringResult (..),
    ValidatedLoweredProgram,
    lowerTypedCoreExpressionDirectCall,
    lowerValidatedTypedCoreExpressionDirectCall,
    validatedLoweredProgram,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.Emit (emitAnalyzedModule)
import Jazz.Compiler.LoweredIR.Lower.Shapes (analyzeTypedModule)
import Jazz.Compiler.LoweredIR.Lower.Types
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validateTypedProgramOnce,
    validatedTypedProgram,
  )

newtype ValidatedLoweredProgram = ValidatedLoweredProgram LoweredProgram
  deriving (Eq, Show)

validatedLoweredProgram :: ValidatedLoweredProgram -> LoweredProgram
validatedLoweredProgram (ValidatedLoweredProgram loweredProgram) = loweredProgram

data LoweredIRLoweringResult
  = LoweredIRTypedCoreFailures (NonEmpty TypedCoreValidationFailure)
  | LoweredIRUnsupported (NonEmpty LoweredIRLoweringFailure)
  | LoweredIRInvariantFailures (NonEmpty LoweredIRValidationFailure)
  | LoweredIRSucceeded ValidatedLoweredProgram
  deriving (Eq, Show)

lowerTypedCoreExpressionDirectCall :: TypedProgram -> LoweredIRLoweringResult
lowerTypedCoreExpressionDirectCall typedProgram =
  case validateTypedProgramOnce typedProgram of
    Left failures -> LoweredIRTypedCoreFailures failures
    Right validatedProgram -> lowerValidatedTypedCoreExpressionDirectCall validatedProgram

-- | Lower a Typed Program whose structural validation was already performed
-- by a trusted producer. Raw external values must use the checked entry point
-- above.
lowerValidatedTypedCoreExpressionDirectCall :: ValidatedTypedProgram -> LoweredIRLoweringResult
lowerValidatedTypedCoreExpressionDirectCall validatedProgram =
  case lowerValidatedProgram (validatedTypedProgram validatedProgram) of
    Left failures -> LoweredIRUnsupported failures
    Right loweredProgram ->
      case NonEmpty.nonEmpty (validateLoweredProgram loweredProgram) of
        Just failures -> LoweredIRInvariantFailures failures
        Nothing -> LoweredIRSucceeded (ValidatedLoweredProgram loweredProgram)

lowerValidatedProgram :: TypedProgram -> Either (NonEmpty LoweredIRLoweringFailure) LoweredProgram
lowerValidatedProgram (TypedProgram maybePrelude modules entryModulePath) =
  case filter ((== entryModulePath) . typedModulePath) modules of
    [entryModule] ->
      case lowerValidatedModule entryModule of
        Left failures ->
          case NonEmpty.nonEmpty (programFailures <> failures) of
            Just checkedFailures -> Left checkedFailures
            Nothing -> Left (NonEmpty.singleton unsupportedProgramFailure)
        Right loweredProgram
          | Just failures <- NonEmpty.nonEmpty programFailures -> Left failures
          | otherwise -> Right loweredProgram
    _ ->
      case NonEmpty.nonEmpty programFailures of
        Just failures -> Left failures
        Nothing -> Left (NonEmpty.singleton unsupportedProgramFailure)
  where
    programFailures
      | maybePrelude == Nothing,
        [_] <- modules =
          []
      | otherwise =
          [ LoweredIRLoweringFailure
              TypedProgramPath
              LoweredIRUnsupportedProgram
              LoweredIRNoFailureDetail
          ]
    unsupportedProgramFailure =
      LoweredIRLoweringFailure
        TypedProgramPath
        LoweredIRUnsupportedProgram
        LoweredIRNoFailureDetail
    typedModulePath (TypedModule modulePath _ _ _ _ _ _ _) = modulePath

lowerValidatedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedModule typedModule = do
  analysis <- analyzeTypedModule typedModule
  emitAnalyzedModule analysis
