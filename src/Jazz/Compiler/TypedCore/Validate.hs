-- | Complete structural validation for the semantic typed-core boundary.
-- Validation is deliberately independent of inference, evaluation, and
-- lowering: it accepts an already-constructed contract value and reports all
-- invariant failures in stable structural order.
module Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validateTypedProgram,
    validateTypedProgramOnce,
    validatedTypedProgram,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Jazz.Compiler.TypedCore (TypedCoreValidationFailure, TypedProgram)
import Jazz.Compiler.TypedCore.Validate.Internal
  ( ValidatedTypedProgram (ValidatedTypedProgram),
    validatedTypedProgram,
  )
import Jazz.Compiler.TypedCore.Validate.Program (validateTypedProgramInternal)

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram = validateTypedProgramInternal

validateTypedProgramOnce :: TypedProgram -> Either (NonEmpty TypedCoreValidationFailure) ValidatedTypedProgram
validateTypedProgramOnce typedProgram =
  case NonEmpty.nonEmpty (validateTypedProgramInternal typedProgram) of
    Nothing -> Right (ValidatedTypedProgram typedProgram)
    Just failures -> Left failures
