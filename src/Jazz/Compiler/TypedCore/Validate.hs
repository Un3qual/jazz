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

import Jazz.Compiler.TypedCore (TypedCoreValidationFailure, TypedProgram)
import Jazz.Compiler.TypedCore.Validate.Internal
  ( ValidatedTypedProgram (ValidatedTypedProgram),
    validatedTypedProgram,
  )
import Jazz.Compiler.TypedCore.Validate.Program (validateTypedProgramInternal)

validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgram = validateTypedProgramInternal

validateTypedProgramOnce :: TypedProgram -> Either [TypedCoreValidationFailure] ValidatedTypedProgram
validateTypedProgramOnce typedProgram =
  case validateTypedProgramInternal typedProgram of
    [] -> Right (ValidatedTypedProgram typedProgram)
    failures -> Left failures
