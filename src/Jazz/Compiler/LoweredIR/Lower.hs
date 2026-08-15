{-# LANGUAGE OverloadedStrings #-}

-- | Validated lowering from the first typed-core scalar profile into the
-- permanent backend-neutral lowered IR.
module Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringResult (..),
    lowerTypedCoreExpressionDirectCall,
    lowerValidatedTypedCoreExpressionDirectCall,
  )
where

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
      case validateLoweredProgram loweredProgram of
        failures@(_ : _) -> LoweredIRInvariantFailures failures
        [] -> LoweredIRSucceeded loweredProgram

lowerValidatedProgram :: TypedProgram -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedProgram (TypedProgram maybePrelude modules entryModulePath) =
  case filter ((== entryModulePath) . typedModulePath) modules of
    [entryModule] ->
      case lowerValidatedModule entryModule of
        Left failures -> Left (programFailures <> failures)
        Right loweredProgram
          | null programFailures -> Right loweredProgram
          | otherwise -> Left programFailures
    _ ->
      Left
        ( programFailures
            <> [ LoweredIRLoweringFailure
                   TypedProgramPath
                   LoweredIRUnsupportedProgram
                   LoweredIRNoFailureDetail
               | null programFailures
               ]
        )
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
    typedModulePath (TypedModule modulePath _ _ _ _ _ _ _) = modulePath

lowerValidatedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedModule typedModule = do
  analysis <- analyzeTypedModule typedModule
  emitAnalyzedModule analysis
