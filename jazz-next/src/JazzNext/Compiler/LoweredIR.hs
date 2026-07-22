{-# LANGUAGE OverloadedStrings #-}

-- | Permanent backend-neutral lowered representation shared by the stage-0
-- compiler and Jazz-authored compiler boundary.
module JazzNext.Compiler.LoweredIR
  ( LoweredIRVersion (..),
    LoweredFunctionId (..),
    LoweredBlockId (..),
    LoweredTemporaryId (..),
    LoweredLayoutId (..),
    LoweredRuntimeServiceId (..),
    LoweredParameterId (..),
    LoweredIntegerWidth (..),
    LoweredFloatWidth (..),
    LoweredRepresentation (..),
    LoweredCallSignature (..),
    LoweredVariantLayout (..),
    LoweredLayoutShape (..),
    LoweredLayout (..),
    LoweredRuntimeService (..),
    LoweredParameter (..),
    LoweredImmediate (..),
    LoweredOperand (..),
    LoweredArithmeticPrimitive (..),
    LoweredComparisonPrimitive (..),
    LoweredBooleanPrimitive (..),
    LoweredPrimitive (..),
    LoweredOperation (..),
    LoweredInstruction (..),
    LoweredSwitchCase (..),
    LoweredSwitchDefault (..),
    LoweredTerminator (..),
    LoweredBlock (..),
    LoweredFunction (..),
    LoweredProgram (..),
    LoweredIRValidationPath (..),
    LoweredIRValidationKind (..),
    LoweredIRValidationDetail (..),
    LoweredIRValidationFailure (..),
    loweredFunctionCallSignature,
    loweredImmediateRepresentation,
    loweredOperandRepresentation
  ) where

import Data.Text (Text)

newtype LoweredIRVersion = LoweredIRVersion Integer
  deriving (Eq, Ord, Show)

newtype LoweredFunctionId = LoweredFunctionId Text
  deriving (Eq, Ord, Show)

newtype LoweredBlockId = LoweredBlockId Text
  deriving (Eq, Ord, Show)

newtype LoweredTemporaryId = LoweredTemporaryId Text
  deriving (Eq, Ord, Show)

newtype LoweredLayoutId = LoweredLayoutId Text
  deriving (Eq, Ord, Show)

newtype LoweredRuntimeServiceId = LoweredRuntimeServiceId Text
  deriving (Eq, Ord, Show)

newtype LoweredParameterId = LoweredParameterId Text
  deriving (Eq, Ord, Show)

data LoweredIntegerWidth
  = LoweredIntegerWidth8
  | LoweredIntegerWidth16
  | LoweredIntegerWidth32
  | LoweredIntegerWidth64
  deriving (Eq, Ord, Show)

data LoweredFloatWidth
  = LoweredFloatWidth16
  | LoweredFloatWidth32
  | LoweredFloatWidth64
  deriving (Eq, Ord, Show)

data LoweredRepresentation
  = LoweredUnitRepresentation
  | LoweredBoolRepresentation
  | LoweredSignedIntegerRepresentation LoweredIntegerWidth
  | LoweredUnsignedIntegerRepresentation LoweredIntegerWidth
  | LoweredFloatRepresentation LoweredFloatWidth
  | LoweredCharRepresentation
  | LoweredManagedReferenceRepresentation LoweredLayoutId
  | LoweredClosureRepresentation LoweredCallSignature
  deriving (Eq, Ord, Show)

data LoweredCallSignature = LoweredCallSignature [LoweredRepresentation] LoweredRepresentation
  deriving (Eq, Ord, Show)

data LoweredVariantLayout = LoweredVariantLayout Integer [LoweredRepresentation]
  deriving (Eq, Show)

data LoweredLayoutShape
  = LoweredProductLayout [LoweredRepresentation]
  | LoweredVariantLayouts [LoweredVariantLayout]
  | LoweredClosureEnvironmentLayout [LoweredRepresentation]
  | LoweredTextLayout
  | LoweredListLayout LoweredRepresentation
  deriving (Eq, Show)

data LoweredLayout = LoweredLayout LoweredLayoutId LoweredLayoutShape
  deriving (Eq, Show)

data LoweredRuntimeService = LoweredRuntimeService LoweredRuntimeServiceId LoweredCallSignature
  deriving (Eq, Show)

data LoweredParameter = LoweredParameter LoweredParameterId LoweredRepresentation
  deriving (Eq, Show)

data LoweredImmediate
  = LoweredUnitImmediate
  | LoweredBoolImmediate Bool
  | LoweredSignedIntegerImmediate LoweredIntegerWidth Integer
  | LoweredUnsignedIntegerImmediate LoweredIntegerWidth Integer
  | LoweredFloatImmediate LoweredFloatWidth Text
  | LoweredCharImmediate Char
  deriving (Eq, Show)

data LoweredOperand
  = LoweredFunctionParameterOperand LoweredParameterId LoweredRepresentation
  | LoweredBlockParameterOperand LoweredParameterId LoweredRepresentation
  | LoweredTemporaryOperand LoweredTemporaryId LoweredRepresentation
  | LoweredImmediateOperand LoweredImmediate
  deriving (Eq, Show)

data LoweredArithmeticPrimitive
  = LoweredAdd
  | LoweredSubtract
  | LoweredMultiply
  | LoweredDivide
  | LoweredRemainder
  deriving (Eq, Show)

data LoweredComparisonPrimitive
  = LoweredEqual
  | LoweredNotEqual
  | LoweredLessThan
  | LoweredLessThanOrEqual
  | LoweredGreaterThan
  | LoweredGreaterThanOrEqual
  deriving (Eq, Show)

data LoweredBooleanPrimitive
  = LoweredBooleanNot
  | LoweredBooleanAnd
  | LoweredBooleanOr
  deriving (Eq, Show)

data LoweredPrimitive
  = LoweredArithmeticPrimitive LoweredArithmeticPrimitive
  | LoweredComparisonPrimitive LoweredComparisonPrimitive
  | LoweredBooleanPrimitive LoweredBooleanPrimitive
  deriving (Eq, Show)

data LoweredOperation
  = LoweredPrimitiveOperation LoweredPrimitive [LoweredOperand]
  | LoweredConstructProduct LoweredLayoutId [LoweredOperand]
  | LoweredConstructVariant LoweredLayoutId Integer [LoweredOperand]
  | LoweredConstructList LoweredLayoutId [LoweredOperand]
  | LoweredConstructText LoweredLayoutId Text
  | LoweredConstructClosure LoweredFunctionId LoweredOperand
  | LoweredProjectField LoweredLayoutId Int LoweredOperand
  | LoweredProjectVariantTag LoweredLayoutId LoweredOperand
  | LoweredProjectVariantField LoweredLayoutId Integer Int LoweredOperand
  | LoweredDirectCall LoweredFunctionId [LoweredOperand]
  | LoweredClosureCall LoweredOperand [LoweredOperand]
  | LoweredRuntimeCall LoweredRuntimeServiceId [LoweredOperand]
  deriving (Eq, Show)

data LoweredInstruction = LoweredInstruction LoweredTemporaryId LoweredRepresentation LoweredOperation
  deriving (Eq, Show)

data LoweredSwitchCase = LoweredSwitchCase Integer LoweredBlockId [LoweredOperand]
  deriving (Eq, Show)

data LoweredSwitchDefault = LoweredSwitchDefault LoweredBlockId [LoweredOperand]
  deriving (Eq, Show)

data LoweredTerminator
  = LoweredReturn LoweredOperand
  | LoweredJump LoweredBlockId [LoweredOperand]
  | LoweredBranch LoweredOperand LoweredBlockId [LoweredOperand] LoweredBlockId [LoweredOperand]
  | LoweredSwitch LoweredOperand [LoweredSwitchCase] (Maybe LoweredSwitchDefault)
  | LoweredDirectTailCall LoweredFunctionId [LoweredOperand]
  | LoweredClosureTailCall LoweredOperand [LoweredOperand]
  deriving (Eq, Show)

data LoweredBlock = LoweredBlock LoweredBlockId [LoweredParameter] [LoweredInstruction] (Maybe LoweredTerminator)
  deriving (Eq, Show)

data LoweredFunction = LoweredFunction LoweredFunctionId (Maybe LoweredParameter) [LoweredParameter] LoweredRepresentation [LoweredBlock] LoweredBlockId
  deriving (Eq, Show)

data LoweredProgram = LoweredProgram LoweredIRVersion [LoweredLayout] [LoweredRuntimeService] [LoweredFunction] LoweredFunctionId
  deriving (Eq, Show)

data LoweredIRValidationPath
  = LoweredProgramPath
  | LoweredLayoutPath LoweredLayoutId
  | LoweredRuntimeServicePath LoweredRuntimeServiceId
  | LoweredFunctionPath LoweredFunctionId
  | LoweredBlockPath LoweredFunctionId LoweredBlockId
  | LoweredInstructionPath LoweredFunctionId LoweredBlockId Int
  | LoweredTerminatorPath LoweredFunctionId LoweredBlockId
  deriving (Eq, Show)

data LoweredIRValidationKind
  = LoweredDuplicateLayout
  | LoweredUnknownLayout
  | LoweredDuplicateVariantTag
  | LoweredDuplicateRuntimeService
  | LoweredUnknownRuntimeService
  | LoweredDuplicateFunction
  | LoweredMissingEntryFunction
  | LoweredDuplicateBlock
  | LoweredMissingEntryBlock
  | LoweredMissingTerminator
  | LoweredDuplicateTemporary
  | LoweredUseBeforeDefinition
  | LoweredCrossBlockTemporary
  | LoweredUnknownParameter
  | LoweredUnknownFunction
  | LoweredUnknownBlock
  | LoweredInstructionResultRepresentationMismatch
  | LoweredInvalidFieldProjection
  | LoweredInvalidTagProjection
  | LoweredClosureEnvironmentMismatch
  | LoweredEdgeArityMismatch
  | LoweredEdgeRepresentationMismatch
  | LoweredBranchConditionMismatch
  | LoweredDuplicateSwitchCaseTag
  | LoweredReturnRepresentationMismatch
  | LoweredDirectCallSignatureMismatch
  | LoweredClosureCallSignatureMismatch
  | LoweredRuntimeCallSignatureMismatch
  | LoweredDirectTailCallSignatureMismatch
  | LoweredClosureTailCallSignatureMismatch
  deriving (Eq, Show)

data LoweredIRValidationDetail
  = LoweredNoValidationDetail
  | LoweredIdentifierDetail Text
  | LoweredRepresentationDetail LoweredRepresentation LoweredRepresentation
  | LoweredArityDetail Int Int
  | LoweredIndexDetail Int
  | LoweredTagDetail Integer
  deriving (Eq, Show)

data LoweredIRValidationFailure = LoweredIRValidationFailure LoweredIRValidationPath LoweredIRValidationKind LoweredIRValidationDetail
  deriving (Eq, Show)

loweredFunctionCallSignature :: LoweredFunction -> LoweredCallSignature
loweredFunctionCallSignature (LoweredFunction _ _ parameters resultRepresentation _ _) =
  LoweredCallSignature (map parameterRepresentation parameters) resultRepresentation
  where
    parameterRepresentation (LoweredParameter _ representation) = representation

loweredImmediateRepresentation :: LoweredImmediate -> LoweredRepresentation
loweredImmediateRepresentation immediate =
  case immediate of
    LoweredUnitImmediate -> LoweredUnitRepresentation
    LoweredBoolImmediate _ -> LoweredBoolRepresentation
    LoweredSignedIntegerImmediate width _ -> LoweredSignedIntegerRepresentation width
    LoweredUnsignedIntegerImmediate width _ -> LoweredUnsignedIntegerRepresentation width
    LoweredFloatImmediate width _ -> LoweredFloatRepresentation width
    LoweredCharImmediate _ -> LoweredCharRepresentation

loweredOperandRepresentation :: LoweredOperand -> LoweredRepresentation
loweredOperandRepresentation operand =
  case operand of
    LoweredFunctionParameterOperand _ representation -> representation
    LoweredBlockParameterOperand _ representation -> representation
    LoweredTemporaryOperand _ representation -> representation
    LoweredImmediateOperand immediate -> loweredImmediateRepresentation immediate
