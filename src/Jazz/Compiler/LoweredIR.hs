{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Permanent backend-neutral lowered representation shared by the stage-0
-- compiler and Jazz-authored compiler boundary.
module Jazz.Compiler.LoweredIR
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
    supportedLoweredIRVersion,
    loweredFunctionCallSignature,
    loweredImmediateRepresentation,
    loweredOperandRepresentation,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype LoweredIRVersion = LoweredIRVersion Integer
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

supportedLoweredIRVersion :: LoweredIRVersion
supportedLoweredIRVersion = LoweredIRVersion 1

newtype LoweredFunctionId = LoweredFunctionId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype LoweredBlockId = LoweredBlockId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype LoweredTemporaryId = LoweredTemporaryId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype LoweredLayoutId = LoweredLayoutId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype LoweredRuntimeServiceId = LoweredRuntimeServiceId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype LoweredParameterId = LoweredParameterId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LoweredIntegerWidth
  = LoweredIntegerWidth8
  | LoweredIntegerWidth16
  | LoweredIntegerWidth32
  | LoweredIntegerWidth64
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LoweredFloatWidth
  = LoweredFloatWidth16
  | LoweredFloatWidth32
  | LoweredFloatWidth64
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LoweredRepresentation
  = LoweredUnitRepresentation
  | LoweredBoolRepresentation
  | LoweredSignedIntegerRepresentation LoweredIntegerWidth
  | LoweredUnsignedIntegerRepresentation LoweredIntegerWidth
  | LoweredFloatRepresentation LoweredFloatWidth
  | LoweredCharRepresentation
  | LoweredManagedReferenceRepresentation LoweredLayoutId
  | LoweredClosureRepresentation LoweredCallSignature
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LoweredCallSignature = LoweredCallSignature [LoweredRepresentation] LoweredRepresentation
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LoweredVariantLayout = LoweredVariantLayout Integer [LoweredRepresentation]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredLayoutShape
  = LoweredProductLayout [LoweredRepresentation]
  | LoweredVariantLayouts [LoweredVariantLayout]
  | LoweredClosureEnvironmentLayout [LoweredRepresentation]
  | LoweredTextLayout
  | LoweredListLayout LoweredRepresentation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredLayout = LoweredLayout LoweredLayoutId LoweredLayoutShape
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredRuntimeService = LoweredRuntimeService LoweredRuntimeServiceId LoweredCallSignature
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredParameter = LoweredParameter LoweredParameterId LoweredRepresentation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredImmediate
  = LoweredUnitImmediate
  | LoweredBoolImmediate Bool
  | LoweredSignedIntegerImmediate LoweredIntegerWidth Integer
  | LoweredUnsignedIntegerImmediate LoweredIntegerWidth Integer
  | LoweredFloatImmediate LoweredFloatWidth Text
  | LoweredCharImmediate Char
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredOperand
  = LoweredFunctionParameterOperand LoweredParameterId LoweredRepresentation
  | LoweredBlockParameterOperand LoweredParameterId LoweredRepresentation
  | LoweredTemporaryOperand LoweredTemporaryId LoweredRepresentation
  | LoweredImmediateOperand LoweredImmediate
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredArithmeticPrimitive
  = LoweredAdd
  | LoweredSubtract
  | LoweredMultiply
  | LoweredDivide
  | LoweredRemainder
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredComparisonPrimitive
  = LoweredEqual
  | LoweredNotEqual
  | LoweredLessThan
  | LoweredLessThanOrEqual
  | LoweredGreaterThan
  | LoweredGreaterThanOrEqual
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredBooleanPrimitive
  = LoweredBooleanNot
  | LoweredBooleanAnd
  | LoweredBooleanOr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredPrimitive
  = LoweredArithmeticPrimitive LoweredArithmeticPrimitive
  | LoweredComparisonPrimitive LoweredComparisonPrimitive
  | LoweredBooleanPrimitive LoweredBooleanPrimitive
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredInstruction = LoweredInstruction LoweredTemporaryId LoweredRepresentation LoweredOperation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredSwitchCase = LoweredSwitchCase Integer LoweredBlockId [LoweredOperand]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredSwitchDefault = LoweredSwitchDefault LoweredBlockId [LoweredOperand]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredTerminator
  = LoweredReturn LoweredOperand
  | LoweredJump LoweredBlockId [LoweredOperand]
  | LoweredBranch LoweredOperand LoweredBlockId [LoweredOperand] LoweredBlockId [LoweredOperand]
  | LoweredSwitch LoweredOperand [LoweredSwitchCase] (Maybe LoweredSwitchDefault)
  | LoweredDirectTailCall LoweredFunctionId [LoweredOperand]
  | LoweredClosureTailCall LoweredOperand [LoweredOperand]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredBlock = LoweredBlock LoweredBlockId [LoweredParameter] [LoweredInstruction] (Maybe LoweredTerminator)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredFunction = LoweredFunction LoweredFunctionId (Maybe LoweredParameter) [LoweredParameter] LoweredRepresentation [LoweredBlock] LoweredBlockId
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data LoweredProgram = LoweredProgram LoweredIRVersion [LoweredLayout] [LoweredRuntimeService] [LoweredFunction] LoweredFunctionId
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

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
  = LoweredUnsupportedVersion
  | LoweredDuplicateLayout
  | LoweredUnknownLayout
  | LoweredDuplicateVariantTag
  | LoweredDuplicateRuntimeService
  | LoweredUnknownRuntimeService
  | LoweredDuplicateFunction
  | LoweredMissingEntryFunction
  | LoweredEntryFunctionParameters
  | LoweredDuplicateBlock
  | LoweredMissingEntryBlock
  | LoweredDuplicateParameter
  | LoweredEntryBlockParameters
  | LoweredEntryBlockIncomingEdge
  | LoweredMissingTerminator
  | LoweredDuplicateTemporary
  | LoweredUseBeforeDefinition
  | LoweredCrossBlockTemporary
  | LoweredUnknownParameter
  | LoweredUnknownFunction
  | LoweredUnknownBlock
  | LoweredImmediateOutOfRange
  | LoweredTagOutOfRange
  | LoweredPrimitiveSignatureMismatch
  | LoweredInstructionResultRepresentationMismatch
  | LoweredInvalidFieldProjection
  | LoweredInvalidTagProjection
  | LoweredClosureEnvironmentMismatch
  | LoweredEdgeArityMismatch
  | LoweredEdgeRepresentationMismatch
  | LoweredBranchConditionMismatch
  | LoweredDuplicateSwitchCaseTag
  | LoweredMissingSwitchCaseTag
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
  | LoweredVersionDetail LoweredIRVersion LoweredIRVersion
  | LoweredRepresentationDetail LoweredRepresentation LoweredRepresentation
  | LoweredImmediateRangeDetail LoweredRepresentation
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
