{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalLoweredIRComparison
  ( canonicalLoweredProgramRuntimeValue,
    canonicalLoweredProgramsRuntimeValue,
    canonicalLoweredValidationFailuresRuntimeValue
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
    runtimeIntValue
  )
import JazzNext.Compiler.LoweredIR
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    untypedIntMetadata
  )

canonicalLoweredProgramRuntimeValue :: LoweredProgram -> RuntimeValue
canonicalLoweredProgramRuntimeValue (LoweredProgram version layouts services functions entryFunction) =
  constructor
    "LoweredProgram"
    [ versionValue version,
      listValue layoutValue layouts,
      listValue runtimeServiceValue services,
      listValue functionValue functions,
      functionIdValue entryFunction
    ]

canonicalLoweredProgramsRuntimeValue :: [LoweredProgram] -> RuntimeValue
canonicalLoweredProgramsRuntimeValue = listValue canonicalLoweredProgramRuntimeValue

canonicalLoweredValidationFailuresRuntimeValue :: [LoweredIRValidationFailure] -> RuntimeValue
canonicalLoweredValidationFailuresRuntimeValue = listValue validationFailureValue

versionValue :: LoweredIRVersion -> RuntimeValue
versionValue (LoweredIRVersion version) = constructor "LoweredIRVersion" [integerValue version]

functionIdValue :: LoweredFunctionId -> RuntimeValue
functionIdValue (LoweredFunctionId name) = constructor "LoweredFunctionId" [VText name]

blockIdValue :: LoweredBlockId -> RuntimeValue
blockIdValue (LoweredBlockId name) = constructor "LoweredBlockId" [VText name]

temporaryIdValue :: LoweredTemporaryId -> RuntimeValue
temporaryIdValue (LoweredTemporaryId name) = constructor "LoweredTemporaryId" [VText name]

layoutIdValue :: LoweredLayoutId -> RuntimeValue
layoutIdValue (LoweredLayoutId name) = constructor "LoweredLayoutId" [VText name]

runtimeServiceIdValue :: LoweredRuntimeServiceId -> RuntimeValue
runtimeServiceIdValue (LoweredRuntimeServiceId name) = constructor "LoweredRuntimeServiceId" [VText name]

parameterIdValue :: LoweredParameterId -> RuntimeValue
parameterIdValue (LoweredParameterId name) = constructor "LoweredParameterId" [VText name]

integerWidthValue :: LoweredIntegerWidth -> RuntimeValue
integerWidthValue width =
  nullary
    ( case width of
        LoweredIntegerWidth8 -> "LoweredIntegerWidth8"
        LoweredIntegerWidth16 -> "LoweredIntegerWidth16"
        LoweredIntegerWidth32 -> "LoweredIntegerWidth32"
        LoweredIntegerWidth64 -> "LoweredIntegerWidth64"
    )

floatWidthValue :: LoweredFloatWidth -> RuntimeValue
floatWidthValue width =
  nullary
    ( case width of
        LoweredFloatWidth16 -> "LoweredFloatWidth16"
        LoweredFloatWidth32 -> "LoweredFloatWidth32"
        LoweredFloatWidth64 -> "LoweredFloatWidth64"
    )

representationValue :: LoweredRepresentation -> RuntimeValue
representationValue representation =
  case representation of
    LoweredUnitRepresentation -> nullary "LoweredUnitRepresentation"
    LoweredBoolRepresentation -> nullary "LoweredBoolRepresentation"
    LoweredSignedIntegerRepresentation width -> constructor "LoweredSignedIntegerRepresentation" [integerWidthValue width]
    LoweredUnsignedIntegerRepresentation width -> constructor "LoweredUnsignedIntegerRepresentation" [integerWidthValue width]
    LoweredFloatRepresentation width -> constructor "LoweredFloatRepresentation" [floatWidthValue width]
    LoweredCharRepresentation -> nullary "LoweredCharRepresentation"
    LoweredManagedReferenceRepresentation layoutId -> constructor "LoweredManagedReferenceRepresentation" [layoutIdValue layoutId]
    LoweredClosureRepresentation signature -> constructor "LoweredClosureRepresentation" [callSignatureValue signature]

callSignatureValue :: LoweredCallSignature -> RuntimeValue
callSignatureValue (LoweredCallSignature parameters resultRepresentation) =
  constructor "LoweredCallSignature" [listValue representationValue parameters, representationValue resultRepresentation]

variantLayoutValue :: LoweredVariantLayout -> RuntimeValue
variantLayoutValue (LoweredVariantLayout tag fields) =
  constructor "LoweredVariantLayout" [integerValue tag, listValue representationValue fields]

layoutShapeValue :: LoweredLayoutShape -> RuntimeValue
layoutShapeValue shape =
  case shape of
    LoweredProductLayout fields -> constructor "LoweredProductLayout" [listValue representationValue fields]
    LoweredVariantLayouts variants -> constructor "LoweredVariantLayouts" [listValue variantLayoutValue variants]
    LoweredClosureEnvironmentLayout fields -> constructor "LoweredClosureEnvironmentLayout" [listValue representationValue fields]
    LoweredTextLayout -> nullary "LoweredTextLayout"
    LoweredListLayout elementRepresentation -> constructor "LoweredListLayout" [representationValue elementRepresentation]

layoutValue :: LoweredLayout -> RuntimeValue
layoutValue (LoweredLayout layoutId shape) = constructor "LoweredLayout" [layoutIdValue layoutId, layoutShapeValue shape]

runtimeServiceValue :: LoweredRuntimeService -> RuntimeValue
runtimeServiceValue (LoweredRuntimeService serviceId signature) =
  constructor "LoweredRuntimeService" [runtimeServiceIdValue serviceId, callSignatureValue signature]

parameterValue :: LoweredParameter -> RuntimeValue
parameterValue (LoweredParameter parameterId representation) =
  constructor "LoweredParameter" [parameterIdValue parameterId, representationValue representation]

immediateValue :: LoweredImmediate -> RuntimeValue
immediateValue immediate =
  case immediate of
    LoweredUnitImmediate -> nullary "LoweredUnitImmediate"
    LoweredBoolImmediate value -> constructor "LoweredBoolImmediate" [VBool value]
    LoweredSignedIntegerImmediate width value -> constructor "LoweredSignedIntegerImmediate" [integerWidthValue width, integerValue value]
    LoweredUnsignedIntegerImmediate width value -> constructor "LoweredUnsignedIntegerImmediate" [integerWidthValue width, integerValue value]
    LoweredFloatImmediate width value -> constructor "LoweredFloatImmediate" [floatWidthValue width, VText value]
    LoweredCharImmediate value -> constructor "LoweredCharImmediate" [VChar value]

operandValue :: LoweredOperand -> RuntimeValue
operandValue operand =
  case operand of
    LoweredFunctionParameterOperand parameterId representation ->
      constructor "LoweredFunctionParameterOperand" [parameterIdValue parameterId, representationValue representation]
    LoweredBlockParameterOperand parameterId representation ->
      constructor "LoweredBlockParameterOperand" [parameterIdValue parameterId, representationValue representation]
    LoweredTemporaryOperand temporaryId representation ->
      constructor "LoweredTemporaryOperand" [temporaryIdValue temporaryId, representationValue representation]
    LoweredImmediateOperand immediate -> constructor "LoweredImmediateOperand" [immediateValue immediate]

arithmeticPrimitiveValue :: LoweredArithmeticPrimitive -> RuntimeValue
arithmeticPrimitiveValue primitive =
  nullary
    ( case primitive of
        LoweredAdd -> "LoweredAdd"
        LoweredSubtract -> "LoweredSubtract"
        LoweredMultiply -> "LoweredMultiply"
        LoweredDivide -> "LoweredDivide"
        LoweredRemainder -> "LoweredRemainder"
    )

comparisonPrimitiveValue :: LoweredComparisonPrimitive -> RuntimeValue
comparisonPrimitiveValue primitive =
  nullary
    ( case primitive of
        LoweredEqual -> "LoweredEqual"
        LoweredNotEqual -> "LoweredNotEqual"
        LoweredLessThan -> "LoweredLessThan"
        LoweredLessThanOrEqual -> "LoweredLessThanOrEqual"
        LoweredGreaterThan -> "LoweredGreaterThan"
        LoweredGreaterThanOrEqual -> "LoweredGreaterThanOrEqual"
    )

booleanPrimitiveValue :: LoweredBooleanPrimitive -> RuntimeValue
booleanPrimitiveValue primitive =
  nullary
    ( case primitive of
        LoweredBooleanNot -> "LoweredBooleanNot"
        LoweredBooleanAnd -> "LoweredBooleanAnd"
        LoweredBooleanOr -> "LoweredBooleanOr"
    )

primitiveValue :: LoweredPrimitive -> RuntimeValue
primitiveValue primitive =
  case primitive of
    LoweredArithmeticPrimitive operation -> constructor "LoweredArithmeticPrimitive" [arithmeticPrimitiveValue operation]
    LoweredComparisonPrimitive operation -> constructor "LoweredComparisonPrimitive" [comparisonPrimitiveValue operation]
    LoweredBooleanPrimitive operation -> constructor "LoweredBooleanPrimitive" [booleanPrimitiveValue operation]

operationValue :: LoweredOperation -> RuntimeValue
operationValue operation =
  case operation of
    LoweredPrimitiveOperation primitive operands -> constructor "LoweredPrimitiveOperation" [primitiveValue primitive, listValue operandValue operands]
    LoweredConstructProduct layoutId operands -> constructor "LoweredConstructProduct" [layoutIdValue layoutId, listValue operandValue operands]
    LoweredConstructVariant layoutId tag operands -> constructor "LoweredConstructVariant" [layoutIdValue layoutId, integerValue tag, listValue operandValue operands]
    LoweredConstructList layoutId operands -> constructor "LoweredConstructList" [layoutIdValue layoutId, listValue operandValue operands]
    LoweredConstructText layoutId value -> constructor "LoweredConstructText" [layoutIdValue layoutId, VText value]
    LoweredConstructClosure functionId environment -> constructor "LoweredConstructClosure" [functionIdValue functionId, operandValue environment]
    LoweredProjectField layoutId fieldIndex operand -> constructor "LoweredProjectField" [layoutIdValue layoutId, runtimeIntValue fieldIndex, operandValue operand]
    LoweredProjectVariantTag layoutId operand -> constructor "LoweredProjectVariantTag" [layoutIdValue layoutId, operandValue operand]
    LoweredProjectVariantField layoutId tag fieldIndex operand -> constructor "LoweredProjectVariantField" [layoutIdValue layoutId, integerValue tag, runtimeIntValue fieldIndex, operandValue operand]
    LoweredDirectCall functionId operands -> constructor "LoweredDirectCall" [functionIdValue functionId, listValue operandValue operands]
    LoweredClosureCall functionOperand operands -> constructor "LoweredClosureCall" [operandValue functionOperand, listValue operandValue operands]
    LoweredRuntimeCall serviceId operands -> constructor "LoweredRuntimeCall" [runtimeServiceIdValue serviceId, listValue operandValue operands]

instructionValue :: LoweredInstruction -> RuntimeValue
instructionValue (LoweredInstruction temporaryId representation operation) =
  constructor "LoweredInstruction" [temporaryIdValue temporaryId, representationValue representation, operationValue operation]

switchCaseValue :: LoweredSwitchCase -> RuntimeValue
switchCaseValue (LoweredSwitchCase tag blockId operands) =
  constructor "LoweredSwitchCase" [integerValue tag, blockIdValue blockId, listValue operandValue operands]

switchDefaultValue :: LoweredSwitchDefault -> RuntimeValue
switchDefaultValue (LoweredSwitchDefault blockId operands) =
  constructor "LoweredSwitchDefault" [blockIdValue blockId, listValue operandValue operands]

terminatorValue :: LoweredTerminator -> RuntimeValue
terminatorValue terminator =
  case terminator of
    LoweredReturn operand -> constructor "LoweredReturn" [operandValue operand]
    LoweredJump blockId operands -> constructor "LoweredJump" [blockIdValue blockId, listValue operandValue operands]
    LoweredBranch condition thenBlock thenOperands elseBlock elseOperands ->
      constructor
        "LoweredBranch"
        [ operandValue condition,
          blockIdValue thenBlock,
          listValue operandValue thenOperands,
          blockIdValue elseBlock,
          listValue operandValue elseOperands
        ]
    LoweredSwitch operand cases maybeDefault ->
      constructor "LoweredSwitch" [operandValue operand, listValue switchCaseValue cases, maybeValue switchDefaultValue maybeDefault]
    LoweredDirectTailCall functionId operands -> constructor "LoweredDirectTailCall" [functionIdValue functionId, listValue operandValue operands]
    LoweredClosureTailCall functionOperand operands -> constructor "LoweredClosureTailCall" [operandValue functionOperand, listValue operandValue operands]

blockValue :: LoweredBlock -> RuntimeValue
blockValue (LoweredBlock blockId parameters instructions maybeTerminator) =
  constructor
    "LoweredBlock"
    [ blockIdValue blockId,
      listValue parameterValue parameters,
      listValue instructionValue instructions,
      maybeValue terminatorValue maybeTerminator
    ]

functionValue :: LoweredFunction -> RuntimeValue
functionValue (LoweredFunction functionId maybeEnvironment parameters resultRepresentation blocks entryBlock) =
  constructor
    "LoweredFunction"
    [ functionIdValue functionId,
      maybeValue parameterValue maybeEnvironment,
      listValue parameterValue parameters,
      representationValue resultRepresentation,
      listValue blockValue blocks,
      blockIdValue entryBlock
    ]

validationPathValue :: LoweredIRValidationPath -> RuntimeValue
validationPathValue path =
  case path of
    LoweredProgramPath -> nullary "LoweredProgramPath"
    LoweredLayoutPath layoutId -> constructor "LoweredLayoutPath" [layoutIdValue layoutId]
    LoweredRuntimeServicePath serviceId -> constructor "LoweredRuntimeServicePath" [runtimeServiceIdValue serviceId]
    LoweredFunctionPath functionId -> constructor "LoweredFunctionPath" [functionIdValue functionId]
    LoweredBlockPath functionId blockId -> constructor "LoweredBlockPath" [functionIdValue functionId, blockIdValue blockId]
    LoweredInstructionPath functionId blockId instructionIndex ->
      constructor "LoweredInstructionPath" [functionIdValue functionId, blockIdValue blockId, runtimeIntValue instructionIndex]
    LoweredTerminatorPath functionId blockId -> constructor "LoweredTerminatorPath" [functionIdValue functionId, blockIdValue blockId]

validationKindValue :: LoweredIRValidationKind -> RuntimeValue
validationKindValue kind = nullary (validationKindName kind)

validationKindName :: LoweredIRValidationKind -> Text
validationKindName kind =
  case kind of
    LoweredDuplicateLayout -> "LoweredDuplicateLayout"
    LoweredUnknownLayout -> "LoweredUnknownLayout"
    LoweredDuplicateVariantTag -> "LoweredDuplicateVariantTag"
    LoweredDuplicateRuntimeService -> "LoweredDuplicateRuntimeService"
    LoweredUnknownRuntimeService -> "LoweredUnknownRuntimeService"
    LoweredDuplicateFunction -> "LoweredDuplicateFunction"
    LoweredMissingEntryFunction -> "LoweredMissingEntryFunction"
    LoweredDuplicateBlock -> "LoweredDuplicateBlock"
    LoweredMissingEntryBlock -> "LoweredMissingEntryBlock"
    LoweredMissingTerminator -> "LoweredMissingTerminator"
    LoweredDuplicateTemporary -> "LoweredDuplicateTemporary"
    LoweredUseBeforeDefinition -> "LoweredUseBeforeDefinition"
    LoweredCrossBlockTemporary -> "LoweredCrossBlockTemporary"
    LoweredUnknownParameter -> "LoweredUnknownParameter"
    LoweredUnknownFunction -> "LoweredUnknownFunction"
    LoweredUnknownBlock -> "LoweredUnknownBlock"
    LoweredInstructionResultRepresentationMismatch -> "LoweredInstructionResultRepresentationMismatch"
    LoweredInvalidFieldProjection -> "LoweredInvalidFieldProjection"
    LoweredInvalidTagProjection -> "LoweredInvalidTagProjection"
    LoweredClosureEnvironmentMismatch -> "LoweredClosureEnvironmentMismatch"
    LoweredEdgeArityMismatch -> "LoweredEdgeArityMismatch"
    LoweredEdgeRepresentationMismatch -> "LoweredEdgeRepresentationMismatch"
    LoweredBranchConditionMismatch -> "LoweredBranchConditionMismatch"
    LoweredDuplicateSwitchCaseTag -> "LoweredDuplicateSwitchCaseTag"
    LoweredReturnRepresentationMismatch -> "LoweredReturnRepresentationMismatch"
    LoweredDirectCallSignatureMismatch -> "LoweredDirectCallSignatureMismatch"
    LoweredClosureCallSignatureMismatch -> "LoweredClosureCallSignatureMismatch"
    LoweredRuntimeCallSignatureMismatch -> "LoweredRuntimeCallSignatureMismatch"
    LoweredDirectTailCallSignatureMismatch -> "LoweredDirectTailCallSignatureMismatch"
    LoweredClosureTailCallSignatureMismatch -> "LoweredClosureTailCallSignatureMismatch"

validationDetailValue :: LoweredIRValidationDetail -> RuntimeValue
validationDetailValue detail =
  case detail of
    LoweredNoValidationDetail -> nullary "LoweredNoValidationDetail"
    LoweredIdentifierDetail identifier -> constructor "LoweredIdentifierDetail" [VText identifier]
    LoweredRepresentationDetail expected actual -> constructor "LoweredRepresentationDetail" [representationValue expected, representationValue actual]
    LoweredArityDetail expected actual -> constructor "LoweredArityDetail" [runtimeIntValue expected, runtimeIntValue actual]
    LoweredIndexDetail index -> constructor "LoweredIndexDetail" [runtimeIntValue index]
    LoweredTagDetail tag -> constructor "LoweredTagDetail" [integerValue tag]

validationFailureValue :: LoweredIRValidationFailure -> RuntimeValue
validationFailureValue (LoweredIRValidationFailure path kind detail) =
  constructor "LoweredIRValidationFailure" [validationPathValue path, validationKindValue kind, validationDetailValue detail]

constructor :: Text -> [RuntimeValue] -> RuntimeValue
constructor = canonicalConstructor

nullary :: Text -> RuntimeValue
nullary = canonicalNullaryConstructor

listValue :: (value -> RuntimeValue) -> [value] -> RuntimeValue
listValue render values = VList (map render values) Nothing

maybeValue :: (value -> RuntimeValue) -> Maybe value -> RuntimeValue
maybeValue render maybeInput =
  case maybeInput of
    Nothing -> nullary "Nothing"
    Just value -> constructor "Just" [render value]

integerValue :: Integer -> RuntimeValue
integerValue value = VInt value untypedIntMetadata
