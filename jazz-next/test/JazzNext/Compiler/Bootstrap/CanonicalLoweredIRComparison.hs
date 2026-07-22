{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalLoweredIRComparison
  ( canonicalLoweredProgramRuntimeValue,
    canonicalLoweredProgramsRuntimeValue,
    canonicalLoweredValidationFailuresRuntimeValue,
    decodeCanonicalLoweredValidationFailuresRuntimeValue
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
    runtimeIntValue
  )
import JazzNext.Compiler.LoweredIR
import JazzNext.Compiler.Name (identifierText)
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

decodeCanonicalLoweredValidationFailuresRuntimeValue :: RuntimeValue -> Either Text [LoweredIRValidationFailure]
decodeCanonicalLoweredValidationFailuresRuntimeValue value =
  case value of
    VList failures _ -> traverse decodeValidationFailure failures
    _ -> Left ("validation failures expected a List, got " <> runtimeValueCategory value)

decodeValidationFailure :: RuntimeValue -> Either Text LoweredIRValidationFailure
decodeValidationFailure value = do
  (name, arguments) <- expectConstructor "validation failure" value
  if name /= "LoweredIRValidationFailure"
    then Left ("unknown validation failure constructor '" <> name <> "'")
    else do
      fields <- expectArity name 3 arguments
      case fields of
        [pathValue, kindValue, detailValue] ->
          LoweredIRValidationFailure
            <$> decodeValidationPath pathValue
            <*> decodeValidationKind kindValue
            <*> decodeValidationDetail detailValue
        _ -> impossibleArity name

decodeValidationPath :: RuntimeValue -> Either Text LoweredIRValidationPath
decodeValidationPath value = do
  (name, arguments) <- expectConstructor "validation path" value
  case name of
    "LoweredProgramPath" -> expectNullary name arguments LoweredProgramPath
    "LoweredLayoutPath" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [layoutId] -> LoweredLayoutPath <$> decodeLayoutId layoutId
        _ -> impossibleArity name
    "LoweredRuntimeServicePath" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [serviceId] -> LoweredRuntimeServicePath <$> decodeRuntimeServiceId serviceId
        _ -> impossibleArity name
    "LoweredFunctionPath" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [functionId] -> LoweredFunctionPath <$> decodeFunctionId functionId
        _ -> impossibleArity name
    "LoweredBlockPath" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [functionId, blockId] -> LoweredBlockPath <$> decodeFunctionId functionId <*> decodeBlockId blockId
        _ -> impossibleArity name
    "LoweredInstructionPath" -> do
      fields <- expectArity name 3 arguments
      case fields of
        [functionId, blockId, instructionIndex] ->
          LoweredInstructionPath <$> decodeFunctionId functionId <*> decodeBlockId blockId <*> decodeInt "instruction index" instructionIndex
        _ -> impossibleArity name
    "LoweredTerminatorPath" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [functionId, blockId] -> LoweredTerminatorPath <$> decodeFunctionId functionId <*> decodeBlockId blockId
        _ -> impossibleArity name
    _ -> Left ("unknown validation path constructor '" <> name <> "'")

decodeValidationKind :: RuntimeValue -> Either Text LoweredIRValidationKind
decodeValidationKind value = do
  (name, arguments) <- expectConstructor "validation kind" value
  kind <-
    case name of
      "LoweredUnsupportedVersion" -> Right LoweredUnsupportedVersion
      "LoweredDuplicateLayout" -> Right LoweredDuplicateLayout
      "LoweredUnknownLayout" -> Right LoweredUnknownLayout
      "LoweredDuplicateVariantTag" -> Right LoweredDuplicateVariantTag
      "LoweredDuplicateRuntimeService" -> Right LoweredDuplicateRuntimeService
      "LoweredUnknownRuntimeService" -> Right LoweredUnknownRuntimeService
      "LoweredDuplicateFunction" -> Right LoweredDuplicateFunction
      "LoweredMissingEntryFunction" -> Right LoweredMissingEntryFunction
      "LoweredDuplicateBlock" -> Right LoweredDuplicateBlock
      "LoweredMissingEntryBlock" -> Right LoweredMissingEntryBlock
      "LoweredDuplicateParameter" -> Right LoweredDuplicateParameter
      "LoweredEntryBlockParameters" -> Right LoweredEntryBlockParameters
      "LoweredMissingTerminator" -> Right LoweredMissingTerminator
      "LoweredDuplicateTemporary" -> Right LoweredDuplicateTemporary
      "LoweredUseBeforeDefinition" -> Right LoweredUseBeforeDefinition
      "LoweredCrossBlockTemporary" -> Right LoweredCrossBlockTemporary
      "LoweredUnknownParameter" -> Right LoweredUnknownParameter
      "LoweredUnknownFunction" -> Right LoweredUnknownFunction
      "LoweredUnknownBlock" -> Right LoweredUnknownBlock
      "LoweredImmediateOutOfRange" -> Right LoweredImmediateOutOfRange
      "LoweredPrimitiveSignatureMismatch" -> Right LoweredPrimitiveSignatureMismatch
      "LoweredInstructionResultRepresentationMismatch" -> Right LoweredInstructionResultRepresentationMismatch
      "LoweredInvalidFieldProjection" -> Right LoweredInvalidFieldProjection
      "LoweredInvalidTagProjection" -> Right LoweredInvalidTagProjection
      "LoweredClosureEnvironmentMismatch" -> Right LoweredClosureEnvironmentMismatch
      "LoweredEdgeArityMismatch" -> Right LoweredEdgeArityMismatch
      "LoweredEdgeRepresentationMismatch" -> Right LoweredEdgeRepresentationMismatch
      "LoweredBranchConditionMismatch" -> Right LoweredBranchConditionMismatch
      "LoweredDuplicateSwitchCaseTag" -> Right LoweredDuplicateSwitchCaseTag
      "LoweredReturnRepresentationMismatch" -> Right LoweredReturnRepresentationMismatch
      "LoweredDirectCallSignatureMismatch" -> Right LoweredDirectCallSignatureMismatch
      "LoweredClosureCallSignatureMismatch" -> Right LoweredClosureCallSignatureMismatch
      "LoweredRuntimeCallSignatureMismatch" -> Right LoweredRuntimeCallSignatureMismatch
      "LoweredDirectTailCallSignatureMismatch" -> Right LoweredDirectTailCallSignatureMismatch
      "LoweredClosureTailCallSignatureMismatch" -> Right LoweredClosureTailCallSignatureMismatch
      _ -> Left ("unknown validation kind constructor '" <> name <> "'")
  expectNullary name arguments kind

decodeValidationDetail :: RuntimeValue -> Either Text LoweredIRValidationDetail
decodeValidationDetail value = do
  (name, arguments) <- expectConstructor "validation detail" value
  case name of
    "LoweredNoValidationDetail" -> expectNullary name arguments LoweredNoValidationDetail
    "LoweredIdentifierDetail" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [identifier] -> LoweredIdentifierDetail <$> decodeText "validation identifier" identifier
        _ -> impossibleArity name
    "LoweredVersionDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> LoweredVersionDetail <$> decodeVersion expected <*> decodeVersion actual
        _ -> impossibleArity name
    "LoweredRepresentationDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> LoweredRepresentationDetail <$> decodeRepresentation expected <*> decodeRepresentation actual
        _ -> impossibleArity name
    "LoweredImmediateRangeDetail" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [representation] -> LoweredImmediateRangeDetail <$> decodeRepresentation representation
        _ -> impossibleArity name
    "LoweredArityDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> LoweredArityDetail <$> decodeInt "expected arity" expected <*> decodeInt "actual arity" actual
        _ -> impossibleArity name
    "LoweredIndexDetail" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [index] -> LoweredIndexDetail <$> decodeInt "validation index" index
        _ -> impossibleArity name
    "LoweredTagDetail" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [tag] -> LoweredTagDetail <$> decodeInteger "validation tag" tag
        _ -> impossibleArity name
    _ -> Left ("unknown validation detail constructor '" <> name <> "'")

decodeRepresentation :: RuntimeValue -> Either Text LoweredRepresentation
decodeRepresentation value = do
  (name, arguments) <- expectConstructor "representation" value
  case name of
    "LoweredUnitRepresentation" -> expectNullary name arguments LoweredUnitRepresentation
    "LoweredBoolRepresentation" -> expectNullary name arguments LoweredBoolRepresentation
    "LoweredSignedIntegerRepresentation" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [width] -> LoweredSignedIntegerRepresentation <$> decodeIntegerWidth width
        _ -> impossibleArity name
    "LoweredUnsignedIntegerRepresentation" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [width] -> LoweredUnsignedIntegerRepresentation <$> decodeIntegerWidth width
        _ -> impossibleArity name
    "LoweredFloatRepresentation" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [width] -> LoweredFloatRepresentation <$> decodeFloatWidth width
        _ -> impossibleArity name
    "LoweredCharRepresentation" -> expectNullary name arguments LoweredCharRepresentation
    "LoweredManagedReferenceRepresentation" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [layoutId] -> LoweredManagedReferenceRepresentation <$> decodeLayoutId layoutId
        _ -> impossibleArity name
    "LoweredClosureRepresentation" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [signature] -> LoweredClosureRepresentation <$> decodeCallSignature signature
        _ -> impossibleArity name
    _ -> Left ("unknown representation constructor '" <> name <> "'")

decodeVersion :: RuntimeValue -> Either Text LoweredIRVersion
decodeVersion value = do
  fields <- expectNamedConstructor "lowered IR version" "LoweredIRVersion" 1 value
  case fields of
    [version] -> LoweredIRVersion <$> decodeInteger "lowered IR version" version
    _ -> impossibleArity "LoweredIRVersion"

decodeCallSignature :: RuntimeValue -> Either Text LoweredCallSignature
decodeCallSignature value = do
  fields <- expectNamedConstructor "call signature" "LoweredCallSignature" 2 value
  case fields of
    [parameters, result] -> LoweredCallSignature <$> decodeList "call parameters" decodeRepresentation parameters <*> decodeRepresentation result
    _ -> impossibleArity "LoweredCallSignature"

decodeIntegerWidth :: RuntimeValue -> Either Text LoweredIntegerWidth
decodeIntegerWidth value = do
  (name, arguments) <- expectConstructor "integer width" value
  width <-
    case name of
      "LoweredIntegerWidth8" -> Right LoweredIntegerWidth8
      "LoweredIntegerWidth16" -> Right LoweredIntegerWidth16
      "LoweredIntegerWidth32" -> Right LoweredIntegerWidth32
      "LoweredIntegerWidth64" -> Right LoweredIntegerWidth64
      _ -> Left ("unknown integer width constructor '" <> name <> "'")
  expectNullary name arguments width

decodeFloatWidth :: RuntimeValue -> Either Text LoweredFloatWidth
decodeFloatWidth value = do
  (name, arguments) <- expectConstructor "float width" value
  width <-
    case name of
      "LoweredFloatWidth16" -> Right LoweredFloatWidth16
      "LoweredFloatWidth32" -> Right LoweredFloatWidth32
      "LoweredFloatWidth64" -> Right LoweredFloatWidth64
      _ -> Left ("unknown float width constructor '" <> name <> "'")
  expectNullary name arguments width

decodeLayoutId :: RuntimeValue -> Either Text LoweredLayoutId
decodeLayoutId value = LoweredLayoutId <$> decodeTextConstructor "LoweredLayoutId" value

decodeRuntimeServiceId :: RuntimeValue -> Either Text LoweredRuntimeServiceId
decodeRuntimeServiceId value = LoweredRuntimeServiceId <$> decodeTextConstructor "LoweredRuntimeServiceId" value

decodeFunctionId :: RuntimeValue -> Either Text LoweredFunctionId
decodeFunctionId value = LoweredFunctionId <$> decodeTextConstructor "LoweredFunctionId" value

decodeBlockId :: RuntimeValue -> Either Text LoweredBlockId
decodeBlockId value = LoweredBlockId <$> decodeTextConstructor "LoweredBlockId" value

decodeTextConstructor :: Text -> RuntimeValue -> Either Text Text
decodeTextConstructor name value = do
  fields <- expectNamedConstructor name name 1 value
  case fields of
    [textValue] -> decodeText (name <> " value") textValue
    _ -> impossibleArity name

decodeList :: Text -> (RuntimeValue -> Either Text value) -> RuntimeValue -> Either Text [value]
decodeList label decodeElement value =
  case value of
    VList elements _ -> traverse decodeElement elements
    _ -> Left (label <> " expected a List, got " <> runtimeValueCategory value)

decodeText :: Text -> RuntimeValue -> Either Text Text
decodeText label value =
  case value of
    VText textValue -> Right textValue
    _ -> Left (label <> " expected Text, got " <> runtimeValueCategory value)

decodeInteger :: Text -> RuntimeValue -> Either Text Integer
decodeInteger label value =
  case value of
    VInt integer _ -> Right integer
    _ -> Left (label <> " expected Int, got " <> runtimeValueCategory value)

decodeInt :: Text -> RuntimeValue -> Either Text Int
decodeInt label value = do
  integer <- decodeInteger label value
  if integer < toInteger (minBound :: Int) || integer > toInteger (maxBound :: Int)
    then Left (label <> " is outside the host Int range: " <> Text.pack (show integer))
    else Right (fromInteger integer)

expectConstructor :: Text -> RuntimeValue -> Either Text (Text, [RuntimeValue])
expectConstructor label value =
  case value of
    VConstructor _ _ constructorName _ arguments -> Right (identifierText constructorName, arguments)
    _ -> Left (label <> " expected a constructor, got " <> runtimeValueCategory value)

expectNamedConstructor :: Text -> Text -> Int -> RuntimeValue -> Either Text [RuntimeValue]
expectNamedConstructor label expectedName expectedArity value = do
  (actualName, arguments) <- expectConstructor label value
  if actualName /= expectedName
    then Left (label <> " expected constructor '" <> expectedName <> "', got '" <> actualName <> "'")
    else expectArity expectedName expectedArity arguments

expectArity :: Text -> Int -> [RuntimeValue] -> Either Text [RuntimeValue]
expectArity name expected arguments
  | length arguments == expected = Right arguments
  | otherwise = Left (name <> " expected " <> Text.pack (show expected) <> " field(s), got " <> Text.pack (show (length arguments)))

expectNullary :: Text -> [RuntimeValue] -> value -> Either Text value
expectNullary name arguments value = do
  _ <- expectArity name 0 arguments
  Right value

impossibleArity :: Text -> Either Text value
impossibleArity name = Left ("internal checked-adapter arity mismatch for '" <> name <> "'")

runtimeValueCategory :: RuntimeValue -> Text
runtimeValueCategory value =
  case value of
    VInt {} -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VChar {} -> "Char"
    VText {} -> "Text"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VConstructor {} -> "constructor"
    VClosure {} -> "closure"
    VBuiltin {} -> "builtin"
    VOperator {} -> "operator"
    VSectionLeft {} -> "left section"
    VSectionRight {} -> "right section"
    VQualifiedMethod {} -> "qualified method"
    VTyped {} -> "typed value"
    VExplicitTypeApplication {} -> "explicit type application"
    _ -> "runtime value"

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
    LoweredUnsupportedVersion -> "LoweredUnsupportedVersion"
    LoweredDuplicateLayout -> "LoweredDuplicateLayout"
    LoweredUnknownLayout -> "LoweredUnknownLayout"
    LoweredDuplicateVariantTag -> "LoweredDuplicateVariantTag"
    LoweredDuplicateRuntimeService -> "LoweredDuplicateRuntimeService"
    LoweredUnknownRuntimeService -> "LoweredUnknownRuntimeService"
    LoweredDuplicateFunction -> "LoweredDuplicateFunction"
    LoweredMissingEntryFunction -> "LoweredMissingEntryFunction"
    LoweredDuplicateBlock -> "LoweredDuplicateBlock"
    LoweredMissingEntryBlock -> "LoweredMissingEntryBlock"
    LoweredDuplicateParameter -> "LoweredDuplicateParameter"
    LoweredEntryBlockParameters -> "LoweredEntryBlockParameters"
    LoweredMissingTerminator -> "LoweredMissingTerminator"
    LoweredDuplicateTemporary -> "LoweredDuplicateTemporary"
    LoweredUseBeforeDefinition -> "LoweredUseBeforeDefinition"
    LoweredCrossBlockTemporary -> "LoweredCrossBlockTemporary"
    LoweredUnknownParameter -> "LoweredUnknownParameter"
    LoweredUnknownFunction -> "LoweredUnknownFunction"
    LoweredUnknownBlock -> "LoweredUnknownBlock"
    LoweredImmediateOutOfRange -> "LoweredImmediateOutOfRange"
    LoweredPrimitiveSignatureMismatch -> "LoweredPrimitiveSignatureMismatch"
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
    LoweredVersionDetail expected actual -> constructor "LoweredVersionDetail" [versionValue expected, versionValue actual]
    LoweredRepresentationDetail expected actual -> constructor "LoweredRepresentationDetail" [representationValue expected, representationValue actual]
    LoweredImmediateRangeDetail representation -> constructor "LoweredImmediateRangeDetail" [representationValue representation]
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
