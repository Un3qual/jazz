-- | Complete structural validation for backend-neutral lowered IR.
module JazzNext.Compiler.LoweredIR.Validate
  ( validateLoweredProgram
  ) where

import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.LoweredIR

data ProgramContext = ProgramContext
  { contextLayouts :: Map LoweredLayoutId LoweredLayoutShape,
    contextServices :: Map LoweredRuntimeServiceId LoweredCallSignature,
    contextFunctions :: Map LoweredFunctionId LoweredFunction
  }

data FunctionContext = FunctionContext
  { functionContextProgram :: ProgramContext,
    functionContextFunction :: LoweredFunction,
    functionContextBlocks :: Map LoweredBlockId LoweredBlock,
    functionContextTemporaryOwners :: Map LoweredTemporaryId (Set LoweredBlockId),
    functionContextParameters :: Map LoweredParameterId LoweredRepresentation
  }

validateLoweredProgram :: LoweredProgram -> [LoweredIRValidationFailure]
validateLoweredProgram (LoweredProgram version layouts services functions entryFunction) =
  versionFailures
    <> duplicateLayoutFailures layouts
    <> duplicateServiceFailures services
    <> duplicateFunctionFailures functions
    <> missingEntryFailure
    <> entryFunctionParameterFailures
    <> concatMap (validateLayout programContext) layouts
    <> concatMap (validateRuntimeService programContext) services
    <> concatMap (validateFunction programContext) functions
  where
    programContext =
      ProgramContext
        { contextLayouts = Map.fromList [(layoutId, shape) | LoweredLayout layoutId shape <- layouts],
          contextServices = Map.fromList [(serviceId, signature) | LoweredRuntimeService serviceId signature <- services],
          contextFunctions = Map.fromList [(functionId, function) | function@(LoweredFunction functionId _ _ _ _ _) <- functions]
        }
    missingEntryFailure
      | Map.member entryFunction (contextFunctions programContext) = []
      | otherwise = [failure LoweredProgramPath LoweredMissingEntryFunction (identifierDetail (functionIdText entryFunction))]
    entryFunctionParameterFailures =
      case Map.lookup entryFunction (contextFunctions programContext) of
        Just (LoweredFunction _ maybeEnvironment parameters _ _ _)
          | let parameterCount = length (maybeToList maybeEnvironment <> parameters),
            parameterCount > 0 ->
              [ failure
                  (LoweredFunctionPath entryFunction)
                  LoweredEntryFunctionParameters
                  (LoweredArityDetail 0 parameterCount)
              ]
        _ -> []
    versionFailures
      | version == supportedLoweredIRVersion = []
      | otherwise = [failure LoweredProgramPath LoweredUnsupportedVersion (LoweredVersionDetail supportedLoweredIRVersion version)]

duplicateLayoutFailures :: [LoweredLayout] -> [LoweredIRValidationFailure]
duplicateLayoutFailures =
  duplicateFailures
    (\(LoweredLayout layoutId _) -> layoutId)
    (\layoutId -> LoweredLayoutPath layoutId)
    LoweredDuplicateLayout
    layoutIdText

duplicateServiceFailures :: [LoweredRuntimeService] -> [LoweredIRValidationFailure]
duplicateServiceFailures =
  duplicateFailures
    (\(LoweredRuntimeService serviceId _) -> serviceId)
    (\serviceId -> LoweredRuntimeServicePath serviceId)
    LoweredDuplicateRuntimeService
    runtimeServiceIdText

duplicateFunctionFailures :: [LoweredFunction] -> [LoweredIRValidationFailure]
duplicateFunctionFailures =
  duplicateFailures
    (\(LoweredFunction functionId _ _ _ _ _) -> functionId)
    (\functionId -> LoweredFunctionPath functionId)
    LoweredDuplicateFunction
    functionIdText

duplicateFailures :: Ord identifier => (value -> identifier) -> (identifier -> LoweredIRValidationPath) -> LoweredIRValidationKind -> (identifier -> Text) -> [value] -> [LoweredIRValidationFailure]
duplicateFailures identifierOf pathOf kind renderIdentifier values = snd (foldl' step (Set.empty, []) values)
  where
    step (seen, failures) value =
      let identifier = identifierOf value
       in if Set.member identifier seen
            then (seen, failures <> [failure (pathOf identifier) kind (identifierDetail (renderIdentifier identifier))])
            else (Set.insert identifier seen, failures)

validateLayout :: ProgramContext -> LoweredLayout -> [LoweredIRValidationFailure]
validateLayout programContext (LoweredLayout layoutId shape) =
  concatMap (validateRepresentation programContext path) (layoutRepresentations shape)
    <> variantTagRangeFailures
    <> duplicateVariantTagFailures
  where
    path = LoweredLayoutPath layoutId
    variantTagRangeFailures =
      case shape of
        LoweredVariantLayouts variants -> concatMap (validateTag path . variantTag) variants
        _ -> []
    duplicateVariantTagFailures =
      case shape of
        LoweredVariantLayouts variants -> snd (foldl' collectDuplicateTag (Set.empty, []) variants)
        _ -> []
    collectDuplicateTag (seen, failures) (LoweredVariantLayout tag _)
      | Set.member tag seen =
          (seen, failures <> [failure path LoweredDuplicateVariantTag (LoweredTagDetail tag)])
      | otherwise = (Set.insert tag seen, failures)
    variantTag (LoweredVariantLayout tag _) = tag

layoutRepresentations :: LoweredLayoutShape -> [LoweredRepresentation]
layoutRepresentations shape =
  case shape of
    LoweredProductLayout fields -> fields
    LoweredVariantLayouts variants -> concatMap (\(LoweredVariantLayout _ fields) -> fields) variants
    LoweredClosureEnvironmentLayout fields -> fields
    LoweredTextLayout -> []
    LoweredListLayout elementRepresentation -> [elementRepresentation]

validateRuntimeService :: ProgramContext -> LoweredRuntimeService -> [LoweredIRValidationFailure]
validateRuntimeService programContext (LoweredRuntimeService serviceId signature) =
  validateCallSignatureRepresentations programContext (LoweredRuntimeServicePath serviceId) signature

validateFunction :: ProgramContext -> LoweredFunction -> [LoweredIRValidationFailure]
validateFunction programContext function@(LoweredFunction functionId maybeEnvironment parameters resultRepresentation blocks entryBlock) =
  duplicateBlockFailures
    <> missingEntryBlockFailure
    <> duplicateParameterFailures (LoweredFunctionPath functionId) (maybeToList maybeEnvironment <> parameters)
    <> entryBlockParameterFailures
    <> maybe [] (validateClosureEnvironmentParameter programContext (LoweredFunctionPath functionId)) maybeEnvironment
    <> concatMap (validateRepresentation programContext (LoweredFunctionPath functionId)) functionRepresentations
    <> concatMap (validateBlock functionContext) blocks
  where
    blocksById = Map.fromList [(blockId, block) | block@(LoweredBlock blockId _ _ _) <- blocks]
    functionContext =
      FunctionContext
        { functionContextProgram = programContext,
          functionContextFunction = function,
          functionContextBlocks = blocksById,
          functionContextTemporaryOwners =
            Map.fromListWith Set.union
              [ (temporaryId, Set.singleton blockId)
                | LoweredBlock blockId _ instructions _ <- blocks,
                  LoweredInstruction temporaryId _ _ <- instructions
              ],
          functionContextParameters =
            Map.fromList
              [ (parameterId, representation)
                | LoweredParameter parameterId representation <- maybeToList maybeEnvironment <> parameters
              ]
        }
    duplicateBlockFailures =
      duplicateFailures
        (\(LoweredBlock blockId _ _ _) -> blockId)
        (\blockId -> LoweredBlockPath functionId blockId)
        LoweredDuplicateBlock
        blockIdText
        blocks
    missingEntryBlockFailure
      | Map.member entryBlock blocksById = []
      | otherwise = [failure (LoweredFunctionPath functionId) LoweredMissingEntryBlock (identifierDetail (blockIdText entryBlock))]
    entryBlockParameterFailures =
      case Map.lookup entryBlock blocksById of
        Just (LoweredBlock _ entryParameters _ _)
          | not (null entryParameters) ->
              [ failure
                  (LoweredBlockPath functionId entryBlock)
                  LoweredEntryBlockParameters
                  (LoweredArityDetail 0 (length entryParameters))
              ]
        _ -> []
    functionRepresentations =
      resultRepresentation
        : [representation | LoweredParameter _ representation <- maybeToList maybeEnvironment <> parameters]

validateBlock :: FunctionContext -> LoweredBlock -> [LoweredIRValidationFailure]
validateBlock functionContext (LoweredBlock blockId parameters instructions maybeTerminator) =
  duplicateParameterFailures blockPath parameters
    <> concatMap (validateRepresentation programContext blockPath) blockParameterRepresentations
    <> duplicateTemporaryFailures
    <> instructionFailures
    <> missingTerminatorFailure
    <> terminatorFailures
  where
    functionId = currentFunctionId functionContext
    programContext = functionContextProgram functionContext
    blockPath = LoweredBlockPath functionId blockId
    blockParameterRepresentations = [representation | LoweredParameter _ representation <- parameters]
    blockParameters = Map.fromList [(parameterId, representation) | LoweredParameter parameterId representation <- parameters]
    duplicateTemporaryFailures =
      duplicateFailures
        (\(LoweredInstruction temporaryId _ _) -> temporaryId)
        (const blockPath)
        LoweredDuplicateTemporary
        temporaryIdText
        instructions
        & zipWithInstructionPaths functionId blockId instructions
    (_, instructionFailures) =
      foldl'
        (validateInstruction functionContext blockId blockParameters)
        (Set.empty, [])
        (zip [0 ..] instructions)
    missingTerminatorFailure =
      case maybeTerminator of
        Nothing -> [failure blockPath LoweredMissingTerminator LoweredNoValidationDetail]
        Just _ -> []
    terminatorFailures =
      case maybeTerminator of
        Nothing -> []
        Just terminator -> validateTerminator functionContext blockId blockParameters (Set.fromList (map instructionTemporaryId instructions)) terminator

zipWithInstructionPaths :: LoweredFunctionId -> LoweredBlockId -> [LoweredInstruction] -> [LoweredIRValidationFailure] -> [LoweredIRValidationFailure]
zipWithInstructionPaths functionId blockId instructions failures =
  zipWith replacePath duplicateIndices failures
  where
    duplicateIndices = duplicateInstructionIndices instructions
    replacePath instructionIndex (LoweredIRValidationFailure _ kind detail) =
      LoweredIRValidationFailure (LoweredInstructionPath functionId blockId instructionIndex) kind detail

duplicateInstructionIndices :: [LoweredInstruction] -> [Int]
duplicateInstructionIndices instructions = snd (foldl' step (Set.empty, []) (zip [0 ..] instructions))
  where
    step (seen, indices) (instructionIndex, LoweredInstruction temporaryId _ _)
      | Set.member temporaryId seen = (seen, indices <> [instructionIndex])
      | otherwise = (Set.insert temporaryId seen, indices)

validateInstruction :: FunctionContext -> LoweredBlockId -> Map LoweredParameterId LoweredRepresentation -> (Set LoweredTemporaryId, [LoweredIRValidationFailure]) -> (Int, LoweredInstruction) -> (Set LoweredTemporaryId, [LoweredIRValidationFailure])
validateInstruction functionContext blockId blockParameters (seenTemporaries, accumulatedFailures) (instructionIndex, LoweredInstruction temporaryId resultRepresentation operation) =
  (Set.insert temporaryId seenTemporaries, accumulatedFailures <> operationFailures <> resultFailures)
  where
    functionId = currentFunctionId functionContext
    path = LoweredInstructionPath functionId blockId instructionIndex
    (operationFailures, maybeExpectedRepresentation) =
      validateOperation functionContext blockId blockParameters seenTemporaries path operation
    resultFailures =
      validateRepresentation (functionContextProgram functionContext) path resultRepresentation
        <> case maybeExpectedRepresentation of
          Just expectedRepresentation
            | expectedRepresentation /= resultRepresentation ->
                [ failure
                    path
                    LoweredInstructionResultRepresentationMismatch
                    (LoweredRepresentationDetail expectedRepresentation resultRepresentation)
                ]
          _ -> []

validateOperation :: FunctionContext -> LoweredBlockId -> Map LoweredParameterId LoweredRepresentation -> Set LoweredTemporaryId -> LoweredIRValidationPath -> LoweredOperation -> ([LoweredIRValidationFailure], Maybe LoweredRepresentation)
validateOperation functionContext blockId blockParameters seenTemporaries path operation =
  case operation of
    LoweredPrimitiveOperation primitive operands ->
      (operandFailures operands <> primitiveFailures path primitive operands, primitiveResult primitive operands)
    LoweredConstructProduct layoutId operands ->
      case Map.lookup layoutId layouts of
        Just (LoweredProductLayout fields) -> constructResult layoutId fields operands
        Just (LoweredClosureEnvironmentLayout fields) -> constructResult layoutId fields operands
        Just _ -> (operandFailures operands <> [failure path LoweredInvalidFieldProjection LoweredNoValidationDetail], Nothing)
        Nothing -> (operandFailures operands <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredConstructVariant layoutId tag operands ->
      case Map.lookup layoutId layouts of
        Just (LoweredVariantLayouts variants) ->
          case lookupVariant tag variants of
            Just fields -> constructResult layoutId fields operands
            Nothing -> (operandFailures operands <> [failure path LoweredInvalidTagProjection (LoweredTagDetail tag)], Nothing)
        Just _ -> (operandFailures operands <> [failure path LoweredInvalidTagProjection LoweredNoValidationDetail], Nothing)
        Nothing -> (operandFailures operands <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredConstructList layoutId operands ->
      case Map.lookup layoutId layouts of
        Just (LoweredListLayout elementRepresentation) -> constructResult layoutId (replicate (length operands) elementRepresentation) operands
        Just _ -> (operandFailures operands <> [failure path LoweredInvalidFieldProjection LoweredNoValidationDetail], Nothing)
        Nothing -> (operandFailures operands <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredConstructText layoutId _ ->
      case Map.lookup layoutId layouts of
        Just LoweredTextLayout -> ([], Just (LoweredManagedReferenceRepresentation layoutId))
        Just _ -> ([failure path LoweredInvalidFieldProjection LoweredNoValidationDetail], Nothing)
        Nothing -> ([failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredConstructClosure functionId environment ->
      case Map.lookup functionId functions of
        Nothing -> (operandFailures [environment] <> [failure path LoweredUnknownFunction (identifierDetail (functionIdText functionId))], Nothing)
        Just targetFunction@(LoweredFunction _ maybeEnvironment _ _ _ _) ->
          let environmentFailures = operandFailures [environment]
              expectedEnvironment = fmap parameterRepresentation maybeEnvironment
              actualEnvironment = loweredOperandRepresentation environment
              environmentLayoutFailures = maybe [] (validateClosureEnvironmentParameter programContext path) maybeEnvironment
              closureFailures =
                case expectedEnvironment of
                  Just expected
                    | expected /= actualEnvironment ->
                        [failure path LoweredClosureEnvironmentMismatch (LoweredRepresentationDetail expected actualEnvironment)]
                  Nothing -> [failure path LoweredClosureEnvironmentMismatch LoweredNoValidationDetail]
                  _ -> []
           in (environmentFailures <> environmentLayoutFailures <> closureFailures, Just (LoweredClosureRepresentation (loweredFunctionCallSignature targetFunction)))
    LoweredProjectField layoutId fieldIndex operand ->
      case Map.lookup layoutId layouts of
        Just (LoweredProductLayout fields) -> projectField fields
        Just (LoweredClosureEnvironmentLayout fields) -> projectField fields
        Just _ -> (operandFailures [operand] <> [failure path LoweredInvalidFieldProjection (LoweredIndexDetail fieldIndex)], Nothing)
        Nothing -> (operandFailures [operand] <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
      where
        projectField fields =
          case indexMaybe fields fieldIndex of
            Just representation -> (operandFailures [operand] <> managedOperandFailures layoutId operand, Just representation)
            Nothing -> (operandFailures [operand] <> [failure path LoweredInvalidFieldProjection (LoweredIndexDetail fieldIndex)], Nothing)
    LoweredProjectVariantTag layoutId operand ->
      case Map.lookup layoutId layouts of
        Just (LoweredVariantLayouts _) ->
          (operandFailures [operand] <> managedOperandFailures layoutId operand, Just (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64))
        Just _ -> (operandFailures [operand] <> [failure path LoweredInvalidTagProjection LoweredNoValidationDetail], Nothing)
        Nothing -> (operandFailures [operand] <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredProjectVariantField layoutId tag fieldIndex operand ->
      case Map.lookup layoutId layouts of
        Just (LoweredVariantLayouts variants) ->
          case lookupVariant tag variants of
            Nothing -> (operandFailures [operand] <> [failure path LoweredInvalidTagProjection (LoweredTagDetail tag)], Nothing)
            Just fields ->
              case indexMaybe fields fieldIndex of
                Just representation -> (operandFailures [operand] <> managedOperandFailures layoutId operand, Just representation)
                Nothing -> (operandFailures [operand] <> [failure path LoweredInvalidFieldProjection (LoweredIndexDetail fieldIndex)], Nothing)
        Just _ -> (operandFailures [operand] <> [failure path LoweredInvalidTagProjection (LoweredTagDetail tag)], Nothing)
        Nothing -> (operandFailures [operand] <> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))], Nothing)
    LoweredDirectCall functionId operands ->
      case Map.lookup functionId functions of
        Nothing -> (operandFailures operands <> [failure path LoweredUnknownFunction (identifierDetail (functionIdText functionId))], Nothing)
        Just targetFunction ->
          let LoweredCallSignature _ resultRepresentation = loweredFunctionCallSignature targetFunction
           in (operandFailures operands <> directCallMismatchFailure path LoweredDirectCallSignatureMismatch targetFunction operands, Just resultRepresentation)
    LoweredClosureCall functionOperand operands ->
      case loweredOperandRepresentation functionOperand of
        LoweredClosureRepresentation signature@(LoweredCallSignature _ resultRepresentation) ->
          (operandFailures (functionOperand : operands) <> callMismatchFailure path LoweredClosureCallSignatureMismatch signature operands, Just resultRepresentation)
        actualRepresentation ->
          ( operandFailures (functionOperand : operands)
              <> [failure path LoweredClosureCallSignatureMismatch (LoweredRepresentationDetail (LoweredClosureRepresentation (LoweredCallSignature [] LoweredUnitRepresentation)) actualRepresentation)],
            Nothing
          )
    LoweredRuntimeCall serviceId operands ->
      case Map.lookup serviceId services of
        Nothing -> (operandFailures operands <> [failure path LoweredUnknownRuntimeService (identifierDetail (runtimeServiceIdText serviceId))], Nothing)
        Just signature@(LoweredCallSignature _ resultRepresentation) ->
          (operandFailures operands <> callMismatchFailure path LoweredRuntimeCallSignatureMismatch signature operands, Just resultRepresentation)
  where
    programContext = functionContextProgram functionContext
    layouts = contextLayouts programContext
    services = contextServices programContext
    functions = contextFunctions programContext
    operandFailures = concatMap (validateOperand functionContext blockId blockParameters seenTemporaries path)
    constructResult layoutId expectedRepresentations operands =
      ( operandFailures operands <> representationListFailures path LoweredInstructionResultRepresentationMismatch expectedRepresentations (map loweredOperandRepresentation operands),
        Just (LoweredManagedReferenceRepresentation layoutId)
      )
    managedOperandFailures layoutId operand
      | loweredOperandRepresentation operand == LoweredManagedReferenceRepresentation layoutId = []
      | otherwise =
          [ failure
              path
              LoweredInstructionResultRepresentationMismatch
              (LoweredRepresentationDetail (LoweredManagedReferenceRepresentation layoutId) (loweredOperandRepresentation operand))
          ]

validateOperand :: FunctionContext -> LoweredBlockId -> Map LoweredParameterId LoweredRepresentation -> Set LoweredTemporaryId -> LoweredIRValidationPath -> LoweredOperand -> [LoweredIRValidationFailure]
validateOperand functionContext blockId blockParameters seenTemporaries path operand =
  case operand of
    LoweredFunctionParameterOperand parameterId representation ->
      validateParameterReference (functionContextParameters functionContext) parameterId representation
    LoweredBlockParameterOperand parameterId representation ->
      validateParameterReference blockParameters parameterId representation
    LoweredTemporaryOperand temporaryId representation ->
      case Map.lookup temporaryId (functionContextTemporaryOwners functionContext) of
        Just ownerBlocks
          | Set.member blockId ownerBlocks,
            Set.notMember temporaryId seenTemporaries ->
              [failure path LoweredUseBeforeDefinition (identifierDetail (temporaryIdText temporaryId))]
          | Set.member blockId ownerBlocks -> validateTemporaryRepresentation temporaryId representation
          | otherwise -> [failure path LoweredCrossBlockTemporary (identifierDetail (temporaryIdText temporaryId))]
        Nothing -> [failure path LoweredUseBeforeDefinition (identifierDetail (temporaryIdText temporaryId))]
    LoweredImmediateOperand immediate -> validateImmediate path immediate
  where
    validateParameterReference available parameterId representation =
      case Map.lookup parameterId available of
        Nothing -> [failure path LoweredUnknownParameter (identifierDetail (parameterIdText parameterId))]
        Just expected
          | expected /= representation -> [failure path LoweredEdgeRepresentationMismatch (LoweredRepresentationDetail expected representation)]
          | otherwise -> []
    validateTemporaryRepresentation temporaryId representation =
      case lookupTemporaryRepresentation functionContext blockId temporaryId of
        Just expected
          | expected /= representation -> [failure path LoweredInstructionResultRepresentationMismatch (LoweredRepresentationDetail expected representation)]
        _ -> []

validateImmediate :: LoweredIRValidationPath -> LoweredImmediate -> [LoweredIRValidationFailure]
validateImmediate path immediate =
  case immediate of
    LoweredSignedIntegerImmediate width value ->
      let (minimumValue, maximumValue) = signedIntegerBounds width
       in integerRangeFailures (LoweredSignedIntegerRepresentation width) minimumValue maximumValue value
    LoweredUnsignedIntegerImmediate width value ->
      integerRangeFailures (LoweredUnsignedIntegerRepresentation width) 0 (unsignedIntegerMaximum width) value
    LoweredCharImmediate value
      | not (unicodeScalar value) ->
          [ failure
              path
              LoweredImmediateOutOfRange
              (LoweredImmediateRangeDetail LoweredCharRepresentation)
          ]
    _ -> []
  where
    integerRangeFailures representation minimumValue maximumValue actualValue
      | actualValue < minimumValue || actualValue > maximumValue =
          [ failure
              path
              LoweredImmediateOutOfRange
              (LoweredImmediateRangeDetail representation)
          ]
      | otherwise = []

unicodeScalar :: Char -> Bool
unicodeScalar value =
  let scalar = ord value
   in scalar < 0xD800 || scalar > 0xDFFF

signedIntegerBounds :: LoweredIntegerWidth -> (Integer, Integer)
signedIntegerBounds width =
  let magnitude = 2 ^ (integerWidthBits width - 1)
   in (negate magnitude, magnitude - 1)

unsignedIntegerMaximum :: LoweredIntegerWidth -> Integer
unsignedIntegerMaximum width =
  2 ^ integerWidthBits width - 1

integerWidthBits :: LoweredIntegerWidth -> Int
integerWidthBits width =
  case width of
    LoweredIntegerWidth8 -> 8
    LoweredIntegerWidth16 -> 16
    LoweredIntegerWidth32 -> 32
    LoweredIntegerWidth64 -> 64

validateTerminator :: FunctionContext -> LoweredBlockId -> Map LoweredParameterId LoweredRepresentation -> Set LoweredTemporaryId -> LoweredTerminator -> [LoweredIRValidationFailure]
validateTerminator functionContext blockId blockParameters seenTemporaries terminator =
  case terminator of
    LoweredReturn operand ->
      operandFailures [operand]
        <> representationMismatchFailure path LoweredReturnRepresentationMismatch resultRepresentation (loweredOperandRepresentation operand)
    LoweredJump targetBlock operands ->
      operandFailures operands <> validateEdge functionContext path targetBlock operands
    LoweredBranch condition thenBlock thenOperands elseBlock elseOperands ->
      operandFailures (condition : thenOperands <> elseOperands)
        <> representationMismatchFailure path LoweredBranchConditionMismatch LoweredBoolRepresentation (loweredOperandRepresentation condition)
        <> validateEdge functionContext path thenBlock thenOperands
        <> validateEdge functionContext path elseBlock elseOperands
    LoweredSwitch operand cases maybeDefault ->
      operandFailures (operand : concatMap switchCaseOperands cases <> maybe [] switchDefaultOperands maybeDefault)
        <> concatMap (validateTag path . switchCaseTag) cases
        <> switchShapeFailures functionContext path operand cases maybeDefault
        <> duplicateSwitchTagFailures path cases
        <> concatMap (validateSwitchCase functionContext path) cases
        <> maybe [] (validateSwitchDefault functionContext path) maybeDefault
    LoweredDirectTailCall targetFunction operands ->
      operandFailures operands <> validateDirectTailCall functionContext path resultRepresentation targetFunction operands
    LoweredClosureTailCall functionOperand operands ->
      operandFailures (functionOperand : operands) <> validateClosureTailCall path resultRepresentation functionOperand operands
  where
    functionId = currentFunctionId functionContext
    path = LoweredTerminatorPath functionId blockId
    LoweredFunction _ _ _ resultRepresentation _ _ = functionContextFunction functionContext
    operandFailures = concatMap (validateOperand functionContext blockId blockParameters seenTemporaries path)
    switchCaseTag (LoweredSwitchCase tag _ _) = tag

validateEdge :: FunctionContext -> LoweredIRValidationPath -> LoweredBlockId -> [LoweredOperand] -> [LoweredIRValidationFailure]
validateEdge functionContext path targetBlock operands =
  case Map.lookup targetBlock (functionContextBlocks functionContext) of
    Nothing -> [failure path LoweredUnknownBlock (identifierDetail (blockIdText targetBlock))]
    Just _
      | targetBlock == entryBlock ->
          [failure path LoweredEntryBlockIncomingEdge (identifierDetail (blockIdText targetBlock))]
    Just (LoweredBlock _ parameters _ _) ->
      representationListFailures
        path
        LoweredEdgeRepresentationMismatch
        [representation | LoweredParameter _ representation <- parameters]
        (map loweredOperandRepresentation operands)
  where
    LoweredFunction _ _ _ _ _ entryBlock = functionContextFunction functionContext

validateSwitchCase :: FunctionContext -> LoweredIRValidationPath -> LoweredSwitchCase -> [LoweredIRValidationFailure]
validateSwitchCase functionContext path (LoweredSwitchCase _ targetBlock operands) = validateEdge functionContext path targetBlock operands

validateSwitchDefault :: FunctionContext -> LoweredIRValidationPath -> LoweredSwitchDefault -> [LoweredIRValidationFailure]
validateSwitchDefault functionContext path (LoweredSwitchDefault targetBlock operands) = validateEdge functionContext path targetBlock operands

duplicateSwitchTagFailures :: LoweredIRValidationPath -> [LoweredSwitchCase] -> [LoweredIRValidationFailure]
duplicateSwitchTagFailures path cases = snd (foldl' step (Set.empty, []) cases)
  where
    step (seen, failures) (LoweredSwitchCase tag _ _)
      | Set.member tag seen = (seen, failures <> [failure path LoweredDuplicateSwitchCaseTag (LoweredTagDetail tag)])
      | otherwise = (Set.insert tag seen, failures)

validateDirectTailCall :: FunctionContext -> LoweredIRValidationPath -> LoweredRepresentation -> LoweredFunctionId -> [LoweredOperand] -> [LoweredIRValidationFailure]
validateDirectTailCall functionContext path resultRepresentation targetFunction operands =
  case Map.lookup targetFunction (contextFunctions (functionContextProgram functionContext)) of
    Nothing -> [failure path LoweredUnknownFunction (identifierDetail (functionIdText targetFunction))]
    Just function ->
      let LoweredCallSignature _ targetResult = loweredFunctionCallSignature function
       in directCallMismatchFailure path LoweredDirectTailCallSignatureMismatch function operands
            <> representationMismatchFailure path LoweredDirectTailCallSignatureMismatch resultRepresentation targetResult

validateClosureTailCall :: LoweredIRValidationPath -> LoweredRepresentation -> LoweredOperand -> [LoweredOperand] -> [LoweredIRValidationFailure]
validateClosureTailCall path resultRepresentation functionOperand operands =
  case loweredOperandRepresentation functionOperand of
    LoweredClosureRepresentation signature@(LoweredCallSignature _ targetResult) ->
      callMismatchFailure path LoweredClosureTailCallSignatureMismatch signature operands
        <> representationMismatchFailure path LoweredClosureTailCallSignatureMismatch resultRepresentation targetResult
    actualRepresentation ->
      [ failure
          path
          LoweredClosureTailCallSignatureMismatch
          (LoweredRepresentationDetail (LoweredClosureRepresentation (LoweredCallSignature [] resultRepresentation)) actualRepresentation)
      ]

validateRepresentation :: ProgramContext -> LoweredIRValidationPath -> LoweredRepresentation -> [LoweredIRValidationFailure]
validateRepresentation programContext path representation =
  case representation of
    LoweredManagedReferenceRepresentation layoutId
      | Map.notMember layoutId (contextLayouts programContext) ->
          [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))]
    LoweredClosureRepresentation signature -> validateCallSignatureRepresentations programContext path signature
    _ -> []

validateCallSignatureRepresentations :: ProgramContext -> LoweredIRValidationPath -> LoweredCallSignature -> [LoweredIRValidationFailure]
validateCallSignatureRepresentations programContext path (LoweredCallSignature parameters resultRepresentation) =
  concatMap (validateRepresentation programContext path) (resultRepresentation : parameters)

primitiveResult :: LoweredPrimitive -> [LoweredOperand] -> Maybe LoweredRepresentation
primitiveResult primitive operands =
  case primitive of
    LoweredArithmeticPrimitive _ -> firstOperandRepresentation operands
    LoweredComparisonPrimitive _ -> Just LoweredBoolRepresentation
    LoweredBooleanPrimitive _ -> Just LoweredBoolRepresentation

primitiveFailures :: LoweredIRValidationPath -> LoweredPrimitive -> [LoweredOperand] -> [LoweredIRValidationFailure]
primitiveFailures path primitive operands =
  case primitive of
    LoweredArithmeticPrimitive operation ->
      binaryHomogeneousFailures
        <> case operands of
          [operand, _]
            | arithmeticRepresentationAllowed operation (loweredOperandRepresentation operand) -> []
            | otherwise -> [failure path LoweredPrimitiveSignatureMismatch LoweredNoValidationDetail]
          _ -> []
    LoweredComparisonPrimitive operation ->
      binaryHomogeneousFailures
        <> case operands of
          [operand, _]
            | comparisonRepresentationAllowed operation (loweredOperandRepresentation operand) -> []
            | otherwise -> [failure path LoweredPrimitiveSignatureMismatch LoweredNoValidationDetail]
          _ -> []
    LoweredBooleanPrimitive operation ->
      representationListFailures
        path
        LoweredPrimitiveSignatureMismatch
        (replicate (booleanPrimitiveArity operation) LoweredBoolRepresentation)
        operandRepresentations
  where
    operandRepresentations = map loweredOperandRepresentation operands
    binaryHomogeneousFailures =
      case operands of
        [left, right] ->
          representationMismatchFailure
            path
            LoweredPrimitiveSignatureMismatch
            (loweredOperandRepresentation left)
            (loweredOperandRepresentation right)
        _ -> [failure path LoweredPrimitiveSignatureMismatch (LoweredArityDetail 2 (length operands))]

arithmeticRepresentationAllowed :: LoweredArithmeticPrimitive -> LoweredRepresentation -> Bool
arithmeticRepresentationAllowed _ = isNumericRepresentation

comparisonRepresentationAllowed :: LoweredComparisonPrimitive -> LoweredRepresentation -> Bool
comparisonRepresentationAllowed operation representation =
  case operation of
    LoweredEqual -> True
    LoweredNotEqual -> True
    _ -> isNumericRepresentation representation

booleanPrimitiveArity :: LoweredBooleanPrimitive -> Int
booleanPrimitiveArity operation =
  case operation of
    LoweredBooleanNot -> 1
    LoweredBooleanAnd -> 2
    LoweredBooleanOr -> 2

isIntegralRepresentation :: LoweredRepresentation -> Bool
isIntegralRepresentation representation =
  case representation of
    LoweredSignedIntegerRepresentation _ -> True
    LoweredUnsignedIntegerRepresentation _ -> True
    _ -> False

isNumericRepresentation :: LoweredRepresentation -> Bool
isNumericRepresentation representation =
  isIntegralRepresentation representation
    || case representation of
      LoweredFloatRepresentation _ -> True
      _ -> False

directCallMismatchFailure :: LoweredIRValidationPath -> LoweredIRValidationKind -> LoweredFunction -> [LoweredOperand] -> [LoweredIRValidationFailure]
directCallMismatchFailure path kind function@(LoweredFunction _ maybeEnvironment _ _ _ _) operands =
  case maybeEnvironment of
    Just _ -> [failure path kind LoweredNoValidationDetail]
    Nothing -> callMismatchFailure path kind (loweredFunctionCallSignature function) operands

duplicateParameterFailures :: LoweredIRValidationPath -> [LoweredParameter] -> [LoweredIRValidationFailure]
duplicateParameterFailures path =
  duplicateFailures
    (\(LoweredParameter parameterId _) -> parameterId)
    (const path)
    LoweredDuplicateParameter
    parameterIdText

validateClosureEnvironmentParameter :: ProgramContext -> LoweredIRValidationPath -> LoweredParameter -> [LoweredIRValidationFailure]
validateClosureEnvironmentParameter programContext path (LoweredParameter _ representation) =
  case representation of
    LoweredManagedReferenceRepresentation layoutId ->
      case Map.lookup layoutId (contextLayouts programContext) of
        Just (LoweredClosureEnvironmentLayout _) -> []
        _ -> [failure path LoweredClosureEnvironmentMismatch (identifierDetail (layoutIdText layoutId))]
    _ -> [failure path LoweredClosureEnvironmentMismatch LoweredNoValidationDetail]

switchShapeFailures :: FunctionContext -> LoweredIRValidationPath -> LoweredOperand -> [LoweredSwitchCase] -> Maybe LoweredSwitchDefault -> [LoweredIRValidationFailure]
switchShapeFailures functionContext path operand cases maybeDefault =
  case loweredOperandRepresentation operand of
    LoweredManagedReferenceRepresentation layoutId ->
      case Map.lookup layoutId (contextLayouts (functionContextProgram functionContext)) of
        Nothing -> [failure path LoweredUnknownLayout (identifierDetail (layoutIdText layoutId))]
        Just (LoweredVariantLayouts variants) ->
          invalidSwitchTagFailures variants <> switchCoverageFailures variants
        Just _ -> [failure path LoweredInvalidTagProjection LoweredNoValidationDetail]
    _ -> [failure path LoweredInvalidTagProjection LoweredNoValidationDetail]
  where
    invalidSwitchTagFailures variants =
      [ failure path LoweredInvalidTagProjection (LoweredTagDetail tag)
        | LoweredSwitchCase tag _ _ <- cases,
          tagWithinSharedCarrier tag,
          lookupVariant tag variants == Nothing
      ]
    switchCoverageFailures variants =
      case maybeDefault of
        Just _ -> []
        Nothing -> snd (foldl' collectMissingTag (Set.empty, []) variants)
      where
        caseTags = Set.fromList [tag | LoweredSwitchCase tag _ _ <- cases]
        collectMissingTag (seen, failures) (LoweredVariantLayout tag _)
          | Set.member tag seen = (seen, failures)
          | not (tagWithinSharedCarrier tag) = (Set.insert tag seen, failures)
          | Set.member tag caseTags = (Set.insert tag seen, failures)
          | otherwise =
              ( Set.insert tag seen,
                failures <> [failure path LoweredMissingSwitchCaseTag (LoweredTagDetail tag)]
              )

validateTag :: LoweredIRValidationPath -> Integer -> [LoweredIRValidationFailure]
validateTag path tag
  | tagWithinSharedCarrier tag = []
  | otherwise = [failure path LoweredTagOutOfRange (LoweredTagDetail tag)]

tagWithinSharedCarrier :: Integer -> Bool
tagWithinSharedCarrier tag = tag >= 0 && tag <= 9223372036854775807

callMismatchFailure :: LoweredIRValidationPath -> LoweredIRValidationKind -> LoweredCallSignature -> [LoweredOperand] -> [LoweredIRValidationFailure]
callMismatchFailure path kind (LoweredCallSignature expectedRepresentations _) operands =
  representationListFailures path kind expectedRepresentations (map loweredOperandRepresentation operands)

representationListFailures :: LoweredIRValidationPath -> LoweredIRValidationKind -> [LoweredRepresentation] -> [LoweredRepresentation] -> [LoweredIRValidationFailure]
representationListFailures path kind expected actual
  | length expected /= length actual = [failure path (arityKind kind) (LoweredArityDetail (length expected) (length actual))]
  | otherwise =
      case firstRepresentationMismatch expected actual of
        Just (expectedRepresentation, actualRepresentation) ->
          [failure path kind (LoweredRepresentationDetail expectedRepresentation actualRepresentation)]
        Nothing -> []
  where
    arityKind candidateKind
      | candidateKind == LoweredEdgeRepresentationMismatch = LoweredEdgeArityMismatch
      | otherwise = candidateKind

representationMismatchFailure :: LoweredIRValidationPath -> LoweredIRValidationKind -> LoweredRepresentation -> LoweredRepresentation -> [LoweredIRValidationFailure]
representationMismatchFailure path kind expected actual
  | expected == actual = []
  | otherwise = [failure path kind (LoweredRepresentationDetail expected actual)]

firstRepresentationMismatch :: [LoweredRepresentation] -> [LoweredRepresentation] -> Maybe (LoweredRepresentation, LoweredRepresentation)
firstRepresentationMismatch expected actual =
  case [(expectedRepresentation, actualRepresentation) | (expectedRepresentation, actualRepresentation) <- zip expected actual, expectedRepresentation /= actualRepresentation] of
    mismatch : _ -> Just mismatch
    [] -> Nothing

lookupVariant :: Integer -> [LoweredVariantLayout] -> Maybe [LoweredRepresentation]
lookupVariant tag variants =
  case [fields | LoweredVariantLayout candidateTag fields <- variants, candidateTag == tag] of
    fields : _ -> Just fields
    [] -> Nothing

indexMaybe :: [value] -> Int -> Maybe value
indexMaybe values index
  | index < 0 = Nothing
  | otherwise = go values index
  where
    go [] _ = Nothing
    go (value : _) 0 = Just value
    go (_ : rest) remaining = go rest (remaining - 1)

lookupTemporaryRepresentation :: FunctionContext -> LoweredBlockId -> LoweredTemporaryId -> Maybe LoweredRepresentation
lookupTemporaryRepresentation functionContext blockId temporaryId = do
  LoweredBlock _ _ instructions _ <- Map.lookup blockId (functionContextBlocks functionContext)
  case [representation | LoweredInstruction candidateId representation _ <- instructions, candidateId == temporaryId] of
    representation : _ -> Just representation
    [] -> Nothing

instructionTemporaryId :: LoweredInstruction -> LoweredTemporaryId
instructionTemporaryId (LoweredInstruction temporaryId _ _) = temporaryId

firstOperandRepresentation :: [LoweredOperand] -> Maybe LoweredRepresentation
firstOperandRepresentation operands =
  case operands of
    operand : _ -> Just (loweredOperandRepresentation operand)
    [] -> Nothing

parameterRepresentation :: LoweredParameter -> LoweredRepresentation
parameterRepresentation (LoweredParameter _ representation) = representation

currentFunctionId :: FunctionContext -> LoweredFunctionId
currentFunctionId functionContext =
  case functionContextFunction functionContext of
    LoweredFunction functionId _ _ _ _ _ -> functionId

switchCaseOperands :: LoweredSwitchCase -> [LoweredOperand]
switchCaseOperands (LoweredSwitchCase _ _ operands) = operands

switchDefaultOperands :: LoweredSwitchDefault -> [LoweredOperand]
switchDefaultOperands (LoweredSwitchDefault _ operands) = operands

layoutIdText :: LoweredLayoutId -> Text
layoutIdText (LoweredLayoutId value) = value

runtimeServiceIdText :: LoweredRuntimeServiceId -> Text
runtimeServiceIdText (LoweredRuntimeServiceId value) = value

functionIdText :: LoweredFunctionId -> Text
functionIdText (LoweredFunctionId value) = value

blockIdText :: LoweredBlockId -> Text
blockIdText (LoweredBlockId value) = value

temporaryIdText :: LoweredTemporaryId -> Text
temporaryIdText (LoweredTemporaryId value) = value

parameterIdText :: LoweredParameterId -> Text
parameterIdText (LoweredParameterId value) = value

identifierDetail :: Text -> LoweredIRValidationDetail
identifierDetail = LoweredIdentifierDetail

failure :: LoweredIRValidationPath -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
failure = LoweredIRValidationFailure

maybeToList :: Maybe value -> [value]
maybeToList maybeValue =
  case maybeValue of
    Nothing -> []
    Just value -> [value]

(&) :: value -> (value -> result) -> result
value & function = function value
