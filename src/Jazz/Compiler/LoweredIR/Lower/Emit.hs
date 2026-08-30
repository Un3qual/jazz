{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.LoweredIR.Lower.Emit
  ( emitAnalyzedModule,
  )
where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
  ( constructorApplicationLayout,
    constructorLayoutFor,
    constructorPatternLayoutFor,
    nodeInstantiations,
    orderedManagedLayouts,
    productLayoutFields,
    representationForRecipe,
  )
import Jazz.Compiler.LoweredIR.Lower.Requirements
  ( requiredRuntimeLayouts,
    textEqualityOperation,
    textRuntimeServiceApplication,
  )
import Jazz.Compiler.LoweredIR.Lower.Shapes
import Jazz.Compiler.LoweredIR.Lower.Types
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextEqualService),
    orderedRuntimeServices,
    runtimeServiceContract,
    textLayoutId,
    textRepresentation,
  )
import Jazz.Compiler.TypedCore
import Text.Read (readMaybe)

emitAnalyzedModule :: LoweringAnalysis -> Either [LoweredIRLoweringFailure] LoweredProgram
emitAnalyzedModule analysis =
  case ( traverse (emitFunction modulePath functionIndex) functionShapes,
         emitEntry modulePath functionIndex statements
       ) of
    (Right functions, Right (resultOperand, finalState)) ->
      Right
        ( LoweredProgram
            supportedLoweredIRVersion
            ( requiredRuntimeLayouts runtimeRequirements
                <> orderedManagedLayouts managedLayoutCatalog
                <> orderedClosureLayouts functionShapes
            )
            (orderedRuntimeServices (runtimeRequiredServices runtimeRequirements))
            ( functions
                <> [ LoweredFunction
                       entryFunctionId
                       Nothing
                       []
                       resultRepresentation
                       (finishFunctionBlocks resultOperand finalState)
                       entryBlockId
                   ]
            )
            entryFunctionId
        )
    (Left failures, _) -> Left failures
    (_, Left failures) -> Left failures
  where
    modulePath = analyzedModulePath analysis
    statements = analyzedStatements analysis
    functionShapes = analyzedFunctionShapes analysis
    functionIndex = analyzedFunctionIndex analysis
    resultRepresentation = analyzedResultRepresentation analysis
    runtimeRequirements = analyzedRuntimeRequirements analysis
    managedLayoutCatalog = indexedManagedLayoutCatalog functionIndex
    entryFunctionId =
      LoweredFunctionId (Text.intercalate "::" (modulePath <> ["$entry"]))
    entryBlockId = LoweredBlockId "entry"

finishCurrentBlock :: LoweredTerminator -> LoweringState -> LoweringState
finishCurrentBlock terminator state =
  state
    { loweringInstructions = [],
      loweringCompletedBlocks =
        LoweredBlock
          (loweringCurrentBlockId state)
          (loweringCurrentBlockParameters state)
          (reverse (loweringInstructions state))
          (Just terminator)
          : loweringCompletedBlocks state
    }

startBlock :: LoweredBlockId -> [LoweredParameter] -> LoweringState -> LoweringState
startBlock blockId parameters state =
  state
    { loweringNextTemporary = 1,
      loweringInstructions = [],
      loweringCurrentBlockId = blockId,
      loweringCurrentBlockParameters = parameters
    }

finishFunctionResult :: TypedExpr -> LoweredRepresentation -> LoweredOperand -> LoweringState -> Maybe LoweringState
finishFunctionResult expression expected operand state
  | loweredOperandRepresentation operand /= expected = Nothing
  | LoweredTemporaryOperand temporary representation <- operand,
    LoweredInstruction produced representation' operation : prior <- loweringInstructions state,
    produced == temporary,
    representation' == representation,
    Just terminator <- tailTerminator expression operation =
      Just (finishCurrentBlock terminator state {loweringInstructions = prior})
  | otherwise = Just (finishCurrentBlock (LoweredReturn operand) state)

finishFunctionBlocks :: LoweredOperand -> LoweringState -> [LoweredBlock]
finishFunctionBlocks resultOperand =
  reverse
    . loweringCompletedBlocks
    . finishCurrentBlock (LoweredReturn resultOperand)

tailTerminator :: TypedExpr -> LoweredOperation -> Maybe LoweredTerminator
tailTerminator expression operation =
  case operation of
    LoweredDirectCall functionId operands -> Just (LoweredDirectTailCall functionId operands)
    LoweredClosureCall functionOperand operands
      | completeClosureApplication expression -> Just (LoweredClosureTailCall functionOperand operands)
    _ -> Nothing

completeClosureApplication :: TypedExpr -> Bool
completeClosureApplication expression =
  case expression of
    TypedApplyExpr _ function _ ->
      case typedNodeRecipe (typedExpressionInfo function) of
        TypedClosureRecipe _ result -> not (isClosureRecipe result)
        _ -> False
    _ -> False
  where
    isClosureRecipe recipe =
      case recipe of
        TypedClosureRecipe _ _ -> True
        _ -> False

conditionalBlockId :: [Int] -> [Int] -> Text -> LoweredBlockId
conditionalBlockId statementPath reversedExpressionPath role =
  LoweredBlockId
    ( "if$s"
        <> count statementPath
        <> "$"
        <> indexes statementPath
        <> "$e"
        <> count expressionPath
        <> "$"
        <> indexes expressionPath
        <> "$"
        <> role
    )
  where
    expressionPath = reverse reversedExpressionPath
    count = Text.pack . show . length
    indexes = Text.intercalate "," . map (Text.pack . show)

patternCaseBlockId :: [Int] -> [Int] -> Int -> Text -> LoweredBlockId
patternCaseBlockId statementPath reversedExpressionPath armIndex role =
  LoweredBlockId
    ( patternCaseBlockPrefix statementPath reversedExpressionPath
        <> "$a"
        <> Text.pack (show armIndex)
        <> "$"
        <> role
    )

patternCaseJoinBlockId :: [Int] -> [Int] -> LoweredBlockId
patternCaseJoinBlockId statementPath reversedExpressionPath =
  LoweredBlockId
    (patternCaseBlockPrefix statementPath reversedExpressionPath <> "$join")

patternCaseBlockPrefix :: [Int] -> [Int] -> Text
patternCaseBlockPrefix statementPath reversedExpressionPath =
  "case$s"
    <> count statementPath
    <> "$"
    <> indexes statementPath
    <> "$e"
    <> count expressionPath
    <> "$"
    <> indexes expressionPath
  where
    expressionPath = reverse reversedExpressionPath
    count = Text.pack . show . length
    indexes = Text.intercalate "," . map (Text.pack . show)

ambientSlots :: LoweringState -> [AmbientSlot]
ambientSlots state =
  [ AmbientLocalSlot binder (loweredOperandRepresentation operand)
  | (binder, operand) <- Map.toAscList (loweringLocalBindings state),
    blockLocalOperand operand
  ]
    <> [ AmbientSharedEnvironmentSlot layoutId (loweredOperandRepresentation operand)
       | (layoutId, operand) <- Map.toAscList (loweringSharedEnvironments state),
         blockLocalOperand operand
       ]
    <> [ AmbientCarriedOperandSlot carrier (loweredOperandRepresentation operand)
       | (carrier, operand) <- Map.toAscList (loweringCarriedOperands state),
         blockLocalOperand operand
       ]

ambientParameters :: [AmbientSlot] -> [LoweredParameter]
ambientParameters slots =
  [ LoweredParameter
      (LoweredParameterId ("live" <> Text.pack (show index)))
      (ambientSlotRepresentation slot)
  | (index, slot) <- zip [1 :: Int ..] slots
  ]

ambientArguments :: [AmbientSlot] -> LoweringState -> Maybe [LoweredOperand]
ambientArguments slots state = traverse lookupSlot slots
  where
    lookupSlot slot =
      case slot of
        AmbientLocalSlot binder _ -> Map.lookup binder (loweringLocalBindings state)
        AmbientSharedEnvironmentSlot layoutId _ ->
          Map.lookup layoutId (loweringSharedEnvironments state)
        AmbientCarriedOperandSlot carrier _ ->
          Map.lookup carrier (loweringCarriedOperands state)

remapAmbient :: [AmbientSlot] -> [LoweredParameter] -> LoweringState -> LoweringState
remapAmbient slots parameters state =
  foldl' remapSlot state (zip slots parameters)
  where
    remapSlot currentState (slot, LoweredParameter parameterId representation) =
      let operand = LoweredBlockParameterOperand parameterId representation
       in case slot of
            AmbientLocalSlot binder _ ->
              currentState
                { loweringLocalBindings =
                    Map.insert binder operand (loweringLocalBindings currentState)
                }
            AmbientSharedEnvironmentSlot layoutId _ ->
              currentState
                { loweringSharedEnvironments =
                    Map.insert layoutId operand (loweringSharedEnvironments currentState)
                }
            AmbientCarriedOperandSlot carrier _ ->
              currentState
                { loweringCarriedOperands =
                    Map.insert carrier operand (loweringCarriedOperands currentState)
                }

ambientSlotRepresentation :: AmbientSlot -> LoweredRepresentation
ambientSlotRepresentation slot =
  case slot of
    AmbientLocalSlot _ representation -> representation
    AmbientSharedEnvironmentSlot _ representation -> representation
    AmbientCarriedOperandSlot _ representation -> representation

blockLocalOperand :: LoweredOperand -> Bool
blockLocalOperand operand =
  case operand of
    LoweredTemporaryOperand {} -> True
    LoweredBlockParameterOperand {} -> True
    _ -> False

carryOperand :: LoweredOperand -> LoweringState -> (Int, LoweringState)
carryOperand operand state =
  let carrier = loweringNextCarrier state
   in ( carrier,
        state
          { loweringNextCarrier = carrier + 1,
            loweringCarriedOperands =
              Map.insert carrier operand (loweringCarriedOperands state)
          }
      )

releaseCarriedOperands :: [Int] -> LoweringState -> (Maybe [LoweredOperand], LoweringState)
releaseCarriedOperands carriers state =
  ( traverse (`Map.lookup` loweringCarriedOperands state) carriers,
    state
      { loweringCarriedOperands =
          foldl' (flip Map.delete) (loweringCarriedOperands state) carriers
      }
  )

emitFunction ::
  [Text] ->
  FunctionIndex ->
  FunctionShape ->
  Either [LoweredIRLoweringFailure] LoweredFunction
emitFunction modulePath functions function =
  case lowerFunctionResult
    modulePath
    [functionShapeStatementIndex function]
    (functionShapeReversedBodyPath function)
    functions
    (functionShapeParameters function)
    (functionShapeResultRepresentation function)
    initialState
    (functionShapeBody function) of
    ([], finalState)
      | null (loweringInstructions finalState) ->
          Right
            ( LoweredFunction
                (functionShapeId function)
                (functionEnvironmentParameter function)
                (map functionParameter (functionShapeParameters function))
                (functionShapeResultRepresentation function)
                (reverse (loweringCompletedBlocks finalState))
                (LoweredBlockId "entry")
            )
    (failures@(_ : _), _) -> Left failures
    _ ->
      Left
        [ LoweredIRLoweringFailure
            (TypedStatementPath modulePath [functionShapeStatementIndex function])
            LoweredIRInvalidFunctionShape
            (LoweredIRNameFailureDetail (functionShapeName function))
        ]
  where
    initialState = initializeFunctionState function

lowerFunctionResult ::
  [Text] ->
  [Int] ->
  [Int] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweredRepresentation ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], LoweringState)
lowerFunctionResult modulePath statementPath expressionPath functions parameters expected state expression =
  case expression of
    TypedIfExpr info condition thenExpression elseExpression ->
      discardOperand
        ( lowerConditionalTo
            (FinishFunction expected)
            modulePath
            statementPath
            expressionPath
            path
            info
            condition
            thenExpression
            elseExpression
            functions
            parameters
            state
        )
    TypedPatternCaseExpr info scrutinee arms ->
      discardOperand
        ( lowerScalarPatternCaseTo
            (FinishFunction expected)
            modulePath
            statementPath
            expressionPath
            path
            info
            scrutinee
            arms
            functions
            parameters
            state
        )
    _ -> lowerToDestination (FinishFunction expected)
  where
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    discardOperand (failures, _, finalState) = (failures, finalState)
    lowerToDestination destination =
      case lowerExpression modulePath statementPath expressionPath functions parameters state expression of
        (failures, Just operand, finalState) ->
          case destination of
            ProduceValue -> (failures, finalState)
            FinishFunction resultRepresentation ->
              case finishFunctionResult expression resultRepresentation operand finalState of
                Just finishedState -> (failures, finishedState)
                Nothing -> (failures, finalState)
        (failures, Nothing, finalState) -> (failures, finalState)

initializeFunctionState :: FunctionShape -> LoweringState
initializeFunctionState function =
  case functionShapeEnvironmentLayout function of
    Just layoutId ->
      let environmentOperand =
            LoweredFunctionParameterOperand
              (LoweredParameterId "environment")
              (LoweredManagedReferenceRepresentation layoutId)
          projectedState =
            foldl'
              (projectCapture layoutId)
              emptyState
              (zip [0 ..] (functionShapeCaptures function))
       in projectedState
            { loweringSharedEnvironments =
                Map.singleton layoutId environmentOperand
            }
    Nothing -> emptyState
  where
    emptyState =
      LoweringState
        { loweringNextTemporary = 1,
          loweringNextCarrier = 1,
          loweringInstructions = [],
          loweringCompletedBlocks = [],
          loweringCurrentBlockId = LoweredBlockId "entry",
          loweringCurrentBlockParameters = [],
          loweringLocalBindings = Map.empty,
          loweringSharedEnvironments = Map.empty,
          loweringCarriedOperands = Map.empty
        }
    projectCapture layoutId state (fieldIndex, capture) =
      let temporaryIndex = loweringNextTemporary state
          projectedTemporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
          representation = captureShapeRepresentation capture
          environmentOperand =
            LoweredFunctionParameterOperand
              (LoweredParameterId "environment")
              (LoweredManagedReferenceRepresentation layoutId)
          instruction =
            LoweredInstruction
              projectedTemporaryId
              representation
              (LoweredProjectField layoutId fieldIndex environmentOperand)
          operand = LoweredTemporaryOperand projectedTemporaryId representation
       in state
            { loweringNextTemporary = temporaryIndex + 1,
              loweringInstructions = instruction : loweringInstructions state,
              loweringLocalBindings =
                Map.insert (captureShapeBinder capture) operand (loweringLocalBindings state)
            }

emitEntry ::
  [Text] ->
  FunctionIndex ->
  [TypedStatement] ->
  Either [LoweredIRLoweringFailure] (LoweredOperand, LoweringState)
emitEntry modulePath functions =
  go 0 Nothing initialState
  where
    initialState =
      LoweringState
        { loweringNextTemporary = 1,
          loweringNextCarrier = 1,
          loweringInstructions = [],
          loweringCompletedBlocks = [],
          loweringCurrentBlockId = LoweredBlockId "entry",
          loweringCurrentBlockParameters = [],
          loweringLocalBindings = Map.empty,
          loweringSharedEnvironments = Map.empty,
          loweringCarriedOperands = Map.empty
        }
    go _ (Just resultOperand) state [] = Right (resultOperand, state)
    go _ Nothing _ [] =
      Left
        [ LoweredIRLoweringFailure
            (TypedModulePath modulePath)
            LoweredIRUnsupportedModule
            LoweredIRNoFailureDetail
        ]
    go statementIndex resultOperand state (statement : rest) =
      case statement of
        TypedLetStatement binder _ _ scheme expression
          | Just (schemeBinder, expectedRepresentation) <- valueSchemeContract (indexedManagedLayoutCatalog functions) scheme,
            binder == schemeBinder ->
              case lowerExpression
                modulePath
                [statementIndex]
                [0]
                functions
                []
                state
                expression of
                ([], Just operand, nextState)
                  | loweredOperandRepresentation operand == expectedRepresentation ->
                      go
                        (statementIndex + 1)
                        resultOperand
                        nextState
                          { loweringLocalBindings =
                              Map.insert binder operand (loweringLocalBindings nextState)
                          }
                        rest
                (failures@(_ : _), _, _) -> Left failures
                _ ->
                  Left
                    [ LoweredIRLoweringFailure
                        (TypedExpressionPath modulePath [statementIndex] [0])
                        LoweredIRUnsupportedExpression
                        LoweredIRNoFailureDetail
                    ]
        TypedLetStatement binder _ _ _ _
          | Just function <- Map.lookup statementIndex (indexedFunctionShapesByStatement functions),
            functionShapeCallableShape function == TypedClosureCallableShape,
            Just groupMembers <- Map.lookup binder (indexedRecursiveGroupMembers functions) ->
              case prepareRecursiveEnvironment modulePath statementIndex groupMembers function state of
                Left failures -> Left failures
                Right environmentState ->
                  case lowerClosureValue
                    (TypedExpressionPath modulePath [statementIndex] [0])
                    []
                    function
                    environmentState of
                    ([], Just operand, nextState) ->
                      go
                        (statementIndex + 1)
                        resultOperand
                        nextState
                          { loweringLocalBindings =
                              Map.insert binder operand (loweringLocalBindings nextState)
                          }
                        rest
                    (failures@(_ : _), _, _) -> Left failures
                    _ -> Left [recursiveFailure statementIndex function]
        TypedExpressionStatement _ expression ->
          case lowerExpression
            modulePath
            [statementIndex]
            [0]
            functions
            []
            state
            expression of
            ([], Just operand, nextState) ->
              go (statementIndex + 1) (Just operand) nextState rest
            (failures@(_ : _), _, _) -> Left failures
            _ ->
              Left
                [ LoweredIRLoweringFailure
                    (TypedExpressionPath modulePath [statementIndex] [0])
                    LoweredIRUnsupportedExpression
                    LoweredIRNoFailureDetail
                ]
        _ -> go (statementIndex + 1) resultOperand state rest

    recursiveFailure statementIndex function =
      LoweredIRLoweringFailure
        (TypedStatementPath modulePath [statementIndex])
        LoweredIRRecursiveFunctionUnsupported
        (LoweredIRNameFailureDetail (functionShapeName function))

prepareRecursiveEnvironment ::
  [Text] ->
  Int ->
  [TypedBinderId] ->
  FunctionShape ->
  LoweringState ->
  Either [LoweredIRLoweringFailure] LoweringState
prepareRecursiveEnvironment modulePath statementIndex groupMembers function state =
  case (functionShapeEnvironmentLayout function, groupMembers) of
    (Just layoutId, _)
      | Map.member layoutId (loweringSharedEnvironments state) -> Right state
    (Just layoutId, firstMember : _)
      | functionShapeBinder function == firstMember ->
          case traverse captureOperand (functionShapeCaptures function) of
            Just environmentFields ->
              let temporaryIndex = loweringNextTemporary state
                  temporary = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
                  representation = LoweredManagedReferenceRepresentation layoutId
                  operand = LoweredTemporaryOperand temporary representation
                  instruction =
                    LoweredInstruction
                      temporary
                      representation
                      (LoweredConstructProduct layoutId environmentFields)
               in Right
                    state
                      { loweringNextTemporary = temporaryIndex + 1,
                        loweringInstructions = instruction : loweringInstructions state,
                        loweringSharedEnvironments =
                          Map.insert layoutId operand (loweringSharedEnvironments state)
                      }
            Nothing -> Left [unsupportedFailure]
    _ -> Left [unsupportedFailure]
  where
    captureOperand capture = do
      operand <- Map.lookup (captureShapeBinder capture) (loweringLocalBindings state)
      if loweredOperandRepresentation operand == captureShapeRepresentation capture
        then Just operand
        else Nothing
    unsupportedFailure =
      LoweredIRLoweringFailure
        (TypedStatementPath modulePath [statementIndex])
        LoweredIRRecursiveFunctionUnsupported
        (LoweredIRNameFailureDetail (functionShapeName function))

lowerExpression ::
  [Text] ->
  [Int] ->
  [Int] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerExpression modulePath statementPath expressionPath functions parameters state expression =
  case expression of
    TypedLiteralExpr info literal ->
      lowerLiteral path info literal state
    TypedVariableExpr info _ binderReference ->
      case binderReference >>= (\binder -> constructorLayoutFor managedLayoutCatalog binder (nodeInstantiations info)) of
        Just constructor
          | null (managedConstructorFields constructor) ->
              lowerNullaryManagedVariant path managedLayoutCatalog info constructor state
        Just _ -> unsupportedExpression path state
        Nothing ->
          case binderReference >>= (`Map.lookup` loweringLocalBindings state) of
            Just operand
              | representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just (loweredOperandRepresentation operand) ->
                  ([], Just operand, state)
            Just _ -> unsupportedExpression path state
            Nothing ->
              case findParameterShape binderReference parameters of
                Just (FunctionParameterShape _ (LoweredParameter parameterId representation))
                  | representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just representation ->
                      ([], Just (LoweredFunctionParameterOperand parameterId representation), state)
                _ ->
                  case findFunctionShape binderReference functions of
                    Just function
                      | functionShapeCallableShape function == TypedClosureCallableShape,
                        representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
                          lowerClosureValue path parameters function state
                    _ -> unsupportedExpression path state
    TypedLambdaExpr info parameterBinder _ _ ->
      case findFunctionShape (Just parameterBinder) functions of
        Just function
          | not (functionShapeSourceBinding function),
            representationForRecipe (indexedManagedLayoutCatalog functions) (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
              lowerClosureValue path parameters function state
        _ -> unsupportedExpression path state
    TypedTupleExpr info [] ->
      case typedNodeRecipe info of
        TypedUnitRecipe ->
          ([], Just (LoweredImmediateOperand LoweredUnitImmediate), state)
        recipe -> unsupportedRepresentation path recipe state
    TypedTupleExpr info elements ->
      lowerManagedProduct
        modulePath
        statementPath
        expressionPath
        path
        info
        elements
        functions
        parameters
        state
    TypedIfExpr info condition thenExpression elseExpression ->
      lowerConditional
        modulePath
        statementPath
        expressionPath
        path
        info
        condition
        thenExpression
        elseExpression
        functions
        parameters
        state
    TypedPatternCaseExpr info scrutinee arms ->
      lowerScalarPatternCase
        modulePath
        statementPath
        expressionPath
        path
        info
        scrutinee
        arms
        functions
        parameters
        state
    TypedBinaryExpr info operator left right ->
      case textEqualityOperation expression of
        Just negateResult ->
          lowerTextEquality
            modulePath
            statementPath
            expressionPath
            path
            info
            negateResult
            left
            right
            functions
            parameters
            state
        Nothing ->
          lowerBinary
            modulePath
            statementPath
            expressionPath
            path
            info
            operator
            left
            right
            functions
            parameters
            state
    TypedApplyExpr {} ->
      lowerApplication
        modulePath
        statementPath
        expressionPath
        path
        functions
        parameters
        state
        expression
    _ -> unsupportedExpression path state
  where
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    managedLayoutCatalog = indexedManagedLayoutCatalog functions

lowerManagedProduct ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  [TypedExpr] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerManagedProduct modulePath statementPath expressionPath path info elements functions parameters state =
  case (representationFailures, maybeResultRepresentation, typedNodeRecipe info) of
    (failures@(_ : _), _, _) -> (failures, Nothing, state)
    ( [],
      Just resultRepresentation@(LoweredManagedReferenceRepresentation layoutId),
      TypedManagedProductRecipe fieldRecipes
      )
        | length fieldRecipes == length elements,
          Just fieldRepresentations <- traverse (representationForRecipe managedLayoutCatalog) fieldRecipes,
          productLayoutFields managedLayoutCatalog layoutId == Just fieldRepresentations ->
            case lowerExpressionsLeftToRight
              modulePath
              statementPath
              expressionPath
              functions
              parameters
              state
              elements of
              (failures@(_ : _), _, childState) -> (failures, Nothing, childState)
              ([], Just operands, childState)
                | map loweredOperandRepresentation operands == fieldRepresentations ->
                    let (operand, nextState) =
                          emitManagedConstruction
                            resultRepresentation
                            (LoweredConstructProduct layoutId operands)
                            childState
                     in ([], Just operand, nextState)
              ([], _, childState) -> unsupportedExpression path childState
    _ -> unsupportedExpression path state
  where
    managedLayoutCatalog = indexedManagedLayoutCatalog functions
    (representationFailures, maybeResultRepresentation) =
      representationAtPath managedLayoutCatalog path (typedNodeRecipe info)

lowerNullaryManagedVariant ::
  TypedCoreValidationPath ->
  ManagedLayoutCatalog ->
  TypedNodeInfo ->
  ManagedConstructorLayout ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerNullaryManagedVariant path managedLayoutCatalog info constructor state
  | null (managedConstructorFields constructor),
    representationForRecipe managedLayoutCatalog (typedNodeRecipe info)
      == Just resultRepresentation =
      let (operand, nextState) =
            emitManagedConstruction
              resultRepresentation
              (LoweredConstructVariant layoutId (fromIntegral (managedConstructorTag constructor)) [])
              state
       in ([], Just operand, nextState)
  | otherwise = unsupportedExpression path state
  where
    layoutId = managedConstructorLayoutId constructor
    resultRepresentation = LoweredManagedReferenceRepresentation layoutId

lowerExpressionsLeftToRight ::
  [Text] ->
  [Int] ->
  [Int] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  [TypedExpr] ->
  ([LoweredIRLoweringFailure], Maybe [LoweredOperand], LoweringState)
lowerExpressionsLeftToRight modulePath statementPath expressionPath functions parameters state expressions =
  lowerExpressionsAtPathsLeftToRight
    modulePath
    statementPath
    functions
    parameters
    state
    (zipWith (\index expression -> (index : expressionPath, expression)) [0 ..] expressions)

lowerExpressionsAtPathsLeftToRight ::
  [Text] ->
  [Int] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  [([Int], TypedExpr)] ->
  ([LoweredIRLoweringFailure], Maybe [LoweredOperand], LoweringState)
lowerExpressionsAtPathsLeftToRight modulePath statementPath functions parameters =
  go []
  where
    go reversedCarriers state [] =
      case releaseCarriedOperands (reverse reversedCarriers) state of
        (Just operands, releasedState) -> ([], Just operands, releasedState)
        (Nothing, releasedState) -> ([], Nothing, releasedState)
    go reversedCarriers state ((expressionPath, expression) : rest) =
      case lowerExpression
        modulePath
        statementPath
        expressionPath
        functions
        parameters
        state
        expression of
        ([], Just operand, nextState) ->
          let (carrier, carriedState) = carryOperand operand nextState
           in go (carrier : reversedCarriers) carriedState rest
        (failures@(_ : _), _, nextState) ->
          let (_, releasedState) = releaseCarriedOperands reversedCarriers nextState
           in (failures, Nothing, releasedState)
        ([], Nothing, nextState) ->
          let (_, releasedState) = releaseCarriedOperands reversedCarriers nextState
           in ([], Nothing, releasedState)

emitManagedConstruction ::
  LoweredRepresentation ->
  LoweredOperation ->
  LoweringState ->
  (LoweredOperand, LoweringState)
emitManagedConstruction representation operation state =
  let temporaryIndex = loweringNextTemporary state
      temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
      instruction = LoweredInstruction temporaryId representation operation
      nextState =
        state
          { loweringNextTemporary = temporaryIndex + 1,
            loweringInstructions = instruction : loweringInstructions state
          }
   in (LoweredTemporaryOperand temporaryId representation, nextState)

lowerConditional ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedExpr ->
  TypedExpr ->
  TypedExpr ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerConditional modulePath statementPath expressionPath path info condition thenExpression elseExpression functions parameters state =
  lowerConditionalTo
    ProduceValue
    modulePath
    statementPath
    expressionPath
    path
    info
    condition
    thenExpression
    elseExpression
    functions
    parameters
    state

lowerConditionalTo ::
  ResultDestination ->
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedExpr ->
  TypedExpr ->
  TypedExpr ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerConditionalTo destination modulePath statementPath expressionPath path info condition thenExpression elseExpression functions parameters state =
  case resultRepresentationFailures <> conditionFailures of
    failures@(_ : _) -> (failures, Nothing, conditionState)
    [] ->
      case (maybeResultRepresentation, maybeConditionOperand, ambientArguments slots conditionState) of
        (Just resultRepresentation, Just conditionOperand, Just branchArguments)
          | loweredOperandRepresentation conditionOperand == LoweredBoolRepresentation ->
              case destination of
                ProduceValue -> lowerValueBranches resultRepresentation conditionOperand branchArguments
                FinishFunction expected
                  | expected == resultRepresentation ->
                      lowerFunctionBranches expected conditionOperand branchArguments
                _ -> unsupportedExpression path conditionState
        _ -> unsupportedExpression path conditionState
  where
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe info)
    (conditionFailures, maybeConditionOperand, conditionState) =
      lowerExpression
        modulePath
        statementPath
        (0 : expressionPath)
        functions
        parameters
        state
        condition
    slots = ambientSlots conditionState
    branchParameters = ambientParameters slots
    thenBlockId = conditionalBlockId statementPath expressionPath "then"
    elseBlockId = conditionalBlockId statementPath expressionPath "else"
    joinBlockId = conditionalBlockId statementPath expressionPath "join"

    lowerValueBranches resultRepresentation conditionOperand branchArguments =
      case thenFailures of
        failures@(_ : _) -> (failures, Nothing, thenState)
        [] ->
          case (maybeThenOperand, ambientArguments slots thenState) of
            (Just thenOperand, Just thenAmbientArguments)
              | loweredOperandRepresentation thenOperand == resultRepresentation ->
                  lowerElse resultRepresentation thenAmbientArguments thenOperand
            _ -> unsupportedExpression path thenState
      where
        conditionFinished =
          finishCurrentBlock
            ( LoweredBranch
                conditionOperand
                thenBlockId
                branchArguments
                elseBlockId
                branchArguments
            )
            conditionState
        thenInitial =
          remapAmbient
            slots
            branchParameters
            (startBlock thenBlockId branchParameters conditionFinished)
        (thenFailures, maybeThenOperand, thenState) =
          lowerExpression
            modulePath
            statementPath
            (1 : expressionPath)
            functions
            parameters
            thenInitial
            thenExpression

        lowerElse currentResultRepresentation thenAmbientArguments thenOperand =
          case elseFailures of
            failures@(_ : _) -> (failures, Nothing, elseState)
            [] ->
              case (maybeElseOperand, ambientArguments slots elseState) of
                (Just elseOperand, Just elseAmbientArguments)
                  | loweredOperandRepresentation elseOperand == currentResultRepresentation ->
                      let elseFinished =
                            finishCurrentBlock
                              (LoweredJump joinBlockId (elseAmbientArguments <> [elseOperand]))
                              elseState
                          resultParameter =
                            LoweredParameter
                              (LoweredParameterId "result")
                              currentResultRepresentation
                          joinParameters = branchParameters <> [resultParameter]
                          joinBase =
                            conditionState
                              { loweringCompletedBlocks = loweringCompletedBlocks elseFinished
                              }
                          joinState =
                            remapAmbient
                              slots
                              branchParameters
                              (startBlock joinBlockId joinParameters joinBase)
                          resultOperand =
                            LoweredBlockParameterOperand
                              (LoweredParameterId "result")
                              currentResultRepresentation
                       in ([], Just resultOperand, joinState)
                _ -> unsupportedExpression path elseState
          where
            thenFinished =
              finishCurrentBlock
                (LoweredJump joinBlockId (thenAmbientArguments <> [thenOperand]))
                thenState
            elseBase =
              conditionState
                { loweringCompletedBlocks = loweringCompletedBlocks thenFinished
                }
            elseInitial =
              remapAmbient
                slots
                branchParameters
                (startBlock elseBlockId branchParameters elseBase)
            (elseFailures, maybeElseOperand, elseState) =
              lowerExpression
                modulePath
                statementPath
                (2 : expressionPath)
                functions
                parameters
                elseInitial
                elseExpression

    lowerFunctionBranches expected conditionOperand branchArguments =
      case thenFailures of
        failures@(_ : _) -> (failures, Nothing, thenState)
        [] ->
          case elseFailures of
            failures@(_ : _) -> (failures, Nothing, elseState)
            [] -> ([], Nothing, elseState)
      where
        conditionFinished =
          finishCurrentBlock
            ( LoweredBranch
                conditionOperand
                thenBlockId
                branchArguments
                elseBlockId
                branchArguments
            )
            conditionState
        thenInitial =
          remapAmbient
            slots
            branchParameters
            (startBlock thenBlockId branchParameters conditionFinished)
        (thenFailures, thenState) =
          lowerFunctionResult
            modulePath
            statementPath
            (1 : expressionPath)
            functions
            parameters
            expected
            thenInitial
            thenExpression
        elseBase =
          conditionState
            { loweringCompletedBlocks = loweringCompletedBlocks thenState
            }
        elseInitial =
          remapAmbient
            slots
            branchParameters
            (startBlock elseBlockId branchParameters elseBase)
        (elseFailures, elseState) =
          lowerFunctionResult
            modulePath
            statementPath
            (2 : expressionPath)
            functions
            parameters
            expected
            elseInitial
            elseExpression

lowerScalarPatternCase ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedExpr ->
  [TypedCaseArm] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerScalarPatternCase modulePath statementPath expressionPath path info scrutinee arms functions parameters state =
  lowerScalarPatternCaseTo
    ProduceValue
    modulePath
    statementPath
    expressionPath
    path
    info
    scrutinee
    arms
    functions
    parameters
    state

lowerScalarPatternCaseTo ::
  ResultDestination ->
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedExpr ->
  [TypedCaseArm] ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerScalarPatternCaseTo destination modulePath statementPath expressionPath path info scrutinee arms functions parameters state =
  case resultRepresentationFailures <> scrutineeFailures of
    failures@(_ : _) -> (failures, Nothing, scrutineeState)
    [] ->
      case (maybeResultRepresentation, maybeScrutineeOperand) of
        (Just resultRepresentation, Just scrutineeOperand)
          | Just scrutineeRepresentation <- representationForRecipe (indexedManagedLayoutCatalog functions) (typedNodeRecipe (typedExpressionInfo scrutinee)),
            loweredOperandRepresentation scrutineeOperand == scrutineeRepresentation ->
              case destination of
                ProduceValue -> lowerSelectedArmChain resultRepresentation scrutineeOperand
                FinishFunction expected
                  | expected == resultRepresentation ->
                      lowerSelectedArmChain resultRepresentation scrutineeOperand
                _ -> unsupportedExpression path scrutineeState
        _ -> unsupportedExpression path scrutineeState
  where
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe info)
    (scrutineeFailures, maybeScrutineeOperand, scrutineeState) =
      lowerExpression
        modulePath
        statementPath
        (0 : expressionPath)
        functions
        parameters
        state
        scrutinee
    joinBlockId = patternCaseJoinBlockId statementPath expressionPath

    lowerSelectedArmChain resultRepresentation scrutineeOperand
      | managedPatternCase = lowerManagedArmChain resultRepresentation scrutineeOperand
      | earlyUnguardedCatchAll arms =
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRIncompletePatternCase
                LoweredIRNoFailureDetail
            ],
            Nothing,
            scrutineeState
          )
      | otherwise = lowerArmChain resultRepresentation scrutineeOperand

    managedPatternCase =
      case typedNodeRecipe (typedExpressionInfo scrutinee) of
        TypedManagedProductRecipe {} -> True
        TypedManagedVariantRecipe {} -> True
        _ -> False

    earlyUnguardedCatchAll caseArms =
      case reverse caseArms of
        [] -> False
        _ : reversedPreceding ->
          any unsupportedPrecedingCatchAll reversedPreceding

    unsupportedPrecedingCatchAll (TypedCaseArm patternValue maybeGuard _) =
      case (patternValue, maybeGuard) of
        (TypedWildcardPattern {}, Nothing) -> True
        (TypedVariablePattern {}, Nothing) -> True
        _ -> False

    lowerArmChainWith lowerProfileArms selectedArms resultRepresentation scrutineeOperand =
      let outerSlots = ambientSlots scrutineeState
          outerParameters = ambientParameters outerSlots
          (scrutineeCarrier, carriedState) = carryOperand scrutineeOperand scrutineeState
          controlSlots = ambientSlots carriedState
          controlParameters = ambientParameters controlSlots
       in case lowerProfileArms
            resultRepresentation
            scrutineeCarrier
            outerSlots
            controlSlots
            controlParameters
            0
            selectedArms
            carriedState of
            (failures@(_ : _), finalArmState) -> (failures, Nothing, finalArmState)
            ([], finalArmState) ->
              case destination of
                FinishFunction _ -> ([], Nothing, finalArmState)
                ProduceValue ->
                  let joinBase =
                        scrutineeState
                          { loweringNextCarrier = loweringNextCarrier finalArmState,
                            loweringCompletedBlocks = loweringCompletedBlocks finalArmState
                          }
                      joinParameters =
                        outerParameters
                          <> [LoweredParameter (LoweredParameterId "result") resultRepresentation]
                      joinState =
                        remapAmbient
                          outerSlots
                          outerParameters
                          (startBlock joinBlockId joinParameters joinBase)
                      resultOperand =
                        LoweredBlockParameterOperand
                          (LoweredParameterId "result")
                          resultRepresentation
                   in ([], Just resultOperand, joinState)

    lowerManagedArmChain resultRepresentation scrutineeOperand =
      case patternCaseTotalPrefixLength managedLayoutCatalog statementPath expressionPath scrutinee arms of
        Left failure -> ([failure], Nothing, scrutineeState)
        Right Nothing ->
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRIncompletePatternCase
                LoweredIRNoFailureDetail
            ],
            Nothing,
            scrutineeState
          )
        Right (Just totalPrefixLength) ->
          lowerArmChainWith
            lowerManagedArms
            (take totalPrefixLength arms)
            resultRepresentation
            scrutineeOperand

    lowerManagedArms resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex remainingArms currentState =
      case remainingArms of
        [] ->
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRIncompletePatternCase
                LoweredIRNoFailureDetail
            ],
            currentState
          )
        arm@(TypedCaseArm patternValue _ _) : laterArms ->
          case scrutineeAt scrutineeCarrier currentState of
            Nothing -> ([unsupportedFailure path], currentState)
            Just _ ->
              let canonicalBindings = patternArmBindings managedLayoutCatalog patternValue
                  canonicalShapes = map snd canonicalBindings
                  canonicalParameters = map functionParameter canonicalShapes
                  successBlockId = matchedArmEntry armIndex arm
                  rowFailureBlockId = nextManagedArmEntry armIndex laterArms
                  alternatives = managedPatternAlternatives patternValue
                  (matcherFailures, matcherState) =
                    lowerManagedAlternatives
                      armIndex
                      canonicalBindings
                      successBlockId
                      rowFailureBlockId
                      controlSlots
                      controlParameters
                      currentState
                      scrutineeCarrier
                      alternatives
                      currentState
               in case matcherFailures of
                    failures@(_ : _) -> (failures, matcherState)
                    [] ->
                      let successParameters = controlParameters <> canonicalParameters
                          successBase = continuationState currentState matcherState
                          successInitial =
                            bindManagedPatternParameters
                              canonicalShapes
                              ( remapAmbient
                                  controlSlots
                                  controlParameters
                                  (startBlock successBlockId successParameters successBase)
                              )
                       in lowerManagedMatchedArm
                            resultRepresentation
                            scrutineeCarrier
                            outerSlots
                            controlSlots
                            controlParameters
                            canonicalShapes
                            armIndex
                            arm
                            laterArms
                            currentState
                            successInitial

    lowerManagedAlternatives armIndex canonicalBindings successBlockId rowFailureBlockId controlSlots controlParameters rowTemplate scrutineeCarrier alternatives initialState =
      go alternatives initialState
      where
        go [] currentState = ([unsupportedFailure path], currentState)
        go ((alternativeIndex, patternPath, alternative) : remaining) currentState =
          case scrutineeAt scrutineeCarrier currentState of
            Nothing -> ([unsupportedFailure path], currentState)
            Just scrutineeOperand ->
              let alternativeFailureBlockId =
                    case remaining of
                      (nextAlternativeIndex, _, _) : _ ->
                        Just (managedAlternativeEntry armIndex nextAlternativeIndex)
                      [] -> rowFailureBlockId
                  (alternativeFailures, alternativeFallsThrough, alternativeState) =
                    lowerManagedPatternQueue
                      armIndex
                      alternativeIndex
                      canonicalBindings
                      successBlockId
                      alternativeFailureBlockId
                      controlSlots
                      controlParameters
                      Seq.empty
                      [(alternative, patternPath, scrutineeOperand)]
                      currentState
               in case (alternativeFailures, alternativeFallsThrough, remaining) of
                    (failures@(_ : _), _, _) -> (failures, alternativeState)
                    ([], _, []) -> ([], alternativeState)
                    ([], False, _) -> ([], alternativeState)
                    ([], True, (nextAlternativeIndex, _, _) : _) ->
                      let nextInitial =
                            remapAmbient
                              controlSlots
                              controlParameters
                              ( startBlock
                                  (managedAlternativeEntry armIndex nextAlternativeIndex)
                                  controlParameters
                                  (continuationState rowTemplate alternativeState)
                              )
                       in go remaining nextInitial

    lowerManagedPatternQueue armIndex alternativeIndex canonicalBindings successBlockId failureBlockId controlSlots controlParameters matchedOperands pendingPatterns currentState =
      case pendingPatterns of
        [] -> finishManagedPatternSuccess canonicalBindings successBlockId controlSlots matchedOperands currentState
        (patternValue, patternPath, operand) : rest ->
          case patternValue of
            TypedWildcardPattern patternInfo
              | managedPatternOperandMatches patternInfo operand ->
                  recurse matchedOperands rest currentState
            TypedVariablePattern patternInfo _ name
              | managedPatternOperandMatches patternInfo operand ->
                  recurse (matchedOperands |> (name, operand)) rest currentState
            TypedAsPattern patternInfo _ name nestedPattern
              | managedPatternOperandMatches patternInfo operand ->
                  recurse
                    (matchedOperands |> (name, operand))
                    ((nestedPattern, patternPath <> [0], operand) : rest)
                    currentState
            TypedTuplePattern patternInfo fields
              | managedPatternOperandMatches patternInfo operand,
                LoweredManagedReferenceRepresentation layoutId <- loweredOperandRepresentation operand,
                Just fieldRepresentations <- productLayoutFields managedLayoutCatalog layoutId,
                length fields == length fieldRepresentations ->
                  let (fieldOperands, projectedState) =
                        emitManagedPatternProjections
                          [ ( representation,
                              LoweredProjectField layoutId fieldIndex operand
                            )
                          | (fieldIndex, representation) <- zip [0 :: Int ..] fieldRepresentations
                          ]
                          currentState
                      fieldPatterns =
                        [ (fieldPattern, patternPath <> [fieldIndex], fieldOperand)
                        | (fieldIndex, (fieldPattern, fieldOperand)) <- zip [0 :: Int ..] (zip fields fieldOperands)
                        ]
                   in recurse matchedOperands (fieldPatterns <> rest) projectedState
            TypedConstructorPattern patternInfo constructorName fields
              | managedPatternOperandMatches patternInfo operand,
                Just constructor <- constructorPatternLayoutFor managedLayoutCatalog patternInfo constructorName,
                loweredOperandRepresentation operand
                  == LoweredManagedReferenceRepresentation (managedConstructorLayoutId constructor),
                length fields == length (managedConstructorFields constructor) ->
                  lowerManagedConstructorPattern
                    armIndex
                    alternativeIndex
                    canonicalBindings
                    successBlockId
                    failureBlockId
                    controlSlots
                    controlParameters
                    matchedOperands
                    patternPath
                    operand
                    fields
                    constructor
                    rest
                    currentState
            TypedLiteralPattern patternInfo literal
              | managedPatternOperandMatches patternInfo operand ->
                  lowerManagedLiteralPattern
                    armIndex
                    alternativeIndex
                    canonicalBindings
                    successBlockId
                    failureBlockId
                    controlSlots
                    controlParameters
                    matchedOperands
                    patternPath
                    operand
                    patternInfo
                    literal
                    rest
                    currentState
            _ -> ([managedPatternFailure armIndex patternPath], False, currentState)
      where
        recurse nextMatched nextPending nextState =
          lowerManagedPatternQueue
            armIndex
            alternativeIndex
            canonicalBindings
            successBlockId
            failureBlockId
            controlSlots
            controlParameters
            nextMatched
            nextPending
            nextState

    lowerManagedConstructorPattern armIndex alternativeIndex canonicalBindings successBlockId failureBlockId controlSlots controlParameters matchedOperands patternPath operand fields constructor rest currentState =
      case ambientArguments controlSlots currentState of
        Nothing -> ([unsupportedFailure path], False, currentState)
        Just controlArguments ->
          let matchBlockId = managedPatternBlock armIndex alternativeIndex patternPath "fields"
              queuedOperands = [pendingOperand | (_, _, pendingOperand) <- rest]
              transportedOperands = map snd (toList matchedOperands) <> (operand : queuedOperands)
              matchParameters = managedMatchParameters transportedOperands
              matchArguments = controlArguments <> transportedOperands
              defaultTarget =
                case failureBlockId of
                  Just blockId -> LoweredSwitchDefault blockId controlArguments
                  Nothing -> LoweredSwitchDefault matchBlockId matchArguments
              switchedState =
                finishCurrentBlock
                  ( LoweredSwitch
                      operand
                      [ LoweredSwitchCase
                          (toInteger (managedConstructorTag constructor))
                          matchBlockId
                          matchArguments
                      ]
                      (Just defaultTarget)
                  )
                  currentState
              matchInitial =
                remapAmbient
                  controlSlots
                  controlParameters
                  (startBlock matchBlockId (controlParameters <> matchParameters) switchedState)
              remappedTransported = managedMatchOperands matchParameters
              (remappedMatched, remappedQueued) = splitAt (Seq.length matchedOperands) remappedTransported
              remappedMatchedBindings =
                Seq.zip
                  (fmap fst matchedOperands)
                  (Seq.fromList remappedMatched)
           in case remappedQueued of
                remappedOperand : remappedRestOperands ->
                  let (fieldOperands, projectedState) =
                        emitManagedPatternProjections
                          [ ( representation,
                              LoweredProjectVariantField
                                (managedConstructorLayoutId constructor)
                                (toInteger (managedConstructorTag constructor))
                                fieldIndex
                                remappedOperand
                            )
                          | (fieldIndex, representation) <- zip [0 :: Int ..] (managedConstructorFields constructor)
                          ]
                          matchInitial
                      fieldPatterns =
                        [ (fieldPattern, patternPath <> [fieldIndex], fieldOperand)
                        | (fieldIndex, (fieldPattern, fieldOperand)) <- zip [0 :: Int ..] (zip fields fieldOperands)
                        ]
                      remappedRest =
                        [ (pendingPattern, pendingPath, pendingOperand)
                        | ((pendingPattern, pendingPath, _), pendingOperand) <- zip rest remappedRestOperands
                        ]
                      (nestedFailures, nestedFallsThrough, nestedState) =
                        lowerManagedPatternQueue
                          armIndex
                          alternativeIndex
                          canonicalBindings
                          successBlockId
                          failureBlockId
                          controlSlots
                          controlParameters
                          remappedMatchedBindings
                          (fieldPatterns <> remappedRest)
                          projectedState
                      constructorFallsThrough =
                        case failureBlockId of
                          Just _ -> True
                          Nothing -> nestedFallsThrough
                   in (nestedFailures, constructorFallsThrough, nestedState)
                [] -> ([unsupportedFailure path], False, matchInitial)
    lowerManagedLiteralPattern armIndex alternativeIndex canonicalBindings successBlockId failureBlockId controlSlots controlParameters matchedOperands patternPath operand patternInfo literal rest currentState =
      case lowerLiteral (managedTypedPatternPath armIndex patternPath) patternInfo literal currentState of
        (failures@(_ : _), _, literalState) -> (failures, False, literalState)
        ([], Just literalOperand, literalState)
          | loweredOperandRepresentation literalOperand == loweredOperandRepresentation operand ->
              let comparisonIndex = loweringNextTemporary literalState
                  comparisonTemporary = LoweredTemporaryId ("t" <> Text.pack (show comparisonIndex))
                  comparisonInstruction =
                    LoweredInstruction
                      comparisonTemporary
                      LoweredBoolRepresentation
                      ( LoweredPrimitiveOperation
                          (LoweredComparisonPrimitive LoweredEqual)
                          [operand, literalOperand]
                      )
                  comparisonState =
                    literalState
                      { loweringNextTemporary = comparisonIndex + 1,
                        loweringInstructions = comparisonInstruction : loweringInstructions literalState
                      }
               in lowerManagedLiteralContinuation
                    armIndex
                    alternativeIndex
                    canonicalBindings
                    successBlockId
                    failureBlockId
                    controlSlots
                    controlParameters
                    matchedOperands
                    patternPath
                    rest
                    (LoweredTemporaryOperand comparisonTemporary LoweredBoolRepresentation)
                    comparisonState
        ([], _, literalState) -> ([managedPatternFailure armIndex patternPath], False, literalState)

    lowerManagedLiteralContinuation armIndex alternativeIndex canonicalBindings successBlockId failureBlockId controlSlots controlParameters matchedOperands patternPath rest comparisonOperand comparisonState =
      case (ambientArguments controlSlots comparisonState, failureBlockId) of
        (Just controlArguments, Just failureTarget) ->
          case rest of
            [] ->
              case managedSuccessArguments canonicalBindings controlArguments matchedOperands of
                Just successArguments ->
                  ( [],
                    True,
                    finishCurrentBlock
                      ( LoweredBranch
                          comparisonOperand
                          successBlockId
                          successArguments
                          failureTarget
                          controlArguments
                      )
                      comparisonState
                  )
                Nothing -> ([unsupportedFailure path], False, comparisonState)
            _ ->
              let continuationBlockId = managedPatternBlock armIndex alternativeIndex patternPath "next"
                  queuedOperands = [pendingOperand | (_, _, pendingOperand) <- rest]
                  transportedOperands = map snd (toList matchedOperands) <> queuedOperands
                  matchParameters = managedMatchParameters transportedOperands
                  continuationArguments = controlArguments <> transportedOperands
                  branchState =
                    finishCurrentBlock
                      ( LoweredBranch
                          comparisonOperand
                          continuationBlockId
                          continuationArguments
                          failureTarget
                          controlArguments
                      )
                      comparisonState
                  continuationInitial =
                    remapAmbient
                      controlSlots
                      controlParameters
                      (startBlock continuationBlockId (controlParameters <> matchParameters) branchState)
                  remappedTransported = managedMatchOperands matchParameters
                  (remappedMatched, remappedQueued) = splitAt (Seq.length matchedOperands) remappedTransported
                  remappedMatchedBindings =
                    Seq.zip
                      (fmap fst matchedOperands)
                      (Seq.fromList remappedMatched)
                  remappedRest =
                    [ (pendingPattern, pendingPath, pendingOperand)
                    | ((pendingPattern, pendingPath, _), pendingOperand) <- zip rest remappedQueued
                    ]
                  (continuationFailures, _, continuationState') =
                    lowerManagedPatternQueue
                      armIndex
                      alternativeIndex
                      canonicalBindings
                      successBlockId
                      failureBlockId
                      controlSlots
                      controlParameters
                      remappedMatchedBindings
                      remappedRest
                      continuationInitial
               in (continuationFailures, True, continuationState')
        _ -> ([unsupportedFailure path], False, comparisonState)

    finishManagedPatternSuccess canonicalBindings successBlockId controlSlots matchedOperands currentState =
      case ambientArguments controlSlots currentState of
        Just controlArguments ->
          case managedSuccessArguments canonicalBindings controlArguments matchedOperands of
            Just successArguments ->
              ([], False, finishCurrentBlock (LoweredJump successBlockId successArguments) currentState)
            Nothing -> ([unsupportedFailure path], False, currentState)
        Nothing -> ([unsupportedFailure path], False, currentState)

    managedSuccessArguments canonicalBindings controlArguments matchedOperands = do
      if Seq.length matchedOperands == length canonicalBindings then pure () else Nothing
      let operandsByName = Map.fromList (toList matchedOperands)
      orderedOperands <- traverse (flip Map.lookup operandsByName . fst) canonicalBindings
      let expectedRepresentations =
            [ representation
            | (_, FunctionParameterShape _ (LoweredParameter _ representation)) <- canonicalBindings
            ]
      if map loweredOperandRepresentation orderedOperands == expectedRepresentations
        then Just (controlArguments <> orderedOperands)
        else Nothing

    lowerManagedMatchedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters canonicalShapes armIndex arm@(TypedCaseArm _ maybeGuard _) laterArms continuationTemplate matchedState =
      case maybeGuard of
        Just guard ->
          lowerManagedGuardedArm
            resultRepresentation
            scrutineeCarrier
            outerSlots
            controlSlots
            controlParameters
            canonicalShapes
            armIndex
            arm
            laterArms
            continuationTemplate
            matchedState
            guard
        Nothing ->
          lowerManagedArmBody
            resultRepresentation
            scrutineeCarrier
            outerSlots
            controlSlots
            controlParameters
            canonicalShapes
            armIndex
            arm
            laterArms
            continuationTemplate
            matchedState

    lowerManagedGuardedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters canonicalShapes armIndex arm laterArms continuationTemplate guardState guard =
      let (guardFailures, maybeGuardOperand, loweredGuardState) =
            lowerExpression
              modulePath
              statementPath
              (0 : armIndex + 1 : expressionPath)
              functions
              parameters
              guardState
              guard
       in case (guardFailures, maybeGuardOperand, nextManagedArmEntry armIndex laterArms, ambientArguments controlSlots loweredGuardState, managedPatternArguments canonicalShapes loweredGuardState) of
            ([], Just guardOperand, Just nextBlockId, Just controlArguments, Just patternArguments)
              | loweredOperandRepresentation guardOperand == LoweredBoolRepresentation ->
                  let bodyBlockId = patternCaseBlockId statementPath expressionPath armIndex "body"
                      successParameters = controlParameters <> map functionParameter canonicalShapes
                      bodyArguments = controlArguments <> patternArguments
                      branchState =
                        finishCurrentBlock
                          ( LoweredBranch
                              guardOperand
                              bodyBlockId
                              bodyArguments
                              nextBlockId
                              controlArguments
                          )
                          loweredGuardState
                      bodyInitial =
                        bindManagedPatternParameters
                          canonicalShapes
                          ( remapAmbient
                              controlSlots
                              controlParameters
                              (startBlock bodyBlockId successParameters branchState)
                          )
                   in lowerManagedArmBody
                        resultRepresentation
                        scrutineeCarrier
                        outerSlots
                        controlSlots
                        controlParameters
                        canonicalShapes
                        armIndex
                        arm
                        laterArms
                        continuationTemplate
                        bodyInitial
            (failures@(_ : _), _, _, _, _) -> (failures, loweredGuardState)
            _ -> ([unsupportedFailure path], loweredGuardState)

    lowerManagedArmBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters _ armIndex (TypedCaseArm _ _ body) laterArms continuationTemplate bodyState =
      case destination of
        FinishFunction expected ->
          let (bodyFailures, loweredBodyState) =
                lowerFunctionResult
                  modulePath
                  statementPath
                  (1 : armIndex + 1 : expressionPath)
                  functions
                  parameters
                  expected
                  bodyState
                  body
           in case bodyFailures of
                [] -> continueLaterArms loweredBodyState
                failures@(_ : _) -> (failures, loweredBodyState)
        ProduceValue ->
          let (bodyFailures, maybeBodyOperand, loweredBodyState) =
                lowerExpression
                  modulePath
                  statementPath
                  (1 : armIndex + 1 : expressionPath)
                  functions
                  parameters
                  bodyState
                  body
           in case (bodyFailures, maybeBodyOperand, ambientArguments outerSlots loweredBodyState) of
                ([], Just bodyOperand, Just joinAmbientArguments)
                  | loweredOperandRepresentation bodyOperand == resultRepresentation ->
                      continueLaterArms
                        ( finishCurrentBlock
                            (LoweredJump joinBlockId (joinAmbientArguments <> [bodyOperand]))
                            loweredBodyState
                        )
                (failures@(_ : _), _, _) -> (failures, loweredBodyState)
                _ -> ([unsupportedFailure path], loweredBodyState)
      where
        continueLaterArms bodyFinished =
          case laterArms of
            [] -> ([], bodyFinished)
            _ ->
              case nextManagedArmEntry armIndex laterArms of
                Just nextBlockId ->
                  let nextBase = continuationState continuationTemplate bodyFinished
                      nextInitial =
                        remapAmbient
                          controlSlots
                          controlParameters
                          (startBlock nextBlockId controlParameters nextBase)
                   in lowerManagedArms
                        resultRepresentation
                        scrutineeCarrier
                        outerSlots
                        controlSlots
                        controlParameters
                        (armIndex + 1)
                        laterArms
                        nextInitial
                Nothing -> ([unsupportedFailure path], bodyFinished)

    bindManagedPatternParameters canonicalShapes currentState =
      foldl'
        ( \nextState (FunctionParameterShape binder (LoweredParameter parameterId representation)) ->
            nextState
              { loweringLocalBindings =
                  Map.insert
                    binder
                    (LoweredBlockParameterOperand parameterId representation)
                    (loweringLocalBindings nextState)
              }
        )
        currentState
        canonicalShapes

    managedPatternArguments canonicalShapes currentState =
      traverse
        (\(FunctionParameterShape binder _) -> Map.lookup binder (loweringLocalBindings currentState))
        canonicalShapes

    managedPatternAlternatives patternValue =
      case patternValue of
        TypedOrPattern _ alternatives ->
          [ (alternativeIndex, [alternativeIndex], alternative)
          | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives
          ]
        _ -> [(0, [], patternValue)]

    managedPatternOperandMatches patternInfo operand =
      representationForRecipe managedLayoutCatalog (typedNodeRecipe patternInfo)
        == Just (loweredOperandRepresentation operand)

    managedAlternativeEntry armIndex alternativeIndex =
      patternCaseBlockId
        statementPath
        expressionPath
        armIndex
        ("alt" <> Text.pack (show alternativeIndex) <> "$test")

    managedPatternBlock armIndex alternativeIndex patternPath role =
      patternCaseBlockId
        statementPath
        expressionPath
        armIndex
        ( "alt"
            <> Text.pack (show alternativeIndex)
            <> "$p"
            <> ( if null patternPath
                   then "root"
                   else Text.intercalate "," (map (Text.pack . show) patternPath)
               )
            <> "$"
            <> role
        )

    managedMatchParameters operands =
      [ LoweredParameter
          (LoweredParameterId ("match" <> Text.pack (show parameterIndex)))
          (loweredOperandRepresentation operand)
      | (parameterIndex, operand) <- zip [1 :: Int ..] operands
      ]

    managedMatchOperands matchParameters =
      [LoweredBlockParameterOperand parameterId representation | LoweredParameter parameterId representation <- matchParameters]

    emitManagedPatternOperation representation operation currentState =
      let temporaryIndex = loweringNextTemporary currentState
          temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
          instruction = LoweredInstruction temporaryId representation operation
       in ( LoweredTemporaryOperand temporaryId representation,
            currentState
              { loweringNextTemporary = temporaryIndex + 1,
                loweringInstructions = instruction : loweringInstructions currentState
              }
          )

    emitManagedPatternProjections operations currentState =
      let (reversedOperands, finalState) =
            foldl'
              ( \(operands, nextState) (representation, operation) ->
                  let (operand, projectedState) = emitManagedPatternOperation representation operation nextState
                   in (operand : operands, projectedState)
              )
              ([], currentState)
              operations
       in (reverse reversedOperands, finalState)

    nextManagedArmEntry armIndex laterArms =
      case laterArms of
        _ : _ -> Just (patternCaseBlockId statementPath expressionPath (armIndex + 1) "test")
        [] -> Nothing

    managedTypedPatternPath armIndex patternPath =
      TypedPatternPath modulePath statementPath (reverse expressionPath <> [armIndex] <> patternPath)

    managedPatternFailure armIndex patternPath =
      LoweredIRLoweringFailure
        (managedTypedPatternPath armIndex patternPath)
        LoweredIRUnsupportedPattern
        LoweredIRNoFailureDetail

    managedLayoutCatalog = indexedManagedLayoutCatalog functions

    lowerArmChain resultRepresentation scrutineeOperand =
      lowerArmChainWith lowerArms arms resultRepresentation scrutineeOperand

    lowerArms resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex remainingArms currentState =
      case remainingArms of
        [] ->
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRIncompletePatternCase
                LoweredIRNoFailureDetail
            ],
            currentState
          )
        arm@(TypedCaseArm patternValue _ _) : laterArms ->
          case patternValue of
            TypedLiteralPattern patternInfo literal ->
              lowerLiteralArm
                resultRepresentation
                scrutineeCarrier
                outerSlots
                controlSlots
                controlParameters
                armIndex
                arm
                patternInfo
                literal
                laterArms
                currentState
            TypedWildcardPattern _ ->
              lowerCatchAllArm
                resultRepresentation
                scrutineeCarrier
                outerSlots
                controlSlots
                controlParameters
                armIndex
                arm
                laterArms
                currentState
            TypedVariablePattern _ _ _ ->
              lowerCatchAllArm
                resultRepresentation
                scrutineeCarrier
                outerSlots
                controlSlots
                controlParameters
                armIndex
                arm
                laterArms
                currentState
            _ ->
              ( [ LoweredIRLoweringFailure
                    (TypedPatternPath modulePath statementPath (reverse expressionPath <> [armIndex]))
                    LoweredIRUnsupportedPattern
                    LoweredIRNoFailureDetail
                ],
                currentState
              )

    lowerLiteralArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm patternInfo literal laterArms currentState =
      case (scrutineeAt scrutineeCarrier currentState, nextArmEntry armIndex laterArms) of
        (Just scrutineeOperand, Just nextBlockId) ->
          let patternPath =
                TypedPatternPath modulePath statementPath (reverse expressionPath <> [armIndex])
              (literalFailures, maybeLiteralOperand, literalState) =
                lowerLiteral patternPath patternInfo literal currentState
           in case (literalFailures, maybeLiteralOperand) of
                ([], Just literalOperand)
                  | loweredOperandRepresentation literalOperand == loweredOperandRepresentation scrutineeOperand ->
                      let comparisonIndex = loweringNextTemporary literalState
                          comparisonTemporary = LoweredTemporaryId ("t" <> Text.pack (show comparisonIndex))
                          comparisonInstruction =
                            LoweredInstruction
                              comparisonTemporary
                              LoweredBoolRepresentation
                              ( LoweredPrimitiveOperation
                                  (LoweredComparisonPrimitive LoweredEqual)
                                  [scrutineeOperand, literalOperand]
                              )
                          comparisonState =
                            literalState
                              { loweringNextTemporary = comparisonIndex + 1,
                                loweringInstructions =
                                  comparisonInstruction : loweringInstructions literalState
                              }
                       in case ambientArguments controlSlots comparisonState of
                            Just branchArguments ->
                              let matchedBlockId = matchedArmEntry armIndex arm
                                  branchState =
                                    finishCurrentBlock
                                      ( LoweredBranch
                                          (LoweredTemporaryOperand comparisonTemporary LoweredBoolRepresentation)
                                          matchedBlockId
                                          branchArguments
                                          nextBlockId
                                          branchArguments
                                      )
                                      comparisonState
                                  matchedInitial =
                                    remapAmbient
                                      controlSlots
                                      controlParameters
                                      (startBlock matchedBlockId controlParameters branchState)
                               in lowerMatchedArm
                                    resultRepresentation
                                    scrutineeCarrier
                                    outerSlots
                                    controlSlots
                                    controlParameters
                                    armIndex
                                    arm
                                    laterArms
                                    matchedInitial
                            Nothing -> ([unsupportedFailure path], comparisonState)
                (failures@(_ : _), _) -> (failures, literalState)
                _ -> ([unsupportedFailure patternPath], literalState)
        _ -> ([unsupportedFailure path], currentState)

    lowerCatchAllArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms currentState =
      let matchedBlockId = matchedArmEntry armIndex arm
       in if loweringCurrentBlockId currentState == matchedBlockId
            then
              lowerMatchedArm
                resultRepresentation
                scrutineeCarrier
                outerSlots
                controlSlots
                controlParameters
                armIndex
                arm
                laterArms
                currentState
            else case ambientArguments controlSlots currentState of
              Just jumpArguments ->
                let enteredState =
                      remapAmbient
                        controlSlots
                        controlParameters
                        ( startBlock
                            matchedBlockId
                            controlParameters
                            (finishCurrentBlock (LoweredJump matchedBlockId jumpArguments) currentState)
                        )
                 in lowerMatchedArm
                      resultRepresentation
                      scrutineeCarrier
                      outerSlots
                      controlSlots
                      controlParameters
                      armIndex
                      arm
                      laterArms
                      enteredState
              Nothing -> ([unsupportedFailure path], currentState)

    lowerMatchedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm@(TypedCaseArm patternValue maybeGuard _) laterArms currentState =
      case scrutineeAt scrutineeCarrier currentState of
        Nothing -> ([unsupportedFailure path], currentState)
        Just scrutineeOperand ->
          let scopedState = bindPattern patternValue scrutineeOperand currentState
           in case maybeGuard of
                Just guard ->
                  lowerGuardedArm
                    resultRepresentation
                    scrutineeCarrier
                    outerSlots
                    controlSlots
                    controlParameters
                    armIndex
                    arm
                    laterArms
                    currentState
                    scopedState
                    guard
                Nothing ->
                  lowerArmBody
                    resultRepresentation
                    scrutineeCarrier
                    outerSlots
                    controlSlots
                    controlParameters
                    armIndex
                    arm
                    laterArms
                    currentState
                    scopedState

    lowerGuardedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms continuationTemplate guardState guard =
      let (guardFailures, maybeGuardOperand, loweredGuardState) =
            lowerExpression
              modulePath
              statementPath
              (0 : armIndex + 1 : expressionPath)
              functions
              parameters
              guardState
              guard
       in case (guardFailures, maybeGuardOperand, nextArmEntry armIndex laterArms, ambientArguments controlSlots loweredGuardState) of
            ([], Just guardOperand, Just nextBlockId, Just branchArguments)
              | loweredOperandRepresentation guardOperand == LoweredBoolRepresentation ->
                  let bodyBlockId = patternCaseBlockId statementPath expressionPath armIndex "body"
                      branchState =
                        finishCurrentBlock
                          ( LoweredBranch
                              guardOperand
                              bodyBlockId
                              branchArguments
                              nextBlockId
                              branchArguments
                          )
                          loweredGuardState
                      bodyInitial =
                        bindCurrentPattern
                          arm
                          scrutineeCarrier
                          ( remapAmbient
                              controlSlots
                              controlParameters
                              (startBlock bodyBlockId controlParameters branchState)
                          )
                   in lowerArmBody
                        resultRepresentation
                        scrutineeCarrier
                        outerSlots
                        controlSlots
                        controlParameters
                        armIndex
                        arm
                        laterArms
                        continuationTemplate
                        bodyInitial
            (failures@(_ : _), _, _, _) -> (failures, loweredGuardState)
            _ -> ([unsupportedFailure path], loweredGuardState)

    lowerArmBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex (TypedCaseArm _ _ body) laterArms continuationTemplate bodyState =
      case destination of
        FinishFunction expected ->
          let (bodyFailures, loweredBodyState) =
                lowerFunctionResult
                  modulePath
                  statementPath
                  (1 : armIndex + 1 : expressionPath)
                  functions
                  parameters
                  expected
                  bodyState
                  body
           in case bodyFailures of
                [] -> continueLaterArms loweredBodyState
                failures@(_ : _) -> (failures, loweredBodyState)
        ProduceValue ->
          let (bodyFailures, maybeBodyOperand, loweredBodyState) =
                lowerExpression
                  modulePath
                  statementPath
                  (1 : armIndex + 1 : expressionPath)
                  functions
                  parameters
                  bodyState
                  body
           in case (bodyFailures, maybeBodyOperand, ambientArguments outerSlots loweredBodyState) of
                ([], Just bodyOperand, Just joinAmbientArguments)
                  | loweredOperandRepresentation bodyOperand == resultRepresentation ->
                      continueLaterArms
                        ( finishCurrentBlock
                            (LoweredJump joinBlockId (joinAmbientArguments <> [bodyOperand]))
                            loweredBodyState
                        )
                (failures@(_ : _), _, _) -> (failures, loweredBodyState)
                _ -> ([unsupportedFailure path], loweredBodyState)
      where
        continueLaterArms bodyFinished =
          case laterArms of
            [] -> ([], bodyFinished)
            _ ->
              case nextArmEntry armIndex laterArms of
                Just nextBlockId ->
                  let nextBase = continuationState continuationTemplate bodyFinished
                      nextInitial =
                        remapAmbient
                          controlSlots
                          controlParameters
                          (startBlock nextBlockId controlParameters nextBase)
                   in lowerArms
                        resultRepresentation
                        scrutineeCarrier
                        outerSlots
                        controlSlots
                        controlParameters
                        (armIndex + 1)
                        laterArms
                        nextInitial
                Nothing -> ([unsupportedFailure path], bodyFinished)

    continuationState template completedState =
      template
        { loweringNextCarrier = loweringNextCarrier completedState,
          loweringCompletedBlocks = loweringCompletedBlocks completedState
        }

    nextArmEntry armIndex laterArms =
      case laterArms of
        nextArm : _ -> Just (armEntryBlock (armIndex + 1) nextArm)
        [] -> Nothing

    armEntryBlock armIndex arm@(TypedCaseArm patternValue _ _) =
      case patternValue of
        TypedLiteralPattern {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        _ -> matchedArmEntry armIndex arm

    matchedArmEntry armIndex (TypedCaseArm _ maybeGuard _) =
      patternCaseBlockId
        statementPath
        expressionPath
        armIndex
        (case maybeGuard of Just _ -> "guard"; Nothing -> "body")

    bindPattern patternValue scrutineeOperand currentState =
      case patternValue of
        TypedVariablePattern _ binder _ ->
          currentState
            { loweringLocalBindings =
                Map.insert binder scrutineeOperand (loweringLocalBindings currentState)
            }
        _ -> currentState

    bindCurrentPattern (TypedCaseArm patternValue _ _) scrutineeCarrier currentState =
      case scrutineeAt scrutineeCarrier currentState of
        Just scrutineeOperand -> bindPattern patternValue scrutineeOperand currentState
        Nothing -> currentState

    scrutineeAt carrier currentState =
      Map.lookup carrier (loweringCarriedOperands currentState)

    unsupportedFailure failurePath =
      LoweredIRLoweringFailure
        failurePath
        LoweredIRUnsupportedExpression
        LoweredIRNoFailureDetail

lowerClosureValue ::
  TypedCoreValidationPath ->
  [FunctionParameterShape] ->
  FunctionShape ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerClosureValue path parameters function state =
  case functionShapeEnvironmentLayout function of
    Just layoutId
      | Just environmentOperand <- Map.lookup layoutId (loweringSharedEnvironments state) ->
          constructClosure environmentOperand state
    Just layoutId
      | Just environmentFields <- captureOperands ->
          let environmentIndex = loweringNextTemporary state
              environmentTemporaryId = temporaryId environmentIndex
              environmentRepresentation = LoweredManagedReferenceRepresentation layoutId
              environmentInstruction =
                LoweredInstruction
                  environmentTemporaryId
                  environmentRepresentation
                  (LoweredConstructProduct layoutId environmentFields)
              environmentState =
                state
                  { loweringNextTemporary = environmentIndex + 1,
                    loweringInstructions =
                      environmentInstruction : loweringInstructions state
                  }
           in constructClosure
                (LoweredTemporaryOperand environmentTemporaryId environmentRepresentation)
                environmentState
    _ -> unsupportedExpression path state
  where
    constructClosure environmentOperand currentState =
      let closureIndex = loweringNextTemporary currentState
          closureTemporaryId = temporaryId closureIndex
          closureRepresentation = functionClosureRepresentation function
          closureInstruction =
            LoweredInstruction
              closureTemporaryId
              closureRepresentation
              (LoweredConstructClosure (functionShapeId function) environmentOperand)
          nextState =
            currentState
              { loweringNextTemporary = closureIndex + 1,
                loweringInstructions = closureInstruction : loweringInstructions currentState
              }
       in ([], Just (LoweredTemporaryOperand closureTemporaryId closureRepresentation), nextState)
    captureOperands = traverse captureOperand (functionShapeCaptures function)
    captureOperand capture =
      case Map.lookup (captureShapeBinder capture) (loweringLocalBindings state) of
        Just operand
          | loweredOperandRepresentation operand == captureShapeRepresentation capture -> Just operand
        Just _ -> Nothing
        Nothing ->
          case findParameterShape (Just (captureShapeBinder capture)) parameters of
            Just (FunctionParameterShape _ (LoweredParameter parameterId representation))
              | representation == captureShapeRepresentation capture ->
                  Just (LoweredFunctionParameterOperand parameterId representation)
            _ -> Nothing
    temporaryId index = LoweredTemporaryId ("t" <> Text.pack (show index))

lowerLiteral ::
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedLiteral ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerLiteral path info literal state =
  case (literal, typedNodeRecipe info) of
    (TypedBooleanLiteral value, TypedBoolRecipe) ->
      loweredImmediate (LoweredBoolImmediate value)
    (TypedCharacterLiteral value, TypedCharRecipe) ->
      loweredImmediate (LoweredCharImmediate value)
    (TypedIntegerLiteral source, TypedSignedIntegerRecipe bits) ->
      lowerInteger source (LoweredSignedIntegerImmediate <$> integerWidth bits)
    (TypedIntegerLiteral source, TypedUnsignedIntegerRecipe bits) ->
      lowerInteger source (LoweredUnsignedIntegerImmediate <$> integerWidth bits)
    (TypedFractionalLiteral whole fractional _, TypedFloatRecipe bits) ->
      case floatWidth bits of
        Just width ->
          loweredImmediate
            (LoweredFloatImmediate width (whole <> "." <> fractional))
        Nothing -> unsupportedRepresentation path (typedNodeRecipe info) state
    (TypedTextLiteral value, TypedManagedTextRecipe) ->
      let temporaryIndex = loweringNextTemporary state
          temporary = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
          instruction =
            LoweredInstruction
              temporary
              textRepresentation
              (LoweredConstructText textLayoutId value)
       in ( [],
            Just (LoweredTemporaryOperand temporary textRepresentation),
            state
              { loweringNextTemporary = temporaryIndex + 1,
                loweringInstructions = instruction : loweringInstructions state
              }
          )
    _ -> unsupportedRepresentation path (typedNodeRecipe info) state
  where
    loweredImmediate immediate =
      ([], Just (LoweredImmediateOperand immediate), state)
    lowerInteger source maybeConstructor =
      case (readMaybe (Text.unpack source), maybeConstructor) of
        (Just value, Just constructor) -> loweredImmediate (constructor value)
        _ -> unsupportedRepresentation path (typedNodeRecipe info) state

lowerBinary ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedOperatorRef ->
  TypedExpr ->
  TypedExpr ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerBinary modulePath statementPath expressionPath path info operator left right functions parameters state =
  case operatorFailures <> resultRepresentationFailures <> leftFailures <> rightFailures of
    failures@(_ : _) -> (failures, Nothing, rightState)
    [] ->
      case (maybePrimitive, maybeResultRepresentation, maybeTransportedLeftOperand, maybeRightOperand) of
        (Just primitive, Just resultRepresentation, Just leftOperand, Just rightOperand) ->
          let temporaryIndex = loweringNextTemporary rightState
              temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
              instruction =
                LoweredInstruction
                  temporaryId
                  resultRepresentation
                  (LoweredPrimitiveOperation primitive [leftOperand, rightOperand])
              nextState =
                rightState
                  { loweringNextTemporary = temporaryIndex + 1,
                    loweringInstructions = instruction : loweringInstructions rightState
                  }
           in ([], Just (LoweredTemporaryOperand temporaryId resultRepresentation), nextState)
        _ -> unsupportedExpression path rightState
  where
    (operatorFailures, maybePrimitive) =
      case loweredPrimitive operator of
        Just primitive -> ([], Just primitive)
        Nothing ->
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRUnsupportedOperator
                (LoweredIROperatorFailureDetail operator)
            ],
            Nothing
          )
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe info)
    (leftFailures, maybeLeftOperand, leftState) =
      lowerExpression
        modulePath
        statementPath
        (0 : expressionPath)
        functions
        parameters
        state
        left
    (maybeLeftCarrier, rightInitialState) =
      case maybeLeftOperand of
        Just leftOperand ->
          let (carrier, carriedState) = carryOperand leftOperand leftState
           in (Just carrier, carriedState)
        Nothing -> (Nothing, leftState)
    (rightFailures, maybeRightOperand, carriedRightState) =
      lowerExpression
        modulePath
        statementPath
        (1 : expressionPath)
        functions
        parameters
        rightInitialState
        right
    (maybeTransportedLeftOperand, rightState) =
      case maybeLeftCarrier of
        Just carrier ->
          case releaseCarriedOperands [carrier] carriedRightState of
            (Just [leftOperand], releasedState) -> (Just leftOperand, releasedState)
            (_, releasedState) -> (Nothing, releasedState)
        Nothing -> (Nothing, carriedRightState)

lowerTextEquality ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  Bool ->
  TypedExpr ->
  TypedExpr ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerTextEquality modulePath statementPath expressionPath path info negateResult left right functions parameters state =
  case resultRepresentationFailures <> leftFailures <> rightFailures of
    failures@(_ : _) -> (failures, Nothing, rightState)
    [] ->
      case (maybeResultRepresentation, maybeTransportedLeftOperand, maybeRightOperand) of
        (Just resultRepresentation, Just leftOperand, Just rightOperand) ->
          case emitRuntimeServiceInstruction TextEqualService resultRepresentation [leftOperand, rightOperand] rightState of
            Just (equalityOperand, equalityState)
              | negateResult ->
                  case emitBooleanNotInstruction equalityOperand equalityState of
                    Just (negatedOperand, negatedState) ->
                      ([], Just negatedOperand, negatedState)
                    Nothing -> unsupportedExpression path rightState
              | otherwise -> ([], Just equalityOperand, equalityState)
            Nothing -> unsupportedExpression path rightState
        _ -> unsupportedExpression path rightState
  where
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe info)
    (leftFailures, maybeLeftOperand, leftState) =
      lowerExpression
        modulePath
        statementPath
        (0 : expressionPath)
        functions
        parameters
        state
        left
    (maybeLeftCarrier, rightInitialState) =
      case maybeLeftOperand of
        Just leftOperand ->
          let (carrier, carriedState) = carryOperand leftOperand leftState
           in (Just carrier, carriedState)
        Nothing -> (Nothing, leftState)
    (rightFailures, maybeRightOperand, carriedRightState) =
      lowerExpression
        modulePath
        statementPath
        (1 : expressionPath)
        functions
        parameters
        rightInitialState
        right
    (maybeTransportedLeftOperand, rightState) =
      case maybeLeftCarrier of
        Just carrier ->
          case releaseCarriedOperands [carrier] carriedRightState of
            (Just [leftOperand], releasedState) -> (Just leftOperand, releasedState)
            (_, releasedState) -> (Nothing, releasedState)
        Nothing -> (Nothing, carriedRightState)

lowerApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerApplication modulePath statementPath expressionPath path functions parameters state expression =
  case constructorApplicationLayout managedLayoutCatalog callee of
    Just constructor ->
      lowerManagedVariantApplication
        modulePath
        statementPath
        expressionPath
        path
        functions
        parameters
        state
        constructor
        expression
    Nothing ->
      case textRuntimeServiceApplication expression of
        Just serviceKey ->
          lowerTextRuntimeApplication
            modulePath
            statementPath
            expressionPath
            path
            functions
            parameters
            state
            serviceKey
            expression
        Nothing ->
          lowerOrdinaryApplication
            modulePath
            statementPath
            expressionPath
            path
            functions
            parameters
            state
            expression
  where
    managedLayoutCatalog = indexedManagedLayoutCatalog functions
    (callee, _, _) = applicationSpine expressionPath expression

lowerManagedVariantApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ManagedConstructorLayout ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerManagedVariantApplication modulePath statementPath expressionPath path functions parameters state constructor expression
  | length arguments /= length fieldRepresentations =
      ( [ LoweredIRLoweringFailure
            path
            LoweredIRCallArityUnsupported
            (LoweredIRArityFailureDetail (length fieldRepresentations) (length arguments))
        ],
        Nothing,
        state
      )
  | otherwise =
      case representationAtPath managedLayoutCatalog path (typedNodeRecipe (typedExpressionInfo expression)) of
        (failures@(_ : _), _) -> (failures, Nothing, state)
        ([], Just resultRepresentation)
          | resultRepresentation == expectedResultRepresentation ->
              case lowerExpressionsAtPathsLeftToRight
                modulePath
                statementPath
                functions
                parameters
                state
                arguments of
                (failures@(_ : _), _, argumentState) -> (failures, Nothing, argumentState)
                ([], Just operands, argumentState)
                  | map loweredOperandRepresentation operands == fieldRepresentations ->
                      let (operand, nextState) =
                            emitManagedConstruction
                              resultRepresentation
                              (LoweredConstructVariant layoutId (fromIntegral (managedConstructorTag constructor)) operands)
                              argumentState
                       in ([], Just operand, nextState)
                ([], _, argumentState) -> unsupportedExpression path argumentState
        _ -> unsupportedExpression path state
  where
    managedLayoutCatalog = indexedManagedLayoutCatalog functions
    layoutId = managedConstructorLayoutId constructor
    fieldRepresentations = managedConstructorFields constructor
    expectedResultRepresentation = LoweredManagedReferenceRepresentation layoutId
    (_, _, arguments) = applicationSpine expressionPath expression

lowerOrdinaryApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerOrdinaryApplication modulePath statementPath expressionPath path functions parameters state expression =
  case callee of
    TypedVariableExpr _ name binderReference ->
      case binderReference >>= (`Map.lookup` loweringLocalBindings state) of
        Just operand
          | LoweredClosureRepresentation {} <- loweredOperandRepresentation operand ->
              lowerClosureApplication
        _ ->
          case findFunctionShape binderReference functions of
            Just target
              | functionShapeCallableShape target == TypedClosureCallableShape ->
                  lowerClosureApplication
              | length arguments > length (functionShapeParameters target) ->
                  lowerClosureApplication
              | length arguments == length (functionShapeParameters target) ->
                  case resultRepresentationFailures <> argumentFailures of
                    failures@(_ : _) -> (failures, Nothing, argumentState)
                    [] ->
                      case (maybeResultRepresentation, argumentOperands) of
                        (Just resultRepresentation, Just operands) ->
                          let temporaryIndex = loweringNextTemporary argumentState
                              temporaryId =
                                LoweredTemporaryId
                                  ("t" <> Text.pack (show temporaryIndex))
                              instruction =
                                LoweredInstruction
                                  temporaryId
                                  resultRepresentation
                                  (LoweredDirectCall (functionShapeId target) operands)
                              nextState =
                                argumentState
                                  { loweringNextTemporary = temporaryIndex + 1,
                                    loweringInstructions =
                                      instruction : loweringInstructions argumentState
                                  }
                           in ([], Just (LoweredTemporaryOperand temporaryId resultRepresentation), nextState)
                        _ -> unsupportedExpression path argumentState
              | otherwise ->
                  ( [ LoweredIRLoweringFailure
                        path
                        LoweredIRCallArityUnsupported
                        ( LoweredIRArityFailureDetail
                            (length (functionShapeParameters target))
                            (length arguments)
                        )
                    ],
                    Nothing,
                    state
                  )
            Nothing
              | Just _ <- findParameterShape binderReference parameters ->
                  lowerClosureApplication
              | otherwise ->
                  ( [ LoweredIRLoweringFailure
                        path
                        LoweredIRNonLocalCallUnsupported
                        (LoweredIRNameFailureDetail name)
                    ],
                    Nothing,
                    state
                  )
    _ ->
      lowerUnaryClosureApplication
        modulePath
        statementPath
        expressionPath
        path
        functions
        parameters
        state
        expression
  where
    lowerClosureApplication =
      lowerUnaryClosureApplication
        modulePath
        statementPath
        expressionPath
        path
        functions
        parameters
        state
        expression
    (callee, _, arguments) = applicationSpine expressionPath expression
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe (typedExpressionInfo expression))
    (reversedArgumentFailureChunks, reversedArgumentCarriers, carriedArgumentState) =
      foldl'
        lowerArgument
        ([], [], state)
        arguments
    argumentFailures = concat (reverse reversedArgumentFailureChunks)
    (argumentOperands, argumentState) =
      case sequence (reverse reversedArgumentCarriers) of
        Just carriers -> releaseCarriedOperands carriers carriedArgumentState
        Nothing -> (Nothing, carriedArgumentState)
    lowerArgument (reversedFailureChunks, reversedCarriers, currentState) (argumentPath, argument) =
      let (nextFailures, maybeOperand, nextState) =
            lowerExpression
              modulePath
              statementPath
              argumentPath
              functions
              parameters
              currentState
              argument
          (maybeCarrier, carriedState) =
            case maybeOperand of
              Just operand ->
                let (carrier, operandState) = carryOperand operand nextState
                 in (Just carrier, operandState)
              Nothing -> (Nothing, nextState)
       in (nextFailures : reversedFailureChunks, maybeCarrier : reversedCarriers, carriedState)

lowerTextRuntimeApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  RuntimeServiceKey ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerTextRuntimeApplication modulePath statementPath expressionPath path functions parameters state serviceKey expression =
  case resultRepresentationFailures <> argumentFailures of
    failures@(_ : _) -> (failures, Nothing, argumentState)
    [] ->
      case (maybeResultRepresentation, argumentOperands) of
        (Just resultRepresentation, Just operands) ->
          case emitRuntimeServiceInstruction serviceKey resultRepresentation operands argumentState of
            Just (resultOperand, resultState) ->
              ([], Just resultOperand, resultState)
            Nothing -> unsupportedExpression path argumentState
        _ -> unsupportedExpression path argumentState
  where
    (_, _, arguments) = applicationSpine expressionPath expression
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe (typedExpressionInfo expression))
    (reversedArgumentFailureChunks, reversedArgumentCarriers, carriedArgumentState) =
      foldl'
        lowerArgument
        ([], [], state)
        arguments
    argumentFailures = concat (reverse reversedArgumentFailureChunks)
    (argumentOperands, argumentState) =
      case sequence (reverse reversedArgumentCarriers) of
        Just carriers -> releaseCarriedOperands carriers carriedArgumentState
        Nothing -> (Nothing, carriedArgumentState)
    lowerArgument (reversedFailureChunks, reversedCarriers, currentState) (argumentPath, argument) =
      let (nextFailures, maybeOperand, nextState) =
            lowerExpression
              modulePath
              statementPath
              argumentPath
              functions
              parameters
              currentState
              argument
          (maybeCarrier, carriedState) =
            case maybeOperand of
              Just operand ->
                let (carrier, operandState) = carryOperand operand nextState
                 in (Just carrier, operandState)
              Nothing -> (Nothing, nextState)
       in (nextFailures : reversedFailureChunks, maybeCarrier : reversedCarriers, carriedState)

emitRuntimeServiceInstruction ::
  RuntimeServiceKey ->
  LoweredRepresentation ->
  [LoweredOperand] ->
  LoweringState ->
  Maybe (LoweredOperand, LoweringState)
emitRuntimeServiceInstruction serviceKey resultRepresentation operands state =
  case runtimeServiceContract serviceKey of
    LoweredRuntimeService serviceId (LoweredCallSignature expectedArguments expectedResult)
      | map loweredOperandRepresentation operands == expectedArguments,
        resultRepresentation == expectedResult ->
          let temporaryIndex = loweringNextTemporary state
              temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
              instruction =
                LoweredInstruction
                  temporaryId
                  resultRepresentation
                  (LoweredRuntimeCall serviceId operands)
              nextState =
                state
                  { loweringNextTemporary = temporaryIndex + 1,
                    loweringInstructions = instruction : loweringInstructions state
                  }
           in Just (LoweredTemporaryOperand temporaryId resultRepresentation, nextState)
      | otherwise -> Nothing

emitBooleanNotInstruction :: LoweredOperand -> LoweringState -> Maybe (LoweredOperand, LoweringState)
emitBooleanNotInstruction operand state
  | loweredOperandRepresentation operand == LoweredBoolRepresentation =
      let temporaryIndex = loweringNextTemporary state
          temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
          instruction =
            LoweredInstruction
              temporaryId
              LoweredBoolRepresentation
              (LoweredPrimitiveOperation (LoweredBooleanPrimitive LoweredBooleanNot) [operand])
          nextState =
            state
              { loweringNextTemporary = temporaryIndex + 1,
                loweringInstructions = instruction : loweringInstructions state
              }
       in Just (LoweredTemporaryOperand temporaryId LoweredBoolRepresentation, nextState)
  | otherwise = Nothing

lowerUnaryClosureApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerUnaryClosureApplication modulePath statementPath expressionPath path functions parameters state expression =
  case expression of
    TypedApplyExpr info function argument ->
      case resultRepresentationFailures <> functionFailures <> argumentFailures of
        failures@(_ : _) -> (failures, Nothing, argumentState)
        [] ->
          case (maybeResultRepresentation, maybeTransportedFunctionOperand, maybeArgumentOperand) of
            ( Just resultRepresentation,
              Just functionOperand,
              Just argumentOperand
              )
                | LoweredClosureRepresentation (LoweredCallSignature [argumentRepresentation] expectedResultRepresentation) <- loweredOperandRepresentation functionOperand,
                  loweredOperandRepresentation argumentOperand == argumentRepresentation,
                  resultRepresentation == expectedResultRepresentation ->
                    let temporaryIndex = loweringNextTemporary argumentState
                        temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
                        instruction =
                          LoweredInstruction
                            temporaryId
                            resultRepresentation
                            (LoweredClosureCall functionOperand [argumentOperand])
                        nextState =
                          argumentState
                            { loweringNextTemporary = temporaryIndex + 1,
                              loweringInstructions = instruction : loweringInstructions argumentState
                            }
                     in ([], Just (LoweredTemporaryOperand temporaryId resultRepresentation), nextState)
            _ -> unsupportedExpression path argumentState
      where
        (resultRepresentationFailures, maybeResultRepresentation) =
          representationAtPath (indexedManagedLayoutCatalog functions) path (typedNodeRecipe info)
        (functionFailures, maybeFunctionOperand, functionState) =
          lowerExpression
            modulePath
            statementPath
            (0 : expressionPath)
            functions
            parameters
            state
            function
        (maybeFunctionCarrier, argumentInitialState) =
          case maybeFunctionOperand of
            Just functionOperand ->
              let (carrier, carriedState) = carryOperand functionOperand functionState
               in (Just carrier, carriedState)
            Nothing -> (Nothing, functionState)
        (argumentFailures, maybeArgumentOperand, carriedArgumentState) =
          lowerExpression
            modulePath
            statementPath
            (1 : expressionPath)
            functions
            parameters
            argumentInitialState
            argument
        (maybeTransportedFunctionOperand, argumentState) =
          case maybeFunctionCarrier of
            Just carrier ->
              case releaseCarriedOperands [carrier] carriedArgumentState of
                (Just [functionOperand], releasedState) -> (Just functionOperand, releasedState)
                (_, releasedState) -> (Nothing, releasedState)
            Nothing -> (Nothing, carriedArgumentState)
    _ -> unsupportedExpression path state

unsupportedExpression ::
  TypedCoreValidationPath ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
unsupportedExpression path state =
  ( [ LoweredIRLoweringFailure
        path
        LoweredIRUnsupportedExpression
        LoweredIRNoFailureDetail
    ],
    Nothing,
    state
  )

unsupportedRepresentation ::
  TypedCoreValidationPath ->
  TypedRepresentationRecipe ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
unsupportedRepresentation path recipe state =
  ( [ LoweredIRLoweringFailure
        path
        LoweredIRUnsupportedRepresentation
        (LoweredIRRecipeFailureDetail recipe)
    ],
    Nothing,
    state
  )
