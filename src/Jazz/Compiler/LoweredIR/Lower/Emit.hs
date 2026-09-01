{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.LoweredIR.Lower.Emit
  ( emitAnalyzedModule,
  )
where

import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
  ( constructorApplicationLayout,
    constructorLayoutFor,
    managedLayoutShapeFor,
    managedPatternConstructorsFor,
    nodeInstantiations,
    orderedManagedLayouts,
    productLayoutFields,
    representationForRecipe,
  )
import Jazz.Compiler.LoweredIR.Lower.ManagedPatterns
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
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.Compiler.TypedCore
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

data ManagedDecisionAccessor
  = ManagedDecisionRoot
  | ManagedDecisionProductField ManagedDecisionAccessor LoweredLayoutId Int LoweredRepresentation
  | ManagedDecisionVariantField ManagedDecisionAccessor LoweredLayoutId Natural Int LoweredRepresentation

data ManagedDecisionConstructor
  = ManagedDecisionProduct LoweredLayoutId [(TypedNodeInfo, LoweredRepresentation)]
  | ManagedDecisionVariant ManagedPatternConstructor

data ManagedAlternativeDecision
  = ManagedAlternativeSuccess Int
  | ManagedAlternativeVariant ManagedDecisionAccessor [(ManagedPatternConstructor, ManagedAlternativeDecision)]
  | ManagedAlternativeLiteral ManagedDecisionAccessor TypedNodeInfo TypedLiteral ManagedAlternativeDecision ManagedAlternativeDecision

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
        ( lowerPatternCaseTo
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
      lowerPatternCase
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

lowerPatternCase ::
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
lowerPatternCase modulePath statementPath expressionPath path info scrutinee arms functions parameters state =
  lowerPatternCaseTo
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

lowerPatternCaseTo ::
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
lowerPatternCaseTo destination modulePath statementPath expressionPath path info scrutinee arms functions parameters state =
  case analyzeManagedPatternCase (indexedManagedLayoutCatalog functions) modulePath statementPath (reverse expressionPath) scrutinee arms of
    Left failure -> ([failure], Nothing, state)
    Right armPlan ->
      lowerManagedPatternCaseTo destination modulePath statementPath expressionPath path info scrutinee armPlan functions parameters state

lowerManagedPatternCaseTo ::
  ResultDestination ->
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedExpr ->
  NonEmpty ManagedPatternArm ->
  FunctionIndex ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerManagedPatternCaseTo destination modulePath statementPath expressionPath path info scrutinee armPlan functions parameters state =
  case resultRepresentationFailures <> scrutineeFailures of
    failures@(_ : _) -> (failures, Nothing, scrutineeState)
    [] ->
      case (maybeResultRepresentation, maybeScrutineeOperand) of
        (Just resultRepresentation, Just scrutineeOperand)
          | Just scrutineeRepresentation <- representationForRecipe (indexedManagedLayoutCatalog functions) (typedNodeRecipe (typedExpressionInfo scrutinee)),
            loweredOperandRepresentation scrutineeOperand == scrutineeRepresentation ->
              case destination of
                ProduceValue -> lowerArmChain resultRepresentation scrutineeOperand
                FinishFunction expected
                  | expected == resultRepresentation ->
                      lowerArmChain resultRepresentation scrutineeOperand
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

    lowerArmChain resultRepresentation scrutineeOperand =
      let outerSlots = ambientSlots scrutineeState
          outerParameters = ambientParameters outerSlots
          (scrutineeCarrier, carriedState) = carryOperand scrutineeOperand scrutineeState
          controlSlots = ambientSlots carriedState
          controlParameters = ambientParameters controlSlots
          plannedArms = NonEmpty.toList armPlan
          loweredArms
            | constructorArmPlan plannedArms =
                lowerConstructorArms resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters plannedArms carriedState
            | otherwise =
                lowerArms resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters 0 plannedArms carriedState
       in case loweredArms of
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
        arm@(ManagedPatternArm patternValue _ _) : laterArms ->
          case patternValue of
            ManagedLiteral patternInfo literal ->
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
            ManagedWildcard _ ->
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
            ManagedVariable {} ->
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
            ManagedTuple {} -> lowerProjectedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms currentState
            ManagedConstructor {} -> lowerProjectedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms currentState
            ManagedAs {} -> lowerProjectedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms currentState
            ManagedOr _ alternatives -> lowerOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState

    constructorArmPlan = all constructorArm
      where
        constructorArm (ManagedPatternArm patternValue Nothing _) =
          case constructorPattern patternValue of
            Just _ -> True
            Nothing -> False
        constructorArm _ = False

    constructorPattern patternValue =
      case patternValue of
        ManagedConstructor constructor children -> Just (constructor, children, [])
        ManagedAs _ binder nested -> do
          (constructor, children, binders) <- constructorPattern nested
          Just (constructor, children, binder : binders)
        _ -> Nothing

    lowerProjectedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm@(ManagedPatternArm patternValue _ _) laterArms currentState =
      case scrutineeAt scrutineeCarrier currentState >>= (\operand -> matchPatternWork controlSlots controlParameters armIndex (ordinaryMatchBlockId armIndex) (armEntryBlock (armIndex + 1) <$> nonEmptyHead laterArms) (1 :: Int) [] [] [(patternValue, operand)] currentState) of
        Just (nextMatchIndex, binderOperands, conditions, projectedState) ->
          enterProjectedBody
            resultRepresentation
            scrutineeCarrier
            outerSlots
            controlSlots
            controlParameters
            armIndex
            arm
            laterArms
            currentState
            nextMatchIndex
            binderOperands
            conditions
            (armEntryBlock (armIndex + 1) <$> nonEmptyHead laterArms)
            projectedState
        Nothing -> ([patternUnsupported armIndex], currentState)

    lowerOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState =
      case nonEmptyHead laterArms of
        Just _ -> lowerChainedOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState
        Nothing -> lowerExhaustiveOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState

    lowerChainedOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState =
      case ambientArguments controlSlots currentState of
        Just initialArguments ->
          let indexedAlternatives = zip [0 :: Int ..] (NonEmpty.toList alternatives)
              firstBlockId = alternativeBlockId (0 :: Int)
              enteredState = finishCurrentBlock (LoweredJump firstBlockId initialArguments) currentState
           in compileAlternatives indexedAlternatives Nothing enteredState
        Nothing -> ([unsupportedFailure path], currentState)
      where
        alternativeBlockId alternativeIndex =
          patternCaseBlockId statementPath expressionPath armIndex ("alternative" <> Text.pack (show alternativeIndex))
        alternativeMatchBlockId alternativeIndex matchIndex =
          patternCaseBlockId statementPath expressionPath armIndex ("alternative" <> Text.pack (show alternativeIndex) <> "$match" <> Text.pack (show matchIndex))
        finalFailureBlock = armEntryBlock (armIndex + 1) <$> nonEmptyHead laterArms
        compileAlternatives indexedAlternatives maybeExpectedBinders completedState =
          case indexedAlternatives of
            [] -> ([patternUnsupported armIndex], completedState)
            (alternativeIndex, alternative) : remaining ->
              let blockId = alternativeBlockId alternativeIndex
                  blockBase = continuationState currentState completedState
                  blockState = remapAmbient controlSlots controlParameters (startBlock blockId controlParameters blockBase)
                  maybeFailureBlock =
                    case remaining of
                      (nextAlternativeIndex, _) : _ -> Just (alternativeBlockId nextAlternativeIndex)
                      [] -> finalFailureBlock
               in case scrutineeAt scrutineeCarrier blockState of
                    Just currentScrutinee ->
                      case matchPatternWork controlSlots controlParameters armIndex (alternativeMatchBlockId alternativeIndex) maybeFailureBlock 1 [] [] [(alternative, currentScrutinee)] blockState of
                        Just (nextMatchIndex, binderOperands, conditions, matchedState)
                          | binderContractMatches maybeExpectedBinders binderOperands ->
                              case remaining of
                                [] ->
                                  enterProjectedBody
                                    resultRepresentation
                                    scrutineeCarrier
                                    outerSlots
                                    controlSlots
                                    controlParameters
                                    armIndex
                                    arm
                                    laterArms
                                    currentState
                                    nextMatchIndex
                                    binderOperands
                                    conditions
                                    maybeFailureBlock
                                    matchedState
                                _ ->
                                  case enterAlternativeSuccess alternativeIndex nextMatchIndex binderOperands conditions maybeFailureBlock matchedState of
                                    (failures@(_ : _), failedState) -> (failures, failedState)
                                    ([], succeededState) ->
                                      compileAlternatives
                                        remaining
                                        (Just [(binder, loweredOperandRepresentation operand) | (binder, operand) <- binderOperands])
                                        succeededState
                        _ -> ([patternUnsupported armIndex], blockState)
                    Nothing -> ([unsupportedFailure path], blockState)
        enterAlternativeSuccess alternativeIndex matchIndex binderOperands conditions maybeFailureBlock matchedState =
          case maybeFailureBlock of
            Nothing -> ([patternUnsupported armIndex], matchedState)
            Just failureBlockId -> lowerConditions matchIndex binderOperands conditions matchedState
              where
                bodyBlockId = matchedArmEntry armIndex arm
                lowerConditions currentMatchIndex currentBinders remainingConditions conditionState =
                  case ambientArguments controlSlots conditionState of
                    Nothing -> ([unsupportedFailure path], conditionState)
                    Just currentControlArguments ->
                      case remainingConditions of
                        [] ->
                          ([], finishCurrentBlock (LoweredJump bodyBlockId (currentControlArguments <> map snd currentBinders)) conditionState)
                        (literalInfo, literal, comparedOperand) : laterConditions ->
                          let (literalFailures, maybeLiteralOperand, literalState) = lowerLiteral path literalInfo literal conditionState
                           in case (literalFailures, maybeLiteralOperand, ambientArguments controlSlots literalState) of
                                ([], Just literalOperand, Just literalControlArguments)
                                  | loweredOperandRepresentation literalOperand == loweredOperandRepresentation comparedOperand ->
                                      let comparisonIndex = loweringNextTemporary literalState
                                          comparisonTemporary = LoweredTemporaryId ("t" <> Text.pack (show comparisonIndex))
                                          comparisonInstruction =
                                            LoweredInstruction
                                              comparisonTemporary
                                              LoweredBoolRepresentation
                                              (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [comparedOperand, literalOperand])
                                          comparisonState =
                                            literalState
                                              { loweringNextTemporary = comparisonIndex + 1,
                                                loweringInstructions = comparisonInstruction : loweringInstructions literalState
                                              }
                                          conditionOperand = LoweredTemporaryOperand comparisonTemporary LoweredBoolRepresentation
                                       in case laterConditions of
                                            [] ->
                                              ( [],
                                                finishCurrentBlock
                                                  (LoweredBranch conditionOperand bodyBlockId (literalControlArguments <> map snd currentBinders) failureBlockId literalControlArguments)
                                                  comparisonState
                                              )
                                            _ ->
                                              let nextBlockId = alternativeMatchBlockId alternativeIndex currentMatchIndex
                                                  pendingBinderParameters =
                                                    [ LoweredParameter (LoweredParameterId ("pending" <> Text.pack (show index))) (loweredOperandRepresentation binderOperand)
                                                    | (index, (_, binderOperand)) <- zip [1 :: Int ..] currentBinders
                                                    ]
                                                  matchParameters =
                                                    [ LoweredParameter (LoweredParameterId ("match" <> Text.pack (show index))) (loweredOperandRepresentation remainingOperand)
                                                    | (index, (_, _, remainingOperand)) <- zip [1 :: Int ..] laterConditions
                                                    ]
                                                  nextArguments = literalControlArguments <> map snd currentBinders <> [remainingOperand | (_, _, remainingOperand) <- laterConditions]
                                                  nextState =
                                                    remapAmbient
                                                      controlSlots
                                                      controlParameters
                                                      ( startBlock
                                                          nextBlockId
                                                          (controlParameters <> pendingBinderParameters <> matchParameters)
                                                          (finishCurrentBlock (LoweredBranch conditionOperand nextBlockId nextArguments failureBlockId literalControlArguments) comparisonState)
                                                      )
                                                  pendingBinders =
                                                    zipWith
                                                      (\(binder, _) (LoweredParameter parameterId representation) -> (binder, LoweredBlockParameterOperand parameterId representation))
                                                      currentBinders
                                                      pendingBinderParameters
                                                  pendingConditions =
                                                    zipWith
                                                      (\(remainingInfo, remainingLiteral, _) (LoweredParameter parameterId representation) -> (remainingInfo, remainingLiteral, LoweredBlockParameterOperand parameterId representation))
                                                      laterConditions
                                                      matchParameters
                                               in lowerConditions (currentMatchIndex + 1) pendingBinders pendingConditions nextState
                                (failures@(_ : _), _, _) -> (failures, literalState)
                                _ -> ([patternUnsupported armIndex], literalState)
        binderContractMatches maybeExpected binderOperands =
          case maybeExpected of
            Nothing -> True
            Just expected -> expected == [(binder, loweredOperandRepresentation operand) | (binder, operand) <- binderOperands]

    lowerExhaustiveOrArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm alternatives laterArms currentState =
      case (managedPatternArmGuard arm, scrutineeAt scrutineeCarrier currentState, orPatternInfo >>= (\nodeInfo -> buildDecision [(nodeInfo, ManagedDecisionRoot)] indexedAlternatives)) of
        (Nothing, Just rootOperand, Just decision) ->
          case compileDecision [] rootOperand decision currentState of
            (failures@(_ : _), decisionState) -> (failures, decisionState)
            ([], decisionState) ->
              case compileAlternativeBlocks decision decisionState of
                (failures@(_ : _), alternativeState) -> (failures, alternativeState)
                ([], alternativeState) -> enterCommonBody decision alternativeState
        _ -> ([patternUnsupported armIndex], currentState)
      where
        indexedAlternatives = [(index, [alternative]) | (index, alternative) <- zip [0 :: Int ..] (NonEmpty.toList alternatives)]
        orPatternInfo =
          case managedPatternArmPattern arm of
            ManagedOr nodeInfo _ -> Just nodeInfo
            _ -> Nothing
        alternativeBlockId alternativeIndex =
          patternCaseBlockId statementPath expressionPath armIndex ("alternative" <> Text.pack (show alternativeIndex))
        decisionBlockId decisionPath =
          patternCaseBlockId statementPath expressionPath armIndex ("decision" <> Text.intercalate "," (map (Text.pack . show) decisionPath))
        decisionTarget decisionPath decision =
          case decision of
            ManagedAlternativeSuccess alternativeIndex -> alternativeBlockId alternativeIndex
            _ -> decisionBlockId decisionPath
        buildDecision columns rows =
          case rows of
            [] -> Nothing
            (alternativeIndex, firstPatterns) : _
              | all decisionIrrefutable firstPatterns -> Just (ManagedAlternativeSuccess alternativeIndex)
              | Just selectedIndex <- findIndex (not . decisionIrrefutable) firstPatterns,
                Just (selectedInfo, selectedAccessor) <- valueAt selectedIndex columns,
                Just selectedPattern <- valueAt selectedIndex firstPatterns ->
                  case decisionConstructors selectedInfo of
                    Just [constructor@ManagedDecisionProduct {}] -> do
                      specializedRows <- nonEmptyRows (specializeDecisionRows selectedIndex constructor rows)
                      buildDecision (decisionChildColumns selectedAccessor constructor <> removeAt selectedIndex columns) specializedRows
                    Just constructors@(_ : _) -> do
                      branches <-
                        traverse
                          ( \constructor -> do
                              specializedRows <- nonEmptyRows (specializeDecisionRows selectedIndex constructor rows)
                              branch <- buildDecision (decisionChildColumns selectedAccessor constructor <> removeAt selectedIndex columns) specializedRows
                              case constructor of
                                ManagedDecisionVariant variant -> Just (variant, branch)
                                _ -> Nothing
                          )
                          constructors
                      Just (ManagedAlternativeVariant selectedAccessor branches)
                    _ ->
                      case selectedPattern of
                        ManagedLiteral literalInfo literal -> do
                          matchingRows <- nonEmptyRows (literalMatchingRows selectedIndex literal rows)
                          remainingRows <- nonEmptyRows (literalRemainingRows selectedIndex literal rows)
                          matchingDecision <- buildDecision (removeAt selectedIndex columns) matchingRows
                          remainingDecision <- buildDecision columns remainingRows
                          Just (ManagedAlternativeLiteral selectedAccessor literalInfo literal matchingDecision remainingDecision)
                        _ -> Nothing
            _ -> Nothing
        decisionIrrefutable patternValue =
          case patternValue of
            ManagedWildcard {} -> True
            ManagedVariable {} -> True
            ManagedAs _ _ nested -> decisionIrrefutable nested
            ManagedOr _ nestedAlternatives -> any decisionIrrefutable nestedAlternatives
            _ -> False
        decisionConstructors nodeInfo =
          case (typedNodeType nodeInfo, typedNodeRecipe nodeInfo, representationForRecipe (indexedManagedLayoutCatalog functions) (typedNodeRecipe nodeInfo)) of
            (SemanticTuple types@(_ : _), TypedManagedProductRecipe recipes, Just (LoweredManagedReferenceRepresentation layoutId))
              | length types == length recipes,
                let fieldInfos = zipWith (\typeValue recipe -> TypedNodeInfo typeValue recipe [] []) types recipes,
                Just representations <- traverse (representationForRecipe (indexedManagedLayoutCatalog functions) . typedNodeRecipe) fieldInfos ->
                  Just [ManagedDecisionProduct layoutId (zip fieldInfos representations)]
            (SemanticData {}, TypedManagedVariantRecipe {}, _) ->
              map ManagedDecisionVariant <$> managedPatternConstructorsFor (indexedManagedLayoutCatalog functions) nodeInfo
            _ -> Nothing
        decisionChildColumns accessor constructor =
          case constructor of
            ManagedDecisionProduct layoutId fields ->
              [ (fieldInfo, ManagedDecisionProductField accessor layoutId fieldIndex representation)
              | (fieldIndex, (fieldInfo, representation)) <- zip [0 ..] fields
              ]
            ManagedDecisionVariant variant ->
              let layout = managedPatternConstructorLayout variant
                  layoutId = managedConstructorLayoutId layout
                  tag = managedConstructorTag layout
               in [ (fieldInfo, ManagedDecisionVariantField accessor layoutId tag fieldIndex representation)
                  | (fieldIndex, (fieldInfo, representation)) <- zip [0 ..] (zip (managedPatternConstructorFields variant) (managedConstructorFields layout))
                  ]
        specializeDecisionRows selectedIndex constructor = concatMap specializeRow
          where
            specializeRow (alternativeIndex, patterns) =
              case valueAt selectedIndex patterns >>= specializeDecisionPattern constructor of
                Just children -> [(alternativeIndex, children <> removeAt selectedIndex patterns)]
                Nothing -> []
        specializeDecisionPattern constructor patternValue =
          case patternValue of
            ManagedWildcard _ -> Just (decisionWildcards constructor)
            ManagedVariable _ _ -> Just (decisionWildcards constructor)
            ManagedAs _ _ nested -> specializeDecisionPattern constructor nested
            ManagedTuple _ layoutId children ->
              case constructor of
                ManagedDecisionProduct expectedLayout _
                  | layoutId == expectedLayout -> Just children
                _ -> Nothing
            ManagedConstructor actual children ->
              case constructor of
                ManagedDecisionVariant expected
                  | sameDecisionConstructor expected actual -> Just children
                _ -> Nothing
            _ -> Nothing
        decisionWildcards constructor =
          case constructor of
            ManagedDecisionProduct _ fields -> [ManagedWildcard fieldInfo | (fieldInfo, _) <- fields]
            ManagedDecisionVariant variant -> map ManagedWildcard (managedPatternConstructorFields variant)
        sameDecisionConstructor left right =
          managedPatternConstructorName left == managedPatternConstructorName right
            && managedPatternConstructorLayout left == managedPatternConstructorLayout right
        literalMatchingRows selectedIndex literal = concatMap matchingRow
          where
            matchingRow (alternativeIndex, patterns) =
              case valueAt selectedIndex patterns of
                Just patternValue
                  | decisionIrrefutable patternValue -> [(alternativeIndex, removeAt selectedIndex patterns)]
                Just (ManagedLiteral _ actual)
                  | actual == literal -> [(alternativeIndex, removeAt selectedIndex patterns)]
                _ -> []
        literalRemainingRows selectedIndex literal = concatMap remainingRow
          where
            remainingRow row@(alternativeIndex, patterns) =
              case valueAt selectedIndex patterns of
                Just patternValue
                  | decisionIrrefutable patternValue -> [(alternativeIndex, removeAt selectedIndex patterns)]
                Just (ManagedLiteral _ actual)
                  | actual == literal -> []
                Just ManagedLiteral {} -> [row]
                _ -> []
        nonEmptyRows rows = if null rows then Nothing else Just rows
        removeAt index values = take index values <> drop (index + 1) values
        valueAt index values =
          case drop index values of
            value : _ -> Just value
            [] -> Nothing

        compileDecision decisionPath rootOperand decision decisionState =
          case decision of
            ManagedAlternativeSuccess alternativeIndex ->
              case ambientArguments controlSlots decisionState of
                Just arguments -> ([], finishCurrentBlock (LoweredJump (alternativeBlockId alternativeIndex) arguments) decisionState)
                Nothing -> ([unsupportedFailure path], decisionState)
            ManagedAlternativeVariant accessor branches@((firstConstructor, _) : _) ->
              case emitDecisionAccessor rootOperand accessor decisionState of
                (decisionOperand, accessedState) ->
                  case ambientArguments controlSlots accessedState of
                    Just arguments ->
                      let (_, taggedState) = emitProjection variantTagRepresentation (LoweredProjectVariantTag (managedConstructorLayoutId (managedPatternConstructorLayout firstConstructor)) decisionOperand) accessedState
                          switchCases =
                            [ LoweredSwitchCase
                                (fromIntegral (managedConstructorTag (managedPatternConstructorLayout constructor)))
                                (decisionTarget (decisionPath <> [branchIndex]) branch)
                                arguments
                            | (branchIndex, (constructor, branch)) <- zip [0 :: Int ..] branches
                            ]
                          switchedState = finishCurrentBlock (LoweredSwitch decisionOperand switchCases Nothing) taggedState
                       in compileDecisionChildren decisionPath branches switchedState
                    Nothing -> ([unsupportedFailure path], accessedState)
            ManagedAlternativeVariant _ [] -> ([unsupportedFailure path], decisionState)
            ManagedAlternativeLiteral accessor literalInfo literal matching remaining ->
              let (decisionOperand, accessedState) = emitDecisionAccessor rootOperand accessor decisionState
                  (literalFailures, maybeLiteralOperand, literalState) = lowerLiteral path literalInfo literal accessedState
               in case (literalFailures, maybeLiteralOperand, ambientArguments controlSlots literalState) of
                    ([], Just literalOperand, Just arguments)
                      | loweredOperandRepresentation literalOperand == loweredOperandRepresentation decisionOperand ->
                          let comparisonIndex = loweringNextTemporary literalState
                              comparisonTemporary = LoweredTemporaryId ("t" <> Text.pack (show comparisonIndex))
                              comparisonInstruction =
                                LoweredInstruction
                                  comparisonTemporary
                                  LoweredBoolRepresentation
                                  (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [decisionOperand, literalOperand])
                              comparedState =
                                literalState
                                  { loweringNextTemporary = comparisonIndex + 1,
                                    loweringInstructions = comparisonInstruction : loweringInstructions literalState
                                  }
                              matchingPath = decisionPath <> [0]
                              remainingPath = decisionPath <> [1]
                              branchedState =
                                finishCurrentBlock
                                  ( LoweredBranch
                                      (LoweredTemporaryOperand comparisonTemporary LoweredBoolRepresentation)
                                      (decisionTarget matchingPath matching)
                                      arguments
                                      (decisionTarget remainingPath remaining)
                                      arguments
                                  )
                                  comparedState
                           in compileLiteralChildren matchingPath matching remainingPath remaining branchedState
                    (failures@(_ : _), _, _) -> (failures, literalState)
                    _ -> ([patternUnsupported armIndex], literalState)
        compileDecisionChildren decisionPath branches initialState =
          foldl'
            ( \(failures, completedState) (branchIndex, (_, branch)) ->
                if null failures
                  then compileDecisionChild (decisionPath <> [branchIndex]) branch completedState
                  else (failures, completedState)
            )
            ([], initialState)
            (zip [0 :: Int ..] branches)
        compileLiteralChildren matchingPath matching remainingPath remaining branchedState =
          case compileDecisionChild matchingPath matching branchedState of
            (failures@(_ : _), failedState) -> (failures, failedState)
            ([], matchingState) -> compileDecisionChild remainingPath remaining matchingState
        compileDecisionChild decisionPath decision completedState =
          case decision of
            ManagedAlternativeSuccess {} -> ([], completedState)
            _ ->
              let blockBase = continuationState currentState completedState
                  blockState = remapAmbient controlSlots controlParameters (startBlock (decisionBlockId decisionPath) controlParameters blockBase)
               in case scrutineeAt scrutineeCarrier blockState of
                    Just currentRoot -> compileDecision decisionPath currentRoot decision blockState
                    Nothing -> ([unsupportedFailure path], blockState)

        compileAlternativeBlocks decision initialState = foldl' compileAlternative ([], initialState) usedAlternatives
          where
            usedAlternativeIndices = decisionAlternatives decision
            usedAlternatives = [(index, alternative) | (index, alternative) <- zip [0 :: Int ..] (NonEmpty.toList alternatives), index `elem` usedAlternativeIndices]
            compileAlternative (failures, completedState) (alternativeIndex, alternative)
              | not (null failures) = (failures, completedState)
              | otherwise =
                  let blockBase = continuationState currentState completedState
                      blockState = remapAmbient controlSlots controlParameters (startBlock (alternativeBlockId alternativeIndex) controlParameters blockBase)
                   in case (scrutineeAt scrutineeCarrier blockState, ambientArguments controlSlots blockState) of
                        (Just currentRoot, Just arguments) ->
                          case extractMatchedBinders alternative currentRoot blockState of
                            Just (binderOperands, extractedState)
                              | binderRepresentations binderOperands == expectedBinderContract ->
                                  ([], finishCurrentBlock (LoweredJump (matchedArmEntry armIndex arm) (arguments <> map snd binderOperands)) extractedState)
                            _ -> ([patternUnsupported armIndex], blockState)
                        _ -> ([unsupportedFailure path], blockState)
        decisionAlternatives decision =
          case decision of
            ManagedAlternativeSuccess alternativeIndex -> [alternativeIndex]
            ManagedAlternativeVariant _ branches -> foldl' appendUnique [] [decisionAlternatives branch | (_, branch) <- branches]
            ManagedAlternativeLiteral _ _ _ matching remaining -> appendUnique (decisionAlternatives matching) (decisionAlternatives remaining)
        appendUnique existing additions = foldl' (\values value -> if value `elem` values then values else values <> [value]) existing additions
        expectedBinderContract =
          case alternativeBinderContract (NonEmpty.head alternatives) of
            Just contract -> contract
            Nothing -> []
        alternativeBinderContract patternValue =
          traverse
            ( \(binder, binderInfo) -> do
                representation <- representationForRecipe (indexedManagedLayoutCatalog functions) (typedNodeRecipe binderInfo)
                Just (binder, representation)
            )
            (patternBindersWithInfo patternValue)
        binderRepresentations = map (\(binder, operand) -> (binder, loweredOperandRepresentation operand))
        patternBindersWithInfo patternValue =
          case patternValue of
            ManagedVariable binderInfo binder -> [(binder, binderInfo)]
            ManagedConstructor _ children -> concatMap patternBindersWithInfo children
            ManagedTuple _ _ children -> concatMap patternBindersWithInfo children
            ManagedAs binderInfo binder nested -> (binder, binderInfo) : patternBindersWithInfo nested
            ManagedOr _ nestedAlternatives -> patternBindersWithInfo (NonEmpty.head nestedAlternatives)
            _ -> []
        patternHasBinders = not . null . patternBindersWithInfo
        extractMatchedBinders patternValue operand binderState =
          case patternValue of
            ManagedWildcard {} -> Just ([], binderState)
            ManagedVariable _ binder -> Just ([(binder, operand)], binderState)
            ManagedLiteral {} -> Just ([], binderState)
            ManagedAs _ binder nested -> do
              (nestedBinders, nestedState) <- extractMatchedBinders nested operand binderState
              Just ((binder, operand) : nestedBinders, nestedState)
            ManagedTuple _ layoutId children -> do
              representations <- productLayoutFields (indexedManagedLayoutCatalog functions) layoutId
              extractChildBinders (\fieldIndex _ -> LoweredProjectField layoutId fieldIndex operand) children representations binderState
            ManagedConstructor constructor children ->
              let layout = managedPatternConstructorLayout constructor
               in extractChildBinders
                    (\fieldIndex _ -> LoweredProjectVariantField (managedConstructorLayoutId layout) (fromIntegral (managedConstructorTag layout)) fieldIndex operand)
                    children
                    (managedConstructorFields layout)
                    binderState
            ManagedOr {} -> Nothing
        extractChildBinders project children representations binderState
          | length children /= length representations = Nothing
          | otherwise = foldl' extractChild (Just ([], binderState)) (zip3 [0 :: Int ..] children representations)
          where
            extractChild Nothing _ = Nothing
            extractChild (Just (binders, childState)) (fieldIndex, child, representation)
              | patternHasBinders child = do
                  let (childOperand, projectedState) = emitProjection representation (project fieldIndex representation) childState
                  (childBinders, extractedState) <- extractMatchedBinders child childOperand projectedState
                  Just (binders <> childBinders, extractedState)
              | otherwise = Just (binders, childState)
        emitDecisionAccessor rootOperand accessor accessorState =
          case accessor of
            ManagedDecisionRoot -> (rootOperand, accessorState)
            ManagedDecisionProductField parent layoutId fieldIndex representation ->
              let (parentOperand, parentState) = emitDecisionAccessor rootOperand parent accessorState
               in emitProjection representation (LoweredProjectField layoutId fieldIndex parentOperand) parentState
            ManagedDecisionVariantField parent layoutId tag fieldIndex representation ->
              let (parentOperand, parentState) = emitDecisionAccessor rootOperand parent accessorState
               in emitProjection representation (LoweredProjectVariantField layoutId (fromIntegral tag) fieldIndex parentOperand) parentState

        enterCommonBody decision completedState =
          case expectedBinderContract of
            [] | not (null (patternBindersWithInfo (NonEmpty.head alternatives))) -> ([patternUnsupported armIndex], completedState)
            binderContract ->
              let binderParameters =
                    [LoweredParameter (LoweredParameterId ("pattern" <> Text.pack (show index))) representation | (index, (_, representation)) <- zip [1 :: Int ..] binderContract]
                  bodyParameters = controlParameters <> binderParameters
                  blockBase = continuationState currentState completedState
                  bodyState = remapAmbient controlSlots controlParameters (startBlock (matchedArmEntry armIndex arm) bodyParameters blockBase)
                  binderOperands =
                    zipWith
                      (\(binder, _) (LoweredParameter parameterId representation) -> (binder, LoweredBlockParameterOperand parameterId representation))
                      binderContract
                      binderParameters
                  scopedState = bindProjectedOperands binderOperands binderParameters bodyState
               in case decisionAlternatives decision of
                    [] -> ([patternUnsupported armIndex], bodyState)
                    _ -> lowerArmBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms currentState scopedState

    lowerConstructorArms resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters plannedArms currentState =
      case (plannedArms, scrutineeAt scrutineeCarrier currentState, ambientArguments controlSlots currentState) of
        (firstArm : _, Just scrutineeOperand, Just switchArguments)
          | Just (firstConstructor, _, _) <- constructorPattern (managedPatternArmPattern firstArm) ->
              case constructorSwitchCases switchArguments plannedArms of
                [] -> ([unsupportedFailure path], currentState)
                switchCases ->
                  let layoutId = managedConstructorLayoutId (managedPatternConstructorLayout firstConstructor)
                      tagIndex = loweringNextTemporary currentState
                      tagTemporary = LoweredTemporaryId ("t" <> Text.pack (show tagIndex))
                      tagInstruction =
                        LoweredInstruction
                          tagTemporary
                          variantTagRepresentation
                          (LoweredProjectVariantTag layoutId scrutineeOperand)
                      switchedState =
                        finishCurrentBlock
                          (LoweredSwitch scrutineeOperand switchCases Nothing)
                          currentState
                            { loweringNextTemporary = tagIndex + 1,
                              loweringInstructions = tagInstruction : loweringInstructions currentState
                            }
                   in compileConstructorRows
                        resultRepresentation
                        scrutineeCarrier
                        outerSlots
                        controlSlots
                        controlParameters
                        0
                        plannedArms
                        currentState
                        switchedState
        _ -> ([unsupportedFailure path], currentState)

    compileConstructorRows resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex remainingArms continuationTemplate completedState =
      case remainingArms of
        [] -> ([], completedState)
        arm : laterArms
          | Just (constructor, children, prefixBinders) <- constructorPattern (managedPatternArmPattern arm) ->
              let entryBlockId = constructorArmEntry armIndex children prefixBinders arm
                  entryParameters = controlParameters
                  entryBase = continuationState continuationTemplate completedState
                  entryState = remapAmbient controlSlots controlParameters (startBlock entryBlockId entryParameters entryBase)
               in case scrutineeAt scrutineeCarrier entryState of
                    Nothing -> ([unsupportedFailure path], entryState)
                    Just currentScrutinee ->
                      case projectConstructorFields constructor children currentScrutinee entryState of
                        Nothing -> ([patternUnsupported armIndex], entryState)
                        Just (projectedOperands, projectedState) ->
                          case matchPatternWork controlSlots controlParameters armIndex (ordinaryMatchBlockId armIndex) (nextConstructorEntry constructor (armIndex + 1) laterArms) (1 :: Int) [(binder, currentScrutinee) | binder <- prefixBinders] [] (zip children projectedOperands) projectedState of
                            Nothing -> ([patternUnsupported armIndex], entryState)
                            Just (nextMatchIndex, binderOperands, conditions, matchedState) ->
                              let bodyResult =
                                    if null children && null prefixBinders
                                      then lowerArmBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm [] entryState entryState
                                      else
                                        enterProjectedBody
                                          resultRepresentation
                                          scrutineeCarrier
                                          outerSlots
                                          controlSlots
                                          controlParameters
                                          armIndex
                                          arm
                                          []
                                          entryState
                                          nextMatchIndex
                                          binderOperands
                                          conditions
                                          (nextConstructorEntry constructor (armIndex + 1) laterArms)
                                          matchedState
                               in case bodyResult of
                                    (failures@(_ : _), bodyState) -> (failures, bodyState)
                                    ([], bodyState) ->
                                      compileConstructorRows
                                        resultRepresentation
                                        scrutineeCarrier
                                        outerSlots
                                        controlSlots
                                        controlParameters
                                        (armIndex + 1)
                                        laterArms
                                        continuationTemplate
                                        bodyState
        _ -> ([unsupportedFailure path], completedState)

    constructorSwitchCases switchArguments = go [] . zip [0 ..]
      where
        go _ [] = []
        go seen ((armIndex, arm) : later) =
          case constructorPattern (managedPatternArmPattern arm) of
            Just (constructor, children, prefixBinders)
              | tag `elem` seen -> go seen later
              | otherwise ->
                  LoweredSwitchCase tag (constructorArmEntry armIndex children prefixBinders arm) switchArguments
                    : go (tag : seen) later
              where
                tag = fromIntegral (managedConstructorTag (managedPatternConstructorLayout constructor))
            Nothing -> go seen later

    constructorArmEntry armIndex children prefixBinders arm
      | null children && null prefixBinders = matchedArmEntry armIndex arm
      | otherwise = patternCaseBlockId statementPath expressionPath armIndex "selected"

    projectConstructorFields constructor children scrutineeOperand currentState =
      let layout = managedPatternConstructorLayout constructor
          layoutId = managedConstructorLayoutId layout
          tag = fromIntegral (managedConstructorTag layout)
          representations = managedConstructorFields layout
       in if length children /= length representations
            then Nothing
            else Just (emitProjections (zipWith (projectVariantField layoutId tag scrutineeOperand) [0 ..] representations) currentState)

    matchPatternWork controlSlots controlParameters armIndex matchBlockFor maybeFailureBlock nextMatchIndex binderOperands conditions work currentState =
      case work of
        [] -> Just (nextMatchIndex, binderOperands, conditions, currentState)
        (patternValue, operand) : laterWork ->
          case patternValue of
            ManagedWildcard _ ->
              matchPatternWork controlSlots controlParameters armIndex matchBlockFor maybeFailureBlock nextMatchIndex binderOperands conditions laterWork currentState
            ManagedVariable _ binder ->
              matchPatternWork controlSlots controlParameters armIndex matchBlockFor maybeFailureBlock nextMatchIndex (binderOperands <> [(binder, operand)]) conditions laterWork currentState
            ManagedLiteral literalInfo literal ->
              matchPatternWork controlSlots controlParameters armIndex matchBlockFor maybeFailureBlock nextMatchIndex binderOperands (conditions <> [(literalInfo, literal, operand)]) laterWork currentState
            ManagedTuple _ layoutId children -> do
              representations <- productLayoutFields (indexedManagedLayoutCatalog functions) layoutId
              if length children /= length representations
                then Nothing
                else
                  let (projectedOperands, projectedState) =
                        emitProjections
                          (zipWith (projectProductField layoutId operand) [0 ..] representations)
                          currentState
                   in matchPatternWork
                        controlSlots
                        controlParameters
                        armIndex
                        matchBlockFor
                        maybeFailureBlock
                        nextMatchIndex
                        binderOperands
                        conditions
                        (zip children projectedOperands <> laterWork)
                        projectedState
            ManagedConstructor constructor children -> do
              controlArguments <- ambientArguments controlSlots currentState
              let layout = managedPatternConstructorLayout constructor
                  layoutId = managedConstructorLayoutId layout
                  tag = fromIntegral (managedConstructorTag layout)
                  representations = managedConstructorFields layout
              switchDefault <-
                case managedLayoutShapeFor (indexedManagedLayoutCatalog functions) layoutId of
                  Just (LoweredVariantLayouts [LoweredVariantLayout onlyTag _])
                    | onlyTag == tag -> Just Nothing
                  _ -> do
                    failureBlockId <- maybeFailureBlock
                    Just (Just (LoweredSwitchDefault failureBlockId controlArguments))
              if length children /= length representations
                then Nothing
                else
                  let matchBlockId = matchBlockFor nextMatchIndex
                      pendingBinderParameters =
                        [ LoweredParameter (LoweredParameterId ("pending" <> Text.pack (show index))) (loweredOperandRepresentation binderOperand)
                        | (index, (_, binderOperand)) <- zip [1 :: Int ..] binderOperands
                        ]
                      conditionParameters =
                        [ LoweredParameter (LoweredParameterId ("condition" <> Text.pack (show index))) (loweredOperandRepresentation conditionOperand)
                        | (index, (_, _, conditionOperand)) <- zip [1 :: Int ..] conditions
                        ]
                      matchedOperands = operand : map snd laterWork
                      matchParameters =
                        [ LoweredParameter (LoweredParameterId ("match" <> Text.pack (show index))) (loweredOperandRepresentation matchedOperand)
                        | (index, matchedOperand) <- zip [1 :: Int ..] matchedOperands
                        ]
                      successArguments =
                        controlArguments
                          <> map snd binderOperands
                          <> [conditionOperand | (_, _, conditionOperand) <- conditions]
                          <> matchedOperands
                      tagIndex = loweringNextTemporary currentState
                      tagTemporary = LoweredTemporaryId ("t" <> Text.pack (show tagIndex))
                      taggedState =
                        currentState
                          { loweringNextTemporary = tagIndex + 1,
                            loweringInstructions =
                              LoweredInstruction tagTemporary variantTagRepresentation (LoweredProjectVariantTag layoutId operand)
                                : loweringInstructions currentState
                          }
                      switchedState =
                        finishCurrentBlock
                          ( LoweredSwitch
                              operand
                              [LoweredSwitchCase tag matchBlockId successArguments]
                              switchDefault
                          )
                          taggedState
                      matchState =
                        remapAmbient
                          controlSlots
                          controlParameters
                          (startBlock matchBlockId (controlParameters <> pendingBinderParameters <> conditionParameters <> matchParameters) switchedState)
                      pendingBinderOperands =
                        zipWith
                          (\(binder, _) (LoweredParameter parameterId representation) -> (binder, LoweredBlockParameterOperand parameterId representation))
                          binderOperands
                          pendingBinderParameters
                      pendingConditions =
                        zipWith
                          (\(literalInfo, literal, _) (LoweredParameter parameterId representation) -> (literalInfo, literal, LoweredBlockParameterOperand parameterId representation))
                          conditions
                          conditionParameters
                      matchedBlockOperands =
                        [LoweredBlockParameterOperand parameterId representation | LoweredParameter parameterId representation <- matchParameters]
                   in case matchedBlockOperands of
                        matchedOperand : laterMatchedOperands ->
                          let (projectedOperands, projectedState) =
                                emitProjections
                                  (zipWith (projectVariantField layoutId tag matchedOperand) [0 ..] representations)
                                  matchState
                           in matchPatternWork
                                controlSlots
                                controlParameters
                                armIndex
                                matchBlockFor
                                maybeFailureBlock
                                (nextMatchIndex + 1)
                                pendingBinderOperands
                                pendingConditions
                                (zip children projectedOperands <> zip (map fst laterWork) laterMatchedOperands)
                                projectedState
                        [] -> Nothing
            ManagedAs _ binder nested ->
              matchPatternWork controlSlots controlParameters armIndex matchBlockFor maybeFailureBlock nextMatchIndex (binderOperands <> [(binder, operand)]) conditions ((nested, operand) : laterWork) currentState
            ManagedOr {} -> Nothing

    emitProjections projections initialState =
      let step (accumulatedOperands, nextState) project =
            let (projectedOperand, nextProjectedState) = project nextState
             in (projectedOperand : accumulatedOperands, nextProjectedState)
          (finalReversedOperands, finalProjectedState) = foldl' step ([], initialState) projections
       in (reverse finalReversedOperands, finalProjectedState)

    projectProductField layoutId operand fieldIndex representation currentState =
      emitProjection representation (LoweredProjectField layoutId fieldIndex operand) currentState

    projectVariantField layoutId tag operand fieldIndex representation currentState =
      emitProjection representation (LoweredProjectVariantField layoutId tag fieldIndex operand) currentState

    emitProjection representation operation currentState =
      let temporaryIndex = loweringNextTemporary currentState
          temporaryId = LoweredTemporaryId ("t" <> Text.pack (show temporaryIndex))
          instruction = LoweredInstruction temporaryId representation operation
       in ( LoweredTemporaryOperand temporaryId representation,
            currentState
              { loweringNextTemporary = temporaryIndex + 1,
                loweringInstructions = instruction : loweringInstructions currentState
              }
          )

    enterProjectedBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm laterArms continuationTemplate nextMatchIndex binderOperands conditions maybeFailureBlock projectedState =
      case ambientArguments controlSlots projectedState of
        Nothing -> ([unsupportedFailure path], projectedState)
        Just _ ->
          let bodyBlockId = matchedArmEntry armIndex arm
              binderParameters =
                [ LoweredParameter (LoweredParameterId ("pattern" <> Text.pack (show index))) (loweredOperandRepresentation operand)
                | (index, (_, operand)) <- zip [1 :: Int ..] binderOperands
                ]
              bodyParameters = controlParameters <> binderParameters
              lowerBody currentBinders bodyState =
                case arm of
                  ManagedPatternArm _ (Just guard) _ ->
                    lowerProjectedGuard currentBinders binderParameters bodyState guard
                  _ ->
                    lowerArmBody
                      resultRepresentation
                      scrutineeCarrier
                      outerSlots
                      controlSlots
                      controlParameters
                      armIndex
                      arm
                      laterArms
                      continuationTemplate
                      bodyState
              lowerProjectedGuard currentBinders currentBinderParameters guardState guard =
                let (guardFailures, maybeGuardOperand, loweredGuardState) =
                      lowerExpression
                        modulePath
                        statementPath
                        (0 : armIndex + 1 : expressionPath)
                        functions
                        parameters
                        guardState
                        guard
                    binderArguments =
                      [LoweredBlockParameterOperand parameterId representation | LoweredParameter parameterId representation <- currentBinderParameters]
                 in case (guardFailures, maybeGuardOperand, maybeFailureBlock, ambientArguments controlSlots loweredGuardState) of
                      ([], Just guardOperand, Just failureBlockId, Just currentControlArguments)
                        | loweredOperandRepresentation guardOperand == LoweredBoolRepresentation ->
                            let actualBodyBlockId = patternCaseBlockId statementPath expressionPath armIndex "body"
                                branchState =
                                  finishCurrentBlock
                                    ( LoweredBranch
                                        guardOperand
                                        actualBodyBlockId
                                        (currentControlArguments <> binderArguments)
                                        failureBlockId
                                        currentControlArguments
                                    )
                                    loweredGuardState
                                actualBodyState =
                                  bindProjectedOperands
                                    currentBinders
                                    currentBinderParameters
                                    (remapAmbient controlSlots controlParameters (startBlock actualBodyBlockId bodyParameters branchState))
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
                                  actualBodyState
                      (failures@(_ : _), _, _, _) -> (failures, loweredGuardState)
                      _ -> ([unsupportedFailure path], loweredGuardState)
              lowerConditions matchIndex currentBinders remainingConditions conditionState =
                case ambientArguments controlSlots conditionState of
                  Nothing -> ([unsupportedFailure path], conditionState)
                  Just currentControlArguments ->
                    case remainingConditions of
                      [] ->
                        let currentBodyArguments = currentControlArguments <> map snd currentBinders
                         in lowerBody currentBinders (startBodyWith currentBinders (finishCurrentBlock (LoweredJump bodyBlockId currentBodyArguments) conditionState))
                      (literalInfo, literal, comparedOperand) : laterConditions ->
                        case maybeFailureBlock of
                          Nothing -> ([patternUnsupported armIndex], conditionState)
                          Just failureBlockId ->
                            let (literalFailures, maybeLiteralOperand, literalState) = lowerLiteral path literalInfo literal conditionState
                             in case (literalFailures, maybeLiteralOperand) of
                                  ([], Just literalOperand)
                                    | loweredOperandRepresentation literalOperand == loweredOperandRepresentation comparedOperand ->
                                        let comparisonIndex = loweringNextTemporary literalState
                                            comparisonTemporary = LoweredTemporaryId ("t" <> Text.pack (show comparisonIndex))
                                            comparisonInstruction =
                                              LoweredInstruction
                                                comparisonTemporary
                                                LoweredBoolRepresentation
                                                (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [comparedOperand, literalOperand])
                                            comparisonState =
                                              literalState
                                                { loweringNextTemporary = comparisonIndex + 1,
                                                  loweringInstructions = comparisonInstruction : loweringInstructions literalState
                                                }
                                            conditionOperand = LoweredTemporaryOperand comparisonTemporary LoweredBoolRepresentation
                                         in case laterConditions of
                                              [] ->
                                                let currentBodyArguments = currentControlArguments <> map snd currentBinders
                                                    finishedState =
                                                      finishCurrentBlock
                                                        (LoweredBranch conditionOperand bodyBlockId currentBodyArguments failureBlockId currentControlArguments)
                                                        comparisonState
                                                 in lowerBody currentBinders (startBodyWith currentBinders finishedState)
                                              _ ->
                                                let matchBlockId = patternCaseBlockId statementPath expressionPath armIndex ("match" <> Text.pack (show matchIndex))
                                                    pendingBinderParameters =
                                                      [ LoweredParameter (LoweredParameterId ("pending" <> Text.pack (show index))) (loweredOperandRepresentation binderOperand)
                                                      | (index, (_, binderOperand)) <- zip [1 :: Int ..] currentBinders
                                                      ]
                                                    matchParameters =
                                                      [ LoweredParameter (LoweredParameterId ("match" <> Text.pack (show index))) (loweredOperandRepresentation remainingOperand)
                                                      | (index, (_, _, remainingOperand)) <- zip [1 :: Int ..] laterConditions
                                                      ]
                                                    matchArguments =
                                                      currentControlArguments
                                                        <> map snd currentBinders
                                                        <> [remainingOperand | (_, _, remainingOperand) <- laterConditions]
                                                    finishedState =
                                                      finishCurrentBlock
                                                        (LoweredBranch conditionOperand matchBlockId matchArguments failureBlockId currentControlArguments)
                                                        comparisonState
                                                    matchState =
                                                      remapAmbient
                                                        controlSlots
                                                        controlParameters
                                                        (startBlock matchBlockId (controlParameters <> pendingBinderParameters <> matchParameters) finishedState)
                                                    pendingBinders =
                                                      zipWith
                                                        (\(binder, _) (LoweredParameter parameterId representation) -> (binder, LoweredBlockParameterOperand parameterId representation))
                                                        currentBinders
                                                        pendingBinderParameters
                                                    pendingConditions =
                                                      zipWith
                                                        (\(remainingInfo, remainingLiteral, _) (LoweredParameter parameterId representation) -> (remainingInfo, remainingLiteral, LoweredBlockParameterOperand parameterId representation))
                                                        laterConditions
                                                        matchParameters
                                                 in lowerConditions (matchIndex + 1) pendingBinders pendingConditions matchState
                                  (failures@(_ : _), _) -> (failures, literalState)
                                  _ -> ([patternUnsupported armIndex], literalState)

              startBodyWith currentBinders finishedState =
                bindProjectedOperands
                  currentBinders
                  binderParameters
                  (remapAmbient controlSlots controlParameters (startBlock bodyBlockId bodyParameters finishedState))
           in lowerConditions nextMatchIndex binderOperands conditions projectedState

    nextConstructorEntry constructor nextArmIndex = go nextArmIndex
      where
        expectedTag = managedConstructorTag (managedPatternConstructorLayout constructor)
        go _ [] = Nothing
        go index (arm : later) =
          case constructorPattern (managedPatternArmPattern arm) of
            Just (laterConstructor, children, prefixBinders)
              | managedConstructorTag (managedPatternConstructorLayout laterConstructor) == expectedTag ->
                  Just (constructorArmEntry index children prefixBinders arm)
            _ -> go (index + 1) later

    nonEmptyHead values =
      case values of
        value : _ -> Just value
        [] -> Nothing

    ordinaryMatchBlockId armIndex matchIndex =
      patternCaseBlockId statementPath expressionPath armIndex ("match" <> Text.pack (show matchIndex))

    bindProjectedOperands binderOperands binderParameters currentState =
      currentState
        { loweringLocalBindings =
            foldl'
              (\bindings ((binder, _), LoweredParameter parameterId representation) -> Map.insert binder (LoweredBlockParameterOperand parameterId representation) bindings)
              (loweringLocalBindings currentState)
              (zip binderOperands binderParameters)
        }

    patternUnsupported armIndex =
      LoweredIRLoweringFailure
        (TypedPatternPath modulePath statementPath (reverse expressionPath <> [armIndex]))
        LoweredIRUnsupportedPattern
        LoweredIRNoFailureDetail

    variantTagRepresentation = LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64

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

    lowerMatchedArm resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex arm@(ManagedPatternArm patternValue maybeGuard _) laterArms currentState =
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

    lowerArmBody resultRepresentation scrutineeCarrier outerSlots controlSlots controlParameters armIndex (ManagedPatternArm _ _ body) laterArms continuationTemplate bodyState =
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

    armEntryBlock armIndex arm@(ManagedPatternArm patternValue _ _) =
      case patternValue of
        ManagedLiteral {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        ManagedTuple {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        ManagedConstructor {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        ManagedAs {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        ManagedOr {} -> patternCaseBlockId statementPath expressionPath armIndex "test"
        _ -> matchedArmEntry armIndex arm

    matchedArmEntry armIndex (ManagedPatternArm _ maybeGuard _) =
      patternCaseBlockId
        statementPath
        expressionPath
        armIndex
        (case maybeGuard of Just _ -> "guard"; Nothing -> "body")

    bindPattern patternValue scrutineeOperand currentState =
      case patternValue of
        ManagedVariable _ binder ->
          currentState
            { loweringLocalBindings =
                Map.insert binder scrutineeOperand (loweringLocalBindings currentState)
            }
        _ -> currentState

    bindCurrentPattern (ManagedPatternArm patternValue _ _) scrutineeCarrier currentState =
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
