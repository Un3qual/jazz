{-# LANGUAGE OverloadedStrings #-}

-- | Validated lowering from the first typed-core scalar profile into the
-- permanent backend-neutral lowered IR.
module JazzNext.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringResult (..),
    lowerTypedCoreExpressionDirectCall,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.LoweredIR
import JazzNext.Compiler.LoweredIR.Validate (validateLoweredProgram)
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
import Text.Read (readMaybe)

data LoweredIRLoweringKind
  = LoweredIRUnsupportedProgram
  | LoweredIRUnsupportedModule
  | LoweredIRUnsupportedStatement
  | LoweredIRUnsupportedExpression
  | LoweredIRUnsupportedRepresentation
  | LoweredIRUnsupportedOperator
  deriving (Eq, Show)

data LoweredIRLoweringDetail
  = LoweredIRNoFailureDetail
  | LoweredIRRecipeFailureDetail TypedRepresentationRecipe
  | LoweredIROperatorFailureDetail TypedOperatorRef
  deriving (Eq, Show)

data LoweredIRLoweringFailure
  = LoweredIRLoweringFailure
      TypedCoreValidationPath
      LoweredIRLoweringKind
      LoweredIRLoweringDetail
  deriving (Eq, Show)

data LoweredIRLoweringResult
  = LoweredIRTypedCoreFailures [TypedCoreValidationFailure]
  | LoweredIRUnsupported [LoweredIRLoweringFailure]
  | LoweredIRInvariantFailures [LoweredIRValidationFailure]
  | LoweredIRSucceeded LoweredProgram
  deriving (Eq, Show)

data LoweringState = LoweringState
  { loweringFunctionId :: LoweredFunctionId,
    loweringNextTemporary :: Int,
    loweringInstructions :: [LoweredInstruction]
  }

lowerTypedCoreExpressionDirectCall :: TypedProgram -> LoweredIRLoweringResult
lowerTypedCoreExpressionDirectCall typedProgram =
  case validateTypedProgram typedProgram of
    failures@(_ : _) -> LoweredIRTypedCoreFailures failures
    [] ->
      case lowerValidatedProgram typedProgram of
        Left failures -> LoweredIRUnsupported failures
        Right loweredProgram ->
          case validateLoweredProgram loweredProgram of
            failures@(_ : _) -> LoweredIRInvariantFailures failures
            [] -> LoweredIRSucceeded loweredProgram

lowerValidatedProgram :: TypedProgram -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedProgram (TypedProgram maybePrelude modules entryModulePath) =
  case (maybePrelude, modules) of
    (Nothing, [moduleValue@(TypedModule modulePath _ _ _ _ _ _)])
      | modulePath == entryModulePath -> lowerValidatedModule moduleValue
    _ ->
      Left
        [ LoweredIRLoweringFailure
            TypedProgramPath
            LoweredIRUnsupportedProgram
            LoweredIRNoFailureDetail
        ]

lowerValidatedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedModule (TypedModule modulePath _ imports exports moduleInterface statements moduleInfo) =
  case moduleFailures <> resultRepresentationFailures <> statementFailures of
    failures@(_ : _) -> Left failures
    [] ->
      case (maybeResultRepresentation, maybeResultOperand) of
        (Just resultRepresentation, Just resultOperand) ->
          Right
            ( LoweredProgram
                supportedLoweredIRVersion
                []
                []
                [ LoweredFunction
                    (loweringFunctionId finalState)
                    Nothing
                    []
                    resultRepresentation
                    [ LoweredBlock
                        entryBlockId
                        []
                        (loweringInstructions finalState)
                        (Just (LoweredReturn resultOperand))
                    ]
                    entryBlockId
                ]
                (loweringFunctionId finalState)
            )
        _ ->
          Left
            [ LoweredIRLoweringFailure
                (TypedModulePath modulePath)
                LoweredIRUnsupportedModule
                LoweredIRNoFailureDetail
            ]
  where
    entryFunctionId =
      LoweredFunctionId (Text.intercalate "::" (modulePath <> ["$entry"]))
    entryBlockId = LoweredBlockId "entry"
    initialState =
      LoweringState
        { loweringFunctionId = entryFunctionId,
          loweringNextTemporary = 1,
          loweringInstructions = []
        }
    moduleFailures
      | null imports,
        null exports,
        emptyModuleInterface moduleInterface =
          []
      | otherwise =
          [ LoweredIRLoweringFailure
              (TypedModulePath modulePath)
              LoweredIRUnsupportedModule
              LoweredIRNoFailureDetail
          ]
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (TypedModulePath modulePath) (nodeRecipe moduleInfo)
    (statementFailures, maybeResultOperand, finalState) =
      lowerStatements modulePath initialState statements

emptyModuleInterface :: TypedModuleInterface -> Bool
emptyModuleInterface (TypedModuleInterface values datas classes impls) =
  null values && null datas && null classes && null impls

lowerStatements ::
  [Text] ->
  LoweringState ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerStatements modulePath =
  go 0 [] Nothing
  where
    go _ failures resultOperand state [] = (failures, resultOperand, state)
    go statementIndex failures resultOperand state (statement : rest) =
      case statement of
        TypedExpressionStatement _ expression ->
          let (expressionFailures, maybeOperand, nextState) =
                lowerExpression
                  modulePath
                  [statementIndex]
                  []
                  state
                  expression
           in go
                (statementIndex + 1)
                (failures <> expressionFailures)
                maybeOperand
                nextState
                rest
        _ ->
          go
            (statementIndex + 1)
            ( failures
                <> [ LoweredIRLoweringFailure
                       (TypedStatementPath modulePath [statementIndex])
                       LoweredIRUnsupportedStatement
                       LoweredIRNoFailureDetail
                   ]
            )
            resultOperand
            state
            rest

lowerExpression ::
  [Text] ->
  [Int] ->
  [Int] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerExpression modulePath statementPath expressionPath state expression =
  case expression of
    TypedLiteralExpr info literal ->
      lowerLiteral path info literal state
    TypedVariableExpr {} -> unsupportedExpression path state
    TypedTupleExpr info [] ->
      case nodeRecipe info of
        TypedUnitRecipe ->
          ([], Just (LoweredImmediateOperand LoweredUnitImmediate), state)
        recipe -> unsupportedRepresentation path recipe state
    TypedBinaryExpr info operator left right ->
      lowerBinary
        modulePath
        statementPath
        expressionPath
        path
        info
        operator
        left
        right
        state
    _ -> unsupportedExpression path state
  where
    path = TypedExpressionPath modulePath statementPath expressionPath

lowerLiteral ::
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedLiteral ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerLiteral path info literal state =
  case (literal, nodeRecipe info) of
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
        Nothing -> unsupportedRepresentation path (nodeRecipe info) state
    _ -> unsupportedRepresentation path (nodeRecipe info) state
  where
    loweredImmediate immediate =
      ([], Just (LoweredImmediateOperand immediate), state)
    lowerInteger source maybeConstructor =
      case (readMaybe (Text.unpack source), maybeConstructor) of
        (Just value, Just constructor) -> loweredImmediate (constructor value)
        _ -> unsupportedRepresentation path (nodeRecipe info) state

lowerBinary ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  TypedNodeInfo ->
  TypedOperatorRef ->
  TypedExpr ->
  TypedExpr ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerBinary modulePath statementPath expressionPath path info operator left right state =
  case operatorFailures <> resultRepresentationFailures <> leftFailures <> rightFailures of
    failures@(_ : _) -> (failures, Nothing, rightState)
    [] ->
      case (maybePrimitive, maybeResultRepresentation, maybeLeftOperand, maybeRightOperand) of
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
                    loweringInstructions = loweringInstructions rightState <> [instruction]
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
      representationAtPath path (nodeRecipe info)
    (leftFailures, maybeLeftOperand, leftState) =
      lowerExpression
        modulePath
        statementPath
        (expressionPath <> [0])
        state
        left
    (rightFailures, maybeRightOperand, rightState) =
      lowerExpression
        modulePath
        statementPath
        (expressionPath <> [1])
        leftState
        right

loweredPrimitive :: TypedOperatorRef -> Maybe LoweredPrimitive
loweredPrimitive operator =
  case operator of
    TypedBuiltinOperator "+" -> arithmetic LoweredAdd
    TypedBuiltinOperator "-" -> arithmetic LoweredSubtract
    TypedBuiltinOperator "*" -> arithmetic LoweredMultiply
    TypedBuiltinOperator "/" -> arithmetic LoweredDivide
    TypedBuiltinOperator "<" -> comparison LoweredLessThan
    TypedBuiltinOperator "<=" -> comparison LoweredLessThanOrEqual
    TypedBuiltinOperator ">" -> comparison LoweredGreaterThan
    TypedBuiltinOperator ">=" -> comparison LoweredGreaterThanOrEqual
    TypedBuiltinOperator "==" -> comparison LoweredEqual
    TypedBuiltinOperator "!=" -> comparison LoweredNotEqual
    _ -> Nothing
  where
    arithmetic = Just . LoweredArithmeticPrimitive
    comparison = Just . LoweredComparisonPrimitive

representationAtPath ::
  TypedCoreValidationPath ->
  TypedRepresentationRecipe ->
  ([LoweredIRLoweringFailure], Maybe LoweredRepresentation)
representationAtPath path recipe =
  case loweredRepresentation recipe of
    Just representation -> ([], Just representation)
    Nothing ->
      ( [ LoweredIRLoweringFailure
            path
            LoweredIRUnsupportedRepresentation
            (LoweredIRRecipeFailureDetail recipe)
        ],
        Nothing
      )

loweredRepresentation :: TypedRepresentationRecipe -> Maybe LoweredRepresentation
loweredRepresentation recipe =
  case recipe of
    TypedUnitRecipe -> Just LoweredUnitRepresentation
    TypedBoolRecipe -> Just LoweredBoolRepresentation
    TypedSignedIntegerRecipe bits ->
      LoweredSignedIntegerRepresentation <$> integerWidth bits
    TypedUnsignedIntegerRecipe bits ->
      LoweredUnsignedIntegerRepresentation <$> integerWidth bits
    TypedFloatRecipe bits ->
      LoweredFloatRepresentation <$> floatWidth bits
    TypedCharRecipe -> Just LoweredCharRepresentation
    _ -> Nothing

integerWidth :: Int -> Maybe LoweredIntegerWidth
integerWidth bits =
  case bits of
    8 -> Just LoweredIntegerWidth8
    16 -> Just LoweredIntegerWidth16
    32 -> Just LoweredIntegerWidth32
    64 -> Just LoweredIntegerWidth64
    _ -> Nothing

floatWidth :: Int -> Maybe LoweredFloatWidth
floatWidth bits =
  case bits of
    16 -> Just LoweredFloatWidth16
    32 -> Just LoweredFloatWidth32
    64 -> Just LoweredFloatWidth64
    _ -> Nothing

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

nodeRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
nodeRecipe (TypedNodeInfo _ recipe _ _) = recipe
