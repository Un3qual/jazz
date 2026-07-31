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

import Data.List (find, nub)
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
  | LoweredIRInvalidFunctionShape
  | LoweredIRCaptureUnsupported
  | LoweredIRRecursiveFunctionUnsupported
  | LoweredIRCallableValueUnsupported
  | LoweredIRCallArityUnsupported
  | LoweredIRNonLocalCallUnsupported
  deriving (Eq, Show)

data LoweredIRLoweringDetail
  = LoweredIRNoFailureDetail
  | LoweredIRRecipeFailureDetail TypedRepresentationRecipe
  | LoweredIROperatorFailureDetail TypedOperatorRef
  | LoweredIRNameFailureDetail TypedCoreName
  | LoweredIRArityFailureDetail Int Int
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

data FunctionParameterShape = FunctionParameterShape
  { functionParameterName :: TypedCoreName,
    functionParameter :: LoweredParameter
  }

data FunctionShape = FunctionShape
  { functionShapeName :: TypedCoreName,
    functionShapeId :: LoweredFunctionId,
    functionShapeStatementIndex :: Int,
    functionShapeParameters :: [FunctionParameterShape],
    functionShapeResultRepresentation :: LoweredRepresentation,
    functionShapeBodyPath :: [Int],
    functionShapeBody :: TypedExpr
  }

data ExpressionCheck = ExpressionCheck
  { expressionCheckFailures :: [LoweredIRLoweringFailure],
    expressionCheckCalls :: [TypedCoreName]
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
    typedModulePath (TypedModule modulePath _ _ _ _ _ _) = modulePath

lowerValidatedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedModule (TypedModule modulePath _ imports exports moduleInterface statements moduleInfo) =
  case allFailures of
    failures@(_ : _) -> Left failures
    [] ->
      emitProgram
  where
    entryFunctionId =
      LoweredFunctionId (Text.intercalate "::" (modulePath <> ["$entry"]))
    entryBlockId = LoweredBlockId "entry"
    (shapeFailures, functionShapes, localValueNames) =
      collectFunctionShapes modulePath statements
    moduleFailures
      | supportedModuleMetadata imports exports moduleInterface functionShapes =
          []
      | otherwise =
          [ LoweredIRLoweringFailure
              (TypedModulePath modulePath)
              LoweredIRUnsupportedModule
              LoweredIRNoFailureDetail
          ]
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath (TypedModulePath modulePath) (nodeRecipe moduleInfo)
    (profileFailures, functionCalls) =
      validateStatementProfiles modulePath functionShapes localValueNames statements
    recursiveFailures =
      recursiveFunctionFailures modulePath functionShapes functionCalls
    allFailures =
      moduleFailures
        <> resultRepresentationFailures
        <> shapeFailures
        <> profileFailures
        <> recursiveFailures
    emitProgram =
      case
          ( maybeResultRepresentation,
            traverse (emitFunction modulePath functionShapes) functionShapes,
            emitEntry modulePath entryFunctionId functionShapes statements
          )
        of
          (Just resultRepresentation, Right functions, Right (resultOperand, finalState)) ->
            Right
              ( LoweredProgram
                  supportedLoweredIRVersion
                  []
                  []
                  ( functions
                      <> [ LoweredFunction
                             entryFunctionId
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
                  )
                  entryFunctionId
              )
          (_, Left failures, _) -> Left failures
          (_, _, Left failures) -> Left failures
          _ ->
            Left
              [ LoweredIRLoweringFailure
                  (TypedModulePath modulePath)
                  LoweredIRUnsupportedModule
                  LoweredIRNoFailureDetail
              ]

supportedModuleMetadata ::
  [TypedResolvedImport] ->
  [TypedModuleExport] ->
  TypedModuleInterface ->
  [FunctionShape] ->
  Bool
supportedModuleMetadata imports exports (TypedModuleInterface values datas classes impls) functions =
  null imports
    && null datas
    && null classes
    && null impls
    && all supportedExport exports
    && all supportedInterfaceValue values
  where
    supportedExport (TypedModuleExport namespace identifier) =
      namespace == TypedValueNamespace
        && any (matchesIdentifier identifier . functionShapeName) functions
    supportedInterfaceValue (TypedValueInterface name _) =
      any ((== name) . functionShapeName) functions
    matchesIdentifier identifier name =
      case name of
        TypedResolvedName TypedCurrentModule TypedValueNamespace candidate ->
          identifier == candidate
        _ -> False

collectFunctionShapes ::
  [Text] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [FunctionShape], [TypedCoreName])
collectFunctionShapes modulePath =
  go 0 [] [] []
  where
    go _ failures functions localNames [] = (failures, functions, localNames)
    go statementIndex failures functions localNames (statement : rest) =
      case statement of
        TypedSignatureStatement {} ->
          continue failures functions localNames
        TypedLetStatement _ name _ scheme expression ->
          case collectFunctionShape modulePath statementIndex name scheme expression of
            Just function ->
              continue failures (functions <> [function]) (localNames <> [name])
            Nothing ->
              continue
                ( failures
                    <> [ LoweredIRLoweringFailure
                           (TypedStatementPath modulePath [statementIndex])
                           LoweredIRInvalidFunctionShape
                           (LoweredIRNameFailureDetail name)
                       ]
                )
                functions
                (localNames <> [name])
        TypedExpressionStatement {} ->
          continue failures functions localNames
        _ ->
          continue
            ( failures
                <> [ LoweredIRLoweringFailure
                       (TypedStatementPath modulePath [statementIndex])
                       LoweredIRUnsupportedStatement
                       LoweredIRNoFailureDetail
                   ]
            )
            functions
            localNames
      where
        continue nextFailures nextFunctions nextLocalNames =
          go
            (statementIndex + 1)
            nextFailures
            nextFunctions
            nextLocalNames
            rest

collectFunctionShape ::
  [Text] ->
  Int ->
  TypedCoreName ->
  TypedScheme ->
  TypedExpr ->
  Maybe FunctionShape
collectFunctionShape modulePath statementIndex name scheme expression = do
  identifier <- localValueIdentifier name
  (schemeType, schemeRecipe) <- monomorphicSchemeContract scheme
  (parameters, resultRepresentation, bodyPath, body) <-
    flattenLeadingLambdas schemeType schemeRecipe [0] [] expression
  if null parameters || duplicateParameterNames parameters
    then Nothing
    else
      Just
        FunctionShape
          { functionShapeName = name,
            functionShapeId =
              LoweredFunctionId
                (Text.intercalate "::" (modulePath <> [identifier])),
            functionShapeStatementIndex = statementIndex,
            functionShapeParameters = parameters,
            functionShapeResultRepresentation = resultRepresentation,
            functionShapeBodyPath = bodyPath,
            functionShapeBody = body
          }
  where
    duplicateParameterNames parameters =
      let names = map functionParameterName parameters
       in length names /= length (nub names)

localValueIdentifier :: TypedCoreName -> Maybe Text
localValueIdentifier name =
  case name of
    TypedResolvedName TypedCurrentModule TypedValueNamespace identifier -> Just identifier
    _ -> Nothing

monomorphicSchemeContract :: TypedScheme -> Maybe (TypedType, TypedRepresentationRecipe)
monomorphicSchemeContract (TypedScheme _ typeParameters evidence primitive typeValue recipe)
  | null typeParameters,
    null evidence,
    null primitive =
      Just (typeValue, recipe)
  | otherwise = Nothing

flattenLeadingLambdas ::
  TypedType ->
  TypedRepresentationRecipe ->
  [Int] ->
  [FunctionParameterShape] ->
  TypedExpr ->
  Maybe ([FunctionParameterShape], LoweredRepresentation, [Int], TypedExpr)
flattenLeadingLambdas expectedType expectedRecipe expressionPath reversedParameters expression =
  case expression of
    TypedLambdaExpr info _ parameterName body -> do
      if nodeType info == expectedType && nodeRecipe info == expectedRecipe
        then pure ()
        else Nothing
      (argumentType, resultType) <-
        case expectedType of
          TypedFunctionType argument result -> Just (argument, result)
          _ -> Nothing
      (argumentRecipe, resultRecipe) <-
        case expectedRecipe of
          TypedClosureRecipe (argument : rest) result ->
            Just
              ( argument,
                case rest of
                  [] -> result
                  _ -> TypedClosureRecipe rest result
              )
          _ -> Nothing
      parameterRepresentation <- scalarRepresentation argumentType argumentRecipe
      let parameterIndex = length reversedParameters + 1
          parameter =
            FunctionParameterShape
              { functionParameterName = parameterName,
                functionParameter =
                  LoweredParameter
                    (LoweredParameterId ("arg" <> Text.pack (show parameterIndex)))
                    parameterRepresentation
              }
      flattenLeadingLambdas
        resultType
        resultRecipe
        (expressionPath <> [0])
        (reversedParameters <> [parameter])
        body
    _ -> do
      resultRepresentation <- scalarRepresentation expectedType expectedRecipe
      if nodeType (expressionInfo expression) == expectedType
          && nodeRecipe (expressionInfo expression) == expectedRecipe
        then Just (reversedParameters, resultRepresentation, expressionPath, expression)
        else Nothing

scalarRepresentation :: TypedType -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
scalarRepresentation typeValue recipe =
  case (typeValue, loweredRepresentation recipe) of
    (TypedTupleType [], Just LoweredUnitRepresentation) -> Just LoweredUnitRepresentation
    (TypedBoolType, Just LoweredBoolRepresentation) -> Just LoweredBoolRepresentation
    (TypedCharType, Just LoweredCharRepresentation) -> Just LoweredCharRepresentation
    (TypedIntType, Just representation@LoweredSignedIntegerRepresentation {}) -> Just representation
    (TypedFloatType, Just representation@LoweredFloatRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredSignedIntegerRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredUnsignedIntegerRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredFloatRepresentation {}) -> Just representation
    _ -> Nothing

validateStatementProfiles ::
  [Text] ->
  [FunctionShape] ->
  [TypedCoreName] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [(TypedCoreName, [TypedCoreName])])
validateStatementProfiles modulePath functions localValueNames =
  go 0 [] []
  where
    go _ failures calls [] = (failures, calls)
    go statementIndex failures calls (statement : rest) =
      case statement of
        TypedSignatureStatement {} -> continue failures calls
        TypedLetStatement {} ->
          case find ((== statementIndex) . functionShapeStatementIndex) functions of
            Nothing -> continue failures calls
            Just function ->
              let check =
                    inspectExpression
                      modulePath
                      [statementIndex]
                      (functionShapeBodyPath function)
                      functions
                      localValueNames
                      (functionShapeParameters function)
                      (functionShapeBody function)
               in continue
                    (failures <> expressionCheckFailures check)
                    (calls <> [(functionShapeName function, expressionCheckCalls check)])
        TypedExpressionStatement _ expression ->
          let check =
                inspectExpression
                  modulePath
                  [statementIndex]
                  [0]
                  functions
                  localValueNames
                  []
                  expression
           in continue
                (failures <> expressionCheckFailures check)
                calls
        _ -> continue failures calls
      where
        continue nextFailures nextCalls =
          go (statementIndex + 1) nextFailures nextCalls rest

inspectExpression ::
  [Text] ->
  [Int] ->
  [Int] ->
  [FunctionShape] ->
  [TypedCoreName] ->
  [FunctionParameterShape] ->
  TypedExpr ->
  ExpressionCheck
inspectExpression modulePath statementPath expressionPath functions localValueNames parameters expression =
  case expression of
    TypedLiteralExpr info _ ->
      representationCheck info
    TypedVariableExpr info name ->
      case find ((== name) . functionParameterName) parameters of
        Just (FunctionParameterShape _ (LoweredParameter _ expectedRepresentation))
          | loweredRepresentation (nodeRecipe info) == Just expectedRepresentation ->
              noExpressionFailures
          | otherwise -> representationCheck info
        Nothing ->
          case find ((== name) . functionShapeName) functions of
            Just _ ->
              oneFailure
                LoweredIRCallableValueUnsupported
                (LoweredIRNameFailureDetail name)
            Nothing
              | name `elem` localValueNames ->
                  oneFailure
                    LoweredIRCaptureUnsupported
                    (LoweredIRNameFailureDetail name)
              | otherwise ->
                  oneFailure
                    LoweredIRCallableValueUnsupported
                    (LoweredIRNameFailureDetail name)
    TypedTupleExpr info [] ->
      representationCheck info
    TypedBinaryExpr info operator left right ->
      combineExpressionChecks
        [ representationCheck info,
          operatorCheck operator,
          child 0 left,
          child 1 right
        ]
    TypedApplyExpr {} ->
      inspectApplication
        modulePath
        statementPath
        expressionPath
        functions
        localValueNames
        parameters
        expression
    _ ->
      oneFailure LoweredIRUnsupportedExpression LoweredIRNoFailureDetail
  where
    path = TypedExpressionPath modulePath statementPath expressionPath
    noExpressionFailures = ExpressionCheck [] []
    oneFailure kind detail =
      ExpressionCheck
        [LoweredIRLoweringFailure path kind detail]
        []
    representationCheck info =
      case loweredRepresentation (nodeRecipe info) of
        Just _ -> noExpressionFailures
        Nothing ->
          oneFailure
            LoweredIRUnsupportedRepresentation
            (LoweredIRRecipeFailureDetail (nodeRecipe info))
    operatorCheck operator =
      case loweredPrimitive operator of
        Just _ -> noExpressionFailures
        Nothing ->
          oneFailure
            LoweredIRUnsupportedOperator
            (LoweredIROperatorFailureDetail operator)
    child childIndex =
      inspectExpression
        modulePath
        statementPath
        (expressionPath <> [childIndex])
        functions
        localValueNames
        parameters

combineExpressionChecks :: [ExpressionCheck] -> ExpressionCheck
combineExpressionChecks =
  foldl'
    ( \(ExpressionCheck failures calls) (ExpressionCheck nextFailures nextCalls) ->
        ExpressionCheck
          (failures <> nextFailures)
          (calls <> nextCalls)
    )
    (ExpressionCheck [] [])

inspectApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  [FunctionShape] ->
  [TypedCoreName] ->
  [FunctionParameterShape] ->
  TypedExpr ->
  ExpressionCheck
inspectApplication modulePath statementPath expressionPath functions localValueNames parameters expression =
  combineExpressionChecks
    (targetCheck : map (uncurry inspectArgument) arguments)
  where
    path = TypedExpressionPath modulePath statementPath expressionPath
    (callee, _, arguments) = applicationSpine expressionPath expression
    inspectArgument argumentPath argument =
      inspectExpression
        modulePath
        statementPath
        argumentPath
        functions
        localValueNames
        parameters
        argument
    targetCheck =
      case callee of
        TypedVariableExpr _ name ->
          case find ((== name) . functionShapeName) functions of
            Just target
              | expectedArity == actualArity ->
                  ExpressionCheck [] [name]
              | otherwise ->
                  ExpressionCheck
                    [ LoweredIRLoweringFailure
                        path
                        LoweredIRCallArityUnsupported
                        (LoweredIRArityFailureDetail expectedArity actualArity)
                    ]
                    [name]
              where
                expectedArity = length (functionShapeParameters target)
                actualArity = length arguments
            Nothing
              | name `elem` localValueNames ->
                  ExpressionCheck [] []
            Nothing ->
              ExpressionCheck
                [ LoweredIRLoweringFailure
                    path
                    LoweredIRNonLocalCallUnsupported
                    (LoweredIRNameFailureDetail name)
                ]
                []
        _ ->
          ExpressionCheck
            [ LoweredIRLoweringFailure
                path
                LoweredIRCallableValueUnsupported
                LoweredIRNoFailureDetail
            ]
            []

applicationSpine :: [Int] -> TypedExpr -> (TypedExpr, [Int], [([Int], TypedExpr)])
applicationSpine rootPath =
  go rootPath []
  where
    go currentPath arguments expression =
      case expression of
        TypedApplyExpr _ function argument ->
          go
            (currentPath <> [0])
            ((currentPath <> [1], argument) : arguments)
            function
        _ -> (expression, currentPath, arguments)

recursiveFunctionFailures ::
  [Text] ->
  [FunctionShape] ->
  [(TypedCoreName, [TypedCoreName])] ->
  [LoweredIRLoweringFailure]
recursiveFunctionFailures modulePath functions functionCalls =
  [ LoweredIRLoweringFailure
      (TypedStatementPath modulePath [functionShapeStatementIndex function])
      LoweredIRRecursiveFunctionUnsupported
      (LoweredIRNameFailureDetail (functionShapeName function))
  | function <- functions,
    recursivelyReaches (functionShapeName function)
  ]
  where
    recursivelyReaches source =
      any (reaches source []) (callsFrom source)
    reaches target seen current
      | current == target = True
      | current `elem` seen = False
      | otherwise =
          any (reaches target (current : seen)) (callsFrom current)
    callsFrom name =
      case lookup name functionCalls of
        Just calls -> calls
        Nothing -> []

emitFunction ::
  [Text] ->
  [FunctionShape] ->
  FunctionShape ->
  Either [LoweredIRLoweringFailure] LoweredFunction
emitFunction modulePath functions function =
  case lowerExpression
    modulePath
    [functionShapeStatementIndex function]
    (functionShapeBodyPath function)
    functions
    (functionShapeParameters function)
    initialState
    (functionShapeBody function) of
    ([], Just resultOperand, finalState) ->
      Right
        ( LoweredFunction
            (functionShapeId function)
            Nothing
            (map functionParameter (functionShapeParameters function))
            (functionShapeResultRepresentation function)
            [ LoweredBlock
                (LoweredBlockId "entry")
                []
                (loweringInstructions finalState)
                (Just (LoweredReturn resultOperand))
            ]
            (LoweredBlockId "entry")
        )
    (failures@(_ : _), _, _) -> Left failures
    _ ->
      Left
        [ LoweredIRLoweringFailure
            (TypedStatementPath modulePath [functionShapeStatementIndex function])
            LoweredIRInvalidFunctionShape
            (LoweredIRNameFailureDetail (functionShapeName function))
        ]
  where
    initialState =
      LoweringState
        { loweringFunctionId = functionShapeId function,
          loweringNextTemporary = 1,
          loweringInstructions = []
        }

emitEntry ::
  [Text] ->
  LoweredFunctionId ->
  [FunctionShape] ->
  [TypedStatement] ->
  Either [LoweredIRLoweringFailure] (LoweredOperand, LoweringState)
emitEntry modulePath entryFunctionId functions =
  go 0 Nothing initialState
  where
    initialState =
      LoweringState
        { loweringFunctionId = entryFunctionId,
          loweringNextTemporary = 1,
          loweringInstructions = []
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

lowerExpression ::
  [Text] ->
  [Int] ->
  [Int] ->
  [FunctionShape] ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerExpression modulePath statementPath expressionPath functions parameters state expression =
  case expression of
    TypedLiteralExpr info literal ->
      lowerLiteral path info literal state
    TypedVariableExpr info name ->
      case find ((== name) . functionParameterName) parameters of
        Just (FunctionParameterShape _ (LoweredParameter parameterId representation))
          | loweredRepresentation (nodeRecipe info) == Just representation ->
              ([], Just (LoweredFunctionParameterOperand parameterId representation), state)
        _ -> unsupportedExpression path state
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
  [FunctionShape] ->
  [FunctionParameterShape] ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerBinary modulePath statementPath expressionPath path info operator left right functions parameters state =
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
        functions
        parameters
        state
        left
    (rightFailures, maybeRightOperand, rightState) =
      lowerExpression
        modulePath
        statementPath
        (expressionPath <> [1])
        functions
        parameters
        leftState
        right

lowerApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  [FunctionShape] ->
  [FunctionParameterShape] ->
  LoweringState ->
  TypedExpr ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerApplication modulePath statementPath expressionPath path functions parameters state expression =
  case callee of
    TypedVariableExpr _ name ->
      case find ((== name) . functionShapeName) functions of
        Just target
          | length arguments == length (functionShapeParameters target) ->
              case resultRepresentationFailures <> argumentFailures of
                failures@(_ : _) -> (failures, Nothing, argumentState)
                [] ->
                  case (maybeResultRepresentation, sequence argumentOperands) of
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
                                  loweringInstructions argumentState <> [instruction]
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
        Nothing ->
          ( [ LoweredIRLoweringFailure
                path
                LoweredIRNonLocalCallUnsupported
                (LoweredIRNameFailureDetail name)
            ],
            Nothing,
            state
          )
    _ ->
      ( [ LoweredIRLoweringFailure
            path
            LoweredIRCallableValueUnsupported
            LoweredIRNoFailureDetail
        ],
        Nothing,
        state
      )
  where
    (callee, _, arguments) = applicationSpine expressionPath expression
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath path (nodeRecipe (expressionInfo expression))
    (argumentFailures, argumentOperands, argumentState) =
      foldl'
        lowerArgument
        ([], [], state)
        arguments
    lowerArgument (failures, operands, currentState) (argumentPath, argument) =
      let (nextFailures, maybeOperand, nextState) =
            lowerExpression
              modulePath
              statementPath
              argumentPath
              functions
              parameters
              currentState
              argument
       in (failures <> nextFailures, operands <> [maybeOperand], nextState)

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

nodeType :: TypedNodeInfo -> TypedType
nodeType (TypedNodeInfo typeValue _ _ _) = typeValue

expressionInfo :: TypedExpr -> TypedNodeInfo
expressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedOperatorValueExpr info _ -> info
    TypedListExpr info _ -> info
    TypedTupleExpr info _ -> info
    TypedApplyExpr info _ _ -> info
    TypedTypeApplicationExpr info _ _ _ -> info
    TypedIfExpr info _ _ _ -> info
    TypedPatternCaseExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedLeftSectionExpr info _ _ -> info
    TypedRightSectionExpr info _ _ -> info
    TypedBlockExpr info _ -> info
