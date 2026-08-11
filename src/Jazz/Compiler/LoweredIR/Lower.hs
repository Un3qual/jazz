{-# LANGUAGE OverloadedStrings #-}

-- | Validated lowering from the first typed-core scalar profile into the
-- permanent backend-neutral lowered IR.
module Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringResult (..),
    lowerTypedCoreExpressionDirectCall,
  )
where

import Data.List (find)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Text.Read (readMaybe)

data LoweredIRLoweringKind
  = LoweredIRUnsupportedProgram
  | LoweredIRUnsupportedModule
  | LoweredIRUnsupportedStatement
  | LoweredIRUnsupportedExpression
  | LoweredIRUnsupportedRepresentation
  | LoweredIRUnsupportedOperator
  | LoweredIRInvalidFunctionShape
  | LoweredIRDuplicateFunctionIdentity
  | LoweredIRDuplicateGeneratedIdentity
  | LoweredIRDuplicateParameterIdentity
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
  | LoweredIRGeneratedIdentityFailureDetail Text
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
  { loweringNextTemporary :: Int,
    loweringInstructions :: [LoweredInstruction]
  }

data FunctionParameterShape = FunctionParameterShape
  { functionParameterBinder :: TypedBinderId,
    functionParameter :: LoweredParameter
  }

data FunctionDeclaration = FunctionDeclaration
  { functionDeclarationBinder :: TypedBinderId,
    functionDeclarationName :: TypedCoreName,
    functionDeclarationStatementIndex :: Int
  }

data FunctionShape = FunctionShape
  { functionShapeBinder :: TypedBinderId,
    functionShapeName :: TypedCoreName,
    functionShapeCallableShape :: TypedCallableShape,
    functionShapeId :: LoweredFunctionId,
    functionShapeEnvironmentLayout :: Maybe LoweredLayoutId,
    functionShapeStatementIndex :: Int,
    functionShapeParameters :: [FunctionParameterShape],
    functionShapeResultRepresentation :: LoweredRepresentation,
    functionShapeReversedBodyPath :: [Int],
    functionShapeBody :: TypedExpr
  }

newtype ExpressionCheck = ExpressionCheck
  { expressionCheckFailures :: [LoweredIRLoweringFailure]
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
    functionDeclarations = collectFunctionDeclarations statements
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
      representationAtPath (TypedModulePath modulePath) (typedNodeRecipe moduleInfo)
    (profileFailures, functionDependencies) =
      validateStatementProfiles modulePath functionDeclarations functionShapes localValueNames statements
    recursiveFailures =
      recursiveFunctionFailures modulePath functionDeclarations functionDependencies
    statementFailures =
      orderedStatementFailures
        (length statements)
        [shapeFailures, recursiveFailures, profileFailures]
    allFailures =
      moduleFailures
        <> resultRepresentationFailures
        <> statementFailures
    emitProgram =
      case ( maybeResultRepresentation,
            traverse (emitFunction modulePath functionShapes) functionShapes,
             emitEntry modulePath functionShapes statements
           ) of
          (Just resultRepresentation, Right functions, Right (resultOperand, finalState)) ->
            Right
              ( LoweredProgram
                  supportedLoweredIRVersion
                  [ LoweredLayout layoutId (LoweredClosureEnvironmentLayout [])
                  | function <- functionShapes,
                    Just layoutId <- [functionShapeEnvironmentLayout function]
                  ]
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
                                 (reverse (loweringInstructions finalState))
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

orderedStatementFailures ::
  Int ->
  [[LoweredIRLoweringFailure]] ->
  [LoweredIRLoweringFailure]
orderedStatementFailures statementCount failureGroups =
  concatMap failuresAtStatement [0 .. statementCount - 1]
  where
    failuresAtStatement statementIndex =
      concatMap
        (filter (ownedByStatement statementIndex))
        failureGroups
    ownedByStatement statementIndex (LoweredIRLoweringFailure path _ _) =
      case path of
        TypedStatementPath _ (owner : _) -> owner == statementIndex
        TypedExpressionPath _ (owner : _) _ -> owner == statementIndex
        TypedPatternPath _ (owner : _) _ -> owner == statementIndex
        _ -> False

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
  go 0 [] [] [] Set.empty Set.empty
  where
    go _ reversedFailures reversedFunctions reversedLocalNames _ _ [] =
      (reverse reversedFailures, reverse reversedFunctions, reverse reversedLocalNames)
    go statementIndex reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities (statement : rest) =
      case statement of
        TypedSignatureStatement {} ->
          continue reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities
        TypedLetStatement _ name _ scheme expression ->
          if Set.member name seenNames
            then
              continue
                ( LoweredIRLoweringFailure
                    (TypedStatementPath modulePath [statementIndex])
                    LoweredIRDuplicateFunctionIdentity
                    (LoweredIRNameFailureDetail name)
                    : reversedFailures
                )
                reversedFunctions
                reversedLocalNames
                seenNames
                seenGeneratedIdentities
            else case duplicateLeadingParameters expression of
                duplicateParameters@(_ : _) ->
                  continue
                    ( reverse
                        [ LoweredIRLoweringFailure
                            (TypedExpressionPath modulePath [statementIndex] parameterPath)
                            LoweredIRDuplicateParameterIdentity
                            (LoweredIRNameFailureDetail parameterName)
                        | (parameterPath, parameterName) <- duplicateParameters
                        ]
                        <> reversedFailures
                    )
                    reversedFunctions
                    (name : reversedLocalNames)
                    (Set.insert name seenNames)
                    seenGeneratedIdentities
                [] ->
                  case collectFunctionShape modulePath statementIndex name scheme expression of
                    Just function ->
                      let maybeGeneratedIdentity = functionShapeEnvironmentLayout function
                          duplicateGeneratedIdentity =
                            case maybeGeneratedIdentity of
                              Just identityValue
                                | Set.member identityValue seenGeneratedIdentities ->
                                    [ LoweredIRLoweringFailure
                                        (TypedStatementPath modulePath [statementIndex])
                                        LoweredIRDuplicateGeneratedIdentity
                                        (loweredIRGeneratedIdentityFailureDetail identityValue)
                                    ]
                              _ -> []
                          nextGeneratedIdentities =
                            maybe seenGeneratedIdentities (`Set.insert` seenGeneratedIdentities) maybeGeneratedIdentity
                       in continue
                        (reverse duplicateGeneratedIdentity <> reversedFailures)
                        (function : reversedFunctions)
                        (name : reversedLocalNames)
                        (Set.insert name seenNames)
                        nextGeneratedIdentities
                    Nothing ->
                      continue
                        ( LoweredIRLoweringFailure
                            (TypedStatementPath modulePath [statementIndex])
                            LoweredIRInvalidFunctionShape
                            (LoweredIRNameFailureDetail name)
                            : reversedFailures
                        )
                        reversedFunctions
                        (name : reversedLocalNames)
                        (Set.insert name seenNames)
                        seenGeneratedIdentities
        TypedExpressionStatement {} ->
          continue reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities
        _ ->
          continue
            ( LoweredIRLoweringFailure
                (TypedStatementPath modulePath [statementIndex])
                LoweredIRUnsupportedStatement
                LoweredIRNoFailureDetail
                : reversedFailures
            )
            reversedFunctions
            reversedLocalNames
            seenNames
            seenGeneratedIdentities
      where
        continue nextFailures nextFunctions nextLocalNames nextSeenNames nextSeenGeneratedIdentities =
          go
            (statementIndex + 1)
            nextFailures
            nextFunctions
            nextLocalNames
            nextSeenNames
            nextSeenGeneratedIdentities
            rest

collectFunctionDeclarations :: [TypedStatement] -> [FunctionDeclaration]
collectFunctionDeclarations =
  go 0
  where
    go _ [] = []
    go statementIndex (statement : rest) =
      case statement of
        TypedLetStatement binder name _ scheme _
          | callableScheme scheme ->
              FunctionDeclaration binder name statementIndex : continue
        _ -> continue
      where
        continue = go (statementIndex + 1) rest
    callableScheme (TypedScheme _ _ _ _ typeValue _ maybeCallableShape) =
      case (typeValue, maybeCallableShape) of
        (TypedFunctionType {}, Just _) -> True
        _ -> False

duplicateLeadingParameters :: TypedExpr -> [([Int], TypedCoreName)]
duplicateLeadingParameters =
  go [0] Set.empty
  where
    go reversedPath seenNames expression =
      case expression of
        TypedLambdaExpr _ _ parameterName body
          | Set.member parameterName seenNames ->
              (reverse reversedPath, parameterName)
                : go (0 : reversedPath) seenNames body
          | otherwise ->
              go (0 : reversedPath) (Set.insert parameterName seenNames) body
        _ -> []

collectFunctionShape ::
  [Text] ->
  Int ->
  TypedCoreName ->
  TypedScheme ->
  TypedExpr ->
  Maybe FunctionShape
collectFunctionShape modulePath statementIndex name scheme expression = do
  identifier <- localValueIdentifier name
  (schemeBinder, schemeType, schemeRecipe, callableShape) <- monomorphicSchemeContract scheme
  environmentLayout <-
    case callableShape of
      TypedDirectCallableShape -> Just Nothing
      TypedClosureCallableShape -> Just <$> closureEnvironmentLayoutId schemeBinder
  (parameters, resultRepresentation, reversedBodyPath, body) <-
    case callableShape of
      TypedDirectCallableShape ->
        flattenLeadingLambdas schemeType schemeRecipe [0] [] expression
      TypedClosureCallableShape ->
        collectUnaryClosureShape schemeType schemeRecipe [0] expression
  if null parameters
    then Nothing
    else
      Just
        FunctionShape
          { functionShapeBinder = schemeBinder,
            functionShapeName = name,
            functionShapeCallableShape = callableShape,
            functionShapeId =
              LoweredFunctionId
                (Text.intercalate "::" (modulePath <> [identifier])),
            functionShapeEnvironmentLayout = environmentLayout,
            functionShapeStatementIndex = statementIndex,
            functionShapeParameters = parameters,
            functionShapeResultRepresentation = resultRepresentation,
            functionShapeReversedBodyPath = reversedBodyPath,
            functionShapeBody = body
          }

localValueIdentifier :: TypedCoreName -> Maybe Text
localValueIdentifier name =
  case name of
    TypedResolvedName TypedCurrentModule TypedValueNamespace identifier -> Just identifier
    _ -> Nothing

closureEnvironmentLayoutId :: TypedBinderId -> Maybe LoweredLayoutId
closureEnvironmentLayoutId (TypedBinderId (binderModulePath, binderPath, binderName)) = do
  identifier <- localValueIdentifier binderName
  pure
    ( LoweredLayoutId
        ( "$jz1$closure-env$m"
            <> decimal (length binderModulePath)
            <> foldMap (("$" <>) . lengthPrefixedSegment) binderModulePath
            <> "$p"
            <> decimal (length binderPath)
            <> "$"
            <> Text.intercalate "," (map decimal binderPath)
            <> "$n"
            <> lengthPrefixedSegment identifier
        )
    )
  where
    decimal = Text.pack . show
    lengthPrefixedSegment segment = decimal (Text.length segment) <> ":" <> segment

monomorphicSchemeContract :: TypedScheme -> Maybe (TypedBinderId, TypedType, TypedRepresentationRecipe, TypedCallableShape)
monomorphicSchemeContract (TypedScheme owner typeParameters evidence primitive typeValue recipe (Just callableShape))
  | null typeParameters,
    null evidence,
    null primitive =
      Just (owner, typeValue, recipe, callableShape)
monomorphicSchemeContract _ = Nothing

collectUnaryClosureShape ::
  TypedType ->
  TypedRepresentationRecipe ->
  [Int] ->
  TypedExpr ->
  Maybe ([FunctionParameterShape], LoweredRepresentation, [Int], TypedExpr)
collectUnaryClosureShape expectedType expectedRecipe reversedExpressionPath expression =
  case expression of
    TypedLambdaExpr info parameterBinder _ body -> do
      if typedNodeType info == expectedType && typedNodeRecipe info == expectedRecipe
        then pure ()
        else Nothing
      (argumentType, resultType) <-
        case expectedType of
          TypedFunctionType argument result -> Just (argument, result)
          _ -> Nothing
      (argumentRecipe, resultRecipe) <-
        case expectedRecipe of
          TypedClosureRecipe [argument] result -> Just (argument, result)
          _ -> Nothing
      parameterRepresentation <- valueRepresentation argumentType argumentRecipe
      resultRepresentation <- valueRepresentation resultType resultRecipe
      if typedNodeType (typedExpressionInfo body) == resultType
        && typedNodeRecipe (typedExpressionInfo body) == resultRecipe
        then
          Just
            ( [ FunctionParameterShape
                  { functionParameterBinder = parameterBinder,
                    functionParameter =
                      LoweredParameter
                        (LoweredParameterId "arg1")
                        parameterRepresentation
                  }
              ],
              resultRepresentation,
              0 : reversedExpressionPath,
              body
            )
        else Nothing
    _ -> Nothing

flattenLeadingLambdas ::
  TypedType ->
  TypedRepresentationRecipe ->
  [Int] ->
  [FunctionParameterShape] ->
  TypedExpr ->
  Maybe ([FunctionParameterShape], LoweredRepresentation, [Int], TypedExpr)
flattenLeadingLambdas expectedType expectedRecipe reversedExpressionPath reversedParameters expression =
  case expression of
    TypedLambdaExpr info parameterBinder _ body -> do
      if typedNodeType info == expectedType && typedNodeRecipe info == expectedRecipe
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
      parameterRepresentation <- valueRepresentation argumentType argumentRecipe
      let parameterIndex = length reversedParameters + 1
          parameter =
            FunctionParameterShape
              { functionParameterBinder = parameterBinder,
                functionParameter =
                  LoweredParameter
                    (LoweredParameterId ("arg" <> Text.pack (show parameterIndex)))
                    parameterRepresentation
              }
      flattenLeadingLambdas
        resultType
        resultRecipe
        (0 : reversedExpressionPath)
        (parameter : reversedParameters)
        body
    _ -> do
      resultRepresentation <- valueRepresentation expectedType expectedRecipe
      if typedNodeType (typedExpressionInfo expression) == expectedType
        && typedNodeRecipe (typedExpressionInfo expression) == expectedRecipe
        then Just (reverse reversedParameters, resultRepresentation, reversedExpressionPath, expression)
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

valueRepresentation :: TypedType -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
valueRepresentation typeValue recipe =
  case (typeValue, loweredRepresentation recipe) of
    (TypedFunctionType {}, Just representation@LoweredClosureRepresentation {}) -> Just representation
    _ -> scalarRepresentation typeValue recipe

validateStatementProfiles ::
  [Text] ->
  [FunctionDeclaration] ->
  [FunctionShape] ->
  [TypedCoreName] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [(TypedBinderId, [TypedBinderId])])
validateStatementProfiles modulePath declarations functions localValueNames =
  go 0 [] []
  where
    go _ reversedFailureChunks reversedCalls [] =
      (concat (reverse reversedFailureChunks), reverse reversedCalls)
    go statementIndex reversedFailureChunks reversedCalls (statement : rest) =
      case statement of
        TypedSignatureStatement {} -> continue reversedFailureChunks reversedCalls
        TypedLetStatement _ _ _ _ expression ->
          let nextCalls =
                case find ((== statementIndex) . functionDeclarationStatementIndex) declarations of
                  Just declaration ->
                    ( functionDeclarationBinder declaration,
                      localFunctionDependencies declarations expression
                    ) : reversedCalls
                  Nothing -> reversedCalls
           in case find ((== statementIndex) . functionShapeStatementIndex) functions of
            Nothing ->
              case expression of
                TypedLambdaExpr {} -> continue reversedFailureChunks nextCalls
                _ ->
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
                        (expressionCheckFailures check : reversedFailureChunks)
                        nextCalls
            Just function ->
              let check =
                    inspectExpression
                      modulePath
                      [statementIndex]
                      (functionShapeReversedBodyPath function)
                      functions
                      localValueNames
                      (functionShapeParameters function)
                      (functionShapeBody function)
               in continue
                    (expressionCheckFailures check : reversedFailureChunks)
                    nextCalls
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
                (expressionCheckFailures check : reversedFailureChunks)
                reversedCalls
        _ -> continue reversedFailureChunks reversedCalls
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
    TypedVariableExpr info name binderReference ->
      case findParameterShape binderReference parameters of
        Just (FunctionParameterShape _ (LoweredParameter _ expectedRepresentation))
          | loweredRepresentation (typedNodeRecipe info) == Just expectedRepresentation ->
              noExpressionFailures
          | otherwise -> representationCheck info
        Nothing ->
          case findFunctionShape binderReference functions of
            Just function
              | functionShapeCallableShape function == TypedClosureCallableShape,
                loweredRepresentation (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
                  noExpressionFailures
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
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    noExpressionFailures = ExpressionCheck []
    oneFailure kind detail =
      ExpressionCheck
        [LoweredIRLoweringFailure path kind detail]
    representationCheck info =
      case loweredRepresentation (typedNodeRecipe info) of
        Just _ -> noExpressionFailures
        Nothing ->
          oneFailure
            LoweredIRUnsupportedRepresentation
            (LoweredIRRecipeFailureDetail (typedNodeRecipe info))
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
        (childIndex : expressionPath)
        functions
        localValueNames
        parameters

combineExpressionChecks :: [ExpressionCheck] -> ExpressionCheck
combineExpressionChecks checks =
  ExpressionCheck
    (concatMap expressionCheckFailures checks)

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
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
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
        TypedVariableExpr _ name binderReference ->
          case findFunctionShape binderReference functions of
            Just target
              | functionShapeCallableShape target == TypedClosureCallableShape,
                actualArity == 1 ->
                  ExpressionCheck []
              | expectedArity == actualArity ->
                  ExpressionCheck []
              | otherwise ->
                  ExpressionCheck
                    [ LoweredIRLoweringFailure
                        path
                        LoweredIRCallArityUnsupported
                        (LoweredIRArityFailureDetail expectedArity actualArity)
                    ]
              where
                expectedArity = length (functionShapeParameters target)
            Nothing
              | Just _ <- findParameterShape binderReference parameters,
                actualArity == 1 ->
                  ExpressionCheck []
              | Just _ <- findParameterShape binderReference parameters ->
                  ExpressionCheck
                    [ LoweredIRLoweringFailure
                        path
                        LoweredIRCallArityUnsupported
                        (LoweredIRArityFailureDetail 1 actualArity)
                    ]
            Nothing
              | name `elem` localValueNames ->
                  ExpressionCheck []
            Nothing ->
              ExpressionCheck
                [ LoweredIRLoweringFailure
                    path
                    LoweredIRNonLocalCallUnsupported
                    (LoweredIRNameFailureDetail name)
                ]
        _ ->
          ExpressionCheck
            [ LoweredIRLoweringFailure
                path
                LoweredIRCallableValueUnsupported
                LoweredIRNoFailureDetail
            ]

    actualArity = length arguments

localFunctionDependencies :: [FunctionDeclaration] -> TypedExpr -> [TypedBinderId]
localFunctionDependencies declarations expression =
  case expression of
    TypedLiteralExpr {} -> []
    TypedVariableExpr _ _ binderReference ->
      case findFunctionDeclaration binderReference declarations of
        Just declaration -> [functionDeclarationBinder declaration]
        Nothing -> []
    TypedLambdaExpr _ _ _ body -> dependencies body
    TypedOperatorValueExpr {} -> []
    TypedListExpr _ elements -> concatMap dependencies elements
    TypedTupleExpr _ elements -> concatMap dependencies elements
    TypedApplyExpr _ function argument -> dependencies function <> dependencies argument
    TypedTypeApplicationExpr _ function _ _ -> dependencies function
    TypedIfExpr _ condition thenExpression elseExpression ->
      dependencies condition <> dependencies thenExpression <> dependencies elseExpression
    TypedPatternCaseExpr _ scrutinee arms ->
      dependencies scrutinee <> concatMap armDependencies arms
    TypedBinaryExpr _ _ left right -> dependencies left <> dependencies right
    TypedLeftSectionExpr _ left _ -> dependencies left
    TypedRightSectionExpr _ _ right -> dependencies right
    TypedBlockExpr _ statements -> concatMap statementDependencies statements
  where
    dependencies = localFunctionDependencies declarations
    armDependencies (TypedCaseArm _ maybeGuard result) =
      maybe [] dependencies maybeGuard <> dependencies result
    statementDependencies statement =
      case statement of
        TypedLetStatement _ _ _ _ initializer -> dependencies initializer
        TypedExpressionStatement _ result -> dependencies result
        TypedImplStatement (TypedImplDeclaration _ _ methods) -> concatMap methodDependencies methods
        _ -> []
    methodDependencies (TypedMethodDefinition _ _ _ _ body) = dependencies body

loweredIRGeneratedIdentityFailureDetail :: LoweredLayoutId -> LoweredIRLoweringDetail
loweredIRGeneratedIdentityFailureDetail (LoweredLayoutId identityValue) =
  LoweredIRGeneratedIdentityFailureDetail identityValue

applicationSpine :: [Int] -> TypedExpr -> (TypedExpr, [Int], [([Int], TypedExpr)])
applicationSpine rootPath =
  go rootPath []
  where
    go currentPath arguments expression =
      case expression of
        TypedApplyExpr _ function argument ->
          go
            (0 : currentPath)
            ((1 : currentPath, argument) : arguments)
            function
        _ -> (expression, currentPath, arguments)

findFunctionShape :: Maybe TypedBinderId -> [FunctionShape] -> Maybe FunctionShape
findFunctionShape binderReference functions = do
  binder <- binderReference
  find ((== binder) . functionShapeBinder) functions

findFunctionDeclaration :: Maybe TypedBinderId -> [FunctionDeclaration] -> Maybe FunctionDeclaration
findFunctionDeclaration binderReference declarations = do
  binder <- binderReference
  find ((== binder) . functionDeclarationBinder) declarations

findParameterShape :: Maybe TypedBinderId -> [FunctionParameterShape] -> Maybe FunctionParameterShape
findParameterShape binderReference parameters = do
  binder <- binderReference
  find ((== binder) . functionParameterBinder) parameters

functionClosureRepresentation :: FunctionShape -> LoweredRepresentation
functionClosureRepresentation function =
  LoweredClosureRepresentation
    ( LoweredCallSignature
        [ representation
        | FunctionParameterShape _ (LoweredParameter _ representation) <- functionShapeParameters function
        ]
        (functionShapeResultRepresentation function)
    )

functionEnvironmentParameter :: FunctionShape -> Maybe LoweredParameter
functionEnvironmentParameter function = do
  layoutId <- functionShapeEnvironmentLayout function
  pure
    ( LoweredParameter
        (LoweredParameterId "environment")
        (LoweredManagedReferenceRepresentation layoutId)
    )

recursiveFunctionFailures ::
  [Text] ->
  [FunctionDeclaration] ->
  [(TypedBinderId, [TypedBinderId])] ->
  [LoweredIRLoweringFailure]
recursiveFunctionFailures modulePath declarations functionDependencies =
  [ LoweredIRLoweringFailure
      (TypedStatementPath modulePath [functionDeclarationStatementIndex declaration])
      LoweredIRRecursiveFunctionUnsupported
      (LoweredIRNameFailureDetail (functionDeclarationName declaration))
  | declaration <- declarations,
    recursivelyReaches (functionDeclarationBinder declaration)
  ]
  where
    recursivelyReaches source =
      any (reaches source []) (dependenciesFrom source)
    reaches target seen current
      | current == target = True
      | current `elem` seen = False
      | otherwise =
          any (reaches target (current : seen)) (dependenciesFrom current)
    dependenciesFrom binder =
      case lookup binder functionDependencies of
        Just dependencies -> dependencies
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
    (functionShapeReversedBodyPath function)
    functions
    (functionShapeParameters function)
    initialState
    (functionShapeBody function) of
    ([], Just resultOperand, finalState) ->
      Right
        ( LoweredFunction
            (functionShapeId function)
            (functionEnvironmentParameter function)
            (map functionParameter (functionShapeParameters function))
            (functionShapeResultRepresentation function)
            [ LoweredBlock
                (LoweredBlockId "entry")
                []
                (reverse (loweringInstructions finalState))
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
        { loweringNextTemporary = 1,
          loweringInstructions = []
        }

emitEntry ::
  [Text] ->
  [FunctionShape] ->
  [TypedStatement] ->
  Either [LoweredIRLoweringFailure] (LoweredOperand, LoweringState)
emitEntry modulePath functions =
  go 0 Nothing initialState
  where
    initialState =
      LoweringState
        { loweringNextTemporary = 1,
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
    TypedVariableExpr info _ binderReference ->
      case findParameterShape binderReference parameters of
        Just (FunctionParameterShape _ (LoweredParameter parameterId representation))
          | loweredRepresentation (typedNodeRecipe info) == Just representation ->
              ([], Just (LoweredFunctionParameterOperand parameterId representation), state)
        _ ->
          case findFunctionShape binderReference functions of
            Just function
              | functionShapeCallableShape function == TypedClosureCallableShape,
                loweredRepresentation (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
                  lowerNamedClosureValue path function state
            _ -> unsupportedExpression path state
    TypedTupleExpr info [] ->
      case typedNodeRecipe info of
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
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)

lowerNamedClosureValue ::
  TypedCoreValidationPath ->
  FunctionShape ->
  LoweringState ->
  ([LoweredIRLoweringFailure], Maybe LoweredOperand, LoweringState)
lowerNamedClosureValue path function state =
  case functionShapeEnvironmentLayout function of
    Just layoutId ->
      let environmentIndex = loweringNextTemporary state
          environmentTemporaryId = temporaryId environmentIndex
          environmentRepresentation = LoweredManagedReferenceRepresentation layoutId
          environmentInstruction =
            LoweredInstruction
              environmentTemporaryId
              environmentRepresentation
              (LoweredConstructProduct layoutId [])
          closureIndex = environmentIndex + 1
          closureTemporaryId = temporaryId closureIndex
          closureRepresentation = functionClosureRepresentation function
          closureInstruction =
            LoweredInstruction
              closureTemporaryId
              closureRepresentation
              ( LoweredConstructClosure
                  (functionShapeId function)
                  (LoweredTemporaryOperand environmentTemporaryId environmentRepresentation)
              )
          nextState =
            state
              { loweringNextTemporary = closureIndex + 1,
                loweringInstructions =
                  closureInstruction : environmentInstruction : loweringInstructions state
              }
       in ([], Just (LoweredTemporaryOperand closureTemporaryId closureRepresentation), nextState)
    Nothing -> unsupportedExpression path state
  where
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
      representationAtPath path (typedNodeRecipe info)
    (leftFailures, maybeLeftOperand, leftState) =
      lowerExpression
        modulePath
        statementPath
        (0 : expressionPath)
        functions
        parameters
        state
        left
    (rightFailures, maybeRightOperand, rightState) =
      lowerExpression
        modulePath
        statementPath
        (1 : expressionPath)
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
    TypedVariableExpr _ name binderReference ->
      case findFunctionShape binderReference functions of
        Just target
          | functionShapeCallableShape target == TypedClosureCallableShape ->
              lowerUnaryClosureApplication
                modulePath
                statementPath
                expressionPath
                path
                functions
                parameters
                state
                expression
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
              lowerUnaryClosureApplication
                modulePath
                statementPath
                expressionPath
                path
                functions
                parameters
                state
                expression
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
    (callee, _, arguments) = applicationSpine expressionPath expression
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath path (typedNodeRecipe (typedExpressionInfo expression))
    (reversedArgumentFailureChunks, reversedArgumentOperands, argumentState) =
      foldl'
        lowerArgument
        ([], [], state)
        arguments
    argumentFailures = concat (reverse reversedArgumentFailureChunks)
    argumentOperands = reverse reversedArgumentOperands
    lowerArgument (reversedFailureChunks, reversedOperands, currentState) (argumentPath, argument) =
      let (nextFailures, maybeOperand, nextState) =
            lowerExpression
              modulePath
              statementPath
              argumentPath
              functions
              parameters
              currentState
              argument
       in (nextFailures : reversedFailureChunks, maybeOperand : reversedOperands, nextState)

lowerUnaryClosureApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedCoreValidationPath ->
  [FunctionShape] ->
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
          case (maybeResultRepresentation, maybeFunctionOperand, maybeArgumentOperand) of
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
          representationAtPath path (typedNodeRecipe info)
        (functionFailures, maybeFunctionOperand, functionState) =
          lowerExpression
            modulePath
            statementPath
            (0 : expressionPath)
            functions
            parameters
            state
            function
        (argumentFailures, maybeArgumentOperand, argumentState) =
          lowerExpression
            modulePath
            statementPath
            (1 : expressionPath)
            functions
            parameters
            functionState
            argument
    _ -> unsupportedExpression path state

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
    TypedClosureRecipe arguments result -> do
      argumentRepresentations <- traverse loweredRepresentation arguments
      resultRepresentation <- loweredRepresentation result
      pure
        ( LoweredClosureRepresentation
            (LoweredCallSignature argumentRepresentations resultRepresentation)
        )
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
