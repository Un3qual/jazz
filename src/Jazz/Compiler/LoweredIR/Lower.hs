{-# LANGUAGE OverloadedStrings #-}

-- | Validated lowering from the first typed-core scalar profile into the
-- permanent backend-neutral lowered IR.
module Jazz.Compiler.LoweredIR.Lower
  ( LoweredIRLoweringKind (..),
    LoweredIRLoweringDetail (..),
    LoweredIRLoweringFailure (..),
    LoweredIRLoweringResult (..),
    lowerTypedCoreExpressionDirectCall,
    lowerValidatedTypedCoreExpressionDirectCall,
  )
where

import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
    builtinSymbolArity,
    lookupBuiltinSymbolInMode,
  )
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextEqualService),
    orderedRuntimeServices,
    runtimeServiceContract,
    textLayout,
    textLayoutId,
    textOperationService,
    textRepresentation,
  )
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validateTypedProgramOnce,
    validatedTypedProgram,
  )
import Text.Read (readMaybe)

data LoweredIRLoweringKind
  = LoweredIRUnsupportedProgram
  | LoweredIRUnsupportedModule
  | LoweredIRUnsupportedStatement
  | LoweredIRUnsupportedExpression
  | LoweredIRUnsupportedPattern
  | LoweredIRIncompletePatternCase
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
    loweringNextCarrier :: Int,
    loweringInstructions :: [LoweredInstruction],
    loweringCompletedBlocks :: [LoweredBlock],
    loweringCurrentBlockId :: LoweredBlockId,
    loweringCurrentBlockParameters :: [LoweredParameter],
    loweringLocalBindings :: Map.Map TypedBinderId LoweredOperand,
    loweringSharedEnvironments :: Map.Map LoweredLayoutId LoweredOperand,
    loweringCarriedOperands :: Map.Map Int LoweredOperand
  }

data RuntimeRequirements = RuntimeRequirements
  { runtimeRequiresTextLayout :: Bool,
    runtimeRequiredServices :: Set.Set RuntimeServiceKey
  }

data ResultDestination
  = ProduceValue
  | FinishFunction LoweredRepresentation

data AmbientSlot
  = AmbientLocalSlot TypedBinderId LoweredRepresentation
  | AmbientSharedEnvironmentSlot LoweredLayoutId LoweredRepresentation
  | AmbientCarriedOperandSlot Int LoweredRepresentation

data FunctionParameterShape = FunctionParameterShape
  { functionParameterBinder :: TypedBinderId,
    functionParameter :: LoweredParameter
  }

data FunctionDeclaration = FunctionDeclaration
  { functionDeclarationBinder :: TypedBinderId,
    functionDeclarationName :: TypedCoreName,
    functionDeclarationStatementIndex :: Int
  }

data CaptureShape = CaptureShape
  { captureShapeBinder :: TypedBinderId,
    captureShapeRepresentation :: LoweredRepresentation
  }

data FunctionShape = FunctionShape
  { functionShapeBinder :: TypedBinderId,
    functionShapeName :: TypedCoreName,
    functionShapeCallableShape :: TypedCallableShape,
    functionShapeId :: LoweredFunctionId,
    functionShapeEnvironmentLayout :: Maybe LoweredLayoutId,
    functionShapeStatementIndex :: Int,
    functionShapeParameters :: [FunctionParameterShape],
    functionShapeCaptures :: [CaptureShape],
    functionShapeResultRepresentation :: LoweredRepresentation,
    functionShapeReversedBodyPath :: [Int],
    functionShapeBody :: TypedExpr,
    functionShapeSourceBinding :: Bool
  }

data FunctionIndex = FunctionIndex
  { indexedFunctionShapes :: Map.Map TypedBinderId FunctionShape,
    indexedFunctionShapesByStatement :: Map.Map Int FunctionShape,
    indexedRecursiveGroupMembers :: Map.Map TypedBinderId [TypedBinderId],
    indexedScalarRepresentations :: Map.Map TypedBinderId LoweredRepresentation
  }

lowerTypedCoreExpressionDirectCall :: TypedProgram -> LoweredIRLoweringResult
lowerTypedCoreExpressionDirectCall typedProgram =
  case validateTypedProgramOnce typedProgram of
    Left failures -> LoweredIRTypedCoreFailures failures
    Right validatedProgram -> lowerValidatedTypedCoreExpressionDirectCall validatedProgram

-- | Lower a Typed Program whose structural validation was already performed
-- by a trusted producer. Raw external values must use the checked entry point
-- above.
lowerValidatedTypedCoreExpressionDirectCall :: ValidatedTypedProgram -> LoweredIRLoweringResult
lowerValidatedTypedCoreExpressionDirectCall validatedProgram =
  case lowerValidatedProgram (validatedTypedProgram validatedProgram) of
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
    typedModulePath (TypedModule modulePath _ _ _ _ _ _ _) = modulePath

lowerValidatedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweredProgram
lowerValidatedModule typedModule@(TypedModule modulePath _ imports exports moduleInterface recursiveGroups statements moduleInfo) =
  case allFailures of
    failures@(_ : _) -> Left failures
    [] ->
      emitProgram
  where
    entryFunctionId =
      LoweredFunctionId (Text.intercalate "::" (modulePath <> ["$entry"]))
    entryBlockId = LoweredBlockId "entry"
    (shapeFailures, collectedFunctionShapes, localValueNames) =
      collectFunctionShapes modulePath statements
    functionShapes =
      applyRecursiveClosureGroups recursiveGroups collectedFunctionShapes
    functionDeclarations = collectFunctionDeclarations statements
    functionIndex =
      FunctionIndex
        { indexedFunctionShapes =
            Map.fromList
              [ (functionShapeBinder function, function)
              | function <- functionShapes
              ],
          indexedFunctionShapesByStatement =
            Map.fromList
              [ (functionShapeStatementIndex function, function)
              | function <- functionShapes,
                functionShapeSourceBinding function
              ],
          indexedRecursiveGroupMembers =
            Map.fromList
              [ (member, members)
              | TypedRecursiveGroup members <- recursiveGroups,
                member <- members
              ],
          indexedScalarRepresentations =
            Map.fromList
              [ (binder, representation)
              | TypedLetStatement binder _ _ scheme _ <- statements,
                Just (_, representation) <- [valueSchemeContract scheme]
              ]
        }
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
    profileFailures =
      validateStatementProfiles modulePath functionIndex (Set.fromList localValueNames) statements
    recursiveFailures =
      recursiveGroupProfileFailures
        modulePath
        functionIndex
        collectedFunctionShapes
        functionDeclarations
    statementFailures =
      orderedStatementFailures
        (length statements)
        [recursiveFailures, shapeFailures, profileFailures]
    allFailures =
      moduleFailures
        <> resultRepresentationFailures
        <> statementFailures
    emitProgram =
      case ( maybeResultRepresentation,
             traverse (emitFunction modulePath functionIndex) functionShapes,
             emitEntry modulePath functionIndex statements
           ) of
        (Just resultRepresentation, Right functions, Right (resultOperand, finalState)) ->
          Right
            ( LoweredProgram
                supportedLoweredIRVersion
                (requiredLayouts runtimeRequirements functionShapes)
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
        (_, Left failures, _) -> Left failures
        (_, _, Left failures) -> Left failures
        _ ->
          Left
            [ LoweredIRLoweringFailure
                (TypedModulePath modulePath)
                LoweredIRUnsupportedModule
                LoweredIRNoFailureDetail
            ]
    runtimeRequirements = collectRuntimeRequirements typedModule

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
    sourceFunctions = filter functionShapeSourceBinding functions
    supportedExport (TypedModuleExport namespace identifier) =
      namespace == TypedValueNamespace
        && any (matchesIdentifier identifier . functionShapeName) sourceFunctions
    supportedInterfaceValue (TypedValueInterface name _) =
      any ((== name) . functionShapeName) sourceFunctions
    matchesIdentifier identifier name =
      case name of
        TypedResolvedName TypedCurrentModule TypedValueNamespace candidate ->
          identifier == candidate
        _ -> False

collectFunctionShapes ::
  [Text] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [FunctionShape], [TypedCoreName])
collectFunctionShapes modulePath statements =
  (rootFailures, orderedFunctionShapes, localValueNames)
  where
    (rootFailures, rootFunctionShapes, localValueNames) =
      collectRootFunctionShapes modulePath statements
    rootShapesByStatement =
      Map.fromList
        [ (functionShapeStatementIndex function, function)
        | function <- rootFunctionShapes
        ]
    rootShapesByBinder =
      Map.fromList
        [ (functionShapeBinder function, function)
        | function <- rootFunctionShapes
        ]
    orderedFunctionShapes =
      concat
        [ shapesForStatement statementIndex statement
        | (statementIndex, statement) <- zip [0 ..] statements
        ]
    shapesForStatement statementIndex statement =
      case Map.lookup statementIndex rootShapesByStatement of
        Just rootFunction ->
          let capturedRoot = attachFunctionCaptures rootShapesByBinder rootFunction
           in capturedRoot
                : collectGeneratedFunctionShapes
                  rootShapesByBinder
                  statementIndex
                  (functionShapeReversedBodyPath capturedRoot)
                  (functionShapeBody capturedRoot)
        Nothing ->
          case statement of
            TypedLetStatement _ _ _ _ expression ->
              collectGeneratedFunctionShapes rootShapesByBinder statementIndex [0] expression
            TypedExpressionStatement _ expression ->
              collectGeneratedFunctionShapes rootShapesByBinder statementIndex [0] expression
            _ -> []

applyRecursiveClosureGroups :: [TypedRecursiveGroup] -> [FunctionShape] -> [FunctionShape]
applyRecursiveClosureGroups recursiveGroups initialFunctions =
  foldl' applyGroup initialFunctions recursiveGroups
  where
    applyGroup functions (TypedRecursiveGroup members) =
      case (members, traverse (`Map.lookup` functionsByBinder) members) of
        (firstMember : _, Just memberFunctions)
          | all closureSourceFunction memberFunctions,
            Just layoutId <- recursiveEnvironmentLayoutId firstMember ->
              let memberSet = Set.fromList members
                  sharedCaptures =
                    stableCaptureUnion
                      [ collectCaptureShapes
                          functionsByBinder
                          memberSet
                          (Set.fromList (map functionParameterBinder (functionShapeParameters function)))
                          (functionShapeBody function)
                      | function <- memberFunctions
                      ]
               in map (shareGroupEnvironment memberSet layoutId sharedCaptures) functions
        _ -> functions
      where
        functionsByBinder =
          Map.fromList
            [ (functionShapeBinder function, function)
            | function <- functions
            ]
    closureSourceFunction function =
      functionShapeSourceBinding function
        && functionShapeCallableShape function == TypedClosureCallableShape
    shareGroupEnvironment memberSet layoutId captures function
      | Set.member (functionShapeBinder function) memberSet =
          function
            { functionShapeEnvironmentLayout = Just layoutId,
              functionShapeCaptures = captures
            }
      | otherwise = function

stableCaptureUnion :: [[CaptureShape]] -> [CaptureShape]
stableCaptureUnion = reverse . snd . foldl' collectGroup (Set.empty, [])
  where
    collectGroup state = foldl' collectCapture state
    collectCapture (seen, reversedCaptures) capture
      | Set.member (captureShapeBinder capture) seen = (seen, reversedCaptures)
      | otherwise =
          ( Set.insert (captureShapeBinder capture) seen,
            capture : reversedCaptures
          )

orderedClosureLayouts :: [FunctionShape] -> [LoweredLayout]
orderedClosureLayouts = reverse . snd . foldl' collect (Set.empty, [])
  where
    collect state function =
      case functionShapeEnvironmentLayout function of
        Nothing -> state
        Just layoutId
          | Set.member layoutId (fst state) -> state
          | otherwise ->
              ( Set.insert layoutId (fst state),
                LoweredLayout
                  layoutId
                  ( LoweredClosureEnvironmentLayout
                      (map captureShapeRepresentation (functionShapeCaptures function))
                  )
                  : snd state
              )

requiredLayouts :: RuntimeRequirements -> [FunctionShape] -> [LoweredLayout]
requiredLayouts requirements functions =
  [textLayout | runtimeRequiresTextLayout requirements]
    <> orderedClosureLayouts functions

collectRuntimeRequirements :: TypedModule -> RuntimeRequirements
collectRuntimeRequirements (TypedModule _ _ _ _ moduleInterface _ statements moduleInfo) =
  foldl'
    mergeRuntimeRequirements
    (requirementsForNodeInfo moduleInfo)
    ( requirementsForInterface moduleInterface
        : map requirementsForStatement statements
    )

emptyRuntimeRequirements :: RuntimeRequirements
emptyRuntimeRequirements = RuntimeRequirements False Set.empty

mergeRuntimeRequirements :: RuntimeRequirements -> RuntimeRequirements -> RuntimeRequirements
mergeRuntimeRequirements left right =
  RuntimeRequirements
    { runtimeRequiresTextLayout =
        runtimeRequiresTextLayout left || runtimeRequiresTextLayout right,
      runtimeRequiredServices =
        Set.union
          (runtimeRequiredServices left)
          (runtimeRequiredServices right)
    }

requirementsForInterface :: TypedModuleInterface -> RuntimeRequirements
requirementsForInterface (TypedModuleInterface values _ _ _) =
  foldl'
    mergeRuntimeRequirements
    emptyRuntimeRequirements
    [requirementsForScheme scheme | TypedValueInterface _ scheme <- values]

requirementsForStatement :: TypedStatement -> RuntimeRequirements
requirementsForStatement statement =
  case statement of
    TypedLetStatement _ _ _ scheme expression ->
      requirementsForScheme scheme
        `mergeRuntimeRequirements` requirementsForExpression expression
    TypedSignatureStatement _ _ _ scheme -> requirementsForScheme scheme
    TypedExpressionStatement _ expression -> requirementsForExpression expression
    TypedDataStatement {} -> emptyRuntimeRequirements
    TypedClassStatement {} -> emptyRuntimeRequirements
    TypedImplStatement {} -> emptyRuntimeRequirements

requirementsForScheme :: TypedScheme -> RuntimeRequirements
requirementsForScheme (TypedScheme _ _ _ _ _ recipe _) =
  requirementsForRecipe recipe

requirementsForNodeInfo :: TypedNodeInfo -> RuntimeRequirements
requirementsForNodeInfo info = requirementsForRecipe (typedNodeRecipe info)

requirementsForRecipe :: TypedRepresentationRecipe -> RuntimeRequirements
requirementsForRecipe recipe =
  case recipe of
    TypedManagedTextRecipe -> RuntimeRequirements True Set.empty
    TypedClosureRecipe arguments result ->
      foldl'
        mergeRuntimeRequirements
        (requirementsForRecipe result)
        (map requirementsForRecipe arguments)
    _ -> emptyRuntimeRequirements

requirementsForExpression :: TypedExpr -> RuntimeRequirements
requirementsForExpression expression =
  foldl'
    mergeRuntimeRequirements
    ( requirementsForNodeInfo (typedExpressionInfo expression)
        `mergeRuntimeRequirements` requirementsForSemanticExpression expression
    )
    ( case expression of
        TypedLiteralExpr {} -> []
        TypedVariableExpr {} -> []
        TypedLambdaExpr _ _ _ body -> [requirementsForExpression body]
        TypedOperatorValueExpr {} -> []
        TypedListExpr _ values -> map requirementsForExpression values
        TypedTupleExpr _ values -> map requirementsForExpression values
        TypedApplyExpr _ function argument -> map requirementsForExpression [function, argument]
        TypedTypeApplicationExpr _ function _ _ -> [requirementsForExpression function]
        TypedIfExpr _ condition consequent alternative -> map requirementsForExpression [condition, consequent, alternative]
        TypedPatternCaseExpr _ scrutinee arms ->
          requirementsForExpression scrutinee : map requirementsForArm arms
        TypedBinaryExpr _ _ left right -> map requirementsForExpression [left, right]
        TypedLeftSectionExpr _ left _ -> [requirementsForExpression left]
        TypedRightSectionExpr _ _ right -> [requirementsForExpression right]
        TypedBlockExpr _ blockStatements -> map requirementsForStatement blockStatements
    )

requirementsForSemanticExpression :: TypedExpr -> RuntimeRequirements
requirementsForSemanticExpression expression =
  case textEqualityOperation expression of
    Just _ -> runtimeServiceRequirement TextEqualService
    Nothing ->
      case textRuntimeServiceApplication expression of
        Just serviceKey -> runtimeServiceRequirement serviceKey
        Nothing -> emptyRuntimeRequirements

runtimeServiceRequirement :: RuntimeServiceKey -> RuntimeRequirements
runtimeServiceRequirement serviceKey =
  RuntimeRequirements False (Set.singleton serviceKey)

requirementsForArm :: TypedCaseArm -> RuntimeRequirements
requirementsForArm (TypedCaseArm patternValue guard result) =
  foldl'
    mergeRuntimeRequirements
    (requirementsForPattern patternValue)
    (requirementsForExpression result : maybe [] ((: []) . requirementsForExpression) guard)

requirementsForPattern :: TypedPattern -> RuntimeRequirements
requirementsForPattern patternValue =
  foldl'
    mergeRuntimeRequirements
    (requirementsForNodeInfo (patternInfo patternValue))
    (map requirementsForPattern (patternChildren patternValue))
  where
    patternInfo patternNode =
      case patternNode of
        TypedWildcardPattern info -> info
        TypedVariablePattern info _ _ -> info
        TypedLiteralPattern info _ -> info
        TypedConstructorPattern info _ _ -> info
        TypedListPattern info _ -> info
        TypedConsListPattern info _ _ -> info
        TypedTuplePattern info _ -> info
        TypedAsPattern info _ _ _ -> info
        TypedOrPattern info _ -> info
    patternChildren patternNode =
      case patternNode of
        TypedConstructorPattern _ _ children -> children
        TypedListPattern _ children -> children
        TypedConsListPattern _ headPattern tailPattern -> [headPattern, tailPattern]
        TypedTuplePattern _ children -> children
        TypedAsPattern _ _ _ nested -> [nested]
        TypedOrPattern _ alternatives -> alternatives
        _ -> []

collectRootFunctionShapes ::
  [Text] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [FunctionShape], [TypedCoreName])
collectRootFunctionShapes modulePath =
  go 0 [] [] [] Set.empty Set.empty
  where
    go _ reversedFailures reversedFunctions reversedLocalNames _ _ [] =
      (reverse reversedFailures, reverse reversedFunctions, reverse reversedLocalNames)
    go statementIndex reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities (statement : rest) =
      case statement of
        TypedSignatureStatement {} ->
          continue reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities
        TypedLetStatement _ name _ scheme expression
          | Just _ <- valueSchemeContract scheme ->
              continue
                reversedFailures
                reversedFunctions
                (name : reversedLocalNames)
                seenNames
                seenGeneratedIdentities
          | Set.member name seenNames ->
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
          | otherwise ->
              case duplicateLeadingParameters expression of
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

callableScheme :: TypedScheme -> Bool
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
            functionShapeCaptures = [],
            functionShapeResultRepresentation = resultRepresentation,
            functionShapeReversedBodyPath = reversedBodyPath,
            functionShapeBody = body,
            functionShapeSourceBinding = True
          }

attachFunctionCaptures :: Map.Map TypedBinderId FunctionShape -> FunctionShape -> FunctionShape
attachFunctionCaptures globalFunctions function
  | functionShapeCallableShape function == TypedClosureCallableShape =
      function
        { functionShapeCaptures =
            collectCaptureShapes
              globalFunctions
              (Set.singleton (functionShapeBinder function))
              (Set.fromList (map functionParameterBinder (functionShapeParameters function)))
              (functionShapeBody function)
        }
  | otherwise = function

collectGeneratedFunctionShapes ::
  Map.Map TypedBinderId FunctionShape ->
  Int ->
  [Int] ->
  TypedExpr ->
  [FunctionShape]
collectGeneratedFunctionShapes globalFunctions statementIndex reversedExpressionPath expression =
  case expression of
    TypedLambdaExpr info parameterBinder parameterName _ ->
      case ( collectUnaryClosureShape
               (typedNodeType info)
               (typedNodeRecipe info)
               reversedExpressionPath
               expression,
             generatedFunctionId parameterBinder,
             closureEnvironmentLayoutId parameterBinder
           ) of
        (Just (parameters, resultRepresentation, reversedBodyPath, functionBody), Just functionId, Just layoutId) ->
          let function =
                FunctionShape
                  { functionShapeBinder = parameterBinder,
                    functionShapeName = parameterName,
                    functionShapeCallableShape = TypedClosureCallableShape,
                    functionShapeId = functionId,
                    functionShapeEnvironmentLayout = Just layoutId,
                    functionShapeStatementIndex = statementIndex,
                    functionShapeParameters = parameters,
                    functionShapeCaptures =
                      collectCaptureShapes
                        globalFunctions
                        Set.empty
                        (Set.fromList (map functionParameterBinder parameters))
                        functionBody,
                    functionShapeResultRepresentation = resultRepresentation,
                    functionShapeReversedBodyPath = reversedBodyPath,
                    functionShapeBody = functionBody,
                    functionShapeSourceBinding = False
                  }
           in function
                : collectGeneratedFunctionShapes
                  globalFunctions
                  statementIndex
                  reversedBodyPath
                  functionBody
        _ -> []
    TypedApplyExpr _ function argument ->
      children [(0, function), (1, argument)]
    TypedBinaryExpr _ _ left right ->
      children [(0, left), (1, right)]
    TypedTupleExpr _ elements -> children (zip [0 ..] elements)
    TypedListExpr _ elements -> children (zip [0 ..] elements)
    TypedTypeApplicationExpr _ function _ _ -> child 0 function
    TypedIfExpr _ condition thenExpression elseExpression ->
      children [(0, condition), (1, thenExpression), (2, elseExpression)]
    TypedPatternCaseExpr _ scrutinee arms ->
      child 0 scrutinee
        <> concat
          [ maybe [] (childPath [armIndex + 1, 0]) maybeGuard
              <> childPath [armIndex + 1, 1] body
          | (armIndex, TypedCaseArm _ maybeGuard body) <- zip [0 ..] arms
          ]
    TypedLeftSectionExpr _ left _ -> child 0 left
    TypedRightSectionExpr _ _ right -> child 0 right
    _ -> []
  where
    child childIndex =
      collectGeneratedFunctionShapes
        globalFunctions
        statementIndex
        (childIndex : reversedExpressionPath)
    children = concatMap (uncurry child)
    childPath pathSuffix =
      collectGeneratedFunctionShapes
        globalFunctions
        statementIndex
        (reverse pathSuffix <> reversedExpressionPath)

collectCaptureShapes :: Map.Map TypedBinderId FunctionShape -> Set.Set TypedBinderId -> Set.Set TypedBinderId -> TypedExpr -> [CaptureShape]
collectCaptureShapes globalFunctions initiallyExpanded initiallyBound expression =
  snd (go initiallyExpanded initiallyBound Set.empty expression)
  where
    go expandedFunctions boundBinders seenBinders currentExpression =
      case currentExpression of
        TypedVariableExpr info _ (Just binder)
          | Set.member binder boundBinders -> (seenBinders, [])
          | Just function <- Map.lookup binder globalFunctions,
            functionShapeCallableShape function == TypedClosureCallableShape,
            Set.notMember binder expandedFunctions ->
              go
                (Set.insert binder expandedFunctions)
                (Set.fromList (map functionParameterBinder (functionShapeParameters function)))
                seenBinders
                (functionShapeBody function)
          | Map.member binder globalFunctions -> (seenBinders, [])
          | Set.notMember binder seenBinders,
            Just representation <- loweredRepresentation (typedNodeRecipe info) ->
              ( Set.insert binder seenBinders,
                [CaptureShape binder representation]
              )
          | otherwise -> (seenBinders, [])
        TypedLambdaExpr _ parameterBinder _ body ->
          go expandedFunctions (Set.insert parameterBinder boundBinders) seenBinders body
        TypedApplyExpr _ function argument ->
          combine expandedFunctions boundBinders seenBinders [function, argument]
        TypedBinaryExpr _ _ left right ->
          combine expandedFunctions boundBinders seenBinders [left, right]
        TypedTupleExpr _ elements -> combine expandedFunctions boundBinders seenBinders elements
        TypedListExpr _ elements -> combine expandedFunctions boundBinders seenBinders elements
        TypedTypeApplicationExpr _ function _ _ -> go expandedFunctions boundBinders seenBinders function
        TypedIfExpr _ condition thenExpression elseExpression ->
          combine expandedFunctions boundBinders seenBinders [condition, thenExpression, elseExpression]
        TypedPatternCaseExpr _ scrutinee arms ->
          let (seenAfterScrutinee, scrutineeCaptures) =
                go expandedFunctions boundBinders seenBinders scrutinee
              (finalSeen, armCaptures) =
                foldl'
                  (collectArm expandedFunctions boundBinders)
                  (seenAfterScrutinee, [])
                  arms
           in (finalSeen, scrutineeCaptures <> armCaptures)
        TypedLeftSectionExpr _ left _ -> go expandedFunctions boundBinders seenBinders left
        TypedRightSectionExpr _ _ right -> go expandedFunctions boundBinders seenBinders right
        TypedBlockExpr _ statements ->
          combine expandedFunctions boundBinders seenBinders (concatMap statementExpressions statements)
        _ -> (seenBinders, [])

    combine expandedFunctions boundBinders seenBinders expressions =
      foldl' collectOne (seenBinders, []) expressions
      where
        collectOne (seenSoFar, captures) childExpression =
          let (nextSeenBinders, childCaptures) = go expandedFunctions boundBinders seenSoFar childExpression
           in (nextSeenBinders, captures <> childCaptures)

    collectArm expandedFunctions boundBinders (seenBinders, captures) (TypedCaseArm patternValue maybeGuard result) =
      let armBoundBinders =
            boundBinders <> Set.fromList (typedPatternBinderIds patternValue)
          (seenAfterGuard, guardCaptures) =
            case maybeGuard of
              Just guard -> go expandedFunctions armBoundBinders seenBinders guard
              Nothing -> (seenBinders, [])
          (nextSeenBinders, resultCaptures) =
            go expandedFunctions armBoundBinders seenAfterGuard result
       in (nextSeenBinders, captures <> guardCaptures <> resultCaptures)
    statementExpressions statement =
      case statement of
        TypedLetStatement _ _ _ _ initializer -> [initializer]
        TypedExpressionStatement _ result -> [result]
        TypedImplStatement (TypedImplDeclaration _ _ methods) ->
          [body | TypedMethodDefinition _ _ _ _ body <- methods]
        _ -> []

localValueIdentifier :: TypedCoreName -> Maybe Text
localValueIdentifier name =
  case name of
    TypedResolvedName TypedCurrentModule TypedValueNamespace identifier -> Just identifier
    _ -> Nothing

closureEnvironmentLayoutId :: TypedBinderId -> Maybe LoweredLayoutId
closureEnvironmentLayoutId binder = LoweredLayoutId <$> generatedIdentity "closure-env" binder

recursiveEnvironmentLayoutId :: TypedBinderId -> Maybe LoweredLayoutId
recursiveEnvironmentLayoutId (TypedBinderId (binderModulePath, binderPath, _)) =
  Just
    ( LoweredLayoutId
        (generatedIdentityText "recursive-env" binderModulePath binderPath "group")
    )

generatedFunctionId :: TypedBinderId -> Maybe LoweredFunctionId
generatedFunctionId binder = LoweredFunctionId <$> generatedIdentity "lambda-fn" binder

generatedIdentity :: Text -> TypedBinderId -> Maybe Text
generatedIdentity domain (TypedBinderId (binderModulePath, binderPath, binderName)) = do
  identifier <- localValueIdentifier binderName
  pure (generatedIdentityText domain binderModulePath binderPath identifier)

generatedIdentityText :: Text -> [Text] -> [Int] -> Text -> Text
generatedIdentityText domain binderModulePath binderPath identifier =
  "$jz1$"
    <> domain
    <> "$m"
    <> decimal (length binderModulePath)
    <> foldMap (("$" <>) . lengthPrefixedSegment) binderModulePath
    <> "$p"
    <> decimal (length binderPath)
    <> "$"
    <> Text.intercalate "," (map decimal binderPath)
    <> "$n"
    <> lengthPrefixedSegment identifier
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

valueSchemeContract :: TypedScheme -> Maybe (TypedBinderId, LoweredRepresentation)
valueSchemeContract (TypedScheme owner typeParameters evidence primitive typeValue recipe Nothing)
  | null typeParameters,
    null evidence,
    null primitive =
      case valueRepresentation typeValue recipe of
        Just representation -> Just (owner, representation)
        Nothing -> Nothing
valueSchemeContract _ = Nothing

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
    (TypedTextType, Just representation@LoweredManagedReferenceRepresentation {}) -> Just representation
    _ -> scalarRepresentation typeValue recipe

validateStatementProfiles ::
  [Text] ->
  FunctionIndex ->
  Set.Set TypedCoreName ->
  [TypedStatement] ->
  [LoweredIRLoweringFailure]
validateStatementProfiles modulePath functions localValueNames statements =
  sortOn loweringFailurePath (statementFailures <> generatedFunctionFailures)
  where
    loweringFailurePath (LoweredIRLoweringFailure path _ _) = path
    statementFailures = go 0 [] statements
    generatedFunctionFailures =
      concat
        [ inspectExpression
            modulePath
            [functionShapeStatementIndex function]
            (functionShapeReversedBodyPath function)
            functions
            localValueNames
            False
            (functionShapeParameters function)
            (functionShapeCaptures function)
            (functionShapeBody function)
        | function <- Map.elems (indexedFunctionShapes functions),
          not (functionShapeSourceBinding function)
        ]
    go _ reversedFailureChunks [] =
      concat (reverse reversedFailureChunks)
    go statementIndex reversedFailureChunks (statement : rest) =
      case statement of
        TypedSignatureStatement {} -> continue reversedFailureChunks
        TypedLetStatement _ _ _ _ expression ->
          case Map.lookup statementIndex (indexedFunctionShapesByStatement functions) of
            Nothing ->
              case expression of
                TypedLambdaExpr {} -> continue reversedFailureChunks
                _ ->
                  let check =
                        inspectExpression
                          modulePath
                          [statementIndex]
                          [0]
                          functions
                          localValueNames
                          True
                          []
                          []
                          expression
                   in continue (check : reversedFailureChunks)
            Just function ->
              let check =
                    inspectExpression
                      modulePath
                      [statementIndex]
                      (functionShapeReversedBodyPath function)
                      functions
                      localValueNames
                      False
                      (functionShapeParameters function)
                      (functionShapeCaptures function)
                      (functionShapeBody function)
               in continue (check : reversedFailureChunks)
        TypedExpressionStatement _ expression ->
          let check =
                inspectExpression
                  modulePath
                  [statementIndex]
                  [0]
                  functions
                  localValueNames
                  True
                  []
                  []
                  expression
           in continue
                (check : reversedFailureChunks)
        _ -> continue reversedFailureChunks
      where
        continue nextFailures =
          go (statementIndex + 1) nextFailures rest

inspectExpression ::
  [Text] ->
  [Int] ->
  [Int] ->
  FunctionIndex ->
  Set.Set TypedCoreName ->
  Bool ->
  [FunctionParameterShape] ->
  [CaptureShape] ->
  TypedExpr ->
  [LoweredIRLoweringFailure]
inspectExpression modulePath statementPath expressionPath functions localValueNames allowEntryLocals parameters captures expression =
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
          case findCaptureShape binderReference captures of
            Just capture
              | loweredRepresentation (typedNodeRecipe info) == Just (captureShapeRepresentation capture) ->
                  noExpressionFailures
            Just _ -> representationCheck info
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
                  | Just expectedRepresentation <- findScalarRepresentation binderReference functions,
                    allowEntryLocals,
                    loweredRepresentation (typedNodeRecipe info) == Just expectedRepresentation ->
                      noExpressionFailures
                  | Just _ <- findScalarRepresentation binderReference functions,
                    allowEntryLocals ->
                      representationCheck info
                  | Set.member name localValueNames ->
                      oneFailure
                        LoweredIRCaptureUnsupported
                        (LoweredIRNameFailureDetail name)
                  | otherwise ->
                      oneFailure
                        LoweredIRCallableValueUnsupported
                        (LoweredIRNameFailureDetail name)
    TypedLambdaExpr info parameterBinder _ _ ->
      case findFunctionShape (Just parameterBinder) functions of
        Just function
          | not (functionShapeSourceBinding function),
            loweredRepresentation (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
              noExpressionFailures
        _ ->
          oneFailure
            LoweredIRCallableValueUnsupported
            LoweredIRNoFailureDetail
    TypedTupleExpr info [] ->
      representationCheck info
    TypedBinaryExpr info operator left right ->
      combineExpressionChecks
        [ representationCheck info,
          operatorCheck operator,
          child 0 left,
          child 1 right
        ]
    TypedIfExpr info condition thenExpression elseExpression ->
      combineExpressionChecks
        [ representationCheck info,
          child 0 condition,
          child 1 thenExpression,
          child 2 elseExpression
        ]
    TypedPatternCaseExpr info scrutinee arms ->
      case scalarPatternCaseProfileFailures modulePath statementPath expressionPath scrutinee arms of
        failures@(_ : _) -> failures
        [] ->
          combineExpressionChecks
            ( representationCheck info
                : child 0 scrutinee
                : [ let armParameters = patternArmParameters patternValue <> parameters
                     in maybe
                          []
                          ( inspectExpression
                              modulePath
                              statementPath
                              (0 : armIndex + 1 : expressionPath)
                              functions
                              localValueNames
                              allowEntryLocals
                              armParameters
                              captures
                          )
                          maybeGuard
                          <> inspectExpression
                            modulePath
                            statementPath
                            (1 : armIndex + 1 : expressionPath)
                            functions
                            localValueNames
                            allowEntryLocals
                            armParameters
                            captures
                            body
                  | (armIndex, TypedCaseArm patternValue maybeGuard body) <- zip [0 ..] arms
                  ]
            )
    TypedApplyExpr {} ->
      inspectApplication
        modulePath
        statementPath
        expressionPath
        functions
        localValueNames
        allowEntryLocals
        parameters
        captures
        expression
    _ ->
      oneFailure LoweredIRUnsupportedExpression LoweredIRNoFailureDetail
  where
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    noExpressionFailures = []
    oneFailure kind detail =
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
        allowEntryLocals
        parameters
        captures

combineExpressionChecks :: [[LoweredIRLoweringFailure]] -> [LoweredIRLoweringFailure]
combineExpressionChecks = concat

scalarPatternCaseProfileFailures ::
  [Text] ->
  [Int] ->
  [Int] ->
  TypedExpr ->
  [TypedCaseArm] ->
  [LoweredIRLoweringFailure]
scalarPatternCaseProfileFailures modulePath statementPath reversedExpressionPath scrutinee arms =
  case unsupportedArmFailures of
    failure : _ -> [failure]
    []
      | scalarRepresentation
          (typedNodeType scrutineeInfo)
          (typedNodeRecipe scrutineeInfo)
          == Nothing ->
          [expressionFailure LoweredIRUnsupportedPattern]
      | not (totalArmChain arms) ->
          [expressionFailure LoweredIRIncompletePatternCase]
      | otherwise -> []
  where
    expressionPath = reverse reversedExpressionPath
    scrutineeInfo = typedExpressionInfo scrutinee
    unsupportedArmFailures =
      [ LoweredIRLoweringFailure
          (TypedPatternPath modulePath statementPath (expressionPath <> [armIndex]))
          LoweredIRUnsupportedPattern
          LoweredIRNoFailureDetail
      | (armIndex, TypedCaseArm patternValue _ _) <- zip [0 ..] arms,
        not (supportedPattern patternValue)
      ]
    supportedPattern patternValue =
      case patternValue of
        TypedWildcardPattern info -> matchingScrutineeInfo info
        TypedVariablePattern info _ _ -> matchingScrutineeInfo info
        TypedLiteralPattern info _ ->
          matchingScrutineeInfo info
            && scalarRepresentation (typedNodeType info) (typedNodeRecipe info) /= Nothing
        _ -> False
    matchingScrutineeInfo info =
      typedNodeType info == typedNodeType scrutineeInfo
        && typedNodeRecipe info == typedNodeRecipe scrutineeInfo
    totalArmChain caseArms =
      case reverse caseArms of
        TypedCaseArm finalPattern Nothing _ : precedingArms ->
          catchAllPattern finalPattern
            && all supportedPrecedingArm precedingArms
        _ -> False
    supportedPrecedingArm (TypedCaseArm patternValue maybeGuard _) =
      not (catchAllPattern patternValue) || maybeGuard /= Nothing
    catchAllPattern patternValue =
      case patternValue of
        TypedWildcardPattern {} -> True
        TypedVariablePattern {} -> True
        _ -> False
    expressionFailure kind =
      LoweredIRLoweringFailure
        (TypedExpressionPath modulePath statementPath expressionPath)
        kind
        LoweredIRNoFailureDetail

patternArmParameters :: TypedPattern -> [FunctionParameterShape]
patternArmParameters patternValue =
  case patternValue of
    TypedVariablePattern info binder _ ->
      case loweredRepresentation (typedNodeRecipe info) of
        Just representation ->
          [ FunctionParameterShape
              binder
              (LoweredParameter (LoweredParameterId "pattern") representation)
          ]
        Nothing -> []
    _ -> []

typedPatternBinderIds :: TypedPattern -> [TypedBinderId]
typedPatternBinderIds patternValue =
  case patternValue of
    TypedVariablePattern _ binder _ -> [binder]
    TypedConstructorPattern _ _ patterns -> concatMap typedPatternBinderIds patterns
    TypedListPattern _ patterns -> concatMap typedPatternBinderIds patterns
    TypedConsListPattern _ headPattern tailPattern ->
      typedPatternBinderIds headPattern <> typedPatternBinderIds tailPattern
    TypedTuplePattern _ patterns -> concatMap typedPatternBinderIds patterns
    TypedAsPattern _ binder _ nestedPattern ->
      binder : typedPatternBinderIds nestedPattern
    TypedOrPattern _ [] -> []
    TypedOrPattern _ (alternative : _) -> typedPatternBinderIds alternative
    _ -> []

inspectApplication ::
  [Text] ->
  [Int] ->
  [Int] ->
  FunctionIndex ->
  Set.Set TypedCoreName ->
  Bool ->
  [FunctionParameterShape] ->
  [CaptureShape] ->
  TypedExpr ->
  [LoweredIRLoweringFailure]
inspectApplication modulePath statementPath expressionPath functions localValueNames allowEntryLocals parameters captures expression =
  combineExpressionChecks
    (targetCheck : map (uncurry inspectArgument) arguments)
  where
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    (callee, calleePath, arguments) = applicationSpine expressionPath expression
    inspectArgument argumentPath argument =
      inspectExpression
        modulePath
        statementPath
        argumentPath
        functions
        localValueNames
        allowEntryLocals
        parameters
        captures
        argument
    targetCheck =
      case textRuntimeServiceApplication expression of
        Just _ -> []
        Nothing ->
          case callee of
            TypedVariableExpr _ name binderReference ->
              case findFunctionShape binderReference functions of
                Just target
                  | functionShapeCallableShape target == TypedClosureCallableShape,
                    actualArity >= 1 ->
                      []
                  | functionShapeCallableShape target == TypedDirectCallableShape,
                    actualArity >= expectedArity ->
                      []
                  | otherwise ->
                      [ LoweredIRLoweringFailure
                          path
                          LoweredIRCallArityUnsupported
                          (LoweredIRArityFailureDetail expectedArity actualArity)
                      ]
                  where
                    expectedArity = length (functionShapeParameters target)
                Nothing
                  | Just _ <- findParameterShape binderReference parameters,
                    actualArity >= 1 ->
                      []
                  | Just _ <- findParameterShape binderReference parameters ->
                      [ LoweredIRLoweringFailure
                          path
                          LoweredIRCallArityUnsupported
                          (LoweredIRArityFailureDetail 1 actualArity)
                      ]
                Nothing
                  | Just (CaptureShape _ LoweredClosureRepresentation {}) <- findCaptureShape binderReference captures,
                    actualArity >= 1 ->
                      []
                  | Just (CaptureShape _ LoweredClosureRepresentation {}) <- findCaptureShape binderReference captures ->
                      [ LoweredIRLoweringFailure
                          path
                          LoweredIRCallArityUnsupported
                          (LoweredIRArityFailureDetail 1 actualArity)
                      ]
                Nothing
                  | Set.member name localValueNames ->
                      []
                Nothing ->
                  [ LoweredIRLoweringFailure
                      path
                      LoweredIRNonLocalCallUnsupported
                      (LoweredIRNameFailureDetail name)
                  ]
            _ ->
              inspectExpression
                modulePath
                statementPath
                calleePath
                functions
                localValueNames
                allowEntryLocals
                parameters
                captures
                callee

    actualArity = length arguments

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

textEqualityOperation :: TypedExpr -> Maybe Bool
textEqualityOperation expression =
  case expression of
    TypedBinaryExpr info (TypedBuiltinOperator operator) left right
      | typedNodeRecipe info == TypedBoolRecipe,
        typedNodeRecipe (typedExpressionInfo left) == TypedManagedTextRecipe,
        typedNodeRecipe (typedExpressionInfo right) == TypedManagedTextRecipe ->
          case operator of
            "==" -> Just False
            "!=" -> Just True
            _ -> Nothing
    _ -> Nothing

textRuntimeServiceApplication :: TypedExpr -> Maybe RuntimeServiceKey
textRuntimeServiceApplication expression = do
  let (callee, _, arguments) = applicationSpine [] expression
  (identifier, binderReference) <-
    case callee of
      TypedVariableExpr _ (TypedBuiltinName name) binder -> Just (name, binder)
      _ -> Nothing
  case binderReference of
    Just _ -> Nothing
    Nothing -> do
      symbol <- lookupBuiltinSymbolInMode ResolveKernelOnly identifier
      serviceKey <- textOperationService symbol
      if length arguments == builtinSymbolArity symbol
        then Just serviceKey
        else Nothing

findFunctionShape :: Maybe TypedBinderId -> FunctionIndex -> Maybe FunctionShape
findFunctionShape binderReference functions = do
  binder <- binderReference
  Map.lookup binder (indexedFunctionShapes functions)

findScalarRepresentation :: Maybe TypedBinderId -> FunctionIndex -> Maybe LoweredRepresentation
findScalarRepresentation binderReference functions = do
  binder <- binderReference
  Map.lookup binder (indexedScalarRepresentations functions)

findParameterShape :: Maybe TypedBinderId -> [FunctionParameterShape] -> Maybe FunctionParameterShape
findParameterShape binderReference parameters = do
  binder <- binderReference
  find ((== binder) . functionParameterBinder) parameters

findCaptureShape :: Maybe TypedBinderId -> [CaptureShape] -> Maybe CaptureShape
findCaptureShape binderReference captures = do
  binder <- binderReference
  find ((== binder) . captureShapeBinder) captures

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

recursiveGroupProfileFailures ::
  [Text] ->
  FunctionIndex ->
  [FunctionShape] ->
  [FunctionDeclaration] ->
  [LoweredIRLoweringFailure]
recursiveGroupProfileFailures modulePath functions unsharedFunctions declarations =
  [ LoweredIRLoweringFailure
      (TypedStatementPath modulePath [functionDeclarationStatementIndex declaration])
      LoweredIRRecursiveFunctionUnsupported
      (LoweredIRNameFailureDetail (functionDeclarationName declaration))
  | declaration <- declarations,
    Just groupMembers <-
      [ Map.lookup
          (functionDeclarationBinder declaration)
          (indexedRecursiveGroupMembers functions)
      ],
    not (supportedMember declaration groupMembers)
  ]
  where
    unsharedFunctionsByBinder =
      Map.fromList
        [ (functionShapeBinder function, function)
        | function <- unsharedFunctions
        ]
    supportedMember declaration members =
      case traverse (`Map.lookup` indexedFunctionShapes functions) members of
        Just memberFunctions
          | all functionShapeSourceBinding memberFunctions,
            all ((== TypedClosureCallableShape) . functionShapeCallableShape) memberFunctions ->
              memberCapturesAvailableAtGroupStart members (functionDeclarationBinder declaration)
          | all functionShapeSourceBinding memberFunctions,
            all ((== TypedDirectCallableShape) . functionShapeCallableShape) memberFunctions ->
              not
                ( any
                    (closureShapeReferencesGroup memberSet memberStatementIndexes)
                    (Map.elems (indexedFunctionShapes functions))
                )
        _ -> False
      where
        memberSet = Set.fromList members
        memberStatementIndexes =
          Set.fromList
            [ functionShapeStatementIndex member
            | binder <- members,
              Just member <- [Map.lookup binder (indexedFunctionShapes functions)]
            ]
    memberCapturesAvailableAtGroupStart members memberBinder =
      case (members, Map.lookup memberBinder unsharedFunctionsByBinder) of
        (firstMember : _, Just member) ->
          all
            (captureAvailableBefore firstMember)
            ( collectCaptureShapes
                unsharedFunctionsByBinder
                (Set.fromList members)
                (Set.fromList (map functionParameterBinder (functionShapeParameters member)))
                (functionShapeBody member)
            )
        _ -> False
    captureAvailableBefore firstMember capture =
      case (captureShapeBinder capture, firstMember) of
        (TypedBinderId (_, captureStatement : _, _), TypedBinderId (_, firstStatement : _, _)) -> captureStatement < firstStatement
        _ -> False
    closureShapeReferencesGroup memberSet memberStatementIndexes function =
      not (functionShapeSourceBinding function)
        && Set.member
          (functionShapeStatementIndex function)
          memberStatementIndexes
        && expressionReferencesAnyBinder
          memberSet
          (functionShapeBody function)

expressionReferencesAnyBinder :: Set.Set TypedBinderId -> TypedExpr -> Bool
expressionReferencesAnyBinder binders expression =
  case expression of
    TypedLiteralExpr {} -> False
    TypedVariableExpr _ _ binderReference ->
      maybe False (`Set.member` binders) binderReference
    TypedLambdaExpr _ _ _ body -> child body
    TypedOperatorValueExpr {} -> False
    TypedListExpr _ elements -> any child elements
    TypedTupleExpr _ elements -> any child elements
    TypedApplyExpr _ function argument -> child function || child argument
    TypedTypeApplicationExpr _ function _ _ -> child function
    TypedIfExpr _ condition thenExpression elseExpression ->
      any child [condition, thenExpression, elseExpression]
    TypedPatternCaseExpr _ scrutinee arms ->
      child scrutinee || any armReferencesBinder arms
    TypedBinaryExpr _ _ left right -> child left || child right
    TypedLeftSectionExpr _ left _ -> child left
    TypedRightSectionExpr _ _ right -> child right
    TypedBlockExpr _ statements -> any statementReferencesBinder statements
  where
    child = expressionReferencesAnyBinder binders
    armReferencesBinder (TypedCaseArm _ maybeGuard result) =
      maybe False child maybeGuard || child result
    statementReferencesBinder statement =
      case statement of
        TypedLetStatement _ _ _ _ initializer -> child initializer
        TypedExpressionStatement _ result -> child result
        TypedImplStatement (TypedImplDeclaration _ _ methods) ->
          any methodReferencesBinder methods
        _ -> False
    methodReferencesBinder (TypedMethodDefinition _ _ _ _ body) = child body

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
          | Just (schemeBinder, expectedRepresentation) <- valueSchemeContract scheme,
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
      case binderReference >>= (`Map.lookup` loweringLocalBindings state) of
        Just operand
          | loweredRepresentation (typedNodeRecipe info) == Just (loweredOperandRepresentation operand) ->
              ([], Just operand, state)
        Just _ -> unsupportedExpression path state
        Nothing ->
          case findParameterShape binderReference parameters of
            Just (FunctionParameterShape _ (LoweredParameter parameterId representation))
              | loweredRepresentation (typedNodeRecipe info) == Just representation ->
                  ([], Just (LoweredFunctionParameterOperand parameterId representation), state)
            _ ->
              case findFunctionShape binderReference functions of
                Just function
                  | functionShapeCallableShape function == TypedClosureCallableShape,
                    loweredRepresentation (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
                      lowerClosureValue path parameters function state
                _ -> unsupportedExpression path state
    TypedLambdaExpr info parameterBinder _ _ ->
      case findFunctionShape (Just parameterBinder) functions of
        Just function
          | not (functionShapeSourceBinding function),
            loweredRepresentation (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
              lowerClosureValue path parameters function state
        _ -> unsupportedExpression path state
    TypedTupleExpr info [] ->
      case typedNodeRecipe info of
        TypedUnitRecipe ->
          ([], Just (LoweredImmediateOperand LoweredUnitImmediate), state)
        recipe -> unsupportedRepresentation path recipe state
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
      representationAtPath path (typedNodeRecipe info)
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
  case profileFailures <> resultRepresentationFailures <> scrutineeFailures of
    failures@(_ : _) -> (failures, Nothing, scrutineeState)
    [] ->
      case (maybeResultRepresentation, maybeScrutineeOperand) of
        (Just resultRepresentation, Just scrutineeOperand)
          | Just scrutineeRepresentation <- loweredRepresentation (typedNodeRecipe (typedExpressionInfo scrutinee)),
            loweredOperandRepresentation scrutineeOperand == scrutineeRepresentation ->
              case destination of
                ProduceValue -> lowerArmChain resultRepresentation scrutineeOperand
                FinishFunction expected
                  | expected == resultRepresentation ->
                      lowerArmChain resultRepresentation scrutineeOperand
                _ -> unsupportedExpression path scrutineeState
        _ -> unsupportedExpression path scrutineeState
  where
    profileFailures =
      scalarPatternCaseProfileFailures
        modulePath
        statementPath
        expressionPath
        scrutinee
        arms
    (resultRepresentationFailures, maybeResultRepresentation) =
      representationAtPath path (typedNodeRecipe info)
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
       in case lowerArms
            resultRepresentation
            scrutineeCarrier
            outerSlots
            controlSlots
            controlParameters
            0
            arms
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
      representationAtPath path (typedNodeRecipe (typedExpressionInfo expression))
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
      representationAtPath path (typedNodeRecipe (typedExpressionInfo expression))
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
    TypedManagedTextRecipe -> Just textRepresentation
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
