{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.LoweredIR.Lower.Shapes
  ( analyzeTypedModule,
    patternArmParameters,
    patternCaseTotalPrefixLength,
    orderedClosureLayouts,
    valueSchemeContract,
    loweredIRGeneratedIdentityFailureDetail,
    applicationSpine,
    findFunctionShape,
    findScalarRepresentation,
    findParameterShape,
    findCaptureShape,
    functionClosureRepresentation,
    functionEnvironmentParameter,
    loweredPrimitive,
    representationAtPath,
    integerWidth,
    floatWidth,
  )
where

import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
  ( collectManagedLayoutCatalog,
    constructorApplicationLayout,
    constructorLayoutFor,
    constructorPatternLayoutFor,
    managedLayoutShapeFor,
    nodeInstantiations,
    orderedManagedLayouts,
    productLayoutFields,
    representationForRecipe,
  )
import Jazz.Compiler.LoweredIR.Lower.Requirements
  ( collectRuntimeRequirements,
    requirementsForManagedLayouts,
    textRuntimeServiceApplication,
  )
import Jazz.Compiler.LoweredIR.Lower.Types
import Jazz.Compiler.TypedCore

analyzeTypedModule :: TypedModule -> Either [LoweredIRLoweringFailure] LoweringAnalysis
analyzeTypedModule typedModule = do
  managedLayoutCatalog <- collectManagedLayoutCatalog typedModule
  analyzeTypedModuleWithCatalog managedLayoutCatalog typedModule

analyzeTypedModuleWithCatalog :: ManagedLayoutCatalog -> TypedModule -> Either [LoweredIRLoweringFailure] LoweringAnalysis
analyzeTypedModuleWithCatalog managedLayoutCatalog typedModule@(TypedModule modulePath _ imports exports moduleInterface recursiveGroups statements moduleInfo) =
  case allFailures of
    failures@(_ : _) -> Left failures
    [] ->
      case maybeResultRepresentation of
        Just resultRepresentation ->
          Right
            LoweringAnalysis
              { analyzedModulePath = modulePath,
                analyzedStatements = statements,
                analyzedFunctionShapes = functionShapes,
                analyzedFunctionIndex = functionIndex,
                analyzedResultRepresentation = resultRepresentation,
                analyzedRuntimeRequirements =
                  collectRuntimeRequirements typedModule
                    <> requirementsForManagedLayouts (orderedManagedLayouts managedLayoutCatalog)
              }
        Nothing ->
          Left
            [ LoweredIRLoweringFailure
                (TypedModulePath modulePath)
                LoweredIRUnsupportedRepresentation
                (LoweredIRRecipeFailureDetail (typedNodeRecipe moduleInfo))
            ]
  where
    (shapeFailures, collectedFunctionShapes, localValueNames) =
      collectFunctionShapes managedLayoutCatalog modulePath statements
    functionShapes =
      applyRecursiveClosureGroups managedLayoutCatalog recursiveGroups collectedFunctionShapes
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
                Just (_, representation) <- [valueSchemeContract managedLayoutCatalog scheme]
              ],
          indexedManagedLayoutCatalog = managedLayoutCatalog
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
      representationAtPath managedLayoutCatalog (TypedModulePath modulePath) (typedNodeRecipe moduleInfo)
    profileFailures =
      validateStatementProfiles managedLayoutCatalog modulePath functionIndex (Set.fromList localValueNames) statements
    recursiveFailures =
      recursiveGroupProfileFailures
        managedLayoutCatalog
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
    && null classes
    && null impls
    && all supportedExport exports
    && all supportedInterfaceValue values
    && all supportedDataInterface datas
  where
    sourceFunctions = filter functionShapeSourceBinding functions
    supportedExport (TypedModuleExport namespace identifier) =
      case namespace of
        TypedValueNamespace -> any (matchesIdentifier identifier . functionShapeName) sourceFunctions
        TypedTypeNamespace -> any (dataNameMatches identifier) datas
        TypedConstructorNamespace -> any (dataConstructorMatches identifier) datas
        TypedCapabilityNamespace -> False
    supportedInterfaceValue (TypedValueInterface name _) =
      any ((== name) . functionShapeName) sourceFunctions
    supportedDataInterface (TypedDataInterface (TypedDataDeclaration _ name _ constructors)) =
      currentTypeName name && all currentConstructor constructors
    currentTypeName name =
      case name of
        TypedResolvedName TypedCurrentModule TypedTypeNamespace _ -> True
        _ -> False
    currentConstructor (TypedConstructorDeclaration _ name _ _) =
      case name of
        TypedResolvedName TypedCurrentModule TypedConstructorNamespace _ -> True
        _ -> False
    dataNameMatches identifier (TypedDataInterface (TypedDataDeclaration _ name _ _)) =
      name == TypedResolvedName TypedCurrentModule TypedTypeNamespace identifier
    dataConstructorMatches identifier (TypedDataInterface (TypedDataDeclaration _ _ _ constructors)) =
      any
        (\(TypedConstructorDeclaration _ name _ _) -> name == TypedResolvedName TypedCurrentModule TypedConstructorNamespace identifier)
        constructors
    matchesIdentifier identifier name =
      case name of
        TypedResolvedName TypedCurrentModule TypedValueNamespace candidate ->
          identifier == candidate
        _ -> False

collectFunctionShapes ::
  ManagedLayoutCatalog ->
  [Text] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [FunctionShape], [TypedCoreName])
collectFunctionShapes managedLayoutCatalog modulePath statements =
  (rootFailures, orderedFunctionShapes, localValueNames)
  where
    (rootFailures, rootFunctionShapes, localValueNames) =
      collectRootFunctionShapes managedLayoutCatalog modulePath statements
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
          let capturedRoot = attachFunctionCaptures managedLayoutCatalog rootShapesByBinder rootFunction
           in capturedRoot
                : collectGeneratedFunctionShapes
                  managedLayoutCatalog
                  rootShapesByBinder
                  statementIndex
                  (functionShapeReversedBodyPath capturedRoot)
                  (functionShapeBody capturedRoot)
        Nothing ->
          case statement of
            TypedLetStatement _ _ _ _ expression ->
              collectGeneratedFunctionShapes managedLayoutCatalog rootShapesByBinder statementIndex [0] expression
            TypedExpressionStatement _ expression ->
              collectGeneratedFunctionShapes managedLayoutCatalog rootShapesByBinder statementIndex [0] expression
            _ -> []

applyRecursiveClosureGroups :: ManagedLayoutCatalog -> [TypedRecursiveGroup] -> [FunctionShape] -> [FunctionShape]
applyRecursiveClosureGroups managedLayoutCatalog recursiveGroups initialFunctions =
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
                          managedLayoutCatalog
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

collectRootFunctionShapes ::
  ManagedLayoutCatalog ->
  [Text] ->
  [TypedStatement] ->
  ([LoweredIRLoweringFailure], [FunctionShape], [TypedCoreName])
collectRootFunctionShapes managedLayoutCatalog modulePath =
  go 0 [] [] [] Set.empty Set.empty
  where
    go _ reversedFailures reversedFunctions reversedLocalNames _ _ [] =
      (reverse reversedFailures, reverse reversedFunctions, reverse reversedLocalNames)
    go statementIndex reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities (statement : rest) =
      case statement of
        TypedSignatureStatement {} ->
          continue reversedFailures reversedFunctions reversedLocalNames seenNames seenGeneratedIdentities
        TypedLetStatement _ name _ scheme expression
          | Just _ <- valueSchemeContract managedLayoutCatalog scheme ->
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
                  case collectFunctionShape managedLayoutCatalog modulePath statementIndex name scheme expression of
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
        TypedDataStatement {} ->
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
  ManagedLayoutCatalog ->
  [Text] ->
  Int ->
  TypedCoreName ->
  TypedScheme ->
  TypedExpr ->
  Maybe FunctionShape
collectFunctionShape managedLayoutCatalog modulePath statementIndex name scheme expression = do
  identifier <- localValueIdentifier name
  (schemeBinder, schemeType, schemeRecipe, callableShape) <- monomorphicSchemeContract scheme
  environmentLayout <-
    case callableShape of
      TypedDirectCallableShape -> Just Nothing
      TypedClosureCallableShape -> Just <$> closureEnvironmentLayoutId schemeBinder
  (parameters, resultRepresentation, reversedBodyPath, body) <-
    case callableShape of
      TypedDirectCallableShape ->
        flattenLeadingLambdas managedLayoutCatalog schemeType schemeRecipe [0] [] expression
      TypedClosureCallableShape ->
        collectUnaryClosureShape managedLayoutCatalog schemeType schemeRecipe [0] expression
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

attachFunctionCaptures :: ManagedLayoutCatalog -> Map.Map TypedBinderId FunctionShape -> FunctionShape -> FunctionShape
attachFunctionCaptures managedLayoutCatalog globalFunctions function
  | functionShapeCallableShape function == TypedClosureCallableShape =
      function
        { functionShapeCaptures =
            collectCaptureShapes
              managedLayoutCatalog
              globalFunctions
              (Set.singleton (functionShapeBinder function))
              (Set.fromList (map functionParameterBinder (functionShapeParameters function)))
              (functionShapeBody function)
        }
  | otherwise = function

collectGeneratedFunctionShapes ::
  ManagedLayoutCatalog ->
  Map.Map TypedBinderId FunctionShape ->
  Int ->
  [Int] ->
  TypedExpr ->
  [FunctionShape]
collectGeneratedFunctionShapes managedLayoutCatalog globalFunctions statementIndex reversedExpressionPath expression =
  case expression of
    TypedLambdaExpr info parameterBinder parameterName _ ->
      case ( collectUnaryClosureShape
               managedLayoutCatalog
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
                        managedLayoutCatalog
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
                  managedLayoutCatalog
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
        managedLayoutCatalog
        globalFunctions
        statementIndex
        (childIndex : reversedExpressionPath)
    children = concatMap (uncurry child)
    childPath pathSuffix =
      collectGeneratedFunctionShapes
        managedLayoutCatalog
        globalFunctions
        statementIndex
        (reverse pathSuffix <> reversedExpressionPath)

collectCaptureShapes :: ManagedLayoutCatalog -> Map.Map TypedBinderId FunctionShape -> Set.Set TypedBinderId -> Set.Set TypedBinderId -> TypedExpr -> [CaptureShape]
collectCaptureShapes managedLayoutCatalog globalFunctions initiallyExpanded initiallyBound expression =
  snd (go initiallyExpanded initiallyBound Set.empty expression)
  where
    go expandedFunctions boundBinders seenBinders currentExpression =
      case currentExpression of
        TypedVariableExpr info _ (Just binder)
          | Set.member binder boundBinders -> (seenBinders, [])
          | Map.member binder (catalogConstructors managedLayoutCatalog) -> (seenBinders, [])
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
            Just representation <- representationForRecipe managedLayoutCatalog (typedNodeRecipe info) ->
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

valueSchemeContract :: ManagedLayoutCatalog -> TypedScheme -> Maybe (TypedBinderId, LoweredRepresentation)
valueSchemeContract managedLayoutCatalog (TypedScheme owner typeParameters evidence primitive typeValue recipe Nothing)
  | null typeParameters,
    null evidence,
    null primitive =
      case valueRepresentation managedLayoutCatalog typeValue recipe of
        Just representation -> Just (owner, representation)
        Nothing -> Nothing
valueSchemeContract _ _ = Nothing

collectUnaryClosureShape ::
  ManagedLayoutCatalog ->
  TypedType ->
  TypedRepresentationRecipe ->
  [Int] ->
  TypedExpr ->
  Maybe ([FunctionParameterShape], LoweredRepresentation, [Int], TypedExpr)
collectUnaryClosureShape managedLayoutCatalog expectedType expectedRecipe reversedExpressionPath expression =
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
      parameterRepresentation <- valueRepresentation managedLayoutCatalog argumentType argumentRecipe
      resultRepresentation <- valueRepresentation managedLayoutCatalog resultType resultRecipe
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
  ManagedLayoutCatalog ->
  TypedType ->
  TypedRepresentationRecipe ->
  [Int] ->
  [FunctionParameterShape] ->
  TypedExpr ->
  Maybe ([FunctionParameterShape], LoweredRepresentation, [Int], TypedExpr)
flattenLeadingLambdas managedLayoutCatalog expectedType expectedRecipe reversedExpressionPath reversedParameters expression =
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
      parameterRepresentation <- valueRepresentation managedLayoutCatalog argumentType argumentRecipe
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
        managedLayoutCatalog
        resultType
        resultRecipe
        (0 : reversedExpressionPath)
        (parameter : reversedParameters)
        body
    _ -> do
      resultRepresentation <- valueRepresentation managedLayoutCatalog expectedType expectedRecipe
      if typedNodeType (typedExpressionInfo expression) == expectedType
        && typedNodeRecipe (typedExpressionInfo expression) == expectedRecipe
        then Just (reverse reversedParameters, resultRepresentation, reversedExpressionPath, expression)
        else Nothing

scalarRepresentation :: ManagedLayoutCatalog -> TypedType -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
scalarRepresentation managedLayoutCatalog typeValue recipe =
  case (typeValue, representationForRecipe managedLayoutCatalog recipe) of
    (TypedTupleType [], Just LoweredUnitRepresentation) -> Just LoweredUnitRepresentation
    (TypedBoolType, Just LoweredBoolRepresentation) -> Just LoweredBoolRepresentation
    (TypedCharType, Just LoweredCharRepresentation) -> Just LoweredCharRepresentation
    (TypedIntType, Just representation@LoweredSignedIntegerRepresentation {}) -> Just representation
    (TypedFloatType, Just representation@LoweredFloatRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredSignedIntegerRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredUnsignedIntegerRepresentation {}) -> Just representation
    (TypedNumericType _, Just representation@LoweredFloatRepresentation {}) -> Just representation
    _ -> Nothing

valueRepresentation :: ManagedLayoutCatalog -> TypedType -> TypedRepresentationRecipe -> Maybe LoweredRepresentation
valueRepresentation managedLayoutCatalog typeValue recipe =
  case (typeValue, representationForRecipe managedLayoutCatalog recipe) of
    (TypedFunctionType {}, Just representation@LoweredClosureRepresentation {}) -> Just representation
    (TypedTextType, Just representation@LoweredManagedReferenceRepresentation {}) -> Just representation
    (TypedTupleType (_ : _), Just representation@LoweredManagedReferenceRepresentation {}) -> Just representation
    (TypedDataType {}, Just representation@LoweredManagedReferenceRepresentation {}) -> Just representation
    _ -> scalarRepresentation managedLayoutCatalog typeValue recipe

validateStatementProfiles ::
  ManagedLayoutCatalog ->
  [Text] ->
  FunctionIndex ->
  Set.Set TypedCoreName ->
  [TypedStatement] ->
  [LoweredIRLoweringFailure]
validateStatementProfiles managedLayoutCatalog modulePath functions localValueNames statements =
  sortOn loweringFailurePath (statementFailures <> generatedFunctionFailures)
  where
    loweringFailurePath (LoweredIRLoweringFailure path _ _) = path
    statementFailures = go 0 [] statements
    generatedFunctionFailures =
      concat
        [ inspectExpression
            managedLayoutCatalog
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
                          managedLayoutCatalog
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
                      managedLayoutCatalog
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
                  managedLayoutCatalog
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
  ManagedLayoutCatalog ->
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
inspectExpression managedLayoutCatalog modulePath statementPath expressionPath functions localValueNames allowEntryLocals parameters captures expression =
  case expression of
    TypedLiteralExpr info _ ->
      representationCheck info
    TypedVariableExpr info name binderReference ->
      case binderReference >>= (\binder -> constructorLayoutFor managedLayoutCatalog binder (nodeInstantiations info)) of
        Just constructor
          | null (managedConstructorFields constructor),
            representationForRecipe managedLayoutCatalog (typedNodeRecipe info)
              == Just (LoweredManagedReferenceRepresentation (managedConstructorLayoutId constructor)) ->
              noExpressionFailures
        Just _ ->
          oneFailure
            LoweredIRCallableValueUnsupported
            (LoweredIRNameFailureDetail name)
        Nothing ->
          case findParameterShape binderReference parameters of
            Just (FunctionParameterShape _ (LoweredParameter _ expectedRepresentation))
              | representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just expectedRepresentation ->
                  noExpressionFailures
              | otherwise -> representationCheck info
            Nothing ->
              case findCaptureShape binderReference captures of
                Just capture
                  | representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just (captureShapeRepresentation capture) ->
                      noExpressionFailures
                Just _ -> representationCheck info
                Nothing ->
                  case findFunctionShape binderReference functions of
                    Just function
                      | functionShapeCallableShape function == TypedClosureCallableShape,
                        representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
                          noExpressionFailures
                    Just _ ->
                      oneFailure
                        LoweredIRCallableValueUnsupported
                        (LoweredIRNameFailureDetail name)
                    Nothing
                      | Just expectedRepresentation <- findScalarRepresentation binderReference functions,
                        allowEntryLocals,
                        representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just expectedRepresentation ->
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
            representationForRecipe managedLayoutCatalog (typedNodeRecipe info) == Just (functionClosureRepresentation function) ->
              noExpressionFailures
        _ ->
          oneFailure
            LoweredIRCallableValueUnsupported
            LoweredIRNoFailureDetail
    TypedTupleExpr info [] ->
      representationCheck info
    TypedTupleExpr info elements ->
      case (representationForRecipe managedLayoutCatalog (typedNodeRecipe info), typedNodeRecipe info) of
        (Just (LoweredManagedReferenceRepresentation layoutId), TypedManagedProductRecipe fieldRecipes)
          | length fieldRecipes == length elements,
            productLayoutFields managedLayoutCatalog layoutId == traverse (representationForRecipe managedLayoutCatalog) fieldRecipes ->
              combineExpressionChecks (zipWith child [0 ..] elements)
        _ -> representationCheck info
    TypedBinaryExpr info operator left right ->
      case unsupportedManagedEqualityRecipe operator left of
        Just recipe ->
          oneFailure
            LoweredIRUnsupportedRepresentation
            (LoweredIRRecipeFailureDetail recipe)
        Nothing ->
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
      case patternCaseProfileFailures managedLayoutCatalog modulePath statementPath expressionPath scrutinee arms of
        failures@(_ : _) -> failures
        [] ->
          combineExpressionChecks
            ( representationCheck info
                : child 0 scrutinee
                : [ let armParameters = patternArmParameters managedLayoutCatalog patternValue <> parameters
                     in maybe
                          []
                          ( inspectExpression
                              managedLayoutCatalog
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
                            managedLayoutCatalog
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
        managedLayoutCatalog
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
      case representationForRecipe managedLayoutCatalog (typedNodeRecipe info) of
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
    unsupportedManagedEqualityRecipe operator operand
      | operator `elem` [TypedBuiltinOperator "==", TypedBuiltinOperator "!="],
        let recipe = typedNodeRecipe (typedExpressionInfo operand),
        isManagedStructuredRecipe recipe =
          Just recipe
      | otherwise = Nothing
    isManagedStructuredRecipe recipe =
      case recipe of
        TypedManagedProductRecipe {} -> True
        TypedManagedVariantRecipe {} -> True
        _ -> False
    child childIndex =
      inspectExpression
        managedLayoutCatalog
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

data LowererCoveragePattern
  = LowererCoverageCatchAll
  | LowererCoverageLiteral
  | LowererCoverageConstructor Integer [LowererCoveragePattern]
  | LowererCoverageTuple [LowererCoveragePattern]
  | LowererCoverageOr [LowererCoveragePattern]
  deriving (Eq)

patternCaseProfileFailures ::
  ManagedLayoutCatalog ->
  [Text] ->
  [Int] ->
  [Int] ->
  TypedExpr ->
  [TypedCaseArm] ->
  [LoweredIRLoweringFailure]
patternCaseProfileFailures managedLayoutCatalog modulePath statementPath reversedExpressionPath scrutinee arms =
  case patternCaseTotalPrefixLength managedLayoutCatalog statementPath reversedExpressionPath scrutinee arms of
    Left failure -> [failure]
    Right maybeTotalPrefixLength
      | not (admittedScrutinee scrutineeInfo scrutineeRepresentation) ->
          [expressionFailure LoweredIRUnsupportedPattern]
      | earlyUnguardedCatchAll arms ->
          [expressionFailure LoweredIRIncompletePatternCase]
      | maybeTotalPrefixLength == Nothing ->
          [expressionFailure LoweredIRIncompletePatternCase]
      | otherwise -> []
  where
    expressionPath = reverse reversedExpressionPath
    scrutineeInfo = typedExpressionInfo scrutinee
    scrutineeRepresentation = representationForRecipe managedLayoutCatalog (typedNodeRecipe scrutineeInfo)
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
    admittedScrutinee info maybeRepresentation =
      case (typedNodeRecipe info, maybeRepresentation) of
        (_, Just representation)
          | scalarRepresentation managedLayoutCatalog (typedNodeType info) (typedNodeRecipe info) == Just representation ->
              True
        (TypedManagedProductRecipe _, Just (LoweredManagedReferenceRepresentation layoutId)) ->
          case managedLayoutShapeFor managedLayoutCatalog layoutId of
            Just LoweredProductLayout {} -> True
            _ -> False
        (TypedManagedVariantRecipe _ _, Just (LoweredManagedReferenceRepresentation layoutId)) ->
          case managedLayoutShapeFor managedLayoutCatalog layoutId of
            Just LoweredVariantLayouts {} -> True
            _ -> False
        _ -> False
    expressionFailure kind =
      LoweredIRLoweringFailure
        (TypedExpressionPath modulePath statementPath expressionPath)
        kind
        LoweredIRNoFailureDetail

patternCaseTotalPrefixLength ::
  ManagedLayoutCatalog ->
  [Int] ->
  [Int] ->
  TypedExpr ->
  [TypedCaseArm] ->
  Either LoweredIRLoweringFailure (Maybe Int)
patternCaseTotalPrefixLength managedLayoutCatalog statementPath reversedExpressionPath scrutinee arms =
  earliestTotalPrefix <$> traverse admitArm (zip [0 :: Int ..] arms)
  where
    expressionPath = reverse reversedExpressionPath
    scrutineeRepresentation = representationForRecipe managedLayoutCatalog (typedNodeRecipe (typedExpressionInfo scrutinee))
    admitArm (armIndex, TypedCaseArm patternValue maybeGuard _) = do
      admittedPattern <-
        admitPattern
          managedLayoutCatalog
          statementPath
          scrutineeRepresentation
          True
          (expressionPath <> [armIndex])
          patternValue
      pure (admittedPattern, maybeGuard)
    earliestTotalPrefix = go 0 []
      where
        go _ _ [] = Nothing
        go prefixLength coverageRows ((patternValue, maybeGuard) : remainingArms) =
          let nextPrefixLength = prefixLength + 1
              nextCoverageRows =
                case maybeGuard of
                  Just _ -> coverageRows
                  Nothing ->
                    coverageRows
                      <> [ [alternative]
                         | alternative <- expandTopLevelAlternative patternValue
                         ]
           in if coverageMatrixTotal managedLayoutCatalog [scrutineeRepresentation] nextCoverageRows
                then Just nextPrefixLength
                else go nextPrefixLength nextCoverageRows remainingArms

admitPattern ::
  ManagedLayoutCatalog ->
  [Int] ->
  Maybe LoweredRepresentation ->
  Bool ->
  [Int] ->
  TypedPattern ->
  Either LoweredIRLoweringFailure LowererCoveragePattern
admitPattern managedLayoutCatalog statementPath expectedRepresentation allowTopLevelOr patternPath patternValue =
  case patternValue of
    TypedWildcardPattern info
      | matchingRepresentation info -> Right LowererCoverageCatchAll
    TypedVariablePattern info _ _
      | matchingRepresentation info -> Right LowererCoverageCatchAll
    TypedLiteralPattern info literal
      | matchingRepresentation info ->
          case (expectedRepresentation, literal) of
            (Just representation, _)
              | scalarRepresentation managedLayoutCatalog (typedNodeType info) (typedNodeRecipe info) == Just representation ->
                  Right LowererCoverageLiteral
            _ -> unsupported
    TypedConstructorPattern info constructorName fields
      | matchingRepresentation info ->
          case constructorPatternLayoutFor managedLayoutCatalog info constructorName of
            Just constructorLayout
              | expectedRepresentation
                  == Just (LoweredManagedReferenceRepresentation (managedConstructorLayoutId constructorLayout)),
                length fields == length (managedConstructorFields constructorLayout) ->
                  LowererCoverageConstructor (toInteger (managedConstructorTag constructorLayout))
                    <$> traverseChildPatterns (managedConstructorFields constructorLayout) fields
            _ -> unsupported
    TypedTuplePattern info fields
      | matchingRepresentation info ->
          case expectedRepresentation of
            Just (LoweredManagedReferenceRepresentation layoutId) ->
              case productLayoutFields managedLayoutCatalog layoutId of
                Just fieldRepresentations
                  | length fields == length fieldRepresentations ->
                      LowererCoverageTuple <$> traverseChildPatterns fieldRepresentations fields
                _ -> unsupported
            _ -> unsupported
    TypedAsPattern info _ _ nestedPattern
      | matchingRepresentation info ->
          admitPattern managedLayoutCatalog statementPath expectedRepresentation False (patternPath <> [0]) nestedPattern
    TypedOrPattern info alternatives
      | allowTopLevelOr,
        matchingRepresentation info ->
          LowererCoverageOr
            <$> traverse
              (uncurry (admitPattern managedLayoutCatalog statementPath expectedRepresentation False))
              [ (patternPath <> [alternativeIndex], alternative)
              | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives
              ]
    _ -> unsupported
  where
    matchingRepresentation info =
      case (representationForRecipe managedLayoutCatalog (typedNodeRecipe info), expectedRepresentation) of
        (Just actual, Just expected) -> actual == expected
        _ -> False
    traverseChildPatterns representations patterns =
      traverse
        (\(childIndex, representation, patternChild) -> admitPattern managedLayoutCatalog statementPath (Just representation) False (patternPath <> [childIndex]) patternChild)
        [ (childIndex, representation, patternChild)
        | (childIndex, (representation, patternChild)) <- zip [0 :: Int ..] (zip representations patterns)
        ]
    unsupported =
      Left
        ( LoweredIRLoweringFailure
            (TypedPatternPath (catalogModulePath managedLayoutCatalog) statementPath patternPath)
            LoweredIRUnsupportedPattern
            LoweredIRNoFailureDetail
        )

expandTopLevelAlternative :: LowererCoveragePattern -> [LowererCoveragePattern]
expandTopLevelAlternative patternValue =
  case patternValue of
    LowererCoverageOr alternatives -> alternatives
    _ -> [patternValue]

coverageMatrixTotal :: ManagedLayoutCatalog -> [Maybe LoweredRepresentation] -> [[LowererCoveragePattern]] -> Bool
coverageMatrixTotal _ [] rows = any null rows
coverageMatrixTotal _ (Nothing : _) _ = False
coverageMatrixTotal managedLayoutCatalog (Just representation : remainingRepresentations) rows =
  case representation of
    LoweredManagedReferenceRepresentation layoutId ->
      case managedLayoutShapeFor managedLayoutCatalog layoutId of
        Just (LoweredProductLayout fields) ->
          coverageMatrixTotal managedLayoutCatalog (map Just fields <> remainingRepresentations) (specializeProduct fields rows)
        Just (LoweredVariantLayouts variants) ->
          all
            (\(LoweredVariantLayout tag fields) -> coverageMatrixTotal managedLayoutCatalog (map Just fields <> remainingRepresentations) (specializeVariant tag fields rows))
            variants
        _ -> openRepresentationTotal
    _ -> openRepresentationTotal
  where
    openRepresentationTotal =
      coverageMatrixTotal
        managedLayoutCatalog
        remainingRepresentations
        [remainingPatterns | LowererCoverageCatchAll : remainingPatterns <- rows]

specializeProduct :: [LoweredRepresentation] -> [[LowererCoveragePattern]] -> [[LowererCoveragePattern]]
specializeProduct fields = foldMap specialize
  where
    specialize row =
      case row of
        LowererCoverageCatchAll : remaining -> [replicate (length fields) LowererCoverageCatchAll <> remaining]
        LowererCoverageTuple patterns : remaining
          | length patterns == length fields -> [patterns <> remaining]
        _ -> []

specializeVariant :: Integer -> [LoweredRepresentation] -> [[LowererCoveragePattern]] -> [[LowererCoveragePattern]]
specializeVariant tag fields = foldMap specialize
  where
    specialize row =
      case row of
        LowererCoverageCatchAll : remaining -> [replicate (length fields) LowererCoverageCatchAll <> remaining]
        LowererCoverageConstructor patternTag patterns : remaining
          | patternTag == tag,
            length patterns == length fields ->
              [patterns <> remaining]
        _ -> []

patternArmParameters :: ManagedLayoutCatalog -> TypedPattern -> [FunctionParameterShape]
patternArmParameters managedLayoutCatalog patternValue =
  [ FunctionParameterShape
      binder
      (LoweredParameter (patternParameterId parameterIndex) representation)
  | (parameterIndex, (binder, info)) <- zip [0 :: Int ..] (patternBindings patternValue),
    Just representation <- [representationForRecipe managedLayoutCatalog (typedNodeRecipe info)]
  ]
  where
    patternParameterId parameterIndex =
      LoweredParameterId
        ( if parameterIndex == 0
            then "pattern"
            else "pattern" <> Text.pack (show parameterIndex)
        )
    patternBindings patternNode =
      case patternNode of
        TypedVariablePattern info binder _ -> [(binder, info)]
        TypedConstructorPattern _ _ patterns -> concatMap patternBindings patterns
        TypedTuplePattern _ patterns -> concatMap patternBindings patterns
        TypedAsPattern info binder _ nestedPattern -> (binder, info) : patternBindings nestedPattern
        TypedOrPattern _ [] -> []
        TypedOrPattern _ (alternative : _) -> patternBindings alternative
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
  ManagedLayoutCatalog ->
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
inspectApplication managedLayoutCatalog modulePath statementPath expressionPath functions localValueNames allowEntryLocals parameters captures expression =
  combineExpressionChecks
    (targetCheck : map (uncurry inspectArgument) arguments)
  where
    path = TypedExpressionPath modulePath statementPath (reverse expressionPath)
    (callee, calleePath, arguments) = applicationSpine expressionPath expression
    inspectArgument argumentPath argument =
      inspectExpression
        managedLayoutCatalog
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
      case constructorApplicationLayout managedLayoutCatalog callee of
        Just constructor
          | length arguments == length (managedConstructorFields constructor) -> []
          | otherwise ->
              [ LoweredIRLoweringFailure
                  path
                  LoweredIRCallArityUnsupported
                  (LoweredIRArityFailureDetail (length (managedConstructorFields constructor)) (length arguments))
              ]
        Nothing -> case textRuntimeServiceApplication expression of
          Just _ -> []
          Nothing -> case callee of
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
                managedLayoutCatalog
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
  ManagedLayoutCatalog ->
  [Text] ->
  FunctionIndex ->
  [FunctionShape] ->
  [FunctionDeclaration] ->
  [LoweredIRLoweringFailure]
recursiveGroupProfileFailures managedLayoutCatalog modulePath functions unsharedFunctions declarations =
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
                managedLayoutCatalog
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
  ManagedLayoutCatalog ->
  TypedCoreValidationPath ->
  TypedRepresentationRecipe ->
  ([LoweredIRLoweringFailure], Maybe LoweredRepresentation)
representationAtPath managedLayoutCatalog path recipe =
  case representationForRecipe managedLayoutCatalog recipe of
    Just representation -> ([], Just representation)
    Nothing ->
      ( [ LoweredIRLoweringFailure
            path
            LoweredIRUnsupportedRepresentation
            (LoweredIRRecipeFailureDetail recipe)
        ],
        Nothing
      )

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
