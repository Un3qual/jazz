{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module JazzNext.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionProfile (..),
    TypedCoreProductionStatus (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredProductionFailure (..),
    InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    ProvisionalTypedScope (..),
    blockProductionFailureKindAndDetail,
    finalizeTypedCoreExpressionDirectCall,
  ) where

import Data.Either (partitionEithers)
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST (Literal (..), NumericType (..), Statement (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import JazzNext.Compiler.ModuleExports (ModuleExport (..), exportInventoryEntries)
import JazzNext.Compiler.ModuleGraph (ResolvedModule (..))
import JazzNext.Compiler.Name
  ( GeneratedNameKind (OperatorBinding),
    Name (..),
    NameNamespace (..),
    identifierText
  )
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
import JazzNext.Compiler.TypeInference.Solver
  ( integerLiteralRangeFitsNumericType,
    resolveType
  )
import JazzNext.Compiler.TypeInference.State (InferState)
import JazzNext.Compiler.TypeInference.Types (ExpressionType (..), TypeBinding (..))

data TypedCoreProductionProfile
  = TypedCoreExpressionDirectCallProfile
  deriving (Eq, Show)

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

data TypedCoreProductionPath
  = TypedCoreProductionInputPath
  | TypedCoreProductionModulePath [Text]
  | TypedCoreProductionStatementPath [Text] Int
  | TypedCoreProductionExpressionPath [Text] Int [Int]
  deriving (Eq, Show)

data TypedCoreProductionFailureKind
  = TypedCoreModulePathMismatch
  | TypedCoreInvalidPortableSourcePath
  | TypedCoreResolvedImportsUnsupported
  | TypedCoreImportedInputsUnsupported
  | TypedCoreAmbientPreludeInputUnsupported
  | TypedCoreUnsupportedRootExpression
  | TypedCoreManagedValueUnsupported
  | TypedCoreStructuredValueUnsupported
  | TypedCoreControlFlowUnsupported
  | TypedCorePatternCaseUnsupported
  | TypedCoreNestedBlockUnsupported
  | TypedCoreUserDefinedOperatorUnsupported
  | TypedCoreCallableValueUnsupported
  | TypedCoreCallArityUnsupported
  | TypedCoreCaptureUnsupported
  | TypedCoreRecursiveFunctionUnsupported
  | TypedCoreFunctionRebindingUnsupported
  | TypedCoreDuplicateParameterUnsupported
  | TypedCoreNonMonomorphicFunctionUnsupported
  | TypedCoreNonLocalCallUnsupported
  | TypedCoreUnsupportedExport
  | TypedCoreUnresolvedExpressionType
  deriving (Eq, Show)

data TypedCoreProductionFailureDetail
  = TypedCoreNoFailureDetail
  | TypedCoreTextValueDetail
  | TypedCoreListValueDetail
  | TypedCoreTupleValueDetail
  | TypedCoreDataValueDetail
  | TypedCoreConditionalDetail
  | TypedCorePatternCaseDetail
  | TypedCoreLocalBlockDetail
  | TypedCoreUnsupportedRootDetail
  | TypedCoreNameDetail Text
  | TypedCoreArityDetail Int Int
  deriving (Eq, Show)

data TypedCoreProductionFailure
  = TypedCoreProductionFailure
      TypedCoreProductionPath
      TypedCoreProductionFailureKind
      TypedCoreProductionFailureDetail
  deriving (Eq, Show)

data TypedCoreProductionMode
  = InferenceOnly
  | ProduceTypedCoreExpressionDirectCall
  deriving (Eq, Show)

-- | Keep the unsupported block classification beside the failure contract so
-- root and nested production traversals cannot drift apart.
blockProductionFailureKindAndDetail ::
  [Statement] ->
  (TypedCoreProductionFailureKind, TypedCoreProductionFailureDetail)
blockProductionFailureKindAndDetail statements
  | any isDataStatement statements =
      (TypedCoreStructuredValueUnsupported, TypedCoreDataValueDetail)
  | otherwise =
      (TypedCoreNestedBlockUnsupported, TypedCoreLocalBlockDetail)
  where
    isDataStatement statement =
      case statement of
        SData {} -> True
        _ -> False

-- | The private result threaded by production-aware inference. Existing
-- inference-only helpers retain no provisional node.
data InferredExpr = InferredExpr
  { inferredExpressionType :: Maybe ExpressionType,
    inferredProvisionalExpr :: Maybe ProvisionalTypedExpr,
    inferredProductionFailures :: [InferredProductionFailure]
  }
  deriving (Eq, Show)

data InferredProductionFailure
  = InferredProductionFailure
      [Int]
      TypedCoreProductionFailureKind
      TypedCoreProductionFailureDetail
  deriving (Eq, Show)

data ProvisionalTypedExpr
  = ProvisionalUnitExpression
  | ProvisionalLiteralExpression Literal ExpressionType
  | ProvisionalBinaryExpression Text ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalVariableExpression Name ExpressionType
  | ProvisionalLambdaExpression Name ExpressionType ProvisionalTypedExpr
  | ProvisionalApplyExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalScopeStatements [ProvisionalTypedStatement]
  | ProvisionalUnsupportedExpression TypedCoreProductionFailureKind TypedCoreProductionFailureDetail
  | ProvisionalRetainedFailures [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalTypedStatement
  = ProvisionalSignature Int Name SourceSpan ExpressionType
  | ProvisionalFunctionBinding Int Name SourceSpan ExpressionType Bool (Maybe TypeBinding) ProvisionalTypedExpr
  | ProvisionalTerminalExpression Int SourceSpan ProvisionalTypedExpr
  | ProvisionalFunctionFailures Int [InferredProductionFailure]
  | ProvisionalUnsupportedStatement Int
  deriving (Eq, Show)

newtype ProvisionalTypedScope = ProvisionalTypedScope ProvisionalTypedExpr
  deriving (Eq, Show)

data FunctionProfile = FunctionProfile
  { functionStatementIndex :: Int,
    functionType :: ExpressionType,
    functionArity :: Int,
    functionExpression :: ProvisionalTypedExpr
  }

-- | Finalize the initial unit-only root against the permanent contract.
-- Future profile slices extend the provisional scope rather than changing the
-- typed-core constructors themselves.
finalizeTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ResolvedModule ->
  InferState ->
  ProvisionalTypedScope ->
  TypedCoreProductionStatus
finalizeTypedCoreExpressionDirectCall sourcePath resolvedModule state (ProvisionalTypedScope provisionalScope) =
  case provisionalScope of
    ProvisionalScopeStatements provisionalStatements
      | not (hasTerminalResult provisionalStatements) ->
          TypedCoreProductionUnsupported [missingModuleResultFailure]
      | otherwise ->
          let functions = functionTable provisionalStatements
              reboundFunctions = reboundFunctionStatements provisionalStatements
              recursiveNames = recursiveFunctionNames functions
              finalizedStatements = map (finalizeStatement functions reboundFunctions recursiveNames) provisionalStatements
              exportResult = finalizeExports functions
              productionFailures = concatMap fst finalizedStatements <> fst exportResult
           in case productionFailures of
                _ : _ -> TypedCoreProductionUnsupported productionFailures
                [] ->
                  let typedStatements = map requireTypedStatement finalizedStatements
                   in case reverse typedStatements of
                        terminalStatement@TypedExpressionStatement {} : _ ->
                          let programValue =
                                typedProgram
                                  (snd exportResult)
                                  typedStatements
                                  (typedStatementInfo terminalStatement)
                           in case validateTypedProgram programValue of
                                [] -> TypedCoreProductionSucceeded programValue
                                failures -> TypedCoreProductionInvariantFailures failures
                        _ -> TypedCoreProductionUnsupported [missingModuleResultFailure]
    ProvisionalUnsupportedExpression kind detail ->
      TypedCoreProductionUnsupported [failureAt 0 [] kind detail]
    _ -> TypedCoreProductionUnsupported [failureAt 0 [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
  where
    modulePath = resolvedModulePath resolvedModule

    typedProgram typedInterface typedStatements moduleInfo =
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            sourcePath
            []
            (typedExports typedInterface)
            typedInterface
            typedStatements
            moduleInfo
        ]
        modulePath

    hasTerminalResult statements =
      case reverse statements of
        ProvisionalTerminalExpression {} : _ -> True
        _ -> False

    missingModuleResultFailure =
      TypedCoreProductionFailure
        (TypedCoreProductionModulePath modulePath)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

    typedExports (TypedModuleInterface values _ _ _) =
      [TypedModuleExport TypedValueNamespace name | TypedValueInterface (TypedResolvedName _ _ name) _ <- values]

    failureAt statementIndex childPath kind detail =
      TypedCoreProductionFailure (TypedCoreProductionExpressionPath modulePath statementIndex childPath) kind detail

    finalizeStatement functions reboundFunctions recursiveNames statement =
      case statement of
        ProvisionalSignature statementIndex name spanValue expressionType ->
          case callableInfo statementIndex [] expressionType of
            Left failure -> ([failure], Nothing)
            Right info ->
              let typedName = resolvedValueName name
                  owner = binderAt statementIndex [] typedName
               in ([], Just (TypedSignatureStatement owner typedName (typedSpan spanValue) (scheme owner info)))
        ProvisionalFunctionBinding statementIndex name spanValue expressionType _forwardEligible maybeBinding expression ->
          let typedName = resolvedValueName name
              owner = binderAt statementIndex [] typedName
              rebindingFailures =
                [ statementFailure statementIndex TypedCoreFunctionRebindingUnsupported (TypedCoreNameDetail (identifierText name))
                | Map.member statementIndex reboundFunctions
                ]
              recursiveFailures =
                [ statementFailure statementIndex TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail (identifierText name))
                | Set.member name recursiveNames
                ]
              schemeFailures =
                case maybeBinding of
                  Just PlainTypeBinding {} -> []
                  _ -> [statementFailure statementIndex TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail (identifierText name))]
              shapeFailures =
                case expression of
                  ProvisionalLambdaExpression {} -> []
                  _ -> [statementFailure statementIndex TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
              (expressionFailures, maybeExpression) =
                finalizeExpression functions statementIndex [0] Set.empty False expression
              infoResult = callableInfo statementIndex [] expressionType
              infoFailures = either (: []) (const []) infoResult
              failures = rebindingFailures <> recursiveFailures <> schemeFailures <> shapeFailures <> infoFailures <> expressionFailures
              typedStatement = do
                info <- either (const Nothing) Just infoResult
                typedExpression <- maybeExpression
                pure (TypedLetStatement owner typedName (typedSpan spanValue) (scheme owner info) typedExpression)
           in (failures, if null failures then typedStatement else Nothing)
        ProvisionalTerminalExpression statementIndex spanValue expression ->
          let (failures, maybeTypedExpression) =
                finalizeExpression functions statementIndex [] Set.empty False expression
           in (failures, TypedExpressionStatement (typedSpan spanValue) <$> maybeTypedExpression)
        ProvisionalFunctionFailures statementIndex failures ->
          (map (qualifyInferredFailure statementIndex [0]) failures, Nothing)
        ProvisionalUnsupportedStatement statementIndex ->
          ([statementFailure statementIndex TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail], Nothing)

    finalizeExpression functions statementIndex childPath parameters calleePosition expression =
      case expression of
        ProvisionalUnitExpression ->
          ([], Just (TypedTupleExpr unitInfo []))
        ProvisionalLiteralExpression literal expressionType ->
          case scalarInfo statementIndex childPath expressionType of
            Left failure -> ([failure], Nothing)
            Right info ->
              case typedLiteral statementIndex childPath literal info of
                Left failure -> ([failure], Nothing)
                Right literalValue -> ([], Just (TypedLiteralExpr info literalValue))
        ProvisionalBinaryExpression operatorSymbol expressionType left right
          | operatorSymbol `elem` admittedOperators ->
              let (operatorFailures, maybeInfo) =
                    case scalarInfo statementIndex childPath expressionType of
                      Left failure -> ([failure], Nothing)
                      Right info -> ([], Just info)
                  (leftFailures, maybeLeft) = finalizeExpression functions statementIndex (childPath <> [0]) parameters False left
                  (rightFailures, maybeRight) = finalizeExpression functions statementIndex (childPath <> [1]) parameters False right
                  failures = operatorFailures <> leftFailures <> rightFailures
                  typedExpression =
                    TypedBinaryExpr <$> maybeInfo <*> pure (TypedBuiltinOperator operatorSymbol) <*> maybeLeft <*> maybeRight
               in (failures, if null failures then typedExpression else Nothing)
          | otherwise ->
              let (leftFailures, _) = finalizeExpression functions statementIndex (childPath <> [0]) parameters False left
                  (rightFailures, _) = finalizeExpression functions statementIndex (childPath <> [1]) parameters False right
               in (failureAt statementIndex childPath TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail : leftFailures <> rightFailures, Nothing)
        ProvisionalVariableExpression name expressionType
          | Set.member name parameters ->
              case scalarInfo statementIndex childPath expressionType of
                Left failure -> ([failure], Nothing)
                Right info -> ([], Just (TypedVariableExpr info (resolvedValueName name)))
          | Just function <- Map.lookup name functions ->
              if calleePosition
                then case callableInfo statementIndex childPath (functionType function) of
                  Left failure -> ([failure], Nothing)
                  Right info -> ([], Just (TypedVariableExpr info (resolvedValueName name)))
                else
                  ( [failureAt statementIndex childPath TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))],
                    Nothing
                  )
          | otherwise ->
              ( [failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))],
                Nothing
              )
        ProvisionalLambdaExpression parameterName expressionType body ->
          case callableInfo statementIndex childPath expressionType of
            Left failure -> ([failure], Nothing)
            Right info ->
              let duplicateParameterFailures =
                    [ failureAt
                        statementIndex
                        childPath
                        TypedCoreDuplicateParameterUnsupported
                        (TypedCoreNameDetail (identifierText parameterName))
                    | Set.member parameterName parameters
                    ]
                  parameterPath = childPath
                  (bodyFailures, maybeBody) =
                    finalizeExpression functions statementIndex (childPath <> [0]) (Set.insert parameterName parameters) False body
                  parameterBinder = TypedBinderId (modulePath, statementIndex : parameterPath, resolvedValueName parameterName)
                  failures = duplicateParameterFailures <> bodyFailures
               in (failures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
        ProvisionalApplyExpression _ _ _ ->
          finalizeApplicationSpine functions statementIndex childPath parameters expression
        ProvisionalScopeStatements _ -> ([failureAt statementIndex childPath TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail], Nothing)
        ProvisionalUnsupportedExpression kind detail -> ([failureAt statementIndex childPath kind detail], Nothing)
        ProvisionalRetainedFailures failures ->
          (map (qualifyInferredFailure statementIndex childPath) failures, Nothing)

    qualifyInferredFailure statementIndex parentPath (InferredProductionFailure relativePath kind detail) =
      failureAt statementIndex (parentPath <> relativePath) kind detail

    finalizeApplicationSpine functions statementIndex childPath parameters expression =
      let (callee, arguments, resultTypes) = applicationSpine expression
       in case callee of
            ProvisionalVariableExpression name _
              | Just function <- Map.lookup name functions ->
                  let expectedArity = functionArity function
                      actualArity = length arguments
                   in if actualArity /= expectedArity
                        then
                          ( [failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)],
                            Nothing
                          )
                        else
                          let (calleeFailures, maybeCallee) =
                                finalizeExpression functions statementIndex childPath parameters True callee
                              finalizedArguments =
                                map
                                  ( \(argumentPath, argument) ->
                                      finalizeExpression
                                        functions
                                        statementIndex
                                        (childPath <> argumentPath)
                                        parameters
                                        False
                                        argument
                                  )
                                  arguments
                              (resultInfoFailures, resultInfos) =
                                partitionEithers
                                  ( zipWith
                                      (scalarOrCallableInfo statementIndex)
                                      [childPath <> replicate remainingApplications 0 | remainingApplications <- reverse [0 .. actualArity - 1]]
                                      resultTypes
                                  )
                              failures = calleeFailures <> concatMap fst finalizedArguments <> resultInfoFailures
                              maybeArguments = traverse snd finalizedArguments
                              typedApplication = do
                                typedCallee <- maybeCallee
                                typedArguments <- maybeArguments
                                pure
                                  ( foldl'
                                      (\typedFunction (info, argument) -> TypedApplyExpr info typedFunction argument)
                                      typedCallee
                                      (zip resultInfos typedArguments)
                                  )
                           in (failures, if null failures then typedApplication else Nothing)
            ProvisionalVariableExpression name _ ->
              ( [failureAt statementIndex childPath TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail (identifierText name))],
                Nothing
              )
            _ ->
              ([failureAt statementIndex childPath TypedCoreCallableValueUnsupported TypedCoreUnsupportedRootDetail], Nothing)

    applicationSpine = go [] [] []
      where
        go calleePath arguments resultTypes expression =
          case expression of
            ProvisionalApplyExpression resultType function argument ->
              go
                (calleePath <> [0])
                ((calleePath <> [1], argument) : arguments)
                (resultType : resultTypes)
                function
            _ -> (expression, arguments, resultTypes)

    requireTypedStatement (_, Just typedStatement) = typedStatement
    requireTypedStatement _ = error "profile failures must be handled before typed-core validation"

    statementFailure statementIndex kind detail =
      TypedCoreProductionFailure (TypedCoreProductionStatementPath modulePath statementIndex) kind detail

    binderAt statementIndex suffix name =
      TypedBinderId (modulePath, statementIndex : suffix, name)

    resolvedValueName name =
      case name of
        GeneratedName (OperatorBinding storageName) -> TypedGeneratedName (TypedOperatorBinding storageName)
        _ -> TypedResolvedName TypedCurrentModule TypedValueNamespace (identifierText name)

    scheme owner info =
      TypedScheme owner [] [] [] (nodeType info) (nodeRecipe info)

    callableInfo statementIndex childPath expressionType =
      case typeAndRecipe statementIndex childPath expressionType of
        Right (typeValue@TypedFunctionType {}, recipe@TypedClosureRecipe {}) ->
          Right (TypedNodeInfo typeValue recipe [] [])
        Right _ -> Left (failureAt statementIndex childPath TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail)
        Left failure -> Left failure

    scalarOrCallableInfo statementIndex childPath expressionType =
      case typeAndRecipe statementIndex childPath expressionType of
        Right (typeValue, recipe) -> Right (TypedNodeInfo typeValue recipe [] [])
        Left failure -> Left failure

    typeAndRecipe statementIndex childPath expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        TFunctionType argument result -> do
          (argumentType, argumentRecipe) <- typeAndRecipe statementIndex childPath argument
          (resultType, resultRecipe) <- typeAndRecipe statementIndex childPath result
          Right (TypedFunctionType argumentType resultType, prependClosureRecipe argumentRecipe resultRecipe)
        other ->
          case scalarInfo statementIndex childPath other of
            Right (TypedNodeInfo typeValue recipe _ _) -> Right (typeValue, recipe)
            Left failure -> Left failure

    prependClosureRecipe argumentRecipe resultRecipe =
      case resultRecipe of
        TypedClosureRecipe arguments finalResult -> TypedClosureRecipe (argumentRecipe : arguments) finalResult
        _ -> TypedClosureRecipe [argumentRecipe] resultRecipe

    functionTable statements =
      foldl'
        collect
        Map.empty
        statements
      where
        collect functions statement =
          case statement of
            ProvisionalFunctionBinding statementIndex name _ expressionType _ _ expression
              | lambdaCount expression > 0 ->
                  Map.insertWith
                    (\_ firstFunction -> firstFunction)
                    name
                    (FunctionProfile statementIndex expressionType (lambdaCount expression) expression)
                    functions
            _ -> functions

    reboundFunctionStatements statements =
      snd (foldl' collect (Set.empty, Map.empty) statements)
      where
        collect (seenNames, reboundStatements) statement =
          case statement of
            ProvisionalFunctionBinding statementIndex name _ _ _ _ _
              | Set.member name seenNames ->
                  (seenNames, Map.insert statementIndex name reboundStatements)
              | otherwise ->
                  (Set.insert name seenNames, reboundStatements)
            _ -> (seenNames, reboundStatements)

    recursiveFunctionNames functions =
      Set.fromList
        [ name
        | component <-
            stronglyConnComp
              [ (name, name, Set.toList (localCalls functions (functionExpression function)))
              | (name, function) <- Map.toList functions
              ],
          name <- case component of
            AcyclicSCC candidate
              | Set.member candidate (localCalls functions (functionExpression (functions Map.! candidate))) -> [candidate]
            AcyclicSCC _ -> []
            CyclicSCC names -> names
        ]

    localCalls functions expression =
      case expression of
        ProvisionalApplyExpression _ function argument ->
          let (callee, _, _) = applicationSpine expression
              own =
                case callee of
                  ProvisionalVariableExpression name _
                    | Map.member name functions -> Set.singleton name
                  _ -> Set.empty
           in own <> localCalls functions function <> localCalls functions argument
        ProvisionalLambdaExpression _ _ body -> localCalls functions body
        ProvisionalBinaryExpression _ _ left right -> localCalls functions left <> localCalls functions right
        _ -> Set.empty

    finalizeExports functions =
      foldl'
        collect
        ([], TypedModuleInterface [] [] [] [])
        (Set.toAscList (exportInventoryEntries (resolvedModuleExportInventory resolvedModule)))
      where
        collect (failures, TypedModuleInterface values datas classes impls) (ModuleExport namespace name)
          | namespace == ValueNamespace =
              case [(sourceName, function) | (sourceName, function) <- Map.toList functions, identifierText sourceName == name] of
                [(_, function)] ->
                  case callableInfo (functionStatementIndex function) [] (functionType function) of
                    Right info ->
                      let typedName = TypedResolvedName TypedCurrentModule TypedValueNamespace name
                          owner = binderAt (functionStatementIndex function) [] typedName
                       in (failures, TypedModuleInterface (values <> [TypedValueInterface typedName (scheme owner info)]) datas classes impls)
                    Left failure -> (failures <> [failure], TypedModuleInterface values datas classes impls)
                _ -> (failures <> [TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name)], TypedModuleInterface values datas classes impls)
          | otherwise =
              (failures <> [TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name)], TypedModuleInterface values datas classes impls)

    scalarInfo statementIndex childPath expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        TIntType -> Right (TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] [])
        TIntegerLiteralType {} -> Left (failureAt statementIndex childPath TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail)
        TFloatType -> Right (TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] [])
        TNumericType numericType ->
          let (numericTypeValue, recipe) = numericInfo numericType
           in Right (TypedNodeInfo (TypedNumericType numericTypeValue) recipe [] [])
        TBoolType -> Right (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [])
        TCharType -> Right (TypedNodeInfo TypedCharType TypedCharRecipe [] [])
        TTextType -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreTextValueDetail)
        TListType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreListValueDetail)
        TTupleType [] -> Right unitInfo
        TTupleType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail)
        TDataType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail)
        TFunctionType {} -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail)
        TVarType {} -> Left (failureAt statementIndex childPath TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail)

    typedLiteral statementIndex childPath literal info =
      case (literal, nodeType info) of
        (LInt value, TypedIntType) -> Right (TypedIntegerLiteral (Text.pack (show value)))
        (LInt value, TypedNumericType _) -> Right (TypedIntegerLiteral (Text.pack (show value)))
        (LFloat _ source _, TypedFloatType) -> Right (fractionalLiteral source Nothing)
        (LFloat _ source (Just numericType), TypedNumericType _) -> Right (fractionalLiteral source (Just (typedNumericType numericType)))
        (LFloat _ source Nothing, TypedNumericType numericType) -> Right (fractionalLiteral source (Just numericType))
        (LBool value, TypedBoolType) -> Right (TypedBooleanLiteral value)
        (LChar value, TypedCharType) -> Right (TypedCharacterLiteral value)
        (LText _, _) -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreTextValueDetail)
        _ -> Left (failureAt statementIndex childPath TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail)

    fractionalLiteral source maybeNumericType =
      let (whole, fractional, scale) = fractionalLiteralSourceParts source
          digitCount = max 0 (length (show scale) - 1)
          fractionalDigits =
            Text.justifyRight
              digitCount
              '0'
              (Text.pack (show (abs fractional)))
       in TypedFractionalLiteral (Text.pack (show whole)) fractionalDigits maybeNumericType

    unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

    lambdaCount :: ProvisionalTypedExpr -> Int
    lambdaCount expression =
      case expression of
        ProvisionalLambdaExpression _ _ body -> 1 + lambdaCount body
        _ -> 0

admittedOperators :: [Text]
admittedOperators = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]

typedSpan :: SourceSpan -> TypedSpan
typedSpan spanValue = TypedSpan (spanLine spanValue) (spanColumn spanValue)

numericInfo :: NumericType -> (TypedNumericType, TypedRepresentationRecipe)
numericInfo numericType =
  case numericType of
    NumericInt8 -> (TypedInt8Type, TypedSignedIntegerRecipe 8)
    NumericInt16 -> (TypedInt16Type, TypedSignedIntegerRecipe 16)
    NumericInt32 -> (TypedInt32Type, TypedSignedIntegerRecipe 32)
    NumericInt64 -> (TypedInt64Type, TypedSignedIntegerRecipe 64)
    NumericUInt8 -> (TypedUInt8Type, TypedUnsignedIntegerRecipe 8)
    NumericUInt16 -> (TypedUInt16Type, TypedUnsignedIntegerRecipe 16)
    NumericUInt32 -> (TypedUInt32Type, TypedUnsignedIntegerRecipe 32)
    NumericUInt64 -> (TypedUInt64Type, TypedUnsignedIntegerRecipe 64)
    NumericFloat16 -> (TypedFloat16Type, TypedFloatRecipe 16)
    NumericFloat32 -> (TypedFloat32Type, TypedFloatRecipe 32)
    NumericFloat64 -> (TypedFloat64Type, TypedFloatRecipe 64)

typedNumericType :: NumericType -> TypedNumericType
typedNumericType = fst . numericInfo

typedExpressionInfo :: TypedExpr -> TypedNodeInfo
typedExpressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedTupleExpr info _ -> info
    TypedApplyExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    _ -> error "direct-call typed-core elaboration produced an unsupported expression"

typedStatementInfo :: TypedStatement -> TypedNodeInfo
typedStatementInfo statement =
  case statement of
    TypedExpressionStatement _ expression -> typedExpressionInfo expression
    TypedLetStatement _ _ _ schemeValue _ ->
      case schemeValue of
        TypedScheme _ _ _ _ typeValue recipe -> TypedNodeInfo typeValue recipe [] []
    TypedSignatureStatement _ _ _ schemeValue ->
      case schemeValue of
        TypedScheme _ _ _ _ typeValue recipe -> TypedNodeInfo typeValue recipe [] []
    _ -> error "direct-call typed-core elaboration produced an unsupported statement"

nodeType :: TypedNodeInfo -> TypedType
nodeType (TypedNodeInfo typeValue _ _ _) = typeValue

nodeRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
nodeRecipe (TypedNodeInfo _ recipe _ _) = recipe

defaultScalarLiterals :: ExpressionType -> ExpressionType
defaultScalarLiterals expressionType =
  case expressionType of
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> TIntType
    _ -> expressionType
