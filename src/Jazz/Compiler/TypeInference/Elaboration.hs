{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module Jazz.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionStatus (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredProductionFailure (..),
    InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    blockProductionFailureKindAndDetail,
    specializeInferredExpression,
    finalizeTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
  )
where

import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (Literal (..), NumericType (..), Statement (..))
import Jazz.Compiler.BuiltinCatalog (numericTypeIsIntegral)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import Jazz.Compiler.ModuleExports (ModuleExport (..), exportInventoryEntries)
import Jazz.Compiler.ModuleGraph (ResolvedModule (..))
import Jazz.Compiler.Name
  ( GeneratedNameKind (OperatorBinding),
    Name (..),
    NameNamespace (..),
    identifierText,
  )
import Jazz.Compiler.TypeInference.Solver
  ( integerLiteralRangeFitsNumericType,
    resolveType,
  )
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType (..), TypeBinding (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)

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

-- | The private result threaded by the shared inference traversal. Ordinary
-- inference projects the expression type; production also consumes the
-- provisional node and ordered profile failures.
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
  | ProvisionalBinaryExpression Text ExpressionType ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalVariableExpression Name ExpressionType
  | ProvisionalLambdaExpression Name ExpressionType ProvisionalTypedExpr
  | ProvisionalApplyExpression ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalScopeStatements [ProvisionalTypedStatement]
  | ProvisionalUnsupportedExpression TypedCoreProductionFailureKind TypedCoreProductionFailureDetail
  | ProvisionalRetainedFailures [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalTypedStatement
  = ProvisionalSignature Int Name SourceSpan ExpressionType
  | ProvisionalFunctionBinding Int Name SourceSpan ExpressionType (Maybe TypeBinding) ProvisionalTypedExpr
  | ProvisionalTerminalExpression Int SourceSpan ProvisionalTypedExpr
  | ProvisionalUnsupportedStatement Int TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  deriving (Eq, Show)

data FunctionProfile = FunctionProfile
  { functionStatementIndex :: Int,
    functionType :: ExpressionType,
    functionArity :: Int,
    functionExpression :: ProvisionalTypedExpr
  }

data ExpressionRole
  = FunctionBindingExpression
  | CalleeExpression
  | ScalarExpression

-- | Finalize the initial unit-only root against the permanent contract.
-- Future production slices extend the provisional scope rather than changing the
-- typed-core constructors themselves.
finalizeTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ResolvedModule ->
  InferState ->
  ProvisionalTypedExpr ->
  TypedCoreProductionStatus
finalizeTypedCoreExpressionDirectCall sourcePath resolvedModule state provisionalScope =
  case provisionalScope of
    ProvisionalScopeStatements provisionalStatements ->
      let functions = functionTable provisionalStatements
          reboundFunctions = reboundFunctionStatements provisionalStatements
          recursiveNames = recursiveFunctionNames functions
          finalizedStatements = map (finalizeStatement functions reboundFunctions recursiveNames) provisionalStatements
          exportResult = finalizeExports functions
          missingResultFailures =
            [ missingModuleResultFailure
            | not (hasTerminalResult provisionalStatements)
            ]
          productionFailures = concatMap fst finalizedStatements <> fst exportResult <> missingResultFailures
       in case productionFailures of
            _ : _ -> TypedCoreProductionUnsupported productionFailures
            [] ->
              case traverse snd finalizedStatements of
                Just typedStatements ->
                  case reverse typedStatements of
                    TypedExpressionStatement _ terminalExpression : _ ->
                      let programValue =
                            typedProgram
                              (snd exportResult)
                              typedStatements
                              (typedExpressionInfo terminalExpression)
                       in case validateTypedProgram programValue of
                            [] -> TypedCoreProductionSucceeded programValue
                            failures -> TypedCoreProductionInvariantFailures failures
                    _ -> TypedCoreProductionUnsupported [missingModuleResultFailure]
                Nothing -> TypedCoreProductionUnsupported [missingModuleResultFailure]
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
        ProvisionalFunctionBinding statementIndex name spanValue expressionType maybeBinding expression ->
          let typedName = resolvedValueName name
              owner = binderAt statementIndex [] typedName
              generatedOperatorFailures =
                case name of
                  GeneratedName (OperatorBinding _) ->
                    [statementFailure statementIndex TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
                  _ -> []
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
                finalizeExpression functions statementIndex [0] Set.empty FunctionBindingExpression expression
              infoResult = callableInfo statementIndex [] expressionType
              infoFailures = either (: []) (const []) infoResult
              failures = generatedOperatorFailures <> rebindingFailures <> recursiveFailures <> schemeFailures <> shapeFailures <> infoFailures <> expressionFailures
              typedStatement = do
                info <- either (const Nothing) Just infoResult
                typedExpression <- maybeExpression
                pure (TypedLetStatement owner typedName (typedSpan spanValue) (scheme owner info) typedExpression)
           in (failures, if null failures then typedStatement else Nothing)
        ProvisionalTerminalExpression statementIndex spanValue expression ->
          let (failures, maybeTypedExpression) =
                finalizeExpression functions statementIndex [] Set.empty ScalarExpression expression
           in (failures, TypedExpressionStatement (typedSpan spanValue) <$> maybeTypedExpression)
        ProvisionalUnsupportedStatement statementIndex kind detail childFailures ->
          ( statementFailure statementIndex kind detail
              : map (qualifyInferredFailure statementIndex []) childFailures,
            Nothing
          )

    finalizeExpression functions statementIndex childPath parameters expressionRole expression =
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
        ProvisionalBinaryExpression operatorSymbol expressionType _ left right
          | isTypedCoreDirectCallOperator operatorSymbol ->
              let (operatorFailures, maybeInfo) =
                    case scalarInfo statementIndex childPath expressionType of
                      Left failure -> ([failure], Nothing)
                      Right info -> ([], Just info)
                  (leftFailures, maybeLeft) = finalizeExpression functions statementIndex (childPath <> [0]) parameters ScalarExpression left
                  (rightFailures, maybeRight) = finalizeExpression functions statementIndex (childPath <> [1]) parameters ScalarExpression right
                  failures = operatorFailures <> leftFailures <> rightFailures
                  typedExpression =
                    TypedBinaryExpr <$> maybeInfo <*> pure (TypedBuiltinOperator operatorSymbol) <*> maybeLeft <*> maybeRight
               in (failures, if null failures then typedExpression else Nothing)
          | otherwise ->
              let (leftFailures, _) = finalizeExpression functions statementIndex (childPath <> [0]) parameters ScalarExpression left
                  (rightFailures, _) = finalizeExpression functions statementIndex (childPath <> [1]) parameters ScalarExpression right
               in (failureAt statementIndex childPath TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail : leftFailures <> rightFailures, Nothing)
        ProvisionalVariableExpression name expressionType
          | TDataType {} <- resolveType state expressionType ->
              ( [failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail],
                Nothing
              )
          | Set.member name parameters ->
              case scalarInfo statementIndex childPath expressionType of
                Left failure -> ([failure], Nothing)
                Right info -> ([], Just (TypedVariableExpr info (resolvedValueName name) Nothing))
          | Just function <- Map.lookup name functions ->
              case expressionRole of
                CalleeExpression ->
                  case callableInfo statementIndex childPath (functionType function) of
                    Left failure -> ([failure], Nothing)
                    Right info -> ([], Just (TypedVariableExpr info (resolvedValueName name) Nothing))
                _ ->
                  ( [failureAt statementIndex childPath TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))],
                    Nothing
                  )
          | otherwise ->
              ( [failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))],
                Nothing
              )
        ProvisionalLambdaExpression parameterName expressionType body ->
          case expressionRole of
            FunctionBindingExpression ->
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
                        finalizeExpression
                          functions
                          statementIndex
                          (childPath <> [0])
                          (Set.insert parameterName parameters)
                          FunctionBindingExpression
                          body
                      parameterBinder = TypedBinderId (modulePath, statementIndex : parameterPath, resolvedValueName parameterName)
                      failures = duplicateParameterFailures <> bodyFailures
                   in (failures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
            _ ->
              ( [failureAt statementIndex childPath TypedCoreCallableValueUnsupported TypedCoreUnsupportedRootDetail],
                Nothing
              )
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
          finalizedArguments =
            map
              ( \(argumentPath, argument) ->
                  finalizeExpression
                    functions
                    statementIndex
                    (childPath <> argumentPath)
                    parameters
                    ScalarExpression
                    argument
              )
              arguments
          argumentFailures = concatMap fst finalizedArguments
       in case callee of
            ProvisionalVariableExpression name _
              | Just function <- Map.lookup name functions ->
                  let expectedArity = functionArity function
                      actualArity = length arguments
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity /= expectedArity
                        ]
                      (calleeFailures, maybeCallee) =
                        finalizeExpression functions statementIndex childPath parameters CalleeExpression callee
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          let (resultInfoFailures, resultInfos) =
                                partitionEithers
                                  ( zipWith
                                      (scalarOrCallableInfo statementIndex)
                                      [childPath <> replicate remainingApplications 0 | remainingApplications <- reverse [0 .. actualArity - 1]]
                                      resultTypes
                                  )
                              failures = childFailures <> resultInfoFailures
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
              ( failureAt statementIndex childPath TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail (identifierText name))
                  : argumentFailures,
                Nothing
              )
            _ ->
              ( failureAt statementIndex childPath TypedCoreCallableValueUnsupported TypedCoreUnsupportedRootDetail
                  : argumentFailures,
                Nothing
              )

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

    statementFailure statementIndex kind detail =
      TypedCoreProductionFailure (TypedCoreProductionStatementPath modulePath statementIndex) kind detail

    binderAt statementIndex suffix name =
      TypedBinderId (modulePath, statementIndex : suffix, name)

    resolvedValueName name =
      case name of
        GeneratedName (OperatorBinding storageName) -> TypedGeneratedName (TypedOperatorBinding storageName)
        _ -> TypedResolvedName TypedCurrentModule TypedValueNamespace (identifierText name)

    scheme owner info =
      TypedScheme owner [] [] [] (typedNodeType info) (typedNodeRecipe info) callableShape
      where
        callableShape =
          case typedNodeType info of
            TypedFunctionType {} -> Just TypedDirectCallableShape
            _ -> Nothing

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
          TypedNodeInfo argumentType argumentRecipe _ _ <- scalarInfo statementIndex childPath argument
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
            ProvisionalFunctionBinding statementIndex name _ expressionType _ expression
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
            ProvisionalFunctionBinding statementIndex name _ _ _ _
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
        ProvisionalBinaryExpression _ _ _ left right -> localCalls functions left <> localCalls functions right
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
      case (literal, typedNodeType info) of
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

-- | Commit a successfully selected integral context into the provisional tree.
-- Integer-range unification validates compatibility but deliberately creates no
-- solver substitution, so production must retain the concrete representation
-- selected by a signature, operator operand, or function parameter.
specializeInferredExpression :: InferState -> ExpressionType -> InferredExpr -> InferredExpr
specializeInferredExpression state expectedType inferred =
  inferred
    { inferredExpressionType =
        specializeExpressionType state expectedType
          <$> inferredExpressionType inferred,
      inferredProvisionalExpr =
        specializeProvisionalExpression state (Just expectedType)
          <$> inferredProvisionalExpr inferred
    }

specializeProvisionalExpression :: InferState -> Maybe ExpressionType -> ProvisionalTypedExpr -> ProvisionalTypedExpr
specializeProvisionalExpression state maybeExpected expression =
  case expression of
    ProvisionalUnitExpression -> ProvisionalUnitExpression
    ProvisionalLiteralExpression literal expressionType ->
      ProvisionalLiteralExpression literal (specializedType expressionType)
    ProvisionalBinaryExpression operatorSymbol expressionType operandType left right ->
      let resultType = specializedType expressionType
          resolvedOperandType = resolveType state operandType
          operandExpected = concreteIntegralType resultType <|> concreteIntegralType resolvedOperandType
          specializedOperandType = maybe resolvedOperandType id operandExpected
       in ProvisionalBinaryExpression
            operatorSymbol
            resultType
            specializedOperandType
            (specializeProvisionalExpression state operandExpected left)
            (specializeProvisionalExpression state operandExpected right)
    ProvisionalVariableExpression name expressionType ->
      ProvisionalVariableExpression name (specializedType expressionType)
    ProvisionalLambdaExpression parameterName expressionType body ->
      let specializedFunctionType = specializedType expressionType
          bodyExpected =
            case specializedFunctionType of
              TFunctionType _ resultType -> Just resultType
              _ -> Nothing
       in ProvisionalLambdaExpression
            parameterName
            specializedFunctionType
            (specializeProvisionalExpression state bodyExpected body)
    ProvisionalApplyExpression expressionType function argument ->
      let resultType = specializedType expressionType
          argumentExpected =
            case provisionalExpressionType state function of
              Just (TFunctionType parameterType _) -> Just parameterType
              _ -> Nothing
       in ProvisionalApplyExpression
            resultType
            (specializeProvisionalExpression state Nothing function)
            (specializeProvisionalExpression state argumentExpected argument)
    ProvisionalScopeStatements statements -> ProvisionalScopeStatements statements
    ProvisionalUnsupportedExpression kind detail -> ProvisionalUnsupportedExpression kind detail
    ProvisionalRetainedFailures failures -> ProvisionalRetainedFailures failures
  where
    specializedType expressionType =
      case maybeExpected of
        Just expectedType -> specializeExpressionType state expectedType expressionType
        Nothing -> resolveType state expressionType

provisionalExpressionType :: InferState -> ProvisionalTypedExpr -> Maybe ExpressionType
provisionalExpressionType state expression =
  resolveType state <$> case expression of
    ProvisionalUnitExpression -> Just (TTupleType [])
    ProvisionalLiteralExpression _ expressionType -> Just expressionType
    ProvisionalBinaryExpression _ expressionType _ _ _ -> Just expressionType
    ProvisionalVariableExpression _ expressionType -> Just expressionType
    ProvisionalLambdaExpression _ expressionType _ -> Just expressionType
    ProvisionalApplyExpression expressionType _ _ -> Just expressionType
    ProvisionalScopeStatements {} -> Nothing
    ProvisionalUnsupportedExpression {} -> Nothing
    ProvisionalRetainedFailures {} -> Nothing

specializeExpressionType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
specializeExpressionType state expectedType expressionType =
  let resolvedExpected = resolveType state expectedType
      resolvedExpression = resolveType state expressionType
   in case (resolvedExpression, resolvedExpected) of
        (TIntegerLiteralType literalRange, TIntType)
          | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> TIntType
        (TIntegerLiteralType literalRange, numericType@(TNumericType concreteType))
          | integerLiteralRangeFitsNumericType literalRange concreteType -> numericType
        (TIntType, TNumericType NumericInt64) -> resolvedExpected
        (TNumericType NumericInt64, TIntType) -> resolvedExpected
        (TFloatType, TNumericType NumericFloat64) -> resolvedExpected
        (TNumericType NumericFloat64, TFloatType) -> resolvedExpected
        _ -> resolvedExpression

concreteIntegralType :: ExpressionType -> Maybe ExpressionType
concreteIntegralType expressionType =
  case expressionType of
    TIntType -> Just TIntType
    numericType@(TNumericType concreteType)
      | numericTypeIsIntegral concreteType -> Just numericType
    _ -> Nothing

isTypedCoreDirectCallOperator :: Text -> Bool
isTypedCoreDirectCallOperator operatorSymbol =
  operatorSymbol `elem` ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]

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

defaultScalarLiterals :: ExpressionType -> ExpressionType
defaultScalarLiterals expressionType =
  case expressionType of
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> TIntType
    _ -> expressionType
