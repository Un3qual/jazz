{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module Jazz.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionStatus (..),
    TypedCoreProductionOutcome,
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredProductionFailure (..),
    InferredExpr (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    blockProductionFailureKindAndDetail,
    blockedTypedCoreProductionOutcome,
    expressionDependencyNames,
    specializeInferredExpression,
    finalizeValidatedTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
    typedCoreProductionOutcomeStatus,
    typedCoreProductionOutcomeValidatedProgram,
    unsupportedTypedCoreProductionOutcome,
  )
where

import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CaseArm (..), DataConstructor (..), Expr (..), ImplMethod (..), Literal (..), NumericType (..), Pattern (..), Statement (..))
import Jazz.Compiler.BuiltinCatalog (numericTypeIsIntegral)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExport (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
    inventoryHasExport,
  )
import Jazz.Compiler.ModuleGraph (CoreModule (..), DeclaredModuleExports (..), ResolvedModule (..))
import Jazz.Compiler.Name
  ( GeneratedNameKind (OperatorBinding),
    Name (..),
    NameNamespace (..),
    identifierText,
    operatorBindingName,
  )
import Jazz.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import Jazz.Compiler.TypeInference.Solver
  ( integerLiteralRangeFitsNumericType,
    resolveType,
  )
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType (..), TypeBinding (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( ValidatedTypedProgram,
    validateTypedProgramOnce,
    validatedTypedProgram,
  )

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

-- | The private production outcome keeps a successful raw program and its
-- validation proof in one constructor. Public callers can observe the legacy
-- status and trusted proof without being able to forge an inconsistent pair.
data TypedCoreProductionOutcome
  = ProductionBlockedByDiagnostics
  | ProductionUnsupported [TypedCoreProductionFailure]
  | ProductionInvariantFailures [TypedCoreValidationFailure]
  | ProductionSucceeded ValidatedTypedProgram
  deriving (Eq, Show)

blockedTypedCoreProductionOutcome :: TypedCoreProductionOutcome
blockedTypedCoreProductionOutcome = ProductionBlockedByDiagnostics

unsupportedTypedCoreProductionOutcome :: [TypedCoreProductionFailure] -> TypedCoreProductionOutcome
unsupportedTypedCoreProductionOutcome = ProductionUnsupported

typedCoreProductionOutcomeStatus :: TypedCoreProductionOutcome -> TypedCoreProductionStatus
typedCoreProductionOutcomeStatus outcome =
  case outcome of
    ProductionBlockedByDiagnostics -> TypedCoreProductionBlockedByDiagnostics
    ProductionUnsupported failures -> TypedCoreProductionUnsupported failures
    ProductionInvariantFailures failures -> TypedCoreProductionInvariantFailures failures
    ProductionSucceeded validatedProgram ->
      TypedCoreProductionSucceeded (validatedTypedProgram validatedProgram)

typedCoreProductionOutcomeValidatedProgram :: TypedCoreProductionOutcome -> Maybe ValidatedTypedProgram
typedCoreProductionOutcomeValidatedProgram outcome =
  case outcome of
    ProductionSucceeded validatedProgram -> Just validatedProgram
    _ -> Nothing

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
  | ProvisionalFunctionBinding ProvisionalCallableDeclaration ProvisionalTypedExpr
  | ProvisionalScalarBinding Int Name SourceSpan ExpressionType ProvisionalTypedExpr
  | ProvisionalTerminalExpression Int SourceSpan ProvisionalTypedExpr
  | ProvisionalUnsupportedCallableBinding ProvisionalCallableDeclaration TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  | ProvisionalUnsupportedStatement Int TypedCoreProductionFailureKind TypedCoreProductionFailureDetail [InferredProductionFailure]
  deriving (Eq, Show)

data ProvisionalCallableDeclaration = ProvisionalCallableDeclaration
  { provisionalCallableStatementIndex :: Int,
    provisionalCallableName :: Name,
    provisionalCallableSpan :: SourceSpan,
    provisionalCallableType :: ExpressionType,
    provisionalCallableBinding :: Maybe TypeBinding,
    provisionalCallableRecursiveGroupMembers :: Maybe [Int]
  }
  deriving (Eq, Show)

data FunctionProfile = FunctionProfile
  { functionStatementIndex :: Int,
    functionType :: ExpressionType,
    functionArity :: Int
  }

-- | Canonical free value references for dependency analysis. This walks the
-- resolved core expression rather than the provisional production tree, so a
-- rejected expression cannot erase dependency evidence. Scope separately
-- transports canonical recursive-group membership after applying declaration
-- position, rebinding, outer-binding, and lexical-shadow semantics.
expressionDependencyNames :: Expr -> Set.Set Name
expressionDependencyNames = go
  where
    go expression =
      case expression of
        ELit {} -> Set.empty
        EVar name -> Set.singleton name
        ELambda parameterName body -> Set.delete parameterName (go body)
        EOperatorValue operatorSymbol -> operatorDependencies operatorSymbol
        EList elements -> foldMap go elements
        ETuple elements -> foldMap go elements
        EApply function argument -> go function <> go argument
        ETypeApplication function _ _ -> go function
        EIf condition thenExpression elseExpression ->
          go condition <> go thenExpression <> go elseExpression
        EPatternCase scrutinee arms -> go scrutinee <> foldMap armDependencies arms
        EBinary operatorSymbol left right ->
          operatorDependencies operatorSymbol <> go left <> go right
        ESectionLeft left operatorSymbol -> operatorDependencies operatorSymbol <> go left
        ESectionRight operatorSymbol right -> operatorDependencies operatorSymbol <> go right
        EBlock statements -> blockDependencies Set.empty statements
    armDependencies (CaseArm patternValue maybeGuard result) =
      let boundNames = patternBindingNames patternValue
       in (maybe Set.empty go maybeGuard <> go result) Set.\\ boundNames
    methodDependencies (ImplMethod _ _ body) = go body
    blockDependencies _ [] = Set.empty
    blockDependencies lexicalNames (statement : rest) =
      case statement of
        SLet name _ initializer ->
          (go initializer Set.\\ lexicalNames)
            <> blockDependencies (Set.insert name lexicalNames) rest
        SExpr _ result ->
          (go result Set.\\ lexicalNames) <> blockDependencies lexicalNames rest
        SImpl _ _ _ methods ->
          (foldMap methodDependencies methods Set.\\ lexicalNames)
            <> blockDependencies lexicalNames rest
        _ -> blockDependencies lexicalNames rest
    patternBindingNames patternValue =
      case patternValue of
        PWildcard -> Set.empty
        PVariable name -> Set.singleton name
        PLiteral {} -> Set.empty
        PConstructor _ fields -> foldMap patternBindingNames fields
        PList elements -> foldMap patternBindingNames elements
        PConsList headPattern tailPattern -> patternBindingNames headPattern <> patternBindingNames tailPattern
        PTuple elements -> foldMap patternBindingNames elements
        PAs name nested -> Set.insert name (patternBindingNames nested)
        POr alternatives -> foldMap patternBindingNames alternatives
    operatorDependencies operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = Set.empty
      | otherwise = Set.singleton (operatorBindingName operatorSymbol)

data ExpressionRole
  = FunctionBindingExpression TypedCallableShape Int
  | CalleeExpression
  | ScalarExpression

-- | Finalize once while retaining the opaque validation proof for a trusted
-- downstream lowering handoff. The public status keeps exposing the exact raw
-- Typed Program artifact for compatibility.
finalizeValidatedTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ResolvedModule ->
  InferState ->
  ProvisionalTypedExpr ->
  TypedCoreProductionOutcome
finalizeValidatedTypedCoreExpressionDirectCall sourcePath resolvedModule state provisionalScope =
  case provisionalScope of
    ProvisionalScopeStatements provisionalStatements ->
      let functions = functionTable provisionalStatements
          declarations = callableDeclarations provisionalStatements
          callableShapes = callableShapeTable functions provisionalStatements
          reboundFunctions = reboundFunctionStatements provisionalStatements
          typedRecursiveGroups = orderedTypedRecursiveGroups declarations
          recursiveBinders = recursiveDeclarationBinders declarations
          (acceptedRecursiveBinders, recursiveScalarCaptureTypes, unavailableClosureCaptureBinders) =
            supportedRecursiveProfile
              functions
              callableShapes
              reboundFunctions
              provisionalStatements
              typedRecursiveGroups
          unsupportedRecursiveBinders = recursiveBinders Set.\\ acceptedRecursiveBinders
          (statementFailures, typedStatements) =
            finalizeStatements
              functions
              callableShapes
              reboundFunctions
              unsupportedRecursiveBinders
              unavailableClosureCaptureBinders
              recursiveScalarCaptureTypes
              provisionalStatements
          exportResult = finalizeExports functions callableShapes
          missingResultFailures =
            [ missingModuleResultFailure
            | not (hasTerminalResult provisionalStatements)
            ]
          moduleFailures = missingResultFailures <> fst exportResult
          productionFailures = moduleFailures <> statementFailures
       in case productionFailures of
            _ : _ -> ProductionUnsupported productionFailures
            [] ->
              case reverse typedStatements of
                TypedExpressionStatement _ terminalExpression : _ ->
                  let programValue =
                        typedProgram
                          (snd exportResult)
                          typedRecursiveGroups
                          typedStatements
                          (typedExpressionInfo terminalExpression)
                   in case validateTypedProgramOnce programValue of
                        Right validatedProgram -> ProductionSucceeded validatedProgram
                        Left failures -> ProductionInvariantFailures failures
                _ -> ProductionUnsupported [missingModuleResultFailure]
    ProvisionalUnsupportedExpression kind detail ->
      ProductionUnsupported [failureAt 0 [] kind detail]
    _ ->
      ProductionUnsupported [failureAt 0 [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
  where
    modulePath = resolvedModulePath resolvedModule

    typedProgram typedInterface typedRecursiveGroups typedStatements moduleInfo =
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            sourcePath
            []
            (typedExports typedInterface)
            typedInterface
            typedRecursiveGroups
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

    finalizeStatements functions callableShapes reboundFunctions recursiveBinders unavailableClosureCaptureBinders recursiveScalarCaptureTypes =
      go Map.empty
      where
        go _ [] = ([], [])
        go scalarBindings (statement : rest) =
          let (failures, maybeStatement, nextScalarBindings) =
                finalizeStatement
                  functions
                  callableShapes
                  reboundFunctions
                  recursiveBinders
                  unavailableClosureCaptureBinders
                  recursiveScalarCaptureTypes
                  scalarBindings
                  statement
              (restFailures, restStatements) = go nextScalarBindings rest
           in (failures <> restFailures, maybe restStatements (: restStatements) maybeStatement)

    finalizeStatement functions callableShapes reboundFunctions recursiveBinders unavailableClosureCaptureBinders recursiveScalarCaptureTypes scalarBindings statement =
      case statement of
        ProvisionalSignature statementIndex name spanValue expressionType ->
          let callableShape = shapeFor callableShapes name
              directArity = maybe (resolvedFunctionArity expressionType) functionArity (Map.lookup name functions)
              infoResult =
                case defaultScalarLiterals (resolveType state expressionType) of
                  TFunctionType {} -> callableInfo callableShape directArity statementIndex [] expressionType
                  _ -> valueInfo statementIndex [] expressionType
           in case infoResult of
                Left failure -> ([failure], Nothing, scalarBindings)
                Right info ->
                  let typedName = resolvedValueName name
                      owner = binderAt statementIndex [] typedName
                   in ([], Just (TypedSignatureStatement owner typedName (typedSpan spanValue) (scheme owner callableShape info)), scalarBindings)
        ProvisionalFunctionBinding declaration expression ->
          let typedName = resolvedValueName name
              owner = binderAt statementIndex [] typedName
              callableShape = shapeFor callableShapes name
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
                | Set.member owner recursiveBinders
                ]
              captureAvailabilityFailures =
                [ statementFailure statementIndex TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))
                | Set.member owner unavailableClosureCaptureBinders
                ]
              schemeFailures =
                case maybeBinding of
                  Just PlainTypeBinding {} -> []
                  _ -> [statementFailure statementIndex TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail (identifierText name))]
              shapeFailures =
                case expression of
                  ProvisionalLambdaExpression {} -> []
                  _ -> [statementFailure statementIndex TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
              directArity = maybe 0 functionArity (Map.lookup name functions)
              (expressionFailures, maybeExpression) =
                finalizeExpression recursiveScalarCaptureTypes functions callableShapes statementIndex [0] scalarBindings (FunctionBindingExpression callableShape directArity) expression
              infoResult = callableInfo callableShape directArity statementIndex [] expressionType
              infoFailures = either (: []) (const []) infoResult
              owningStatementFailures =
                shapeFailures
                  <> generatedOperatorFailures
                  <> recursiveFailures
                  <> captureAvailabilityFailures
                  <> rebindingFailures
                  <> schemeFailures
              failures = owningStatementFailures <> infoFailures <> expressionFailures
              typedStatement = do
                info <- either (const Nothing) Just infoResult
                typedExpression <- maybeExpression
                pure (TypedLetStatement owner typedName (typedSpan spanValue) (scheme owner callableShape info) typedExpression)
           in (failures, if null failures then typedStatement else Nothing, scalarBindings)
          where
            statementIndex = provisionalCallableStatementIndex declaration
            name = provisionalCallableName declaration
            spanValue = provisionalCallableSpan declaration
            expressionType = provisionalCallableType declaration
            maybeBinding = provisionalCallableBinding declaration
        ProvisionalScalarBinding statementIndex name spanValue expressionType expression ->
          let typedName = resolvedValueName name
              owner = binderAt statementIndex [] typedName
              selectedExpressionType =
                case expression of
                  ProvisionalVariableExpression referencedName _
                    | Just referencedBinder <- Map.lookup referencedName scalarBindings,
                      Just captureType <- Map.lookup referencedBinder recursiveScalarCaptureTypes ->
                        specializeExpressionType state captureType expressionType
                  _ -> Map.findWithDefault expressionType owner recursiveScalarCaptureTypes
              selectedExpression =
                specializeProvisionalExpression
                  state
                  (Just selectedExpressionType)
                  expression
              callableNameCollisionFailures =
                [statementFailure statementIndex TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail | Map.member name functions]
              infoResult = valueInfo statementIndex [] selectedExpressionType
              infoFailures = either (: []) (const []) infoResult
              (expressionFailures, maybeExpression) =
                finalizeExpression
                  recursiveScalarCaptureTypes
                  functions
                  callableShapes
                  statementIndex
                  [0]
                  scalarBindings
                  ScalarExpression
                  selectedExpression
              failures = callableNameCollisionFailures <> infoFailures <> expressionFailures
              typedStatement = do
                info <- either (const Nothing) Just infoResult
                typedExpression <- maybeExpression
                pure (TypedLetStatement owner typedName (typedSpan spanValue) (scheme owner TypedDirectCallableShape info) typedExpression)
              acceptedStatement = if null failures then typedStatement else Nothing
              nextScalarBindings =
                case acceptedStatement of
                  Just _ -> Map.insert name owner scalarBindings
                  Nothing -> scalarBindings
           in (failures, acceptedStatement, nextScalarBindings)
        ProvisionalTerminalExpression statementIndex spanValue expression ->
          let (failures, maybeTypedExpression) =
                finalizeExpression recursiveScalarCaptureTypes functions callableShapes statementIndex [] scalarBindings ScalarExpression expression
           in (failures, TypedExpressionStatement (typedSpan spanValue) <$> maybeTypedExpression, scalarBindings)
        ProvisionalUnsupportedCallableBinding declaration kind detail childFailures ->
          ( recursiveFailures
              <> rebindingFailures
              <> ( statementFailure statementIndex kind detail
                     : map (qualifyInferredFailure statementIndex []) childFailures
                 ),
            Nothing,
            scalarBindings
          )
          where
            statementIndex = provisionalCallableStatementIndex declaration
            name = provisionalCallableName declaration
            owner = binderAt statementIndex [] (resolvedValueName name)
            recursiveFailures =
              [ statementFailure statementIndex TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail (identifierText name))
              | Set.member owner recursiveBinders
              ]
            rebindingFailures =
              [ statementFailure statementIndex TypedCoreFunctionRebindingUnsupported (TypedCoreNameDetail (identifierText name))
              | Map.member statementIndex reboundFunctions
              ]
        ProvisionalUnsupportedStatement statementIndex kind detail childFailures ->
          ( statementFailure statementIndex kind detail
              : map (qualifyInferredFailure statementIndex []) childFailures,
            Nothing,
            scalarBindings
          )
    finalizeExpression scalarCaptureTypes functions callableShapes statementIndex childPath parameters expressionRole expression =
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
                  (leftFailures, maybeLeft) = finalizeExpression scalarCaptureTypes functions callableShapes statementIndex (childPath <> [0]) parameters ScalarExpression left
                  (rightFailures, maybeRight) = finalizeExpression scalarCaptureTypes functions callableShapes statementIndex (childPath <> [1]) parameters ScalarExpression right
                  failures = operatorFailures <> leftFailures <> rightFailures
                  typedExpression =
                    TypedBinaryExpr <$> maybeInfo <*> pure (TypedBuiltinOperator operatorSymbol) <*> maybeLeft <*> maybeRight
               in (failures, if null failures then typedExpression else Nothing)
          | otherwise ->
              let (leftFailures, _) = finalizeExpression scalarCaptureTypes functions callableShapes statementIndex (childPath <> [0]) parameters ScalarExpression left
                  (rightFailures, _) = finalizeExpression scalarCaptureTypes functions callableShapes statementIndex (childPath <> [1]) parameters ScalarExpression right
               in (failureAt statementIndex childPath TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail : leftFailures <> rightFailures, Nothing)
        ProvisionalVariableExpression name expressionType
          | TDataType {} <- resolveType state expressionType ->
              ( [failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail],
                Nothing
              )
          | Just parameterBinder <- Map.lookup name parameters ->
              let selectedExpressionType =
                    case Map.lookup parameterBinder scalarCaptureTypes of
                      Just captureType -> specializeExpressionType state captureType expressionType
                      Nothing -> expressionType
                  selectedType = defaultScalarLiterals (resolveType state selectedExpressionType)
                  captureTypeMismatch =
                    case Map.lookup parameterBinder scalarCaptureTypes of
                      Just captureType ->
                        selectedType /= defaultScalarLiterals (resolveType state captureType)
                      Nothing -> False
               in if captureTypeMismatch
                    then
                      ( [failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))],
                        Nothing
                      )
                    else case valueInfo statementIndex childPath selectedExpressionType of
                      Left failure -> ([failure], Nothing)
                      Right info -> ([], Just (TypedVariableExpr info (resolvedValueName name) (Just parameterBinder)))
          | Just function <- Map.lookup name functions ->
              let callableShape = shapeFor callableShapes name
                  valueUseSupported = callableShape == TypedClosureCallableShape
               in case expressionRole of
                    CalleeExpression -> finalizeNamedFunctionReference name callableShape function
                    _
                      | valueUseSupported -> finalizeNamedFunctionReference name callableShape function
                      | otherwise ->
                          ( [failureAt statementIndex childPath TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))],
                            Nothing
                          )
          | otherwise ->
              ( [failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))],
                Nothing
              )
        ProvisionalLambdaExpression parameterName expressionType body ->
          case expressionRole of
            FunctionBindingExpression callableShape remainingDirectArity ->
              case callableInfo callableShape remainingDirectArity statementIndex childPath expressionType of
                Left failure -> ([failure], Nothing)
                Right info ->
                  let duplicateParameterFailures =
                        [ failureAt
                            statementIndex
                            childPath
                            TypedCoreDuplicateParameterUnsupported
                            (TypedCoreNameDetail (identifierText parameterName))
                        | Just existingBinder <- [Map.lookup parameterName parameters],
                          binderBelongsToStatement statementIndex existingBinder
                        ]
                      parameterPath = childPath
                      parameterBinder = TypedBinderId (modulePath, statementIndex : parameterPath, resolvedValueName parameterName)
                      (bodyFailures, maybeBody) =
                        finalizeExpression
                          scalarCaptureTypes
                          functions
                          callableShapes
                          statementIndex
                          (childPath <> [0])
                          (Map.insert parameterName parameterBinder parameters)
                          (FunctionBindingExpression callableShape (max 0 (remainingDirectArity - 1)))
                          body
                      failures = duplicateParameterFailures <> bodyFailures
                   in (failures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
            _ ->
              case callableInfo TypedClosureCallableShape (1 :: Int) statementIndex childPath expressionType of
                Left failure -> ([failure], Nothing)
                Right info ->
                  let parameterBinder = TypedBinderId (modulePath, statementIndex : childPath, resolvedValueName parameterName)
                      (bodyFailures, maybeBody) =
                        finalizeExpression
                          scalarCaptureTypes
                          functions
                          callableShapes
                          statementIndex
                          (childPath <> [0])
                          (Map.insert parameterName parameterBinder parameters)
                          ScalarExpression
                          body
                   in (bodyFailures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
        ProvisionalApplyExpression _ _ _ ->
          finalizeApplicationSpine scalarCaptureTypes functions callableShapes statementIndex childPath parameters expression
        ProvisionalScopeStatements _ -> ([failureAt statementIndex childPath TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail], Nothing)
        ProvisionalUnsupportedExpression kind detail -> ([failureAt statementIndex childPath kind detail], Nothing)
        ProvisionalRetainedFailures failures ->
          (map (qualifyInferredFailure statementIndex childPath) failures, Nothing)
      where
        finalizeNamedFunctionReference name callableShape function =
          case callableInfo callableShape (functionArity function) statementIndex childPath (functionType function) of
            Left failure -> ([failure], Nothing)
            Right info ->
              let typedName = resolvedValueName name
                  functionBinder = binderAt (functionStatementIndex function) [] typedName
               in ([], Just (TypedVariableExpr info typedName (Just functionBinder)))

    qualifyInferredFailure statementIndex parentPath (InferredProductionFailure relativePath kind detail) =
      failureAt statementIndex (parentPath <> relativePath) kind detail

    finalizeApplicationSpine scalarCaptureTypes functions callableShapes statementIndex childPath parameters expression =
      let (callee, arguments, resultTypes) = applicationSpine expression
          finalizedArguments =
            map
              ( \(argumentPath, argument) ->
                  finalizeExpression
                    scalarCaptureTypes
                    functions
                    callableShapes
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
              | Map.member name parameters ->
                  let expectedArity = 1
                      actualArity = length arguments
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity > expectedArity,
                          not (callableOversaturationSupported expectedArity resultTypes)
                        ]
                      (calleeFailures, maybeCallee) =
                        finalizeExpression scalarCaptureTypes functions callableShapes statementIndex (childPath <> replicate actualArity 0) parameters CalleeExpression callee
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes
              | Just function <- Map.lookup name functions ->
                  let expectedArity = functionArity function
                      actualArity = length arguments
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity > expectedArity,
                          not (callableOversaturationSupported expectedArity resultTypes)
                        ]
                      (calleeFailures, maybeCallee) =
                        finalizeExpression scalarCaptureTypes functions callableShapes statementIndex childPath parameters CalleeExpression callee
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes
            ProvisionalVariableExpression name _ ->
              ( failureAt statementIndex childPath TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail (identifierText name))
                  : argumentFailures,
                Nothing
              )
            _ ->
              let actualArity = length arguments
                  arityFailures =
                    [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail 1 actualArity)
                    | actualArity > 1,
                      not (callableOversaturationSupported 1 resultTypes)
                    ]
                  (calleeFailures, maybeCallee) =
                    finalizeExpression
                      scalarCaptureTypes
                      functions
                      callableShapes
                      statementIndex
                      (childPath <> replicate actualArity 0)
                      parameters
                      CalleeExpression
                      callee
                  childFailures = calleeFailures <> argumentFailures
               in case (arityFailures, resultTypes, finalizedArguments) of
                    (_ : _, _, _) -> (arityFailures <> childFailures, Nothing)
                    ([], _, _) -> finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes

    finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes =
      let (resultInfoFailures, resultInfos) =
            partitionEithers
              ( zipWith
                  (scalarOrCallableInfo statementIndex)
                  [childPath <> replicate remainingApplications 0 | remainingApplications <- reverse [0 .. length resultTypes - 1]]
                  resultTypes
              )
          failures = childFailures <> resultInfoFailures
          typedApplication = do
            typedCallee <- maybeCallee
            typedArguments <- traverse snd finalizedArguments
            pure
              ( foldl'
                  (\typedFunction (info, argument) -> TypedApplyExpr info typedFunction argument)
                  typedCallee
                  (zip resultInfos typedArguments)
              )
       in (failures, if null failures then typedApplication else Nothing)

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

    callableOversaturationSupported directArity resultTypes =
      all isCallableResult intermediateOversaturationResults
      where
        intermediateOversaturationResults =
          take
            (max 0 (length resultTypes - directArity))
            (drop (max 0 (directArity - 1)) resultTypes)
        isCallableResult expressionType =
          case defaultScalarLiterals (resolveType state expressionType) of
            TFunctionType {} -> True
            _ -> False

    statementFailure statementIndex kind detail =
      TypedCoreProductionFailure (TypedCoreProductionStatementPath modulePath statementIndex) kind detail

    binderAt statementIndex suffix name =
      TypedBinderId (modulePath, statementIndex : suffix, name)

    binderBelongsToStatement statementIndex (TypedBinderId (_, binderPath, _)) =
      case binderPath of
        ownerStatementIndex : _ -> ownerStatementIndex == statementIndex
        [] -> False

    resolvedValueName name =
      case name of
        GeneratedName (OperatorBinding storageName) -> TypedGeneratedName (TypedOperatorBinding storageName)
        _ -> TypedResolvedName TypedCurrentModule TypedValueNamespace (identifierText name)

    scheme owner callableShape info =
      TypedScheme owner [] [] [] (typedNodeType info) (typedNodeRecipe info) maybeCallableShape
      where
        maybeCallableShape =
          case typedNodeType info of
            TypedFunctionType {} -> Just callableShape
            _ -> Nothing

    callableInfo callableShape directArity statementIndex childPath expressionType =
      case callableTypeAndRecipe callableShape directArity statementIndex childPath expressionType of
        Right (typeValue@TypedFunctionType {}, recipe@TypedClosureRecipe {}) ->
          Right (TypedNodeInfo typeValue recipe [] [])
        Right _ -> Left (failureAt statementIndex childPath TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail)
        Left failure -> Left failure

    scalarOrCallableInfo statementIndex childPath expressionType =
      valueInfo statementIndex childPath expressionType

    valueInfo statementIndex childPath expressionType =
      case valueTypeAndRecipe statementIndex childPath expressionType of
        Right (typeValue, recipe) -> Right (TypedNodeInfo typeValue recipe [] [])
        Left failure -> Left failure

    callableTypeAndRecipe callableShape directArity =
      case callableShape of
        TypedDirectCallableShape -> directTypeAndRecipe directArity
        TypedClosureCallableShape -> stagedTypeAndRecipe

    directTypeAndRecipe remainingDirectArity statementIndex childPath expressionType =
      case (remainingDirectArity, defaultScalarLiterals (resolveType state expressionType)) of
        (remaining, TFunctionType argument result)
          | remaining > 0 -> do
              (argumentType, argumentRecipe) <- valueTypeAndRecipe statementIndex childPath argument
              (resultType, resultRecipe) <-
                if remaining == 1
                  then valueTypeAndRecipe statementIndex childPath result
                  else directTypeAndRecipe (remaining - 1) statementIndex childPath result
              let recipe =
                    if remaining == 1
                      then TypedClosureRecipe [argumentRecipe] resultRecipe
                      else prependClosureRecipe argumentRecipe resultRecipe
              Right (TypedFunctionType argumentType resultType, recipe)
        (_, other) -> scalarTypeAndRecipe statementIndex childPath other

    resolvedFunctionArity expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        TFunctionType _ result -> 1 + resolvedFunctionArity result
        _ -> 0

    stagedTypeAndRecipe statementIndex childPath expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        TFunctionType argument result -> do
          (argumentType, argumentRecipe) <- valueTypeAndRecipe statementIndex childPath argument
          (resultType, resultRecipe) <- valueTypeAndRecipe statementIndex childPath result
          Right (TypedFunctionType argumentType resultType, TypedClosureRecipe [argumentRecipe] resultRecipe)
        other -> scalarTypeAndRecipe statementIndex childPath other

    valueTypeAndRecipe statementIndex childPath expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        resolvedFunctionType@TFunctionType {} -> stagedTypeAndRecipe statementIndex childPath resolvedFunctionType
        other -> scalarTypeAndRecipe statementIndex childPath other

    scalarTypeAndRecipe statementIndex childPath expressionType =
      case scalarInfo statementIndex childPath expressionType of
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
            ProvisionalFunctionBinding declaration expression
              | lambdaCount expression > 0 ->
                  Map.insertWith
                    (\_ firstFunction -> firstFunction)
                    name
                    (FunctionProfile statementIndex expressionType (lambdaCount expression))
                    functions
              where
                statementIndex = provisionalCallableStatementIndex declaration
                name = provisionalCallableName declaration
                expressionType = provisionalCallableType declaration
            _ -> functions

    callableDeclarations statements =
      [ declaration
      | statement <- statements,
        declaration <-
          case statement of
            ProvisionalFunctionBinding candidate _ -> [candidate]
            ProvisionalUnsupportedCallableBinding candidate _ _ _ -> [candidate]
            _ -> []
      ]

    callableShapeTable functions statements =
      foldl' promoteRecursiveGroup transitiveShapes orderedRecursiveGroupNames
      where
        (_, baseShapes, directCaptureFunctions) =
          foldl'
            collect
            (Set.empty, Map.map (const TypedDirectCallableShape) functions, Set.empty)
            statements
        closureDependencies =
          foldl'
            collectClosureDependencies
            Map.empty
            statements
        collectClosureDependencies dependencies statement =
          case statement of
            ProvisionalFunctionBinding declaration expression ->
              Map.insertWith
                Set.union
                (provisionalCallableName declaration)
                (Set.intersection (Map.keysSet functions) (provisionalFreeNames expression))
                dependencies
            _ -> dependencies
        transitiveCaptureFunctions = propagateCaptureDependencies directCaptureFunctions
        transitiveShapes = Map.mapWithKey promoteTransitiveCapture baseShapes
        orderedRecursiveGroupNames =
          snd (foldl' collectRecursiveGroup (Set.empty, []) (callableDeclarations statements))
        namesByStatement =
          Map.fromList
            [ (provisionalCallableStatementIndex declaration, provisionalCallableName declaration)
            | declaration <- callableDeclarations statements
            ]
        collectRecursiveGroup (seenGroups, groups) declaration =
          case provisionalCallableRecursiveGroupMembers declaration of
            Just memberStatements
              | Set.notMember memberStatements seenGroups ->
                  case traverse (`Map.lookup` namesByStatement) memberStatements of
                    Just memberNames ->
                      (Set.insert memberStatements seenGroups, groups <> [memberNames])
                    Nothing -> (Set.insert memberStatements seenGroups, groups)
            _ -> (seenGroups, groups)
        promoteRecursiveGroup shapes memberNames
          | any ((== TypedClosureCallableShape) . shapeFor shapes) memberNames =
              foldl' (flip markClosure) shapes memberNames
          | otherwise = shapes
        propagateCaptureDependencies capturingFunctions =
          let nextCapturingFunctions =
                Map.foldlWithKey'
                  promote
                  capturingFunctions
                  closureDependencies
           in if nextCapturingFunctions == capturingFunctions
                then capturingFunctions
                else propagateCaptureDependencies nextCapturingFunctions
          where
            promote accumulatedCaptures name dependencies
              | Set.null (Set.intersection accumulatedCaptures dependencies) = accumulatedCaptures
              | otherwise = Set.insert name accumulatedCaptures
        promoteTransitiveCapture name shape
          | Set.member name transitiveCaptureFunctions = TypedClosureCallableShape
          | otherwise = shape
        collect (visibleScalars, callableShapes, capturingFunctions) statement =
          let shapesAfterUses = collectStatementCallableUses functions Set.empty callableShapes statement
           in case statement of
                ProvisionalScalarBinding _ name _ _ _ ->
                  (Set.insert name visibleScalars, shapesAfterUses, capturingFunctions)
                ProvisionalFunctionBinding declaration expression ->
                  let name = provisionalCallableName declaration
                      capturesScalar = not (Set.disjoint visibleScalars (provisionalFreeNames expression))
                      shapesAfterCapture =
                        if capturesScalar
                          then markClosure name shapesAfterUses
                          else shapesAfterUses
                      nextCapturingFunctions =
                        if capturesScalar
                          then Set.insert name capturingFunctions
                          else capturingFunctions
                   in (Set.delete name visibleScalars, shapesAfterCapture, nextCapturingFunctions)
                ProvisionalUnsupportedCallableBinding declaration _ _ _ ->
                  (Set.delete (provisionalCallableName declaration) visibleScalars, shapesAfterUses, capturingFunctions)
                _ -> (visibleScalars, shapesAfterUses, capturingFunctions)

    provisionalFreeNames = freeNames Set.empty
      where
        freeNames boundNames expression =
          case expression of
            ProvisionalVariableExpression name _
              | Set.member name boundNames -> Set.empty
              | otherwise -> Set.singleton name
            ProvisionalLambdaExpression parameterName _ body ->
              freeNames (Set.insert parameterName boundNames) body
            ProvisionalApplyExpression _ function argument ->
              freeNames boundNames function <> freeNames boundNames argument
            ProvisionalBinaryExpression _ _ _ left right ->
              freeNames boundNames left <> freeNames boundNames right
            ProvisionalScopeStatements nestedStatements -> scopeFreeNames boundNames nestedStatements
            _ -> Set.empty

        scopeFreeNames _ [] = Set.empty
        scopeFreeNames boundNames (statement : rest) =
          case statement of
            ProvisionalFunctionBinding declaration expression ->
              let name = provisionalCallableName declaration
                  nextBoundNames = Set.insert name boundNames
               in freeNames nextBoundNames expression <> scopeFreeNames nextBoundNames rest
            ProvisionalScalarBinding _ name _ _ expression ->
              freeNames boundNames expression <> scopeFreeNames (Set.insert name boundNames) rest
            ProvisionalTerminalExpression _ _ expression ->
              freeNames boundNames expression <> scopeFreeNames boundNames rest
            _ -> scopeFreeNames boundNames rest

    collectStatementCallableUses functions lexicalNames callableShapes statement =
      case statement of
        ProvisionalFunctionBinding _ expression ->
          collectExpressionCallableUses functions lexicalNames callableShapes expression
        ProvisionalScalarBinding _ _ _ _ expression ->
          collectExpressionCallableUses functions lexicalNames callableShapes expression
        ProvisionalTerminalExpression _ _ expression ->
          collectExpressionCallableUses functions lexicalNames callableShapes expression
        _ -> callableShapes

    collectExpressionCallableUses functions lexicalNames callableShapes expression =
      case expression of
        ProvisionalVariableExpression name _
          | Set.notMember name lexicalNames,
            Map.member name functions ->
              markClosure name callableShapes
          | otherwise -> callableShapes
        ProvisionalLambdaExpression parameterName _ body ->
          collectExpressionCallableUses functions (Set.insert parameterName lexicalNames) callableShapes body
        ProvisionalApplyExpression {} ->
          let (callee, arguments, _) = applicationSpine expression
              afterCallee =
                case callee of
                  ProvisionalVariableExpression name _
                    | Set.notMember name lexicalNames,
                      Just function <- Map.lookup name functions,
                      length arguments >= functionArity function ->
                        callableShapes
                  _ -> collectExpressionCallableUses functions lexicalNames callableShapes callee
           in foldl'
                (\shapes (_, argument) -> collectExpressionCallableUses functions lexicalNames shapes argument)
                afterCallee
                arguments
        ProvisionalBinaryExpression _ _ _ left right ->
          collectExpressionCallableUses
            functions
            lexicalNames
            (collectExpressionCallableUses functions lexicalNames callableShapes left)
            right
        ProvisionalScopeStatements nestedStatements ->
          collectScopeCallableUses functions lexicalNames callableShapes nestedStatements
        _ -> callableShapes

    markClosure name = Map.insert name TypedClosureCallableShape

    collectScopeCallableUses functions = go
      where
        go _ callableShapes [] = callableShapes
        go lexicalNames callableShapes statements@(statement : rest) =
          case statement of
            ProvisionalFunctionBinding declaration expression ->
              let expressionLexicalNames = Set.insert name (lexicalNames <> forwardFunctionNames statements)
                  nextShapes = collectExpressionCallableUses functions expressionLexicalNames callableShapes expression
               in go (Set.insert name lexicalNames) nextShapes rest
              where
                name = provisionalCallableName declaration
            ProvisionalScalarBinding _ name _ _ expression ->
              let nextShapes = collectExpressionCallableUses functions lexicalNames callableShapes expression
               in go (Set.insert name lexicalNames) nextShapes rest
            ProvisionalTerminalExpression _ _ expression ->
              go lexicalNames (collectExpressionCallableUses functions lexicalNames callableShapes expression) rest
            _ -> go lexicalNames callableShapes rest
        forwardFunctionNames statements =
          Set.fromList
            [ name
            | ProvisionalSignature _ name _ (TFunctionType _ _) <- statements
            ]

    shapeFor callableShapes name =
      Map.findWithDefault TypedDirectCallableShape name callableShapes

    reboundFunctionStatements statements =
      snd (foldl' collect (Set.empty, Map.empty) statements)
      where
        collect (seenNames, reboundStatements) statement =
          case statement of
            ProvisionalFunctionBinding declaration _ -> collectDeclaration seenNames reboundStatements declaration
            ProvisionalUnsupportedCallableBinding declaration _ _ _ -> collectDeclaration seenNames reboundStatements declaration
            _ -> (seenNames, reboundStatements)

        collectDeclaration seenNames reboundStatements declaration
          | Set.member name seenNames =
              (seenNames, Map.insert statementIndex name reboundStatements)
          | otherwise =
              (Set.insert name seenNames, reboundStatements)
          where
            statementIndex = provisionalCallableStatementIndex declaration
            name = provisionalCallableName declaration

    recursiveDeclarationBinders declarations =
      Set.fromList
        [ binderAt
            (provisionalCallableStatementIndex declaration)
            []
            (resolvedValueName (provisionalCallableName declaration))
        | declaration <- declarations,
          Just _ <- [provisionalCallableRecursiveGroupMembers declaration]
        ]

    orderedTypedRecursiveGroups ::
      [ProvisionalCallableDeclaration] ->
      [TypedRecursiveGroup]
    orderedTypedRecursiveGroups declarations =
      snd (foldl' collect (Set.empty, []) declarations)
      where
        declarationBindersByStatement =
          Map.fromList
            [ ( provisionalCallableStatementIndex declaration,
                binderAt
                  (provisionalCallableStatementIndex declaration)
                  []
                  (resolvedValueName (provisionalCallableName declaration))
              )
            | declaration <- declarations
            ]
        collect (seenGroups, groups) declaration =
          case provisionalCallableRecursiveGroupMembers declaration of
            Just memberStatements
              | Set.notMember memberStatements seenGroups ->
                  case traverse (`Map.lookup` declarationBindersByStatement) memberStatements of
                    Just memberBinders ->
                      ( Set.insert memberStatements seenGroups,
                        groups <> [TypedRecursiveGroup memberBinders]
                      )
                    Nothing -> (Set.insert memberStatements seenGroups, groups)
            _ -> (seenGroups, groups)

    supportedRecursiveProfile functions callableShapes reboundFunctions statements typedRecursiveGroups =
      ( Set.fromList (map fst supportedMemberCaptures),
        foldl' addCaptureType Map.empty (concatMap snd supportedMemberCaptures),
        unavailableClosureCaptureBinders
      )
      where
        recursiveMemberSet =
          Set.fromList
            [ member
            | TypedRecursiveGroup members <- typedRecursiveGroups,
              member <- members
            ]
        supportedMemberCaptures =
          concat
            [ supportedGroupMembers (Set.fromList members) members
            | TypedRecursiveGroup members <- typedRecursiveGroups
            ]
        addCaptureType captureTypes (binder, expressionType) =
          Map.insertWith
            (\_ existingType -> existingType)
            binder
            expressionType
            captureTypes
        scalarBindingsBeforeStatements =
          snd (foldl' collectScalarBinding (Map.empty, Map.empty) (zip [0 ..] statements))
        collectScalarBinding (visibleBindings, snapshots) (statementIndex, statement) =
          ( nextVisibleBindings,
            Map.insert statementIndex visibleBindings snapshots
          )
          where
            nextVisibleBindings =
              case statement of
                ProvisionalScalarBinding _ name _ _ _ ->
                  Map.insert
                    name
                    (binderAt statementIndex [] (resolvedValueName name))
                    visibleBindings
                _ -> visibleBindings
        scalarBinderStatements =
          Map.fromList
            [ (binderAt statementIndex [] (resolvedValueName name), statementIndex)
            | (statementIndex, ProvisionalScalarBinding _ name _ _ _) <- zip [0 ..] statements,
              Map.notMember name functions
            ]
        supportedMembers =
          Map.fromList
            [ (owner, (name, callableShape, directArity, typedExpression, scalarCaptureTypes, closureDependencies))
            | ProvisionalFunctionBinding declaration expression <- statements,
              let statementIndex = provisionalCallableStatementIndex declaration,
              let name = provisionalCallableName declaration,
              let owner = binderAt statementIndex [] (resolvedValueName name),
              ProvisionalLambdaExpression {} <- [expression],
              let callableShape = shapeFor callableShapes name,
              Map.notMember statementIndex reboundFunctions,
              not (generatedOperatorName name),
              Just PlainTypeBinding {} <- [provisionalCallableBinding declaration],
              let directArity = maybe 0 functionArity (Map.lookup name functions),
              Right _ <- [callableInfo callableShape directArity statementIndex [] (provisionalCallableType declaration)],
              let scalarBindings = Map.findWithDefault Map.empty statementIndex scalarBindingsBeforeStatements,
              let (expressionFailures, maybeTypedExpression) =
                    finalizeExpression
                      Map.empty
                      functions
                      callableShapes
                      statementIndex
                      [0]
                      scalarBindings
                      (FunctionBindingExpression callableShape directArity)
                      expression,
              null expressionFailures,
              Just typedExpression <- [maybeTypedExpression],
              let scalarCaptureTypes = provisionalScalarReferenceTypes scalarBindings expression,
              let closureDependencies =
                    Set.toAscList
                      (Set.intersection (Map.keysSet functions) (provisionalFreeNames expression))
            ]
        supportedMemberOwnersByName =
          Map.fromList
            [ (name, owner)
            | (owner, (name, _, _, _, _, _)) <- Map.toList supportedMembers
            ]
        unavailableClosureCaptureBinders =
          Set.fromList
            [ owner
            | (owner, (_, callableShape, _, _, _, _)) <- Map.toList supportedMembers,
              callableShape == TypedClosureCallableShape,
              Set.notMember owner recursiveMemberSet,
              Just statementIndex <- [binderStatementIndex owner],
              capturesAtOrAfter statementIndex (transitiveScalarCaptureTypes Set.empty owner)
            ]
        supportedGroupMembers memberSet members =
          case traverse (`Map.lookup` supportedMembers) members of
            Just supported@((_, groupShape, _, _, _, _) : _)
              | all ((== groupShape) . memberShape) supported ->
                  case groupShape of
                    TypedDirectCallableShape ->
                      [ (member, [])
                      | all
                          ( \(_, _, directArity, typedExpression, _, _) ->
                              not (nestedLambdaReferencesAnyBinder directArity memberSet typedExpression)
                          )
                          supported,
                        member <- members
                      ]
                    TypedClosureCallableShape ->
                      case members of
                        firstMember : _ ->
                          case binderStatementIndex firstMember of
                            Just firstStatement ->
                              [ (member, scalarCaptureTypes)
                              | member <- members,
                                let scalarCaptureTypes = transitiveScalarCaptureTypes memberSet member,
                                not (capturesAtOrAfter firstStatement scalarCaptureTypes)
                              ]
                            Nothing -> []
                        [] -> []
            _ -> []
        memberShape (_, shape, _, _, _, _) = shape
        transitiveScalarCaptureTypes memberSet = stableCaptureTypes . expandCaptures Set.empty
          where
            expandCaptures expandedFunctions member
              | Set.member member expandedFunctions = []
              | otherwise =
                  case Map.lookup member supportedMembers of
                    Just (_, _, _, _, scalarCaptureTypes, dependencies) ->
                      scalarCaptureTypes
                        <> concat
                          [ expandCaptures nextExpandedFunctions dependency
                          | dependencyName <- dependencies,
                            shapeFor callableShapes dependencyName == TypedClosureCallableShape,
                            Just dependency <- [Map.lookup dependencyName supportedMemberOwnersByName],
                            Set.notMember dependency memberSet
                          ]
                      where
                        nextExpandedFunctions = Set.insert member expandedFunctions
                    Nothing -> []
        stableCaptureTypes = reverse . snd . foldl' collectCapture (Set.empty, [])
          where
            collectCapture (seenCaptures, reversedCaptures) capture@(binder, _)
              | Set.member binder seenCaptures = (seenCaptures, reversedCaptures)
              | otherwise =
                  (Set.insert binder seenCaptures, capture : reversedCaptures)
        capturesAtOrAfter firstStatement scalarCaptureTypes =
          any
            (>= firstStatement)
            [ scalarStatement
            | (binder, _) <- scalarCaptureTypes,
              Just scalarStatement <- [Map.lookup binder scalarBinderStatements]
            ]
        binderStatementIndex (TypedBinderId (_, statementIndex : _, _)) = Just statementIndex
        binderStatementIndex _ = Nothing
        generatedOperatorName name =
          case name of
            GeneratedName (OperatorBinding _) -> True
            _ -> False

    provisionalScalarReferenceTypes scalarBindings = go Set.empty
      where
        go boundNames expression =
          case expression of
            ProvisionalUnitExpression -> []
            ProvisionalLiteralExpression {} -> []
            ProvisionalBinaryExpression _ _ _ left right -> child left <> child right
            ProvisionalVariableExpression name expressionType
              | Set.notMember name boundNames,
                Just binder <- Map.lookup name scalarBindings ->
                  [(binder, expressionType)]
              | otherwise -> []
            ProvisionalLambdaExpression parameterName _ body ->
              go (Set.insert parameterName boundNames) body
            ProvisionalApplyExpression _ function argument -> child function <> child argument
            ProvisionalScopeStatements nestedStatements -> scope boundNames nestedStatements
            ProvisionalUnsupportedExpression {} -> []
            ProvisionalRetainedFailures {} -> []
          where
            child = go boundNames

        scope _ [] = []
        scope boundNames (statement : rest) =
          case statement of
            ProvisionalFunctionBinding declaration expression ->
              let name = provisionalCallableName declaration
                  nextBoundNames = Set.insert name boundNames
               in go nextBoundNames expression <> scope nextBoundNames rest
            ProvisionalScalarBinding _ name _ _ expression ->
              go boundNames expression <> scope (Set.insert name boundNames) rest
            ProvisionalTerminalExpression _ _ expression ->
              go boundNames expression <> scope boundNames rest
            _ -> scope boundNames rest

    nestedLambdaReferencesAnyBinder leadingLambdaCount binders = skipLeading leadingLambdaCount
      where
        skipLeading remaining (TypedLambdaExpr _ _ _ body)
          | remaining > 0 = skipLeading (remaining - 1) body
        skipLeading _ expression = nestedReference expression

        nestedReference expression =
          case expression of
            TypedLiteralExpr {} -> False
            TypedVariableExpr {} -> False
            TypedLambdaExpr _ _ _ body -> expressionReferencesAnyBinder binders body
            TypedOperatorValueExpr {} -> False
            TypedListExpr _ elements -> any nestedReference elements
            TypedTupleExpr _ elements -> any nestedReference elements
            TypedApplyExpr _ function argument -> nestedReference function || nestedReference argument
            TypedTypeApplicationExpr _ function _ _ -> nestedReference function
            TypedIfExpr _ condition thenExpression elseExpression ->
              any nestedReference [condition, thenExpression, elseExpression]
            TypedPatternCaseExpr _ scrutinee arms ->
              nestedReference scrutinee || any armHasNestedReference arms
            TypedBinaryExpr _ _ left right -> nestedReference left || nestedReference right
            TypedLeftSectionExpr _ left _ -> nestedReference left
            TypedRightSectionExpr _ _ right -> nestedReference right
            TypedBlockExpr _ blockStatements -> any statementHasNestedReference blockStatements

        armHasNestedReference (TypedCaseArm _ maybeGuard result) =
          maybe False nestedReference maybeGuard || nestedReference result
        statementHasNestedReference statement =
          case statement of
            TypedLetStatement _ _ _ _ initializer -> nestedReference initializer
            TypedExpressionStatement _ result -> nestedReference result
            TypedImplStatement (TypedImplDeclaration _ _ methods) ->
              any methodHasNestedReference methods
            _ -> False
        methodHasNestedReference (TypedMethodDefinition _ _ _ _ body) = nestedReference body

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
        TypedBlockExpr _ blockStatements -> any statementReferencesBinder blockStatements
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

    finalizeExports functions callableShapes =
      foldl'
        collect
        ([], TypedModuleInterface [] [] [] [])
        orderedModuleExports
      where
        collect (failures, TypedModuleInterface values datas classes impls) (ModuleExport namespace name)
          | namespace == ValueNamespace =
              case [(sourceName, function) | (sourceName, function) <- Map.toList functions, identifierText sourceName == name] of
                [(sourceName, function)] ->
                  let callableShape = shapeFor callableShapes sourceName
                   in case callableInfo callableShape (functionArity function) (functionStatementIndex function) [] (functionType function) of
                        Right info ->
                          let typedName = TypedResolvedName TypedCurrentModule TypedValueNamespace name
                              owner = binderAt (functionStatementIndex function) [] typedName
                           in (failures, TypedModuleInterface (values <> [TypedValueInterface typedName (scheme owner callableShape info)]) datas classes impls)
                        Left _ -> (failures, TypedModuleInterface values datas classes impls)
                _ -> (failures <> [TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name)], TypedModuleInterface values datas classes impls)
          | otherwise =
              (failures <> [TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name)], TypedModuleInterface values datas classes impls)

    orderedModuleExports =
      stableUniqueExports
        ( case coreModuleDeclaredExports coreModule of
            Nothing -> filter publicExport sourceOrderedDeclarations
            Just declaredExports ->
              concatMap exportsForSelector (declaredModuleExportSelectors declaredExports)
        )
      where
        coreModule = resolvedModuleCore resolvedModule
        publicInventory = resolvedModuleExportInventory resolvedModule
        publicExport = (`inventoryHasExport` publicInventory)
        sourceOrderedDeclarations =
          case coreModuleExpr coreModule of
            EBlock statements -> concatMap statementExports statements
            _ -> []

        statementExports statement =
          case statement of
            SLet name _ _
              | not (generatedOperatorName name) ->
                  [ModuleExport ValueNamespace (identifierText name)]
            SData _ typeName _ constructors ->
              ModuleExport TypeNamespace (identifierText typeName)
                : [ ModuleExport ConstructorNamespace (identifierText constructorName)
                  | DataConstructor constructorName _ <- constructors
                  ]
            SClass _ className _ _ ->
              [ModuleExport CapabilityNamespace (identifierText className)]
            _ -> []

        generatedOperatorName name =
          case name of
            GeneratedName (OperatorBinding _) -> True
            _ -> False

        exportsForSelector selector =
          case selector of
            ModuleExportSelector maybeNamespace name ->
              let matchingDeclarations =
                    [ export
                    | export <- sourceOrderedDeclarations,
                      moduleExportName export == name,
                      maybe True (== moduleExportNamespace export) maybeNamespace,
                      publicExport export
                    ]
               in case matchingDeclarations of
                    _ : _ -> matchingDeclarations
                    [] ->
                      [ export
                      | namespace <- maybe exportNamespaces (: []) maybeNamespace,
                        let export = ModuleExport namespace name,
                        publicExport export
                      ]
            ModuleTypeExportSelector typeName _ constructorSelector ->
              filter publicExport (ModuleExport TypeNamespace typeName : selectedConstructors typeName constructorSelector)

        selectedConstructors typeName constructorSelector =
          case constructorSelector of
            AbstractType -> []
            AllTypeConstructors _ -> sourceConstructors typeName
            SelectedTypeConstructors constructors ->
              [ ModuleExport ConstructorNamespace (locatedModuleExportName constructor)
              | constructor <- NonEmpty.toList constructors
              ]

        sourceConstructors typeName =
          case coreModuleExpr coreModule of
            EBlock statements ->
              concat
                [ [ModuleExport ConstructorNamespace (identifierText constructorName) | DataConstructor constructorName _ <- constructors]
                | SData _ sourceTypeName _ constructors <- statements,
                  identifierText sourceTypeName == typeName
                ]
            _ -> []

        exportNamespaces =
          [ ValueNamespace,
            ConstructorNamespace,
            TypeNamespace,
            CapabilityNamespace
          ]

        stableUniqueExports = reverse . snd . foldl' keep (Set.empty, [])
          where
            keep (seen, exports) export
              | Set.member export seen = (seen, exports)
              | otherwise = (Set.insert export seen, export : exports)

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
