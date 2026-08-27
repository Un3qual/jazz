{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module Jazz.Compiler.TypeInference.Elaboration.Finalize
  ( finalizeValidatedTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (DataConstructor (..), Expr (..), Literal (..), NumericType (..), Pattern (..), Statement (..))
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
    BuiltinSymbol (BuiltinTextAppend, BuiltinTextAppendChar, BuiltinTextLength),
    builtinSymbolArity,
    builtinSymbolKernelName,
    lookupBuiltinSymbolInMode,
  )
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
  )
import Jazz.Compiler.Pattern (patternBinderNames)
import Jazz.Compiler.TypeInference.Elaboration.Profiles
  ( FinalizationProfile (..),
    analyzeFinalizationProfile,
    provisionalFreeNames,
    shapeFor,
  )
import Jazz.Compiler.TypeInference.Elaboration.Specialize
  ( concreteIntegralType,
    defaultScalarLiterals,
    provisionalExpressionType,
    provisionalParameterReferenceTypes,
    specializeCallableCaptureType,
    specializeCompatibleType,
    specializeExpressionType,
    specializeProvisionalCallableCapture,
    specializeProvisionalExpression,
    specializeProvisionalParameterReferences,
  )
import Jazz.Compiler.TypeInference.Elaboration.StructuredValues
  ( StructuredConstructor (..),
    StructuredValueCatalog,
    buildStructuredValueCatalog,
    structuredConstructorBySourceName,
    structuredDataStatement,
    structuredNodeInfo,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( ExpressionEvaluation (..),
    ExpressionRole (..),
    FinalizationEnv (..),
    FinalizationLocation (..),
    FunctionProfile (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalConstructorDeclaration (..),
    ProvisionalDataDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionOutcome,
    TypedCoreProductionPath (..),
    invariantFailuresTypedCoreProductionOutcome,
    succeededTypedCoreProductionOutcome,
    unsupportedTypedCoreProductionOutcome,
  )
import Jazz.Compiler.TypeInference.Solver
  ( resolveType,
  )
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType (..), TypeBinding (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( validateTypedProgramOnce,
  )

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
      let structuredCatalogResult = buildStructuredValueCatalog modulePath state provisionalStatements
          profile = analyzeFinalizationProfile modulePath provisionalStatements
          baseFunctions = profileBaseFunctions profile
          callableShapes = profileCallableShapes profile
          reboundFunctions = profileReboundFunctions profile
          typedRecursiveGroups = profileTypedRecursiveGroups profile
          recursiveBinders = profileRecursiveBinders profile
          (acceptedRecursiveBinders, recursiveScalarCaptureTypes, unavailableClosureCaptureBinders, eagerClosureCaptureStatements) =
            case structuredCatalogResult of
              Left _ -> (Set.empty, Map.empty, Set.empty, Map.empty)
              Right structuredCatalog ->
                supportedRecursiveProfile
                  structuredCatalog
                  baseFunctions
                  callableShapes
                  reboundFunctions
                  provisionalStatements
                  typedRecursiveGroups
          functions = specializeFunctionProfiles recursiveScalarCaptureTypes provisionalStatements baseFunctions
          unsupportedRecursiveBinders = recursiveBinders Set.\\ acceptedRecursiveBinders
          finalizationEnv =
            FinalizationEnv
              { finalizationInferState = state,
                finalizationModulePath = modulePath,
                finalizationFunctions = functions,
                finalizationCallableShapes = callableShapes,
                finalizationScalarCaptureTypes = recursiveScalarCaptureTypes,
                finalizationEagerClosureCaptureStatements = eagerClosureCaptureStatements
              }
          (statementFailures, typedStatements) =
            case structuredCatalogResult of
              Left _ -> ([], [])
              Right structuredCatalog ->
                finalizeStatements
                  structuredCatalog
                  finalizationEnv
                  reboundFunctions
                  unsupportedRecursiveBinders
                  unavailableClosureCaptureBinders
                  provisionalStatements
          exportResult =
            case structuredCatalogResult of
              Left _ -> ([], TypedModuleInterface [] [] [] [])
              Right structuredCatalog -> finalizeExports structuredCatalog provisionalStatements functions callableShapes
          missingResultFailures =
            [ missingModuleResultFailure
            | not (hasTerminalResult provisionalStatements)
            ]
          moduleFailures = missingResultFailures <> fst exportResult
          structuredCatalogFailures = either id (const []) structuredCatalogResult
          productionFailures = structuredCatalogFailures <> moduleFailures <> statementFailures
       in case productionFailures of
            _ : _ -> unsupportedTypedCoreProductionOutcome productionFailures
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
                        Right validatedProgram -> succeededTypedCoreProductionOutcome validatedProgram
                        Left failures -> invariantFailuresTypedCoreProductionOutcome failures
                _ -> unsupportedTypedCoreProductionOutcome [missingModuleResultFailure]
    ProvisionalUnsupportedExpression kind detail ->
      unsupportedTypedCoreProductionOutcome [failureAt 0 [] kind detail]
    _ ->
      unsupportedTypedCoreProductionOutcome [failureAt 0 [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
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

    typedExports _ =
      [ TypedModuleExport (typedNamespace namespace) name
      | ModuleExport namespace name <- orderedModuleExports
      ]

    typedNamespace namespace =
      case namespace of
        ValueNamespace -> TypedValueNamespace
        ConstructorNamespace -> TypedConstructorNamespace
        TypeNamespace -> TypedTypeNamespace
        CapabilityNamespace -> TypedCapabilityNamespace

    failureAt statementIndex childPath kind detail =
      TypedCoreProductionFailure (TypedCoreProductionExpressionPath modulePath statementIndex childPath) kind detail

    finalizeStatements structuredCatalog finalizationEnv reboundFunctions recursiveBinders unavailableClosureCaptureBinders =
      go Map.empty
      where
        go _ [] = ([], [])
        go scalarBindings (statement : rest) =
          let (failures, maybeStatement, nextScalarBindings) =
                finalizeStatement
                  structuredCatalog
                  finalizationEnv
                  reboundFunctions
                  recursiveBinders
                  unavailableClosureCaptureBinders
                  scalarBindings
                  statement
              (restFailures, restStatements) = go nextScalarBindings rest
           in (failures <> restFailures, maybe restStatements (: restStatements) maybeStatement)

    finalizeStatement structuredCatalog finalizationEnv reboundFunctions recursiveBinders unavailableClosureCaptureBinders scalarBindings statement =
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
              selectedCaptureType =
                case Map.lookup name functions of
                  Just function
                    | functionStatementIndex function == statementIndex ->
                        scalarCaptureExpectedType recursiveScalarCaptureTypes scalarBindings expression
                  _ -> Nothing
              selectedExpressionType =
                case Map.lookup name functions of
                  Just function
                    | functionStatementIndex function == statementIndex -> functionType function
                  _ -> expressionType
              captureSpecializedExpression =
                case selectedCaptureType of
                  Just captureType -> specializeProvisionalCallableCapture state captureType expression
                  Nothing -> expression
              selectedExpression =
                specializeProvisionalCallableProfile
                  functions
                  selectedExpressionType
                  captureSpecializedExpression
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
                finalizeExpression
                  structuredCatalog
                  finalizationEnv
                  ( FinalizationLocation
                      statementIndex
                      [0]
                      Map.empty
                      scalarBindings
                      DeferredExpression
                      (FunctionBindingExpression callableShape directArity)
                  )
                  selectedExpression
              infoResult = callableInfo callableShape directArity statementIndex [] selectedExpressionType
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
              selectedCaptureType =
                Map.lookup owner recursiveScalarCaptureTypes
                  <|> scalarCaptureExpectedType recursiveScalarCaptureTypes scalarBindings expression
              selectedExpressionType =
                case selectedCaptureType of
                  Just captureType -> specializeExpressionType state captureType expressionType
                  Nothing -> expressionType
              selectedExpression =
                specializeProvisionalExpression
                  state
                  (selectedCaptureType <|> Just selectedExpressionType)
                  expression
              callableNameCollisionFailures =
                [statementFailure statementIndex TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail | Map.member name functions]
              infoResult = valueInfo statementIndex [] selectedExpressionType
              infoFailures = either (: []) (const []) infoResult
              (expressionFailures, maybeExpression) =
                finalizeExpression
                  structuredCatalog
                  finalizationEnv
                  (FinalizationLocation statementIndex [0] Map.empty scalarBindings EagerExpression ScalarExpression)
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
          let namedApplicationExpression = specializeProvisionalNamedApplications functions expression
              selectedExpression =
                case scalarCaptureExpectedType recursiveScalarCaptureTypes scalarBindings expression of
                  Just captureType ->
                    case namedApplicationExpression of
                      ProvisionalLambdaExpression {} ->
                        specializeProvisionalCallableProfile
                          functions
                          (maybe (resolveType state captureType) id (provisionalExpressionType state namedApplicationExpression))
                          (specializeProvisionalCallableCapture state captureType namedApplicationExpression)
                      _ -> specializeProvisionalExpression state (Just captureType) namedApplicationExpression
                  Nothing -> namedApplicationExpression
              (failures, maybeTypedExpression) =
                finalizeExpression
                  structuredCatalog
                  finalizationEnv
                  (FinalizationLocation statementIndex [] Map.empty scalarBindings EagerExpression ScalarExpression)
                  selectedExpression
           in (failures, TypedExpressionStatement (typedSpan spanValue) <$> maybeTypedExpression, scalarBindings)
        ProvisionalDataStatement (ProvisionalDataDeclaration statementIndex _ _ _ _) ->
          case structuredDataStatement structuredCatalog statementIndex of
            Just typedStatement -> ([], Just typedStatement, scalarBindings)
            Nothing ->
              ( [statementFailure statementIndex TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail],
                Nothing,
                scalarBindings
              )
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
      where
        functions = finalizationFunctions finalizationEnv
        callableShapes = finalizationCallableShapes finalizationEnv
        recursiveScalarCaptureTypes = finalizationScalarCaptureTypes finalizationEnv
    scalarCaptureExpectedType scalarCaptureTypes scalarBindings expression =
      case [ captureType
           | (binder, _) <- provisionalScalarReferenceTypes scalarBindings expression,
             Just captureType <- [Map.lookup binder scalarCaptureTypes]
           ] of
        captureType : _ -> Just captureType
        [] -> Nothing

    finalizeExpression :: StructuredValueCatalog -> FinalizationEnv -> FinalizationLocation -> ProvisionalTypedExpr -> ([TypedCoreProductionFailure], Maybe TypedExpr)
    finalizeExpression structuredCatalog finalizationEnv finalizationLocation expression =
      case expression of
        ProvisionalUnitExpression ->
          ([], Just (TypedTupleExpr unitInfo []))
        ProvisionalTupleExpression expressionType elements ->
          let finalizedElements =
                [ finalizeExpression structuredCatalog finalizationEnv (childLocation [elementIndex] ScalarExpression) element
                | (elementIndex, element) <- zip [0 :: Int ..] elements
                ]
              elementFailures = concatMap fst finalizedElements
           in case elementFailures of
                _ : _ -> (elementFailures, Nothing)
                [] -> case structuredNodeInfo structuredCatalog finalizationState expressionType of
                  Just info -> ([], TypedTupleExpr info <$> traverse snd finalizedElements)
                  Nothing ->
                    ( [failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail],
                      Nothing
                    )
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
                  (leftFailures, maybeLeft) = finalizeExpression structuredCatalog finalizationEnv (childLocation [0] ScalarExpression) left
                  (rightFailures, maybeRight) = finalizeExpression structuredCatalog finalizationEnv (childLocation [1] ScalarExpression) right
                  failures = operatorFailures <> leftFailures <> rightFailures
                  typedExpression =
                    TypedBinaryExpr <$> maybeInfo <*> pure (TypedBuiltinOperator operatorSymbol) <*> maybeLeft <*> maybeRight
               in (failures, if null failures then typedExpression else Nothing)
          | otherwise ->
              let (leftFailures, _) = finalizeExpression structuredCatalog finalizationEnv (childLocation [0] ScalarExpression) left
                  (rightFailures, _) = finalizeExpression structuredCatalog finalizationEnv (childLocation [1] ScalarExpression) right
               in (failureAt statementIndex childPath TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail : leftFailures <> rightFailures, Nothing)
        ProvisionalVariableExpression name expressionType
          | Just constructor <- structuredConstructorBySourceName structuredCatalog name ->
              case structuredConstructorFieldTypes constructor of
                [] ->
                  case concreteConstructorContract structuredCatalog finalizationState constructor expressionType of
                    Just (_, resultInfo, instantiations) ->
                      ( [],
                        Just
                          ( TypedVariableExpr
                              (withInstantiations resultInfo instantiations)
                              (structuredConstructorName constructor)
                              (Just (structuredConstructorBinder constructor))
                          )
                      )
                    Nothing ->
                      ( [failureAt statementIndex childPath TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail],
                        Nothing
                      )
                _ ->
                  ( [failureAt statementIndex childPath TypedCoreCallableValueUnsupported (TypedCoreNameDetail (identifierText name))],
                    Nothing
                  )
          | TDataType {} <- resolveType finalizationState expressionType ->
              ( [failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail],
                Nothing
              )
          | Just parameterBinder <- Map.lookup name lexicalBindings ->
              let selectedExpressionType =
                    case Map.lookup parameterBinder scalarCaptureTypes of
                      Just captureType -> specializeExpressionType finalizationState captureType expressionType
                      Nothing -> expressionType
                  selectedType = defaultScalarLiterals (resolveType finalizationState selectedExpressionType)
                  captureTypeMismatch =
                    case Map.lookup parameterBinder scalarCaptureTypes of
                      Just captureType ->
                        selectedType /= defaultScalarLiterals (resolveType finalizationState captureType)
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
          | Just _ <- approvedTextRuntimeServiceBuiltin name ->
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
                        | Just existingBinder <- [Map.lookup parameterName lexicalBindings],
                          binderBelongsToStatement statementIndex existingBinder
                        ]
                      parameterPath = childPath
                      parameterBinder = TypedBinderId (finalizationModulePathValue, statementIndex : parameterPath, resolvedValueName parameterName)
                      (bodyFailures, maybeBody) =
                        finalizeExpression
                          structuredCatalog
                          finalizationEnv
                          ( finalizationLocation
                              { finalizationChildPath = childPath <> [0],
                                finalizationParameters = Map.insert parameterName parameterBinder parameterBindings,
                                finalizationExpressionEvaluation = DeferredExpression,
                                finalizationExpressionRole = FunctionBindingExpression callableShape (max 0 (remainingDirectArity - 1))
                              }
                          )
                          body
                      failures = lambdaConstructionFailures parameterName body <> duplicateParameterFailures <> bodyFailures
                   in (failures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
            _ ->
              case callableInfo TypedClosureCallableShape (1 :: Int) statementIndex childPath expressionType of
                Left failure -> ([failure], Nothing)
                Right info ->
                  let parameterBinder = TypedBinderId (finalizationModulePathValue, statementIndex : childPath, resolvedValueName parameterName)
                      (bodyFailures, maybeBody) =
                        finalizeExpression
                          structuredCatalog
                          finalizationEnv
                          ( finalizationLocation
                              { finalizationChildPath = childPath <> [0],
                                finalizationParameters = Map.insert parameterName parameterBinder parameterBindings,
                                finalizationExpressionEvaluation = DeferredExpression,
                                finalizationExpressionRole = ScalarExpression
                              }
                          )
                          body
                      failures = lambdaConstructionFailures parameterName body <> bodyFailures
                   in (failures, TypedLambdaExpr info parameterBinder (resolvedValueName parameterName) <$> maybeBody)
        ProvisionalApplyExpression _ _ _ ->
          finalizeApplicationSpine structuredCatalog finalizationEnv finalizationLocation expression
        ProvisionalIfExpression expressionType condition thenExpression elseExpression ->
          let infoResult = valueInfo statementIndex childPath expressionType
              infoFailures = either (: []) (const []) infoResult
              (conditionFailures, maybeCondition) =
                finalizeExpression structuredCatalog finalizationEnv (childLocation [0] ScalarExpression) condition
              (thenFailures, maybeThenExpression) =
                finalizeExpression structuredCatalog finalizationEnv (childLocation [1] ScalarExpression) thenExpression
              (elseFailures, maybeElseExpression) =
                finalizeExpression structuredCatalog finalizationEnv (childLocation [2] ScalarExpression) elseExpression
              failures = infoFailures <> conditionFailures <> thenFailures <> elseFailures
              typedExpression = TypedIfExpr <$> either (const Nothing) Just infoResult <*> maybeCondition <*> maybeThenExpression <*> maybeElseExpression
           in (failures, if null failures then typedExpression else Nothing)
        ProvisionalPatternCaseExpression expressionType scrutinee arms ->
          let infoResult = valueInfo statementIndex childPath expressionType
              infoFailures = either (: []) (const []) infoResult
              (scrutineeFailures, maybeScrutinee) =
                finalizeExpression
                  structuredCatalog
                  finalizationEnv
                  (childLocation [0] ScalarExpression)
                  scrutinee
              (armFailures, maybeArms) =
                case maybeScrutinee of
                  Just typedScrutinee ->
                    finalizePatternCaseArms
                      (defaultScalarLiterals <$> provisionalExpressionType finalizationState scrutinee)
                      (typedExpressionInfo typedScrutinee)
                      arms
                  Nothing -> ([], Nothing)
              failures =
                infoFailures
                  <> scrutineeFailures
                  <> armFailures
              typedExpression =
                TypedPatternCaseExpr
                  <$> either (const Nothing) Just infoResult
                  <*> maybeScrutinee
                  <*> maybeArms
           in (failures, if null failures then typedExpression else Nothing)
        ProvisionalScopeStatements _ -> ([failureAt statementIndex childPath TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail], Nothing)
        ProvisionalUnsupportedExpression kind detail -> ([failureAt statementIndex childPath kind detail], Nothing)
        ProvisionalRetainedFailures failures ->
          (map (qualifyInferredFailure statementIndex childPath) failures, Nothing)
      where
        finalizationState = finalizationInferState finalizationEnv
        finalizationModulePathValue = finalizationModulePath finalizationEnv
        functions = finalizationFunctions finalizationEnv
        callableShapes = finalizationCallableShapes finalizationEnv
        scalarCaptureTypes = finalizationScalarCaptureTypes finalizationEnv
        eagerClosureCaptureStatements = finalizationEagerClosureCaptureStatements finalizationEnv
        statementIndex = finalizationStatementIndex finalizationLocation
        childPath = finalizationChildPath finalizationLocation
        parameterBindings = finalizationParameters finalizationLocation
        scalarBindings = finalizationScalarBindings finalizationLocation
        lexicalBindings = Map.union parameterBindings scalarBindings
        expressionEvaluation = finalizationExpressionEvaluation finalizationLocation
        expressionRole = finalizationExpressionRole finalizationLocation

        childLocation relativePath role =
          finalizationLocation
            { finalizationChildPath = childPath <> relativePath,
              finalizationExpressionRole = role
            }

        finalizePatternCaseArms maybeScrutineeType scrutineeInfo arms' =
          let finalized = zipWith (finalizePatternCaseArm maybeScrutineeType scrutineeInfo) [0 ..] arms'
              failures = concatMap fst finalized
              maybeArms = traverse snd finalized
           in (failures, maybeArms)

        finalizePatternCaseArm maybeScrutineeType scrutineeInfo armIndex (ProvisionalPatternCaseArm pattern maybeGuard body) =
          let patternPath = childPath <> [armIndex]
              (patternFailures, maybePattern, armParameters) =
                case pattern of
                  PWildcard -> ([], Just (TypedWildcardPattern scrutineeInfo), parameterBindings)
                  PVariable name ->
                    let typedName = resolvedValueName name
                        owner = binderAt statementIndex patternPath typedName
                     in ( [],
                          Just (TypedVariablePattern scrutineeInfo owner typedName),
                          Map.insert name owner parameterBindings
                        )
                  PLiteral literal ->
                    case typedLiteral statementIndex patternPath literal scrutineeInfo of
                      Left failure -> ([failure], Nothing, parameterBindings)
                      Right literalValue ->
                        ([], Just (TypedLiteralPattern scrutineeInfo literalValue), parameterBindings)
                  _ ->
                    ( [ failureAt
                          statementIndex
                          patternPath
                          TypedCorePatternCaseUnsupported
                          TypedCorePatternCaseDetail
                      ],
                      Nothing,
                      parameterBindings
                    )
              specializeArmExpression armExpression =
                case (pattern, maybeScrutineeType) of
                  (PVariable name, Just scrutineeType) ->
                    specializeProvisionalParameterReferences
                      finalizationState
                      name
                      scrutineeType
                      armExpression
                  _ -> armExpression
              (guardFailures, maybeTypedGuard) =
                case maybeGuard of
                  Nothing -> ([], Just Nothing)
                  Just guardExpression ->
                    let (childGuardFailures, typedGuard) =
                          finalizeExpression
                            structuredCatalog
                            finalizationEnv
                            ( finalizationLocation
                                { finalizationChildPath = childPath <> [armIndex + 1, 0],
                                  finalizationParameters = armParameters,
                                  finalizationExpressionRole = ScalarExpression
                                }
                            )
                            (specializeArmExpression guardExpression)
                     in (childGuardFailures, Just <$> typedGuard)
              (bodyFailures, maybeTypedBody) =
                finalizeExpression
                  structuredCatalog
                  finalizationEnv
                  ( finalizationLocation
                      { finalizationChildPath = childPath <> [armIndex + 1, 1],
                        finalizationParameters = armParameters,
                        finalizationExpressionRole = ScalarExpression
                      }
                  )
                  (specializeArmExpression body)
              failures = patternFailures <> guardFailures <> bodyFailures
              typedArm = TypedCaseArm <$> maybePattern <*> maybeTypedGuard <*> maybeTypedBody
           in (failures, if null failures then typedArm else Nothing)

        lambdaConstructionFailures parameterName body =
          case expressionEvaluation of
            EagerExpression ->
              [ failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))
              | name <- Set.toAscList (Set.delete parameterName (provisionalFreeNames body)),
                Just captureStatement <- [Map.lookup name eagerClosureCaptureStatements],
                captureStatement >= statementIndex
              ]
            DeferredExpression -> []

        finalizeNamedFunctionReference name callableShape function =
          case (expressionEvaluation, Map.lookup name eagerClosureCaptureStatements) of
            (EagerExpression, Just captureStatement)
              | captureStatement >= statementIndex ->
                  ( [failureAt statementIndex childPath TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name))],
                    Nothing
                  )
            _ ->
              case callableInfo callableShape (functionArity function) statementIndex childPath (functionType function) of
                Left failure -> ([failure], Nothing)
                Right info ->
                  let typedName = resolvedValueName name
                      functionBinder = binderAt (functionStatementIndex function) [] typedName
                   in ([], Just (TypedVariableExpr info typedName (Just functionBinder)))

    qualifyInferredFailure statementIndex parentPath (InferredProductionFailure relativePath kind detail) =
      failureAt statementIndex (parentPath <> relativePath) kind detail

    finalizeApplicationSpine structuredCatalog finalizationEnv finalizationLocation expression =
      let (callee, arguments, resultTypes) = applicationSpine expression
          functions = finalizationFunctions finalizationEnv
          statementIndex = finalizationStatementIndex finalizationLocation
          childPath = finalizationChildPath finalizationLocation
          lexicalBindings =
            Map.union
              (finalizationParameters finalizationLocation)
              (finalizationScalarBindings finalizationLocation)
          selectedArguments =
            case callee of
              ProvisionalVariableExpression name expressionType
                | Just function <- Map.lookup name functions ->
                    applicationArguments (functionType function) arguments
                | Map.notMember name lexicalBindings,
                  Just _ <- approvedTextRuntimeServiceBuiltin name ->
                    applicationArguments expressionType arguments
              _ -> arguments
          finalizedArguments =
            map
              ( \(argumentPath, argument) ->
                  finalizeExpression
                    structuredCatalog
                    finalizationEnv
                    ( finalizationLocation
                        { finalizationChildPath = childPath <> argumentPath,
                          finalizationExpressionRole = ScalarExpression
                        }
                    )
                    argument
              )
              selectedArguments
          argumentFailures = concatMap fst finalizedArguments
       in case callee of
            ProvisionalVariableExpression name _
              | Just constructor <- structuredConstructorBySourceName structuredCatalog name ->
                  finalizeStructuredConstructorApplication
                    structuredCatalog
                    finalizationEnv
                    finalizationLocation
                    constructor
                    expression
                    finalizedArguments
                    argumentFailures
            ProvisionalVariableExpression name expressionType
              | Map.notMember name lexicalBindings,
                Map.notMember name functions,
                Just symbol <- approvedTextRuntimeServiceBuiltin name ->
                  let expectedArity = builtinSymbolArity symbol
                      actualArity = length arguments
                      selectedResultTypes = applicationResultTypes expressionType resultTypes
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity /= expectedArity
                        ]
                      (calleeFailures, maybeCallee) =
                        case callableInfo TypedDirectCallableShape expectedArity statementIndex childPath expressionType of
                          Left failure -> ([failure], Nothing)
                          Right info ->
                            ( [],
                              Just
                                ( TypedVariableExpr
                                    info
                                    (TypedBuiltinName (builtinSymbolKernelName symbol))
                                    Nothing
                                )
                            )
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments selectedResultTypes
            ProvisionalVariableExpression name _
              | Map.member name lexicalBindings ->
                  let expectedArity = 1
                      actualArity = length arguments
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity > expectedArity,
                          not (callableOversaturationSupported expectedArity resultTypes)
                        ]
                      (calleeFailures, maybeCallee) =
                        finalizeExpression
                          structuredCatalog
                          finalizationEnv
                          ( finalizationLocation
                              { finalizationChildPath = childPath <> replicate actualArity 0,
                                finalizationExpressionRole = CalleeExpression
                              }
                          )
                          callee
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes
              | Just function <- Map.lookup name functions ->
                  let expectedArity = functionArity function
                      actualArity = length arguments
                      selectedResultTypes = applicationResultTypes (functionType function) resultTypes
                      arityFailures =
                        [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
                        | actualArity > expectedArity,
                          not (callableOversaturationSupported expectedArity selectedResultTypes)
                        ]
                      (calleeFailures, maybeCallee) =
                        finalizeExpression
                          structuredCatalog
                          finalizationEnv
                          (finalizationLocation {finalizationExpressionRole = CalleeExpression})
                          callee
                      childFailures = calleeFailures <> argumentFailures
                   in case arityFailures of
                        _ : _ -> (arityFailures <> childFailures, Nothing)
                        [] ->
                          finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments selectedResultTypes
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
                      structuredCatalog
                      finalizationEnv
                      ( finalizationLocation
                          { finalizationChildPath = childPath <> replicate actualArity 0,
                            finalizationExpressionRole = CalleeExpression
                          }
                      )
                      callee
                  childFailures = calleeFailures <> argumentFailures
               in case arityFailures of
                    _ : _ -> (arityFailures <> childFailures, Nothing)
                    [] -> finalizeStagedApplications statementIndex childPath childFailures maybeCallee finalizedArguments resultTypes

    finalizeStructuredConstructorApplication structuredCatalog finalizationEnv finalizationLocation constructor expression finalizedArguments argumentFailures =
      let statementIndex = finalizationStatementIndex finalizationLocation
          childPath = finalizationChildPath finalizationLocation
          expectedArity = length (structuredConstructorFieldTypes constructor)
          actualArity = length finalizedArguments
          arityFailures =
            [ failureAt statementIndex childPath TypedCoreCallArityUnsupported (TypedCoreArityDetail expectedArity actualArity)
            | actualArity /= expectedArity
            ]
       in case arityFailures of
            _ : _ -> (arityFailures <> argumentFailures, Nothing)
            [] ->
              case provisionalExpressionType (finalizationInferState finalizationEnv) expression of
                Nothing ->
                  ( [failureAt statementIndex childPath TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail]
                      <> argumentFailures,
                    Nothing
                  )
                Just resultExpressionType ->
                  case concreteConstructorContract structuredCatalog (finalizationInferState finalizationEnv) constructor resultExpressionType of
                    Nothing ->
                      ( [failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail]
                          <> argumentFailures,
                        Nothing
                      )
                    Just (fieldInfos, resultInfo, instantiations) ->
                      let constructorInfo =
                            TypedNodeInfo
                              ( foldr
                                  (TypedFunctionType . typedNodeType)
                                  (typedNodeType resultInfo)
                                  fieldInfos
                              )
                              (TypedClosureRecipe (map typedNodeRecipe fieldInfos) (typedNodeRecipe resultInfo))
                              instantiations
                              []
                          constructorExpression =
                            TypedVariableExpr
                              constructorInfo
                              (structuredConstructorName constructor)
                              (Just (structuredConstructorBinder constructor))
                          resultInfos =
                            [ stagedConstructorResultInfo remainingFields resultInfo
                            | appliedCount <- [1 .. length fieldInfos],
                              let remainingFields = drop appliedCount fieldInfos
                            ]
                          typedApplication = do
                            typedArguments <- traverse snd finalizedArguments
                            pure
                              ( foldl'
                                  (\typedFunction (info, argument) -> TypedApplyExpr info typedFunction argument)
                                  constructorExpression
                                  (zip resultInfos typedArguments)
                              )
                       in (argumentFailures, if null argumentFailures then typedApplication else Nothing)

    stagedConstructorResultInfo remainingFields resultInfo =
      case remainingFields of
        [] -> resultInfo
        _ ->
          TypedNodeInfo
            (foldr (TypedFunctionType . typedNodeType) (typedNodeType resultInfo) remainingFields)
            (TypedClosureRecipe (map typedNodeRecipe remainingFields) (typedNodeRecipe resultInfo))
            []
            []

    concreteConstructorContract structuredCatalog finalizationState constructor resultExpressionType = do
      resultInfo@(TypedNodeInfo resultType _ _ _) <- structuredNodeInfo structuredCatalog finalizationState resultExpressionType
      concreteArguments <-
        case resultType of
          TypedDataType dataName arguments
            | dataName == structuredConstructorDataName constructor -> Just arguments
          _ -> Nothing
      guard (length concreteArguments == length (structuredConstructorParameters constructor))
      parameterContracts <- traverse parameterContract concreteArguments
      let bindings = Map.fromList (zip (structuredConstructorParameters constructor) parameterContracts)
      fieldTypes <- traverse (substituteStructuredType bindings) (structuredConstructorFieldTypes constructor)
      fieldRecipes <- traverse (substituteStructuredRecipe bindings) (structuredConstructorFieldRecipes constructor)
      let fieldInfos = zipWith (\typeValue recipe -> TypedNodeInfo typeValue recipe [] []) fieldTypes fieldRecipes
          instantiations =
            [ TypedInstantiation
                (structuredConstructorBinder constructor)
                ( zipWith
                    TypedTypeArgument
                    (structuredConstructorParameters constructor)
                    concreteArguments
                )
                Nothing
            | not (null concreteArguments)
            ]
      pure (fieldInfos, resultInfo, instantiations)
      where
        parameterContract typeValue = do
          recipe <- representationRecipeForTypedType typeValue
          pure (typeValue, recipe)

    substituteStructuredType bindings typeValue =
      case typeValue of
        TypedListType elementType -> TypedListType <$> child elementType
        TypedTupleType elementTypes -> TypedTupleType <$> traverse child elementTypes
        TypedDataType dataName arguments -> TypedDataType dataName <$> traverse child arguments
        TypedFunctionType argument result -> TypedFunctionType <$> child argument <*> child result
        TypedTypeParameterType parameter -> fst <$> Map.lookup parameter bindings
        _ -> Just typeValue
      where
        child = substituteStructuredType bindings

    substituteStructuredRecipe bindings recipe =
      case recipe of
        TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe <$> child elementRecipe
        TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe <$> traverse child elementRecipes
        TypedManagedVariantRecipe dataName arguments ->
          TypedManagedVariantRecipe dataName <$> traverse (substituteStructuredType bindings) arguments
        TypedClosureRecipe arguments result -> TypedClosureRecipe <$> traverse child arguments <*> child result
        TypedRepresentationParameterRecipe parameter -> snd <$> Map.lookup parameter bindings
        _ -> Just recipe
      where
        child = substituteStructuredRecipe bindings

    representationRecipeForTypedType typeValue =
      case typeValue of
        TypedIntType -> Just (TypedSignedIntegerRecipe 64)
        TypedFloatType -> Just (TypedFloatRecipe 64)
        TypedNumericType numericType -> Just (typedNumericRecipe numericType)
        TypedBoolType -> Just TypedBoolRecipe
        TypedCharType -> Just TypedCharRecipe
        TypedTextType -> Just TypedManagedTextRecipe
        TypedListType {} -> Nothing
        TypedTupleType elementTypes ->
          case elementTypes of
            [] -> Just TypedUnitRecipe
            _ -> TypedManagedProductRecipe <$> traverse representationRecipeForTypedType elementTypes
        TypedDataType dataName arguments -> Just (TypedManagedVariantRecipe dataName arguments)
        TypedFunctionType argument result ->
          TypedClosureRecipe
            <$> ((: []) <$> representationRecipeForTypedType argument)
            <*> representationRecipeForTypedType result
        TypedTypeParameterType {} -> Nothing

    typedNumericRecipe numericType =
      case numericType of
        TypedInt8Type -> TypedSignedIntegerRecipe 8
        TypedInt16Type -> TypedSignedIntegerRecipe 16
        TypedInt32Type -> TypedSignedIntegerRecipe 32
        TypedInt64Type -> TypedSignedIntegerRecipe 64
        TypedUInt8Type -> TypedUnsignedIntegerRecipe 8
        TypedUInt16Type -> TypedUnsignedIntegerRecipe 16
        TypedUInt32Type -> TypedUnsignedIntegerRecipe 32
        TypedUInt64Type -> TypedUnsignedIntegerRecipe 64
        TypedFloat16Type -> TypedFloatRecipe 16
        TypedFloat32Type -> TypedFloatRecipe 32
        TypedFloat64Type -> TypedFloatRecipe 64

    withInstantiations (TypedNodeInfo typeValue recipe _ evidence) instantiations =
      TypedNodeInfo typeValue recipe instantiations evidence

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

    applicationResultTypes expressionType provisionalResultTypes =
      zipWith selectResultType selectedResultTypes provisionalResultTypes
        <> drop (length selectedResultTypes) provisionalResultTypes
      where
        selectedResultTypes = take (length provisionalResultTypes) (resultTypes expressionType)
        selectResultType selectedType provisionalType =
          case (resolveType state provisionalType, concreteIntegralType (resolveType state selectedType)) of
            (TIntegerLiteralType {}, Just concreteType) -> specializeExpressionType state concreteType provisionalType
            _ -> provisionalType
        resultTypes selectedType =
          case resolveType state selectedType of
            TFunctionType _ resultType -> resultType : resultTypes resultType
            _ -> []

    applicationArguments expressionType provisionalArguments =
      zipWith selectArgumentType selectedArgumentTypes provisionalArguments
        <> drop (length selectedArgumentTypes) provisionalArguments
      where
        selectedArgumentTypes = take (length provisionalArguments) (argumentTypes expressionType)
        selectArgumentType selectedType (argumentPath, argument) =
          case (provisionalExpressionType state argument, concreteIntegralType (resolveType state selectedType)) of
            (Just TIntegerLiteralType {}, Just concreteType) ->
              (argumentPath, specializeProvisionalExpression state (Just concreteType) argument)
            _ -> (argumentPath, argument)
        argumentTypes selectedType =
          case resolveType state selectedType of
            TFunctionType argumentType resultType -> argumentType : argumentTypes resultType
            _ -> []

    specializeProvisionalNamedApplications functions = specializeProvisionalNamedApplicationsWith functions Set.empty

    specializeProvisionalNamedApplicationsWith functions initialLexicalNames = go initialLexicalNames
      where
        go lexicalNames expression =
          case expression of
            ProvisionalVariableExpression name expressionType
              | Set.notMember name lexicalNames,
                Just function <- Map.lookup name functions ->
                  ProvisionalVariableExpression name (functionType function)
              | otherwise -> ProvisionalVariableExpression name expressionType
            ProvisionalBinaryExpression operatorSymbol expressionType operandType left right ->
              ProvisionalBinaryExpression
                operatorSymbol
                expressionType
                operandType
                (go lexicalNames left)
                (go lexicalNames right)
            ProvisionalLambdaExpression parameterName expressionType body ->
              ProvisionalLambdaExpression
                parameterName
                expressionType
                (go (Set.insert parameterName lexicalNames) body)
            ProvisionalApplyExpression {} ->
              let (callee, arguments, resultTypes) = applicationSpine expression
                  specializedCallee = go lexicalNames callee
                  specializedArguments =
                    [ (argumentPath, go lexicalNames argument)
                    | (argumentPath, argument) <- arguments
                    ]
                  selectedFunctionType =
                    case callee of
                      ProvisionalVariableExpression name _
                        | Set.notMember name lexicalNames,
                          Just function <- Map.lookup name functions ->
                            Just (functionType function)
                      _ -> provisionalExpressionType state specializedCallee
                  (selectedArguments, selectedResultTypes) =
                    case selectedFunctionType of
                      Just selectedFunctionTypeValue ->
                        ( applicationArguments selectedFunctionTypeValue specializedArguments,
                          applicationResultTypes selectedFunctionTypeValue resultTypes
                        )
                      Nothing -> (specializedArguments, resultTypes)
               in foldl'
                    ( \functionExpression (resultType, (_, argument)) ->
                        ProvisionalApplyExpression resultType functionExpression argument
                    )
                    specializedCallee
                    (zip selectedResultTypes selectedArguments)
            ProvisionalIfExpression expressionType condition thenExpression elseExpression ->
              ProvisionalIfExpression
                expressionType
                (go lexicalNames condition)
                (go lexicalNames thenExpression)
                (go lexicalNames elseExpression)
            ProvisionalPatternCaseExpression expressionType scrutinee arms ->
              ProvisionalPatternCaseExpression
                expressionType
                (go lexicalNames scrutinee)
                [ let armLexicalNames = lexicalNames <> patternBinderNames pattern
                   in ProvisionalPatternCaseArm
                        pattern
                        (go armLexicalNames <$> maybeGuard)
                        (go armLexicalNames body)
                | ProvisionalPatternCaseArm pattern maybeGuard body <- arms
                ]
            _ -> expression

    specializeProvisionalCallableProfile functions = go Set.empty
      where
        go lexicalNames expectedType expression =
          case expression of
            ProvisionalLambdaExpression parameterName expressionType body ->
              let resolvedExpressionType = resolveType state expressionType
                  resolvedExpectedType = resolveType state expectedType
                  (parameterType, resultType) =
                    case (resolvedExpressionType, resolvedExpectedType) of
                      (TFunctionType fallbackParameter fallbackResult, TFunctionType expectedParameter expectedResult) ->
                        ( specializeCompatibleType state expectedParameter fallbackParameter,
                          specializeCompatibleType state expectedResult fallbackResult
                        )
                      (TFunctionType fallbackParameter fallbackResult, _) ->
                        (fallbackParameter, fallbackResult)
                      _ -> (resolvedExpressionType, resolvedExpressionType)
                  nextLexicalNames = Set.insert parameterName lexicalNames
                  parameterSpecializedBody =
                    specializeProvisionalParameterReferences state parameterName parameterType body
                  applicationSpecializedBody =
                    specializeProvisionalNamedApplicationsWith functions nextLexicalNames parameterSpecializedBody
                  specializedBody =
                    case applicationSpecializedBody of
                      ProvisionalLambdaExpression {} ->
                        go nextLexicalNames resultType applicationSpecializedBody
                      _ -> specializeProvisionalExpression state (Just resultType) applicationSpecializedBody
                  selectedParameterType =
                    foldl'
                      (\selectedType referenceType -> specializeCompatibleType state referenceType selectedType)
                      parameterType
                      (provisionalParameterReferenceTypes parameterName specializedBody)
                  selectedResultType = maybe resultType id (provisionalExpressionType state specializedBody)
               in ProvisionalLambdaExpression
                    parameterName
                    (TFunctionType selectedParameterType selectedResultType)
                    specializedBody
            _ -> specializeProvisionalExpression state (Just expectedType) expression

    collectStatementCallProfiles referenceFunctions functions statement =
      case statement of
        ProvisionalFunctionBinding declaration expression ->
          let statementIndex = provisionalCallableStatementIndex declaration
              name = provisionalCallableName declaration
              selectedExpression =
                case Map.lookup name referenceFunctions of
                  Just function
                    | functionStatementIndex function == statementIndex ->
                        specializeProvisionalCallableProfile referenceFunctions (functionType function) expression
                  _ -> expression
           in collectExpressionCallProfiles referenceFunctions Set.empty functions selectedExpression
        ProvisionalScalarBinding _ _ _ _ expression -> collectExpressionCallProfiles referenceFunctions Set.empty functions expression
        ProvisionalTerminalExpression _ _ expression -> collectExpressionCallProfiles referenceFunctions Set.empty functions expression
        _ -> functions

    collectExpressionCallProfiles referenceFunctions lexicalNames functions expression =
      case expression of
        ProvisionalTupleExpression _ elements ->
          foldl'
            (collectExpressionCallProfiles referenceFunctions lexicalNames)
            functions
            elements
        ProvisionalBinaryExpression _ _ _ left right ->
          collectExpressionCallProfiles
            referenceFunctions
            lexicalNames
            (collectExpressionCallProfiles referenceFunctions lexicalNames functions left)
            right
        ProvisionalLambdaExpression parameterName _ body ->
          collectExpressionCallProfiles referenceFunctions (Set.insert parameterName lexicalNames) functions body
        ProvisionalApplyExpression {} ->
          let (callee, arguments, _) = applicationSpine expression
              specializedArguments =
                [ (argumentPath, specializeProvisionalNamedApplicationsWith referenceFunctions lexicalNames argument)
                | (argumentPath, argument) <- arguments
                ]
              callSpecializedFunctions =
                case callee of
                  ProvisionalVariableExpression name _
                    | Set.notMember name lexicalNames,
                      Just function <- Map.lookup name functions ->
                        Map.insert
                          name
                          function {functionType = specializeHigherOrderArguments (functionType function) specializedArguments}
                          functions
                  _ -> functions
              childSpecializedFunctions =
                collectExpressionCallProfiles referenceFunctions lexicalNames callSpecializedFunctions callee
           in foldl'
                (\accumulated (_, argument) -> collectExpressionCallProfiles referenceFunctions lexicalNames accumulated argument)
                childSpecializedFunctions
                specializedArguments
        ProvisionalIfExpression _ condition thenExpression elseExpression ->
          foldl'
            (collectExpressionCallProfiles referenceFunctions lexicalNames)
            functions
            [condition, thenExpression, elseExpression]
        ProvisionalPatternCaseExpression _ scrutinee arms ->
          foldl'
            collectArm
            (collectExpressionCallProfiles referenceFunctions lexicalNames functions scrutinee)
            arms
        _ -> functions
      where
        collectArm functionsAcc (ProvisionalPatternCaseArm pattern maybeGuard body) =
          let armLexicalNames = lexicalNames <> patternBinderNames pattern
              functionsAfterGuard =
                maybe
                  functionsAcc
                  (collectExpressionCallProfiles referenceFunctions armLexicalNames functionsAcc)
                  maybeGuard
           in collectExpressionCallProfiles referenceFunctions armLexicalNames functionsAfterGuard body

        specializeHigherOrderArguments functionTypeValue arguments =
          foldl'
            specializeArgument
            functionTypeValue
            (zip [0 :: Int ..] arguments)
        specializeArgument selectedFunctionType (argumentIndex, (_, argument)) =
          case provisionalExpressionType state argument of
            Just argumentType -> specializeHigherOrderArgumentAt argumentIndex argumentType selectedFunctionType
            Nothing -> selectedFunctionType
        specializeHigherOrderArgumentAt argumentIndex argumentType selectedFunctionType =
          case resolveType state selectedFunctionType of
            TFunctionType parameterType resultType
              | argumentIndex == 0,
                TFunctionType {} <- resolveType state parameterType,
                TFunctionType {} <- resolveType state argumentType ->
                  TFunctionType (specializeCompatibleType state argumentType parameterType) resultType
              | argumentIndex > 0 ->
                  TFunctionType parameterType (specializeHigherOrderArgumentAt (argumentIndex - 1) argumentType resultType)
            _ -> selectedFunctionType

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

    approvedTextRuntimeServiceBuiltin name =
      case lookupBuiltinSymbolInMode ResolveKernelOnly (identifierText name) of
        Just symbol@BuiltinTextLength -> Just symbol
        Just symbol@BuiltinTextAppend -> Just symbol
        Just symbol@BuiltinTextAppendChar -> Just symbol
        _ -> Nothing

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

    specializeFunctionProfiles scalarCaptureTypes statements initialFunctions = converge initialFunctions
      where
        converge functions
          | nextFunctions == functions = functions
          | otherwise = converge nextFunctions
          where
            bindingFunctions = fst (foldl' collect (functions, Map.empty) statements)
            nextFunctions = foldl' (collectStatementCallProfiles bindingFunctions) bindingFunctions statements

        collect (functions, scalarBindings) statement =
          case statement of
            ProvisionalFunctionBinding declaration expression ->
              let statementIndex = provisionalCallableStatementIndex declaration
                  name = provisionalCallableName declaration
                  selectedCaptureType = scalarCaptureExpectedType scalarCaptureTypes scalarBindings expression
                  captureSpecializedExpression =
                    case selectedCaptureType of
                      Just captureType -> specializeProvisionalCallableCapture state captureType expression
                      Nothing -> expression
                  currentType =
                    case Map.lookup name functions of
                      Just function
                        | functionStatementIndex function == statementIndex -> functionType function
                      _ -> provisionalCallableType declaration
                  selectedExpression =
                    specializeProvisionalCallableProfile
                      functions
                      currentType
                      captureSpecializedExpression
                  selectedType =
                    case selectedCaptureType of
                      Just captureType ->
                        maybe
                          (specializeCallableCaptureType state captureType currentType)
                          id
                          (provisionalExpressionType state selectedExpression)
                      Nothing -> maybe currentType id (provisionalExpressionType state selectedExpression)
                  nextFunctions =
                    case Map.lookup name functions of
                      Just function
                        | functionStatementIndex function == statementIndex ->
                            Map.insert name function {functionType = selectedType} functions
                      _ -> functions
               in (nextFunctions, scalarBindings)
            ProvisionalScalarBinding statementIndex name _ _ _ ->
              ( functions,
                Map.insert name (binderAt statementIndex [] (resolvedValueName name)) scalarBindings
              )
            _ -> (functions, scalarBindings)

    supportedRecursiveProfile structuredCatalog functions callableShapes reboundFunctions statements typedRecursiveGroups =
      ( Set.fromList (map fst supportedMemberCaptures),
        propagatedScalarCaptureTypes,
        unavailableClosureCaptureBinders,
        eagerClosureCaptureStatements
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
        initialScalarCaptureTypes =
          foldl'
            addCaptureType
            Map.empty
            (concatMap expandScalarAliasCapture (concatMap snd supportedMemberCaptures))
        propagatedScalarCaptureTypes = propagateScalarCaptureTypes initialScalarCaptureTypes
        propagateScalarCaptureTypes captureTypes
          | nextCaptureTypes == captureTypes = captureTypes
          | otherwise = propagateScalarCaptureTypes nextCaptureTypes
          where
            nextCaptureTypes = foldl' (propagateStatementCaptureTypes specializedFunctions) captureTypes statements
            specializedFunctions = specializeFunctionProfiles captureTypes statements functions
        propagateStatementCaptureTypes specializedFunctions captureTypes statement =
          case statement of
            ProvisionalScalarBinding statementIndex name _ _ expression ->
              let scalarBindings = Map.findWithDefault Map.empty statementIndex scalarBindingsBeforeStatements
                  owner = binderAt statementIndex [] (resolvedValueName name)
                  namedApplicationCaptureTypes =
                    propagateNamedApplicationCaptureTypes
                      specializedFunctions
                      captureTypes
                      scalarBindings
                      (Just owner)
                      expression
                  selectedCaptureType =
                    Map.lookup owner namedApplicationCaptureTypes
                      <|> scalarCaptureExpectedType namedApplicationCaptureTypes scalarBindings expression
               in propagateExpressionCaptureTypes namedApplicationCaptureTypes selectedCaptureType scalarBindings (Just owner) expression
            ProvisionalFunctionBinding declaration expression ->
              let statementIndex = provisionalCallableStatementIndex declaration
                  scalarBindings = Map.findWithDefault Map.empty statementIndex scalarBindingsBeforeStatements
                  namedApplicationCaptureTypes =
                    propagateNamedApplicationCaptureTypes
                      specializedFunctions
                      captureTypes
                      scalarBindings
                      Nothing
                      expression
                  selectedCaptureType = scalarCaptureExpectedType namedApplicationCaptureTypes scalarBindings expression
               in case selectedCaptureType of
                    Just captureType ->
                      foldl'
                        addCaptureType
                        namedApplicationCaptureTypes
                        ( provisionalRecoloredScalarReferenceTypes
                            scalarBindings
                            expression
                            (specializeProvisionalCallableCapture state captureType expression)
                        )
                    Nothing -> namedApplicationCaptureTypes
            ProvisionalTerminalExpression statementIndex _ expression ->
              let scalarBindings = Map.findWithDefault Map.empty statementIndex scalarBindingsBeforeStatements
                  namedApplicationCaptureTypes =
                    propagateNamedApplicationCaptureTypes
                      specializedFunctions
                      captureTypes
                      scalarBindings
                      Nothing
                      expression
                  selectedCaptureType = scalarCaptureExpectedType namedApplicationCaptureTypes scalarBindings expression
               in propagateExpressionCaptureTypes namedApplicationCaptureTypes selectedCaptureType scalarBindings Nothing expression
            _ -> captureTypes
        propagateNamedApplicationCaptureTypes specializedFunctions captureTypes scalarBindings maybeOwner expression =
          foldl'
            addCaptureType
            captureTypes
            ( ownerSpecialization
                <> provisionalRecoloredScalarReferenceTypes scalarBindings expression specializedExpression
            )
          where
            specializedExpression = specializeProvisionalNamedApplications specializedFunctions expression
            ownerSpecialization =
              [ (owner, concreteType)
              | owner <- maybe [] (: []) maybeOwner,
                Just originalType <- [provisionalExpressionType state expression],
                Just specializedType <- [provisionalExpressionType state specializedExpression],
                resolveType state originalType /= resolveType state specializedType,
                Just concreteType <- [concreteIntegralType (resolveType state specializedType)]
              ]
        propagateExpressionCaptureTypes captureTypes maybeCaptureType scalarBindings maybeOwner expression =
          case maybeCaptureType of
            Just captureType ->
              foldl'
                addCaptureType
                captureTypes
                ( [ (owner, captureType)
                  | owner <- maybe [] (: []) maybeOwner,
                    Just expressionType <- [provisionalExpressionType state expression],
                    specializeExpressionType state captureType expressionType == resolveType state captureType
                  ]
                    <> provisionalScalarSpecializationTypes scalarBindings (Just captureType) expression
                )
            Nothing -> captureTypes
        scalarBindingsBeforeStatements =
          snd (foldl' collectScalarBinding (Map.empty, Map.empty) statements)
        collectScalarBinding (visibleBindings, snapshots) statement =
          ( nextVisibleBindings,
            Map.insert statementIndex visibleBindings snapshots
          )
          where
            statementIndex = provisionalStatementIndex statement
            nextVisibleBindings =
              case statement of
                ProvisionalScalarBinding _ name _ _ _ ->
                  Map.insert
                    name
                    (binderAt statementIndex [] (resolvedValueName name))
                    visibleBindings
                _ -> visibleBindings
        scalarAliasSources =
          Map.fromList
            [ (owner, referencedBinder)
            | ProvisionalScalarBinding statementIndex name _ _ (ProvisionalVariableExpression referencedName _) <- statements,
              let scalarBindings = Map.findWithDefault Map.empty statementIndex scalarBindingsBeforeStatements,
              Just referencedBinder <- [Map.lookup referencedName scalarBindings],
              let owner = binderAt statementIndex [] (resolvedValueName name)
            ]
        expandScalarAliasCapture (binder, expressionType) =
          [ (aliasBinder, expressionType)
          | aliasBinder <- aliasSourceChain Set.empty binder
          ]
        aliasSourceChain seen binder
          | Set.member binder seen = []
          | otherwise =
              binder
                : case Map.lookup binder scalarAliasSources of
                  Just sourceBinder -> aliasSourceChain (Set.insert binder seen) sourceBinder
                  Nothing -> []
        scalarBinderStatements =
          Map.fromList
            [ (binderAt statementIndex [] (resolvedValueName name), statementIndex)
            | ProvisionalScalarBinding statementIndex name _ _ _ <- statements,
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
              let profileFinalizationEnv =
                    FinalizationEnv
                      { finalizationInferState = state,
                        finalizationModulePath = modulePath,
                        finalizationFunctions = functions,
                        finalizationCallableShapes = callableShapes,
                        finalizationScalarCaptureTypes = Map.empty,
                        finalizationEagerClosureCaptureStatements = Map.empty
                      },
              let (expressionFailures, maybeTypedExpression) =
                    finalizeExpression
                      structuredCatalog
                      profileFinalizationEnv
                      ( FinalizationLocation
                          statementIndex
                          [0]
                          Map.empty
                          scalarBindings
                          DeferredExpression
                          (FunctionBindingExpression callableShape directArity)
                      )
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
        fullScalarCaptureTypesByOwner =
          Map.mapWithKey
            (\owner _ -> transitiveScalarCaptureTypes Set.empty owner)
            supportedMembers
        unavailableClosureCaptureBinders =
          Set.fromList
            [ owner
            | (owner, (_, callableShape, _, _, _, _)) <- Map.toList supportedMembers,
              callableShape == TypedClosureCallableShape,
              Set.notMember owner recursiveMemberSet,
              Just statementIndex <- [binderStatementIndex owner],
              let scalarCaptureTypes = Map.findWithDefault [] owner fullScalarCaptureTypesByOwner,
              capturesAtOrAfter statementIndex scalarCaptureTypes
            ]
        eagerClosureCaptureStatements =
          Map.fromList
            [ (name, maximum captureStatements)
            | (owner, (name, callableShape, _, _, _, _)) <- Map.toList supportedMembers,
              callableShape == TypedClosureCallableShape,
              let captureStatements =
                    [ scalarStatement
                    | (binder, _) <- Map.findWithDefault [] owner fullScalarCaptureTypesByOwner,
                      Just scalarStatement <- [Map.lookup binder scalarBinderStatements]
                    ],
              not (null captureStatements)
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
        provisionalStatementIndex statement =
          case statement of
            ProvisionalSignature statementIndex _ _ _ -> statementIndex
            ProvisionalFunctionBinding declaration _ -> provisionalCallableStatementIndex declaration
            ProvisionalScalarBinding statementIndex _ _ _ _ -> statementIndex
            ProvisionalTerminalExpression statementIndex _ _ -> statementIndex
            ProvisionalDataStatement (ProvisionalDataDeclaration statementIndex _ _ _ _) -> statementIndex
            ProvisionalUnsupportedCallableBinding declaration _ _ _ -> provisionalCallableStatementIndex declaration
            ProvisionalUnsupportedStatement statementIndex _ _ _ -> statementIndex
        generatedOperatorName name =
          case name of
            GeneratedName (OperatorBinding _) -> True
            _ -> False

    provisionalScalarReferenceTypes scalarBindings = go Set.empty
      where
        go boundNames expression =
          case expression of
            ProvisionalUnitExpression -> []
            ProvisionalTupleExpression _ elements -> foldMap child elements
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
            ProvisionalIfExpression _ condition thenExpression elseExpression ->
              child condition <> child thenExpression <> child elseExpression
            ProvisionalPatternCaseExpression _ scrutinee arms ->
              child scrutinee <> foldMap armChildren arms
            ProvisionalScopeStatements nestedStatements -> scope boundNames nestedStatements
            ProvisionalUnsupportedExpression {} -> []
            ProvisionalRetainedFailures {} -> []
          where
            child = go boundNames
            armChildren (ProvisionalPatternCaseArm pattern maybeGuard body) =
              let armChild = go (boundNames <> patternBinderNames pattern)
               in maybe [] armChild maybeGuard <> armChild body

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

    provisionalRecoloredScalarReferenceTypes scalarBindings originalExpression specializedExpression =
      [ (binder, specializedType)
      | ((originalBinder, originalType), (binder, specializedType)) <-
          zip
            (provisionalScalarReferenceTypes scalarBindings originalExpression)
            (provisionalScalarReferenceTypes scalarBindings specializedExpression),
        originalBinder == binder,
        resolveType state originalType /= resolveType state specializedType
      ]

    provisionalScalarSpecializationTypes scalarBindings = go Set.empty
      where
        go boundNames maybeExpected expression =
          case expression of
            ProvisionalUnitExpression -> []
            ProvisionalTupleExpression expressionType elements ->
              let selectedTupleType = specializedType maybeExpected expressionType
                  elementExpectations =
                    case selectedTupleType of
                      TTupleType elementTypes
                        | length elementTypes == length elements -> map Just elementTypes
                      _ -> replicate (length elements) Nothing
               in concat (zipWith child elementExpectations elements)
            ProvisionalLiteralExpression {} -> []
            ProvisionalBinaryExpression _ expressionType operandType left right ->
              let resultType = specializedType maybeExpected expressionType
                  resolvedOperandType = resolveType state operandType
                  operandExpected =
                    concreteIntegralType resultType
                      <|> concreteIntegralType resolvedOperandType
                      <|> (maybeExpected >>= concreteIntegralType . resolveType state)
                      <|> (provisionalExpressionType state left >>= concreteIntegralType . resolveType state)
                      <|> (provisionalExpressionType state right >>= concreteIntegralType . resolveType state)
               in child operandExpected left <> child operandExpected right
            ProvisionalVariableExpression name expressionType
              | Set.notMember name boundNames,
                Just binder <- Map.lookup name scalarBindings,
                Just expectedType <- maybeExpected ->
                  [(binder, specializeExpressionType state expectedType expressionType)]
              | otherwise -> []
            ProvisionalLambdaExpression parameterName expressionType body ->
              let specializedFunctionType = specializedType maybeExpected expressionType
                  bodyExpected =
                    case specializedFunctionType of
                      TFunctionType _ resultType -> Just resultType
                      _ -> Nothing
               in go (Set.insert parameterName boundNames) bodyExpected body
            ProvisionalApplyExpression _ function argument ->
              let argumentExpected =
                    case provisionalExpressionType state function of
                      Just (TFunctionType parameterType _) -> Just parameterType
                      _ -> Nothing
               in child Nothing function <> child argumentExpected argument
            ProvisionalIfExpression expressionType condition thenExpression elseExpression ->
              let resultType = specializedType maybeExpected expressionType
               in child (Just TBoolType) condition
                    <> child (Just resultType) thenExpression
                    <> child (Just resultType) elseExpression
            ProvisionalPatternCaseExpression expressionType scrutinee arms ->
              let resultType = specializedType maybeExpected expressionType
               in child Nothing scrutinee
                    <> foldMap (armChildren resultType) arms
            ProvisionalScopeStatements {} -> []
            ProvisionalUnsupportedExpression {} -> []
            ProvisionalRetainedFailures {} -> []
          where
            child = go boundNames
            armChildren resultType (ProvisionalPatternCaseArm pattern maybeGuard body) =
              let armChild = go (boundNames <> patternBinderNames pattern)
               in maybe [] (armChild (Just TBoolType)) maybeGuard
                    <> armChild (Just resultType) body
            specializedType expected expressionType =
              case expected of
                Just expectedType -> specializeExpressionType state expectedType expressionType
                Nothing -> resolveType state expressionType

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

    finalizeExports structuredCatalog provisionalStatements functions callableShapes =
      foldl'
        collect
        ([], TypedModuleInterface [] selectedDataInterfaces [] [])
        orderedModuleExports
      where
        localDataDeclarations =
          [ ( identifierText sourceName,
              Set.fromList [identifierText constructorName | ProvisionalConstructorDeclaration constructorName _ <- constructors],
              declaration
            )
          | ProvisionalDataStatement (ProvisionalDataDeclaration statementIndex _ sourceName _ constructors) <- provisionalStatements,
            Just (TypedDataStatement declaration) <- [structuredDataStatement structuredCatalog statementIndex]
          ]

        selectedDataInterfaces =
          [ TypedDataInterface declaration
          | (typeName, constructorNames, declaration) <- localDataDeclarations,
            any (selectsData typeName constructorNames) orderedModuleExports
          ]

        selectsData typeName constructorNames (ModuleExport namespace name) =
          (namespace == TypeNamespace && name == typeName)
            || (namespace == ConstructorNamespace && Set.member name constructorNames)

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
          | namespace == TypeNamespace || namespace == ConstructorNamespace =
              case [ ()
                   | (typeName, constructorNames, _) <- localDataDeclarations,
                     (namespace == TypeNamespace && name == typeName)
                       || (namespace == ConstructorNamespace && Set.member name constructorNames)
                   ] of
                [_] -> (failures, TypedModuleInterface values datas classes impls)
                _ ->
                  ( failures <> [TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name)],
                    TypedModuleInterface values datas classes impls
                  )
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
        TTextType -> Right (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [])
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
        (LText value, TypedTextType) -> Right (TypedTextLiteral value)
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

-- | Operators whose representation is supported by direct-call production.
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
