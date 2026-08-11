{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    TypedCoreProductionStatus (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionResult (..),
    inferResolvedModuleTypedCoreExpressionDirectCall,
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltinsAndSourceUnitStatements,
    inferExpressionWithBuiltins,
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements,
    inferExpression,
    inferExpressionDefault,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( DataConstructor (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.Analyzer
  ( AnalysisBinding (..),
    AnalysisInputs (..),
    AnalysisResult (..),
    analyzeProgramWithInputs,
    analyzeProgramWithInputsAndScopeFacts,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol,
    builtinNamesInMode,
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeLiteralIntegerBounds,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    isErrorDiagnostic,
  )
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleExportForBinding,
  )
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    generatedName,
    identifierText,
    mkIdentifier,
    operatorBindingName,
    renderName,
    sourceName,
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.RecursiveBindings
  ( buildRecursiveScopeFacts,
  )
import Jazz.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
  )
import Jazz.Compiler.TypeInference.Capabilities
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Elaboration
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalTypedExpr (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionStatus (..),
    blockProductionFailureKindAndDetail,
    finalizeTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
    specializeInferredExpression,
  )
import Jazz.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    binaryNumericPromotionType,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType,
  )
import Jazz.Compiler.TypeInference.Pattern
  ( inferPatternCaseTypeWithResults,
  )
import Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplicationWithResult,
    inferNestedScopeTypeWithMode,
    inferScopeTypeWithMode,
    inferScopeTypeWithModeAndForwardBindingsUsingFacts,
    instantiateNonBuiltinTypeBinding,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    combineIntegerLiteralRanges,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeFitsNumericType,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferDataTypes,
    inferErrorsRev,
    inferModuleCapabilityFacts,
    inferRuntimeTypeHints,
    inferVisibleTypes,
    initialInferState,
    modifyInferenceOutput,
  )
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypedCore (TypedSourcePath, validTypedSourcePath)
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings,
  )

-- | `InferenceResult` keeps the canonicalized expression plus one ordered
-- diagnostic stream containing analyzer reports followed by type-inference
-- reports.
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredDiagnostics :: [Diagnostic],
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey SignatureType,
    inferredModuleInterface :: ModuleInterface
  }
  deriving (Eq, Show)

data InferenceInputs = InferenceInputs
  { inferenceBuiltinMode :: BuiltinResolutionMode,
    inferenceWarningSettings :: WarningSettings,
    inferenceImportedTypes :: TypeEnv,
    inferenceImportedDataTypes :: Map Text DataTypeBinding,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
    inferenceImportedClassNames :: Set Text,
    inferenceCurrentModulePath :: Maybe [Text]
  }

data TypedCoreProductionResult = TypedCoreProductionResult
  { typedCoreProductionInferenceResult :: InferenceResult,
    typedCoreProductionStatus :: TypedCoreProductionStatus
  }
  deriving (Eq, Show)

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression = inferExpressionWithBuiltins ResolveKernelOnly

inferExpressionWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO InferenceResult
inferExpressionWithBuiltins builtinMode =
  inferExpressionWithBuiltinsAndHiddenStatements builtinMode Set.empty

inferExpressionWithBuiltinsAndHiddenStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO InferenceResult
inferExpressionWithBuiltinsAndHiddenStatements builtinMode hiddenStatementIndices settings =
  inferExpressionWithBuiltinsAndSourceUnitStatements
    builtinMode
    hiddenStatementIndices
    hiddenStatementIndices
    settings

inferExpressionWithBuiltinsAndSourceUnitStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO InferenceResult
inferExpressionWithBuiltinsAndSourceUnitStatements builtinMode hiddenStatementIndices preludeStatementIndices settings =
  inferExpressionWithInputsAndSourceUnitStatements
    (emptyInferenceInputs builtinMode settings)
    hiddenStatementIndices
    preludeStatementIndices

inferExpressionWithInputs :: InferenceInputs -> Expr -> IO InferenceResult
inferExpressionWithInputs inputs =
  inferExpressionWithInputsAndHiddenStatements inputs Set.empty

inferExpressionWithInputsAndHiddenStatements :: InferenceInputs -> Set Int -> Expr -> IO InferenceResult
inferExpressionWithInputsAndHiddenStatements inputs hiddenStatementIndices expr =
  inferExpressionWithInputsAndSourceUnitStatements inputs hiddenStatementIndices hiddenStatementIndices expr

inferExpressionWithInputsAndSourceUnitStatements :: InferenceInputs -> Set Int -> Set Int -> Expr -> IO InferenceResult
inferExpressionWithInputsAndSourceUnitStatements inputs hiddenStatementIndices preludeStatementIndices expr =
  fst <$> inferExpressionWithInputsAndSourceUnitStatementsAndState inputs hiddenStatementIndices preludeStatementIndices expr

inferExpressionWithInputsAndSourceUnitStatementsAndState :: InferenceInputs -> Set Int -> Set Int -> Expr -> IO (InferenceResult, InferState)
inferExpressionWithInputsAndSourceUnitStatementsAndState inputs hiddenStatementIndices preludeStatementIndices expr =
  do
    (inferenceResult, finalState, _, _) <-
      inferExpressionWithInputsAndSourceUnitStatementsAndStateInMode
        InferenceOnly
        inputs
        hiddenStatementIndices
        preludeStatementIndices
        expr
    pure (inferenceResult, finalState)

inferExpressionWithInputsAndSourceUnitStatementsAndStateInMode :: TypedCoreProductionMode -> InferenceInputs -> Set Int -> Set Int -> Expr -> IO (InferenceResult, InferState, InferredExpr, Map Int (Name, SourceSpan))
inferExpressionWithInputsAndSourceUnitStatementsAndStateInMode mode inputs hiddenStatementIndices preludeStatementIndices expr =
  {-# SCC "jazz-stage:type-inference" #-}
  do
  let initialState = initialStateForInference inputs
      (inferredResult, finalState, forwardBindings, topLevelRecursiveScopeFacts) =
        case expr of
          EBlock statements ->
            let recursiveScopeFactsValue =
                  buildRecursiveScopeFacts
                    ( Set.union
                        (Map.keysSet (inferenceImportedTypes inputs))
                        (Set.map (sourceName . mkIdentifier) (builtinNamesInMode (inferenceBuiltinMode inputs)))
                    )
                    (zip [0 ..] statements)
                (blockResult, blockState, bindings) =
                  inferScopeTypeWithModeAndForwardBindingsUsingFacts
                    recursiveScopeFactsValue
                    preludeStatementIndices
                    ( \childMode childBuiltin childEnv childState childExpr ->
                        inferExprTypeWithMode False childMode Set.empty childBuiltin childEnv childState childExpr
                    )
                    mode
                    (inferenceBuiltinMode inputs)
                    (inferenceImportedTypes inputs)
                    initialState
                    statements
             in (blockResult, blockState, bindings, Just recursiveScopeFactsValue)
          _ ->
            let (result, resultState) =
                  inferExprTypeWithMode
                    True
                    mode
                    preludeStatementIndices
                    (inferenceBuiltinMode inputs)
                    (inferenceImportedTypes inputs)
                    initialState
                    expr
             in (result, resultState, Map.empty, Nothing)
      typeErrors = reverse (inferErrorsRev finalState)
      runtimeTypeHints = inferRuntimeTypeHints finalState
  AnalysisResult _ analyzerDiagnostics <-
    case topLevelRecursiveScopeFacts of
      Just recursiveScopeFactsValue ->
        analyzeProgramWithInputsAndScopeFacts
          (analysisInputsForInference inputs (forwardAnalysisValues mode forwardBindings))
          hiddenStatementIndices
          recursiveScopeFactsValue
          expr
      Nothing ->
        analyzeProgramWithInputs
          (analysisInputsForInference inputs (forwardAnalysisValues mode forwardBindings))
          hiddenStatementIndices
          expr
  inferredExpressionType inferredResult `seq`
    pure
      ( InferenceResult
          { inferredExpr = expr,
            inferredDiagnostics = analyzerDiagnostics <> typeErrors,
            inferredRuntimeTypeHints = runtimeTypeHints,
            inferredModuleInterface = moduleInterfaceFromState inputs expr finalState
          },
        finalState,
        inferredResult,
        forwardBindings
      )

inferResolvedModuleTypedCoreExpressionDirectCall ::
  InferenceInputs ->
  TypedSourcePath ->
  ModuleGraph.ResolvedModule ->
  IO TypedCoreProductionResult
inferResolvedModuleTypedCoreExpressionDirectCall inputs sourcePath resolvedModule = do
  let expression = ModuleGraph.coreModuleExpr (ModuleGraph.resolvedModuleCore resolvedModule)
  (inferenceResult, finalState, inferredResult, _) <-
    inferExpressionWithInputsAndSourceUnitStatementsAndStateInMode ProduceTypedCoreExpressionDirectCall inputs Set.empty Set.empty expression
  pure
    TypedCoreProductionResult
      { typedCoreProductionInferenceResult = inferenceResult,
        typedCoreProductionStatus = productionStatus inputs sourcePath resolvedModule finalState inferenceResult inferredResult
      }

productionStatus :: InferenceInputs -> TypedSourcePath -> ModuleGraph.ResolvedModule -> InferState -> InferenceResult -> InferredExpr -> TypedCoreProductionStatus
productionStatus inputs sourcePath resolvedModule finalState inferenceResult inferredResult
  | any isErrorDiagnostic (inferredDiagnostics inferenceResult) = TypedCoreProductionBlockedByDiagnostics
  | not (null profileFailures) = TypedCoreProductionUnsupported profileFailures
  | otherwise =
      case inferredProvisionalExpr inferredResult of
        Just provisionalExpr -> finalizeTypedCoreExpressionDirectCall sourcePath resolvedModule finalState provisionalExpr
        Nothing ->
          TypedCoreProductionUnsupported
            [TypedCoreProductionFailure (TypedCoreProductionModulePath (ModuleGraph.resolvedModulePath resolvedModule)) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
  where
    profileFailures = inputFailures <> moduleFailures
    inputFailures =
      concat
        [ [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail
          | inferenceCurrentModulePath inputs /= Just (ModuleGraph.resolvedModulePath resolvedModule)
          ],
          [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail
          | not (validTypedSourcePath sourcePath)
          ],
          [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail
          | not (Map.null (inferenceImportedTypes inputs))
              || not (Map.null (inferenceImportedDataTypes inputs))
              || inferenceImportedCapabilities inputs /= emptyScopeCapabilityFacts
          ],
          [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail
          | not (Set.null (inferenceImportedClassNames inputs))
          ]
        ]
    moduleFailures =
      [ TypedCoreProductionFailure
          (TypedCoreProductionModulePath (ModuleGraph.resolvedModulePath resolvedModule))
          TypedCoreResolvedImportsUnsupported
          TypedCoreNoFailureDetail
      | not (null (ModuleGraph.resolvedModuleImports resolvedModule))
      ]

emptyInferenceInputs :: BuiltinResolutionMode -> WarningSettings -> InferenceInputs
emptyInferenceInputs builtinMode settings =
  InferenceInputs
    { inferenceBuiltinMode = builtinMode,
      inferenceWarningSettings = settings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

analysisInputsForInference :: InferenceInputs -> Map Int (Name, AnalysisBinding) -> AnalysisInputs
analysisInputsForInference inputs forwardValues =
  AnalysisInputs
    { analysisBuiltinMode = inferenceBuiltinMode inputs,
      analysisWarningSettings = inferenceWarningSettings inputs,
      analysisImportedValues =
        Map.map (const (AnalysisBinding Nothing True)) (inferenceImportedTypes inputs),
      analysisForwardFunctions = forwardValues,
      analysisImportedClasses =
        Set.map
          (sourceName . mkIdentifier)
          ( Set.union
              (inferenceImportedClassNames inputs)
              (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs)))
          ),
      analysisModulePath = inferenceCurrentModulePath inputs
    }

forwardAnalysisValues :: TypedCoreProductionMode -> Map Int (Name, SourceSpan) -> Map Int (Name, AnalysisBinding)
forwardAnalysisValues mode forwardBindings
  | mode /= ProduceTypedCoreExpressionDirectCall = Map.empty
  | otherwise =
          Map.map
            (\(name, bindingSpan) -> (name, AnalysisBinding (Just bindingSpan) False))
        forwardBindings

initialStateForInference :: InferenceInputs -> InferState
initialStateForInference inputs =
  applyCapabilityFacts
    (inferenceImportedCapabilities inputs)
    initialInferState
      { inferDeclarations =
          (inferDeclarations initialInferState)
            { declarationDataTypes = inferenceImportedDataTypes inputs
            },
        inferModule =
          (inferModule initialInferState)
            { inferenceModulePath = inferenceCurrentModulePath inputs,
              inferenceRuntimeHintPath = inferenceCurrentModulePath inputs
            }
      }

moduleInterfaceFromState :: InferenceInputs -> Expr -> InferState -> ModuleInterface
moduleInterfaceFromState inputs expr state =
  emptyModuleInterface
    { interfaceValueTypes =
        Map.fromList
          [ (moduleExportForBinding (renderName name) binding, binding)
            | name <- Set.toList declaredValues,
              Just binding <- [Map.lookup name (inferVisibleTypes state)]
          ],
      interfaceDataTypes = Map.restrictKeys (inferDataTypes state) declaredDataTypes,
      interfaceClassFacts = scopeClassFacts localCapabilities,
      interfaceGeneratedEqualityClassFacts = scopeGeneratedEqualityClassFacts localCapabilities,
      interfaceConcreteImplFacts = scopeConcreteImplFacts localCapabilities,
      interfaceClassMethods = scopeClassMethodSignatures localCapabilities,
      interfaceConcreteImplMethods = scopeConcreteImplMethods localCapabilities,
      interfaceRuntimeHints = inferRuntimeTypeHints state
    }
  where
    (declaredValues, declaredDataTypes) = declaredModuleNames expr
    localCapabilities =
      case inferenceCurrentModulePath inputs of
        Just modulePath -> Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)
        Nothing -> capabilityFactsFromState state

declaredModuleNames :: Expr -> (Set Name, Set Text)
declaredModuleNames expression =
  case expression of
    EBlock statements -> foldl' collect (Set.empty, Set.empty) statements
    _ -> (Set.empty, Set.empty)
  where
    collect (valueNames, dataTypeNames) statement =
      case statement of
        SLet name _ _
          | publicModuleValue name -> (Set.insert name valueNames, dataTypeNames)
          | otherwise -> (valueNames, dataTypeNames)
        SData _ typeName _ constructors ->
          ( foldl'
              (\names (DataConstructor constructorName _) -> Set.insert constructorName names)
              valueNames
              constructors,
            Set.insert (renderName typeName) dataTypeNames
          )
        _ -> (valueNames, dataTypeNames)

    publicModuleValue name =
      case name of
        GeneratedName {} -> False
        _ -> True

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

instantiateEnvBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateEnvBinding binding state =
  case binding of
    BuiltinAliasTypeBinding builtinSymbol ->
      case instantiateBuiltinSymbolType builtinSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    _ -> instantiateNonBuiltinTypeBinding binding state

-- Core expressions do not retain inner-node source spans yet, so inference
-- reuses the enclosing statement span as the best available location metadata.
inferExprTypeWithMode ::
  Bool ->
  TypedCoreProductionMode ->
  Set Int ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (InferredExpr, InferState)
inferExprTypeWithMode allowForwardSignedFunctions mode preludeStatementIndices builtinMode env state expr =
  case expr of
    EBlock statements
      | mode == ProduceTypedCoreExpressionDirectCall,
        (failureKind, failureDetail) <- blockProductionFailureKindAndDetail statements,
        failureKind == TypedCoreStructuredValueUnsupported ->
          let (blockResult, finalState) = inferBlock mode statements
              failures =
                InferredProductionFailure [] failureKind failureDetail
                  : inferredProductionFailures blockResult
           in
            ( InferredExpr
                (inferredExpressionType blockResult)
                (Just (ProvisionalRetainedFailures failures))
                failures,
              finalState
            )
    EBlock statements ->
      inferBlock mode statements
    _ -> inferExprTypeDetailed builtinMode env state expr
  where
    inferBlock blockMode statements =
      (if allowForwardSignedFunctions then inferScopeTypeWithMode else inferNestedScopeTypeWithMode)
        preludeStatementIndices
        (\childMode childBuiltin childEnv childState childExpr ->
           inferExprTypeWithMode False childMode Set.empty childBuiltin childEnv childState childExpr
        )
        blockMode
        builtinMode
        env
        state
        statements

-- | The shared traversal infers every expression once. Production consumes the
-- provisional nodes it can lower and keeps ordered failures for the rest;
-- ordinary inference projects only the inferred type and state.
inferExprTypeDetailed ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (InferredExpr, InferState)
inferExprTypeDetailed builtinMode env state expr =
  case expr of
    ELit literal ->
      let expressionType = Just (literalExpressionType literal)
          finalState = checkLiteralType state literal
       in (InferredExpr expressionType (ProvisionalLiteralExpression literal <$> expressionType) [], finalState)
    ETuple [] ->
      (InferredExpr (Just (TTupleType [])) (Just ProvisionalUnitExpression) [], state)
    EBinary operatorSymbol leftExpr rightExpr
      | isTypedCoreDirectCallOperator operatorSymbol ->
          let (leftResult, stateAfterLeft) = inferExprTypeDetailed builtinMode env state leftExpr
              (rightResult, stateAfterRight) = inferExprTypeDetailed builtinMode env stateAfterLeft rightExpr
              (expressionType, finalState) =
                case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
                  (Just leftType, Just rightType) ->
                    inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType stateAfterRight
                  _ -> (Nothing, stateAfterRight)
              promotionFailures =
                case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
                  (Just leftType, Just rightType)
                    | Just _ <-
                        binaryNumericPromotionType
                          operatorSymbol
                          leftExpr
                          rightExpr
                          leftType
                          rightType
                          finalState ->
                        [InferredProductionFailure [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
                  _ -> []
              failures =
                promotionFailures
                  <> childFailures 0 leftResult
                  <> childFailures 1 rightResult
              provisionalExpr =
                case promotionFailures of
                  _ : _ -> Just (ProvisionalRetainedFailures failures)
                  [] -> do
                    resultType <- expressionType
                    leftType <- inferredExpressionType leftResult
                    rightType <- inferredExpressionType rightResult
                    provisionalBinaryExpression
                      finalState
                      operatorSymbol
                      resultType
                      (mergedUnifiedType finalState leftType rightType)
                      leftResult
                      rightResult
           in (InferredExpr expressionType provisionalExpr failures, finalState)
    EBinary {} ->
      inferUnsupportedWithProduction
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    EIf conditionExpr thenExpr elseExpr ->
      let (conditionResult, stateAfterCondition) =
            inferExprTypeDetailed builtinMode env state conditionExpr
          (thenResult, stateAfterThen) =
            inferExprTypeDetailed builtinMode env stateAfterCondition thenExpr
          (elseResult, stateAfterElse) =
            inferExprTypeDetailed builtinMode env stateAfterThen elseExpr
          (expressionType, finalState) =
            inferIfFromResults
              conditionResult
              thenResult
              elseResult
              stateAfterElse
          failures =
            InferredProductionFailure [] TypedCoreControlFlowUnsupported TypedCoreConditionalDetail
              : childFailures 0 conditionResult
                <> childFailures 1 thenResult
                <> childFailures 2 elseResult
       in (InferredExpr expressionType (Just (ProvisionalRetainedFailures failures)) failures, finalState)
    EPatternCase {} ->
      inferUnsupportedWithProduction
        TypedCorePatternCaseUnsupported
        TypedCorePatternCaseDetail
    EList elements ->
      let (expressionType, finalState, elementResults) = inferListWithProduction state elements
          failures =
            InferredProductionFailure [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
              : concat (zipWith childFailures [0 ..] elementResults)
       in (InferredExpr expressionType (Just (ProvisionalRetainedFailures failures)) failures, finalState)
    ETuple elements ->
      let (expressionType, finalState, elementResults) = inferTupleWithProduction state elements
          failures =
            InferredProductionFailure [] TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail
              : concat (zipWith childFailures [0 ..] elementResults)
       in (InferredExpr expressionType (Just (ProvisionalRetainedFailures failures)) failures, finalState)
    EBlock statements ->
      let (failureKind, failureDetail) =
            blockProductionFailureKindAndDetail statements
       in inferUnsupportedWithProduction failureKind failureDetail
    EVar name ->
      let (expressionType, finalState) = inferVariableType name state
       in (InferredExpr expressionType (ProvisionalVariableExpression name <$> expressionType) [], finalState)
    ELambda parameterName bodyExpr ->
      let (parameterType, stateAfterParameter) = freshTypeVar state
          extendedEnv = Map.insert parameterName (PlainTypeBinding parameterType) env
          (bodyResult, stateAfterBody) = inferExprTypeDetailed builtinMode extendedEnv stateAfterParameter bodyExpr
          expressionType =
            TFunctionType (resolveType stateAfterBody parameterType)
              <$> inferredExpressionType bodyResult
          provisionalExpr = do
            inferredType <- expressionType
            body <- inferredProvisionalExpr bodyResult
            pure (ProvisionalLambdaExpression parameterName inferredType body)
          failures = childFailures 0 bodyResult
       in (InferredExpr expressionType provisionalExpr failures, stateAfterBody)
    EOperatorValue {} ->
      inferUnsupportedWithProduction
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    EApply functionExpr argumentExpr
      | Just (operatorSymbol, maybeAliasScheme, leftOperand, rightOperand, sectionFallback) <-
          builtinOperatorApplicationSpine env expr ->
          if sectionFallback
            then
              inferSectionApplicationWithFallback
                functionExpr
                argumentExpr
                operatorSymbol
                leftOperand
                rightOperand
            else inferBuiltinOperatorApplication operatorSymbol maybeAliasScheme leftOperand rightOperand
      | Just (methodName, methodKey, argumentExprs) <- qualifiedMethodApplicationSpine expr state,
        Map.notMember methodName env ->
          let (expressionType, finalState, argumentResults) =
                inferQualifiedMethodApplicationWithResults
                  inferExprTypeDetailed
                  inferredExpressionType
                  builtinMode
                  env
                  state
                  methodKey
                  argumentExprs
              argumentFailures =
                concat
                  [ prefixFailures (applicationArgumentPath (length argumentResults) argumentIndex) argumentResult
                  | (argumentIndex, argumentResult) <- zip [0 ..] argumentResults
                  ]
           in retainedUnsupported
                expressionType
                finalState
                TypedCoreNonLocalCallUnsupported
                (TypedCoreNameDetail methodKey)
                argumentFailures
      | otherwise ->
          inferGenericApplication functionExpr argumentExpr
    ETypeApplication {} ->
      inferUnsupportedWithProduction
        TypedCoreManagedValueUnsupported
        TypedCoreUnsupportedRootDetail
    ESectionLeft {} ->
      inferUnsupportedWithProduction
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    ESectionRight {} ->
      inferUnsupportedWithProduction
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
  where
    unsupported failureKind failureDetail =
      let (expressionType, finalState) = inferUnsupportedLeafType expr state
          failures = [InferredProductionFailure [] failureKind failureDetail]
       in (InferredExpr expressionType (Just (ProvisionalUnsupportedExpression failureKind failureDetail)) failures, finalState)

    inferVariableType name initialState =
      case Map.lookup name env of
        Just localType -> instantiateEnvBinding localType initialState
        Nothing ->
          case instantiateBuiltinType builtinMode (identifierText name) initialState of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing ->
              case instantiateQualifiedMethodType (identifierText name) initialState of
                Just qualifiedMethodResult -> qualifiedMethodResult
                Nothing -> (Nothing, initialState)

    inferUnsupportedLeafType unsupportedExpr initialState =
      case unsupportedExpr of
        ELit literal -> (Just (literalExpressionType literal), checkLiteralType initialState literal)
        EVar name -> inferVariableType name initialState
        EOperatorValue operatorSymbol ->
          case instantiateOperatorType operatorSymbol initialState of
            Just (operatorType, nextState) -> (Just operatorType, nextState)
            Nothing
              | isBuiltinOperatorSymbol operatorSymbol ->
                  (Nothing, addTypeError initialState (mkUnsupportedOperatorValueError operatorSymbol))
            Nothing -> instantiateDeclaredOperatorBindingType env operatorSymbol initialState
        _ -> (Nothing, initialState)

    childFailures childIndex result =
      prefixFailures [childIndex] result

    prefixFailures prefix result =
      [ InferredProductionFailure (prefix <> childPath) kind detail
      | InferredProductionFailure childPath kind detail <- inferredProductionFailures result
      ]

    applicationArgumentPath argumentCount argumentIndex =
      replicate (argumentCount - argumentIndex - 1) 0 <> [1]

    inferBuiltinOperatorApplication operatorSymbol maybeAliasScheme (leftPath, leftExpr) (rightPath, rightExpr) =
      let (leftResult, stateAfterLeft) =
            inferExprTypeDetailed builtinMode env state leftExpr
          (rightResult, stateAfterRight) =
            inferExprTypeDetailed builtinMode env stateAfterLeft rightExpr
          (expressionType, stateAfterBinary) =
            case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
              (Just leftType, Just rightType) ->
                inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType stateAfterRight
              _ -> (Nothing, stateAfterRight)
          finalState =
            case (maybeAliasScheme, inferredExpressionType leftResult, inferredExpressionType rightResult) of
              (Just aliasScheme, Just leftType, Just rightType)
                | Just _ <- expressionType ->
                    applyOperatorAliasSchemeConstraints
                      operatorSymbol
                      aliasScheme
                      leftType
                      rightType
                      stateAfterBinary
              _ -> stateAfterBinary
          promotionFailures =
            case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
              (Just leftType, Just rightType)
                | Just _ <-
                    binaryNumericPromotionType
                      operatorSymbol
                      leftExpr
                      rightExpr
                      leftType
                      rightType
                      finalState ->
                    [InferredProductionFailure [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
              _ -> []
          childProductionFailures =
            prefixFailures leftPath leftResult <> prefixFailures rightPath rightResult
          failures = promotionFailures <> childProductionFailures
          provisionalExpr =
            case failures of
              _ : _ -> Just (ProvisionalRetainedFailures failures)
              [] -> do
                resultType <- expressionType
                leftType <- inferredExpressionType leftResult
                rightType <- inferredExpressionType rightResult
                provisionalBinaryExpression
                  finalState
                  operatorSymbol
                  resultType
                  (mergedUnifiedType finalState leftType rightType)
                  leftResult
                  rightResult
       in (InferredExpr expressionType provisionalExpr failures, finalState)

    provisionalBinaryExpression finalState operatorSymbol resultType operandType leftResult rightResult = do
      leftProvisional <-
        inferredProvisionalExpr
          (specializeInferredExpression finalState operandType leftResult)
      rightProvisional <-
        inferredProvisionalExpr
          (specializeInferredExpression finalState operandType rightResult)
      pure
        ( ProvisionalBinaryExpression
            operatorSymbol
            resultType
            operandType
            leftProvisional
            rightProvisional
        )

    inferSectionApplicationWithFallback functionExpr argumentExpr operatorSymbol leftOperand rightOperand =
      let genericResult@(inferredResult, _) = inferGenericApplication functionExpr argumentExpr
       in case inferredExpressionType inferredResult of
            Just _ -> genericResult
            Nothing ->
              let builtinResult@(builtinInferredResult, _) =
                    inferBuiltinOperatorApplication operatorSymbol Nothing leftOperand rightOperand
               in case inferredExpressionType builtinInferredResult of
                    Just _ -> builtinResult
                    Nothing -> genericResult

    inferGenericApplication functionExpr argumentExpr =
      let (functionResult, stateAfterFunction) =
            inferExprTypeDetailed builtinMode env state functionExpr
          (argumentResult, stateAfterArgument) =
            inferExprTypeDetailed builtinMode env stateAfterFunction argumentExpr
          (expressionType, finalState) =
            inferApplicationFromResults
              env
              state
              functionExpr
              argumentExpr
              functionResult
              argumentResult
              stateAfterArgument
          specializedArgumentResult =
            case (expressionType, inferredExpressionType functionResult) of
              (Just _, Just functionType) ->
                case resolveType finalState functionType of
                  TFunctionType parameterType _ ->
                    specializeInferredExpression finalState parameterType argumentResult
                  _ -> argumentResult
              _ -> argumentResult
          failures = childFailures 0 functionResult <> childFailures 1 specializedArgumentResult
          provisionalExpr =
            case failures of
              _ : _ -> Just (ProvisionalRetainedFailures failures)
              [] -> do
                resultType <- expressionType
                function <- inferredProvisionalExpr functionResult
                argument <- inferredProvisionalExpr specializedArgumentResult
                pure (ProvisionalApplyExpression resultType function argument)
       in (InferredExpr expressionType provisionalExpr failures, finalState)

    inferIfFromResults conditionResult thenResult elseResult stateAfterElse =
      let stateAfterConditionCheck =
            case inferredExpressionType conditionResult of
              Just inferredConditionType ->
                case unifyTypes inferredConditionType TBoolType stateAfterElse of
                  Just unifiedState -> unifiedState
                  Nothing ->
                    addTypeError
                      stateAfterElse
                      (mkIfConditionTypeError (resolveType stateAfterElse inferredConditionType))
              Nothing -> stateAfterElse
       in case (inferredExpressionType thenResult, inferredExpressionType elseResult) of
            (Just inferredThenType, Just inferredElseType) ->
              case unifyTypes inferredThenType inferredElseType stateAfterConditionCheck of
                Just unifiedState ->
                  (Just (mergedUnifiedType unifiedState inferredThenType inferredElseType), unifiedState)
                Nothing ->
                  ( Nothing,
                    addTypeError
                      stateAfterConditionCheck
                      ( mkIfBranchTypeMismatchError
                          (resolveType stateAfterConditionCheck inferredThenType)
                          (resolveType stateAfterConditionCheck inferredElseType)
                      )
                  )
            _ -> (Nothing, stateAfterConditionCheck)

    inferApplicationFromResults currentEnv applicationStartState functionExpr argumentExpr functionResult argumentResult stateAfterArgument =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAfterArgument
       in case (inferredExpressionType functionResult, inferredExpressionType argumentResult) of
            (Just functionType, Just argumentType) ->
              case unifyTypes functionType (TFunctionType argumentType resultTypeVar) stateWithResultVar of
                Just unifiedState ->
                  case numericConversionLiteralDiagnostic builtinMode currentEnv functionExpr argumentExpr of
                    Just diagnostic -> (Nothing, addTypeError unifiedState diagnostic)
                    Nothing -> (Just (resolveType unifiedState resultTypeVar), unifiedState)
                Nothing ->
                  ( Nothing,
                    addTypeError
                      (discardFailedFunctionApplicationConstraints applicationStartState stateWithResultVar)
                      (mkApplyTypeError (resolveType stateWithResultVar functionType) (resolveType stateWithResultVar argumentType))
                  )
            _ ->
              ( Nothing,
                discardFailedFunctionApplicationConstraints applicationStartState stateWithResultVar
              )

    inferUnsupportedBinaryWithProduction operatorSymbol leftExpr rightExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let (leftResult, stateAfterLeft) =
                inferExprTypeDetailed builtinMode env state leftExpr
              (rightResult, stateAfterRight) =
                inferExprTypeDetailed builtinMode env stateAfterLeft rightExpr
              (expressionType, finalState) =
                case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
                  (Just leftType, Just rightType) ->
                    inferBinaryType
                      operatorSymbol
                      leftExpr
                      rightExpr
                      leftType
                      rightType
                      stateAfterRight
                  _ -> (Nothing, stateAfterRight)
           in (expressionType, finalState, leftResult, rightResult)
      | otherwise =
          inferDeclaredBinaryWithProduction env state operatorSymbol leftExpr rightExpr

    inferDeclaredBinaryWithProduction currentEnv initialState operatorSymbol leftExpr rightExpr =
      let operatorExpr = EOperatorValue operatorSymbol
          (operatorType, stateAfterOperator) =
            instantiateDeclaredOperatorBindingType currentEnv operatorSymbol initialState
          operatorResult = InferredExpr operatorType Nothing []
          (leftResult, stateAfterLeft) =
            inferExprTypeDetailed builtinMode currentEnv stateAfterOperator leftExpr
          (intermediateType, stateAfterFirstApplication) =
            inferApplicationFromResults
              currentEnv
              initialState
              operatorExpr
              leftExpr
              operatorResult
              leftResult
              stateAfterLeft
          intermediateExpr = EApply operatorExpr leftExpr
          intermediateResult = InferredExpr intermediateType Nothing []
          (rightResult, stateAfterRight) =
            inferExprTypeDetailed builtinMode currentEnv stateAfterFirstApplication rightExpr
          (expressionType, finalState) =
            inferApplicationFromResults
              currentEnv
              stateAfterFirstApplication
              intermediateExpr
              rightExpr
              intermediateResult
              rightResult
              stateAfterRight
       in (expressionType, finalState, leftResult, rightResult)

    inferUnsupportedLeftSectionWithProduction operatorSymbol leftExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let (leftResult, stateAfterLeft) =
                inferExprTypeDetailed builtinMode env state leftExpr
              (expressionType, finalState) =
                case inferredExpressionType leftResult of
                  Just leftType ->
                    inferSectionLeftType operatorSymbol leftType stateAfterLeft
                  Nothing -> (Nothing, stateAfterLeft)
           in (expressionType, finalState, leftResult)
      | otherwise =
          let operatorExpr = EOperatorValue operatorSymbol
              (operatorType, stateAfterOperator) =
                instantiateDeclaredOperatorBindingType env operatorSymbol state
              operatorResult = InferredExpr operatorType Nothing []
              (leftResult, stateAfterLeft) =
                inferExprTypeDetailed builtinMode env stateAfterOperator leftExpr
              (expressionType, finalState) =
                inferApplicationFromResults
                  env
                  state
                  operatorExpr
                  leftExpr
                  operatorResult
                  leftResult
                  stateAfterLeft
           in (expressionType, finalState, leftResult)

    inferUnsupportedRightSectionWithProduction operatorSymbol rightExpr
      | hasOperatorRule operatorSymbol || isBuiltinOperatorSymbol operatorSymbol =
          let (rightResult, stateAfterRight) =
                inferExprTypeDetailed builtinMode env state rightExpr
              (expressionType, finalState) =
                case inferredExpressionType rightResult of
                  Just rightType ->
                    inferSectionRightType operatorSymbol rightType stateAfterRight
                  Nothing -> (Nothing, stateAfterRight)
           in (expressionType, finalState, rightResult)
      | otherwise =
          let (leftType, stateAfterLeftType) = freshTypeVar state
              leftName = generatedName OperatorSectionLeft
              extendedEnv =
                Map.insert leftName (PlainTypeBinding leftType) env
              (bodyType, finalState, _, rightResult) =
                inferDeclaredBinaryWithProduction
                  extendedEnv
                  stateAfterLeftType
                  operatorSymbol
                  (EVar leftName)
                  rightExpr
              expressionType =
                TFunctionType (resolveType finalState leftType)
                  <$> bodyType
           in (expressionType, finalState, rightResult)

    retainedUnsupported expressionType finalState failureKind failureDetail childProductionFailures =
      let failures =
            InferredProductionFailure [] failureKind failureDetail
              : childProductionFailures
       in ( InferredExpr
            expressionType
            (Just (ProvisionalRetainedFailures failures))
            failures,
          finalState
        )

    -- Keep this match exhaustive. Unsupported leaves intentionally retain only
    -- their root failure; every composite constructor owns an explicit
    -- production-aware child traversal.
    inferUnsupportedWithProduction failureKind failureDetail =
      case expr of
        ELit {} -> unsupported failureKind failureDetail
        EVar {} -> unsupported failureKind failureDetail
        EOperatorValue {} -> unsupported failureKind failureDetail
        ELambda parameterName bodyExpr ->
          let (parameterType, stateAfterParameter) = freshTypeVar state
              extendedEnv = Map.insert parameterName (PlainTypeBinding parameterType) env
              (bodyResult, stateAfterBody) =
                inferExprTypeDetailed builtinMode extendedEnv stateAfterParameter bodyExpr
              expressionType =
                TFunctionType (resolveType stateAfterBody parameterType)
                  <$> inferredExpressionType bodyResult
           in retainedUnsupported expressionType stateAfterBody failureKind failureDetail (childFailures 0 bodyResult)
        EList elements ->
          let (expressionType, finalState, elementResults) =
                inferListWithProduction state elements
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (concat (zipWith childFailures [0 ..] elementResults))
        ETuple elements ->
          let (expressionType, finalState, elementResults) =
                inferTupleWithProduction state elements
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (concat (zipWith childFailures [0 ..] elementResults))
        EApply functionExpr argumentExpr ->
          let (functionResult, stateAfterFunction) =
                inferExprTypeDetailed builtinMode env state functionExpr
              (argumentResult, stateAfterArgument) =
                inferExprTypeDetailed builtinMode env stateAfterFunction argumentExpr
              (expressionType, finalState) =
                inferApplicationFromResults
                  env
                  state
                  functionExpr
                  argumentExpr
                  functionResult
                  argumentResult
                  stateAfterArgument
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (childFailures 0 functionResult <> childFailures 1 argumentResult)
        ETypeApplication functionExpr typeArgumentSpan typeArgument ->
          let (expressionType, finalState, maybeFunctionResult) =
                inferExplicitTypeApplicationWithResult
                  inferExprTypeDetailed
                  builtinMode
                  env
                  state
                  functionExpr
                  typeArgumentSpan
                  typeArgument
              functionFailures =
                maybe [] (childFailures 0) maybeFunctionResult
           in retainedUnsupported expressionType finalState failureKind failureDetail functionFailures
        EIf conditionExpr thenExpr elseExpr ->
          let (conditionResult, stateAfterCondition) =
                inferExprTypeDetailed builtinMode env state conditionExpr
              (thenResult, stateAfterThen) =
                inferExprTypeDetailed builtinMode env stateAfterCondition thenExpr
              (elseResult, finalState) =
                inferExprTypeDetailed builtinMode env stateAfterThen elseExpr
              (expressionType, checkedState) =
                inferIfFromResults conditionResult thenResult elseResult finalState
           in retainedUnsupported
              expressionType
              checkedState
              failureKind
              failureDetail
              ( childFailures 0 conditionResult
                  <> childFailures 1 thenResult
                  <> childFailures 2 elseResult
              )
        EPatternCase scrutineeExpr caseArms ->
          let (scrutineeResult, stateAfterScrutinee) =
                inferExprTypeDetailed builtinMode env state scrutineeExpr
              (scrutineeType, stateWithScrutineeType) =
                case inferredExpressionType scrutineeResult of
                  Just inferredScrutineeType ->
                    (inferredScrutineeType, stateAfterScrutinee)
                  Nothing ->
                    freshTypeVar stateAfterScrutinee
              (expressionType, finalState, armResults) =
                inferPatternCaseTypeWithResults
                  inferExprTypeDetailed
                  builtinMode
                  env
                  scrutineeType
                  stateWithScrutineeType
                  caseArms
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              ( childFailures 0 scrutineeResult
                  <> concat (zipWith childFailures [1 ..] armResults)
              )
        EBinary operatorSymbol leftExpr rightExpr ->
          let (expressionType, finalState, leftResult, rightResult) =
                inferUnsupportedBinaryWithProduction operatorSymbol leftExpr rightExpr
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (childFailures 0 leftResult <> childFailures 1 rightResult)
        ESectionLeft leftExpr operatorSymbol ->
          let (expressionType, finalState, leftResult) =
                inferUnsupportedLeftSectionWithProduction operatorSymbol leftExpr
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (childFailures 0 leftResult)
        ESectionRight operatorSymbol rightExpr ->
          let (expressionType, finalState, rightResult) =
                inferUnsupportedRightSectionWithProduction operatorSymbol rightExpr
           in retainedUnsupported
              expressionType
              finalState
              failureKind
              failureDetail
              (childFailures 0 rightResult)
        EBlock statements ->
          let (blockResult, finalState) =
                inferNestedScopeTypeWithMode
                  Set.empty
                  ( \childMode childBuiltin childEnv childState childExpr ->
                      inferExprTypeWithMode
                        False
                        childMode
                        Set.empty
                        childBuiltin
                        childEnv
                        childState
                        childExpr
                  )
                  ProduceTypedCoreExpressionDirectCall
                  builtinMode
                  env
                  state
                  statements
           in retainedUnsupported
              (inferredExpressionType blockResult)
              finalState
              failureKind
              failureDetail
              (inferredProductionFailures blockResult)

    inferListWithProduction initialState elements =
      case elements of
        [] ->
          let (elementType, finalState) = freshTypeVar initialState
           in (Just (TListType elementType), finalState, [])
        firstElement : restElements ->
          let (firstResult, stateAfterFirst) =
                inferExprTypeDetailed builtinMode env initialState firstElement
              (finalElementType, finalState, reversedResults) =
                foldl'
                  inferNextListElement
                  (inferredExpressionType firstResult, stateAfterFirst, [firstResult])
                  restElements
           in (TListType <$> finalElementType, finalState, reverse reversedResults)

    inferNextListElement (expectedType, stateAcc, reversedResults) element =
      let (actualResult, stateAfterElement) =
            inferExprTypeDetailed builtinMode env stateAcc element
          actualType = inferredExpressionType actualResult
          (nextExpectedType, finalState) =
            case (expectedType, actualType) of
              (Just inferredExpectedType, Just inferredActualType) ->
                case unifyTypes inferredExpectedType inferredActualType stateAfterElement of
                  Just unifiedState ->
                    ( Just
                        ( mergeIntegerLiteralRanges
                            (resolveType unifiedState inferredExpectedType)
                            (resolveType unifiedState inferredActualType)
                        ),
                      unifiedState
                    )
                  Nothing ->
                    ( Just inferredExpectedType,
                      addTypeError
                        stateAfterElement
                        ( mkListElementTypeMismatchError
                            (resolveType stateAfterElement inferredExpectedType)
                            (resolveType stateAfterElement inferredActualType)
                        )
                    )
              _ -> (expectedType, stateAfterElement)
       in (nextExpectedType, finalState, actualResult : reversedResults)

    inferTupleWithProduction initialState elements =
      goTuple (Just []) initialState [] elements
      where
        goTuple maybeReversedTypes stateAcc reversedResults remainingElements =
          case remainingElements of
            [] ->
              (TTupleType . reverse <$> maybeReversedTypes, stateAcc, reverse reversedResults)
            element : rest ->
              let (elementResult, stateAfterElement) =
                    inferExprTypeDetailed builtinMode env stateAcc element
                  nextReversedTypes =
                    case (maybeReversedTypes, inferredExpressionType elementResult) of
                      (Just reversedTypes, Just inferredElementType) ->
                        Just (resolveType stateAfterElement inferredElementType : reversedTypes)
                      _ -> Nothing
               in goTuple nextReversedTypes stateAfterElement (elementResult : reversedResults) rest

discardFailedFunctionApplicationConstraints :: InferState -> InferState -> InferState
discardFailedFunctionApplicationConstraints stateBeforeFunction stateAfterApplication =
  modifyInferenceOutput
    ( \output ->
        output
          { outputDeferredConstraints =
              outputDeferredConstraints (inferOutput stateBeforeFunction),
            outputDeferredConstraintCount =
              outputDeferredConstraintCount (inferOutput stateBeforeFunction),
            outputInferredConstraints =
              outputInferredConstraints (inferOutput stateBeforeFunction),
            outputInferredConstraintCount =
              outputInferredConstraintCount (inferOutput stateBeforeFunction)
          }
    )
    stateAfterApplication

qualifiedMethodApplicationSpine :: Expr -> InferState -> Maybe (Name, Text, [Expr])
qualifiedMethodApplicationSpine expr state =
  case applicationSpine expr of
    Just (methodName, argumentExprs)
      | let methodKey = identifierText methodName,
        qualifiedMethodClassIsVisible methodKey state ->
          Just (methodName, methodKey, argumentExprs)
    _ -> Nothing

applicationSpine :: Expr -> Maybe (Name, [Expr])
applicationSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply (EOperatorValue "$") functionExpr ->
          go argumentExprs functionExpr
        EApply functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar name ->
          Just (name, argumentExprs)
        _ ->
          Nothing

builtinOperatorApplicationSpine ::
  TypeEnv ->
  Expr ->
  Maybe (Text, Maybe TypeScheme, ([Int], Expr), ([Int], Expr), Bool)
builtinOperatorApplicationSpine env expr =
  case expr of
    EApply (EApply dollarExpr sectionExpr) argumentExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          case sectionExpr of
            ESectionLeft leftExpr operatorSymbol
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([0, 1, 0], leftExpr), ([1], argumentExpr), True)
            ESectionRight operatorSymbol rightExpr
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([1], argumentExpr), ([0, 1, 0], rightExpr), True)
            _ -> Nothing
    EApply (ESectionLeft leftExpr operatorSymbol) rightExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([0, 0], leftExpr), ([1], rightExpr), True)
    EApply (ESectionRight operatorSymbol rightExpr) leftExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([1], leftExpr), ([0, 0], rightExpr), True)
    EApply (EApply operatorExpr leftExpr) rightExpr -> do
      (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr env operatorExpr
      if hasOperatorRule operatorSymbol
        then Just (operatorSymbol, maybeAliasScheme, ([0, 1], leftExpr), ([1], rightExpr), False)
        else Nothing
    _ -> Nothing

builtinOperatorSymbolExpr :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme)
builtinOperatorSymbolExpr env expr =
  case expr of
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing)
    EApply dollarExpr operatorExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSymbolExpr env operatorExpr
    EVar name ->
      case Map.lookup name env of
        Just (BuiltinOperatorAliasTypeBinding operatorSymbol) -> Just (operatorSymbol, Nothing)
        Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) -> Just (operatorSymbol, Just typeScheme)
        _ -> Nothing
    _ -> Nothing

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt value -> TIntegerLiteralType (singletonIntegerLiteralRange value)
    LFloat _ _ maybeTargetType ->
      case maybeTargetType of
        Just targetType -> TNumericType targetType
        Nothing -> TFloatType
    LBool _ -> TBoolType
    LChar _ -> TCharType
    LText _ -> TTextType

checkLiteralType :: InferState -> Literal -> InferState
checkLiteralType state literal =
  case literal of
    LFloat literalValue literalSource (Just targetType) ->
      case targetedFloatLiteralDiagnostic targetType literalValue literalSource of
        Just diagnostic -> addTypeError state diagnostic
        Nothing -> state
    _ -> state

numericConversionLiteralDiagnostic :: BuiltinResolutionMode -> TypeEnv -> Expr -> Expr -> Maybe Diagnostic
numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr =
  case (functionExpr, argumentExpr) of
    (EVar functionName, ELit (LInt literalValue)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          case numericTypeLiteralIntegerBounds targetType of
            Just bounds@(lowerBound, upperBound)
              | literalValue < lowerBound || literalValue > upperBound ->
                  Just (mkNumericConversionLiteralTypeError (identifierText functionName) literalValue targetType bounds)
            _ -> Nothing
        Nothing -> Nothing
    (EVar functionName, ELit (LFloat literalValue literalSource _)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          numericConversionFloatLiteralDiagnostic
            (identifierText functionName)
            targetType
            literalValue
            literalSource
        Nothing -> Nothing
    _ -> Nothing

numericConversionFloatLiteralDiagnostic :: Text -> NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
numericConversionFloatLiteralDiagnostic conversionName targetType literalValue literalSource =
  case numericTypeIntegerBounds targetType of
    Just bounds@(lowerBound, upperBound) ->
      case fractionalLiteralIntegralValue literalSource of
        Just integralValue
          | finiteFloat literalValue,
            integralValue >= lowerBound,
            integralValue <= upperBound ->
              Nothing
        _ ->
          Just (mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType bounds)
    Nothing ->
      case numericTypeFloatMax targetType of
        Just maxMagnitude
          | not (finiteFloat literalValue)
              || abs literalValue > maxMagnitude
              || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
              Just (mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude)
        _ -> Nothing

targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
targetedFloatLiteralDiagnostic targetType literalValue literalSource =
  case numericTypeFloatMax targetType of
    Just maxMagnitude
      | not (finiteFloat literalValue)
          || abs literalValue > maxMagnitude
          || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
          Just (mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude)
    _ -> Nothing

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

numericConversionTargetFromCallable :: BuiltinResolutionMode -> TypeEnv -> Name -> Maybe NumericType
numericConversionTargetFromCallable builtinMode env functionName =
  let nameText = identifierText functionName
   in case Map.lookup functionName env of
        Just (BuiltinAliasTypeBinding builtinSymbol) ->
          builtinSymbolNumericConversionTarget builtinSymbol
        Just _ ->
          Nothing
        Nothing ->
          lookupBuiltinSymbolInMode builtinMode nameText >>= builtinSymbolNumericConversionTarget

singletonIntegerLiteralRange :: Integer -> IntegerLiteralRange
singletonIntegerLiteralRange value = IntegerLiteralRange value value

mergedUnifiedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
mergedUnifiedType state leftType rightType =
  mergeIntegerLiteralRanges (resolveType state leftType) (resolveType state rightType)

mergeIntegerLiteralRanges :: ExpressionType -> ExpressionType -> ExpressionType
mergeIntegerLiteralRanges leftType rightType =
  case (leftType, rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (combineIntegerLiteralRanges leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (TListType leftElementType, TListType rightElementType) ->
      TListType (mergeIntegerLiteralRanges leftElementType rightElementType)
    (TTupleType leftElementTypes, TTupleType rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          TTupleType (zipWith mergeIntegerLiteralRanges leftElementTypes rightElementTypes)
    (TDataType leftName leftArguments, TDataType rightName rightArguments)
      | leftName == rightName,
        length leftArguments == length rightArguments ->
          TDataType leftName (zipWith mergeIntegerLiteralRanges leftArguments rightArguments)
    (TFunctionType leftInputType leftOutputType, TFunctionType rightInputType rightOutputType) ->
      TFunctionType
        (mergeIntegerLiteralRanges leftInputType rightInputType)
        (mergeIntegerLiteralRanges leftOutputType rightOutputType)
    _ -> leftType

instantiateBuiltinType :: BuiltinResolutionMode -> Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinType builtinMode name state =
  case lookupBuiltinSymbolInMode builtinMode name of
    Just builtinSymbol -> instantiateBuiltinSymbolType builtinSymbol state
    Nothing -> Nothing

instantiateDeclaredOperatorBindingType :: TypeEnv -> Text -> InferState -> (Maybe ExpressionType, InferState)
instantiateDeclaredOperatorBindingType env operatorSymbol state =
  case Map.lookup (operatorBindingName operatorSymbol) env of
    Just binding ->
      instantiateEnvBinding binding state
    Nothing ->
      ( Nothing,
        addTypeError state (mkMissingOperatorBindingError operatorSymbol)
      )

-- | Instantiate builtin symbol types on demand so each use site gets fresh type
-- variables instead of sharing one global schematic type.
instantiateBuiltinSymbolType :: BuiltinSymbol -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolType builtinSymbol state =
  -- Use catalog names here so newly-added symbols safely fall back to `Nothing`
  -- until an explicit type-instantiation rule is defined.
  case builtinSymbolNumericConversionTarget builtinSymbol of
    Just targetType ->
      let (sourceTypeVar, sourceType, stateAfterSourceType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint sourceTypeVar AnyNumericConstraint stateAfterSourceType
       in Just (TFunctionType sourceType (TNumericType targetType), stateAfterNumericConstraint)
    Nothing ->
      instantiateBuiltinSymbolTypeByName (builtinSymbolName builtinSymbol) state

instantiateBuiltinSymbolTypeByName :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolTypeByName builtinName state =
  case builtinName of
    "hd" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) elementType, stateAfterElement)
    "tl" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    "map" ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in Just
          ( TFunctionType
              (TFunctionType sourceType targetType)
              (TFunctionType (TListType sourceType) (TListType targetType)),
            stateAfterTarget
          )
    "filter" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
          ( TFunctionType
              (TFunctionType elementType TBoolType)
              (TFunctionType (TListType elementType) (TListType elementType)),
            stateAfterElement
          )
    "print!" ->
      -- Stub-v1 runtime keeps `print!` as an impure primitive that returns the
      -- evaluated argument value unchanged so compile/runtime paths stay simple.
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (TFunctionType valueType valueType, stateAfterValueType)
    "listPrependRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
          ( TFunctionType
              elementType
              (TFunctionType (TListType elementType) (TListType elementType)),
            stateAfterElement
          )
    "listReverseRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    "charToUInt32" ->
      Just (TFunctionType TCharType (TNumericType NumericUInt32), state)
    "charFromUInt32Raw" ->
      Just (TFunctionType (TNumericType NumericUInt32) (TListType TCharType), state)
    "charIsAlpha" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsAlphaNum" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsDigit" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsSpace" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsHexDigit" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsLower" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charIsUpper" ->
      Just (TFunctionType TCharType TBoolType, state)
    "charToLower" ->
      Just (TFunctionType TCharType TCharType, state)
    "charToUpper" ->
      Just (TFunctionType TCharType TCharType, state)
    "textLength" ->
      Just (TFunctionType TTextType TIntType, state)
    "textUnconsRaw" ->
      Just
        ( TFunctionType
            TTextType
            (TListType (TTupleType [TCharType, TTextType])),
          state
        )
    "textAppend" ->
      Just (TFunctionType TTextType (TFunctionType TTextType TTextType), state)
    "textAppendChar" ->
      Just (TFunctionType TTextType (TFunctionType TCharType TTextType), state)
    "textFromChars" ->
      Just (TFunctionType (TListType TCharType) TTextType, state)
    "textConcat" ->
      Just (TFunctionType (TListType TTextType) TTextType, state)
    "renderValue" ->
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (TFunctionType valueType TTextType, stateAfterValueType)
    "readTextRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "writeTextRaw!" ->
      Just
        ( TFunctionType
            TTextType
            (TFunctionType TTextType hostIOOutcomeType),
          state
        )
    "readStdinRaw!" ->
      Just (TFunctionType unitType hostIOOutcomeType, state)
    "writeStdoutRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "writeStderrRaw!" ->
      Just (TFunctionType TTextType hostIOOutcomeType, state)
    "arguments!" ->
      Just (TFunctionType unitType (TListType TTextType), state)
    "exit!" ->
      Just (TFunctionType TIntType unitType, state)
    _ -> Nothing

hostIOOutcomeType :: ExpressionType
hostIOOutcomeType = TTupleType [TBoolType, TTextType, TTextType, TTextType]

unitType :: ExpressionType
unitType = TTupleType []
