{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionResult,
    typedCoreProductionInferenceResult,
    typedCoreProductionBuildResult,
    typedCoreProductionValidatedProgram,
    inferResolvedModuleTypedCoreExpressionDirectCall,
    analyzeSourceUnitExpressionWithBuiltins,
    inferExpressionWithBuiltins,
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements,
    analyzeExpressionWithInputs,
    inferExpressionDefault,
  )
where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode,
    CorePhase (..),
    CoreSort (ExpressionSort),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..),
    coreNodeId,
  )
import Jazz.Compiler.Analyzer
  ( AnalysisBinding (..),
    AnalysisInputs (..),
    AnalysisResult (..),
    analyzeProgramWithInputs,
    analyzeProgramWithInputsAndPreparedScope,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (BuiltinListPrependRaw),
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
import Jazz.Compiler.Diagnostics.Strictness (forceDiagnostic)
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity (ModulePath, modulePathTextSegments, standaloneModulePath)
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    emptyModuleInterface,
    moduleExportForBinding,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (CapabilityNamespace, ValueNamespace),
    ResolvedName,
    UnresolvedName,
    identifierText,
    mkIdentifier,
    operatorBindingName,
    renderName,
    resolvedAmbientName,
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.PatternCoverage
  ( PatternCoverageFailure (..),
    PatternCoverageSite (..),
    analyzePatternCoverage,
    constructorInventoryFromBindingsWithWitnessNames,
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    prepareRecursiveScope,
    preparedRecursiveScopeStatements,
  )
import Jazz.Compiler.SemanticFacts
  ( BinaryOperandTyping (..),
    BinaryOperation (..),
    CoreBinderId,
    CoreNodeId,
    SemanticFactInvariantFailure,
    StatementDeclarationFact,
    StatementFacts,
  )
import Jazz.Compiler.TypeInference.Analyzed
  ( attachAnalyzedExpression,
    attachAnalyzedSourceUnitExpression,
    attachAnalyzedStatementFacts,
  )
import Jazz.Compiler.TypeInference.Capabilities
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Elaboration
  ( isTypedCoreDirectCallOperator,
    specializeInferredExpression,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    TypedCoreBuildResult (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    TypedCoreProductionPath (..),
    blockProductionFailureKindAndDetail,
    typedCoreBuildValidatedProgram,
  )
import Jazz.Compiler.TypeInference.Evidence (implementationEvidenceCandidatesInModule, implementationEvidenceCandidatesInSourceUnit)
import Jazz.Compiler.TypeInference.Operator
  ( applyOperatorAliasSchemeConstraints,
    builtinSectionOperatorSymbol,
    hasOperatorRule,
    inferBinaryType,
    inferSectionLeftType,
    inferSectionRightType,
    instantiateOperatorType,
  )
import Jazz.Compiler.TypeInference.Pattern
  ( InferredPatternCaseArm (..),
    inferPatternCaseTypeWithResults,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.Scope
  ( inferExplicitTypeApplicationWithResult,
    inferNestedScopeTypeWithMode,
    inferScopeTypeWithMode,
    inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope,
    instantiateNonBuiltinTypeBinding,
  )
import Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    freshIntegerLiteralType,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeFitsNumericType,
    integerLiteralRangeFor,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( DeclarationState (..),
    ImplementationEvidenceCandidate,
    InferState (..),
    InferenceOutput (..),
    ModuleInferenceState (..),
    inferConstructorWitnessNames,
    inferDataTypes,
    inferErrorsRev,
    inferExpressionFactTypes,
    inferModuleCapabilityFacts,
    inferPatternCoverageSites,
    inferVisibleTypes,
    initialInferState,
    modifyInferenceOutput,
    modifyModuleInferenceState,
    recordBinaryOperation,
    recordExpressionFactType,
    recordPatternCoverageSite,
    recordStatementFactSeed,
    reservePatternCoverageSite,
  )
import Jazz.Compiler.TypeInference.TypeOps (mergedUnifiedType)
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ExpressionType,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))
import Jazz.Compiler.TypedCore (TypedSourcePath, validTypedSourcePath)
import Jazz.Compiler.TypedCore.Build (buildTypedProgram)
import Jazz.Compiler.TypedCore.Validate (ValidatedTypedProgram)
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings,
  )

data InferenceInputs = InferenceInputs
  { inferenceBuiltinMode :: BuiltinResolutionMode,
    inferenceWarningSettings :: WarningSettings,
    inferenceImportedTypes :: TypeEnv,
    inferenceImportedDataTypes :: Map Text DataTypeBinding,
    inferenceImportedConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    inferenceImportedCapabilities :: ScopeCapabilityFacts,
    inferenceImportedClassNames :: Set Text,
    inferenceCurrentModulePath :: Maybe [Text]
  }

data InferenceRequest = InferenceRequest
  { requestedInferenceInputs :: InferenceInputs,
    requestedHiddenStatementIndices :: Set Int,
    requestedPreludeStatementIndices :: Set Int,
    requestedModuleStatementFacts :: [(CoreNodeId, StatementDeclarationFact)],
    requestedImplementationEvidenceCandidates :: Map Text [ImplementationEvidenceCandidate]
  }

-- | The constructor is private so callers can observe, but cannot rewrite, the
-- inference result and its proof-carrying production outcome independently.
data TypedCoreProductionResult = TypedCoreProductionResult InferenceResult TypedCoreBuildResult
  deriving (Eq, Show)

typedCoreProductionInferenceResult :: TypedCoreProductionResult -> InferenceResult
typedCoreProductionInferenceResult (TypedCoreProductionResult inferenceResult _) = inferenceResult

typedCoreProductionBuildResult :: TypedCoreProductionResult -> TypedCoreBuildResult
typedCoreProductionBuildResult (TypedCoreProductionResult _ outcome) = outcome

typedCoreProductionValidatedProgram :: TypedCoreProductionResult -> Maybe ValidatedTypedProgram
typedCoreProductionValidatedProgram (TypedCoreProductionResult _ outcome) =
  typedCoreBuildValidatedProgram outcome

inferExpressionWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithBuiltins builtinMode settings =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = emptyInferenceInputs builtinMode settings,
        requestedHiddenStatementIndices = Set.empty,
        requestedPreludeStatementIndices = Set.empty,
        requestedModuleStatementFacts = [],
        requestedImplementationEvidenceCandidates = Map.empty
      }

analyzeSourceUnitExpressionWithBuiltins ::
  BuiltinResolutionMode ->
  ModulePath ->
  Set Int ->
  Set Int ->
  WarningSettings ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed))
    )
analyzeSourceUnitExpressionWithBuiltins builtinMode preludePath hiddenStatementIndices preludeStatementIndices settings expression = do
  (inference, finalState) <-
    inferExpressionWithRequestAndState
      InferenceRequest
        { requestedInferenceInputs = emptyInferenceInputs builtinMode settings,
          requestedHiddenStatementIndices = hiddenStatementIndices,
          requestedPreludeStatementIndices = preludeStatementIndices,
          requestedModuleStatementFacts = [],
          requestedImplementationEvidenceCandidates =
            implementationEvidenceCandidatesInSourceUnit
              standaloneModulePath
              preludePath
              preludeStatementIndices
              expression
        }
      expression
  if any isErrorDiagnostic (inferredDiagnostics inference)
    then pure (inference, Right Nothing)
    else
      pure
        ( inference,
          Just
            <$> attachAnalyzedSourceUnitExpression
              standaloneModulePath
              preludePath
              preludeStatementIndices
              finalState
              (inferredExpr inference)
        )

inferExpressionWithInputs :: InferenceInputs -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithInputs inputs =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = inputs,
        requestedHiddenStatementIndices = Set.empty,
        requestedPreludeStatementIndices = Set.empty,
        requestedModuleStatementFacts = [],
        requestedImplementationEvidenceCandidates = Map.empty
      }

inferExpressionWithInputsAndHiddenStatements :: InferenceInputs -> Set Int -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithInputsAndHiddenStatements inputs hiddenStatementIndices =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = inputs,
        requestedHiddenStatementIndices = hiddenStatementIndices,
        requestedPreludeStatementIndices = hiddenStatementIndices,
        requestedModuleStatementFacts = [],
        requestedImplementationEvidenceCandidates = Map.empty
      }

inferExpressionWithRequest :: InferenceRequest -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithRequest request expr = fst <$> inferExpressionWithRequestAndState request expr

inferExpressionWithRequestAndState :: InferenceRequest -> Expr 'Resolved -> IO (InferenceResult, InferState)
inferExpressionWithRequestAndState request expr =
  {-# SCC "jazz-stage:type-inference" #-}
  let inputs = requestedInferenceInputs request
      (inferredResult, finalState, forwardBindings, inferenceSubject) =
        inferExpressionWork
          InferenceOnly
          inputs
          (requestedModuleStatementFacts request)
          (requestedImplementationEvidenceCandidates request)
          expr
      expression = inferenceSubjectExpr inferenceSubject
      finalizedInference = finalizeInferenceState inputs expression finalState
   in expression `seq`
        forceFinalizedInferenceContainers finalizedInference `seq`
          do
            inference <-
              finishInference
                InferenceOnly
                inputs
                (requestedHiddenStatementIndices request)
                inferenceSubject
                inferredResult
                forwardBindings
                finalizedInference
            pure (inference, finalState)

analyzeExpressionWithInputs ::
  ModulePath ->
  [(CoreNodeId, StatementDeclarationFact)] ->
  Map ResolvedName CoreBinderId ->
  Map Text [ImplementationEvidenceCandidate] ->
  InferenceInputs ->
  Set Int ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed, Map CoreNodeId StatementFacts))
    )
analyzeExpressionWithInputs modulePath moduleStatementFacts importedBinders evidenceCandidates inputs hiddenStatementIndices expression = do
  (inference, finalState) <-
    inferExpressionWithRequestAndState
      InferenceRequest
        { requestedInferenceInputs = inputs,
          requestedHiddenStatementIndices = hiddenStatementIndices,
          requestedPreludeStatementIndices = hiddenStatementIndices,
          requestedModuleStatementFacts = moduleStatementFacts,
          requestedImplementationEvidenceCandidates = evidenceCandidates
        }
      expression
  if any isErrorDiagnostic (inferredDiagnostics inference)
    then pure (inference, Right Nothing)
    else
      pure
        ( inference,
          Just
            <$> ( (,)
                    <$> attachAnalyzedExpression modulePath importedBinders finalState (inferredExpr inference)
                    <*> attachAnalyzedStatementFacts modulePath finalState (map fst moduleStatementFacts)
                )
        )

data InferenceSubject
  = InferenceExpression (Expr 'Resolved)
  | InferencePreparedScope (Expr 'Resolved) (PreparedRecursiveScope 'Resolved)

inferenceSubjectExpr :: InferenceSubject -> Expr 'Resolved
inferenceSubjectExpr subject =
  case subject of
    InferenceExpression expr -> expr
    InferencePreparedScope expr preparedScope ->
      preparedRecursiveScopeStatements preparedScope `seq` expr

inferExpressionWork :: TypedCoreProductionMode -> InferenceInputs -> [(CoreNodeId, StatementDeclarationFact)] -> Map Text [ImplementationEvidenceCandidate] -> Expr 'Resolved -> (InferredExpr, InferState, Map Int (ResolvedName, SourceSpan), InferenceSubject)
inferExpressionWork mode inputs moduleStatementFacts evidenceCandidates expr =
  let initialState =
        foldl'
          (\state (nodeId, declarationFact) -> recordStatementFactSeed nodeId ([], declarationFact) state)
          ( modifyModuleInferenceState
              (\moduleState -> moduleState {inferenceImplementationEvidenceCandidates = evidenceCandidates})
              (initialStateForInference inputs)
          )
          moduleStatementFacts
   in case expr of
        EBlock _ statements ->
          let preparedScope =
                prepareRecursiveScope
                  ( Set.union
                      (Map.keysSet (inferenceImportedTypes inputs))
                      (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) (builtinNamesInMode (inferenceBuiltinMode inputs)))
                  )
                  statements
              (blockResult, rawBlockState, bindings) =
                inferScopeTypeWithModeAndForwardBindingsUsingPreparedScope
                  preparedScope
                  ( \childMode childBuiltin childEnv childState childExpr ->
                      inferExprTypeWithMode False childMode childBuiltin childEnv childState childExpr
                  )
                  mode
                  (inferenceBuiltinMode inputs)
                  (inferenceImportedTypes inputs)
                  initialState
              blockState =
                recordExpressionFactType
                  (coreNodeId (expressionNode expr))
                  ( case inferredExpressionType blockResult of
                      Just expressionType -> expressionType
                      Nothing -> unitType
                  )
                  rawBlockState
           in (blockResult, blockState, bindings, InferencePreparedScope expr preparedScope)
        _ ->
          let (result, resultState) =
                inferExprTypeWithMode
                  True
                  mode
                  (inferenceBuiltinMode inputs)
                  (inferenceImportedTypes inputs)
                  initialState
                  expr
           in (result, resultState, Map.empty, InferenceExpression expr)

data FinalizedInference = FinalizedInference
  { finalizedTypeErrors :: [Diagnostic],
    finalizedPatternCoverageDiagnostics :: [Diagnostic],
    finalizedModuleInterface :: ModuleInterface
  }

finalizeInferenceState :: InferenceInputs -> Expr 'Resolved -> InferState -> FinalizedInference
finalizeInferenceState inputs expr finalState =
  FinalizedInference
    { finalizedTypeErrors = reverse (inferErrorsRev finalState),
      finalizedPatternCoverageDiagnostics =
        concatMap
          (patternCoverageDiagnostics finalState)
          (sortOn patternCoverageSiteOrdinal (inferPatternCoverageSites finalState)),
      finalizedModuleInterface = moduleInterfaceFromState inputs expr finalState
    }

finishInference :: TypedCoreProductionMode -> InferenceInputs -> Set Int -> InferenceSubject -> InferredExpr -> Map Int (ResolvedName, SourceSpan) -> FinalizedInference -> IO InferenceResult
finishInference mode inputs hiddenStatementIndices subject inferredResult forwardBindings finalizedInference = do
  let expression = inferenceSubjectExpr subject
  AnalysisResult _ analyzerDiagnostics <-
    case subject of
      InferencePreparedScope _ preparedScope ->
        analyzeProgramWithInputsAndPreparedScope
          (analysisInputsForInference inputs (forwardAnalysisValues mode forwardBindings))
          hiddenStatementIndices
          expression
          preparedScope
      InferenceExpression expr ->
        analyzeProgramWithInputs
          (analysisInputsForInference inputs (forwardAnalysisValues mode forwardBindings))
          hiddenStatementIndices
          expr
  let baseDiagnostics = analyzerDiagnostics <> finalizedTypeErrors finalizedInference
      coverageDiagnostics
        | any isErrorDiagnostic baseDiagnostics = []
        | otherwise = finalizedPatternCoverageDiagnostics finalizedInference
      diagnostics = baseDiagnostics <> coverageDiagnostics
  expression `seq`
    inferredExpressionType inferredResult `seq`
      pure
        InferenceResult
          { inferredExpr = expression,
            inferredDiagnostics = diagnostics,
            inferredModuleInterface = finalizedModuleInterface finalizedInference
          }

-- Ordinary inference owns the finalized diagnostics before the analyzer walk,
-- so rendering thunks cannot keep the complete solver state alive. The
-- remaining result containers are materialized only to WHNF; the Typed Core
-- producer skips this boundary because its finalizer still needs that state.
forceFinalizedInferenceContainers :: FinalizedInference -> ()
forceFinalizedInferenceContainers finalizedInference =
  forceListWith forceDiagnostic (finalizedTypeErrors finalizedInference) `seq`
    forceListWith forceDiagnostic (finalizedPatternCoverageDiagnostics finalizedInference) `seq`
      forceModuleInterfaceContainers (finalizedModuleInterface finalizedInference)

patternCoverageDiagnostics :: InferState -> PatternCoverageSite -> [Diagnostic]
patternCoverageDiagnostics finalState site =
  map
    coverageFailureDiagnostic
    ( analyzePatternCoverage
        (patternCoverageSiteConstructorInventory site)
        (resolveType finalState (patternCoverageSiteScrutineeType site))
        (patternCoverageSiteArms site)
    )

coverageFailureDiagnostic :: PatternCoverageFailure -> Diagnostic
coverageFailureDiagnostic failure =
  case failure of
    NonExhaustivePattern missingPattern ->
      mkNonExhaustivePatternMatchError missingPattern
    UnreachablePatternArm armIndex ->
      mkUnreachablePatternArmError armIndex

forceModuleInterfaceContainers :: ModuleInterface -> ()
forceModuleInterfaceContainers moduleInterface =
  forceMapEntriesWhnf (interfaceValueTypes moduleInterface) `seq`
    forceMapEntriesWhnf (interfaceDataTypes moduleInterface) `seq`
      forceMapEntriesWhnf (interfaceClassFacts moduleInterface) `seq`
        forceSetEntriesWhnf (interfaceGeneratedEqualityClassFacts moduleInterface) `seq`
          forceSetEntriesWhnf (interfaceConcreteImplFacts moduleInterface) `seq`
            forceMapEntriesWhnf (interfaceClassMethods moduleInterface) `seq`
              forceMapEntriesWhnf (interfaceConcreteImplMethods moduleInterface)

forceMapEntriesWhnf :: Map key value -> ()
forceMapEntriesWhnf = Map.foldrWithKey (\key value forced -> key `seq` value `seq` forced) ()

forceSetEntriesWhnf :: Set value -> ()
forceSetEntriesWhnf = Set.foldr (\value forced -> value `seq` forced) ()

forceListWith :: (value -> ()) -> [value] -> ()
forceListWith forceValue values =
  case values of
    [] -> ()
    value : remaining -> forceValue value `seq` forceListWith forceValue remaining

inferResolvedModuleTypedCoreExpressionDirectCall ::
  InferenceInputs ->
  TypedSourcePath ->
  ModuleGraph.CoreModule 'Resolved ->
  IO TypedCoreProductionResult
inferResolvedModuleTypedCoreExpressionDirectCall inputs sourcePath resolvedModule =
  {-# SCC "jazz-stage:type-inference" #-}
  do
    let sourceExpression = ModuleGraph.coreModuleExpr resolvedModule
        (inferredResult, finalState, forwardBindings, inferenceSubject) =
          inferExpressionWork ProduceTypedCoreExpressionDirectCall inputs [] (implementationEvidenceCandidatesInModule (ModuleGraph.coreModulePath resolvedModule) sourceExpression) sourceExpression
        expression = inferenceSubjectExpr inferenceSubject
        finalizedInference = finalizeInferenceState inputs expression finalState
    expression `seq` pure ()
    inferenceResult <-
      finishInference
        ProduceTypedCoreExpressionDirectCall
        inputs
        Set.empty
        inferenceSubject
        inferredResult
        forwardBindings
        finalizedInference
    outcome <- productionOutcome inputs sourcePath resolvedModule finalState inferenceResult
    pure (TypedCoreProductionResult inferenceResult outcome)

productionOutcome :: InferenceInputs -> TypedSourcePath -> ModuleGraph.CoreModule 'Resolved -> InferState -> InferenceResult -> IO TypedCoreBuildResult
productionOutcome inputs sourcePath resolvedModule finalState inferenceResult
  | any isErrorDiagnostic (inferredDiagnostics inferenceResult) = pure TypedCoreProductionBlockedByDiagnostics
  | otherwise =
      case NonEmpty.nonEmpty profileFailures of
        Just failures -> pure (TypedCoreProductionUnsupported failures)
        Nothing ->
          case attachAnalyzedExpression (ModuleGraph.coreModulePath resolvedModule) Map.empty finalState (inferredExpr inferenceResult) of
            Left failures -> fail ("semantic fact invariant failure in Typed Core production: " <> show failures)
            Right (EBlock _ statements) ->
              pure (buildTypedProgram sourcePath modulePath (ModuleGraph.coreModuleFacts resolvedModule) statements)
            Right _ -> pure unsupportedRoot
  where
    unsupportedRoot =
      TypedCoreProductionUnsupported
        (NonEmpty.singleton (TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail))
    profileFailures = inputFailures <> moduleFailures
    inputFailures =
      concat
        [ [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail
          | inferenceCurrentModulePath inputs /= Just modulePath
          ],
          [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail
          | not (validTypedSourcePath sourcePath)
          ],
          [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail
          | not (Map.null (inferenceImportedTypes inputs))
              || not (Map.null (inferenceImportedDataTypes inputs))
              || inferenceImportedCapabilities inputs /= emptyScopeCapabilityFacts
          ],
          [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail
          | not (Set.null (inferenceImportedClassNames inputs))
          ]
        ]
    moduleFailures =
      [ TypedCoreProductionFailure
          (TypedCoreProductionModulePath modulePath)
          TypedCoreResolvedImportsUnsupported
          TypedCoreNoFailureDetail
      | not (null (ModuleGraph.coreModuleImports resolvedModule))
      ]
    modulePath =
      NonEmpty.toList (modulePathTextSegments (ModuleGraph.coreModulePath resolvedModule))

emptyInferenceInputs :: BuiltinResolutionMode -> WarningSettings -> InferenceInputs
emptyInferenceInputs builtinMode settings =
  InferenceInputs
    { inferenceBuiltinMode = builtinMode,
      inferenceWarningSettings = settings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

analysisInputsForInference :: InferenceInputs -> Map Int (ResolvedName, AnalysisBinding) -> AnalysisInputs
analysisInputsForInference inputs forwardValues =
  AnalysisInputs
    { analysisBuiltinMode = inferenceBuiltinMode inputs,
      analysisWarningSettings = inferenceWarningSettings inputs,
      analysisImportedValues =
        Map.map (const (AnalysisBinding Nothing True)) (inferenceImportedTypes inputs),
      analysisForwardFunctions = forwardValues,
      analysisImportedClasses =
        Set.map
          (resolvedAmbientName CapabilityNamespace . mkIdentifier)
          ( Set.union
              (inferenceImportedClassNames inputs)
              (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs)))
          ),
      analysisModulePath = inferenceCurrentModulePath inputs
    }

forwardAnalysisValues :: TypedCoreProductionMode -> Map Int (ResolvedName, SourceSpan) -> Map Int (ResolvedName, AnalysisBinding)
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
              inferenceConstructorWitnessNames =
                inferenceImportedConstructorWitnessNames inputs
            }
      }

moduleInterfaceFromState :: InferenceInputs -> Expr 'Resolved -> InferState -> ModuleInterface
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
      interfaceConcreteImplMethods = scopeConcreteImplMethods localCapabilities
    }
  where
    (declaredValues, declaredDataTypes) = declaredModuleNames expr
    localCapabilities =
      case inferenceCurrentModulePath inputs of
        Just modulePath -> Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)
        Nothing -> capabilityFactsFromState state

declaredModuleNames :: Expr 'Resolved -> (Set ResolvedName, Set Text)
declaredModuleNames expression =
  case expression of
    EBlock _ statements -> foldl' collect (Set.empty, Set.empty) statements
    _ -> (Set.empty, Set.empty)
  where
    collect (valueNames, dataTypeNames) statement =
      case statement of
        SLet _ name _
          | publicModuleValue name -> (Set.insert name valueNames, dataTypeNames)
          | otherwise -> (valueNames, dataTypeNames)
        SData _ typeName _ constructors ->
          ( foldl'
              (\names (DataConstructor _ constructorName _) -> Set.insert constructorName names)
              valueNames
              constructors,
            Set.insert (renderName typeName) dataTypeNames
          )
        _ -> (valueNames, dataTypeNames)

    publicModuleValue name =
      case name of
        GeneratedName {} -> False
        _ -> True

inferExpressionDefault :: Expr 'Resolved -> IO InferenceResult
inferExpressionDefault =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = emptyInferenceInputs ResolveKernelOnly defaultWarningSettings,
        requestedHiddenStatementIndices = Set.empty,
        requestedPreludeStatementIndices = Set.empty,
        requestedModuleStatementFacts = [],
        requestedImplementationEvidenceCandidates = Map.empty
      }

expressionNode :: Expr phase -> CoreNode phase 'ExpressionSort
expressionNode expr =
  case expr of
    ELit node _ -> node
    EVar node _ -> node
    ELambda node _ _ -> node
    EOperatorValue node _ -> node
    EList node _ -> node
    ETuple node _ -> node
    EApply node _ _ -> node
    ETypeApplication node _ _ _ -> node
    EIf node _ _ _ -> node
    EPatternCase node _ _ -> node
    EBinary node _ _ _ -> node
    ESectionLeft node _ _ -> node
    ESectionRight node _ _ -> node
    EBlock node _ -> node

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

inferExprTypeWithMode ::
  Bool ->
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  (InferredExpr, InferState)
inferExprTypeWithMode allowForwardSignedFunctions mode builtinMode env state expr =
  case expr of
    EBlock _ statements
      | mode == ProduceTypedCoreExpressionDirectCall,
        (failureKind, failureDetail) <- blockProductionFailureKindAndDetail statements,
        failureKind == TypedCoreStructuredValueUnsupported ->
          let (blockResult, inferredState) = inferBlock mode statements
              failures =
                InferredProductionFailure [] failureKind failureDetail
                  : inferredProductionFailures blockResult
              result =
                InferredExpr
                  (inferredExpressionType blockResult)
                  (Just (ProvisionalRetainedFailures failures))
                  failures
           in recordBlockResult result inferredState
    EBlock _ statements ->
      uncurry recordBlockResult (inferBlock mode statements)
    _ -> inferExprTypeDetailedWithMode mode builtinMode env state expr
  where
    recordBlockResult result inferredState =
      ( result,
        maybe
          inferredState
          (\expressionType -> recordExpressionFactType (coreNodeId (expressionNode expr)) expressionType inferredState)
          (inferredExpressionType result)
      )

    inferBlock blockMode statements =
      (if allowForwardSignedFunctions then inferScopeTypeWithMode else inferNestedScopeTypeWithMode)
        ( \childMode childBuiltin childEnv childState childExpr ->
            inferExprTypeWithMode False childMode childBuiltin childEnv childState childExpr
        )
        blockMode
        builtinMode
        env
        state
        statements

-- | The shared traversal infers every expression once. Production consumes the
-- provisional nodes it can lower and keeps ordered failures for the rest;
-- ordinary inference projects only the inferred type and state.
inferExprTypeDetailedWithMode ::
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  (InferredExpr, InferState)
inferExprTypeDetailedWithMode _mode = inferExprTypeDetailed

inferExprTypeDetailed ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  (InferredExpr, InferState)
inferExprTypeDetailed builtinMode env state expr =
  let (result, inferredState) = inferExprTypeDetailedRaw builtinMode env state expr
   in ( result,
        maybe
          inferredState
          (\expressionType -> recordExpressionFactType (coreNodeId (expressionNode expr)) expressionType inferredState)
          (inferredExpressionType result)
      )

inferExprTypeDetailedRaw ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr 'Resolved ->
  (InferredExpr, InferState)
inferExprTypeDetailedRaw builtinMode env state expr =
  case expr of
    ELit _ literal ->
      let (literalType, stateAfterLiteral) = literalExpressionType literal state
          expressionType = Just literalType
          finalState = checkLiteralType stateAfterLiteral literal
       in (InferredExpr expressionType (ProvisionalLiteralExpression literal <$> expressionType) [], finalState)
    ETuple _ [] ->
      (InferredExpr (Just (SemanticTuple [])) (Just ProvisionalUnitExpression) [], state)
    EBinary _ operatorSymbol leftExpr rightExpr
      | isTypedCoreDirectCallOperator operatorSymbol ->
          let (leftResult, stateAfterLeft) = inferExprTypeDetailed builtinMode env state leftExpr
              (rightResult, stateAfterRight) = inferExprTypeDetailed builtinMode env stateAfterLeft rightExpr
              (expressionType, operandTyping, finalState) =
                case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
                  (Just leftType, Just rightType) ->
                    inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType stateAfterRight
                  _ -> (Nothing, Nothing, stateAfterRight)
              operation = selectedBinaryOperation operatorSymbol leftExpr rightExpr <$> operandTyping
              promotionFailures = operationPromotionFailures operation
              failures =
                promotionFailures
                  <> childFailures 0 leftResult
                  <> childFailures 1 rightResult
              provisionalExpr =
                case promotionFailures of
                  _ : _ -> Just (ProvisionalRetainedFailures failures)
                  [] -> do
                    resultType <- expressionType
                    BinaryOperation _ (UniformBinaryOperands operandType) _ _ <- operation
                    provisionalBinaryExpression
                      finalState
                      operatorSymbol
                      resultType
                      operandType
                      leftResult
                      rightResult
              stateWithOperation = recordSelectedBinaryOperation operation finalState
           in (InferredExpr expressionType provisionalExpr failures, stateWithOperation)
    EBinary {} ->
      inferUnsupportedWithProduction
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    EIf _ conditionExpr thenExpr elseExpr ->
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
            childFailures 0 conditionResult
              <> childFailures 1 thenResult
              <> childFailures 2 elseResult
          provisionalExpr = do
            case failures of
              _ : _ -> pure (ProvisionalRetainedFailures failures)
              [] -> do
                resultType <- expressionType
                condition <-
                  inferredProvisionalExpr
                    (specializeInferredExpression finalState SemanticBool conditionResult)
                thenExpression <-
                  inferredProvisionalExpr
                    (specializeInferredExpression finalState resultType thenResult)
                elseExpression <-
                  inferredProvisionalExpr
                    (specializeInferredExpression finalState resultType elseResult)
                pure
                  ( ProvisionalIfExpression
                      resultType
                      condition
                      thenExpression
                      elseExpression
                  )
       in (InferredExpr expressionType provisionalExpr failures, finalState)
    EPatternCase _ scrutineeExpr caseArms ->
      let (coverageOrdinal, stateWithCoverageOrdinal) =
            reservePatternCoverageSite state
          (scrutineeResult, stateAfterScrutinee) =
            inferExprTypeDetailed builtinMode env stateWithCoverageOrdinal scrutineeExpr
          (scrutineeType, stateWithScrutineeType) =
            case inferredExpressionType scrutineeResult of
              Just inferredScrutineeType ->
                (inferredScrutineeType, stateAfterScrutinee)
              Nothing -> freshTypeVar stateAfterScrutinee
          (expressionType, inferredFinalState, armResults) =
            inferPatternCaseTypeWithResults
              inferExprTypeDetailedWithMode
              ProduceTypedCoreExpressionDirectCall
              builtinMode
              env
              scrutineeType
              stateWithScrutineeType
              caseArms
          finalState =
            recordPatternCoverageSite
              ( PatternCoverageSite
                  { patternCoverageSiteOrdinal = coverageOrdinal,
                    patternCoverageSiteConstructorInventory =
                      constructorInventoryFromBindingsWithWitnessNames
                        (inferConstructorWitnessNames inferredFinalState)
                        (inferDataTypes inferredFinalState)
                        env,
                    patternCoverageSiteScrutineeType = scrutineeType,
                    patternCoverageSiteArms = caseArms
                  }
              )
              inferredFinalState
          failures =
            childFailures 0 scrutineeResult
              <> concat (zipWith patternCaseArmFailures [0 ..] armResults)
              <> scalarPatternCaseProfileFailures finalState scrutineeType caseArms
          provisionalExpr = do
            case failures of
              _ : _ -> pure (ProvisionalRetainedFailures failures)
              [] -> do
                resultType <- expressionType
                scrutinee <-
                  inferredProvisionalExpr
                    (specializeInferredExpression finalState scrutineeType scrutineeResult)
                arms <- traverse (provisionalPatternCaseArm finalState resultType) armResults
                pure (ProvisionalPatternCaseExpression resultType scrutinee arms)
       in (InferredExpr expressionType provisionalExpr failures, finalState)
    EList _ elements ->
      let (expressionType, finalState, elementResults) = inferListWithProduction state elements
          failures =
            InferredProductionFailure [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
              : concat (zipWith childFailures [0 ..] elementResults)
       in (InferredExpr expressionType (Just (ProvisionalRetainedFailures failures)) failures, finalState)
    ETuple _ elements ->
      let (expressionType, finalState, elementResults) = inferTupleWithProduction state elements
          failures =
            concat (zipWith childFailures [0 ..] elementResults)
          provisional = do
            tupleType <- expressionType
            children <- traverse inferredProvisionalExpr elementResults
            pure (ProvisionalTupleExpression tupleType children)
       in (InferredExpr expressionType provisional failures, finalState)
    EBlock _ statements ->
      let (failureKind, failureDetail) =
            blockProductionFailureKindAndDetail statements
       in inferUnsupportedWithProduction failureKind failureDetail
    EVar node name ->
      let (expressionType, finalState) = inferVariableType (coreNodeId node) name state
       in (InferredExpr expressionType (ProvisionalVariableExpression name <$> expressionType) [], finalState)
    ELambda _ parameterName bodyExpr ->
      let (parameterType, stateAfterParameter) = freshTypeVar state
          extendedEnv = Map.insert parameterName (PlainTypeBinding parameterType) env
          (bodyResult, stateAfterBody) = inferExprTypeDetailed builtinMode extendedEnv stateAfterParameter bodyExpr
          expressionType =
            SemanticFunction (resolveType stateAfterBody parameterType)
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
    EApply _ functionExpr argumentExpr
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
                  inferExprTypeDetailedWithMode
                  ProduceTypedCoreExpressionDirectCall
                  builtinMode
                  env
                  state
                  (coreNodeId (expressionNode expr))
                  methodKey
                  argumentExprs
              stateWithSpineFacts =
                case (expressionType, traverse inferredExpressionType argumentResults) of
                  (Just resultType, Just argumentTypes) ->
                    recordQualifiedMethodSpineFacts
                      expr
                      ( foldr
                          SemanticFunction
                          (resolveType finalState resultType)
                          (map (resolveType finalState) argumentTypes)
                      )
                      finalState
                  _ -> finalState
              argumentFailures =
                concat
                  [ prefixFailures (applicationArgumentPath (length argumentResults) argumentIndex) argumentResult
                  | (argumentIndex, argumentResult) <- zip [0 ..] argumentResults
                  ]
           in retainedUnsupported
                expressionType
                stateWithSpineFacts
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

    inferVariableType nodeId name initialState =
      case Map.lookup name env of
        Just localType -> instantiateEnvBinding localType initialState
        Nothing ->
          case instantiateBuiltinType builtinMode (identifierText name) initialState of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing ->
              case instantiateQualifiedMethodType nodeId (identifierText name) initialState of
                Just qualifiedMethodResult -> qualifiedMethodResult
                Nothing -> (Nothing, initialState)

    inferUnsupportedLeafType unsupportedExpr initialState =
      case unsupportedExpr of
        ELit _ literal ->
          let (literalType, stateAfterLiteral) = literalExpressionType literal initialState
           in (Just literalType, checkLiteralType stateAfterLiteral literal)
        EVar node name -> inferVariableType (coreNodeId node) name initialState
        EOperatorValue _ operatorSymbol ->
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

    patternCaseArmFailures armIndex (InferredPatternCaseArm _ maybeGuardResult maybeBodyResult) =
      maybe [] (prefixFailures [armIndex + 1, 0]) maybeGuardResult
        <> maybe [] (prefixFailures [armIndex + 1, 1]) maybeBodyResult

    scalarPatternCaseProfileFailures finalState scrutineeType caseArms
      | supportedScalarScrutinee finalState scrutineeType
          && supportedScalarArms caseArms =
          []
      | supportedManagedScrutinee finalState scrutineeType
          && any (isManagedPattern . armPattern) caseArms =
          concat
            [ managedPatternFailures [armIndex] pattern
            | (armIndex, CaseArm _ pattern _ _) <- zip [0 :: Int ..] caseArms
            ]
      | otherwise =
          [ InferredProductionFailure
              []
              TypedCorePatternCaseUnsupported
              TypedCorePatternCaseDetail
          ]

    supportedScalarScrutinee finalState scrutineeType =
      case resolveType finalState scrutineeType of
        SemanticInt -> True
        literalType
          | Just literalRange <- integerLiteralRangeFor finalState literalType ->
              integerLiteralRangeFitsNumericType literalRange NumericInt64
        SemanticFloat -> True
        SemanticNumeric {} -> True
        SemanticBool -> True
        SemanticChar -> True
        SemanticTuple [] -> True
        _ -> False

    supportedManagedScrutinee finalState scrutineeType =
      case resolveType finalState scrutineeType of
        SemanticTuple (_ : _) -> True
        SemanticData {} -> True
        _ -> False

    armPattern (CaseArm _ pattern _ _) = pattern

    isManagedPattern pattern =
      case pattern of
        PConstructor _ _ (_ : _) -> True
        PTuple _ (_ : _) -> True
        PAs _ _ nested -> isManagedPattern nested
        POr _ alternatives -> any isManagedPattern alternatives
        _ -> False

    supportedScalarArms caseArms =
      case reverse caseArms of
        CaseArm _ finalPattern Nothing _ : precedingArms ->
          catchAllPattern finalPattern
            && all supportedPrecedingArm precedingArms
        _ -> False

    supportedPrecedingArm (CaseArm _ pattern maybeGuard _) =
      supportedScalarPattern pattern
        && (not (catchAllPattern pattern) || hasGuard maybeGuard)

    supportedScalarPattern pattern =
      case pattern of
        PLiteral {} -> True
        PWildcard _ -> True
        PVariable {} -> True
        _ -> False

    managedPatternFailures patternPath pattern =
      case pattern of
        PWildcard _ -> []
        PVariable {} -> []
        PLiteral _ LText {} -> unsupportedPattern patternPath
        PLiteral {} -> []
        PConstructor _ _ nested -> nestedFailures nested
        PTuple _ nested -> nestedFailures nested
        PAs _ _ nested -> managedPatternFailures (patternPath <> [0]) nested
        POr _ alternatives -> concat [managedPatternFailures (patternPath <> [alternativeIndex]) alternative | (alternativeIndex, alternative) <- zip [0 :: Int ..] alternatives]
        PList {} -> unsupportedPattern patternPath
        PConsList {} -> unsupportedPattern patternPath
      where
        nestedFailures nested =
          concat
            [ managedPatternFailures (patternPath <> [nestedIndex]) nestedPattern
            | (nestedIndex, nestedPattern) <- zip [0 :: Int ..] nested
            ]
        unsupportedPattern path = [InferredProductionFailure path TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]

    catchAllPattern pattern =
      case pattern of
        PWildcard _ -> True
        PVariable {} -> True
        _ -> False

    hasGuard maybeGuard =
      case maybeGuard of
        Just _ -> True
        Nothing -> False

    provisionalPatternCaseArm finalState resultType (InferredPatternCaseArm pattern maybeGuardResult maybeBodyResult) = do
      guardExpression <-
        traverse
          (inferredProvisionalExpr . specializeInferredExpression finalState SemanticBool)
          maybeGuardResult
      bodyResult <- maybeBodyResult
      bodyExpression <-
        inferredProvisionalExpr
          (specializeInferredExpression finalState resultType bodyResult)
      pure (ProvisionalPatternCaseArm pattern guardExpression bodyExpression)

    applicationArgumentPath argumentCount argumentIndex =
      replicate (argumentCount - argumentIndex - 1) 0 <> [1]

    inferBuiltinOperatorApplication operatorSymbol maybeAliasScheme (leftPath, leftExpr) (rightPath, rightExpr) =
      let (leftResult, stateAfterLeft) =
            inferExprTypeDetailed builtinMode env state leftExpr
          (rightResult, stateAfterRight) =
            inferExprTypeDetailed builtinMode env stateAfterLeft rightExpr
          (expressionType, operandTyping, stateAfterBinary) =
            case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
              (Just leftType, Just rightType) ->
                inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType stateAfterRight
              _ -> (Nothing, Nothing, stateAfterRight)
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
          stateWithSpineFacts =
            case (inferredExpressionType leftResult, inferredExpressionType rightResult, expressionType) of
              (Just leftType, Just rightType, Just resultType) ->
                recordBuiltinOperatorSpineFacts
                  operatorSymbol
                  expr
                  leftType
                  rightType
                  resultType
                  finalState
              _ -> finalState
          operation = selectedBinaryOperation operatorSymbol leftExpr rightExpr <$> operandTyping
          promotionFailures = operationPromotionFailures operation
          childProductionFailures =
            prefixFailures leftPath leftResult <> prefixFailures rightPath rightResult
          failures = promotionFailures <> childProductionFailures
          provisionalExpr =
            case failures of
              _ : _ -> Just (ProvisionalRetainedFailures failures)
              [] -> do
                resultType <- expressionType
                BinaryOperation _ (UniformBinaryOperands operandType) _ _ <- operation
                provisionalBinaryExpression
                  stateWithSpineFacts
                  operatorSymbol
                  resultType
                  operandType
                  leftResult
                  rightResult
          stateWithOperation = recordSelectedBinaryOperation operation stateWithSpineFacts
       in (InferredExpr expressionType provisionalExpr failures, stateWithOperation)

    selectedBinaryOperation operatorSymbol leftExpr rightExpr operandTyping =
      BinaryOperation
        operatorSymbol
        operandTyping
        (coreNodeId (expressionNode leftExpr))
        (coreNodeId (expressionNode rightExpr))

    operationPromotionFailures operation = case binaryOperationOperandTyping <$> operation of
      Just Float64PromotedOperands ->
        [InferredProductionFailure [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
      _ -> []

    recordSelectedBinaryOperation operation finalState =
      maybe
        finalState
        (\selected -> recordBinaryOperation (coreNodeId (expressionNode expr)) selected finalState)
        operation

    -- Optimized builtin operator inference visits the two operands directly.
    -- Record the callable and partial-application nodes it intentionally
    -- bypasses, using the final operand and result decisions.
    recordBuiltinOperatorSpineFacts operatorSymbol expression leftType rightType resultType finalState =
      case expression of
        EApply _ (EApply applicationNode operatorExpr _) _
          | Just (spineSymbol, _) <- builtinOperatorSymbolExpr env operatorExpr,
            spineSymbol == operatorSymbol ->
              record
                applicationNode
                (SemanticFunction resolvedRight resolvedResult)
                (recordBuiltinCallableFacts operatorType operatorExpr finalState)
        _ -> finalState
      where
        resolvedLeft = resolveType finalState leftType
        resolvedRight = resolveType finalState rightType
        resolvedResult = resolveType finalState resultType
        operatorType = SemanticFunction resolvedLeft (SemanticFunction resolvedRight resolvedResult)
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

    recordBuiltinCallableFacts operatorType operatorExpr finalState =
      case operatorExpr of
        EOperatorValue node _ -> record node operatorType finalState
        EVar node _ -> record node operatorType finalState
        EApply applicationNode dollarExpr nestedOperator ->
          record
            applicationNode
            operatorType
            ( recordBuiltinCallableFacts
                operatorType
                nestedOperator
                (recordDollarFunctionFacts operatorType dollarExpr finalState)
            )
        _ -> finalState
      where
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

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
              let (builtinInferredResult, builtinState) =
                    inferBuiltinOperatorApplication operatorSymbol Nothing leftOperand rightOperand
               in case inferredExpressionType builtinInferredResult of
                    Just resultType ->
                      ( builtinInferredResult,
                        recordSectionFallbackFacts
                          functionExpr
                          argumentExpr
                          resultType
                          builtinState
                      )
                    Nothing -> genericResult

    -- The direct builtin fallback infers the two operands without traversing
    -- the section syntax that presents one operand as a function. Reconstruct
    -- those wrapper facts from the final operand and result decisions so every
    -- successful resolved node still receives an analyzed runtime plan.
    recordSectionFallbackFacts functionExpr argumentExpr resultType finalState =
      case Map.lookup (coreNodeId (expressionNode argumentExpr)) (inferExpressionFactTypes finalState) of
        Nothing -> finalState
        Just argumentType ->
          recordSectionFunctionFacts
            ( SemanticFunction
                (resolveType finalState argumentType)
                (resolveType finalState resultType)
            )
            functionExpr
            finalState

    recordSectionFunctionFacts sectionType functionExpr finalState =
      case functionExpr of
        ESectionLeft node _ _ -> record node sectionType finalState
        ESectionRight node _ _ -> record node sectionType finalState
        EApply applicationNode dollarExpr sectionExpr ->
          record
            applicationNode
            sectionType
            ( recordSectionFunctionFacts
                sectionType
                sectionExpr
                (recordDollarFunctionFacts sectionType dollarExpr finalState)
            )
        _ -> finalState
      where
        record node expressionType = recordExpressionFactType (coreNodeId node) expressionType

    recordDollarFunctionFacts sectionType dollarExpr finalState =
      if builtinDollarOperatorExpr env dollarExpr
        then record (expressionNode dollarExpr) finalState
        else finalState
      where
        record node =
          recordExpressionFactType
            (coreNodeId node)
            (SemanticFunction sectionType sectionType)

    inferGenericApplication functionExpr argumentExpr =
      let (functionResult, stateAfterFunction) =
            inferExprTypeDetailed builtinMode env state functionExpr
          (argumentResult, stateAfterArgument) =
            inferExprTypeDetailed builtinMode env stateAfterFunction argumentExpr
          (rawExpressionType, rawFinalState) =
            inferApplicationFromResults
              env
              state
              functionExpr
              argumentExpr
              functionResult
              argumentResult
              stateAfterArgument
          (expressionType, finalState) =
            specializeListPrependRawResult
              functionExpr
              argumentResult
              rawExpressionType
              rawFinalState
          specializedArgumentResult =
            case (expressionType, inferredExpressionType functionResult) of
              (Just _, Just functionType) ->
                case resolveType finalState functionType of
                  SemanticFunction parameterType _ ->
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

    -- The raw prepend primitive deliberately adopts the concrete element type
    -- carried by its list argument and coerces the prepended value to match.
    -- Ordinary left-to-right application inference has already instantiated
    -- the polymorphic callable from the head argument, so refine the recorded
    -- callable spine from the tail here when it carries the more specific
    -- Int64/Float64 representation behind an Int/Float alias.
    specializeListPrependRawResult functionExpr argumentResult expressionType finalState =
      case (functionExpr, inferredExpressionType argumentResult, expressionType) of
        (EApply applicationNode builtinExpr _, Just (SemanticList tailElementType), Just _)
          | builtinListPrependRawExpr builtinExpr ->
              let resolvedElementType = resolveType finalState tailElementType
                  listType = SemanticList resolvedElementType
                  partialType = SemanticFunction listType listType
                  callableType = SemanticFunction resolvedElementType partialType
                  stateWithPartialFact =
                    replaceExpressionFactType
                      (coreNodeId applicationNode)
                      partialType
                      finalState
                  stateWithCallableFact =
                    replaceExpressionFactType
                      (coreNodeId (expressionNode builtinExpr))
                      callableType
                      stateWithPartialFact
               in (Just listType, stateWithCallableFact)
        _ -> (expressionType, finalState)

    replaceExpressionFactType nodeId expressionType =
      modifyInferenceOutput
        ( \output ->
            output
              { outputExpressionFactTypes =
                  Map.insert nodeId expressionType (outputExpressionFactTypes output)
              }
        )

    builtinListPrependRawExpr :: Expr 'Resolved -> Bool
    builtinListPrependRawExpr expression =
      case expression of
        EVar _ name ->
          lookupBuiltinSymbolInMode builtinMode (identifierText name)
            == Just BuiltinListPrependRaw
        _ -> False

    inferIfFromResults conditionResult thenResult elseResult stateAfterElse =
      let stateAfterConditionCheck =
            case inferredExpressionType conditionResult of
              Just inferredConditionType ->
                case unifyTypes inferredConditionType SemanticBool stateAfterElse of
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
      case inferApplicationFromResultsUnchecked applicationStartState functionResult argumentResult stateAfterArgument of
        result@(Nothing, _) -> result
        result@(Just _, unifiedState) ->
          case numericConversionLiteralDiagnostic builtinMode currentEnv functionExpr argumentExpr of
            Just diagnostic -> (Nothing, addTypeError unifiedState diagnostic)
            Nothing -> result

    inferApplicationFromResultsUnchecked applicationStartState functionResult argumentResult stateAfterArgument =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAfterArgument
       in case (inferredExpressionType functionResult, inferredExpressionType argumentResult) of
            (Just functionType, Just argumentType) ->
              case unifyTypes functionType (SemanticFunction argumentType resultTypeVar) stateWithResultVar of
                Just unifiedState ->
                  (Just (resolveType unifiedState resultTypeVar), unifiedState)
                Nothing ->
                  ( Nothing,
                    addTypeError
                      (discardFailedFunctionApplicationConstraints applicationStartState stateWithResultVar)
                      ( mkApplyTypeError
                          (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar functionType))
                          (defaultLiteralTypes stateWithResultVar (resolveType stateWithResultVar argumentType))
                      )
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
              (expressionType, operandTyping, finalState) =
                case (inferredExpressionType leftResult, inferredExpressionType rightResult) of
                  (Just leftType, Just rightType) ->
                    inferBinaryType
                      operatorSymbol
                      leftExpr
                      rightExpr
                      leftType
                      rightType
                      stateAfterRight
                  _ -> (Nothing, Nothing, stateAfterRight)
              operation = selectedBinaryOperation operatorSymbol leftExpr rightExpr <$> operandTyping
           in (expressionType, recordSelectedBinaryOperation operation finalState, leftResult, rightResult)
      | otherwise =
          inferDeclaredBinaryWithProduction env state operatorSymbol leftExpr rightExpr

    inferDeclaredBinaryWithProduction currentEnv initialState operatorSymbol leftExpr rightExpr =
      let (operatorType, stateAfterOperator) =
            instantiateDeclaredOperatorBindingType currentEnv operatorSymbol initialState
          operatorResult = InferredExpr operatorType Nothing []
          (leftResult, stateAfterLeft) =
            inferExprTypeDetailed builtinMode currentEnv stateAfterOperator leftExpr
          (intermediateType, stateAfterFirstApplication) =
            inferApplicationFromResultsUnchecked
              initialState
              operatorResult
              leftResult
              stateAfterLeft
          intermediateResult = InferredExpr intermediateType Nothing []
          (rightResult, stateAfterRight) =
            inferExprTypeDetailed builtinMode currentEnv stateAfterFirstApplication rightExpr
          (expressionType, finalState) =
            inferApplicationFromResultsUnchecked
              stateAfterFirstApplication
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
          let (operatorType, stateAfterOperator) =
                instantiateDeclaredOperatorBindingType env operatorSymbol state
              operatorResult = InferredExpr operatorType Nothing []
              (leftResult, stateAfterLeft) =
                inferExprTypeDetailed builtinMode env stateAfterOperator leftExpr
              (expressionType, finalState) =
                inferApplicationFromResultsUnchecked
                  state
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
              (operatorType, stateAfterOperator) =
                instantiateDeclaredOperatorBindingType env operatorSymbol stateAfterLeftType
              operatorResult = InferredExpr operatorType Nothing []
              leftResult = InferredExpr (Just leftType) Nothing []
              (intermediateType, stateAfterFirstApplication) =
                inferApplicationFromResultsUnchecked
                  stateAfterLeftType
                  operatorResult
                  leftResult
                  stateAfterOperator
              intermediateResult = InferredExpr intermediateType Nothing []
              (rightResult, stateAfterRight) =
                inferExprTypeDetailed builtinMode env stateAfterFirstApplication rightExpr
              (bodyType, finalState) =
                inferApplicationFromResultsUnchecked
                  stateAfterFirstApplication
                  intermediateResult
                  rightResult
                  stateAfterRight
              expressionType =
                SemanticFunction (resolveType finalState leftType)
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
        ELambda _ parameterName bodyExpr ->
          let (parameterType, stateAfterParameter) = freshTypeVar state
              extendedEnv = Map.insert parameterName (PlainTypeBinding parameterType) env
              (bodyResult, stateAfterBody) =
                inferExprTypeDetailed builtinMode extendedEnv stateAfterParameter bodyExpr
              expressionType =
                SemanticFunction (resolveType stateAfterBody parameterType)
                  <$> inferredExpressionType bodyResult
           in retainedUnsupported expressionType stateAfterBody failureKind failureDetail (childFailures 0 bodyResult)
        EList _ elements ->
          let (expressionType, finalState, elementResults) =
                inferListWithProduction state elements
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                (concat (zipWith childFailures [0 ..] elementResults))
        ETuple _ elements ->
          let (expressionType, finalState, elementResults) =
                inferTupleWithProduction state elements
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                (concat (zipWith childFailures [0 ..] elementResults))
        EApply _ functionExpr argumentExpr ->
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
        ETypeApplication applicationNode functionExpr typeArgumentSpan typeArgument ->
          let (expressionType, finalState, maybeFunctionResult) =
                inferExplicitTypeApplicationWithResult
                  inferExprTypeDetailedWithMode
                  ProduceTypedCoreExpressionDirectCall
                  builtinMode
                  env
                  state
                  (coreNodeId applicationNode)
                  functionExpr
                  typeArgumentSpan
                  typeArgument
              functionFailures =
                maybe [] (childFailures 0) maybeFunctionResult
           in retainedUnsupported expressionType finalState failureKind failureDetail functionFailures
        EIf _ conditionExpr thenExpr elseExpr ->
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
        EPatternCase _ scrutineeExpr caseArms ->
          let (coverageOrdinal, stateWithCoverageOrdinal) =
                reservePatternCoverageSite state
              (scrutineeResult, stateAfterScrutinee) =
                inferExprTypeDetailed builtinMode env stateWithCoverageOrdinal scrutineeExpr
              (scrutineeType, stateWithScrutineeType) =
                case inferredExpressionType scrutineeResult of
                  Just inferredScrutineeType ->
                    (inferredScrutineeType, stateAfterScrutinee)
                  Nothing ->
                    freshTypeVar stateAfterScrutinee
              (expressionType, inferredFinalState, armResults) =
                inferPatternCaseTypeWithResults
                  inferExprTypeDetailedWithMode
                  ProduceTypedCoreExpressionDirectCall
                  builtinMode
                  env
                  scrutineeType
                  stateWithScrutineeType
                  caseArms
              finalState =
                recordPatternCoverageSite
                  ( PatternCoverageSite
                      { patternCoverageSiteOrdinal = coverageOrdinal,
                        patternCoverageSiteConstructorInventory =
                          constructorInventoryFromBindingsWithWitnessNames
                            (inferConstructorWitnessNames inferredFinalState)
                            (inferDataTypes inferredFinalState)
                            env,
                        patternCoverageSiteScrutineeType = scrutineeType,
                        patternCoverageSiteArms = caseArms
                      }
                  )
                  inferredFinalState
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                ( childFailures 0 scrutineeResult
                    <> concat (zipWith patternCaseArmFailures [0 ..] armResults)
                )
        EBinary _ operatorSymbol leftExpr rightExpr ->
          let (expressionType, finalState, leftResult, rightResult) =
                inferUnsupportedBinaryWithProduction operatorSymbol leftExpr rightExpr
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                (childFailures 0 leftResult <> childFailures 1 rightResult)
        ESectionLeft _ leftExpr operatorSymbol ->
          let (expressionType, finalState, leftResult) =
                inferUnsupportedLeftSectionWithProduction operatorSymbol leftExpr
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                (childFailures 0 leftResult)
        ESectionRight _ operatorSymbol rightExpr ->
          let (expressionType, finalState, rightResult) =
                inferUnsupportedRightSectionWithProduction operatorSymbol rightExpr
           in retainedUnsupported
                expressionType
                finalState
                failureKind
                failureDetail
                (childFailures 0 rightResult)
        EBlock _ statements ->
          let (blockResult, finalState) =
                inferNestedScopeTypeWithMode
                  ( \childMode childBuiltin childEnv childState childExpr ->
                      inferExprTypeWithMode
                        False
                        childMode
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
           in (Just (SemanticList elementType), finalState, [])
        firstElement : restElements ->
          let (firstResult, stateAfterFirst) =
                inferExprTypeDetailed builtinMode env initialState firstElement
              (finalElementType, finalState, reversedResults) =
                foldl'
                  inferNextListElement
                  (inferredExpressionType firstResult, stateAfterFirst, [firstResult])
                  restElements
           in (SemanticList <$> finalElementType, finalState, reverse reversedResults)

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
                        (mergedUnifiedType unifiedState inferredExpectedType inferredActualType),
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
              (SemanticTuple . reverse <$> maybeReversedTypes, stateAcc, reverse reversedResults)
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
            outputInferredConstraints =
              outputInferredConstraints (inferOutput stateBeforeFunction),
            outputInferredConstraintCount =
              outputInferredConstraintCount (inferOutput stateBeforeFunction)
          }
    )
    stateAfterApplication

qualifiedMethodApplicationSpine :: Expr 'Resolved -> InferState -> Maybe (ResolvedName, Text, [Expr 'Resolved])
qualifiedMethodApplicationSpine expr state =
  case applicationSpine expr of
    Just (methodName, argumentExprs)
      | let methodKey = identifierText methodName,
        qualifiedMethodClassIsVisible methodKey state ->
          Just (methodName, methodKey, argumentExprs)
    _ -> Nothing

applicationSpine :: Expr 'Resolved -> Maybe (ResolvedName, [Expr 'Resolved])
applicationSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply _ (EOperatorValue _ "$") functionExpr ->
          go argumentExprs functionExpr
        EApply _ functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar _ name ->
          Just (name, argumentExprs)
        _ ->
          Nothing

recordQualifiedMethodSpineFacts :: Expr 'Resolved -> ExpressionType -> InferState -> InferState
recordQualifiedMethodSpineFacts expression methodType state =
  case expression of
    EApply _ functionExpression _ -> snd (recordSpine functionExpression state)
    _ -> state
  where
    recordSpine currentExpression currentState =
      case currentExpression of
        EApply applicationNode (EOperatorValue dollarNode "$") functionExpression ->
          let (functionType, stateAfterFunction) = recordSpine functionExpression currentState
              stateAfterDollar = record dollarNode (SemanticFunction functionType functionType) stateAfterFunction
           in (functionType, record applicationNode functionType stateAfterDollar)
        EApply applicationNode functionExpression _ ->
          let (functionType, stateAfterFunction) = recordSpine functionExpression currentState
              resultType = applicationResultType functionType
           in (resultType, record applicationNode resultType stateAfterFunction)
        ETypeApplication applicationNode functionExpression _ _ ->
          let (_, stateAfterFunction) = recordSpine functionExpression currentState
           in (methodType, record applicationNode methodType stateAfterFunction)
        _ ->
          ( methodType,
            record (expressionNode currentExpression) methodType currentState
          )
    record node expressionType =
      recordExpressionFactType (coreNodeId node) expressionType
    applicationResultType expressionType =
      case expressionType of
        SemanticFunction _ resultType -> resultType
        _ -> expressionType

builtinOperatorApplicationSpine ::
  TypeEnv ->
  Expr 'Resolved ->
  Maybe (Text, Maybe TypeScheme, ([Int], Expr 'Resolved), ([Int], Expr 'Resolved), Bool)
builtinOperatorApplicationSpine env expr =
  case expr of
    EApply _ (EApply _ dollarExpr sectionExpr) argumentExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          case sectionExpr of
            ESectionLeft _ leftExpr operatorSymbol
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([0, 1, 0], leftExpr), ([1], argumentExpr), True)
            ESectionRight _ operatorSymbol rightExpr
              | builtinSectionOperatorSymbol operatorSymbol ->
                  Just (operatorSymbol, Nothing, ([1], argumentExpr), ([0, 1, 0], rightExpr), True)
            _ -> Nothing
    EApply _ (ESectionLeft _ leftExpr operatorSymbol) rightExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([0, 0], leftExpr), ([1], rightExpr), True)
    EApply _ (ESectionRight _ operatorSymbol rightExpr) leftExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing, ([1], leftExpr), ([0, 0], rightExpr), True)
    EApply _ (EApply _ operatorExpr leftExpr) rightExpr -> do
      (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr env operatorExpr
      if hasOperatorRule operatorSymbol
        then Just (operatorSymbol, maybeAliasScheme, ([0, 1], leftExpr), ([1], rightExpr), False)
        else Nothing
    _ -> Nothing

builtinOperatorSymbolExpr :: TypeEnv -> Expr 'Resolved -> Maybe (Text, Maybe TypeScheme)
builtinOperatorSymbolExpr env expr =
  case expr of
    EOperatorValue _ operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing)
    EApply _ dollarExpr operatorExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSymbolExpr env operatorExpr
    EVar _ name ->
      case Map.lookup name env of
        Just (BuiltinOperatorAliasTypeBinding operatorSymbol) -> Just (operatorSymbol, Nothing)
        Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) -> Just (operatorSymbol, Just typeScheme)
        _ -> Nothing
    _ -> Nothing

literalExpressionType :: Literal -> InferState -> (ExpressionType, InferState)
literalExpressionType literal state =
  case literal of
    LInt value -> freshIntegerLiteralType (singletonIntegerLiteralRange value) state
    LFloat _ _ maybeTargetType ->
      ( case maybeTargetType of
          Just targetType -> SemanticNumeric targetType
          Nothing -> SemanticFloat,
        state
      )
    LBool _ -> (SemanticBool, state)
    LChar _ -> (SemanticChar, state)
    LText _ -> (SemanticText, state)

checkLiteralType :: InferState -> Literal -> InferState
checkLiteralType state literal =
  case literal of
    LFloat literalValue literalSource (Just targetType) ->
      case targetedFloatLiteralDiagnostic targetType literalValue literalSource of
        Just diagnostic -> addTypeError state diagnostic
        Nothing -> state
    _ -> state

numericConversionLiteralDiagnostic :: BuiltinResolutionMode -> TypeEnv -> Expr 'Resolved -> Expr 'Resolved -> Maybe Diagnostic
numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr =
  case (functionExpr, argumentExpr) of
    (EVar _ functionName, ELit _ (LInt literalValue)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          case numericTypeLiteralIntegerBounds targetType of
            Just bounds@(lowerBound, upperBound)
              | literalValue < lowerBound || literalValue > upperBound ->
                  Just (mkNumericConversionLiteralTypeError (identifierText functionName) literalValue targetType bounds)
            _ -> Nothing
        Nothing -> Nothing
    (EVar _ functionName, ELit _ (LFloat literalValue literalSource _)) ->
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

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

numericConversionTargetFromCallable :: BuiltinResolutionMode -> TypeEnv -> ResolvedName -> Maybe NumericType
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
       in Just (SemanticFunction sourceType (SemanticNumeric targetType), stateAfterNumericConstraint)
    Nothing ->
      instantiateBuiltinSymbolTypeByName (builtinSymbolName builtinSymbol) state

instantiateBuiltinSymbolTypeByName :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolTypeByName builtinName state =
  case builtinName of
    "hd" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) elementType, stateAfterElement)
    "tl" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) (SemanticList elementType), stateAfterElement)
    "map" ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in Just
            ( SemanticFunction
                (SemanticFunction sourceType targetType)
                (SemanticFunction (SemanticList sourceType) (SemanticList targetType)),
              stateAfterTarget
            )
    "filter" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
            ( SemanticFunction
                (SemanticFunction elementType SemanticBool)
                (SemanticFunction (SemanticList elementType) (SemanticList elementType)),
              stateAfterElement
            )
    "print!" ->
      -- Stub-v1 runtime keeps `print!` as an impure primitive that returns the
      -- evaluated argument value unchanged so compile/runtime paths stay simple.
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (SemanticFunction valueType valueType, stateAfterValueType)
    "listPrependRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just
            ( SemanticFunction
                elementType
                (SemanticFunction (SemanticList elementType) (SemanticList elementType)),
              stateAfterElement
            )
    "listReverseRaw" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (SemanticFunction (SemanticList elementType) (SemanticList elementType), stateAfterElement)
    "charToUInt32" ->
      Just (SemanticFunction SemanticChar (SemanticNumeric NumericUInt32), state)
    "charFromUInt32Raw" ->
      Just (SemanticFunction (SemanticNumeric NumericUInt32) (SemanticList SemanticChar), state)
    "charIsAlpha" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsAlphaNum" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsDigit" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsSpace" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsHexDigit" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsLower" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charIsUpper" ->
      Just (SemanticFunction SemanticChar SemanticBool, state)
    "charToLower" ->
      Just (SemanticFunction SemanticChar SemanticChar, state)
    "charToUpper" ->
      Just (SemanticFunction SemanticChar SemanticChar, state)
    "textLength" ->
      Just (SemanticFunction SemanticText SemanticInt, state)
    "textUnconsRaw" ->
      Just
        ( SemanticFunction
            SemanticText
            (SemanticList (SemanticTuple [SemanticChar, SemanticText])),
          state
        )
    "textAppend" ->
      Just (SemanticFunction SemanticText (SemanticFunction SemanticText SemanticText), state)
    "textAppendChar" ->
      Just (SemanticFunction SemanticText (SemanticFunction SemanticChar SemanticText), state)
    "textFromChars" ->
      Just (SemanticFunction (SemanticList SemanticChar) SemanticText, state)
    "textConcat" ->
      Just (SemanticFunction (SemanticList SemanticText) SemanticText, state)
    "renderValue" ->
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (SemanticFunction valueType SemanticText, stateAfterValueType)
    "readTextRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "writeTextRaw!" ->
      Just
        ( SemanticFunction
            SemanticText
            (SemanticFunction SemanticText hostIOOutcomeType),
          state
        )
    "readStdinRaw!" ->
      Just (SemanticFunction unitType hostIOOutcomeType, state)
    "writeStdoutRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "writeStderrRaw!" ->
      Just (SemanticFunction SemanticText hostIOOutcomeType, state)
    "arguments!" ->
      Just (SemanticFunction unitType (SemanticList SemanticText), state)
    "exit!" ->
      Just (SemanticFunction SemanticInt unitType, state)
    _ -> Nothing

hostIOOutcomeType :: ExpressionType
hostIOOutcomeType = SemanticTuple [SemanticBool, SemanticText, SemanticText, SemanticText]

unitType :: ExpressionType
unitType = SemanticTuple []
