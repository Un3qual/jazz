{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Coordinate checking, coverage, binding diagnostics and warning policy for
-- one module. Dependencies supply public semantic interfaces.
module Jazz.Compiler.ModuleAnalysis
  ( ImportedInterface,
    InferenceInputs (..),
    InferenceResult (..),
    inferredDiagnostics,
    analyzeResolvedExpression,
    inferExpressionWithInputs,
    inferExpressionDefault,
    analyzeModule,
    dependencyImportInterface,
    importWholeInterface,
  )
where

import Data.List (partition, sortOn, union)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CoreNode (..),
    CorePhase (..),
    CoreSort (StatementSort),
    Expr (EBlock),
    coreNodeId,
    expressionNode,
    statementNode,
  )
import Jazz.Compiler.Analyzer (AnalysisBinding (..), AnalysisInputs (..), AnalysisResult (..), analyzeProgramWithInputs, analyzeProgramWithInputsAndPreparedScope)
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactCapability,
  )
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey, ResolvedReference (LexicalReference), capabilityExportName, capabilityResolvedName)
import Jazz.Compiler.Diagnostics (CompilationDiagnostics (..), Diagnostic, diagnosticWarningCategory, isErrorDiagnostic)
import Jazz.Compiler.Diagnostics.Strictness (forceDiagnostic)
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    exportNamesInNamespace,
    inventoryHasExport,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule,
    ModuleImport,
    coreModuleExpr,
    coreModuleFacts,
    coreModuleIdentity,
    coreModuleImports,
    coreModulePath,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    SourceUnitOwner (..),
    renderModulePath,
  )
import Jazz.Compiler.ModuleImportScope (ValidatedImportScope, dependencyImportViews)
import Jazz.Compiler.ModuleInterface
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (CapabilityNamespace, ConstructorNamespace),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    UnresolvedName,
    mkIdentifier,
    qualifiedName,
    resolvedAmbientName,
    sourceName,
  )
import Jazz.Compiler.PatternCoverage (PatternCoverageFailure (..), PatternCoverageSite (..), analyzePatternCoverage)
import Jazz.Compiler.SemanticDeclarations (DeclarationVariable)
import Jazz.Compiler.SemanticFacts
  ( CoreNodeId,
    SemanticFactInvariantFailure (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceSubject (..),
    inferExpressionWork,
    inferenceSubjectExpr,
    moduleInterfaceFromState,
  )
import Jazz.Compiler.TypeInference.Analyzed (finalizeCheckedExpression)
import Jazz.Compiler.TypeInference.Diagnostics (mkNonExhaustivePatternMatchError, mkUnreachablePatternArmError)
import Jazz.Compiler.TypeInference.Draft (CheckedExpr (..))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..), inferredDiagnostics)
import Jazz.Compiler.TypeInference.Solver (resolveType)
import Jazz.Compiler.TypeInference.State (InferState, inferErrorsRev, inferPatternCoverageSites)
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ExpressionType,
    ScopeCapabilityFacts (..),
    SemanticBinding,
    TypeEnvKey (..),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.WarningConfig (WarningSettings, defaultWarningSettings)

-- | Analyze one resolved module against its complete imported interface. The
-- caller supplies source ownership and the bundled-prelude warning policy,
-- while dependency availability and diagnostic accumulation belong to the driver.
analyzeModule :: CompileInputs -> (ModulePath -> SourceUnitOwner) -> Bool -> ImportedInterface -> CoreModule 'Resolved -> IO (InferenceResult, Maybe (CoreModule 'Analyzed))
analyzeModule inputs owner hideRootBindings importedInterface resolvedModule = do
  let modulePath = coreModulePath resolvedModule
  (inference, attachment) <-
    analyzeExpressionWithInputs
      (moduleStatementFactSeeds resolvedModule)
      ((moduleInferenceInputs inputs resolvedModule importedInterface) {inferenceCurrentModulePath = case owner modulePath of StandaloneSourceUnit _ -> Nothing; _ -> Just modulePath})
      hideRootBindings
      (coreModuleExpr resolvedModule)
  maybeAnalyzedExpression <- checkedAttachment modulePath attachment
  maybeAnalyzedModule <-
    traverse
      ( \(analyzedExpression, moduleStatementFacts) ->
          checkedAnalyzedModule
            modulePath
            (analyzedModuleFromExpression resolvedModule inference moduleStatementFacts analyzedExpression)
      )
      maybeAnalyzedExpression
  case maybeAnalyzedModule of
    Just analyzed
      | moduleInterfaceExportInventory (ModuleGraph.analyzedModuleInterface (coreModuleFacts analyzed)) /= ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule) ->
          fail ("typed module exports disagree with resolution in " <> Text.unpack (renderModulePath modulePath) <> ": expected " <> show (ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule)) <> ", got " <> show (moduleInterfaceExportInventory (ModuleGraph.analyzedModuleInterface (coreModuleFacts analyzed))))
    _ -> pure ()
  pure (inference, maybeAnalyzedModule)

checkedAttachment :: ModulePath -> Either (NonEmpty.NonEmpty SemanticFactInvariantFailure) (Maybe value) -> IO (Maybe value)
checkedAttachment modulePath attachment =
  case attachment of
    Left failures -> fail ("semantic fact invariant failure in " <> Text.unpack (renderModulePath modulePath) <> ": " <> show failures)
    Right value -> pure value

checkedAnalyzedModule :: ModulePath -> Either SemanticFactInvariantFailure value -> IO value
checkedAnalyzedModule modulePath result =
  case result of
    Left failure -> fail ("semantic fact invariant failure in " <> Text.unpack (renderModulePath modulePath) <> ": " <> show failure)
    Right value -> pure value

moduleStatementFactSeeds :: CoreModule 'Resolved -> [(CoreNode 'Resolved 'StatementSort, StatementDeclarationFact)]
moduleStatementFactSeeds = map importSeed . coreModuleImports
  where
    importSeed importDecl =
      ( ModuleGraph.moduleImportNode importDecl,
        ImportDeclaration (ModuleGraph.importedModule importDecl)
      )

moduleInferenceInputs :: CompileInputs -> CoreModule 'Resolved -> ImportedInterface -> InferenceInputs
moduleInferenceInputs inputs resolvedModule importedInterface =
  InferenceInputs
    { inferencePublicExports = Just (ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule)),
      inferenceWarningSettings = compileInputWarningSettings inputs,
      inferenceExternalUses = compileInputExternalUses inputs,
      inferenceImportedTypes = importedTypes importedInterface,
      inferenceImportedDataTypes = importedDataTypes importedInterface,
      inferenceImportedConstructorWitnessNames = importedConstructorWitnessNames importedInterface,
      inferenceImportedCapabilities = importedCapabilities importedInterface,
      inferenceImportedClassNames = importedClassNames importedInterface,
      inferenceCurrentModulePath = Just (coreModulePath resolvedModule)
    }

analyzedModuleFromExpression :: CoreModule 'Resolved -> InferenceResult -> Map CoreNodeId StatementFacts -> Expr 'Analyzed -> Either SemanticFactInvariantFailure (CoreModule 'Analyzed)
analyzedModuleFromExpression resolvedModule inference moduleStatementFacts analyzedExpression =
  case analyzedExpression of
    EBlock bodyNode statements -> do
      analyzedImports <- traverse (analyzedImport statementFactsByNode) (coreModuleImports resolvedModule)
      pure
        ( ModuleGraph.CoreModule
            { ModuleGraph.coreModuleIdentity = coreModuleIdentity resolvedModule,
              ModuleGraph.coreModuleBodyNode = bodyNode,
              ModuleGraph.coreModuleImports = analyzedImports,
              ModuleGraph.coreModuleStatements = statements,
              ModuleGraph.coreModuleFacts =
                ModuleGraph.AnalyzedModuleFacts
                  { ModuleGraph.analyzedModuleExports = ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule),
                    ModuleGraph.analyzedModuleExportSelectors = ModuleGraph.resolvedModuleExportSelectors (coreModuleFacts resolvedModule),
                    ModuleGraph.analyzedModuleInterface = moduleInterface,
                    ModuleGraph.analyzedModuleImportScope = ModuleGraph.resolvedModuleImportScope (coreModuleFacts resolvedModule),
                    ModuleGraph.analyzedModuleDiagnosticGroups = inferredDiagnosticGroups inference
                  }
            }
        )
      where
        statementFactsByNode =
          Map.union
            moduleStatementFacts
            ( Map.fromList
                [ (nodeId, facts)
                | statement <- statements,
                  let CoreNode nodeId _ facts = statementNode statement
                ]
            )
        moduleInterface = inferredModuleInterface inference
    _ -> Left (AnalyzedModuleRootNotBlock (coreNodeId (expressionNode analyzedExpression)))

analyzedImport :: Map CoreNodeId StatementFacts -> ModuleImport 'Resolved -> Either SemanticFactInvariantFailure (ModuleImport 'Analyzed)
analyzedImport factsByNode importDecl =
  case ModuleGraph.moduleImportNode importDecl of
    CoreNode nodeId spanValue _ ->
      case Map.lookup nodeId factsByNode of
        Nothing -> Left (MissingStatementFacts nodeId)
        Just facts ->
          Right
            ModuleGraph.ModuleImport
              { ModuleGraph.moduleImportNode = CoreNode nodeId spanValue facts,
                ModuleGraph.importedModule = ModuleGraph.importedModule importDecl,
                ModuleGraph.importExposure = ModuleGraph.importExposure importDecl
              }

dependencyImportInterface :: ValidatedImportScope -> ModulePath -> ModuleInterface -> ImportedInterface
dependencyImportInterface scope path interface =
  foldMap (\(alias, selected) -> importSelectedInterface (ImportedModule path) alias selected interface) (dependencyImportViews path scope)

data ImportedInterface = ImportedInterface
  { importedTypes :: Map TypeEnvKey (SemanticBinding DeclarationVariable),
    importedDataTypes :: Map ResolvedName DataTypeBinding,
    importedConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    importedCapabilities :: ScopeCapabilityFacts,
    importedClassNames :: Set.Set Text
  }

instance Semigroup ImportedInterface where
  left <> right =
    ImportedInterface
      { importedTypes = Map.union (importedTypes left) (importedTypes right),
        importedDataTypes = Map.union (importedDataTypes left) (importedDataTypes right),
        importedConstructorWitnessNames =
          Map.union
            (importedConstructorWitnessNames left)
            (importedConstructorWitnessNames right),
        importedCapabilities =
          let leftFacts = importedCapabilities left
              rightFacts = importedCapabilities right
           in (leftFacts <> rightFacts)
                { scopeConcreteImplMethods =
                    Map.unionWith
                      union
                      (scopeConcreteImplMethods leftFacts)
                      (scopeConcreteImplMethods rightFacts)
                },
        importedClassNames = Set.union (importedClassNames left) (importedClassNames right)
      }

instance Monoid ImportedInterface where
  mempty =
    ImportedInterface
      { importedTypes = Map.empty,
        importedDataTypes = Map.empty,
        importedConstructorWitnessNames = Map.empty,
        importedCapabilities = mempty,
        importedClassNames = Set.empty
      }

importWholeInterface :: ResolvedNameOrigin -> ModuleInterface -> ImportedInterface
importWholeInterface origin moduleInterface =
  importSelectedInterface
    origin
    Nothing
    (moduleInterfaceExportInventory moduleInterface)
    moduleInterface

importSelectedInterface :: ResolvedNameOrigin -> Maybe Text -> ModuleExportInventory -> ModuleInterface -> ImportedInterface
importSelectedInterface origin maybeAlias selectedInventory moduleInterface =
  ImportedInterface
    { importedTypes =
        Map.fromList
          [ ( TypeEnvKey (LexicalReference binder) (UserName (ResolvedUserName origin (moduleExportNamespace export) (mkIdentifier (moduleExportName export)))),
              binding
            )
          | (export, ModuleValueBinding binder binding) <- Map.toList selectedValueTypes
          ],
      importedDataTypes = interfaceDataTypes moduleInterface,
      importedConstructorWitnessNames =
        Map.fromList
          [ (importedName export, sourceConstructorName export)
          | export <- Map.keys selectedValueTypes,
            moduleExportNamespace export == ConstructorNamespace
          ],
      importedCapabilities = selectedCapabilities,
      importedClassNames = case maybeAlias of
        Nothing -> selectedClassNames
        Just _ -> Set.empty
    }
  where
    importedName export =
      UserName
        ( ResolvedUserName
            origin
            (moduleExportNamespace export)
            (mkIdentifier (moduleExportName export))
        )

    sourceConstructorName export =
      case maybeAlias of
        Nothing -> sourceName member
        Just alias -> qualifiedName (mkIdentifier alias) member
      where
        member = mkIdentifier (moduleExportName export)

    selectedValueTypes =
      Map.filterWithKey
        (\export _ -> inventoryHasExport export selectedInventory)
        (interfaceValueBindings moduleInterface)
    selectedClassNames = exportNamesInNamespace CapabilityNamespace selectedInventory
    selectedClassFacts =
      Map.filterWithKey
        (\capability _ -> Set.member (capabilityExportName capability) selectedClassNames)
        (interfaceClassFacts moduleInterface)
    selectedCapabilities =
      ScopeCapabilityFacts
        { scopeClassFacts = selectedClassFacts,
          scopeGeneratedEqualityClassFacts =
            Set.filter
              (\capability -> Set.member (capabilityExportName capability) selectedClassNames)
              (interfaceGeneratedEqualityClassFacts moduleInterface),
          scopeConcreteImplFacts =
            Set.filter (factUsesClass selectedClassNames) (interfaceConcreteImplFacts moduleInterface),
          scopeClassMethodSignatures =
            Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceClassMethods moduleInterface),
          scopeConcreteImplMethods =
            Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceConcreteImplMethods moduleInterface)
        }

factUsesClass :: Set.Set Text -> ConcreteImplFact -> Bool
factUsesClass classNames fact = Set.member (capabilityExportName (concreteImplFactCapability fact)) classNames

methodUsesClass :: Set.Set Text -> CapabilityMethodKey -> value -> Bool
methodUsesClass classNames methodKey _ =
  Set.member (capabilityExportName (fst methodKey)) classNames

data InferenceRequest = InferenceRequest
  { requestedInferenceInputs :: InferenceInputs,
    requestedHideRootBindings :: Bool
  }

analyzeResolvedExpression ::
  WarningSettings ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed))
    )
analyzeResolvedExpression settings expression = do
  (inference, finalState, checked) <-
    inferExpressionWithRequestAndState
      InferenceRequest
        { requestedInferenceInputs = emptyInferenceInputs settings,
          requestedHideRootBindings = False
        }
      expression
  if any isErrorDiagnostic (inferredDiagnostics inference)
    then pure (inference, Right Nothing)
    else
      pure
        ( inference,
          Just
            <$> finalizeCheckedExpression finalState checked
        )

inferExpressionWithInputs :: InferenceInputs -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithInputs inputs =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = inputs,
        requestedHideRootBindings = False
      }

inferExpressionWithRequest :: InferenceRequest -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithRequest request expr = (\(result, _, _) -> result) <$> inferExpressionWithRequestAndState request expr

inferExpressionWithRequestAndState :: InferenceRequest -> Expr 'Resolved -> IO (InferenceResult, InferState, CheckedExpr)
inferExpressionWithRequestAndState request expr =
  {-# SCC "jazz-stage:type-inference" #-}
  let inputs = requestedInferenceInputs request
      (inferredResult, finalState, inferenceSubject) =
        inferExpressionWork
          inputs
          expr
      expression = inferenceSubjectExpr inferenceSubject
      finalizedInference = finalizeInferenceState inputs expression finalState
   in expression `seq`
        forceFinalizedInferenceContainers finalizedInference `seq`
          do
            inference <-
              finishInference
                inputs
                (requestedHideRootBindings request)
                inferenceSubject
                (checkedExprType inferredResult)
                finalizedInference
            pure (inference, finalState, inferredResult)

analyzeExpressionWithInputs ::
  [(CoreNode 'Resolved 'StatementSort, StatementDeclarationFact)] ->
  InferenceInputs ->
  Bool ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed, Map CoreNodeId StatementFacts))
    )
analyzeExpressionWithInputs moduleStatementFacts inputs hideRootBindings expression = do
  (inference, finalState, checked) <-
    inferExpressionWithRequestAndState
      InferenceRequest
        { requestedInferenceInputs = inputs,
          requestedHideRootBindings = hideRootBindings
        }
      expression
  if any isErrorDiagnostic (inferredDiagnostics inference)
    then pure (inference, Right Nothing)
    else
      pure
        ( inference,
          Just
            <$> ( (,)
                    <$> finalizeCheckedExpression finalState checked
                    <*> pure (Map.fromList [(coreNodeId node, StatementFacts (coreNodeFacts node) [] Map.empty declaration) | (node, declaration) <- moduleStatementFacts])
                )
        )

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

finishInference :: InferenceInputs -> Bool -> InferenceSubject -> Maybe ExpressionType -> FinalizedInference -> IO InferenceResult
finishInference inputs hideRootBindings subject inferredResult finalizedInference = do
  let expression = inferenceSubjectExpr subject
  AnalysisResult _ analyzerDiagnostics <-
    case subject of
      InferencePreparedScope _ preparedScope ->
        analyzeProgramWithInputsAndPreparedScope
          (analysisInputsForInference inputs)
          hideRootBindings
          expression
          preparedScope
      InferenceExpression expr ->
        analyzeProgramWithInputs
          (analysisInputsForInference inputs)
          hideRootBindings
          expr
  let (warnings, analysisErrors) = partition (isJust . diagnosticWarningCategory) analyzerDiagnostics
      diagnostics = CompilationDiagnostics warnings analysisErrors (finalizedTypeErrors finalizedInference) (finalizedPatternCoverageDiagnostics finalizedInference)
  expression `seq`
    inferredResult `seq`
      pure
        InferenceResult
          { inferenceResolvedExpr = expression,
            inferredDiagnosticGroups = diagnostics,
            inferredModuleInterface = finalizedModuleInterface finalizedInference
          }

-- Ordinary inference owns the finalized diagnostics before the analyzer walk,
-- so rendering thunks cannot keep the complete solver state alive. The
-- remaining result containers are materialized only to WHNF.
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
  Map.foldrWithKey (\export (ModuleValueBinding binder binding) forced -> export `seq` binder `seq` binding `seq` forced) () (interfaceValueBindings moduleInterface) `seq`
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

emptyInferenceInputs :: WarningSettings -> InferenceInputs
emptyInferenceInputs settings =
  InferenceInputs
    { inferencePublicExports = Nothing,
      inferenceWarningSettings = settings,
      inferenceExternalUses = Set.empty,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

analysisInputsForInference :: InferenceInputs -> AnalysisInputs
analysisInputsForInference inputs =
  AnalysisInputs
    { analysisWarningSettings = inferenceWarningSettings inputs,
      analysisExternalUses = inferenceExternalUses inputs,
      analysisImportedValues =
        Map.mapKeys typeEnvName (Map.map (const (AnalysisBinding Nothing True)) (inferenceImportedTypes inputs)),
      analysisForwardFunctions = Map.empty,
      analysisImportedClasses =
        Set.union
          (Set.map (resolvedAmbientName CapabilityNamespace . mkIdentifier) (inferenceImportedClassNames inputs))
          (Set.map capabilityResolvedName (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs))))
    }

inferExpressionDefault :: Expr 'Resolved -> IO InferenceResult
inferExpressionDefault =
  inferExpressionWithRequest
    InferenceRequest
      { requestedInferenceInputs = emptyInferenceInputs defaultWarningSettings,
        requestedHideRootBindings = False
      }
