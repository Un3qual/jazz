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

import Control.DeepSeq (rnf)
import Data.List (partition, sortOn, union)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CoreNode (..),
    CorePhase (..),
    Expr (EBlock),
    coreNodeId,
    expressionNode,
  )
import Jazz.Compiler.Analyzer (AnalysisBinding (..), AnalysisInputs (..), AnalysisResult (..), analyzeProgramWithInputs, analyzeProgramWithInputsAndPreparedScope)
import Jazz.Compiler.CoreIdentity (ResolvedReference (LexicalReference), capabilityExportName, capabilityResolvedName, resolvedNodeOwner)
import Jazz.Compiler.Diagnostics (CompilationDiagnostics (..), Diagnostic, diagnosticWarningCategory, isErrorDiagnostic)
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
import Jazz.Compiler.SemanticDeclarations (DeclarationVariable, filterScopeCapabilities)
import Jazz.Compiler.SemanticFacts
  ( SemanticFactInvariantFailure (..),
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
-- resolved root supplies source ownership and the caller selects warning policy,
-- while dependency availability and diagnostic accumulation belong to the driver.
analyzeModule :: CompileInputs -> Bool -> ImportedInterface -> CoreModule 'Resolved -> IO (InferenceResult, Maybe (CoreModule 'Analyzed))
analyzeModule inputs hideRootBindings importedInterface resolvedModule = do
  let modulePath = coreModulePath resolvedModule
  (inference, attachment) <-
    analyzeExpressionWithInputs
      (moduleInferenceInputs inputs resolvedModule importedInterface)
      hideRootBindings
      (coreModuleExpr resolvedModule)
  maybeAnalyzedExpression <- checkedSemanticFacts modulePath attachment
  maybeAnalyzedModule <-
    traverse
      ( \analyzedExpression ->
          checkedSemanticFacts
            modulePath
            (analyzedModuleFromExpression resolvedModule inference analyzedExpression)
      )
      maybeAnalyzedExpression
  case maybeAnalyzedModule of
    Just analyzed
      | moduleInterfaceExportInventory (ModuleGraph.analyzedModuleInterface (coreModuleFacts analyzed)) /= ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule) ->
          fail ("typed module exports disagree with resolution in " <> Text.unpack (renderModulePath modulePath) <> ": expected " <> show (ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule)) <> ", got " <> show (moduleInterfaceExportInventory (ModuleGraph.analyzedModuleInterface (coreModuleFacts analyzed))))
    _ -> pure ()
  pure (inference, maybeAnalyzedModule)

checkedSemanticFacts :: (Show failure) => ModulePath -> Either failure value -> IO value
checkedSemanticFacts modulePath result = case result of
  Left failure -> fail ("semantic fact invariant failure in " <> Text.unpack (renderModulePath modulePath) <> ": " <> show failure)
  Right value -> pure value

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
      inferenceCurrentModulePath = case resolvedNodeOwner (coreNodeFacts (ModuleGraph.coreModuleBodyNode resolvedModule)) of StandaloneSourceUnit _ -> Nothing; _ -> Just modulePath
    }
  where
    modulePath = coreModulePath resolvedModule

analyzedModuleFromExpression :: CoreModule 'Resolved -> InferenceResult -> Expr 'Analyzed -> Either SemanticFactInvariantFailure (CoreModule 'Analyzed)
analyzedModuleFromExpression resolvedModule inference analyzedExpression =
  case analyzedExpression of
    EBlock bodyNode statements ->
      pure
        ( ModuleGraph.CoreModule
            { ModuleGraph.coreModuleIdentity = coreModuleIdentity resolvedModule,
              ModuleGraph.coreModuleBodyNode = bodyNode,
              ModuleGraph.coreModuleImports = map analyzedImport (coreModuleImports resolvedModule),
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
        moduleInterface = inferredModuleInterface inference
    _ -> Left (AnalyzedModuleRootNotBlock (coreNodeId (expressionNode analyzedExpression)))

-- Imports already own their resolved target; no inference output is needed to
-- publish their analyzed declaration facts.
analyzedImport :: ModuleImport 'Resolved -> ModuleImport 'Analyzed
analyzedImport importDecl =
  ModuleGraph.ModuleImport
    { ModuleGraph.moduleImportNode =
        case ModuleGraph.moduleImportNode importDecl of
          CoreNode nodeId spanValue resolution ->
            CoreNode nodeId spanValue (StatementFacts resolution [] Map.empty (ImportDeclaration (ModuleGraph.importedModule importDecl))),
      ModuleGraph.importedModule = ModuleGraph.importedModule importDecl,
      ModuleGraph.importExposure = ModuleGraph.importExposure importDecl
    }

dependencyImportInterface :: ValidatedImportScope -> ModulePath -> ModuleInterface -> ImportedInterface
dependencyImportInterface scope path interface =
  -- Prefer a qualified witness when available: local declarations can shadow
  -- the unqualified spelling. Alias order must not depend on import order.
  foldMap (\(alias, selected) -> importSelectedInterface (ImportedModule path) alias selected interface) $
    sortOn (\(alias, _) -> (isNothing alias, alias)) (dependencyImportViews path scope)

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
          [ ( TypeEnvKey (LexicalReference binder) (importedName export),
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
    capabilities = interfaceCapabilities moduleInterface
    selectedCapabilities =
      filterScopeCapabilities ((`Set.member` selectedClassNames) . capabilityExportName) capabilities

analyzeResolvedExpression ::
  WarningSettings ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed))
    )
analyzeResolvedExpression settings = analyzeExpressionWithInputs (emptyInferenceInputs settings) False

inferExpressionWithInputs :: InferenceInputs -> Expr 'Resolved -> IO InferenceResult
inferExpressionWithInputs inputs expr =
  (\(result, _, _) -> result) <$> inferExpressionWithState inputs False expr

inferExpressionWithState :: InferenceInputs -> Bool -> Expr 'Resolved -> IO (InferenceResult, InferState, CheckedExpr)
inferExpressionWithState inputs hideRootBindings expr =
  {-# SCC "jazz-stage:type-inference" #-}
  let (inferredResult, finalState, inferenceSubject) =
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
                hideRootBindings
                inferenceSubject
                (checkedExprType inferredResult)
                finalizedInference
            pure (inference, finalState, inferredResult)

analyzeExpressionWithInputs ::
  InferenceInputs ->
  Bool ->
  Expr 'Resolved ->
  IO
    ( InferenceResult,
      Either
        (NonEmpty.NonEmpty SemanticFactInvariantFailure)
        (Maybe (Expr 'Analyzed))
    )
analyzeExpressionWithInputs inputs hideRootBindings expression = do
  (inference, finalState, checked) <-
    inferExpressionWithState inputs hideRootBindings expression
  if any isErrorDiagnostic (inferredDiagnostics inference)
    then pure (inference, Right Nothing)
    else
      pure
        ( inference,
          Just <$> finalizeCheckedExpression finalState checked
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
  rnf (finalizedTypeErrors finalizedInference) `seq`
    rnf (finalizedPatternCoverageDiagnostics finalizedInference) `seq`
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
      forceMapEntriesWhnf (scopeClassFacts capabilities) `seq`
        forceSetEntriesWhnf (scopeGeneratedEqualityClassFacts capabilities) `seq`
          forceSetEntriesWhnf (scopeConcreteImplFacts capabilities) `seq`
            forceMapEntriesWhnf (scopeClassMethodSignatures capabilities) `seq`
              forceMapEntriesWhnf (scopeConcreteImplMethods capabilities)
  where
    capabilities = interfaceCapabilities moduleInterface

forceMapEntriesWhnf :: Map key value -> ()
forceMapEntriesWhnf = Map.foldrWithKey (\key value forced -> key `seq` value `seq` forced) ()

forceSetEntriesWhnf :: Set value -> ()
forceSetEntriesWhnf = Set.foldr (\value forced -> value `seq` forced) ()

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
      analysisImportedClasses =
        Set.union
          (Set.map (resolvedAmbientName CapabilityNamespace . mkIdentifier) (inferenceImportedClassNames inputs))
          (Set.map capabilityResolvedName (Map.keysSet (scopeClassFacts (inferenceImportedCapabilities inputs))))
    }

inferExpressionDefault :: Expr 'Resolved -> IO InferenceResult
inferExpressionDefault = inferExpressionWithInputs (emptyInferenceInputs defaultWarningSettings)
