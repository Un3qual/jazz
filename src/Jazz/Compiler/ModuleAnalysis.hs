{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Single-module semantics and the imported facts required at that boundary.
module Jazz.Compiler.ModuleAnalysis
  ( ImportedInterface,
    analyzeModule,
    dependencyImportInterface,
    importWholeInterface,
  )
where

import Data.List (union)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CoreNode (..),
    CorePhase (..),
    CoreSort (StatementSort),
    Expr (EBlock),
    expressionNode,
    statementNode,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactCapability,
  )
import Jazz.Compiler.CoreIdentity (CapabilityMethodKey, ResolvedReference (LexicalReference), capabilityExportName)
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
    sourceName,
  )
import Jazz.Compiler.SemanticDeclarations (DeclarationVariable)
import Jazz.Compiler.SemanticFacts
  ( CoreNodeId,
    SemanticFactInvariantFailure (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    analyzeExpressionWithInputs,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ScopeCapabilityFacts (..),
    SemanticBinding,
    TypeEnvKey (..),
  )

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
