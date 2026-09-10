{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Single-module semantics and the imported facts required at that boundary.
module Jazz.Compiler.ModuleAnalysis
  ( ImportedInterface,
    analyzeModule,
    dependencyImportInterface,
    importWholeInterface,
    moduleBinderInventory,
    moduleEvidenceCandidates,
  )
where

import Data.Bifunctor (bimap)
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
    SignaturePayload,
    SignatureType,
    Statement (..),
    expressionNode,
    statementNode,
  )
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactClassName,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportOrigin,
    selectExportNames,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule,
    ImportExposure (..),
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
    modulePathTextSegments,
    moduleQualifierIdentifier,
    renderModulePath,
  )
import Jazz.Compiler.ModuleInterface
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (CapabilityNamespace, ConstructorNamespace, TypeNamespace, ValueNamespace),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    UnresolvedName,
    identifierText,
    mkIdentifier,
    qualifiedName,
    renderName,
    sourceName,
  )
import Jazz.Compiler.SemanticFacts
  ( CoreBinderId,
    CoreNodeId,
    SemanticFactInvariantFailure (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    analyzeExpressionWithInputs,
  )
import Jazz.Compiler.TypeInference.Evidence (implementationEvidenceCandidatesInModule)
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.State
  ( ImplementationEvidenceCandidate (..),
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    ImplMethodType (..),
    SchemeConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

-- | Analyze one resolved module against its complete imported interface. The
-- caller chooses the source owner and hidden statements (the prelude uses both),
-- while dependency availability and diagnostic accumulation belong to the driver.
analyzeModule :: CompileInputs -> (ModulePath -> SourceUnitOwner) -> Set.Set Int -> ImportedInterface -> CoreModule 'Resolved -> IO (InferenceResult, Maybe (CoreModule 'Analyzed))
analyzeModule inputs owner hiddenStatements importedInterface resolvedModule = do
  let modulePath = coreModulePath resolvedModule
  (inference, attachment) <-
    analyzeExpressionWithInputs
      modulePath
      (moduleStatementFactSeeds resolvedModule)
      (importedBinderIds importedInterface)
      (Map.unionWith (<>) (moduleEvidenceCandidates owner resolvedModule) (importedEvidenceCandidates importedInterface))
      (moduleInferenceInputs inputs modulePath importedInterface)
      hiddenStatements
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

moduleStatementFactSeeds :: CoreModule 'Resolved -> [(CoreNodeId, StatementDeclarationFact)]
moduleStatementFactSeeds = map importSeed . coreModuleImports
  where
    importSeed importDecl =
      ( coreNodeId (ModuleGraph.moduleImportNode importDecl),
        ImportDeclaration
          (NonEmpty.toList (modulePathTextSegments (ModuleGraph.importedModule importDecl)))
      )

moduleInferenceInputs :: CompileInputs -> ModulePath -> ImportedInterface -> InferenceInputs
moduleInferenceInputs inputs modulePath importedInterface =
  InferenceInputs
    { inferenceWarningSettings = compileInputWarningSettings inputs,
      inferenceImportedTypes = importedTypes importedInterface,
      inferenceImportedDataTypes = importedDataTypes importedInterface,
      inferenceImportedConstructorWitnessNames = importedConstructorWitnessNames importedInterface,
      inferenceImportedCapabilities = importedCapabilities importedInterface,
      inferenceImportedClassNames = importedClassNames importedInterface,
      inferenceCurrentModulePath = Just (modulePathTexts modulePath)
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
                    ModuleGraph.analyzedModuleDiagnostics = inferredDiagnostics inference
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
    CoreNode nodeId spanValue () ->
      case Map.lookup nodeId factsByNode of
        Nothing -> Left (MissingStatementFacts nodeId)
        Just facts ->
          Right
            ModuleGraph.ModuleImport
              { ModuleGraph.moduleImportNode = CoreNode nodeId spanValue facts,
                ModuleGraph.importedModule = ModuleGraph.importedModule importDecl,
                ModuleGraph.importExposure = ModuleGraph.importExposure importDecl
              }

moduleBinderInventory :: CoreModule 'Analyzed -> Map ModuleExport CoreBinderId
moduleBinderInventory coreModule =
  Map.fromList (foldMap statementBinders (ModuleGraph.coreModuleStatements coreModule))
  where
    statementBinders :: Statement 'Analyzed -> [(ModuleExport, CoreBinderId)]
    statementBinders statement =
      case statement of
        AST.SLet node name _ -> binding ValueNamespace name node
        AST.SSignature node name _ -> binding ValueNamespace name node
        AST.SData _ _ _ constructors ->
          foldMap
            (\(AST.DataConstructor node name _) -> binding ConstructorNamespace name node)
            constructors
        _ -> []

    binding :: NameNamespace -> ResolvedName -> CoreNode 'Analyzed 'StatementSort -> [(ModuleExport, CoreBinderId)]
    binding namespace name (CoreNode _ _ facts) =
      case statementBinderIds facts of
        [binderId] -> [(ModuleExport namespace (identifierText name), binderId)]
        _ -> []

moduleEvidenceCandidates :: (ModulePath -> SourceUnitOwner) -> CoreModule 'Resolved -> Map Text [ImplementationEvidenceCandidate]
moduleEvidenceCandidates owner coreModule =
  implementationEvidenceCandidatesInModule
    (owner (coreModulePath coreModule))
    (coreModuleExpr coreModule)

dependencyImportInterface :: ModuleImport 'Resolved -> (ModuleExportInventory, ImportedInterface) -> ImportedInterface
dependencyImportInterface importDecl (publicInventory, canonical) =
  canonical
    { importedTypes = Map.restrictKeys (importedTypes canonical) selectedNames,
      importedConstructorWitnessNames =
        Map.fromList
          [ (resolvedExport entry, sourceConstructorName (moduleExportName entry))
          | entry <- Set.toList (exportInventoryEntries selectedInventory),
            moduleExportNamespace entry == ConstructorNamespace
          ],
      importedCapabilities = selectedCapabilities,
      importedClassNames = case ModuleGraph.importExposure importDecl of
        ImportQualifiedOnly _ -> Set.empty
        _ -> exportNamesInNamespace CapabilityNamespace selectedInventory,
      importedBinderIds = Map.restrictKeys (importedBinderIds canonical) selectedNames,
      importedEvidenceCandidates = Map.filterWithKey (methodUsesClass selectedClasses) (importedEvidenceCandidates canonical)
    }
  where
    dependencyPath = ModuleGraph.importedModule importDecl
    selectedInventory = selectExportNames symbols publicInventory
    symbols = case ModuleGraph.importExposure importDecl of
      ImportOnlyUnqualified names -> Just (map identifierText (NonEmpty.toList names))
      _ -> Nothing
    resolvedExport entry =
      UserName
        ( ResolvedUserName
            (ImportedModule (exportOrigin dependencyPath entry publicInventory))
            (moduleExportNamespace entry)
            (mkIdentifier (moduleExportName entry))
        )
    selectedNames = Set.map resolvedExport (exportInventoryEntries selectedInventory)
    selectedClasses =
      Set.fromList
        [ renderName (resolvedExport entry)
        | entry <- Set.toList (exportInventoryEntries selectedInventory),
          moduleExportNamespace entry == CapabilityNamespace
        ]
    sourceConstructorName name = case ModuleGraph.importExposure importDecl of
      ImportQualifiedOnly qualifier -> qualifiedName (moduleQualifierIdentifier qualifier) (mkIdentifier name)
      _ -> sourceName (mkIdentifier name)
    facts = importedCapabilities canonical
    selectedCapabilities =
      facts
        { scopeClassFacts = Map.restrictKeys (scopeClassFacts facts) selectedClasses,
          scopeGeneratedEqualityClassFacts = Set.intersection selectedClasses (scopeGeneratedEqualityClassFacts facts),
          scopeConcreteImplFacts = Set.filter (factUsesClass selectedClasses) (scopeConcreteImplFacts facts),
          scopeClassMethodSignatures = Map.filterWithKey (methodUsesClass selectedClasses) (scopeClassMethodSignatures facts),
          scopeConcreteImplMethods = Map.filterWithKey (methodUsesClass selectedClasses) (scopeConcreteImplMethods facts)
        }

data ImportedInterface = ImportedInterface
  { importedTypes :: TypeEnv,
    importedDataTypes :: Map Text DataTypeBinding,
    importedConstructorWitnessNames :: Map ResolvedName UnresolvedName,
    importedCapabilities :: ScopeCapabilityFacts,
    importedClassNames :: Set.Set Text,
    importedBinderIds :: Map ResolvedName CoreBinderId,
    importedEvidenceCandidates :: Map Text [ImplementationEvidenceCandidate]
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
                    -- Equal targets are not equal implementations. Preserve
                    -- distinct method identities so inference reports ambiguity
                    -- before semantic evidence attachment.
                    Map.union
                      (Map.map (map (ImplMethodType . implementationCandidateTarget)) evidenceCandidates)
                      (Map.unionWith union (scopeConcreteImplMethods leftFacts) (scopeConcreteImplMethods rightFacts))
                },
        importedClassNames = Set.union (importedClassNames left) (importedClassNames right),
        importedBinderIds = Map.union (importedBinderIds left) (importedBinderIds right),
        importedEvidenceCandidates = evidenceCandidates
      }
    where
      evidenceCandidates = Map.unionWith union (importedEvidenceCandidates left) (importedEvidenceCandidates right)

instance Monoid ImportedInterface where
  mempty =
    ImportedInterface
      { importedTypes = Map.empty,
        importedDataTypes = Map.empty,
        importedConstructorWitnessNames = Map.empty,
        importedCapabilities = mempty,
        importedClassNames = Set.empty,
        importedBinderIds = Map.empty,
        importedEvidenceCandidates = Map.empty
      }

-- | Rebase owned declarations once. Dependency boundaries subsequently select
-- this canonical metadata without assigning the facade as a new owner.
importWholeInterface :: ResolvedNameOrigin -> Map ModuleExport CoreBinderId -> Map Text [ImplementationEvidenceCandidate] -> ModuleInterface -> ImportedInterface
importWholeInterface origin binderIds evidenceCandidates moduleInterface =
  ImportedInterface
    { importedTypes =
        Map.fromList
          [ (importedName entry, rebaseTypeBinding origin dataTypeNames classNames binding)
          | (entry, binding) <- Map.toList (interfaceValueTypes moduleInterface)
          ],
      importedDataTypes =
        Map.fromList
          [ (qualifiedKey origin name, rebaseDataTypeBinding origin dataTypeNames classNames binding)
          | (name, binding) <- Map.toList (interfaceDataTypes moduleInterface)
          ],
      importedConstructorWitnessNames =
        Map.fromList
          [ (importedName entry, sourceName (mkIdentifier (moduleExportName entry)))
          | entry <- Map.keys (interfaceValueTypes moduleInterface),
            moduleExportNamespace entry == ConstructorNamespace
          ],
      importedCapabilities = rebaseCapabilityFacts origin dataTypeNames classNames capabilities,
      importedClassNames = classNames,
      importedBinderIds = Map.mapKeys importedName binderIds,
      importedEvidenceCandidates =
        Map.fromList
          [ (rebaseMethodKey origin classNames key, map (rebaseEvidenceCandidate origin dataTypeNames classNames) candidates)
          | (key, candidates) <- Map.toList evidenceCandidates
          ]
    }
  where
    importedName entry = UserName (ResolvedUserName origin (moduleExportNamespace entry) (mkIdentifier (moduleExportName entry)))
    dataTypeNames = Map.keysSet (interfaceDataTypes moduleInterface)
    classNames = Map.keysSet (interfaceClassFacts moduleInterface)
    capabilities =
      ScopeCapabilityFacts
        { scopeClassFacts = interfaceClassFacts moduleInterface,
          scopeGeneratedEqualityClassFacts = interfaceGeneratedEqualityClassFacts moduleInterface,
          scopeConcreteImplFacts = interfaceConcreteImplFacts moduleInterface,
          scopeClassMethodSignatures = interfaceClassMethods moduleInterface,
          scopeConcreteImplMethods = interfaceConcreteImplMethods moduleInterface
        }

qualifiedKey :: ResolvedNameOrigin -> Text -> Text
qualifiedKey origin name =
  case origin of
    ImportedModule modulePath -> renderModulePath modulePath <> "::" <> name
    _ -> name

modulePathTexts :: ModulePath -> [Text]
modulePathTexts = NonEmpty.toList . modulePathTextSegments

factUsesClass :: Set.Set Text -> ConcreteImplFact -> Bool
factUsesClass classNames fact = Set.member (concreteImplFactClassName fact) classNames

methodUsesClass :: Set.Set Text -> Text -> value -> Bool
methodUsesClass classNames methodKey _ =
  any (\className -> (className <> "::") `Text.isPrefixOf` methodKey) (Set.toList classNames)

rebaseTypeBinding :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> TypeBinding -> TypeBinding
rebaseTypeBinding origin dataTypeNames classNames binding =
  case binding of
    PlainTypeBinding expressionType ->
      PlainTypeBinding (rebaseExpressionType origin dataTypeNames expressionType)
    SchemeTypeBinding typeScheme ->
      SchemeTypeBinding (rebaseTypeScheme origin dataTypeNames classNames typeScheme)
    BuiltinAliasTypeBinding {} -> binding
    BuiltinOperatorAliasTypeBinding {} -> binding
    OperatorAliasSchemeTypeBinding operatorSymbol typeScheme ->
      OperatorAliasSchemeTypeBinding operatorSymbol (rebaseTypeScheme origin dataTypeNames classNames typeScheme)
    ConstructorTypeBinding typeName parameters arguments ->
      ConstructorTypeBinding
        (rebaseKnownName origin TypeNamespace dataTypeNames typeName)
        parameters
        (map (rebaseConstructorArgument origin dataTypeNames) arguments)

rebaseDataTypeBinding :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> DataTypeBinding -> DataTypeBinding
rebaseDataTypeBinding origin dataTypeNames _ (DataTypeBinding parameters constructors) =
  DataTypeBinding parameters (map (map (rebaseConstructorArgument origin dataTypeNames)) constructors)

rebaseConstructorArgument :: ResolvedNameOrigin -> Set.Set Text -> ConstructorArgumentType -> ConstructorArgumentType
rebaseConstructorArgument origin dataTypeNames argument =
  case argument of
    ConstructorArgumentMonomorphic SemanticVariable {} ->
      ConstructorArgumentFresh
    ConstructorArgumentMonomorphic expressionType ->
      ConstructorArgumentMonomorphic (rebaseExpressionType origin dataTypeNames expressionType)
    ConstructorArgumentParameter {} -> argument
    ConstructorArgumentStructured fieldType ->
      ConstructorArgumentStructured
        (rebaseSignatureTypeNames origin dataTypeNames fieldType)
    ConstructorArgumentFresh -> argument

rebaseExpressionType :: ResolvedNameOrigin -> Set.Set Text -> ExpressionType -> ExpressionType
rebaseExpressionType origin dataTypeNames expressionType =
  case expressionType of
    SemanticList elementType -> SemanticList (rebaseExpressionType origin dataTypeNames elementType)
    SemanticTuple elementTypes -> SemanticTuple (map (rebaseExpressionType origin dataTypeNames) elementTypes)
    SemanticData typeName arguments ->
      SemanticData
        (rebaseKnownName origin TypeNamespace dataTypeNames typeName)
        (map (rebaseExpressionType origin dataTypeNames) arguments)
    SemanticFunction argumentType resultType ->
      SemanticFunction
        (rebaseExpressionType origin dataTypeNames argumentType)
        (rebaseExpressionType origin dataTypeNames resultType)
    _ -> expressionType

rebaseTypeScheme :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> TypeScheme -> TypeScheme
rebaseTypeScheme origin dataTypeNames classNames typeScheme =
  typeScheme
    { schemeClassConstraints = map rebaseSchemeConstraint (schemeClassConstraints typeScheme),
      schemePrimitiveConstraints = map rebasePrimitiveConstraint (schemePrimitiveConstraints typeScheme),
      schemeDefiningCapabilities = rebaseCapabilityFacts origin dataTypeNames classNames (schemeDefiningCapabilities typeScheme),
      schemeResultType = rebaseExpressionType origin dataTypeNames (schemeResultType typeScheme)
    }
  where
    rebaseSchemeConstraint constraint =
      case constraint of
        TypeSchemeConstraint capabilityName argumentType ->
          TypeSchemeConstraint (rebaseKnownText origin classNames capabilityName) (rebaseExpressionType origin dataTypeNames argumentType)
        TypeSchemeInferredConstraint capabilityName argumentType ->
          TypeSchemeInferredConstraint (rebaseKnownText origin classNames capabilityName) (rebaseExpressionType origin dataTypeNames argumentType)
        TypeSchemeMethodConstraint capabilityName methodKey argumentType ->
          TypeSchemeMethodConstraint
            (rebaseKnownText origin classNames capabilityName)
            (rebaseMethodKey origin classNames methodKey)
            (rebaseExpressionType origin dataTypeNames argumentType)
    rebasePrimitiveConstraint = fmap (rebaseExpressionType origin dataTypeNames)

rebaseCapabilityFacts :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ScopeCapabilityFacts -> ScopeCapabilityFacts
rebaseCapabilityFacts origin dataTypeNames classNames facts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.mapKeys (rebaseKnownText origin classNames) (scopeClassFacts facts),
      scopeGeneratedEqualityClassFacts = Set.map (rebaseKnownText origin classNames) (scopeGeneratedEqualityClassFacts facts),
      scopeConcreteImplFacts = Set.map (rebaseConcreteImplFact origin dataTypeNames classNames) (scopeConcreteImplFacts facts),
      scopeClassMethodSignatures =
        Map.fromList
          [ (rebaseMethodKey origin classNames methodKey, rebaseClassMethod origin dataTypeNames classNames methodType)
          | (methodKey, methodType) <- Map.toList (scopeClassMethodSignatures facts)
          ],
      scopeConcreteImplMethods =
        Map.fromList
          [ (rebaseMethodKey origin classNames methodKey, map (rebaseImplMethod origin dataTypeNames classNames) methodTypes)
          | (methodKey, methodTypes) <- Map.toList (scopeConcreteImplMethods facts)
          ]
    }

rebaseClassMethod :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ClassMethodType -> ClassMethodType
rebaseClassMethod origin dataTypeNames classNames (ClassMethodType parameter payload) =
  ClassMethodType parameter (rebaseSignaturePayload origin dataTypeNames classNames payload)

rebaseImplMethod :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ImplMethodType -> ImplMethodType
rebaseImplMethod origin dataTypeNames _ (ImplMethodType target) =
  ImplMethodType (rebaseSignatureTypeNames origin dataTypeNames target)

rebaseEvidenceCandidate :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ImplementationEvidenceCandidate -> ImplementationEvidenceCandidate
rebaseEvidenceCandidate origin dataTypeNames classNames candidate =
  candidate
    { implementationCandidateCapability = rebaseKnownName origin CapabilityNamespace classNames (implementationCandidateCapability candidate),
      implementationCandidateTarget = rebaseSignatureTypeNames origin dataTypeNames (implementationCandidateTarget candidate)
    }

rebaseSignaturePayload :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> SignaturePayload 'Resolved -> SignaturePayload 'Resolved
rebaseSignaturePayload origin dataTypeNames classNames payload =
  case payload of
    TypeRepresentation.SignatureType signatureType ->
      TypeRepresentation.SignatureType (rebaseSignatureTypeNames origin dataTypeNames signatureType)
    TypeRepresentation.ConstrainedSignature constraints signatureType ->
      TypeRepresentation.ConstrainedSignature
        [ TypeRepresentation.SignatureConstraint
            (rebaseKnownName origin CapabilityNamespace classNames capabilityName)
            (map (rebaseSignatureTypeNames origin dataTypeNames) arguments)
        | TypeRepresentation.SignatureConstraint capabilityName arguments <- constraints
        ]
        (rebaseSignatureTypeNames origin dataTypeNames signatureType)
    TypeRepresentation.UnsupportedSignature tokens ->
      TypeRepresentation.UnsupportedSignature
        (fmap (rebaseKnownName origin TypeNamespace dataTypeNames) <$> tokens)

rebaseConcreteImplFact ::
  ResolvedNameOrigin ->
  Set.Set Text ->
  Set.Set Text ->
  ConcreteImplFact ->
  ConcreteImplFact
rebaseConcreteImplFact origin dataTypeNames classNames (ConcreteImplFact capabilityName argument) =
  ConcreteImplFact
    (rebaseKnownName origin CapabilityNamespace classNames capabilityName)
    (rebaseSignatureTypeNames origin dataTypeNames argument)

rebaseSignatureTypeNames :: ResolvedNameOrigin -> Set.Set Text -> SignatureType 'Resolved -> SignatureType 'Resolved
rebaseSignatureTypeNames origin dataTypeNames =
  bimap rebaseTypeName rebaseTypeName
  where
    rebaseTypeName = rebaseKnownName origin TypeNamespace dataTypeNames

rebaseKnownName :: ResolvedNameOrigin -> NameNamespace -> Set.Set Text -> ResolvedName -> ResolvedName
rebaseKnownName origin namespace knownNames name =
  case name of
    UserName (ResolvedUserName CurrentModule _ identifier)
      | Set.member (identifierText identifier) knownNames ->
          UserName (ResolvedUserName origin namespace identifier)
    _ -> name

rebaseKnownText :: ResolvedNameOrigin -> Set.Set Text -> Text -> Text
rebaseKnownText origin knownNames name
  | Set.member name knownNames = qualifiedKey origin name
  | otherwise = name

rebaseMethodKey :: ResolvedNameOrigin -> Set.Set Text -> Text -> Text
rebaseMethodKey origin classNames methodKey =
  case [className | className <- Set.toList classNames, (className <> "::") `Text.isPrefixOf` methodKey] of
    className : _ -> qualifiedKey origin className <> Text.drop (Text.length className) methodKey
    [] -> methodKey
