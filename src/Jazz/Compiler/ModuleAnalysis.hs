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
    SignatureType,
    expressionNode,
    statementNode,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactClassName,
  )
import Jazz.Compiler.CoreIdentity (ResolvedReference (LexicalReference))
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    exportNamesInNamespace,
    inventoryHasExport,
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
    NameNamespace (CapabilityNamespace, ConstructorNamespace, TypeNamespace),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    UnresolvedName,
    identifierText,
    mkIdentifier,
    qualifiedName,
    sourceName,
  )
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
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ImplMethodType (..),
    SchemeConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeEnvKey (..),
    TypeScheme (..),
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
      ((moduleInferenceInputs inputs modulePath importedInterface) {inferenceCurrentModulePath = case owner modulePath of StandaloneSourceUnit _ -> Nothing; _ -> Just (modulePathTexts modulePath)})
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
        ImportDeclaration
          (NonEmpty.toList (modulePathTextSegments (ModuleGraph.importedModule importDecl)))
      )

moduleInferenceInputs :: CompileInputs -> ModulePath -> ImportedInterface -> InferenceInputs
moduleInferenceInputs inputs modulePath importedInterface =
  InferenceInputs
    { inferenceWarningSettings = compileInputWarningSettings inputs,
      inferenceExternalUses = compileInputExternalUses inputs,
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

dependencyImportInterface :: ModuleImport 'Resolved -> (ModuleExportInventory, ModuleInterface) -> ImportedInterface
dependencyImportInterface importDecl (publicInventory, moduleInterface) =
  case ModuleGraph.importExposure importDecl of
    ImportAllUnqualified ->
      importSelectedInterface
        (moduleOrigin (ModuleGraph.importedModule importDecl))
        Nothing
        Nothing
        publicInventory
        moduleInterface
    ImportOnlyUnqualified symbolNames ->
      importSelectedInterface
        (moduleOrigin (ModuleGraph.importedModule importDecl))
        Nothing
        (Just (map identifierText (NonEmpty.toList symbolNames)))
        publicInventory
        moduleInterface
    ImportQualifiedOnly qualifier ->
      importSelectedInterface
        (moduleOrigin (ModuleGraph.importedModule importDecl))
        (Just (identifierText (moduleQualifierIdentifier qualifier)))
        Nothing
        publicInventory
        moduleInterface

data ImportedInterface = ImportedInterface
  { importedTypes :: TypeEnv,
    importedDataTypes :: Map Text DataTypeBinding,
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
    Nothing
    (moduleInterfaceExportInventory moduleInterface)
    moduleInterface

importSelectedInterface :: ResolvedNameOrigin -> Maybe Text -> Maybe [Text] -> ModuleExportInventory -> ModuleInterface -> ImportedInterface
importSelectedInterface origin maybeAlias maybeSymbols publicInventory moduleInterface =
  ImportedInterface
    { importedTypes =
        Map.fromList
          [ ( TypeEnvKey (LexicalReference binder) (UserName (ResolvedUserName origin (moduleExportNamespace export) (mkIdentifier (moduleExportName export)))),
              rebaseTypeBinding origin dataTypeNames classNames binding
            )
          | (export, ModuleValueBinding binder binding) <- Map.toList selectedValueTypes
          ],
      importedDataTypes =
        Map.fromList
          [ ( qualifiedKey origin dataTypeName,
              rebaseDataTypeBinding origin dataTypeNames classNames dataType
            )
          | (dataTypeName, dataType) <- Map.toList (interfaceDataTypes moduleInterface)
          ],
      importedConstructorWitnessNames =
        Map.fromList
          [ (importedName export, sourceConstructorName export)
          | export <- Map.keys selectedValueTypes,
            moduleExportNamespace export == ConstructorNamespace
          ],
      importedCapabilities =
        rebaseCapabilityFacts origin dataTypeNames classNames selectedCapabilities,
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

    dataTypeNames = Map.keysSet (interfaceDataTypes moduleInterface)
    classNames = Map.keysSet (interfaceClassFacts moduleInterface)
    selectedInventory =
      selectExportNames
        maybeSymbols
        publicInventory
    selectedValueTypes =
      Map.filterWithKey
        (\export _ -> inventoryHasExport export selectedInventory)
        (interfaceValueBindings moduleInterface)
    selectedClassNames = exportNamesInNamespace CapabilityNamespace selectedInventory
    selectedClassFacts =
      Map.restrictKeys
        (interfaceClassFacts moduleInterface)
        selectedClassNames
    selectedCapabilities =
      ScopeCapabilityFacts
        { scopeClassFacts = selectedClassFacts,
          scopeGeneratedEqualityClassFacts =
            Set.filter
              (`Set.member` selectedClassNames)
              (interfaceGeneratedEqualityClassFacts moduleInterface),
          scopeConcreteImplFacts =
            Set.filter (factUsesClass selectedClassNames) (interfaceConcreteImplFacts moduleInterface),
          scopeClassMethodSignatures =
            Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceClassMethods moduleInterface),
          scopeConcreteImplMethods =
            Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceConcreteImplMethods moduleInterface)
        }

qualifiedKey :: ResolvedNameOrigin -> Text -> Text
qualifiedKey origin name =
  case origin of
    ImportedModule modulePath -> renderModulePath modulePath <> "::" <> name
    _ -> name

moduleOrigin :: ModulePath -> ResolvedNameOrigin
moduleOrigin = ImportedModule

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
    ConstructorArgumentType fieldType ->
      ConstructorArgumentType (rebaseExpressionType origin dataTypeNames fieldType)
    ConstructorArgumentFresh -> argument

rebaseExpressionType :: ResolvedNameOrigin -> Set.Set Text -> SemanticType ResolvedName variable -> SemanticType ResolvedName variable
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
rebaseClassMethod origin dataTypeNames _ (ClassMethodType parameter methodType) =
  ClassMethodType parameter (rebaseExpressionType origin dataTypeNames methodType)

rebaseImplMethod :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ImplMethodType -> ImplMethodType
rebaseImplMethod origin dataTypeNames _ method =
  method {implMethodTarget = rebaseExpressionType origin dataTypeNames (implMethodTarget method)}

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
