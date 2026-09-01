{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile resolved modules once against explicit dependency interfaces.
module Jazz.Compiler.ModuleCompiler
  ( compilePreparedPrelude,
    compileResolvedModule,
    compileResolvedProgram,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (..),
    SignaturePayload,
    SignatureType,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactClassName,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    ModuleImportMode (..),
    exportInventory,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (coreModuleExpr),
    ImportExposure (..),
    ResolvedImport (..),
    ResolvedModule (..),
    ResolvedProgram (..),
  )
import Jazz.Compiler.ModuleIdentity (mkModulePath, renderModulePath)
import Jazz.Compiler.ModuleInterface
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
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
import Jazz.Compiler.Prelude (PreparedPrelude (..))
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    ImplMethodType (..),
    ScopeCapabilityFacts (..),
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation
import Jazz.Compiler.WarningConfig (WarningSettings)

compilePreparedPrelude :: WarningSettings -> PreparedPrelude -> IO CompiledPrelude
compilePreparedPrelude settings preparedPrelude =
  case preparedPreludeExpr preparedPrelude of
    Nothing ->
      pure
        emptyCompiledPrelude
          { compiledPreludeBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
          }
    Just preludeExpr ->
      case resolveStandaloneExprNames (preparedPreludeBuiltinMode preparedPrelude) (exportInventory []) preludeExpr of
        Left diagnostics ->
          pure
            emptyCompiledPrelude
              { compiledPreludeBuiltinMode = preparedPreludeBuiltinMode preparedPrelude,
                compiledPreludeDiagnostics = NonEmpty.toList diagnostics
              }
        Right resolvedPreludeExpr -> do
          inference <-
            inferExpressionWithInputsAndHiddenStatements
              InferenceInputs
                { inferenceBuiltinMode = preparedPreludeBuiltinMode preparedPrelude,
                  inferenceWarningSettings = settings,
                  inferenceImportedTypes = Map.empty,
                  inferenceImportedDataTypes = Map.empty,
                  inferenceImportedConstructorWitnessNames = Map.empty,
                  inferenceImportedCapabilities = emptyScopeCapabilityFacts,
                  inferenceImportedClassNames = Set.empty,
                  inferenceCurrentModulePath = Just []
                }
              (preparedPreludeHiddenStatementIndices preparedPrelude)
              resolvedPreludeExpr
          pure
            CompiledPrelude
              { compiledPreludeBuiltinMode = preparedPreludeBuiltinMode preparedPrelude,
                compiledPreludeInterface = inferredModuleInterface inference,
                compiledPreludeDiagnostics = inferredDiagnostics inference,
                compiledPreludeExpr = Just (inferredExpr inference),
                compiledPreludeRuntimeHints = inferredRuntimeTypeHints inference
              }

compileResolvedProgram :: CompileInputs -> ResolvedProgram -> IO CompiledProgram
compileResolvedProgram inputs resolvedProgram =
  {-# SCC "jazz-stage:runtime-preparation" #-}
  do
    compiledModules <- reverse . fst <$> foldModules [] Map.empty (resolvedProgramModules resolvedProgram)
    let compiledPrelude = compileInputPrelude inputs
    pure
      CompiledProgram
        { compiledProgramPrelude = compiledPrelude,
          compiledProgramEntryPath = resolvedProgramEntryPath resolvedProgram,
          compiledProgramModules = compiledModules
        }
  where
    ambientInterface = ambientPreludeInterface (compileInputPrelude inputs)
    foldModules compiledReversed compiledByPath remaining =
      case remaining of
        [] -> pure (compiledReversed, compiledByPath)
        resolvedModule : rest -> do
          compiledModule <- compileResolvedModuleWithIndex inputs ambientInterface compiledByPath resolvedModule
          foldModules
            (compiledModule : compiledReversed)
            (Map.insert (resolvedModulePath resolvedModule) (compiledDependency compiledModule) compiledByPath)
            rest

compileResolvedModule :: CompileInputs -> [CompiledModule] -> ResolvedModule -> IO CompiledModule
compileResolvedModule inputs compiledDependencies =
  compileResolvedModuleWithIndex
    inputs
    (ambientPreludeInterface (compileInputPrelude inputs))
    (buildCompiledDependencyPathIndex compiledDependencies)

compileResolvedModuleWithIndex :: CompileInputs -> ImportedInterface -> Map [Text] CompiledDependency -> ResolvedModule -> IO CompiledModule
compileResolvedModuleWithIndex inputs ambientInterface compiledDependenciesByPath resolvedModule = do
  let importedInterface =
        ambientInterface
          <> foldMap
            (uncurry dependencyImportInterface)
            [ (importDecl, dependency)
            | importDecl <- resolvedModuleImports resolvedModule,
              Just dependency <- [Map.lookup (resolvedImportPath importDecl) compiledDependenciesByPath]
            ]
      modulePath = resolvedModulePath resolvedModule
      moduleExpr = coreModuleExpr (resolvedModuleCore resolvedModule)
  inference <-
    inferExpressionWithInputs
      InferenceInputs
        { inferenceBuiltinMode = compileInputBuiltinMode inputs,
          inferenceWarningSettings = compileInputWarningSettings inputs,
          inferenceImportedTypes = interfaceTypeEnv importedInterface,
          inferenceImportedDataTypes = importedDataTypes importedInterface,
          inferenceImportedConstructorWitnessNames =
            interfaceConstructorWitnessNames importedInterface,
          inferenceImportedCapabilities = interfaceCapabilities importedInterface,
          inferenceImportedClassNames = importedClassNames importedInterface,
          inferenceCurrentModulePath = Just modulePath
        }
      moduleExpr
  pure
    CompiledModule
      { compiledModulePath = modulePath,
        compiledModuleImports = resolvedModuleImports resolvedModule,
        compiledModuleExportInventory = resolvedModuleExportInventory resolvedModule,
        compiledModuleInterface = inferredModuleInterface inference,
        compiledModuleDiagnostics = inferredDiagnostics inference,
        compiledModuleExpr = inferredExpr inference
      }

data CompiledDependency = CompiledDependency
  { dependencyCompiledModule :: CompiledModule,
    dependencyWholeInterface :: ImportedInterface
  }

compiledDependency :: CompiledModule -> CompiledDependency
compiledDependency compiledModule =
  CompiledDependency
    { dependencyCompiledModule = compiledModule,
      dependencyWholeInterface = importWholeCompiledModuleInterface compiledModule
    }

buildCompiledDependencyPathIndex :: [CompiledModule] -> Map [Text] CompiledDependency
buildCompiledDependencyPathIndex =
  Map.fromListWith (\_ firstDependency -> firstDependency)
    . map
      (\compiledModule -> (compiledModulePath compiledModule, compiledDependency compiledModule))

ambientPreludeInterface :: CompiledPrelude -> ImportedInterface
ambientPreludeInterface compiledPrelude =
  importWholeInterface AmbientPrelude (compiledPreludeInterface compiledPrelude)

dependencyImportInterface :: ResolvedImport -> CompiledDependency -> ImportedInterface
dependencyImportInterface importDecl dependency =
  case resolvedImportExposure importDecl of
    ImportAll -> dependencyWholeInterface dependency
    ImportOnly symbolNames ->
      importSelectedInterface
        (moduleOrigin (resolvedImportPath importDecl))
        Nothing
        (Just (NonEmpty.toList symbolNames))
        (compiledModuleExportInventory compiledModule)
        (compiledModuleInterface compiledModule)
    ImportQualified aliasName ->
      importSelectedInterface
        (moduleOrigin (resolvedImportPath importDecl))
        (Just aliasName)
        Nothing
        (compiledModuleExportInventory compiledModule)
        (compiledModuleInterface compiledModule)
  where
    compiledModule = dependencyCompiledModule dependency

importWholeCompiledModuleInterface :: CompiledModule -> ImportedInterface
importWholeCompiledModuleInterface compiledModule =
  importSelectedInterface
    (moduleOrigin modulePath)
    Nothing
    Nothing
    (compiledModuleExportInventory compiledModule)
    (compiledModuleInterface compiledModule)
  where
    modulePath = compiledModulePath compiledModule

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
          importedCapabilities left <> importedCapabilities right,
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

interfaceTypeEnv :: ImportedInterface -> TypeEnv
interfaceTypeEnv = importedTypes

interfaceCapabilities :: ImportedInterface -> ScopeCapabilityFacts
interfaceCapabilities = importedCapabilities

interfaceConstructorWitnessNames :: ImportedInterface -> Map ResolvedName UnresolvedName
interfaceConstructorWitnessNames = importedConstructorWitnessNames

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
          [ ( UserName (ResolvedUserName origin (moduleExportNamespace export) (mkIdentifier (moduleExportName export))),
              rebaseTypeBinding origin dataTypeNames classNames binding
            )
          | (export, binding) <- Map.toList selectedValueTypes
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
      importedClassNames = selectedClassNames
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
    importMode =
      case maybeAlias of
        Nothing -> UnqualifiedImport
        Just _ -> QualifiedAliasImport
    selectedInventory =
      visibleImportInventory
        importMode
        maybeSymbols
        publicInventory
    selectedValueTypes =
      Map.filterWithKey
        (\export _ -> inventoryHasExport export selectedInventory)
        (interfaceValueTypes moduleInterface)
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

moduleOrigin :: [Text] -> ResolvedNameOrigin
moduleOrigin =
  maybe AmbientPrelude (ImportedModule . mkModulePath . fmap mkIdentifier)
    . NonEmpty.nonEmpty

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
    rebasePrimitiveConstraint primitiveConstraint =
      case primitiveConstraint of
        TypeSchemeNumericConstraint numericConstraint argumentType ->
          TypeSchemeNumericConstraint numericConstraint (rebaseExpressionType origin dataTypeNames argumentType)
        TypeSchemeStrictEqualityConstraint argumentType ->
          TypeSchemeStrictEqualityConstraint (rebaseExpressionType origin dataTypeNames argumentType)

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
