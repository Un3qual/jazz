{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile resolved modules once against explicit dependency interfaces.
module Jazz.Compiler.ModuleCompiler
  ( CompiledModule,
    CompiledProgram,
    compilePreparedPrelude,
    compileResolvedModule,
    compileResolvedProgram,
    compiledModuleErrors,
    compiledModuleDiagnostics,
    compiledModuleExportInventory,
    compiledModuleExpr,
    compiledModuleImports,
    compiledModuleInterface,
    compiledModulePath,
    compiledModuleWarnings,
    compiledProgramDiagnostics,
    compiledProgramEntryPath,
    compiledProgramErrors,
    compiledProgramModules,
    compiledProgramPreludePath,
    compiledProgramPrelude,
    compiledProgramWarnings,
    firstCompiledProgramError,
    lookupCompiledModule,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr,
    SignaturePayload,
    SignatureType,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFactClassName,
  )
import Jazz.Compiler.Diagnostics (Diagnostic, isErrorDiagnostic, isWarningDiagnostic)
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    ModuleImportMode (..),
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule,
    CoreProgram,
    ImportExposure (..),
    ModuleImport,
    PreludeArtifact,
    coreModuleExpr,
    coreModuleFacts,
    coreModuleImports,
    coreModulePath,
    coreProgramEntry,
    coreProgramModules,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    moduleIdentityPath,
    modulePathTextSegments,
    moduleQualifierIdentifier,
    preludeModulePath,
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

data CompiledModule = CompiledModule
  { storedCompiledModulePath :: ModulePath,
    storedCompiledModuleImports :: [ModuleImport 'Resolved],
    storedCompiledModuleExportInventory :: ModuleExportInventory,
    storedCompiledModuleInterface :: ModuleInterface,
    storedCompiledModuleDiagnostics :: [Diagnostic],
    storedCompiledModuleExpr :: Expr 'Resolved
  }
  deriving stock (Eq)

instance Show CompiledModule where
  showsPrec precedence compiledModule =
    showParen (precedence > 10) $
      showString "CompiledModule {compiledModulePath = "
        . shows (compiledModulePath compiledModule)
        . showString ", compiledModuleImports = "
        . shows (compiledModuleImports compiledModule)
        . showString ", compiledModuleExportInventory = "
        . shows (compiledModuleExportInventory compiledModule)
        . showString ", compiledModuleInterface = "
        . shows (compiledModuleInterface compiledModule)
        . showString ", compiledModuleDiagnostics = "
        . shows (compiledModuleDiagnostics compiledModule)
        . showString ", compiledModuleExpr = "
        . shows (compiledModuleExpr compiledModule)
        . showChar '}'

instance NFData CompiledModule where
  rnf (CompiledModule modulePath imports exports moduleInterface diagnostics expr) =
    rnf modulePath `seq`
      rnf imports `seq`
        rnf exports `seq`
          rnf moduleInterface `seq`
            rnf diagnostics `seq`
              rnf expr

data CompiledProgram = CompiledProgram
  { storedCompiledProgramPrelude :: CompiledPrelude,
    storedCompiledProgramPreludePath :: ModulePath,
    storedCompiledProgramEntryPath :: ModulePath,
    storedCompiledProgramModules :: [CompiledModule]
  }
  deriving stock (Eq)

instance Show CompiledProgram where
  showsPrec precedence compiledProgram =
    showParen (precedence > 10) $
      showString "CompiledProgram {compiledProgramPrelude = "
        . shows (compiledProgramPrelude compiledProgram)
        . showString ", compiledProgramPreludePath = "
        . shows (compiledProgramPreludePath compiledProgram)
        . showString ", compiledProgramEntryPath = "
        . shows (compiledProgramEntryPath compiledProgram)
        . showString ", compiledProgramModules = "
        . shows (compiledProgramModules compiledProgram)
        . showChar '}'

instance NFData CompiledProgram where
  rnf (CompiledProgram prelude preludePath entryPath modules) =
    rnf prelude `seq`
      rnf preludePath `seq`
        rnf entryPath `seq`
          rnf modules

compiledModulePath :: CompiledModule -> ModulePath
compiledModulePath = storedCompiledModulePath

compiledModuleImports :: CompiledModule -> [ModuleImport 'Resolved]
compiledModuleImports = storedCompiledModuleImports

compiledModuleExportInventory :: CompiledModule -> ModuleExportInventory
compiledModuleExportInventory = storedCompiledModuleExportInventory

compiledModuleInterface :: CompiledModule -> ModuleInterface
compiledModuleInterface = storedCompiledModuleInterface

compiledModuleDiagnostics :: CompiledModule -> [Diagnostic]
compiledModuleDiagnostics = storedCompiledModuleDiagnostics

compiledModuleExpr :: CompiledModule -> Expr 'Resolved
compiledModuleExpr = storedCompiledModuleExpr

compiledProgramPrelude :: CompiledProgram -> CompiledPrelude
compiledProgramPrelude = storedCompiledProgramPrelude

compiledProgramPreludePath :: CompiledProgram -> ModulePath
compiledProgramPreludePath = storedCompiledProgramPreludePath

compiledProgramEntryPath :: CompiledProgram -> ModulePath
compiledProgramEntryPath = storedCompiledProgramEntryPath

compiledProgramModules :: CompiledProgram -> [CompiledModule]
compiledProgramModules = storedCompiledProgramModules

compiledProgramDiagnostics :: CompiledProgram -> [Diagnostic]
compiledProgramDiagnostics compiledProgram =
  compiledPreludeDiagnostics (compiledProgramPrelude compiledProgram)
    <> concatMap compiledModuleDiagnostics (compiledProgramModules compiledProgram)

compiledModuleWarnings :: CompiledModule -> [Diagnostic]
compiledModuleWarnings = filter isWarningDiagnostic . compiledModuleDiagnostics

compiledModuleErrors :: CompiledModule -> [Diagnostic]
compiledModuleErrors = filter isErrorDiagnostic . compiledModuleDiagnostics

compiledProgramWarnings :: CompiledProgram -> [Diagnostic]
compiledProgramWarnings = filter isWarningDiagnostic . compiledProgramDiagnostics

compiledProgramErrors :: CompiledProgram -> [Diagnostic]
compiledProgramErrors = filter isErrorDiagnostic . compiledProgramDiagnostics

firstCompiledProgramError :: CompiledProgram -> Maybe Diagnostic
firstCompiledProgramError = find isErrorDiagnostic . compiledProgramDiagnostics

lookupCompiledModule :: ModulePath -> CompiledProgram -> Maybe CompiledModule
lookupCompiledModule modulePath =
  find ((== modulePath) . compiledModulePath) . compiledProgramModules

compilePreparedPrelude :: WarningSettings -> Set.Set Int -> PreludeArtifact 'Resolved -> IO CompiledPrelude
compilePreparedPrelude settings hiddenStatementIndices prelude =
  case ModuleGraph.preludeModule prelude of
    Nothing ->
      pure
        emptyCompiledPrelude
          { compiledPreludeBuiltinMode = ModuleGraph.preludeBuiltinMode prelude
          }
    Just resolvedPreludeModule -> do
      inference <-
        inferExpressionWithInputsAndHiddenStatements
          InferenceInputs
            { inferenceBuiltinMode = ModuleGraph.preludeBuiltinMode prelude,
              inferencePreludeModulePath =
                moduleIdentityPath (ModuleGraph.preludeIdentity prelude),
              inferenceWarningSettings = settings,
              inferenceImportedTypes = Map.empty,
              inferenceImportedDataTypes = Map.empty,
              inferenceImportedConstructorWitnessNames = Map.empty,
              inferenceImportedCapabilities = emptyScopeCapabilityFacts,
              inferenceImportedClassNames = Set.empty,
              inferenceCurrentModulePath =
                Just (modulePathTexts (moduleIdentityPath (ModuleGraph.preludeIdentity prelude)))
            }
          hiddenStatementIndices
          (coreModuleExpr resolvedPreludeModule)
      pure
        CompiledPrelude
          { compiledPreludeBuiltinMode = ModuleGraph.preludeBuiltinMode prelude,
            compiledPreludeInterface = inferredModuleInterface inference,
            compiledPreludeDiagnostics = inferredDiagnostics inference,
            compiledPreludeExpr = Just (inferredExpr inference),
            compiledPreludeRuntimeHints = inferredRuntimeTypeHints inference
          }

compileResolvedProgram :: CompileInputs -> CoreProgram 'Resolved -> IO CompiledProgram
compileResolvedProgram inputs resolvedProgram =
  {-# SCC "jazz-stage:runtime-preparation" #-}
  do
    (compiledModules, _) <-
      foldM compileModule (Seq.empty, Map.empty) (NonEmpty.toList (coreProgramModules resolvedProgram))
    pure (projectCompiledProgram inputs resolvedProgram (toList compiledModules))
  where
    ambientInterface = ambientPreludeInterface (compileInputPrelude inputs)
    programPreludePath =
      moduleIdentityPath
        (ModuleGraph.preludeIdentity (ModuleGraph.coreProgramPrelude resolvedProgram))
    compileModule (compiledModules, compiledByPath) resolvedModule = do
      compiledModule <-
        compileResolvedModuleWithIndex
          inputs
          ambientInterface
          programPreludePath
          compiledByPath
          resolvedModule
      pure
        ( compiledModules Seq.|> compiledModule,
          Map.insert (coreModulePath resolvedModule) (compiledDependency compiledModule) compiledByPath
        )

-- The only projection into the temporary runtime carrier. Task 9 removes it.
projectCompiledProgram :: CompileInputs -> CoreProgram 'Resolved -> [CompiledModule] -> CompiledProgram
projectCompiledProgram inputs resolvedProgram compiledModules =
  CompiledProgram
    { storedCompiledProgramPrelude = compileInputPrelude inputs,
      storedCompiledProgramPreludePath =
        moduleIdentityPath
          (ModuleGraph.preludeIdentity (ModuleGraph.coreProgramPrelude resolvedProgram)),
      storedCompiledProgramEntryPath = coreProgramEntry resolvedProgram,
      storedCompiledProgramModules = compiledModules
    }

compileResolvedModule :: CompileInputs -> [CompiledModule] -> CoreModule 'Resolved -> IO CompiledModule
compileResolvedModule inputs compiledDependencies =
  compileResolvedModuleWithIndex
    inputs
    (ambientPreludeInterface (compileInputPrelude inputs))
    preludeModulePath
    (buildCompiledDependencyPathIndex compiledDependencies)

compileResolvedModuleWithIndex :: CompileInputs -> ImportedInterface -> ModulePath -> Map ModulePath CompiledDependency -> CoreModule 'Resolved -> IO CompiledModule
compileResolvedModuleWithIndex inputs ambientInterface preludePath compiledDependenciesByPath resolvedModule = do
  let importedInterface =
        ambientInterface
          <> foldMap
            (uncurry dependencyImportInterface)
            [ (importDecl, dependency)
            | importDecl <- coreModuleImports resolvedModule,
              Just dependency <- [Map.lookup (ModuleGraph.importedModule importDecl) compiledDependenciesByPath]
            ]
      modulePath = coreModulePath resolvedModule
      moduleExpr = coreModuleExpr resolvedModule
  inference <-
    inferExpressionWithInputs
      InferenceInputs
        { inferenceBuiltinMode = compileInputBuiltinMode inputs,
          inferencePreludeModulePath = preludePath,
          inferenceWarningSettings = compileInputWarningSettings inputs,
          inferenceImportedTypes = interfaceTypeEnv importedInterface,
          inferenceImportedDataTypes = importedDataTypes importedInterface,
          inferenceImportedConstructorWitnessNames =
            interfaceConstructorWitnessNames importedInterface,
          inferenceImportedCapabilities = interfaceCapabilities importedInterface,
          inferenceImportedClassNames = importedClassNames importedInterface,
          inferenceCurrentModulePath = Just (modulePathTexts modulePath)
        }
      moduleExpr
  pure
    CompiledModule
      { storedCompiledModulePath = modulePath,
        storedCompiledModuleImports = coreModuleImports resolvedModule,
        storedCompiledModuleExportInventory = ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule),
        storedCompiledModuleInterface = inferredModuleInterface inference,
        storedCompiledModuleDiagnostics = inferredDiagnostics inference,
        storedCompiledModuleExpr = inferredExpr inference
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

buildCompiledDependencyPathIndex :: [CompiledModule] -> Map ModulePath CompiledDependency
buildCompiledDependencyPathIndex =
  Map.fromListWith (\_ firstDependency -> firstDependency)
    . map
      (\compiledModule -> (compiledModulePath compiledModule, compiledDependency compiledModule))

ambientPreludeInterface :: CompiledPrelude -> ImportedInterface
ambientPreludeInterface compiledPrelude =
  importWholeInterface AmbientPrelude (compiledPreludeInterface compiledPrelude)

dependencyImportInterface :: ModuleImport 'Resolved -> CompiledDependency -> ImportedInterface
dependencyImportInterface importDecl dependency =
  case ModuleGraph.importExposure importDecl of
    ImportAllUnqualified -> dependencyWholeInterface dependency
    ImportOnlyUnqualified symbolNames ->
      importSelectedInterface
        (moduleOrigin (ModuleGraph.importedModule importDecl))
        Nothing
        (Just (map identifierText (NonEmpty.toList symbolNames)))
        (compiledModuleExportInventory compiledModule)
        (compiledModuleInterface compiledModule)
    ImportQualifiedOnly ->
      importSelectedInterface
        (moduleOrigin (ModuleGraph.importedModule importDecl))
        (fmap (identifierText . moduleQualifierIdentifier) (ModuleGraph.importAlias importDecl))
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
