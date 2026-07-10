{-# LANGUAGE OverloadedStrings #-}

-- | Compile resolved modules once against explicit dependency interfaces.
module JazzNext.Compiler.ModuleCompiler
  ( compilePreparedPrelude,
    compileResolvedProgram
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..)
  )
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (coreModuleExpr),
    ResolvedImport (..),
    ResolvedModule (..),
    ResolvedProgram (..)
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory
  )
import JazzNext.Compiler.ModuleInterface
import JazzNext.Compiler.Name
  ( Name (..),
    NameNamespace (CapabilityNamespace),
    ResolvedNameOrigin (..),
    identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Prelude (PreparedPrelude (..))
import JazzNext.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements
  )
import JazzNext.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    ImplMethodType (..),
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
    TypeScheme (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts
  )
import JazzNext.Compiler.WarningConfig (WarningSettings)

compilePreparedPrelude :: WarningSettings -> PreparedPrelude -> IO CompiledPrelude
compilePreparedPrelude settings preparedPrelude =
  case preparedPreludeExpr preparedPrelude of
    Nothing ->
      pure
        emptyCompiledPrelude
          { compiledPreludeBuiltinMode = preparedPreludeBuiltinMode preparedPrelude
          }
    Just preludeExpr -> do
      inference <-
        inferExpressionWithInputsAndHiddenStatements
          InferenceInputs
            { inferenceBuiltinMode = preparedPreludeBuiltinMode preparedPrelude,
              inferenceWarningSettings = settings,
              inferenceImportedTypes = Map.empty,
              inferenceImportedDataTypes = Map.empty,
              inferenceImportedCapabilities = emptyScopeCapabilityFacts,
              inferenceImportedClassNames = Set.empty,
              inferenceCurrentModulePath = Just []
            }
          (preparedPreludeHiddenStatementIndices preparedPrelude)
          preludeExpr
      pure
        CompiledPrelude
          { compiledPreludeBuiltinMode = preparedPreludeBuiltinMode preparedPrelude,
            compiledPreludeInterface = inferredModuleInterface inference,
            compiledPreludeWarnings = inferredWarnings inference,
            compiledPreludeErrors = inferredErrors inference,
            compiledPreludeExpr = Just (inferredExpr inference),
            compiledPreludeRuntimeHints = inferredRuntimeTypeHints inference
          }

compileResolvedProgram :: CompileInputs -> ResolvedProgram -> IO CompiledProgram
compileResolvedProgram inputs resolvedProgram = do
  compiledModules <- foldModules [] (resolvedProgramModules resolvedProgram)
  let compiledPrelude = compileInputPrelude inputs
      moduleWarnings = concatMap compiledModuleWarnings compiledModules
      moduleErrors = concatMap compiledModuleErrors compiledModules
  pure
    CompiledProgram
      { compiledProgramPrelude = compiledPrelude,
        compiledProgramEntryPath = resolvedProgramEntryPath resolvedProgram,
        compiledProgramModules = compiledModules,
        compiledProgramWarnings = compiledPreludeWarnings compiledPrelude <> moduleWarnings,
        compiledProgramErrors = compiledPreludeErrors compiledPrelude <> moduleErrors
      }
  where
    foldModules compiled remaining =
      case remaining of
        [] -> pure compiled
        resolvedModule : rest -> do
          compiledModule <- compileModule compiled resolvedModule
          foldModules (compiled <> [compiledModule]) rest

    compileModule compiledDependencies resolvedModule = do
      let importedInterface =
            foldl'
              mergeModuleInterfaces
              (ambientPreludeInterface (compileInputPrelude inputs))
              [ dependencyImportInterface importDecl dependency
                | importDecl <- resolvedModuleImports resolvedModule,
                  Just dependency <- [lookupDependency (resolvedImportPath importDecl) compiledDependencies]
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
              inferenceImportedCapabilities = interfaceCapabilities importedInterface,
              inferenceImportedClassNames = importedClassNames importedInterface,
              inferenceCurrentModulePath = Just modulePath
            }
          moduleExpr
      pure
        CompiledModule
          { compiledResolvedModule = resolvedModule,
            compiledModuleInterface = inferredModuleInterface inference,
            compiledModuleWarnings = inferredWarnings inference,
            compiledModuleErrors = inferredErrors inference,
            compiledModuleExpr = inferredExpr inference
          }

lookupDependency :: [Text] -> [CompiledModule] -> Maybe CompiledModule
lookupDependency modulePath =
  go
  where
    go modules =
      case modules of
        [] -> Nothing
        compiledModule : rest
          | resolvedModulePath (compiledResolvedModule compiledModule) == modulePath -> Just compiledModule
          | otherwise -> go rest

ambientPreludeInterface :: CompiledPrelude -> ImportedInterface
ambientPreludeInterface compiledPrelude =
  importWholeInterface AmbientPrelude (compiledPreludeInterface compiledPrelude)

dependencyImportInterface :: ResolvedImport -> CompiledModule -> ImportedInterface
dependencyImportInterface importDecl compiledModule =
  importSelectedInterface
    (ImportedModule (resolvedImportPath importDecl))
    (resolvedImportAlias importDecl)
    (resolvedImportSymbols importDecl)
    (resolvedModuleExportInventory (compiledResolvedModule compiledModule))
    (compiledModuleInterface compiledModule)

data ImportedInterface = ImportedInterface
  { importedTypes :: TypeEnv,
    importedDataTypes :: Map Text DataTypeBinding,
    importedCapabilities :: ScopeCapabilityFacts,
    importedClassNames :: Set.Set Text
  }

emptyImportedInterface :: ImportedInterface
emptyImportedInterface = ImportedInterface Map.empty Map.empty emptyScopeCapabilityFacts Set.empty

interfaceTypeEnv :: ImportedInterface -> TypeEnv
interfaceTypeEnv = importedTypes

interfaceCapabilities :: ImportedInterface -> ScopeCapabilityFacts
interfaceCapabilities = importedCapabilities

mergeModuleInterfaces :: ImportedInterface -> ImportedInterface -> ImportedInterface
mergeModuleInterfaces left right =
  ImportedInterface
    { importedTypes = Map.union (importedTypes left) (importedTypes right),
      importedDataTypes = Map.union (importedDataTypes left) (importedDataTypes right),
      importedCapabilities =
        ScopeCapabilityFacts
          { scopeClassFacts = Map.union (scopeClassFacts (importedCapabilities left)) (scopeClassFacts (importedCapabilities right)),
            scopeGeneratedEqualityClassFacts = Set.union (scopeGeneratedEqualityClassFacts (importedCapabilities left)) (scopeGeneratedEqualityClassFacts (importedCapabilities right)),
            scopeConcreteImplFacts = Set.union (scopeConcreteImplFacts (importedCapabilities left)) (scopeConcreteImplFacts (importedCapabilities right)),
            scopeClassMethodSignatures = Map.union (scopeClassMethodSignatures (importedCapabilities left)) (scopeClassMethodSignatures (importedCapabilities right)),
            scopeConcreteImplMethods = Map.unionWith (<>) (scopeConcreteImplMethods (importedCapabilities left)) (scopeConcreteImplMethods (importedCapabilities right))
          },
      importedClassNames = Set.union (importedClassNames left) (importedClassNames right)
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
          [ ( ResolvedName origin (moduleExportNamespace export) (mkIdentifier (moduleExportName export)),
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
      importedCapabilities =
        rebaseCapabilityFacts origin dataTypeNames classNames selectedCapabilities,
      importedClassNames = selectedClassNames
    }
  where
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
    ImportedModule modulePath -> Text.intercalate "::" (modulePath <> [name])
    _ -> name

factUsesClass :: Set.Set Text -> Text -> Bool
factUsesClass classNames fact = Set.member (fst (Text.breakOn "(" fact)) classNames

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
        (rebaseKnownName origin dataTypeNames typeName)
        parameters
        (map (rebaseConstructorArgument origin dataTypeNames) arguments)

rebaseDataTypeBinding :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> DataTypeBinding -> DataTypeBinding
rebaseDataTypeBinding origin dataTypeNames _ (DataTypeBinding parameters constructors) =
  DataTypeBinding parameters (map (map (rebaseConstructorArgument origin dataTypeNames)) constructors)

rebaseConstructorArgument :: ResolvedNameOrigin -> Set.Set Text -> ConstructorArgumentType -> ConstructorArgumentType
rebaseConstructorArgument origin dataTypeNames argument =
  case argument of
    ConstructorArgumentMonomorphic TVarType {} ->
      ConstructorArgumentFresh
    ConstructorArgumentMonomorphic expressionType ->
      ConstructorArgumentMonomorphic (rebaseExpressionType origin dataTypeNames expressionType)
    ConstructorArgumentParameter {} -> argument
    ConstructorArgumentFresh -> argument

rebaseExpressionType :: ResolvedNameOrigin -> Set.Set Text -> ExpressionType -> ExpressionType
rebaseExpressionType origin dataTypeNames expressionType =
  case expressionType of
    TListType elementType -> TListType (rebaseExpressionType origin dataTypeNames elementType)
    TTupleType elementTypes -> TTupleType (map (rebaseExpressionType origin dataTypeNames) elementTypes)
    TDataType typeName arguments ->
      TDataType
        (rebaseKnownName origin dataTypeNames typeName)
        (map (rebaseExpressionType origin dataTypeNames) arguments)
    TFunctionType argumentType resultType ->
      TFunctionType
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
      scopeConcreteImplFacts = Set.map (rebaseFact origin dataTypeNames classNames) (scopeConcreteImplFacts facts),
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
rebaseImplMethod origin dataTypeNames classNames (ImplMethodType target) =
  ImplMethodType (rebaseConstraintType origin dataTypeNames classNames target)

rebaseSignaturePayload :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> SignaturePayload -> SignaturePayload
rebaseSignaturePayload origin dataTypeNames classNames payload =
  case payload of
    SignatureType {} -> payload
    ConstrainedSignature constraints signatureType ->
      ConstrainedSignature
        [ SignatureConstraint
            (rebaseKnownName origin classNames capabilityName)
            (map (rebaseConstraintType origin dataTypeNames classNames) arguments)
          | SignatureConstraint capabilityName arguments <- constraints
        ]
        (rebaseConstraintType origin dataTypeNames classNames signatureType)
    UnsupportedSignature tokens ->
      UnsupportedSignature
        [ case token of
            SignatureNameToken name -> SignatureNameToken (rebaseKnownName origin dataTypeNames name)
            _ -> token
          | token <- tokens
        ]

rebaseConstraintType :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> ConstraintSignatureType -> ConstraintSignatureType
rebaseConstraintType origin dataTypeNames _ signatureType =
  case signatureType of
    ConstraintTypeName typeName -> ConstraintTypeName (rebaseKnownName origin dataTypeNames typeName)
    ConstraintTypeApplication typeName arguments ->
      ConstraintTypeApplication
        (rebaseKnownName origin dataTypeNames typeName)
        (map (rebaseConstraintType origin dataTypeNames Set.empty) arguments)
    ConstraintTypeList elementType -> ConstraintTypeList (rebaseConstraintType origin dataTypeNames Set.empty elementType)
    ConstraintTypeTuple elementTypes -> ConstraintTypeTuple (map (rebaseConstraintType origin dataTypeNames Set.empty) elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      ConstraintTypeFunction
        (rebaseConstraintType origin dataTypeNames Set.empty argumentType)
        (rebaseConstraintType origin dataTypeNames Set.empty resultType)

rebaseKnownName :: ResolvedNameOrigin -> Set.Set Text -> Name -> Name
rebaseKnownName origin knownNames name =
  case name of
    ResolvedName CurrentModule namespace identifier
      | Set.member (identifierText identifier) knownNames ->
          ResolvedName origin namespace identifier
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

rebaseFact :: ResolvedNameOrigin -> Set.Set Text -> Set.Set Text -> Text -> Text
rebaseFact origin dataTypeNames classNames =
  Text.concat . map rebaseToken . Text.groupBy sameTokenKind
  where
    knownNames = Set.union dataTypeNames classNames
    rebaseToken token
      | Text.all identifierCharacter token = rebaseKnownText origin knownNames token
      | otherwise = token
    sameTokenKind left right = identifierCharacter left == identifierCharacter right
    identifierCharacter character =
      character == ':' || character == '_' || ('0' <= character && character <= '9') || ('A' <= character && character <= 'Z') || ('a' <= character && character <= 'z')
