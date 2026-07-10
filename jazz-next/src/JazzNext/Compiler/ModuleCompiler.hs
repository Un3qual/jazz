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
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (coreModuleExpr),
    ResolvedImport (..),
    ResolvedModule (..),
    ResolvedProgram (..)
  )
import JazzNext.Compiler.ModuleInterface
import JazzNext.Compiler.Name
  ( Name (ResolvedName),
    NameNamespace (..),
    ResolvedNameOrigin (..)
  )
import JazzNext.Compiler.Identifier (mkIdentifier)
import JazzNext.Compiler.Prelude (PreparedPrelude (..))
import JazzNext.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    inferExpressionWithInputs,
    inferExpressionWithInputsAndHiddenStatements
  )
import JazzNext.Compiler.TypeInference.Types
  ( DataTypeBinding,
    ScopeCapabilityFacts (..),
    TypeBinding (..),
    TypeEnv,
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
              inferenceCurrentModulePath = Nothing
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
    (compiledModuleInterface compiledModule)

data ImportedInterface = ImportedInterface
  { importedTypes :: TypeEnv,
    importedDataTypes :: Map Text DataTypeBinding,
    importedCapabilities :: ScopeCapabilityFacts
  }

emptyImportedInterface :: ImportedInterface
emptyImportedInterface = ImportedInterface Map.empty Map.empty emptyScopeCapabilityFacts

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
          }
    }

importWholeInterface :: ResolvedNameOrigin -> ModuleInterface -> ImportedInterface
importWholeInterface origin = importSelectedInterface origin Nothing Nothing

importSelectedInterface :: ResolvedNameOrigin -> Maybe Text -> Maybe [Text] -> ModuleInterface -> ImportedInterface
importSelectedInterface origin maybeAlias maybeSymbols moduleInterface =
  ImportedInterface
    { importedTypes =
        Map.fromList
          [ (ResolvedName origin (bindingNamespace binding) (mkIdentifier exportName), binding)
            | (exportName, binding) <- Map.toList selectedValueTypes
          ],
      importedDataTypes =
        Map.fromList
          [ (qualifiedKey origin dataTypeName, dataType)
            | (dataTypeName, dataType) <- Map.toList (interfaceDataTypes moduleInterface)
          ],
      importedCapabilities = selectedCapabilities
    }
  where
    selected name = maybe True (name `elem`) maybeSymbols
    selectedValueTypes = Map.filterWithKey (\name _ -> selected name) (interfaceValueTypes moduleInterface)
    includeCapabilities = maybeAlias == Nothing
    selectedClassFacts
      | includeCapabilities = Map.filterWithKey (\name _ -> selected name) (interfaceClassFacts moduleInterface)
      | otherwise = Map.empty
    selectedClassNames = Map.keysSet selectedClassFacts
    selectedCapabilities =
      ScopeCapabilityFacts
        { scopeClassFacts = Map.mapKeys (qualifiedKey origin) selectedClassFacts,
          scopeGeneratedEqualityClassFacts =
            Set.map (qualifiedKey origin) $
              Set.filter
                (`Set.member` selectedClassNames)
                (interfaceGeneratedEqualityClassFacts moduleInterface),
          scopeConcreteImplFacts =
            Set.map (qualifyFact origin selectedClassNames) (Set.filter (factUsesClass selectedClassNames) (interfaceConcreteImplFacts moduleInterface)),
          scopeClassMethodSignatures =
            Map.mapKeys (qualifiedKey origin) (Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceClassMethods moduleInterface)),
          scopeConcreteImplMethods =
            Map.mapKeys (qualifiedKey origin) (Map.filterWithKey (methodUsesClass selectedClassNames) (interfaceConcreteImplMethods moduleInterface))
        }

bindingNamespace :: TypeBinding -> NameNamespace
bindingNamespace binding =
  case binding of
    ConstructorTypeBinding {} -> ConstructorNamespace
    _ -> ValueNamespace

qualifiedKey :: ResolvedNameOrigin -> Text -> Text
qualifiedKey origin name =
  case origin of
    ImportedModule modulePath -> Text.intercalate "::" (modulePath <> [name])
    _ -> name

factUsesClass :: Set.Set Text -> Text -> Bool
factUsesClass classNames fact = Set.member (fst (Text.breakOn "(" fact)) classNames

qualifyFact :: ResolvedNameOrigin -> Set.Set Text -> Text -> Text
qualifyFact origin _ fact =
  let (className, arguments) = Text.breakOn "(" fact
   in qualifiedKey origin className <> arguments

methodUsesClass :: Set.Set Text -> Text -> value -> Bool
methodUsesClass classNames methodKey _ =
  any (\className -> (className <> "::") `Text.isPrefixOf` methodKey) (Set.toList classNames)
