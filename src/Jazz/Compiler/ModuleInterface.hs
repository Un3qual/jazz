{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile-time and runtime-facing module boundary records.
module Jazz.Compiler.ModuleInterface
  ( CompileInputs (..),
    CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    compiledModuleErrors,
    compiledModuleWarnings,
    compiledPreludeErrors,
    compiledPreludeWarnings,
    compiledProgramDiagnostics,
    compiledProgramErrors,
    compiledProgramWarnings,
    firstCompiledProgramError,
    ModuleExport (..),
    ModuleInterface (..),
    compileInputs,
    emptyCompileInputs,
    emptyCompiledPrelude,
    emptyModuleInterface,
    lookupCompiledModule,
    moduleExportForBinding,
    moduleInterfaceExportInventory,
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.AST (Expr, SignatureType)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    isErrorDiagnostic,
    isWarningDiagnostic,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
  )
import Jazz.Compiler.ModuleGraph (ResolvedImport)
import Jazz.Compiler.Name (NameNamespace (..))
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ImplMethodType,
    TypeBinding (..),
  )
import Jazz.Compiler.WarningConfig (WarningSettings)

moduleExportForBinding :: Text -> TypeBinding -> ModuleExport
moduleExportForBinding exportName binding =
  ModuleExport
    { moduleExportNamespace =
        case binding of
          ConstructorTypeBinding {} -> ConstructorNamespace
          _ -> ValueNamespace,
      moduleExportName = exportName
    }

data ModuleInterface = ModuleInterface
  { interfaceValueTypes :: Map ModuleExport TypeBinding,
    interfaceDataTypes :: Map Text DataTypeBinding,
    interfaceClassFacts :: Map Text Int,
    interfaceGeneratedEqualityClassFacts :: Set Text,
    interfaceConcreteImplFacts :: Set Text,
    interfaceClassMethods :: Map Text ClassMethodType,
    interfaceConcreteImplMethods :: Map Text [ImplMethodType],
    interfaceRuntimeHints :: Map BindingRuntimeHintKey SignatureType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

moduleInterfaceExportInventory :: ModuleInterface -> ModuleExportInventory
moduleInterfaceExportInventory interface =
  exportInventory
    ( Map.keys (interfaceValueTypes interface)
        <> [ ModuleExport TypeNamespace name
           | name <- Map.keys (interfaceDataTypes interface)
           ]
        <> [ ModuleExport CapabilityNamespace name
           | name <- Map.keys (interfaceClassFacts interface)
           ]
    )

emptyModuleInterface :: ModuleInterface
emptyModuleInterface =
  ModuleInterface
    { interfaceValueTypes = Map.empty,
      interfaceDataTypes = Map.empty,
      interfaceClassFacts = Map.empty,
      interfaceGeneratedEqualityClassFacts = Set.empty,
      interfaceConcreteImplFacts = Set.empty,
      interfaceClassMethods = Map.empty,
      interfaceConcreteImplMethods = Map.empty,
      interfaceRuntimeHints = Map.empty
    }

data CompiledPrelude = CompiledPrelude
  { compiledPreludeBuiltinMode :: BuiltinResolutionMode,
    compiledPreludeInterface :: ModuleInterface,
    compiledPreludeDiagnostics :: [Diagnostic],
    compiledPreludeExpr :: Maybe Expr,
    compiledPreludeRuntimeHints :: Map BindingRuntimeHintKey SignatureType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

emptyCompiledPrelude :: CompiledPrelude
emptyCompiledPrelude =
  CompiledPrelude
    { compiledPreludeBuiltinMode = ResolveKernelOnly,
      compiledPreludeInterface = emptyModuleInterface,
      compiledPreludeDiagnostics = [],
      compiledPreludeExpr = Nothing,
      compiledPreludeRuntimeHints = Map.empty
    }

data CompiledModule = CompiledModule
  { compiledModulePath :: [Text],
    compiledModuleImports :: [ResolvedImport],
    compiledModuleExportInventory :: ModuleExportInventory,
    compiledModuleInterface :: ModuleInterface,
    compiledModuleDiagnostics :: [Diagnostic],
    compiledModuleExpr :: Expr
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data CompiledProgram = CompiledProgram
  { compiledProgramPrelude :: CompiledPrelude,
    compiledProgramEntryPath :: [Text],
    compiledProgramModules :: [CompiledModule]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

compiledProgramDiagnostics :: CompiledProgram -> [Diagnostic]
compiledProgramDiagnostics compiledProgram =
  compiledPreludeDiagnostics (compiledProgramPrelude compiledProgram)
    <> concatMap compiledModuleDiagnostics (compiledProgramModules compiledProgram)

compiledPreludeWarnings :: CompiledPrelude -> [Diagnostic]
compiledPreludeWarnings = filter isWarningDiagnostic . compiledPreludeDiagnostics

compiledPreludeErrors :: CompiledPrelude -> [Diagnostic]
compiledPreludeErrors = filter isErrorDiagnostic . compiledPreludeDiagnostics

compiledModuleWarnings :: CompiledModule -> [Diagnostic]
compiledModuleWarnings = filter isWarningDiagnostic . compiledModuleDiagnostics

compiledModuleErrors :: CompiledModule -> [Diagnostic]
compiledModuleErrors = filter isErrorDiagnostic . compiledModuleDiagnostics

compiledProgramWarnings :: CompiledProgram -> [Diagnostic]
compiledProgramWarnings = filter isWarningDiagnostic . compiledProgramDiagnostics

compiledProgramErrors :: CompiledProgram -> [Diagnostic]
compiledProgramErrors = filter isErrorDiagnostic . compiledProgramDiagnostics

firstCompiledProgramError :: CompiledProgram -> Maybe Diagnostic
firstCompiledProgramError compiledProgram =
  case firstError (compiledPreludeDiagnostics (compiledProgramPrelude compiledProgram)) of
    Just diagnostic -> Just diagnostic
    Nothing -> firstModuleError (compiledProgramModules compiledProgram)
  where
    firstError diagnostics =
      case diagnostics of
        [] -> Nothing
        diagnostic : rest
          | isErrorDiagnostic diagnostic -> Just diagnostic
          | otherwise -> firstError rest
    firstModuleError compiledModules =
      case compiledModules of
        [] -> Nothing
        compiledModule : rest ->
          case firstError (compiledModuleDiagnostics compiledModule) of
            Just diagnostic -> Just diagnostic
            Nothing -> firstModuleError rest

data CompileInputs = CompileInputs
  { compileInputWarningSettings :: WarningSettings,
    compileInputBuiltinMode :: BuiltinResolutionMode,
    compileInputPrelude :: CompiledPrelude
  }

emptyCompileInputs :: WarningSettings -> CompileInputs
emptyCompileInputs settings =
  CompileInputs
    { compileInputWarningSettings = settings,
      compileInputBuiltinMode = ResolveKernelOnly,
      compileInputPrelude = emptyCompiledPrelude
    }

compileInputs :: WarningSettings -> CompiledPrelude -> CompileInputs
compileInputs settings compiledPrelude =
  CompileInputs
    { compileInputWarningSettings = settings,
      compileInputBuiltinMode = compiledPreludeBuiltinMode compiledPrelude,
      compileInputPrelude = compiledPrelude
    }

lookupCompiledModule :: [Text] -> CompiledProgram -> Maybe CompiledModule
lookupCompiledModule modulePath =
  go . compiledProgramModules
  where
    go modules =
      case modules of
        [] -> Nothing
        compiledModule : rest
          | compiledModulePath compiledModule == modulePath -> Just compiledModule
          | otherwise -> go rest
