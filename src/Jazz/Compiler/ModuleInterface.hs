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
    compiledProgramErrors,
    compiledProgramWarnings,
    ModuleExport (..),
    ModuleInterface (..),
    compileInputs,
    emptyCompileInputs,
    emptyCompiledPrelude,
    emptyModuleInterface,
    lookupCompiledModule,
    moduleExportForBinding,
    moduleInterfaceExportInventory
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (SignatureType, Expr)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    isErrorDiagnostic,
    isWarningDiagnostic
  )
import Jazz.Compiler.ModuleGraph (ResolvedModule (resolvedModulePath))
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory
  )
import Jazz.Compiler.Name (NameNamespace (..))
import Jazz.Compiler.RuntimeHints (BindingRuntimeHintKey)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ImplMethodType,
    TypeBinding (..)
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  { compiledResolvedModule :: ResolvedModule,
    compiledModuleInterface :: ModuleInterface,
    compiledModuleDiagnostics :: [Diagnostic],
    compiledModuleExpr :: Expr
  }
  deriving (Eq, Show)

data CompiledProgram = CompiledProgram
  { compiledProgramPrelude :: CompiledPrelude,
    compiledProgramEntryPath :: [Text],
    compiledProgramModules :: [CompiledModule],
    compiledProgramDiagnostics :: [Diagnostic]
  }
  deriving (Eq, Show)

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
          | resolvedModulePath (compiledResolvedModule compiledModule) == modulePath -> Just compiledModule
          | otherwise -> go rest
