{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Compile-time and runtime-facing module boundary records.
module Jazz.Compiler.ModuleInterface
  ( CompileInputs (..),
    CompiledPrelude (..),
    compiledPreludeErrors,
    compiledPreludeWarnings,
    ModuleExport (..),
    ModuleInterface (..),
    compileInputs,
    emptyCompileInputs,
    emptyCompiledPrelude,
    emptyModuleInterface,
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
import Jazz.Compiler.AST (CorePhase (Resolved), Expr, SignatureType)
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.CapabilityFacts (ConcreteImplFact)
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
    interfaceConcreteImplFacts :: Set ConcreteImplFact,
    interfaceClassMethods :: Map Text ClassMethodType,
    interfaceConcreteImplMethods :: Map Text [ImplMethodType],
    interfaceRuntimeHints :: Map BindingRuntimeHintKey (SignatureType 'Resolved)
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
    compiledPreludeExpr :: Maybe (Expr 'Resolved),
    compiledPreludeRuntimeHints :: Map BindingRuntimeHintKey (SignatureType 'Resolved)
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

compiledPreludeWarnings :: CompiledPrelude -> [Diagnostic]
compiledPreludeWarnings = filter isWarningDiagnostic . compiledPreludeDiagnostics

compiledPreludeErrors :: CompiledPrelude -> [Diagnostic]
compiledPreludeErrors = filter isErrorDiagnostic . compiledPreludeDiagnostics

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
