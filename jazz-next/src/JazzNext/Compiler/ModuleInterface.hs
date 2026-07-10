{-# LANGUAGE OverloadedStrings #-}

-- | Compile-time and runtime-facing module boundary records.
module JazzNext.Compiler.ModuleInterface
  ( CompileInputs (..),
    CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..),
    compileInputs,
    emptyCompileInputs,
    emptyCompiledPrelude,
    emptyModuleInterface,
    lookupCompiledModule
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST (ConstraintSignatureType, Expr)
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.Diagnostics (Diagnostic, WarningRecord)
import JazzNext.Compiler.ModuleGraph (ResolvedModule (resolvedModulePath))
import JazzNext.Compiler.RuntimeHints (BindingRuntimeHintKey)
import JazzNext.Compiler.TypeInference.Types
  ( ClassMethodType,
    DataTypeBinding,
    ImplMethodType,
    TypeBinding
  )
import JazzNext.Compiler.WarningConfig (WarningSettings)

data ModuleInterface = ModuleInterface
  { interfaceValueTypes :: Map Text TypeBinding,
    interfaceDataTypes :: Map Text DataTypeBinding,
    interfaceClassFacts :: Map Text Int,
    interfaceGeneratedEqualityClassFacts :: Set Text,
    interfaceConcreteImplFacts :: Set Text,
    interfaceClassMethods :: Map Text ClassMethodType,
    interfaceConcreteImplMethods :: Map Text [ImplMethodType],
    interfaceRuntimeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
  }
  deriving (Eq, Show)

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
    compiledPreludeWarnings :: [WarningRecord],
    compiledPreludeErrors :: [Diagnostic],
    compiledPreludeExpr :: Maybe Expr,
    compiledPreludeRuntimeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
  }
  deriving (Eq, Show)

emptyCompiledPrelude :: CompiledPrelude
emptyCompiledPrelude =
  CompiledPrelude
    { compiledPreludeBuiltinMode = ResolveKernelOnly,
      compiledPreludeInterface = emptyModuleInterface,
      compiledPreludeWarnings = [],
      compiledPreludeErrors = [],
      compiledPreludeExpr = Nothing,
      compiledPreludeRuntimeHints = Map.empty
    }

data CompiledModule = CompiledModule
  { compiledResolvedModule :: ResolvedModule,
    compiledModuleInterface :: ModuleInterface,
    compiledModuleWarnings :: [WarningRecord],
    compiledModuleErrors :: [Diagnostic],
    compiledModuleExpr :: Expr
  }
  deriving (Eq, Show)

data CompiledProgram = CompiledProgram
  { compiledProgramPrelude :: CompiledPrelude,
    compiledProgramEntryPath :: [Text],
    compiledProgramModules :: [CompiledModule],
    compiledProgramWarnings :: [WarningRecord],
    compiledProgramErrors :: [Diagnostic]
  }
  deriving (Eq, Show)

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
