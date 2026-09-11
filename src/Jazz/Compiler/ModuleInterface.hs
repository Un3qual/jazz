{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Compile-time and runtime-facing module boundary records.
module Jazz.Compiler.ModuleInterface
  ( CompileInputs (..),
    ModuleExport (..),
    ModuleInterface (..),
    ModuleValueBinding (..),
    emptyCompileInputs,
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
import Jazz.Compiler.CapabilityFacts (ConcreteImplFact)
import Jazz.Compiler.CoreIdentity (CoreBinderId)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
  )
import Jazz.Compiler.Name (NameNamespace (..))
import Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType,
    DataTypeBinding,
    DeclarationVariable,
    ImplMethodType,
    SemanticBinding (..),
  )
import Jazz.Compiler.WarningConfig (WarningSettings)

moduleExportForBinding :: Text -> SemanticBinding variable -> ModuleExport
moduleExportForBinding exportName binding =
  ModuleExport
    { moduleExportNamespace =
        case binding of
          ConstructorTypeBinding {} -> ConstructorNamespace
          _ -> ValueNamespace,
      moduleExportName = exportName
    }

-- | An exported type and the declaration whose value supplies it. Import
-- aliases change the visible name, never this defining identity.
data ModuleValueBinding = ModuleValueBinding
  { interfaceBindingId :: CoreBinderId,
    interfaceBindingType :: SemanticBinding DeclarationVariable
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ModuleInterface = ModuleInterface
  { interfaceValueBindings :: Map ModuleExport ModuleValueBinding,
    interfaceDataTypes :: Map Text DataTypeBinding,
    interfaceClassFacts :: Map Text Int,
    interfaceGeneratedEqualityClassFacts :: Set Text,
    interfaceConcreteImplFacts :: Set ConcreteImplFact,
    interfaceClassMethods :: Map Text ClassMethodType,
    interfaceConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

moduleInterfaceExportInventory :: ModuleInterface -> ModuleExportInventory
moduleInterfaceExportInventory interface =
  exportInventory
    ( Map.keys (interfaceValueBindings interface)
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
    { interfaceValueBindings = Map.empty,
      interfaceDataTypes = Map.empty,
      interfaceClassFacts = Map.empty,
      interfaceGeneratedEqualityClassFacts = Set.empty,
      interfaceConcreteImplFacts = Set.empty,
      interfaceClassMethods = Map.empty,
      interfaceConcreteImplMethods = Map.empty
    }

data CompileInputs = CompileInputs
  { compileInputWarningSettings :: WarningSettings,
    compileInputExternalUses :: Set CoreBinderId
  }

emptyCompileInputs :: WarningSettings -> CompileInputs
emptyCompileInputs settings =
  CompileInputs
    { compileInputWarningSettings = settings,
      compileInputExternalUses = Set.empty
    }
