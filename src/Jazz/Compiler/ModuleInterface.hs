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
    publishModuleInterface,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifoldable (bifoldMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.CoreIdentity (CapabilityId, CapabilityMethodKey, CoreBinderId, renderCapabilityId)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
    inventoryHasExport,
    restrictExportInventory,
  )
import Jazz.Compiler.Name (NameNamespace (..), ResolvedName, renderName)
import Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType (..),
    ConcreteImplFact (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    DeclarationVariable,
    ImplMethodType (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
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
  { interfacePublicExports :: ModuleExportInventory,
    interfaceValueBindings :: Map ModuleExport ModuleValueBinding,
    interfaceDataTypes :: Map ResolvedName DataTypeBinding,
    interfaceClassFacts :: Map CapabilityId Int,
    interfaceGeneratedEqualityClassFacts :: Set CapabilityId,
    interfaceConcreteImplFacts :: Set ConcreteImplFact,
    interfaceClassMethods :: Map CapabilityMethodKey ClassMethodType,
    interfaceConcreteImplMethods :: Map CapabilityMethodKey [ImplMethodType]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

moduleInterfaceExportInventory :: ModuleInterface -> ModuleExportInventory
moduleInterfaceExportInventory = interfacePublicExports

-- The discovery inventory is checked against this typed declaration view before
-- publication. Supporting nominal definitions are retained only when reachable
-- from a public declaration; they do not introduce importable names.
publishModuleInterface :: Maybe ModuleExportInventory -> Map ResolvedName DataTypeBinding -> ModuleInterface -> ModuleInterface
publishModuleInterface requested typeDefinitions declarations =
  public {interfaceDataTypes = reachableTypes roots}
  where
    available = declaredInterfaceInventory declarations
    exports = maybe available (\inventory -> restrictExportInventory (Set.intersection (exportInventoryEntries available) (exportInventoryEntries inventory)) inventory) requested
    public =
      declarations
        { interfacePublicExports = exports,
          interfaceValueBindings = Map.filterWithKey (\name _ -> inventoryHasExport name exports) (interfaceValueBindings declarations),
          interfaceClassFacts = Map.filterWithKey (\capability _ -> publicCapability capability) (interfaceClassFacts declarations),
          interfaceGeneratedEqualityClassFacts = Set.filter publicCapability (interfaceGeneratedEqualityClassFacts declarations),
          interfaceConcreteImplFacts = Set.filter (\(ConcreteImplFact capability _) -> publicCapability capability) (interfaceConcreteImplFacts declarations),
          interfaceClassMethods = Map.filterWithKey (\(capability, _) _ -> publicCapability capability) (interfaceClassMethods declarations),
          interfaceConcreteImplMethods = Map.filterWithKey (\(capability, _) _ -> publicCapability capability) (interfaceConcreteImplMethods declarations)
        }
    publicCapability capability = inventoryHasExport (ModuleExport CapabilityNamespace (renderCapabilityId capability)) exports
    roots =
      Set.unions
        [ Map.keysSet (Map.filterWithKey (\name _ -> inventoryHasExport (ModuleExport TypeNamespace (renderName name)) exports) (interfaceDataTypes declarations)),
          foldMap (bindingNames . interfaceBindingType) (interfaceValueBindings public),
          foldMap (\(ClassMethodType _ value) -> typeNames value) (interfaceClassMethods public),
          foldMap (\(ConcreteImplFact _ value) -> typeNames value) (interfaceConcreteImplFacts public),
          foldMap (foldMap (typeNames . implMethodTarget)) (interfaceConcreteImplMethods public)
        ]
    reachableTypes names =
      let definitions = Map.restrictKeys typeDefinitions names
          expanded = Set.union names (foldMap (\(DataTypeBinding _ constructors) -> foldMap (foldMap fieldNames) constructors) definitions)
       in if names == expanded then definitions else reachableTypes expanded

    typeNames = bifoldMap Set.singleton (const Set.empty)
    fieldNames (ConstructorArgumentType value) = typeNames value
    fieldNames ConstructorArgumentFresh = Set.empty
    bindingNames binding = case binding of
      PlainTypeBinding value -> typeNames value
      SchemeTypeBinding scheme -> schemeNames scheme
      OperatorAliasSchemeTypeBinding _ scheme -> schemeNames scheme
      ConstructorTypeBinding name _ fields -> Set.insert name (foldMap fieldNames fields)
      _ -> Set.empty
    schemeNames scheme =
      Set.unions
        [ typeNames (schemeResultType scheme),
          foldMap (foldMap typeNames) (schemeClassConstraints scheme),
          foldMap (foldMap typeNames) (schemePrimitiveConstraints scheme),
          let facts = schemeDefiningCapabilities scheme
           in foldMap (\(ConcreteImplFact _ value) -> typeNames value) (scopeConcreteImplFacts facts)
                <> foldMap (\(ClassMethodType _ value) -> typeNames value) (scopeClassMethodSignatures facts)
                <> foldMap (foldMap (typeNames . implMethodTarget)) (scopeConcreteImplMethods facts)
        ]

declaredInterfaceInventory :: ModuleInterface -> ModuleExportInventory
declaredInterfaceInventory interface =
  exportInventory
    ( Map.keys (interfaceValueBindings interface)
        <> [ ModuleExport TypeNamespace (renderName name)
           | name <- Map.keys (interfaceDataTypes interface)
           ]
        <> [ ModuleExport CapabilityNamespace (renderCapabilityId name)
           | name <- Map.keys (interfaceClassFacts interface)
           ]
    )

emptyModuleInterface :: ModuleInterface
emptyModuleInterface =
  ModuleInterface
    { interfacePublicExports = exportInventory [],
      interfaceValueBindings = Map.empty,
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
