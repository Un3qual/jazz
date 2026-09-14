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
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CapabilityMethodKey, CoreBinderId, ResolvedReference, renderCapabilityId)
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
    inventoryHasExport,
    restrictExportInventory,
    withClassMethods,
  )
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..))
import Jazz.Compiler.Name (Name (..), NameNamespace (..), ResolvedName, ResolvedNameOrigin (..), ResolvedUserName (..), identifierText, renderName)
import Jazz.Compiler.SemanticDeclarations
  ( ClassDefinition (..),
    ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    DeclarationVariable,
    ImplementationTemplate (..),
    SchemeConstraint (..),
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
    implementationTarget,
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
  { interfaceBindingReference :: ResolvedReference,
    interfaceBindingType :: SemanticBinding DeclarationVariable
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ModuleInterface = ModuleInterface
  { interfacePublicExports :: ModuleExportInventory,
    interfaceValueBindings :: Map ModuleExport ModuleValueBinding,
    interfaceDataTypes :: Map ResolvedName DataTypeBinding,
    interfaceCapabilities :: ScopeCapabilityFacts
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
  public
    { interfaceValueBindings = Map.map (\binding -> binding {interfaceBindingType = publishBindingNames (interfaceBindingType binding)}) (interfaceValueBindings public),
      interfaceDataTypes = Map.mapKeys publishedName (Map.map publishDataNames (reachableTypes roots)),
      interfaceCapabilities = publishCapabilityNames publicCapabilities
    }
  where
    available = declaredInterfaceInventory declarations
    exports = maybe available (\inventory -> restrictExportInventory (Set.intersection (exportInventoryEntries available) (exportInventoryEntries inventory)) inventory) requested
    public =
      declarations
        { interfacePublicExports = exports,
          interfaceValueBindings = Map.filterWithKey (\name _ -> inventoryHasExport name exports) (interfaceValueBindings declarations),
          interfaceCapabilities = publicCapabilities
        }
    capabilities = interfaceCapabilities declarations
    publicCapabilities = capabilities
    roots =
      Set.unions
        [ Map.keysSet (Map.filterWithKey (\name _ -> inventoryHasExport (ModuleExport TypeNamespace (renderName name)) exports) (interfaceDataTypes declarations)),
          foldMap (bindingNames . interfaceBindingType) (interfaceValueBindings public),
          foldMap (schemeNames . classMethodScheme) (scopeClassMethodSignatures publicCapabilities),
          foldMap (schemeNames . implementationScheme) (scopeImplementations publicCapabilities)
        ]
    reachableTypes = visitTypes Set.empty
    visitTypes seen pending = case Set.minView pending of
      Nothing -> Map.restrictKeys typeDefinitions seen
      Just (name, remaining) ->
        let visited = Set.insert name seen
            dependencies = maybe Set.empty (\(DataTypeBinding _ constructors) -> foldMap (foldMap fieldNames) constructors) (Map.lookup name typeDefinitions)
         in visitTypes visited (remaining <> Set.difference dependencies visited)

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
           in foldMap (typeNames . implementationTarget) (scopeImplementations facts)
                <> foldMap (\(ClassMethodType _ value) -> typeNames value) (scopeClassMethodSignatures facts)
        ]

declaredInterfaceInventory :: ModuleInterface -> ModuleExportInventory
declaredInterfaceInventory interface =
  withClassMethods (Map.fromListWith Set.union [(renderCapabilityId capability, Set.singleton (identifierText member)) | (capability, member) <- Map.keys (scopeClassMethodSignatures (interfaceCapabilities interface))]) $
    exportInventory
      ( Map.keys (interfaceValueBindings interface)
          <> [ ModuleExport TypeNamespace (renderName name)
             | name <- Map.keys (interfaceDataTypes interface)
             ]
          <> [ ModuleExport CapabilityNamespace (renderCapabilityId name)
             | name <- Map.keys (scopeClassFacts (interfaceCapabilities interface))
             ]
      )

emptyModuleInterface :: ModuleInterface
emptyModuleInterface =
  ModuleInterface
    { interfacePublicExports = exportInventory [],
      interfaceValueBindings = Map.empty,
      interfaceDataTypes = Map.empty,
      interfaceCapabilities = mempty
    }

-- Publication gives nominal names their external diagnostic spelling once.
-- Their defining owner (and therefore equality/ordering) is unchanged. Import
-- aliases never rewrite declaration identities or their nested semantic types.
publishedName :: ResolvedName -> ResolvedName
publishedName (UserName (ResolvedUserName (LocalDeclaration (NamedSourceUnit path)) namespace identifier)) =
  UserName (ResolvedUserName (ImportedModule path) namespace identifier)
publishedName name = name

publishedCapability :: CapabilityId -> CapabilityId
publishedCapability (CapabilityId name) = CapabilityId (publishedName name)

publishedMethod :: CapabilityMethodKey -> CapabilityMethodKey
publishedMethod = first publishedCapability

publishBindingNames :: SemanticBinding variable -> SemanticBinding variable
publishBindingNames binding = case binding of
  PlainTypeBinding value -> PlainTypeBinding (first publishedName value)
  SchemeTypeBinding scheme -> SchemeTypeBinding (publishSchemeNames scheme)
  OperatorAliasSchemeTypeBinding symbol scheme -> OperatorAliasSchemeTypeBinding symbol (publishSchemeNames scheme)
  ConstructorTypeBinding name parameters fields -> ConstructorTypeBinding (publishedName name) parameters (map publishFieldNames fields)
  _ -> binding

publishSchemeNames :: SemanticScheme variable -> SemanticScheme variable
publishSchemeNames scheme =
  scheme
    { schemeClassConstraints = map publishConstraint (schemeClassConstraints scheme),
      schemePrimitiveConstraints = map (fmap (first publishedName)) (schemePrimitiveConstraints scheme),
      schemeDefiningCapabilities = publishCapabilityNames (schemeDefiningCapabilities scheme),
      schemeResultType = first publishedName (schemeResultType scheme)
    }
  where
    publishConstraint constraint = case constraint of
      TypeSchemeConstraint capability value -> TypeSchemeConstraint (publishedCapability capability) (first publishedName value)
      TypeSchemeInferredConstraint capability value -> TypeSchemeInferredConstraint (publishedCapability capability) (first publishedName value)
      TypeSchemeMethodConstraint capability method value -> TypeSchemeMethodConstraint (publishedCapability capability) (publishedMethod method) (first publishedName value)

publishCapabilityNames :: ScopeCapabilityFacts -> ScopeCapabilityFacts
publishCapabilityNames facts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.mapKeys publishedCapability (Map.map (\definition -> definition {classSuperclasses = map publishedCapability (classSuperclasses definition)}) (scopeClassFacts facts)),
      scopeGeneratedEqualityClassFacts = Set.map publishedCapability (scopeGeneratedEqualityClassFacts facts),
      scopeClassMethodSignatures = Map.mapKeys publishedMethod (Map.map publishMethodType (scopeClassMethodSignatures facts)),
      scopeImplementations = Map.map publishImplementation (scopeImplementations facts)
    }

publishImplementation :: ImplementationTemplate -> ImplementationTemplate
publishImplementation template =
  template
    { implementationCapability = publishedCapability (implementationCapability template),
      implementationScheme = publishDeclarationScheme (implementationScheme template)
    }

publishMethodType :: ClassMethodType -> ClassMethodType
publishMethodType (ClassMethodScheme parameter scheme) = ClassMethodScheme parameter (publishDeclarationScheme scheme)

publishDeclarationScheme :: SemanticScheme variable -> SemanticScheme variable
publishDeclarationScheme scheme =
  scheme
    { schemeResultType = first publishedName (schemeResultType scheme),
      schemeClassConstraints = map publishConstraint (schemeClassConstraints scheme)
    }
  where
    publishConstraint constraint = case constraint of
      TypeSchemeConstraint capability target -> TypeSchemeConstraint (publishedCapability capability) (first publishedName target)
      TypeSchemeInferredConstraint capability target -> TypeSchemeInferredConstraint (publishedCapability capability) (first publishedName target)
      TypeSchemeMethodConstraint capability method target -> TypeSchemeMethodConstraint (publishedCapability capability) (publishedMethod method) (first publishedName target)

publishFieldNames :: ConstructorArgumentType -> ConstructorArgumentType
publishFieldNames (ConstructorArgumentType value) = ConstructorArgumentType (first publishedName value)
publishFieldNames ConstructorArgumentFresh = ConstructorArgumentFresh

publishDataNames :: DataTypeBinding -> DataTypeBinding
publishDataNames binding = binding {dataTypeConstructors = map (map publishFieldNames) (dataTypeConstructors binding)}

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
