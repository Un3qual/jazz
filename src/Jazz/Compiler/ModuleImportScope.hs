{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Resolved import visibility, shared by name resolution, checking and execution.
-- Only import validation constructs populated scopes.
module Jazz.Compiler.ModuleImportScope
  ( BindingOrigin (..),
    ValidatedImportScope (..),
    emptyImportScope,
    importedNameOrigins,
    dependencyImportViews,
  )
where

import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.Diagnostics (SourceSpan)
import Jazz.Compiler.ModuleExports (ModuleExport (..), ModuleExportInventory, exportInventoryEntries, restrictExportInventory)
import Jazz.Compiler.ModuleIdentity (ModulePath)
import Jazz.Compiler.Name (NameNamespace)

-- | Origin metadata for imported bindings/aliases used in collision
-- diagnostics.
data BindingOrigin = BindingOrigin
  { bindingOriginModulePath :: ModulePath,
    bindingOriginSpan :: SourceSpan
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Successful import validation is the owner of imported visibility. Resolution
-- consumes these namespace-aware targets instead of selecting exports again.
data ValidatedImportScope = ValidatedImportScope
  { importScopeAliases :: Map Text BindingOrigin,
    importScopeNames :: Map NameNamespace (Map Text (NonEmpty BindingOrigin)),
    importScopeInventories :: Map ModulePath ModuleExportInventory
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

emptyImportScope :: ValidatedImportScope
emptyImportScope = ValidatedImportScope Map.empty Map.empty Map.empty

importedNameOrigins :: NameNamespace -> ValidatedImportScope -> Map Text (NonEmpty ModulePath)
importedNameOrigins namespace =
  Map.map (fmap bindingOriginModulePath) . Map.findWithDefault Map.empty namespace . importScopeNames

-- | Alias views retain all public names; unqualified views use the exact
-- namespace selection already accepted by validation. No selector is rerun.
dependencyImportViews :: ModulePath -> ValidatedImportScope -> [(Maybe Text, ModuleExportInventory)]
dependencyImportViews path scope = case Map.lookup path (importScopeInventories scope) of
  Nothing -> []
  Just inventory ->
    map snd $
      sortOn fst $
        [ (spanValue, (Nothing, restrictExportInventory (Set.intersection (exportInventoryEntries inventory) selected) inventory))
        | (spanValue, selected) <- Map.toList unqualifiedSelections
        ]
          <> [ (bindingOriginSpan origin, (Just alias, inventory))
             | (alias, origin) <- Map.toList (importScopeAliases scope),
               bindingOriginModulePath origin == path
             ]
  where
    unqualifiedSelections =
      Map.fromListWith
        Set.union
        [ (bindingOriginSpan origin, Set.singleton (ModuleExport namespace name))
        | (namespace, names) <- Map.toList (importScopeNames scope),
          (name, origins) <- Map.toList names,
          origin <- NonEmpty.toList origins,
          bindingOriginModulePath origin == path
        ]
