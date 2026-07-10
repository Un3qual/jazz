{-# LANGUAGE OverloadedStrings #-}

-- | Shared typed inventory for source and compiled module exports.
module JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    selectorEligibleNames,
    selectExportNames,
    visibleImportInventory,
    inventoryHasExport,
    firstExportNamespace
  )
where

import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Name (NameNamespace (..))

data ModuleExport = ModuleExport
  { moduleExportNamespace :: NameNamespace,
    moduleExportName :: Text
  }
  deriving (Eq, Ord, Show)

newtype ModuleExportInventory = ModuleExportInventory (Set ModuleExport)
  deriving (Eq, Show)

data ModuleImportMode
  = UnqualifiedImport
  | QualifiedAliasImport
  deriving (Eq, Show)

exportInventory :: [ModuleExport] -> ModuleExportInventory
exportInventory = ModuleExportInventory . Set.fromList

exportInventoryEntries :: ModuleExportInventory -> Set ModuleExport
exportInventoryEntries (ModuleExportInventory entries) = entries

exportNamesInNamespace :: NameNamespace -> ModuleExportInventory -> Set Text
exportNamesInNamespace namespace =
  Set.map moduleExportName
    . Set.filter ((== namespace) . moduleExportNamespace)
    . exportInventoryEntries

exportNamesInNamespaces :: [NameNamespace] -> ModuleExportInventory -> Set Text
exportNamesInNamespaces namespaces inventory =
  Set.unions [exportNamesInNamespace namespace inventory | namespace <- namespaces]

selectorEligibleNames :: ModuleExportInventory -> Set Text
selectorEligibleNames =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace, CapabilityNamespace]

selectExportNames :: Maybe [Text] -> ModuleExportInventory -> ModuleExportInventory
selectExportNames maybeNames inventory =
  case maybeNames of
    Nothing -> inventory
    Just names ->
      let selectedNames = Set.fromList names
       in ModuleExportInventory
            ( Set.filter
                ((`Set.member` selectedNames) . moduleExportName)
                (exportInventoryEntries inventory)
            )

visibleImportInventory ::
  ModuleImportMode ->
  Maybe [Text] ->
  ModuleExportInventory ->
  ModuleExportInventory
visibleImportInventory mode maybeNames inventory =
  case mode of
    UnqualifiedImport -> selected
    QualifiedAliasImport ->
      ModuleExportInventory
        ( Set.filter
            ( (`elem` [ValueNamespace, ConstructorNamespace])
                . moduleExportNamespace
            )
            (exportInventoryEntries selected)
        )
  where
    selected = selectExportNames maybeNames inventory

inventoryHasExport :: ModuleExport -> ModuleExportInventory -> Bool
inventoryHasExport export = Set.member export . exportInventoryEntries

firstExportNamespace ::
  [NameNamespace] ->
  Text ->
  ModuleExportInventory ->
  Maybe NameNamespace
firstExportNamespace namespaces name inventory =
  find
    (\namespace -> inventoryHasExport (ModuleExport namespace name) inventory)
    namespaces
