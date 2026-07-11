{-# LANGUAGE OverloadedStrings #-}

-- | Shared typed inventory for source and compiled module exports.
module JazzNext.Compiler.ModuleExports
  ( ModuleExportSelector (..),
    ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    declarationExportNames,
    selectorEligibleNames,
    inventoryHasSelector,
    renderModuleExportSelector,
    selectExportNames,
    selectModuleExportSelectors,
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

data ModuleExportSelector = ModuleExportSelector
  { moduleExportSelectorNamespace :: Maybe NameNamespace,
    moduleExportSelectorName :: Text
  }
  deriving (Eq, Ord, Show)

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

declarationExportNames :: ModuleExportInventory -> Set Text
declarationExportNames =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]

selectorEligibleNames :: ModuleExportInventory -> Set Text
selectorEligibleNames =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace, CapabilityNamespace]

inventoryHasSelector :: ModuleExportSelector -> ModuleExportInventory -> Bool
inventoryHasSelector selector =
  any (moduleExportSelectorMatches selector) . Set.toList . exportInventoryEntries

renderModuleExportSelector :: ModuleExportSelector -> Text
renderModuleExportSelector selector =
  case moduleExportSelectorNamespace selector of
    Nothing -> "'" <> moduleExportSelectorName selector <> "'"
    Just namespace ->
      moduleExportNamespaceKeyword namespace
        <> " '"
        <> moduleExportSelectorName selector
        <> "'"

moduleExportNamespaceKeyword :: NameNamespace -> Text
moduleExportNamespaceKeyword namespace =
  case namespace of
    ValueNamespace -> "value"
    ConstructorNamespace -> "constructor"
    TypeNamespace -> "type"
    CapabilityNamespace -> "class"

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

selectModuleExportSelectors :: [ModuleExportSelector] -> ModuleExportInventory -> ModuleExportInventory
selectModuleExportSelectors selectors inventory =
  ModuleExportInventory
    ( Set.filter
        (\export -> any (`moduleExportSelectorMatches` export) selectors)
        (exportInventoryEntries inventory)
    )

moduleExportSelectorMatches :: ModuleExportSelector -> ModuleExport -> Bool
moduleExportSelectorMatches selector export =
  moduleExportSelectorName selector == moduleExportName export
    && case moduleExportSelectorNamespace selector of
      Nothing -> True
      Just namespace -> namespace == moduleExportNamespace export

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
            ( (`elem` [ValueNamespace, ConstructorNamespace, TypeNamespace])
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
