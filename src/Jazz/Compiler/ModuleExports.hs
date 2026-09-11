{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared typed inventory for source and compiled module exports.
module Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleTypeConstructorSelector (..),
    ModuleExportSelector (..),
    moduleExportSelectorName,
    moduleExportSelectorNamespace,
    qualifyModuleExportSelectorSpans,
    ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
    exportedConstructorOwners,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    declarationExportNames,
    selectorEligibleNames,
    inventoryHasSelector,
    renderModuleExportSelector,
    selectExportNames,
    selectModuleExportSelectors,
    selectValidatedModuleExportSelectors,
    inventoryHasExport,
    restrictExportInventory,
    firstExportNamespace,
  )
where

import Control.DeepSeq (NFData (..))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.Diagnostics (SourceSpan, qualifySourceSpan)
import Jazz.Compiler.Name (NameNamespace (..))

data LocatedModuleExportName = LocatedModuleExportName
  { locatedModuleExportName :: Text,
    locatedModuleExportSpan :: SourceSpan
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ModuleTypeConstructorSelector
  = AbstractType
  | AllTypeConstructors SourceSpan
  | SelectedTypeConstructors (NonEmpty LocatedModuleExportName)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ModuleExportSelector
  = ModuleExportSelector (Maybe NameNamespace) Text
  | ModuleTypeExportSelector Text SourceSpan ModuleTypeConstructorSelector
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

moduleExportSelectorName :: ModuleExportSelector -> Text
moduleExportSelectorName selector =
  case selector of
    ModuleExportSelector _ name -> name
    ModuleTypeExportSelector name _ _ -> name

moduleExportSelectorNamespace :: ModuleExportSelector -> Maybe NameNamespace
moduleExportSelectorNamespace selector =
  case selector of
    ModuleExportSelector namespace _ -> namespace
    ModuleTypeExportSelector {} -> Just TypeNamespace

qualifyModuleExportSelectorSpans :: FilePath -> ModuleExportSelector -> ModuleExportSelector
qualifyModuleExportSelectorSpans sourcePath selector =
  case selector of
    ModuleExportSelector {} -> selector
    ModuleTypeExportSelector typeName typeSpan constructorSelector ->
      ModuleTypeExportSelector
        typeName
        (qualifySourceSpan sourcePath typeSpan)
        (qualifyConstructorSelectorSpans constructorSelector)
  where
    qualifyConstructorSelectorSpans constructorSelector =
      case constructorSelector of
        AbstractType -> AbstractType
        AllTypeConstructors allSpan ->
          AllTypeConstructors (qualifySourceSpan sourcePath allSpan)
        SelectedTypeConstructors constructors ->
          SelectedTypeConstructors (fmap qualifyLocatedName constructors)

    qualifyLocatedName locatedName =
      locatedName
        { locatedModuleExportSpan =
            qualifySourceSpan sourcePath (locatedModuleExportSpan locatedName)
        }

data ModuleExport = ModuleExport
  { moduleExportNamespace :: NameNamespace,
    moduleExportName :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data ModuleExportInventory = ModuleExportInventory
  { inventoryEntries :: Set ModuleExport,
    inventoryConstructorOwners :: Map Text (Set Text)
  }
  deriving stock (Eq, Show)

instance NFData ModuleExportInventory where
  rnf (ModuleExportInventory entries constructorOwners) =
    rnf entries `seq` rnf constructorOwners

instance Semigroup ModuleExportInventory where
  left <> right =
    ModuleExportInventory
      { inventoryEntries = Set.union (inventoryEntries left) (inventoryEntries right),
        inventoryConstructorOwners =
          Map.unionWith
            Set.union
            (inventoryConstructorOwners left)
            (inventoryConstructorOwners right)
      }

instance Monoid ModuleExportInventory where
  mempty = ModuleExportInventory Set.empty Map.empty

exportInventory :: [ModuleExport] -> ModuleExportInventory
exportInventory entries = ModuleExportInventory (Set.fromList entries) Map.empty

exportInventoryEntries :: ModuleExportInventory -> Set ModuleExport
exportInventoryEntries = inventoryEntries

exportedConstructorOwners :: Text -> ModuleExportInventory -> Set Text
exportedConstructorOwners constructorName =
  Map.findWithDefault Set.empty constructorName . inventoryConstructorOwners

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
  case selector of
    ModuleExportSelector Nothing name -> "'" <> name <> "'"
    ModuleExportSelector (Just namespace) name ->
      moduleExportNamespaceKeyword namespace <> " '" <> name <> "'"
    ModuleTypeExportSelector typeName _ constructorSelector ->
      "type '" <> typeName <> renderConstructorSelector constructorSelector <> "'"
  where
    renderConstructorSelector constructorSelector =
      case constructorSelector of
        AbstractType -> ""
        AllTypeConstructors _ -> "(..)"
        SelectedTypeConstructors constructors ->
          "("
            <> Text.intercalate ", " (map locatedModuleExportName (NonEmpty.toList constructors))
            <> ")"

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
       in restrictExportInventory
            (Set.filter ((`Set.member` selectedNames) . moduleExportName) (exportInventoryEntries inventory))
            inventory

selectModuleExportSelectors :: [ModuleExportSelector] -> ModuleExportInventory -> ModuleExportInventory
selectModuleExportSelectors selectors inventory =
  restrictExportInventory
    ( Set.filter
        (\export -> any (`moduleExportSelectorMatches` export) selectors)
        (exportInventoryEntries inventory)
    )
    inventory

selectValidatedModuleExportSelectors ::
  Map Text (Set Text) ->
  [ModuleExportSelector] ->
  ModuleExportInventory ->
  ModuleExportInventory
selectValidatedModuleExportSelectors constructorOwners selectors inventory =
  foldMap selectedInventory selectors
  where
    selectedInventory selector =
      case selector of
        ModuleExportSelector {} ->
          selectModuleExportSelectors [selector] inventory
        ModuleTypeExportSelector typeName _ constructorSelector ->
          exportInventory [ModuleExport TypeNamespace typeName]
            <> constructorInventory typeName (selectedConstructorEntries typeName constructorSelector)

    selectedConstructorEntries typeName constructorSelector =
      case constructorSelector of
        AbstractType -> Set.empty
        AllTypeConstructors _ ->
          Set.map (ModuleExport ConstructorNamespace) (Map.findWithDefault Set.empty typeName constructorOwners)
        SelectedTypeConstructors constructors ->
          Set.fromList
            [ ModuleExport ConstructorNamespace (locatedModuleExportName constructor)
            | constructor <- NonEmpty.toList constructors
            ]

    constructorInventory typeName entries =
      ModuleExportInventory
        { inventoryEntries = entries,
          inventoryConstructorOwners =
            Map.fromList
              [ (moduleExportName entry, Set.singleton typeName)
              | entry <- Set.toList entries
              ]
        }

moduleExportSelectorMatches :: ModuleExportSelector -> ModuleExport -> Bool
moduleExportSelectorMatches selector export =
  moduleExportSelectorName selector == moduleExportName export
    && case moduleExportSelectorNamespace selector of
      Nothing -> True
      Just namespace -> namespace == moduleExportNamespace export

restrictExportInventory :: Set ModuleExport -> ModuleExportInventory -> ModuleExportInventory
restrictExportInventory selectedEntries inventory =
  ModuleExportInventory
    { inventoryEntries = selectedEntries,
      inventoryConstructorOwners =
        Map.mapMaybe
          retainSelectedOwners
          ( Map.restrictKeys
              (inventoryConstructorOwners inventory)
              selectedConstructorNames
          )
    }
  where
    selectedConstructorNames =
      Set.map
        moduleExportName
        (Set.filter ((== ConstructorNamespace) . moduleExportNamespace) selectedEntries)
    selectedTypeNames =
      Set.map
        moduleExportName
        (Set.filter ((== TypeNamespace) . moduleExportNamespace) selectedEntries)
    retainSelectedOwners owners =
      case Set.intersection selectedTypeNames owners of
        selectedOwners
          | Set.null selectedOwners -> Nothing
          | otherwise -> Just selectedOwners

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
