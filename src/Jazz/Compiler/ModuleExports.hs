{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
    selectValidatedModuleExportSelectors,
    visibleImportInventory,
    inventoryHasExport,
    firstExportNamespace,
  )
where

import Control.DeepSeq (NFData)
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

newtype ModuleExportInventory = ModuleExportInventory (Set ModuleExport)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup ModuleExportInventory where
  ModuleExportInventory left <> ModuleExportInventory right =
    ModuleExportInventory (Set.union left right)

instance Monoid ModuleExportInventory where
  mempty = ModuleExportInventory Set.empty

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
            <> exportInventory
              (Set.toList (selectedConstructorEntries typeName constructorSelector))

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
