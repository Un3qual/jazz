{-# LANGUAGE DataKinds #-}

-- | Preserve authored export order and construct interfaces from analyzed data
-- declarations and the function schemes selected by checked construction.
module Jazz.Compiler.TypedCore.Build.Exports (buildExports, sourceOrderedExports) where

import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (CorePhase (Analyzed), DataConstructor (..), Statement (..))
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExport (..),
    ModuleExportInventory,
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
    exportedConstructorOwners,
    inventoryHasExport,
  )
import Jazz.Compiler.Name (GeneratedNameKind (OperatorBinding), Name (..), NameNamespace (..), ResolvedName, identifierText)
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Build.Result
  ( TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionPath (..),
  )
import Jazz.Compiler.TypedCore.Build.StructuredValues (StructuredValueCatalog, structuredDataStatement)

buildExports :: [Text] -> ModuleExportInventory -> [ModuleExport] -> [Statement 'Analyzed] -> StructuredValueCatalog -> Map.Map ResolvedName (Maybe TypedScheme) -> ([TypedCoreProductionFailure], TypedModuleInterface)
buildExports modulePath publicInventory orderedExports analyzedStatements structuredCatalog functionSchemes =
  let (reversedFailures, TypedModuleInterface reversedValues datas classes impls) =
        foldl'
          collect
          ([], TypedModuleInterface [] selectedDataInterfaces [] [])
          orderedExports
   in (reverse reversedFailures, TypedModuleInterface (reverse reversedValues) datas classes impls)
  where
    localDataDeclarations =
      [ ( identifierText sourceName,
          Set.fromList [identifierText constructorName | DataConstructor _ constructorName _ <- constructors],
          declaration
        )
      | (statementIndex, SData _ sourceName _ constructors) <- zip [0 ..] analyzedStatements,
        Just (TypedDataStatement declaration) <- [structuredDataStatement structuredCatalog statementIndex]
      ]

    selectedDataInterfaces =
      [ TypedDataInterface declaration
      | (typeName, _, declaration) <- localDataDeclarations,
        Set.member typeName selectedDataNames
      ]

    localDataByName =
      Map.fromList
        [(typeName, declaration) | (typeName, _, declaration) <- localDataDeclarations]

    visibleConstructorOwners =
      Map.fromList
        [ (constructorName, typeName)
        | (typeName, constructorNames, _) <- localDataDeclarations,
          constructorName <- Set.toList constructorNames
        ]

    constructorExportRepresentable constructorName =
      case selectedConstructorOwner constructorName of
        Just owner -> flattenedConstructorOwner constructorName == Just owner
        Nothing -> False

    selectedConstructorOwner constructorName =
      declaredConstructorOwner constructorName
        <|> Map.lookup constructorName visibleConstructorOwners

    declaredConstructorOwner constructorName =
      case Set.toList
        ( exportedConstructorOwners
            constructorName
            publicInventory
        ) of
        [owner] -> Just owner
        _ -> Nothing

    flattenedConstructorOwner constructorName =
      case exportedCandidates of
        [owner] -> Just owner
        [] -> listToMaybe (reverse candidates)
        _ -> Nothing
      where
        candidates =
          [ typeName
          | (typeName, constructorNames, _) <- localDataDeclarations,
            Set.member typeName selectedDataNames,
            Set.member constructorName constructorNames
          ]
        exportedCandidates =
          [ typeName
          | typeName <- candidates,
            ModuleExport TypeNamespace typeName `elem` orderedExports
          ]

    selectedDataNames =
      closeDataNames
        ( Set.fromList
            (directlySelectedDataNames <> exportedValueDataNames)
        )

    directlySelectedDataNames =
      concatMap dataNamesForExport orderedExports

    dataNamesForExport (ModuleExport namespace name) =
      case namespace of
        TypeNamespace
          | Map.member name localDataByName -> [name]
        ConstructorNamespace ->
          case selectedConstructorOwner name of
            Just owner -> [owner]
            Nothing -> []
        _ -> []

    exportedValueDataNames =
      [ dependencyName
      | ModuleExport ValueNamespace exportName <- orderedExports,
        Just (TypedScheme _ _ _ _ expressionType _ _) <- exportedFunctions exportName,
        dependencyName <- localTypedDataIdentifiers expressionType,
        Map.member dependencyName localDataByName
      ]

    exportedFunctions exportName =
      [ selectedScheme
      | (sourceName, selectedScheme) <- Map.toList functionSchemes,
        identifierText sourceName == exportName
      ]

    closeDataNames initial = go initial initial
      where
        go selected pending
          | Set.null pending = selected
          | otherwise =
              let dependencies =
                    Set.fromList
                      [ dependencyName
                      | selectedName <- Set.toList pending,
                        declaration <- maybe [] (: []) (Map.lookup selectedName localDataByName),
                        dependencyName <- dataDeclarationDependencies declaration,
                        Map.member dependencyName localDataByName
                      ]
                  unseen = Set.difference dependencies selected
               in go (Set.union selected unseen) unseen

    dataDeclarationDependencies (TypedDataDeclaration _ _ _ constructors) =
      concat
        [ concatMap localTypedDataIdentifiers fields
        | TypedConstructorDeclaration _ _ fields _ <- constructors
        ]

    localTypedDataIdentifiers typeValue =
      case typeValue of
        SemanticList elementType -> localTypedDataIdentifiers elementType
        SemanticTuple elementTypes -> concatMap localTypedDataIdentifiers elementTypes
        SemanticData name arguments ->
          localIdentifier name <> concatMap localTypedDataIdentifiers arguments
        SemanticFunction argument result ->
          localTypedDataIdentifiers argument <> localTypedDataIdentifiers result
        _ -> []

    localIdentifier name =
      case name of
        TypedResolvedName TypedCurrentModule TypedTypeNamespace identifier -> [identifier]
        _ -> []

    collect (reversedFailures, TypedModuleInterface reversedValues datas classes impls) (ModuleExport namespace name)
      | namespace == ValueNamespace =
          case exportedFunctions name of
            [Just selectedScheme] ->
              let typedName = TypedResolvedName TypedCurrentModule TypedValueNamespace name
               in (reversedFailures, TypedModuleInterface (TypedValueInterface typedName selectedScheme : reversedValues) datas classes impls)
            [Nothing] -> (reversedFailures, TypedModuleInterface reversedValues datas classes impls)
            _ -> (TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name) : reversedFailures, TypedModuleInterface reversedValues datas classes impls)
      | namespace == TypeNamespace,
        Map.member name localDataByName =
          (reversedFailures, TypedModuleInterface reversedValues datas classes impls)
      | namespace == ConstructorNamespace,
        Map.member name visibleConstructorOwners,
        constructorExportRepresentable name =
          (reversedFailures, TypedModuleInterface reversedValues datas classes impls)
      | otherwise =
          (TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedExport (TypedCoreNameDetail name) : reversedFailures, TypedModuleInterface reversedValues datas classes impls)

sourceOrderedExports :: ModuleExportInventory -> Maybe [ModuleExportSelector] -> [Statement 'Analyzed] -> [ModuleExport]
sourceOrderedExports publicInventory selectors analyzedStatements =
  stableUniqueExports
    ( case selectors of
        Nothing -> filter publicExport sourceOrderedDeclarations
        Just selected -> concatMap exportsForSelector selected
    )
  where
    publicExport = (`inventoryHasExport` publicInventory)
    exportsForSelector selector =
      case selector of
        ModuleExportSelector maybeNamespace name ->
          filter
            ( \export ->
                moduleExportName export == name
                  && maybe True (== moduleExportNamespace export) maybeNamespace
                  && publicExport export
            )
            sourceOrderedDeclarations
        ModuleTypeExportSelector typeName _ constructorSelector ->
          filter publicExport [ModuleExport TypeNamespace typeName]
            <> constructorExports typeName constructorSelector

    constructorExports typeName constructorSelector =
      case constructorSelector of
        AbstractType -> []
        AllTypeConstructors _ ->
          filter
            ( \export ->
                moduleExportNamespace export == ConstructorNamespace
                  && Set.member typeName (exportedConstructorOwners (moduleExportName export) publicInventory)
                  && publicExport export
            )
            sourceOrderedDeclarations
        SelectedTypeConstructors constructors ->
          filter
            publicExport
            [ ModuleExport ConstructorNamespace (locatedModuleExportName constructor)
            | constructor <- NonEmpty.toList constructors
            ]
    sourceOrderedDeclarations =
      concatMap statementExports analyzedStatements

    statementExports statement =
      case statement of
        SLet _ name _
          | not (generatedOperatorName name) ->
              [ModuleExport ValueNamespace (identifierText name)]
        SData _ typeName _ constructors ->
          ModuleExport TypeNamespace (identifierText typeName)
            : [ ModuleExport ConstructorNamespace (identifierText constructorName)
              | DataConstructor _ constructorName _ <- constructors
              ]
        SClass _ className _ _ ->
          [ModuleExport CapabilityNamespace (identifierText className)]
        _ -> []

    generatedOperatorName name =
      case name of
        GeneratedName (OperatorBinding _) -> True
        _ -> False

    stableUniqueExports = reverse . snd . foldl' keep (Set.empty, [])
      where
        keep (seen, exports) export
          | Set.member export seen = (seen, exports)
          | otherwise = (Set.insert export seen, export : exports)
