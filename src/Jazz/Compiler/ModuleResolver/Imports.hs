{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Import visibility validation and declaration views.
module Jazz.Compiler.ModuleResolver.Imports
  ( declaredImportSpan,
    ResolverImport,
    resolverImportModulePath,
    resolverImportAlias,
    resolverImportSymbols,
    ValidatedImportScope,
    emptyImportScope,
    importScopeAliases,
    importScopeInventories,
    importedNameOrigins,
    BindingOrigin (..),
    validateImportBindings,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.List
  ( sortOn,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( listToMaybe,
  )
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CoreNode (..),
    CorePhase (..),
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkErrorDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    exportInventory,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    selectExportNames,
    selectorEligibleNames,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    moduleQualifierIdentifier,
    renderModulePath,
  )
import Jazz.Compiler.Name
  ( NameNamespace (..),
    identifierText,
  )
import Jazz.Compiler.SourceSpan (unqualifySourceSpan)

-- | Origin metadata for imported bindings/aliases used in collision
-- diagnostics.
data BindingOrigin = BindingOrigin
  { bindingOriginModulePath :: ModulePath,
    bindingOriginSpan :: SourceSpan
  }

-- | Successful import validation is the owner of imported visibility. Resolution
-- consumes these namespace-aware targets instead of selecting exports again.
data ValidatedImportScope = ValidatedImportScope
  { importScopeAliases :: Map Text BindingOrigin,
    importScopeNames :: Map NameNamespace (Map Text BindingOrigin),
    importScopeInventories :: Map ModulePath ModuleExportInventory
  }

emptyImportScope :: ValidatedImportScope
emptyImportScope = ValidatedImportScope Map.empty Map.empty Map.empty

importedNameOrigins :: NameNamespace -> ValidatedImportScope -> Map Text ModulePath
importedNameOrigins namespace =
  Map.map bindingOriginModulePath . Map.findWithDefault Map.empty namespace . importScopeNames

declaredImportSpan :: ModuleGraph.ModuleImport 'Lowered -> SourceSpan
declaredImportSpan importDecl =
  unqualifySourceSpan (coreNodeSpan (ModuleGraph.moduleImportNode importDecl))

declaredImportAliasText :: ModuleGraph.ModuleImport 'Lowered -> Maybe Text
declaredImportAliasText = fmap (identifierText . moduleQualifierIdentifier) . ModuleGraph.importAlias

declaredImportSymbols :: ModuleGraph.ModuleImport 'Lowered -> Maybe [Text]
declaredImportSymbols importDecl =
  case ModuleGraph.importExposure importDecl of
    ModuleGraph.DeclaredImportAll _ -> Nothing
    ModuleGraph.DeclaredImportOnly _ names -> Just (map identifierText (NonEmpty.toList names))

type ResolverImport = ModuleGraph.ModuleImport 'Lowered

resolverImportSpan :: ResolverImport -> SourceSpan
resolverImportSpan = declaredImportSpan

resolverImportModulePath :: ResolverImport -> ModulePath
resolverImportModulePath = ModuleGraph.importedModule

resolverImportAlias :: ResolverImport -> Maybe Text
resolverImportAlias = declaredImportAliasText

resolverImportSymbols :: ResolverImport -> Maybe [Text]
resolverImportSymbols = declaredImportSymbols

-- | Validate alias and explicit-symbol imports after dependencies have been
-- resolved so the exporting module inventories are known.
validateImportBindings ::
  FilePath ->
  ModulePath ->
  [ResolverImport] ->
  Set Text ->
  Set Text ->
  Set (Text, Text) ->
  Set (Text, Text) ->
  Map (Text, Text) (SourceSpan, SourceSpan) ->
  Set Text ->
  Set Text ->
  Map ModulePath ModuleExportInventory ->
  Either Diagnostic ValidatedImportScope
validateImportBindings sourcePath importerPath imports localClassNames referencedNames qualifiedReferences qualifiedTypeReferences qualifiedClassReferences ambientVisibleSymbols ambientVisibleClassNames inventoriesByModule = do
  aliases <- go Map.empty Map.empty Map.empty imports
  scope <- foldM collectImportScope (ValidatedImportScope aliases Map.empty inventoriesByModule) imports
  let visibleSymbols = Set.union (visibleNames ValueNamespace scope) (visibleNames ConstructorNamespace scope)
      visibleClassNames = visibleNames CapabilityNamespace scope
  validateQualifiedReferences (Set.unions [localClassNames, visibleClassNames, ambientVisibleClassNames])
  validateQualifiedTypeReferences
  mapM_ validateQualifiedClassReference (Map.toList qualifiedClassReferences)
  let visibleOrAmbientSymbols = Set.union visibleSymbols ambientVisibleSymbols
  case findHiddenExplicitImportReference visibleOrAmbientSymbols of
    Just (symbolName, importDecl) ->
      Left (mkHiddenExplicitImportSymbolError symbolName importDecl)
    Nothing ->
      case findHiddenAliasImportReference visibleOrAmbientSymbols of
        Just (symbolName, importDecl, aliasName) ->
          Left (mkHiddenAliasImportSymbolError symbolName importDecl aliasName)
        Nothing -> Right scope
  where
    visibleNames namespace = Map.keysSet . importedNameOrigins namespace

    dependencyInventory importDecl =
      Map.lookup (resolverImportModulePath importDecl) inventoriesByModule

    eligibleImportNames = selectorEligibleNames

    visibleUnqualifiedInventory importDecl inventory =
      case resolverImportAlias importDecl of
        Just _ -> exportInventory []
        Nothing ->
          selectExportNames
            (resolverImportSymbols importDecl)
            inventory

    aliasMemberNames inventory =
      exportNamesInNamespaces
        [ValueNamespace, ConstructorNamespace]
        (selectExportNames Nothing inventory)

    aliasTypeNames inventory =
      exportNamesInNamespace
        TypeNamespace
        (selectExportNames Nothing inventory)

    valueAndConstructorNames =
      exportNamesInNamespaces [ValueNamespace, ConstructorNamespace]

    go seenSymbols seenTypes seenAliases remainingImports =
      case remainingImports of
        [] ->
          Right seenAliases
        importDecl : rest -> do
          seenAliasesAfterImport <- validateImportAlias seenAliases importDecl
          seenSymbolsAfterImport <- validateImportSymbols seenSymbols importDecl
          seenTypesAfterImport <- validateImportTypes seenTypes importDecl
          go seenSymbolsAfterImport seenTypesAfterImport seenAliasesAfterImport rest

    validateImportAlias :: Map Text BindingOrigin -> ResolverImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportAlias seenAliases importDecl =
      case resolverImportAlias importDecl of
        Nothing ->
          Right seenAliases
        Just aliasName ->
          case Map.lookup aliasName seenAliases of
            Just previousOrigin ->
              Left (mkImportAliasCollisionError aliasName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    aliasName
                    BindingOrigin
                      { bindingOriginModulePath = resolverImportModulePath importDecl,
                        bindingOriginSpan = resolverImportSpan importDecl
                      }
                    seenAliases
                )

    validateImportSymbols :: Map Text BindingOrigin -> ResolverImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbols seenSymbols importDecl =
      case resolverImportAlias importDecl of
        Just _ ->
          Right seenSymbols
        Nothing ->
          case dependencyInventory importDecl of
            Nothing ->
              Left
                ( mkErrorDiagnostic
                    E4010
                    CompilationOrigin
                    ( "internal resolver error while validating imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (resolverImportModulePath importDecl)
                        <> "'"
                    )
                )
            Just inventory ->
              let exportedImportSymbols = eligibleImportNames inventory
                  importedSymbolNames =
                    case resolverImportSymbols importDecl of
                      Nothing -> Set.toAscList exportedImportSymbols
                      Just explicitSymbolNames -> explicitSymbolNames
               in foldM
                    (validateImportSymbol importDecl exportedImportSymbols)
                    seenSymbols
                    importedSymbolNames

    validateImportTypes :: Map Text BindingOrigin -> ResolverImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportTypes seenTypes importDecl =
      case resolverImportAlias importDecl of
        Just _ ->
          Right seenTypes
        Nothing ->
          case dependencyInventory importDecl of
            Nothing ->
              Left
                ( mkErrorDiagnostic
                    E4010
                    CompilationOrigin
                    ( "internal resolver error while validating type imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (resolverImportModulePath importDecl)
                        <> "'"
                    )
                )
            Just inventory ->
              foldM
                (validateImportType importDecl)
                seenTypes
                ( Set.toAscList
                    (exportNamesInNamespace TypeNamespace (visibleUnqualifiedInventory importDecl inventory))
                )

    validateImportType :: ResolverImport -> Map Text BindingOrigin -> Text -> Either Diagnostic (Map Text BindingOrigin)
    validateImportType importDecl seenTypes typeName =
      case Map.lookup typeName seenTypes of
        Just previousOrigin
          | bindingOriginModulePath previousOrigin == resolverImportModulePath importDecl ->
              Right seenTypes
          | otherwise ->
              Left (mkImportTypeCollisionError typeName previousOrigin importDecl)
        Nothing ->
          Right
            ( Map.insert
                typeName
                BindingOrigin
                  { bindingOriginModulePath = resolverImportModulePath importDecl,
                    bindingOriginSpan = resolverImportSpan importDecl
                  }
                seenTypes
            )

    validateQualifiedReferences :: Set Text -> Either Diagnostic ()
    validateQualifiedReferences visibleClassNames =
      foldM
        validateQualifiedReference
        ()
        (Set.toList qualifiedReferences)
      where
        validateQualifiedReference :: () -> (Text, Text) -> Either Diagnostic ()
        validateQualifiedReference () (aliasName, symbolName)
          | Set.member aliasName visibleClassNames =
              Right ()
          | otherwise =
              case findAliasImport aliasName of
                Nothing ->
                  Left (mkUnknownQualifiedAliasError aliasName symbolName)
                Just importDecl ->
                  case dependencyInventory importDecl of
                    Nothing ->
                      Left
                        ( mkErrorDiagnostic
                            E4010
                            CompilationOrigin
                            ( "internal resolver error while validating imports for '"
                                <> renderModulePath importerPath
                                <> "': missing exports for module '"
                                <> renderModulePath (resolverImportModulePath importDecl)
                                <> "'"
                            )
                        )
                    Just inventory ->
                      let exportedSymbols = aliasMemberNames inventory
                       in if Set.member symbolName exportedSymbols
                            then Right ()
                            else Left (mkMissingQualifiedAliasSymbolError symbolName importDecl aliasName exportedSymbols)

    validateQualifiedTypeReferences :: Either Diagnostic ()
    validateQualifiedTypeReferences =
      foldM
        validateQualifiedTypeReference
        ()
        (Set.toList qualifiedTypeReferences)
      where
        validateQualifiedTypeReference :: () -> (Text, Text) -> Either Diagnostic ()
        validateQualifiedTypeReference () (aliasName, typeName) =
          case findAliasImport aliasName of
            Nothing ->
              Left (mkUnknownQualifiedAliasError aliasName typeName)
            Just importDecl ->
              case dependencyInventory importDecl of
                Nothing ->
                  Left
                    ( mkErrorDiagnostic
                        E4010
                        CompilationOrigin
                        ( "internal resolver error while validating type imports for '"
                            <> renderModulePath importerPath
                            <> "': missing exports for module '"
                            <> renderModulePath (resolverImportModulePath importDecl)
                            <> "'"
                        )
                    )
                Just inventory ->
                  let exportedTypes = aliasTypeNames inventory
                   in if Set.member typeName exportedTypes
                        then Right ()
                        else Left (mkMissingQualifiedAliasSymbolError typeName importDecl aliasName exportedTypes)

    validateQualifiedClassReference ((aliasName, className), (aliasSpan, classSpan)) =
      case findAliasImport aliasName of
        Nothing -> Left (setDiagnosticPrimarySpan aliasSpan (mkUnknownQualifiedAliasError aliasName className))
        Just importDecl ->
          case dependencyInventory importDecl of
            Nothing -> Left (mkErrorDiagnostic E4010 CompilationOrigin "missing dependency inventory while validating a qualified class")
            Just inventory ->
              let classes = exportNamesInNamespace CapabilityNamespace inventory
               in if Set.member className classes
                    then Right ()
                    else Left (setDiagnosticPrimarySpan classSpan (mkMissingQualifiedAliasSymbolError className importDecl aliasName classes))

    findAliasImport :: Text -> Maybe ResolverImport
    findAliasImport aliasName =
      listToMaybe
        [ importDecl
        | importDecl <- imports,
          resolverImportAlias importDecl == Just aliasName
        ]

    validateImportSymbol ::
      ResolverImport ->
      Set Text ->
      Map Text BindingOrigin ->
      Text ->
      Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbol importDecl exportedSymbols seenSymbols symbolName
      | not (Set.member symbolName exportedSymbols) =
          Left (mkMissingImportSymbolError symbolName importDecl exportedSymbols)
      | otherwise =
          case Map.lookup symbolName seenSymbols of
            Just previousOrigin
              | bindingOriginModulePath previousOrigin == resolverImportModulePath importDecl ->
                  Right seenSymbols
              | otherwise ->
                  Left (mkImportSymbolCollisionError symbolName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    symbolName
                    BindingOrigin
                      { bindingOriginModulePath = resolverImportModulePath importDecl,
                        bindingOriginSpan = resolverImportSpan importDecl
                      }
                    seenSymbols
                )

    mkMissingImportSymbolError :: Text -> ResolverImport -> Set Text -> Diagnostic
    mkMissingImportSymbolError symbolName importDecl exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (resolverImportSpan importDecl)
          ( mkErrorDiagnostic
              E4007
              CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (resolverImportModulePath importDecl)
                  <> "' imported by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'; available exports: "
                  <> renderExports exportedSymbols
              )
          )

    mkImportSymbolCollisionError :: Text -> BindingOrigin -> ResolverImport -> Diagnostic
    mkImportSymbolCollisionError symbolName previousOrigin importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (resolverImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4008
                  CompilationOrigin
                  ( "import binding collision for symbol '"
                      <> symbolName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already imported from '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot re-import from '"
                      <> renderModulePath (resolverImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    mkImportTypeCollisionError :: Text -> BindingOrigin -> ResolverImport -> Diagnostic
    mkImportTypeCollisionError typeName previousOrigin importDecl =
      setDiagnosticSubject typeName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (resolverImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4008
                  CompilationOrigin
                  ( "import type collision for '"
                      <> typeName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already imported from '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot re-import from '"
                      <> renderModulePath (resolverImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    mkUnknownQualifiedAliasError :: Text -> Text -> Diagnostic
    mkUnknownQualifiedAliasError aliasName symbolName =
      setDiagnosticSubject aliasName $
        mkErrorDiagnostic
          E4013
          CompilationOrigin
          ( "qualified import alias '"
              <> aliasName
              <> "' is not declared in module '"
              <> renderModulePath importerPath
              <> "' while resolving '"
              <> aliasName
              <> "::"
              <> symbolName
              <> "' in '"
              <> Text.pack sourcePath
              <> "'"
          )

    mkMissingQualifiedAliasSymbolError :: Text -> ResolverImport -> Text -> Set Text -> Diagnostic
    mkMissingQualifiedAliasSymbolError symbolName importDecl aliasName exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (resolverImportSpan importDecl)
          ( mkErrorDiagnostic
              E4014
              CompilationOrigin
              ( "qualified import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (resolverImportModulePath importDecl)
                  <> "' imported as '"
                  <> aliasName
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'; available exports: "
                  <> renderExports exportedSymbols
              )
          )

    -- One namespace-aware selection serves reference validation and resolution.
    -- Check every dependency, including alias-only imports, in source order.
    collectImportScope scope importDecl =
      case dependencyInventory importDecl of
        Nothing ->
          Left
            ( mkErrorDiagnostic
                E4010
                CompilationOrigin
                ( "internal resolver error while validating imports for '"
                    <> renderModulePath importerPath
                    <> "': missing exports for module '"
                    <> renderModulePath (resolverImportModulePath importDecl)
                    <> "'"
                )
            )
        Just inventory ->
          let selected = visibleUnqualifiedInventory importDecl inventory
              origin = BindingOrigin (resolverImportModulePath importDecl) (resolverImportSpan importDecl)
              addNamespace current namespace =
                Map.insertWith Map.union namespace (Map.fromSet (const origin) (exportNamesInNamespace namespace selected)) current
           in Right scope {importScopeNames = foldl' addNamespace (importScopeNames scope) [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]}

    findHiddenExplicitImportReference :: Set Text -> Maybe (Text, ResolverImport)
    findHiddenExplicitImportReference visibleSymbols =
      listToMaybe
        [ (symbolName, importDecl)
        | importDecl <- imports,
          Just symbolNames <- [resolverImportSymbols importDecl],
          Just inventory <- [dependencyInventory importDecl],
          let exportedSymbols = valueAndConstructorNames inventory,
          let hiddenSymbols = Set.difference exportedSymbols (Set.fromList symbolNames),
          symbolName <- Set.toList hiddenSymbols,
          Set.member symbolName referencedNames,
          not (Set.member symbolName visibleSymbols)
        ]

    findHiddenAliasImportReference :: Set Text -> Maybe (Text, ResolverImport, Text)
    findHiddenAliasImportReference visibleSymbols =
      listToMaybe
        [ (symbolName, importDecl, aliasName)
        | importDecl <- imports,
          Just aliasName <- [resolverImportAlias importDecl],
          Just inventory <- [dependencyInventory importDecl],
          let exportedSymbols = valueAndConstructorNames inventory,
          symbolName <- Set.toList exportedSymbols,
          Set.member symbolName referencedNames,
          not (Set.member symbolName visibleSymbols)
        ]

    mkHiddenExplicitImportSymbolError :: Text -> ResolverImport -> Diagnostic
    mkHiddenExplicitImportSymbolError symbolName importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (resolverImportSpan importDecl)
          ( mkErrorDiagnostic
              E4011
              CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible from explicit import of module '"
                  <> renderModulePath (resolverImportModulePath importDecl)
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkHiddenAliasImportSymbolError :: Text -> ResolverImport -> Text -> Diagnostic
    mkHiddenAliasImportSymbolError symbolName importDecl aliasName =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (resolverImportSpan importDecl)
          ( mkErrorDiagnostic
              E4012
              CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible unqualified from alias import of module '"
                  <> renderModulePath (resolverImportModulePath importDecl)
                  <> "' as '"
                  <> aliasName
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkImportAliasCollisionError :: Text -> BindingOrigin -> ResolverImport -> Diagnostic
    mkImportAliasCollisionError aliasName previousOrigin importDecl =
      setDiagnosticSubject aliasName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (resolverImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4009
                  CompilationOrigin
                  ( "import alias collision for '"
                      <> aliasName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already aliased to module '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot alias module '"
                      <> renderModulePath (resolverImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    renderExports :: Set Text -> Text
    renderExports exports
      | Set.null exports = "<none>"
      | otherwise = Text.intercalate ", " (sortOn id (Set.toList exports))
