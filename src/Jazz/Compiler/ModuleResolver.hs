{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Module graph resolver for `module` and `import` forms. It loads source,
-- validates module declarations/import bindings, and returns modules in
-- dependency order for the driver.
module Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolutionContext,
    parseModulePathText,
    importedOperators,
    resolveExprNames,
    resolvePreludeArtifact,
    resolveStandaloneExprNames,
    resolveStandaloneProgram,
    resolveProgramWithAmbientExports,
  )
where

import Control.DeepSeq
  ( NFData,
  )
import Control.Monad
  ( foldM,
  )
import Control.Monad.Trans.Except
  ( ExceptT (..),
    except,
    runExceptT,
    throwE,
  )
import Data.Bifunctor
  ( bimap,
    first,
  )
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
  ( toList,
  )
import Data.List
  ( find,
    sortOn,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Sequence
  ( Seq,
  )
import qualified Data.Sequence as Seq
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import GHC.Generics
  ( Generic,
  )
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts),
    CorePhase (..),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference)
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan (..),
    mkErrorDiagnostic,
    prependDiagnosticSummary,
    qualifyDiagnosticSpans,
    qualifySourceSpan,
    setDiagnosticErrorCode,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
  )
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExport (..),
    ModuleExportInventory,
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
    declarationExportNames,
    exportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    exportedConstructorOwners,
    inventoryHasSelector,
    moduleExportSelectorName,
    moduleExportSelectorSpan,
    renderModuleExportSelector,
    restrictExportInventory,
    selectValidatedModuleExportSelectors,
    unqualifiedModuleExportSelector,
    withClassMethods,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    SourceUnitOwner (..),
    mkModulePath,
    mkSourceFile,
    moduleIdentity,
    modulePathRelativeFile,
    parseModulePathText,
    renderModulePath,
    sourceUnitOwnerModulePath,
  )
import Jazz.Compiler.ModuleImportScope (BindingOrigin (..), dependencyImportViews, importScopeAliases, importScopeNames)
import Jazz.Compiler.ModuleResolver.Imports
  ( ResolverImport,
    ValidatedImportScope,
    declaredImportSpan,
    emptyImportScope,
    validateImportBindings,
  )
import Jazz.Compiler.ModuleResolver.Names
  ( ResolutionContext (..),
    resolveExprNames,
    resolveNode,
    resolveStandaloneExprNames,
    resolvedPublicReferences,
    standaloneLocalInventory,
  )
import Jazz.Compiler.Name
  ( IdentifierLike,
    Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    operatorBindingIdentifierText,
    renderOperatorBindingIdentifier,
    splitQualifiedIdentifierText,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgramTokensWithContextDetailed,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceName (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfacePatternLambdaClause (..),
    SurfaceSignatureConstraint,
    SurfaceSignaturePayload,
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context (ParserContext (..), initialParserContext)
import Jazz.Compiler.Parser.Failure (ParserDeclarationFailure (..), ParserFailure (..), ParserFailureReason (..), parserFailureDiagnostic)
import Jazz.Compiler.Parser.Lexer (Token, tokenize)
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceModule,
  )
import Jazz.Compiler.Parser.ModuleDeclaration (discoverModuleDeclarations, registerImportAliases)
import Jazz.Compiler.Parser.Operator (OperatorInfo (..), builtinOperatorFunction, operatorTableFromDeclarations)
import Jazz.Compiler.SourceProgram (standaloneSourceModule)
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeFunction,
    pattern TypeList,
    pattern TypeName,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
  )
import System.FilePath
  ( normalise,
    (</>),
  )

-- | File-system lookup policy for module loading.
data ModuleResolutionConfig = ModuleResolutionConfig
  { moduleRoots :: [FilePath],
    moduleExtension :: String
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData)

data ModuleDiscoveryFacts = ModuleDiscoveryFacts
  { discoveryLocalInventory :: ModuleExportInventory,
    discoveryConstructorOwners :: Map Text (Set Text),
    discoveryReferences :: ReferenceInventory,
    discoveryOperators :: [OperatorInfo],
    discoveryCoreModule :: ModuleGraph.CoreModule 'Lowered
  }

data ReferenceInventory = ReferenceInventory
  { referenceFactUnqualified :: !(Set Text),
    referenceFactQualifiedValues :: !(Set (Text, Text)),
    referenceFactQualifiedTypes :: !(Set (Text, Text)),
    referenceFactQualifiedClasses :: !(Map (Text, Text) (SourceSpan, SourceSpan))
  }

data ResolvedState = ResolvedState
  { resolvedSetState :: Set ModulePath,
    resolvedModulesState :: Seq (ModuleGraph.CoreModule 'Resolved),
    resolvedModuleFactsState :: Map ModulePath ModuleGraph.ResolvedModuleFacts,
    resolvedPublicReferencesState :: Map ResolvedName ResolvedReference
  }

modulePathFromTextSegments :: [Text] -> Either Diagnostic ModulePath
modulePathFromTextSegments segments =
  case NonEmpty.nonEmpty (map mkIdentifier segments) of
    Just identifiers -> Right (mkModulePath identifiers)
    Nothing -> Left (mkErrorDiagnostic E4016 CompilationOrigin "empty entry module path")

resolveProgramWithAmbientExports ::
  ModuleResolutionConfig ->
  ModuleGraph.PreludeArtifact 'Resolved ->
  ModuleExportInventory ->
  (FilePath -> IO (Maybe Text)) ->
  [Text] ->
  IO (Either Diagnostic (ModuleGraph.CoreProgram 'Resolved))
resolveProgramWithAmbientExports config prelude ambientExports loadSource entryModulePath =
  {-# SCC "jazz-stage:module-discovery" #-}
  case modulePathFromTextSegments entryModulePath of
    Left diagnostic -> pure (Left diagnostic)
    Right nominalEntryPath ->
      fmap (>>= finalizeProgram nominalEntryPath) $
        resolveStateWithLookupAndVisibleSymbols
          config
          ambientExports
          (maybe Map.empty (resolvedPublicReferences AmbientPrelude ambientExports . ModuleGraph.coreModuleStatements) (ModuleGraph.preludeModule prelude))
          loadSource
          nominalEntryPath
  where
    finalizeProgram nominalEntryPath state =
      case NonEmpty.nonEmpty (toList (resolvedModulesState state)) of
        Nothing -> Left (mkErrorDiagnostic E4016 CompilationOrigin "resolved program has no entry module")
        Just modules ->
          case ModuleGraph.mkCoreProgram prelude nominalEntryPath modules of
            Left failures -> Left (mkProgramInvariantDiagnostic failures)
            Right program -> Right program

    mkProgramInvariantDiagnostic failures =
      mkErrorDiagnostic E4016 CompilationOrigin ("resolved program invariant failed: " <> Text.pack (show failures))

-- | Standalone source enters the same graph coordinator with an independent
-- prelude artifact. A source header retains its named-unit ownership.
resolveStandaloneProgram ::
  ModuleGraph.PreludeArtifact 'Resolved ->
  ModuleExportInventory ->
  Expr 'Lowered ->
  Either Diagnostic (ModuleGraph.CoreProgram 'Resolved)
resolveStandaloneProgram prelude ambientExports expression = do
  resolvedModule <-
    first NonEmpty.head $
      resolveCoreModuleNames owner ambientReferences ambientExports inventory inventory (localExportNames CurrentModule inventory) Map.empty [] emptyImportScope [] loweredModule
  first (\failures -> mkErrorDiagnostic E4016 CompilationOrigin ("standalone program invariant failed: " <> Text.pack (show failures))) $
    ModuleGraph.mkCoreProgram prelude modulePath (NonEmpty.singleton resolvedModule)
  where
    loweredModule = standaloneSourceModule expression
    modulePath = ModuleGraph.coreModulePath loweredModule
    owner =
      if any isModule (ModuleGraph.coreModuleStatements loweredModule)
        then NamedSourceUnit modulePath
        else StandaloneSourceUnit modulePath
    isModule SModule {} = True
    isModule _ = False
    inventory = standaloneLocalInventory (ModuleGraph.coreModuleExpr loweredModule)
    ambientReferences =
      maybe
        Map.empty
        ( \preludeModule ->
            Map.union
              (resolvedPublicReferences AmbientPrelude ambientExports (ModuleGraph.coreModuleStatements preludeModule))
              (resolvedPublicReferences CurrentModule ambientExports (ModuleGraph.coreModuleStatements preludeModule))
        )
        (ModuleGraph.preludeModule prelude)

resolveStateWithLookupAndVisibleSymbols ::
  (Monad m) =>
  ModuleResolutionConfig ->
  ModuleExportInventory ->
  Map ResolvedName ResolvedReference ->
  (FilePath -> m (Maybe Text)) ->
  ModulePath ->
  m (Either Diagnostic ResolvedState)
resolveStateWithLookupAndVisibleSymbols config ambientExports ambientReferences loadSource entryModulePath =
  runExceptT (visitModule [] initialState entryModulePath)
  where
    initialState =
      ResolvedState
        { resolvedSetState = Set.empty,
          resolvedModulesState = Seq.empty,
          resolvedModuleFactsState = Map.empty,
          resolvedPublicReferencesState = ambientReferences
        }

    visitModule callStack state modulePath
      | modulePath `Set.member` resolvedSetState state = pure state
      | modulePath `elem` callStack = throwE (mkCycleError modulePath callStack)
      | otherwise = do
          (sourcePath, sourceText) <- ExceptT (loadModuleSource callStack modulePath)
          tokens <- except (first (moduleParseDiagnostic sourcePath) (tokenize sourceText))
          declarations <- except (first (moduleParseDiagnostic sourcePath) (discoverModuleDeclarations tokens))
          header <- except (lowerSurfaceModule (moduleIdentity modulePath (mkSourceFile sourcePath)) (SurfaceExpr (SourceSpan 1 1) (SEBlock declarations)))
          let nextStack = modulePath : callStack
              imports = ModuleGraph.coreModuleImports header
              sortedImports = sortModulePaths (collectImportPaths imports)
          stateAfterDeps <- foldM (visitModule nextStack) state sortedImports
          let dependencyFacts = resolvedModuleFactsState stateAfterDeps
              inventories = Map.map ModuleGraph.resolvedModuleExports dependencyFacts
              names = Map.map ModuleGraph.resolvedModuleExportNames dependencyFacts
          earlyScope <- except (validateImportBindings sourcePath modulePath imports Set.empty Set.empty Set.empty Set.empty Map.empty ambientVisibleSymbols ambientVisibleClassNames inventories names)
          let context =
                initialParserContext
                  { parserKnownAliases = registerImportAliases Set.empty declarations,
                    parserDeclaredOperators = operatorTableFromDeclarations (importedOperators earlyScope dependencyFacts)
                  }
          discovery <- except (parseModuleDetails sourcePath modulePath earlyScope context tokens)
          let coreModule = discoveryCoreModule discovery
              references = discoveryReferences discovery
          importScope <-
            except $
              validateImportBindings
                sourcePath
                modulePath
                imports
                (exportNamesInNamespace CapabilityNamespace (discoveryLocalInventory discovery))
                (referenceFactUnqualified references)
                (referenceFactQualifiedValues references)
                (referenceFactQualifiedTypes references)
                (referenceFactQualifiedClasses references)
                ambientVisibleSymbols
                ambientVisibleClassNames
                (Map.map ModuleGraph.resolvedModuleExports (resolvedModuleFactsState stateAfterDeps))
                (Map.map ModuleGraph.resolvedModuleExportNames (resolvedModuleFactsState stateAfterDeps))
          (publicInventory, publicNames) <-
            except $
              validatePublicExportInventory
                sourcePath
                modulePath
                (ModuleGraph.declaredModuleExports (ModuleGraph.coreModuleFacts coreModule))
                (discoveryConstructorOwners discovery)
                (discoveryLocalInventory discovery)
                importScope
                (resolvedModuleFactsState stateAfterDeps)
          resolvedModule <-
            except $
              first NonEmpty.head $
                resolveCoreModuleNames
                  (NamedSourceUnit modulePath)
                  (resolvedPublicReferencesState stateAfterDeps)
                  ambientExports
                  (discoveryLocalInventory discovery)
                  publicInventory
                  publicNames
                  (Map.map ModuleGraph.resolvedModuleExportNames (resolvedModuleFactsState stateAfterDeps))
                  (publicOperators modulePath publicNames (discoveryOperators discovery) dependencyFacts)
                  importScope
                  imports
                  coreModule
          pure
            stateAfterDeps
              { resolvedSetState = Set.insert modulePath (resolvedSetState stateAfterDeps),
                resolvedModulesState = resolvedModulesState stateAfterDeps Seq.|> resolvedModule,
                resolvedModuleFactsState = Map.insert modulePath (ModuleGraph.coreModuleFacts resolvedModule) (resolvedModuleFactsState stateAfterDeps),
                resolvedPublicReferencesState = Map.union (resolvedPublicReferences (ImportedModule modulePath) (discoveryLocalInventory discovery) (ModuleGraph.coreModuleStatements resolvedModule)) (resolvedPublicReferencesState stateAfterDeps)
              }

    ambientVisibleSymbols =
      exportNamesInNamespaces
        [ValueNamespace, ConstructorNamespace]
        ambientExports

    ambientVisibleClassNames =
      exportNamesInNamespace CapabilityNamespace ambientExports

    loadModuleSource callStack modulePath = do
      let relativePath = modulePathRelativeFile (moduleExtension config) modulePath
          candidatePaths =
            nubOrd
              (map (normalise . appendRelativePath relativePath) (moduleRoots config))
      candidatesWithContents <-
        mapM
          ( \candidatePath -> do
              sourceText <- {-# SCC "jazz-stage:source-loading" #-} loadSource candidatePath
              pure (candidatePath, sourceText)
          )
          candidatePaths
      let matchingCandidates =
            [ (candidatePath, sourceText)
            | (candidatePath, Just sourceText) <- candidatesWithContents
            ]
      pure $
        case matchingCandidates of
          [] ->
            Left
              ( mkErrorDiagnostic
                  E4001
                  CompilationOrigin
                  ( "unresolved import '"
                      <> renderModulePath modulePath
                      <> "'"
                      <> renderImporterContext callStack
                      <> "; looked in "
                      <> Text.intercalate ", " (map Text.pack candidatePaths)
                  )
              )
          [(sourcePath, sourceText)] ->
            Right (sourcePath, sourceText)
          _ ->
            Left
              ( mkErrorDiagnostic
                  E4002
                  CompilationOrigin
                  ( "ambiguous import '"
                      <> renderModulePath modulePath
                      <> "'"
                      <> renderImporterContext callStack
                      <> "; matched "
                      <> Text.intercalate ", " (map (Text.pack . fst) matchingCandidates)
                  )
              )

resolveImportExposure :: SourceUnitOwner -> ResolverImport -> Either Diagnostic (ModuleGraph.ModuleImport 'Resolved)
resolveImportExposure owner coreImport = do
  resolvedExposure <- exposure
  pure
    ModuleGraph.ModuleImport
      { ModuleGraph.moduleImportNode =
          let node = resolveNode owner (ModuleGraph.moduleImportNode coreImport)
           in node {coreNodeFacts = (coreNodeFacts node) {resolvedNodeImportTarget = Just (ModuleGraph.importedModule coreImport)}},
        ModuleGraph.importedModule = ModuleGraph.importedModule coreImport,
        ModuleGraph.importExposure = resolvedExposure
      }
  where
    exposure =
      case ModuleGraph.importExposure coreImport of
        ModuleGraph.DeclaredImportAll Nothing -> Right ModuleGraph.ImportAllUnqualified
        ModuleGraph.DeclaredImportOnly Nothing symbolNames -> Right (ModuleGraph.ImportOnlyUnqualified symbolNames)
        ModuleGraph.DeclaredImportAll (Just alias) -> Right (ModuleGraph.ImportQualifiedOnly alias)
        ModuleGraph.DeclaredImportOnly (Just _) _ -> Left (mkImportExposureInvariantError (sourceUnitOwnerModulePath owner) coreImport)

mkImportExposureInvariantError :: ModulePath -> ResolverImport -> Diagnostic
mkImportExposureInvariantError importerPath coreImport =
  setDiagnosticPrimarySpan
    (declaredImportSpan coreImport)
    ( mkErrorDiagnostic
        E4010
        CompilationOrigin
        ( "internal resolver invariant failed while finalizing import '"
            <> renderModulePath (ModuleGraph.importedModule coreImport)
            <> "' for module '"
            <> renderModulePath importerPath
            <> "'"
        )
    )

appendRelativePath :: FilePath -> FilePath -> FilePath
appendRelativePath relativePath root
  | null root = relativePath
  | otherwise = root </> relativePath

-- | Parse a module's surface source and extract only the details needed by the
-- resolver: declarations, imports, and top-level exports.
moduleParseDiagnostic :: FilePath -> Diagnostic -> Diagnostic
moduleParseDiagnostic sourcePath =
  setDiagnosticErrorCode E4004
    . prependDiagnosticSummary ("module parse error at '" <> Text.pack sourcePath <> "': ")
    . qualifyDiagnosticSpans sourcePath

parseModuleDetails :: FilePath -> ModulePath -> ValidatedImportScope -> ParserContext -> [Token] -> Either Diagnostic ModuleDiscoveryFacts
parseModuleDetails sourcePath expectedModulePath scope context tokens = do
  (surfaceExpr, operators) <- first contextualFailure (parseSurfaceProgramTokensWithContextDetailed context tokens)
  coreModule <- lowerSurfaceModule (moduleIdentity expectedModulePath (mkSourceFile sourcePath)) surfaceExpr
  let (localInventory, constructorOwners, references) = discoverModuleFacts surfaceExpr
  Right
    ModuleDiscoveryFacts
      { discoveryLocalInventory = localInventory,
        discoveryConstructorOwners = constructorOwners,
        discoveryOperators = operators,
        discoveryReferences = references {referenceFactQualifiedClasses = Map.map (bimap (qualifySourceSpan sourcePath) (qualifySourceSpan sourcePath)) (referenceFactQualifiedClasses references)},
        discoveryCoreModule = coreModule
      }
  where
    contextualFailure failure =
      let diagnostic = moduleParseDiagnostic sourcePath (parserFailureDiagnostic failure)
          origin = case parserFailureReason failure of
            DeclarationFailure (DuplicateOperatorDeclaration symbol) ->
              case splitQualifiedIdentifierText symbol of
                Just (alias, _) -> Map.lookup alias (importScopeAliases scope)
                Nothing -> NonEmpty.head <$> (Map.lookup ValueNamespace (importScopeNames scope) >>= Map.lookup (operatorBindingIdentifierText symbol))
            _ -> Nothing
       in maybe diagnostic (\earlier -> setDiagnosticRelatedSpan (qualifySourceSpan sourcePath (bindingOriginSpan earlier)) diagnostic) origin

importedOperators :: ValidatedImportScope -> Map ModulePath ModuleGraph.ResolvedModuleFacts -> [OperatorInfo]
importedOperators scope dependencies =
  [ info {operatorSymbol = maybe (operatorSymbol info) (\qualifier -> qualifier <> "::" <> operatorSymbol info) alias}
  | (path, facts) <- Map.toAscList dependencies,
    (alias, inventory) <- dependencyImportViews path scope,
    info <- ModuleGraph.resolvedModuleOperators facts,
    Set.member (operatorBindingIdentifierText (operatorSymbol info)) (exportNamesInNamespace ValueNamespace inventory)
  ]

publicOperators :: ModulePath -> Map ModuleExport ResolvedName -> [OperatorInfo] -> Map ModulePath ModuleGraph.ResolvedModuleFacts -> [OperatorInfo]
publicOperators path names authored dependencies =
  Map.elems $
    Map.fromList
      [ (operatorSymbol info, info)
      | (target, info) <-
          [(UserName (ResolvedUserName (ImportedModule path) ValueNamespace (mkIdentifier (operatorBindingIdentifierText (operatorSymbol info)))), info) | info <- authored]
            <> [(target, info) | facts <- Map.elems dependencies, info <- ModuleGraph.resolvedModuleOperators facts, Just target <- [Map.lookup (ModuleExport ValueNamespace (operatorBindingIdentifierText (operatorSymbol info))) (ModuleGraph.resolvedModuleExportNames facts)]],
        Map.lookup (ModuleExport ValueNamespace (operatorBindingIdentifierText (operatorSymbol info))) names == Just target
      ]

localExportNames :: ResolvedNameOrigin -> ModuleExportInventory -> Map ModuleExport ResolvedName
localExportNames origin inventory =
  Map.fromSet
    (\entry -> UserName (ResolvedUserName origin (moduleExportNamespace entry) (mkIdentifier (moduleExportName entry))))
    (exportInventoryEntries inventory)

validatePublicExportInventory ::
  FilePath ->
  ModulePath ->
  Maybe ModuleGraph.DeclaredModuleExports ->
  Map Text (Set Text) ->
  ModuleExportInventory ->
  ValidatedImportScope ->
  Map ModulePath ModuleGraph.ResolvedModuleFacts ->
  Either Diagnostic (ModuleExportInventory, Map ModuleExport ResolvedName)
validatePublicExportInventory sourcePath modulePath maybeExplicitExports constructorOwners localInventory scope dependencies =
  case maybeExplicitExports of
    Nothing -> Right (defaultInventory, Map.restrictKeys localNames (exportInventoryEntries defaultInventory))
    Just declarations -> do
      (inventory, targets, _) <- foldM select (mempty, Map.empty, Map.empty) (ModuleGraph.declaredModuleExportSelectors declarations)
      pure (inventory, targets)
  where
    defaultInventory = restrictExportInventory (Set.filter (not . isOperatorBindingIdentifierText . moduleExportName) (exportInventoryEntries localInventory)) localView
    localNames = localExportNames (ImportedModule modulePath) localInventory
    localView =
      localInventory
        <> selectValidatedModuleExportSelectors
          constructorOwners
          [ModuleTypeExportSelector name (SourceSpan 1 1) (AllTypeConstructors (SourceSpan 1 1)) | name <- Map.keys constructorOwners]
          localInventory
    importedViews =
      [ (inventory, Map.restrictKeys (ModuleGraph.resolvedModuleExportNames facts) (exportInventoryEntries inventory))
      | (path, facts) <- Map.toAscList dependencies,
        (Nothing, inventory) <- dependencyImportViews path scope
      ]
    -- Groups select constructors/methods from the chosen type/class view,
    -- independently of same-spelled declarations in the facade's other scopes.
    view Nothing selector
      | inventoryHasSelector selector localView = Right (localView, localNames)
      | otherwise =
          let matches = filter (inventoryHasSelector selector . fst) importedViews
           in Right (foldMap fst matches, Map.unions (map snd matches))
    view (Just alias) _ = case Map.lookup alias (importScopeAliases scope) >>= (\origin -> Map.lookup (bindingOriginModulePath origin) dependencies) of
      Just facts -> Right (ModuleGraph.resolvedModuleExports facts, ModuleGraph.resolvedModuleExportNames facts)
      Nothing -> Left ("unknown import alias '" <> alias <> "'")

    select accumulated authored =
      let (alias, selector) = unqualifiedModuleExportSelector authored
          parts = case (alias, selector) of
            (Nothing, ModuleExportSelector Nothing located) ->
              [ part
              | namespace <- [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace],
                let part = ModuleExportSelector (Just namespace) located,
                inventoryHasSelector part localView || any (inventoryHasSelector part . fst) importedViews
              ]
            _ -> [selector]
       in foldM (selectPart authored alias) accumulated (if null parts then [selector] else parts)

    selectPart authored alias (published, targets, locations) selector = do
      let spanValue = moduleExportSelectorSpan authored
          selectedView = view alias selector
          available = either (const Set.empty) (declarationExportNames . fst) selectedView
          failure message at =
            setDiagnosticSubject (moduleExportSelectorName authored) $
              setDiagnosticPrimarySpan at $
                mkErrorDiagnostic
                  E4015
                  CompilationOrigin
                  (message <> " module '" <> renderModulePath modulePath <> "' in '" <> Text.pack sourcePath <> "'; available declarations: " <> if Set.null available then "<none>" else Text.intercalate ", " (map renderOperatorBindingIdentifier (Set.toAscList available)))
      (inventory, names) <- first (\message -> failure message spanValue) selectedView
      let owners =
            Map.fromListWith
              Set.union
              [ (owner, Set.singleton constructor)
              | constructor <- Set.toList (exportNamesInNamespace ConstructorNamespace inventory),
                owner <- Set.toList (exportedConstructorOwners constructor inventory)
              ]
      if inventoryHasSelector selector inventory
        then pure ()
        else Left (failure ("module export " <> renderModuleExportSelector authored <> " is not declared by") spanValue)
      case selector of
        ModuleTypeExportSelector typeName _ (SelectedTypeConstructors constructors) ->
          case find ((`Set.notMember` Map.findWithDefault Set.empty typeName owners) . locatedModuleExportName) (NonEmpty.toList constructors) of
            Nothing -> pure ()
            Just constructor -> Left (setDiagnosticSubject (locatedModuleExportName constructor) (failure ("module export constructor '" <> locatedModuleExportName constructor <> "' is not declared by type '" <> typeName <> "' in") (locatedModuleExportSpan constructor)))
        _ -> pure ()
      let selected = selectValidatedModuleExportSelectors owners [selector] inventory
      (nextTargets, nextLocations) <-
        foldM
          (addTarget failure spanValue names)
          (targets, locations)
          (Set.toAscList (exportInventoryEntries selected))
      pure (published <> selected, nextTargets, nextLocations)

    addTarget failure spanValue names (targets, locations) entry = case Map.lookup entry names of
      Nothing -> Left (failure "missing original declaration for export in" spanValue)
      Just target -> case Map.lookup entry targets of
        Just earlier
          | earlier /= target ->
              Left $
                maybe id setDiagnosticRelatedSpan (Map.lookup entry locations) $
                  failure ("conflicting module export '" <> renderOperatorBindingIdentifier (moduleExportName entry) <> "' in") spanValue
        _ -> Right (Map.insert entry target targets, Map.insertWith (const id) entry spanValue locations)

discoverModuleFacts :: SurfaceExpr -> (ModuleExportInventory, Map Text (Set Text), ReferenceInventory)
discoverModuleFacts surfaceExpr =
  case surfaceExprForm surfaceExpr of
    SEBlock statements -> go [] Set.empty Map.empty emptySurfaceReferenceFacts statements
    _ ->
      finalize [] Set.empty Map.empty (collectExprReferenceFacts Set.empty surfaceExpr emptySurfaceReferenceFacts)
  where
    go !exports !operatorBindings !constructorOwners !referenceFacts statements =
      case statements of
        [] -> finalize exports operatorBindings constructorOwners referenceFacts
        statement : rest ->
          go
            (collectExports statement exports)
            (collectOperatorBinding statement operatorBindings)
            (collectConstructorOwners statement constructorOwners)
            (collectStatementReferenceFacts Set.empty statement referenceFacts)
            rest

    finalize exportsRev operatorBindings constructorOwners referenceFacts =
      let localInventory =
            withClassMethods
              (Map.fromList [(identifierText name, Set.fromList [identifierText method | SurfaceClassMethodSignature method _ _ <- methods]) | SSClass _ name _ methods _ _ <- case surfaceExprForm surfaceExpr of SEBlock body -> body; _ -> []])
              (exportInventory (reverse exportsRev))
          topLevelBindings =
            Set.unions
              [ operatorBindings,
                exportNamesInNamespace ValueNamespace localInventory,
                exportNamesInNamespace ConstructorNamespace localInventory
              ]
       in ( localInventory,
            constructorOwners,
            referenceFacts
              { referenceFactUnqualified = referenceFactUnqualified referenceFacts Set.\\ topLevelBindings
              }
          )

    collectOperatorBinding statement bindingNames =
      case statement of
        SSLet bindingName _ _
          | isOperatorBindingIdentifierText (identifierText bindingName) ->
              Set.insert (identifierText bindingName) bindingNames
        _ -> bindingNames

    collectExports statement exportsRev =
      case statement of
        SSLet bindingName _ _ ->
          ModuleExport ValueNamespace (identifierText bindingName) : exportsRev
        SSData _ typeName _ constructors ->
          foldl'
            ( \current (SurfaceDataConstructor constructorName _) ->
                ModuleExport ConstructorNamespace (identifierText constructorName) : current
            )
            (ModuleExport TypeNamespace (identifierText typeName) : exportsRev)
            constructors
        SSClass _ className _ methods _ _ ->
          [ModuleExport ValueNamespace (identifierText method) | SurfaceClassMethodSignature method _ _ <- methods]
            <> (ModuleExport CapabilityNamespace (identifierText className) : exportsRev)
        _ -> exportsRev

    collectConstructorOwners statement owners =
      case statement of
        SSData _ typeName _ constructors ->
          Map.insert
            (identifierText typeName)
            ( Set.fromList
                [ identifierText constructorName
                | SurfaceDataConstructor constructorName _ <- constructors
                ]
            )
            owners
        _ -> owners

collectImportPaths :: [ModuleGraph.ModuleImport phase] -> [ModulePath]
collectImportPaths imports =
  [ ModuleGraph.importedModule importDecl
  | importDecl <- imports
  ]

resolveCoreModuleNames ::
  SourceUnitOwner ->
  Map ResolvedName ResolvedReference ->
  ModuleExportInventory ->
  ModuleExportInventory ->
  ModuleExportInventory ->
  Map ModuleExport ResolvedName ->
  Map ModulePath (Map ModuleExport ResolvedName) ->
  [OperatorInfo] ->
  ValidatedImportScope ->
  [ModuleGraph.ModuleImport 'Lowered] ->
  ModuleGraph.CoreModule 'Lowered ->
  Either (NonEmpty Diagnostic) (ModuleGraph.CoreModule 'Resolved)
resolveCoreModuleNames owner externalReferences ambientExports localInventory publicInventory publicNames importNames operators importScope imports coreModule = do
  let resolvedExpr = resolveExprNames context (ModuleGraph.coreModuleExpr coreModule)
  resolvedImports <-
    either
      (Left . NonEmpty.singleton)
      Right
      (traverse (resolveImportExposure owner) imports)
  case resolvedExpr of
    EBlock bodyNode statements ->
      pure
        ModuleGraph.CoreModule
          { ModuleGraph.coreModuleIdentity = ModuleGraph.coreModuleIdentity coreModule,
            ModuleGraph.coreModuleBodyNode = bodyNode,
            ModuleGraph.coreModuleImports = resolvedImports,
            ModuleGraph.coreModuleStatements = statements,
            ModuleGraph.coreModuleFacts =
              ModuleGraph.ResolvedModuleFacts
                { ModuleGraph.resolvedModuleExports = publicInventory,
                  ModuleGraph.resolvedModuleExportNames = publicNames,
                  ModuleGraph.resolvedModuleOperators = operators,
                  ModuleGraph.resolvedModuleImportScope = importScope,
                  ModuleGraph.resolvedModuleExportSelectors =
                    ModuleGraph.declaredModuleExportSelectors
                      <$> ModuleGraph.declaredModuleExports (ModuleGraph.coreModuleFacts coreModule)
                }
          }
    _ ->
      Left
        ( NonEmpty.singleton
            (mkErrorDiagnostic E4016 CompilationOrigin "resolved module body is not a block")
        )
  where
    context =
      ResolutionContext
        { resolutionSourceOwner = owner,
          resolutionExternalReferences = externalReferences,
          resolutionAmbientExports = ambientExports,
          resolutionLocalInventory = localInventory,
          resolutionImportNames = importNames,
          resolutionImportScope = importScope
        }

resolvePreludeArtifact ::
  ModuleExportInventory ->
  ModuleGraph.PreludeArtifact 'Lowered ->
  Either Diagnostic (ModuleGraph.PreludeArtifact 'Resolved)
resolvePreludeArtifact publicInventory artifact =
  case ModuleGraph.preludeModule artifact of
    Nothing -> Right (artifactWithoutModule artifact)
    Just loweredModule ->
      case resolveCoreModuleNames
        (PreludeSourceUnit (ModuleGraph.coreModulePath loweredModule))
        Map.empty
        (exportInventory [])
        publicInventory
        publicInventory
        (localExportNames AmbientPrelude publicInventory)
        Map.empty
        []
        emptyImportScope
        (ModuleGraph.coreModuleImports loweredModule)
        loweredModule of
        Left failures -> Left (NonEmpty.head failures)
        Right resolvedModule ->
          Right
            ModuleGraph.PreludeArtifact
              { ModuleGraph.preludeIdentity = ModuleGraph.preludeIdentity artifact,
                ModuleGraph.preludeModule = Just resolvedModule
              }
  where
    artifactWithoutModule loweredArtifact =
      ModuleGraph.PreludeArtifact
        { ModuleGraph.preludeIdentity = ModuleGraph.preludeIdentity loweredArtifact,
          ModuleGraph.preludeModule = Nothing
        }

-- | The resolver needs four reference namespaces with identical expression
-- recursion. Collect them together so each surface node is visited once.
emptySurfaceReferenceFacts :: ReferenceInventory
emptySurfaceReferenceFacts = ReferenceInventory Set.empty Set.empty Set.empty Map.empty

collectExprReferenceFacts :: Set Text -> SurfaceExpr -> ReferenceInventory -> ReferenceInventory
collectExprReferenceFacts boundNames surfaceExpr facts =
  case surfaceExprForm surfaceExpr of
    SELit _ -> facts
    SEVar name
      | identifierText name `Set.member` boundNames -> facts
      | otherwise ->
          facts
            { referenceFactUnqualified = Set.insert (identifierText name) (referenceFactUnqualified facts)
            }
    SEQualifiedVar qualifier member ->
      facts
        { referenceFactQualifiedValues =
            Set.insert
              (identifierText qualifier, identifierText member)
              (referenceFactQualifiedValues facts)
        }
    SEQualifiedMethod alias className _ aliasSpan classSpan _ ->
      collectQualifiedClassReference
        (aliasSpan, classSpan)
        (identifierText alias, identifierText className)
        facts
    SELambda params body ->
      let parameterList = NonEmpty.toList params
          parameterFacts = foldl' (flip collectLambdaParameterReferenceFacts) facts parameterList
          parameterBinders = Set.unions (map collectLambdaParameterBinders parameterList)
       in collectExprReferenceFacts (Set.union boundNames parameterBinders) body parameterFacts
    SEPatternLambda clauses ->
      foldl'
        (\current clause -> collectPatternLambdaClauseReferenceFacts boundNames clause current)
        facts
        (NonEmpty.toList clauses)
    SEOperatorValue symbol -> operatorFacts symbol
    SEList items -> collectExprReferenceFactList boundNames items facts
    SETuple items -> collectExprReferenceFactList boundNames items facts
    SEApply function argument ->
      collectExprReferenceFacts
        boundNames
        argument
        (collectExprReferenceFacts boundNames function facts)
    SETypeApplication function _ signatureType ->
      collectSignatureTypeReferenceFacts
        signatureType
        (collectExprReferenceFacts boundNames function facts)
    SEIf condition trueBranch falseBranch ->
      collectExprReferenceFactList boundNames [condition, trueBranch, falseBranch] facts
    SECase scrutinee arms ->
      foldl'
        (\current arm -> collectCaseArmReferenceFacts boundNames arm current)
        (collectExprReferenceFacts boundNames scrutinee facts)
        arms
    SEBinary symbol left right ->
      collectExprReferenceFacts
        boundNames
        right
        (collectExprReferenceFacts boundNames left (operatorFacts symbol))
    SESectionLeft left symbol -> collectExprReferenceFacts boundNames left (operatorFacts symbol)
    SESectionRight symbol right -> collectExprReferenceFacts boundNames right (operatorFacts symbol)
    SEBlock statements -> collectBlockReferenceFacts boundNames statements facts
  where
    operatorFacts symbol = case builtinOperatorFunction symbol of
      Just name | Set.notMember name boundNames -> facts {referenceFactUnqualified = Set.insert name (referenceFactUnqualified facts)}
      _ -> case splitQualifiedIdentifierText symbol of
        Just (alias, spelling) -> facts {referenceFactQualifiedValues = Set.insert (alias, operatorBindingIdentifierText spelling) (referenceFactQualifiedValues facts)}
        Nothing -> facts {referenceFactUnqualified = Set.insert (operatorBindingIdentifierText symbol) (referenceFactUnqualified facts)}

collectExprReferenceFactList :: Set Text -> [SurfaceExpr] -> ReferenceInventory -> ReferenceInventory
collectExprReferenceFactList boundNames expressions initialFacts =
  foldl'
    (\current expr -> collectExprReferenceFacts boundNames expr current)
    initialFacts
    expressions

collectBlockReferenceFacts :: Set Text -> [SurfaceStatement] -> ReferenceInventory -> ReferenceInventory
collectBlockReferenceFacts boundNames statements facts =
  foldl'
    (\current statement -> collectStatementReferenceFacts blockBoundNames statement current)
    facts
    statements
  where
    blockBindingNames =
      Set.fromList
        [ identifierText bindingName
        | SSLet bindingName _ _ <- statements
        ]
    blockBoundNames = Set.union boundNames blockBindingNames

collectStatementReferenceFacts :: Set Text -> SurfaceStatement -> ReferenceInventory -> ReferenceInventory
collectStatementReferenceFacts boundNames statement facts =
  case statement of
    SSLet _ _ valueExpr -> collectExprReferenceFacts boundNames valueExpr facts
    SSSignature _ _ payload -> collectSignaturePayloadReferenceFacts payload facts
    SSData _ _ _ constructors ->
      foldl'
        (flip collectSignatureTypeReferenceFacts)
        facts
        [ fieldType
        | SurfaceDataConstructor _ fieldTypes <- constructors,
          fieldType <- fieldTypes
        ]
    SSClass _ _ _ methods context defaults ->
      foldl'
        (\current (SurfaceImplMethod _ _ body) -> collectExprReferenceFacts boundNames body current)
        ( foldl'
            (\current (SurfaceClassMethodSignature _ _ payload) -> collectSignaturePayloadReferenceFacts payload current)
            (foldl' (flip collectSignatureConstraintReferenceFacts) facts context)
            methods
        )
        defaults
    SSImpl _ className arguments methods context ->
      foldl'
        (\current (SurfaceImplMethod _ _ body) -> collectExprReferenceFacts boundNames body current)
        (foldl' (flip collectSignatureConstraintReferenceFacts) (foldl' (flip collectSignatureTypeReferenceFacts) (collectClassNameReference className facts) arguments) context)
        methods
    SSModule {} -> facts
    SSImport {} -> facts
    SSExpr _ expr -> collectExprReferenceFacts boundNames expr facts

collectCaseArmReferenceFacts :: Set Text -> SurfaceCaseArm -> ReferenceInventory -> ReferenceInventory
collectCaseArmReferenceFacts boundNames (SurfaceCaseArm patternValue guard body) facts =
  let armBoundNames = Set.union boundNames (collectPatternBinders patternValue)
      patternFacts = collectPatternReferenceFacts patternValue facts
      guardFacts = maybe patternFacts (\guardExpr -> collectExprReferenceFacts armBoundNames guardExpr patternFacts) guard
   in collectExprReferenceFacts armBoundNames body guardFacts

collectPatternLambdaClauseReferenceFacts :: Set Text -> SurfacePatternLambdaClause -> ReferenceInventory -> ReferenceInventory
collectPatternLambdaClauseReferenceFacts boundNames (SurfacePatternLambdaClause _ patterns body) facts =
  let patternList = NonEmpty.toList patterns
      clauseBoundNames = Set.union boundNames (Set.unions (map collectPatternBinders patternList))
      patternFacts = foldl' (flip collectPatternReferenceFacts) facts patternList
   in collectExprReferenceFacts clauseBoundNames body patternFacts

collectPatternReferenceFacts :: SurfacePattern -> ReferenceInventory -> ReferenceInventory
collectPatternReferenceFacts patternValue facts =
  case surfacePatternForm patternValue of
    SPWildcard -> facts
    SPVariable _ -> facts
    SPLiteral _ -> facts
    SPConstructor constructorName nestedPatterns ->
      let constructorFacts =
            facts
              { referenceFactUnqualified =
                  Set.insert (identifierText constructorName) (referenceFactUnqualified facts)
              }
       in foldl' (flip collectPatternReferenceFacts) constructorFacts nestedPatterns
    SPList nestedPatterns -> foldl' (flip collectPatternReferenceFacts) facts nestedPatterns
    SPConsList headPattern tailPattern ->
      collectPatternReferenceFacts tailPattern (collectPatternReferenceFacts headPattern facts)
    SPTuple nestedPatterns -> foldl' (flip collectPatternReferenceFacts) facts nestedPatterns
    SPAs _ nestedPattern -> collectPatternReferenceFacts nestedPattern facts
    SPOr alternatives -> foldl' (flip collectPatternReferenceFacts) facts alternatives

collectPatternBinders :: SurfacePattern -> Set Text
collectPatternBinders patternValue =
  case surfacePatternForm patternValue of
    SPWildcard -> Set.empty
    SPVariable name -> Set.singleton (identifierText name)
    SPLiteral _ -> Set.empty
    SPConstructor _ nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)
    SPList nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)
    SPConsList headPattern tailPattern ->
      Set.union (collectPatternBinders headPattern) (collectPatternBinders tailPattern)
    SPTuple nestedPatterns ->
      Set.unions (map collectPatternBinders nestedPatterns)
    SPAs name nestedPattern ->
      Set.insert (identifierText name) (collectPatternBinders nestedPattern)
    SPOr alternatives ->
      commonPatternBinders alternatives

commonPatternBinders :: [SurfacePattern] -> Set Text
commonPatternBinders alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (collectPatternBinders firstAlternative)
        (map collectPatternBinders rest)

collectLambdaParameterBinders :: SurfaceLambdaParameter -> Set Text
collectLambdaParameterBinders parameter =
  case parameter of
    SurfaceLambdaIdentifier _ name -> Set.singleton (identifierText name)
    SurfaceLambdaPattern patternValue -> collectPatternBinders patternValue

collectLambdaParameterReferenceFacts :: SurfaceLambdaParameter -> ReferenceInventory -> ReferenceInventory
collectLambdaParameterReferenceFacts parameter facts =
  case parameter of
    SurfaceLambdaIdentifier _ _ -> facts
    SurfaceLambdaPattern patternValue -> collectPatternReferenceFacts patternValue facts

collectSignaturePayloadReferenceFacts :: SurfaceSignaturePayload -> ReferenceInventory -> ReferenceInventory
collectSignaturePayloadReferenceFacts payload facts =
  case payload of
    SignatureType signatureType ->
      collectSignatureTypeReferenceFacts signatureType facts
    ConstrainedSignature constraints signatureType ->
      collectSignatureTypeReferenceFacts
        signatureType
        (foldl' (flip collectSignatureConstraintReferenceFacts) facts constraints)
    UnsupportedSignature _ -> facts

collectSignatureConstraintReferenceFacts :: SurfaceSignatureConstraint -> ReferenceInventory -> ReferenceInventory
collectSignatureConstraintReferenceFacts (SignatureConstraint name arguments) facts =
  foldl' (flip collectSignatureTypeReferenceFacts) (collectClassNameReference name facts) arguments

collectClassNameReference :: SurfaceName -> ReferenceInventory -> ReferenceInventory
collectClassNameReference name facts =
  case (splitQualifiedIdentifierText (identifierText name), surfaceNameQualifierSpan name) of
    (Just reference, Just aliasSpan) -> collectQualifiedClassReference (aliasSpan, surfaceNameSpan name) reference facts
    _ -> facts

collectQualifiedClassReference :: (SourceSpan, SourceSpan) -> (Text, Text) -> ReferenceInventory -> ReferenceInventory
collectQualifiedClassReference spans reference facts =
  facts {referenceFactQualifiedClasses = Map.insertWith (const id) reference spans (referenceFactQualifiedClasses facts)}

collectSignatureTypeReferenceFacts :: SurfaceSignatureType -> ReferenceInventory -> ReferenceInventory
collectSignatureTypeReferenceFacts signatureType facts =
  case signatureType of
    TypeVariable name -> collectQualifiedTypeReference name facts
    TypeName name -> collectQualifiedTypeReference name facts
    TypeApplication name arguments ->
      foldl'
        (flip collectSignatureTypeReferenceFacts)
        (collectQualifiedTypeReference name facts)
        arguments
    TypeList innerType -> collectSignatureTypeReferenceFacts innerType facts
    TypeTuple elementTypes ->
      foldl' (flip collectSignatureTypeReferenceFacts) facts elementTypes
    TypeFunction argumentType resultType ->
      collectSignatureTypeReferenceFacts
        resultType
        (collectSignatureTypeReferenceFacts argumentType facts)
    _ -> facts

collectQualifiedTypeReference :: (IdentifierLike name) => name -> ReferenceInventory -> ReferenceInventory
collectQualifiedTypeReference name facts =
  case splitQualifiedIdentifierText (identifierText name) of
    Nothing -> facts
    Just qualifiedReference ->
      facts
        { referenceFactQualifiedTypes = Set.insert qualifiedReference (referenceFactQualifiedTypes facts)
        }

-- | Provide a deterministic lexical import order for traversal and diagnostics.
-- Encounter order is intentionally discarded by `Set`-based deduplication and
-- the final `renderModulePath` sort.
sortModulePaths :: [ModulePath] -> [ModulePath]
sortModulePaths modulePaths =
  map snd . sortOn fst $ map (\modulePath -> (renderModulePath modulePath, modulePath)) uniquePaths
  where
    uniquePaths = Set.toList (Set.fromList modulePaths)

mkCycleError :: ModulePath -> [ModulePath] -> Diagnostic
mkCycleError repeatedModulePath callStack =
  mkErrorDiagnostic
    E4003
    CompilationOrigin
    ("module import cycle detected: " <> Text.intercalate " -> " (map renderModulePath cycleTrace))
  where
    rootToLeaf = reverse callStack
    suffixStartingAtRepeat = dropWhile (/= repeatedModulePath) rootToLeaf
    cycleTrace = suffixStartingAtRepeat ++ [repeatedModulePath]

renderImporterContext :: [ModulePath] -> Text
renderImporterContext callStack =
  case callStack of
    importerPath : _ -> " imported by '" <> renderModulePath importerPath <> "'"
    [] -> ""
