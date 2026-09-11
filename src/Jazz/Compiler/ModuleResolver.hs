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
  ( CorePhase (..),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.CoreIdentity (ResolvedReference)
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
    inventoryHasSelector,
    moduleExportSelectorName,
    moduleExportSelectorNamespace,
    renderModuleExportSelector,
    selectValidatedModuleExportSelectors,
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
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    splitQualifiedIdentifierText,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
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
    SurfaceSignaturePayload,
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceModule,
  )
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
    discoveryPublicInventory :: ModuleExportInventory,
    discoveryReferences :: ReferenceInventory,
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
    resolvedExportInventoriesState :: Map ModulePath ModuleExportInventory,
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
      resolveCoreModuleNames owner ambientReferences ambientExports inventory inventory emptyImportScope [] loweredModule
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
          resolvedExportInventoriesState = Map.empty,
          resolvedPublicReferencesState = ambientReferences
        }

    visitModule callStack state modulePath
      | modulePath `Set.member` resolvedSetState state = pure state
      | modulePath `elem` callStack = throwE (mkCycleError modulePath callStack)
      | otherwise = do
          (sourcePath, sourceText) <- ExceptT (loadModuleSource callStack modulePath)
          discovery <- except (parseModuleDetails sourcePath modulePath sourceText)
          let nextStack = modulePath : callStack
              coreModule = discoveryCoreModule discovery
              imports = ModuleGraph.coreModuleImports coreModule
              references = discoveryReferences discovery
              sortedImports = sortModulePaths (collectImportPaths imports)
          stateAfterDeps <- foldM (visitModule nextStack) state sortedImports
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
                (resolvedExportInventoriesState stateAfterDeps)
          resolvedModule <-
            except $
              first NonEmpty.head $
                resolveCoreModuleNames
                  (NamedSourceUnit modulePath)
                  (resolvedPublicReferencesState stateAfterDeps)
                  ambientExports
                  (discoveryLocalInventory discovery)
                  (discoveryPublicInventory discovery)
                  importScope
                  imports
                  coreModule
          pure
            stateAfterDeps
              { resolvedSetState = Set.insert modulePath (resolvedSetState stateAfterDeps),
                resolvedModulesState = resolvedModulesState stateAfterDeps Seq.|> resolvedModule,
                resolvedExportInventoriesState = Map.insert modulePath (discoveryPublicInventory discovery) (resolvedExportInventoriesState stateAfterDeps),
                resolvedPublicReferencesState = Map.union (resolvedPublicReferences (ImportedModule modulePath) (discoveryPublicInventory discovery) (ModuleGraph.coreModuleStatements resolvedModule)) (resolvedPublicReferencesState stateAfterDeps)
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
            dedupePreservingOrder
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
      { ModuleGraph.moduleImportNode = resolveNode owner (ModuleGraph.moduleImportNode coreImport),
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
parseModuleDetails :: FilePath -> ModulePath -> Text -> Either Diagnostic ModuleDiscoveryFacts
parseModuleDetails sourcePath expectedModulePath sourceText =
  {-# SCC "jazz-stage:module-resolution" #-}
  case parseSurfaceProgram sourceText of
    Left parseError ->
      Left
        ( setDiagnosticErrorCode
            E4004
            ( prependDiagnosticSummary
                ("module parse error at '" <> Text.pack sourcePath <> "': ")
                (qualifyDiagnosticSpans sourcePath parseError)
            )
        )
    Right surfaceExpr -> do
      coreModule <-
        lowerSurfaceModule
          (moduleIdentity expectedModulePath (mkSourceFile sourcePath))
          surfaceExpr
      let (localInventory, constructorOwners, references) = discoverModuleFacts surfaceExpr
      publicInventory <-
        validatePublicExportInventory
          sourcePath
          expectedModulePath
          (ModuleGraph.declaredModuleExports (ModuleGraph.coreModuleFacts coreModule))
          constructorOwners
          localInventory
      Right
        ModuleDiscoveryFacts
          { discoveryLocalInventory = localInventory,
            discoveryPublicInventory = publicInventory,
            discoveryReferences = references {referenceFactQualifiedClasses = Map.map (bimap (qualifySourceSpan sourcePath) (qualifySourceSpan sourcePath)) (referenceFactQualifiedClasses references)},
            discoveryCoreModule = coreModule
          }

validatePublicExportInventory ::
  FilePath ->
  ModulePath ->
  Maybe ModuleGraph.DeclaredModuleExports ->
  Map Text (Set Text) ->
  ModuleExportInventory ->
  Either Diagnostic ModuleExportInventory
validatePublicExportInventory sourcePath modulePath maybeExplicitExports constructorOwners localInventory =
  case maybeExplicitExports of
    Nothing -> Right localInventory
    Just declaredExports ->
      let moduleSpan = ModuleGraph.declaredModuleExportsSpan declaredExports
          selectors = ModuleGraph.declaredModuleExportSelectors declaredExports
       in case firstInvalidExport moduleSpan selectors of
            Nothing -> Right (selectValidatedModuleExportSelectors constructorOwners selectors localInventory)
            Just invalidExport ->
              Left
                ( setDiagnosticSubject
                    (invalidExportName invalidExport)
                    ( setDiagnosticPrimarySpan
                        (invalidExportSpan invalidExport)
                        ( mkErrorDiagnostic
                            E4015
                            CompilationOrigin
                            ( invalidExportSummary invalidExport
                                <> " module '"
                                <> renderModulePath modulePath
                                <> "' in '"
                                <> Text.pack sourcePath
                                <> "'; available declarations: "
                                <> renderAvailableDeclarations (invalidExportSelector invalidExport)
                            )
                        )
                    )
                )
  where
    firstInvalidExport _ [] = Nothing
    firstInvalidExport moduleSpan (selector : rest) =
      case validateSelector moduleSpan selector of
        Nothing -> firstInvalidExport moduleSpan rest
        invalid -> invalid

    validateSelector moduleSpan selector =
      case selector of
        ModuleExportSelector {}
          | inventoryHasSelector selector localInventory -> Nothing
          | otherwise ->
              Just
                InvalidModuleExport
                  { invalidExportSelector = selector,
                    invalidExportName = moduleExportSelectorName selector,
                    invalidExportSpan = moduleSpan,
                    invalidExportSummary = "module export " <> renderModuleExportSelector selector <> " is not declared by"
                  }
        ModuleTypeExportSelector typeName typeSpan constructorSelector ->
          case Map.lookup typeName constructorOwners of
            Nothing ->
              Just
                InvalidModuleExport
                  { invalidExportSelector = selector,
                    invalidExportName = typeName,
                    invalidExportSpan = typeSpan,
                    invalidExportSummary = "module export " <> renderModuleExportSelector selector <> " is not declared by"
                  }
            Just ownedConstructors -> validateConstructors selector typeName ownedConstructors constructorSelector

    validateConstructors selector typeName ownedConstructors constructorSelector =
      case constructorSelector of
        AbstractType -> Nothing
        AllTypeConstructors _ -> Nothing
        SelectedTypeConstructors constructors ->
          case find ((`Set.notMember` ownedConstructors) . locatedModuleExportName) (NonEmpty.toList constructors) of
            Nothing -> Nothing
            Just constructor ->
              Just
                InvalidModuleExport
                  { invalidExportSelector = selector,
                    invalidExportName = locatedModuleExportName constructor,
                    invalidExportSpan = locatedModuleExportSpan constructor,
                    invalidExportSummary =
                      "module export constructor '"
                        <> locatedModuleExportName constructor
                        <> "' is not declared by type '"
                        <> typeName
                        <> "' in"
                  }

    availableNames = declarationExportNames localInventory
    renderAvailableDeclarations selector =
      case moduleExportSelectorNamespace selector of
        Nothing -> renderDeclarationNames availableNames
        Just _ ->
          renderDeclarationLabels
            [ renderModuleExportSelector
                (ModuleExportSelector (Just (moduleExportNamespace export)) (moduleExportName export))
            | export <- Set.toAscList (exportInventoryEntries localInventory)
            ]

renderDeclarationNames :: Set Text -> Text
renderDeclarationNames = renderDeclarationLabels . Set.toAscList

renderDeclarationLabels :: [Text] -> Text
renderDeclarationLabels labels
  | null labels = "<none>"
  | otherwise = Text.intercalate ", " labels

data InvalidModuleExport = InvalidModuleExport
  { invalidExportSelector :: ModuleExportSelector,
    invalidExportName :: Text,
    invalidExportSpan :: SourceSpan,
    invalidExportSummary :: Text
  }

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
      let localInventory = exportInventory (reverse exportsRev)
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
        SSLet bindingName _ _
          | not (isOperatorBindingIdentifierText (identifierText bindingName)) ->
              ModuleExport ValueNamespace (identifierText bindingName) : exportsRev
        SSData _ typeName _ constructors ->
          foldl'
            ( \current (SurfaceDataConstructor constructorName _) ->
                ModuleExport ConstructorNamespace (identifierText constructorName) : current
            )
            (ModuleExport TypeNamespace (identifierText typeName) : exportsRev)
            constructors
        SSClass _ className _ _ ->
          ModuleExport CapabilityNamespace (identifierText className) : exportsRev
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
  ValidatedImportScope ->
  [ModuleGraph.ModuleImport 'Lowered] ->
  ModuleGraph.CoreModule 'Lowered ->
  Either (NonEmpty Diagnostic) (ModuleGraph.CoreModule 'Resolved)
resolveCoreModuleNames owner externalReferences ambientExports localInventory publicInventory importScope imports coreModule = do
  resolvedExpr <- resolveExprNames context (ModuleGraph.coreModuleExpr coreModule)
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
    SEOperatorValue _ -> facts
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
    SEBinary _ left right ->
      collectExprReferenceFacts
        boundNames
        right
        (collectExprReferenceFacts boundNames left facts)
    SESectionLeft left _ -> collectExprReferenceFacts boundNames left facts
    SESectionRight _ right -> collectExprReferenceFacts boundNames right facts
    SEBlock statements -> collectBlockReferenceFacts boundNames statements facts

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
    SSClass _ _ _ methods ->
      foldl'
        (\current (SurfaceClassMethodSignature _ _ payload) -> collectSignaturePayloadReferenceFacts payload current)
        facts
        methods
    SSImpl _ className arguments methods ->
      foldl'
        (\current (SurfaceImplMethod _ _ body) -> collectExprReferenceFacts boundNames body current)
        (foldl' (flip collectSignatureTypeReferenceFacts) (collectClassNameReference className facts) arguments)
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
        (foldl' collectConstraint facts constraints)
      where
        collectConstraint current (SignatureConstraint name arguments) =
          foldl'
            (flip collectSignatureTypeReferenceFacts)
            (collectClassNameReference name current)
            arguments
    UnsupportedSignature _ -> facts

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

-- | Preserve the first occurrence of each candidate path so module-root lookup
-- order remains stable while removing duplicates.
dedupePreservingOrder :: (Ord a) => [a] -> [a]
dedupePreservingOrder =
  reverse . fst . foldl' step ([], Set.empty)
  where
    step (uniqueRev, seen) value
      | Set.member value seen = (uniqueRev, seen)
      | otherwise = (value : uniqueRev, Set.insert value seen)
