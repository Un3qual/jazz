{-# LANGUAGE OverloadedStrings #-}

-- | Module graph resolver for `module` and `import` forms. It loads source,
-- validates module declarations/import bindings, and returns modules in
-- dependency order for the driver.
module JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    parseModulePathText,
    resolveModuleGraph,
    resolveModuleGraphWithLookup,
    resolveModuleGraphWithLookupAndVisibleSymbols,
    resolveProgram
  ) where

import Control.Monad (foldM)
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Identity
  ( Identity (..),
    runIdentity
  )
import Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    mkErrorDiagnostic,
    prependDiagnosticSummary,
    qualifyDiagnosticSpans,
    setDiagnosticErrorCode,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    SignatureType (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    lookupBuiltinSymbolInMode
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleExportSelector (..),
    ModuleImportMode (..),
    declarationExportNames,
    exportInventory,
    exportInventoryEntries,
    exportNamesInNamespace,
    exportNamesInNamespaces,
    firstExportNamespace,
    inventoryHasSelector,
    renderModuleExportSelector,
    selectorEligibleNames,
    selectModuleExportSelectors,
    visibleImportInventory
  )
import qualified JazzNext.Compiler.ModuleGraph as ModuleGraph
import JazzNext.Compiler.Name
  ( Identifier,
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    identifierText,
    isOperatorBindingIdentifierText,
    mkIdentifier,
    renderName,
    splitQualifiedIdentifierText
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.Lower (lowerSurfaceModule)
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    inferRecursiveGroupsOrdered
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructorArgument (..),
    SurfaceDataConstructor (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureType (..),
    SurfaceExpr (..),
    SurfaceStatement (..)
  )
import System.FilePath
  ( normalise,
    (</>)
  )

-- | File-system lookup policy for module loading.
data ModuleResolutionConfig = ModuleResolutionConfig
  { moduleRoots :: [FilePath],
    moduleExtension :: String
  }
  deriving (Eq, Show)

-- | Compatibility summary returned by the resolver's inventory-only entrypoints.
data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedImports :: [[Text]]
  }
  deriving (Eq, Show)

-- | Import declaration details retained after parsing so validation can report
-- diagnostics with the original import span.
data ParsedImport = ParsedImport
  { parsedImportSpan :: SourceSpan,
    parsedImportModulePath :: [Text],
    parsedImportAlias :: Maybe Text,
    parsedImportSymbols :: Maybe [Text]
  }
  deriving (Eq, Show)

-- | Resolver-local view of a parsed module. It deliberately stores only import,
-- export, and reference inventories, not the lowered executable program.
data ParsedModule = ParsedModule
  { parsedModuleImports :: [ParsedImport],
    parsedModuleLocalInventory :: ModuleExportInventory,
    parsedModulePublicInventory :: ModuleExportInventory,
    parsedModuleReferences :: Set Text,
    parsedModuleQualifiedReferences :: Set (Text, Text),
    parsedModuleQualifiedTypeReferences :: Set (Text, Text),
    parsedModuleCore :: ModuleGraph.CoreModule
  }

-- | Origin metadata for imported bindings/aliases used in collision
-- diagnostics.
data BindingOrigin = BindingOrigin
  { bindingOriginModulePath :: [Text],
    bindingOriginSpan :: SourceSpan
  }

data ResolvedState = ResolvedState
  { resolvedSetState :: Set [Text],
    resolvedModulesRevState :: [ResolvedModule],
    resolvedGraphModulesRevState :: [ModuleGraph.ResolvedModule],
    resolvedExportInventoriesState :: Map [Text] ModuleExportInventory
  }

modulePathToRelativeFile :: [Text] -> FilePath
modulePathToRelativeFile = modulePathToRelativeFileWithExt ".jz"

-- | Parse a user-provided module path like `Foo::Bar` and reject empty or
-- non-identifier segments before resolution starts.
parseModulePathText :: Text -> Either Diagnostic [Text]
parseModulePathText rawModulePath
  | Text.null rawModulePath =
      Left (mkErrorDiagnostic E4016 CompilationOrigin "entry module path cannot be empty")
  | any Text.null segments =
      Left
        ( mkErrorDiagnostic E4016 CompilationOrigin
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': empty path segment"
            )
        )
  | not (all isValidSegment segments) =
      Left
        ( mkErrorDiagnostic E4016 CompilationOrigin
            ( "invalid entry module path '"
                <> rawModulePath
                <> "': segments must be identifiers"
            )
        )
  | otherwise =
      Right segments
  where
    segments = Text.splitOn "::" rawModulePath

    isValidSegment :: Text -> Bool
    isValidSegment segment =
      case Text.uncons segment of
        Nothing -> False
        Just (firstChar, restChars) ->
          isIdentifierStart firstChar && Text.all isIdentifierRest restChars

    isIdentifierStart ch = isAlpha ch || ch == '_'
    isIdentifierRest ch = isAlphaNum ch || ch == '_' || ch == '\'' || ch == '!'

resolveModuleGraph ::
  ModuleResolutionConfig ->
  Map FilePath Text ->
  [Text] ->
  Either Diagnostic [ResolvedModule]
resolveModuleGraph config sources entryModulePath =
  runIdentity $
    resolveModuleGraphWithLookup
      config
      (\path -> pure (Map.lookup path sources))
      entryModulePath

-- | Resolve an entry module and all of its imports using an abstract source
-- lookup function so tests and CLI can share the same resolver logic.
resolveModuleGraphWithLookup ::
  Monad m =>
  ModuleResolutionConfig ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic [ResolvedModule])
resolveModuleGraphWithLookup config =
  resolveModuleGraphWithLookupAndVisibleSymbols config Set.empty Set.empty

resolveModuleGraphWithLookupAndVisibleSymbols ::
  Monad m =>
  ModuleResolutionConfig ->
  Set Text ->
  Set Text ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic [ResolvedModule])
resolveModuleGraphWithLookupAndVisibleSymbols config ambientVisibleSymbols ambientVisibleClassNames loadSource entryModulePath =
  fmap
    (fmap (reverse . resolvedModulesRevState))
    ( resolveStateWithLookupAndVisibleSymbols
        config
        ResolveKernelOnly
        ambientVisibleSymbols
        ambientVisibleClassNames
        loadSource
        entryModulePath
    )

resolveProgram ::
  ModuleResolutionConfig ->
  BuiltinResolutionMode ->
  Set Name ->
  Set Name ->
  (FilePath -> IO (Maybe Text)) ->
  [Text] ->
  IO (Either Diagnostic ModuleGraph.ResolvedProgram)
resolveProgram config builtinMode ambientValues ambientClasses loadSource entryModulePath =
  {-# SCC "jazz-stage:module-discovery" #-}
  fmap
    ( fmap
        ( \state ->
            ModuleGraph.ResolvedProgram
              { ModuleGraph.resolvedProgramEntryPath = entryModulePath,
                ModuleGraph.resolvedProgramModules = reverse (resolvedGraphModulesRevState state)
              }
        )
    )
    ( resolveStateWithLookupAndVisibleSymbols
        config
        builtinMode
        (Set.map renderName ambientValues)
        (Set.map renderName ambientClasses)
        loadSource
        entryModulePath
    )

resolveStateWithLookupAndVisibleSymbols ::
  Monad m =>
  ModuleResolutionConfig ->
  BuiltinResolutionMode ->
  Set Text ->
  Set Text ->
  (FilePath -> m (Maybe Text)) ->
  [Text] ->
  m (Either Diagnostic ResolvedState)
resolveStateWithLookupAndVisibleSymbols config builtinMode ambientVisibleSymbols ambientVisibleClassNames loadSource entryModulePath
  | null entryModulePath =
      pure (Left (mkErrorDiagnostic E4016 CompilationOrigin "empty entry module path"))
  | otherwise =
      visitModule [] initialState entryModulePath
  where
    initialState =
      ResolvedState
        { resolvedSetState = Set.empty,
          resolvedModulesRevState = [],
          resolvedGraphModulesRevState = [],
          resolvedExportInventoriesState = Map.empty
        }

    visitModule callStack state modulePath
      | modulePath `Set.member` resolvedSetState state =
          pure (Right state)
      | modulePath `elem` callStack =
          pure (Left (mkCycleError modulePath callStack))
      | otherwise = do
          sourceResult <- loadModuleSource callStack modulePath
          case sourceResult of
            Left err -> pure (Left err)
            Right (sourcePath, sourceText) ->
              case parseModuleDetails sourcePath modulePath sourceText of
                Left err -> pure (Left err)
                Right parsedModule -> do
                  let nextStack = modulePath : callStack
                      sortedImports = sortModulePaths (collectImportPaths (parsedModuleImports parsedModule))
                  resolvedDepsResult <-
                    foldM
                      (visitDependency nextStack)
                      (Right state)
                      sortedImports
                  case resolvedDepsResult of
                    Left err -> pure (Left err)
                    Right stateAfterDeps ->
                      case
                          validateImportBindings
                            sourcePath
                            modulePath
                            (parsedModuleImports parsedModule)
                            (exportNamesInNamespace CapabilityNamespace (parsedModuleLocalInventory parsedModule))
                            (parsedModuleReferences parsedModule)
                            (parsedModuleQualifiedReferences parsedModule)
                            (parsedModuleQualifiedTypeReferences parsedModule)
                            ambientVisibleSymbols
                            ambientVisibleClassNames
                            (resolvedExportInventoriesState stateAfterDeps) of
                        Left err -> pure (Left err)
                        Right () ->
                          let resolvedModule =
                                ResolvedModule
                                  { resolvedModulePath = modulePath,
                                    resolvedSourcePath = sourcePath,
                                    resolvedImports = sortedImports
                                  }
                              resolvedCore =
                                resolveCoreModuleNames
                                  builtinMode
                                  modulePath
                                  ambientVisibleSymbols
                                  ambientVisibleClassNames
                                  (parsedModuleLocalInventory parsedModule)
                                  (resolvedExportInventoriesState stateAfterDeps)
                                  (parsedModuleImports parsedModule)
                                  (parsedModuleCore parsedModule)
                              resolvedGraphModule =
                                ModuleGraph.ResolvedModule
                                  { ModuleGraph.resolvedModulePath = modulePath,
                                    ModuleGraph.resolvedSourcePath = sourcePath,
                                    ModuleGraph.resolvedModuleImports = ModuleGraph.coreModuleImports resolvedCore,
                                    ModuleGraph.resolvedModuleExportInventory = parsedModulePublicInventory parsedModule,
                                    ModuleGraph.resolvedModuleCore = resolvedCore
                                  }
                           in pure
                                ( Right
                                    stateAfterDeps
                                      { resolvedSetState =
                                          Set.insert modulePath (resolvedSetState stateAfterDeps),
                                        resolvedModulesRevState =
                                          resolvedModule : resolvedModulesRevState stateAfterDeps,
                                        resolvedGraphModulesRevState =
                                          resolvedGraphModule : resolvedGraphModulesRevState stateAfterDeps,
                                        resolvedExportInventoriesState =
                                          Map.insert
                                            modulePath
                                            (parsedModulePublicInventory parsedModule)
                                            (resolvedExportInventoriesState stateAfterDeps)
                                      }
                                )

    visitDependency nextStack accumulator importPath =
      case accumulator of
        Left err -> pure (Left err)
        Right currentState ->
          visitModule nextStack currentState importPath

    loadModuleSource callStack modulePath = do
      let relativePath = modulePathToRelativeFileWithExt (moduleExtension config) modulePath
          candidatePaths =
            dedupePreservingOrder
              (map (normalise . appendRelativePath relativePath) (moduleRoots config))
      candidatesWithContents <-
        mapM
          (\candidatePath -> do
             sourceText <- {-# SCC "jazz-stage:source-loading" #-} loadSource candidatePath
             pure (candidatePath, sourceText))
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
                  E4001 CompilationOrigin
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
                  E4002 CompilationOrigin
                  ( "ambiguous import '"
                      <> renderModulePath modulePath
                      <> "'"
                      <> renderImporterContext callStack
                      <> "; matched "
                      <> Text.intercalate ", " (map (Text.pack . fst) matchingCandidates)
                  )
              )

appendRelativePath :: FilePath -> FilePath -> FilePath
appendRelativePath relativePath root
  | null root = relativePath
  | otherwise = root </> relativePath

modulePathToRelativeFileWithExt :: String -> [Text] -> FilePath
modulePathToRelativeFileWithExt extension modulePath =
  case modulePath of
    [] -> extension
    _ -> foldr1 joinSegments (map Text.unpack modulePath) <> extension
  where
    joinSegments segment acc = segment <> "/" <> acc

-- | Parse a module's surface source and extract only the details needed by the
-- resolver: declarations, imports, and top-level exports.
parseModuleDetails :: FilePath -> [Text] -> Text -> Either Diagnostic ParsedModule
parseModuleDetails sourcePath expectedModulePath sourceText =
  {-# SCC "jazz-stage:module-resolution" #-}
  case parseSurfaceProgram sourceText of
    Left parseError ->
      Left
        ( setDiagnosticErrorCode E4004
            ( prependDiagnosticSummary
                ("module parse error at '" <> Text.pack sourcePath <> "': ")
                (qualifyDiagnosticSpans sourcePath parseError)
            )
        )
    Right surfaceExpr -> do
      coreModule <- lowerSurfaceModule sourcePath expectedModulePath surfaceExpr
      let localInventory = collectModuleExportInventory surfaceExpr
          topLevelBindings =
            Set.union
              (exportNamesInNamespace ValueNamespace localInventory)
              (exportNamesInNamespace ConstructorNamespace localInventory)
      publicInventory <-
        validatePublicExportInventory
          sourcePath
          expectedModulePath
          (ModuleGraph.coreModuleDeclaredExports coreModule)
          localInventory
      Right
        ParsedModule
          { parsedModuleImports = collectImports surfaceExpr,
            parsedModuleLocalInventory = localInventory,
            parsedModulePublicInventory = publicInventory,
            parsedModuleReferences = collectReferencedNames surfaceExpr Set.\\ topLevelBindings,
            parsedModuleQualifiedReferences = collectQualifiedReferences surfaceExpr,
            parsedModuleQualifiedTypeReferences = collectQualifiedTypeReferences surfaceExpr,
            parsedModuleCore = coreModule
          }

validatePublicExportInventory ::
  FilePath ->
  [Text] ->
  Maybe ModuleGraph.DeclaredModuleExports ->
  ModuleExportInventory ->
  Either Diagnostic ModuleExportInventory
validatePublicExportInventory sourcePath modulePath maybeExplicitExports localInventory =
  case maybeExplicitExports of
    Nothing -> Right localInventory
    Just declaredExports ->
      let moduleSpan = ModuleGraph.declaredModuleExportsSpan declaredExports
          selectors = ModuleGraph.declaredModuleExportSelectors declaredExports
       in case find (not . (`inventoryHasSelector` localInventory)) selectors of
            Nothing -> Right (selectModuleExportSelectors selectors localInventory)
            Just missingSelector ->
              Left
                ( setDiagnosticSubject (moduleExportSelectorName missingSelector)
                    ( setDiagnosticPrimarySpan
                        moduleSpan
                        ( mkErrorDiagnostic
                            E4015 CompilationOrigin
                            ( "module export "
                                <> renderModuleExportSelector missingSelector
                                <> " is not declared by module '"
                                <> renderModulePath modulePath
                                <> "' in '"
                                <> Text.pack sourcePath
                                <> "'; available declarations: "
                                <> renderAvailableDeclarations missingSelector
                            )
                        )
                    )
                )
  where
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

collectImports :: SurfaceExpr -> [ParsedImport]
collectImports surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      [ ParsedImport spanValue modulePath alias importedSymbols
        | SSImport spanValue modulePath alias importedSymbols <- statements
      ]
    _ -> []

collectImportPaths :: [ParsedImport] -> [[Text]]
collectImportPaths imports =
  [ parsedImportModulePath importDecl
    | importDecl <- imports
  ]

collectModuleExportInventory :: SurfaceExpr -> ModuleExportInventory
collectModuleExportInventory surfaceExpr =
  exportInventory
    ( case surfaceExpr of
        SEBlock statements -> concatMap statementExports statements
        _ -> []
    )
  where
    statementExports statement =
      case statement of
        SSLet bindingName _ _
          | not (isOperatorBindingIdentifierText (identifierText bindingName)) ->
              [ModuleExport ValueNamespace (identifierText bindingName)]
        SSData _ typeName _ constructors ->
          ModuleExport TypeNamespace (identifierText typeName)
            : [ ModuleExport ConstructorNamespace (identifierText constructorName)
                | SurfaceDataConstructor constructorName _ <- constructors
              ]
        SSClass _ className _ _ ->
          [ModuleExport CapabilityNamespace (identifierText className)]
        _ -> []

resolveCoreModuleNames ::
  BuiltinResolutionMode ->
  [Text] ->
  Set Text ->
  Set Text ->
  ModuleExportInventory ->
  Map [Text] ModuleExportInventory ->
  [ParsedImport] ->
  ModuleGraph.CoreModule ->
  ModuleGraph.CoreModule
resolveCoreModuleNames builtinMode _modulePath ambientValues ambientClasses localInventory inventoriesByModule imports coreModule =
  coreModule {ModuleGraph.coreModuleExpr = resolveExpr Set.empty (ModuleGraph.coreModuleExpr coreModule)}
  where
    localValues = exportNamesInNamespace ValueNamespace localInventory
    localDataTypes = exportNamesInNamespace TypeNamespace localInventory
    localConstructors = exportNamesInNamespace ConstructorNamespace localInventory
    localClasses = exportNamesInNamespace CapabilityNamespace localInventory

    aliasPaths =
      Map.fromList
        [ (aliasName, parsedImportModulePath importDecl)
          | importDecl <- imports,
            Just aliasName <- [parsedImportAlias importDecl]
        ]

    visibleValueOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- Set.toList (exportNamesInNamespace ValueNamespace (visibleDependencyInventory importDecl))
        ]

    visibleConstructorOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- Set.toList (exportNamesInNamespace ConstructorNamespace (visibleDependencyInventory importDecl))
        ]

    visibleTypeOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- Set.toList (exportNamesInNamespace TypeNamespace (visibleDependencyInventory importDecl))
        ]

    visibleClassOrigins =
      Map.fromList
        [ (name, modulePath)
          | importDecl <- imports,
            parsedImportAlias importDecl == Nothing,
            let modulePath = parsedImportModulePath importDecl,
            name <- Set.toList (exportNamesInNamespace CapabilityNamespace (visibleDependencyInventory importDecl))
        ]

    visibleDependencyInventory importDecl =
      case Map.lookup (parsedImportModulePath importDecl) inventoriesByModule of
        Nothing -> exportInventory []
        Just inventory ->
          visibleImportInventory
            UnqualifiedImport
            (parsedImportSymbols importDecl)
            inventory

    resolveName boundValues namespace name =
      case name of
        SourceName identifier -> resolveUnqualified boundValues namespace identifier
        QualifiedName qualifier member ->
          let qualifierText = identifierText qualifier
              memberText = identifierText member
           in case Map.lookup qualifierText aliasPaths of
                Just dependencyPath ->
                  ResolvedName
                    (ImportedModule dependencyPath)
                    (importedNamespace dependencyPath memberText namespace)
                    member
                Nothing ->
                  ResolvedName
                    (classOrigin qualifierText)
                    ValueNamespace
                    (mkIdentifier (qualifierText <> "::" <> memberText))
        _ -> name

    resolveUnqualified boundValues namespace identifier
      | namespace == ValueNamespace,
        Set.member nameText boundValues =
          ResolvedName CurrentModule ValueNamespace identifier
      | localName namespace nameText =
          ResolvedName CurrentModule namespace identifier
      | Just dependencyPath <- importedOrigin namespace nameText =
          ResolvedName (ImportedModule dependencyPath) (importedNamespace dependencyPath nameText namespace) identifier
      | ambientName namespace nameText =
          ResolvedName AmbientPrelude namespace identifier
      | namespace == ValueNamespace,
        Just _ <- lookupBuiltinSymbolInMode builtinMode nameText =
          BuiltinName identifier
      | otherwise =
          ResolvedName CurrentModule namespace identifier
      where
        nameText = identifierText identifier

    localName namespace nameText =
      case namespace of
        ValueNamespace -> Set.member nameText localValues
        ConstructorNamespace -> Set.member nameText localConstructors
        CapabilityNamespace -> Set.member nameText localClasses
        TypeNamespace -> Set.member nameText localDataTypes

    importedOrigin namespace nameText =
      case namespace of
        ConstructorNamespace -> Map.lookup nameText visibleConstructorOrigins
        TypeNamespace -> Map.lookup nameText visibleTypeOrigins
        CapabilityNamespace -> Map.lookup nameText visibleClassOrigins
        _ -> Map.lookup nameText visibleValueOrigins

    importedNamespace dependencyPath nameText fallbackNamespace
      | fallbackNamespace /= ValueNamespace = fallbackNamespace
      | otherwise =
          fromMaybe
            fallbackNamespace
            ( firstExportNamespace
                [ValueNamespace, ConstructorNamespace, CapabilityNamespace]
                nameText
                dependencyInventory
            )
      where
        dependencyInventory =
          Map.findWithDefault (exportInventory []) dependencyPath inventoriesByModule

    ambientName namespace nameText =
      case namespace of
        CapabilityNamespace -> Set.member nameText ambientClasses
        _ -> Set.member nameText ambientValues

    classOrigin className
      | Set.member className localClasses = CurrentModule
      | Just dependencyPath <- Map.lookup className visibleClassOrigins = ImportedModule dependencyPath
      | Set.member className ambientClasses = AmbientPrelude
      | otherwise = CurrentModule

    resolveExpr boundValues expression =
      case expression of
        ELit literal -> ELit literal
        EVar name -> EVar (resolveName boundValues (referenceNamespace boundValues name) name)
        ELambda parameter body ->
          let lambdaBoundValues = maybe boundValues (`Set.insert` boundValues) (sourceNameText parameter)
           in ELambda (resolveBinder ValueNamespace parameter) (resolveExpr lambdaBoundValues body)
        EOperatorValue symbol -> EOperatorValue symbol
        EList items -> EList (map (resolveExpr boundValues) items)
        ETuple items -> ETuple (map (resolveExpr boundValues) items)
        EApply function argument -> EApply (resolveExpr boundValues function) (resolveExpr boundValues argument)
        ETypeApplication function spanValue signatureType ->
          ETypeApplication (resolveExpr boundValues function) spanValue (resolveSignatureType signatureType)
        EIf condition trueBranch falseBranch ->
          EIf (resolveExpr boundValues condition) (resolveExpr boundValues trueBranch) (resolveExpr boundValues falseBranch)
        EPatternCase scrutinee arms ->
          EPatternCase (resolveExpr boundValues scrutinee) (map (resolveCaseArm boundValues) arms)
        EBinary symbol left right -> EBinary symbol (resolveExpr boundValues left) (resolveExpr boundValues right)
        ESectionLeft left symbol -> ESectionLeft (resolveExpr boundValues left) symbol
        ESectionRight symbol right -> ESectionRight symbol (resolveExpr boundValues right)
        EBlock statements ->
          EBlock (resolveBlockStatements boundValues statements)

    resolveBlockStatements initialBoundValues statements =
      reverse resolvedStatementsRev
      where
        indexedStatements = zip [0 ..] statements
        bindingNamesByStatement = collectBindingNames indexedStatements
        outerBindingNames =
          Set.map
            (SourceName . mkIdentifier)
            ( Set.unions
                [ initialBoundValues,
                  localConstructors,
                  ambientValues,
                  Map.keysSet visibleValueOrigins,
                  Map.keysSet visibleConstructorOrigins,
                  builtinNamesInMode builtinMode
                ]
            )
        recursiveGroupsByStatement = inferRecursiveGroupsOrdered outerBindingNames indexedStatements
        (_, resolvedStatementsRev) = foldl' resolveBlockStatement (initialBoundValues, []) indexedStatements

        resolveBlockStatement (visibleBoundValues, resolvedRev) (statementIndex, statement) =
          let statementBoundValues =
                case statement of
                  SLet bindingName _ _ ->
                    Set.unions
                      [ visibleBoundValues,
                        maybe Set.empty Set.singleton (sourceNameText bindingName),
                        recursivePeerBoundValues statementIndex
                      ]
                  _ -> visibleBoundValues
              resolvedStatement = resolveStatement statementBoundValues statement
              nextVisibleBoundValues =
                case statement of
                  SLet bindingName _ _ ->
                    maybe visibleBoundValues (`Set.insert` visibleBoundValues) (sourceNameText bindingName)
                  _ -> visibleBoundValues
           in (nextVisibleBoundValues, resolvedStatement : resolvedRev)

        recursivePeerBoundValues statementIndex =
          Set.fromList
            [ peerNameText
              | peerIndex <- Map.findWithDefault [] statementIndex recursiveGroupsByStatement,
                Just peerName <- [Map.lookup peerIndex bindingNamesByStatement],
                Just peerNameText <- [sourceNameText peerName]
            ]

    referenceNamespace boundValues name =
      case name of
        SourceName identifier
          | Set.member nameText boundValues -> ValueNamespace
          | Set.member nameText localValues -> ValueNamespace
          | Set.member nameText localConstructors -> ConstructorNamespace
          | Map.member nameText visibleValueOrigins -> ValueNamespace
          | Map.member nameText visibleConstructorOrigins -> ConstructorNamespace
          where
            nameText = identifierText identifier
        _ -> ValueNamespace

    resolveBinder namespace name =
      case name of
        SourceName identifier -> ResolvedName CurrentModule namespace identifier
        _ -> name

    resolveCaseArm boundValues (CaseArm patternValue guard body) =
      let armBoundValues = Set.union boundValues (corePatternBinders patternValue)
       in CaseArm
            (resolvePattern patternValue)
            (fmap (resolveExpr armBoundValues) guard)
            (resolveExpr armBoundValues body)

    resolvePattern patternValue =
      case patternValue of
        PWildcard -> PWildcard
        PVariable name -> PVariable (resolveBinder ValueNamespace name)
        PLiteral literal -> PLiteral literal
        PConstructor name patterns ->
          PConstructor (resolveName Set.empty ConstructorNamespace name) (map resolvePattern patterns)
        PList patterns -> PList (map resolvePattern patterns)
        PConsList headPattern tailPattern ->
          PConsList (resolvePattern headPattern) (resolvePattern tailPattern)
        PTuple patterns -> PTuple (map resolvePattern patterns)
        PAs name pattern' -> PAs (resolveBinder ValueNamespace name) (resolvePattern pattern')
        POr patterns -> POr (map resolvePattern patterns)

    resolveStatement boundValues statement =
      case statement of
        SLet name spanValue value ->
          SLet (resolveBinder ValueNamespace name) spanValue (resolveExpr boundValues value)
        SSignature name spanValue payload ->
          SSignature (resolveBinder ValueNamespace name) spanValue (resolveSignaturePayload payload)
        SData spanValue name parameters constructors ->
          SData
            spanValue
            (resolveBinder TypeNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveDataConstructor constructors)
        SClass spanValue name parameters methods ->
          SClass
            spanValue
            (resolveBinder CapabilityNamespace name)
            (map (resolveBinder TypeNamespace) parameters)
            (map resolveClassMethod methods)
        SImpl spanValue name arguments methods ->
          SImpl
            spanValue
            (resolveName Set.empty CapabilityNamespace name)
            (map resolveSignatureType arguments)
            (map (resolveImplMethod boundValues) methods)
        SModule spanValue path -> SModule spanValue path
        SImport spanValue path alias symbols -> SImport spanValue path alias symbols
        SExpr spanValue value -> SExpr spanValue (resolveExpr boundValues value)

    resolveDataConstructor (DataConstructor name arguments) =
      DataConstructor
        (resolveBinder ConstructorNamespace name)
        (map resolveDataConstructorArgument arguments)

    resolveDataConstructorArgument argument =
      case argument of
        DataConstructorArgumentName name ->
          DataConstructorArgumentName (resolveName Set.empty TypeNamespace name)
        DataConstructorArgumentOpaque -> DataConstructorArgumentOpaque

    resolveClassMethod (ClassMethodSignature name spanValue payload) =
      ClassMethodSignature (resolveBinder ValueNamespace name) spanValue (resolveSignaturePayload payload)

    resolveImplMethod boundValues (ImplMethod name spanValue body) =
      ImplMethod (resolveBinder ValueNamespace name) spanValue (resolveExpr boundValues body)

    resolveSignaturePayload payload =
      case payload of
        SignatureType signatureType -> SignatureType (resolveSignatureType signatureType)
        ConstrainedSignature constraints signatureType ->
          ConstrainedSignature
            (map resolveSignatureConstraint constraints)
            (resolveSignatureType signatureType)
        UnsupportedSignature tokens -> UnsupportedSignature (map resolveSignatureToken tokens)

    resolveSignatureToken token =
      case token of
        SignatureNameToken name -> SignatureNameToken (resolveName Set.empty TypeNamespace name)
        _ -> token

    resolveSignatureConstraint (SignatureConstraint name arguments) =
      SignatureConstraint (resolveName Set.empty CapabilityNamespace name) (map resolveSignatureType arguments)

    resolveSignatureType signatureType =
      case signatureType of
        TypeVariable name -> TypeVariable name
        TypeName name -> TypeName (resolveName Set.empty TypeNamespace name)
        TypeApplication name arguments ->
          TypeApplication (resolveName Set.empty TypeNamespace name) (map resolveSignatureType arguments)
        TypeList innerType -> TypeList (resolveSignatureType innerType)
        TypeTuple elementTypes -> TypeTuple (map resolveSignatureType elementTypes)
        TypeFunction argumentType resultType ->
          TypeFunction (resolveSignatureType argumentType) (resolveSignatureType resultType)
        _ -> signatureType

    sourceNameText name =
      case name of
        SourceName identifier -> Just (identifierText identifier)
        _ -> Nothing

    corePatternBinders patternValue =
      case patternValue of
        PWildcard -> Set.empty
        PVariable name -> maybe Set.empty Set.singleton (sourceNameText name)
        PLiteral _ -> Set.empty
        PConstructor _ patterns -> Set.unions (map corePatternBinders patterns)
        PList patterns -> Set.unions (map corePatternBinders patterns)
        PConsList headPattern tailPattern ->
          Set.union (corePatternBinders headPattern) (corePatternBinders tailPattern)
        PTuple patterns -> Set.unions (map corePatternBinders patterns)
        PAs name nestedPattern ->
          maybe id Set.insert (sourceNameText name) (corePatternBinders nestedPattern)
        POr alternatives ->
          case alternatives of
            [] -> Set.empty
            firstAlternative : rest ->
              foldl' Set.intersection (corePatternBinders firstAlternative) (map corePatternBinders rest)

-- | Collect unqualified free references used to validate explicit and alias
-- import visibility before core names are resolved structurally.
collectReferencedNames :: SurfaceExpr -> Set Text
collectReferencedNames = collectExprReferences Set.empty

collectExprReferences :: Set Text -> SurfaceExpr -> Set Text
collectExprReferences boundNames surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar name
      | identifierText name `Set.member` boundNames -> Set.empty
      | otherwise -> Set.singleton (identifierText name)
    SEQualifiedVar _ _ -> Set.empty
    SELambda params body ->
      let parameterList = NonEmpty.toList params
       in Set.union
            (Set.unions (map collectLambdaParameterReferences parameterList))
            ( collectExprReferences
                (Set.union boundNames (Set.unions (map collectLambdaParameterBinders parameterList)))
                body
            )
    SEOperatorValue _ -> Set.empty
    SEList items ->
      Set.unions (map (collectExprReferences boundNames) items)
    SETuple items ->
      Set.unions (map (collectExprReferences boundNames) items)
    SEApply function argument ->
      Set.union
        (collectExprReferences boundNames function)
        (collectExprReferences boundNames argument)
    SETypeApplication function _ _ ->
      collectExprReferences boundNames function
    SEIf condition trueBranch falseBranch ->
      Set.unions
        [ collectExprReferences boundNames condition,
          collectExprReferences boundNames trueBranch,
          collectExprReferences boundNames falseBranch
        ]
    SECase scrutinee arms ->
      Set.union
        (collectExprReferences boundNames scrutinee)
        (Set.unions (map (collectCaseArmReferences boundNames) arms))
    SEBinary _ left right ->
      Set.union
        (collectExprReferences boundNames left)
        (collectExprReferences boundNames right)
    SESectionLeft left _ ->
      collectExprReferences boundNames left
    SESectionRight _ right ->
      collectExprReferences boundNames right
    SEBlock statements ->
      collectBlockReferences boundNames statements

collectBlockReferences :: Set Text -> [SurfaceStatement] -> Set Text
collectBlockReferences boundNames statements =
  Set.unions (map collectStatementReferences statements)
  where
    -- Match analyzer/runtime recursive binding semantics: all `let` binders in
    -- a block are visible while collecting free import references.
    blockBoundNames =
      Set.union
        boundNames
        ( Set.fromList
            [ identifierText bindingName
              | SSLet bindingName _ _ <- statements
            ]
        )

    collectStatementReferences statement =
      case statement of
        SSLet _ _ valueExpr ->
          collectExprReferences blockBoundNames valueExpr
        SSExpr _ expr ->
          collectExprReferences blockBoundNames expr
        SSSignature {} -> Set.empty
        SSData {} -> Set.empty
        SSClass {} -> Set.empty
        SSImpl _ _ _ methods ->
          Set.unions
            [ collectExprReferences blockBoundNames body
              | SurfaceImplMethod _ _ body <- methods
            ]
        SSModule {} -> Set.empty
        SSImport {} -> Set.empty

collectCaseArmReferences :: Set Text -> SurfaceCaseArm -> Set Text
collectCaseArmReferences boundNames (SurfaceCaseArm patternValue guard body) =
  let armBoundNames = Set.union boundNames (collectPatternBinders patternValue)
   in Set.unions
        [ collectPatternReferences patternValue,
          maybe Set.empty (collectExprReferences armBoundNames) guard,
          collectExprReferences armBoundNames body
        ]

collectPatternReferences :: SurfacePattern -> Set Text
collectPatternReferences patternValue =
  case patternValue of
    SPWildcard -> Set.empty
    SPVariable _ -> Set.empty
    SPLiteral _ -> Set.empty
    SPConstructor constructorName nestedPatterns ->
      Set.insert (identifierText constructorName) (Set.unions (map collectPatternReferences nestedPatterns))
    SPList nestedPatterns ->
      Set.unions (map collectPatternReferences nestedPatterns)
    SPConsList headPattern tailPattern ->
      Set.union (collectPatternReferences headPattern) (collectPatternReferences tailPattern)
    SPTuple nestedPatterns ->
      Set.unions (map collectPatternReferences nestedPatterns)
    SPAs _ nestedPattern ->
      collectPatternReferences nestedPattern
    SPOr alternatives ->
      Set.unions (map collectPatternReferences alternatives)

collectPatternBinders :: SurfacePattern -> Set Text
collectPatternBinders patternValue =
  case patternValue of
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

collectLambdaParameterReferences :: SurfaceLambdaParameter -> Set Text
collectLambdaParameterReferences parameter =
  case parameter of
    SurfaceLambdaIdentifier _ -> Set.empty
    SurfaceLambdaPattern patternValue -> collectPatternReferences patternValue

collectLambdaParameterBinders :: SurfaceLambdaParameter -> Set Text
collectLambdaParameterBinders parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> Set.singleton (identifierText name)
    SurfaceLambdaPattern patternValue -> collectPatternBinders patternValue

-- Qualified alias lookups live in the module-alias namespace. Lexical binders
-- intentionally do not shadow aliases, and this traversal should stay aligned
-- with `collectExprReferences` whenever new surface expression forms are added.
collectQualifiedReferences :: SurfaceExpr -> Set (Text, Text)
collectQualifiedReferences surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar _ -> Set.empty
    SEQualifiedVar qualifier member ->
      Set.singleton (identifierText qualifier, identifierText member)
    SELambda _ body ->
      collectQualifiedReferences body
    SEOperatorValue _ -> Set.empty
    SEList items ->
      Set.unions (map collectQualifiedReferences items)
    SETuple items ->
      Set.unions (map collectQualifiedReferences items)
    SEApply function argument ->
      Set.union
        (collectQualifiedReferences function)
        (collectQualifiedReferences argument)
    SETypeApplication function _ _ ->
      collectQualifiedReferences function
    SEIf condition trueBranch falseBranch ->
      Set.unions
        [ collectQualifiedReferences condition,
          collectQualifiedReferences trueBranch,
          collectQualifiedReferences falseBranch
        ]
    SECase scrutinee arms ->
      Set.union
        (collectQualifiedReferences scrutinee)
        (Set.unions (map collectQualifiedCaseArmReferences arms))
    SEBinary _ left right ->
      Set.union
        (collectQualifiedReferences left)
        (collectQualifiedReferences right)
    SESectionLeft left _ ->
      collectQualifiedReferences left
    SESectionRight _ right ->
      collectQualifiedReferences right
    SEBlock statements ->
      Set.unions (map collectQualifiedStatementReferences statements)

collectQualifiedStatementReferences :: SurfaceStatement -> Set (Text, Text)
collectQualifiedStatementReferences statement =
  case statement of
    SSLet _ _ valueExpr ->
      collectQualifiedReferences valueExpr
    SSExpr _ expr ->
      collectQualifiedReferences expr
    SSSignature {} -> Set.empty
    SSData {} -> Set.empty
    SSClass {} -> Set.empty
    SSImpl _ _ _ methods ->
      Set.unions
        [ collectQualifiedReferences body
          | SurfaceImplMethod _ _ body <- methods
        ]
    SSModule {} -> Set.empty
    SSImport {} -> Set.empty

collectQualifiedCaseArmReferences :: SurfaceCaseArm -> Set (Text, Text)
collectQualifiedCaseArmReferences (SurfaceCaseArm _ guard body) =
  Set.union
    (maybe Set.empty collectQualifiedReferences guard)
    (collectQualifiedReferences body)

-- Qualified type heads use the module-alias namespace just like qualified
-- value references, but visibility is checked against the public type
-- inventory before core-name resolution.
collectQualifiedTypeReferences :: SurfaceExpr -> Set (Text, Text)
collectQualifiedTypeReferences surfaceExpr =
  case surfaceExpr of
    SELit _ -> Set.empty
    SEVar _ -> Set.empty
    SEQualifiedVar _ _ -> Set.empty
    SELambda _ body -> collectQualifiedTypeReferences body
    SEOperatorValue _ -> Set.empty
    SEList items -> Set.unions (map collectQualifiedTypeReferences items)
    SETuple items -> Set.unions (map collectQualifiedTypeReferences items)
    SEApply function argument ->
      Set.union
        (collectQualifiedTypeReferences function)
        (collectQualifiedTypeReferences argument)
    SETypeApplication function _ signatureType ->
      Set.union
        (collectQualifiedTypeReferences function)
        (collectQualifiedSignatureTypeReferences signatureType)
    SEIf condition trueBranch falseBranch ->
      Set.unions
        [ collectQualifiedTypeReferences condition,
          collectQualifiedTypeReferences trueBranch,
          collectQualifiedTypeReferences falseBranch
        ]
    SECase scrutinee arms ->
      Set.union
        (collectQualifiedTypeReferences scrutinee)
        (Set.unions (map collectQualifiedCaseArmTypeReferences arms))
    SEBinary _ left right ->
      Set.union
        (collectQualifiedTypeReferences left)
        (collectQualifiedTypeReferences right)
    SESectionLeft left _ -> collectQualifiedTypeReferences left
    SESectionRight _ right -> collectQualifiedTypeReferences right
    SEBlock statements ->
      Set.unions (map collectQualifiedStatementTypeReferences statements)

collectQualifiedStatementTypeReferences :: SurfaceStatement -> Set (Text, Text)
collectQualifiedStatementTypeReferences statement =
  case statement of
    SSLet _ _ valueExpr -> collectQualifiedTypeReferences valueExpr
    SSSignature _ _ payload -> collectQualifiedSignaturePayloadReferences payload
    SSData _ _ _ constructors ->
      Set.unions (map collectQualifiedDataConstructorTypeReferences constructors)
    SSClass _ _ _ methods ->
      Set.unions
        [ collectQualifiedSignaturePayloadReferences payload
          | SurfaceClassMethodSignature _ _ payload <- methods
        ]
    SSImpl _ _ arguments methods ->
      Set.union
        (Set.unions (map collectQualifiedSignatureTypeReferences arguments))
        ( Set.unions
            [ collectQualifiedTypeReferences body
              | SurfaceImplMethod _ _ body <- methods
            ]
        )
    SSModule {} -> Set.empty
    SSImport {} -> Set.empty
    SSExpr _ expr -> collectQualifiedTypeReferences expr

collectQualifiedDataConstructorTypeReferences :: SurfaceDataConstructor -> Set (Text, Text)
collectQualifiedDataConstructorTypeReferences (SurfaceDataConstructor _ arguments) =
  Set.unions
    [ collectQualifiedIdentifierReference name
      | SurfaceDataConstructorArgumentName name <- arguments
    ]

collectQualifiedCaseArmTypeReferences :: SurfaceCaseArm -> Set (Text, Text)
collectQualifiedCaseArmTypeReferences (SurfaceCaseArm _ guard body) =
  Set.union
    (maybe Set.empty collectQualifiedTypeReferences guard)
    (collectQualifiedTypeReferences body)

collectQualifiedSignaturePayloadReferences :: SurfaceSignaturePayload -> Set (Text, Text)
collectQualifiedSignaturePayloadReferences payload =
  case payload of
    SurfaceSignatureType signatureType ->
      collectQualifiedSignatureTypeReferences signatureType
    SurfaceConstrainedSignature constraints signatureType ->
      Set.union
        ( Set.unions
            [ Set.unions (map collectQualifiedSignatureTypeReferences arguments)
              | SurfaceSignatureConstraint _ arguments <- constraints
            ]
        )
        (collectQualifiedSignatureTypeReferences signatureType)
    SurfaceUnsupportedSignature _ -> Set.empty

collectQualifiedSignatureTypeReferences :: SurfaceSignatureType -> Set (Text, Text)
collectQualifiedSignatureTypeReferences signatureType =
  case signatureType of
    SurfaceTypeVariable name -> collectQualifiedIdentifierReference name
    SurfaceTypeName name -> collectQualifiedIdentifierReference name
    SurfaceTypeApplication name arguments ->
      Set.union
        (collectQualifiedIdentifierReference name)
        (Set.unions (map collectQualifiedSignatureTypeReferences arguments))
    SurfaceTypeList innerType -> collectQualifiedSignatureTypeReferences innerType
    SurfaceTypeTuple elementTypes ->
      Set.unions (map collectQualifiedSignatureTypeReferences elementTypes)
    SurfaceTypeFunction argumentType resultType ->
      Set.union
        (collectQualifiedSignatureTypeReferences argumentType)
        (collectQualifiedSignatureTypeReferences resultType)
    _ -> Set.empty

collectQualifiedIdentifierReference :: Identifier -> Set (Text, Text)
collectQualifiedIdentifierReference name =
  maybe Set.empty Set.singleton
    (splitQualifiedIdentifierText (identifierText name))

-- | Validate alias and explicit-symbol imports after dependencies have been
-- resolved so the exporting module inventories are known.
validateImportBindings ::
  FilePath ->
  [Text] ->
  [ParsedImport] ->
  Set Text ->
  Set Text ->
  Set (Text, Text) ->
  Set (Text, Text) ->
  Set Text ->
  Set Text ->
  Map [Text] ModuleExportInventory ->
  Either Diagnostic ()
validateImportBindings sourcePath importerPath imports localClassNames referencedNames qualifiedReferences qualifiedTypeReferences ambientVisibleSymbols ambientVisibleClassNames inventoriesByModule = do
  go Map.empty Map.empty Map.empty imports
  visibleSymbols <- collectVisibleImportSymbols imports
  visibleClassNames <- collectVisibleImportClassNames imports
  validateQualifiedReferences (Set.unions [localClassNames, visibleClassNames, ambientVisibleClassNames])
  validateQualifiedTypeReferences
  let visibleOrAmbientSymbols = Set.union visibleSymbols ambientVisibleSymbols
  case findHiddenExplicitImportReference visibleOrAmbientSymbols of
    Just (symbolName, importDecl) ->
      Left (mkHiddenExplicitImportSymbolError symbolName importDecl)
    Nothing ->
      case findHiddenAliasImportReference visibleOrAmbientSymbols of
        Just (symbolName, importDecl, aliasName) ->
          Left (mkHiddenAliasImportSymbolError symbolName importDecl aliasName)
        Nothing -> Right ()
  where
    dependencyInventory importDecl =
      Map.lookup (parsedImportModulePath importDecl) inventoriesByModule

    eligibleImportNames = selectorEligibleNames

    visibleUnqualifiedInventory importDecl inventory =
      case parsedImportAlias importDecl of
        Just _ -> exportInventory []
        Nothing ->
          visibleImportInventory
            UnqualifiedImport
            (parsedImportSymbols importDecl)
            inventory

    aliasMemberNames inventory =
      exportNamesInNamespaces
        [ValueNamespace, ConstructorNamespace]
        (visibleImportInventory QualifiedAliasImport Nothing inventory)

    aliasTypeNames inventory =
      exportNamesInNamespace
        TypeNamespace
        (visibleImportInventory QualifiedAliasImport Nothing inventory)

    valueAndConstructorNames =
      exportNamesInNamespaces [ValueNamespace, ConstructorNamespace]

    go seenSymbols seenTypes seenAliases remainingImports =
      case remainingImports of
        [] ->
          Right ()
        importDecl : rest -> do
          seenAliasesAfterImport <- validateImportAlias seenAliases importDecl
          seenSymbolsAfterImport <- validateImportSymbols seenSymbols importDecl
          seenTypesAfterImport <- validateImportTypes seenTypes importDecl
          go seenSymbolsAfterImport seenTypesAfterImport seenAliasesAfterImport rest

    validateImportAlias :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportAlias seenAliases importDecl =
      case parsedImportAlias importDecl of
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
                      { bindingOriginModulePath = parsedImportModulePath importDecl,
                        bindingOriginSpan = parsedImportSpan importDecl
                      }
                    seenAliases
                )

    validateImportSymbols :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbols seenSymbols importDecl =
      case parsedImportAlias importDecl of
        Just _ ->
          Right seenSymbols
        Nothing ->
          case dependencyInventory importDecl of
            Nothing ->
              Left
                ( mkErrorDiagnostic
                    E4010 CompilationOrigin
                    ( "internal resolver error while validating imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (parsedImportModulePath importDecl)
                        <> "'"
                    )
                )
            Just inventory ->
              let exportedImportSymbols = eligibleImportNames inventory
                  importedSymbolNames =
                    case parsedImportSymbols importDecl of
                      Nothing -> Set.toAscList exportedImportSymbols
                      Just explicitSymbolNames -> explicitSymbolNames
               in
              foldM
                (validateImportSymbol importDecl exportedImportSymbols)
                seenSymbols
                importedSymbolNames

    validateImportTypes :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportTypes seenTypes importDecl =
      case parsedImportAlias importDecl of
        Just _ ->
          Right seenTypes
        Nothing ->
          case dependencyInventory importDecl of
            Nothing ->
              Left
                ( mkErrorDiagnostic
                    E4010 CompilationOrigin
                    ( "internal resolver error while validating type imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (parsedImportModulePath importDecl)
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

    validateImportType :: ParsedImport -> Map Text BindingOrigin -> Text -> Either Diagnostic (Map Text BindingOrigin)
    validateImportType importDecl seenTypes typeName =
      case Map.lookup typeName seenTypes of
        Just previousOrigin
          | bindingOriginModulePath previousOrigin == parsedImportModulePath importDecl ->
              Right seenTypes
          | otherwise ->
              Left (mkImportTypeCollisionError typeName previousOrigin importDecl)
        Nothing ->
          Right
            ( Map.insert
                typeName
                BindingOrigin
                  { bindingOriginModulePath = parsedImportModulePath importDecl,
                    bindingOriginSpan = parsedImportSpan importDecl
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
                            E4010 CompilationOrigin
                            ( "internal resolver error while validating imports for '"
                                <> renderModulePath importerPath
                                <> "': missing exports for module '"
                                <> renderModulePath (parsedImportModulePath importDecl)
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
                        E4010 CompilationOrigin
                        ( "internal resolver error while validating type imports for '"
                            <> renderModulePath importerPath
                            <> "': missing exports for module '"
                            <> renderModulePath (parsedImportModulePath importDecl)
                            <> "'"
                        )
                    )
                Just inventory ->
                  let exportedTypes = aliasTypeNames inventory
                   in if Set.member typeName exportedTypes
                        then Right ()
                        else Left (mkMissingQualifiedAliasSymbolError typeName importDecl aliasName exportedTypes)

    findAliasImport :: Text -> Maybe ParsedImport
    findAliasImport aliasName =
      firstMatch
        [ importDecl
          | importDecl <- imports,
            parsedImportAlias importDecl == Just aliasName
        ]

    validateImportSymbol ::
      ParsedImport ->
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
              | bindingOriginModulePath previousOrigin == parsedImportModulePath importDecl ->
                  Right seenSymbols
              | otherwise ->
                  Left (mkImportSymbolCollisionError symbolName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    symbolName
                    BindingOrigin
                      { bindingOriginModulePath = parsedImportModulePath importDecl,
                        bindingOriginSpan = parsedImportSpan importDecl
                      }
                    seenSymbols
                )

    mkMissingImportSymbolError :: Text -> ParsedImport -> Set Text -> Diagnostic
    mkMissingImportSymbolError symbolName importDecl exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkErrorDiagnostic
              E4007 CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' imported by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'; available exports: "
                  <> renderExports exportedSymbols
              )
          )

    mkImportSymbolCollisionError :: Text -> BindingOrigin -> ParsedImport -> Diagnostic
    mkImportSymbolCollisionError symbolName previousOrigin importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (parsedImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4008 CompilationOrigin
                  ( "import binding collision for symbol '"
                      <> symbolName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already imported from '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot re-import from '"
                      <> renderModulePath (parsedImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    mkImportTypeCollisionError :: Text -> BindingOrigin -> ParsedImport -> Diagnostic
    mkImportTypeCollisionError typeName previousOrigin importDecl =
      setDiagnosticSubject typeName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (parsedImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4008 CompilationOrigin
                  ( "import type collision for '"
                      <> typeName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already imported from '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot re-import from '"
                      <> renderModulePath (parsedImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    mkUnknownQualifiedAliasError :: Text -> Text -> Diagnostic
    mkUnknownQualifiedAliasError aliasName symbolName =
      setDiagnosticSubject aliasName $
        mkErrorDiagnostic
          E4013 CompilationOrigin
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

    mkMissingQualifiedAliasSymbolError :: Text -> ParsedImport -> Text -> Set Text -> Diagnostic
    mkMissingQualifiedAliasSymbolError symbolName importDecl aliasName exportedSymbols =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkErrorDiagnostic
              E4014 CompilationOrigin
              ( "qualified import symbol '"
                  <> symbolName
                  <> "' is not exported by module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
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

    -- Visible imports include all bare imports and explicit symbol-list imports;
    -- alias-only imports intentionally expose nothing unqualified.
    collectVisibleImportSymbols :: [ParsedImport] -> Either Diagnostic (Set Text)
    collectVisibleImportSymbols =
      foldM collectVisibleImportSymbol Set.empty

    collectVisibleImportSymbol :: Set Text -> ParsedImport -> Either Diagnostic (Set Text)
    collectVisibleImportSymbol visibleSymbols importDecl =
      case dependencyInventory importDecl of
        Nothing ->
          Left
            ( mkErrorDiagnostic
                E4010 CompilationOrigin
                ( "internal resolver error while validating imports for '"
                    <> renderModulePath importerPath
                    <> "': missing exports for module '"
                    <> renderModulePath (parsedImportModulePath importDecl)
                    <> "'"
                )
            )
        Just inventory ->
          Right
            ( Set.union
                visibleSymbols
                ( valueAndConstructorNames
                    (visibleUnqualifiedInventory importDecl inventory)
                )
            )

    collectVisibleImportClassNames :: [ParsedImport] -> Either Diagnostic (Set Text)
    collectVisibleImportClassNames =
      foldM collectVisibleImportClassName Set.empty

    collectVisibleImportClassName :: Set Text -> ParsedImport -> Either Diagnostic (Set Text)
    collectVisibleImportClassName visibleClassNames importDecl =
      case dependencyInventory importDecl of
        Nothing ->
          Left
            ( mkErrorDiagnostic
                E4010 CompilationOrigin
                ( "internal resolver error while validating imports for '"
                    <> renderModulePath importerPath
                    <> "': missing exports for module '"
                    <> renderModulePath (parsedImportModulePath importDecl)
                    <> "'"
                )
            )
        Just inventory ->
          Right
            ( Set.union
                visibleClassNames
                ( exportNamesInNamespace
                    CapabilityNamespace
                    (visibleUnqualifiedInventory importDecl inventory)
                )
            )

    findHiddenExplicitImportReference :: Set Text -> Maybe (Text, ParsedImport)
    findHiddenExplicitImportReference visibleSymbols =
      firstMatch
        [ (symbolName, importDecl)
          | importDecl <- imports,
            Just symbolNames <- [parsedImportSymbols importDecl],
            Just inventory <- [dependencyInventory importDecl],
            let exportedSymbols = valueAndConstructorNames inventory,
            let hiddenSymbols = Set.difference exportedSymbols (Set.fromList symbolNames),
            symbolName <- Set.toList hiddenSymbols,
            Set.member symbolName referencedNames,
            not (Set.member symbolName visibleSymbols)
        ]

    findHiddenAliasImportReference :: Set Text -> Maybe (Text, ParsedImport, Text)
    findHiddenAliasImportReference visibleSymbols =
      firstMatch
        [ (symbolName, importDecl, aliasName)
          | importDecl <- imports,
            Just aliasName <- [parsedImportAlias importDecl],
            Just inventory <- [dependencyInventory importDecl],
            let exportedSymbols = valueAndConstructorNames inventory,
            symbolName <- Set.toList exportedSymbols,
            Set.member symbolName referencedNames,
            not (Set.member symbolName visibleSymbols)
        ]

    firstMatch :: [a] -> Maybe a
    firstMatch matches =
      case matches of
        [] -> Nothing
        match : _ -> Just match

    mkHiddenExplicitImportSymbolError :: Text -> ParsedImport -> Diagnostic
    mkHiddenExplicitImportSymbolError symbolName importDecl =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkErrorDiagnostic
              E4011 CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible from explicit import of module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkHiddenAliasImportSymbolError :: Text -> ParsedImport -> Text -> Diagnostic
    mkHiddenAliasImportSymbolError symbolName importDecl aliasName =
      setDiagnosticSubject symbolName $
        setDiagnosticPrimarySpan
          (parsedImportSpan importDecl)
          ( mkErrorDiagnostic
              E4012 CompilationOrigin
              ( "import symbol '"
                  <> symbolName
                  <> "' is not visible unqualified from alias import of module '"
                  <> renderModulePath (parsedImportModulePath importDecl)
                  <> "' as '"
                  <> aliasName
                  <> "' by '"
                  <> renderModulePath importerPath
                  <> "' in '"
                  <> Text.pack sourcePath
                  <> "'"
              )
          )

    mkImportAliasCollisionError :: Text -> BindingOrigin -> ParsedImport -> Diagnostic
    mkImportAliasCollisionError aliasName previousOrigin importDecl =
      setDiagnosticSubject aliasName $
        setDiagnosticRelatedSpan
          (bindingOriginSpan previousOrigin)
          ( setDiagnosticPrimarySpan
              (parsedImportSpan importDecl)
              ( mkErrorDiagnostic
                  E4009 CompilationOrigin
                  ( "import alias collision for '"
                      <> aliasName
                      <> "' in module '"
                      <> renderModulePath importerPath
                      <> "' at '"
                      <> Text.pack sourcePath
                      <> "'; already aliased to module '"
                      <> renderModulePath (bindingOriginModulePath previousOrigin)
                      <> "', cannot alias module '"
                      <> renderModulePath (parsedImportModulePath importDecl)
                      <> "'"
                  )
              )
          )

    renderExports :: Set Text -> Text
    renderExports exports
      | Set.null exports = "<none>"
      | otherwise = Text.intercalate ", " (sortOn id (Set.toList exports))

-- | Provide a deterministic lexical import order for traversal and diagnostics.
-- Encounter order is intentionally discarded by `Set`-based deduplication and
-- the final `renderModulePath` sort.
sortModulePaths :: [[Text]] -> [[Text]]
sortModulePaths modulePaths =
  map snd . sortOn fst $ map (\modulePath -> (renderModulePath modulePath, modulePath)) uniquePaths
  where
    uniquePaths = Set.toList (Set.fromList modulePaths)

mkCycleError :: [Text] -> [[Text]] -> Diagnostic
mkCycleError repeatedModulePath callStack =
  mkErrorDiagnostic
    E4003 CompilationOrigin
    ("module import cycle detected: " <> Text.intercalate " -> " (map renderModulePath cycleTrace))
  where
    rootToLeaf = reverse callStack
    suffixStartingAtRepeat = dropWhile (/= repeatedModulePath) rootToLeaf
    cycleTrace = suffixStartingAtRepeat ++ [repeatedModulePath]

renderImporterContext :: [[Text]] -> Text
renderImporterContext callStack =
  case callStack of
    importerPath : _ -> " imported by '" <> renderModulePath importerPath <> "'"
    [] -> ""

renderModulePath :: [Text] -> Text
renderModulePath segments = Text.intercalate "::" segments

-- | Preserve the first occurrence of each candidate path so module-root lookup
-- order remains stable while removing duplicates.
dedupePreservingOrder :: Ord a => [a] -> [a]
dedupePreservingOrder =
  reverse . fst . foldl' step ([], Set.empty)
  where
    step (uniqueRev, seen) value
      | Set.member value seen = (uniqueRev, seen)
      | otherwise = (value : uniqueRev, Set.insert value seen)
