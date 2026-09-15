{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CorePhase (Lowered, Resolved), coreNodeSpan)
import Jazz.Compiler.DiagnosticCatalog
  ( diagnosticCodeText,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSubject,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleExportSelector (..),
    exportInventory,
    exportInventoryEntries,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    mkModulePath,
    mkSourceFile,
    moduleIdentity,
    moduleIdentitySource,
    modulePathRelativeFile,
    modulePathTextSegments,
    moduleQualifierIdentifier,
    renderModulePath,
    sourceFilePath,
  )
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    parseModulePathText,
    resolveProgramWithAmbientExports,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ConstructorNamespace, TypeNamespace, ValueNamespace),
    identifierText,
    mkIdentifier,
  )
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceModule)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertLeftDiagnosticCodeAndContains,
    assertRight,
    failTest,
    runTestSuite,
  )

data ResolvedModuleSummary = ResolvedModuleSummary
  { summaryModulePath :: [Text],
    summarySourcePath :: FilePath,
    summaryImports :: [[Text]]
  }
  deriving (Eq, Show)

data ImportExposureSummary
  = AllUnqualifiedSummary
  | OnlyUnqualifiedSummary [Text]
  | QualifiedOnlySummary
  deriving (Eq, Show)

data ResolvedImportSummary = ResolvedImportSummary
  { summaryImportSpan :: SourceSpan,
    summaryImportPath :: [Text],
    summaryImportAlias :: Maybe Text,
    summaryImportExposure :: ImportExposureSummary
  }
  deriving (Eq, Show)

resolvedImportSummary :: ModuleGraph.ModuleImport 'Resolved -> ResolvedImportSummary
resolvedImportSummary importDecl =
  ResolvedImportSummary
    { summaryImportSpan = coreNodeSpan (ModuleGraph.moduleImportNode importDecl),
      summaryImportPath = modulePathSegments (ModuleGraph.importedModule importDecl),
      summaryImportAlias =
        case ModuleGraph.importExposure importDecl of
          ModuleGraph.ImportQualifiedOnly qualifier -> Just (identifierText (moduleQualifierIdentifier qualifier))
          _ -> Nothing,
      summaryImportExposure =
        case ModuleGraph.importExposure importDecl of
          ModuleGraph.ImportAllUnqualified -> AllUnqualifiedSummary
          ModuleGraph.ImportOnlyUnqualified names ->
            OnlyUnqualifiedSummary (map identifierText (NonEmpty.toList names))
          ModuleGraph.ImportQualifiedOnly _ -> QualifiedOnlySummary
    }

resolvedModuleSummary :: ModuleGraph.CoreModule 'Resolved -> ResolvedModuleSummary
resolvedModuleSummary resolvedModule =
  ResolvedModuleSummary
    { summaryModulePath = resolvedModulePathSegments resolvedModule,
      summarySourcePath = sourceFilePath (moduleIdentitySource (ModuleGraph.coreModuleIdentity resolvedModule)),
      summaryImports = normalizedImportPaths (ModuleGraph.coreModuleImports resolvedModule)
    }
  where
    normalizedImportPaths imports =
      map snd . sortOn fst $
        [ (Text.intercalate "::" modulePath, modulePath)
        | modulePath <- Set.toList (Set.fromList (map (modulePathSegments . ModuleGraph.importedModule) imports))
        ]

resolvedModulePathSegments :: ModuleGraph.CoreModule phase -> [Text]
resolvedModulePathSegments = modulePathSegments . ModuleGraph.coreModulePath

modulePathSegments :: ModulePath -> [Text]
modulePathSegments = NonEmpty.toList . modulePathTextSegments

programModules :: ModuleGraph.CoreProgram phase -> [ModuleGraph.CoreModule phase]
programModules = NonEmpty.toList . ModuleGraph.coreProgramModules

resolvedModuleExportInventory :: ModuleGraph.CoreModule 'Resolved -> ModuleExportInventory
resolvedModuleExportInventory =
  ModuleGraph.resolvedModuleExports . ModuleGraph.coreModuleFacts

resolvedModuleExportSelectors :: ModuleGraph.CoreModule 'Resolved -> Maybe [ModuleExportSelector]
resolvedModuleExportSelectors =
  ModuleGraph.resolvedModuleExportSelectors . ModuleGraph.coreModuleFacts

resolveTestProgram ::
  ModuleResolutionConfig ->
  (FilePath -> IO (Maybe Text)) ->
  [Text] ->
  IO (Either Diagnostic (ModuleGraph.CoreProgram 'Resolved))
resolveTestProgram config =
  resolveProgramWithAmbientExports config testPrelude (exportInventory [])

resolveTestModuleGraph ::
  ModuleResolutionConfig ->
  Map.Map FilePath Text ->
  [Text] ->
  IO (Either Diagnostic [ModuleGraph.CoreModule 'Resolved])
resolveTestModuleGraph config sources entryModulePath =
  fmap (fmap programModules) $
    resolveProgramWithAmbientExports
      config
      testPrelude
      (exportInventory [])
      (\path -> pure (Map.lookup path sources))
      entryModulePath

assertTestModulesRight ::
  Text ->
  IO (Either Diagnostic [ModuleGraph.CoreModule 'Resolved]) ->
  ([ModuleGraph.CoreModule 'Resolved] -> IO ()) ->
  IO ()
assertTestModulesRight label resolution check =
  resolution >>= \result -> assertRight label result check

assertTestModulesLeftDiagnostic ::
  Text ->
  Text ->
  Text ->
  IO (Either Diagnostic [ModuleGraph.CoreModule 'Resolved]) ->
  IO ()
assertTestModulesLeftDiagnostic label expectedCode needle resolution =
  resolution >>= assertLeftDiagnosticCodeAndContains label expectedCode needle

main :: IO ()
main = runTestSuite "ModuleResolution" tests

tests :: [NamedTest]
tests =
  [ ("core programs reject a missing entry module", testCoreProgramRejectsMissingEntry),
    ("core programs reject duplicate module paths", testCoreProgramRejectsDuplicatePath),
    ("core programs reject dependencies ordered after dependents", testCoreProgramRejectsDependencyAfterDependent),
    ("core programs reject imports outside the program", testCoreProgramRejectsUnknownImport),
    ("core programs preserve dependency-first module order", testCoreProgramPreservesDependencyOrder),
    ("rejects empty entry module path before traversal", testRejectsEmptyEntryModulePath),
    ("resolved program retains lowered modules", testResolvedProgramRetainsLoweredModules),
    ("resolved module carries explicit public inventory", testResolvedModuleCarriesExplicitPublicInventory),
    ("resolved module preserves authored explicit export selector order", testResolvedModulePreservesExplicitExportSelectorOrder),
    ("resolves mixed module facts without changing inventories", testResolvesMixedModuleFacts),
    ("empty export list produces empty inventory", testEmptyExportListProducesEmptyInventory),
    ("namespace-aware exports select exact public entries", testNamespaceAwareExportsSelectExactEntries),
    ("namespace-aware exports reject same-name wrong namespace", testNamespaceAwareExportsRejectWrongNamespace),
    ("namespace-aware export diagnostics render an empty inventory", testNamespaceAwareExportDiagnosticRendersEmptyInventory),
    ("grouped type exports expand into one flat public inventory", testGroupedTypeExportsExpandFlatInventory),
    ("grouped type exports reject unknown types at the type span", testGroupedTypeExportsRejectUnknownType),
    ("grouped type exports reject unknown constructors at the member span", testGroupedTypeExportsRejectUnknownConstructor),
    ("grouped type exports reject constructors owned by another local type", testGroupedTypeExportsRejectWrongOwner),
    ("grouped type exports reject imported constructors", testGroupedTypeExportsRejectImportedConstructor),
    ("explicit exports keep private local bindings resolvable", testExplicitExportsKeepPrivateLocalsUsable),
    ("rejects unknown module export names", testRejectsUnknownModuleExport),
    ("rejects imported-only module export names", testRejectsImportedOnlyModuleExport),
    ("explicit imports reject private module bindings", testExplicitImportRejectsPrivateModuleBinding),
    ("module paths parse into a non-empty nominal identity", testParseNominalModulePath),
    ("invalid module path text retains E4016", testRejectsInvalidModulePathText),
    ("accepts lexer-compatible continuation characters in CLI module paths", testParseModulePathContinuations),
    ("preserves exact module path segments while resolving", testPreservesExactModulePathSegments),
    ("maps module path to relative .jz file", testModulePathMapping),
    ("maps nested module paths to canonical .jz files", testNestedModulePathMapping),
    ("accepts omitted module declaration from resolved source path", testAcceptsOmittedModuleDeclaration),
    ("accepts matching module declaration in resolved file", testAcceptsMatchingModuleDeclaration),
    ("resolves dependency graph in deterministic order", testResolveDependencyGraph),
    ("source loading stops at the first dependency failure", testSourceLoadingStopsAtDependencyFailure),
    ("retains checked import exposure in declaration order", testRetainsCheckedImportExposureInDeclarationOrder),
    ("resolves imports in lexical rendered-path order", testResolveImportsInLexicalRenderedPathOrder),
    ("collapses duplicate imports to one dependency edge", testCollapsesDuplicateImports),
    ("reuses already-resolved modules across branches", testReusesAlreadyResolvedModuleAcrossBranches),
    ("deduplicates duplicate module roots before ambiguity checks", testDeduplicatesDuplicateRoots),
    ("deduplicates lexically equivalent module roots before ambiguity checks", testDeduplicatesEquivalentRoots),
    ("reports unresolved import with importer context", testReportsUnresolvedImport),
    ("reports ambiguous module candidates across roots", testReportsAmbiguousImport),
    ("reports import cycles with minimal trace", testReportsCycle),
    ("reports nested import cycles with minimal trace", testReportsNestedCycleMinimalTrace),
    ("reports parse failures while loading imported modules", testReportsImportedModuleParseFailure),
    ("reports module declaration mismatch for resolved file path", testReportsModuleDeclarationMismatch),
    ("reports nested module declaration parse failure in a module file", testReportsNestedModuleDeclarationParseFailure),
    ("accepts symbol-list imports when requested symbols are exported", testAcceptsValidImportSymbolList),
    ("accepts symbol-list imports for data constructors", testAcceptsDataConstructorImportSymbolList),
    ("accepts type applications while collecting module references", testAcceptsTypeApplicationsWhileCollectingModuleReferences),
    ("accepts bare imports as unqualified visible exports", testAcceptsBareImportUnqualifiedExport),
    ("accepts local bindings over hidden symbol-list exports", testAcceptsLocalBindingOverHiddenExplicitImport),
    ("reports non-exported import symbols with module context", testReportsMissingImportSymbol),
    ("reports unqualified references hidden by explicit symbol lists", testReportsHiddenExplicitImportValueReference),
    ("reports import symbol collisions across imported modules", testReportsImportSymbolCollision),
    ("reports import symbol collisions across bare imports", testReportsBareImportSymbolCollision),
    ("reports import symbol collisions across bare and symbol-list imports", testReportsMixedImportSymbolCollision),
    ("reports import alias collisions across imported modules", testReportsImportAliasCollision),
    ("reports pattern references to constructors hidden by explicit imports", testReportsHiddenExplicitImportConstructorPatternReference),
    ("reports unqualified references to bindings imported only by alias", testReportsUnqualifiedAliasImportReference),
    ("reports pattern references to constructors hidden by alias imports", testReportsHiddenAliasImportConstructorPatternReference),
    ("accepts qualified alias references before alias declaration", testAcceptsQualifiedAliasReferenceBeforeImport),
    ("accepts local bindings sharing qualified alias names", testAcceptsLocalBindingSharingAliasName),
    ("accepts qualified references through alias imports", testAcceptsQualifiedAliasImportReference),
    ("accepts qualified references to data constructors through alias imports", testAcceptsQualifiedAliasDataConstructorReference),
    ("accepts explicit class import symbols", testAcceptsExplicitClassImportSymbol),
    ("rejects type-only explicit import symbols", testRejectsTypeOnlyImportSymbol),
    ("reports class import collisions", testReportsClassImportCollision),
    ("reports type import collisions", testReportsTypeImportCollision),
    ("keeps repeated class imports idempotent", testKeepsRepeatedClassImportsIdempotent),
    ("reports qualified references through unknown aliases", testReportsUnknownQualifiedAliasReference),
    ("reports standalone qualified references through unknown aliases", testReportsStandaloneUnknownQualifiedAliasReference),
    ("reports qualified alias references to missing exports", testReportsMissingQualifiedAliasExport),
    ("implementation methods inventory hidden unqualified references", testImplMethodRejectsHiddenUnqualifiedReference),
    ("implementation methods inventory hidden qualified references", testImplMethodRejectsHiddenQualifiedReference),
    ("module lexer failures retain source-qualified structured detail", testModuleLexerFailureRetainsStructuredDetail)
  ]

testCoreProgramRejectsMissingEntry :: IO ()
testCoreProgramRejectsMissingEntry = do
  dependency <- lowerInvariantModule ["Lib", "Value"] "answer = 1."
  assertEqual
    "missing entry failure"
    (Left (ModuleGraph.MissingEntryModule entryPath :| []))
    (ModuleGraph.mkCoreProgram absentPrelude entryPath (dependency :| []))

testCoreProgramRejectsDuplicatePath :: IO ()
testCoreProgramRejectsDuplicatePath = do
  entry <- lowerInvariantModule ["App", "Main"] "0."
  duplicate <- lowerInvariantModule ["App", "Main"] "1."
  assertEqual
    "duplicate module path failure"
    (Left (ModuleGraph.DuplicateModulePath entryPath :| []))
    (ModuleGraph.mkCoreProgram absentPrelude entryPath (entry :| [duplicate]))

testCoreProgramRejectsDependencyAfterDependent :: IO ()
testCoreProgramRejectsDependencyAfterDependent = do
  entry <- lowerInvariantModule ["App", "Main"] "import Lib::Value. answer."
  dependency <- lowerInvariantModule ["Lib", "Value"] "answer = 1."
  assertEqual
    "dependency order failure"
    (Left (ModuleGraph.DependencyAfterDependent entryPath dependencyPath :| []))
    (ModuleGraph.mkCoreProgram absentPrelude entryPath (entry :| [dependency]))

testCoreProgramRejectsUnknownImport :: IO ()
testCoreProgramRejectsUnknownImport = do
  entry <- lowerInvariantModule ["App", "Main"] "import Lib::Value. answer."
  assertEqual
    "unknown import failure"
    (Left (ModuleGraph.UnknownImportedModule entryPath dependencyPath :| []))
    (ModuleGraph.mkCoreProgram absentPrelude entryPath (entry :| []))

testCoreProgramPreservesDependencyOrder :: IO ()
testCoreProgramPreservesDependencyOrder = do
  dependency <- lowerInvariantModule ["Lib", "Value"] "answer = 1."
  entry <- lowerInvariantModule ["App", "Main"] "import Lib::Value. answer."
  case ModuleGraph.mkCoreProgram absentPrelude entryPath (dependency :| [entry]) of
    Left failures -> failTest ("expected valid core program, got " <> Text.pack (show failures))
    Right program ->
      assertEqual
        "dependency-first order"
        [dependencyPath, entryPath]
        (map ModuleGraph.coreModulePath (NonEmpty.toList (ModuleGraph.coreProgramModules program)))

lowerInvariantModule :: [Text] -> Text -> IO (ModuleGraph.CoreModule 'Lowered)
lowerInvariantModule path source =
  case parseSurfaceProgram source of
    Left diagnostic -> failTest ("invariant fixture parse failed: " <> renderDiagnostic diagnostic)
    Right surface ->
      case NonEmpty.nonEmpty (map mkIdentifier path) of
        Nothing -> failTest "invariant fixture module path must be nonempty"
        Just pathSegments ->
          case lowerSurfaceModule
            ( moduleIdentity
                (mkModulePath pathSegments)
                (mkSourceFile (Text.unpack (Text.intercalate "/" path) <> ".jz"))
            )
            surface of
            Left diagnostic -> failTest ("invariant fixture lowering failed: " <> renderDiagnostic diagnostic)
            Right coreModule -> pure coreModule

absentPrelude :: ModuleGraph.PreludeArtifact 'Lowered
absentPrelude =
  ModuleGraph.PreludeArtifact
    { ModuleGraph.preludeIdentity =
        moduleIdentity
          (mkModulePath (mkIdentifier "Jazz" :| [mkIdentifier "Prelude"]))
          (mkSourceFile "<absent-prelude>"),
      ModuleGraph.preludeModule = Nothing
    }

testPrelude :: ModuleGraph.PreludeArtifact phase
testPrelude =
  ModuleGraph.PreludeArtifact
    { ModuleGraph.preludeIdentity = ModuleGraph.preludeIdentity absentPrelude,
      ModuleGraph.preludeModule = Nothing
    }

entryPath :: ModulePath
entryPath = mkModulePath (mkIdentifier "App" :| [mkIdentifier "Main"])

dependencyPath :: ModulePath
dependencyPath = mkModulePath (mkIdentifier "Lib" :| [mkIdentifier "Value"])

testResolvedProgramRetainsLoweredModules :: IO ()
testResolvedProgramRetainsLoweredModules = do
  result <-
    resolveTestProgram
      resolverConfig
      lookupSource
      ["App", "Main"]
  assertRight "resolved program" result $ \program -> do
    assertEqual
      "module order"
      [["Lib", "Value"], ["App", "Main"]]
      (map resolvedModulePathSegments (programModules program))
    assertEqual "entry path" ["App", "Main"] (modulePathSegments (ModuleGraph.coreProgramEntry program))
    assertEqual "module count" 2 (length (programModules program))
  where
    resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testResolvedModuleCarriesExplicitPublicInventory :: IO ()
testResolvedModuleCarriesExplicitPublicInventory = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["App", "Main"]
  assertRight "resolved explicit public inventory" result $ \program ->
    case [ resolvedModule
         | resolvedModule <- programModules program,
           resolvedModulePathSegments resolvedModule == ["Lib", "Value"]
         ] of
      [resolvedModule] ->
        assertEqual
          "public inventory contains only answer"
          (Set.singleton (ModuleExport ValueNamespace "answer"))
          ( exportInventoryEntries
              (resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Value module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Value (answer).
            answer.
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value (answer) {
            helper = 1.
            answer = helper.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testResolvedModulePreservesExplicitExportSelectorOrder :: IO ()
testResolvedModulePreservesExplicitExportSelectorOrder = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["App", "Main"]
  assertRight "resolved explicit export selector order" result $ \program ->
    case programModules program of
      [resolvedModule] ->
        assertEqual
          "authored selector order"
          ( Just
              [ ModuleExportSelector (Just ValueNamespace) "zeta",
                ModuleExportSelector (Just ValueNamespace) "alpha"
              ]
          )
          (resolvedModuleExportSelectors resolvedModule)
      modules -> failTest ("expected one resolved App::Main module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.singleton
        "src/App/Main.jz"
        """
        module App::Main (value zeta, value alpha) {
        alpha = 1.
        zeta = 2.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testResolvesMixedModuleFacts :: IO ()
testResolvesMixedModuleFacts = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["App", "Main"]
  assertRight "resolved mixed module facts" result $ \program -> do
    assertEqual
      "dependency order"
      [["Lib", "Types"], ["Lib", "Values"], ["App", "Main"]]
      (map resolvedModulePathSegments (programModules program))
    case [ resolvedModule
         | resolvedModule <- programModules program,
           resolvedModulePathSegments resolvedModule == ["App", "Main"]
         ] of
      [resolvedModule] ->
        assertEqual
          "mixed public inventory"
          ( Set.fromList
              [ ModuleExport TypeNamespace "Local",
                ModuleExport ConstructorNamespace "Local",
                ModuleExport ValueNamespace "main"
              ]
          )
          (exportInventoryEntries (resolvedModuleExportInventory resolvedModule))
      modules -> failTest ("expected one resolved App::Main module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main (type Local(..), value main) {
            import Lib::Types as T.
            import Lib::Values (seed).
            import Lib::Values as V.
            data Local = Local T::Box.
            main :: Int.
            main = V::box seed.
            }
            """
          ),
          ("src/Lib/Types.jz", "module Lib::Types (type Box(..)) { data Box = Box. }"),
          ("src/Lib/Values.jz", "module Lib::Values (seed, box) { seed = 1. box = \\(candidate) -> candidate. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testEmptyExportListProducesEmptyInventory :: IO ()
testEmptyExportListProducesEmptyInventory = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["App", "Main"]
  assertRight "resolved empty public inventory" result $ \program ->
    case [ resolvedModule
         | resolvedModule <- programModules program,
           resolvedModulePathSegments resolvedModule == ["Lib", "Value"]
         ] of
      [resolvedModule] ->
        assertEqual
          "public inventory is empty"
          Set.empty
          ( exportInventoryEntries
              (resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Value module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Value.
            0.
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value () {
            hidden = 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportsSelectExactEntries :: IO ()
testNamespaceAwareExportsSelectExactEntries = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["Lib", "Box"]
  assertRight "resolved namespace-aware public inventory" result $ \program ->
    case programModules program of
      [resolvedModule] ->
        assertEqual
          "public inventory contains exact type and value exports"
          ( Set.fromList
              [ ModuleExport TypeNamespace "Box",
                ModuleExport ValueNamespace "Box"
              ]
          )
          ( exportInventoryEntries
              (resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Box module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.singleton
        "src/Lib/Box.jz"
        """
        module Lib::Box (type Box, value Box) {
        data Box a = Box a.
        Box = 1.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportsRejectWrongNamespace :: IO ()
testNamespaceAwareExportsRejectWrongNamespace = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["Lib", "Token"]
  assertLeftDiagnosticCodeAndContains
    "wrong namespace module export"
    "E4015"
    "module export type 'Token' is not declared by module 'Lib::Token'"
    result
  where
    sources =
      Map.singleton
        "src/Lib/Token.jz"
        """
        module Lib::Token (type Token) {
        data Box = Token.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportDiagnosticRendersEmptyInventory :: IO ()
testNamespaceAwareExportDiagnosticRendersEmptyInventory = do
  result <-
    resolveTestProgram
      testResolverConfig
      lookupSource
      ["Lib", "Empty"]
  assertLeftDiagnosticCodeAndContains
    "empty namespace-aware module export inventory"
    "E4015"
    "available declarations: <none>"
    result
  where
    sources =
      Map.singleton
        "src/Lib/Empty.jz"
        """
        module Lib::Empty (type Missing) {
        0.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testGroupedTypeExportsExpandFlatInventory :: IO ()
testGroupedTypeExportsExpandFlatInventory = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Types"]
  assertRight "resolved grouped public inventory" result $ \program ->
    case programModules program of
      [resolvedModule] ->
        assertEqual
          "grouped selectors expand and deduplicate"
          ( Set.fromList
              [ ModuleExport TypeNamespace "Opaque",
                ModuleExport TypeNamespace "Choice",
                ModuleExport ConstructorNamespace "First",
                ModuleExport ConstructorNamespace "Second",
                ModuleExport TypeNamespace "Pair",
                ModuleExport ConstructorNamespace "Pair",
                ModuleExport ConstructorNamespace "Unit"
              ]
          )
          (exportInventoryEntries (resolvedModuleExportInventory resolvedModule))
      modules -> failTest ("expected one resolved Lib::Types module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.singleton
        "src/Lib/Types.jz"
        """
        module Lib::Types (type Opaque, type Choice(..), type Pair(Pair), constructor Pair, constructor Unit) {
        data Opaque = Hidden.
        data Choice = First | Second.
        data Pair a = Pair a | Unit.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testGroupedTypeExportsRejectUnknownType :: IO ()
testGroupedTypeExportsRejectUnknownType = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Types"]
  assertLeftDiagnosticCodeAndContains
    "unknown grouped type"
    "E4015"
    "module export type 'Missing(..)' is not declared"
    result
  assertLeftDiagnosticMetadata
    "unknown grouped type metadata"
    (Just (SourceRangeIn "src/Lib/Types.jz" 1 25 1 32))
    Nothing
    (Just "Missing")
    result
  where
    sources = Map.singleton "src/Lib/Types.jz" "module Lib::Types (type Missing(..)) { data Present = Present. }"
    lookupSource path = pure (Map.lookup path sources)

testGroupedTypeExportsRejectUnknownConstructor :: IO ()
testGroupedTypeExportsRejectUnknownConstructor = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Types"]
  assertLeftDiagnosticCodeAndContains
    "unknown grouped constructor"
    "E4015"
    "constructor 'Missing' is not declared by type 'Choice'"
    result
  assertLeftDiagnosticMetadata
    "unknown grouped constructor metadata"
    (Just (SourceRangeIn "src/Lib/Types.jz" 1 32 1 39))
    Nothing
    (Just "Missing")
    result
  where
    sources = Map.singleton "src/Lib/Types.jz" "module Lib::Types (type Choice(Missing)) { data Choice = Present. }"
    lookupSource path = pure (Map.lookup path sources)

testGroupedTypeExportsRejectWrongOwner :: IO ()
testGroupedTypeExportsRejectWrongOwner = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Types"]
  assertLeftDiagnosticCodeAndContains
    "wrong-owner grouped constructor"
    "E4015"
    "constructor 'RightC' is not declared by type 'Left'"
    result
  assertLeftDiagnosticMetadata
    "wrong-owner grouped constructor metadata"
    (Just (SourceRangeIn "src/Lib/Types.jz" 1 30 1 36))
    Nothing
    (Just "RightC")
    result
  where
    sources = Map.singleton "src/Lib/Types.jz" "module Lib::Types (type Left(RightC)) { data Left = LeftC. data Right = RightC. }"
    lookupSource path = pure (Map.lookup path sources)

testGroupedTypeExportsRejectImportedConstructor :: IO ()
testGroupedTypeExportsRejectImportedConstructor = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Wrapper"]
  assertLeftDiagnosticCodeAndContains
    "imported grouped constructor"
    "E4015"
    "constructor 'Origin' is not declared by type 'Local'"
    result
  assertLeftDiagnosticMetadata
    "imported grouped constructor metadata"
    (Just (SourceRangeIn "src/Lib/Wrapper.jz" 1 33 1 39))
    Nothing
    (Just "Origin")
    result
  where
    sources =
      Map.fromList
        [ ("src/Lib/Wrapper.jz", "module Lib::Wrapper (type Local(Origin)) { import Lib::Origin. data Local = Local. }"),
          ("src/Lib/Origin.jz", "module Lib::Origin { data Origin = Origin. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testExplicitExportsKeepPrivateLocalsUsable :: IO ()
testExplicitExportsKeepPrivateLocalsUsable = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertRight "private local remains resolvable" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Value (answer).
            answer.
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value (answer) {
            helper = 1.
            answer = helper.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsUnknownModuleExport :: IO ()
testRejectsUnknownModuleExport = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Value"]
  assertLeftDiagnosticCodeAndContains
    "unknown module export"
    "E4015"
    "module export 'missing' is not declared by module 'Lib::Value'"
    result
  assertLeftDiagnosticMetadata
    "unknown module export metadata"
    (Just (SourceRangeIn "src/Lib/Value.jz" 1 1 1 7))
    Nothing
    (Just "missing")
    result
  where
    sources =
      Map.singleton
        "src/Lib/Value.jz"
        """
        module Lib::Value (missing) {
        answer = 1.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testRejectsImportedOnlyModuleExport :: IO ()
testRejectsImportedOnlyModuleExport = do
  result <- resolveTestProgram testResolverConfig lookupSource ["Lib", "Wrapper"]
  assertLeftDiagnosticCodeAndContains
    "imported-only module export"
    "E4015"
    "module export 'answer' is not declared by module 'Lib::Wrapper'"
    result
  where
    sources =
      Map.fromList
        [ ( "src/Lib/Wrapper.jz",
            """
            module Lib::Wrapper (answer) {
            import Lib::Origin (answer).
            wrapper = answer.
            }
            """
          ),
          ( "src/Lib/Origin.jz",
            """
            module Lib::Origin {
            answer = 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testExplicitImportRejectsPrivateModuleBinding :: IO ()
testExplicitImportRejectsPrivateModuleBinding = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "private explicit import"
    "E4007"
    "import symbol 'helper' is not exported by module 'Lib::Value'"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Value (helper).
            helper.
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value (answer) {
            helper = 1.
            answer = helper.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

sharedCycleSourceFiles :: Map.Map FilePath Text
sharedCycleSourceFiles =
  Map.fromList
    [ ( "src/A/One.jz",
        """
        import B::Two.
        a = 1.
        """
      ),
      ( "src/B/Two.jz",
        """
        import A::One.
        b = 2.
        """
      )
    ]

testRejectsEmptyEntryModulePath :: IO ()
testRejectsEmptyEntryModulePath =
  assertTestModulesLeftDiagnostic
    "empty entry path"
    "E4016"
    "empty entry module path"
    (resolveTestModuleGraph config sourceFiles [])
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            util.
            """
          ),
          ("src/Lib/Util.jz", "util = 1.")
        ]

testModulePathMapping :: IO ()
testModulePathMapping =
  assertEqual
    "relative file path"
    "App/Core.jz"
    (modulePathRelativeFile ".jz" (mkModulePath (mkIdentifier "App" :| [mkIdentifier "Core"])))

testNestedModulePathMapping :: IO ()
testNestedModulePathMapping = do
  assertEqual
    "nested relative file path"
    "App/Core/Parser.jz"
    (modulePathRelativeFile ".jz" (mkModulePath (mkIdentifier "App" :| [mkIdentifier "Core", mkIdentifier "Parser"])))
  assertEqual
    "punctuated relative file path"
    "Lib/Build!.jz"
    (modulePathRelativeFile ".jz" (mkModulePath (mkIdentifier "Lib" :| [mkIdentifier "Build!"])))

testParseNominalModulePath :: IO ()
testParseNominalModulePath =
  assertEqual
    "parsed path segments and rendering"
    (Right ("Foo" :| ["Bar"], "Foo::Bar"))
    (fmap (\modulePath -> (modulePathTextSegments modulePath, renderModulePath modulePath)) (parseModulePathText "Foo::Bar"))

testRejectsInvalidModulePathText :: IO ()
testRejectsInvalidModulePathText =
  mapM_
    ( \modulePath ->
        case parseModulePathText modulePath of
          Left diagnostic ->
            assertEqual
              ("invalid path " <> modulePath)
              "E4016"
              (diagnosticCodeText (diagnosticCode diagnostic))
          Right _ -> failTest ("expected invalid module path: " <> modulePath)
    )
    ["", "::Foo", "Foo::", "Foo::not-valid"]

testParseModulePathContinuations :: IO ()
testParseModulePathContinuations =
  assertEqual
    "continuation chars"
    (Right ("App" :| ["Main'", "Build!"]))
    (modulePathTextSegments <$> parseModulePathText "App::Main'::Build!")

testPreservesExactModulePathSegments :: IO ()
testPreservesExactModulePathSegments =
  assertTestModulesRight
    "case-distinct module paths resolve independently"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            import lib::Util.
            main = upperValue.
            """
          ),
          ("src/Lib/Util.jz", "upperValue = 1."),
          ("src/lib/Util.jz", "lowerValue = 2.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["lib", "Util"],
            summarySourcePath = "src/lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"], ["lib", "Util"]]
          }
      ]

testAcceptsOmittedModuleDeclaration :: IO ()
testAcceptsOmittedModuleDeclaration =
  assertTestModulesRight
    "omitted declaration uses resolved source path"
    (resolveTestModuleGraph config sourceFiles ["App", "Nested", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Nested/Main.jz", "main = 1.")]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["App", "Nested", "Main"],
            summarySourcePath = "src/App/Nested/Main.jz",
            summaryImports = []
          }
      ]

testSourceLoadingStopsAtDependencyFailure :: IO ()
testSourceLoadingStopsAtDependencyFailure = do
  loadedPaths <- newIORef []
  let sources =
        Map.fromList
          [ ("src/App/Main.jz", "import Z::Last. import A::First. 1."),
            ("src/Z/Last.jz", "1.")
          ]
      loadSource path = do
        modifyIORef' loadedPaths (<> [path])
        pure (Map.lookup path sources)
  result <- resolveTestProgram testResolverConfig loadSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains "first lexical dependency failure" "E4001" "A::First" result
  paths <- readIORef loadedPaths
  assertEqual "later dependencies are not loaded after failure" ["src/App/Main.jz", "src/A/First.jz"] paths

testResolveDependencyGraph :: IO ()
testResolveDependencyGraph =
  assertTestModulesRight
    "resolve graph"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            main = util.
            """
          ),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"]]
          }
      ]

testRetainsCheckedImportExposureInDeclarationOrder :: IO ()
testRetainsCheckedImportExposureInDeclarationOrder =
  assertTestModulesRight
    "checked import exposure resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    ( \modules ->
        case [ resolvedModule
             | resolvedModule <- modules,
               resolvedModulePathSegments resolvedModule == ["App", "Main"]
             ] of
          [resolvedModule] ->
            assertEqual
              "checked imports preserve declaration, duplicate, and selector order"
              expectedImports
              (map resolvedImportSummary (ModuleGraph.coreModuleImports resolvedModule))
          _ -> failTest "expected exactly one resolved App::Main module"
    )
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Zulu as Zed.
            import Lib::Alpha (second, first).
            import Lib::Middle.
            import Lib::Alpha (second, first).
            main = middle.
            """
          ),
          ("src/Lib/Alpha.jz", "first = 1. second = 2."),
          ("src/Lib/Middle.jz", "middle = 3."),
          ("src/Lib/Zulu.jz", "zulu = 4.")
        ]
    expectedImports =
      [ ResolvedImportSummary
          { summaryImportSpan = SourceRangeIn "src/App/Main.jz" 1 1 1 7,
            summaryImportPath = ["Lib", "Zulu"],
            summaryImportAlias = Just "Zed",
            summaryImportExposure = QualifiedOnlySummary
          },
        ResolvedImportSummary
          { summaryImportSpan = SourceRangeIn "src/App/Main.jz" 2 1 2 7,
            summaryImportPath = ["Lib", "Alpha"],
            summaryImportAlias = Nothing,
            summaryImportExposure = OnlyUnqualifiedSummary ["second", "first"]
          },
        ResolvedImportSummary
          { summaryImportSpan = SourceRangeIn "src/App/Main.jz" 3 1 3 7,
            summaryImportPath = ["Lib", "Middle"],
            summaryImportAlias = Nothing,
            summaryImportExposure = AllUnqualifiedSummary
          },
        ResolvedImportSummary
          { summaryImportSpan = SourceRangeIn "src/App/Main.jz" 4 1 4 7,
            summaryImportPath = ["Lib", "Alpha"],
            summaryImportAlias = Nothing,
            summaryImportExposure = OnlyUnqualifiedSummary ["second", "first"]
          }
      ]

testResolveImportsInLexicalRenderedPathOrder :: IO ()
testResolveImportsInLexicalRenderedPathOrder =
  assertTestModulesRight
    "reverse source imports resolve lexically"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Zoo::Dep.
            import Alpha::Dep.
            main = alpha.
            """
          ),
          ("src/Alpha/Dep.jz", "alpha = 1."),
          ("src/Zoo/Dep.jz", "zoo = 2.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Alpha", "Dep"],
            summarySourcePath = "src/Alpha/Dep.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["Zoo", "Dep"],
            summarySourcePath = "src/Zoo/Dep.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Alpha", "Dep"], ["Zoo", "Dep"]]
          }
      ]

testCollapsesDuplicateImports :: IO ()
testCollapsesDuplicateImports =
  assertTestModulesRight
    "duplicate imports collapse"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            import Lib::Util.
            main = util.
            """
          ),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"]]
          }
      ]

testReusesAlreadyResolvedModuleAcrossBranches :: IO ()
testReusesAlreadyResolvedModuleAcrossBranches =
  assertTestModulesRight
    "shared dependency is reused"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::One.
            import B::Two.
            main = a.
            """
          ),
          ( "src/A/One.jz",
            """
            import Shared::Util.
            a = shared.
            """
          ),
          ( "src/B/Two.jz",
            """
            import Shared::Util.
            b = shared.
            """
          ),
          ("src/Shared/Util.jz", "shared = 1.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Shared", "Util"],
            summarySourcePath = "src/Shared/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["A", "One"],
            summarySourcePath = "src/A/One.jz",
            summaryImports = [["Shared", "Util"]]
          },
        ResolvedModuleSummary
          { summaryModulePath = ["B", "Two"],
            summarySourcePath = "src/B/Two.jz",
            summaryImports = [["Shared", "Util"]]
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["A", "One"], ["B", "Two"]]
          }
      ]

testAcceptsMatchingModuleDeclaration :: IO ()
testAcceptsMatchingModuleDeclaration =
  assertTestModulesRight
    "matching declaration is accepted"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Util.
            util.
            }
            """
          ),
          ( "src/Lib/Util.jz",
            """
            module Lib::Util {
            util = 1.
            }
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"]]
          }
      ]

testDeduplicatesDuplicateRoots :: IO ()
testDeduplicatesDuplicateRoots =
  assertTestModulesRight
    "duplicate roots are not treated as ambiguity"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config =
      ModuleResolutionConfig
        { moduleRoots = ["src", "src"],
          moduleExtension = ".jz"
        }
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            util.
            """
          ),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"]]
          }
      ]

testDeduplicatesEquivalentRoots :: IO ()
testDeduplicatesEquivalentRoots =
  assertTestModulesRight
    "equivalent roots are not treated as ambiguity"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config =
      ModuleResolutionConfig
        { moduleRoots = ["src", "src/."],
          moduleExtension = ".jz"
        }
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            util.
            """
          ),
          ( "src/./App/Main.jz",
            """
            import Lib::Util.
            util.
            """
          ),
          ("src/Lib/Util.jz", "util = 1."),
          ("src/./Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModuleSummary ["Lib", "Util"] "src/Lib/Util.jz" [],
        ResolvedModuleSummary ["App", "Main"] "src/App/Main.jz" [["Lib", "Util"]]
      ]

testReportsUnresolvedImport :: IO ()
testReportsUnresolvedImport = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "unresolved code" "E4001" result
  assertLeftContains "unresolved module" "Missing::Thing" result
  assertLeftContains "importer context" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Missing::Thing.
            main = 1.
            """
          )
        ]

testReportsAmbiguousImport :: IO ()
testReportsAmbiguousImport = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "ambiguous code" "E4002" result
  assertLeftContains "ambiguous first candidate" "rootA/Lib/Util.jz" result
  assertLeftContains "ambiguous second candidate" "rootB/Lib/Util.jz" result
  assertLeftContains "ambiguous candidate order" "matched rootA/Lib/Util.jz, rootB/Lib/Util.jz" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["rootA", "rootB"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "rootA/App/Main.jz",
            """
            import Lib::Util.
            main = util.
            """
          ),
          ("rootA/Lib/Util.jz", "util = 1."),
          ("rootB/Lib/Util.jz", "util = 2.")
        ]

testReportsCycle :: IO ()
testReportsCycle = do
  result <- resolveTestModuleGraph config sourceFiles ["A", "One"]
  assertLeftContains "cycle code" "E4003" result
  assertLeftContains "cycle trace" "A::One -> B::Two -> A::One" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles = sharedCycleSourceFiles

testReportsNestedCycleMinimalTrace :: IO ()
testReportsNestedCycleMinimalTrace = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "nested cycle code" "E4003" result
  assertLeftContains "nested cycle trace" "A::One -> B::Two -> A::One" result
  assertLeftDiagnosticNotContains "nested cycle excludes entry" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.insert
        "src/App/Main.jz"
        """
        import A::One.
        main = a.
        """
        sharedCycleSourceFiles

testReportsImportedModuleParseFailure :: IO ()
testReportsImportedModuleParseFailure = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "parse failure code" "E4004" result
  assertLeftContains "parse failure path" "src/Lib/Util.jz" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util.
            main = util.
            """
          ),
          ("src/Lib/Util.jz", "broken = .")
        ]

testModuleLexerFailureRetainsStructuredDetail :: IO ()
testModuleLexerFailureRetainsStructuredDetail = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  case result of
    Left diagnostic -> do
      assertEqual "module lexer failure code" "E4004" (diagnosticCodeText (diagnosticCode diagnostic))
      assertEqual
        "module lexer failure qualified primary span"
        (Just (SourceSpanIn "src/Lib/Bad.jz" 1 10))
        (diagnosticPrimarySpan diagnostic)
      assertLeftContains "module lexer original detail" "unterminated text literal" result
    Right modules -> failTest ("expected module lexer failure, got " <> Text.pack (show modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Bad.
            main = 1.
            """
          ),
          ("src/Lib/Bad.jz", "broken = \"unterminated")
        ]

testImplMethodRejectsHiddenUnqualifiedReference :: IO ()
testImplMethodRejectsHiddenUnqualifiedReference = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "implementation method hidden unqualified reference"
    "E4011"
    "helper"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Value (answer).
            class Use(a) { use :: a -> Int. }.
            impl Use(Int) { use = \\(item) -> helper. }.
            main = answer.
            """
          ),
          ("src/Lib/Value.jz", "module Lib::Value { helper = 41. answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testImplMethodRejectsHiddenQualifiedReference :: IO ()
testImplMethodRejectsHiddenQualifiedReference = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "implementation method hidden qualified reference"
    "E4014"
    "helper"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Value as Value.
            class Use(a) { use :: a -> Int. }.
            impl Use(Int) { use = \\(item) -> Value::helper. }.
            main = 1.
            """
          ),
          ("src/Lib/Value.jz", "module Lib::Value (answer) { helper = 41. answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsModuleDeclarationMismatch :: IO ()
testReportsModuleDeclarationMismatch = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "mismatch code" "E4006" result
  assertLeftContains "declared module name" "Wrong::Name" result
  assertLeftContains "expected module name" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module Wrong::Name {
            main = 1.
            }
            """
          )
        ]

testReportsNestedModuleDeclarationParseFailure :: IO ()
testReportsNestedModuleDeclarationParseFailure = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "nested module parse failure code" "E4004" result
  assertLeftContains "nested module parse failure path" "src/App/Main.jz" result
  assertLeftContains "nested module parse failure text" "top-level" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            module App::Main {
            main = 1.
            }
            }
            """
          )
        ]

testAcceptsValidImportSymbolList :: IO ()
testAcceptsValidImportSymbolList =
  assertTestModulesRight
    "valid import symbol list resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math (add).
            main = add.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            sub = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testAcceptsDataConstructorImportSymbolList :: IO ()
testAcceptsDataConstructorImportSymbolList =
  assertTestModulesRight
    "data constructor import symbol list resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Maybe (Just).
            main = Just 1.
            """
          ),
          ("src/Lib/Maybe.jz", "data Maybe a = Just a | Nothing.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Maybe"],
            summarySourcePath = "src/Lib/Maybe.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Maybe"]]
          }
      ]

testAcceptsTypeApplicationsWhileCollectingModuleReferences :: IO ()
testAcceptsTypeApplicationsWhileCollectingModuleReferences =
  assertTestModulesRight
    "type applications in module reference collection"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Util as Util.
            main = Util::id @Int 1.
            """
          ),
          ( "src/Lib/Util.jz",
            """
            id = \\(item) -> item.
            result = id @Int 1.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Util"],
            summarySourcePath = "src/Lib/Util.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Util"]]
          }
      ]

testAcceptsBareImportUnqualifiedExport :: IO ()
testAcceptsBareImportUnqualifiedExport =
  assertTestModulesRight
    "bare import makes exports visible"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math.
            main = subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testAcceptsLocalBindingOverHiddenExplicitImport :: IO ()
testAcceptsLocalBindingOverHiddenExplicitImport =
  assertTestModulesRight
    "local binding shadows hidden import export"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math (add).
            subtract = 0.
            main = subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testReportsMissingImportSymbol :: IO ()
testReportsMissingImportSymbol = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "missing symbol code" "E4007" result
  assertLeftContains "missing symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "missing symbol metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math (subtract).
            main = 1.
            """
          ),
          ("src/Lib/Math.jz", "add = 1.")
        ]

testReportsHiddenExplicitImportValueReference :: IO ()
testReportsHiddenExplicitImportValueReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "explicit hidden value code" "E4011" result
  assertLeftContains "hidden value text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "explicit hidden value metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math (add).
            main = subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]

testReportsImportSymbolCollision :: IO ()
testReportsImportSymbolCollision = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "symbol collision code" "E4008" result
  assertLeftContains "symbol collision text" "symbol 'map'" result
  assertLeftContains "first module context" "A::Ops" result
  assertLeftContains "second module context" "B::Ops" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "symbol collision metadata"
    (Just (SourceRange 2 1 2 7))
    (Just (SourceRange 1 1 1 7))
    (Just "map")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Ops (map).
            import B::Ops (map).
            main = map.
            """
          ),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsBareImportSymbolCollision :: IO ()
testReportsBareImportSymbolCollision = do
  assertCollision
    "A then B"
    """
    import A::Ops.
    import B::Ops.
    main = map.
    """
  assertCollision
    "B then A"
    """
    import B::Ops.
    import A::Ops.
    main = map.
    """
  where
    assertCollision label importerSource = do
      result <- resolveTestModuleGraph config (sourceFiles importerSource) ["App", "Main"]
      assertLeftContains (label <> " collision code") "E4008" result
      assertLeftContains (label <> " collision symbol") "symbol 'map'" result
      assertLeftDiagnosticMetadata
        (label <> " collision metadata")
        (Just (SourceRange 2 1 2 7))
        (Just (SourceRange 1 1 1 7))
        (Just "map")
        result

    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles importerSource =
      Map.fromList
        [ ("src/App/Main.jz", importerSource),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsMixedImportSymbolCollision :: IO ()
testReportsMixedImportSymbolCollision = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "mixed collision code" "E4008" result
  assertLeftContains "mixed collision symbol" "symbol 'map'" result
  assertLeftDiagnosticMetadata
    "mixed collision metadata"
    (Just (SourceRange 2 1 2 7))
    (Just (SourceRange 1 1 1 7))
    (Just "map")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Ops.
            import B::Ops (map).
            main = map.
            """
          ),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsImportAliasCollision :: IO ()
testReportsImportAliasCollision = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias collision code" "E4009" result
  assertLeftContains "alias collision text" "alias collision" result
  assertLeftContains "first module context" "A::Ops" result
  assertLeftContains "second module context" "B::Ops" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias collision metadata"
    (Just (SourceRange 2 1 2 7))
    (Just (SourceRange 1 1 1 7))
    (Just "Ops")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Ops as Ops.
            import B::Ops as Ops.
            main = 1.
            """
          ),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsHiddenExplicitImportConstructorPatternReference :: IO ()
testReportsHiddenExplicitImportConstructorPatternReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "explicit hidden constructor code" "E4011" result
  assertLeftContains "hidden constructor text" "Just" result
  assertLeftContains "imported module context" "Lib::Maybe" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "explicit hidden constructor metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "Just")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Maybe (Nothing).
            main = case Nothing { | Just item -> item | _ -> 0 }.
            """
          ),
          ("src/Lib/Maybe.jz", "data Maybe a = Just a | Nothing.")
        ]

testReportsUnqualifiedAliasImportReference :: IO ()
testReportsUnqualifiedAliasImportReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias visibility code" "E4012" result
  assertLeftContains "hidden symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "import alias context" "Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias visibility metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math as Math.
            main = subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]

testReportsHiddenAliasImportConstructorPatternReference :: IO ()
testReportsHiddenAliasImportConstructorPatternReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias hidden constructor code" "E4012" result
  assertLeftContains "hidden constructor text" "Just" result
  assertLeftContains "imported module context" "Lib::Maybe" result
  assertLeftContains "import alias context" "Maybe" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias hidden constructor metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "Just")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Maybe as Maybe.
            main = case Maybe::Nothing { | Just item -> item | _ -> 0 }.
            """
          ),
          ("src/Lib/Maybe.jz", "data Maybe a = Just a | Nothing.")
        ]

testAcceptsQualifiedAliasReferenceBeforeImport :: IO ()
testAcceptsQualifiedAliasReferenceBeforeImport =
  assertTestModulesRight
    "qualified alias reference before import resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            main = Math::subtract.
            import Lib::Math as Math.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testAcceptsLocalBindingSharingAliasName :: IO ()
testAcceptsLocalBindingSharingAliasName =
  assertTestModulesRight
    "local binding does not shadow qualified alias"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math as math.
            math = 0.
            main = math::subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testAcceptsQualifiedAliasImportReference :: IO ()
testAcceptsQualifiedAliasImportReference =
  assertTestModulesRight
    "qualified alias import reference resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math as Math.
            main = Math::subtract.
            """
          ),
          ( "src/Lib/Math.jz",
            """
            add = 1.
            subtract = 2.
            """
          )
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Math"],
            summarySourcePath = "src/Lib/Math.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Math"]]
          }
      ]

testAcceptsQualifiedAliasDataConstructorReference :: IO ()
testAcceptsQualifiedAliasDataConstructorReference =
  assertTestModulesRight
    "qualified alias data constructor reference resolves"
    (resolveTestModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules (map resolvedModuleSummary modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Maybe as Maybe.
            main = Maybe::Just 1.
            """
          ),
          ("src/Lib/Maybe.jz", "data Maybe a = Just a | Nothing.")
        ]
    expectedModules =
      [ ResolvedModuleSummary
          { summaryModulePath = ["Lib", "Maybe"],
            summarySourcePath = "src/Lib/Maybe.jz",
            summaryImports = []
          },
        ResolvedModuleSummary
          { summaryModulePath = ["App", "Main"],
            summarySourcePath = "src/App/Main.jz",
            summaryImports = [["Lib", "Maybe"]]
          }
      ]

testReportsUnknownQualifiedAliasReference :: IO ()
testReportsUnknownQualifiedAliasReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "unknown alias code" "E4013" result
  assertLeftContains "unknown alias text" "Math" result
  assertLeftContains "referenced symbol text" "subtract" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "unknown alias metadata"
    Nothing
    Nothing
    (Just "Math")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "main = Math::subtract.")]

testReportsStandaloneUnknownQualifiedAliasReference :: IO ()
testReportsStandaloneUnknownQualifiedAliasReference = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "standalone unknown alias code" "E4013" result
  assertLeftContains "standalone unknown alias text" "Math" result
  assertLeftContains "standalone referenced symbol text" "subtract" result
  assertLeftContains "standalone importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "standalone unknown alias metadata"
    Nothing
    Nothing
    (Just "Math")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "Math::subtract.")]

testReportsMissingQualifiedAliasExport :: IO ()
testReportsMissingQualifiedAliasExport = do
  result <- resolveTestModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "missing qualified alias code" "E4014" result
  assertLeftContains "missing symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "alias context" "Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "missing qualified alias metadata"
    (Just (SourceRange 1 1 1 7))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Math as Math.
            main = Math::subtract.
            """
          ),
          ("src/Lib/Math.jz", "add = 1.")
        ]

testResolverConfig :: ModuleResolutionConfig
testResolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

testAcceptsExplicitClassImportSymbol :: IO ()
testAcceptsExplicitClassImportSymbol = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertRight "explicit class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Facts (Equatable).
            x :: @{Equatable(Int)}: Int.
            x = 1.
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            class Equatable(a) { }.
            impl Equatable(Int) { }.
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsTypeOnlyImportSymbol :: IO ()
testRejectsTypeOnlyImportSymbol = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "type-only import"
    "E4007"
    "import symbol 'Optional' is not exported"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Types (Optional).
            x = 1.
            """
          ),
          ("src/Lib/Types.jz", "data Optional a = Some a | None.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsClassImportCollision :: IO ()
testReportsClassImportCollision = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "class import collision"
    "E4008"
    "import binding collision for symbol 'Equatable'"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Facts.
            import B::Facts.
            x = 1.
            """
          ),
          ("src/A/Facts.jz", "class Equatable(a) { }."),
          ("src/B/Facts.jz", "class Equatable(a) { }.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsTypeImportCollision :: IO ()
testReportsTypeImportCollision = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "type import collision"
    "E4008"
    "import type collision for 'Box'"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Types.
            import B::Types.
            result :: Box(Int).
            result = ABox 1.
            """
          ),
          ("src/A/Types.jz", "data Box a = ABox a."),
          ("src/B/Types.jz", "data Box a = BBox a.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testKeepsRepeatedClassImportsIdempotent :: IO ()
testKeepsRepeatedClassImportsIdempotent = do
  result <- resolveTestProgram testResolverConfig lookupSource ["App", "Main"]
  assertRight "repeated class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Facts.
            import Lib::Facts.
            x :: @{Equatable(Int)}: Int.
            x = 1.
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            class Equatable(a) { }.
            impl Equatable(Int) { }.
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

assertLeftDiagnosticMetadata ::
  (Show a) =>
  Text ->
  Maybe SourceSpan ->
  Maybe SourceSpan ->
  Maybe Text ->
  Either Diagnostic a ->
  IO ()
assertLeftDiagnosticMetadata label expectedPrimary expectedRelated expectedSubject value =
  case value of
    Left diagnostic -> do
      assertEqual (label <> " primary span") expectedPrimary (diagnosticPrimarySpan diagnostic)
      assertEqual (label <> " related span") expectedRelated (diagnosticRelatedSpan diagnostic)
      assertEqual (label <> " subject") expectedSubject (diagnosticSubject diagnostic)
    Right ok ->
      failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

assertLeftDiagnosticNotContains ::
  (Show a) =>
  Text ->
  Text ->
  Either Diagnostic a ->
  IO ()
assertLeftDiagnosticNotContains label needle value =
  case value of
    Left diagnostic ->
      let rendered = renderDiagnostic diagnostic
       in if needle `Text.isInfixOf` rendered
            then failTest (label <> ": expected not to find '" <> needle <> "' in '" <> rendered <> "'")
            else pure ()
    Right ok ->
      failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))
