{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    ResolvedPrelude (..),
    RunResult (..),
    compileModuleGraph,
    compileModuleGraphWithResolvedPrelude,
    compileModuleGraphWithPrelude,
    runModuleGraph,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "Loader" tests

tests :: [NamedTest]
tests =
  [ ("compile module graph succeeds for resolvable entry module", testCompileModuleGraphSuccess),
    ("run module graph produces runtime output from entry module", testRunModuleGraphSuccess),
    ("compile module graph default helper loads bundled prelude", testCompileModuleGraphDefaultLoadsBundledPrelude),
    ("compile module graph default helper exposes bundled capability facts in modules", testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules),
    ("run module graph default helper executes bundled prelude aliases across files", testRunModuleGraphDefaultLoadsBundledPrelude),
    ("compile module graph without prelude rejects public aliases across files", testCompileModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles),
    ("run module graph without prelude rejects public aliases across files", testRunModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles),
    ("compile module graph without prelude keeps kernel bridge aliases across files", testCompileModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles),
    ("run module graph without prelude executes kernel bridge aliases across files", testRunModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles),
    ("run module graph explicit prelude exposes public helpers across files", testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles),
    ("run module graph ignores dependency expression statements", testRunModuleGraphIgnoresDependencyExpressions),
    ("compile module graph validates dependency expression statements", testCompileModuleGraphValidatesDependencyExpressions),
    ("run module graph validates dependency expression statements before runtime", testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime),
    ("compile module graph validates hidden dependency exports", testCompileModuleGraphValidatesHiddenDependencyExports),
    ("compile module graph rewrites hidden constructor dependency expressions", testCompileModuleGraphRewritesHiddenConstructorDependencyExpressions),
    ("compile module graph reports unresolved import diagnostics", testCompileModuleGraphUnresolved),
    ("compile module graph reports ambiguous import diagnostics", testCompileModuleGraphAmbiguousImport),
    ("compile module graph reports module source parse diagnostics", testCompileModuleGraphParseFailure),
    ("compile module graph reports missing import symbols", testCompileModuleGraphMissingImportSymbol),
    ("compile module graph hides dependency bindings excluded by explicit import list", testCompileModuleGraphExplicitImportListHidesUnlistedBindings),
    ("compile module graph hides capability facts excluded by explicit import list", testCompileModuleGraphExplicitImportListHidesCapabilityFacts),
    ("compile module graph keeps hidden constructor dependencies for validation", testCompileModuleGraphKeepsHiddenConstructorValidationDependencies),
    ("compile module graph allows explicit-import hidden name supplied by prelude", testCompileModuleGraphExplicitImportAllowsPreludeBinding),
    ("compile module graph hides dependency bindings imported only by alias", testCompileModuleGraphAliasImportHidesUnqualifiedBindings),
    ("compile module graph allows alias-hidden name supplied by prelude", testCompileModuleGraphAliasImportAllowsPreludeBinding),
    ("run module graph rewrites visible export hidden constructor dependencies", testRunModuleGraphVisibleExportRewritesHiddenConstructorDependency),
    ("run module graph keeps explicit-import hidden dependency export from shadowing prelude", testRunModuleGraphExplicitImportHiddenExportUsesPrelude),
    ("run module graph keeps alias-hidden dependency export from shadowing prelude", testRunModuleGraphAliasImportHiddenExportUsesPrelude),
    ("run module graph keeps alias-hidden data constructor from shadowing prelude", testRunModuleGraphAliasHiddenDataConstructorUsesPrelude),
    ("run module graph resolves qualified alias data constructor lookup", testRunModuleGraphQualifiedAliasDataConstructorLookup),
    ("compile module graph preserves alias-qualified generic constructor schemes", testCompileModuleGraphPreservesAliasQualifiedGenericConstructorSchemes),
    ("compile module graph keeps alias-qualified ADT equality distinct from local ADT", testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct),
    ("compile module graph resolves alias-qualified impl method references", testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences),
    ("compile module graph rewrites hidden impl method references", testCompileModuleGraphRewritesHiddenImplMethodReferences),
    ("compile module graph keeps replayed ADT impl facts distinct", testCompileModuleGraphKeepsReplayedAdtImplFactsDistinct),
    ("run module graph keeps local data constructor from hidden import rewrite", testRunModuleGraphLocalDataConstructorShadowsHiddenImportRewrite),
    ("run module graph preserves alias-qualified float literal targets", testRunModuleGraphPreservesAliasQualifiedFloatLiteralTargets),
    ("run module graph keeps hidden qualified export pattern constructors available", testRunModuleGraphHiddenQualifiedPatternExportKeepsConstructorBridge),
    ("run module graph keeps alias-qualified dependency export visible with prelude", testRunModuleGraphAliasQualifiedExportUsesDependencyWithPrelude),
    ("run module graph keeps transitive alias-hidden dependency export from shadowing prelude", testRunModuleGraphTransitiveAliasHiddenExportUsesPrelude),
    ("compile module graph hides transitive alias-only exports from unqualified replay", testCompileModuleGraphTransitiveAliasImportHidesUnqualifiedExport),
    ("run module graph keeps alias-hidden prelude binding isolated from visible importer", testRunModuleGraphAliasHiddenExportUsesPreludeDespiteVisibleImporter),
    ("run module graph keeps visible sibling import isolated from alias-hidden replay", testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenReplay),
    ("compile module graph keeps sibling capability facts isolated", testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated),
    ("compile module graph exposes capability facts through visible imports", testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports),
    ("run module graph keeps hidden qualified export dependencies available", testRunModuleGraphHiddenQualifiedExportKeepsDependencyBridge),
    ("run module graph resolves qualified alias lookup", testRunModuleGraphQualifiedAliasLookup),
    ("run module graph resolves qualified alias lookup through dependency export", testRunModuleGraphQualifiedAliasLookupUsesDependencyExport),
    ("compile module graph accepts qualified alias use before import", testCompileModuleGraphQualifiedAliasLookupBeforeImport),
    ("run module graph allows bundled class-qualified method lookup", testRunModuleGraphAllowsBundledClassQualifiedMethodLookup),
    ("run module graph allows imported class-qualified method lookup", testRunModuleGraphAllowsImportedClassQualifiedMethodLookup),
    ("run module graph allows aliased imported class-qualified method lookup", testRunModuleGraphAllowsAliasedImportedClassQualifiedMethodLookup),
    ("run module graph allows imported pre-module class-qualified method lookup", testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup),
    ("run module graph keeps hidden impls out of runtime dispatch", testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch),
    ("run module graph retains local capabilities needed by exported bindings", testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings),
    ("run module graph retains local capabilities needed by imported capability bodies", testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies),
    ("run module graph retains local capabilities needed by imported signatures", testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures),
    ("compile module graph reports module declaration mismatch diagnostics", testCompileModuleGraphModuleDeclarationMismatch),
    ("run module graph reports cycle diagnostics", testRunModuleGraphCycle),
    ("loader reuses memoized source lookup across resolve and replay", testMemoizedLookupReuse)
  ]

testCompileModuleGraphSuccess :: IO ()
testCompileModuleGraphSuccess = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphSuccess :: IO ()
testRunModuleGraphSuccess = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphDefaultLoadsBundledPrelude :: IO ()
testCompileModuleGraphDefaultLoadsBundledPrelude = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Data.\nmap hd values.\n}"),
          ("src/Lib/Data.jz", "module Lib::Data {\nvalues = [[1, 2], [3], [4, 5]].\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules :: IO ()
testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nx :: @{Eq(Int)}: Int.\nx = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphDefaultLoadsBundledPrelude :: IO ()
testRunModuleGraphDefaultLoadsBundledPrelude = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3, 4]") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Data.\nmap hd values.\n}"),
          ("src/Lib/Data.jz", "module Lib::Data {\nvalues = [[1, 2], [3], [4, 5]].\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles :: IO ()
testCompileModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual
    "public aliases are unavailable across module graph without prelude"
    ["E1001: unbound variable 'map'", "E1001: unbound variable 'hd'"]
    (map renderDiagnostic (compileErrors result))
  where
    sourceMap = moduleGraphProjectedSources "map hd values"
    lookupSource = lookupSourceIn sourceMap

testRunModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles :: IO ()
testRunModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual
    "run-mode public aliases are unavailable across module graph without prelude"
    ["E1001: unbound variable 'map'", "E1001: unbound variable 'hd'"]
    (map renderDiagnostic (runCompileErrors result))
  assertEqual "runtime errors stay empty on compile failure" [] (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on compile failure" Nothing (runOutput result)
  where
    sourceMap = moduleGraphProjectedSources "map hd values"
    lookupSource = lookupSourceIn sourceMap

testCompileModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles :: IO ()
testCompileModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap = moduleGraphProjectedSources "__kernel_map __kernel_hd values"
    lookupSource = lookupSourceIn sourceMap

testRunModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles :: IO ()
testRunModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3]") (runOutput result)
  where
    sourceMap = moduleGraphProjectedSources "__kernel_map __kernel_hd values"
    lookupSource = lookupSourceIn sourceMap

testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles :: IO ()
testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "__kernel_map = __kernel_map.\n__kernel_hd = __kernel_hd.\nmap = __kernel_map.\nhd = __kernel_hd.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3]") (runOutput result)
  where
    sourceMap = moduleGraphProjectedSources "map hd values"
    lookupSource = lookupSourceIn sourceMap

testRunModuleGraphIgnoresDependencyExpressions :: IO ()
testRunModuleGraphIgnoresDependencyExpressions = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n1 / 0.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphValidatesDependencyExpressions :: IO ()
testCompileModuleGraphValidatesDependencyExpressions = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] ->
      assertContains
        "signature adjacency"
        "must be immediately followed by a matching binding"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one dependency signature adjacency error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil :: Int.\nTrue.\nutil = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime :: IO ()
testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output is suppressed" Nothing (runOutput result)
  case runCompileErrors result of
    [err] ->
      assertContains
        "dependency validation error"
        "must be immediately followed by a matching binding"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one dependency validation compile error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil :: Int.\nTrue.\nutil = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphValidatesHiddenDependencyExports :: IO ()
testCompileModuleGraphValidatesHiddenDependencyExports = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] ->
      assertContains "hidden export unbound" "unbound variable 'missingName'" (renderDiagnostic err)
    _ -> failTest "expected exactly one hidden dependency export validation error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nadd."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = missingName.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRewritesHiddenConstructorDependencyExpressions :: IO ()
testCompileModuleGraphRewritesHiddenConstructorDependencyExpressions = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe (x).\nx."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value.\nx = 1.\nJust 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphUnresolved :: IO ()
testCompileModuleGraphUnresolved = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      assertContains "unresolved code" "E4001" (renderDiagnostic err)
      assertContains "missing module" "Missing::Thing" (renderDiagnostic err)
    _ -> failTest "expected exactly one unresolved import error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "import Missing::Thing.\n1.")]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphAmbiguousImport :: IO ()
testCompileModuleGraphAmbiguousImport = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      ambiguousResolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "ambiguous code" "E4002" rendered
      assertContains "first candidate" "rootA/Lib/Util.jz" rendered
      assertContains "second candidate" "rootB/Lib/Util.jz" rendered
      assertContains "importer context" "App::Main" rendered
    _ -> failTest "expected exactly one ambiguous import error"
  where
    ambiguousResolverConfig =
      ModuleResolutionConfig
        { moduleRoots = ["rootA", "rootB"],
          moduleExtension = ".jz"
        }
    sourceMap =
      Map.fromList
        [ ("rootA/App/Main.jz", "import Lib::Util.\nutil."),
          ("rootA/Lib/Util.jz", "util = 1."),
          ("rootB/Lib/Util.jz", "util = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphParseFailure :: IO ()
testCompileModuleGraphParseFailure = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "module parse code" "E4004" rendered
      assertContains "module parse path" "src/App/Main.jz" rendered
      assertContains "fail-fast module syntax" "expected '{'" rendered
    _ -> failTest "expected exactly one module parse error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "module App::Main.")]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphMissingImportSymbol :: IO ()
testCompileModuleGraphMissingImportSymbol = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "missing symbol code" "E4007" rendered
      assertContains "missing symbol text" "subtract" rendered
      assertContains "imported module context" "Lib::Math" rendered
      assertContains "importer context" "App::Main" rendered
    _ -> failTest "expected exactly one missing import symbol error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (subtract).\n1."),
          ("src/Lib/Math.jz", "add = 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExplicitImportListHidesUnlistedBindings :: IO ()
testCompileModuleGraphExplicitImportListHidesUnlistedBindings = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "hidden import code" "E4011" rendered
      assertContains "hidden symbol" "subtract" rendered
      assertContains "imported module context" "Lib::Math" rendered
      assertContains "importer context" "App::Main" rendered
    _ -> failTest "expected exactly one hidden import symbol error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExplicitImportListHidesCapabilityFacts :: IO ()
testCompileModuleGraphExplicitImportListHidesCapabilityFacts = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "explicit import capability fact isolation"
        "missing class declaration 'Hidden'"
        (renderDiagnostic err)
    errors ->
      failTest
        ( "expected exactly one hidden capability fact error, got "
            <> Text.pack (show (map renderDiagnostic errors))
        )
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Facts (facts).\nuse :: @{Hidden(Int)}: Int.\nuse = 1."),
          ("src/Lib/Facts.jz", "facts = 0.\nclass Hidden(a) { }.\nimpl Hidden(Int) { }.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsHiddenConstructorValidationDependencies :: IO ()
testCompileModuleGraphKeepsHiddenConstructorValidationDependencies = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\n1."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value.\nx = Just 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExplicitImportAllowsPreludeBinding :: IO ()
testCompileModuleGraphExplicitImportAllowsPreludeBinding = do
  result <-
    compileModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphAliasImportHidesUnqualifiedBindings :: IO ()
testCompileModuleGraphAliasImportHidesUnqualifiedBindings = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "alias visibility code" "E4012" rendered
      assertContains "hidden symbol" "subtract" rendered
      assertContains "imported module context" "Lib::Math" rendered
      assertContains "alias context" "Math" rendered
      assertContains "importer context" "App::Main" rendered
    _ -> failTest "expected exactly one alias visibility error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphAliasImportAllowsPreludeBinding :: IO ()
testCompileModuleGraphAliasImportAllowsPreludeBinding = do
  result <-
    compileModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphVisibleExportRewritesHiddenConstructorDependency :: IO ()
testRunModuleGraphVisibleExportRewritesHiddenConstructorDependency = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "Just(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe (x).\nx."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value.\nx = Just 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphExplicitImportHiddenExportUsesPrelude :: IO ()
testRunModuleGraphExplicitImportHiddenExportUsesPrelude = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "99") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAliasImportHiddenExportUsesPrelude :: IO ()
testRunModuleGraphAliasImportHiddenExportUsesPrelude = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "99") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nsubtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAliasHiddenDataConstructorUsesPrelude :: IO ()
testRunModuleGraphAliasHiddenDataConstructorUsesPrelude = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "Nothing = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "99") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\nNothing."),
          ("src/Lib/Maybe.jz", "data Maybe = Nothing.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphQualifiedAliasDataConstructorLookup :: IO ()
testRunModuleGraphQualifiedAliasDataConstructorLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "Just(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\nMaybe::Just 1."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphPreservesAliasQualifiedGenericConstructorSchemes :: IO ()
testCompileModuleGraphPreservesAliasQualifiedGenericConstructorSchemes = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Box as Box.\nfirst = Box::Box 1.\nsecond = Box::Box True.\nsecond."),
          ("src/Lib/Box.jz", "data Box a = Box a.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct :: IO ()
testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "alias-qualified ADT equality mismatch"
        "E2004"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one alias-qualified ADT equality mismatch"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Box as L.\ndata Box a = Box a.\nleft = L::Box 1.\nright = Box 1.\nsame = left == right."),
          ("src/Lib/Box.jz", "data Box a = Box a.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences :: IO ()
testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nclass Sample(a) {\nmethod :: Int.\n}.\nimpl Sample(Int) {\nmethod = Math::one.\n}.\nx = 1."),
          ("src/Lib/Math.jz", "one = 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRewritesHiddenImplMethodReferences :: IO ()
testCompileModuleGraphRewritesHiddenImplMethodReferences = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Thing as Thing.\nx = 0."),
          ("src/Lib/Thing.jz", "helper = 1.\nclass Sample(a) {\nmethod :: Int.\n}.\nimpl Sample(Int) {\nmethod = helper.\n}.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsReplayedAdtImplFactsDistinct :: IO ()
testCompileModuleGraphKeepsReplayedAdtImplFactsDistinct = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "replayed ADT impl fact isolation"
        "missing impl fact 'Eq(Box(Int))'"
        (renderDiagnostic err)
    errors ->
      failTest
        ( "expected exactly one missing local ADT impl fact, got "
            <> Text.pack (show (map renderDiagnostic errors))
        )
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Box.\ndata Box a = Box a.\nclass Eq(a) { }.\nuse :: @{Eq(Box(Int))}: Int.\nuse = 1."),
          ("src/Lib/Box.jz", "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphLocalDataConstructorShadowsHiddenImportRewrite :: IO ()
testRunModuleGraphLocalDataConstructorShadowsHiddenImportRewrite = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "Just(1, 2)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMaybe.\nimport Lib::Maybe (Just).\ndata Pair = Just left right.\nJust 1 2."),
          ("src/App/UsesMaybe.jz", "import Lib::Maybe as Maybe.\nuse = 0."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphPreservesAliasQualifiedFloatLiteralTargets :: IO ()
testRunModuleGraphPreservesAliasQualifiedFloatLiteralTargets = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Floats as Floats.\n(Floats::x16, Floats::x32)."),
          ("src/Lib/Floats.jz", "x16 :: Float16.\nx16 = 2049.0.\nx32 :: Float32.\nx32 = 1.00000001.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphHiddenQualifiedPatternExportKeepsConstructorBridge :: IO ()
testRunModuleGraphHiddenQualifiedPatternExportKeepsConstructorBridge = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "7") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\nMaybe::fromDefault."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.\ndefault = Just 7.\nfromDefault = case default { | Just value -> value | Nothing -> 0 }.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAliasQualifiedExportUsesDependencyWithPrelude :: IO ()
testRunModuleGraphAliasQualifiedExportUsesDependencyWithPrelude = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nMath::subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphTransitiveAliasHiddenExportUsesPrelude :: IO ()
testRunModuleGraphTransitiveAliasHiddenExportUsesPrelude = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "99") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nuse."),
          ("src/App/UsesMath.jz", "import Lib::Math as Math.\nuse = subtract."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphTransitiveAliasImportHidesUnqualifiedExport :: IO ()
testCompileModuleGraphTransitiveAliasImportHidesUnqualifiedExport = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "unbound code" "E1001" rendered
      assertContains "hidden export" "unbound variable 'subtract'" rendered
    _ -> failTest "expected exactly one unbound hidden export error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nsubtract."),
          ("src/App/UsesMath.jz", "import Lib::Math as Math.\nuse = 0."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAliasHiddenExportUsesPreludeDespiteVisibleImporter :: IO ()
testRunModuleGraphAliasHiddenExportUsesPreludeDespiteVisibleImporter = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "99") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nimport App::UsesPrelude.\npreludeValue."),
          ("src/App/UsesMath.jz", "import Lib::Math.\nmathValue = subtract."),
          ("src/App/UsesPrelude.jz", "import Lib::Math as Math.\npreludeValue = subtract."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenReplay :: IO ()
testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenReplay = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit "subtract = 99.")
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nimport App::UsesPrelude.\nmathValue."),
          ("src/App/UsesMath.jz", "import Lib::Math.\nmathValue = subtract."),
          ("src/App/UsesPrelude.jz", "import Lib::Math as Math.\npreludeValue = subtract."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated :: IO ()
testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "sibling capability fact isolation error"
        "missing class declaration 'Eq'"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one sibling capability fact isolation error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Facts.\nimport Lib::UsesEq.\nuses."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.\nfacts = 0."),
          ("src/Lib/UsesEq.jz", "uses :: @{Eq(Int)}: Int.\nuses = 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports :: IO ()
testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Facts.\nuse :: @{Eq(Int)}: Int.\nuse = 1."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.\nfacts = 0.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphHiddenQualifiedExportKeepsDependencyBridge :: IO ()
testRunModuleGraphHiddenQualifiedExportKeepsDependencyBridge = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nMath::use."),
          ("src/Lib/Math.jz", "subtract = 2.\nuse = subtract.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphQualifiedAliasLookup :: IO ()
testRunModuleGraphQualifiedAliasLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nMath::subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphQualifiedAliasLookupUsesDependencyExport :: IO ()
testRunModuleGraphQualifiedAliasLookupUsesDependencyExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "subtract = 99.\nimport Lib::Math as Math.\nMath::subtract."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphQualifiedAliasLookupBeforeImport :: IO ()
testCompileModuleGraphQualifiedAliasLookupBeforeImport = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "math::subtract.\nimport Lib::Math as math."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsBundledClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsBundledClassQualifiedMethodLookup = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nEq::equals 1 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsImportedClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsImportedClassQualifiedMethodLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsAliasedImportedClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsAliasedImportedClassQualifiedMethodLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts as Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}."
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch :: IO ()
testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Api (Choice).\nimport Lib::Hidden (val).\nChoice::pick 1.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\n}"
          ),
          ( "src/Lib/Hidden.jz",
            "module Lib::Hidden {\nimport Lib::Api (Choice).\nval = 0.\nimpl Choice(UInt8) {\npick = \\(value) -> False.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Api (foo).\nfoo.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\nfoo = Choice::pick 1.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Api (Choice).\nChoice::pick 1.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Flag(a) {\nenabled :: Bool.\n}.\nimpl Flag(Int) {\nenabled = True.\n}.\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> Flag::enabled.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Api (foo).\nfoo.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Need(a) {\n}.\nimpl Need(Int) {\n}.\nfoo :: @{Need(Int)}: Int.\nfoo = 1.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphModuleDeclarationMismatch :: IO ()
testCompileModuleGraphModuleDeclarationMismatch = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "mismatch code" "E4006" rendered
      assertContains "mismatch declared module" "Wrong::Name" rendered
      assertContains "mismatch expected module" "App::Main" rendered
    _ -> failTest "expected exactly one module declaration mismatch error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "module Wrong::Name {\nmain = 1.\n}")]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphCycle :: IO ()
testRunModuleGraphCycle = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["A", "One"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)
  case runCompileErrors result of
    [err] -> assertContains "cycle code" "E4003" (renderDiagnostic err)
    _ -> failTest "expected exactly one cycle error"
  where
    sourceMap =
      Map.fromList
        [ ("src/A/One.jz", "import B::Two.\na."),
          ("src/B/Two.jz", "import A::One.\nb."),
          ("src/b.jz", "b = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testMemoizedLookupReuse :: IO ()
testMemoizedLookupReuse = do
  readCountsRef <- newIORef (Map.empty :: Map.Map FilePath Int)
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (lookupSource readCountsRef)
  readCounts <- readIORef readCountsRef
  assertEqual "run succeeds" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  assertEqual
    "entry module read exactly once"
    (Just 1)
    (Map.lookup "src/App/Main.jz" readCounts)
  assertEqual
    "dependency module read exactly once"
    (Just 1)
    (Map.lookup "src/Lib/Util.jz" readCounts)
  where
    lookupSource readCountsRef path = do
      readCounts <- readIORef readCountsRef
      let previousReads = Map.findWithDefault 0 path readCounts
          nextReadCount = previousReads + 1
      writeIORef readCountsRef (Map.insert path nextReadCount readCounts)
      pure (lookupByReadCount path nextReadCount)

    lookupByReadCount :: FilePath -> Int -> Maybe Text
    lookupByReadCount path readCount =
      case path of
        -- Without memoization this second read would replace the resolver-accepted
        -- source and fail replay. Memoized lookup should keep first-read content.
        "src/App/Main.jz"
          | readCount == 1 -> Just "module App::Main {\nimport Lib::Util.\nutil.\n}"
          | otherwise -> Just "broken = ."
        "src/Lib/Util.jz" -> Just "module Lib::Util {\nutil = 1.\n}"
        _ -> Nothing

moduleGraphProjectedSources :: Text -> Map.Map FilePath Text
moduleGraphProjectedSources projectedExpr =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Data.\nprojected.\n}"),
      ("src/Lib/Data.jz", "module Lib::Data {\nvalues = [[1, 2], [3]].\nprojected = " <> projectedExpr <> ".\n}")
    ]

lookupSourceIn :: Map.Map FilePath Text -> FilePath -> IO (Maybe Text)
lookupSourceIn sourceMap path = pure (Map.lookup path sourceMap)

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }
