{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.VisibilityTests
  ( visibilityTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    ResolvedPrelude (..),
    RunResult (..),
    compileModuleGraphWithResolvedPrelude,
    compileModuleGraphWithPrelude,
    runModuleGraph,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest
  )
import JazzNext.Compiler.Modules.Loader.Shared

visibilityTests :: [NamedTest]
visibilityTests =
  [ ("run module graph default helper executes bundled prelude aliases across files", testRunModuleGraphDefaultLoadsBundledPrelude)
    , ("run module graph transports Char/Text values", testRunModuleGraphTransportsCharTextValues)
    , ("compile module graph without prelude rejects public aliases across files", testCompileModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles)
    , ("run module graph without prelude rejects public aliases across files", testRunModuleGraphWithoutPreludeRejectsPublicAliasesAcrossFiles)
    , ("compile module graph without prelude keeps kernel bridge aliases across files", testCompileModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles)
    , ("run module graph without prelude executes kernel bridge aliases across files", testRunModuleGraphWithoutPreludeKeepsKernelBridgeAliasesAcrossFiles)
    , ("compile module graph validates hidden dependency exports", testCompileModuleGraphValidatesHiddenDependencyExports)
    , ("compile module graph rewrites hidden constructor dependency expressions", testCompileModuleGraphRewritesHiddenConstructorDependencyExpressions)
    , ("compile module graph hides dependency bindings excluded by explicit import list", testCompileModuleGraphExplicitImportListHidesUnlistedBindings)
    , ("compile module graph keeps hidden constructor dependencies for validation", testCompileModuleGraphKeepsHiddenConstructorValidationDependencies)
    , ("compile module graph allows explicit-import hidden name supplied by prelude", testCompileModuleGraphExplicitImportAllowsPreludeBinding)
    , ("compile module graph hides dependency bindings imported only by alias", testCompileModuleGraphAliasImportHidesUnqualifiedBindings)
    , ("compile module graph allows alias-hidden name supplied by prelude", testCompileModuleGraphAliasImportAllowsPreludeBinding)
    , ("run module graph rewrites visible export hidden constructor dependencies", testRunModuleGraphVisibleExportRewritesHiddenConstructorDependency)
    , ("run module graph keeps explicit-import hidden dependency export from shadowing prelude", testRunModuleGraphExplicitImportHiddenExportUsesPrelude)
    , ("run module graph keeps alias-hidden dependency export from shadowing prelude", testRunModuleGraphAliasImportHiddenExportUsesPrelude)
    , ("run module graph keeps alias-hidden data constructor from shadowing prelude", testRunModuleGraphAliasHiddenDataConstructorUsesPrelude)
    , ("run module graph resolves qualified alias data constructor lookup", testRunModuleGraphQualifiedAliasDataConstructorLookup)
    , ("compile module graph preserves alias-qualified generic constructor schemes", testCompileModuleGraphPreservesAliasQualifiedGenericConstructorSchemes)
    , ("run module graph resolves alias-qualified types in signatures", testRunModuleGraphResolvesAliasQualifiedTypesInSignatures)
    , ("compile module graph rejects private alias-qualified types", testCompileModuleGraphRejectsPrivateAliasQualifiedType)
    , ("run module graph resolves zero-arity types through lowercase aliases", testRunModuleGraphResolvesLowercaseAliasZeroArityType)
    , ("run module graph accepts impl targets through lowercase aliases", testRunModuleGraphAcceptsLowercaseAliasImplTarget)
    , ("run module graph resolves generic types from lowercase module paths", testRunModuleGraphResolvesGenericTypeFromLowercaseModulePath)
    , ("run module graph transports signed generic named schemes", testRunModuleGraphTransportsSignedGenericNamedSchemes)
    , ("run module graph keeps local data constructor from hidden import rewrite", testRunModuleGraphLocalDataConstructorShadowsHiddenImportRewrite)
    , ("run module graph preserves alias-qualified float literal targets", testRunModuleGraphPreservesAliasQualifiedFloatLiteralTargets)
    , ("run module graph keeps hidden qualified export pattern constructors available", testRunModuleGraphHiddenQualifiedPatternExportKeepsConstructorBridge)
    , ("run module graph resolves imported constructors in or-pattern alternatives", testRunModuleGraphResolvesImportedConstructorsInOrPatternAlternatives)
    , ("run module graph resolves imported constructors in lambda or-pattern alternatives", testRunModuleGraphResolvesImportedConstructorsInLambdaOrPatternAlternatives)
    , ("run module graph keeps alias-qualified dependency export visible with prelude", testRunModuleGraphAliasQualifiedExportUsesDependencyWithPrelude)
    , ("run module graph keeps transitive alias-hidden dependency export from shadowing prelude", testRunModuleGraphTransitiveAliasHiddenExportUsesPrelude)
    , ("compile module graph hides transitive alias-only exports from unqualified visibility", testCompileModuleGraphTransitiveAliasImportHidesUnqualifiedExport)
    , ("run module graph keeps alias-hidden prelude binding isolated from visible importer", testRunModuleGraphAliasHiddenExportUsesPreludeDespiteVisibleImporter)
    , ("run module graph keeps visible sibling import isolated from alias-hidden modules", testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenModule)
    , ("run module graph keeps hidden qualified export dependencies available", testRunModuleGraphHiddenQualifiedExportKeepsDependencyBridge)
    , ("run module graph resolves qualified alias lookup", testRunModuleGraphQualifiedAliasLookup)
    , ("run module graph resolves qualified alias lookup through dependency export", testRunModuleGraphQualifiedAliasLookupUsesDependencyExport)
    , ("compile module graph accepts qualified alias use before import", testCompileModuleGraphQualifiedAliasLookupBeforeImport)
    , ("run module graph lets ordinary bindings shadow local constructors", testRunModuleGraphOrdinaryBindingShadowsLocalConstructor)
    , ("run module graph imports ordinary bindings that shadow constructors", testRunModuleGraphImportsOrdinaryBindingThatShadowsConstructor)
    , ("run module graph executes public closure with private helper", testRunModuleGraphExecutesPublicClosureWithPrivateHelper)
    , ("compile module graph rejects private alias member", testCompileModuleGraphRejectsPrivateAliasMember)
    , ("compile module graph supports opaque exported type", testCompileModuleGraphSupportsOpaqueExportedType)
    , ("run module graph imports exported constructor without type name", testRunModuleGraphImportsExportedConstructorWithoutTypeName)
    , ("run module graph keeps private entry bindings usable", testRunModuleGraphKeepsPrivateEntryBindingsUsable)
    , ("run module graph keeps earlier imported values visible before later block binders", testRunModuleGraphKeepsEarlierImportedValueBeforeLaterBlockBinder)
    , ("run module graph keeps imported values outside later recursive-looking block cycles", testRunModuleGraphKeepsImportedValueOutsideLaterBlockCycle)
    , ("run module graph keeps earlier imported constructors visible before later block binders", testRunModuleGraphKeepsEarlierImportedConstructorBeforeLaterBlockBinder)
    , ("run module graph preserves nested mutual recursion while resolving block binders sequentially", testRunModuleGraphPreservesNestedMutualRecursionDuringSequentialResolution)
  ]

testRunModuleGraphKeepsEarlierImportedValueBeforeLaterBlockBinder :: IO ()
testRunModuleGraphKeepsEarlierImportedValueBeforeLaterBlockBinder = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "earlier imported value compile errors" [] (runCompileErrors result)
  assertEqual "earlier imported value runtime errors" [] (runRuntimeErrors result)
  assertEqual "earlier imported value output" (Just "41") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/Lib/Values.jz", "module Lib::Values { importedValue = 41. }"),
          ( "src/App/Main.jz",
            "module App::Main { import Lib::Values. { result = importedValue. importedValue = 2. result. }. }"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsImportedValueOutsideLaterBlockCycle :: IO ()
testRunModuleGraphKeepsImportedValueOutsideLaterBlockCycle = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "recursive-looking import compile errors" [] (runCompileErrors result)
  assertEqual "recursive-looking import runtime errors" [] (runRuntimeErrors result)
  assertEqual "recursive-looking import output" (Just "41") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/Lib/Values.jz", "module Lib::Values { importedValue = 41. }"),
          ( "src/App/Main.jz",
            "module App::Main { import Lib::Values. { result = importedValue. importedValue = result. result. }. }"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsEarlierImportedConstructorBeforeLaterBlockBinder :: IO ()
testRunModuleGraphKeepsEarlierImportedConstructorBeforeLaterBlockBinder = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "earlier imported constructor compile errors" [] (runCompileErrors result)
  assertEqual "earlier imported constructor runtime errors" [] (runRuntimeErrors result)
  assertEqual "earlier imported constructor output" (Just "Nothing") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/Lib/MaybeValue.jz", "module Lib::MaybeValue { data MaybeValue = Nothing. }"),
          ( "src/App/Main.jz",
            "module App::Main { import Lib::MaybeValue. { result = Nothing. Nothing = 1. result. }. }"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphPreservesNestedMutualRecursionDuringSequentialResolution :: IO ()
testRunModuleGraphPreservesNestedMutualRecursionDuringSequentialResolution = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "nested mutual recursion compile errors" [] (runCompileErrors result)
  assertEqual "nested mutual recursion runtime errors" [] (runRuntimeErrors result)
  assertEqual "nested mutual recursion output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main { { even = \\(n) -> if n == 0 then True else odd (n - 1). odd = \\(n) -> if n == 0 then False else even (n - 1). even 4. }. }"
          )
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

testRunModuleGraphTransportsCharTextValues :: IO ()
testRunModuleGraphTransportsCharTextValues = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/Lib/TextValues.jz", "module Lib::TextValues (value letter, value message) { letter :: Char. letter = 'J'. message :: Text. message = \"Jazz\". }"),
          ("src/App/Main.jz", "module App::Main { import Lib::TextValues (letter, message). (letter == 'J', message == \"Jazz\"). }")
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

testRunModuleGraphResolvesAliasQualifiedTypesInSignatures :: IO ()
testRunModuleGraphResolvesAliasQualifiedTypesInSignatures = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "alias-qualified type compile errors" [] (runCompileErrors result)
  assertEqual "alias-qualified type runtime errors" [] (runRuntimeErrors result)
  assertEqual "alias-qualified type output" (Just "Box(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Box as B.\nvalue :: B::Box(Int).\nvalue = B::Box 1.\nvalue.\n}"
          ),
          ( "src/Lib/Box.jz",
            "module Lib::Box {\ndata Box a = Box a.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsPrivateAliasQualifiedType :: IO ()
testCompileModuleGraphRejectsPrivateAliasQualifiedType = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "private alias-qualified type code" "E4014" (renderDiagnostic diagnostic)
      assertContains "private alias-qualified type name" "Secret" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one E4014 diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Types as T.\nclass Marker(a) { }.\nimpl Marker(T::Secret(Int)) { }.\n0.\n}"
          ),
          ( "src/Lib/Types.jz",
            "module Lib::Types () {\ndata Secret a = Secret a.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphResolvesLowercaseAliasZeroArityType :: IO ()
testRunModuleGraphResolvesLowercaseAliasZeroArityType = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "lowercase alias zero-arity compile errors" [] (runCompileErrors result)
  assertEqual "lowercase alias zero-arity runtime errors" [] (runRuntimeErrors result)
  assertEqual "lowercase alias zero-arity output" (Just "Token") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Token as t.\nvalue :: t::Token.\nvalue = t::Token.\nvalue.\n}"
          ),
          ( "src/Lib/Token.jz",
            "module Lib::Token {\ndata Token = Token.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAcceptsLowercaseAliasImplTarget :: IO ()
testRunModuleGraphAcceptsLowercaseAliasImplTarget = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "lowercase alias impl compile errors" [] (runCompileErrors result)
  assertEqual "lowercase alias impl runtime errors" [] (runRuntimeErrors result)
  assertEqual "lowercase alias impl output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Token as t.\nclass Marker(a) {\nmark :: a -> Bool.\n}.\nimpl Marker(t::Token) {\nmark = \\(value) -> True.\n}.\nMarker::mark t::Token.\n}"
          ),
          ( "src/Lib/Token.jz",
            "module Lib::Token {\ndata Token = Token.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphResolvesGenericTypeFromLowercaseModulePath :: IO ()
testRunModuleGraphResolvesGenericTypeFromLowercaseModulePath = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "lowercase module-path generic compile errors" [] (runCompileErrors result)
  assertEqual "lowercase module-path generic runtime errors" [] (runRuntimeErrors result)
  assertEqual "lowercase module-path generic output" (Just "Box(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport lib::Types.\nvalue :: Box(Int).\nvalue = Box 1.\nvalue.\n}"
          ),
          ( "src/lib/Types.jz",
            "module lib::Types {\ndata Box a = Box a.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphTransportsSignedGenericNamedSchemes :: IO ()
testRunModuleGraphTransportsSignedGenericNamedSchemes = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "generic interface compile errors" [] (runCompileErrors result)
  assertEqual "generic interface runtime errors" [] (runRuntimeErrors result)
  assertEqual "generic interface output" (Just "Box(True)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Box.\nintBox = keep (Box 1).\nboolBox = keep (Box True).\nboolBox.\n}"
          ),
          ( "src/Lib/Box.jz",
            "module Lib::Box {\ndata Box a = Box a.\nkeep :: Box(a) -> Box(a).\nkeep = \\(value) -> value.\n}"
          )
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

testRunModuleGraphResolvesImportedConstructorsInOrPatternAlternatives :: IO ()
testRunModuleGraphResolvesImportedConstructorsInOrPatternAlternatives = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe.\nvalue = Also 41.\ncase value { | Just item | Also item -> item + 1 | Nothing -> 0 }."),
          ("src/Lib/Maybe.jz", "module Lib::Maybe {\ndata Maybe = Nothing | Just value | Also value.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphResolvesImportedConstructorsInLambdaOrPatternAlternatives :: IO ()
testRunModuleGraphResolvesImportedConstructorsInLambdaOrPatternAlternatives = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "42") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe.\nchoose = \\(Just item | Also item) -> item + 1.\nchoose (Also 41)."),
          ("src/Lib/Maybe.jz", "module Lib::Maybe {\ndata Maybe = Nothing | Just value | Also value.\n}")
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

testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenModule :: IO ()
testRunModuleGraphVisibleSiblingImportSurvivesAliasHiddenModule = do
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

testRunModuleGraphOrdinaryBindingShadowsLocalConstructor :: IO ()
testRunModuleGraphOrdinaryBindingShadowsLocalConstructor = do
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
        [ ("src/App/Main.jz", "module App::Main {\ndata Maybe = Just value.\nJust = 1.\nJust.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportsOrdinaryBindingThatShadowsConstructor :: IO ()
testRunModuleGraphImportsOrdinaryBindingThatShadowsConstructor = do
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Maybe (Just).\nJust.\n}"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe {\ndata Maybe = Just value.\nJust = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphExecutesPublicClosureWithPrivateHelper :: IO ()
testRunModuleGraphExecutesPublicClosureWithPrivateHelper = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private helper closure compile errors" [] (runCompileErrors result)
  assertEqual "private helper closure runtime errors" [] (runRuntimeErrors result)
  assertEqual "private helper closure output" (Just "42") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value (answer).\nanswer 41.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = \\(x) -> x + 1.\nanswer = \\(x) -> helper x.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsPrivateAliasMember :: IO ()
testCompileModuleGraphRejectsPrivateAliasMember = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "private alias code" "E4014" (renderDiagnostic diagnostic)
      assertContains "private alias member" "helper" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one E4014 diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value as Value.\nValue::helper.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = 1.\nanswer = helper.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphSupportsOpaqueExportedType :: IO ()
testCompileModuleGraphSupportsOpaqueExportedType = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "opaque exported type compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Box.\nclass Use(a) {\nuse :: a -> Bool.\n}.\nimpl Use(Box) {\nuse = \\(value) -> True.\n}.\nUse::use boxed.\n}"),
          ("src/Lib/Box.jz", "module Lib::Box (Box, boxed) {\ndata Box = Pack Int.\nboxed = Pack 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportsExportedConstructorWithoutTypeName :: IO ()
testRunModuleGraphImportsExportedConstructorWithoutTypeName = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "constructor-only export compile errors" [] (runCompileErrors result)
  assertEqual "constructor-only export runtime errors" [] (runRuntimeErrors result)
  assertEqual "constructor-only export output" (Just "Pack(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Box (Pack).\nPack 1.\n}"),
          ("src/Lib/Box.jz", "module Lib::Box (Pack) {\ndata Box = Pack Int.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsPrivateEntryBindingsUsable :: IO ()
testRunModuleGraphKeepsPrivateEntryBindingsUsable = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private entry binding compile errors" [] (runCompileErrors result)
  assertEqual "private entry binding runtime errors" [] (runRuntimeErrors result)
  assertEqual "private entry binding output" (Just "41") (runOutput result)
  where
    sourceMap = Map.singleton "src/App/Main.jz" "module App::Main () {\nhelper = 41.\nhelper.\n}"
    lookupSource path = pure (Map.lookup path sourceMap)
