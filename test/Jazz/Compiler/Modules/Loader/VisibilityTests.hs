{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.VisibilityTests
  ( visibilityTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import Jazz.Compiler.Driver
  ( ResolvedPrelude (..),
    RunResult (..),
    compileErrors,
    compileModuleGraphWithResolvedPrelude,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runModuleGraph,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude,
    runRuntimeErrors,
    runWarnings
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest
  )
import Jazz.Compiler.Modules.Loader.Shared

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
    , ("compile module graph qualifies alias-only pattern coverage witnesses", testCompileModuleGraphQualifiesAliasOnlyPatternCoverageWitness)
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
    , ("run module graph imports selected grouped constructors in expressions and patterns", testRunModuleGraphImportsSelectedGroupedConstructor)
    , ("compile module graph hides unselected grouped constructors", testCompileModuleGraphHidesUnselectedGroupedConstructor)
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
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Data.
        map hd values.
        }
        """),
          ("src/Lib/Data.jz", """
          module Lib::Data {
          values = [[1, 2], [3], [4, 5]].
          }
          """)
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
    ["error: E1001: unbound variable 'map'", "error: E1001: unbound variable 'hd'"]
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
    ["error: E1001: unbound variable 'map'", "error: E1001: unbound variable 'hd'"]
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        add.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = missingName.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe (x).
        x.
        """),
          ("src/Lib/Maybe.jz", """
          data Maybe = Just Int.
          x = 1.
          Just 1.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        1.
        """),
          ("src/Lib/Maybe.jz", """
          data Maybe = Just Int.
          x = Just 1.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe (x).
        x.
        """),
          ("src/Lib/Maybe.jz", """
          data Maybe = Just Int.
          x = Just 1.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        Nothing.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        Maybe::Just 1.
        """),
          ("src/Lib/Maybe.jz", "data Maybe = Just Int | Nothing.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphQualifiesAliasOnlyPatternCoverageWitness :: IO ()
testCompileModuleGraphQualifiesAliasOnlyPatternCoverageWitness = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "alias-only coverage code" "E2018" (renderDiagnostic diagnostic)
      assertContains
        "alias-only coverage witness"
        "missing pattern: Choice::Second _"
        (renderDiagnostic diagnostic)
    diagnostics ->
      failTest
        ( "expected one alias-qualified E2018 diagnostic, got "
            <> Text.pack (show diagnostics)
        )
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Choice as Choice.
        selected = Choice::Third.
        case selected { | _ if False -> 0 }.
        """),
          ("src/Lib/Choice.jz", "data Choice = Second Int | Third.")
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
        [ ("src/App/Main.jz", """
        import Lib::Box as Box.
        first = Box::Box 1.
        second = Box::Box True.
        second.
        """),
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
            """
            module App::Main {
            import Lib::Box as B.
            boxed :: B::Box(Int).
            boxed = B::Box 1.
            boxed.
            }
            """
          ),
          ( "src/Lib/Box.jz",
            """
            module Lib::Box {
            data Box a = Box a.
            }
            """
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
            """
            module App::Main {
            import Lib::Types as T.
            class Marker(a) { }.
            impl Marker(T::Secret(Int)) { }.
            0.
            }
            """
          ),
          ( "src/Lib/Types.jz",
            """
            module Lib::Types () {
            data Secret a = Secret a.
            }
            """
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
            """
            module App::Main {
            import Lib::Token as t.
            token :: t::Token.
            token = t::Token.
            token.
            }
            """
          ),
          ( "src/Lib/Token.jz",
            """
            module Lib::Token {
            data Token = Token.
            }
            """
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
            """
            module App::Main {
            import Lib::Token as t.
            class Marker(a) {
            mark :: a -> Bool.
            }.
            impl Marker(t::Token) {
            mark = \\(ignored) -> True.
            }.
            Marker::mark t::Token.
            }
            """
          ),
          ( "src/Lib/Token.jz",
            """
            module Lib::Token {
            data Token = Token.
            }
            """
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
            """
            module App::Main {
            import lib::Types.
            boxed :: Box(Int).
            boxed = Box 1.
            boxed.
            }
            """
          ),
          ( "src/lib/Types.jz",
            """
            module lib::Types {
            data Box a = Box a.
            }
            """
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
            """
            module App::Main {
            import Lib::Box.
            intBox = keep (Box 1).
            boolBox = keep (Box True).
            boolBox.
            }
            """
          ),
          ( "src/Lib/Box.jz",
            """
            module Lib::Box {
            data Box a = Box a.
            keep :: Box(a) -> Box(a).
            keep = \\(box) -> box.
            }
            """
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
        [ ("src/App/Main.jz", """
        import App::UsesMaybe.
        import Lib::Maybe (Just).
        data Pair = Just Int Int.
        Just 1 2.
        """),
          ("src/App/UsesMaybe.jz", """
          import Lib::Maybe as Maybe.
          use = 0.
          """),
          ("src/Lib/Maybe.jz", "data Maybe = Just Int.")
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
        [ ("src/App/Main.jz", """
        import Lib::Floats as Floats.
        (Floats::x16, Floats::x32).
        """),
          ("src/Lib/Floats.jz", """
          x16 :: Float16.
          x16 = 2049.0.
          x32 :: Float32.
          x32 = 1.00000001.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        Maybe::fromDefault.
        """),
          ("src/Lib/Maybe.jz", """
          data Maybe = Just Int | Nothing.
          default = Just 7.
          fromDefault = case default { | Just item -> item | Nothing -> 0 }.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe.
        selected = Also 41.
        case selected { | Just item | Also item -> item + 1 | Nothing -> 0 }.
        """),
          ("src/Lib/Maybe.jz", """
          module Lib::Maybe {
          data Maybe = Nothing | Just Int | Also Int.
          }
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe.
        choose = \\|(Just item | Also item) -> item + 1
                  |(Nothing) -> 0.
        choose (Also 41).
        """),
          ("src/Lib/Maybe.jz", """
          module Lib::Maybe {
          data Maybe = Nothing | Just Int | Also Int.
          }
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        Math::subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import App::UsesMath.
        use.
        """),
          ("src/App/UsesMath.jz", """
          import Lib::Math as Math.
          use = subtract.
          """),
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
        [ ("src/App/Main.jz", """
        import App::UsesMath.
        subtract.
        """),
          ("src/App/UsesMath.jz", """
          import Lib::Math as Math.
          use = 0.
          """),
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
        [ ("src/App/Main.jz", """
        import App::UsesMath.
        import App::UsesPrelude.
        preludeValue.
        """),
          ("src/App/UsesMath.jz", """
          import Lib::Math.
          mathValue = subtract.
          """),
          ("src/App/UsesPrelude.jz", """
          import Lib::Math as Math.
          preludeValue = subtract.
          """),
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
        [ ("src/App/Main.jz", """
        import App::UsesMath.
        import App::UsesPrelude.
        mathValue.
        """),
          ("src/App/UsesMath.jz", """
          import Lib::Math.
          mathValue = subtract.
          """),
          ("src/App/UsesPrelude.jz", """
          import Lib::Math as Math.
          preludeValue = subtract.
          """),
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        Math::use.
        """),
          ("src/Lib/Math.jz", """
          subtract = 2.
          use = subtract.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        Math::subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        subtract = 99.
        import Lib::Math as Math.
        Math::subtract.
        """),
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
        [ ("src/App/Main.jz", """
        math::subtract.
        import Lib::Math as math.
        """),
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
        [ ("src/App/Main.jz", """
        module App::Main {
        data Maybe = Just Int.
        Just = 1.
        Just.
        }
        """)
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
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Maybe (Just).
        Just.
        }
        """),
          ("src/Lib/Maybe.jz", """
          module Lib::Maybe {
          data Maybe = Just Int.
          Just = 1.
          }
          """)
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
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value (answer).
        answer 41.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value (answer) {
          helper = \\(x) -> x + 1.
          answer = \\(x) -> helper x.
          }
          """)
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
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value as Value.
        Value::helper.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value (answer) {
          helper = 1.
          answer = helper.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphSupportsOpaqueExportedType :: IO ()
testCompileModuleGraphSupportsOpaqueExportedType = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "opaque exported type compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Box.
        class Use(a) {
        use :: a -> Bool.
        }.
        impl Use(Box) {
        use = \\(ignored) -> True.
        }.
        Use::use boxed.
        }
        """),
          ("src/Lib/Box.jz", """
          module Lib::Box (Box, boxed) {
          data Box = Pack Int.
          boxed = Pack 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportsSelectedGroupedConstructor :: IO ()
testRunModuleGraphImportsSelectedGroupedConstructor = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "selected grouped constructor compile errors" [] (runCompileErrors result)
  assertEqual "selected grouped constructor runtime errors" [] (runRuntimeErrors result)
  assertEqual "selected grouped constructor output" (Just "41") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Choice. selected = First 41. case selected { | First item -> item | _ -> 0 }. }"),
          ("src/Lib/Choice.jz", "module Lib::Choice (type Choice(First)) { data Choice = First Int | Second Int. }")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphHidesUnselectedGroupedConstructor :: IO ()
testCompileModuleGraphHidesUnselectedGroupedConstructor = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  case compileErrors result of
    [diagnostic] -> assertContains "hidden grouped constructor" "Second" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one hidden grouped-constructor diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Choice. Second 41. }"),
          ("src/Lib/Choice.jz", "module Lib::Choice (type Choice(First)) { data Choice = First Int | Second Int. }")
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
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Box (Pack).
        Pack 1.
        }
        """),
          ("src/Lib/Box.jz", """
          module Lib::Box (Pack) {
          data Box = Pack Int.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsPrivateEntryBindingsUsable :: IO ()
testRunModuleGraphKeepsPrivateEntryBindingsUsable = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private entry binding compile errors" [] (runCompileErrors result)
  assertEqual "private entry binding runtime errors" [] (runRuntimeErrors result)
  assertEqual "private entry binding output" (Just "41") (runOutput result)
  where
    sourceMap = Map.singleton "src/App/Main.jz" """
    module App::Main () {
    helper = 41.
    helper.
    }
    """
    lookupSource path = pure (Map.lookup path sourceMap)
