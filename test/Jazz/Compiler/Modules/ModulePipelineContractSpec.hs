{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CorePhase (Resolved))
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( CompileResult,
    RunExecution (..),
    RunResult,
    compileErrors,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runExecution,
    runExitStatus,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runOutput,
    runRuntimeErrors,
    runRuntimeValue,
  )
import Jazz.Compiler.ModuleCompiler
  ( CompiledProgram,
    compileResolvedProgram,
    compiledModuleExportInventory,
    compiledModuleExpr,
    compiledModuleInterface,
    compiledProgramErrors,
    compiledProgramModules,
    compiledProgramPrelude,
    lookupCompiledModule,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    exportInventory,
    exportInventoryEntries,
  )
import Jazz.Compiler.ModuleGraph (PreludeArtifact (..))
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    mkModulePath,
    mkSourceFile,
    moduleIdentity,
  )
import Jazz.Compiler.ModuleInterface
  ( CompiledPrelude (..),
    ModuleInterface (..),
    emptyCompileInputs,
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..), resolveProgramWithAmbientExports)
import Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (runtimeModuleExports, runtimeModulePath),
    RuntimeProgram (runtimeProgramModules, runtimeProgramOutput),
    evaluateCompiledProgram,
    lookupRuntimeModule,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ConstructorNamespace, TypeNamespace, ValueNamespace),
    identifierText,
    mkIdentifier,
  )
import Jazz.Compiler.Runtime
  ( RuntimeCell,
    renderRuntimeValue,
    runtimeExprRequiresHost,
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost (..),
    RuntimeHostExit (..),
    disabledRuntimeHost,
    productionRuntimeHost,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "ModulePipelineContract" tests

tests :: [NamedTest]
tests =
  [ ("dependency expressions are checked but not executed", testDependencyExpressionContract),
    ("compiled interfaces expose only declared exports", testCompiledInterfacesExposeOnlyDeclaredExports),
    ("runtime modules publish only declared exports", testRuntimeModulePublishesDeclaredExports),
    ("compiled modules retain private interfaces with public inventories", testCompiledModuleKeepsPrivateInterfaceWithPublicInventory),
    ("runtime modules publish explicit value exports only", testRuntimeModulePublishesExplicitExportsOnly),
    ("runtime modules publish methods only for public classes", testRuntimeModulePublishesPublicClassMethodsOnly),
    ("module export identities distinguish shadowed values and constructors", testModuleExportIdentityPreservesNamespaces),
    ("namespace-aware runtime exports publish selected value only", testNamespaceAwareRuntimeExportPublishesValueOnly),
    ("namespace-aware runtime exports publish selected constructor only", testNamespaceAwareRuntimeExportPublishesConstructorOnly),
    ("grouped exports publish selected constructors through interface and runtime inventories", testGroupedExportsPublishSelectedConstructor),
    ("compiled generic constructor fields remain module-stable", testCompiledGenericConstructorFieldsRemainModuleStable),
    ("compiled dependency terminal expressions are skipped", testCompiledDependencyTerminalExpressionIsSkipped),
    ("host-free and host-capable module paths preserve observable results", testModuleRuntimePathParity),
    ("run result projections distinguish all execution states", testRunResultProjectionInvariants),
    ("module graph execution carries one host through dependency exports", testModuleGraphInjectsRuntimeHost),
    ("alias imports stay qualified", testAliasIsolationContract),
    ("transitive imports do not leak", testTransitiveVisibilityContract),
    ("module diagnostics retain source paths", testSourcePathContract),
    ("lexical binders shadow imported and builtin names", testLexicalBindersShadowImportedAndBuiltinNames)
  ]

testCompiledGenericConstructorFieldsRemainModuleStable :: IO ()
testCompiledGenericConstructorFieldsRemainModuleStable = do
  compiled <- compileFixtureProgram sources
  case lookupCompiledModule (nominalModulePath ("Lib" :| ["Box"])) compiled of
    Nothing -> fail "missing compiled Lib::Box module"
    Just boxModule ->
      case Map.lookup "Box" (interfaceDataTypes (compiledModuleInterface boxModule)) of
        Just
          ( DataTypeBinding
              [_]
              [[ConstructorArgumentStructured (TypeList (TypeVariable parameterName))]]
            ) ->
            assertEqual "stable constructor parameter name" "a" (identifierText parameterName)
        binding ->
          fail ("unexpected compiled Box constructor metadata: " <> show binding)
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "cross-module structured constructor output"
        (Just "Box([1])")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Box. Box [1]. }"),
          ("src/Lib/Box.jz", "module Lib::Box { data Box a = Box [a]. }")
        ]

testLexicalBindersShadowImportedAndBuiltinNames :: IO ()
testLexicalBindersShadowImportedAndBuiltinNames = do
  compiled <- compileFixtureProgram sources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "lexical shadowing output"
        (Just "(1, 2, 3, 4)")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
              import Lib::Value.
              ((\\(x) -> x) 1, case 2 { | x -> x }, { x = 3. x. }, (\\(map) -> map) 4).
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value {
              x = 99.
            }
            """
          )
        ]

testRuntimeModulePublishesDeclaredExports :: IO ()
testRuntimeModulePublishesDeclaredExports = do
  compiled <- compileFixtureProgram simpleSources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> fail "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "export names"
            (Set.fromList [RuntimeBindingExport (ModuleExport ValueNamespace "answer")])
            (Map.keysSet (runtimeModuleExports runtimeModule))

testCompiledModuleKeepsPrivateInterfaceWithPublicInventory :: IO ()
testCompiledModuleKeepsPrivateInterfaceWithPublicInventory = do
  compiled <- compileFixtureProgram explicitExportSources
  case lookupCompiledModule (nominalModulePath ("Lib" :| ["Value"])) compiled of
    Nothing -> fail "missing compiled Lib::Value module"
    Just valueModule -> do
      assertEqual
        "full compiled interface"
        (Set.fromList [ModuleExport ValueNamespace "answer", ModuleExport ValueNamespace "helper"])
        (Map.keysSet (interfaceValueTypes (compiledModuleInterface valueModule)))
      assertEqual
        "public compiled inventory"
        (Set.singleton (ModuleExport ValueNamespace "answer"))
        ( exportInventoryEntries
            (compiledModuleExportInventory valueModule)
        )

testRuntimeModulePublishesExplicitExportsOnly :: IO ()
testRuntimeModulePublishesExplicitExportsOnly = do
  compiled <- compileFixtureProgram explicitExportSources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail (Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> fail "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "public runtime exports"
            (Set.singleton (RuntimeBindingExport (ModuleExport ValueNamespace "answer")))
            (Map.keysSet (runtimeModuleExports runtimeModule))

testRuntimeModulePublishesPublicClassMethodsOnly :: IO ()
testRuntimeModulePublishesPublicClassMethodsOnly = do
  compiled <- compileFixtureProgram explicitCapabilitySources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail (Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Facts"] runtime of
        Nothing -> fail "missing runtime Lib::Facts module"
        Just runtimeModule ->
          assertEqual
            "public class method runtime exports"
            (Set.singleton (RuntimeCapabilityMethodExport "Eq" "equals"))
            (Map.keysSet (runtimeModuleExports runtimeModule))

explicitExportSources :: Map.Map FilePath Text
explicitExportSources =
  Map.fromList
    [ ( "src/App/Main.jz",
        """
        module App::Main {
        import Lib::Value (answer).
        answer 41.
        }
        """
      ),
      ( "src/Lib/Value.jz",
        """
        module Lib::Value (answer) {
        helper = \\(x) -> x + 1.
        answer = \\(x) -> helper x.
        }
        """
      )
    ]

explicitCapabilitySources :: Map.Map FilePath Text
explicitCapabilitySources =
  Map.fromList
    [ ( "src/App/Main.jz",
        """
        module App::Main {
        import Lib::Facts (Eq).
        Eq::equals 1 1.
        }
        """
      ),
      ( "src/Lib/Facts.jz",
        """
        module Lib::Facts (Eq) {
        class Eq(a) {
        equals :: a -> a -> Bool.
        }.
        class Hidden(a) {
        secret :: a -> Bool.
        }.
        impl Eq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl Hidden(Int) {
        secret = \\(item) -> False.
        }.
        }
        """
      )
    ]

testModuleExportIdentityPreservesNamespaces :: IO ()
testModuleExportIdentityPreservesNamespaces = do
  compiled <- compileFixtureProgram shadowingSources
  case lookupCompiledModule (nominalModulePath ("Lib" :| ["Maybe"])) compiled of
    Nothing -> fail "missing compiled Lib::Maybe module"
    Just maybeModule ->
      assertEqual
        "compiled shadowed export identities"
        expectedExports
        ( Map.keysSet
            ( Map.filterWithKey
                (\moduleExport _ -> moduleExportName moduleExport == "Just")
                (interfaceValueTypes (compiledModuleInterface maybeModule))
            )
        )
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "runtime shadowed export identities"
            expectedRuntimeExports
            ( Map.keysSet
                ( Map.filterWithKey
                    ( \runtimeExport _ ->
                        case runtimeExport of
                          RuntimeBindingExport moduleExport -> moduleExportName moduleExport == "Just"
                          RuntimeCapabilityMethodExport {} -> False
                    )
                    (runtimeModuleExports runtimeModule)
                )
            )
  where
    expectedExports =
      Set.fromList
        [ ModuleExport ValueNamespace "Just",
          ModuleExport ConstructorNamespace "Just"
        ]
    expectedRuntimeExports = Set.map RuntimeBindingExport expectedExports
    shadowingSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe { data Maybe a = Just a. Just = 1. }")
        ]

testNamespaceAwareRuntimeExportPublishesValueOnly :: IO ()
testNamespaceAwareRuntimeExportPublishesValueOnly = do
  compiled <- compileFixtureProgram sources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "namespace-selected runtime exports"
            (Set.singleton (RuntimeBindingExport (ModuleExport ValueNamespace "Just")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe (value Just) { data Maybe a = Just a. Just = 1. }")
        ]

testNamespaceAwareRuntimeExportPublishesConstructorOnly :: IO ()
testNamespaceAwareRuntimeExportPublishesConstructorOnly = do
  compiled <- compileFixtureProgram sources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "namespace-selected constructor runtime export"
            (Set.singleton (RuntimeBindingExport (ModuleExport ConstructorNamespace "Just")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe (constructor Just) { data Maybe a = Just a. Just = 1. }")
        ]

testGroupedExportsPublishSelectedConstructor :: IO ()
testGroupedExportsPublishSelectedConstructor = do
  compiled <- compileFixtureProgram sources
  case lookupCompiledModule (nominalModulePath ("Lib" :| ["Choice"])) compiled of
    Nothing -> fail "missing compiled Lib::Choice module"
    Just choiceModule ->
      do
        assertEqual
          "full grouped compiled interface retains private constructors"
          ( Set.fromList
              [ ModuleExport ConstructorNamespace "First",
                ModuleExport ConstructorNamespace "Second"
              ]
          )
          (Map.keysSet (interfaceValueTypes (compiledModuleInterface choiceModule)))
        assertEqual
          "grouped public inventory"
          ( Set.fromList
              [ ModuleExport ConstructorNamespace "First",
                ModuleExport TypeNamespace "Choice"
              ]
          )
          ( exportInventoryEntries
              (compiledModuleExportInventory choiceModule)
          )
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Choice"] runtime of
        Nothing -> fail "missing runtime Lib::Choice module"
        Just runtimeModule ->
          assertEqual
            "grouped runtime export inventory"
            (Set.singleton (RuntimeBindingExport (ModuleExport ConstructorNamespace "First")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Choice. First 1. }"),
          ("src/Lib/Choice.jz", "module Lib::Choice (type Choice(First)) { data Choice a = First a | Second a. }")
        ]

testCompiledDependencyTerminalExpressionIsSkipped :: IO ()
testCompiledDependencyTerminalExpressionIsSkipped = do
  compiled <- compileFixtureProgram dependencyExpressionSources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "entry output"
        (Just "1")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)

testModuleRuntimePathParity :: IO ()
testModuleRuntimePathParity = do
  hostFreeProgram <- compileFixtureProgram hostFreeParitySources
  hostCapableProgram <- compileFixtureProgram hostCapableParitySources
  assertAbsentCompiledPrelude "host-free program" hostFreeProgram
  assertAbsentCompiledPrelude "host-capable program" hostCapableProgram
  assertEqual
    "host-free module requirements select the pure path"
    [False, False]
    (map (runtimeExprRequiresHost . compiledModuleExpr) (compiledProgramModules hostFreeProgram))
  assertEqual
    "unselected host call selects the host-capable path"
    [False, True]
    (map (runtimeExprRequiresHost . compiledModuleExpr) (compiledProgramModules hostCapableProgram))
  case (evaluateCompiledProgram hostFreeProgram, evaluateCompiledProgram hostCapableProgram) of
    (Right hostFreeRuntime, Right hostCapableRuntime) -> do
      let hostFreeProjection = observableRuntimeProgram hostFreeRuntime
          hostCapableProjection = observableRuntimeProgram hostCapableRuntime
          expectedProjection =
            ( ["Lib::Value", "App::Main"],
              [ ( "Lib::Value",
                  [ (RuntimeBindingExport (ModuleExport ConstructorNamespace "Other"), "Other"),
                    (RuntimeBindingExport (ModuleExport ConstructorNamespace "Shared"), "Shared")
                  ]
                ),
                ("App::Main", [])
              ],
              Just "(Shared, Other)"
            )
      assertEqual "host-free observable module result" expectedProjection hostFreeProjection
      assertEqual "host-capable observable module result" expectedProjection hostCapableProjection
      assertEqual "pure and host-capable observable module parity" hostFreeProjection hostCapableProjection
    (Left diagnostic, _) -> fail ("host-free runtime failed: " <> Text.unpack (renderDiagnostic diagnostic))
    (_, Left diagnostic) -> fail ("host-capable runtime failed: " <> Text.unpack (renderDiagnostic diagnostic))

testRunResultProjectionInvariants :: IO ()
testRunResultProjectionInvariants =
  mapM_ assertProjection cases
  where
    assertProjection (label, action, expected) = do
      result <- action
      assertEqual label expected (runResultProjection result)
    cases =
      [ ( "not executed",
          runProjectionFixture disabledRuntimeHost "module App::Main { missing. }",
          ("not-executed", Nothing, Nothing, Nothing)
        ),
        ( "runtime failed",
          runProjectionFixture disabledRuntimeHost "module App::Main { 1 / 0. }",
          ("runtime-failed", Nothing, Nothing, Nothing)
        ),
        ( "explicit exit",
          runProjectionFixture productionRuntimeHost "module App::Main { __kernel_exit! 7. }",
          ("exited", Nothing, Just 7, Nothing)
        ),
        ( "completed with value",
          runProjectionFixture disabledRuntimeHost "module App::Main { 42. }",
          ("completed", Just "42", Nothing, Just "42")
        ),
        ( "completed without terminal value",
          runProjectionFixture disabledRuntimeHost "module App::Main { answer = 42. }",
          ("completed", Nothing, Nothing, Nothing)
        )
      ]

runResultProjection :: RunResult -> (Text, Maybe Text, Maybe Integer, Maybe Text)
runResultProjection result =
  ( runExecutionTag (runExecution result),
    renderRuntimeValue <$> runRuntimeValue result,
    runExitStatus result,
    runOutput result
  )

runExecutionTag :: RunExecution -> Text
runExecutionTag execution =
  case execution of
    RunNotExecuted -> "not-executed"
    RunRuntimeFailed -> "runtime-failed"
    RunExited _ -> "exited"
    RunCompleted _ -> "completed"

runProjectionFixture :: RuntimeHost IO -> Text -> IO RunResult
runProjectionFixture host source =
  runModuleGraphWithPreludeAndHost
    host
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    (\path -> pure (Map.lookup path sources))
  where
    sources = Map.singleton "src/App/Main.jz" source

assertAbsentCompiledPrelude :: String -> CompiledProgram -> IO ()
assertAbsentCompiledPrelude label compiledProgram =
  case compiledPreludeExpr (compiledProgramPrelude compiledProgram) of
    Nothing -> pure ()
    Just _ -> fail (label <> " unexpectedly compiled a prelude")

observableRuntimeProgram :: RuntimeProgram -> ([Text], [(Text, [(RuntimeExport, Text)])], Maybe Text)
observableRuntimeProgram runtimeProgram =
  ( map (renderModulePath . runtimeModulePath) runtimeModules,
    map renderModuleExports runtimeModules,
    renderRuntimeValue <$> runtimeProgramOutput runtimeProgram
  )
  where
    runtimeModules = runtimeProgramModules runtimeProgram
    renderModuleExports runtimeModule =
      ( renderModulePath (runtimeModulePath runtimeModule),
        [(runtimeExport, renderRuntimeCell cell) | (runtimeExport, cell) <- Map.toAscList (runtimeModuleExports runtimeModule)]
      )

renderModulePath :: [Text] -> Text
renderModulePath = Text.intercalate "::"

renderRuntimeCell :: RuntimeCell -> Text
renderRuntimeCell cell =
  case cell of
    Left diagnostic -> renderDiagnostic diagnostic
    Right value -> renderRuntimeValue value

hostFreeParitySources :: Map.Map FilePath Text
hostFreeParitySources = moduleRuntimeParitySources "(Shared, Other)"

hostCapableParitySources :: Map.Map FilePath Text
hostCapableParitySources =
  moduleRuntimeParitySources
    "if True then (Shared, Other) else (\\(ignored) -> (Shared, Other)) (__kernel_arguments! ())"

moduleRuntimeParitySources :: Text -> Map.Map FilePath Text
moduleRuntimeParitySources entryExpression =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. " <> entryExpression <> ". }"),
      ("src/Lib/Value.jz", "module Lib::Value { data Marker = Shared | Other. }")
    ]

testModuleGraphInjectsRuntimeHost :: IO ()
testModuleGraphInjectsRuntimeHost = do
  callsRef <- newIORef []
  result <-
    runModuleGraphWithPreludeAndHost
      (recordingHost callsRef)
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (\path -> pure (Map.lookup path sources))
  calls <- readIORef callsRef
  assertEqual "host module compile errors" [] (runCompileErrors result)
  assertEqual "host module runtime errors" [] (runRuntimeErrors result)
  assertEqual "host module output" (Just "(True, \"\", \"\", \"\")") (runOutput result)
  assertEqual "host module call order" ["entry"] calls
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main { import Lib::Emit (emit!). emit! \"entry\". }"
          ),
          ( "src/Lib/Emit.jz",
            "module Lib::Emit (emit!) { emit! = \\(contents) -> __kernel_writeStdoutRaw! contents. }"
          )
        ]

recordingHost :: IORef [Text] -> RuntimeHost IO
recordingHost callsRef =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (error "unexpected readText host call"),
      runtimeHostWriteText = \_ _ -> pure (error "unexpected writeText host call"),
      runtimeHostReadStdin = pure (error "unexpected readStdin host call"),
      runtimeHostWriteStdout = \contents -> do
        modifyIORef' callsRef (<> [contents])
        pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (error "unexpected writeStderr host call"),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right RuntimeHostExitReturned)
    }

compileFixtureProgram :: Map.Map FilePath Text -> IO CompiledProgram
compileFixtureProgram sources = do
  resolvedResult <-
    resolveProgramWithAmbientExports
      resolverConfig
      testPrelude
      (exportInventory [])
      (\path -> pure (Map.lookup path sources))
      ["App", "Main"]
  case resolvedResult of
    Left diagnostic -> fail ("resolution failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right resolved -> compileResolvedProgram (emptyCompileInputs defaultWarningSettings) resolved

simpleSources :: Map.Map FilePath Text
simpleSources =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
      ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
    ]

dependencyExpressionSources :: Map.Map FilePath Text
dependencyExpressionSources =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. result. }"),
      ("src/Lib/Value.jz", "module Lib::Value { result = 1. 1 / 0. }")
    ]

testCompiledInterfacesExposeOnlyDeclaredExports :: IO ()
testCompiledInterfacesExposeOnlyDeclaredExports = do
  resolvedResult <-
    resolveProgramWithAmbientExports
      resolverConfig
      testPrelude
      (exportInventory [])
      lookupSource
      ["App", "Main"]
  case resolvedResult of
    Left diagnostic -> fail ("resolution failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right resolved -> do
      compiled <- compileResolvedProgram (emptyCompileInputs defaultWarningSettings) resolved
      case lookupCompiledModule (nominalModulePath ("Lib" :| ["Value"])) compiled of
        Nothing -> fail "missing compiled Lib::Value module"
        Just valueModule ->
          assertEqual
            "exported values"
            (Set.fromList [ModuleExport ValueNamespace "answer"])
            (Map.keysSet (interfaceValueTypes (compiledModuleInterface valueModule)))
      assertEqual "no compile errors" [] (compiledProgramErrors compiled)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testDependencyExpressionContract :: IO ()
testDependencyExpressionContract = do
  result <- runGraph localDependencyExpressionSources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "entry output" (Just "1") (runOutput result)
  where
    localDependencyExpressionSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. result. }"),
          ("src/Lib/Value.jz", "module Lib::Value { result = 1. 1 / 0. }")
        ]

testAliasIsolationContract :: IO ()
testAliasIsolationContract = do
  result <- runGraph sources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value as Value. Value::answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]

testTransitiveVisibilityContract :: IO ()
testTransitiveVisibilityContract = do
  result <- compileGraph sources
  assertEqual "warning count" 0 (length (compileWarnings result))
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "unbound code"
    "E1001"
    (renderFirstCompileError result)
  assertContains
    "alias-hidden transitive export"
    "unbound variable 'subtract'"
    (renderFirstCompileError result)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import App::UsesMath.
            subtract.
            """
          ),
          ( "src/App/UsesMath.jz",
            """
            import Lib::Math as Math.
            use = 0.
            """
          ),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]

testSourcePathContract :: IO ()
testSourcePathContract = do
  result <- compileGraph sources
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "dependency primary source path"
    "src/Lib/Bad.jz:1:1"
    (renderFirstCompileError result)
  assertContains
    "dependency related source path"
    "related src/Lib/Bad.jz:2:1"
    (renderFirstCompileError result)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Bad (x).
            x.
            """
          ),
          ( "src/Lib/Bad.jz",
            """
            x :: Int.
            x = True.
            """
          )
        ]

runGraph :: Map.Map FilePath Text -> IO RunResult
runGraph sources =
  runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

compileGraph :: Map.Map FilePath Text -> IO CompileResult
compileGraph sources =
  compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

renderFirstCompileError :: CompileResult -> Text
renderFirstCompileError result =
  case compileErrors result of
    [] -> "<no compile error>"
    firstError : _ -> renderDiagnostic firstError

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

testPrelude :: PreludeArtifact 'Resolved
testPrelude =
  PreludeArtifact
    { preludeIdentity =
        moduleIdentity
          (nominalModulePath ("Prelude" :| []))
          (mkSourceFile "<module-pipeline-test-prelude>"),
      preludeBuiltinMode = ResolveKernelOnly,
      preludeModule = Nothing
    }

nominalModulePath :: NonEmpty Text -> ModulePath
nominalModulePath = mkModulePath . fmap mkIdentifier
