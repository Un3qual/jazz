{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Expr (..),
    Literal (..),
    SignatureType (..),
    Statement (..)
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import Jazz.Compiler.Driver
  ( CompileResult,
    RunResult (..),
    compileErrors,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runRuntimeErrors
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.ModuleResolver (resolveProgram)
import Jazz.Compiler.ModuleCompiler
  ( compileResolvedModule,
    compileResolvedProgram
  )
import Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (runtimeModuleExports, runtimeModulePath),
    RuntimeProgram (runtimeProgramModules, runtimeProgramOutput),
    evaluateCompiledProgram,
    evaluateCompiledProgramWithHost,
    lookupRuntimeModule
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost (..),
    RuntimeHostExit (..)
  )
import Jazz.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledProgram (..),
    ModuleInterface (..),
    compiledProgramErrors,
    emptyCompiledPrelude,
    emptyCompileInputs,
    emptyModuleInterface,
    lookupCompiledModule
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    ResolvedImport (..),
    ResolvedModule (..)
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Name
  ( Name (BuiltinName),
    NameNamespace (ConstructorNamespace, TypeNamespace, ValueNamespace),
    identifierText,
    mkIdentifier,
    resolvedImportedName,
    resolvedLocalName
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (TIntType, TTextType),
    TypeBinding (PlainTypeBinding)
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite
  )
import System.Timeout (timeout)

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
    ("module graph execution carries one host through dependency exports", testModuleGraphInjectsRuntimeHost),
    ("long compiled dependency chains preserve pure runtime behavior", testLongCompiledDependencyChainPure),
    ("long compiled dependency chains preserve host runtime behavior", testLongCompiledDependencyChainHost),
    ("duplicate compiled module paths preserve first-match imports and lookup", testDuplicateCompiledModulePathsPreserveFirstMatch),
    ("module compilation preserves first-match dependency lookup", testCompileResolvedModulePreservesFirstDependency),
    ("alias imports stay qualified", testAliasIsolationContract),
    ("transitive imports do not leak", testTransitiveVisibilityContract),
    ("module diagnostics retain source paths", testSourcePathContract),
    ("lexical binders shadow imported and builtin names", testLexicalBindersShadowImportedAndBuiltinNames)
  ]

testCompiledGenericConstructorFieldsRemainModuleStable :: IO ()
testCompiledGenericConstructorFieldsRemainModuleStable = do
  compiled <- compileFixtureProgram sources
  case lookupCompiledModule ["Lib", "Box"] compiled of
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

testDuplicateCompiledModulePathsPreserveFirstMatch :: IO ()
testDuplicateCompiledModulePathsPreserveFirstMatch =
  case evaluateCompiledProgram duplicatePathProgram of
    Left diagnostic -> fail ("duplicate-path program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime -> do
      assertEqual
        "duplicate-path entry output"
        (Just "(\"first\", \"first\")")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)
      assertEqual
        "duplicate-path module order"
        [duplicatePath, middlePath, duplicatePath, ["App", "Main"]]
        (map runtimeModulePath (runtimeProgramModules runtime))
      case lookupRuntimeModule duplicatePath runtime of
        Nothing -> fail "missing first duplicate runtime module"
        Just runtimeModule ->
          assertEqual
            "public lookup keeps first duplicate"
            (Set.singleton (RuntimeBindingExport firstExport))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    duplicatePath = ["Lib", "Duplicate"]
    middlePath = ["Middle"]
    firstExport = ModuleExport ValueNamespace "value"
    middleExport = ModuleExport ValueNamespace "middle"
    secondExport = ModuleExport ValueNamespace "other"
    firstModule =
      compiledTextBindingModule duplicatePath [] firstExport (ELit (LText "first"))
    middleModule =
      compiledTextBindingModule
        middlePath
        [chainImport duplicatePath]
        middleExport
        (EVar (resolvedImportedName duplicatePath ValueNamespace (mkIdentifier "value")))
    secondModule =
      compiledTextBindingModule duplicatePath [] secondExport (ELit (LText "second"))
    entryStatements =
      [ SExpr
          (SourceSpan 1 1)
          ( ETuple
              [ EVar (resolvedImportedName duplicatePath ValueNamespace (mkIdentifier "value")),
                EVar (resolvedImportedName middlePath ValueNamespace (mkIdentifier "middle"))
              ]
          )
      ]
    entryModule =
      compiledModule
        ["App", "Main"]
        [chainImport duplicatePath, chainImport middlePath]
        entryStatements
        (exportInventory [])
        emptyModuleInterface
    duplicatePathProgram =
      CompiledProgram
        { compiledProgramPrelude = emptyCompiledPrelude,
          compiledProgramEntryPath = ["App", "Main"],
          compiledProgramModules = [firstModule, middleModule, secondModule, entryModule],
          compiledProgramDiagnostics = []
        }

testCompileResolvedModulePreservesFirstDependency :: IO ()
testCompileResolvedModulePreservesFirstDependency = do
  compiled <-
    compileResolvedModule
      (emptyCompileInputs defaultWarningSettings)
      [firstDependency, secondDependency]
      targetModule
  assertEqual
    "first dependency interface wins"
    (Just (PlainTypeBinding TTextType))
    (Map.lookup targetExport (interfaceValueTypes (compiledModuleInterface compiled)))
  where
    dependencyPath = ["Lib", "Duplicate"]
    dependencyExport = ModuleExport ValueNamespace "value"
    dependencyInventory = exportInventory [dependencyExport]
    firstDependency =
      compiledModule
        dependencyPath
        []
        [SLet (resolvedLocalName ValueNamespace (mkIdentifier "value")) (SourceSpan 1 1) (ELit (LText "first"))]
        dependencyInventory
        (emptyModuleInterface {interfaceValueTypes = Map.singleton dependencyExport (PlainTypeBinding TTextType)})
    secondDependency =
      compiledModule
        dependencyPath
        []
        [SLet (resolvedLocalName ValueNamespace (mkIdentifier "value")) (SourceSpan 1 1) (ELit (LInt 2))]
        dependencyInventory
        (emptyModuleInterface {interfaceValueTypes = Map.singleton dependencyExport (PlainTypeBinding TIntType)})
    targetExport = ModuleExport ValueNamespace "copied"
    targetImport = chainImport dependencyPath
    targetExpr =
      EBlock
        [ SLet
            (resolvedLocalName ValueNamespace (mkIdentifier "copied"))
            (SourceSpan 1 1)
            (EVar (resolvedImportedName dependencyPath ValueNamespace (mkIdentifier "value")))
        ]
    targetModule =
      ResolvedModule
        { resolvedModulePath = ["App", "Main"],
          resolvedSourcePath = "<module-index-test>",
          resolvedModuleImports = [targetImport],
          resolvedModuleExportInventory = exportInventory [targetExport],
          resolvedModuleCore = CoreModule (Just ["App", "Main"]) Nothing [targetImport] targetExpr
        }

compiledTextBindingModule :: [Text] -> [ResolvedImport] -> ModuleExport -> Expr -> CompiledModule
compiledTextBindingModule path imports moduleExport valueExpr =
  compiledModule
    path
    imports
    [ SLet
        (resolvedLocalName ValueNamespace (mkIdentifier (moduleExportName moduleExport)))
        (SourceSpan 1 1)
        valueExpr
    ]
    (exportInventory [moduleExport])
    ( emptyModuleInterface
        { interfaceValueTypes = Map.singleton moduleExport (PlainTypeBinding TTextType)
        }
    )

testLongCompiledDependencyChainPure :: IO ()
testLongCompiledDependencyChainPure = do
  let moduleCount = 12000
      compiled = compiledChainProgram moduleCount False
  outcome <- timeout 15000000 (evaluatePureChain compiled moduleCount)
  case outcome of
    Nothing -> fail "pure compiled dependency chain timed out"
    Just () -> pure ()

testLongCompiledDependencyChainHost :: IO ()
testLongCompiledDependencyChainHost = do
  callsRef <- newIORef []
  let moduleCount = 6000
      compiled = compiledChainProgram moduleCount True
      host = (recordingHost callsRef) {runtimeHostArguments = modifyIORef' callsRef (<> ["arguments"]) >> pure []}
  outcome <- timeout 15000000 (evaluateHostChain host compiled moduleCount)
  case outcome of
    Nothing -> fail "host compiled dependency chain timed out"
    Just () -> pure ()
  calls <- readIORef callsRef
  assertEqual "host chain calls" ["arguments"] calls

evaluatePureChain :: CompiledProgram -> Int -> IO ()
evaluatePureChain compiled moduleCount =
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail ("pure chain failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime -> assertChainRuntime runtime moduleCount

evaluateHostChain :: RuntimeHost IO -> CompiledProgram -> Int -> IO ()
evaluateHostChain host compiled moduleCount = do
  result <- evaluateCompiledProgramWithHost host compiled
  case result of
    Left diagnostic -> fail ("host chain failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime -> assertChainRuntime runtime moduleCount

assertChainRuntime :: RuntimeProgram -> Int -> IO ()
assertChainRuntime runtime moduleCount = do
  assertEqual
    "dependency order"
    (map chainPath [0 .. moduleCount - 1] <> [["App", "Main"]])
    (map runtimeModulePath (runtimeProgramModules runtime))
  assertEqual "entry output" (Just "\"chain-value\"") (renderRuntimeValue <$> runtimeProgramOutput runtime)
  case lookupRuntimeModule (chainPath (moduleCount `div` 2)) runtime of
    Nothing -> fail "missing middle runtime dependency"
    Just runtimeModule ->
      assertEqual
        "middle dependency export"
        (Set.singleton (RuntimeBindingExport chainExport))
        (Map.keysSet (runtimeModuleExports runtimeModule))

compiledChainProgram :: Int -> Bool -> CompiledProgram
compiledChainProgram moduleCount requiresHost =
  CompiledProgram
    { compiledProgramPrelude = emptyCompiledPrelude,
      compiledProgramEntryPath = ["App", "Main"],
      compiledProgramModules = map chainDependency [0 .. moduleCount - 1] <> [chainEntry requiresHost moduleCount],
      compiledProgramDiagnostics = []
    }

chainDependency :: Int -> CompiledModule
chainDependency index =
  compiledModule path imports statements chainInventory chainInterface
  where
    path = chainPath index
    imports = if index == 0 then [] else [chainImport (chainPath (index - 1))]
    valueExpr =
      if index == 0
        then ELit (LText "chain-value")
        else EVar (resolvedImportedName (chainPath (index - 1)) ValueNamespace (mkIdentifier "value"))
    statements = [SLet (resolvedLocalName ValueNamespace (mkIdentifier "value")) (SourceSpan 1 1) valueExpr]

chainEntry :: Bool -> Int -> CompiledModule
chainEntry requiresHost moduleCount =
  compiledModule ["App", "Main"] [chainImport dependencyPath] statements (exportInventory []) emptyModuleInterface
  where
    dependencyPath = chainPath (moduleCount - 1)
    importedValue = EVar (resolvedImportedName dependencyPath ValueNamespace (mkIdentifier "value"))
    hostResultName = resolvedLocalName ValueNamespace (mkIdentifier "host-result")
    hostStatements =
      [ SLet
          hostResultName
          (SourceSpan 1 1)
          ( EApply
              (EVar (BuiltinName (mkIdentifier "__kernel_arguments!")))
              (ETuple [])
          )
      | requiresHost
      ]
    entryValue =
      if requiresHost
        then
          EApply
            (ELambda (resolvedLocalName ValueNamespace (mkIdentifier "ignored-host-result")) importedValue)
            (EVar hostResultName)
        else importedValue
    statements = hostStatements <> [SExpr (SourceSpan 2 1) entryValue]

compiledModule :: [Text] -> [ResolvedImport] -> [Statement] -> ModuleExportInventory -> ModuleInterface -> CompiledModule
compiledModule path imports statements inventory moduleInterface =
  CompiledModule
    { compiledResolvedModule =
        ResolvedModule
          { resolvedModulePath = path,
            resolvedSourcePath = "<runtime-chain>",
            resolvedModuleImports = imports,
            resolvedModuleExportInventory = inventory,
            resolvedModuleCore = CoreModule (Just path) Nothing imports (EBlock statements)
          },
      compiledModuleInterface = moduleInterface,
      compiledModuleDiagnostics = [],
      compiledModuleExpr = EBlock statements
    }

chainImport :: [Text] -> ResolvedImport
chainImport path = ResolvedImport (SourceSpan 1 1) path Nothing Nothing

chainPath :: Int -> [Text]
chainPath index = ["Chain", Text.pack (show index)]

chainExport :: ModuleExport
chainExport = ModuleExport ValueNamespace "value"

chainInventory :: ModuleExportInventory
chainInventory = exportInventory [chainExport]

chainInterface :: ModuleInterface
chainInterface =
  emptyModuleInterface
    { interfaceValueTypes = Map.singleton chainExport (PlainTypeBinding TTextType)
    }

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
          ("src/Lib/Value.jz", """
          module Lib::Value {
            x = 99.
          }
          """)
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
  case lookupCompiledModule ["Lib", "Value"] compiled of
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
            (ModuleGraph.resolvedModuleExportInventory (compiledResolvedModule valueModule))
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

explicitCapabilitySources :: Map.Map FilePath Text
explicitCapabilitySources =
  Map.fromList
    [ ("src/App/Main.jz", """
    module App::Main {
    import Lib::Facts (Eq).
    Eq::equals 1 1.
    }
    """),
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
  case lookupCompiledModule ["Lib", "Maybe"] compiled of
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
                    (\runtimeExport _ ->
                       case runtimeExport of
                         RuntimeBindingExport moduleExport -> moduleExportName moduleExport == "Just"
                         RuntimeCapabilityMethodExport {} -> False)
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
  case lookupCompiledModule ["Lib", "Choice"] compiled of
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
              (ModuleGraph.resolvedModuleExportInventory (compiledResolvedModule choiceModule))
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
    resolveProgram
      resolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
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
    resolveProgram
      resolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["App", "Main"]
  case resolvedResult of
    Left diagnostic -> fail ("resolution failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right resolved -> do
      compiled <- compileResolvedProgram (emptyCompileInputs defaultWarningSettings) resolved
      case lookupCompiledModule ["Lib", "Value"] compiled of
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
        [ ("src/App/Main.jz", """
        import Lib::Bad (x).
        x.
        """),
          ("src/Lib/Bad.jz", """
          x :: Int.
          x = True.
          """)
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
