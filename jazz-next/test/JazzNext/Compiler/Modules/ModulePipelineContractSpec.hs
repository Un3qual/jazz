{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics (renderDiagnostic)
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileModuleGraphWithPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.ModuleResolver (resolveProgram)
import JazzNext.Compiler.ModuleCompiler (compileResolvedProgram)
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (compiledModuleInterface),
    CompiledProgram (compiledProgramErrors),
    ModuleInterface (interfaceValueTypes),
    emptyCompileInputs,
    lookupCompiledModule
  )
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModulePipelineContract" tests

tests :: [NamedTest]
tests =
  [ ("dependency expressions are checked but not executed", testDependencyExpressionContract),
    ("compiled interfaces expose only declared exports", testCompiledInterfacesExposeOnlyDeclaredExports),
    ("alias imports stay qualified", testAliasIsolationContract),
    ("transitive imports do not leak", testTransitiveVisibilityContract),
    ("module diagnostics retain source paths", testSourcePathContract)
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
        Just compiledModule ->
          assertEqual
            "exported values"
            (Set.fromList ["answer"])
            (Map.keysSet (interfaceValueTypes (compiledModuleInterface compiledModule)))
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
  result <- runGraph dependencyExpressionSources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "entry output" (Just "1") (runOutput result)
  where
    dependencyExpressionSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. value. }"),
          ("src/Lib/Value.jz", "module Lib::Value { value = 1. 1 / 0. }")
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
    (renderDiagnostic (head (compileErrors result)))
  assertContains
    "alias-hidden transitive export"
    "unbound variable 'subtract'"
    (renderDiagnostic (head (compileErrors result)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import App::UsesMath.\nsubtract."),
          ("src/App/UsesMath.jz", "import Lib::Math as Math.\nuse = 0."),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]

testSourcePathContract :: IO ()
testSourcePathContract = do
  result <- compileGraph sources
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "dependency primary source path"
    "src/Lib/Bad.jz:1:1"
    (renderDiagnostic (head (compileErrors result)))
  assertContains
    "dependency related source path"
    "related src/Lib/Bad.jz:2:1"
    (renderDiagnostic (head (compileErrors result)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Bad (x).\nx."),
          ("src/Lib/Bad.jz", "x :: Int.\nx = True.")
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

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
