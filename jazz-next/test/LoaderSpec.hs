{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileModuleGraphWithPrelude,
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
    ("compile module graph reports unresolved import diagnostics", testCompileModuleGraphUnresolved),
    ("run module graph reports cycle diagnostics", testRunModuleGraphCycle),
    ("loader performs deterministic dependency-first source reads", testDependencyReadOrder)
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
  assertEqual "generated output" (Just "/* jazz-next codegen placeholder */") (generatedJs result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "util = 1.")
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
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "util = 1.")
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
  assertEqual "generated output" Nothing (generatedJs result)
  case compileErrors result of
    [err] -> do
      assertContains "unresolved code" "E4001" err
      assertContains "missing module" "Missing::Thing" err
    _ -> failTest "expected exactly one unresolved import error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "import Missing::Thing.\n1.")]
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
    [err] -> assertContains "cycle code" "E4003" err
    _ -> failTest "expected exactly one cycle error"
  where
    sourceMap =
      Map.fromList
        [ ("src/A/One.jz", "import B::Two.\na."),
          ("src/B/Two.jz", "import A::One.\nb."),
          ("src/b.jz", "b = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testDependencyReadOrder :: IO ()
testDependencyReadOrder = do
  readOrderRef <- newIORef ([] :: [FilePath])
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (lookupSource readOrderRef)
  readOrder <- reverse <$> readIORef readOrderRef
  assertEqual "run succeeds" [] (runCompileErrors result)
  assertEqual
    "dependency-first final replay order"
    ["src/App/Main.jz", "src/Lib/Util.jz", "src/Lib/Util.jz", "src/App/Main.jz"]
    readOrder
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    lookupSource readOrderRef path = do
      current <- readIORef readOrderRef
      writeIORef readOrderRef (path : current)
      pure (Map.lookup path sourceMap)

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }
