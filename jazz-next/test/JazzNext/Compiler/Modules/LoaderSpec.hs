{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
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
    ("run module graph ignores dependency expression statements", testRunModuleGraphIgnoresDependencyExpressions),
    ("compile module graph validates dependency expression statements", testCompileModuleGraphValidatesDependencyExpressions),
    ("compile module graph reports unresolved import diagnostics", testCompileModuleGraphUnresolved),
    ("compile module graph reports missing import symbols", testCompileModuleGraphMissingImportSymbol),
    ("compile module graph hides dependency bindings excluded by explicit import list", testCompileModuleGraphExplicitImportListHidesUnlistedBindings),
    ("compile module graph hides dependency bindings imported only by alias", testCompileModuleGraphAliasImportHidesUnqualifiedBindings),
    ("run module graph resolves qualified alias lookup", testRunModuleGraphQualifiedAliasLookup),
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
  assertEqual "generated output" (Just "/* jazz-next codegen placeholder */") (generatedJs result)
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
  assertEqual "generated output" Nothing (generatedJs result)
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
      assertContains "unresolved code" "E4001" (renderDiagnostic err)
      assertContains "missing module" "Missing::Thing" (renderDiagnostic err)
    _ -> failTest "expected exactly one unresolved import error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "import Missing::Thing.\n1.")]
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
  assertEqual "generated output" Nothing (generatedJs result)
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
  assertEqual "generated output" Nothing (generatedJs result)
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
  assertEqual "generated output" Nothing (generatedJs result)
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
  assertEqual "generated output" Nothing (generatedJs result)
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

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }
