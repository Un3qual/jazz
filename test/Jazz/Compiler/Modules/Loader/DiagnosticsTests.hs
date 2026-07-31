{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.DiagnosticsTests
  ( diagnosticTests
  ) where

import qualified Data.Map.Strict as Map
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import Jazz.Compiler.Driver
  ( RunResult (..),
    compileErrors,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runModuleGraphWithPrelude,
    runRuntimeErrors,
    runWarnings
  )
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
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

diagnosticTests :: [NamedTest]
diagnosticTests =
  [ ("compile module graph reports unresolved import diagnostics", testCompileModuleGraphUnresolved)
    , ("compile module graph reports ambiguous import diagnostics", testCompileModuleGraphAmbiguousImport)
    , ("compile module graph reports missing import symbols", testCompileModuleGraphMissingImportSymbol)
    , ("compile module graph reports module declaration mismatch diagnostics", testCompileModuleGraphModuleDeclarationMismatch)
    , ("run module graph reports cycle diagnostics", testRunModuleGraphCycle)
  ]

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
        [("src/App/Main.jz", """
        import Missing::Thing.
        1.
        """)]
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
        [ ("rootA/App/Main.jz", """
        import Lib::Util.
        util.
        """),
          ("rootA/Lib/Util.jz", "util = 1."),
          ("rootB/Lib/Util.jz", "util = 2.")
        ]
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
        [ ("src/App/Main.jz", """
        import Lib::Math (subtract).
        1.
        """),
          ("src/Lib/Math.jz", "add = 1.")
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
        [("src/App/Main.jz", """
        module Wrong::Name {
        main = 1.
        }
        """)]
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
        [ ("src/A/One.jz", """
        import B::Two.
        a.
        """),
          ("src/B/Two.jz", """
          import A::One.
          b.
          """),
          ("src/b.jz", "b = 2.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
