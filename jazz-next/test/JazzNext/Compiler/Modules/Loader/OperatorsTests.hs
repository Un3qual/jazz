{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.OperatorsTests
  ( operatorTests
  ) where

import qualified Data.Map.Strict as Map
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
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

operatorTests :: [NamedTest]
operatorTests =
  [ ("run module graph retains local operator binding needed by exported binding", testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding)
    , ("run module graph retains local operator signature needed by exported binding", testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding)
    , ("run module graph retains local operator binding needed by explicit imported export", testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport)
    , ("run module graph does not leak retained operator binding into importer", testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter)
    , ("run module graph imported right operator section captures right operand", testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand)
    , ("run module graph ignores hidden operator binding collisions", testRunModuleGraphIgnoresHiddenOperatorBindingCollisions)
  ]

testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding :: IO ()
testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Ops.\nplus.\n}"),
          ("src/Lib/Ops.jz", "module Lib::Ops {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\nplus = 1 %% 2.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding :: IO ()
testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Ops.\nplus.\n}"),
          ("src/Lib/Ops.jz", "module Lib::Ops {\noperator %% tier 2.\n(%%) :: Int -> Int -> Int.\n(%%) = \\(left) -> \\(right) -> left + right.\nplus = 1 %% 2.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport :: IO ()
testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Ops (plus).\nplus.\n}"),
          ("src/Lib/Ops.jz", "module Lib::Ops {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\nplus = 1 %% 2.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter :: IO ()
testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)
  case runCompileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "missing binding code" "E2010" rendered
      assertContains "missing binding operator" "operator '%%' has no executable binding" rendered
    _ -> failTest "expected exactly one missing operator binding error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Ops (plus).\noperator %% tier 2.\nresult = (10 %% 3) + plus.\nresult.\n}"),
          ("src/Lib/Ops.jz", "module Lib::Ops {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\nplus = 1 %% 2.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand :: IO ()
testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime output is suppressed on right operand failure" Nothing (runOutput result)
  case runRuntimeErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "right section capture runtime code" "E3001" rendered
      assertContains "right section capture runtime text" "division by zero" rendered
    _ -> failTest "expected exactly one imported right section runtime error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Ops (section).\nsection.\n}"),
          ("src/Lib/Ops.jz", "module Lib::Ops {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left - right.\nsection = (%% (1 / 0)).\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphIgnoresHiddenOperatorBindingCollisions :: IO ()
testRunModuleGraphIgnoresHiddenOperatorBindingCollisions = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(3, 7)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::A.\nimport Lib::B.\n(a, b).\n}"),
          ("src/Lib/A.jz", "module Lib::A {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\na = 1 %% 2.\n}"),
          ("src/Lib/B.jz", "module Lib::B {\noperator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left * right.\nb = 1 %% 7.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
