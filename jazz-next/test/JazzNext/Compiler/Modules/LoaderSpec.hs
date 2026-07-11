{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.Modules.Loader.Shared
  ( resolverConfig
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.Compiler.Modules.Loader.BasicTests (basicTests)
import JazzNext.Compiler.Modules.Loader.VisibilityTests (visibilityTests)
import JazzNext.Compiler.Modules.Loader.CapabilitiesTests (capabilitiesTests)
import JazzNext.Compiler.Modules.Loader.OperatorsTests (operatorTests)
import JazzNext.Compiler.Modules.Loader.DiagnosticsTests (diagnosticTests)
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)
import System.Directory (doesFileExist)

main :: IO ()
main = runTestSuite "Loader" tests

tests :: [NamedTest]
tests =
  [ ( "checked-in Maybe and Result modules transport generic ADTs through the loader",
      testBootstrapMaybeAndResultModules
    )
  ]
    ++ basicTests
    ++ visibilityTests
    ++ capabilitiesTests
    ++ operatorTests
    ++ diagnosticTests

testBootstrapMaybeAndResultModules :: IO ()
testBootstrapMaybeAndResultModules = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(41, 1)") (runOutput result)
  where
    entrySource =
      "module App::Main {\n"
        <> "import Maybe.\n"
        <> "import Result.\n"
        <> "maybeValue :: Maybe(Int).\n"
        <> "maybeValue = Just 41.\n"
        <> "resultValue :: Result(Text, Int).\n"
        <> "resultValue = Ok 1.\n"
        <> "maybeNumber = case maybeValue { | Nothing -> 0 | Just value -> value }.\n"
        <> "resultNumber = case resultValue { | Err message -> 0 | Ok value -> value }.\n"
        <> "(maybeNumber, resultNumber).\n"
        <> "}"
    lookupSource "src/App/Main.jz" = pure (Just entrySource)
    lookupSource "src/Maybe.jz" = readStdlibSource "Maybe.jz"
    lookupSource "src/Result.jz" = readStdlibSource "Result.jz"
    lookupSource _ = pure Nothing

readStdlibSource :: FilePath -> IO (Maybe Text)
readStdlibSource fileName =
  readFirstExisting
    [ "stdlib/" <> fileName,
      "jazz-next/stdlib/" <> fileName
    ]

readFirstExisting :: [FilePath] -> IO (Maybe Text)
readFirstExisting [] = pure Nothing
readFirstExisting (candidate : rest) = do
  exists <- doesFileExist candidate
  if exists
    then Just <$> TextIO.readFile candidate
    else readFirstExisting rest
