{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "CLISpec" tests

tests :: [NamedTest]
tests =
  [ ("parseCliOptions captures warning flags and config path", testParseOptions),
    ("cli run prints warning to stderr while keeping stdout output", testCliWarningOnlyBehavior),
    ("cli run returns non-zero and suppresses stdout when warning promoted", testCliPromotedWarningBehavior),
    ("cli precedence keeps CLI over env over config", testCliPrecedenceBehavior),
    ("cli respects --warnings-config path override", testCliConfigPathOverride),
    ("cli defers source read until after arg validation", testCliDefersSourceReadOnArgError),
    ("cli reports signature type mismatch from source input", testCliReportsSignatureTypeMismatch)
  ]

testParseOptions :: IO ()
testParseOptions = do
  options <-
    case parseCliOptions ["-Wsame-scope-rebinding", "--warnings-config", "config/warnings.txt"] of
      Left err -> failTest ("parseCliOptions failed: " <> err)
      Right parsed -> pure parsed
  assertEqual "warning flags" ["-Wsame-scope-rebinding"] (cliWarningFlags options)
  assertEqual "config path" (Just "config/warnings.txt") (cliWarningsConfigPath options)

testCliWarningOnlyBehavior :: IO ()
testCliWarningOnlyBehavior = do
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertContains "stderr includes warning category" "same-scope-rebinding" (cliStderr output)
  assertContains "stdout includes generated output" "codegen placeholder" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliPromotedWarningBehavior :: IO ()
testCliPromotedWarningBehavior = do
  output <- runCliWith ["-Werror=same-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertContains "stderr includes error marker" "error:" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliPrecedenceBehavior :: IO ()
testCliPrecedenceBehavior = do
  let envMap =
        Map.fromList
          [("JAZZ_WARNING_FLAGS", "-same-scope-rebinding")]
      configMap = Map.fromList [(".jazz-warnings", "same-scope-rebinding")]
      envLookup key = pure (Map.lookup key envMap)
      configLookup key = pure (Map.lookup key configMap)
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "CLI precedence keeps warning enabled" "W0001" (cliStderr output)

testCliConfigPathOverride :: IO ()
testCliConfigPathOverride = do
  let configMap =
        Map.fromList
          [ (".jazz-warnings", ""),
            ("config/warnings.txt", "same-scope-rebinding")
          ]
      envLookup _ = pure Nothing
      configLookup key = pure (Map.lookup key configMap)
  output <- runCliWith ["--warnings-config", "config/warnings.txt"] envLookup configLookup (pure sampleSource)
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "custom config enables warning" "W0001" (cliStderr output)

testCliDefersSourceReadOnArgError :: IO ()
testCliDefersSourceReadOnArgError = do
  sourceRead <- newIORef False
  output <- runCliWith ["--bad-arg"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stderr parse error prefix" "error: unknown argument" (cliStderr output)
  assertEqual "source should not be read when arg parse fails" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

recordSourceRead :: IORef Bool -> IO Text
recordSourceRead sourceRead = do
  writeIORef sourceRead True
  pure sampleSource

testCliReportsSignatureTypeMismatch :: IO ()
testCliReportsSignatureTypeMismatch = do
  output <- runCliWith [] envLookup configLookup (pure signatureMismatchSource)
  assertEqual "exit code" 1 (cliExitCode output)
  assertContains "stderr includes signature mismatch code" "E2005" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

sampleSource :: Text
sampleSource = "x = 1. x = 2."

signatureMismatchSource :: Text
signatureMismatchSource = "x :: Int.\nx = True."
