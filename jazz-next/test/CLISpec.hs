module Main (main) where

import Control.Exception (Exception, catch, throwIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (isInfixOf)
import JazzNext.CLI.Main
  ( CliOptions (..),
    CliOutput (..),
    parseCliOptions,
    runCliWith
  )
import JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import System.Exit (exitFailure, exitSuccess)

newtype TestFailure = TestFailure String

instance Show TestFailure where
  show (TestFailure msg) = msg

instance Exception TestFailure

main :: IO ()
main = do
  failures <- sequence tests
  let failed = length (filter id failures)
  if failed == 0
    then do
      putStrLn "All CLISpec tests passed."
      exitSuccess
    else do
      putStrLn (show failed ++ " CLISpec test(s) failed.")
      exitFailure

tests :: [IO Bool]
tests =
  [ run "parseCliOptions captures warning flags and config path" testParseOptions,
    run "cli run prints warning to stderr while keeping stdout output" testCliWarningOnlyBehavior,
    run "cli run returns non-zero and suppresses stdout when warning promoted" testCliPromotedWarningBehavior,
    run "cli precedence keeps CLI over env over config" testCliPrecedenceBehavior,
    run "cli respects --warnings-config path override" testCliConfigPathOverride
  ]

run :: String -> IO () -> IO Bool
run name action = do
  failed <- (action >> pure False) `catchFailure` \message -> do
    putStrLn ("FAIL: " ++ name ++ "\n  " ++ message)
    pure True
  if not failed then putStrLn ("PASS: " ++ name) else pure ()
  pure failed

catchFailure :: IO a -> (String -> IO a) -> IO a
catchFailure action handler = action `catch` \(TestFailure msg) -> handler msg

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
  if needle `isInfixOf` haystack
    then pure ()
    else failTest (label ++ ": expected to find '" ++ needle ++ "' in '" ++ haystack ++ "'")

failTest :: String -> IO a
failTest = throwIO . TestFailure

testParseOptions :: IO ()
testParseOptions = do
  options <-
    case parseCliOptions ["-Wsame-scope-rebinding", "--warnings-config", "config/warnings.txt"] of
      Left err -> failTest ("parseCliOptions failed: " ++ err)
      Right parsed -> pure parsed
  assertEqual "warning flags" ["-Wsame-scope-rebinding"] (cliWarningFlags options)
  assertEqual "config path" (Just "config/warnings.txt") (cliWarningsConfigPath options)

testCliWarningOnlyBehavior :: IO ()
testCliWarningOnlyBehavior = do
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup sampleProgram
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "stderr includes warning code" "W0001" (cliStderr output)
  assertContains "stderr includes warning category" "same-scope-rebinding" (cliStderr output)
  assertContains "stdout includes generated output" "codegen placeholder" (cliStdout output)
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing

testCliPromotedWarningBehavior :: IO ()
testCliPromotedWarningBehavior = do
  output <- runCliWith ["-Werror=same-scope-rebinding"] envLookup configLookup sampleProgram
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
  output <- runCliWith ["-Wsame-scope-rebinding"] envLookup configLookup sampleProgram
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
  output <- runCliWith ["--warnings-config", "config/warnings.txt"] envLookup configLookup sampleProgram
  assertEqual "exit code" 0 (cliExitCode output)
  assertContains "custom config enables warning" "W0001" (cliStderr output)

sampleProgram :: Expr
sampleProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SLet "x" (SourceSpan 2 1) (EInt 2)
    ]
