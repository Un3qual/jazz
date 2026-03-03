module Main (main) where

import Control.Exception (Exception, catch, throwIO)
import Data.List (isInfixOf)
import JazzNext.Compiler.WarningConfig
  ( WarningDirective (..),
    WarningSettings,
    isWarningEnabled,
    isWarningError,
    parseCliWarningDirective,
    resolveWarningSettings
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    parseWarningCategory
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
      putStrLn "All WarningConfig tests passed."
      exitSuccess
    else do
      putStrLn (show failed ++ " WarningConfig test(s) failed.")
      exitFailure

tests :: [IO Bool]
tests =
  [ run "parseWarningCategory accepts known category" testParseWarningCategoryKnown,
    run "parseWarningCategory rejects unknown category" testParseWarningCategoryUnknown,
    run "parseCliWarningDirective parses all phase-1 forms" testParseCliWarningDirectiveForms,
    run "resolveWarningSettings applies CLI > env > config > default" testPrecedenceOrder,
    run "resolveWarningSettings defaults to all warnings disabled" testDefaultDisabled,
    run "resolveWarningSettings fails on unknown CLI category" testUnknownCliCategory,
    run "resolveWarningSettings fails on unknown env category" testUnknownEnvCategory,
    run "resolveWarningSettings fails on unknown config category" testUnknownConfigCategory
  ]

run :: String -> IO () -> IO Bool
run name action = do
  result <- (action >> pure False) `catchFailure` \message -> do
    putStrLn ("FAIL: " ++ name ++ "\n  " ++ message)
    pure True
  if not result then putStrLn ("PASS: " ++ name) else pure ()
  pure result

catchFailure :: IO a -> (String -> IO a) -> IO a
catchFailure action handler = action `catch` \(TestFailure msg) -> handler msg

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertLeftContains :: Show a => String -> String -> Either String a -> IO ()
assertLeftContains label needle value =
  case value of
    Left err
      | needle `isInfixOf` err -> pure ()
      | otherwise -> failTest (label ++ ": expected error containing '" ++ needle ++ "', got '" ++ err ++ "'")
    Right ok -> failTest (label ++ ": expected Left, got Right " ++ show ok)

assertRight :: Show e => String -> Either e a -> (a -> IO ()) -> IO ()
assertRight label value check =
  case value of
    Left err -> failTest (label ++ ": expected Right, got Left " ++ show err)
    Right ok -> check ok

failTest :: String -> IO a
failTest = throwIO . TestFailure

testParseWarningCategoryKnown :: IO ()
testParseWarningCategoryKnown =
  assertEqual
    "same-scope-rebinding"
    (Right SameScopeRebinding)
    (parseWarningCategory "same-scope-rebinding")

testParseWarningCategoryUnknown :: IO ()
testParseWarningCategoryUnknown =
  assertLeftContains "unknown category" "unknown warning category" (parseWarningCategory "nope")

testParseCliWarningDirectiveForms :: IO ()
testParseCliWarningDirectiveForms = do
  assertEqual
    "-Wsame-scope-rebinding"
    (Right (EnableCategory SameScopeRebinding))
    (parseCliWarningDirective "-Wsame-scope-rebinding")
  assertEqual
    "-Wno-same-scope-rebinding"
    (Right (DisableCategory SameScopeRebinding))
    (parseCliWarningDirective "-Wno-same-scope-rebinding")
  assertEqual
    "-Werror=same-scope-rebinding"
    (Right (PromoteCategoryToError SameScopeRebinding))
    (parseCliWarningDirective "-Werror=same-scope-rebinding")
  assertEqual
    "-Werror"
    (Right PromoteAllEnabledToError)
    (parseCliWarningDirective "-Werror")
  assertEqual
    "-Wnone"
    (Right DisableAllCategories)
    (parseCliWarningDirective "-Wnone")

testPrecedenceOrder :: IO ()
testPrecedenceOrder =
  assertRight
    "resolve precedence"
    ( resolveWarningSettings
        ["-Wsame-scope-rebinding"]
        (Just "-same-scope-rebinding")
        Nothing
        (Just "same-scope-rebinding")
    )
    (\settings -> assertWarningState settings True False)

testDefaultDisabled :: IO ()
testDefaultDisabled =
  assertRight
    "default config"
    (resolveWarningSettings [] Nothing Nothing Nothing)
    (\settings -> assertWarningState settings False False)

testUnknownCliCategory :: IO ()
testUnknownCliCategory =
  assertLeftContains
    "unknown cli category"
    "unknown warning category"
    (resolveWarningSettings ["-Wnot-real"] Nothing Nothing Nothing)

testUnknownEnvCategory :: IO ()
testUnknownEnvCategory =
  assertLeftContains
    "unknown env category"
    "unknown warning category"
    (resolveWarningSettings [] (Just "not-real") Nothing Nothing)

testUnknownConfigCategory :: IO ()
testUnknownConfigCategory =
  assertLeftContains
    "unknown config category"
    "unknown warning category"
    (resolveWarningSettings [] Nothing Nothing (Just "not-real\n"))

assertWarningState :: WarningSettings -> Bool -> Bool -> IO ()
assertWarningState settings expectedEnabled expectedError = do
  assertEqual
    "enabled state"
    expectedEnabled
    (isWarningEnabled settings SameScopeRebinding)
  assertEqual
    "error state"
    expectedError
    (isWarningError settings SameScopeRebinding)
