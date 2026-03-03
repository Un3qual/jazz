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
    run "resolveWarningSettings handles standalone -Werror=<category>" testCliPromoteCategoryStandalone,
    run "resolveWarningSettings promotes enabled warnings via -Werror" testPromoteAllEnabledToError,
    run "resolveWarningSettings applies CLI > env > config > default" testPrecedenceOrder,
    run "resolveWarningSettings applies env error directives after env warning directives" testEnvErrorOverridesEnvWarning,
    run "resolveWarningSettings defaults to all warnings disabled" testDefaultDisabled,
    run "resolveWarningSettings rejects malformed env warning token list" testMalformedEnvWarningTokenList,
    run "resolveWarningSettings fails on unknown CLI category" testUnknownCliCategory,
    run "resolveWarningSettings fails on unknown env category" testUnknownEnvCategory,
    run "resolveWarningSettings fails on unknown config category" testUnknownConfigCategory
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

testCliPromoteCategoryStandalone :: IO ()
testCliPromoteCategoryStandalone =
  assertRight
    "standalone -Werror promotion"
    (resolveWarningSettings ["-Werror=same-scope-rebinding"] Nothing Nothing Nothing)
    (\settings -> do
        assertWarningState settings True True
        assertCategoryState settings ShadowingOuterScope False False
        assertCategoryState settings UnusedBinding False False
        assertCategoryState settings DeprecatedSyntax False False
    )

testPromoteAllEnabledToError :: IO ()
testPromoteAllEnabledToError =
  assertRight
    "-Werror promotes enabled warnings"
    (resolveWarningSettings ["-Wsame-scope-rebinding", "-Werror"] Nothing Nothing Nothing)
    (\settings -> assertWarningState settings True True)

testEnvErrorOverridesEnvWarning :: IO ()
testEnvErrorOverridesEnvWarning =
  assertRight
    "env error overrides env warning for same category"
    ( resolveWarningSettings
        []
        (Just "-same-scope-rebinding")
        (Just "same-scope-rebinding")
        Nothing
    )
    (\settings -> assertWarningState settings True True)

testDefaultDisabled :: IO ()
testDefaultDisabled =
  assertRight
    "default config"
    (resolveWarningSettings [] Nothing Nothing Nothing)
    (\settings -> assertWarningState settings False False)

testMalformedEnvWarningTokenList :: IO ()
testMalformedEnvWarningTokenList =
  assertLeftContains
    "malformed env warning token list"
    "empty warning token"
    (resolveWarningSettings [] (Just "same-scope-rebinding,,unused-binding") Nothing Nothing)

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

assertCategoryState :: WarningSettings -> WarningCategory -> Bool -> Bool -> IO ()
assertCategoryState settings category expectedEnabled expectedError = do
  assertEqual
    ("enabled state for " ++ show category)
    expectedEnabled
    (isWarningEnabled settings category)
  assertEqual
    ("error state for " ++ show category)
    expectedError
    (isWarningError settings category)
