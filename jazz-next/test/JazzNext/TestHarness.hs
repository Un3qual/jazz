module JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertJust,
    assertLeftContains,
    assertRight,
    failTest,
    runTestSuite
  )
where

import Control.Exception (Exception, catch, throwIO)
import Data.List (isInfixOf)
import System.Exit (exitFailure, exitSuccess)

newtype TestFailure = TestFailure String

instance Show TestFailure where
  show (TestFailure msg) = msg

instance Exception TestFailure

type NamedTest = (String, IO ())

runTestSuite :: String -> [NamedTest] -> IO ()
runTestSuite suiteName tests = do
  failures <- mapM runNamed tests
  let failed = length (filter id failures)
  if failed == 0
    then do
      putStrLn ("All " ++ suiteName ++ " tests passed.")
      exitSuccess
    else do
      putStrLn (show failed ++ " " ++ suiteName ++ " test(s) failed.")
      exitFailure

runNamed :: NamedTest -> IO Bool
runNamed (name, action) = do
  failed <- (action >> pure False) `catchFailure` \message -> do
    putStrLn ("FAIL: " ++ name ++ "\n  " ++ message)
    pure True
  if not failed then putStrLn ("PASS: " ++ name) else pure ()
  pure failed

catchFailure :: IO a -> (String -> IO a) -> IO a
catchFailure action handler = action `catch` \(TestFailure msg) -> handler msg

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
  if needle `isInfixOf` haystack
    then pure ()
    else failTest (label ++ ": expected to find '" ++ needle ++ "' in '" ++ haystack ++ "'")

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertJust :: String -> Maybe a -> IO ()
assertJust label value =
  case value of
    Just _ -> pure ()
    Nothing -> failTest (label ++ ": expected Just, got Nothing")

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
