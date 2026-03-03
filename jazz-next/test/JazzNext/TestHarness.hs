{-# LANGUAGE OverloadedStrings #-}

module JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertJust,
    assertLeftContains,
    assertRight,
    assertSingleErrorContains,
    failTest,
    runTestSuite
  )
where

import Control.Exception (Exception, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)

newtype TestFailure = TestFailure Text

instance Show TestFailure where
  show (TestFailure msg) = Text.unpack msg

instance Exception TestFailure

type NamedTest = (Text, IO ())

runTestSuite :: Text -> [NamedTest] -> IO ()
runTestSuite suiteName tests = do
  failures <- mapM runNamed tests
  let failed = length (filter id failures)
  if failed == 0
    then do
      putStrLn ("All " <> Text.unpack suiteName <> " tests passed.")
      exitSuccess
    else do
      putStrLn (show failed <> " " <> Text.unpack suiteName <> " test(s) failed.")
      exitFailure

runNamed :: NamedTest -> IO Bool
runNamed (name, action) = do
  failed <- (action >> pure False) `catchFailure` \message -> do
    putStrLn ("FAIL: " <> Text.unpack name <> "\n  " <> Text.unpack message)
    pure True
  if not failed then putStrLn ("PASS: " <> Text.unpack name) else pure ()
  pure failed

catchFailure :: IO a -> (Text -> IO a) -> IO a
catchFailure action handler = action `catch` \(TestFailure msg) -> handler msg

assertContains :: Text -> Text -> Text -> IO ()
assertContains label needle haystack =
  if needle `Text.isInfixOf` haystack
    then pure ()
    else failTest (label <> ": expected to find '" <> needle <> "' in '" <> haystack <> "'")

assertEqual :: (Eq a, Show a) => Text -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else
      failTest
        ( label
            <> ": expected "
            <> Text.pack (show expected)
            <> ", got "
            <> Text.pack (show actual)
        )

assertJust :: Text -> Maybe a -> IO ()
assertJust label value =
  case value of
    Just _ -> pure ()
    Nothing -> failTest (label <> ": expected Just, got Nothing")

assertLeftContains :: Show a => Text -> Text -> Either Text a -> IO ()
assertLeftContains label needle value =
  case value of
    Left err
      | needle `Text.isInfixOf` err -> pure ()
      | otherwise -> failTest (label <> ": expected error containing '" <> needle <> "', got '" <> err <> "'")
    Right ok -> failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

assertRight :: Show e => Text -> Either e a -> (a -> IO ()) -> IO ()
assertRight label value check =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> check ok

assertSingleErrorContains :: Text -> Text -> [Text] -> IO ()
assertSingleErrorContains label needle errors =
  case errors of
    [err] ->
      if needle `Text.isInfixOf` err
        then pure ()
        else failTest (label <> ": expected to find '" <> needle <> "' in '" <> err <> "'")
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 error, got "
            <> Text.pack (show (length errors))
            <> if null errors then "" else ": " <> Text.pack (show errors)
        )

failTest :: Text -> IO a
failTest = throwIO . TestFailure
