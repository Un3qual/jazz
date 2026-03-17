{-# LANGUAGE OverloadedStrings #-}

module JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertDiagnosticContains,
    assertEqual,
    assertJust,
    assertLeftDiagnosticCodeAndContains,
    assertLeftContains,
    assertLeftDiagnosticContains,
    assertRight,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
    assertSingleErrorContains,
    failTest,
    runTestSuite
  )
where

import Control.Exception (Exception, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    RenderDiagnostic,
    SourceSpan,
    diagnosticCode,
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSubject,
    renderDiagnostic
  )
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

assertDiagnosticContains :: Text -> Text -> Diagnostic -> IO ()
assertDiagnosticContains label needle diagnostic =
  assertContains label needle (renderDiagnostic diagnostic)

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

assertLeftContains :: (Show a, RenderDiagnostic e) => Text -> Text -> Either e a -> IO ()
assertLeftContains label needle value =
  case value of
    Left err ->
      let rendered = renderDiagnostic err
       in
        if needle `Text.isInfixOf` rendered
          then pure ()
          else failTest (label <> ": expected error containing '" <> needle <> "', got '" <> rendered <> "'")
    Right ok -> failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

assertRight :: Show e => Text -> Either e a -> (a -> IO ()) -> IO ()
assertRight label value check =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> check ok

assertLeftDiagnosticContains :: Show a => Text -> Text -> Either Diagnostic a -> IO ()
assertLeftDiagnosticContains label needle value =
  case value of
    Left diagnostic ->
      assertDiagnosticContains label needle diagnostic
    Right ok -> failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

assertLeftDiagnosticCodeAndContains :: Show a => Text -> Text -> Text -> Either Diagnostic a -> IO ()
assertLeftDiagnosticCodeAndContains label expectedCode needle value =
  case value of
    Left diagnostic -> do
      assertEqual (label <> " code") expectedCode (diagnosticCode diagnostic)
      assertDiagnosticContains label needle diagnostic
    Right ok -> failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

assertSingleDiagnosticContains :: Text -> Text -> [Diagnostic] -> IO ()
assertSingleDiagnosticContains label needle diagnostics =
  case diagnostics of
    [diagnostic] ->
      assertDiagnosticContains label needle diagnostic
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 diagnostic, got "
            <> Text.pack (show (length diagnostics))
            <> if null diagnostics then "" else ": " <> Text.pack (show diagnostics)
        )

assertSingleDiagnosticCode :: Text -> Text -> [Diagnostic] -> IO ()
assertSingleDiagnosticCode label expectedCode diagnostics =
  case diagnostics of
    [diagnostic] ->
      assertEqual label expectedCode (diagnosticCode diagnostic)
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 diagnostic, got "
            <> Text.pack (show (length diagnostics))
            <> if null diagnostics then "" else ": " <> Text.pack (show diagnostics)
        )

assertSingleDiagnosticPrimarySpan :: Text -> SourceSpan -> [Diagnostic] -> IO ()
assertSingleDiagnosticPrimarySpan label expectedSpan diagnostics =
  case diagnostics of
    [diagnostic] ->
      assertEqual label (Just expectedSpan) (diagnosticPrimarySpan diagnostic)
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 diagnostic, got "
            <> Text.pack (show (length diagnostics))
            <> if null diagnostics then "" else ": " <> Text.pack (show diagnostics)
        )

assertSingleDiagnosticRelatedSpan :: Text -> SourceSpan -> [Diagnostic] -> IO ()
assertSingleDiagnosticRelatedSpan label expectedSpan diagnostics =
  case diagnostics of
    [diagnostic] ->
      assertEqual label (Just expectedSpan) (diagnosticRelatedSpan diagnostic)
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 diagnostic, got "
            <> Text.pack (show (length diagnostics))
            <> if null diagnostics then "" else ": " <> Text.pack (show diagnostics)
        )

assertSingleDiagnosticSubject :: Text -> Text -> [Diagnostic] -> IO ()
assertSingleDiagnosticSubject label expectedSubject diagnostics =
  case diagnostics of
    [diagnostic] ->
      assertEqual label (Just expectedSubject) (diagnosticSubject diagnostic)
    _ ->
      failTest
        ( label
            <> ": expected exactly 1 diagnostic, got "
            <> Text.pack (show (length diagnostics))
            <> if null diagnostics then "" else ": " <> Text.pack (show diagnostics)
        )

assertSingleErrorContains :: Text -> Text -> [Diagnostic] -> IO ()
assertSingleErrorContains = assertSingleDiagnosticContains

failTest :: Text -> IO a
failTest = throwIO . TestFailure
