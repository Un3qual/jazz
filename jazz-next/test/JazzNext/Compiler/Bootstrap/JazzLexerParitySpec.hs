{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( canonicalizeLexResult,
    normalizeCanonicalSourcePath,
    renderCanonicalLexResult
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraphWithPrelude,
    runRuntimeErrors
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
  )
import JazzNext.Compiler.Parser.Lexer (tokenizeDetailed)
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    parserFixtureCorpus
  )
import JazzNext.Compiler.Runtime (RuntimeValue (VText), renderRuntimeValue)
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite
  )
import JazzNext.TestSource
  ( JazzSourceRole (..),
    readCheckedInJazzSource,
  )
import System.Environment (lookupEnv)
import System.Timeout (timeout)

main :: IO ()
main = runTestSuite "JazzLexerParity" tests

tests :: [NamedTest]
tests =
  [ ("Jazz lexer renders exact canonical tokens", testExactCanonicalTokens),
    ("Jazz lexer renders then as a canonical keyword", testExactThenKeyword),
    ("Jazz lexer renders exact structured failures", testExactStructuredFailure),
    ("Jazz lexer covers every focused boundary family", testFocusedBoundaryCorpus),
    ("Jazz lexer matches the complete canonical corpus deterministically", testCompleteCorpusParity),
    ("Jazz lexer traverses large whitespace and token inputs stack safely", testLargeTraversal),
    ("Jazz lexer handles long token and quoted-literal runs within the traversal budget", testLongRuns),
    ("Jazz lexer timeout wrapper preserves timeout classification", testTimeoutClassification)
  ]

testExactCanonicalTokens :: IO ()
testExactCanonicalTokens = assertJazzParity "fixtures/lexer/basic.jz" "module value = 00042."

testExactThenKeyword :: IO ()
testExactThenKeyword =
  assertJazzParity
    "fixtures/lexer/then-keyword.jz"
    "if condition then yes else no"

testExactStructuredFailure :: IO ()
testExactStructuredFailure = assertJazzParity "fixtures/lexer/error.jz" "value ` 42."

testFocusedBoundaryCorpus :: IO ()
testFocusedBoundaryCorpus = assertJazzCorpusParity (take 21 parserFixtureCorpus)

testCompleteCorpusParity :: IO ()
testCompleteCorpusParity = do
  expected <- expectedCorpusRendering parserFixtureCorpus
  first <- runJazzLexerBatch parserFixtureCorpus
  second <- runJazzLexerBatch parserFixtureCorpus
  assertEqual "complete corpus first compile errors" [] (runCompileErrors first)
  assertEqual "complete corpus first runtime errors" [] (runRuntimeErrors first)
  assertEqual "complete corpus second compile errors" [] (runCompileErrors second)
  assertEqual "complete corpus second runtime errors" [] (runRuntimeErrors second)
  assertEqual "complete corpus deterministic rendering" (runOutput first) (runOutput second)
  assertEqual "complete corpus stage-0 parity" (Just expected) (runOutput first)

testLargeTraversal :: IO ()
testLargeTraversal = do
  assertLargeTokenCount "large whitespace" (Text.replicate 20000 " ") 0
  assertLargeTokenCount "large token list" (Text.replicate 10000 "x ") 10000

testLongRuns :: IO ()
testLongRuns = do
  interpretedRun <- lookupEnv "JAZZ_NEXT_RUNGHC_IN_CABAL"
  let runLength = maybe 200000 (const 20000) interpretedRun
  assertLongRunParity "long identifier" (Text.replicate runLength "x")
  assertLongRunParity "long quoted text" ("\"" <> Text.replicate runLength "x" <> "\"")

testTimeoutClassification :: IO ()
testTimeoutClassification = do
  result <- tryWithin 1000 (threadDelay 1000000)
  case result of
    Right Nothing -> pure ()
    Left err -> failTest ("timeout leaked host exception: " <> Text.pack (show err))
    Right (Just ()) -> failTest "timeout action unexpectedly completed"

assertLargeTokenCount :: Text -> Text -> Int -> IO ()
assertLargeTokenCount label source expectedCount = do
  timedResult <- tryWithin 60000000 (runJazzLexerCount source)
  case timedResult of
    Right Nothing -> failTest (label <> " timed out")
    Left err -> failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Right (Just result) -> do
      assertEqual (label <> " compile errors") [] (runCompileErrors result)
      assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
      assertEqual (label <> " token count") (Just (Text.pack (show expectedCount))) (runOutput result)

assertLongRunParity :: Text -> Text -> IO ()
assertLongRunParity label source = do
  timedResult <- tryWithin 30000000 (assertJazzParity "fixtures/lexer/long-run.jz" source)
  case timedResult of
    Right Nothing -> failTest (label <> " timed out")
    Left err -> failTest (label <> " leaked host exception: " <> Text.pack (show err))
    Right (Just ()) -> pure ()

tryWithin :: Int -> IO a -> IO (Either SomeException (Maybe a))
tryWithin microseconds action = try (timeout microseconds action)

assertJazzParity :: FilePath -> Text -> IO ()
assertJazzParity logicalPath source = do
  expectedPath <-
    case normalizeCanonicalSourcePath logicalPath of
      Left err -> failTest ("invalid test path: " <> err)
      Right path -> pure path
  result <- runJazzLexer logicalPath source
  assertEqual "Jazz compile errors" [] (runCompileErrors result)
  assertEqual "Jazz runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "canonical rendering"
    (Just (renderCanonicalLexResult (canonicalizeLexResult expectedPath (tokenizeDetailed source))))
    (runOutput result)

assertJazzCorpusParity :: [ParserFixture] -> IO ()
assertJazzCorpusParity fixtures = do
  expected <- expectedCorpusRendering fixtures
  result <- runJazzLexerBatch fixtures
  assertEqual "Jazz corpus compile errors" [] (runCompileErrors result)
  assertEqual "Jazz corpus runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "canonical corpus rendering"
    (Just expected)
    (runOutput result)

expectedCorpusRendering :: [ParserFixture] -> IO Text
expectedCorpusRendering fixtures = do
  expected <- mapM expectedFixture fixtures
  pure ("[" <> Text.intercalate ", " expected <> "]")
  where
    expectedFixture fixture = do
      path <-
        case normalizeCanonicalSourcePath (parserFixturePath fixture) of
          Left err -> failTest ("invalid fixture path: " <> err)
          Right normalized -> pure normalized
      pure (renderCanonicalLexResult (canonicalizeLexResult path (tokenizeDetailed (parserFixtureSource fixture))))

runJazzLexer :: FilePath -> Text -> IO RunResult
runJazzLexer logicalPath source =
  runModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    -- Explicit fragments are intentional: this program embeds runtime fixture data.
    entrySource =
      "module App::Main {\n"
        <> "  import Lexer.\n"
        <> "  import LexerTypes.\n"
        <> "  lexSource (CanonicalSourcePath "
        <> renderRuntimeValue (VText (fromString logicalPath))
        <> ") "
        <> renderRuntimeValue (VText source)
        <> ".\n"
        <> "}"
    lookupSource path =
      case path of
        "src/App/Main.jz" -> pure (Just entrySource)
        "src/Lexer.jz" -> readCompilerSource "Lexer.jz"
        "src/LexerTypes.jz" -> readCompilerSource "LexerTypes.jz"
        "src/List.jz" -> readStdlibSource "List.jz"
        "src/Char.jz" -> readStdlibSource "Char.jz"
        "src/Text.jz" -> readStdlibSource "Text.jz"
        "src/Maybe.jz" -> readStdlibSource "Maybe.jz"
        _ -> pure Nothing

runJazzLexerBatch :: [ParserFixture] -> IO RunResult
runJazzLexerBatch fixtures =
  runModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    -- Explicit fragments are intentional: this program embeds runtime fixture data.
    entrySource =
      "module App::Main {\n"
        <> "  import Lexer.\n"
        <> "  import LexerTypes.\n"
        <> "  ["
        <> Text.intercalate ",\n   " (map renderFixture fixtures)
        <> "].\n"
        <> "}"
    renderFixture fixture =
      "lexSource (CanonicalSourcePath "
        <> renderRuntimeValue (VText (fromString (parserFixturePath fixture)))
        <> ") "
        <> renderRuntimeValue (VText (parserFixtureSource fixture))
    lookupSource path =
      case path of
        "src/App/Main.jz" -> pure (Just entrySource)
        "src/Lexer.jz" -> readCompilerSource "Lexer.jz"
        "src/LexerTypes.jz" -> readCompilerSource "LexerTypes.jz"
        "src/List.jz" -> readStdlibSource "List.jz"
        "src/Char.jz" -> readStdlibSource "Char.jz"
        "src/Text.jz" -> readStdlibSource "Text.jz"
        "src/Maybe.jz" -> readStdlibSource "Maybe.jz"
        _ -> pure Nothing

runJazzLexerCount :: Text -> IO RunResult
runJazzLexerCount source =
  runModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    -- Explicit fragments are intentional: this program embeds runtime fixture data.
    entrySource =
      "module App::Main {\n"
        <> "  import Lexer.\n"
        <> "  import LexerTypes.\n"
        <> "  import List.\n"
        <> "  case lexSource (CanonicalSourcePath \"fixtures/lexer/large.jz\") "
        <> renderRuntimeValue (VText source)
        <> " {\n"
        <> "    | CanonicalLexFailure _ _ -> 999999\n"
        <> "    | CanonicalLexSuccess _ tokens -> listLength tokens\n"
        <> "  }.\n"
        <> "}"
    lookupSource path =
      case path of
        "src/App/Main.jz" -> pure (Just entrySource)
        "src/Lexer.jz" -> readCompilerSource "Lexer.jz"
        "src/LexerTypes.jz" -> readCompilerSource "LexerTypes.jz"
        "src/List.jz" -> readStdlibSource "List.jz"
        "src/Char.jz" -> readStdlibSource "Char.jz"
        "src/Text.jz" -> readStdlibSource "Text.jz"
        "src/Maybe.jz" -> readStdlibSource "Maybe.jz"
        _ -> pure Nothing

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

readStdlibSource :: FilePath -> IO (Maybe Text)
readStdlibSource fileName =
  Just <$> readCheckedInJazzSource StandardLibrarySource fileName

readCompilerSource :: FilePath -> IO (Maybe Text)
readCompilerSource fileName =
  Just <$> readCheckedInJazzSource CompilerSource fileName

fromString :: FilePath -> Text
fromString = Text.pack
