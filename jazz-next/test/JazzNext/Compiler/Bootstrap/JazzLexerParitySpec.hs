{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( canonicalizeLexResult,
    normalizeCanonicalSourcePath,
    renderCanonicalLexResult
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraphWithPrelude
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
import System.Directory (doesFileExist)

main :: IO ()
main = runTestSuite "JazzLexerParity" tests

tests :: [NamedTest]
tests =
  [ ("Jazz lexer renders exact canonical tokens", testExactCanonicalTokens),
    ("Jazz lexer renders exact structured failures", testExactStructuredFailure),
    ("Jazz lexer covers every focused boundary family", testFocusedBoundaryCorpus)
  ]

testExactCanonicalTokens :: IO ()
testExactCanonicalTokens = assertJazzParity "fixtures/lexer/basic.jz" "module value = 00042."

testExactStructuredFailure :: IO ()
testExactStructuredFailure = assertJazzParity "fixtures/lexer/error.jz" "value ` 42."

testFocusedBoundaryCorpus :: IO ()
testFocusedBoundaryCorpus = assertJazzCorpusParity (take 21 parserFixtureCorpus)

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
  expected <- mapM expectedFixture fixtures
  result <- runJazzLexerBatch fixtures
  assertEqual "Jazz corpus compile errors" [] (runCompileErrors result)
  assertEqual "Jazz corpus runtime errors" [] (runRuntimeErrors result)
  assertEqual
    "canonical corpus rendering"
    (Just ("[" <> Text.intercalate ", " expected <> "]"))
    (runOutput result)
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
        "src/Lexer.jz" -> readStdlibSource "Lexer.jz"
        "src/LexerTypes.jz" -> readStdlibSource "LexerTypes.jz"
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
        "src/Lexer.jz" -> readStdlibSource "Lexer.jz"
        "src/LexerTypes.jz" -> readStdlibSource "LexerTypes.jz"
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
readStdlibSource fileName = readFirstExisting ["jazz-next/stdlib/" <> fileName, "stdlib/" <> fileName]

readFirstExisting :: [FilePath] -> IO (Maybe Text)
readFirstExisting candidates =
  case candidates of
    [] -> pure Nothing
    candidate : rest -> do
      exists <- doesFileExist candidate
      if exists then Just <$> TextIO.readFile candidate else readFirstExisting rest

fromString :: FilePath -> Text
fromString = Text.pack
