{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalSourcePath,
    normalizeCanonicalSourcePath,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( canonicalizeSourceResult,
    renderCanonicalSourceResult,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgramTokensDetailed,
  )
import JazzNext.Compiler.Parser.Lexer
  ( tokenizeDetailed,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

main :: IO ()
main = runTestSuite "JazzParserControlFlowPatterns" tests

tests :: [NamedTest]
tests =
  [ ("parses identifier lambdas", assertStage0Parity "identifier lambda" "f = \\(x) -> x."),
    ("parses pattern lambdas", assertStage0Parity "pattern lambda" "f = \\([head | tail]) -> head."),
    ("rejects guarded lambda alternatives", assertStage0Parity "guarded lambda" "f = \\(Just item | Also item if ok) -> item."),
    ("parses nested conditionals", assertStage0Parity "nested conditional" "x = if cond then if inner then a else b else c."),
    ("rejects missing conditional else branches", assertStage0Parity "missing else" "x = if cond then value."),
    ("parses case patterns and guards", assertStage0Parity "guarded case" "x = case value { | Just item | Also item if ok -> item | Nothing -> 0 }."),
    ("preserves nested case and lambda bodies", assertStage0Parity "nested case lambda" "x = case value { | Just item -> \\(next) -> next | _ -> 0 }."),
    ("parses recursive control flow in blocks", assertStage0Parity "recursive block" "x = { loop = \\(value) -> case value { | Just next -> loop next | _ -> if False then value else value }. loop. }.")
  ]

assertStage0Parity :: Text.Text -> Text.Text -> IO ()
assertStage0Parity label source = do
  path <- canonicalPath
  let expected =
        renderCanonicalSourceResult
          ( canonicalizeSourceResult
              path
              ( case tokenizeDetailed source of
                  Left failure -> Left failure
                  Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)
              )
          )
      expression = "parseSource componentPath " <> Text.pack (show source)
  assertJazzOutput label expression expected

canonicalPath :: IO CanonicalSourcePath
canonicalPath =
  case normalizeCanonicalSourcePath "fixtures/parser/control-flow-patterns.jz" of
    Left message -> failTest message
    Right path -> pure path

assertJazzOutput :: Text.Text -> Text.Text -> Text.Text -> IO ()
assertJazzOutput label expression expected = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      (lookupSource expression)
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

lookupSource :: Text.Text -> FilePath -> IO (Maybe Text.Text)
lookupSource expression sourcePath =
  case sourcePath of
    "src/App/Main.jz" ->
      pure
        ( Just
            ( Text.replace
                "__EXPRESSION__"
                expression
                """
                module App::Main {
                  import LexerTypes.
                  import Parser (parseSource).
                  componentPath = CanonicalSourcePath "fixtures/parser/control-flow-patterns.jz".
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
