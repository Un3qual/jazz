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
  [ ("parses direct pattern forms", testDirectPatternForms),
    ("preserves direct pattern failures", testDirectPatternFailures),
    ("keeps pattern alternatives context specific", testPatternBoundaries),
    ("parses identifier lambdas", assertStage0Parity "identifier lambda" "f = \\(x) -> x."),
    ("parses pattern lambdas", assertStage0Parity "pattern lambda" "f = \\([head | tail]) -> head."),
    ("rejects guarded lambda alternatives", assertStage0Parity "guarded lambda" "f = \\(Just item | Also item if ok) -> item."),
    ("parses nested conditionals", assertStage0Parity "nested conditional" "x = if cond then if inner then a else b else c."),
    ("rejects missing conditional else branches", assertStage0Parity "missing else" "x = if cond then value."),
    ("parses case patterns and guards", assertStage0Parity "guarded case" "x = case value { | Just item | Also item if ok -> item | Nothing -> 0 }."),
    ("preserves nested case and lambda bodies", assertStage0Parity "nested case lambda" "x = case value { | Just item -> \\(next) -> next | _ -> 0 }."),
    ("parses recursive control flow in blocks", assertStage0Parity "recursive block" "x = { loop = \\(value) -> case value { | Just next -> loop next | _ -> if False then value else value }. loop. }.")
  ]

testDirectPatternForms :: IO ()
testDirectPatternForms =
  assertJazzOutput
    "direct pattern forms"
    """
    ( parseComponentPattern "0"
    , parseComponentPattern "'x'"
    , parseComponentPattern "\\\"text\\\""
    , parseComponentPattern "_"
    , parseComponentPattern "value"
    , parseComponentPattern "True"
    , parseComponentPattern "False"
    , parseComponentPattern "Nothing"
    , parseComponentPattern "Just item"
    , parseComponentPattern "[]"
    , parseComponentPattern "[head, tail]"
    , parseComponentPattern "[head | tail]"
    , parseComponentPattern "()"
    , parseComponentPattern "(left, right)"
    , parseComponentPattern "whole@[head | tail]"
    , parseComponentArmPattern "Just item | Nothing"
    , parseComponentLambdaParameter "item"
    , parseComponentLambdaParameter "[head | tail]"
    )
    """
    "(TokenParseSucceeded(LiteralPattern(IntegerLiteral(\"0\"))), TokenParseSucceeded(LiteralPattern(CharacterLiteral('x'))), TokenParseSucceeded(LiteralPattern(TextLiteral(\"text\"))), TokenParseSucceeded(WildcardPattern), TokenParseSucceeded(VariablePattern(\"value\")), TokenParseSucceeded(LiteralPattern(BooleanLiteral(True))), TokenParseSucceeded(LiteralPattern(BooleanLiteral(False))), TokenParseSucceeded(ConstructorPattern(\"Nothing\", [])), TokenParseSucceeded(ConstructorPattern(\"Just\", [VariablePattern(\"item\")])), TokenParseSucceeded(ListPattern([])), TokenParseSucceeded(ListPattern([VariablePattern(\"head\"), VariablePattern(\"tail\")])), TokenParseSucceeded(ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\"))), TokenParseSucceeded(TuplePattern([])), TokenParseSucceeded(TuplePattern([VariablePattern(\"left\"), VariablePattern(\"right\")])), TokenParseSucceeded(AsPattern(\"whole\", ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\")))), TokenParseSucceeded(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])])), TokenParseSucceeded(IdentifierParameter(\"item\")), TokenParseSucceeded(PatternParameter(ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\")))))"

testDirectPatternFailures :: IO ()
testDirectPatternFailures =
  assertJazzOutput
    "direct pattern failures"
    """
    ( parseComponentPattern "1.5"
    , parseComponentPattern "[head, tail | rest]"
    , parseComponentPattern "(item)"
    , parseComponentPattern "+"
    , parseComponentLambdaParameter "if"
    )
    """
    "(TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), UnsupportedSyntax(FractionalLiteralPattern))), TokenParseRejected(ParserGrammarFailure(Nothing, PatternFailure(ConsLikeListPatternHeadCount))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 6)), ExpectedSyntax(\"\\',\\'\", FoundToken(PunctuationKind(RightParenPunctuation), \")\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), ExpectedSyntax(\"case pattern\", FoundToken(OperatorKind(\"+\"), \"+\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), ExpectedSyntax(\"identifier\", FoundToken(KeywordKind(IfKeyword), \"if\")))))"

testPatternBoundaries :: IO ()
testPatternBoundaries =
  assertJazzOutput
    "pattern boundaries"
    """
    ( parseComponentPattern "Just item | Nothing"
    , parseComponentArmPattern "Just item | Nothing"
    , parseComponentLambdaParameter "Just item | Nothing"
    , parseComponentArmPattern "(Just item | Nothing)"
    , parseComponentArmPattern "Just item if"
    , parseComponentArmPattern "Just item ->"
    , parseComponentLambdaParameter "item if"
    )
    """
    "(TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(OperatorKind(\"|\"), \"|\"), \"end of input\"))), TokenParseSucceeded(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])])), TokenParseSucceeded(PatternParameter(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])]))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 12)), ExpectedSyntax(\"\\',\\'\", FoundToken(OperatorKind(\"|\"), \"|\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(KeywordKind(IfKeyword), \"if\"), \"end of input\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(PunctuationKind(ArrowPunctuation), \"->\"), \"end of input\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 6)), UnexpectedSyntax(FoundToken(KeywordKind(IfKeyword), \"if\"), \"end of input\"))))"

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
                  import Lexer (lexSource).
                  import Parser (parseSource).
                  import ParserPattern (parseCasePattern, parseCaseArmPattern, parseLambdaParameter).
                  import ParserToken (tokenRunComplete).
                  componentPath = CanonicalSourcePath "fixtures/parser/control-flow-patterns.jz".
                  expressionTokens = \\(source) -> case lexSource componentPath source {
                    | CanonicalLexSuccess path tokens -> tokens
                  }.
                  parseComponentPattern = \\(source) -> tokenRunComplete parseCasePattern (expressionTokens source).
                  parseComponentArmPattern = \\(source) -> tokenRunComplete parseCaseArmPattern (expressionTokens source).
                  parseComponentLambdaParameter = \\(source) -> tokenRunComplete parseLambdaParameter (expressionTokens source).
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
