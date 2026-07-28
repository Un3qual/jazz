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
    ("parses multiple lambda parameters", assertStage0Parity "multiple lambda parameters" "f = \\(x, y) -> x."),
    ("parses literal and constructor lambda parameters", assertStage0Parity "literal constructor lambda parameters" "f = \\(0, 'x', \"text\", True, _, Nothing) -> item."),
    ("parses composite lambda parameters", assertStage0Parity "composite lambda parameters" "f = \\([head, tail], (), (left, right), whole@[head | tail]) -> head."),
    ("parses lambda alternative parameters", assertStage0Parity "lambda alternative parameter" "f = \\(Just item | Nothing) -> item."),
    ("parses nested recursive lambda bodies", assertStage0Parity "nested recursive lambda" "f = \\(x) -> \\(y) -> f x y."),
    ("parses unit lambda parameters", assertStage0Parity "unit lambda parameter" "f = \\() -> 0."),
    ("preserves lambda delimiter ownership", assertStage0Parity "lambda delimiters" "xs = [\\(x) -> x, \\(y) -> y]."),
    ("rejects missing lambda arrows", assertStage0Parity "missing lambda arrow" "f = \\(x) x."),
    ("rejects trailing lambda commas", assertStage0Parity "trailing lambda comma" "f = \\(x,) -> x."),
    ("rejects bare lambda parameters", assertStage0Parity "bare lambda parameter" "f = \\x -> x."),
    ("rejects guarded lambda alternatives", assertStage0Parity "guarded lambda" "f = \\(Just item | Also item if ok) -> item."),
    ("parses basic conditionals", assertStage0Parity "basic conditional" "x = if True then 1 else 2."),
    ("parses nested conditionals", assertStage0Parity "nested conditional" "x = if cond then if inner then a else b else c."),
    ("parses conditional block branches", assertStage0Parity "conditional blocks" "x = if cond then { y = 1. y. } else { z = 2. z. }."),
    ("preserves conditional outer delimiters", assertStage0Parity "conditional delimiters" "xs = [if cond then 1 else 2, if other then 3 else 4]."),
    ("rejects missing conditional conditions", assertStage0Parity "missing condition" "x = if then 1 else 2."),
    ("rejects missing conditional then delimiters", assertStage0Parity "missing then" "x = if cond 1 else 2."),
    ("rejects missing conditional true branches", assertStage0Parity "missing true branch" "x = if cond then else 2."),
    ("rejects missing conditional else branches", assertStage0Parity "missing else" "x = if cond then item."),
    ("rejects missing conditional false branches", assertStage0Parity "missing false branch" "x = if cond then 1 else ."),
    ("rejects extra conditional else delimiters", assertStage0Parity "extra else" "x = if cond then 1 else 2 else 3."),
    ("preserves reserved conditional binding failures", assertStage0Parity "reserved conditional binding" "if = 1."),
    ("parses basic cases", assertStage0Parity "basic case" "x = case item { | Just item -> item | Nothing -> 0 }."),
    ("parses type applications in case scrutinees", assertStage0Parity "case type application scrutinee" "x = case id @Int item { | _ -> item }."),
    ("parses control flow in case scrutinees", assertStage0Parity "case control-flow scrutinee" "x = case if cond then a else b { | _ -> b }."),
    ("parses block-valued case scrutinees", assertStage0Parity "case block scrutinee" "x = case { f = \\(x) -> x. f. } { | _ -> 0 }."),
    ("parses case patterns and guards", assertStage0Parity "guarded case" "x = case item { | Just item | Also item if ok -> item | Nothing -> 0 }."),
    ("starts literal or-pattern arms after pipe bodies", assertStage0Parity "literal or-pattern after pipe body" "x = case item { | _ -> 1 | 2 | 3 | 4 -> 5 }."),
    ("keeps conditionals on case-body pipe right-hand sides", assertStage0Parity "conditional pipe right-hand side" "x = case item { | _ -> left | Just if cond then a else b | Next -> c }."),
    ("rechecks case-arm boundaries after pipe-rooted guards", assertStage0Parity "pipe-rooted guard boundary" "x = case item { | item if a | b | _ -> 1 }."),
    ("keeps pipes inside conditional case-arm branches", assertStage0Parity "conditional branch pipe" "x = case item { | _ -> if cond then a | b else c }."),
    ("keeps pipes inside lambda case-arm bodies", assertStage0Parity "lambda body pipe" "x = case item { | _ -> \\(item) -> item | fallback }."),
    ("parses case block bodies", assertStage0Parity "case block body" "x = case item { | Just item -> { y = item. y. } | Nothing -> 0 }."),
    ("rejects missing case scrutinee braces", assertStage0Parity "missing case brace" "x = case item."),
    ("rejects missing first case pipes", assertStage0Parity "missing first case pipe" "x = case item { item -> item }."),
    ("rejects empty case arms", assertStage0Parity "empty case arms" "x = case item {}."),
    ("rejects missing case guard expressions", assertStage0Parity "missing case guard" "x = case item { | item if -> item }."),
    ("rejects second case guards", assertStage0Parity "second case guard" "x = case item { | item if ok if other -> item }."),
    ("rejects missing case arm arrows", assertStage0Parity "missing case arm arrow" "x = case item { | item item }."),
    ("rejects missing case arm bodies", assertStage0Parity "missing case arm body" "x = case item { | item -> }."),
    ("rejects missing closing case braces", assertStage0Parity "missing closing case brace" "x = case item { | item -> item."),
    ("preserves nested case and lambda bodies", assertStage0Parity "nested case lambda" "x = case item { | Just item -> \\(next) -> next | _ -> 0 }."),
    ("parses recursive control flow in blocks", assertStage0Parity "recursive block" "x = { loop = \\(item) -> case item { | Just next -> loop next | _ -> if False then item else item }. loop. }.")
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
    , parseComponentPattern "item"
    , parseComponentPattern "(item)"
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
    "(TokenParseSucceeded(LiteralPattern(IntegerLiteral(\"0\"))), TokenParseSucceeded(LiteralPattern(CharacterLiteral('x'))), TokenParseSucceeded(LiteralPattern(TextLiteral(\"text\"))), TokenParseSucceeded(WildcardPattern), TokenParseSucceeded(VariablePattern(\"item\")), TokenParseSucceeded(VariablePattern(\"item\")), TokenParseSucceeded(LiteralPattern(BooleanLiteral(True))), TokenParseSucceeded(LiteralPattern(BooleanLiteral(False))), TokenParseSucceeded(ConstructorPattern(\"Nothing\", [])), TokenParseSucceeded(ConstructorPattern(\"Just\", [VariablePattern(\"item\")])), TokenParseSucceeded(ListPattern([])), TokenParseSucceeded(ListPattern([VariablePattern(\"head\"), VariablePattern(\"tail\")])), TokenParseSucceeded(ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\"))), TokenParseSucceeded(TuplePattern([])), TokenParseSucceeded(TuplePattern([VariablePattern(\"left\"), VariablePattern(\"right\")])), TokenParseSucceeded(AsPattern(\"whole\", ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\")))), TokenParseSucceeded(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])])), TokenParseSucceeded(IdentifierParameter(\"item\")), TokenParseSucceeded(PatternParameter(ConsListPattern(VariablePattern(\"head\"), VariablePattern(\"tail\")))))"

testDirectPatternFailures :: IO ()
testDirectPatternFailures =
  assertJazzOutput
    "direct pattern failures"
    """
    ( parseComponentPattern "1.5"
    , parseComponentPattern "[head, tail | rest]"
    , parseComponentPattern "+"
    , parseComponentLambdaParameter "if"
    )
    """
    "(TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), UnsupportedSyntax(FractionalLiteralPattern))), TokenParseRejected(ParserGrammarFailure(Nothing, PatternFailure(ConsLikeListPatternHeadCount))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), ExpectedSyntax(\"case pattern\", FoundToken(OperatorKind(\"+\"), \"+\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 1)), ExpectedSyntax(\"identifier\", FoundToken(KeywordKind(IfKeyword), \"if\")))))"

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
    "(TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(OperatorKind(\"|\"), \"|\"), \"end of input\"))), TokenParseSucceeded(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])])), TokenParseSucceeded(PatternParameter(OrPattern([ConstructorPattern(\"Just\", [VariablePattern(\"item\")]), ConstructorPattern(\"Nothing\", [])]))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 12)), ExpectedSyntax(\"\\')\\'\", FoundToken(OperatorKind(\"|\"), \"|\")))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(KeywordKind(IfKeyword), \"if\"), \"end of input\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 11)), UnexpectedSyntax(FoundToken(PunctuationKind(ArrowPunctuation), \"->\"), \"end of input\"))), TokenParseRejected(ParserGrammarFailure(Just(CanonicalSpan(1, 6)), UnexpectedSyntax(FoundToken(KeywordKind(IfKeyword), \"if\"), \"end of input\"))))"

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
