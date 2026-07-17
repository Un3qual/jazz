{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

main :: IO ()
main = runTestSuite "JazzParserTypesDeclarationsModules" tests

tests :: [NamedTest]
tests =
  [ ("snapshots the remaining canonical tokens without consumption", testTokenRemaining),
    ("transitions immutable parser context across statement scopes", testContextTransitions),
    ("accepts immutable context at the expression parser seam", testContextAwareExpression),
    ("preserves landed binding and expression statement dispatch", testFoundationalDispatch)
  ]

testTokenRemaining :: IO ()
testTokenRemaining =
  assertJazzOutput
    "remaining tokens"
    """
    tokenRun
      (tokenAndThen
        (\\(before) -> tokenAndThen
          (\\(taken) -> tokenTransform (\\(after) -> (before, taken, after)) tokenRemaining)
          tokenIdentifier)
        tokenRemaining)
      [ CanonicalToken (IdentifierKind "value") "value" (CanonicalSpan 1 1)
      , CanonicalToken (PunctuationKind DotPunctuation) "." (CanonicalSpan 1 6)
      ]
    """
    "ParserSucceeded(([CanonicalToken(IdentifierKind(\"value\"), \"value\", CanonicalSpan(1, 1)), CanonicalToken(PunctuationKind(DotPunctuation), \".\", CanonicalSpan(1, 6))], CanonicalToken(IdentifierKind(\"value\"), \"value\", CanonicalSpan(1, 1)), [CanonicalToken(PunctuationKind(DotPunctuation), \".\", CanonicalSpan(1, 6))]), ParserCursor([CanonicalToken(PunctuationKind(DotPunctuation), \".\", CanonicalSpan(1, 6))], 1), Consumed)"

testContextTransitions :: IO ()
testContextTransitions =
  assertJazzOutput
    "context transitions"
    """
    { top = parserContextRegisterAlias parserContextInitial "Outer".
      moduleBody = parserContextModuleBody top.
      nested = parserContextNestedBlock top.
      ( parserContextStatement top
      , parserContextStatement moduleBody
      , parserContextStatement nested
      , parserContextHasAlias moduleBody "Outer"
      , parserContextHasAlias nested "Outer"
      ).
    }
    """
    "(TopLevelContext, ModuleBodyContext, NestedBlockContext, False, True)"

testContextAwareExpression :: IO ()
testContextAwareExpression =
  assertJazzOutput
    "context-aware expression"
    """
    tokenRunComplete
      (parseFoundationalExpressionWithContext parserContextInitial (tokenSucceed []))
      (expressionTokens "value")
    """
    "TokenParseSucceeded(VariableExpression(\"value\"))"

testFoundationalDispatch :: IO ()
testFoundationalDispatch =
  assertJazzOutput
    "foundational dispatch"
    """
    ( parseSource componentPath "value = 1. value."
    , parseSource componentPath "{} ."
    )
    """
    "(CanonicalSourceSuccess(CanonicalSourcePath(\"fixtures/parser/types-declarations-modules.jz\"), BlockExpression([LetStatement(\"value\", CanonicalSpan(1, 1), LiteralExpression(IntegerLiteral(\"1\"))), ExpressionStatement(CanonicalSpan(1, 12), VariableExpression(\"value\"))])), CanonicalSourceSuccess(CanonicalSourcePath(\"fixtures/parser/types-declarations-modules.jz\"), BlockExpression([ExpressionStatement(CanonicalSpan(1, 1), BlockExpression([]))])))"

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
                  import Lexer (lexSource).
                  import LexerTypes.
                  import Parser (parseSource).
                  import ParserContext.
                  import ParserDeclaration (parseStatementWithContext).
                  import ParserExpression (parseFoundationalExpressionWithContext).
                  import ParserToken.
                  componentPath = CanonicalSourcePath "fixtures/parser/types-declarations-modules.jz".
                  expressionTokens = \\(source) -> case lexSource componentPath source {
                    | CanonicalLexSuccess path tokens -> tokens
                  }.
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
