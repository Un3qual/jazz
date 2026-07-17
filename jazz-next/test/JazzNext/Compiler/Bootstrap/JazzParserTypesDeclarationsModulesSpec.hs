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
main = runTestSuite "JazzParserTypesDeclarationsModules" tests

tests :: [NamedTest]
tests =
  [ ("snapshots the remaining canonical tokens without consumption", testTokenRemaining),
    ("transitions immutable parser context across statement scopes", testContextTransitions),
    ("accepts immutable context at the expression parser seam", testContextAwareExpression),
    ("preserves landed binding and expression statement dispatch", testFoundationalDispatch),
    ("constructs the recursive signature parser lazily", testDirectSignatureType),
    ("matches stage 0 signature type and fallback behavior", testSignatureParity),
    ("matches stage 0 explicit type application behavior", testExplicitTypeApplicationParity)
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

testDirectSignatureType :: IO ()
testDirectSignatureType =
  assertJazzOutput
    "direct signature type parser"
    "tokenRunComplete parseSignatureType (expressionTokens \"Int\")"
    "TokenParseSucceeded(IntType)"

testSignatureParity :: IO ()
testSignatureParity =
  mapM_
    (uncurry assertStage0Parity)
    [ ( "primitive, width, named, and variable types",
        "integer :: Int. floating :: Float. boolean :: Bool. character :: Char. text :: Text. i8 :: Int8. i16 :: Int16. i32 :: Int32. i64 :: Int64. u8 :: UInt8. u16 :: UInt16. u32 :: UInt32. u64 :: UInt64. f16 :: Float16. f32 :: Float32. f64 :: Float64. variable :: a. named :: Result."
      ),
      ( "applications, list normalization, tuples, unit, and functions",
        "maybe :: Maybe(Char). left :: List(a). right :: [a]. pair :: (Int, Bool). unit :: (). apply :: (Int -> Int) -> Int -> Text."
      ),
      ( "qualified signature type",
        "qualified :: Alias::Result."
      ),
      ( "non-empty and empty constraints",
        "constrained :: @{Eq(a), Ord(List(a))}: a -> List(a). empty :: @{}: ()."
      ),
      ( "qualified constraint statement boundary",
        "constrained :: @{Eq(a), Alias::Ord(List(a))}: a -> List(a)."
      ),
      ( "unsupported forall token fallback",
        "x :: forall a. x = 1."
      ),
      ( "missing signature terminator before matching binding",
        "x :: Int x = 1."
      ),
      ( "compact and nested signature integration",
        "value::Int. { nested::Maybe(Int). }."
      ),
      ( "matching binding disambiguates constructor-shaped signature",
        "Result::value. Result = 1."
      ),
      ( "unmatched constructor-shaped payload remains a qualified expression",
        "Result::value. Other = 1."
      ),
      ( "statement boundary inside compact signature",
        "Result::a Other = 0. Result = 1."
      )
    ]

testExplicitTypeApplicationParity :: IO ()
testExplicitTypeApplicationParity =
  mapM_
    (uncurry assertStage0Parity)
    [ ("primitive type application", "value = id @Int 1. value."),
      ("applied type application", "value = id @Maybe(Int) value. value."),
      ("chained type application", "value = id @Maybe(Int) @List(Text) value. value."),
      ("spaced application delimiter", "value = id @Maybe (Int). value."),
      ("missing type application argument", "value = id @ 1. value."),
      ("empty applied type argument list", "value = id @Maybe(). value.")
    ]

assertStage0Parity :: Text.Text -> Text.Text -> IO ()
assertStage0Parity label source = do
  path <- canonicalPath
  let expected =
        renderCanonicalSourceResult
          ( canonicalizeSourceResult
              path
              (case tokenizeDetailed source of
                Left failure -> Left failure
                Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)
              )
          )
      expression = "parseSource componentPath " <> Text.pack (show source)
  assertJazzOutput label expression expected

canonicalPath :: IO CanonicalSourcePath
canonicalPath =
  case normalizeCanonicalSourcePath "fixtures/parser/types-declarations-modules.jz" of
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
                  import Lexer (lexSource).
                  import LexerTypes.
                  import Parser (parseSource).
                  import ParserContext.
                  import ParserDeclaration (parseStatementWithContext).
                  import ParserExpression (parseFoundationalExpressionWithContext).
                  import ParserSignature (parseSignatureType).
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
