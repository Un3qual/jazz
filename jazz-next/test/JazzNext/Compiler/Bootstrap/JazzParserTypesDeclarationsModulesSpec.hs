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
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    parserFixtureCorpus,
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
    ("rejects empty spaced signatures with stage 0 diagnostics", assertStage0Parity "empty spaced signature payload" "value :: ."),
    ("classifies qualified type variables by terminal member", assertStage0Parity "qualified signature type variable" "qualified :: Alias::a."),
    ("matches stage 0 explicit type application behavior", testExplicitTypeApplicationParity),
    ("classifies qualified explicit type variables by terminal member", assertStage0Parity "qualified explicit type variable" "value = id @Alias::a item."),
    ("matches stage 0 data, class, and impl declarations", testTypeDeclarationParity),
    ("rejects operator signatures in class bodies", assertStage0Parity "operator class method" "class Eq(a) { operator :: Int. }."),
    ("rejects operator bindings in impl bodies", assertStage0Parity "operator impl method" "impl Eq(Int) { operator = 1. }."),
    ("diagnoses uppercase data parameters at the header", assertStage0Parity "uppercase data type parameter" "data Box A = Box."),
    ("rejects qualified lowercase impl targets", assertStage0Parity "qualified lowercase impl target" "impl Eq(Alias::a) { }."),
    ("preserves unsupported capability header diagnostics", assertStage0Parity "unsupported capability header argument" "class Eq(forall a) { }."),
    ("preserves capability header EOF context", assertStage0Parity "capability header EOF context" "class Eq(a"),
    ("requires matching bindings for qualified constructor payloads", assertStage0Parity "unmatched qualified constructor payload" "Result::Alias::a."),
    ("accepts qualified constructor payloads before matching bindings", assertStage0Parity "matched qualified constructor payload" "Result::Alias::a. Result = 1."),
    ("accepts qualified concrete constructor payloads", assertStage0Parity "qualified concrete constructor payload" "Result::Alias::Int."),
    ("requires matching bindings for parenthesized qualified constructor payloads", assertStage0Parity "unmatched parenthesized qualified constructor payload" "Result::(Alias::a)."),
    ("accepts parenthesized qualified constructor payloads before matching bindings", assertStage0Parity "matched parenthesized qualified constructor payload" "Result::(Alias::a). Result = 1."),
    ("accepts parenthesized qualified concrete constructor payloads", assertStage0Parity "parenthesized qualified concrete constructor payload" "Result::(Alias::Int)."),
    ("preserves compact integer payloads before matching bindings", assertStage0Parity "matched unsupported integer payload" "value::1. value = 0."),
    ("preserves compact operator payloads before matching bindings", assertStage0Parity "matched unsupported operator payload" "value::+. value = 0."),
    ("rejects unmatched compact integer payloads like stage 0", assertStage0Parity "unmatched unsupported integer payload" "value::1."),
    ("rejects unmatched compact operator payloads like stage 0", assertStage0Parity "unmatched unsupported operator payload" "value::+."),
    ("does not treat a leading extra separator as signature syntax", assertStage0Parity "unsupported extra separator payload" "value::::A."),
    ("preserves later syntax evidence after a leading extra separator", assertStage0Parity "unsupported extra separator payload with syntax evidence" "value::::A -> B."),
    ("anchors parenthesized operator binding boundaries at the opener", assertStage0Parity "parenthesized operator binding boundary" "value :: Int (+) = 1."),
    ("anchors parenthesized operator signature boundaries at the opener", assertStage0Parity "parenthesized operator signature boundary" "value :: Int (+) :: Int."),
    ("rejects unmatched multiply-qualified payloads like stage 0", assertStage0Parity "unmatched multiply-qualified payload" "value::A::B::C."),
    ("rejects unmatched constructor multiply-qualified payloads like stage 0", assertStage0Parity "unmatched constructor multiply-qualified payload" "Result::Alias::Member::Extra."),
    ("preserves syntax evidence after extra qualification", assertStage0Parity "multiply-qualified payload with syntax evidence" "value::A::B::C -> D."),
    ("keeps constructor extra qualification at the statement boundary despite later syntax", assertStage0Parity "constructor multiply-qualified payload with later syntax" "Result::Alias::Member::Extra -> D."),
    ("preserves multiply-qualified payloads before matching bindings", assertStage0Parity "matched multiply-qualified payload" "value::A::B::C. value = 0."),
    ("keeps constructor extra qualification at the statement boundary before matching bindings", assertStage0Parity "constructor multiply-qualified payload before matching binding" "Result::Alias::Member::Extra. Result = 0."),
    ("rejects reserved compact signature names before alias routing", assertStage0Parity "reserved signature name before alias" "True::x. import M as True."),
    ("preserves data-header EOF context after the keyword", assertStage0Parity "data header EOF after keyword" "data"),
    ("preserves data-header EOF context after the type name", assertStage0Parity "data header EOF after type name" "data Box"),
    ("preserves data-header EOF context after type parameters", assertStage0Parity "data header EOF after type parameters" "data Box a"),
    ("preserves data-constructor EOF context after equals", assertStage0Parity "data constructor EOF after equals" "data Maybe ="),
    ("preserves data-constructor EOF context after a pipe", assertStage0Parity "data constructor EOF after pipe" "data Maybe = Just |"),
    ("reports missing signature terminators as dots", assertStage0Parity "signature terminator EOF" "value :: Int"),
    ("preserves empty constructor-export EOF context", assertStage0Parity "empty constructor export EOF" "module M(type Box("),
    ("anchors incomplete all-constructor exports at the second dot", assertStage0Parity "all-constructor export EOF" "module M(type Box(.."),
    ("anchors malformed all-constructor exports at the second dot", assertStage0Parity "all-constructor export following token" "module M(type Box(..Other"),
    ("stops nested alias discovery at the enclosing brace", assertStage0Parity "nested alias scan boundary" "{ Alias::a. }. import M as Alias."),
    ("matches stage 0 modules imports exports and alias scopes", testModuleDeclarationParity)
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

testTypeDeclarationParity :: IO ()
testTypeDeclarationParity = do
  mapM_
    assertFixtureParity
    [ "parser-corpus-0064",
      "parser-corpus-0065",
      "parser-corpus-0066",
      "parser-corpus-0067",
      "parser-corpus-0068",
      "parser-corpus-0069",
      "parser-corpus-0070",
      "parser-corpus-0071",
      "parser-corpus-0072",
      "parser-corpus-0052",
      "parser-corpus-0053",
      "parser-corpus-0054",
      "parser-corpus-0055",
      "parser-corpus-0056",
      "parser-corpus-0057",
      "parser-corpus-0058",
      "parser-corpus-0059",
      "parser-corpus-0060",
      "parser-corpus-0061",
      "parser-corpus-0062",
      "parser-corpus-0104",
      "parser-corpus-0105",
      "parser-corpus-0106",
      "parser-corpus-0107",
      "parser-corpus-0108",
      "types-declarations-modules-foundational-impl-method"
    ]
  mapM_
    (uncurry assertStage0Parity)
    [ ("requires uppercase data type names", "data thing = Thing."),
      ("requires uppercase data constructor names", "data Thing = thing."),
      ("rejects mismatched constructor argument delimiters", "data Thing a = Thing (a]."),
      ("rejects unclosed constructor argument delimiters", "data Thing a = Thing [a."),
      ("requires uppercase class names", "class eq(a) { }."),
      ("requires uppercase impl names", "impl eq(Int) { }."),
      ("rejects unclosed capability headers", "class Eq(a { }."),
      ("rejects nested data declarations", "{ data Thing = Thing. }."),
      ("preserves nested class declaration behavior", "{ class Eq(a) { }. }."),
      ("preserves nested impl declaration behavior", "{ impl Eq(Int) { }. }.")
    ]

testModuleDeclarationParity :: IO ()
testModuleDeclarationParity = do
  mapM_
    assertFixtureParity
    [ "parser-corpus-0109",
      "parser-corpus-0110",
      "parser-corpus-0111",
      "parser-corpus-0112",
      "parser-corpus-0113",
      "parser-corpus-0114",
      "parser-corpus-0115",
      "parser-corpus-0116",
      "parser-corpus-0117",
      "parser-corpus-0118",
      "parser-corpus-0119",
      "parser-corpus-0120",
      "parser-corpus-0121",
      "parser-corpus-0122",
      "parser-corpus-0123",
      "parser-corpus-0124",
      "parser-corpus-0125",
      "parser-corpus-0126",
      "parser-corpus-0127",
      "parser-corpus-0128",
      "parser-corpus-0129",
      "parser-corpus-0133",
      "parser-corpus-0134",
      "parser-corpus-0135",
      "parser-corpus-0136",
      "parser-corpus-0137",
      "parser-corpus-0138",
      "parser-corpus-0139",
      "parser-corpus-0140",
      "parser-corpus-0141",
      "parser-corpus-0142",
      "parser-corpus-0143",
      "parser-corpus-0144",
      "parser-corpus-0145",
      "parser-corpus-0146",
      "parser-corpus-0147",
      "parser-corpus-0148",
      "parser-corpus-0150",
      "parser-corpus-0151",
      "parser-corpus-0152",
      "parser-corpus-0153",
      "parser-corpus-0154",
      "parser-corpus-0155",
      "parser-corpus-0156",
      "parser-corpus-0157",
      "parser-corpus-0158",
      "parser-corpus-0159",
      "parser-corpus-0235",
      "parser-corpus-0306"
    ]
  mapM_
    (uncurry assertStage0Parity)
    [ ( "registers duplicate aliases idempotently",
        "import A as Alias. import B as Alias. Alias::value."
      ),
      ( "pre-collects aliases in a module body and inherits them in nested blocks",
        "module App { result = { Math::answer. }. import Lib::Math as Math. }"
      ),
      ( "does not collect a nested block qualifier into its enclosing scope",
        "result = { Local::answer. }. Local::answer."
      )
    ]

assertFixtureParity :: Text.Text -> IO ()
assertFixtureParity fixtureName =
  case filter ((== fixtureName) . parserFixtureName) parserFixtureCorpus of
    [fixture] -> assertStage0Parity fixtureName (parserFixtureSource fixture)
    fixtures -> failTest (fixtureName <> ": expected one fixture, found " <> Text.pack (show (length fixtures)))

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
