{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalSourcePath (..),
    canonicalizeLexResult,
    normalizeCanonicalSourcePath,
    renderCanonicalLexResult
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure (..),
    LexicalFailureReason (..),
    LexicalLiteralKind (..),
    Token,
    tokenize,
    tokenizeDetailed
  )
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    ParserFixtureExpectation (..),
    parserFixtureCorpus
  )
import JazzNext.Compiler.Parser (parseSurfaceProgram)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import System.Directory (doesFileExist)
import qualified Data.Set as Set
import Data.Either (isRight)

main :: IO ()
main = runTestSuite "CanonicalLexerComparison" tests

tests :: [NamedTest]
tests =
  [ ("preserves unexpected characters structurally", testUnexpectedCharacter),
    ("preserves invalid escapes structurally", testInvalidEscape),
    ("preserves non-scalar escapes structurally", testNonScalarEscape),
    ("preserves character length failures structurally", testCharacterLengths),
    ("preserves unterminated literals structurally", testUnterminatedLiterals),
    ("preserves raw newlines structurally", testRawNewline),
    ("preserves malformed Unicode escapes structurally", testMalformedUnicodeEscapes),
    ("keeps the legacy diagnostic wrapper", testLegacyDiagnosticWrapper),
    ("normalizes logical source paths", testNormalizesLogicalPaths),
    ("rejects non-logical source paths", testRejectsNonLogicalPaths),
    ("renders canonical tokens through the runtime renderer", testRendersCanonicalTokens),
    ("preserves arbitrary-precision integer payloads", testPreservesArbitraryPrecisionIntegers),
    ("maps every token constructor", testMapsEveryTokenConstructor),
    ("maps every lexical failure constructor", testMapsEveryLexicalFailureConstructor),
    ("uses runtime escaping for decoded values", testUsesRuntimeEscaping),
    ("renders the same canonical value from Jazz", testJazzCanonicalRendering),
    ("keeps the parser fixture corpus well formed", testParserFixtureCorpusWellFormed),
    ("keeps parser classifications current", testParserFixtureClassifications),
    ("adapts the parser corpus deterministically", testParserFixtureDeterminism)
  ]

testUnexpectedCharacter :: IO ()
testUnexpectedCharacter =
  assertEqual
    "unexpected character"
    (Left (LexicalFailure (UnexpectedCharacter '`') (SourceSpan 1 7)))
    (tokenizeDetailed "value ` 42.")

testInvalidEscape :: IO ()
testInvalidEscape =
  assertEqual
    "invalid escape"
    (Left (LexicalFailure (InvalidEscape 'x') (SourceSpan 1 1)))
    (tokenizeDetailed "'\\x'")

testNonScalarEscape :: IO ()
testNonScalarEscape =
  assertEqual
    "surrogate escape"
    (Left (LexicalFailure (NonScalarUnicodeEscape "D800") (SourceSpan 1 1)))
    (tokenizeDetailed "'\\u{D800}'")

testCharacterLengths :: IO ()
testCharacterLengths = do
  assertDetailedFailure
    "empty character"
    (LexicalFailure (InvalidCharacterLength 0) (SourceSpan 1 1))
    "''"
  assertDetailedFailure
    "multi-scalar character"
    (LexicalFailure (InvalidCharacterLength 2) (SourceSpan 1 1))
    "'ab'"

testUnterminatedLiterals :: IO ()
testUnterminatedLiterals = do
  assertDetailedFailure
    "unterminated character"
    (LexicalFailure (UnterminatedLiteral CharacterLiteral) (SourceSpan 1 1))
    "'a"
  assertDetailedFailure
    "unterminated text"
    (LexicalFailure (UnterminatedLiteral TextLiteral) (SourceSpan 1 1))
    "\"abc"
  assertDetailedFailure
    "unterminated Unicode escape"
    (LexicalFailure UnterminatedUnicodeEscape (SourceSpan 1 1))
    "'\\u"

testRawNewline :: IO ()
testRawNewline =
  assertDetailedFailure
    "raw text newline"
    (LexicalFailure (RawNewline TextLiteral) (SourceSpan 1 1))
    "\"a\nb\""

testMalformedUnicodeEscapes :: IO ()
testMalformedUnicodeEscapes = do
  assertDetailedFailure
    "empty Unicode escape"
    (LexicalFailure (MalformedUnicodeEscape "") (SourceSpan 1 1))
    "'\\u{}'"
  assertDetailedFailure
    "non-hex Unicode escape"
    (LexicalFailure (MalformedUnicodeEscape "xyz") (SourceSpan 1 1))
    "'\\u{xyz}'"

testLegacyDiagnosticWrapper :: IO ()
testLegacyDiagnosticWrapper =
  case tokenize "value ` 42." of
    Left diagnostic -> do
      assertEqual "legacy code" "E0001" (diagnosticCode diagnostic)
      assertEqual "legacy span" (Just (SourceSpan 1 7)) (diagnosticPrimarySpan diagnostic)
      assertContains "legacy summary" "unexpected character '`' at 1:7" (renderDiagnostic diagnostic)
    Right tokens -> failTest ("expected lexical failure, got " <> showText tokens)

testNormalizesLogicalPaths :: IO ()
testNormalizesLogicalPaths =
  assertEqual
    "normalized logical path"
    (Right (CanonicalSourcePath "fixtures/parser/basic.jz"))
    (normalizeCanonicalSourcePath "fixtures/./parser//basic.jz")

testRejectsNonLogicalPaths :: IO ()
testRejectsNonLogicalPaths = do
  assertEqual
    "absolute path"
    (Left "canonical source path must be relative")
    (normalizeCanonicalSourcePath "/tmp/basic.jz")
  assertEqual
    "parent path"
    (Left "canonical source path must not contain '..'")
    (normalizeCanonicalSourcePath "fixtures/../basic.jz")
  assertEqual
    "backslash path"
    (Left "canonical source path must use '/' separators")
    (normalizeCanonicalSourcePath "fixtures\\basic.jz")

testRendersCanonicalTokens :: IO ()
testRendersCanonicalTokens = do
  path <- normalizedPath "fixtures/parser/basic.jz"
  result <- detailedResult "module value = 00042."
  assertEqual
    "canonical token rendering"
    ( "CanonicalLexSuccess(CanonicalSourcePath(\"fixtures/parser/basic.jz\"), "
        <> "[CanonicalToken(KeywordKind(ModuleKeyword), \"module\", CanonicalSpan(1, 1)), "
        <> "CanonicalToken(IdentifierKind(\"value\"), \"value\", CanonicalSpan(1, 8)), "
        <> "CanonicalToken(PunctuationKind(EqualsPunctuation), \"=\", CanonicalSpan(1, 14)), "
        <> "CanonicalToken(IntegerKind(\"42\"), \"00042\", CanonicalSpan(1, 16)), "
        <> "CanonicalToken(PunctuationKind(DotPunctuation), \".\", CanonicalSpan(1, 21))])"
    )
    (renderCanonicalLexResult (canonicalizeLexResult path result))

testPreservesArbitraryPrecisionIntegers :: IO ()
testPreservesArbitraryPrecisionIntegers = do
  path <- normalizedPath "fixtures/parser/huge-integer.jz"
  result <- detailedResult "9223372036854775808."
  assertContains
    "arbitrary integer canonical decimal"
    "IntegerKind(\"9223372036854775808\")"
    (renderCanonicalLexResult (canonicalizeLexResult path result))

testMapsEveryTokenConstructor :: IO ()
testMapsEveryTokenConstructor = do
  path <- normalizedPath "fixtures/parser/all-tokens.jz"
  result <-
    detailedResult
      "module import as data if then else case -> @ = : :: . { } ( ) [ ] , \\ + name 0 'a' \"x\""
  let rendered = renderCanonicalLexResult (canonicalizeLexResult path result)
      expectedKinds =
        [ "KeywordKind(ModuleKeyword)",
          "KeywordKind(ImportKeyword)",
          "KeywordKind(AsKeyword)",
          "KeywordKind(DataKeyword)",
          "KeywordKind(IfKeyword)",
          "KeywordKind(ThenKeyword)",
          "KeywordKind(ElseKeyword)",
          "KeywordKind(CaseKeyword)",
          "PunctuationKind(ArrowPunctuation)",
          "PunctuationKind(AtPunctuation)",
          "PunctuationKind(EqualsPunctuation)",
          "PunctuationKind(ColonPunctuation)",
          "PunctuationKind(DoubleColonPunctuation)",
          "PunctuationKind(DotPunctuation)",
          "PunctuationKind(LeftBracePunctuation)",
          "PunctuationKind(RightBracePunctuation)",
          "PunctuationKind(LeftParenPunctuation)",
          "PunctuationKind(RightParenPunctuation)",
          "PunctuationKind(LeftBracketPunctuation)",
          "PunctuationKind(RightBracketPunctuation)",
          "PunctuationKind(CommaPunctuation)",
          "PunctuationKind(LambdaPunctuation)",
          "OperatorKind(\"+\")",
          "IdentifierKind(\"name\")",
          "IntegerKind(\"0\")",
          "CharacterKind('a')",
          "TextKind(\"x\")"
        ]
  mapM_ (\expected -> assertContains expected expected rendered) expectedKinds

testMapsEveryLexicalFailureConstructor :: IO ()
testMapsEveryLexicalFailureConstructor = do
  path <- normalizedPath "fixtures/parser/all-errors.jz"
  let spanValue = SourceSpan 2 9
      cases =
        [ (LexicalFailure (UnexpectedCharacter '`') spanValue, "UnexpectedCharacter('`')"),
          (LexicalFailure UnexpectedEndOfInput spanValue, "UnexpectedEndOfInput"),
          (LexicalFailure (InvalidCharacterLength 2) spanValue, "InvalidCharacterLength(2)"),
          (LexicalFailure (UnterminatedLiteral CharacterLiteral) spanValue, "UnterminatedLiteral(CharacterLiteral)"),
          (LexicalFailure (RawNewline TextLiteral) spanValue, "RawNewline(TextLiteral)"),
          (LexicalFailure (InvalidEscape 'x') spanValue, "InvalidEscape('x')"),
          (LexicalFailure UnterminatedUnicodeEscape spanValue, "UnterminatedUnicodeEscape"),
          (LexicalFailure (MalformedUnicodeEscape "xyz") spanValue, "MalformedUnicodeEscape(\"xyz\")"),
          (LexicalFailure (NonScalarUnicodeEscape "D800") spanValue, "NonScalarUnicodeEscape(\"D800\")"),
          (LexicalFailure (InvalidLiteralCharacter TextLiteral '`') spanValue, "InvalidLiteralCharacter(TextLiteral, '`')"),
          (LexicalFailure (InvalidIntegerLiteral "00x") spanValue, "InvalidIntegerLiteral(\"00x\")")
        ]
  mapM_
    ( \(failure, expected) ->
        assertContains
          expected
          expected
          (renderCanonicalLexResult (canonicalizeLexResult path (Left failure)))
    )
    cases

testUsesRuntimeEscaping :: IO ()
testUsesRuntimeEscaping = do
  path <- normalizedPath "fixtures/parser/escapes.jz"
  result <- detailedResult "'\\n' \"quote: \\\"; tab: \\t\""
  let rendered = renderCanonicalLexResult (canonicalizeLexResult path result)
  assertContains "character escape" "CharacterKind('\\n')" rendered
  assertContains "text escape" "TextKind(\"quote: \\\"; tab: \\t\")" rendered

testJazzCanonicalRendering :: IO ()
testJazzCanonicalRendering = do
  lexerTypesSource <- readLexerTypesSource
  first <- runJazzCanonicalFixture lexerTypesSource
  second <- runJazzCanonicalFixture lexerTypesSource
  path <- normalizedPath "fixtures/parser/basic.jz"
  result <- detailedResult "module"
  let expected = renderCanonicalLexResult (canonicalizeLexResult path result)
  assertEqual "Jazz canonical output" (Just expected) (runOutput first)
  assertEqual "Jazz canonical determinism" (runOutput first) (runOutput second)

runJazzCanonicalFixture :: Text.Text -> IO RunResult
runJazzCanonicalFixture lexerTypesSource =
  runModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    ["App", "Main"]
    lookupSource
  where
    lookupSource sourcePath =
      pure
        ( case sourcePath of
            "src/App/Main.jz" -> Just jazzCanonicalFixtureSource
            "src/LexerTypes.jz" -> Just lexerTypesSource
            _ -> Nothing
        )

jazzCanonicalFixtureSource :: Text.Text
jazzCanonicalFixtureSource =
  Text.unlines
    [ "module App::Main {",
      "  import LexerTypes (CanonicalLexSuccess, CanonicalSourcePath, CanonicalToken, KeywordKind, ModuleKeyword, CanonicalSpan).",
      "  CanonicalLexSuccess (CanonicalSourcePath \"fixtures/parser/basic.jz\") [CanonicalToken (KeywordKind ModuleKeyword) \"module\" (CanonicalSpan 1 1)].",
      "}"
    ]

readLexerTypesSource :: IO Text.Text
readLexerTypesSource = readFirstExisting ["jazz-next/stdlib/LexerTypes.jz", "stdlib/LexerTypes.jz"]

readFirstExisting :: [FilePath] -> IO Text.Text
readFirstExisting candidates =
  case candidates of
    [] -> failTest "could not locate LexerTypes.jz"
    candidate : rest -> do
      exists <- doesFileExist candidate
      if exists then TextIO.readFile candidate else readFirstExisting rest

testParserFixtureCorpusWellFormed :: IO ()
testParserFixtureCorpusWellFormed = do
  let names = map parserFixtureName parserFixtureCorpus
      focusedNames =
        [ "lexer-leading-zero-integer",
          "lexer-crlf-spans",
          "lexer-unicode-and-escape-values",
          "lexer-arbitrary-precision-integer",
          "lexer-all-token-constructors",
          "lexer-comments-spaces-and-tabs",
          "lexer-lf-spans",
          "lexer-all-supported-escapes",
          "lexer-operator-runs",
          "lexer-empty-character",
          "lexer-multi-scalar-character",
          "lexer-unterminated-character",
          "lexer-unterminated-text",
          "lexer-raw-newline",
          "lexer-invalid-escape",
          "lexer-unterminated-unicode-escape",
          "lexer-empty-unicode-escape",
          "lexer-nonhex-unicode-escape",
          "lexer-overlong-unicode-escape",
          "lexer-nonscalar-unicode-escape",
          "lexer-unexpected-character"
        ]
      observedNames =
        [ "parser-corpus-" <> Text.justifyRight 4 '0' (showText index)
          | index <- [1 :: Int .. 312]
        ]
  assertEqual "corpus is nonempty" False (null parserFixtureCorpus)
  assertEqual "fixture names are unique" (length names) (Set.size (Set.fromList names))
  assertEqual "fixture manifest order" (focusedNames <> observedNames) names
  assertEqual
    "fixture paths normalize"
    (replicate (length parserFixtureCorpus) True)
    [isRight (normalizeCanonicalSourcePath (parserFixturePath fixture)) | fixture <- parserFixtureCorpus]
  assertEqual
    "corpus includes accepted parser sources"
    True
    (ParserAccepted `elem` map parserFixtureExpectation parserFixtureCorpus)
  assertEqual
    "corpus includes rejected parser sources"
    True
    (ParserRejected `elem` map parserFixtureExpectation parserFixtureCorpus)

testParserFixtureClassifications :: IO ()
testParserFixtureClassifications =
  mapM_
    ( \fixture ->
        assertEqual
          ("parser classification " <> parserFixtureName fixture)
          (parserFixtureExpectation fixture == ParserAccepted)
          (isRight (parseSurfaceProgram (parserFixtureSource fixture)))
    )
    parserFixtureCorpus

testParserFixtureDeterminism :: IO ()
testParserFixtureDeterminism =
  mapM_
    ( \fixture -> do
        path <- normalizedPath (parserFixturePath fixture)
        let canonical = canonicalizeLexResult path (tokenizeDetailed (parserFixtureSource fixture))
            first = renderCanonicalLexResult canonical
            second = renderCanonicalLexResult canonical
        assertEqual ("deterministic fixture " <> parserFixtureName fixture) first second
    )
    parserFixtureCorpus

showText :: Show a => a -> Text.Text
showText = Text.pack . show

assertDetailedFailure :: Text.Text -> LexicalFailure -> Text.Text -> IO ()
assertDetailedFailure label expected source =
  assertEqual label (Left expected) (tokenizeDetailed source)

normalizedPath :: FilePath -> IO CanonicalSourcePath
normalizedPath path =
  case normalizeCanonicalSourcePath path of
    Left err -> failTest ("expected valid logical path: " <> err)
    Right normalized -> pure normalized

detailedResult :: Text.Text -> IO (Either LexicalFailure [Token])
detailedResult source = pure (tokenizeDetailed source)
