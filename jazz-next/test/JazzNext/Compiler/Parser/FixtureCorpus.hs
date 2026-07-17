{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    ParserFixtureExpectation (..),
    ParserFixtureFamily (..),
    ParserFixtureManifestViolation (..),
    lookupParserFixtureFamily,
    parserFixtureCorpus,
    parserFixtureFamilyNames,
    validateParserFixtureManifest
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

data ParserFixtureExpectation
  = ParserAccepted
  | ParserRejected
  deriving (Eq, Show)

data ParserFixture = ParserFixture
  { parserFixtureName :: Text,
    parserFixturePath :: FilePath,
    parserFixtureSource :: Text,
    parserFixtureExpectation :: ParserFixtureExpectation
  }
  deriving (Eq, Show)

data ParserFixtureFamily
  = ExpressionFoundation
  deriving (Eq, Show)

data ParserFixtureManifestViolation
  = DuplicateParserFixtureName Text
  | DuplicateParserFixtureFamilyMember ParserFixtureFamily Text
  | MissingParserFixtureFamilyMember ParserFixtureFamily Text
  deriving (Eq, Show)

parserFixtureCorpus :: [ParserFixture]
parserFixtureCorpus =
  focusedLexerFixtures <> observedParserFixtures <> expressionFoundationFixtures

parserFixtureFamilyNames :: ParserFixtureFamily -> [Text]
parserFixtureFamilyNames family =
  case family of
    ExpressionFoundation -> expressionFoundationFixtureNames

lookupParserFixtureFamily ::
  ParserFixtureFamily ->
  Either [ParserFixtureManifestViolation] [ParserFixture]
lookupParserFixtureFamily family =
  case validateParserFixtureManifest parserFixtureCorpus parserFixtureFamilies of
    [] -> Right (resolveFixtureNames (parserFixtureFamilyNames family))
    violations -> Left violations
  where
    resolveFixtureNames names =
      case names of
        [] -> []
        name : remaining ->
          case lookupFixture name parserFixtureCorpus of
            Just fixture -> fixture : resolveFixtureNames remaining
            Nothing -> resolveFixtureNames remaining

validateParserFixtureManifest ::
  [ParserFixture] ->
  [(ParserFixtureFamily, [Text])] ->
  [ParserFixtureManifestViolation]
validateParserFixtureManifest fixtures families =
  map DuplicateParserFixtureName (duplicateValues fixtureNames)
    <> concatMap validateFamily families
  where
    fixtureNames = map parserFixtureName fixtures

    validateFamily (family, memberNames) =
      map (DuplicateParserFixtureFamilyMember family) (duplicateValues memberNames)
        <> map (MissingParserFixtureFamilyMember family) (missingValues memberNames)

    missingValues =
      uniqueValues . filter (not . (`elem` fixtureNames))

parserFixtureFamilies :: [(ParserFixtureFamily, [Text])]
parserFixtureFamilies =
  [(ExpressionFoundation, expressionFoundationFixtureNames)]

duplicateValues :: (Eq value) => [value] -> [value]
duplicateValues = go [] []
  where
    go seen duplicates values =
      case values of
        [] -> reverse duplicates
        value : remaining
          | value `elem` seen && value `notElem` duplicates ->
              go seen (value : duplicates) remaining
          | otherwise -> go (value : seen) duplicates remaining

uniqueValues :: (Eq value) => [value] -> [value]
uniqueValues = go []
  where
    go seen values =
      case values of
        [] -> reverse seen
        value : remaining
          | value `elem` seen -> go seen remaining
          | otherwise -> go (value : seen) remaining

lookupFixture :: Text -> [ParserFixture] -> Maybe ParserFixture
lookupFixture name fixtures =
  case fixtures of
    [] -> Nothing
    fixture : remaining
      | parserFixtureName fixture == name -> Just fixture
      | otherwise -> lookupFixture name remaining

expressionFoundationFixtureNames :: [Text]
expressionFoundationFixtureNames =
  [ "lexer-leading-zero-integer",
    "lexer-crlf-spans",
    "lexer-unicode-and-escape-values",
    "lexer-all-supported-escapes",
    "lexer-unexpected-character",
    "parser-corpus-0001",
    "parser-corpus-0024",
    "parser-corpus-0028",
    "parser-corpus-0032",
    "parser-corpus-0310",
    "parser-corpus-0036",
    "parser-corpus-0051",
    "parser-corpus-0182",
    "parser-corpus-0193",
    "parser-corpus-0194",
    "parser-corpus-0206",
    "parser-corpus-0214",
    "parser-corpus-0233",
    "parser-corpus-0234",
    "parser-corpus-0236",
    "parser-corpus-0237",
    "parser-corpus-0240",
    "parser-corpus-0241",
    "parser-corpus-0308",
    "parser-corpus-0309",
    "parser-corpus-0041",
    "expression-foundation-reserved-true-signature",
    "expression-foundation-reserved-false-signature",
    "expression-foundation-spaced-reserved-true-signature",
    "expression-foundation-spaced-reserved-false-signature",
    "expression-foundation-identifier-operator-tier",
    "expression-foundation-identifier-operator-precedence",
    "expression-foundation-nested-identifier-operator-tier",
    "expression-foundation-parenthesized-signature-statement-boundary",
    "expression-foundation-empty-program",
    "expression-foundation-empty-block",
    "expression-foundation-grouped-name",
    "expression-foundation-empty-list",
    "expression-foundation-list-literals",
    "expression-foundation-parenthesized-application",
    "expression-foundation-list-missing-close",
    "expression-foundation-list-trailing-comma",
    "expression-foundation-tuple-missing-close",
    "expression-foundation-tuple-trailing-comma",
    "expression-foundation-binding-missing-rhs",
    "expression-foundation-binding-missing-dot",
    "expression-foundation-expression-missing-dot",
    "expression-foundation-qualified-missing-member",
    "expression-foundation-qualified-whitespace",
    "expression-foundation-dot-without-expression",
    "expression-foundation-max-float64"
  ]

focusedLexerFixtures :: [ParserFixture]
focusedLexerFixtures =
  [ ParserFixture
      { parserFixtureName = "lexer-leading-zero-integer",
        parserFixturePath = "fixtures/lexer/leading-zero-integer.jz",
        parserFixtureSource = "value = 00042.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-crlf-spans",
        parserFixturePath = "fixtures/lexer/crlf-spans.jz",
        parserFixtureSource = "first = 1.\r\nsecond = 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-unicode-and-escape-values",
        parserFixturePath = "fixtures/lexer/unicode-and-escape-values.jz",
        parserFixtureSource = "'\\u{1F642}' \"a\\t\".",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-arbitrary-precision-integer",
        parserFixturePath = "fixtures/lexer/arbitrary-precision-integer.jz",
        parserFixtureSource = "9223372036854775808.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-all-token-constructors",
        parserFixturePath = "fixtures/lexer/all-token-constructors.jz",
        parserFixtureSource = "module import as data if then else case -> @ = : :: . { } ( ) [ ] , \\ + name 0 'a' \"x\"",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-comments-spaces-and-tabs",
        parserFixturePath = "fixtures/lexer/comments-spaces-and-tabs.jz",
        parserFixtureSource = "first\t= 1. # keep the next span honest\n  second = 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-lf-spans",
        parserFixturePath = "fixtures/lexer/lf-spans.jz",
        parserFixtureSource = "first = 1.\nsecond = 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-all-supported-escapes",
        parserFixturePath = "fixtures/lexer/all-supported-escapes.jz",
        parserFixtureSource = "\"\\\\\\'\\\"\\n\\r\\t\\0\\u{1F642}\".",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "lexer-operator-runs",
        parserFixturePath = "fixtures/lexer/operator-runs.jz",
        parserFixtureSource = "a +-*|%&?^~ b.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-empty-character",
        parserFixturePath = "fixtures/lexer/empty-character.jz",
        parserFixtureSource = "''.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-multi-scalar-character",
        parserFixturePath = "fixtures/lexer/multi-scalar-character.jz",
        parserFixtureSource = "'ab'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-unterminated-character",
        parserFixturePath = "fixtures/lexer/unterminated-character.jz",
        parserFixtureSource = "'a",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-unterminated-text",
        parserFixturePath = "fixtures/lexer/unterminated-text.jz",
        parserFixtureSource = "\"abc",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-raw-newline",
        parserFixturePath = "fixtures/lexer/raw-newline.jz",
        parserFixtureSource = "\"a\nb\".",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-invalid-escape",
        parserFixturePath = "fixtures/lexer/invalid-escape.jz",
        parserFixtureSource = "'\\x'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-unterminated-unicode-escape",
        parserFixturePath = "fixtures/lexer/unterminated-unicode-escape.jz",
        parserFixtureSource = "'\\u",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-empty-unicode-escape",
        parserFixturePath = "fixtures/lexer/empty-unicode-escape.jz",
        parserFixtureSource = "'\\u{}'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-nonhex-unicode-escape",
        parserFixturePath = "fixtures/lexer/nonhex-unicode-escape.jz",
        parserFixtureSource = "'\\u{xyz}'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-overlong-unicode-escape",
        parserFixturePath = "fixtures/lexer/overlong-unicode-escape.jz",
        parserFixtureSource = "'\\u{1234567}'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-nonscalar-unicode-escape",
        parserFixturePath = "fixtures/lexer/nonscalar-unicode-escape.jz",
        parserFixtureSource = "'\\u{D800}'.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "lexer-unexpected-character",
        parserFixturePath = "fixtures/lexer/unexpected-character.jz",
        parserFixtureSource = "value ` 42.",
        parserFixtureExpectation = ParserRejected
      }
  ]

observedParserFixtures :: [ParserFixture]
-- Static snapshot of the exact source values exercised by the parser suites on
-- 2026-07-12. Update this manifest when parser fixtures change; runtime tests do
-- not inspect Haskell source files to reconstruct the corpus.
observedParserFixtures =
  [ ParserFixture
      { parserFixtureName = "parser-corpus-0001",
        parserFixturePath = "fixtures/parser/parser-corpus-0001.jz",
        parserFixtureSource = Text.pack "\tx.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0002",
        parserFixturePath = "fixtures/parser/parser-corpus-0002.jz",
        parserFixtureSource = Text.pack "\"Jazz\" -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0003",
        parserFixturePath = "fixtures/parser/parser-corpus-0003.jz",
        parserFixtureSource = Text.pack "\"\\",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0004",
        parserFixturePath = "fixtures/parser/parser-corpus-0004.jz",
        parserFixtureSource = Text.pack "\"\\u",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0005",
        parserFixturePath = "fixtures/parser/parser-corpus-0005.jz",
        parserFixtureSource = Text.pack "\"a\nb\"",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0006",
        parserFixturePath = "fixtures/parser/parser-corpus-0006.jz",
        parserFixtureSource = Text.pack "\"abc",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0007",
        parserFixturePath = "fixtures/parser/parser-corpus-0007.jz",
        parserFixtureSource = Text.pack "''",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0008",
        parserFixturePath = "fixtures/parser/parser-corpus-0008.jz",
        parserFixtureSource = Text.pack "'\\",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0009",
        parserFixturePath = "fixtures/parser/parser-corpus-0009.jz",
        parserFixtureSource = Text.pack "'\\n' \"quote: \\\"; scalar: \\u{1F3B7}\".",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0010",
        parserFixturePath = "fixtures/parser/parser-corpus-0010.jz",
        parserFixtureSource = Text.pack "'\\u",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0011",
        parserFixturePath = "fixtures/parser/parser-corpus-0011.jz",
        parserFixtureSource = Text.pack "'\\u{110000}'",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0012",
        parserFixturePath = "fixtures/parser/parser-corpus-0012.jz",
        parserFixtureSource = Text.pack "'\\u{41}' \"a\\n\"",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0013",
        parserFixturePath = "fixtures/parser/parser-corpus-0013.jz",
        parserFixtureSource = Text.pack "'\\u{D800}'",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0014",
        parserFixturePath = "fixtures/parser/parser-corpus-0014.jz",
        parserFixtureSource = Text.pack "'\\u{}'",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0015",
        parserFixturePath = "fixtures/parser/parser-corpus-0015.jz",
        parserFixtureSource = Text.pack "'\\x'",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0016",
        parserFixturePath = "fixtures/parser/parser-corpus-0016.jz",
        parserFixtureSource = Text.pack "'a",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0017",
        parserFixturePath = "fixtures/parser/parser-corpus-0017.jz",
        parserFixtureSource = Text.pack "'a' \"Jazz\".",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0018",
        parserFixturePath = "fixtures/parser/parser-corpus-0018.jz",
        parserFixtureSource = Text.pack "'a' -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0019",
        parserFixturePath = "fixtures/parser/parser-corpus-0019.jz",
        parserFixtureSource = Text.pack "'a', next",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0020",
        parserFixturePath = "fixtures/parser/parser-corpus-0020.jz",
        parserFixtureSource = Text.pack "'ab'",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0021",
        parserFixturePath = "fixtures/parser/parser-corpus-0021.jz",
        parserFixtureSource = Text.pack """
        (%%) :: Int -> Int -> Int.
        (%%) = \\(left, right) -> left + right.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0022",
        parserFixturePath = "fixtures/parser/parser-corpus-0022.jz",
        parserFixtureSource = Text.pack "(%%) = \\(left, right) -> left + right.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0023",
        parserFixturePath = "fixtures/parser/parser-corpus-0023.jz",
        parserFixtureSource = Text.pack "() -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0024",
        parserFixturePath = "fixtures/parser/parser-corpus-0024.jz",
        parserFixtureSource = Text.pack "().",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0025",
        parserFixturePath = "fixtures/parser/parser-corpus-0025.jz",
        parserFixtureSource = Text.pack "(+) (10 +) (+ 20).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0026",
        parserFixturePath = "fixtures/parser/parser-corpus-0026.jz",
        parserFixtureSource = Text.pack """
        (+) :: Int -> Int -> Int.
        operator %% tier 2.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0027",
        parserFixturePath = "fixtures/parser/parser-corpus-0027.jz",
        parserFixtureSource = Text.pack "(+) = \\(left, right) -> left + right.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0028",
        parserFixturePath = "fixtures/parser/parser-corpus-0028.jz",
        parserFixtureSource = Text.pack "(1, True).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0029",
        parserFixturePath = "fixtures/parser/parser-corpus-0029.jz",
        parserFixtureSource = Text.pack "1.25f32.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0030",
        parserFixturePath = "fixtures/parser/parser-corpus-0030.jz",
        parserFixtureSource = Text.pack "1.5 -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0031",
        parserFixturePath = "fixtures/parser/parser-corpus-0031.jz",
        parserFixtureSource = Text.pack "1.5.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0032",
        parserFixturePath = "fixtures/parser/parser-corpus-0032.jz",
        parserFixtureSource = Text.pack "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0033",
        parserFixturePath = "fixtures/parser/parser-corpus-0033.jz",
        parserFixtureSource = Text.pack "Alias::value [1, 2] (3, 4).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0034",
        parserFixturePath = "fixtures/parser/parser-corpus-0034.jz",
        parserFixtureSource = Text.pack "False :: Bool.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0035",
        parserFixturePath = "fixtures/parser/parser-corpus-0035.jz",
        parserFixtureSource = Text.pack "Just item, next",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0036",
        parserFixturePath = "fixtures/parser/parser-corpus-0036.jz",
        parserFixtureSource = Text.pack "Math::1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0037",
        parserFixturePath = "fixtures/parser/parser-corpus-0037.jz",
        parserFixtureSource = Text.pack "Pair 'a' \"Jazz\" -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0038",
        parserFixturePath = "fixtures/parser/parser-corpus-0038.jz",
        parserFixtureSource = Text.pack """
        Result :: Int.
        Result = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0039",
        parserFixturePath = "fixtures/parser/parser-corpus-0039.jz",
        parserFixtureSource = Text.pack """
        Result :: a.
        Result = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0040",
        parserFixturePath = "fixtures/parser/parser-corpus-0040.jz",
        parserFixtureSource = Text.pack """
        Result::a.
        other = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0041",
        parserFixturePath = "fixtures/parser/parser-corpus-0041.jz",
        parserFixtureSource = Text.pack "True = 1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0042",
        parserFixturePath = "fixtures/parser/parser-corpus-0042.jz",
        parserFixtureSource = Text.pack "\\(x) -> x.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0043",
        parserFixturePath = "fixtures/parser/parser-corpus-0043.jz",
        parserFixtureSource = Text.pack "a %% b + c.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0044",
        parserFixturePath = "fixtures/parser/parser-corpus-0044.jz",
        parserFixtureSource = Text.pack "a %% b.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0045",
        parserFixturePath = "fixtures/parser/parser-corpus-0045.jz",
        parserFixtureSource = Text.pack "apply = \\(f, x) -> f x.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0046",
        parserFixturePath = "fixtures/parser/parser-corpus-0046.jz",
        parserFixtureSource = Text.pack "case value { | 0 -> 1 | _ -> 2 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0047",
        parserFixturePath = "fixtures/parser/parser-corpus-0047.jz",
        parserFixtureSource = Text.pack """
        character :: Char.
        message :: Text.
        render :: Char -> Text.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0048",
        parserFixturePath = "fixtures/parser/parser-corpus-0048.jz",
        parserFixtureSource = Text.pack "choose = \\(Just item | Also item) -> item.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0049",
        parserFixturePath = "fixtures/parser/parser-corpus-0049.jz",
        parserFixtureSource = Text.pack "choose = \\(Just item | Also item, extra) -> item.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0050",
        parserFixturePath = "fixtures/parser/parser-corpus-0050.jz",
        parserFixtureSource = Text.pack """
        class :: Int.
        class = 1.
        impl :: Bool.
        impl = True.
        trait :: Int.
        trait = 2.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0051",
        parserFixturePath = "fixtures/parser/parser-corpus-0051.jz",
        parserFixtureSource = Text.pack """
        class = 1.
        impl = class.
        trait = impl.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0052",
        parserFixturePath = "fixtures/parser/parser-corpus-0052.jz",
        parserFixtureSource = Text.pack "class Eq { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0053",
        parserFixturePath = "fixtures/parser/parser-corpus-0053.jz",
        parserFixtureSource = Text.pack "class Eq(Int) { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0054",
        parserFixturePath = "fixtures/parser/parser-corpus-0054.jz",
        parserFixtureSource = Text.pack """
        class Eq(a) {
        equals :: a -> a -> Bool.
        notEquals :: a -> a -> Bool.
        }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0055",
        parserFixturePath = "fixtures/parser/parser-corpus-0055.jz",
        parserFixtureSource = Text.pack "class Eq(a) { 1. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0056",
        parserFixturePath = "fixtures/parser/parser-corpus-0056.jz",
        parserFixtureSource = Text.pack "class Eq(a) { equals :: Int. equals :: Bool. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0057",
        parserFixturePath = "fixtures/parser/parser-corpus-0057.jz",
        parserFixtureSource = Text.pack "class Eq(a) { equals = \\value -> value. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0058",
        parserFixturePath = "fixtures/parser/parser-corpus-0058.jz",
        parserFixtureSource = Text.pack "class Eq(a) { }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0059",
        parserFixturePath = "fixtures/parser/parser-corpus-0059.jz",
        parserFixtureSource = Text.pack """
        class Eq(a) { }.
        impl Eq(Int) { }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0060",
        parserFixturePath = "fixtures/parser/parser-corpus-0060.jz",
        parserFixtureSource = Text.pack "class Eq(a, a) { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0061",
        parserFixturePath = "fixtures/parser/parser-corpus-0061.jz",
        parserFixtureSource = Text.pack "class Eq(a, b) { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0062",
        parserFixturePath = "fixtures/parser/parser-corpus-0062.jz",
        parserFixtureSource = Text.pack "class Foo Bar Baz(Int, String) { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0063",
        parserFixturePath = "fixtures/parser/parser-corpus-0063.jz",
        parserFixtureSource = Text.pack "const = \\(x, y) -> x.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0064",
        parserFixturePath = "fixtures/parser/parser-corpus-0064.jz",
        parserFixtureSource = Text.pack "data Maybe = .",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0065",
        parserFixturePath = "fixtures/parser/parser-corpus-0065.jz",
        parserFixtureSource = Text.pack "data Maybe = Just value | .",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0066",
        parserFixturePath = "fixtures/parser/parser-corpus-0066.jz",
        parserFixtureSource = Text.pack "data Maybe = Just value | Nothing",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0067",
        parserFixturePath = "fixtures/parser/parser-corpus-0067.jz",
        parserFixtureSource = Text.pack "data Maybe = Just value | Nothing.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0068",
        parserFixturePath = "fixtures/parser/parser-corpus-0068.jz",
        parserFixtureSource = Text.pack "data Maybe = Nothing | Nothing value.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0069",
        parserFixturePath = "fixtures/parser/parser-corpus-0069.jz",
        parserFixtureSource = Text.pack "data Maybe a = Just b.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0070",
        parserFixturePath = "fixtures/parser/parser-corpus-0070.jz",
        parserFixtureSource = Text.pack "data Maybe a = None | Some a | Pair (a, a) [a].",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0071",
        parserFixturePath = "fixtures/parser/parser-corpus-0071.jz",
        parserFixtureSource = Text.pack "data Maybe a = Nothing | Just a.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0072",
        parserFixturePath = "fixtures/parser/parser-corpus-0072.jz",
        parserFixtureSource = Text.pack "data Pair a a = Pair a a.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0073",
        parserFixturePath = "fixtures/parser/parser-corpus-0073.jz",
        parserFixtureSource = Text.pack "f 1 + g 2 * 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0074",
        parserFixturePath = "fixtures/parser/parser-corpus-0074.jz",
        parserFixtureSource = Text.pack """
        f :: (Int -> Int) -> Int.
        f = applyToOne.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0075",
        parserFixturePath = "fixtures/parser/parser-corpus-0075.jz",
        parserFixtureSource = Text.pack """
        f :: ([Int]) -> ([Int]).
        f = (+).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0076",
        parserFixturePath = "fixtures/parser/parser-corpus-0076.jz",
        parserFixtureSource = Text.pack """
        f :: @{Eq(a), Ord(b)}: a -> b -> c.
        f = combine.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0077",
        parserFixturePath = "fixtures/parser/parser-corpus-0077.jz",
        parserFixtureSource = Text.pack """
        f :: @{Eq(a)}: a -> a.
        f = identity.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0078",
        parserFixturePath = "fixtures/parser/parser-corpus-0078.jz",
        parserFixtureSource = Text.pack """
        f :: @{}: Int.
        f = value.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0079",
        parserFixturePath = "fixtures/parser/parser-corpus-0079.jz",
        parserFixtureSource = Text.pack """
        f :: Float -> Float64.
        f = (+).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0080",
        parserFixturePath = "fixtures/parser/parser-corpus-0080.jz",
        parserFixtureSource = Text.pack """
        f :: Int -> Int -> Int.
        f = (+).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0081",
        parserFixturePath = "fixtures/parser/parser-corpus-0081.jz",
        parserFixtureSource = Text.pack """
        f :: UInt8 -> Int64 -> Float.
        f = (+).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0082",
        parserFixturePath = "fixtures/parser/parser-corpus-0082.jz",
        parserFixtureSource = Text.pack "f = (+ 10).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0083",
        parserFixturePath = "fixtures/parser/parser-corpus-0083.jz",
        parserFixtureSource = Text.pack "f = (+) 1 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0084",
        parserFixturePath = "fixtures/parser/parser-corpus-0084.jz",
        parserFixtureSource = Text.pack "f = (+).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0085",
        parserFixturePath = "fixtures/parser/parser-corpus-0085.jz",
        parserFixtureSource = Text.pack "f = (10 +).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0086",
        parserFixturePath = "fixtures/parser/parser-corpus-0086.jz",
        parserFixtureSource = Text.pack "f = \\((Just item | Also item)) -> item.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0087",
        parserFixturePath = "fixtures/parser/parser-corpus-0087.jz",
        parserFixtureSource = Text.pack "f = \\((left, right)) -> left.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0088",
        parserFixturePath = "fixtures/parser/parser-corpus-0088.jz",
        parserFixtureSource = Text.pack "f = \\(1.5) -> True.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0089",
        parserFixturePath = "fixtures/parser/parser-corpus-0089.jz",
        parserFixtureSource = Text.pack "f = \\(Just item | Also item if item > 0) -> item.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0090",
        parserFixturePath = "fixtures/parser/parser-corpus-0090.jz",
        parserFixtureSource = Text.pack "f = \\(Just item) -> item.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0091",
        parserFixturePath = "fixtures/parser/parser-corpus-0091.jz",
        parserFixtureSource = Text.pack "f = \\(True) -> 1.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0092",
        parserFixturePath = "fixtures/parser/parser-corpus-0092.jz",
        parserFixtureSource = Text.pack "f = \\([head | tail]) -> head.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0093",
        parserFixturePath = "fixtures/parser/parser-corpus-0093.jz",
        parserFixtureSource = Text.pack "f = \\([head, tail]) -> head.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0094",
        parserFixturePath = "fixtures/parser/parser-corpus-0094.jz",
        parserFixtureSource = Text.pack "f = \\(_) -> 1.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0095",
        parserFixturePath = "fixtures/parser/parser-corpus-0095.jz",
        parserFixtureSource = Text.pack "f = \\(if) -> if.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0096",
        parserFixturePath = "fixtures/parser/parser-corpus-0096.jz",
        parserFixtureSource = Text.pack "f = \\(whole @ [head | tail]) -> head.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0097",
        parserFixturePath = "fixtures/parser/parser-corpus-0097.jz",
        parserFixtureSource = Text.pack "f = \\(x,) -> x.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0098",
        parserFixturePath = "fixtures/parser/parser-corpus-0098.jz",
        parserFixtureSource = Text.pack "f = \\x -> x.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0099",
        parserFixturePath = "fixtures/parser/parser-corpus-0099.jz",
        parserFixtureSource = Text.pack """
        fns :: [(Int -> Int)].
        fns = [(+ 1)].
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0100",
        parserFixturePath = "fixtures/parser/parser-corpus-0100.jz",
        parserFixtureSource = Text.pack "id = \\(x) -> x.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0101",
        parserFixturePath = "fixtures/parser/parser-corpus-0101.jz",
        parserFixtureSource = Text.pack "if = 1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0102",
        parserFixturePath = "fixtures/parser/parser-corpus-0102.jz",
        parserFixtureSource = Text.pack "if True then 1 else 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0103",
        parserFixturePath = "fixtures/parser/parser-corpus-0103.jz",
        parserFixtureSource = Text.pack """
        impl Eq(Int) {
        equals = \\(left, right) -> left == right.
        }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0104",
        parserFixturePath = "fixtures/parser/parser-corpus-0104.jz",
        parserFixtureSource = Text.pack "impl Eq(Int) { equals :: Int. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0105",
        parserFixturePath = "fixtures/parser/parser-corpus-0105.jz",
        parserFixtureSource = Text.pack "impl Eq(Int) { equals = 1. equals = 2. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0106",
        parserFixturePath = "fixtures/parser/parser-corpus-0106.jz",
        parserFixtureSource = Text.pack "impl Eq(Int) { }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0107",
        parserFixturePath = "fixtures/parser/parser-corpus-0107.jz",
        parserFixtureSource = Text.pack "impl Eq(a) { equals = 1. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0108",
        parserFixturePath = "fixtures/parser/parser-corpus-0108.jz",
        parserFixtureSource = Text.pack "impl Eq(a) { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0109",
        parserFixturePath = "fixtures/parser/parser-corpus-0109.jz",
        parserFixtureSource = Text.pack "import A::.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0110",
        parserFixturePath = "fixtures/parser/parser-corpus-0110.jz",
        parserFixtureSource = Text.pack "import A::B.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0111",
        parserFixturePath = "fixtures/parser/parser-corpus-0111.jz",
        parserFixtureSource = Text.pack "import Lib::Math (subtract) as Math.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0112",
        parserFixturePath = "fixtures/parser/parser-corpus-0112.jz",
        parserFixtureSource = Text.pack "import Lib::Math as Math (subtract).",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0113",
        parserFixturePath = "fixtures/parser/parser-corpus-0113.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as Math.
        Math::1.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0114",
        parserFixturePath = "fixtures/parser/parser-corpus-0114.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as Math.
        Math::Result.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0115",
        parserFixturePath = "fixtures/parser/parser-corpus-0115.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as Math.
        Math::subtract.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0116",
        parserFixturePath = "fixtures/parser/parser-corpus-0116.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as Math.
        main = Math :: subtract.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0117",
        parserFixturePath = "fixtures/parser/parser-corpus-0117.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as class.
        class::subtract.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0118",
        parserFixturePath = "fixtures/parser/parser-corpus-0118.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as math.
        math :: Int.
        math = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0119",
        parserFixturePath = "fixtures/parser/parser-corpus-0119.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as math.
        math :: a.
        math = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0120",
        parserFixturePath = "fixtures/parser/parser-corpus-0120.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as math.
        math::subtract.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0121",
        parserFixturePath = "fixtures/parser/parser-corpus-0121.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as math.
        result = {
          math::subtract.
        }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0122",
        parserFixturePath = "fixtures/parser/parser-corpus-0122.jz",
        parserFixtureSource = Text.pack """
        import Lib::Math as trait.
        trait::subtract.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0123",
        parserFixturePath = "fixtures/parser/parser-corpus-0123.jz",
        parserFixtureSource = Text.pack "import Std::List ().",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0124",
        parserFixturePath = "fixtures/parser/parser-corpus-0124.jz",
        parserFixtureSource = Text.pack "import Std::List (map) as List.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0125",
        parserFixturePath = "fixtures/parser/parser-corpus-0125.jz",
        parserFixtureSource = Text.pack """
        import Std::List (map, filter).
        map.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0126",
        parserFixturePath = "fixtures/parser/parser-corpus-0126.jz",
        parserFixtureSource = Text.pack "import Std::List (map, filter, map).",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0127",
        parserFixturePath = "fixtures/parser/parser-corpus-0127.jz",
        parserFixtureSource = Text.pack "import Std::List as List (map).",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0128",
        parserFixturePath = "fixtures/parser/parser-corpus-0128.jz",
        parserFixtureSource = Text.pack """
        import Std::List as List.
        List.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0129",
        parserFixturePath = "fixtures/parser/parser-corpus-0129.jz",
        parserFixtureSource = Text.pack "import Std::List as True.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0130",
        parserFixturePath = "fixtures/parser/parser-corpus-0130.jz",
        parserFixtureSource = Text.pack "item, next",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0131",
        parserFixturePath = "fixtures/parser/parser-corpus-0131.jz",
        parserFixtureSource = Text.pack """
        left :: List(a).
        right :: [a].
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0132",
        parserFixturePath = "fixtures/parser/parser-corpus-0132.jz",
        parserFixtureSource = Text.pack "left::member right :: member",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0133",
        parserFixturePath = "fixtures/parser/parser-corpus-0133.jz",
        parserFixtureSource = Text.pack """
        math::subtract.
        import Lib::Math as math.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0134",
        parserFixturePath = "fixtures/parser/parser-corpus-0134.jz",
        parserFixtureSource = Text.pack "module .",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0135",
        parserFixturePath = "fixtures/parser/parser-corpus-0135.jz",
        parserFixtureSource = Text.pack "module A::.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0136",
        parserFixturePath = "fixtures/parser/parser-corpus-0136.jz",
        parserFixtureSource = Text.pack """
        module App::Core
        x = 1.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0137",
        parserFixturePath = "fixtures/parser/parser-corpus-0137.jz",
        parserFixtureSource = Text.pack "module App::Core = 1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0138",
        parserFixturePath = "fixtures/parser/parser-corpus-0138.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        class Eq(a) { }.
        impl Eq(Int) { }.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0139",
        parserFixturePath = "fixtures/parser/parser-corpus-0139.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        import Std::List (map).
        map.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0140",
        parserFixturePath = "fixtures/parser/parser-corpus-0140.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        module Inner::Thing {
        y = 1.
        }
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0141",
        parserFixturePath = "fixtures/parser/parser-corpus-0141.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        operator = 1.
        value = operator.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0142",
        parserFixturePath = "fixtures/parser/parser-corpus-0142.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        trait Eq { }.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0143",
        parserFixturePath = "fixtures/parser/parser-corpus-0143.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        x = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0144",
        parserFixturePath = "fixtures/parser/parser-corpus-0144.jz",
        parserFixtureSource = Text.pack """
        module App::Core {
        x = 1.
        }
        y = 2.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0145",
        parserFixturePath = "fixtures/parser/parser-corpus-0145.jz",
        parserFixtureSource = Text.pack "module App::Core.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0146",
        parserFixturePath = "fixtures/parser/parser-corpus-0146.jz",
        parserFixtureSource = Text.pack """
        module App::Internal () {
        helper = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0147",
        parserFixturePath = "fixtures/parser/parser-corpus-0147.jz",
        parserFixtureSource = Text.pack """
        module App::Main {
        # keep comment line out of spans
          import Lib::Math as Math.
          import Std::List (map).
          result = Math::answer.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0148",
        parserFixturePath = "fixtures/parser/parser-corpus-0148.jz",
        parserFixtureSource = Text.pack """
        module App::Main {
        import Lib::Math as Math.
        result = Math::answer.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0149",
        parserFixturePath = "fixtures/parser/parser-corpus-0149.jz",
        parserFixtureSource = Text.pack """
        module Demo {
        operator %% tier 2.
        (%%) = \\(left, right) -> left + right.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0150",
        parserFixturePath = "fixtures/parser/parser-corpus-0150.jz",
        parserFixtureSource = Text.pack """
        module Lib::Box (type Box, constructor Box) {
        data Box = Box value.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0151",
        parserFixturePath = "fixtures/parser/parser-corpus-0151.jz",
        parserFixtureSource = Text.pack """
        module Lib::Box (type Box, constructor Box, value Box, class Printable, legacy) {
        legacy = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0152",
        parserFixturePath = "fixtures/parser/parser-corpus-0152.jz",
        parserFixtureSource = Text.pack """
        module Lib::Box (type Box, type Box) {
        data Box = Box value.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0153",
        parserFixturePath = "fixtures/parser/parser-corpus-0153.jz",
        parserFixtureSource = Text.pack """
        module Lib::Keywords (value, constructor, type, class) {
        answer = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0154",
        parserFixturePath = "fixtures/parser/parser-corpus-0154.jz",
        parserFixtureSource = Text.pack """
        module Lib::Maybe (Maybe, Just, Nothing, mapMaybe) {
        mapMaybe = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0155",
        parserFixturePath = "fixtures/parser/parser-corpus-0155.jz",
        parserFixtureSource = Text.pack """
        module Lib::Value (answer {
        answer = 1.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0156",
        parserFixturePath = "fixtures/parser/parser-corpus-0156.jz",
        parserFixtureSource = Text.pack """
        module Lib::Value (answer) {
        answer = 1.
        }
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0157",
        parserFixturePath = "fixtures/parser/parser-corpus-0157.jz",
        parserFixtureSource = Text.pack "module Lib::Value (answer).",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0158",
        parserFixturePath = "fixtures/parser/parser-corpus-0158.jz",
        parserFixtureSource = Text.pack """
        module Lib::Value (answer, answer) {
        answer = 1.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0159",
        parserFixturePath = "fixtures/parser/parser-corpus-0159.jz",
        parserFixtureSource = Text.pack """
        module Lib::Value (answer,) {
        answer = 1.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0160",
        parserFixturePath = "fixtures/parser/parser-corpus-0160.jz",
        parserFixtureSource = Text.pack "ok = 1 + 2 == 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0161",
        parserFixturePath = "fixtures/parser/parser-corpus-0161.jz",
        parserFixtureSource = Text.pack "operator %% precedence 0.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0162",
        parserFixturePath = "fixtures/parser/parser-corpus-0162.jz",
        parserFixtureSource = Text.pack "operator %% precedence 100.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0163",
        parserFixturePath = "fixtures/parser/parser-corpus-0163.jz",
        parserFixtureSource = Text.pack """
        operator %% precedence 25.
        x = 10 %% 3 %% 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0164",
        parserFixturePath = "fixtures/parser/parser-corpus-0164.jz",
        parserFixtureSource = Text.pack """
        operator %% precedence 99.
        x = 1 + 2 %% 3 * 4.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0165",
        parserFixturePath = "fixtures/parser/parser-corpus-0165.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2 left.
        x = 10 %% 3 %% 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0166",
        parserFixturePath = "fixtures/parser/parser-corpus-0166.jz",
        parserFixtureSource = Text.pack "operator %% tier 2 sideways.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0167",
        parserFixturePath = "fixtures/parser/parser-corpus-0167.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        (%%) :: Int -> Int -> Int.
        (%%) = \\(left, right) -> left + right.
        result = 1 %% 2.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0168",
        parserFixturePath = "fixtures/parser/parser-corpus-0168.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        (%%) = \\(left, right) -> left + right.
        result = 1 %% 2 * 3.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0169",
        parserFixturePath = "fixtures/parser/parser-corpus-0169.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        module Foo { x = 1. }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0170",
        parserFixturePath = "fixtures/parser/parser-corpus-0170.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        op = (%%).
        left = (10 %%).
        right = (%% 10).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0171",
        parserFixturePath = "fixtures/parser/parser-corpus-0171.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        operator %% tier 3.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0172",
        parserFixturePath = "fixtures/parser/parser-corpus-0172.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        x = 1 + 2 %% 3 * 4.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0173",
        parserFixturePath = "fixtures/parser/parser-corpus-0173.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        x = { (%%) :: Int -> Int -> Int. 0. }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0174",
        parserFixturePath = "fixtures/parser/parser-corpus-0174.jz",
        parserFixtureSource = Text.pack """
        operator %% tier 2.
        x = { (%%) = \\(left, right) -> left + right. 0. }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0175",
        parserFixturePath = "fixtures/parser/parser-corpus-0175.jz",
        parserFixtureSource = Text.pack "operator %% tier 6.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0176",
        parserFixturePath = "fixtures/parser/parser-corpus-0176.jz",
        parserFixtureSource = Text.pack "operator + tier 2.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0177",
        parserFixturePath = "fixtures/parser/parser-corpus-0177.jz",
        parserFixtureSource = Text.pack "operator -- tier 1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0178",
        parserFixturePath = "fixtures/parser/parser-corpus-0178.jz",
        parserFixtureSource = Text.pack "operator -> tier 5.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0179",
        parserFixturePath = "fixtures/parser/parser-corpus-0179.jz",
        parserFixtureSource = Text.pack """
        operator ->? tier 4.
        x = 1 ->? 2.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0180",
        parserFixturePath = "fixtures/parser/parser-corpus-0180.jz",
        parserFixtureSource = Text.pack """
        operator :: Int.
        operator = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0181",
        parserFixturePath = "fixtures/parser/parser-corpus-0181.jz",
        parserFixtureSource = Text.pack """
        operator <| precedence 10 right.
        x = a <| b <| c.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0182",
        parserFixturePath = "fixtures/parser/parser-corpus-0182.jz",
        parserFixtureSource = Text.pack """
        operator = 1.
        value = operator.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0183",
        parserFixturePath = "fixtures/parser/parser-corpus-0183.jz",
        parserFixtureSource = Text.pack """
        operator ?> precedence 1 nonassoc.
        x = 1 $ 2 ?> 3.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0184",
        parserFixturePath = "fixtures/parser/parser-corpus-0184.jz",
        parserFixtureSource = Text.pack """
        operator ?> precedence 1 nonassoc.
        x = case value { | _ -> 1 $ 2 ?> 3 }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0185",
        parserFixturePath = "fixtures/parser/parser-corpus-0185.jz",
        parserFixtureSource = Text.pack """
        operator ?> precedence 1 nonassoc.
        x = case value { | _ if 1 $ 2 ?> 3 -> 1 }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0186",
        parserFixturePath = "fixtures/parser/parser-corpus-0186.jz",
        parserFixtureSource = Text.pack """
        operator ?> precedence 10 nonassoc.
        x = 1 ?> 2 ?> 3.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0187",
        parserFixturePath = "fixtures/parser/parser-corpus-0187.jz",
        parserFixtureSource = Text.pack """
        operator ?> precedence 4 nonassoc.
        x = 1 + 2 ?> 3.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0188",
        parserFixturePath = "fixtures/parser/parser-corpus-0188.jz",
        parserFixtureSource = Text.pack "operator abc tier 2.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0189",
        parserFixturePath = "fixtures/parser/parser-corpus-0189.jz",
        parserFixtureSource = Text.pack """
        operator ~~ tier 5.
        x = f ~~ g ~~ z.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0190",
        parserFixturePath = "fixtures/parser/parser-corpus-0190.jz",
        parserFixtureSource = Text.pack "pair 'a' \"Jazz\".",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0191",
        parserFixturePath = "fixtures/parser/parser-corpus-0191.jz",
        parserFixtureSource = Text.pack """
        pair :: (Int, Bool).
        pair = (1, True).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0192",
        parserFixturePath = "fixtures/parser/parser-corpus-0192.jz",
        parserFixtureSource = Text.pack """
        pair :: @{}: (Int, Bool).
        pair = (1, True).
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0193",
        parserFixturePath = "fixtures/parser/parser-corpus-0193.jz",
        parserFixtureSource = Text.pack """
        result = Eq::equals 1 1.
        result.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0194",
        parserFixturePath = "fixtures/parser/parser-corpus-0194.jz",
        parserFixtureSource = Text.pack """
        result = f {
          x = 1.
          x.
        }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0195",
        parserFixturePath = "fixtures/parser/parser-corpus-0195.jz",
        parserFixtureSource = Text.pack "run = (\\(x) -> x) 1.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0196",
        parserFixturePath = "fixtures/parser/parser-corpus-0196.jz",
        parserFixtureSource = Text.pack """
        scope = {
          operator = 1.
          operator.
        }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0197",
        parserFixturePath = "fixtures/parser/parser-corpus-0197.jz",
        parserFixtureSource = Text.pack "shadow = \\(x, x) -> x.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0198",
        parserFixturePath = "fixtures/parser/parser-corpus-0198.jz",
        parserFixtureSource = Text.pack "sumPair = \\((left, right)) -> left + right.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0199",
        parserFixturePath = "fixtures/parser/parser-corpus-0199.jz",
        parserFixtureSource = Text.pack "thunk = \\(()) -> 42.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0200",
        parserFixturePath = "fixtures/parser/parser-corpus-0200.jz",
        parserFixtureSource = Text.pack "thunk = \\((),) -> 42.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0201",
        parserFixturePath = "fixtures/parser/parser-corpus-0201.jz",
        parserFixtureSource = Text.pack "thunk = \\() -> 42.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0202",
        parserFixturePath = "fixtures/parser/parser-corpus-0202.jz",
        parserFixtureSource = Text.pack "trait Eq { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0203",
        parserFixturePath = "fixtures/parser/parser-corpus-0203.jz",
        parserFixtureSource = Text.pack "trait eq { }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0204",
        parserFixturePath = "fixtures/parser/parser-corpus-0204.jz",
        parserFixtureSource = Text.pack """
        unit :: ().
        unit = ().
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0205",
        parserFixturePath = "fixtures/parser/parser-corpus-0205.jz",
        parserFixtureSource = Text.pack """
        unit :: @{}: ().
        unit = ().
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0206",
        parserFixturePath = "fixtures/parser/parser-corpus-0206.jz",
        parserFixtureSource = Text.pack "value 42.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0207",
        parserFixturePath = "fixtures/parser/parser-corpus-0207.jz",
        parserFixtureSource = Text.pack """
        value :: Maybe(Char).
        map :: (a -> b) -> List(a) -> [b].
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0208",
        parserFixturePath = "fixtures/parser/parser-corpus-0208.jz",
        parserFixtureSource = Text.pack """
        value :: a.
        value = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0209",
        parserFixturePath = "fixtures/parser/parser-corpus-0209.jz",
        parserFixtureSource = Text.pack "value = 42.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0210",
        parserFixturePath = "fixtures/parser/parser-corpus-0210.jz",
        parserFixtureSource = Text.pack """
        value = id @ 1.
        value.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0211",
        parserFixturePath = "fixtures/parser/parser-corpus-0211.jz",
        parserFixtureSource = Text.pack """
        value = id @Int 1.
        value.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0212",
        parserFixturePath = "fixtures/parser/parser-corpus-0212.jz",
        parserFixtureSource = Text.pack """
        value = id @Maybe().
        value.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0213",
        parserFixturePath = "fixtures/parser/parser-corpus-0213.jz",
        parserFixtureSource = Text.pack "value ` 42.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0214",
        parserFixturePath = "fixtures/parser/parser-corpus-0214.jz",
        parserFixtureSource = Text.pack "value.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0215",
        parserFixturePath = "fixtures/parser/parser-corpus-0215.jz",
        parserFixtureSource = Text.pack """
        value::Int.
        other = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0216",
        parserFixturePath = "fixtures/parser/parser-corpus-0216.jz",
        parserFixtureSource = Text.pack """
        value::Int.
        value = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0217",
        parserFixturePath = "fixtures/parser/parser-corpus-0217.jz",
        parserFixtureSource = Text.pack "whole @ Pair (left, right) [1, True, _, item] [head | tail] | Nothing -> body",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0218",
        parserFixturePath = "fixtures/parser/parser-corpus-0218.jz",
        parserFixtureSource = Text.pack """
        x :: Int
        class Eq { }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0219",
        parserFixturePath = "fixtures/parser/parser-corpus-0219.jz",
        parserFixtureSource = Text.pack """
        x :: Int
        x = 1.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0220",
        parserFixturePath = "fixtures/parser/parser-corpus-0220.jz",
        parserFixtureSource = Text.pack """
        x :: Int.
        x = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0221",
        parserFixturePath = "fixtures/parser/parser-corpus-0221.jz",
        parserFixtureSource = Text.pack """
        x :: Int8.
        x = 1.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0222",
        parserFixturePath = "fixtures/parser/parser-corpus-0222.jz",
        parserFixtureSource = Text.pack """
        x :: [[Bool]].
        x = [[True], [False]].
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0223",
        parserFixturePath = "fixtures/parser/parser-corpus-0223.jz",
        parserFixtureSource = Text.pack """
        x = (%%).
        operator %% tier 2.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0224",
        parserFixturePath = "fixtures/parser/parser-corpus-0224.jz",
        parserFixtureSource = Text.pack "x = (+ 1) 2 * 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0225",
        parserFixturePath = "fixtures/parser/parser-corpus-0225.jz",
        parserFixtureSource = Text.pack "x = (+) 1 2 * 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0226",
        parserFixturePath = "fixtures/parser/parser-corpus-0226.jz",
        parserFixtureSource = Text.pack "x = (1 + 2).",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0227",
        parserFixturePath = "fixtures/parser/parser-corpus-0227.jz",
        parserFixtureSource = Text.pack "x = -1.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0228",
        parserFixturePath = "fixtures/parser/parser-corpus-0228.jz",
        parserFixtureSource = Text.pack "x = 1 % 2.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0229",
        parserFixturePath = "fixtures/parser/parser-corpus-0229.jz",
        parserFixtureSource = Text.pack """
        x = 1 %% 2.
        operator %% tier 2.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0230",
        parserFixturePath = "fixtures/parser/parser-corpus-0230.jz",
        parserFixtureSource = Text.pack "x = 1 + 2 * 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0231",
        parserFixturePath = "fixtures/parser/parser-corpus-0231.jz",
        parserFixtureSource = Text.pack "x = 1 + 2 - 3.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0232",
        parserFixturePath = "fixtures/parser/parser-corpus-0232.jz",
        parserFixtureSource = Text.pack "x = 1 +.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0233",
        parserFixturePath = "fixtures/parser/parser-corpus-0233.jz",
        parserFixtureSource = Text.pack "x = 1 y = 2.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0234",
        parserFixturePath = "fixtures/parser/parser-corpus-0234.jz",
        parserFixtureSource = Text.pack """
        x = 1.
        # parser should ignore this line comment
        x.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0235",
        parserFixturePath = "fixtures/parser/parser-corpus-0235.jz",
        parserFixtureSource = Text.pack """
        x = 1.
        module App::Core {
        y = 2.
        }
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0236",
        parserFixturePath = "fixtures/parser/parser-corpus-0236.jz",
        parserFixtureSource = Text.pack """
        x = 1.
        x.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0237",
        parserFixturePath = "fixtures/parser/parser-corpus-0237.jz",
        parserFixtureSource = Text.pack """
        x = 1.
        { x. }.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0238",
        parserFixturePath = "fixtures/parser/parser-corpus-0238.jz",
        parserFixtureSource = Text.pack """
        x = 1.5.
        y = 2.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0239",
        parserFixturePath = "fixtures/parser/parser-corpus-0239.jz",
        parserFixtureSource = Text.pack "x = 10 - 3 - 1.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0240",
        parserFixturePath = "fixtures/parser/parser-corpus-0240.jz",
        parserFixtureSource = Text.pack "x = 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369.0.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0241",
        parserFixturePath = "fixtures/parser/parser-corpus-0241.jz",
        parserFixtureSource = Text.pack "x = 9223372036854775808.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0242",
        parserFixturePath = "fixtures/parser/parser-corpus-0242.jz",
        parserFixtureSource = Text.pack "x = 9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.0.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0243",
        parserFixturePath = "fixtures/parser/parser-corpus-0243.jz",
        parserFixtureSource = Text.pack "x = a && b.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0244",
        parserFixturePath = "fixtures/parser/parser-corpus-0244.jz",
        parserFixtureSource = Text.pack "x = case + { | 0 -> True }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0245",
        parserFixturePath = "fixtures/parser/parser-corpus-0245.jz",
        parserFixtureSource = Text.pack "x = case 1 { | 1.5 -> True | _ -> False }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0246",
        parserFixturePath = "fixtures/parser/parser-corpus-0246.jz",
        parserFixtureSource = Text.pack "x = case f { y = 1. y. } { | 1 -> True | _ -> False }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0247",
        parserFixturePath = "fixtures/parser/parser-corpus-0247.jz",
        parserFixtureSource = Text.pack "x = case f { y = 1. y. } { | 1 True }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0248",
        parserFixturePath = "fixtures/parser/parser-corpus-0248.jz",
        parserFixtureSource = Text.pack "x = case f { y = 1. y. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0249",
        parserFixturePath = "fixtures/parser/parser-corpus-0249.jz",
        parserFixtureSource = Text.pack "x = case f { y = 1.5 }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0250",
        parserFixturePath = "fixtures/parser/parser-corpus-0250.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item != 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0251",
        parserFixturePath = "fixtures/parser/parser-corpus-0251.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item < 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0252",
        parserFixturePath = "fixtures/parser/parser-corpus-0252.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item < 0 | Just if ok -> item | _ -> m }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0253",
        parserFixturePath = "fixtures/parser/parser-corpus-0253.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item <= 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0254",
        parserFixturePath = "fixtures/parser/parser-corpus-0254.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item == 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0255",
        parserFixturePath = "fixtures/parser/parser-corpus-0255.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item == 0 | Just if ok then 1 else 2 -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0256",
        parserFixturePath = "fixtures/parser/parser-corpus-0256.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item > 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0257",
        parserFixturePath = "fixtures/parser/parser-corpus-0257.jz",
        parserFixtureSource = Text.pack "x = case m { | item if item >= 0 | Just -> item | _ -> m }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0258",
        parserFixturePath = "fixtures/parser/parser-corpus-0258.jz",
        parserFixtureSource = Text.pack "x = case n { 0 -> True }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0259",
        parserFixturePath = "fixtures/parser/parser-corpus-0259.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> 0 | _ | 2 -> 1 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0260",
        parserFixturePath = "fixtures/parser/parser-corpus-0260.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> 0 | item | item @ _ -> item }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0261",
        parserFixturePath = "fixtures/parser/parser-corpus-0261.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> 0 | item | other -> item }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0262",
        parserFixturePath = "fixtures/parser/parser-corpus-0262.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> 1 | 2 | _ -> 3 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0263",
        parserFixturePath = "fixtures/parser/parser-corpus-0263.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> True | _ -> False }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0264",
        parserFixturePath = "fixtures/parser/parser-corpus-0264.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> \\(y) -> y | _ -> 3 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0265",
        parserFixturePath = "fixtures/parser/parser-corpus-0265.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> case y { | 1 -> True | _ -> False } | _ -> False }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0266",
        parserFixturePath = "fixtures/parser/parser-corpus-0266.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 -> if True then 1 else 2 | _ -> 3 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0267",
        parserFixturePath = "fixtures/parser/parser-corpus-0267.jz",
        parserFixtureSource = Text.pack "x = case n { | 0 True }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0268",
        parserFixturePath = "fixtures/parser/parser-corpus-0268.jz",
        parserFixtureSource = Text.pack "x = case n { | _ -> 0 | 1 | 2 -> 1 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0269",
        parserFixturePath = "fixtures/parser/parser-corpus-0269.jz",
        parserFixtureSource = Text.pack "x = case pair { | (left, [right) ]) -> left | _ -> 0 }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0270",
        parserFixturePath = "fixtures/parser/parser-corpus-0270.jz",
        parserFixtureSource = Text.pack "x = case pair { | (left, right) -> left | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0271",
        parserFixturePath = "fixtures/parser/parser-corpus-0271.jz",
        parserFixtureSource = Text.pack "x = case value { | 0 -> 0 | item if left | right -> 1 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0272",
        parserFixturePath = "fixtures/parser/parser-corpus-0272.jz",
        parserFixtureSource = Text.pack "x = case value { | 0 -> 1 | _ False }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0273",
        parserFixturePath = "fixtures/parser/parser-corpus-0273.jz",
        parserFixtureSource = Text.pack "x = case value { | 0 -> 1 | _ y }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0274",
        parserFixturePath = "fixtures/parser/parser-corpus-0274.jz",
        parserFixtureSource = Text.pack "x = case value { | Just Nothing -> 1 | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0275",
        parserFixturePath = "fixtures/parser/parser-corpus-0275.jz",
        parserFixtureSource = Text.pack "x = case value { | Just [head | tail] -> head | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0276",
        parserFixturePath = "fixtures/parser/parser-corpus-0276.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item -> 1 | 2 | Nothing -> 3 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0277",
        parserFixturePath = "fixtures/parser/parser-corpus-0277.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item -> item | Nothing -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0278",
        parserFixturePath = "fixtures/parser/parser-corpus-0278.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item if item > 0 -> item | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0279",
        parserFixturePath = "fixtures/parser/parser-corpus-0279.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item | -> item }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0280",
        parserFixturePath = "fixtures/parser/parser-corpus-0280.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item | Also item -> item | f | Nothing -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0281",
        parserFixturePath = "fixtures/parser/parser-corpus-0281.jz",
        parserFixtureSource = Text.pack "x = case value { | Just item | Also item if item > 0 -> item | Nothing -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0282",
        parserFixturePath = "fixtures/parser/parser-corpus-0282.jz",
        parserFixtureSource = Text.pack "x = case value { | Pair Nothing item -> item | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0283",
        parserFixturePath = "fixtures/parser/parser-corpus-0283.jz",
        parserFixtureSource = Text.pack "x = case value { | Pair whole @ Nothing item -> item | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0284",
        parserFixturePath = "fixtures/parser/parser-corpus-0284.jz",
        parserFixtureSource = Text.pack "x = case value { | _ -> 1 | 2 | 3 -> 4 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0285",
        parserFixturePath = "fixtures/parser/parser-corpus-0285.jz",
        parserFixtureSource = Text.pack "x = case value { | _ -> 1 | Just a b }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0286",
        parserFixturePath = "fixtures/parser/parser-corpus-0286.jz",
        parserFixtureSource = Text.pack "x = case value { | _ -> 1 | Nothing }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0287",
        parserFixturePath = "fixtures/parser/parser-corpus-0287.jz",
        parserFixtureSource = Text.pack "x = case value { | _ -> 1 | [2] }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0288",
        parserFixturePath = "fixtures/parser/parser-corpus-0288.jz",
        parserFixtureSource = Text.pack "x = case value { | _ -> 1 | f \\(y) -> y }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0289",
        parserFixturePath = "fixtures/parser/parser-corpus-0289.jz",
        parserFixtureSource = Text.pack "x = case value { | item -> item }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0290",
        parserFixturePath = "fixtures/parser/parser-corpus-0290.jz",
        parserFixtureSource = Text.pack "x = case value { | item if -> item }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0291",
        parserFixturePath = "fixtures/parser/parser-corpus-0291.jz",
        parserFixtureSource = Text.pack "x = case value { | item if left == right | True -> 1 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0292",
        parserFixturePath = "fixtures/parser/parser-corpus-0292.jz",
        parserFixtureSource = Text.pack "x = case value { | item if left | True -> 1 | other if left | Nothing -> 2 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0293",
        parserFixturePath = "fixtures/parser/parser-corpus-0293.jz",
        parserFixtureSource = Text.pack "x = case value { | whole @ Just item -> whole | _ -> value }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0294",
        parserFixturePath = "fixtures/parser/parser-corpus-0294.jz",
        parserFixtureSource = Text.pack "x = case values { | 0 -> 1 | [head tail] -> head }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0295",
        parserFixturePath = "fixtures/parser/parser-corpus-0295.jz",
        parserFixtureSource = Text.pack "x = case values { | [head tail] -> head }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0296",
        parserFixturePath = "fixtures/parser/parser-corpus-0296.jz",
        parserFixtureSource = Text.pack "x = case values { | [head | tail] -> head | _ -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0297",
        parserFixturePath = "fixtures/parser/parser-corpus-0297.jz",
        parserFixtureSource = Text.pack "x = case values { | [head, _] -> head | [] -> 0 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0298",
        parserFixturePath = "fixtures/parser/parser-corpus-0298.jz",
        parserFixtureSource = Text.pack "x = case values { | _ -> 1 | [head] 2 }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0299",
        parserFixturePath = "fixtures/parser/parser-corpus-0299.jz",
        parserFixtureSource = Text.pack "x = f $ g $ z.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0300",
        parserFixturePath = "fixtures/parser/parser-corpus-0300.jz",
        parserFixtureSource = Text.pack "x = f x + g y * z.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0301",
        parserFixturePath = "fixtures/parser/parser-corpus-0301.jz",
        parserFixtureSource = Text.pack "x = if True then 1 else 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0302",
        parserFixturePath = "fixtures/parser/parser-corpus-0302.jz",
        parserFixtureSource = Text.pack "x = if cond then if inner then a else b else c.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0303",
        parserFixturePath = "fixtures/parser/parser-corpus-0303.jz",
        parserFixtureSource = Text.pack "x = if cond then x else y else z.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0304",
        parserFixturePath = "fixtures/parser/parser-corpus-0304.jz",
        parserFixtureSource = Text.pack "x = if cond then x.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0305",
        parserFixturePath = "fixtures/parser/parser-corpus-0305.jz",
        parserFixtureSource = Text.pack "x = if x > 0 then 1 else 2.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0306",
        parserFixturePath = "fixtures/parser/parser-corpus-0306.jz",
        parserFixtureSource = Text.pack """
        x = { module App::Core {
        y = 1.
        } y. }.
        """,
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0307",
        parserFixturePath = "fixtures/parser/parser-corpus-0307.jz",
        parserFixtureSource = Text.pack "x = { operator %% tier 2. y = 1. }.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0308",
        parserFixturePath = "fixtures/parser/parser-corpus-0308.jz",
        parserFixtureSource = Text.pack "x = { y = 1. y.",
        parserFixtureExpectation = ParserRejected
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0309",
        parserFixturePath = "fixtures/parser/parser-corpus-0309.jz",
        parserFixtureSource = Text.pack """
        x16 = 1.5f16.
        x32 = 2.5f32.
        x64 = 3.5f64.
        """,
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0310",
        parserFixturePath = "fixtures/parser/parser-corpus-0310.jz",
        parserFixtureSource = Text.pack "{ Make::make. }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0311",
        parserFixturePath = "fixtures/parser/parser-corpus-0311.jz",
        parserFixtureSource = Text.pack "{ Result::a. }.",
        parserFixtureExpectation = ParserAccepted
      },
    ParserFixture
      { parserFixtureName = "parser-corpus-0312",
        parserFixturePath = "fixtures/parser/parser-corpus-0312.jz",
        parserFixtureSource = Text.pack "{ x = 1. x. }.",
        parserFixtureExpectation = ParserAccepted
      }
  ]

expressionFoundationFixtures :: [ParserFixture]
expressionFoundationFixtures =
  [ fixture
      "reserved-true-signature"
      """
      True::Int.
      """
      ParserRejected,
    fixture
      "reserved-false-signature"
      """
      False::Int.
      """
      ParserRejected,
    fixture
      "spaced-reserved-true-signature"
      """
      True :: Int.
      """
      ParserRejected,
    fixture
      "spaced-reserved-false-signature"
      """
      False:: Int.
      """
      ParserRejected,
    fixture
      "identifier-operator-tier"
      """
      operator plus tier 1.
      """
      ParserRejected,
    fixture
      "identifier-operator-precedence"
      """
      operator plus precedence 1.
      """
      ParserRejected,
    fixture
      "nested-identifier-operator-tier"
      """
      { operator plus tier 1. }.
      """
      ParserRejected,
    fixture
      "parenthesized-signature-statement-boundary"
      """
      Result::(value) Other = 0. Result = 1.
      """
      ParserRejected,
    fixture "empty-program" "" ParserAccepted,
    fixture
      "empty-block"
      """
      {}.
      """
      ParserAccepted,
    fixture
      "grouped-name"
      """
      (value).
      """
      ParserAccepted,
    fixture
      "empty-list"
      """
      [].
      """
      ParserAccepted,
    fixture
      "list-literals"
      """
      [1, True, 'x', "text"].
      """
      ParserAccepted,
    fixture
      "parenthesized-application"
      """
      (identity) 1.
      """
      ParserAccepted,
    fixture
      "list-missing-close"
      """
      [1, 2.
      """
      ParserRejected,
    fixture
      "list-trailing-comma"
      """
      [1,].
      """
      ParserRejected,
    fixture
      "tuple-missing-close"
      """
      (1, 2.
      """
      ParserRejected,
    fixture
      "tuple-trailing-comma"
      """
      (1,).
      """
      ParserRejected,
    fixture
      "binding-missing-rhs"
      """
      value = .
      """
      ParserRejected,
    fixture
      "binding-missing-dot"
      """
      value = 1
      """
      ParserRejected,
    fixture
      "expression-missing-dot"
      """
      value
      """
      ParserRejected,
    fixture
      "qualified-missing-member"
      """
      Alias::.
      """
      ParserRejected,
    fixture "qualified-whitespace" "value = Alias:: value." ParserRejected,
    fixture
      "dot-without-expression"
      """
      .
      """
      ParserRejected,
    fixture
      "max-float64"
      """
      179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0.
      """
      ParserAccepted
  ]
  where
    fixture name source expectation =
      ParserFixture
        { parserFixtureName = "expression-foundation-" <> name,
          parserFixturePath = Text.unpack ("fixtures/parser/expression-foundation-" <> name <> ".jz"),
          parserFixtureSource = source,
          parserFixtureExpectation = expectation
        }
