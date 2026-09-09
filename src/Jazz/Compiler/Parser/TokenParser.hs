{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec adapter for parsing the lexer token stream.
module Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failParserFailure,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseIdentifier,
    parseToken,
    parseTokenKind,
    parseTokenWhere,
    peekToken,
    runTokenParser,
    runTokenParserDetailed,
    runTokenParserPrefix,
    runTokenParserPrefixDetailed,
    runTokenStreamParserDetailed,
    runTokenStreamParserPrefix,
    runTokenStreamParserPrefixDetailed,
  )
where

import Control.Applicative
  ( optional,
  )
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.List.NonEmpty
  ( NonEmpty,
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    diagnosticSummary,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
    ParserInternalInvariant (..),
    parserFailure,
    parserFailureAt,
    parserFailureDiagnostic,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    tokenStreamFromList,
    tokenStreamToList,
  )
import Text.Megaparsec
  ( Parsec,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error
  ( ErrorFancy (..),
    ParseError (..),
    ShowErrorComponent (..),
  )

newtype ParserError
  = ParserError ParserFailure
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError failure) =
    Text.unpack (diagnosticSummary (parserFailureDiagnostic failure))

type Parser = Parsec ParserError TokenStream

runTokenParser :: Text -> Parser a -> [Token] -> Either Diagnostic a
runTokenParser label parser tokens =
  first parserFailureDiagnostic (runTokenParserDetailed label parser tokens)

runTokenParserDetailed :: Text -> Parser a -> [Token] -> Either ParserFailure a
runTokenParserDetailed label parser tokens =
  runTokenStreamParserDetailed label parser (tokenStreamFromList tokens)

runTokenStreamParserDetailed :: Text -> Parser a -> TokenStream -> Either ParserFailure a
runTokenStreamParserDetailed label parser tokens =
  case MP.runParser (parser <* requireEndOfInput) (Text.unpack label) tokens of
    Right value -> Right value
    Left bundle -> Left (tokenParserFailure bundle)

runTokenParserPrefix :: Text -> Parser a -> [Token] -> Either Diagnostic (a, [Token])
runTokenParserPrefix label parser tokens =
  first parserFailureDiagnostic (runTokenParserPrefixDetailed label parser tokens)

runTokenParserPrefixDetailed :: Text -> Parser a -> [Token] -> Either ParserFailure (a, [Token])
runTokenParserPrefixDetailed label parser tokens =
  fmap (fmap tokenStreamToList) (runTokenStreamParserPrefixDetailed label parser (tokenStreamFromList tokens))

runTokenStreamParserPrefix :: Text -> Parser a -> TokenStream -> Either Diagnostic (a, TokenStream)
runTokenStreamParserPrefix label parser tokens =
  first parserFailureDiagnostic (runTokenStreamParserPrefixDetailed label parser tokens)

runTokenStreamParserPrefixDetailed :: Text -> Parser a -> TokenStream -> Either ParserFailure (a, TokenStream)
runTokenStreamParserPrefixDetailed label parser tokens =
  case MP.runParser ((,) <$> parser <*> MP.getInput) (Text.unpack label) tokens of
    Right value -> Right value
    Left bundle -> Left (tokenParserFailure bundle)

parseAnyToken :: Parser Token
parseAnyToken =
  MP.anySingle

peekToken :: Parser (Maybe Token)
peekToken =
  optional (MP.lookAhead parseAnyToken)

requireEndOfInput :: Parser ()
requireEndOfInput = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing -> pure ()
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        ( ExpectedSyntax
            "end of input"
            (ParserFoundToken (tokenKind token) (tokenLexeme token))
        )

parseToken :: TokenKind -> Parser Token
parseToken expectedKind =
  parseTokenWhere
    (\token -> tokenKind token == expectedKind)
    (renderExpectedTokenKind expectedKind)

parseTokenKind :: TokenKind -> Parser TokenKind
parseTokenKind expectedKind =
  tokenKind <$> parseToken expectedKind

parseIdentifier :: Parser Text
parseIdentifier =
  tokenLexeme
    <$> parseTokenWhere
      ( \token ->
          case tokenKind token of
            TIdentifier {} -> True
            _ -> False
      )
      "identifier"

parseTokenWhere :: (Token -> Bool) -> Text -> Parser Token
parseTokenWhere matches expectedDescription = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing ->
      failParserFailure
        (parserFailure (ExpectedSyntax expectedDescription ParserEndOfInput))
    Just token
      | matches token -> parseAnyToken
      | otherwise ->
          failParserFailure
            ( parserFailureAt
                (tokenSpan token)
                ( ExpectedSyntax
                    expectedDescription
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                )
            )

failTokenParser :: ParserFailureReason -> Parser a
failTokenParser = failParserFailure . parserFailure

failTokenParserAt :: SourceSpan -> ParserFailureReason -> Parser a
failTokenParserAt spanValue = failParserFailure . parserFailureAt spanValue

failParserFailure :: ParserFailure -> Parser a
failParserFailure = MP.customFailure . ParserError

tokenParserFailure :: MP.ParseErrorBundle TokenStream ParserError -> ParserFailure
tokenParserFailure bundle =
  case firstCustomParserError (MP.bundleErrors bundle) of
    Just (ParserError failure) -> failure
    Nothing -> parserFailure (InternalParserFailure TokenStreamParseFailure)

firstCustomParserError :: NonEmpty (ParseError TokenStream ParserError) -> Maybe ParserError
firstCustomParserError errors =
  asum (map customErrorMessage (NonEmpty.toList errors))
  where
    customErrorMessage parseError =
      case parseError of
        FancyError _ fancyErrors ->
          asum
            [ Just parserError
            | ErrorCustom parserError <- Set.toList fancyErrors
            ]
        TrivialError {} -> Nothing

renderExpectedTokenKind :: TokenKind -> Text
renderExpectedTokenKind expectedKind =
  case expectedKind of
    TIdentifier name -> "'" <> name <> "'"
    TModule -> "'module'"
    TImport -> "'import'"
    TAs -> "'as'"
    TData -> "'data'"
    TValue -> "'value'"
    TIf -> "'if'"
    TThen -> "'then'"
    TElse -> "'else'"
    TCase -> "'case'"
    TLambda -> "'\\'"
    TArrow -> "'->'"
    TAt -> "'@'"
    TInt value -> Text.pack (show value)
    TChar value -> Text.pack (show value)
    TText value -> Text.pack (show value)
    TEquals -> "'='"
    TOperator symbol -> "'" <> symbol <> "'"
    TColon -> "':'"
    TColonColon -> "'::'"
    TDot -> "'.'"
    TLBrace -> "'{'"
    TRBrace -> "'}'"
    TLParen -> "'('"
    TRParen -> "')'"
    TLBracket -> "'['"
    TRBracket -> "']'"
    TComma -> "','"
