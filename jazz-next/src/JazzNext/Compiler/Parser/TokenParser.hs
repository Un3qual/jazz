{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec adapter for parsing the lexer token stream.
module JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    failDiagnosticTokenParser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseIdentifier,
    parseOperator,
    parseToken,
    parseTokenKind,
    parseTokenWhere,
    parseDiagnostic,
    parseDiagnosticAt,
    peekToken,
    runTokenParser,
    runTokenParserPrefix
  ) where

import Control.Applicative
  ( optional
  )
import Data.List.NonEmpty
  ( NonEmpty
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    diagnosticSummary,
    setDiagnosticPrimaryLabel
  )
import qualified JazzNext.Compiler.Diagnostics as Diagnostics
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import Text.Megaparsec
  ( Parsec
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error
  ( ErrorFancy (..),
    ParseError (..),
    ShowErrorComponent (..)
  )

data ParserError = ParserError Diagnostic
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError diagnostic) = Text.unpack (diagnosticSummary diagnostic)

type Parser = Parsec ParserError [Token]

runTokenParser :: Text -> Parser a -> [Token] -> Either Diagnostic a
runTokenParser label parser tokens =
  case MP.runParser (parser <* MP.eof) (Text.unpack label) tokens of
    Right value -> Right value
    Left bundle -> Left (tokenParserDiagnostic bundle)

runTokenParserPrefix :: Text -> Parser a -> [Token] -> Either Diagnostic (a, [Token])
runTokenParserPrefix label parser tokens =
  case MP.runParser ((,) <$> parser <*> MP.getInput) (Text.unpack label) tokens of
    Right value -> Right value
    Left bundle -> Left (tokenParserDiagnostic bundle)

parseAnyToken :: Parser Token
parseAnyToken =
  MP.anySingle

peekToken :: Parser (Maybe Token)
peekToken =
  optional (MP.lookAhead parseAnyToken)

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

parseOperator :: Parser Text
parseOperator =
  tokenLexeme
    <$> parseTokenWhere
      ( \token ->
          case tokenKind token of
            TOperator {} -> True
            _ -> False
      )
      "operator"

parseTokenWhere :: (Token -> Bool) -> Text -> Parser Token
parseTokenWhere matches expectedDescription = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing ->
      failTokenParser ("expected " <> expectedDescription <> " before end of input")
    Just token
      | matches token -> parseAnyToken
      | otherwise ->
          failTokenParserAt
            (tokenSpan token)
            ( "expected "
                <> expectedDescription
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )

failTokenParser :: Text -> Parser a
failTokenParser message =
  failDiagnosticTokenParser (parseDiagnostic message)

failTokenParserAt :: SourceSpan -> Text -> Parser a
failTokenParserAt spanValue message =
  failDiagnosticTokenParser (parseDiagnosticAt spanValue message)

failDiagnosticTokenParser :: Diagnostic -> Parser a
failDiagnosticTokenParser diagnostic =
  MP.customFailure (ParserError diagnostic)

tokenParserDiagnostic :: MP.ParseErrorBundle [Token] ParserError -> Diagnostic
tokenParserDiagnostic bundle =
  case firstCustomParserError (MP.bundleErrors bundle) of
    Just diagnostic -> diagnostic
    Nothing -> parseDiagnostic "unexpected token stream parse error"

firstCustomParserError :: NonEmpty (ParseError [Token] ParserError) -> Maybe Diagnostic
firstCustomParserError errors =
  firstJust (map customErrorMessage (NonEmpty.toList errors))
  where
    customErrorMessage parseError =
      case parseError of
        FancyError _ fancyErrors ->
          firstJust
            [ Just diagnostic
              | ErrorCustom (ParserError diagnostic) <- Set.toList fancyErrors
            ]
        TrivialError {} -> Nothing

    firstJust values =
      case values of
        [] -> Nothing
        Just value : _ -> Just value
        Nothing : rest -> firstJust rest

renderExpectedTokenKind :: TokenKind -> Text
renderExpectedTokenKind expectedKind =
  case expectedKind of
    TIdentifier name -> "'" <> name <> "'"
    TModule -> "'module'"
    TImport -> "'import'"
    TAs -> "'as'"
    TData -> "'data'"
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

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = Diagnostics.mkErrorDiagnostic E0001 Diagnostics.CompilationOrigin

parseDiagnosticAt :: SourceSpan -> Text -> Diagnostic
parseDiagnosticAt spanValue =
  setDiagnosticPrimaryLabel spanValue "here" . parseDiagnostic
