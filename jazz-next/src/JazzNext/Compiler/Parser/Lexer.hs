{-# LANGUAGE OverloadedStrings #-}

-- | Bootstrap lexer for the current surface syntax. It keeps the token set
-- intentionally small while preserving spans for diagnostics.
module JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
    tokenize
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (chr, isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace, ord)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    mkDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.Parser.Operator
  ( isStage2OperatorSymbolChar
  )
import Text.Megaparsec
  ( Parsec
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( char
  )
import Text.Megaparsec.Error
  ( ErrorFancy (..),
    ParseError (..),
    ShowErrorComponent (..),
    errorBundlePretty
  )
import Text.Megaparsec.Pos
  ( unPos
  )

-- | Token vocabulary understood by the current parser foundation.
data TokenKind
  = TIdentifier Text
  | TModule
  | TImport
  | TAs
  | TData
  | TIf
  | TElse
  | TCase
  | TLambda
  | TArrow
  | TAt
  | TInt Integer
  | TChar Char
  | TText Text
  | TEquals
  | TOperator Text
  | TColon
  | TColonColon
  | TDot
  | TLBrace
  | TRBrace
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TComma
  deriving (Eq, Ord, Show)

-- | Concrete token annotated with the original lexeme and its starting source
-- span so later parser errors can report precise locations.
data Token = Token
  { tokenKind :: TokenKind,
    tokenLexeme :: Text,
    tokenSpan :: SourceSpan
  }
  deriving (Eq, Ord, Show)

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  spanLine (tokenSpan leftToken) == spanLine (tokenSpan rightToken)
    && spanColumn (tokenSpan rightToken)
      == spanColumn (tokenSpan leftToken) + Text.length (tokenLexeme leftToken)

data LexerError = LexerError Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent LexerError where
  showErrorComponent (LexerError message) = Text.unpack message

type LexerParser = Parsec LexerError Text

-- Tokenizes the current parser foundation grammar while preserving 1-based
-- line/column spans for diagnostics.
tokenize :: Text -> Either Diagnostic [Token]
tokenize source =
  case MP.runParser (skipIgnored *> lexerTokens <* MP.eof) "jazz-next source" source of
    Right tokens -> Right tokens
    Left bundle -> Left (parseDiagnostic (lexerErrorMessage bundle))

lexerTokens :: LexerParser [Token]
lexerTokens =
  MP.many (tokenParser <* skipIgnored)

skipIgnored :: LexerParser ()
skipIgnored =
  MP.skipMany (void (MP.satisfy isSpace) <|> lineComment)

lineComment :: LexerParser ()
lineComment = do
  void (char '#')
  void (MP.takeWhileP (Just "comment") (/= '\n'))

tokenParser :: LexerParser Token
tokenParser = do
  position <- MP.getSourcePos
  let spanValue = sourcePosSpan position
  charToken spanValue
    <|> textToken spanValue
    <|> intToken spanValue
    <|> identifierToken spanValue
    <|> symbolToken spanValue

charToken :: SourceSpan -> LexerParser Token
charToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '\'' "character" spanValue)
  case values of
    [value] ->
      pure
        Token
          { tokenKind = TChar value,
            tokenLexeme = raw,
            tokenSpan = spanValue
          }
    _ -> literalFailure spanValue "character literal must contain exactly one Unicode scalar"

textToken :: SourceSpan -> LexerParser Token
textToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '"' "text" spanValue)
  pure
    Token
      { tokenKind = TText (Text.pack values),
        tokenLexeme = raw,
        tokenSpan = spanValue
      }

quotedScalars :: Char -> Text -> SourceSpan -> LexerParser [Char]
quotedScalars delimiter label spanValue = do
  void (char delimiter)
  go []
  where
    go reversedValues = do
      atEnd <- MP.atEnd
      if atEnd
        then literalFailure spanValue ("unterminated " <> label <> " literal")
        else do
          next <- MP.lookAhead MP.anySingle
          if next == delimiter
            then void (char delimiter) *> pure (reverse reversedValues)
            else do
              value <- quotedScalar delimiter label spanValue
              go (value : reversedValues)

quotedScalar :: Char -> Text -> SourceSpan -> LexerParser Char
quotedScalar delimiter label spanValue =
  escapedScalar label spanValue
    <|> MP.satisfy
      ( \value ->
          value /= delimiter
            && value /= '\\'
            && value /= '\n'
            && value /= '\r'
            && unicodeScalar value
      )
    <|> do
      value <- MP.lookAhead MP.anySingle
      if value == '\n' || value == '\r'
        then literalFailure spanValue ("raw newline is not allowed in a " <> label <> " literal")
        else literalFailure spanValue ("invalid " <> label <> " literal character")

escapedScalar :: Text -> SourceSpan -> LexerParser Char
escapedScalar label spanValue = do
  void (char '\\')
  maybeEscape <- MP.optional MP.anySingle
  case maybeEscape of
    Nothing -> literalFailure spanValue ("unterminated " <> label <> " literal")
    Just escape ->
      case escape of
        '\\' -> pure '\\'
        '\'' -> pure '\''
        '"' -> pure '"'
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        '0' -> pure '\0'
        'u' -> unicodeScalarEscape spanValue
        _ -> literalFailure spanValue ("invalid escape '\\" <> Text.singleton escape <> "'")

unicodeScalarEscape :: SourceSpan -> LexerParser Char
unicodeScalarEscape spanValue = do
  maybeOpen <- MP.optional (char '{')
  case maybeOpen of
    Nothing -> literalFailure spanValue "unterminated Unicode escape"
    Just _ -> do
      digits <- MP.takeWhileP (Just "Unicode scalar body") (/= '}')
      maybeClose <- MP.optional (char '}')
      if maybeClose == Nothing
        then literalFailure spanValue "unterminated Unicode escape"
        else
          if Text.length digits < 1 || Text.length digits > 6 || not (Text.all isHexDigit digits)
            then literalFailure spanValue "Unicode escape must contain 1-6 hexadecimal digits"
            else
              case TextRead.hexadecimal digits :: Either String (Integer, Text) of
                Right (value, trailing)
                  | Text.null trailing,
                    value <= 0x10FFFF,
                    not (value >= 0xD800 && value <= 0xDFFF) -> pure (chr (fromInteger value))
                _ -> literalFailure spanValue "Unicode escape is not a scalar value"

unicodeScalar :: Char -> Bool
unicodeScalar value =
  let scalar = ord value
   in scalar < 0xD800 || scalar > 0xDFFF

literalFailure :: SourceSpan -> Text -> LexerParser a
literalFailure spanValue message =
  MP.customFailure (LexerError (message <> " at " <> renderSpanValue spanValue))

intToken :: SourceSpan -> LexerParser Token
intToken spanValue = do
  digits <- MP.takeWhile1P (Just "integer literal") isDigit
  value <- parseIntegerLiteral spanValue digits
  pure
    Token
      { tokenKind = TInt value,
        tokenLexeme = digits,
        tokenSpan = spanValue
      }

identifierToken :: SourceSpan -> LexerParser Token
identifierToken spanValue = do
  firstChar <- MP.satisfy isIdentifierStart
  rest <- MP.takeWhileP (Just "identifier character") isIdentifierContinuation
  let ident = Text.cons firstChar rest
  pure
    Token
      { tokenKind = identifierKind ident,
        tokenLexeme = ident,
        tokenSpan = spanValue
      }

symbolToken :: SourceSpan -> LexerParser Token
symbolToken spanValue = do
  nextChar <- MP.lookAhead MP.anySingle
  case nextChar of
    ':' ->
      fixedToken TColonColon "::" spanValue <|> fixedToken TColon ":" spanValue
    '@' -> fixedToken TAt "@" spanValue
    '=' ->
      operatorToken "==" spanValue
        <|> operatorToken "=>" spanValue
        <|> fixedToken TEquals "=" spanValue
    '!' ->
      operatorToken "!=" spanValue <|> operatorRunToken spanValue
    '<' ->
      operatorToken "<=" spanValue <|> operatorRunToken spanValue
    '>' ->
      operatorToken ">=" spanValue <|> operatorRunToken spanValue
    '+' -> operatorRunToken spanValue
    '-' -> operatorOrArrowRunToken spanValue
    '*' -> operatorRunToken spanValue
    '/' -> operatorRunToken spanValue
    '|' -> operatorRunToken spanValue
    '%' -> operatorRunToken spanValue
    '&' -> operatorRunToken spanValue
    '?' -> operatorRunToken spanValue
    '^' -> operatorRunToken spanValue
    '~' -> operatorRunToken spanValue
    '$' -> operatorToken "$" spanValue
    '\\' -> fixedToken TLambda "\\" spanValue
    '.' -> fixedToken TDot "." spanValue
    '{' -> fixedToken TLBrace "{" spanValue
    '}' -> fixedToken TRBrace "}" spanValue
    '(' -> fixedToken TLParen "(" spanValue
    ')' -> fixedToken TRParen ")" spanValue
    '[' -> fixedToken TLBracket "[" spanValue
    ']' -> fixedToken TRBracket "]" spanValue
    ',' -> fixedToken TComma "," spanValue
    _ -> MP.anySingle *> unexpectedCharacter spanValue nextChar

fixedToken :: TokenKind -> Text -> SourceSpan -> LexerParser Token
fixedToken kind lexeme spanValue = do
  void (MP.chunk lexeme)
  pure
    Token
      { tokenKind = kind,
        tokenLexeme = lexeme,
        tokenSpan = spanValue
      }

operatorToken :: Text -> SourceSpan -> LexerParser Token
operatorToken symbol spanValue = do
  void (MP.chunk symbol)
  pure
    Token
      { tokenKind = TOperator symbol,
        tokenLexeme = symbol,
        tokenSpan = spanValue
      }

operatorRunToken :: SourceSpan -> LexerParser Token
operatorRunToken spanValue = do
  symbol <- MP.takeWhile1P (Just "operator") isStage2OperatorSymbolChar
  pure
    Token
      { tokenKind = TOperator symbol,
        tokenLexeme = symbol,
        tokenSpan = spanValue
      }

operatorOrArrowRunToken :: SourceSpan -> LexerParser Token
operatorOrArrowRunToken spanValue = do
  symbol <- MP.takeWhile1P (Just "operator") isStage2OperatorSymbolChar
  pure
    Token
      { tokenKind =
          case symbol of
            "->" -> TArrow
            _ -> TOperator symbol,
        tokenLexeme = symbol,
        tokenSpan = spanValue
      }

unexpectedCharacter :: SourceSpan -> Char -> LexerParser a
unexpectedCharacter spanValue charValue =
  MP.customFailure
    ( LexerError
        ( "unexpected character '"
            <> Text.singleton charValue
            <> "' at "
            <> renderSpanValue spanValue
        )
    )

identifierKind :: Text -> TokenKind
identifierKind ident =
  case ident of
    "module" -> TModule
    "import" -> TImport
    "as" -> TAs
    "data" -> TData
    "if" -> TIf
    "else" -> TElse
    "case" -> TCase
    _ -> TIdentifier ident

isIdentifierStart :: Char -> Bool
isIdentifierStart charValue = isAlpha charValue || charValue == '_'

isIdentifierContinuation :: Char -> Bool
isIdentifierContinuation charValue =
  isAlphaNum charValue || charValue == '_' || charValue == '\'' || charValue == '!'

sourcePosSpan :: MP.SourcePos -> SourceSpan
sourcePosSpan sourcePosition =
  SourceSpan
    (unPos (MP.sourceLine sourcePosition))
    (unPos (MP.sourceColumn sourcePosition))

lexerErrorMessage :: MP.ParseErrorBundle Text LexerError -> Text
lexerErrorMessage bundle =
  case firstCustomLexerError bundle of
    Just message -> message
    Nothing -> Text.pack (errorBundlePretty bundle)

firstCustomLexerError :: MP.ParseErrorBundle Text LexerError -> Maybe Text
firstCustomLexerError bundle =
  firstJust (map customErrorMessage (NonEmpty.toList (MP.bundleErrors bundle)))
  where
    customErrorMessage parseError =
      case parseError of
        FancyError _ fancyErrors ->
          firstJust
            [ Just message
              | ErrorCustom (LexerError message) <- Set.toList fancyErrors
            ]
        TrivialError {} -> Nothing

    firstJust values =
      case values of
        [] -> Nothing
        Just value : _ -> Just value
        Nothing : rest -> firstJust rest

-- | Render a compact source position for lexer diagnostics before full span
-- rendering is available at this phase.
renderSpanValue :: SourceSpan -> Text
renderSpanValue = renderSourceSpan

parseIntegerLiteral :: SourceSpan -> Text -> LexerParser Integer
parseIntegerLiteral spanValue digits =
  case TextRead.decimal digits :: Either String (Integer, Text) of
    Right (value, trailing)
      | Text.null trailing -> pure value
      | otherwise -> invalidIntegerLiteral digits spanValue
    Left _ -> invalidIntegerLiteral digits spanValue

invalidIntegerLiteral :: Text -> SourceSpan -> LexerParser a
invalidIntegerLiteral digits spanValue =
  MP.customFailure
    ( LexerError
        ( "invalid integer literal '"
            <> digits
            <> "' at "
            <> renderSpanValue spanValue
        )
    )

-- | Parser/lexer diagnostics currently share the `E0001` parse-error code.
parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
