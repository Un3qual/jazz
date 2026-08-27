{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bootstrap lexer for the current surface syntax. It keeps the token set
-- intentionally small while preserving spans for diagnostics.
module Jazz.Compiler.Parser.Lexer
  ( LexicalFailure (..),
    LexicalFailureReason (..),
    LexicalLiteralKind (..),
    Token (..),
    TokenKind (..),
    isImmediatelyAfter,
    tokenize,
    tokenizeDetailed,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Char (chr, isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace, ord)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import GHC.Generics (Generic)
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan (..),
    mkErrorDiagnostic,
    setDiagnosticPrimarySpan,
  )
import Jazz.Compiler.Parser.Operator
  ( isStage2OperatorSymbolChar,
  )
import Text.Megaparsec
  ( Parsec,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( char,
  )
import Text.Megaparsec.Error
  ( ErrorFancy (..),
    ParseError (..),
    ShowErrorComponent (..),
  )
import Text.Megaparsec.Pos
  ( unPos,
  )

-- | Token vocabulary understood by the current parser foundation.
data TokenKind
  = TIdentifier Text
  | TModule
  | TImport
  | TAs
  | TData
  | TValue
  | TIf
  | TThen
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
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | Concrete token annotated with the original lexeme and its starting source
-- span so later parser errors can report precise locations.
data Token = Token
  { tokenKind :: TokenKind,
    tokenLexeme :: Text,
    tokenSpan :: SourceSpan
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data LexicalLiteralKind
  = CharacterLiteral
  | TextLiteral
  deriving (Eq, Ord, Show)

data LexicalFailureReason
  = UnexpectedCharacter Char
  | UnexpectedEndOfInput
  | InvalidCharacterLength Int
  | UnterminatedLiteral LexicalLiteralKind
  | RawNewline LexicalLiteralKind
  | InvalidEscape Char
  | UnterminatedUnicodeEscape
  | MalformedUnicodeEscape Text
  | NonScalarUnicodeEscape Text
  | InvalidLiteralCharacter LexicalLiteralKind Char
  | InvalidIntegerLiteral Text
  deriving (Eq, Ord, Show)

data LexicalFailure = LexicalFailure
  { lexicalFailureReason :: LexicalFailureReason,
    lexicalFailureSpan :: SourceSpan
  }
  deriving (Eq, Ord, Show)

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  spanLine (tokenSpan leftToken) == spanLine (tokenSpan rightToken)
    && spanColumn (tokenSpan rightToken)
      == spanColumn (tokenSpan leftToken) + Text.length (tokenLexeme leftToken)

newtype LexerError = LexerError LexicalFailure
  deriving (Eq, Ord, Show)

instance ShowErrorComponent LexerError where
  showErrorComponent (LexerError failure) = Text.unpack (renderLexicalFailure failure)

type LexerParser = Parsec LexerError Text

-- Tokenizes the current parser foundation grammar while preserving 1-based
-- line/column spans for diagnostics.
tokenize :: Text -> Either Diagnostic [Token]
tokenize source =
  case tokenizeDetailed source of
    Right tokens -> Right tokens
    Left failure -> Left (lexicalFailureDiagnostic failure)

tokenizeDetailed :: Text -> Either LexicalFailure [Token]
tokenizeDetailed source =
  {-# SCC "jazz-stage:lexing" #-}
  case MP.runParser (skipIgnored *> lexerTokens <* MP.eof) "jazz source" source of
    Right tokens -> Right tokens
    Left bundle -> Left (lexerFailureFromBundle source bundle)

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
  nextChar <- MP.lookAhead MP.anySingle
  let spanValue = sourcePosSpan position
  case nextChar of
    '\'' -> charToken spanValue
    '"' -> textToken spanValue
    _
      | isDigit nextChar -> intToken spanValue
      | isIdentifierStart nextChar -> identifierToken spanValue
      | otherwise -> symbolToken spanValue nextChar

charToken :: SourceSpan -> LexerParser Token
charToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '\'' CharacterLiteral spanValue)
  case values of
    [value] ->
      pure
        Token
          { tokenKind = TChar value,
            tokenLexeme = raw,
            tokenSpan = spanValue
          }
    _ -> literalFailure spanValue (InvalidCharacterLength (length values))

textToken :: SourceSpan -> LexerParser Token
textToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '"' TextLiteral spanValue)
  pure
    Token
      { tokenKind = TText (Text.pack values),
        tokenLexeme = raw,
        tokenSpan = spanValue
      }

quotedScalars :: Char -> LexicalLiteralKind -> SourceSpan -> LexerParser [Char]
quotedScalars delimiter literalKind spanValue = do
  void (char delimiter)
  go []
  where
    go reversedValues = do
      atEnd <- MP.atEnd
      if atEnd
        then literalFailure spanValue (UnterminatedLiteral literalKind)
        else do
          next <- MP.lookAhead MP.anySingle
          if next == delimiter
            then void (char delimiter) *> pure (reverse reversedValues)
            else do
              value <- quotedScalar delimiter literalKind spanValue
              go (value : reversedValues)

quotedScalar :: Char -> LexicalLiteralKind -> SourceSpan -> LexerParser Char
quotedScalar delimiter literalKind spanValue =
  escapedScalar literalKind spanValue
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
        then literalFailure spanValue (RawNewline literalKind)
        else literalFailure spanValue (InvalidLiteralCharacter literalKind value)

escapedScalar :: LexicalLiteralKind -> SourceSpan -> LexerParser Char
escapedScalar literalKind spanValue = do
  void (char '\\')
  maybeEscape <- MP.optional MP.anySingle
  case maybeEscape of
    Nothing -> literalFailure spanValue (UnterminatedLiteral literalKind)
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
        _ -> literalFailure spanValue (InvalidEscape escape)

unicodeScalarEscape :: SourceSpan -> LexerParser Char
unicodeScalarEscape spanValue = do
  maybeOpen <- MP.optional (char '{')
  case maybeOpen of
    Nothing -> literalFailure spanValue UnterminatedUnicodeEscape
    Just _ -> do
      digits <- MP.takeWhileP (Just "Unicode scalar body") (/= '}')
      maybeClose <- MP.optional (char '}')
      if maybeClose == Nothing
        then literalFailure spanValue UnterminatedUnicodeEscape
        else
          if Text.length digits < 1 || Text.length digits > 6 || not (Text.all isHexDigit digits)
            then literalFailure spanValue (MalformedUnicodeEscape digits)
            else case TextRead.hexadecimal digits :: Either String (Integer, Text) of
              Right (value, trailing)
                | Text.null trailing,
                  value <= 0x10FFFF,
                  not (value >= 0xD800 && value <= 0xDFFF) ->
                    pure (chr (fromInteger value))
              _ -> literalFailure spanValue (NonScalarUnicodeEscape digits)

unicodeScalar :: Char -> Bool
unicodeScalar value =
  let scalar = ord value
   in scalar < 0xD800 || scalar > 0xDFFF

literalFailure :: SourceSpan -> LexicalFailureReason -> LexerParser a
literalFailure spanValue reason =
  MP.customFailure (LexerError (LexicalFailure reason spanValue))

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

symbolToken :: SourceSpan -> Char -> LexerParser Token
symbolToken spanValue nextChar =
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
  literalFailure spanValue (UnexpectedCharacter charValue)

identifierKind :: Text -> TokenKind
identifierKind ident =
  case ident of
    "module" -> TModule
    "import" -> TImport
    "as" -> TAs
    "data" -> TData
    "value" -> TValue
    "if" -> TIf
    "then" -> TThen
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

lexerFailureFromBundle :: Text -> MP.ParseErrorBundle Text LexerError -> LexicalFailure
lexerFailureFromBundle source bundle =
  case firstCustomLexerFailure bundle of
    Just failure -> failure
    Nothing -> fallbackLexerFailure source bundle

firstCustomLexerFailure :: MP.ParseErrorBundle Text LexerError -> Maybe LexicalFailure
firstCustomLexerFailure bundle =
  firstJust (map customErrorMessage (NonEmpty.toList (MP.bundleErrors bundle)))
  where
    customErrorMessage parseError =
      case parseError of
        FancyError _ fancyErrors ->
          firstJust
            [ Just failure
            | ErrorCustom (LexerError failure) <- Set.toList fancyErrors
            ]
        TrivialError {} -> Nothing

    firstJust values =
      case values of
        [] -> Nothing
        Just value : _ -> Just value
        Nothing : rest -> firstJust rest

fallbackLexerFailure :: Text -> MP.ParseErrorBundle Text LexerError -> LexicalFailure
fallbackLexerFailure source bundle =
  let offset = MP.errorOffset (NonEmpty.head (MP.bundleErrors bundle))
      spanValue = sourceSpanAtOffset offset source
   in case Text.uncons (Text.drop offset source) of
        Just (value, _) -> LexicalFailure (UnexpectedCharacter value) spanValue
        Nothing -> LexicalFailure UnexpectedEndOfInput spanValue

sourceSpanAtOffset :: Int -> Text -> SourceSpan
sourceSpanAtOffset offset source =
  let (lineNumber, columnNumber) =
        Text.foldl' advance (1, 1) (Text.take offset source)
   in SourceSpan lineNumber columnNumber
  where
    advance (lineNumber, columnNumber) value =
      case value of
        '\n' -> (lineNumber + 1, 1)
        '\t' ->
          let nextColumn = columnNumber + (8 - ((columnNumber - 1) `mod` 8))
           in (lineNumber, nextColumn)
        _ -> (lineNumber, columnNumber + 1)

parseIntegerLiteral :: SourceSpan -> Text -> LexerParser Integer
parseIntegerLiteral spanValue digits =
  case TextRead.decimal digits :: Either String (Integer, Text) of
    Right (value, trailing)
      | Text.null trailing -> pure value
      | otherwise -> invalidIntegerLiteral digits spanValue
    Left _ -> invalidIntegerLiteral digits spanValue

invalidIntegerLiteral :: Text -> SourceSpan -> LexerParser a
invalidIntegerLiteral digits spanValue =
  literalFailure spanValue (InvalidIntegerLiteral digits)

lexicalFailureDiagnostic :: LexicalFailure -> Diagnostic
lexicalFailureDiagnostic failure =
  setDiagnosticPrimarySpan
    (lexicalFailureSpan failure)
    (mkErrorDiagnostic E0001 CompilationOrigin (renderLexicalFailure failure))

renderLexicalFailure :: LexicalFailure -> Text
renderLexicalFailure failure =
  renderLexicalFailureReason (lexicalFailureReason failure)

renderLexicalFailureReason :: LexicalFailureReason -> Text
renderLexicalFailureReason reason =
  case reason of
    UnexpectedCharacter value -> "unexpected character '" <> Text.singleton value <> "'"
    UnexpectedEndOfInput -> "unexpected end of input"
    InvalidCharacterLength _ -> "character literal must contain exactly one Unicode scalar"
    UnterminatedLiteral literalKind -> "unterminated " <> literalKindLabel literalKind <> " literal"
    RawNewline literalKind -> "raw newline is not allowed in a " <> literalKindLabel literalKind <> " literal"
    InvalidEscape value -> "invalid escape '\\" <> Text.singleton value <> "'"
    UnterminatedUnicodeEscape -> "unterminated Unicode escape"
    MalformedUnicodeEscape _ -> "Unicode escape must contain 1-6 hexadecimal digits"
    NonScalarUnicodeEscape _ -> "Unicode escape is not a scalar value"
    InvalidLiteralCharacter literalKind _ -> "invalid " <> literalKindLabel literalKind <> " literal character"
    InvalidIntegerLiteral digits -> "invalid integer literal '" <> digits <> "'"

literalKindLabel :: LexicalLiteralKind -> Text
literalKindLabel literalKind =
  case literalKind of
    CharacterLiteral -> "character"
    TextLiteral -> "text"
