{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the current surface syntax. It keeps the token set
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
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Foldable (asum)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
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
import Jazz.Compiler.Name
  ( isIdentifierContinuationCharacter,
    isIdentifierStartCharacter,
  )
import Jazz.Compiler.Parser.Operator
  ( isStage2OperatorSymbolChar,
  )
import Jazz.Compiler.SourceSpan (sourceSpanEnd)
import Text.Megaparsec
  ( Parsec,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( char,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
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
  let leftSpan = tokenSpan leftToken
      rightSpan = tokenSpan rightToken
      end = fromMaybe (spanLine leftSpan, spanColumn leftSpan + Text.length (tokenLexeme leftToken)) (sourceSpanEnd leftSpan)
   in end == (spanLine rightSpan, spanColumn rightSpan)

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
  MP.many (L.lexeme skipIgnored tokenParser)

skipIgnored :: LexerParser ()
skipIgnored =
  L.space space1 (L.skipLineComment "#") MP.empty

tokenParser :: LexerParser Token
tokenParser = do
  start <- MP.getSourcePos
  let spanValue = sourcePosSpan start
  (raw, kind) <- MP.match $ do
    nextChar <- MP.lookAhead MP.anySingle
    case nextChar of
      '\'' -> do
        values <- quotedScalars '\'' CharacterLiteral spanValue
        case values of
          [value] -> pure (TChar value)
          _ -> literalFailure spanValue (InvalidCharacterLength (length values))
      '"' -> TText . Text.pack <$> quotedScalars '"' TextLiteral spanValue
      _
        | isDigit nextChar -> TInt <$> L.decimal
        | isIdentifierStartCharacter nextChar -> identifierKind <$> identifier
        | otherwise -> symbolToken spanValue nextChar
  end <- MP.getSourcePos
  pure
    Token
      { tokenKind = kind,
        tokenLexeme = raw,
        tokenSpan = SourceRange (unPos (MP.sourceLine start)) (unPos (MP.sourceColumn start)) (unPos (MP.sourceLine end)) (unPos (MP.sourceColumn end))
      }
  where
    identifier =
      Text.cons
        <$> MP.satisfy isIdentifierStartCharacter
        <*> MP.takeWhileP (Just "identifier character") isIdentifierContinuationCharacter

quotedScalars :: Char -> LexicalLiteralKind -> SourceSpan -> LexerParser [Char]
quotedScalars delimiter literalKind spanValue =
  char delimiter *> MP.manyTill scalar (char delimiter)
  where
    scalar =
      (MP.eof *> literalFailure spanValue (UnterminatedLiteral literalKind))
        <|> quotedScalar delimiter literalKind spanValue

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

symbolToken :: SourceSpan -> Char -> LexerParser TokenKind
symbolToken spanValue nextChar =
  case nextChar of
    ':' ->
      fixedToken TColonColon "::" <|> fixedToken TColon ":"
    '@' -> fixedToken TAt "@"
    '=' ->
      operatorToken "=="
        <|> operatorToken "=>"
        <|> fixedToken TEquals "="
    '!' ->
      operatorToken "!=" <|> operatorRunKind
    '<' ->
      operatorToken "<=" <|> operatorRunKind
    '>' ->
      operatorToken ">=" <|> operatorRunKind
    '$' -> operatorToken "$"
    '\\' -> fixedToken TLambda "\\"
    '.' -> fixedToken TDot "."
    '{' -> fixedToken TLBrace "{"
    '}' -> fixedToken TRBrace "}"
    '(' -> fixedToken TLParen "("
    ')' -> fixedToken TRParen ")"
    '[' -> fixedToken TLBracket "["
    ']' -> fixedToken TRBracket "]"
    ',' -> fixedToken TComma ","
    _
      | isStage2OperatorSymbolChar nextChar -> operatorRunKind
      | otherwise -> MP.anySingle *> unexpectedCharacter spanValue nextChar

fixedToken :: TokenKind -> Text -> LexerParser TokenKind
fixedToken kind lexeme = kind <$ MP.chunk lexeme

operatorToken :: Text -> LexerParser TokenKind
operatorToken symbol = fixedToken (TOperator symbol) symbol

operatorRunKind :: LexerParser TokenKind
operatorRunKind = do
  symbol <- MP.takeWhile1P (Just "operator") isStage2OperatorSymbolChar
  pure $ case symbol of
    "->" -> TArrow
    _ -> TOperator symbol

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
  asum (map customErrorMessage (NonEmpty.toList (MP.bundleErrors bundle)))
  where
    customErrorMessage parseError =
      case parseError of
        FancyError _ fancyErrors ->
          asum
            [ Just failure
            | ErrorCustom (LexerError failure) <- Set.toList fancyErrors
            ]
        TrivialError {} -> Nothing

fallbackLexerFailure :: Text -> MP.ParseErrorBundle Text LexerError -> LexicalFailure
fallbackLexerFailure source bundle =
  let offset = MP.errorOffset (NonEmpty.head (MP.bundleErrors bundle))
      spanValue = sourcePosSpan (MP.pstateSourcePos (MP.reachOffsetNoLine offset (MP.bundlePosState bundle)))
   in case Text.uncons (Text.drop offset source) of
        Just (value, _) -> LexicalFailure (UnexpectedCharacter value) spanValue
        Nothing -> LexicalFailure UnexpectedEndOfInput spanValue

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
