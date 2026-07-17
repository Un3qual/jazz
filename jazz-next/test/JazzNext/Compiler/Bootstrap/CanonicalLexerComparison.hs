{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalKeyword (..),
    CanonicalLexError (..),
    CanonicalLexErrorReason (..),
    CanonicalLexResult (..),
    CanonicalLiteralKind (..),
    CanonicalPunctuation (..),
    CanonicalSourcePath (..),
    CanonicalSpan (..),
    CanonicalToken (..),
    CanonicalTokenKind (..),
    canonicalLexResultRuntimeValue,
    canonicalLexErrorRuntimeValue,
    canonicalTokenRuntimeValue,
    canonicalTokenKindRuntimeValue,
    canonicalizeFailure,
    canonicalizeTokenKind,
    canonicalizeLexResult,
    normalizeCanonicalSourcePath,
    renderCanonicalLexResult,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( CanonicalSourcePath (..),
    CanonicalSpan (..),
    canonicalConstructor,
    canonicalNullaryConstructor,
    canonicalSourcePathRuntimeValue,
    canonicalSpanRuntimeValue,
    canonicalizeSpan,
    normalizeCanonicalSourcePath,
    runtimeIntValue,
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure (..),
    LexicalFailureReason (..),
    LexicalLiteralKind (..),
    Token (..),
    TokenKind (..),
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
  )

data CanonicalKeyword
  = ModuleKeyword
  | ImportKeyword
  | AsKeyword
  | DataKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | CaseKeyword
  deriving (Eq, Show)

data CanonicalPunctuation
  = ArrowPunctuation
  | AtPunctuation
  | EqualsPunctuation
  | ColonPunctuation
  | DoubleColonPunctuation
  | DotPunctuation
  | LeftBracePunctuation
  | RightBracePunctuation
  | LeftParenPunctuation
  | RightParenPunctuation
  | LeftBracketPunctuation
  | RightBracketPunctuation
  | CommaPunctuation
  | LambdaPunctuation
  deriving (Eq, Show)

data CanonicalLiteralKind
  = CanonicalCharacterLiteral
  | CanonicalTextLiteral
  deriving (Eq, Show)

data CanonicalLexErrorReason
  = CanonicalUnexpectedCharacter Char
  | CanonicalUnexpectedEndOfInput
  | CanonicalInvalidCharacterLength Int
  | CanonicalUnterminatedLiteral CanonicalLiteralKind
  | CanonicalRawNewline CanonicalLiteralKind
  | CanonicalInvalidEscape Char
  | CanonicalUnterminatedUnicodeEscape
  | CanonicalMalformedUnicodeEscape Text
  | CanonicalNonScalarUnicodeEscape Text
  | CanonicalInvalidLiteralCharacter CanonicalLiteralKind Char
  | CanonicalInvalidIntegerLiteral Text
  deriving (Eq, Show)

data CanonicalTokenKind
  = IdentifierKind Text
  | KeywordKind CanonicalKeyword
  | IntegerKind Text
  | CharacterKind Char
  | TextKind Text
  | OperatorKind Text
  | PunctuationKind CanonicalPunctuation
  deriving (Eq, Show)

data CanonicalToken = CanonicalToken CanonicalTokenKind Text CanonicalSpan
  deriving (Eq, Show)

data CanonicalLexError = CanonicalLexError Text CanonicalLexErrorReason CanonicalSpan
  deriving (Eq, Show)

data CanonicalLexResult
  = CanonicalLexSuccess CanonicalSourcePath [CanonicalToken]
  | CanonicalLexFailure CanonicalSourcePath CanonicalLexError
  deriving (Eq, Show)

canonicalizeLexResult :: CanonicalSourcePath -> Either LexicalFailure [Token] -> CanonicalLexResult
canonicalizeLexResult sourcePath result =
  case result of
    Right tokens -> CanonicalLexSuccess sourcePath (map canonicalizeToken tokens)
    Left failure -> CanonicalLexFailure sourcePath (canonicalizeFailure failure)

canonicalizeToken :: Token -> CanonicalToken
canonicalizeToken token =
  CanonicalToken
    (canonicalizeTokenKind (tokenKind token))
    (tokenLexeme token)
    (canonicalizeSpan (tokenSpan token))

canonicalizeTokenKind :: TokenKind -> CanonicalTokenKind
canonicalizeTokenKind tokenKindValue =
  case tokenKindValue of
    TIdentifier name -> IdentifierKind name
    TModule -> KeywordKind ModuleKeyword
    TImport -> KeywordKind ImportKeyword
    TAs -> KeywordKind AsKeyword
    TData -> KeywordKind DataKeyword
    TIf -> KeywordKind IfKeyword
    TThen -> KeywordKind ThenKeyword
    TElse -> KeywordKind ElseKeyword
    TCase -> KeywordKind CaseKeyword
    TInt value -> IntegerKind (Text.pack (show value))
    TChar value -> CharacterKind value
    TText value -> TextKind value
    TOperator symbol -> OperatorKind symbol
    TArrow -> PunctuationKind ArrowPunctuation
    TAt -> PunctuationKind AtPunctuation
    TEquals -> PunctuationKind EqualsPunctuation
    TColon -> PunctuationKind ColonPunctuation
    TColonColon -> PunctuationKind DoubleColonPunctuation
    TDot -> PunctuationKind DotPunctuation
    TLBrace -> PunctuationKind LeftBracePunctuation
    TRBrace -> PunctuationKind RightBracePunctuation
    TLParen -> PunctuationKind LeftParenPunctuation
    TRParen -> PunctuationKind RightParenPunctuation
    TLBracket -> PunctuationKind LeftBracketPunctuation
    TRBracket -> PunctuationKind RightBracketPunctuation
    TComma -> PunctuationKind CommaPunctuation
    TLambda -> PunctuationKind LambdaPunctuation

canonicalizeFailure :: LexicalFailure -> CanonicalLexError
canonicalizeFailure failure =
  CanonicalLexError
    "E0001"
    (canonicalizeFailureReason (lexicalFailureReason failure))
    (canonicalizeSpan (lexicalFailureSpan failure))

canonicalizeFailureReason :: LexicalFailureReason -> CanonicalLexErrorReason
canonicalizeFailureReason reason =
  case reason of
    UnexpectedCharacter value -> CanonicalUnexpectedCharacter value
    UnexpectedEndOfInput -> CanonicalUnexpectedEndOfInput
    InvalidCharacterLength count -> CanonicalInvalidCharacterLength count
    UnterminatedLiteral literalKind -> CanonicalUnterminatedLiteral (canonicalizeLiteralKind literalKind)
    RawNewline literalKind -> CanonicalRawNewline (canonicalizeLiteralKind literalKind)
    InvalidEscape value -> CanonicalInvalidEscape value
    UnterminatedUnicodeEscape -> CanonicalUnterminatedUnicodeEscape
    MalformedUnicodeEscape digits -> CanonicalMalformedUnicodeEscape digits
    NonScalarUnicodeEscape digits -> CanonicalNonScalarUnicodeEscape digits
    InvalidLiteralCharacter literalKind value ->
      CanonicalInvalidLiteralCharacter (canonicalizeLiteralKind literalKind) value
    InvalidIntegerLiteral digits -> CanonicalInvalidIntegerLiteral digits

canonicalizeLiteralKind :: LexicalLiteralKind -> CanonicalLiteralKind
canonicalizeLiteralKind literalKind =
  case literalKind of
    CharacterLiteral -> CanonicalCharacterLiteral
    TextLiteral -> CanonicalTextLiteral

renderCanonicalLexResult :: CanonicalLexResult -> Text
renderCanonicalLexResult = renderRuntimeValue . canonicalLexResultRuntimeValue

canonicalLexResultRuntimeValue :: CanonicalLexResult -> RuntimeValue
canonicalLexResultRuntimeValue result =
  case result of
    CanonicalLexSuccess sourcePath tokens ->
      canonicalConstructor
        "CanonicalLexSuccess"
        [canonicalSourcePathRuntimeValue sourcePath, VList (map canonicalTokenRuntimeValue tokens) Nothing]
    CanonicalLexFailure sourcePath failure ->
      canonicalConstructor
        "CanonicalLexFailure"
        [canonicalSourcePathRuntimeValue sourcePath, canonicalLexErrorRuntimeValue failure]

canonicalTokenRuntimeValue :: CanonicalToken -> RuntimeValue
canonicalTokenRuntimeValue (CanonicalToken kind rawLexeme spanValue) =
  canonicalConstructor
    "CanonicalToken"
    [canonicalTokenKindRuntimeValue kind, VText rawLexeme, canonicalSpanRuntimeValue spanValue]

canonicalTokenKindRuntimeValue :: CanonicalTokenKind -> RuntimeValue
canonicalTokenKindRuntimeValue kind =
  case kind of
    IdentifierKind name -> canonicalConstructor "IdentifierKind" [VText name]
    KeywordKind keyword -> canonicalConstructor "KeywordKind" [canonicalKeywordRuntimeValue keyword]
    IntegerKind decimal -> canonicalConstructor "IntegerKind" [VText decimal]
    CharacterKind value -> canonicalConstructor "CharacterKind" [VChar value]
    TextKind value -> canonicalConstructor "TextKind" [VText value]
    OperatorKind symbol -> canonicalConstructor "OperatorKind" [VText symbol]
    PunctuationKind punctuation ->
      canonicalConstructor "PunctuationKind" [canonicalPunctuationRuntimeValue punctuation]

canonicalKeywordRuntimeValue :: CanonicalKeyword -> RuntimeValue
canonicalKeywordRuntimeValue = canonicalNullaryConstructor . Text.pack . show

canonicalPunctuationRuntimeValue :: CanonicalPunctuation -> RuntimeValue
canonicalPunctuationRuntimeValue = canonicalNullaryConstructor . Text.pack . show

canonicalLexErrorRuntimeValue :: CanonicalLexError -> RuntimeValue
canonicalLexErrorRuntimeValue (CanonicalLexError code reason spanValue) =
  canonicalConstructor
    "CanonicalLexError"
    [VText code, canonicalLexErrorReasonRuntimeValue reason, canonicalSpanRuntimeValue spanValue]

canonicalLexErrorReasonRuntimeValue :: CanonicalLexErrorReason -> RuntimeValue
canonicalLexErrorReasonRuntimeValue reason =
  case reason of
    CanonicalUnexpectedCharacter value -> canonicalConstructor "UnexpectedCharacter" [VChar value]
    CanonicalUnexpectedEndOfInput -> canonicalNullaryConstructor "UnexpectedEndOfInput"
    CanonicalInvalidCharacterLength count ->
      canonicalConstructor "InvalidCharacterLength" [runtimeIntValue count]
    CanonicalUnterminatedLiteral literalKind ->
      canonicalConstructor "UnterminatedLiteral" [canonicalLiteralKindRuntimeValue literalKind]
    CanonicalRawNewline literalKind ->
      canonicalConstructor "RawNewline" [canonicalLiteralKindRuntimeValue literalKind]
    CanonicalInvalidEscape value -> canonicalConstructor "InvalidEscape" [VChar value]
    CanonicalUnterminatedUnicodeEscape -> canonicalNullaryConstructor "UnterminatedUnicodeEscape"
    CanonicalMalformedUnicodeEscape digits ->
      canonicalConstructor "MalformedUnicodeEscape" [VText digits]
    CanonicalNonScalarUnicodeEscape digits ->
      canonicalConstructor "NonScalarUnicodeEscape" [VText digits]
    CanonicalInvalidLiteralCharacter literalKind value ->
      canonicalConstructor
        "InvalidLiteralCharacter"
        [canonicalLiteralKindRuntimeValue literalKind, VChar value]
    CanonicalInvalidIntegerLiteral digits ->
      canonicalConstructor "InvalidIntegerLiteral" [VText digits]

canonicalLiteralKindRuntimeValue :: CanonicalLiteralKind -> RuntimeValue
canonicalLiteralKindRuntimeValue literalKind =
  case literalKind of
    CanonicalCharacterLiteral -> canonicalNullaryConstructor "CharacterLiteral"
    CanonicalTextLiteral -> canonicalNullaryConstructor "TextLiteral"
