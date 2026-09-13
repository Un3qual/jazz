{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Shared token boundaries and name predicates for declaration grammars.
module Jazz.Compiler.Parser.DeclarationTokens
  ( collectUntilDotParser,
    looksLikeOperatorDeclaration,
    looksLikeReservedAbstractionDeclaration,
    looksLikeAbstractionDeclaration,
    rejectNestedDeclaration,
    rejectNestedOperatorDeclaration,
    isReservedLiteralName,
    isConstructorIdentifierText,
    isTypeParameterIdentifierText,
  )
where

import Data.Char
  ( isLower,
    isUpper,
  )
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    parserFailure,
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.TokenParser (Parser, failParserFailure, parseAnyToken)
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )
import qualified Text.Megaparsec as MP

collectUntilDotParser :: Parser [Token]
collectUntilDotParser = go 0 []
  where
    go :: Int -> [Token] -> Parser [Token]
    go depth acc = do
      tokens <- MP.getInput
      case tokens of
        EmptyTokens -> failParserFailure (parserFailure (ExpectedSyntax "'.'" ParserEndOfInput))
        token :< _ -> case tokenKind token of
          TDot
            | null acc -> failParserFailure (parserFailureAt (tokenSpan token) (ExpectedSyntax "signature text" (ParserBeforeToken TDot "." Nothing)))
            | depth > 0 -> failParserFailure (parserFailureAt (tokenSpan token) (ExpectedSyntax "closing delimiter" (ParserBeforeToken TDot "." (Just "signature"))))
            | otherwise -> reverse acc <$ parseAnyToken
          _
            | not (null acc) && beginsStatement tokens && not (continuesQualifiedType acc tokens) ->
                failParserFailure (parserFailureAt (tokenSpan token) (ExpectedSyntax "'.'" (ParserBeforeToken (tokenKind token) (tokenLexeme token) Nothing)))
            | otherwise -> parseAnyToken *> go (nextDepth depth (tokenKind token)) (token : acc)

    nextDepth depth kind = case kind of
      TLParen -> depth + 1
      TLBracket -> depth + 1
      TLBrace -> depth + 1
      TRParen -> max 0 (depth - 1)
      TRBracket -> max 0 (depth - 1)
      TRBrace -> max 0 (depth - 1)
      _ -> depth

    continuesQualifiedType (previous : _) (alias@Token {tokenKind = TIdentifier {}} :< separator@Token {tokenKind = TColonColon} :< member@Token {tokenKind = TIdentifier {}} :< _) =
      isImmediatelyAfter alias separator
        && isImmediatelyAfter separator member
        && tokenKind previous `elem` [TArrow, TLParen, TLBracket, TLBrace, TComma, TColon, TColonColon]
    continuesQualifiedType _ _ = False

beginsStatement :: TokenStream -> Bool
beginsStatement tokens =
  case tokens of
    Token {tokenKind = TModule} :< _ -> True
    Token {tokenKind = TImport} :< _ -> True
    Token {tokenKind = TData} :< _ -> True
    Token {tokenKind = TIdentifier "operator"} :< rest
      | looksLikeOperatorDeclaration rest -> True
    Token {tokenKind = TLParen}
      :< Token {tokenKind = TOperator {}}
      :< Token {tokenKind = TRParen}
      :< Token {tokenKind = TColonColon}
      :< _ -> True
    Token {tokenKind = TLParen}
      :< Token {tokenKind = TOperator {}}
      :< Token {tokenKind = TRParen}
      :< Token {tokenKind = TEquals}
      :< _ -> True
    Token {tokenKind = TIdentifier name} :< rest
      | looksLikeReservedAbstractionDeclaration name rest -> True
    Token {tokenKind = TIdentifier _} :< Token {tokenKind = TEquals} :< _ -> True
    Token {tokenKind = TIdentifier _} :< Token {tokenKind = TColonColon} :< _ -> True
    _ -> False

looksLikeOperatorDeclaration :: TokenStream -> Bool
looksLikeOperatorDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TOperator {}} :< _ -> True
    Token {tokenKind = TArrow} :< _ -> True
    Token {tokenKind = TIdentifier {}} :< rest -> hasOperatorFixityKeywordBeforeTerminator rest
    _ -> False

hasOperatorFixityKeywordBeforeTerminator :: TokenStream -> Bool
hasOperatorFixityKeywordBeforeTerminator tokens =
  case tokens of
    EmptyTokens -> False
    Token {tokenKind = TDot} :< _ -> False
    Token {tokenKind = TIdentifier "tier"} :< _ -> True
    Token {tokenKind = TIdentifier "precedence"} :< _ -> True
    _ :< rest -> hasOperatorFixityKeywordBeforeTerminator rest

looksLikeReservedAbstractionDeclaration :: Text -> TokenStream -> Bool
looksLikeReservedAbstractionDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "trait" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

looksLikeAbstractionDeclaration :: TokenStream -> Bool
looksLikeAbstractionDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier {}} :< rest -> hasAbstractionBodyBeforeTerminator rest
    Token {tokenKind = TAt} :< rest -> hasAbstractionBodyBeforeTerminator rest
    _ -> False

hasAbstractionBodyBeforeTerminator :: TokenStream -> Bool
hasAbstractionBodyBeforeTerminator tokens =
  case tokens of
    EmptyTokens -> False
    Token {tokenKind = TDot} :< _ -> False
    Token {tokenKind = TLBrace} :< _ -> True
    _ :< rest -> hasAbstractionBodyBeforeTerminator rest

rejectNestedOperatorDeclaration :: Token -> Either ParserFailure a
rejectNestedOperatorDeclaration = rejectNestedDeclaration OperatorDeclaration

rejectNestedDeclaration :: ParserDeclarationKind -> Token -> Either ParserFailure a
rejectNestedDeclaration kind token =
  Left
    ( parserFailureAt
        (tokenSpan token)
        (DeclarationFailure (DeclarationOutsideAllowedScope kind))
    )

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
    Nothing -> False

isTypeParameterIdentifierText :: Text -> Bool
isTypeParameterIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False
