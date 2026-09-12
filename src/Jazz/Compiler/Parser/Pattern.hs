{-# LANGUAGE OverloadedStrings #-}

-- | Pattern grammar over lexer tokens.
module Jazz.Compiler.Parser.Pattern
  ( parseCaseArmPatternParser,
    parseCaseArmPatternTokenStream,
    parseCaseArmPatternTokens,
    parseCasePatternParser,
    parseCasePatternTokenStream,
    parseLambdaParameterParser,
    parseLambdaParameterTokens,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan, spanColumn, spanLine)
import Jazz.Compiler.Name (mkIdentifier)
import Jazz.Compiler.Parser.AST
  ( Literal (..),
    SurfaceLambdaParameter (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
  )
import Jazz.Compiler.Parser.DeclarationTokens (isConstructorIdentifierText, isReservedLiteralName)
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailureReason (..),
    ParserPatternFailure (..),
    ParserUnsupportedFeature (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseToken,
    peekToken,
    runTokenParserPrefix,
    runTokenStreamParserPrefix,
    withConsumedSpan,
  )
import Jazz.Compiler.Parser.TokenStream (TokenStream)
import qualified Text.Megaparsec as MP

parseCaseArmPatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCaseArmPatternTokens =
  runTokenParserPrefix "case arm pattern" parseCaseArmPatternParser

parseCaseArmPatternTokenStream :: TokenStream -> Either Diagnostic (SurfacePattern, TokenStream)
parseCaseArmPatternTokenStream =
  runTokenStreamParserPrefix "case arm pattern" parseCaseArmPatternParser

parseCasePatternTokenStream :: TokenStream -> Either Diagnostic (SurfacePattern, TokenStream)
parseCasePatternTokenStream =
  runTokenStreamParserPrefix "case pattern" parseCasePatternParser

parseLambdaParameterTokens :: [Token] -> Either Diagnostic (SurfaceLambdaParameter, [Token])
parseLambdaParameterTokens =
  runTokenParserPrefix "lambda parameter" parseLambdaParameterParser

parseCaseArmPatternParser :: Parser SurfacePattern
parseCaseArmPatternParser = withConsumedSpan locatePatternRange $ do
  maybeStartToken <- peekToken
  firstPattern <- parseCasePatternParser
  collectCasePatternAlternatives
    (maybe (surfacePatternSpan firstPattern) tokenSpan maybeStartToken)
    firstPattern

collectCasePatternAlternatives :: SourceSpan -> SurfacePattern -> Parser SurfacePattern
collectCasePatternAlternatives patternSpan firstPattern = do
  remaining <- MP.many (parseToken (TOperator "|") *> parseCasePatternParser)
  pure $ case remaining of
    [] -> firstPattern
    _ -> SurfacePattern patternSpan (SPOr (firstPattern : remaining))

-- A constructor at the pattern head consumes arguments; a constructor used as
-- an unparenthesized argument is nullary. As-pattern tails retain this context.
data PatternPosition = PatternHead | ConstructorArgument

parseCasePatternParser :: Parser SurfacePattern
parseCasePatternParser = parsePattern PatternHead

parsePattern :: PatternPosition -> Parser SurfacePattern
parsePattern position = withConsumedSpan locatePatternRange $ do
  maybeToken <- peekToken
  case maybeToken of
    Just token@Token {tokenKind = TInt value} -> do
      void parseAnyToken
      parseIntegralPatternLiteral token value
    Just token@Token {tokenKind = TChar value} -> do
      void parseAnyToken
      pure (locatedPattern token (SPLiteral (LChar value)))
    Just token@Token {tokenKind = TText value} -> do
      void parseAnyToken
      pure (locatedPattern token (SPLiteral (LText value)))
    Just token@Token {tokenKind = TLBracket} -> do
      void parseAnyToken
      parseListPattern token
    Just token@Token {tokenKind = TLParen} -> do
      void parseAnyToken
      parseTuplePattern token
    Just token@Token {tokenKind = TIdentifier name} -> do
      void parseAnyToken
      parseIdentifierPattern position token name
    Nothing ->
      failTokenParser (ExpectedSyntax description ParserEndOfInput)
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax description (ParserFoundToken (tokenKind token) (tokenLexeme token)))
  where
    description = case position of
      PatternHead -> "case pattern"
      ConstructorArgument -> "constructor pattern argument"

parseIdentifierPattern :: PatternPosition -> Token -> Text -> Parser SurfacePattern
parseIdentifierPattern position identifierToken name =
  case name of
    "_" -> pure (locatedPattern identifierToken SPWildcard)
    "True" -> pure (locatedPattern identifierToken (SPLiteral (LBool True)))
    "False" -> pure (locatedPattern identifierToken (SPLiteral (LBool False)))
    _
      | isConstructorIdentifierText name ->
          case position of
            PatternHead -> parseConstructorPattern identifierToken name
            ConstructorArgument -> pure (locatedPattern identifierToken (SPConstructor (mkIdentifier name) []))
      | otherwise ->
          parseAsPatternOrVariable identifierToken (parsePattern position) name

parseTuplePattern :: Token -> Parser SurfacePattern
parseTuplePattern leftParenToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRParen} -> do
      void parseAnyToken
      pure (locatedPattern leftParenToken (SPTuple []))
    _ -> do
      firstPattern <- parseCasePatternParser
      maybeComma <- peekToken
      case maybeComma of
        Just Token {tokenKind = TComma} -> do
          void parseAnyToken
          tuplePatterns <- (firstPattern :) <$> (parseCasePatternParser `MP.sepBy1` parseToken TComma)
          void (parseToken TRParen)
          pure (locatedPattern leftParenToken (SPTuple tuplePatterns))
        Just Token {tokenKind = TRParen} -> do
          void parseAnyToken
          pure firstPattern
        Nothing ->
          failTokenParserAt
            (tokenSpan leftParenToken)
            (ExpectedSyntax "',' or ')'" (ParserEndOfInputIn "grouped or tuple pattern"))
        Just token ->
          failTokenParserAt
            (tokenSpan token)
            (ExpectedSyntax "',' or ')'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseConstructorPattern :: Token -> Text -> Parser SurfacePattern
parseConstructorPattern constructorToken constructorName =
  locatedPattern constructorToken . SPConstructor (mkIdentifier constructorName)
    <$> MP.many
      (MP.lookAhead (MP.satisfy startsCasePattern) *> parsePattern ConstructorArgument)

parseIntegralPatternLiteral :: Token -> Integer -> Parser SurfacePattern
parseIntegralPatternLiteral wholeToken wholeValue = do
  maybeDot <- peekToken
  case maybeDot of
    Just dotToken@Token {tokenKind = TDot}
      | isImmediatelyAfter wholeToken dotToken -> do
          maybeFractionalToken <- MP.lookAhead (parseAnyToken *> peekToken)
          case maybeFractionalToken of
            Just fractionalToken@Token {tokenKind = TInt _}
              | isImmediatelyAfter dotToken fractionalToken ->
                  failTokenParserAt
                    (tokenSpan wholeToken)
                    (UnsupportedSyntax FractionalLiteralPattern)
            _ -> pure (locatedPattern wholeToken (SPLiteral (LInt wholeValue)))
    _ -> pure (locatedPattern wholeToken (SPLiteral (LInt wholeValue)))

parseAsPatternOrVariable ::
  Token ->
  Parser SurfacePattern ->
  Text ->
  Parser SurfacePattern
parseAsPatternOrVariable identifierToken parseAsTail name = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TAt} -> do
      void parseAnyToken
      SurfacePattern (tokenSpan identifierToken) . SPAs (mkIdentifier name) <$> parseAsTail
    _ ->
      pure (locatedPattern identifierToken (SPVariable (mkIdentifier name)))

startsCasePattern :: Token -> Bool
startsCasePattern token =
  case tokenKind token of
    TInt _ -> True
    TChar _ -> True
    TText _ -> True
    TIdentifier _ -> True
    TLBracket -> True
    TLParen -> True
    _ -> False

parseListPattern :: Token -> Parser SurfacePattern
parseListPattern leftBracketToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBracket} -> do
      void parseAnyToken
      pure (locatedPattern leftBracketToken (SPList []))
    _ -> do
      patterns <- parseCasePatternParser `MP.sepBy1` parseToken TComma
      afterPatterns <- peekToken
      case afterPatterns of
        Just Token {tokenKind = TOperator "|"} -> do
          void parseAnyToken
          tailPattern <- parseCasePatternParser
          void (parseToken TRBracket)
          case patterns of
            [headPattern] ->
              pure (locatedPattern leftBracketToken (SPConsList headPattern tailPattern))
            _ ->
              failTokenParser (PatternFailure ConsLikeListPatternHeadCount)
        Just Token {tokenKind = TRBracket} -> do
          void parseAnyToken
          pure (locatedPattern leftBracketToken (SPList patterns))
        Nothing ->
          failTokenParser (ExpectedSyntax "']'" (ParserEndOfInputIn "list pattern"))
        Just token ->
          failTokenParserAt
            (tokenSpan token)
            (ExpectedSyntax "',' or ']'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseLambdaParameterParser :: Parser SurfaceLambdaParameter
parseLambdaParameterParser = withConsumedSpan locateLambdaParameter $ do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TInt _} ->
      parsePatternLambdaParameter
    Just Token {tokenKind = TChar _} ->
      parsePatternLambdaParameter
    Just Token {tokenKind = TText _} ->
      parsePatternLambdaParameter
    Just Token {tokenKind = TLParen} ->
      parsePatternLambdaParameter
    Just Token {tokenKind = TLBracket} ->
      parsePatternLambdaParameter
    Just token@Token {tokenKind = TIdentifier parameterName}
      | parameterName == "_"
          || isReservedLiteralName parameterName
          || isConstructorIdentifierText parameterName ->
          parsePatternLambdaParameter
      | otherwise -> do
          void parseAnyToken
          maybeTail <- peekToken
          case maybeTail of
            Just Token {tokenKind = TAt} ->
              SurfaceLambdaPattern
                <$> (parseIdentifierPattern PatternHead token parameterName >>= collectCasePatternAlternatives (tokenSpan token))
            Just Token {tokenKind = TOperator "|"} ->
              SurfaceLambdaPattern
                <$> (parseIdentifierPattern PatternHead token parameterName >>= collectCasePatternAlternatives (tokenSpan token))
            _ ->
              pure (SurfaceLambdaIdentifier (tokenSpan token) (mkIdentifier parameterName))
    Nothing ->
      failTokenParser (ExpectedSyntax "identifier" (ParserEndOfInputIn "lambda parameter list"))
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "identifier" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parsePatternLambdaParameter :: Parser SurfaceLambdaParameter
parsePatternLambdaParameter =
  SurfaceLambdaPattern <$> parseCaseArmPatternParser

locatedPattern :: Token -> SurfacePatternForm -> SurfacePattern
locatedPattern token = SurfacePattern (tokenSpan token)

-- Grouping parentheses have no pattern node of their own.
locatePatternRange :: SourceSpan -> SurfacePattern -> SurfacePattern
locatePatternRange spanValue patternValue
  | (spanLine spanValue, spanColumn spanValue)
      == (spanLine original, spanColumn original) =
      patternValue {surfacePatternSpan = spanValue}
  | otherwise = patternValue
  where
    original = surfacePatternSpan patternValue

locateLambdaParameter :: SourceSpan -> SurfaceLambdaParameter -> SurfaceLambdaParameter
locateLambdaParameter spanValue parameter = case parameter of
  SurfaceLambdaPattern patternValue -> SurfaceLambdaPattern (locatePatternRange spanValue patternValue)
  SurfaceLambdaIdentifier _ name -> SurfaceLambdaIdentifier spanValue name
