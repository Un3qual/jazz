{-# LANGUAGE OverloadedStrings #-}

-- | Pattern grammar over lexer tokens.
module JazzNext.Compiler.Parser.Pattern
  ( parseCaseArmPatternParser,
    parseCaseArmPatternTokens,
    parseCasePatternParser,
    parseCasePatternTokens,
    parseLambdaParameterParser,
    parseLambdaParameterTokens,
  )
where

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics (Diagnostic)
import JazzNext.Compiler.Name (mkIdentifier)
import JazzNext.Compiler.Parser.AST
  ( SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
  )
import JazzNext.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailureReason (..),
    ParserPatternFailure (..),
    ParserUnsupportedFeature (..),
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseIdentifier,
    parseToken,
    peekToken,
    runTokenParserPrefix,
  )
import qualified Text.Megaparsec as MP

parseCaseArmPatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCaseArmPatternTokens =
  runTokenParserPrefix "case arm pattern" parseCaseArmPatternParser

parseCasePatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCasePatternTokens =
  runTokenParserPrefix "case pattern" parseCasePatternParser

parseLambdaParameterTokens :: [Token] -> Either Diagnostic (SurfaceLambdaParameter, [Token])
parseLambdaParameterTokens =
  runTokenParserPrefix "lambda parameter" parseLambdaParameterParser

parseCaseArmPatternParser :: Parser SurfacePattern
parseCaseArmPatternParser = do
  firstPattern <- parseCasePatternParser
  collectCasePatternAlternatives [firstPattern]

collectCasePatternAlternatives :: [SurfacePattern] -> Parser SurfacePattern
collectCasePatternAlternatives reversedPatterns = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TOperator "|"} -> do
      void parseAnyToken
      nextPattern <- parseCasePatternParser
      collectCasePatternAlternatives (nextPattern : reversedPatterns)
    _ ->
      case reverse reversedPatterns of
        [singlePattern] -> pure singlePattern
        alternatives -> pure (SPOr alternatives)

parseCasePatternParser :: Parser SurfacePattern
parseCasePatternParser = do
  maybeToken <- peekToken
  case maybeToken of
    Just token@Token {tokenKind = TInt value} -> do
      void parseAnyToken
      parseIntegralPatternLiteral token value
    Just Token {tokenKind = TChar value} -> do
      void parseAnyToken
      pure (SPLiteral (SLChar value))
    Just Token {tokenKind = TText value} -> do
      void parseAnyToken
      pure (SPLiteral (SLText value))
    Just Token {tokenKind = TLBracket} -> do
      void parseAnyToken
      parseListPattern
    Just token@Token {tokenKind = TLParen} -> do
      void parseAnyToken
      parseTuplePattern token
    Just Token {tokenKind = TIdentifier name} -> do
      void parseAnyToken
      parseIdentifierCasePattern name
    Nothing ->
      failTokenParser (ExpectedSyntax "case pattern" ParserEndOfInput)
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "case pattern" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseIdentifierCasePattern :: Text -> Parser SurfacePattern
parseIdentifierCasePattern name =
  case name of
    "_" -> pure SPWildcard
    "True" -> pure (SPLiteral (SLBool True))
    "False" -> pure (SPLiteral (SLBool False))
    _
      | isConstructorIdentifierText name ->
          parseConstructorPattern name
      | otherwise ->
          parseAsPatternOrVariable parseCasePatternParser name

parseTuplePattern :: Token -> Parser SurfacePattern
parseTuplePattern leftParenToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRParen} -> do
      void parseAnyToken
      pure (SPTuple [])
    _ -> do
      firstPattern <- parseCasePatternParser
      maybeComma <- peekToken
      case maybeComma of
        Just Token {tokenKind = TComma} -> do
          void parseAnyToken
          tuplePatterns <- parseTuplePatternElements [firstPattern]
          void (parseToken TRParen)
          pure (SPTuple tuplePatterns)
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
            (ExpectedSyntax "')'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseTuplePatternElements :: [SurfacePattern] -> Parser [SurfacePattern]
parseTuplePatternElements reversedPatterns = do
  nextPattern <- parseCasePatternParser
  maybeComma <- peekToken
  case maybeComma of
    Just Token {tokenKind = TComma} -> do
      void parseAnyToken
      parseTuplePatternElements (nextPattern : reversedPatterns)
    _ ->
      pure (reverse (nextPattern : reversedPatterns))

parseConstructorPattern :: Text -> Parser SurfacePattern
parseConstructorPattern constructorName =
  go []
  where
    go reversedArguments = do
      maybeToken <- peekToken
      case maybeToken of
        Just token
          | patternArgumentBoundary token ->
              finish reversedArguments
          | startsCasePattern token -> do
              nextArgument <- parseConstructorArgumentPattern
              go (nextArgument : reversedArguments)
        _ ->
          finish reversedArguments

    finish reversedArguments =
      pure (SPConstructor (mkIdentifier constructorName) (reverse reversedArguments))

parseConstructorArgumentPattern :: Parser SurfacePattern
parseConstructorArgumentPattern = do
  maybeToken <- peekToken
  case maybeToken of
    Just token@Token {tokenKind = TInt value} -> do
      void parseAnyToken
      parseIntegralPatternLiteral token value
    Just Token {tokenKind = TChar value} -> do
      void parseAnyToken
      pure (SPLiteral (SLChar value))
    Just Token {tokenKind = TText value} -> do
      void parseAnyToken
      pure (SPLiteral (SLText value))
    Just Token {tokenKind = TIdentifier name} -> do
      void parseAnyToken
      case name of
        "True" -> pure (SPLiteral (SLBool True))
        "False" -> pure (SPLiteral (SLBool False))
        "_" -> pure SPWildcard
        _
          | isConstructorIdentifierText name ->
              pure (SPConstructor (mkIdentifier name) [])
          | otherwise ->
              parseAsPatternOrVariable parseConstructorArgumentPattern name
    Just Token {tokenKind = TLBracket} -> do
      void parseAnyToken
      parseListPattern
    Just token@Token {tokenKind = TLParen} -> do
      void parseAnyToken
      parseTuplePattern token
    Nothing ->
      failTokenParser (ExpectedSyntax "constructor pattern argument" ParserEndOfInput)
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        ( ExpectedSyntax
            "constructor pattern argument"
            (ParserFoundToken (tokenKind token) (tokenLexeme token))
        )

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
            _ -> pure (SPLiteral (SLInt wholeValue))
    _ -> pure (SPLiteral (SLInt wholeValue))

parseAsPatternOrVariable ::
  Parser SurfacePattern ->
  Text ->
  Parser SurfacePattern
parseAsPatternOrVariable parseAsTail name = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TAt} -> do
      void parseAnyToken
      SPAs (mkIdentifier name) <$> parseAsTail
    _ ->
      pure (SPVariable (mkIdentifier name))

patternArgumentBoundary :: Token -> Bool
patternArgumentBoundary token =
  case tokenKind token of
    TArrow -> True
    TComma -> True
    TRBracket -> True
    TRParen -> True
    TRBrace -> True
    _ -> False

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

parseListPattern :: Parser SurfacePattern
parseListPattern = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBracket} -> do
      void parseAnyToken
      pure (SPList [])
    _ -> do
      firstPattern <- parseCasePatternParser
      collectListPatterns [firstPattern]
  where
    collectListPatterns reversedPatterns = do
      maybeToken <- peekToken
      case maybeToken of
        Just Token {tokenKind = TComma} -> do
          void parseAnyToken
          nextPattern <- parseCasePatternParser
          collectListPatterns (nextPattern : reversedPatterns)
        Just Token {tokenKind = TOperator "|"} -> do
          void parseAnyToken
          tailPattern <- parseCasePatternParser
          void (parseToken TRBracket)
          case reverse reversedPatterns of
            [headPattern] ->
              pure (SPConsList headPattern tailPattern)
            _ ->
              failTokenParser (PatternFailure ConsLikeListPatternHeadCount)
        Just Token {tokenKind = TRBracket} -> do
          void parseAnyToken
          pure (SPList (reverse reversedPatterns))
        Nothing ->
          failTokenParser (ExpectedSyntax "']'" (ParserEndOfInputIn "list pattern"))
        Just token ->
          failTokenParserAt
            (tokenSpan token)
            (ExpectedSyntax "',' or ']'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseLambdaParameterParser :: Parser SurfaceLambdaParameter
parseLambdaParameterParser = do
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
    Just Token {tokenKind = TIdentifier parameterName}
      | parameterName == "_"
          || isReservedLiteralName parameterName
          || isConstructorIdentifierText parameterName ->
          parsePatternLambdaParameter
      | otherwise -> do
          parsedName <- parseIdentifier
          maybeTail <- peekToken
          case maybeTail of
            Just Token {tokenKind = TAt} ->
              SurfaceLambdaPattern
                <$> (parseIdentifierCasePattern parsedName >>= collectCasePatternAlternatives . (: []))
            Just Token {tokenKind = TOperator "|"} ->
              SurfaceLambdaPattern
                <$> (parseIdentifierCasePattern parsedName >>= collectCasePatternAlternatives . (: []))
            _ ->
              pure (SurfaceLambdaIdentifier (mkIdentifier parsedName))
    Nothing ->
      failTokenParser (ExpectedSyntax "identifier" (ParserEndOfInputIn "lambda parameter list"))
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "identifier" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parsePatternLambdaParameter :: Parser SurfaceLambdaParameter
parsePatternLambdaParameter =
  SurfaceLambdaPattern <$> parseCaseArmPatternParser

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
    Nothing -> False

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"
