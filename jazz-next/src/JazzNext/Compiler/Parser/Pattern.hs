{-# LANGUAGE OverloadedStrings #-}

-- | Pattern grammar over lexer tokens.
module JazzNext.Compiler.Parser.Pattern
  ( parseCaseArmPatternTokens,
    parseCasePatternTokens,
    parseLambdaParameterTokens
  ) where

import Control.Monad
  ( ap
  )
import Data.Char
  ( isUpper
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderSourceSpan
  )
import JazzNext.Compiler.Identifier
  ( mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    parseDiagnostic,
    runTokenParser
  )
import qualified Text.Megaparsec as MP

newtype PatternParser a = PatternParser
  { runPatternParserState :: Parser (Either Diagnostic a)
  }

instance Functor PatternParser where
  fmap f parser =
    PatternParser (fmap (fmap f) (runPatternParserState parser))

instance Applicative PatternParser where
  pure value = PatternParser (pure (Right value))
  (<*>) = ap

instance Monad PatternParser where
  parser >>= next =
    PatternParser $ do
      result <- runPatternParserState parser
      case result of
        Left diagnostic -> pure (Left diagnostic)
        Right value -> runPatternParserState (next value)

parseCaseArmPatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCaseArmPatternTokens =
  runPatternParser "case arm pattern" (withRemainder parseCaseArmPattern)

parseCasePatternTokens :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCasePatternTokens =
  runPatternParser "case pattern" (withRemainder parseCasePattern)

parseLambdaParameterTokens :: [Token] -> Either Diagnostic (SurfaceLambdaParameter, [Token])
parseLambdaParameterTokens =
  runPatternParser "lambda parameter" (withRemainder parseLambdaParameter)

runPatternParser :: Text -> PatternParser a -> [Token] -> Either Diagnostic a
runPatternParser label parser tokens =
  case runTokenParser label finalize tokens of
    Left diagnostic -> Left diagnostic
    Right (Left diagnostic) -> Left diagnostic
    Right (Right value) -> Right value
  where
    finalize = do
      result <- runPatternParserState parser
      MP.setInput []
      pure result

withRemainder :: PatternParser a -> PatternParser (a, [Token])
withRemainder parser = do
  value <- parser
  remaining <- getRemainingTokens
  pure (value, remaining)

getRemainingTokens :: PatternParser [Token]
getRemainingTokens =
  PatternParser (Right <$> MP.getInput)

setRemainingTokens :: [Token] -> PatternParser ()
setRemainingTokens tokens =
  PatternParser (Right <$> MP.setInput tokens)

throwDiagnostic :: Diagnostic -> PatternParser a
throwDiagnostic diagnostic =
  PatternParser (pure (Left diagnostic))

parseCaseArmPattern :: PatternParser SurfacePattern
parseCaseArmPattern = do
  firstPattern <- parseCasePattern
  collectAlternatives [firstPattern]
  where
    collectAlternatives reversedPatterns = do
      remainingTokens <- getRemainingTokens
      case remainingTokens of
        Token {tokenKind = TOperator "|"} : afterPipe -> do
          setRemainingTokens afterPipe
          nextPattern <- parseCasePattern
          collectAlternatives (nextPattern : reversedPatterns)
        _ ->
          let alternatives = reverse reversedPatterns
           in case alternatives of
                [singlePattern] -> pure singlePattern
                _ -> pure (SPOr alternatives)

parseCasePattern :: PatternParser SurfacePattern
parseCasePattern = do
  tokens <- getRemainingTokens
  case tokens of
    token@Token {tokenKind = TInt value} : rest -> do
      setRemainingTokens rest
      parseIntegralPatternLiteral token value
    Token {tokenKind = TLBracket} : rest -> do
      setRemainingTokens rest
      parseListPattern
    token@Token {tokenKind = TLParen} : rest -> do
      setRemainingTokens rest
      parseTuplePattern token
    Token {tokenKind = TIdentifier name} : rest -> do
      setRemainingTokens rest
      case name of
        "_" -> pure SPWildcard
        "True" -> pure (SPLiteral (SLBool True))
        "False" -> pure (SPLiteral (SLBool False))
        _
          | isConstructorIdentifierText name ->
              parseConstructorPattern name
          | otherwise ->
              parseAsPatternOrVariable parseCasePattern name
    [] ->
      throwDiagnostic (parseDiagnostic "expected case pattern before end of input")
    token : _ ->
      throwDiagnostic (expectedCasePatternDiagnostic token)

expectedCasePatternDiagnostic :: Token -> Diagnostic
expectedCasePatternDiagnostic token =
  parseDiagnostic
    ( "expected case pattern at "
        <> renderSourceSpan (tokenSpan token)
        <> ", found '"
        <> tokenLexeme token
        <> "'"
    )

parseTuplePattern :: Token -> PatternParser SurfacePattern
parseTuplePattern leftParenToken = do
  tokens <- getRemainingTokens
  case tokens of
    Token {tokenKind = TRParen} : rest -> do
      setRemainingTokens rest
      pure (SPTuple [])
    _ -> do
      firstPattern <- parseCasePattern
      afterFirstPattern <- getRemainingTokens
      case afterFirstPattern of
        Token {tokenKind = TComma} : rest -> do
          setRemainingTokens rest
          tuplePatterns <- parseTuplePatternElements [firstPattern]
          consumeRightParen
          pure (SPTuple tuplePatterns)
        _ ->
          throwDiagnostic (expectedCasePatternDiagnostic leftParenToken)

parseTuplePatternElements :: [SurfacePattern] -> PatternParser [SurfacePattern]
parseTuplePatternElements reversedPatterns = do
  nextPattern <- parseCasePattern
  afterNextPattern <- getRemainingTokens
  case afterNextPattern of
    Token {tokenKind = TComma} : rest -> do
      setRemainingTokens rest
      parseTuplePatternElements (nextPattern : reversedPatterns)
    _ ->
      pure (reverse (nextPattern : reversedPatterns))

parseConstructorPattern :: Text -> PatternParser SurfacePattern
parseConstructorPattern constructorName =
  go []
  where
    go revArguments = do
      remainingTokens <- getRemainingTokens
      if patternArgumentBoundary remainingTokens
        then pureConstructor revArguments remainingTokens
        else
          if startsCasePatternTokens remainingTokens
            then do
              nextArgument <- parseConstructorArgumentPattern
              go (nextArgument : revArguments)
            else pureConstructor revArguments remainingTokens

    pureConstructor revArguments _ =
      pure (SPConstructor (mkIdentifier constructorName) (reverse revArguments))

parseConstructorArgumentPattern :: PatternParser SurfacePattern
parseConstructorArgumentPattern = do
  tokens <- getRemainingTokens
  case tokens of
    token@Token {tokenKind = TInt value} : rest -> do
      setRemainingTokens rest
      parseIntegralPatternLiteral token value
    Token {tokenKind = TIdentifier name} : rest -> do
      setRemainingTokens rest
      case name of
        "True" ->
          pure (SPLiteral (SLBool True))
        "False" ->
          pure (SPLiteral (SLBool False))
        "_" ->
          pure SPWildcard
        _
          | isConstructorIdentifierText name ->
              pure (SPConstructor (mkIdentifier name) [])
          | otherwise ->
              parseAsPatternOrVariable parseConstructorArgumentPattern name
    Token {tokenKind = TLBracket} : rest -> do
      setRemainingTokens rest
      parseListPattern
    token@Token {tokenKind = TLParen} : rest -> do
      setRemainingTokens rest
      parseTuplePattern token
    [] ->
      throwDiagnostic (parseDiagnostic "expected constructor pattern argument before end of input")
    token : _ ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected constructor pattern argument at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseIntegralPatternLiteral :: Token -> Integer -> PatternParser SurfacePattern
parseIntegralPatternLiteral wholeToken wholeValue = do
  tokensAfterWhole <- getRemainingTokens
  case tokensAfterWhole of
    dotToken@Token {tokenKind = TDot} : fractionalToken@Token {tokenKind = TInt _} : _
      | isImmediatelyAfter wholeToken dotToken,
        isImmediatelyAfter dotToken fractionalToken ->
          throwDiagnostic
            ( parseDiagnostic
                ( "fractional literal patterns are not supported at "
                    <> renderSourceSpan (tokenSpan wholeToken)
                )
            )
    _ ->
      pure (SPLiteral (SLInt wholeValue))

parseAsPatternOrVariable ::
  PatternParser SurfacePattern ->
  Text ->
  PatternParser SurfacePattern
parseAsPatternOrVariable parseAsTail name = do
  tokensAfterName <- getRemainingTokens
  case tokensAfterName of
    Token {tokenKind = TAt} : tokensAfterAt -> do
      setRemainingTokens tokensAfterAt
      patternExpr <- parseAsTail
      pure (SPAs (mkIdentifier name) patternExpr)
    _ ->
      pure (SPVariable (mkIdentifier name))

patternArgumentBoundary :: [Token] -> Bool
patternArgumentBoundary tokens =
  case tokens of
    [] -> True
    Token {tokenKind = TArrow} : _ -> True
    Token {tokenKind = TComma} : _ -> True
    Token {tokenKind = TRBracket} : _ -> True
    Token {tokenKind = TRParen} : _ -> True
    Token {tokenKind = TRBrace} : _ -> True
    _ -> False

startsCasePatternTokens :: [Token] -> Bool
startsCasePatternTokens tokens =
  case tokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    Token {tokenKind = TLParen} : _ -> True
    _ -> False

parseListPattern :: PatternParser SurfacePattern
parseListPattern = do
  tokensAfterLeftBracket <- getRemainingTokens
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest -> do
      setRemainingTokens rest
      pure (SPList [])
    _ -> do
      firstPattern <- parseCasePattern
      go [firstPattern]
  where
    go revPatterns = do
      remainingTokens <- getRemainingTokens
      case remainingTokens of
        Token {tokenKind = TComma} : rest -> do
          setRemainingTokens rest
          nextPattern <- parseCasePattern
          go (nextPattern : revPatterns)
        Token {tokenKind = TOperator "|"} : rest -> do
          setRemainingTokens rest
          tailPattern <- parseCasePattern
          consumeRightBracket
          case reverse revPatterns of
            [headPattern] ->
              pure (SPConsList headPattern tailPattern)
            _ ->
              throwDiagnostic (parseDiagnostic "cons-like list patterns require exactly one head pattern before '|'")
        Token {tokenKind = TRBracket} : rest -> do
          setRemainingTokens rest
          pure (SPList (reverse revPatterns))
        [] ->
          throwDiagnostic (parseDiagnostic "expected ']' before end of input in list pattern")
        token : _ ->
          throwDiagnostic
            ( parseDiagnostic
                ( "expected ',' or ']' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

parseLambdaParameter :: PatternParser SurfaceLambdaParameter
parseLambdaParameter = do
  tokens <- getRemainingTokens
  case tokens of
    Token {tokenKind = TInt _} : _ -> parsePatternLambdaParameter
    Token {tokenKind = TLParen} : _ -> parsePatternLambdaParameter
    Token {tokenKind = TLBracket} : _ -> parsePatternLambdaParameter
    Token {tokenKind = TIdentifier parameterName} : rest
      | parameterName == "_" ->
          parsePatternLambdaParameter
      | isReservedLiteralName parameterName ->
          parsePatternLambdaParameter
      | isConstructorIdentifierText parameterName ->
          parsePatternLambdaParameter
      | startsAsPatternTail rest ->
          parsePatternLambdaParameter
      | startsLambdaOrPatternTail rest ->
          parsePatternLambdaParameter
      | otherwise -> do
          setRemainingTokens rest
          pure (SurfaceLambdaIdentifier (mkIdentifier parameterName))
    [] ->
      throwDiagnostic (parseDiagnostic "expected identifier before end of input in lambda parameter list")
    token : _ ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected identifier at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parsePatternLambdaParameter :: PatternParser SurfaceLambdaParameter
parsePatternLambdaParameter = do
  patternValue <- parseCaseArmPattern
  pure (SurfaceLambdaPattern patternValue)

startsAsPatternTail :: [Token] -> Bool
startsAsPatternTail tokens =
  case tokens of
    Token {tokenKind = TAt} : _ -> True
    _ -> False

startsLambdaOrPatternTail :: [Token] -> Bool
startsLambdaOrPatternTail tokens =
  case tokens of
    Token {tokenKind = TOperator "|"} : _ -> True
    _ -> False

consumeRightParen :: PatternParser ()
consumeRightParen = do
  tokens <- getRemainingTokens
  case tokens of
    Token {tokenKind = TRParen} : rest -> setRemainingTokens rest
    [] -> throwDiagnostic (parseDiagnostic "expected ')' before end of input")
    token : _ ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeRightBracket :: PatternParser ()
consumeRightBracket = do
  tokens <- getRemainingTokens
  case tokens of
    Token {tokenKind = TRBracket} : rest -> setRemainingTokens rest
    [] -> throwDiagnostic (parseDiagnostic "expected ']' before end of input")
    token : _ ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected ']' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
    Nothing -> False

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  spanLine (tokenSpan leftToken) == spanLine (tokenSpan rightToken)
    && spanColumn (tokenSpan rightToken) == spanColumn (tokenSpan leftToken) + Text.length (tokenLexeme leftToken)
