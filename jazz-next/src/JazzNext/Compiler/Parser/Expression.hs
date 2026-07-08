{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec expression parser over the lexer token stream.
module JazzNext.Compiler.Parser.Expression
  ( parseExpressionTokens
  ) where

import Data.Set
  ( Set
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    mkDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.FractionalLiteral
  ( fractionalLiteralExceedsMagnitude,
    mkFractionalLiteralSource
  )
import JazzNext.Compiler.Identifier
  ( mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo,
    lookupOperatorInfoIn,
    operatorAssociativity,
    operatorPrecedence
  )
import qualified JazzNext.Compiler.Parser.TokenParser as TokenParser
import qualified Text.Megaparsec as MP

-- | Parse one expression from a token stream and return the unconsumed suffix.
--
-- The alias set is accepted now so the eventual integration point can keep the
-- same shape as the legacy parser entry point. Qualified-reference validation
-- remains a later resolver concern, matching the existing parser behavior.
parseExpressionTokens :: Set Text -> [OperatorInfo] -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens _knownAliases declaredOperators =
  runExpressionParser (parseExpressionWithRest declaredOperators)

newtype ExpressionParser a = ExpressionParser
  { unExpressionParser :: TokenParser.Parser (Either Diagnostic a)
  }

instance Functor ExpressionParser where
  fmap function (ExpressionParser parser) =
    ExpressionParser (fmap (fmap function) parser)

instance Applicative ExpressionParser where
  pure value = ExpressionParser (pure (Right value))

  ExpressionParser functionParser <*> ExpressionParser valueParser =
    ExpressionParser $ do
      functionResult <- functionParser
      case functionResult of
        Left diagnostic -> pure (Left diagnostic)
        Right function -> fmap (fmap function) valueParser

instance Monad ExpressionParser where
  ExpressionParser parser >>= next =
    ExpressionParser $ do
      result <- parser
      case result of
        Left diagnostic -> pure (Left diagnostic)
        Right value -> unExpressionParser (next value)

runExpressionParser :: ExpressionParser a -> [Token] -> Either Diagnostic a
runExpressionParser parser tokens =
  case MP.runParser (unExpressionParser parser) "jazz-next expression" tokens of
    Right (Right value) -> Right value
    Right (Left diagnostic) -> Left diagnostic
    Left _bundle -> Left (parseDiagnostic "unexpected token stream parse error")

parseExpressionWithRest :: [OperatorInfo] -> ExpressionParser (SurfaceExpr, [Token])
parseExpressionWithRest declaredOperators = do
  expr <- parseExpression declaredOperators 1
  remaining <- remainingTokens
  pure (expr, remaining)

parseExpression :: [OperatorInfo] -> Int -> ExpressionParser SurfaceExpr
parseExpression declaredOperators minPrecedence = do
  leftExpr <- parseApplicationExpression declaredOperators
  parseInfixTail declaredOperators minPrecedence leftExpr

parseApplicationExpression :: [OperatorInfo] -> ExpressionParser SurfaceExpr
parseApplicationExpression declaredOperators = do
  functionExpr <- parsePrimaryExpression declaredOperators
  parseApplicationTail functionExpr
  where
    parseApplicationTail functionExpr = do
      startsPrimary <- nextTokenStartsPrimaryExpression
      if startsPrimary
        then do
          argumentExpr <- parsePrimaryExpression declaredOperators
          parseApplicationTail (SEApply functionExpr argumentExpr)
        else pure functionExpr

parseInfixTail :: [OperatorInfo] -> Int -> SurfaceExpr -> ExpressionParser SurfaceExpr
parseInfixTail declaredOperators minPrecedence leftExpr = do
  tokens <- remainingTokens
  case tokens of
    operatorToken@Token {tokenKind = TOperator operatorSymbol} : tokensAfterOperator
      | shouldStopForSectionBoundary tokensAfterOperator ->
          pure leftExpr
      | otherwise ->
          case lookupOperatorInfoIn declaredOperators operatorSymbol of
            Nothing ->
              throwDiagnostic (undeclaredOperatorDiagnostic operatorToken operatorSymbol)
            Just operatorInfo
              | operatorPrecedence operatorInfo < minPrecedence ->
                  pure leftExpr
              | otherwise -> do
                  _ <- consumeAnyToken
                  let nextMinPrecedence =
                        case operatorAssociativity operatorInfo of
                          AssocLeft -> operatorPrecedence operatorInfo + 1
                          AssocRight -> operatorPrecedence operatorInfo
                  rightExpr <- parseExpression declaredOperators nextMinPrecedence
                  parseInfixTail declaredOperators minPrecedence (SEBinary operatorSymbol leftExpr rightExpr)
    _ -> pure leftExpr
  where
    shouldStopForSectionBoundary remainingAfterOperator =
      case remainingAfterOperator of
        Token {tokenKind = TRParen} : _ -> True
        _ -> False

parsePrimaryExpression :: [OperatorInfo] -> ExpressionParser SurfaceExpr
parsePrimaryExpression declaredOperators = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing ->
      throwDiagnostic (parseDiagnostic "expected expression before end of input")
    Just token ->
      case tokenKind token of
        TInt value -> do
          _ <- consumeAnyToken
          parseNumericLiteral token value
        TIdentifier name -> do
          _ <- consumeAnyToken
          parseIdentifierExpression token name
        TLParen -> do
          _ <- consumeAnyToken
          parseParenExpression declaredOperators
        TLBracket -> do
          _ <- consumeAnyToken
          parseListExpression declaredOperators
        -- TODO: if/case/lambda/block parsing stays in
        -- JazzNext.Compiler.Parser until those statement and pattern seams move.
        _ ->
          throwDiagnostic
            ( parseDiagnostic
                ( "unexpected token '"
                    <> tokenLexeme token
                    <> "' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> "; expected expression"
                )
            )

parseIdentifierExpression :: Token -> Text -> ExpressionParser SurfaceExpr
parseIdentifierExpression nameToken name =
  case name of
    "True" -> pure (SELit (SLBool True))
    "False" -> pure (SELit (SLBool False))
    _ -> do
      tokens <- remainingTokens
      case tokens of
        colonToken@Token {tokenKind = TColonColon} : memberToken@Token {tokenKind = TIdentifier memberName} : _
          | isImmediatelyAfter nameToken colonToken -> do
              _ <- consumeAnyToken
              _ <- consumeAnyToken
              pure (SEQualifiedVar (mkIdentifier name) (mkIdentifier memberName))
        colonToken@Token {tokenKind = TColonColon} : []
          | isImmediatelyAfter nameToken colonToken ->
              throwDiagnostic (parseDiagnostic "expected member name after '::' before end of input")
        colonToken@Token {tokenKind = TColonColon} : memberToken : _
          | isImmediatelyAfter nameToken colonToken ->
              throwDiagnostic
                ( parseDiagnostic
                    ( "expected member name after '::' at "
                        <> renderSourceSpan (tokenSpan memberToken)
                        <> ", found '"
                        <> tokenLexeme memberToken
                        <> "'"
                    )
                )
        _ -> pure (SEVar (mkIdentifier name))

parseNumericLiteral :: Token -> Integer -> ExpressionParser SurfaceExpr
parseNumericLiteral wholeToken wholeValue = do
  literal <- parseNumericSurfaceLiteral wholeToken wholeValue
  pure (SELit literal)

parseNumericSurfaceLiteral :: Token -> Integer -> ExpressionParser SurfaceLiteral
parseNumericSurfaceLiteral wholeToken wholeValue = do
  tokens <- remainingTokens
  case tokens of
    dotToken@Token {tokenKind = TDot} : fractionalToken@Token {tokenKind = TInt fractionalValue} : _
      | isImmediatelyAfter wholeToken dotToken,
        isImmediatelyAfter dotToken fractionalToken -> do
          _ <- consumeAnyToken
          _ <- consumeAnyToken
          maybeTargetType <- parseFractionalLiteralSuffix fractionalToken
          let literalText = tokenLexeme wholeToken <> "." <> tokenLexeme fractionalToken
              literalSource =
                mkFractionalLiteralSource
                  wholeValue
                  fractionalValue
                  (Text.length (tokenLexeme fractionalToken))
          floatValue <- either throwDiagnostic pure (parseFloatLiteral (tokenSpan wholeToken) literalText)
          if fractionalLiteralExceedsMagnitude literalSource float64MaxFinite
            then throwDiagnostic (invalidFloatLiteralDiagnostic (tokenSpan wholeToken) literalText)
            else pure ()
          pure (SLFloat floatValue literalSource maybeTargetType)
    _ ->
      pure (SLInt wholeValue)

parseFractionalLiteralSuffix :: Token -> ExpressionParser (Maybe SurfaceNumericType)
parseFractionalLiteralSuffix fractionalToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just suffixToken@Token {tokenKind = TIdentifier suffixName}
      | isImmediatelyAfter fractionalToken suffixToken,
        Just targetType <- fractionalLiteralSuffixTarget suffixName -> do
          _ <- consumeAnyToken
          pure (Just targetType)
    _ ->
      pure Nothing

fractionalLiteralSuffixTarget :: Text -> Maybe SurfaceNumericType
fractionalLiteralSuffixTarget suffixName =
  case suffixName of
    "f16" -> Just SurfaceNumericFloat16
    "f32" -> Just SurfaceNumericFloat32
    "f64" -> Just SurfaceNumericFloat64
    _ -> Nothing

parseParenExpression :: [OperatorInfo] -> ExpressionParser SurfaceExpr
parseParenExpression declaredOperators = do
  maybeToken <- peekToken
  case maybeToken of
    Just operatorToken@Token {tokenKind = TOperator operatorSymbol} -> do
      _ <- consumeAnyToken
      requireOperatorVisible declaredOperators operatorToken operatorSymbol
      nextToken <- peekToken
      case nextToken of
        Just Token {tokenKind = TRParen} -> do
          _ <- consumeAnyToken
          pure (SEOperatorValue operatorSymbol)
        _ -> do
          rightExpr <- parseExpression declaredOperators 1
          consumeRightParen
          pure (SESectionRight operatorSymbol rightExpr)
    _ -> do
      innerExpr <- parseExpression declaredOperators 1
      tokens <- remainingTokens
      case tokens of
        Token {tokenKind = TComma} : _ -> do
          _ <- consumeAnyToken
          tupleElements <- parseTupleElements declaredOperators [innerExpr]
          consumeRightParen
          pure (SETuple tupleElements)
        operatorToken@Token {tokenKind = TOperator operatorSymbol} : Token {tokenKind = TRParen} : _ -> do
          requireOperatorVisible declaredOperators operatorToken operatorSymbol
          _ <- consumeAnyToken
          _ <- consumeAnyToken
          pure (SESectionLeft innerExpr operatorSymbol)
        _ -> do
          consumeRightParen
          pure innerExpr

parseTupleElements :: [OperatorInfo] -> [SurfaceExpr] -> ExpressionParser [SurfaceExpr]
parseTupleElements declaredOperators reversedElements = do
  nextElement <- parseExpression declaredOperators 1
  tokens <- remainingTokens
  case tokens of
    Token {tokenKind = TComma} : _ -> do
      _ <- consumeAnyToken
      parseTupleElements declaredOperators (nextElement : reversedElements)
    _ ->
      pure (reverse (nextElement : reversedElements))

parseListExpression :: [OperatorInfo] -> ExpressionParser SurfaceExpr
parseListExpression declaredOperators = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBracket} -> do
      _ <- consumeAnyToken
      pure (SEList [])
    _ -> do
      elements <- parseListElements declaredOperators
      consumeRightBracket
      pure (SEList elements)

parseListElements :: [OperatorInfo] -> ExpressionParser [SurfaceExpr]
parseListElements declaredOperators = do
  firstElement <- parseExpression declaredOperators 1
  go [firstElement]
  where
    go elements = do
      tokens <- remainingTokens
      case tokens of
        Token {tokenKind = TComma} : _ -> do
          _ <- consumeAnyToken
          nextElement <- parseExpression declaredOperators 1
          go (nextElement : elements)
        _ ->
          pure (reverse elements)

nextTokenStartsPrimaryExpression :: ExpressionParser Bool
nextTokenStartsPrimaryExpression = do
  maybeToken <- peekToken
  pure
    ( case tokenKind <$> maybeToken of
        Just (TInt _) -> True
        Just (TIdentifier _) -> True
        Just TLParen -> True
        Just TLBracket -> True
        _ -> False
    )

consumeRightParen :: ExpressionParser ()
consumeRightParen = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRParen} -> do
      _ <- consumeAnyToken
      pure ()
    Nothing ->
      throwDiagnostic (parseDiagnostic "expected ')' before end of input")
    Just token ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeRightBracket :: ExpressionParser ()
consumeRightBracket = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBracket} -> do
      _ <- consumeAnyToken
      pure ()
    Nothing ->
      throwDiagnostic (parseDiagnostic "expected ']' before end of input")
    Just token ->
      throwDiagnostic
        ( parseDiagnostic
            ( "expected ']' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

requireOperatorVisible :: [OperatorInfo] -> Token -> Text -> ExpressionParser ()
requireOperatorVisible declaredOperators operatorToken operatorSymbol =
  case lookupOperatorInfoIn declaredOperators operatorSymbol of
    Just _ -> pure ()
    Nothing -> throwDiagnostic (undeclaredOperatorDiagnostic operatorToken operatorSymbol)

undeclaredOperatorDiagnostic :: Token -> Text -> Diagnostic
undeclaredOperatorDiagnostic operatorToken operatorSymbol =
  parseDiagnostic
    ( "operator '"
        <> operatorSymbol
        <> "' must be declared before use at "
        <> renderSourceSpan (tokenSpan operatorToken)
    )

parseFloatLiteral :: SourceSpan -> Text -> Either Diagnostic Double
parseFloatLiteral literalSpan literalText =
  case TextRead.double literalText of
    Right (value, trailing)
      | Text.null trailing,
        finiteFloat value ->
          Right value
    _ ->
      Left (invalidFloatLiteralDiagnostic literalSpan literalText)

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

float64MaxFinite :: Double
float64MaxFinite =
  encodeFloat
    (floatRadix sample ^ floatDigits sample - 1)
    (snd (floatRange sample) - floatDigits sample)
  where
    sample = 0 :: Double

invalidFloatLiteralDiagnostic :: SourceSpan -> Text -> Diagnostic
invalidFloatLiteralDiagnostic literalSpan literalText =
  parseDiagnostic
    ( "invalid fractional literal '"
        <> literalText
        <> "' at "
        <> renderSourceSpan literalSpan
    )

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  spanLine (tokenSpan leftToken) == spanLine (tokenSpan rightToken)
    && spanColumn (tokenSpan rightToken) == spanColumn (tokenSpan leftToken) + Text.length (tokenLexeme leftToken)

peekToken :: ExpressionParser (Maybe Token)
peekToken =
  liftTokenParser TokenParser.peekToken

consumeAnyToken :: ExpressionParser Token
consumeAnyToken =
  liftTokenParser TokenParser.parseAnyToken

remainingTokens :: ExpressionParser [Token]
remainingTokens =
  liftTokenParser MP.getInput

liftTokenParser :: TokenParser.Parser a -> ExpressionParser a
liftTokenParser parser =
  ExpressionParser (Right <$> parser)

throwDiagnostic :: Diagnostic -> ExpressionParser a
throwDiagnostic diagnostic =
  ExpressionParser (pure (Left diagnostic))

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
