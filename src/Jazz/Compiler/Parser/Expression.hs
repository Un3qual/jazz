{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Direct Megaparsec grammar for surface expressions.
module Jazz.Compiler.Parser.Expression
  ( parseExpressionParser,
    parseExpressionTokens,
  )
where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.FractionalLiteral
  ( fractionalLiteralExceedsMagnitude,
    mkFractionalLiteralSource,
  )
import Jazz.Compiler.Name
  ( identifierText,
    mkIdentifier,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfacePatternLambdaClause (..),
    SurfaceSignatureType,
  )
import Jazz.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
  )
import Jazz.Compiler.Parser.Failure
  ( ParserEncountered (..),
    ParserFailureReason (..),
    ParserInternalInvariant (..),
    ParserOperatorUse (..),
    ParserPatternFailure (..),
    ParserUnsupportedFeature (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    OperatorTable,
    lookupOperatorInfoIn,
  )
import qualified Jazz.Compiler.Parser.Pattern as Pattern
import Jazz.Compiler.Parser.Signature (parseSignatureTypeParser)
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    parseToken,
    peekToken,
    runTokenParserPrefix,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )
import qualified Text.Megaparsec as MP

type Stop = TokenStream -> Bool

parseExpressionTokens ::
  StatementBlockParser ->
  ParserContext ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens parseBlock context =
  runTokenParserPrefix "expression" (parseExpressionParser parseBlock context)

parseExpressionParser :: StatementBlockParser -> ExpressionParser
parseExpressionParser parseBlock context =
  parseExprWithMinPrecedenceUntil parseBlock context neverStop 1

parseExprWithMinPrecedenceUntil ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Int ->
  Parser SurfaceExpr
parseExprWithMinPrecedenceUntil parseBlock context stop minPrecedence = do
  leftExpr <- parseApplicationExprUntil parseBlock context stop
  parseInfixTailWithUntil
    context
    stop
    (parseExprWithMinPrecedenceUntil parseBlock context)
    minPrecedence
    leftExpr

parseApplicationExprUntil ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Parser SurfaceExpr
parseApplicationExprUntil parseBlock context stop = do
  functionExpr <- parsePrimaryExpr parseBlock context stop
  parseApplicationTailUntil parseBlock context stop functionExpr

parseApplicationTailUntil ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  SurfaceExpr ->
  Parser SurfaceExpr
parseApplicationTailUntil parseBlock context stop functionExpr = do
  tokens <- MP.getInput
  if stop tokens
    then pure functionExpr
    else case tokens of
      typeApplicationToken@Token {tokenKind = TAt} :< _ -> do
        void parseAnyToken
        typeArgument <- parseTypeApplicationArgument typeApplicationToken
        parseApplicationTailUntil
          parseBlock
          context
          stop
          (SETypeApplication functionExpr (tokenSpan typeApplicationToken) typeArgument)
      firstToken :< _
        | startsPrimaryExpr firstToken -> do
            argumentExpr <- parsePrimaryExpr parseBlock context stop
            parseApplicationTailUntil parseBlock context stop (SEApply functionExpr argumentExpr)
      _ -> pure functionExpr

parseTypeApplicationArgument :: Token -> Parser SurfaceSignatureType
parseTypeApplicationArgument typeApplicationToken = do
  parsedType <- MP.observing parseSignatureTypeParser
  case parsedType of
    Right signatureType -> pure signatureType
    Left _ ->
      failTokenParserAt
        (tokenSpan typeApplicationToken)
        (UnsupportedSyntax ExplicitTypeApplicationArgument)

neverStop :: Stop
neverStop _ = False

thenStarts :: Stop -> Stop
thenStarts stop tokens =
  stop tokens
    || case tokens of
      Token {tokenKind = TThen} :< _ -> True
      _ -> False

startsPrimaryExpr :: Token -> Bool
startsPrimaryExpr token =
  case tokenKind token of
    TInt _ -> True
    TChar _ -> True
    TText _ -> True
    TIdentifier _ -> True
    TIf -> True
    TCase -> True
    TLambda -> True
    TLParen -> True
    TLBrace -> True
    TLBracket -> True
    _ -> False

parseInfixTailWithUntil ::
  ParserContext ->
  Stop ->
  (Stop -> Int -> Parser SurfaceExpr) ->
  Int ->
  SurfaceExpr ->
  Parser SurfaceExpr
parseInfixTailWithUntil context stop parseRhs minPrecedence leftExpr = do
  tokens <- MP.getInput
  if stop tokens
    then pure leftExpr
    else case tokens of
      operatorToken@Token {tokenKind = TOperator symbol} :< tokensAfterOperator
        | startsRightParen tokensAfterOperator ->
            pure leftExpr
        | otherwise ->
            case lookupOperatorInfoIn (parserDeclaredOperators context) symbol of
              Nothing ->
                failUndeclaredOperator operatorToken symbol
              Just operatorInfo
                | operatorPrecedence operatorInfo < minPrecedence ->
                    pure leftExpr
                | otherwise -> do
                    void parseAnyToken
                    let rhsStop =
                          samePrecedenceNonAssociativeRhsStop
                            (parserDeclaredOperators context)
                            operatorInfo
                            stop
                    rightExpr <- parseRhs rhsStop (operatorNextMinPrecedence operatorInfo)
                    rejectNonAssociativeContinuation context operatorInfo operatorToken
                    parseInfixTailWithUntil
                      context
                      stop
                      parseRhs
                      minPrecedence
                      (SEBinary symbol leftExpr rightExpr)
      _ -> pure leftExpr

operatorNextMinPrecedence :: OperatorInfo -> Int
operatorNextMinPrecedence operatorInfo =
  case operatorAssociativity operatorInfo of
    AssocLeft -> operatorPrecedence operatorInfo + 1
    AssocRight -> operatorPrecedence operatorInfo
    AssocNonAssoc -> operatorPrecedence operatorInfo + 1

rejectNonAssociativeContinuation :: ParserContext -> OperatorInfo -> Token -> Parser ()
rejectNonAssociativeContinuation context operatorInfo operatorToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TOperator nextSymbol} ->
      case lookupOperatorInfoIn (parserDeclaredOperators context) nextSymbol of
        Just nextInfo
          | operatorPrecedence nextInfo == operatorPrecedence operatorInfo,
            operatorAssociativity operatorInfo == AssocNonAssoc
              || operatorAssociativity nextInfo == AssocNonAssoc ->
              failTokenParserAt
                (tokenSpan operatorToken)
                (NonAssociativeOperatorChain (nonAssociativeSymbol nextInfo))
        _ -> pure ()
    _ -> pure ()
  where
    nonAssociativeSymbol nextInfo
      | operatorAssociativity operatorInfo == AssocNonAssoc = operatorSymbol operatorInfo
      | otherwise = operatorSymbol nextInfo

samePrecedenceNonAssociativeRhsStop ::
  OperatorTable ->
  OperatorInfo ->
  Stop ->
  Stop
samePrecedenceNonAssociativeRhsStop declaredOperators operatorInfo stop tokens =
  stop tokens
    || case tokens of
      Token {tokenKind = TOperator nextSymbol} :< _ ->
        case lookupOperatorInfoIn declaredOperators nextSymbol of
          Just nextInfo ->
            operatorPrecedence nextInfo == operatorPrecedence operatorInfo
              && ( operatorAssociativity operatorInfo == AssocNonAssoc
                     || operatorAssociativity nextInfo == AssocNonAssoc
                 )
          Nothing -> False
      _ -> False

parsePrimaryExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Parser SurfaceExpr
parsePrimaryExpr parseBlock context stop = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing ->
      failTokenParser (ExpectedSyntax "expression" ParserEndOfInput)
    Just token -> do
      void parseAnyToken
      case tokenKind token of
        TInt value ->
          SELit <$> parseNumericSurfaceLiteral token value
        TChar value ->
          pure (SELit (SLChar value))
        TText value ->
          pure (SELit (SLText value))
        TIdentifier "True" ->
          pure (SELit (SLBool True))
        TIdentifier "False" ->
          pure (SELit (SLBool False))
        TIdentifier name ->
          parseIdentifierExpr token name
        TIf ->
          parseIfExpr parseBlock context stop token
        TCase ->
          parseCaseExpr parseBlock context token
        TLambda ->
          parseLambdaExpr parseBlock context stop token
        TLParen ->
          parseParenExpr parseBlock context
        TLBrace -> do
          statements <-
            parseBlock
              context
                { parserStatementContext = NestedBlockContext
                }
          pure (SEBlock statements)
        TLBracket ->
          parseListExpr parseBlock context
        _ ->
          failTokenParserAt
            (tokenSpan token)
            ( UnexpectedSyntax
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
                "expression"
            )

parseIdentifierExpr :: Token -> Text -> Parser SurfaceExpr
parseIdentifierExpr identifierToken name = do
  tokens <- MP.getInput
  case tokens of
    colonToken@Token {tokenKind = TColonColon} :< memberToken@Token {tokenKind = TIdentifier memberName} :< _
      | isImmediatelyAfter identifierToken colonToken,
        isImmediatelyAfter colonToken memberToken -> do
          void parseAnyToken
          void parseAnyToken
          pure (SEQualifiedVar (mkIdentifier name) (mkIdentifier memberName))
    colonToken@Token {tokenKind = TColonColon} :< EmptyTokens
      | isImmediatelyAfter identifierToken colonToken ->
          failTokenParser
            (ExpectedSyntax "member name" (ParserEndOfInputAfter "'::'"))
    colonToken@Token {tokenKind = TColonColon} :< memberToken :< _
      | isImmediatelyAfter identifierToken colonToken ->
          failTokenParserAt
            (tokenSpan memberToken)
            ( ExpectedSyntax
                ( case tokenKind memberToken of
                    TIdentifier {} -> "adjacent member name after '::'"
                    _ -> "member name after '::'"
                )
                (ParserFoundToken (tokenKind memberToken) (tokenLexeme memberToken))
            )
    _ ->
      pure (SEVar (mkIdentifier name))

parseNumericSurfaceLiteral :: Token -> Integer -> Parser SurfaceLiteral
parseNumericSurfaceLiteral wholeToken wholeValue = do
  tokens <- MP.getInput
  case tokens of
    dotToken@Token {tokenKind = TDot} :< fractionalToken@Token {tokenKind = TInt fractionalValue} :< _
      | isImmediatelyAfter wholeToken dotToken,
        isImmediatelyAfter dotToken fractionalToken -> do
          void parseAnyToken
          void parseAnyToken
          maybeTargetType <- parseFractionalLiteralSuffix fractionalToken
          let literalText = tokenLexeme wholeToken <> "." <> tokenLexeme fractionalToken
              literalSource =
                mkFractionalLiteralSource
                  wholeValue
                  fractionalValue
                  (Text.length (tokenLexeme fractionalToken))
          floatValue <-
            either
              (const (failTokenParserAt (tokenSpan wholeToken) (InvalidFractionalLiteral literalText)))
              pure
              (parseFloatLiteral literalText)
          if fractionalLiteralExceedsMagnitude literalSource float64MaxFinite
            then failTokenParserAt (tokenSpan wholeToken) (InvalidFractionalLiteral literalText)
            else pure (SLFloat floatValue literalSource maybeTargetType)
    _ ->
      pure (SLInt wholeValue)

parseFractionalLiteralSuffix :: Token -> Parser (Maybe SurfaceNumericType)
parseFractionalLiteralSuffix fractionalToken = do
  maybeToken <- peekToken
  case maybeToken of
    Just suffixToken@Token {tokenKind = TIdentifier suffixName}
      | isImmediatelyAfter fractionalToken suffixToken,
        Just targetType <- fractionalLiteralSuffixTarget suffixName -> do
          void parseAnyToken
          pure (Just targetType)
    _ -> pure Nothing

fractionalLiteralSuffixTarget :: Text -> Maybe SurfaceNumericType
fractionalLiteralSuffixTarget suffixName =
  case suffixName of
    "f16" -> Just SurfaceNumericFloat16
    "f32" -> Just SurfaceNumericFloat32
    "f64" -> Just SurfaceNumericFloat64
    _ -> Nothing

parseFloatLiteral :: Text -> Either Text Double
parseFloatLiteral literalText =
  case TextRead.double literalText of
    Right (value, trailing)
      | Text.null trailing,
        finiteFloat value ->
          Right value
    _ -> Left (invalidFloatLiteralMessage literalText)

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

float64MaxFinite :: Double
float64MaxFinite =
  encodeFloat
    (floatRadix sample ^ floatDigits sample - 1)
    (snd (floatRange sample) - floatDigits sample)
  where
    sample = 0 :: Double

invalidFloatLiteralMessage :: Text -> Text
invalidFloatLiteralMessage literalText =
  "invalid fractional literal '"
    <> literalText
    <> "'"

requireOperatorVisible :: ParserContext -> Token -> Parser ()
requireOperatorVisible context operatorToken =
  case tokenKind operatorToken of
    TOperator symbol ->
      case lookupOperatorInfoIn (parserDeclaredOperators context) symbol of
        Just _ -> pure ()
        Nothing -> failUndeclaredOperator operatorToken symbol
    _ ->
      failTokenParserAt
        (tokenSpan operatorToken)
        (InternalParserFailure (ExpectedOperatorToken OperatorUseInExpression))

failUndeclaredOperator :: Token -> Text -> Parser a
failUndeclaredOperator operatorToken symbol =
  failTokenParserAt
    (tokenSpan operatorToken)
    (UndeclaredOperator symbol OperatorUseInExpression)

parseParenExpr :: StatementBlockParser -> ParserContext -> Parser SurfaceExpr
parseParenExpr parseBlock context = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TRParen} :< _ -> do
      void parseAnyToken
      pure (SETuple [])
    operatorToken@Token {tokenKind = TOperator symbol} :< rest -> do
      void parseAnyToken
      requireOperatorVisible context operatorToken
      case rest of
        Token {tokenKind = TRParen} :< _ -> do
          void parseAnyToken
          pure (SEOperatorValue symbol)
        _ -> do
          rightExpr <- parseExpressionParser parseBlock context
          void (parseToken TRParen)
          pure (SESectionRight symbol rightExpr)
    _ -> do
      innerExpr <- parseExpressionParser parseBlock context
      afterInner <- MP.getInput
      case afterInner of
        Token {tokenKind = TComma} :< _ -> do
          void parseAnyToken
          tupleElements <- parseTupleElements parseBlock context [innerExpr]
          void (parseToken TRParen)
          pure (SETuple tupleElements)
        operatorToken@Token {tokenKind = TOperator symbol} :< Token {tokenKind = TRParen} :< _ -> do
          void parseAnyToken
          void parseAnyToken
          requireOperatorVisible context operatorToken
          pure (SESectionLeft innerExpr symbol)
        _ -> do
          void (parseToken TRParen)
          pure innerExpr

parseTupleElements ::
  StatementBlockParser ->
  ParserContext ->
  [SurfaceExpr] ->
  Parser [SurfaceExpr]
parseTupleElements parseBlock context reversedElements = do
  nextElement <- parseExpressionParser parseBlock context
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TComma} -> do
      void parseAnyToken
      parseTupleElements parseBlock context (nextElement : reversedElements)
    _ -> pure (reverse (nextElement : reversedElements))

parseListExpr :: StatementBlockParser -> ParserContext -> Parser SurfaceExpr
parseListExpr parseBlock context = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBracket} -> do
      void parseAnyToken
      pure (SEList [])
    _ -> do
      elements <- parseListElements parseBlock context
      void (parseToken TRBracket)
      pure (SEList elements)

parseListElements :: StatementBlockParser -> ParserContext -> Parser [SurfaceExpr]
parseListElements parseBlock context = do
  firstElement <- parseExpressionParser parseBlock context
  collect [firstElement]
  where
    collect reversedElements = do
      maybeToken <- peekToken
      case maybeToken of
        Just Token {tokenKind = TComma} -> do
          void parseAnyToken
          nextElement <- parseExpressionParser parseBlock context
          collect (nextElement : reversedElements)
        _ -> pure (reverse reversedElements)

parseIfExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Token ->
  Parser SurfaceExpr
parseIfExpr parseBlock context stop ifToken = do
  conditionExpr <- parseExprWithMinPrecedenceUntil parseBlock context (thenStarts stop) 1
  maybeThen <- peekToken
  case maybeThen of
    Just Token {tokenKind = TThen} -> void parseAnyToken
    Nothing ->
      failTokenParserAt
        (tokenSpan ifToken)
        (ExpectedSyntax "'then'" (ParserEndOfInputAfter "'if'"))
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'then'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
  thenExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
  maybeElse <- peekToken
  case maybeElse of
    Just Token {tokenKind = TElse} -> do
      void parseAnyToken
      elseExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
      pure (SEIf conditionExpr thenExpr elseExpr)
    Nothing ->
      failTokenParserAt
        (tokenSpan ifToken)
        (ExpectedSyntax "'else'" (ParserEndOfInputAfter "'if'"))
    Just token ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'else'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

parseCaseExpr :: StatementBlockParser -> ParserContext -> Token -> Parser SurfaceExpr
parseCaseExpr parseBlock context caseToken = do
  scrutineeExpr <-
    parseExprWithMinPrecedenceUntil parseBlock context caseBodyStarts 1
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TLBrace} :< _ -> do
      void parseAnyToken
      caseArms <- parseCaseArms parseBlock context
      pure (SECase scrutineeExpr caseArms)
    EmptyTokens ->
      failTokenParserAt
        (tokenSpan caseToken)
        (ExpectedSyntax "'{'" (ParserEndOfInputAfter "'case'"))
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'{'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

caseBodyStarts :: Stop
caseBodyStarts tokens =
  case tokens of
    Token {tokenKind = TLBrace} :< rest ->
      case rest of
        Token {tokenKind = TOperator "|"} :< _ -> True
        Token {tokenKind = TRBrace} :< _ -> True
        _ -> hasTopLevelArrowBeforeTerminator rest
    _ -> False

parseCaseArms :: StatementBlockParser -> ParserContext -> Parser [SurfaceCaseArm]
parseCaseArms parseBlock context = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBrace, tokenSpan = rightBraceSpan} ->
      failTokenParserAt
        rightBraceSpan
        (ExpectedSyntax "case arm" (ParserBeforeToken TRBrace "}" Nothing))
    _ -> do
      firstArm <- parseCaseArm parseBlock context
      collect [firstArm]
  where
    collect reversedArms = do
      maybeToken <- peekToken
      case maybeToken of
        Just Token {tokenKind = TRBrace} -> do
          void parseAnyToken
          pure (reverse reversedArms)
        _ -> do
          nextArm <- parseCaseArm parseBlock context
          collect (nextArm : reversedArms)

parseCaseArm :: StatementBlockParser -> ParserContext -> Parser SurfaceCaseArm
parseCaseArm parseBlock context = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TOperator "|"} :< _ -> void parseAnyToken
    EmptyTokens -> failTokenParser (ExpectedSyntax "'|'" (ParserEndOfInputIn "case expression"))
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'|' to start case arm" (ParserAtToken (tokenKind token) (tokenLexeme token)))
  casePattern <- Pattern.parseCaseArmPatternParser
  guardExpr <- parseOptionalCaseArmGuard
  bodyTokens <- MP.getInput
  bodyExpr <-
    -- Without another top-level arrow, no later arm can begin. The ordinary
    -- operator parser can consume pipe chains without reparsing each suffix as
    -- a speculative case pattern.
    if hasTopLevelArrowBeforeCaseBodyEnd bodyTokens
      then parseCaseArmBodyExpr parseBlock context neverStop Nothing 1
      else
        parseExprWithMinPrecedenceUntil
          parseBlock
          context
          stopsBeforeCaseArmTerminator
          1
  pure (SurfaceCaseArm casePattern guardExpr bodyExpr)
  where
    parseOptionalCaseArmGuard = do
      maybeToken <- peekToken
      case maybeToken of
        Just Token {tokenKind = TIf} -> do
          void parseAnyToken
          guardExpr <- parseCaseArmGuard parseBlock context neverStop Nothing 1
          consumeArrow (ParserEndOfInputAfter "case guard")
          pure (Just guardExpr)
        _ -> do
          consumeArrow (ParserEndOfInputAfter "case pattern")
          pure Nothing

parseCaseArmGuard ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Maybe Text ->
  Int ->
  Parser SurfaceExpr
parseCaseArmGuard parseBlock context rhsStop parentOperator minPrecedence = do
  tokens <- MP.getInput
  case tokens of
    EmptyTokens -> failTokenParser (ExpectedSyntax "guard expression" (ParserEndOfInputAfter "'if'"))
    Token {tokenKind = TArrow} :< _ ->
      failTokenParser (ExpectedSyntax "guard expression" (ParserBeforeToken TArrow "->" Nothing))
    Token {tokenKind = TRBrace} :< _ ->
      failTokenParser (ExpectedSyntax "guard expression" (ParserBeforeToken TRBrace "}" Nothing))
    Token {tokenKind = TOperator "|"} :< _ ->
      failTokenParser (ExpectedSyntax "guard expression" (ParserBeforeBoundary "next case arm"))
    _ -> do
      leftExpr <-
        parseApplicationExprUntil
          parseBlock
          context
          (stopsBeforeCaseGuardTerminatorOr rhsStop)
      parseCaseGuardInfixTail parseBlock context rhsStop parentOperator minPrecedence leftExpr

parseCaseArmBodyExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Maybe Text ->
  Int ->
  Parser SurfaceExpr
parseCaseArmBodyExpr parseBlock context rhsStop parentOperator minPrecedence = do
  leftExpr <-
    parseApplicationExprUntil
      parseBlock
      context
      (stopsBeforeCaseArmBoundaryOr rhsStop)
  parseCaseArmBodyInfixTail parseBlock context rhsStop parentOperator minPrecedence leftExpr

parseCaseArmBodyInfixTail ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Maybe Text ->
  Int ->
  SurfaceExpr ->
  Parser SurfaceExpr
parseCaseArmBodyInfixTail parseBlock context rhsStop parentOperator minPrecedence leftExpr = do
  tokens <- MP.getInput
  if stopsBeforeCaseArmTerminator tokens || rhsStop tokens
    then pure leftExpr
    else case tokens of
      operatorToken@Token {tokenKind = TOperator symbol} :< tokensAfterOperator
        | startsRightParen tokensAfterOperator -> pure leftExpr
        | symbol == "|",
          caseArmPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterOperator ->
            pure leftExpr
        | otherwise ->
            case lookupOperatorInfoIn (parserDeclaredOperators context) symbol of
              Nothing -> failUndeclaredOperator operatorToken symbol
              Just operatorInfo
                | operatorPrecedence operatorInfo < minPrecedence -> pure leftExpr
                | otherwise -> do
                    void parseAnyToken
                    let nextStop =
                          samePrecedenceNonAssociativeRhsStop
                            (parserDeclaredOperators context)
                            operatorInfo
                            rhsStop
                    rightExpr <-
                      parseCaseArmBodyExpr
                        parseBlock
                        context
                        nextStop
                        (Just symbol)
                        (operatorNextMinPrecedence operatorInfo)
                    rejectNonAssociativeContinuation context operatorInfo operatorToken
                    parseCaseArmBodyInfixTail
                      parseBlock
                      context
                      rhsStop
                      parentOperator
                      minPrecedence
                      (SEBinary symbol leftExpr rightExpr)
      _ -> pure leftExpr

parseCaseGuardInfixTail ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Maybe Text ->
  Int ->
  SurfaceExpr ->
  Parser SurfaceExpr
parseCaseGuardInfixTail parseBlock context rhsStop parentOperator minPrecedence leftExpr = do
  tokens <- MP.getInput
  if stopsBeforeCaseGuardTerminator tokens || rhsStop tokens
    then pure leftExpr
    else case tokens of
      operatorToken@Token {tokenKind = TOperator symbol} :< tokensAfterOperator
        | startsRightParen tokensAfterOperator -> pure leftExpr
        | symbol == "|",
          caseGuardPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterOperator ->
            pure leftExpr
        | otherwise ->
            case lookupOperatorInfoIn (parserDeclaredOperators context) symbol of
              Nothing -> failUndeclaredOperator operatorToken symbol
              Just operatorInfo
                | operatorPrecedence operatorInfo < minPrecedence -> pure leftExpr
                | otherwise -> do
                    void parseAnyToken
                    let nextStop =
                          samePrecedenceNonAssociativeRhsStop
                            (parserDeclaredOperators context)
                            operatorInfo
                            rhsStop
                    rightExpr <-
                      parseCaseArmGuard
                        parseBlock
                        context
                        nextStop
                        (Just symbol)
                        (operatorNextMinPrecedence operatorInfo)
                    rejectNonAssociativeContinuation context operatorInfo operatorToken
                    parseCaseGuardInfixTail
                      parseBlock
                      context
                      rhsStop
                      parentOperator
                      minPrecedence
                      (SEBinary symbol leftExpr rightExpr)
      _ -> pure leftExpr

stopsBeforeCaseArmTerminator :: Stop
stopsBeforeCaseArmTerminator tokens =
  case tokens of
    Token {tokenKind = TRBrace} :< _ -> True
    _ -> False

stopsBeforeCaseGuardTerminator :: Stop
stopsBeforeCaseGuardTerminator tokens =
  case tokens of
    Token {tokenKind = TArrow} :< _ -> True
    Token {tokenKind = TRBrace} :< _ -> True
    _ -> False

stopsBeforeCaseGuardTerminatorOr :: Stop -> Stop
stopsBeforeCaseGuardTerminatorOr rhsStop tokens =
  rhsStop tokens || stopsBeforeCaseGuardTerminator tokens

stopsBeforeCaseArmBoundaryOr :: Stop -> Stop
stopsBeforeCaseArmBoundaryOr rhsStop tokens =
  rhsStop tokens || stopsBeforeCaseArmBoundary tokens

stopsBeforeCaseArmBoundary :: Stop
stopsBeforeCaseArmBoundary tokens =
  case tokens of
    Token {tokenKind = TOperator "|"} :< rest -> startsDefiniteCaseArm rest
    Token {tokenKind = TRBrace} :< _ -> True
    _ -> False

caseArmPipeStartsBoundary ::
  ParserContext ->
  Maybe Text ->
  Int ->
  SurfaceExpr ->
  TokenStream ->
  Bool
caseArmPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterPipe =
  case Pattern.parseCasePatternTokenStream tokensAfterPipe of
    Right (_, Token {tokenKind = TArrow} :< _) -> True
    Right (_, Token {tokenKind = TIf} :< afterGuard) -> guardTokensEndAtArrow afterGuard
    Right (_, Token {tokenKind = TOperator "|"} :< _) ->
      startsDefiniteOrPatternCaseArm tokensAfterPipe
        && not
          ( startsAllLiteralOrPatternCaseArm tokensAfterPipe
              && casePipeCanContinueExpression context parentOperator minPrecedence leftExpr
          )
    Left _
      | startsCasePatternTokens tokensAfterPipe ->
          hasTopLevelArrowBeforeCaseArmBoundary tokensAfterPipe
    _ -> False

caseGuardPipeStartsBoundary ::
  ParserContext ->
  Maybe Text ->
  Int ->
  SurfaceExpr ->
  TokenStream ->
  Bool
caseGuardPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterPipe =
  startsDefiniteGuardedCaseArmAfterGuardBoundary tokensAfterPipe
    || ( startsDefiniteUnguardedCaseArmAfterGuardBoundary tokensAfterPipe
           && not (casePipeCanContinueExpression context parentOperator minPrecedence leftExpr)
       )

casePipeCanContinueExpression :: ParserContext -> Maybe Text -> Int -> SurfaceExpr -> Bool
casePipeCanContinueExpression context parentOperator minPrecedence leftExpr =
  case compare minPrecedence caseGuardPipePrecedence of
    LT -> not (leftExprHasBoundaryPrecedenceRoot leftExpr)
    EQ -> samePrecedenceGuardPipeCanBind parentOperator leftExpr
    GT -> False
  where
    caseGuardPipePrecedence =
      maybe 0 operatorPrecedence (lookupOperatorInfoIn (parserDeclaredOperators context) "|")
    leftExprHasBoundaryPrecedenceRoot expression =
      case expression of
        SEBinary operatorSymbol' _ _ ->
          maybe
            False
            ((<= caseGuardPipePrecedence) . operatorPrecedence)
            (lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol')
        _ -> False
    samePrecedenceGuardPipeCanBind parent expression =
      case expression of
        SELit {} -> parentOperatorAllowsLiteralPipe parent
        _ -> True
    parentOperatorAllowsLiteralPipe parent =
      case parent of
        Just operatorSymbol' ->
          maybe
            False
            ((< caseGuardPipePrecedence) . operatorPrecedence)
            (lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol')
        Nothing -> False

startsDefiniteCaseArm :: TokenStream -> Bool
startsDefiniteCaseArm remainingTokens =
  case Pattern.parseCasePatternTokenStream remainingTokens of
    Right (_, Token {tokenKind = TArrow} :< _) -> True
    Right (_, Token {tokenKind = TIf} :< afterGuard) ->
      hasTopLevelGuardArrow afterGuard
    Right (_, Token {tokenKind = TOperator "|"} :< _) ->
      case Pattern.parseCaseArmPatternTokenStream remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} :< _) ->
          orPatternStartsDefiniteArmBoundary casePattern
        Right (casePattern, Token {tokenKind = TIf} :< afterGuard) ->
          orPatternStartsDefiniteArmBoundary casePattern && hasTopLevelGuardArrow afterGuard
        _ -> False
    Left _
      | startsCasePatternTokens remainingTokens ->
          hasTopLevelArrowBeforeCaseArmBoundary remainingTokens
    _ -> False

startsDefiniteUnguardedCaseArmAfterGuardBoundary :: TokenStream -> Bool
startsDefiniteUnguardedCaseArmAfterGuardBoundary remainingTokens =
  case Pattern.parseCaseArmPatternTokenStream remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} :< _) ->
      guardBoundaryPatternIsDefinite casePattern
    _ -> False

startsDefiniteGuardedCaseArmAfterGuardBoundary :: TokenStream -> Bool
startsDefiniteGuardedCaseArmAfterGuardBoundary remainingTokens =
  case Pattern.parseCaseArmPatternTokenStream remainingTokens of
    Right (casePattern, Token {tokenKind = TIf} :< afterGuard) ->
      guardBoundaryPatternIsDefinite casePattern && guardTokensEndAtArrow afterGuard
    _ -> False

guardTokensEndAtArrow :: TokenStream -> Bool
guardTokensEndAtArrow tokens =
  hasTopLevelGuardArrow tokens
    && not (hasTopLevelElseBeforeArrow tokens)

-- A top-level `else` before the arrow means the preceding `if` belongs to the
-- expression on the left of the pipe. Treating the constructor-shaped prefix
-- as a guarded arm would split that valid expression too early.
hasTopLevelElseBeforeArrow :: TokenStream -> Bool
hasTopLevelElseBeforeArrow =
  hasTopLevelTokenBefore isElse isArrow
  where
    isElse tokenKind' = tokenKind' == TElse
    isArrow tokenKind' = tokenKind' == TArrow

guardBoundaryPatternIsDefinite :: SurfacePattern -> Bool
guardBoundaryPatternIsDefinite casePattern =
  case casePattern of
    SPVariable {} -> False
    _ -> True

startsDefiniteOrPatternCaseArm :: TokenStream -> Bool
startsDefiniteOrPatternCaseArm remainingTokens =
  case Pattern.parseCaseArmPatternTokenStream remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} :< _) ->
      orPatternStartsDefiniteArmBoundary casePattern
    Right (casePattern, Token {tokenKind = TIf} :< afterGuard) ->
      orPatternStartsDefiniteArmBoundary casePattern && guardTokensEndAtArrow afterGuard
    _ -> False

startsAllLiteralOrPatternCaseArm :: TokenStream -> Bool
startsAllLiteralOrPatternCaseArm remainingTokens =
  case Pattern.parseCaseArmPatternTokenStream remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} :< _) ->
      orPatternIsAllLiteral casePattern
    Right (casePattern, Token {tokenKind = TIf} :< afterGuard) ->
      orPatternIsAllLiteral casePattern && guardTokensEndAtArrow afterGuard
    _ -> False

orPatternStartsDefiniteArmBoundary :: SurfacePattern -> Bool
orPatternStartsDefiniteArmBoundary casePattern =
  case casePattern of
    SPOr alternatives ->
      any patternIsWildcard alternatives
        || all patternIsVariable alternatives
        || all patternIsLiteral alternatives
        || alternativesBindSameNames alternatives
        || case alternatives of
          firstAlternative : _ -> patternCanStartOrArmBoundary firstAlternative
          [] -> False
    _ -> False

orPatternIsAllLiteral :: SurfacePattern -> Bool
orPatternIsAllLiteral casePattern =
  case casePattern of
    SPOr alternatives -> not (null alternatives) && all patternIsLiteral alternatives
    _ -> False

patternCanStartOrArmBoundary :: SurfacePattern -> Bool
patternCanStartOrArmBoundary casePattern =
  case casePattern of
    SPConstructor {} -> True
    SPList {} -> True
    SPConsList {} -> True
    SPTuple {} -> True
    SPAs {} -> True
    _ -> False

patternIsWildcard :: SurfacePattern -> Bool
patternIsWildcard SPWildcard = True
patternIsWildcard _ = False

patternIsVariable :: SurfacePattern -> Bool
patternIsVariable SPVariable {} = True
patternIsVariable _ = False

patternIsLiteral :: SurfacePattern -> Bool
patternIsLiteral SPLiteral {} = True
patternIsLiteral _ = False

alternativesBindSameNames :: [SurfacePattern] -> Bool
alternativesBindSameNames alternatives =
  case alternatives of
    [] -> False
    firstAlternative : rest ->
      let expectedNames = patternBinderNames firstAlternative
       in not (Set.null expectedNames)
            && all ((== expectedNames) . patternBinderNames) rest

patternBinderNames :: SurfacePattern -> Set.Set Text
patternBinderNames casePattern =
  case casePattern of
    SPVariable name -> Set.singleton (identifierText name)
    SPWildcard -> Set.empty
    SPLiteral {} -> Set.empty
    SPConstructor _ patterns -> Set.unions (map patternBinderNames patterns)
    SPList patterns -> Set.unions (map patternBinderNames patterns)
    SPConsList headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    SPTuple patterns -> Set.unions (map patternBinderNames patterns)
    SPAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinderNames nestedPattern)
    SPOr patterns -> commonPatternBinderNames patterns

commonPatternBinderNames :: [SurfacePattern] -> Set.Set Text
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

startsCasePatternTokens :: TokenStream -> Bool
startsCasePatternTokens tokens =
  case tokens of
    Token {tokenKind = TInt _} :< _ -> True
    Token {tokenKind = TIdentifier _} :< _ -> True
    Token {tokenKind = TLBracket} :< _ -> True
    Token {tokenKind = TLParen} :< _ -> True
    _ -> False

parseLambdaExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Token ->
  Parser SurfaceExpr
parseLambdaExpr parseBlock context stop lambdaToken = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TOperator "|"} :< _ ->
      parsePatternLambdaExpr parseBlock context stop lambdaToken
    Token {tokenKind = TLParen} :< _ -> do
      void parseAnyToken
      parameters <- parseLambdaParameters
      tokensAfterParameters <- MP.getInput
      case tokensAfterParameters of
        Token {tokenKind = TArrow} :< _ -> do
          void parseAnyToken
          bodyExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
          pure (SELambda parameters bodyExpr)
        EmptyTokens ->
          failTokenParserAt
            (tokenSpan lambdaToken)
            (ExpectedSyntax "'->'" (ParserEndOfInputAfter "lambda parameters"))
        token :< _ ->
          failTokenParserAt
            (tokenSpan token)
            (ExpectedSyntax "'->'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
    EmptyTokens ->
      failTokenParserAt
        (tokenSpan lambdaToken)
        (ExpectedSyntax "'('" (ParserEndOfInputAfter "lambda introducer"))
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'(' after lambda introducer" (ParserAtToken (tokenKind token) (tokenLexeme token)))

parsePatternLambdaExpr ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Token ->
  Parser SurfaceExpr
parsePatternLambdaExpr parseBlock context stop lambdaToken = do
  firstClause@(SurfacePatternLambdaClause _ firstPatterns _) <-
    parsePatternLambdaClause parseBlock context stop lambdaToken
  collect (NonEmpty.length firstPatterns) firstClause []
  where
    collect expectedArity firstClause reversedRemaining = do
      tokens <- MP.getInput
      if patternLambdaClauseStarts tokens
        then do
          nextClause@(SurfacePatternLambdaClause clauseSpan patterns _) <-
            parsePatternLambdaClause parseBlock context stop lambdaToken
          let actualArity = NonEmpty.length patterns
          if actualArity == expectedArity
            then collect expectedArity firstClause (nextClause : reversedRemaining)
            else
              failTokenParserAt
                clauseSpan
                (PatternFailure (PatternLambdaClauseArityMismatch expectedArity actualArity))
        else
          pure (SEPatternLambda (firstClause :| reverse reversedRemaining))

parsePatternLambdaClause ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Token ->
  Parser SurfacePatternLambdaClause
parsePatternLambdaClause parseBlock context stop lambdaToken = do
  clauseToken <- parsePatternLambdaClausePipe lambdaToken
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TLParen} :< _ -> void parseAnyToken
    EmptyTokens ->
      failTokenParserAt
        (tokenSpan clauseToken)
        (ExpectedSyntax "'('" (ParserEndOfInputAfter "pattern-lambda clause introducer"))
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'('" (ParserAtToken (tokenKind token) (tokenLexeme token)))
  parameters <- parseLambdaParameters
  consumeArrow (ParserEndOfInputAfter "pattern-lambda clause head")
  bodyExpr <-
    parseExprWithMinPrecedenceUntil
      parseBlock
      context
      (patternLambdaClauseBoundaryOr stop)
      1
  pure
    ( SurfacePatternLambdaClause
        (tokenSpan clauseToken)
        (fmap surfaceLambdaParameterPattern parameters)
        bodyExpr
    )

parsePatternLambdaClausePipe :: Token -> Parser Token
parsePatternLambdaClausePipe lambdaToken = do
  tokens <- MP.getInput
  case tokens of
    token@Token {tokenKind = TOperator "|"} :< _ -> do
      void parseAnyToken
      pure token
    EmptyTokens ->
      failTokenParserAt
        (tokenSpan lambdaToken)
        (ExpectedSyntax "'|'" (ParserEndOfInputAfter "pattern-lambda introducer"))
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'|'" (ParserAtToken (tokenKind token) (tokenLexeme token)))

surfaceLambdaParameterPattern :: SurfaceLambdaParameter -> SurfacePattern
surfaceLambdaParameterPattern parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> SPVariable name
    SurfaceLambdaPattern patternValue -> patternValue

patternLambdaClauseBoundaryOr :: Stop -> Stop
patternLambdaClauseBoundaryOr stop tokens =
  stop tokens || patternLambdaClauseStarts tokens

patternLambdaClauseStarts :: Stop
patternLambdaClauseStarts tokens =
  case tokens of
    Token {tokenKind = TOperator "|"} :< rest ->
      parenthesizedHeadEndsAtArrow rest
    _ -> False

parenthesizedHeadEndsAtArrow :: TokenStream -> Bool
parenthesizedHeadEndsAtArrow tokens =
  case tokens of
    Token {tokenKind = TLParen} :< rest -> go 1 rest
    _ -> False
  where
    go :: Int -> TokenStream -> Bool
    go _ EmptyTokens = False
    go depth (token :< rest) =
      case tokenKind token of
        TLParen -> go (depth + 1) rest
        TRParen
          | depth == 1 ->
              case rest of
                Token {tokenKind = TArrow} :< _ -> True
                _ -> False
          | otherwise -> go (depth - 1) rest
        _ -> go depth rest

parseLambdaParameters :: Parser (NonEmpty SurfaceLambdaParameter)
parseLambdaParameters = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRParen} -> do
      void parseAnyToken
      pure (SurfaceLambdaPattern (SPTuple []) :| [])
    _ -> do
      firstParameter <- Pattern.parseLambdaParameterParser
      collect firstParameter []
  where
    collect firstParameter reversedRemaining = do
      tokens <- MP.getInput
      case tokens of
        Token {tokenKind = TComma} :< _ -> do
          void parseAnyToken
          nextParameter <- Pattern.parseLambdaParameterParser
          collect firstParameter (nextParameter : reversedRemaining)
        Token {tokenKind = TRParen} :< _ -> do
          void parseAnyToken
          pure (firstParameter :| reverse reversedRemaining)
        EmptyTokens ->
          failTokenParser (ExpectedSyntax "')'" (ParserEndOfInputIn "lambda parameter list"))
        token :< _ ->
          failTokenParserAt
            (tokenSpan token)
            (ExpectedSyntax "',' or ')'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

consumeArrow :: ParserEncountered -> Parser ()
consumeArrow endOfInputEncountered = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TArrow} :< _ -> void parseAnyToken
    EmptyTokens -> failTokenParser (ExpectedSyntax "'->'" endOfInputEncountered)
    token :< _ ->
      failTokenParserAt
        (tokenSpan token)
        (ExpectedSyntax "'->'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))

startsRightParen :: TokenStream -> Bool
startsRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} :< _ -> True
    _ -> False

hasTopLevelArrowBeforeTerminator :: TokenStream -> Bool
hasTopLevelArrowBeforeTerminator =
  hasTopLevelTokenBefore isArrow isTerminator
  where
    isArrow tokenKind' = tokenKind' == TArrow
    isTerminator tokenKind' =
      case tokenKind' of
        TDot -> True
        TRBrace -> True
        _ -> False

hasTopLevelGuardArrow :: TokenStream -> Bool
hasTopLevelGuardArrow = hasTopLevelArrowBeforeTerminator

hasTopLevelArrowBeforeCaseBodyEnd :: TokenStream -> Bool
hasTopLevelArrowBeforeCaseBodyEnd =
  hasTopLevelTokenBefore isArrow isRightBrace
  where
    isArrow tokenKind' = tokenKind' == TArrow
    isRightBrace tokenKind' = tokenKind' == TRBrace

hasTopLevelArrowBeforeCaseArmBoundary :: TokenStream -> Bool
hasTopLevelArrowBeforeCaseArmBoundary =
  hasTopLevelTokenBefore isArrow isCaseArmBoundary
  where
    isArrow tokenKind' = tokenKind' == TArrow
    isCaseArmBoundary tokenKind' =
      case tokenKind' of
        TOperator "|" -> True
        TRBrace -> True
        _ -> False

hasTopLevelTokenBefore :: (TokenKind -> Bool) -> (TokenKind -> Bool) -> TokenStream -> Bool
hasTopLevelTokenBefore isTarget isTerminator = go 0 0 0
  where
    go :: Int -> Int -> Int -> TokenStream -> Bool
    go parenDepth braceDepth bracketDepth tokens =
      case tokens of
        EmptyTokens -> False
        Token {tokenKind = tokenKind'} :< rest
          | atTopLevel && isTarget tokenKind' -> True
          | atTopLevel && isTerminator tokenKind' -> False
          | otherwise ->
              case tokenKind' of
                TLParen -> go (parenDepth + 1) braceDepth bracketDepth rest
                TRParen -> go (decrement parenDepth) braceDepth bracketDepth rest
                TLBrace -> go parenDepth (braceDepth + 1) bracketDepth rest
                TRBrace -> go parenDepth (decrement braceDepth) bracketDepth rest
                TLBracket -> go parenDepth braceDepth (bracketDepth + 1) rest
                TRBracket -> go parenDepth braceDepth (decrement bracketDepth) rest
                _ -> go parenDepth braceDepth bracketDepth rest
          where
            atTopLevel = parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

    decrement depth = max 0 (depth - 1)
