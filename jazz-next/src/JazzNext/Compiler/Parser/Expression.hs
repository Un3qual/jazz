{-# LANGUAGE OverloadedStrings #-}

-- | Direct Megaparsec grammar for surface expressions.
module JazzNext.Compiler.Parser.Expression
  ( parseExpressionParser,
    parseExpressionTokens
  ) where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderSourceSpan
  )
import JazzNext.Compiler.FractionalLiteral
  ( fractionalLiteralExceedsMagnitude,
    mkFractionalLiteralSource
  )
import JazzNext.Compiler.Identifier
  ( identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfaceSignatureType
  )
import JazzNext.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    lookupOperatorInfoIn
  )
import qualified JazzNext.Compiler.Parser.Pattern as Pattern
import JazzNext.Compiler.Parser.Signature (parseSignatureTypeParser)
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    parseAnyToken,
    parseToken,
    peekToken,
    runTokenParserPrefix
  )
import qualified Text.Megaparsec as MP

type Stop = [Token] -> Bool

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

parseExprWithoutApplicationUntil ::
  StatementBlockParser ->
  ParserContext ->
  Stop ->
  Int ->
  Parser SurfaceExpr
parseExprWithoutApplicationUntil parseBlock context stop minPrecedence = do
  leftExpr <- parsePrimaryExpr parseBlock context stop
  parseInfixTailWithUntil
    context
    stop
    (parseExprWithoutApplicationUntil parseBlock context)
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
    else
      case tokens of
        typeApplicationToken@Token {tokenKind = TAt} : _ -> do
          void parseAnyToken
          typeArgument <- parseTypeApplicationArgument typeApplicationToken
          parseApplicationTailUntil
            parseBlock
            context
            stop
            (SETypeApplication functionExpr typeArgument)
        firstToken : _
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
      failTokenParser
        ( "unsupported explicit type application argument after '@' at "
            <> renderSourceSpan (tokenSpan typeApplicationToken)
        )

neverStop :: Stop
neverStop _ = False

startsPrimaryExpr :: Token -> Bool
startsPrimaryExpr token =
  case tokenKind token of
    TInt _ -> True
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
    else
      case tokens of
        operatorToken@Token {tokenKind = TOperator operatorSymbol} : tokensAfterOperator
          | startsRightParen tokensAfterOperator ->
              pure leftExpr
          | otherwise ->
              case lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol of
                Nothing ->
                  failUndeclaredOperator operatorToken operatorSymbol
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
                        (SEBinary operatorSymbol leftExpr rightExpr)
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
              failTokenParser
                ( "non-associative operator '"
                    <> nonAssociativeSymbol nextInfo
                    <> "' cannot be chained without parentheses at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
        _ -> pure ()
    _ -> pure ()
  where
    nonAssociativeSymbol nextInfo
      | operatorAssociativity operatorInfo == AssocNonAssoc = operatorSymbol operatorInfo
      | otherwise = operatorSymbol nextInfo

samePrecedenceNonAssociativeRhsStop ::
  [OperatorInfo] ->
  OperatorInfo ->
  Stop ->
  Stop
samePrecedenceNonAssociativeRhsStop declaredOperators operatorInfo stop tokens =
  stop tokens
    || case tokens of
      Token {tokenKind = TOperator nextSymbol} : _ ->
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
      failTokenParser "expected expression before end of input"
    Just token -> do
      void parseAnyToken
      case tokenKind token of
        TInt value ->
          SELit <$> parseNumericSurfaceLiteral token value
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
          failTokenParser
            ( "unexpected token '"
                <> tokenLexeme token
                <> "' at "
                <> renderSourceSpan (tokenSpan token)
                <> "; expected expression"
            )

parseIdentifierExpr :: Token -> Text -> Parser SurfaceExpr
parseIdentifierExpr identifierToken name = do
  tokens <- MP.getInput
  case tokens of
    colonToken@Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier memberName} : _
      | isImmediatelyAfter identifierToken colonToken -> do
          void parseAnyToken
          void parseAnyToken
          pure (SEQualifiedVar (mkIdentifier name) (mkIdentifier memberName))
    colonToken@Token {tokenKind = TColonColon} : []
      | isImmediatelyAfter identifierToken colonToken ->
          failTokenParser "expected member name after '::' before end of input"
    colonToken@Token {tokenKind = TColonColon} : memberToken : _
      | isImmediatelyAfter identifierToken colonToken ->
          failTokenParser
            ( "expected member name after '::' at "
                <> renderSourceSpan (tokenSpan memberToken)
                <> ", found '"
                <> tokenLexeme memberToken
                <> "'"
            )
    _ ->
      pure (SEVar (mkIdentifier name))

parseNumericSurfaceLiteral :: Token -> Integer -> Parser SurfaceLiteral
parseNumericSurfaceLiteral wholeToken wholeValue = do
  tokens <- MP.getInput
  case tokens of
    dotToken@Token {tokenKind = TDot} : fractionalToken@Token {tokenKind = TInt fractionalValue} : _
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
            either failTokenParser pure
              (parseFloatLiteral (tokenSpan wholeToken) literalText)
          if fractionalLiteralExceedsMagnitude literalSource float64MaxFinite
            then failTokenParser (invalidFloatLiteralMessage (tokenSpan wholeToken) literalText)
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

parseFloatLiteral :: SourceSpan -> Text -> Either Text Double
parseFloatLiteral literalSpan literalText =
  case TextRead.double literalText of
    Right (value, trailing)
      | Text.null trailing,
        finiteFloat value -> Right value
    _ -> Left (invalidFloatLiteralMessage literalSpan literalText)

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

float64MaxFinite :: Double
float64MaxFinite =
  encodeFloat
    (floatRadix sample ^ floatDigits sample - 1)
    (snd (floatRange sample) - floatDigits sample)
  where
    sample = 0 :: Double

invalidFloatLiteralMessage :: SourceSpan -> Text -> Text
invalidFloatLiteralMessage literalSpan literalText =
  "invalid fractional literal '"
    <> literalText
    <> "' at "
    <> renderSourceSpan literalSpan

requireOperatorVisible :: ParserContext -> Token -> Parser ()
requireOperatorVisible context operatorToken =
  case tokenKind operatorToken of
    TOperator operatorSymbol ->
      case lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol of
        Just _ -> pure ()
        Nothing -> failUndeclaredOperator operatorToken operatorSymbol
    _ ->
      failTokenParser
        ( "internal parser error at "
            <> renderSourceSpan (tokenSpan operatorToken)
            <> ": expected operator token"
        )

failUndeclaredOperator :: Token -> Text -> Parser a
failUndeclaredOperator operatorToken operatorSymbol =
  failTokenParser
    ( "operator '"
        <> operatorSymbol
        <> "' must be declared before use at "
        <> renderSourceSpan (tokenSpan operatorToken)
    )

parseParenExpr :: StatementBlockParser -> ParserContext -> Parser SurfaceExpr
parseParenExpr parseBlock context = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TRParen} : _ -> do
      void parseAnyToken
      pure (SETuple [])
    operatorToken@Token {tokenKind = TOperator operatorSymbol} : rest -> do
      void parseAnyToken
      requireOperatorVisible context operatorToken
      case rest of
        Token {tokenKind = TRParen} : _ -> do
          void parseAnyToken
          pure (SEOperatorValue operatorSymbol)
        _ -> do
          rightExpr <- parseExpressionParser parseBlock context
          void (parseToken TRParen)
          pure (SESectionRight operatorSymbol rightExpr)
    _ -> do
      innerExpr <- parseExpressionParser parseBlock context
      afterInner <- MP.getInput
      case afterInner of
        Token {tokenKind = TComma} : _ -> do
          void parseAnyToken
          tupleElements <- parseTupleElements parseBlock context [innerExpr]
          void (parseToken TRParen)
          pure (SETuple tupleElements)
        operatorToken@Token {tokenKind = TOperator operatorSymbol} : Token {tokenKind = TRParen} : _ -> do
          void parseAnyToken
          void parseAnyToken
          requireOperatorVisible context operatorToken
          pure (SESectionLeft innerExpr operatorSymbol)
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
  conditionExpr <- parseExprWithoutApplicationUntil parseBlock context stop 1
  thenExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
  maybeElse <- peekToken
  case maybeElse of
    Just Token {tokenKind = TElse} -> do
      void parseAnyToken
      elseExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
      pure (SEIf conditionExpr thenExpr elseExpr)
    Nothing ->
      failTokenParser
        ( "expected 'else' before end of input after 'if' at "
            <> renderSourceSpan (tokenSpan ifToken)
        )
    Just token ->
      failTokenParser
        ( "expected 'else' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

parseCaseExpr :: StatementBlockParser -> ParserContext -> Token -> Parser SurfaceExpr
parseCaseExpr parseBlock context caseToken = do
  scrutineeExpr <-
    parseExprWithMinPrecedenceUntil parseBlock context caseBodyStarts 1
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TLBrace} : _ -> do
      void parseAnyToken
      caseArms <- parseCaseArms parseBlock context
      pure (SECase scrutineeExpr caseArms)
    _ ->
      failTokenParser
        ( "expected '{' before end of input after 'case' at "
            <> renderSourceSpan (tokenSpan caseToken)
        )

caseBodyStarts :: Stop
caseBodyStarts tokens =
  case tokens of
    Token {tokenKind = TLBrace} : rest ->
      case rest of
        Token {tokenKind = TOperator "|"} : _ -> True
        Token {tokenKind = TRBrace} : _ -> True
        _ -> hasTopLevelArrowBeforeTerminator rest
    _ -> False

parseCaseArms :: StatementBlockParser -> ParserContext -> Parser [SurfaceCaseArm]
parseCaseArms parseBlock context = do
  maybeToken <- peekToken
  case maybeToken of
    Just Token {tokenKind = TRBrace, tokenSpan = rightBraceSpan} ->
      failTokenParser
        ( "expected case arm before '}' at "
            <> renderSourceSpan rightBraceSpan
        )
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
    Token {tokenKind = TOperator "|"} : _ -> void parseAnyToken
    [] -> failTokenParser "expected '|' before end of input in case expression"
    token : _ ->
      failTokenParser
        ( "expected '|' at "
            <> renderSourceSpan (tokenSpan token)
            <> " to start case arm"
        )
  casePattern <- Pattern.parseCaseArmPatternParser
  guardExpr <- parseOptionalCaseArmGuard
  bodyExpr <- parseCaseArmBodyExpr parseBlock context neverStop Nothing 1
  pure (SurfaceCaseArm casePattern guardExpr bodyExpr)
  where
    parseOptionalCaseArmGuard = do
      maybeToken <- peekToken
      case maybeToken of
        Just Token {tokenKind = TIf} -> do
          void parseAnyToken
          guardExpr <- parseCaseArmGuard parseBlock context neverStop Nothing 1
          consumeArrow "expected '->' before end of input after case guard"
          pure (Just guardExpr)
        _ -> do
          consumeArrow "expected '->' before end of input after case pattern"
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
    [] -> failTokenParser "expected guard expression before end of input after 'if'"
    Token {tokenKind = TArrow} : _ -> failTokenParser "expected guard expression before '->'"
    Token {tokenKind = TRBrace} : _ -> failTokenParser "expected guard expression before '}'"
    Token {tokenKind = TOperator "|"} : _ -> failTokenParser "expected guard expression before next case arm"
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
    else
      case tokens of
        operatorToken@Token {tokenKind = TOperator operatorSymbol} : tokensAfterOperator
          | startsRightParen tokensAfterOperator -> pure leftExpr
          | operatorSymbol == "|",
            caseArmPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterOperator ->
              pure leftExpr
          | otherwise ->
              case lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol of
                Nothing -> failUndeclaredOperator operatorToken operatorSymbol
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
                          (Just operatorSymbol)
                          (operatorNextMinPrecedence operatorInfo)
                      rejectNonAssociativeContinuation context operatorInfo operatorToken
                      parseCaseArmBodyInfixTail
                        parseBlock
                        context
                        rhsStop
                        parentOperator
                        minPrecedence
                        (SEBinary operatorSymbol leftExpr rightExpr)
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
    else
      case tokens of
        operatorToken@Token {tokenKind = TOperator operatorSymbol} : tokensAfterOperator
          | startsRightParen tokensAfterOperator -> pure leftExpr
          | operatorSymbol == "|",
            caseGuardPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterOperator ->
              pure leftExpr
          | otherwise ->
              case lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol of
                Nothing -> failUndeclaredOperator operatorToken operatorSymbol
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
                          (Just operatorSymbol)
                          (operatorNextMinPrecedence operatorInfo)
                      rejectNonAssociativeContinuation context operatorInfo operatorToken
                      parseCaseGuardInfixTail
                        parseBlock
                        context
                        rhsStop
                        parentOperator
                        minPrecedence
                        (SEBinary operatorSymbol leftExpr rightExpr)
        _ -> pure leftExpr

stopsBeforeCaseArmTerminator :: Stop
stopsBeforeCaseArmTerminator tokens =
  case tokens of
    Token {tokenKind = TRBrace} : _ -> True
    _ -> False

stopsBeforeCaseGuardTerminator :: Stop
stopsBeforeCaseGuardTerminator tokens =
  case tokens of
    Token {tokenKind = TArrow} : _ -> True
    Token {tokenKind = TRBrace} : _ -> True
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
    Token {tokenKind = TOperator "|"} : rest -> startsDefiniteCaseArm rest
    Token {tokenKind = TRBrace} : _ -> True
    _ -> False

caseArmPipeStartsBoundary ::
  ParserContext ->
  Maybe Text ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Bool
caseArmPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterPipe =
  case Pattern.parseCasePatternTokens tokensAfterPipe of
    Right (_, Token {tokenKind = TArrow} : _) -> True
    Right (_, Token {tokenKind = TIf} : afterGuard) -> guardTokensEndAtArrow context afterGuard
    Right (_, Token {tokenKind = TOperator "|"} : _) ->
      startsDefiniteOrPatternCaseArm context tokensAfterPipe
        && not
          ( startsAllLiteralOrPatternCaseArm context tokensAfterPipe
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
  [Token] ->
  Bool
caseGuardPipeStartsBoundary context parentOperator minPrecedence leftExpr tokensAfterPipe =
  startsDefiniteGuardedCaseArmAfterGuardBoundary context tokensAfterPipe
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
          maybe False ((<= caseGuardPipePrecedence) . operatorPrecedence)
            (lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol')
        _ -> False
    samePrecedenceGuardPipeCanBind parent expression =
      case expression of
        SELit {} -> parentOperatorAllowsLiteralPipe parent
        _ -> True
    parentOperatorAllowsLiteralPipe parent =
      case parent of
        Just operatorSymbol' ->
          maybe False ((< caseGuardPipePrecedence) . operatorPrecedence)
            (lookupOperatorInfoIn (parserDeclaredOperators context) operatorSymbol')
        Nothing -> False

startsDefiniteCaseArm :: [Token] -> Bool
startsDefiniteCaseArm remainingTokens =
  case Pattern.parseCasePatternTokens remainingTokens of
    Right (_, Token {tokenKind = TArrow} : _) -> True
    Right (_, Token {tokenKind = TIf} : afterGuard) ->
      hasTopLevelGuardArrow afterGuard
    Right (_, Token {tokenKind = TOperator "|"} : _) ->
      case Pattern.parseCaseArmPatternTokens remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} : _) ->
          orPatternStartsDefiniteArmBoundary casePattern
        Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
          orPatternStartsDefiniteArmBoundary casePattern && hasTopLevelGuardArrow afterGuard
        _ -> False
    Left _
      | startsCasePatternTokens remainingTokens ->
          hasTopLevelArrowBeforeCaseArmBoundary remainingTokens
    _ -> False

startsDefiniteUnguardedCaseArmAfterGuardBoundary :: [Token] -> Bool
startsDefiniteUnguardedCaseArmAfterGuardBoundary remainingTokens =
  case Pattern.parseCaseArmPatternTokens remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} : _) ->
      guardBoundaryPatternIsDefinite casePattern
    _ -> False

startsDefiniteGuardedCaseArmAfterGuardBoundary :: ParserContext -> [Token] -> Bool
startsDefiniteGuardedCaseArmAfterGuardBoundary context remainingTokens =
  case Pattern.parseCaseArmPatternTokens remainingTokens of
    Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
      guardBoundaryPatternIsDefinite casePattern && guardTokensEndAtArrow context afterGuard
    _ -> False

guardTokensEndAtArrow :: ParserContext -> [Token] -> Bool
guardTokensEndAtArrow _ tokens =
  hasTopLevelGuardArrow tokens
    && not (hasTopLevelElseBeforeArrow tokens)

-- A top-level `else` before the arrow means the preceding `if` belongs to the
-- expression on the left of the pipe. Treating the constructor-shaped prefix
-- as a guarded arm would split that valid expression too early.
hasTopLevelElseBeforeArrow :: [Token] -> Bool
hasTopLevelElseBeforeArrow = go 0 0 0
  where
    go parenDepth braceDepth bracketDepth tokens =
      case tokens of
        [] -> False
        Token {tokenKind = TArrow} : _
          | atTopLevel -> False
        Token {tokenKind = TElse} : _
          | atTopLevel -> True
        Token {tokenKind = TLParen} : rest -> go (parenDepth + 1) braceDepth bracketDepth rest
        Token {tokenKind = TRParen} : rest -> go (decrement parenDepth) braceDepth bracketDepth rest
        Token {tokenKind = TLBrace} : rest -> go parenDepth (braceDepth + 1) bracketDepth rest
        Token {tokenKind = TRBrace} : rest -> go parenDepth (decrement braceDepth) bracketDepth rest
        Token {tokenKind = TLBracket} : rest -> go parenDepth braceDepth (bracketDepth + 1) rest
        Token {tokenKind = TRBracket} : rest -> go parenDepth braceDepth (decrement bracketDepth) rest
        _ : rest -> go parenDepth braceDepth bracketDepth rest
      where
        atTopLevel = parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

    decrement depth = max 0 (depth - 1)

guardBoundaryPatternIsDefinite :: SurfacePattern -> Bool
guardBoundaryPatternIsDefinite casePattern =
  case casePattern of
    SPVariable {} -> False
    _ -> True

startsDefiniteOrPatternCaseArm :: ParserContext -> [Token] -> Bool
startsDefiniteOrPatternCaseArm context remainingTokens =
  case Pattern.parseCaseArmPatternTokens remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} : _) ->
      orPatternStartsDefiniteArmBoundary casePattern
    Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
      orPatternStartsDefiniteArmBoundary casePattern && guardTokensEndAtArrow context afterGuard
    _ -> False

startsAllLiteralOrPatternCaseArm :: ParserContext -> [Token] -> Bool
startsAllLiteralOrPatternCaseArm context remainingTokens =
  case Pattern.parseCaseArmPatternTokens remainingTokens of
    Right (casePattern, Token {tokenKind = TArrow} : _) ->
      orPatternIsAllLiteral casePattern
    Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
      orPatternIsAllLiteral casePattern && guardTokensEndAtArrow context afterGuard
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
      foldl
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

startsCasePatternTokens :: [Token] -> Bool
startsCasePatternTokens tokens =
  case tokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    Token {tokenKind = TLParen} : _ -> True
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
    Token {tokenKind = TLParen} : _ -> do
      void parseAnyToken
      parameters <- parseLambdaParameters
      tokensAfterParameters <- MP.getInput
      case tokensAfterParameters of
        Token {tokenKind = TArrow} : _ -> do
          void parseAnyToken
          bodyExpr <- parseExprWithMinPrecedenceUntil parseBlock context stop 1
          pure (SELambda parameters bodyExpr)
        [] ->
          failTokenParser
            ( "expected '->' before end of input after lambda parameters at "
                <> renderSourceSpan (tokenSpan lambdaToken)
            )
        token : _ ->
          failTokenParser
            ( "expected '->' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
    [] ->
      failTokenParser
        ( "expected '(' before end of input after lambda introducer at "
            <> renderSourceSpan (tokenSpan lambdaToken)
        )
    token : _ ->
      failTokenParser
        ( "expected '(' at "
            <> renderSourceSpan (tokenSpan token)
            <> " after lambda introducer"
        )

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
        Token {tokenKind = TComma} : _ -> do
          void parseAnyToken
          nextParameter <- Pattern.parseLambdaParameterParser
          collect firstParameter (nextParameter : reversedRemaining)
        Token {tokenKind = TRParen} : _ -> do
          void parseAnyToken
          pure (firstParameter :| reverse reversedRemaining)
        [] ->
          failTokenParser "expected ')' before end of input in lambda parameter list"
        token : _ ->
          failTokenParser
            ( "expected ',' or ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )

consumeArrow :: Text -> Parser ()
consumeArrow endOfInputMessage = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TArrow} : _ -> void parseAnyToken
    [] -> failTokenParser endOfInputMessage
    token : _ ->
      failTokenParser
        ( "expected '->' at "
            <> renderSourceSpan (tokenSpan token)
            <> ", found '"
            <> tokenLexeme token
            <> "'"
        )

startsRightParen :: [Token] -> Bool
startsRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} : _ -> True
    _ -> False

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  tokenLine leftToken == tokenLine rightToken
    && tokenColumn rightToken == tokenColumn leftToken + Text.length (tokenLexeme leftToken)
  where
    tokenLine = spanLine . tokenSpan
    tokenColumn = spanColumn . tokenSpan

hasTopLevelArrowBeforeTerminator :: [Token] -> Bool
hasTopLevelArrowBeforeTerminator = go 0 0 0
  where
    go parenDepth braceDepth bracketDepth tokens =
      case tokens of
        [] -> False
        Token {tokenKind = TArrow} : _
          | atTopLevel -> True
        Token {tokenKind = TDot} : _
          | atTopLevel -> False
        Token {tokenKind = TRBrace} : _
          | atTopLevel -> False
        Token {tokenKind = TLParen} : rest -> go (parenDepth + 1) braceDepth bracketDepth rest
        Token {tokenKind = TRParen} : rest -> go (decrement parenDepth) braceDepth bracketDepth rest
        Token {tokenKind = TLBrace} : rest -> go parenDepth (braceDepth + 1) bracketDepth rest
        Token {tokenKind = TRBrace} : rest -> go parenDepth (decrement braceDepth) bracketDepth rest
        Token {tokenKind = TLBracket} : rest -> go parenDepth braceDepth (bracketDepth + 1) rest
        Token {tokenKind = TRBracket} : rest -> go parenDepth braceDepth (decrement bracketDepth) rest
        _ : rest -> go parenDepth braceDepth bracketDepth rest
      where
        atTopLevel = parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

    decrement depth = max 0 (depth - 1)

hasTopLevelGuardArrow :: [Token] -> Bool
hasTopLevelGuardArrow = hasTopLevelArrowBeforeTerminator

hasTopLevelArrowBeforeCaseArmBoundary :: [Token] -> Bool
hasTopLevelArrowBeforeCaseArmBoundary = go 0 0 0
  where
    go parenDepth braceDepth bracketDepth tokens =
      case tokens of
        [] -> False
        Token {tokenKind = TArrow} : _
          | atTopLevel -> True
        Token {tokenKind = TOperator "|"} : _
          | atTopLevel -> False
        Token {tokenKind = TRBrace} : _
          | atTopLevel -> False
        Token {tokenKind = TLParen} : rest -> go (parenDepth + 1) braceDepth bracketDepth rest
        Token {tokenKind = TRParen} : rest -> go (decrement parenDepth) braceDepth bracketDepth rest
        Token {tokenKind = TLBrace} : rest -> go parenDepth (braceDepth + 1) bracketDepth rest
        Token {tokenKind = TRBrace} : rest -> go parenDepth (decrement braceDepth) bracketDepth rest
        Token {tokenKind = TLBracket} : rest -> go parenDepth braceDepth (bracketDepth + 1) rest
        Token {tokenKind = TRBracket} : rest -> go parenDepth braceDepth (decrement bracketDepth) rest
        _ : rest -> go parenDepth braceDepth bracketDepth rest
      where
        atTopLevel = parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

    decrement depth = max 0 (depth - 1)
