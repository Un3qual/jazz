{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Class and implementation declaration grammar.
module Jazz.Compiler.Parser.CapabilityDeclaration
  ( ImplExpressionParser,
    parseCapabilityDeclarationTokensDetailed,
    looksLikeSupportedCapabilityDeclaration,
    rejectReservedAbstractionSyntax,
  )
where

import Data.Char
  ( isLower,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Name
  ( Identifier,
    identifierText,
    mkIdentifier,
    mkQualifiedIdentifier,
    splitQualifiedIdentifierText,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceExpr,
    SurfaceImplMethod (..),
    SurfaceName (..),
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.DeclarationTokens
  ( collectUntilDot,
    consumeDot,
    isConstructorIdentifierText,
    looksLikeAbstractionDeclaration,
    rejectNestedOperatorDeclaration,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserDuplicateNameRole (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    ParserUnsupportedFeature (..),
    parserFailure,
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.Signature
  ( parseConstrainedSignatureTypeDetailed,
    parseSignaturePayloadDetailed,
    splitTopLevelCommaTokensDetailed,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern TypeApplication,
    pattern TypeFunction,
    pattern TypeList,
    pattern TypeName,
    pattern TypeTuple,
    pattern TypeVariable,
  )

type ImplExpressionParser = TokenStream -> Either ParserFailure (SurfaceExpr, TokenStream)

data CapabilityDeclarationBody
  = CapabilityClassBody [SurfaceClassMethodSignature]
  | CapabilityImplBody [SurfaceImplMethod]

capabilityDeclarationKind :: Text -> ParserDeclarationKind
capabilityDeclarationKind declarationKind =
  case declarationKind of
    "impl" -> ImplDeclaration
    _ -> ClassDeclaration

parseCapabilityDeclarationTokensDetailed ::
  ImplExpressionParser ->
  TokenStream ->
  Either ParserFailure (SurfaceStatement, TokenStream)
parseCapabilityDeclarationTokensDetailed parseImplExpression tokens =
  case tokens of
    declarationToken@Token {tokenKind = TIdentifier declarationKind} :< tokensAfterKeyword ->
      case declarationKind of
        "class" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        "impl" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    EmptyTokens ->
      Left
        (parserFailure (ExpectedSyntax "capability declaration" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "capability declaration"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseCapabilityDeclaration ::
  ImplExpressionParser ->
  Text ->
  Token ->
  TokenStream ->
  Either ParserFailure (SurfaceStatement, TokenStream)
parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword = do
  (capabilityName, maybeHeaderArguments, headerRemaining) <-
    parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword
  let headerArguments =
        case maybeHeaderArguments of
          Just arguments -> arguments
          Nothing -> []
  case declarationKind of
    "class" -> do
      classParameters <-
        validateClassHeaderParameters declarationToken maybeHeaderArguments
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityClassBody methodSignatures ->
          Right (SSClass (tokenSpan declarationToken) (surfaceNameIdentifier capabilityName) classParameters methodSignatures, remaining)
        CapabilityImplBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    "impl" -> do
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityImplBody methods ->
          if surfaceConcreteImplArguments headerArguments
            then Right (SSImpl (tokenSpan declarationToken) capabilityName headerArguments methods, remaining)
            else
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (DeclarationFailure ImplRequiresConcreteTarget)
                )
        CapabilityClassBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    _ ->
      rejectReservedAbstractionSyntax declarationToken

parseCapabilityHeaderName :: Text -> Token -> TokenStream -> Either ParserFailure (SurfaceName, Maybe [SurfaceSignatureType], TokenStream)
parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword =
  case tokensAfterKeyword of
    candidateToken@Token {tokenKind = TIdentifier candidateName, tokenSpan = nameSpan} :< rest ->
      case rest of
        qualifierColon@Token {tokenKind = TColonColon} :< classToken@Token {tokenKind = TIdentifier className} :< afterClass
          | isImmediatelyAfter candidateToken qualifierColon,
            isImmediatelyAfter qualifierColon classToken ->
              if declarationKind == "impl"
                then
                  if isConstructorIdentifierText className
                    then parseCapabilityHeaderTail (SurfaceName (mkQualifiedIdentifier candidateName className) (tokenSpan classToken) (Just nameSpan)) afterClass
                    else
                      Left
                        ( parserFailureAt
                            (tokenSpan classToken)
                            ( ExpectedSyntax
                                "uppercase class name after '::'"
                                (ParserFoundToken (TIdentifier className) className)
                            )
                        )
                else rejectQualifiedClassDeclaration qualifierColon
        qualifierColon@Token {tokenKind = TColonColon} :< EmptyTokens
          | isImmediatelyAfter candidateToken qualifierColon ->
              if declarationKind == "impl"
                then
                  Left
                    ( parserFailureAt
                        (tokenSpan qualifierColon)
                        (ExpectedSyntax "class name" (ParserEndOfInputAfter "'::'"))
                    )
                else rejectQualifiedClassDeclaration qualifierColon
        qualifierColon@Token {tokenKind = TColonColon} :< classToken :< _
          | isImmediatelyAfter candidateToken qualifierColon ->
              if declarationKind == "impl"
                then
                  Left
                    ( parserFailureAt
                        (tokenSpan classToken)
                        ( ExpectedSyntax
                            ( case tokenKind classToken of
                                TIdentifier {} -> "adjacent class name after '::'"
                                _ -> "class name after '::'"
                            )
                            (ParserFoundToken (tokenKind classToken) (tokenLexeme classToken))
                        )
                    )
                else rejectQualifiedClassDeclaration qualifierColon
        _
          | isConstructorIdentifierText candidateName ->
              parseCapabilityHeaderTail (SurfaceName (mkIdentifier candidateName) nameSpan Nothing) rest
          | otherwise ->
              Left
                ( parserFailureAt
                    nameSpan
                    ( ExpectedSyntax
                        "uppercase capability name"
                        (ParserFoundToken (TIdentifier candidateName) candidateName)
                    )
                )
    Token {tokenKind = TLBrace} :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            ( ExpectedSyntax
                "capability name"
                ( ParserBeforeToken
                    TLBrace
                    "{"
                    (Just (declarationKind <> " declaration"))
                )
            )
        )
    Token {tokenKind = TDot} :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            ( ExpectedSyntax
                "capability name"
                (ParserBeforeToken TDot "." (Just (declarationKind <> " declaration")))
            )
        )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "capability name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (ExpectedSyntax "capability name" (ParserEndOfInputIn (declarationKind <> " declaration")))
        )
  where
    rejectQualifiedClassDeclaration qualifierColon =
      Left
        ( parserFailureAt
            (tokenSpan qualifierColon)
            ( ExpectedSyntax
                "unqualified class name"
                (ParserFoundToken TColonColon (tokenLexeme qualifierColon))
            )
        )

    parseCapabilityHeaderTail capabilityName tokens =
      case tokens of
        Token {tokenKind = TLParen} :< rest -> do
          (headerArguments, afterHeaderParameters) <- parseParenthesizedCapabilityHeader rest
          requireCapabilityBodyStart capabilityName (Just headerArguments) afterHeaderParameters
        _ -> requireCapabilityBodyStart capabilityName Nothing tokens

    requireCapabilityBodyStart capabilityName headerArguments tokens =
      case tokens of
        Token {tokenKind = TLBrace} :< _ ->
          Right (capabilityName, headerArguments, tokens)
        Token {tokenKind = TDot} :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                ( ExpectedSyntax
                    "'{'"
                    (ParserBeforeToken TDot "." (Just (declarationKind <> " declaration")))
                )
            )
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                ( UnexpectedSyntaxIn
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                    (declarationKind <> " declaration header")
                )
            )
        EmptyTokens ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (ExpectedSyntax "'{'" (ParserEndOfInputIn (declarationKind <> " declaration")))
            )

    parseParenthesizedCapabilityHeader tokens = do
      (argumentTokens, remaining) <- collectParenthesizedCapabilityHeader tokens
      headerArguments <-
        if null argumentTokens
          then Right []
          else case splitTopLevelCommaTokensDetailed argumentTokens >>= traverse parseConstrainedSignatureTypeDetailed of
            Right parsedArguments -> Right parsedArguments
            Left _ ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (UnsupportedSyntax (DeclarationHeaderArguments (capabilityDeclarationKind declarationKind)))
                )
      Right (headerArguments, remaining)

    collectParenthesizedCapabilityHeader tokens =
      go (1 :: Int) [] tokens
      where
        go depth acc remaining =
          case remaining of
            token@Token {tokenKind = TLParen} :< rest ->
              go (depth + 1) (token : acc) rest
            token@Token {tokenKind = TRParen} :< rest
              | depth == 1 -> Right (reverse acc, rest)
              | otherwise -> go (depth - 1) (token : acc) rest
            Token {tokenKind = TLBrace, tokenSpan = braceSpan} :< _ ->
              Left
                ( parserFailureAt
                    braceSpan
                    ( ExpectedSyntax
                        "')'"
                        ( ParserBeforeToken
                            TLBrace
                            "{"
                            (Just (declarationKind <> " declaration header"))
                        )
                    )
                )
            token :< rest ->
              go depth (token : acc) rest
            EmptyTokens ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (ExpectedSyntax "')'" (ParserEndOfInputIn (declarationKind <> " declaration header")))
                )

parseCapabilityDeclarationBody ::
  ImplExpressionParser ->
  Text ->
  Token ->
  TokenStream ->
  Either ParserFailure (CapabilityDeclarationBody, TokenStream)
parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken tokens =
  case tokens of
    Token {tokenKind = TLBrace} :< rest ->
      case declarationKind of
        "class" -> do
          (methodSignatures, afterBody) <- consumeClassBody Set.empty [] rest
          Right (CapabilityClassBody methodSignatures, afterBody)
        "impl" -> do
          (methods, afterBody) <- consumeImplBody Set.empty [] rest
          Right (CapabilityImplBody methods, afterBody)
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (ExpectedSyntax "'{'" (ParserEndOfInputIn (declarationKind <> " declaration")))
        )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'{'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )
  where
    consumeClassBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        EmptyTokens ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (ExpectedSyntax "'}'" (ParserEndOfInputIn (declarationKind <> " declaration")))
            )
        Token {tokenKind = TRBrace} :< rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} :< _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} :< Token {tokenKind = TColonColon} :< rest
          | Set.member methodName seenMethodNames ->
              Left
                ( parserFailureAt
                    methodSpan
                    (DeclarationFailure (DuplicateName ClassMethodName methodName ClassDeclaration))
                )
          | otherwise -> do
              (signatureTokens, afterSignature) <-
                collectUntilDot rest
              payload <- parseSignaturePayloadDetailed signatureTokens
              let methodSignature =
                    SurfaceClassMethodSignature
                      (mkIdentifier methodName)
                      (tokenSpan methodToken)
                      payload
              consumeClassBody
                (Set.insert methodName seenMethodNames)
                (methodSignature : reversedMethods)
                afterSignature
        Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} :< Token {tokenKind = TEquals} :< _ ->
          Left
            ( parserFailureAt
                methodSpan
                (UnsupportedSyntax (ClassMethodBody methodName))
            )
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                ( ExpectedSyntax
                    ("signature-only method declaration or '}' in " <> declarationKind <> " declaration body")
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                )
            )

    consumeImplBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        EmptyTokens ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (ExpectedSyntax "'}'" (ParserEndOfInputIn (declarationKind <> " declaration")))
            )
        Token {tokenKind = TRBrace} :< rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} :< _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan}
          :< Token {tokenKind = TEquals}
          :< afterEquals
            | Set.member methodName seenMethodNames ->
                Left
                  ( parserFailureAt
                      methodSpan
                      (DeclarationFailure (DuplicateName ImplMethodName methodName ImplDeclaration))
                  )
            | otherwise -> do
                (methodExpr, afterExpr) <-
                  parseImplExpression afterEquals
                afterMethod <- consumeDot afterExpr
                let method =
                      SurfaceImplMethod
                        (mkIdentifier methodName)
                        (tokenSpan methodToken)
                        methodExpr
                consumeImplBody
                  (Set.insert methodName seenMethodNames)
                  (method : reversedMethods)
                  afterMethod
        Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} :< Token {tokenKind = TColonColon} :< _ ->
          Left
            ( parserFailureAt
                methodSpan
                (DeclarationFailure (ExpectedOrdinaryImplMethodBinding methodName))
            )
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                ( ExpectedSyntax
                    "ordinary method binding or '}' in impl declaration body"
                    (ParserFoundToken (tokenKind token) (tokenLexeme token))
                )
            )

surfaceConcreteImplArguments :: [SurfaceSignatureType] -> Bool
surfaceConcreteImplArguments arguments =
  case arguments of
    [argument] -> surfaceConcreteConstraintArgument argument
    _ -> False

surfaceConcreteConstraintArgument :: SurfaceSignatureType -> Bool
surfaceConcreteConstraintArgument signatureType =
  case signatureType of
    TypeVariable {} -> False
    TypeName name ->
      not (surfaceIdentifierLooksLikeTypeVariable (surfaceNameIdentifier name))
    TypeApplication name arguments ->
      not (surfaceIdentifierLooksLikeTypeVariable (surfaceNameIdentifier name)) && all surfaceConcreteConstraintArgument arguments
    TypeList innerType ->
      surfaceConcreteConstraintArgument innerType
    TypeTuple elementTypes ->
      all surfaceConcreteConstraintArgument elementTypes
    TypeFunction {} ->
      False
    _ -> True

surfaceIdentifierLooksLikeTypeVariable :: Identifier -> Bool
surfaceIdentifierLooksLikeTypeVariable name =
  case Text.uncons memberName of
    Just (c, _) -> isLower c
    Nothing -> False
  where
    fullName = identifierText name
    memberName = maybe fullName snd (splitQualifiedIdentifierText fullName)

validateClassHeaderParameters :: Token -> Maybe [SurfaceSignatureType] -> Either ParserFailure [Identifier]
validateClassHeaderParameters declarationToken maybeHeaderArguments =
  case maybeHeaderArguments of
    Nothing ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (DeclarationFailure ClassRequiresExplicitParameterList)
        )
    Just [] ->
      Left
        ( parserFailureAt
            (tokenSpan declarationToken)
            (DeclarationFailure ClassRequiresLowercaseParameter)
        )
    Just headerArguments -> do
      classParameters <- traverse classParameterFromHeaderArgument headerArguments
      case duplicateClassParameterName classParameters of
        Just duplicateName ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (DeclarationFailure (DuplicateClassParameter duplicateName))
            )
        Nothing ->
          case classParameters of
            [_] -> Right classParameters
            _ ->
              Left
                ( parserFailureAt
                    (tokenSpan declarationToken)
                    (DeclarationFailure ClassSupportsExactlyOneParameter)
                )
  where
    classParameterFromHeaderArgument argument =
      case argument of
        TypeVariable parameterName ->
          Right parameterName
        _ ->
          Left
            ( parserFailureAt
                (tokenSpan declarationToken)
                (DeclarationFailure ClassParameterMustBeLowercase)
            )

    duplicateClassParameterName classParameters =
      go Set.empty classParameters

    go seen remaining =
      case remaining of
        [] -> Nothing
        parameter : rest ->
          let parameterText = identifierText parameter
           in if Set.member parameterText seen
                then Just parameterText
                else go (Set.insert parameterText seen) rest

looksLikeSupportedCapabilityDeclaration :: Text -> TokenStream -> Bool
looksLikeSupportedCapabilityDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

rejectReservedAbstractionSyntax :: Token -> Either ParserFailure a
rejectReservedAbstractionSyntax abstractionToken =
  Left
    ( parserFailureAt
        (tokenSpan abstractionToken)
        (UnsupportedSyntax (AbstractionSyntax (tokenLexeme abstractionToken)))
    )
