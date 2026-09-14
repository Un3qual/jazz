{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Class and implementation declaration grammar.
module Jazz.Compiler.Parser.CapabilityDeclaration
  ( parseCapabilityDeclarationParser,
    looksLikeSupportedCapabilityDeclaration,
    rejectReservedAbstractionSyntax,
  )
where

import Data.Char
  ( isLower,
  )
import Data.Maybe (fromMaybe)
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
  ( collectUntilDotParser,
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
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.Signature
  ( constraintPrefixParser,
    parseConstrainedSignatureTypeDetailed,
    parseSignaturePayloadDetailed,
    splitTopLevelCommaTokensDetailed,
  )
import Jazz.Compiler.Parser.TokenParser (Parser, failParserFailure, failTokenParser, failTokenParserAt, foundToken, parseAnyToken, parseToken, peekToken)
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
import qualified Text.Megaparsec as MP

capabilityDeclarationKind :: Text -> ParserDeclarationKind
capabilityDeclarationKind kind = case kind of
  "impl" -> ImplDeclaration
  _ -> ClassDeclaration

parseCapabilityDeclarationParser :: Parser SurfaceExpr -> Parser SurfaceStatement
parseCapabilityDeclarationParser expression = do
  tokens <- MP.getInput
  case tokens of
    declaration@Token {tokenKind = TIdentifier kind} :< _
      | kind == "class" || kind == "impl" -> do
          _ <- parseAnyToken
          next <- peekToken
          context <- case next of
            Just Token {tokenKind = TAt} -> constraintPrefixParser
            _ -> pure []
          (name, arguments) <- parseCapabilityHeader kind declaration
          case kind of
            "class" -> do
              parameters <- either failParserFailure pure (validateClassHeaderParameters declaration arguments)
              _ <- parseToken TLBrace
              (methods, defaults) <- parseClassBody expression declaration Set.empty [] Set.empty []
              SSClass (tokenSpan declaration) (surfaceNameIdentifier name) parameters methods context defaults <$ parseToken TDot
            _ -> do
              _ <- parseToken TLBrace
              methods <- parseImplBody expression declaration Set.empty []
              _ <- parseToken TDot
              let targets = fromMaybe [] arguments
              if surfaceSupportedImplArguments targets
                then pure (SSImpl (tokenSpan declaration) name targets methods context)
                else failTokenParserAt (tokenSpan declaration) (DeclarationFailure ImplRequiresConstructorTarget)
      | otherwise -> either failParserFailure pure (rejectReservedAbstractionSyntax declaration)
    EmptyTokens -> failTokenParser (ExpectedSyntax "capability declaration" ParserEndOfInput)
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "capability declaration" (foundToken token))

parseCapabilityHeader :: Text -> Token -> Parser (SurfaceName, Maybe [SurfaceSignatureType])
parseCapabilityHeader kind declaration = do
  tokens <- MP.getInput
  name <- case tokens of
    candidate@Token {tokenKind = TIdentifier candidateName, tokenSpan = nameSpan} :< rest ->
      case rest of
        colon@Token {tokenKind = TColonColon} :< member@Token {tokenKind = TIdentifier memberName} :< _
          | isImmediatelyAfter candidate colon,
            isImmediatelyAfter colon member ->
              if kind == "impl"
                then
                  if isConstructorIdentifierText memberName
                    then SurfaceName (mkQualifiedIdentifier candidateName memberName) (tokenSpan member) (Just nameSpan) <$ MP.takeP Nothing 3
                    else failTokenParserAt (tokenSpan member) (ExpectedSyntax "uppercase class name after '::'" (foundToken member))
                else rejectQualifiedClass colon
        colon@Token {tokenKind = TColonColon} :< EmptyTokens
          | isImmediatelyAfter candidate colon ->
              if kind == "impl"
                then failTokenParserAt (tokenSpan colon) (ExpectedSyntax "class name" (ParserEndOfInputAfter "'::'"))
                else rejectQualifiedClass colon
        colon@Token {tokenKind = TColonColon} :< member :< _
          | isImmediatelyAfter candidate colon ->
              if kind == "impl"
                then
                  failTokenParserAt
                    (tokenSpan member)
                    (ExpectedSyntax (case tokenKind member of TIdentifier {} -> "adjacent class name after '::'"; _ -> "class name after '::'") (foundToken member))
                else rejectQualifiedClass colon
        _
          | isConstructorIdentifierText candidateName -> SurfaceName (mkIdentifier candidateName) nameSpan Nothing <$ parseAnyToken
          | otherwise -> failTokenParserAt nameSpan (ExpectedSyntax "uppercase capability name" (foundToken candidate))
    token :< _
      | tokenKind token `elem` [TLBrace, TDot] ->
          failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "capability name" (ParserBeforeToken (tokenKind token) (tokenLexeme token) (Just (kind <> " declaration"))))
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "capability name" (foundToken token))
    EmptyTokens -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "capability name" (ParserEndOfInputIn (kind <> " declaration")))
  next <- peekToken
  arguments <- case next of
    Just Token {tokenKind = TLParen} -> do
      _ <- parseAnyToken
      argumentTokens <- collectHeaderArguments kind declaration
      if null argumentTokens
        then pure (Just [])
        else case splitTopLevelCommaTokensDetailed argumentTokens >>= traverse parseConstrainedSignatureTypeDetailed of
          Right parsed -> pure (Just parsed)
          Left _ -> failTokenParserAt (tokenSpan declaration) (UnsupportedSyntax (DeclarationHeaderArguments (capabilityDeclarationKind kind)))
    _ -> pure Nothing
  body <- peekToken
  case body of
    Just Token {tokenKind = TLBrace} -> pure (name, arguments)
    Just Token {tokenKind = TDot} -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "'{'" (ParserBeforeToken TDot "." (Just (kind <> " declaration"))))
    Just token -> failTokenParserAt (tokenSpan token) (UnexpectedSyntaxIn (foundToken token) (kind <> " declaration header"))
    Nothing -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "'{'" (ParserEndOfInputIn (kind <> " declaration")))
  where
    rejectQualifiedClass token = failTokenParserAt (tokenSpan token) (ExpectedSyntax "unqualified class name" (foundToken token))

collectHeaderArguments :: Text -> Token -> Parser [Token]
collectHeaderArguments kind declaration = go (1 :: Int) []
  where
    go depth reversed = do
      next <- peekToken
      case next of
        Just token@Token {tokenKind = TLParen} -> parseAnyToken *> go (depth + 1) (token : reversed)
        Just token@Token {tokenKind = TRParen}
          | depth == 1 -> reverse reversed <$ parseAnyToken
          | otherwise -> parseAnyToken *> go (depth - 1) (token : reversed)
        Just Token {tokenKind = TLBrace, tokenSpan = spanValue} ->
          failTokenParserAt spanValue (ExpectedSyntax "')'" (ParserBeforeToken TLBrace "{" (Just (kind <> " declaration header"))))
        Just token -> parseAnyToken *> go depth (token : reversed)
        Nothing -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "')'" (ParserEndOfInputIn (kind <> " declaration header")))

parseClassBody :: Parser SurfaceExpr -> Token -> Set.Set Text -> [SurfaceClassMethodSignature] -> Set.Set Text -> [SurfaceImplMethod] -> Parser ([SurfaceClassMethodSignature], [SurfaceImplMethod])
parseClassBody expression declaration seen reversed seenBodies reversedBodies = do
  tokens <- MP.getInput
  case tokens of
    EmptyTokens -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "'}'" (ParserEndOfInputIn "class declaration"))
    Token {tokenKind = TRBrace} :< _ -> (reverse reversed, reverse reversedBodies) <$ parseAnyToken
    operator@Token {tokenKind = TIdentifier "operator"} :< _ -> either failParserFailure pure (rejectNestedOperatorDeclaration operator)
    method@Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< Token {tokenKind = TColonColon} :< _
      | Set.member name seen -> failTokenParserAt spanValue (DeclarationFailure (DuplicateName ClassMethodName name ClassDeclaration))
      | otherwise -> do
          _ <- MP.takeP Nothing 2
          signatureTokens <- collectUntilDotParser
          payload <- either failParserFailure pure (parseSignaturePayloadDetailed signatureTokens)
          parseClassBody expression declaration (Set.insert name seen) (SurfaceClassMethodSignature (mkIdentifier name) (tokenSpan method) payload : reversed) seenBodies reversedBodies
    Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< Token {tokenKind = TEquals} :< _
      | Set.member name seenBodies -> failTokenParserAt spanValue (DeclarationFailure (DuplicateName ClassMethodName name ClassDeclaration))
      | otherwise -> do
          binding <- parseMethodBinding expression
          parseClassBody expression declaration seen reversed (Set.insert name seenBodies) (binding : reversedBodies)
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "method signature, default binding, or '}' in class declaration body" (foundToken token))

parseImplBody :: Parser SurfaceExpr -> Token -> Set.Set Text -> [SurfaceImplMethod] -> Parser [SurfaceImplMethod]
parseImplBody expression declaration seen reversed = do
  tokens <- MP.getInput
  case tokens of
    EmptyTokens -> failTokenParserAt (tokenSpan declaration) (ExpectedSyntax "'}'" (ParserEndOfInputIn "impl declaration"))
    Token {tokenKind = TRBrace} :< _ -> reverse reversed <$ parseAnyToken
    operator@Token {tokenKind = TIdentifier "operator"} :< _ -> either failParserFailure pure (rejectNestedOperatorDeclaration operator)
    Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< Token {tokenKind = TEquals} :< _
      | Set.member name seen -> failTokenParserAt spanValue (DeclarationFailure (DuplicateName ImplMethodName name ImplDeclaration))
      | otherwise -> do
          binding <- parseMethodBinding expression
          parseImplBody expression declaration (Set.insert name seen) (binding : reversed)
    Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< Token {tokenKind = TColonColon} :< _ ->
      failTokenParserAt spanValue (DeclarationFailure (ExpectedOrdinaryImplMethodBinding name))
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "ordinary method binding or '}' in impl declaration body" (foundToken token))

parseMethodBinding :: Parser SurfaceExpr -> Parser SurfaceImplMethod
parseMethodBinding expression = do
  method <- parseAnyToken
  _ <- parseToken TEquals
  body <- expression <* parseToken TDot
  pure (SurfaceImplMethod (mkIdentifier (tokenLexeme method)) (tokenSpan method) body)

surfaceSupportedImplArguments :: [SurfaceSignatureType] -> Bool
surfaceSupportedImplArguments [argument]
  | surfaceConcreteConstraintArgument argument = True
  | otherwise = case argument of
      TypeList element -> distinctVariables [element]
      TypeTuple elements -> distinctVariables elements
      TypeApplication name arguments ->
        not (surfaceIdentifierLooksLikeTypeVariable (surfaceNameIdentifier name)) && distinctVariables arguments
      _ -> False
  where
    distinctVariables types = case traverse variable types of
      Just names -> not (null names) && length names == Set.size (Set.fromList names)
      Nothing -> False
    variable (TypeVariable name) = Just (identifierText name)
    variable _ = Nothing
surfaceSupportedImplArguments _ = False

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
    Nothing -> reject ClassRequiresExplicitParameterList
    Just [] -> reject ClassRequiresLowercaseParameter
    Just headerArguments -> do
      classParameters <- traverse classParameterFromHeaderArgument headerArguments
      case duplicateClassParameterName classParameters of
        Just duplicateName -> reject (DuplicateClassParameter duplicateName)
        Nothing ->
          case classParameters of
            [_] -> Right classParameters
            _ -> reject ClassSupportsExactlyOneParameter
  where
    reject = Left . parserFailureAt (tokenSpan declarationToken) . DeclarationFailure

    classParameterFromHeaderArgument argument =
      case argument of
        TypeVariable parameterName ->
          Right parameterName
        _ -> reject ClassParameterMustBeLowercase

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
