{-# LANGUAGE OverloadedStrings #-}

-- | Declaration-level token-stream parsers for the surface parser.
module JazzNext.Compiler.Parser.Declaration
  ( ImplExpressionParser,
    ModuleBodyParser,
    parseCapabilityDeclarationParser,
    parseCapabilityDeclarationTokens,
    parseDataStatementParser,
    parseDataStatementTokens,
    parseImportStatementParser,
    parseImportStatementTokens,
    parseModuleStatementParser,
    parseModuleStatementTokens
  ) where

import Data.Char
  ( isLower,
    isUpper
  )
import Data.Set
  ( Set
  )
import qualified Data.Set as Set
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    mkDiagnostic,
    renderSourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructor (..),
    SurfaceDataConstructorArgument (..),
    SurfaceExpr,
    SurfaceImplMethod (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..)
  )
import JazzNext.Compiler.Parser.Signature
  ( parseConstrainedSignatureType,
    parseSignaturePayload,
    splitTopLevelCommaTokens
  )
import JazzNext.Compiler.Parser.TokenParser
  ( Parser
  )
import qualified Text.Megaparsec as MP

type ModuleBodyParser = [Token] -> Either Diagnostic ([SurfaceStatement], [Token])

type ImplExpressionParser = [Token] -> Either Diagnostic (SurfaceExpr, [Token])

data CapabilityDeclarationBody
  = CapabilityClassBody [SurfaceClassMethodSignature]
  | CapabilityImplBody [SurfaceImplMethod]

parseModuleStatementTokens ::
  ModuleBodyParser ->
  [Token] ->
  Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatementTokens parseModuleBody =
  runDeclarationParser "module declaration" (parseModuleStatementParser parseModuleBody)

parseImportStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementTokens =
  runDeclarationParser "import declaration" parseImportStatementParser

parseDataStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementTokens =
  runDeclarationParser "data declaration" parseDataStatementParser

parseCapabilityDeclarationTokens ::
  ImplExpressionParser ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclarationTokens parseImplExpression =
  runDeclarationParser "capability declaration" (parseCapabilityDeclarationParser parseImplExpression)

parseModuleStatementParser :: ModuleBodyParser -> Parser (Either Diagnostic [SurfaceStatement])
parseModuleStatementParser parseModuleBody =
  parseDeclarationWithRemainder (parseModuleStatementFromTokens parseModuleBody)

parseImportStatementParser :: Parser (Either Diagnostic SurfaceStatement)
parseImportStatementParser =
  parseDeclarationWithRemainder parseImportStatementFromTokens

parseDataStatementParser :: Parser (Either Diagnostic SurfaceStatement)
parseDataStatementParser =
  parseDeclarationWithRemainder parseDataStatementFromTokens

parseCapabilityDeclarationParser :: ImplExpressionParser -> Parser (Either Diagnostic SurfaceStatement)
parseCapabilityDeclarationParser parseImplExpression =
  parseDeclarationWithRemainder (parseCapabilityDeclarationFromTokens parseImplExpression)

runDeclarationParser :: Text -> Parser (Either Diagnostic a) -> [Token] -> Either Diagnostic (a, [Token])
runDeclarationParser label parser tokens =
  case MP.runParser ((,) <$> parser <*> MP.getInput) (Text.unpack label) tokens of
    Right (Right value, remaining) -> Right (value, remaining)
    Right (Left diagnostic, _) -> Left diagnostic
    Left _ -> Left (parseDiagnostic "unexpected declaration token stream parse error")

parseDeclarationWithRemainder ::
  ([Token] -> Either Diagnostic (a, [Token])) ->
  Parser (Either Diagnostic a)
parseDeclarationWithRemainder parseDeclaration = do
  tokens <- MP.getInput
  case parseDeclaration tokens of
    Left diagnostic -> pure (Left diagnostic)
    Right (value, remaining) -> do
      MP.setInput remaining
      pure (Right value)

parseModuleStatementFromTokens ::
  ModuleBodyParser ->
  [Token] ->
  Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatementFromTokens parseModuleBody tokens =
  case tokens of
    moduleToken@Token {tokenKind = TModule} : tokensAfterModuleKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
      case afterModulePath of
        Token {tokenKind = TLBrace} : tokensAfterLeftBrace -> do
          (bodyStatements, remaining) <- parseModuleBody tokensAfterLeftBrace
          pure (SSModule (tokenSpan moduleToken) modulePath : bodyStatements, remaining)
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '{' before end of input after module path at "
                    <> renderSourceSpan (tokenSpan moduleToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '{' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    [] ->
      Left (parseDiagnostic "expected 'module' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'module' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseImportStatementFromTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementFromTokens tokens =
  case tokens of
    importToken@Token {tokenKind = TImport} : tokensAfterImportKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
      parseImportTail importToken modulePath afterModulePath
    [] ->
      Left (parseDiagnostic "expected 'import' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'import' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseImportTail :: Token -> [Text] -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@Token {tokenKind = TAs} : rest ->
      case rest of
        aliasToken@Token {tokenKind = TIdentifier aliasName} : afterAlias
          | isReservedLiteralName aliasName ->
              Left
                ( parseDiagnostic
                    ( "reserved literal '"
                        <> aliasName
                        <> "' cannot be used as an import alias at "
                        <> renderSourceSpan (tokenSpan aliasToken)
                    )
                )
          | otherwise ->
              case afterAlias of
                parenToken@Token {tokenKind = TLParen} : _ ->
                  Left
                    ( parseDiagnostic
                        ( "cannot combine import alias and symbol list at "
                            <> renderSourceSpan (tokenSpan parenToken)
                        )
                    )
                _ -> do
                  remaining <- consumeDot afterAlias
                  pure
                    ( SSImport
                        (tokenSpan importToken)
                        modulePath
                        (Just aliasName)
                        Nothing,
                      remaining
                    )
        [] ->
          Left
            ( parseDiagnostic
                ( "expected import alias before end of input after 'as' at "
                    <> renderSourceSpan (tokenSpan asToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected import alias at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    Token {tokenKind = TLParen} : rest -> do
      (symbols, afterSymbols) <- parseImportSymbolList rest
      case afterSymbols of
        asToken@Token {tokenKind = TAs} : _ ->
          Left
            ( parseDiagnostic
                ( "cannot combine import alias and symbol list at "
                    <> renderSourceSpan (tokenSpan asToken)
                )
            )
        _ -> do
          remaining <- consumeDot afterSymbols
          pure
            ( SSImport
                (tokenSpan importToken)
                modulePath
                Nothing
                (Just symbols),
              remaining
            )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '.', 'as', or '(' before end of input after import path at "
                <> renderSourceSpan (tokenSpan importToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.', 'as', or '(' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDataStatementFromTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementFromTokens tokens =
  case tokens of
    dataToken@Token {tokenKind = TData} : tokensAfterDataKeyword -> do
      (typeName, afterTypeName) <- parseDataTypeName tokensAfterDataKeyword
      (typeParameters, afterTypeParameters) <- parseDataTypeParameters afterTypeName
      afterEquals <-
        consumeEquals
          afterTypeParameters
          ( "expected '=' before end of input after data type name at "
              <> renderSourceSpan (tokenSpan dataToken)
          )
      (constructors, remaining) <- parseDataConstructors typeName typeParameters afterEquals
      pure (SSData (tokenSpan dataToken) typeName typeParameters constructors, remaining)
    [] ->
      Left (parseDiagnostic "expected 'data' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'data' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseCapabilityDeclarationFromTokens ::
  ImplExpressionParser ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclarationFromTokens parseImplExpression tokens =
  case tokens of
    declarationToken@Token {tokenKind = TIdentifier declarationKind} : tokensAfterKeyword ->
      case declarationKind of
        "class" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        "impl" ->
          parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    [] ->
      Left (parseDiagnostic "expected capability declaration before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseCapabilityDeclaration ::
  ImplExpressionParser ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclaration parseImplExpression declarationKind declarationToken tokensAfterKeyword = do
  (capabilityName, maybeHeaderArguments, headerRemaining) <-
    parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword
  let headerArguments =
        case maybeHeaderArguments of
          Just arguments -> arguments
          Nothing -> []
  case declarationKind of
    "class" -> do
      classParameters <- validateClassHeaderParameters declarationToken maybeHeaderArguments
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityClassBody methodSignatures ->
          Right (SSClass (tokenSpan declarationToken) capabilityName classParameters methodSignatures, remaining)
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
                ( parseDiagnostic
                    ( "impl declarations require a concrete impl target at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )
        CapabilityClassBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    _ ->
      rejectReservedAbstractionSyntax declarationToken

parseCapabilityHeaderName :: Text -> Token -> [Token] -> Either Diagnostic (Identifier, Maybe [SurfaceConstrainedSignatureType], [Token])
parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier candidateName, tokenSpan = nameSpan} : rest
      | isConstructorIdentifierText candidateName ->
          parseCapabilityHeaderTail (mkIdentifier candidateName) rest
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected uppercase capability name at "
                    <> renderSourceSpan nameSpan
                    <> ", found '"
                    <> candidateName
                    <> "'"
                )
            )
    Token {tokenKind = TLBrace} : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name before '{' in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Token {tokenKind = TDot} : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name before '.' in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected capability name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected capability name before end of input in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
  where
    parseCapabilityHeaderTail capabilityName tokens =
      case tokens of
        Token {tokenKind = TLParen} : rest -> do
          (headerArguments, afterHeaderParameters) <- parseParenthesizedCapabilityHeader rest
          requireCapabilityBodyStart capabilityName (Just headerArguments) afterHeaderParameters
        _ -> requireCapabilityBodyStart capabilityName Nothing tokens

    requireCapabilityBodyStart capabilityName headerArguments tokens =
      case tokens of
        Token {tokenKind = TLBrace} : _ ->
          Right (capabilityName, headerArguments, tokens)
        Token {tokenKind = TDot} : _ ->
          Left
            ( parseDiagnostic
                ( "expected '{' before '.' in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "unexpected token '"
                    <> tokenLexeme token
                    <> "' in "
                    <> declarationKind
                    <> " declaration header at "
                    <> renderSourceSpan (tokenSpan token)
                )
            )
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '{' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )

    parseParenthesizedCapabilityHeader tokens = do
      (argumentTokens, remaining) <- collectParenthesizedCapabilityHeader tokens
      headerArguments <-
        if null argumentTokens
          then Right []
          else
            case splitTopLevelCommaTokens argumentTokens >>= traverse parseConstrainedSignatureType of
              Just parsedArguments -> Right parsedArguments
              Nothing ->
                Left
                  ( parseDiagnostic
                      ( "unsupported "
                          <> declarationKind
                          <> " declaration header arguments at "
                          <> renderSourceSpan (tokenSpan declarationToken)
                      )
                  )
      Right (headerArguments, remaining)

    collectParenthesizedCapabilityHeader tokens =
      go 1 [] tokens
      where
        go depth acc remaining =
          case remaining of
            token@Token {tokenKind = TLParen} : rest ->
              go (depth + 1) (token : acc) rest
            token@Token {tokenKind = TRParen} : rest
              | depth == 1 -> Right (reverse acc, rest)
              | otherwise -> go (depth - 1) (token : acc) rest
            Token {tokenKind = TLBrace, tokenSpan = braceSpan} : _ ->
              Left
                ( parseDiagnostic
                    ( "expected ')' before '{' in "
                        <> declarationKind
                        <> " declaration header at "
                        <> renderSourceSpan braceSpan
                    )
                )
            token : rest ->
              go depth (token : acc) rest
            [] ->
              Left
                ( parseDiagnostic
                    ( "expected ')' before end of input in "
                        <> declarationKind
                        <> " declaration header at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )

parseCapabilityDeclarationBody ::
  ImplExpressionParser ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (CapabilityDeclarationBody, [Token])
parseCapabilityDeclarationBody parseImplExpression declarationKind declarationToken tokens =
  case tokens of
    Token {tokenKind = TLBrace} : rest ->
      case declarationKind of
        "class" -> do
          (methodSignatures, afterBody) <- consumeClassBody Set.empty [] rest
          Right (CapabilityClassBody methodSignatures, afterBody)
        "impl" -> do
          (methods, afterBody) <- consumeImplBody Set.empty [] rest
          Right (CapabilityImplBody methods, afterBody)
        _ ->
          rejectReservedAbstractionSyntax declarationToken
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '{' before end of input in "
                <> declarationKind
                <> " declaration at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '{' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
  where
    consumeClassBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '}' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : rest
          | Set.member methodName seenMethodNames ->
              Left
                ( parseDiagnostic
                    ( "duplicate method signature '"
                        <> methodName
                        <> "' in class declaration at "
                        <> renderSourceSpan methodSpan
                    )
                )
          | otherwise -> do
              (signatureTokens, afterSignature) <- collectUntilDot rest
              let methodSignature =
                    SurfaceClassMethodSignature
                      (mkIdentifier methodName)
                      (tokenSpan methodToken)
                      (parseSignaturePayload signatureTokens)
              consumeClassBody
                (Set.insert methodName seenMethodNames)
                (methodSignature : reversedMethods)
                afterSignature
        Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TEquals} : _ ->
          Left
            ( parseDiagnostic
                ( "unsupported class method body/default syntax for '"
                    <> methodName
                    <> "' at "
                    <> renderSourceSpan methodSpan
                    <> ": only signature-only method declarations are implemented in jazz-next"
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected signature-only method declaration or '}' in "
                    <> declarationKind
                    <> " declaration body at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

    consumeImplBody seenMethodNames reversedMethods remainingTokens =
      case remainingTokens of
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '}' before end of input in "
                    <> declarationKind
                    <> " declaration at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse reversedMethods, rest)
        operatorToken@Token {tokenKind = TIdentifier "operator"} : _ ->
          rejectNestedOperatorDeclaration operatorToken
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} :
          Token {tokenKind = TEquals} :
          afterEquals
            | Set.member methodName seenMethodNames ->
                Left
                  ( parseDiagnostic
                      ( "duplicate method binding '"
                          <> methodName
                          <> "' in impl declaration at "
                          <> renderSourceSpan methodSpan
                      )
                  )
            | otherwise -> do
                (methodExpr, afterExpr) <- parseImplExpression afterEquals
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
        methodToken@Token {tokenKind = TIdentifier methodName, tokenSpan = methodSpan} : Token {tokenKind = TColonColon} : _ ->
          Left
            ( parseDiagnostic
                ( "expected ordinary method binding for '"
                    <> methodName
                    <> "' in impl declaration body at "
                    <> renderSourceSpan methodSpan
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ordinary method binding or '}' in impl declaration body at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

surfaceConcreteImplArguments :: [SurfaceConstrainedSignatureType] -> Bool
surfaceConcreteImplArguments arguments =
  case arguments of
    [argument] -> surfaceConcreteConstraintArgument argument
    _ -> False

surfaceConcreteConstraintArgument :: SurfaceConstrainedSignatureType -> Bool
surfaceConcreteConstraintArgument signatureType =
  case signatureType of
    SurfaceConstrainedTypeName name ->
      not (surfaceIdentifierLooksLikeTypeVariable name)
    SurfaceConstrainedTypeApplication name arguments ->
      not (surfaceIdentifierLooksLikeTypeVariable name) && all surfaceConcreteConstraintArgument arguments
    SurfaceConstrainedTypeList innerType ->
      surfaceConcreteConstraintArgument innerType
    SurfaceConstrainedTypeTuple elementTypes ->
      all surfaceConcreteConstraintArgument elementTypes
    SurfaceConstrainedTypeFunction {} ->
      False

surfaceIdentifierLooksLikeTypeVariable :: Identifier -> Bool
surfaceIdentifierLooksLikeTypeVariable name =
  case Text.uncons (identifierText name) of
    Just (c, _) -> isLower c
    Nothing -> False

validateClassHeaderParameters :: Token -> Maybe [SurfaceConstrainedSignatureType] -> Either Diagnostic [Identifier]
validateClassHeaderParameters declarationToken maybeHeaderArguments =
  case maybeHeaderArguments of
    Nothing ->
      Left
        ( parseDiagnostic
            ( "class declarations require an explicit parameter list at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Just [] ->
      Left
        ( parseDiagnostic
            ( "class declarations require at least one explicit lowercase parameter at "
                <> renderSourceSpan (tokenSpan declarationToken)
            )
        )
    Just headerArguments -> do
      classParameters <- traverse classParameterFromHeaderArgument headerArguments
      case duplicateClassParameterName classParameters of
        Just duplicateName ->
          Left
            ( parseDiagnostic
                ( "duplicate class parameter '"
                    <> duplicateName
                    <> "' at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
            )
        Nothing ->
          case classParameters of
            [_] -> Right classParameters
            _ ->
              Left
                ( parseDiagnostic
                    ( "class declarations currently support exactly one parameter at "
                        <> renderSourceSpan (tokenSpan declarationToken)
                    )
                )
  where
    classParameterFromHeaderArgument argument =
      case argument of
        SurfaceConstrainedTypeName parameterName
          | surfaceIdentifierLooksLikeTypeVariable parameterName ->
              Right parameterName
        _ ->
          Left
            ( parseDiagnostic
                ( "class parameters must be lowercase type variables at "
                    <> renderSourceSpan (tokenSpan declarationToken)
                )
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

parseDataTypeName :: [Token] -> Either Diagnostic (Identifier, [Token])
parseDataTypeName tokens =
  case tokens of
    Token {tokenKind = TIdentifier typeName, tokenSpan = typeSpan} : rest
      | isConstructorIdentifierText typeName ->
          Right (mkIdentifier typeName, rest)
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected type constructor name at "
                    <> renderSourceSpan typeSpan
                    <> ", found '"
                    <> typeName
                    <> "'"
                )
            )
    [] ->
      Left (parseDiagnostic "expected type constructor name before end of input after 'data'")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected type constructor name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDataTypeParameters :: [Token] -> Either Diagnostic ([Identifier], [Token])
parseDataTypeParameters tokens = go Set.empty [] tokens
  where
    go seenParameters revParameters allTokens =
      case allTokens of
        Token {tokenKind = TEquals} : _ ->
          Right (reverse revParameters, allTokens)
        Token {tokenKind = TIdentifier parameterName, tokenSpan = parameterSpan} : rest
          | isTypeParameterIdentifierText parameterName ->
              if Set.member parameterName seenParameters
                then
                  Left
                    ( parseDiagnostic
                        ("duplicate type parameter '" <> parameterName <> "' in data declaration")
                    )
                else
                  go
                    (Set.insert parameterName seenParameters)
                    (mkIdentifier parameterName : revParameters)
                    rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "expected lowercase type parameter or '=' at "
                        <> renderSourceSpan parameterSpan
                        <> ", found '"
                        <> parameterName
                        <> "'"
                    )
                )
        _ ->
          Right (reverse revParameters, allTokens)

parseDataConstructors :: Identifier -> [Identifier] -> [Token] -> Either Diagnostic ([SurfaceDataConstructor], [Token])
parseDataConstructors typeName typeParameters tokensAfterEquals = do
  (firstConstructor, afterFirstConstructor) <- parseDataConstructor typeName typeParameterNames tokensAfterEquals
  go
    (Set.singleton (surfaceDataConstructorName firstConstructor))
    [firstConstructor]
    afterFirstConstructor
  where
    typeParameterNames = Set.fromList (map identifierText typeParameters)

    go seenConstructors revConstructors allTokens =
      case allTokens of
        Token {tokenKind = TDot} : rest ->
          Right (reverse revConstructors, rest)
        Token {tokenKind = TOperator "|"} : rest -> do
          (nextConstructor, afterNextConstructor) <- parseDataConstructor typeName typeParameterNames rest
          let constructorName = surfaceDataConstructorName nextConstructor
          if Set.member constructorName seenConstructors
            then
              Left
                ( parseDiagnostic
                    ("duplicate constructor declaration '" <> constructorName <> "' in data declaration")
                )
            else
              go
                (Set.insert constructorName seenConstructors)
                (nextConstructor : revConstructors)
                afterNextConstructor
        [] ->
          Left (parseDiagnostic "expected '.' before end of input in data declaration")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '|' or '.' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

    surfaceDataConstructorName :: SurfaceDataConstructor -> Text
    surfaceDataConstructorName (SurfaceDataConstructor constructorName _) =
      identifierText constructorName

parseDataConstructor :: Identifier -> Set Text -> [Token] -> Either Diagnostic (SurfaceDataConstructor, [Token])
parseDataConstructor typeName typeParameterNames tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} : rest
      | isConstructorIdentifierText constructorName -> do
          (constructorArguments, remaining) <- parseDataConstructorArguments typeName typeParameterNames [] rest
          Right
            ( SurfaceDataConstructor (mkIdentifier constructorName) constructorArguments,
              remaining
            )
      | otherwise ->
          Left
            ( parseDiagnostic
                ( "expected constructor declaration at "
                    <> renderSourceSpan constructorSpan
                    <> ", found '"
                    <> constructorName
                    <> "'"
                )
            )
    [] ->
      Left (parseDiagnostic "expected constructor declaration before end of input in data declaration")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDataConstructorArguments ::
  Identifier ->
  Set Text ->
  [SurfaceDataConstructorArgument] ->
  [Token] ->
  Either Diagnostic ([SurfaceDataConstructorArgument], [Token])
parseDataConstructorArguments typeName typeParameterNames revArguments allTokens =
  case allTokens of
    Token {tokenKind = TOperator "|"} : _ ->
      Right (reverse revArguments, allTokens)
    Token {tokenKind = TDot} : _ ->
      Right (reverse revArguments, allTokens)
    [] ->
      Right (reverse revArguments, allTokens)
    _ -> do
      (constructorArgument, remaining) <- parseDataConstructorArgument typeName typeParameterNames allTokens
      parseDataConstructorArguments typeName typeParameterNames (constructorArgument : revArguments) remaining

parseDataConstructorArgument :: Identifier -> Set Text -> [Token] -> Either Diagnostic (SurfaceDataConstructorArgument, [Token])
parseDataConstructorArgument typeName typeParameterNames tokens =
  case tokens of
    Token {tokenKind = TIdentifier argumentName} : rest
      | not (Set.null typeParameterNames)
          && isTypeParameterIdentifierText argumentName
          && Set.notMember argumentName typeParameterNames ->
          Left
            ( parseDiagnostic
                ( "constructor payload type parameter '"
                    <> argumentName
                    <> "' is not declared in data type '"
                    <> identifierText typeName
                    <> "'"
                )
            )
      | otherwise ->
          Right (SurfaceDataConstructorArgumentName (mkIdentifier argumentName), rest)
    Token {tokenKind = TLParen} : rest ->
      fmap ((,) SurfaceDataConstructorArgumentOpaque) (consumeBalancedDataConstructorGroup 1 0 rest)
    Token {tokenKind = TLBracket} : rest ->
      fmap ((,) SurfaceDataConstructorArgumentOpaque) (consumeBalancedDataConstructorGroup 0 1 rest)
    [] ->
      Left (parseDiagnostic "expected constructor argument before end of input in data declaration")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor argument at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeBalancedDataConstructorGroup :: Int -> Int -> [Token] -> Either Diagnostic [Token]
consumeBalancedDataConstructorGroup parenDepth bracketDepth tokens =
  case tokens of
    [] ->
      Left (parseDiagnostic "expected constructor argument to close before end of input in data declaration")
    token : rest ->
      case tokenKind token of
        TLParen ->
          consumeBalancedDataConstructorGroup (parenDepth + 1) bracketDepth rest
        TRParen
          | parenDepth > 0 ->
              let nextParenDepth = parenDepth - 1
               in
                if nextParenDepth == 0 && bracketDepth == 0
                  then Right rest
                  else consumeBalancedDataConstructorGroup nextParenDepth bracketDepth rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "unexpected ')' at "
                        <> renderSourceSpan (tokenSpan token)
                        <> " in constructor argument"
                    )
                )
        TLBracket ->
          consumeBalancedDataConstructorGroup parenDepth (bracketDepth + 1) rest
        TRBracket
          | bracketDepth > 0 ->
              let nextBracketDepth = bracketDepth - 1
               in
                if parenDepth == 0 && nextBracketDepth == 0
                  then Right rest
                  else consumeBalancedDataConstructorGroup parenDepth nextBracketDepth rest
          | otherwise ->
              Left
                ( parseDiagnostic
                    ( "unexpected ']' at "
                        <> renderSourceSpan (tokenSpan token)
                        <> " in constructor argument"
                    )
                )
        _ ->
          consumeBalancedDataConstructorGroup parenDepth bracketDepth rest

parseModulePath :: [Token] -> Either Diagnostic ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected module path before end of input")
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@Token {tokenKind = TColonColon} : [] ->
              Left
                ( parseDiagnostic
                    ( "expected module path segment before end of input at "
                        <> renderSourceSpan (tokenSpan separatorToken)
                    )
                )
            separatorToken@Token {tokenKind = TColonColon} : token : _
              | tokenKind token == TDot ->
                  Left
                    ( parseDiagnostic
                        ( "expected module path segment at "
                            <> renderSourceSpan (tokenSpan separatorToken)
                            <> ", found '"
                            <> tokenLexeme token
                            <> "'"
                        )
                    )
              | otherwise ->
                  Left
                    ( parseDiagnostic
                        ( "expected module path segment at "
                            <> renderSourceSpan (tokenSpan token)
                            <> ", found '"
                            <> tokenLexeme token
                            <> "'"
                        )
                    )
            _ -> Right (reverse revSegments, allTokens)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected module path segment at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseImportSymbolList :: [Token] -> Either Diagnostic ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@Token {tokenKind = TRParen} : _ ->
      Left
        ( parseDiagnostic
            ( "expected at least one import symbol before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ -> do
      (firstSymbol, firstSpan, afterFirstSymbol) <- parseImportSymbol tokensAfterLeftParen
      go [firstSymbol] (Set.singleton firstSymbol) afterFirstSymbol
  where
    go revSymbols seenSymbols allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextSymbol, symbolSpan, afterNextSymbol) <- parseImportSymbol rest
          if Set.member nextSymbol seenSymbols
            then
              Left
                ( parseDiagnostic
                    ( "duplicate import symbol '"
                        <> nextSymbol
                        <> "' at "
                        <> renderSourceSpan symbolSpan
                    )
                )
            else
              go
                (nextSymbol : revSymbols)
                (Set.insert nextSymbol seenSymbols)
                afterNextSymbol
        Token {tokenKind = TRParen} : rest ->
          Right (reverse revSymbols, rest)
        [] ->
          Left (parseDiagnostic "expected ')' before end of input in import symbol list")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ',' or ')' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

parseImportSymbol :: [Token] -> Either Diagnostic (Text, SourceSpan, [Token])
parseImportSymbol tokens =
  case tokens of
    Token {tokenKind = TIdentifier symbolName, tokenSpan = symbolSpan} : rest ->
      Right (symbolName, symbolSpan, rest)
    [] ->
      Left (parseDiagnostic "expected import symbol before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected import symbol at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

collectUntilDot :: [Token] -> Either Diagnostic ([Token], [Token])
collectUntilDot = go []
  where
    go acc [] = Left (parseDiagnostic "expected '.' before end of input")
    go acc allTokens@(token : rest) =
      case tokenKind token of
        TDot
          | null acc ->
              Left
                ( parseDiagnostic
                    ( "expected signature text before '.' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
                )
          | otherwise -> Right (reverse acc, rest)
        _
          | not (null acc) && beginsStatement allTokens ->
              Left
                ( parseDiagnostic
                    ( "expected '.' before '"
                        <> tokenLexeme token
                        <> "' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
                )
          | otherwise -> go (token : acc) rest

beginsStatement :: [Token] -> Bool
beginsStatement tokens =
  case tokens of
    Token {tokenKind = TModule} : _ -> True
    Token {tokenKind = TImport} : _ -> True
    Token {tokenKind = TData} : _ -> True
    Token {tokenKind = TIdentifier "operator"} : rest
      | looksLikeOperatorDeclaration rest -> True
    Token {tokenKind = TLParen} :
      Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TColonColon} :
      _ -> True
    Token {tokenKind = TLParen} :
      Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TEquals} :
      _ -> True
    Token {tokenKind = TIdentifier name} : rest
      | looksLikeReservedAbstractionDeclaration name rest -> True
    Token {tokenKind = TIdentifier _} : Token {tokenKind = TEquals} : _ -> True
    Token {tokenKind = TIdentifier _} : Token {tokenKind = TColonColon} : _ -> True
    _ -> False

looksLikeOperatorDeclaration :: [Token] -> Bool
looksLikeOperatorDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TOperator {}} : _ -> True
    Token {tokenKind = TArrow} : _ -> True
    Token {tokenKind = TIdentifier {}} : rest -> hasOperatorTierBeforeTerminator rest
    _ -> False

hasOperatorTierBeforeTerminator :: [Token] -> Bool
hasOperatorTierBeforeTerminator tokens =
  case tokens of
    [] -> False
    Token {tokenKind = TDot} : _ -> False
    Token {tokenKind = TIdentifier "tier"} : _ -> True
    _ : rest -> hasOperatorTierBeforeTerminator rest

looksLikeReservedAbstractionDeclaration :: Text -> [Token] -> Bool
looksLikeReservedAbstractionDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "trait" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

looksLikeAbstractionDeclaration :: [Token] -> Bool
looksLikeAbstractionDeclaration tokensAfterKeyword =
  case tokensAfterKeyword of
    Token {tokenKind = TIdentifier {}} : rest -> hasAbstractionBodyBeforeTerminator rest
    Token {tokenKind = TAt} : rest -> hasAbstractionBodyBeforeTerminator rest
    _ -> False

hasAbstractionBodyBeforeTerminator :: [Token] -> Bool
hasAbstractionBodyBeforeTerminator tokens =
  case tokens of
    [] -> False
    Token {tokenKind = TDot} : _ -> False
    Token {tokenKind = TLBrace} : _ -> True
    _ : rest -> hasAbstractionBodyBeforeTerminator rest

rejectReservedAbstractionSyntax :: Token -> Either Diagnostic a
rejectReservedAbstractionSyntax abstractionToken =
  Left (parseDiagnostic (abstractionSyntaxDiagnosticText abstractionToken))

abstractionSyntaxDiagnosticText :: Token -> Text
abstractionSyntaxDiagnosticText abstractionToken =
  let abstractionName = tokenLexeme abstractionToken
      location = renderSourceSpan (tokenSpan abstractionToken)
   in case abstractionName of
        "trait" ->
          "unsupported abstraction syntax 'trait' at "
            <> location
            <> ": trait declarations are non-canonical; use class/impl once abstraction semantics land in jazz-next"
        _ ->
          "unsupported abstraction syntax '"
            <> abstractionName
            <> "' at "
            <> location
            <> ": executable class/impl abstraction semantics are deferred in jazz-next"

rejectNestedOperatorDeclaration :: Token -> Either Diagnostic a
rejectNestedOperatorDeclaration operatorToken =
  Left
    ( parseDiagnostic
        ( "operator declarations are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

consumeDot :: [Token] -> Either Diagnostic [Token]
consumeDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected '.' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeEquals :: [Token] -> Text -> Either Diagnostic [Token]
consumeEquals tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TEquals} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '=' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
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

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
