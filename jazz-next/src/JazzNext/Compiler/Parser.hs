{-# LANGUAGE OverloadedStrings #-}

-- | Surface parser for the current `jazz-next` language slice. It turns the
-- token stream into a block-wrapped surface AST while enforcing the current
-- statement and operator grammar.
module JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  ) where

import Control.Applicative ((<|>))
import Data.Char
  ( isLower,
    isUpper
  )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import qualified Data.Set as Set
import Data.Set (Set)
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
  ( Identifier,
    identifierText,
    mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructorArgument (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  )
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    declaredOperatorInfoForTier,
    isBuiltinOperatorSymbol,
    isReservedOperatorSymbol,
    isValidUserOperatorSymbol,
    lookupOperatorInfoIn
  )

type DeclaredOperators = [OperatorInfo]

-- Parses the current minimal surface language into a block-wrapped program.
-- Most top-level forms are dot-terminated; module declarations instead own a
-- brace-delimited body and must be the first top-level form.
parseSurfaceProgram :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgram source =
  case tokenize source of
    Left err ->
      Left err
    Right tokens ->
      case parseStatementsUntilEnd tokens of
        Left err ->
          Left err
        Right (statements, remaining) ->
          case remaining of
            [] -> Right (SEBlock statements)
            token : _ ->
              Left
                ( parseDiagnostic
                    ( "unexpected token '"
                        <> tokenLexeme token
                        <> "' at "
                        <> renderSourceSpan (tokenSpan token)
                    )
                )

-- | Parse a complete sequence of statements until the token stream is
-- exhausted.
parseStatementsUntilEnd :: [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilEnd tokens = go (collectImportAliasesUntilEnd tokens) [] False [] tokens
  where
    go _ _ _ acc [] = Right (reverse acc, [])
    go knownAliases declaredOperators seenPriorTopLevelForm acc allTokens@(token : rest) =
      case tokenKind token of
        TIdentifier "operator"
          | looksLikeOperatorDeclaration rest -> do
              (operatorInfo, remaining) <- parseOperatorDeclaration TopLevelContext declaredOperators token rest
              go knownAliases (operatorInfo : declaredOperators) True acc remaining
        _ -> do
          (statements, remaining) <- parseStatement TopLevelContext knownAliases declaredOperators allTokens
          case leadingModuleDeclaration statements of
            Just moduleSpan
              | seenPriorTopLevelForm ->
                  Left
                    ( parseDiagnostic
                        ( "module declaration must be the first top-level form at "
                            <> renderSourceSpan moduleSpan
                        )
                    )
              | otherwise ->
                  case remaining of
                    [] -> go (registerImportAliases knownAliases statements) declaredOperators True (prependStatements statements acc) remaining
                    nextToken : _ ->
                      Left
                        ( parseDiagnostic
                            ( "unexpected token '"
                                <> tokenLexeme nextToken
                                <> "' at "
                                <> renderSourceSpan (tokenSpan nextToken)
                                <> " after module declaration"
                            )
                        )
            Nothing ->
              go (registerImportAliases knownAliases statements) declaredOperators True (prependStatements statements acc) remaining

-- | Parse statements inside `{ ... }`, stopping as soon as the closing brace is
-- encountered so block parsing can hand the remaining tokens back to callers.
parseStatementsUntilBrace :: StatementContext -> Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatementsUntilBrace context inheritedAliases inheritedOperators tokens =
  go (Set.union inheritedAliases (collectImportAliasesUntilBrace tokens)) inheritedOperators [] tokens
  where
    go _ _ _ [] = Left (parseDiagnostic "expected '}' before end of input")
    go knownAliases declaredOperators acc allTokens@(token : rest) =
      case tokenKind token of
        TRBrace -> Right (reverse acc, rest)
        TIdentifier "operator"
          | looksLikeOperatorDeclaration rest -> do
              (operatorInfo, remaining) <- parseOperatorDeclaration context declaredOperators token rest
              go knownAliases (operatorInfo : declaredOperators) acc remaining
        _ -> do
          (statements, remaining) <- parseStatement context knownAliases declaredOperators allTokens
          go (registerImportAliases knownAliases statements) declaredOperators (prependStatements statements acc) remaining

-- | Statement grammar context. Module and data declarations are intentionally
-- restricted here instead of in later phases so nested declarations fail with
-- parser diagnostics before lowering.
data StatementContext
  = TopLevelContext
  -- Module bodies can contain bindings/imports but must not introduce a second
  -- module declaration.
  | ModuleBodyContext
  -- Ordinary expression blocks must not introduce module declarations.
  | NestedBlockContext

parseOperatorDeclaration :: StatementContext -> DeclaredOperators -> Token -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclaration context declaredOperators operatorToken tokensAfterKeyword =
  case context of
    NestedBlockContext ->
      rejectNestedOperatorDeclaration operatorToken
    TopLevelContext ->
      parseTopLevelOperatorDeclaration
    ModuleBodyContext ->
      parseTopLevelOperatorDeclaration
  where
    parseTopLevelOperatorDeclaration = do
      (operatorSymbol, afterSymbol) <- parseOperatorDeclarationSymbol tokensAfterKeyword
      validateDeclaredOperatorSymbol declaredOperators operatorToken operatorSymbol
      afterTierKeyword <- consumeOperatorTierKeyword operatorToken afterSymbol
      (operatorInfo, afterTier) <- parseOperatorDeclarationTier operatorToken operatorSymbol afterTierKeyword
      remaining <- consumeOperatorDeclarationDot afterTier
      Right (operatorInfo, remaining)

parseOperatorDeclarationSymbol :: [Token] -> Either Diagnostic (Text, [Token])
parseOperatorDeclarationSymbol tokens =
  case tokens of
    Token {tokenKind = TOperator operatorSymbol} : rest ->
      Right (operatorSymbol, rest)
    Token {tokenKind = TArrow, tokenLexeme = arrowLexeme} : rest ->
      Right (arrowLexeme, rest)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator symbol after 'operator' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left (parseDiagnostic "expected operator symbol after 'operator' before end of input")

validateDeclaredOperatorSymbol :: DeclaredOperators -> Token -> Text -> Either Diagnostic ()
validateDeclaredOperatorSymbol declaredOperators operatorToken declaredSymbol
  | isBuiltinOperatorSymbol declaredSymbol =
      Left
        ( parseDiagnostic
            ( "cannot redeclare built-in operator '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | isReservedOperatorSymbol declaredSymbol =
      Left
        ( parseDiagnostic
            ( "reserved operator symbol '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | any ((== declaredSymbol) . operatorSymbol) declaredOperators =
      Left
        ( parseDiagnostic
            ( "duplicate operator declaration '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )
  | isValidUserOperatorSymbol declaredSymbol =
      Right ()
  | otherwise =
      Left
        ( parseDiagnostic
            ( "invalid operator symbol '"
                <> declaredSymbol
                <> "' at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

consumeOperatorTierKeyword :: Token -> [Token] -> Either Diagnostic [Token]
consumeOperatorTierKeyword operatorToken tokens =
  case tokens of
    Token {tokenKind = TIdentifier "tier"} : rest ->
      Right rest
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' in operator declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOperatorDeclarationTier :: Token -> Text -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationTier operatorToken operatorSymbol tokens =
  case tokens of
    Token {tokenKind = TInt tier} : rest ->
      case declaredOperatorInfoForTier operatorSymbol tier of
        Just operatorInfo ->
          Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parseDiagnostic
                ( "operator tier must be between 1 and 5 at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator tier 1-5 at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected operator tier 1-5 before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

consumeOperatorDeclarationDot :: [Token] -> Either Diagnostic [Token]
consumeOperatorDeclarationDot tokens =
  case tokens of
    Token {tokenKind = TDot} : rest ->
      Right rest
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.' after operator declaration tier at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left (parseDiagnostic "expected '.' after operator declaration tier before end of input")

-- | Disambiguate statement-level forms before expression parsing so leading
-- identifiers can become signatures or bindings when followed by `::` or `=`.
parseStatement :: StatementContext -> Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatement context knownAliases declaredOperators tokens =
  case tokens of
    abstractionToken@(Token {tokenKind = TIdentifier name}) : rest
      | isDeclarationContext context,
        looksLikeSupportedCapabilityDeclaration name rest ->
          fmap singleStatement (parseCapabilityDeclaration knownAliases declaredOperators name abstractionToken rest)
      | isDeclarationContext context,
        looksLikeReservedAbstractionDeclaration name rest ->
          rejectReservedAbstractionSyntax abstractionToken
    moduleToken@(Token {tokenKind = TModule}) : rest ->
      case context of
        TopLevelContext ->
          parseModuleStatement moduleToken rest
        ModuleBodyContext ->
          rejectNestedModuleDeclaration moduleToken
        NestedBlockContext ->
          rejectNestedModuleDeclaration moduleToken
    importToken@(Token {tokenKind = TImport}) : rest ->
      fmap singleStatement (parseImportStatement importToken rest)
    dataToken@(Token {tokenKind = TData}) : rest ->
      case context of
        TopLevelContext ->
          fmap singleStatement (parseDataStatement dataToken rest)
        ModuleBodyContext ->
          fmap singleStatement (parseDataStatement dataToken rest)
        NestedBlockContext ->
          rejectNestedDataDeclaration dataToken
    -- Statement-level forms take precedence over expression parsing when the
    -- leading identifier is followed by declaration syntax.
    (nameToken : afterName@(Token {tokenKind = TColonColon} : _))
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( parseDiagnostic
                ( "reserved literal '"
                    <> name
                    <> "' cannot be used as a binding name at "
                    <> renderSourceSpan (tokenSpan nameToken)
                )
            )
      | TIdentifier name <- tokenKind nameToken,
        shouldParseQualifiedAliasStatement knownAliases name nameToken afterName ->
          fmap singleStatement (parseExprStatement knownAliases declaredOperators tokens)
      | TIdentifier name <- tokenKind nameToken ->
          fmap singleStatement (parseSignature (mkIdentifier name) nameToken afterName)
    (nameToken : afterName@(Token {tokenKind = TEquals} : _))
      | TIdentifier name <- tokenKind nameToken,
        isReservedLiteralName name ->
          Left
            ( parseDiagnostic
                ( "reserved literal '"
                    <> name
                    <> "' cannot be used as a binding name at "
                    <> renderSourceSpan (tokenSpan nameToken)
                )
            )
      | TIdentifier name <- tokenKind nameToken ->
          fmap singleStatement (parseLet knownAliases declaredOperators (mkIdentifier name) nameToken afterName)
    _ -> fmap singleStatement (parseExprStatement knownAliases declaredOperators tokens)
  where
    singleStatement (statement, remaining) = ([statement], remaining)

beginsStatement :: [Token] -> Bool
beginsStatement tokens =
  case tokens of
    Token {tokenKind = TModule} : _ -> True
    Token {tokenKind = TImport} : _ -> True
    Token {tokenKind = TData} : _ -> True
    Token {tokenKind = TIdentifier "operator"} : rest
      | looksLikeOperatorDeclaration rest -> True
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

isDeclarationContext :: StatementContext -> Bool
isDeclarationContext context =
  case context of
    TopLevelContext -> True
    ModuleBodyContext -> True
    NestedBlockContext -> False

looksLikeReservedAbstractionDeclaration :: Text -> [Token] -> Bool
looksLikeReservedAbstractionDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "trait" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    _ -> False

looksLikeSupportedCapabilityDeclaration :: Text -> [Token] -> Bool
looksLikeSupportedCapabilityDeclaration name tokensAfterKeyword =
  case name of
    "class" -> looksLikeAbstractionDeclaration tokensAfterKeyword
    "impl" -> looksLikeAbstractionDeclaration tokensAfterKeyword
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
  Left
    ( parseDiagnostic
        (abstractionSyntaxDiagnosticText abstractionToken)
    )

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

data CapabilityDeclarationBody
  = CapabilityClassBody [SurfaceClassMethodSignature]
  | CapabilityImplBody [SurfaceImplMethod]

parseCapabilityDeclaration ::
  Set Text ->
  DeclaredOperators ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (SurfaceStatement, [Token])
parseCapabilityDeclaration knownAliases declaredOperators declarationKind declarationToken tokensAfterKeyword = do
  (capabilityName, maybeHeaderArguments, headerRemaining) <-
    parseCapabilityHeaderName declarationKind declarationToken tokensAfterKeyword
  let headerArguments =
        case maybeHeaderArguments of
          Just arguments -> arguments
          Nothing -> []
  case declarationKind of
    "class" -> do
      classParameters <- validateClassHeaderParameters declarationToken maybeHeaderArguments
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody knownAliases declaredOperators declarationKind declarationToken headerRemaining
      remaining <- consumeDot afterBody
      case capabilityBody of
        CapabilityClassBody methodSignatures ->
          Right (SSClass (tokenSpan declarationToken) capabilityName classParameters methodSignatures, remaining)
        CapabilityImplBody {} ->
          rejectReservedAbstractionSyntax declarationToken
    "impl" -> do
      (capabilityBody, afterBody) <- parseCapabilityDeclarationBody knownAliases declaredOperators declarationKind declarationToken headerRemaining
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
  Set Text ->
  DeclaredOperators ->
  Text ->
  Token ->
  [Token] ->
  Either Diagnostic (CapabilityDeclarationBody, [Token])
parseCapabilityDeclarationBody knownAliases declaredOperators declarationKind declarationToken tokens =
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
                (methodExpr, afterExpr) <- parseExpr knownAliases declaredOperators afterEquals
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

registerImportAliases :: Set Text -> [SurfaceStatement] -> Set Text
registerImportAliases =
  foldl registerImportAlias
  where
    registerImportAlias knownAliases statement =
      case statement of
        SSImport _ _ (Just aliasName) Nothing ->
          Set.insert aliasName knownAliases
        _ -> knownAliases

-- | Decide whether `Name::member` at statement start is a qualified alias
-- expression or a compact signature. The parser has to make this choice before
-- expression parsing because both forms begin with the same token pair.
shouldParseQualifiedAliasStatement :: Set Text -> Text -> Token -> [Token] -> Bool
shouldParseQualifiedAliasStatement knownAliases name nameToken tokensAfterName =
  case tokensAfterName of
    colonToken@(Token {tokenKind = TColonColon}) : _ ->
      isImmediatelyAfter nameToken colonToken
        && ( Set.member name knownAliases
               || not (shouldParseCompactSignature name nameToken tokensAfterName)
           )
    _ ->
      False

shouldParseCompactSignature :: Text -> Token -> [Token] -> Bool
shouldParseCompactSignature name nameToken tokensAfterName =
  case parseSignature (mkIdentifier name) nameToken tokensAfterName of
    Right (SSSignature _ _ signaturePayload, remaining) ->
      isSupportedSignaturePayload signaturePayload
        || isLikelyUnsupportedSignaturePayload signaturePayload
        || not (isConstructorIdentifierText name)
        || nextStatementStartsMatchingBinding name remaining
    Left _ -> False

isSupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isSupportedSignaturePayload signaturePayload =
  case signaturePayload of
    SurfaceSignatureType _ -> True
    SurfaceConstrainedSignature _ _ -> True
    SurfaceUnsupportedSignature _ -> False

isLikelyUnsupportedSignaturePayload :: SurfaceSignaturePayload -> Bool
isLikelyUnsupportedSignaturePayload signaturePayload =
  case signaturePayload of
    SurfaceUnsupportedSignature [SurfaceSignatureNameToken name] ->
      isSingleLetterTypeVariable name
    SurfaceUnsupportedSignature tokens ->
      any isSignatureSyntaxToken tokens
    _ ->
      False

isSingleLetterTypeVariable :: Text -> Bool
isSingleLetterTypeVariable name =
  case Text.uncons name of
    Just (firstChar, rest) -> Text.null rest && isLower firstChar
    Nothing -> False

isSignatureSyntaxToken :: SurfaceSignatureToken -> Bool
isSignatureSyntaxToken signatureToken =
  case signatureToken of
    SurfaceSignatureArrowToken -> True
    SurfaceSignatureAtToken -> True
    SurfaceSignatureColonToken -> True
    SurfaceSignatureLParenToken -> True
    SurfaceSignatureRParenToken -> True
    SurfaceSignatureLBraceToken -> True
    SurfaceSignatureRBraceToken -> True
    SurfaceSignatureLBracketToken -> True
    SurfaceSignatureRBracketToken -> True
    SurfaceSignatureCommaToken -> True
    _ -> False

nextStatementStartsMatchingBinding :: Text -> [Token] -> Bool
nextStatementStartsMatchingBinding name tokens =
  case tokens of
    Token {tokenKind = TIdentifier nextName} : Token {tokenKind = TEquals} : _ ->
      nextName == name
    _ ->
      False

isImmediatelyAfter :: Token -> Token -> Bool
isImmediatelyAfter leftToken rightToken =
  spanLine (tokenSpan leftToken) == spanLine (tokenSpan rightToken)
    && spanColumn (tokenSpan rightToken) == spanColumn (tokenSpan leftToken) + Text.length (tokenLexeme leftToken)

collectImportAliasesUntilEnd :: [Token] -> Set Text
collectImportAliasesUntilEnd = collectImportAliasesInStatementList False

collectImportAliasesUntilBrace :: [Token] -> Set Text
collectImportAliasesUntilBrace = collectImportAliasesInStatementList True

-- | Pre-scan import statements in the current statement list so qualified
-- alias lookups can be parsed even when the alias is declared later in the
-- same top-level/module body.
collectImportAliasesInStatementList :: Bool -> [Token] -> Set Text
collectImportAliasesInStatementList stopAtRightBrace = go 0 Set.empty
  where
    go _ aliases [] = aliases
    go depth aliases (token : rest)
      | stopAtRightBrace && depth == 0 && tokenKind token == TRBrace = aliases
      | otherwise =
          case tokenKind token of
            TImport
              | depth == 0 ->
                  go depth (maybe aliases (`Set.insert` aliases) (collectImportAlias rest)) rest
            TLBrace ->
              go (depth + 1) aliases rest
            TRBrace ->
              go (max 0 (depth - 1)) aliases rest
            _ ->
              go depth aliases rest

    collectImportAlias importTail =
      case importTail of
        [] -> Nothing
        Token {tokenKind = TDot} : _ -> Nothing
        Token {tokenKind = TAs} : Token {tokenKind = TIdentifier aliasName} : _ -> Just aliasName
        _ : rest -> collectImportAlias rest

rejectNestedModuleDeclaration :: Token -> Either Diagnostic a
rejectNestedModuleDeclaration moduleToken =
  Left
    ( parseDiagnostic
        ( "module declaration must remain top-level at "
            <> renderSourceSpan (tokenSpan moduleToken)
        )
    )

rejectNestedDataDeclaration :: Token -> Either Diagnostic a
rejectNestedDataDeclaration dataToken =
  Left
    ( parseDiagnostic
        ( "data declaration must remain top-level at "
            <> renderSourceSpan (tokenSpan dataToken)
        )
    )

rejectNestedOperatorDeclaration :: Token -> Either Diagnostic a
rejectNestedOperatorDeclaration operatorToken =
  Left
    ( parseDiagnostic
        ( "operator declarations are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

prependStatements :: [SurfaceStatement] -> [SurfaceStatement] -> [SurfaceStatement]
prependStatements statements acc = foldl (flip (:)) acc statements

leadingModuleDeclaration :: [SurfaceStatement] -> Maybe SourceSpan
leadingModuleDeclaration statements =
  case statements of
    SSModule spanValue _ : _ -> Just spanValue
    _ -> Nothing

isReservedLiteralName :: Text -> Bool
isReservedLiteralName name = name == "True" || name == "False"

parseModuleStatement :: Token -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatement moduleToken tokensAfterModuleKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
  case afterModulePath of
    Token {tokenKind = TLBrace} : tokensAfterLeftBrace -> do
      -- Keep downstream resolver/driver code on the current flat statement
      -- contract by replaying module-body statements after the declaration.
      (bodyStatements, remaining) <- parseStatementsUntilBrace ModuleBodyContext Set.empty [] tokensAfterLeftBrace
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

parseImportStatement :: Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatement importToken tokensAfterImportKeyword = do
  (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
  parseImportTail importToken modulePath afterModulePath

parseImportTail :: Token -> [Text] -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} : rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@(Token {tokenKind = TAs}) : rest ->
      case rest of
        aliasToken@(Token {tokenKind = TIdentifier aliasName}) : afterAlias
          | isReservedLiteralName aliasName ->
              Left
                ( parseDiagnostic
                    ( "reserved literal '"
                        <> aliasName
                        <> "' cannot be used as an import alias at "
                        <> renderSourceSpan (tokenSpan aliasToken)
                    )
                )
          | otherwise -> do
              case afterAlias of
                parenToken@(Token {tokenKind = TLParen}) : _ ->
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
        asToken@(Token {tokenKind = TAs}) : _ ->
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

parseDataStatement :: Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatement dataToken tokensAfterDataKeyword = do
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

-- | Parse `Foo::Bar` style module paths and leave the first non-path token
-- untouched for the caller.
parseModulePath :: [Token] -> Either Diagnostic ([Text], [Token])
parseModulePath tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected module path before end of input")
    Token {tokenKind = TIdentifier firstSegment} : rest ->
      go [firstSegment] rest
      where
        -- Accumulate in reverse to avoid repeated list appends.
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} : Token {tokenKind = TIdentifier nextSegment} : remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@(Token {tokenKind = TColonColon}) : [] ->
              Left
                ( parseDiagnostic
                    ( "expected module path segment before end of input at "
                        <> renderSourceSpan (tokenSpan separatorToken)
                    )
                )
            separatorToken@(Token {tokenKind = TColonColon}) : token : _
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

-- | Parse import symbol lists and reject duplicates immediately so later module
-- resolution can assume the list is unique.
parseImportSymbolList :: [Token] -> Either Diagnostic ([Text], [Token])
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@(Token {tokenKind = TRParen}) : _ ->
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
    -- Accumulate in reverse to keep symbol-list parsing linear.
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
parseSignature :: Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseSignature name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TColonColon} : rest -> do
      (signatureTokens, remainingAfterDot) <- collectUntilDot rest
      let signaturePayload = parseSignaturePayload signatureTokens
      pure
        ( SSSignature name (tokenSpan nameToken) signaturePayload,
          remainingAfterDot
        )
    _ ->
      Left
        ( parseDiagnostic
            ( "internal parser error at "
                <> renderSourceSpan (tokenSpan nameToken)
                <> ": expected '::' after signature name"
            )
        )

parseLet :: Set Text -> DeclaredOperators -> Identifier -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseLet knownAliases declaredOperators name nameToken tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TEquals} : rest -> do
      (valueExpr, afterExpr) <- parseExpr knownAliases declaredOperators rest
      remaining <- consumeDot afterExpr
      pure (SSLet name (tokenSpan nameToken) valueExpr, remaining)
    _ ->
      Left
        ( parseDiagnostic
            ( "internal parser error at "
                <> renderSourceSpan (tokenSpan nameToken)
                <> ": expected '=' after binding name"
            )
        )

parseExprStatement :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseExprStatement knownAliases declaredOperators tokens = do
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    firstToken : _ -> do
      (expr, afterExpr) <- parseExpr knownAliases declaredOperators tokens
      remaining <- consumeDot afterExpr
      pure (SSExpr (tokenSpan firstToken) expr, remaining)

parseExpr :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpr knownAliases declaredOperators =
  parseExprWithMinPrecedenceUntil knownAliases declaredOperators neverStop 1

-- | Entry point for expression parsing that first folds application via
-- `parseApplicationExpr`, then continues with precedence-climbing for infix
-- operators.
parseExprWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithMinPrecedence = parseExprWithMinPrecedenceUntil Set.empty [] neverStop

parseExprWithMinPrecedenceUntil ::
  Set Text ->
  DeclaredOperators ->
  ([Token] -> Bool) ->
  Int ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseExprWithMinPrecedenceUntil knownAliases declaredOperators stop minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parseApplicationExprUntil knownAliases declaredOperators stop tokens
  parseInfixTailWithUntil
    declaredOperators
    stop
    (parseExprWithMinPrecedenceUntil knownAliases declaredOperators stop)
    minPrecedence
    leftExpr
    remainingTokens

-- Used by `if` parsing to preserve the existing compact `if cond then else`
-- surface form without introducing a `then` delimiter.
parseExprWithoutApplication :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplication = parseExprWithoutApplicationWithMinPrecedenceUntil Set.empty [] neverStop 1

parseExprWithoutApplicationWithMinPrecedence :: Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedence =
  parseExprWithoutApplicationWithMinPrecedenceUntil Set.empty [] neverStop

parseExprWithoutApplicationWithMinPrecedenceUntil ::
  Set Text ->
  DeclaredOperators ->
  ([Token] -> Bool) ->
  Int ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases declaredOperators stop minPrecedence tokens = do
  (leftExpr, remainingTokens) <- parsePrimaryExprUntil knownAliases declaredOperators stop tokens
  parseInfixTailWithUntil
    declaredOperators
    stop
    (parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases declaredOperators stop)
    minPrecedence
    leftExpr
    remainingTokens

parseApplicationExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationExpr = parseApplicationExprUntil Set.empty [] neverStop

parseApplicationExprUntil :: Set Text -> DeclaredOperators -> ([Token] -> Bool) -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationExprUntil knownAliases declaredOperators stop tokens = do
  (functionExpr, remainingTokens) <- parsePrimaryExprUntil knownAliases declaredOperators stop tokens
  parseApplicationTailUntil knownAliases declaredOperators stop functionExpr remainingTokens

-- | Function application binds tighter than infix operators, so adjacent
-- primary expressions are folded into left-associated applications first.
parseApplicationTail :: SurfaceExpr -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseApplicationTail = parseApplicationTailUntil Set.empty [] neverStop

parseApplicationTailUntil ::
  Set Text ->
  DeclaredOperators ->
  ([Token] -> Bool) ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseApplicationTailUntil knownAliases declaredOperators stop functionExpr tokens
  | stop tokens = Right (functionExpr, tokens)
  | otherwise =
      case startsPrimaryExprTokens tokens of
        True -> do
          (argumentExpr, remainingAfterArgument) <- parsePrimaryExprUntil knownAliases declaredOperators stop tokens
          parseApplicationTailUntil knownAliases declaredOperators stop (SEApply functionExpr argumentExpr) remainingAfterArgument
        False -> Right (functionExpr, tokens)

neverStop :: [Token] -> Bool
neverStop _ = False

startsPrimaryExprTokens :: [Token] -> Bool
startsPrimaryExprTokens allTokens =
  case allTokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TIf} : _ -> True
    Token {tokenKind = TCase} : _ -> True
    Token {tokenKind = TLambda} : _ -> True
    Token {tokenKind = TLParen} : _ -> True
    Token {tokenKind = TLBrace} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    _ -> False

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWithUntil ::
  DeclaredOperators ->
  ([Token] -> Bool) ->
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseInfixTailWithUntil declaredOperators stop parseRhs minPrecedence leftExpr tokens
  | stop tokens = Right (leftExpr, tokens)
  | otherwise =
      case tokens of
        operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : tokensAfterOperator
          | shouldStopForSectionBoundary tokensAfterOperator ->
              Right (leftExpr, tokens)
          | otherwise ->
              case lookupOperatorInfoIn declaredOperators operatorSymbol of
                Nothing ->
                  Left
                    ( parseDiagnostic
                        ( "operator '"
                            <> operatorSymbol
                            <> "' must be declared before use at "
                            <> renderSourceSpan (tokenSpan operatorToken)
                        )
                    )
                Just operatorInfo
                  | operatorPrecedence operatorInfo < minPrecedence ->
                      Right (leftExpr, tokens)
                  | otherwise -> do
                      let nextMinPrecedence =
                            case operatorAssociativity operatorInfo of
                              AssocLeft -> operatorPrecedence operatorInfo + 1
                              AssocRight -> operatorPrecedence operatorInfo
                      (rightExpr, remainingAfterRight) <-
                        parseRhs nextMinPrecedence tokensAfterOperator
                      parseInfixTailWithUntil
                        declaredOperators
                        stop
                        parseRhs
                        minPrecedence
                        (SEBinary operatorSymbol leftExpr rightExpr)
                        remainingAfterRight
        _ -> Right (leftExpr, tokens)
  where
    shouldStopForSectionBoundary remainingAfterOperator =
      case remainingAfterOperator of
        Token {tokenKind = TRParen} : _ -> True
        _ -> False

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWith ::
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseInfixTailWith = parseInfixTailWithUntil [] neverStop

parsePrimaryExpr :: [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parsePrimaryExpr = parsePrimaryExprUntil Set.empty [] neverStop

parsePrimaryExprUntil :: Set Text -> DeclaredOperators -> ([Token] -> Bool) -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parsePrimaryExprUntil knownAliases declaredOperators stop tokens =
  case tokens of
    [] -> Left (parseDiagnostic "expected expression before end of input")
    token : rest ->
      case tokenKind token of
        TInt value -> parseNumericLiteral token value rest
        TIdentifier name ->
          case name of
            "True" -> Right (SELit (SLBool True), rest)
            "False" -> Right (SELit (SLBool False), rest)
            _ ->
              case rest of
                colonToken@(Token {tokenKind = TColonColon}) : Token {tokenKind = TIdentifier memberName} : afterMember
                  | isImmediatelyAfter token colonToken ->
                      Right (SEQualifiedVar (mkIdentifier name) (mkIdentifier memberName), afterMember)
                colonToken@(Token {tokenKind = TColonColon}) : []
                  | isImmediatelyAfter token colonToken ->
                      Left (parseDiagnostic "expected member name after '::' before end of input")
                colonToken@(Token {tokenKind = TColonColon}) : memberToken : _
                  | isImmediatelyAfter token colonToken ->
                      Left
                        ( parseDiagnostic
                            ( "expected member name after '::' at "
                                <> renderSourceSpan (tokenSpan memberToken)
                                <> ", found '"
                                <> tokenLexeme memberToken
                                <> "'"
                            )
                        )
                _ -> Right (SEVar (mkIdentifier name), rest)
        TIf -> parseIfExprUntil knownAliases declaredOperators stop token rest
        TCase -> parseCaseExpr knownAliases declaredOperators token rest
        TLambda -> parseLambdaExprUntil knownAliases declaredOperators stop token rest
        TLParen -> parseParenExpr knownAliases declaredOperators rest
        TLBrace -> do
          (statements, afterBrace) <- parseStatementsUntilBrace NestedBlockContext knownAliases declaredOperators rest
          Right (SEBlock statements, afterBrace)
        TLBracket -> parseListExpr knownAliases declaredOperators rest
        _ ->
          Left
            ( parseDiagnostic
                ( "unexpected token '"
                    <> tokenLexeme token
                    <> "' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> "; expected expression"
                )
            )

parseNumericLiteral :: Token -> Integer -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseNumericLiteral wholeToken wholeValue tokensAfterWhole = do
  (literal, remaining) <- parseNumericSurfaceLiteral wholeToken wholeValue tokensAfterWhole
  Right (SELit literal, remaining)

parseNumericSurfaceLiteral :: Token -> Integer -> [Token] -> Either Diagnostic (SurfaceLiteral, [Token])
parseNumericSurfaceLiteral wholeToken wholeValue tokensAfterWhole =
  case tokensAfterWhole of
    dotToken@Token {tokenKind = TDot} : fractionalToken@Token {tokenKind = TInt fractionalValue} : rest
      | isImmediatelyAfter wholeToken dotToken,
        isImmediatelyAfter dotToken fractionalToken -> do
          let (maybeTargetType, remaining) =
                parseFractionalLiteralSuffix fractionalToken rest
          let literalText = tokenLexeme wholeToken <> "." <> tokenLexeme fractionalToken
              literalSource =
                mkFractionalLiteralSource
                  wholeValue
                  fractionalValue
                  (Text.length (tokenLexeme fractionalToken))
          floatValue <- parseFloatLiteral (tokenSpan wholeToken) literalText
          if fractionalLiteralExceedsMagnitude literalSource float64MaxFinite
            then Left (invalidFloatLiteralDiagnostic (tokenSpan wholeToken) literalText)
            else Right ()
          Right (SLFloat floatValue literalSource maybeTargetType, remaining)
    _ ->
      Right (SLInt wholeValue, tokensAfterWhole)

parseFractionalLiteralSuffix :: Token -> [Token] -> (Maybe SurfaceNumericType, [Token])
parseFractionalLiteralSuffix fractionalToken tokens =
  case tokens of
    suffixToken@Token {tokenKind = TIdentifier suffixName} : rest
      | isImmediatelyAfter fractionalToken suffixToken,
        Just targetType <- fractionalLiteralSuffixTarget suffixName ->
          (Just targetType, rest)
    _ ->
      (Nothing, tokens)

fractionalLiteralSuffixTarget :: Text -> Maybe SurfaceNumericType
fractionalLiteralSuffixTarget suffixName =
  case suffixName of
    "f16" -> Just SurfaceNumericFloat16
    "f32" -> Just SurfaceNumericFloat32
    "f64" -> Just SurfaceNumericFloat64
    _ -> Nothing

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

requireOperatorVisible :: DeclaredOperators -> Token -> Either Diagnostic ()
requireOperatorVisible declaredOperators operatorToken =
  case tokenKind operatorToken of
    TOperator operatorSymbol ->
      case lookupOperatorInfoIn declaredOperators operatorSymbol of
        Just _ -> Right ()
        Nothing ->
          Left
            ( parseDiagnostic
                ( "operator '"
                    <> operatorSymbol
                    <> "' must be declared before use at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            )
    _ ->
      Left
        ( parseDiagnostic
            ( "internal parser error at "
                <> renderSourceSpan (tokenSpan operatorToken)
                <> ": expected operator token"
            )
        )

-- | Parenthesized forms cover ordinary grouping, operator values like `(+)`,
-- and left/right operator sections.
parseParenExpr :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseParenExpr knownAliases declaredOperators tokensAfterLeftParen =
  case tokensAfterLeftParen of
    operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : rest ->
      case rest of
        Token {tokenKind = TRParen} : remaining -> do
          requireOperatorVisible declaredOperators operatorToken
          Right (SEOperatorValue operatorSymbol, remaining)
        _ -> do
          requireOperatorVisible declaredOperators operatorToken
          (rightExpr, afterRightExpr) <- parseExpr knownAliases declaredOperators rest
          remaining <- consumeRightParen afterRightExpr
          pure (SESectionRight operatorSymbol rightExpr, remaining)
    _ -> do
      (innerExpr, afterInnerExpr) <- parseExpr knownAliases declaredOperators tokensAfterLeftParen
      case afterInnerExpr of
        Token {tokenKind = TComma} : rest -> do
          (tupleElements, afterTupleElements) <- parseTupleElements knownAliases declaredOperators [innerExpr] rest
          remaining <- consumeRightParen afterTupleElements
          Right (SETuple tupleElements, remaining)
        operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : Token {tokenKind = TRParen} : rest -> do
          requireOperatorVisible declaredOperators operatorToken
          Right (SESectionLeft innerExpr operatorSymbol, rest)
        _ -> do
          remaining <- consumeRightParen afterInnerExpr
          Right (innerExpr, remaining)

parseTupleElements :: Set Text -> DeclaredOperators -> [SurfaceExpr] -> [Token] -> Either Diagnostic ([SurfaceExpr], [Token])
parseTupleElements knownAliases declaredOperators reversedElements tokens = do
  (nextElement, afterNextElement) <- parseExpr knownAliases declaredOperators tokens
  case afterNextElement of
    Token {tokenKind = TComma} : rest ->
      parseTupleElements knownAliases declaredOperators (nextElement : reversedElements) rest
    _ ->
      Right (reverse (nextElement : reversedElements), afterNextElement)

parseListExpr :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseListExpr knownAliases declaredOperators tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SEList [], rest)
    _ -> do
      (elements, afterElements) <- parseListElements knownAliases declaredOperators tokensAfterLeftBracket
      remaining <- consumeRightBracket afterElements
      Right (SEList elements, remaining)

parseListElements :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic ([SurfaceExpr], [Token])
parseListElements knownAliases declaredOperators tokens = do
  (firstElement, remainingAfterFirst) <- parseExpr knownAliases declaredOperators tokens
  go [firstElement] remainingAfterFirst
  where
    go elements allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextElement, remainingAfterNext) <- parseExpr knownAliases declaredOperators rest
          go (nextElement : elements) remainingAfterNext
        _ ->
          Right (reverse elements, allTokens)

-- | Parse the compact `if cond thenExpr else elseExpr` surface form.
parseIfExpr :: Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseIfExpr = parseIfExprUntil Set.empty [] neverStop

parseIfExprUntil :: Set Text -> DeclaredOperators -> ([Token] -> Bool) -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseIfExprUntil knownAliases declaredOperators stop ifToken tokensAfterIf = do
  (conditionExpr, afterCondition) <- parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases declaredOperators stop 1 tokensAfterIf
  (thenExpr, afterThenExpr) <- parseExprWithMinPrecedenceUntil knownAliases declaredOperators stop 1 afterCondition
  case afterThenExpr of
    Token {tokenKind = TElse} : afterElse -> do
      (elseExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases declaredOperators stop 1 afterElse
      pure (SEIf conditionExpr thenExpr elseExpr, remaining)
    [] ->
      Left
        ( parseDiagnostic
            ( "expected 'else' before end of input after 'if' at "
                <> renderSourceSpan (tokenSpan ifToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'else' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseCaseExpr :: Set Text -> DeclaredOperators -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseCaseExpr knownAliases declaredOperators caseToken tokensAfterCase =
    case tryCaseBodyCandidates Nothing [] tokensAfterCase of
      Right parsedCaseExpr ->
        Right parsedCaseExpr
      Left maybeBodyDiagnostic ->
        case maybeBodyDiagnostic of
          Just diagnostic ->
            Left diagnostic
          Nothing ->
            case parseExpr knownAliases declaredOperators tokensAfterCase of
              Left scrutineeDiagnostic ->
                Left scrutineeDiagnostic
              Right _ ->
                Left (parseDiagnostic caseBodyMissingMessage)
  where
    caseBodyMissingMessage =
      "expected '{' before end of input after 'case' at " <> renderSourceSpan (tokenSpan caseToken)

    tryCaseBodyCandidates ::
      Maybe Diagnostic ->
      [Token] ->
      [Token] ->
      Either (Maybe Diagnostic) (SurfaceExpr, [Token])
    -- Block expressions and case bodies both start with `{`, so try each brace
    -- as the body boundary and keep the first split that yields a full
    -- scrutinee expression plus a parseable case-arm list.
    tryCaseBodyCandidates firstBodyDiagnostic revPrefix remainingTokens =
      case remainingTokens of
        [] ->
          case firstBodyDiagnostic of
            Just diagnostic -> Left (Just diagnostic)
            Nothing -> Left Nothing
        candidateTokens@(token@(Token {tokenKind = TLBrace}) : rest) ->
          let scrutineeTokens = reverse revPrefix
           in
            case parseExpr knownAliases declaredOperators scrutineeTokens of
              Right (scrutineeExpr, []) ->
                case parseCaseBodyTokens candidateTokens of
                  Right (caseArms, remainingAfterCase) ->
                    Right (SECase scrutineeExpr caseArms, remainingAfterCase)
                  Left diagnostic ->
                    if braceLooksLikeScrutineeBlock candidateTokens
                      then tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest
                      else
                        tryCaseBodyCandidates
                          (rememberLatestDiagnostic firstBodyDiagnostic diagnostic)
                          (token : revPrefix)
                          rest
              _ ->
                tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest
        token : rest ->
          tryCaseBodyCandidates firstBodyDiagnostic (token : revPrefix) rest

    parseCaseBodyTokens :: [Token] -> Either Diagnostic ([SurfaceCaseArm], [Token])
    parseCaseBodyTokens bodyTokens = do
      tokensAfterLeftBrace <- consumeLeftBrace bodyTokens caseBodyMissingMessage
      parseCaseArms knownAliases declaredOperators tokensAfterLeftBrace

    rememberLatestDiagnostic :: Maybe Diagnostic -> Diagnostic -> Maybe Diagnostic
    rememberLatestDiagnostic _ newDiagnostic = Just newDiagnostic

    braceLooksLikeScrutineeBlock :: [Token] -> Bool
    braceLooksLikeScrutineeBlock tokens =
      case tokens of
        Token {tokenKind = TLBrace} : rest ->
          beginsStatement rest || go Nothing rest
        _ -> False
      where
        go previousToken allTokens =
          case allTokens of
            [] -> False
            dotToken@Token {tokenKind = TDot} : nextToken@Token {tokenKind = TInt _} : rest
              | Just wholeToken@Token {tokenKind = TInt _} <- previousToken,
                isImmediatelyAfter wholeToken dotToken,
                isImmediatelyAfter dotToken nextToken ->
                  go (Just nextToken) rest
            Token {tokenKind = TDot} : _ -> True
            Token {tokenKind = TRBrace} : _ -> False
            token : remaining -> go (Just token) remaining

parseCaseArms :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic ([SurfaceCaseArm], [Token])
parseCaseArms knownAliases declaredOperators tokensAfterLeftBrace =
  case tokensAfterLeftBrace of
    Token {tokenKind = TRBrace, tokenSpan = rightBraceSpan} : _ ->
      Left
        ( parseDiagnostic
            ( "expected case arm before '}' at "
                <> renderSourceSpan rightBraceSpan
            )
        )
    _ -> do
      (firstArm, afterFirstArm) <- parseCaseArm knownAliases declaredOperators tokensAfterLeftBrace
      go [firstArm] afterFirstArm
  where
    go revArms allTokens =
      case allTokens of
        Token {tokenKind = TRBrace} : rest ->
          Right (reverse revArms, rest)
        _ -> do
          (nextArm, afterNextArm) <- parseCaseArm knownAliases declaredOperators allTokens
          go (nextArm : revArms) afterNextArm

parseCaseArm :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceCaseArm, [Token])
parseCaseArm knownAliases declaredOperators tokens = do
  tokensAfterPipe <- consumeCaseArmPipe tokens
  (casePattern, afterPattern) <- parseCasePattern tokensAfterPipe
  (guardExpr, afterArrow) <- parseOptionalCaseArmGuard afterPattern
  (bodyExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases declaredOperators stopsBeforeCaseArmBoundary 1 afterArrow
  pure (SurfaceCaseArm casePattern guardExpr bodyExpr, remaining)
  where
    parseOptionalCaseArmGuard allTokens =
      case allTokens of
        Token {tokenKind = TIf} : afterIf -> do
          (guardExpr, afterGuard) <- parseCaseArmGuard afterIf
          afterArrow <- consumeArrow afterGuard "expected '->' before end of input after case guard"
          pure (Just guardExpr, afterArrow)
        _ -> do
          afterArrow <- consumeArrow allTokens "expected '->' before end of input after case pattern"
          pure (Nothing, afterArrow)

    parseCaseArmGuard allTokens =
      case allTokens of
        [] -> Left (parseDiagnostic "expected guard expression before end of input after 'if'")
        Token {tokenKind = TArrow} : _ ->
          Left (parseDiagnostic "expected guard expression before '->'")
        Token {tokenKind = TRBrace} : _ ->
          Left (parseDiagnostic "expected guard expression before '}'")
        Token {tokenKind = TOperator "|"} : _ ->
          Left (parseDiagnostic "expected guard expression before next case arm")
        _ ->
          parseCaseGuardExprWithMinPrecedence Nothing 1 allTokens

    parseCaseGuardExprWithMinPrecedence parentOperator minPrecedence guardTokens = do
      (leftExpr, remainingTokens) <-
        parseApplicationExprUntil knownAliases declaredOperators stopsBeforeCaseGuardTerminator guardTokens
      parseCaseGuardInfixTail parentOperator minPrecedence leftExpr remainingTokens

    parseCaseGuardInfixTail parentOperator minPrecedence leftExpr guardTokens
      | stopsBeforeCaseGuardTerminator guardTokens = Right (leftExpr, guardTokens)
      | otherwise =
          case guardTokens of
            operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : tokensAfterOperator
              | shouldStopForSectionBoundary tokensAfterOperator ->
                  Right (leftExpr, guardTokens)
              | operatorSymbol == "|" && caseGuardPipeStartsBoundary parentOperator minPrecedence leftExpr tokensAfterOperator ->
                  Right (leftExpr, guardTokens)
              | otherwise ->
                  case lookupOperatorInfoIn declaredOperators operatorSymbol of
                    Nothing ->
                      Left
                        ( parseDiagnostic
                            ( "operator '"
                                <> operatorSymbol
                                <> "' must be declared before use at "
                                <> renderSourceSpan (tokenSpan operatorToken)
                            )
                        )
                    Just operatorInfo
                      | operatorPrecedence operatorInfo < minPrecedence ->
                          Right (leftExpr, guardTokens)
                      | otherwise -> do
                          let nextMinPrecedence =
                                case operatorAssociativity operatorInfo of
                                  AssocLeft -> operatorPrecedence operatorInfo + 1
                                  AssocRight -> operatorPrecedence operatorInfo
                          (rightExpr, remainingAfterRight) <-
                            parseCaseGuardExprWithMinPrecedence (Just operatorSymbol) nextMinPrecedence tokensAfterOperator
                          parseCaseGuardInfixTail
                            parentOperator
                            minPrecedence
                            (SEBinary operatorSymbol leftExpr rightExpr)
                            remainingAfterRight
            _ -> Right (leftExpr, guardTokens)
      where
        shouldStopForSectionBoundary remainingAfterOperator =
          case remainingAfterOperator of
            Token {tokenKind = TRParen} : _ -> True
            _ -> False

    stopsBeforeCaseGuardTerminator allTokens =
      case allTokens of
        Token {tokenKind = TArrow} : _ -> True
        Token {tokenKind = TRBrace} : _ -> True
        _ -> False

    caseGuardPipeStartsBoundary parentOperator minPrecedence leftExpr tokensAfterPipe =
      startsDefiniteGuardedCaseArmAfterGuardBoundary tokensAfterPipe
        || ( startsDefiniteUnguardedCaseArmAfterGuardBoundary tokensAfterPipe
               && not (caseGuardPipeCanContinueExpression parentOperator minPrecedence leftExpr)
           )

    caseGuardPipeCanContinueExpression parentOperator minPrecedence leftExpr =
      case compare minPrecedence caseGuardPipePrecedence of
        LT -> not (leftExprHasLowerPrecedenceRoot leftExpr)
        EQ -> samePrecedenceGuardPipeCanBind parentOperator leftExpr
        GT -> False

    samePrecedenceGuardPipeCanBind parentOperator leftExpr =
      case leftExpr of
        -- Let lower-precedence parents keep literal-led pipe RHS expressions.
        SELit {} -> parentOperatorAllowsLiteralPipe parentOperator
        _ -> True

    parentOperatorAllowsLiteralPipe parentOperator =
      case parentOperator of
        Just operatorSymbol ->
          case lookupOperatorInfoIn declaredOperators operatorSymbol of
            Just operatorInfo -> operatorPrecedence operatorInfo < caseGuardPipePrecedence
            Nothing -> False
        _ -> False

    -- If a higher-precedence pipe was stopped inside a lower-precedence RHS,
    -- keep treating it as a boundary when control returns to the outer tail.
    leftExprHasLowerPrecedenceRoot leftExpr =
      case leftExpr of
        SEBinary operatorSymbol _ _ ->
          case lookupOperatorInfoIn declaredOperators operatorSymbol of
            Just operatorInfo ->
              operatorPrecedence operatorInfo < caseGuardPipePrecedence
            Nothing -> False
        _ -> False

    caseGuardPipePrecedence =
      case lookupOperatorInfoIn declaredOperators "|" of
        Just operatorInfo -> operatorPrecedence operatorInfo
        Nothing -> 0

    -- Case-arm bodies are expression-shaped, so a `|` operator only starts the
    -- next arm when the following tokens can form a pattern and arrow.
    stopsBeforeCaseArmBoundary allTokens =
      case allTokens of
        Token {tokenKind = TOperator "|"} : rest ->
          startsDefiniteCaseArm rest
        Token {tokenKind = TRBrace} : _ -> True
        _ -> False

    startsDefiniteCaseArm remainingTokens =
      case parseCasePattern remainingTokens of
        Right (_, Token {tokenKind = TArrow} : _) -> True
        Right (_, Token {tokenKind = TIf} : afterGuard) ->
          guardTokensEndAtArrow afterGuard
        Left _
          | startsCasePatternTokens remainingTokens ->
              hasTopLevelArrowBeforeCaseArmBoundary remainingTokens
        _ -> False

    startsDefiniteUnguardedCaseArmAfterGuardBoundary remainingTokens =
      case parseCasePattern remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} : _) ->
          guardBoundaryPatternIsDefinite casePattern
        _ -> False

    startsDefiniteGuardedCaseArmAfterGuardBoundary remainingTokens =
      case parseCasePattern remainingTokens of
        Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
          guardBoundaryPatternIsDefinite casePattern && guardTokensEndAtArrow afterGuard
        _ -> False

    guardTokensEndAtArrow remainingTokens =
      case parseCaseArmGuard remainingTokens of
        Right (_, Token {tokenKind = TArrow} : _) -> True
        _ -> False

    guardBoundaryPatternIsDefinite casePattern =
      case casePattern of
        SPVariable {} -> False
        _ -> True

    hasTopLevelGuardArrow = go 0 0 0
      where
        go parenDepth braceDepth bracketDepth allTokens =
          case allTokens of
            []
              -> False
            Token {tokenKind = TArrow} : _
              | atTopLevel -> True
            Token {tokenKind = TRBrace} : _
              | atTopLevel -> False
            Token {tokenKind = TLParen} : rest ->
              go (parenDepth + 1) braceDepth bracketDepth rest
            Token {tokenKind = TRParen} : rest ->
              go (decrementIfPositive parenDepth) braceDepth bracketDepth rest
            Token {tokenKind = TLBrace} : rest ->
              go parenDepth (braceDepth + 1) bracketDepth rest
            Token {tokenKind = TRBrace} : rest ->
              go parenDepth (decrementIfPositive braceDepth) bracketDepth rest
            Token {tokenKind = TLBracket} : rest ->
              go parenDepth braceDepth (bracketDepth + 1) rest
            Token {tokenKind = TRBracket} : rest ->
              go parenDepth braceDepth (decrementIfPositive bracketDepth) rest
            _ : rest ->
              go parenDepth braceDepth bracketDepth rest
          where
            atTopLevel =
              parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

        decrementIfPositive depth
          | depth > 0 = depth - 1
          | otherwise = 0

    hasTopLevelArrowBeforeCaseArmBoundary = go 0 0 0
      where
        go parenDepth braceDepth bracketDepth allTokens =
          case allTokens of
            []
              -> False
            Token {tokenKind = TArrow} : _
              | atTopLevel -> True
            Token {tokenKind = TOperator "|"} : _
              | atTopLevel -> False
            Token {tokenKind = TRBrace} : _
              | atTopLevel -> False
            Token {tokenKind = TLParen} : rest ->
              go (parenDepth + 1) braceDepth bracketDepth rest
            Token {tokenKind = TRParen} : rest ->
              go (decrementIfPositive parenDepth) braceDepth bracketDepth rest
            Token {tokenKind = TLBrace} : rest ->
              go parenDepth (braceDepth + 1) bracketDepth rest
            Token {tokenKind = TRBrace} : rest ->
              go parenDepth (decrementIfPositive braceDepth) bracketDepth rest
            Token {tokenKind = TLBracket} : rest ->
              go parenDepth braceDepth (bracketDepth + 1) rest
            Token {tokenKind = TRBracket} : rest ->
              go parenDepth braceDepth (decrementIfPositive bracketDepth) rest
            _ : rest ->
              go parenDepth braceDepth bracketDepth rest
          where
            atTopLevel =
              parenDepth == 0 && braceDepth == 0 && bracketDepth == 0

        decrementIfPositive depth
          | depth > 0 = depth - 1
          | otherwise = 0

parseCasePattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCasePattern tokens =
  case tokens of
    token@Token {tokenKind = TInt value} : rest ->
      parseIntegralPatternLiteral token value rest
    Token {tokenKind = TLBracket} : rest ->
      parseListPattern rest
    token@Token {tokenKind = TLParen} : rest ->
      parseTuplePattern token rest
    Token {tokenKind = TIdentifier name} : rest ->
      case name of
        "_" -> Right (SPWildcard, rest)
        "True" -> Right (SPLiteral (SLBool True), rest)
        "False" -> Right (SPLiteral (SLBool False), rest)
        _
          | isConstructorIdentifierText name ->
              parseConstructorPattern (mkIdentifier name) rest
          | otherwise ->
              parseAsPatternOrVariable parseCasePattern name rest
    [] ->
      Left (parseDiagnostic "expected case pattern before end of input")
    token : _ ->
      Left (expectedCasePatternDiagnostic token)

expectedCasePatternDiagnostic :: Token -> Diagnostic
expectedCasePatternDiagnostic token =
  parseDiagnostic
    ( "expected case pattern at "
        <> renderSourceSpan (tokenSpan token)
        <> ", found '"
        <> tokenLexeme token
        <> "'"
    )

parseTuplePattern :: Token -> [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseTuplePattern leftParenToken tokensAfterLeftParen = do
  (firstPattern, afterFirstPattern) <- parseCasePattern tokensAfterLeftParen
  case afterFirstPattern of
    Token {tokenKind = TComma} : rest -> do
      (tuplePatterns, afterTuplePatterns) <- parseTuplePatternElements [firstPattern] rest
      remaining <- consumeRightParen afterTuplePatterns
      Right (SPTuple tuplePatterns, remaining)
    _ ->
      Left (expectedCasePatternDiagnostic leftParenToken)

parseTuplePatternElements :: [SurfacePattern] -> [Token] -> Either Diagnostic ([SurfacePattern], [Token])
parseTuplePatternElements reversedPatterns tokens = do
  (nextPattern, afterNextPattern) <- parseCasePattern tokens
  case afterNextPattern of
    Token {tokenKind = TComma} : rest ->
      parseTuplePatternElements (nextPattern : reversedPatterns) rest
    _ ->
      Right (reverse (nextPattern : reversedPatterns), afterNextPattern)

parseConstructorPattern :: Identifier -> [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseConstructorPattern constructorName tokensAfterName =
  go [] tokensAfterName
  where
    go revArguments remainingTokens
      | patternArgumentBoundary remainingTokens =
          Right (SPConstructor constructorName (reverse revArguments), remainingTokens)
      -- Constructor arguments currently use atomic subpatterns so ambiguous
      -- forms like `Pair Nothing item` stay as two outer arguments.
      | startsCasePatternTokens remainingTokens = do
          (nextArgument, afterArgument) <- parseConstructorArgumentPattern remainingTokens
          go (nextArgument : revArguments) afterArgument
      | otherwise =
          Right (SPConstructor constructorName (reverse revArguments), remainingTokens)

parseConstructorArgumentPattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseConstructorArgumentPattern tokens =
  case tokens of
    token@Token {tokenKind = TInt value} : rest ->
      parseIntegralPatternLiteral token value rest
    Token {tokenKind = TIdentifier name} : rest ->
      case name of
        "True" ->
          Right (SPLiteral (SLBool True), rest)
        "False" ->
          Right (SPLiteral (SLBool False), rest)
        "_" ->
          Right (SPWildcard, rest)
        _
          | isConstructorIdentifierText name ->
              Right (SPConstructor (mkIdentifier name) [], rest)
          | otherwise ->
              parseAsPatternOrVariable parseConstructorArgumentPattern name rest
    Token {tokenKind = TLBracket} : rest ->
      parseListPattern rest
    token@Token {tokenKind = TLParen} : rest ->
      parseTuplePattern token rest
    [] ->
      Left (parseDiagnostic "expected constructor pattern argument before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected constructor pattern argument at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseIntegralPatternLiteral :: Token -> Integer -> [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseIntegralPatternLiteral wholeToken wholeValue tokensAfterWhole =
  case tokensAfterWhole of
    dotToken@Token {tokenKind = TDot} : fractionalToken@Token {tokenKind = TInt _} : _
      | isImmediatelyAfter wholeToken dotToken,
        isImmediatelyAfter dotToken fractionalToken ->
          Left
            ( parseDiagnostic
                ( "fractional literal patterns are not supported at "
                    <> renderSourceSpan (tokenSpan wholeToken)
                )
            )
    _ ->
      Right (SPLiteral (SLInt wholeValue), tokensAfterWhole)

parseAsPatternOrVariable ::
  ([Token] -> Either Diagnostic (SurfacePattern, [Token])) ->
  Text ->
  [Token] ->
  Either Diagnostic (SurfacePattern, [Token])
parseAsPatternOrVariable parseAsTail name tokensAfterName =
  case tokensAfterName of
    Token {tokenKind = TAt} : tokensAfterAt -> do
      (patternExpr, remaining) <- parseAsTail tokensAfterAt
      Right (SPAs (mkIdentifier name) patternExpr, remaining)
    _ ->
      Right (SPVariable (mkIdentifier name), tokensAfterName)

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

parseListPattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseListPattern tokensAfterLeftBracket =
  case tokensAfterLeftBracket of
    Token {tokenKind = TRBracket} : rest ->
      Right (SPList [], rest)
    _ -> do
      (firstPattern, afterFirstPattern) <- parseCasePattern tokensAfterLeftBracket
      go [firstPattern] afterFirstPattern
  where
    go revPatterns remainingTokens =
      case remainingTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextPattern, afterNextPattern) <- parseCasePattern rest
          go (nextPattern : revPatterns) afterNextPattern
        Token {tokenKind = TOperator "|"} : rest -> do
          (tailPattern, afterTailPattern) <- parseCasePattern rest
          remaining <- consumeRightBracket afterTailPattern
          case reverse revPatterns of
            [headPattern] ->
              Right (SPConsList headPattern tailPattern, remaining)
            _ ->
              Left (parseDiagnostic "cons-like list patterns require exactly one head pattern before '|'")
        Token {tokenKind = TRBracket} : rest ->
          Right (SPList (reverse revPatterns), rest)
        [] ->
          Left (parseDiagnostic "expected ']' before end of input in list pattern")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ',' or ']' at "
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

isTypeParameterIdentifierText :: Text -> Bool
isTypeParameterIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isLower firstChar
    Nothing -> False

parseLambdaExpr :: Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseLambdaExpr = parseLambdaExprUntil Set.empty [] neverStop

parseLambdaExprUntil :: Set Text -> DeclaredOperators -> ([Token] -> Bool) -> Token -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseLambdaExprUntil knownAliases declaredOperators stop lambdaToken tokensAfterLambda =
  case tokensAfterLambda of
    Token {tokenKind = TLParen} : afterLeftParen -> do
      (parameters, afterParameters) <- parseLambdaParameters afterLeftParen
      case afterParameters of
        Token {tokenKind = TArrow} : afterArrow -> do
          (bodyExpr, remaining) <- parseExprWithMinPrecedenceUntil knownAliases declaredOperators stop 1 afterArrow
          pure (SELambda parameters bodyExpr, remaining)
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '->' before end of input after lambda parameters at "
                    <> renderSourceSpan (tokenSpan lambdaToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '->' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected '(' before end of input after lambda introducer at "
                <> renderSourceSpan (tokenSpan lambdaToken)
            )
        )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '(' at "
                <> renderSourceSpan (tokenSpan token)
                <> " after lambda introducer"
            )
        )

parseLambdaParameters :: [Token] -> Either Diagnostic ([SurfaceLambdaParameter], [Token])
parseLambdaParameters tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@(Token {tokenKind = TRParen}) : _ ->
      Left
        ( parseDiagnostic
            ( "expected lambda parameter before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ -> do
      (firstParameter, afterFirstParameter) <- parseLambdaParameter tokensAfterLeftParen
      go [firstParameter] afterFirstParameter
  where
    go revParameters allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextParameter, afterNextParameter) <- parseLambdaParameter rest
          go (nextParameter : revParameters) afterNextParameter
        Token {tokenKind = TRParen} : rest ->
          Right (reverse revParameters, rest)
        [] ->
          Left (parseDiagnostic "expected ')' before end of input in lambda parameter list")
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

parseLambdaParameter :: [Token] -> Either Diagnostic (SurfaceLambdaParameter, [Token])
parseLambdaParameter tokens =
  case tokens of
    Token {tokenKind = TInt _} : _ -> parsePatternLambdaParameter tokens
    Token {tokenKind = TLParen} : _ -> parsePatternLambdaParameter tokens
    Token {tokenKind = TLBracket} : _ -> parsePatternLambdaParameter tokens
    Token {tokenKind = TIdentifier parameterName} : rest
      | parameterName == "_" ->
          parsePatternLambdaParameter tokens
      | isReservedLiteralName parameterName ->
          parsePatternLambdaParameter tokens
      | isConstructorIdentifierText parameterName ->
          parsePatternLambdaParameter tokens
      | startsAsPatternTail rest ->
          parsePatternLambdaParameter tokens
      | otherwise ->
          Right (SurfaceLambdaIdentifier (mkIdentifier parameterName), rest)
    [] ->
      Left (parseDiagnostic "expected identifier before end of input in lambda parameter list")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected identifier at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parsePatternLambdaParameter :: [Token] -> Either Diagnostic (SurfaceLambdaParameter, [Token])
parsePatternLambdaParameter tokens = do
  (patternValue, rest) <- parseCasePattern tokens
  Right (SurfaceLambdaPattern patternValue, rest)

startsAsPatternTail :: [Token] -> Bool
startsAsPatternTail tokens =
  case tokens of
    Token {tokenKind = TAt} : _ -> True
    _ -> False

collectUntilDot :: [Token] -> Either Diagnostic ([Token], [Token])
collectUntilDot = go []
  where
    -- Type signatures currently keep the type text as raw tokens joined by
    -- spaces. This helper stops exactly at the signature terminator and guards
    -- against accidentally consuming the next statement start.
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

parseSignaturePayload :: [Token] -> SurfaceSignaturePayload
parseSignaturePayload signatureTokens =
  case parseSupportedSignaturePayload signatureTokens of
    Just signaturePayload -> signaturePayload
    Nothing -> SurfaceUnsupportedSignature (map surfaceSignatureTokenFromToken signatureTokens)

parseSupportedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseSupportedSignaturePayload signatureTokens =
  case parseConstrainedSignaturePayload signatureTokens of
    Just signaturePayload ->
      Just signaturePayload
    Nothing ->
      surfaceSignaturePayloadFromType <$> parseSupportedSignatureType signatureTokens

parseConstrainedSignaturePayload :: [Token] -> Maybe SurfaceSignaturePayload
parseConstrainedSignaturePayload signatureTokens =
  case signatureTokens of
    Token {tokenKind = TAt} : Token {tokenKind = TLBrace} : rest -> do
      (constraintTokens, afterConstraintBlock) <- splitConstraintBlockTokens rest
      constraintGroups <-
        if null constraintTokens
          then Just []
          else splitTopLevelCommaTokens constraintTokens
      constraints <- traverse parseSignatureConstraint constraintGroups
      case afterConstraintBlock of
        Token {tokenKind = TColon} : typeTokens -> do
          signatureType <- parseConstrainedSignatureType typeTokens
          Just (SurfaceConstrainedSignature constraints signatureType)
        _ ->
          Nothing
    _ ->
      Nothing

parseSignatureConstraint :: [Token] -> Maybe SurfaceSignatureConstraint
parseSignatureConstraint constraintTokens =
  case parseConstrainedSignatureType constraintTokens of
    Just (SurfaceConstrainedTypeApplication constraintName arguments) ->
      Just (SurfaceSignatureConstraint constraintName arguments)
    Just (SurfaceConstrainedTypeName constraintName) ->
      Just (SurfaceSignatureConstraint constraintName [])
    _ ->
      Nothing

parseConstrainedSignatureType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedSignatureType signatureTokens =
  case splitFirstTopLevelArrowTokens signatureTokens of
    Left () -> Nothing
    Right (Just (argumentTokens, resultTokens)) ->
      SurfaceConstrainedTypeFunction
        <$> parseConstrainedFunctionOperandType argumentTokens
        <*> parseConstrainedSignatureType resultTokens
    Right Nothing ->
      parseConstrainedFunctionOperandType signatureTokens

parseConstrainedFunctionOperandType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedFunctionOperandType signatureTokens =
  case parseConstrainedTypeApplication signatureTokens of
    Just signatureType ->
      Just signatureType
    Nothing ->
      case signatureTokens of
        [Token {tokenKind = TIdentifier name}] ->
          Just (SurfaceConstrainedTypeName (mkIdentifier name))
        _ ->
          case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
            Just innerTokens ->
              SurfaceConstrainedTypeList <$> parseConstrainedSignatureType innerTokens
            Nothing ->
              case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
                Just innerTokens ->
                  parseConstrainedTupleSignatureType innerTokens
                    <|> parseConstrainedSignatureType innerTokens
                Nothing ->
                  Nothing

parseConstrainedTypeApplication :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedTypeApplication signatureTokens =
  case signatureTokens of
    Token {tokenKind = TIdentifier typeName} : argumentTokens -> do
      argumentTokenGroups <-
        stripWrappedSignatureTokens isLParenToken isRParenToken argumentTokens
          >>= splitTopLevelCommaTokens
      arguments <- traverse parseConstrainedSignatureType argumentTokenGroups
      Just (SurfaceConstrainedTypeApplication (mkIdentifier typeName) arguments)
    _ ->
      Nothing

parseConstrainedTupleSignatureType :: [Token] -> Maybe SurfaceConstrainedSignatureType
parseConstrainedTupleSignatureType signatureTokens =
  case splitTopLevelCommaTokens signatureTokens of
    Just elementTokenGroups
      | length elementTokenGroups >= 2 ->
          SurfaceConstrainedTypeTuple <$> traverse parseConstrainedSignatureType elementTokenGroups
    _ -> Nothing

splitConstraintBlockTokens :: [Token] -> Maybe ([Token], [Token])
splitConstraintBlockTokens = go 0 0 []
  where
    go _ _ _ [] = Nothing
    go parenDepth bracketDepth acc (token : rest)
      | isRBraceToken kind && parenDepth == 0 && bracketDepth == 0 =
          Just (reverse acc, rest)
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth (token : acc) rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth (token : acc) rest
            else Nothing
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) (token : acc) rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) (token : acc) rest
            else Nothing
      | otherwise =
          go parenDepth bracketDepth (token : acc) rest
      where
        kind = tokenKind token

splitTopLevelCommaTokens :: [Token] -> Maybe [[Token]]
splitTopLevelCommaTokens tokens =
  if null tokens
    then Nothing
    else go 0 0 [] [] tokens
  where
    go parenDepth bracketDepth currentRev groupsRev remainingTokens =
      case remainingTokens of
        []
          | parenDepth == 0 && bracketDepth == 0 && not (null currentRev) ->
              Just (reverse (reverse currentRev : groupsRev))
          | otherwise ->
              Nothing
        token : rest
          | tokenKind token == TComma && parenDepth == 0 && bracketDepth == 0 ->
              if null currentRev
                then Nothing
                else go parenDepth bracketDepth [] (reverse currentRev : groupsRev) rest
          | isLParenToken kind ->
              go (parenDepth + 1) bracketDepth nextCurrentRev groupsRev rest
          | isRParenToken kind ->
              if parenDepth > 0
                then go (parenDepth - 1) bracketDepth nextCurrentRev groupsRev rest
                else Nothing
          | isLBracketToken kind ->
              go parenDepth (bracketDepth + 1) nextCurrentRev groupsRev rest
          | isRBracketToken kind ->
              if bracketDepth > 0
                then go parenDepth (bracketDepth - 1) nextCurrentRev groupsRev rest
                else Nothing
          | otherwise ->
              go parenDepth bracketDepth nextCurrentRev groupsRev rest
          where
            kind = tokenKind token
            nextCurrentRev = token : currentRev

parseSupportedSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseSupportedSignatureType signatureTokens =
  case splitFirstTopLevelArrowTokens signatureTokens of
    Left () -> Nothing
    Right (Just (argumentTokens, resultTokens)) ->
      SurfaceTypeFunction
        <$> parseFunctionOperandType argumentTokens
        <*> parseSupportedSignatureType resultTokens
    Right Nothing ->
      parseFunctionOperandType signatureTokens

parseFunctionOperandType :: [Token] -> Maybe SurfaceSignatureType
parseFunctionOperandType signatureTokens =
  case signatureTokens of
    [Token {tokenKind = TIdentifier typeName}] ->
      parseNamedSignatureType typeName
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseTupleSignatureType innerTokens
                <|> parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

parseNonFunctionSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseNonFunctionSignatureType signatureTokens =
  case signatureTokens of
    [Token {tokenKind = TIdentifier typeName}] ->
      parseNamedSignatureType typeName
    _ ->
      case stripWrappedSignatureTokens isLBracketToken isRBracketToken signatureTokens of
        Just innerTokens ->
          SurfaceTypeList <$> parseNonFunctionSignatureType innerTokens
        Nothing ->
          case stripWrappedSignatureTokens isLParenToken isRParenToken signatureTokens of
            Just innerTokens ->
              parseTupleSignatureType innerTokens
                <|> parseSupportedSignatureType innerTokens
            Nothing ->
              Nothing

parseTupleSignatureType :: [Token] -> Maybe SurfaceSignatureType
parseTupleSignatureType signatureTokens =
  case splitTopLevelCommaTokens signatureTokens of
    Just elementTokenGroups
      | length elementTokenGroups >= 2 ->
          SurfaceTypeTuple <$> traverse parseSupportedSignatureType elementTokenGroups
    _ -> Nothing

parseNamedSignatureType :: Text -> Maybe SurfaceSignatureType
parseNamedSignatureType typeName =
  case typeName of
    "Int" -> Just SurfaceTypeInt
    "Float" -> Just SurfaceTypeFloat
    "Bool" -> Just SurfaceTypeBool
    _ -> SurfaceTypeNumeric <$> parseSurfaceNumericType typeName

parseSurfaceNumericType :: Text -> Maybe SurfaceNumericType
parseSurfaceNumericType typeName =
  case typeName of
    "Int8" -> Just SurfaceNumericInt8
    "Int16" -> Just SurfaceNumericInt16
    "Int32" -> Just SurfaceNumericInt32
    "Int64" -> Just SurfaceNumericInt64
    "UInt8" -> Just SurfaceNumericUInt8
    "UInt16" -> Just SurfaceNumericUInt16
    "UInt32" -> Just SurfaceNumericUInt32
    "UInt64" -> Just SurfaceNumericUInt64
    "Float16" -> Just SurfaceNumericFloat16
    "Float32" -> Just SurfaceNumericFloat32
    "Float64" -> Just SurfaceNumericFloat64
    _ -> Nothing

surfaceSignaturePayloadFromType :: SurfaceSignatureType -> SurfaceSignaturePayload
surfaceSignaturePayloadFromType = SurfaceSignatureType

splitFirstTopLevelArrowTokens :: [Token] -> Either () (Maybe ([Token], [Token]))
splitFirstTopLevelArrowTokens tokens = go 0 0 [] tokens
  where
    go 0 0 _ [] =
      Right Nothing
    go _ _ _ [] =
      Left ()
    go parenDepth bracketDepth beforeArrowRev (token : rest)
      | isArrowToken kind && parenDepth == 0 && bracketDepth == 0 =
          Right (Just (reverse beforeArrowRev, rest))
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth nextBeforeArrowRev rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth nextBeforeArrowRev rest
            else Left ()
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) nextBeforeArrowRev rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) nextBeforeArrowRev rest
            else Left ()
      | otherwise =
          go parenDepth bracketDepth nextBeforeArrowRev rest
      where
        kind = tokenKind token
        nextBeforeArrowRev = token : beforeArrowRev

stripWrappedSignatureTokens ::
  (TokenKind -> Bool) ->
  (TokenKind -> Bool) ->
  [Token] ->
  Maybe [Token]
stripWrappedSignatureTokens isOpenToken isCloseToken tokens =
  case tokens of
    firstToken : rest
      | isOpenToken (tokenKind firstToken) ->
          go 0 0 [] rest
    _ ->
      Nothing
  where
    go _ _ _ [] = Nothing
    go parenDepth bracketDepth acc (token : rest)
      | isCloseToken kind && parenDepth == 0 && bracketDepth == 0 =
          if null acc || not (null rest)
            then Nothing
            else Just (reverse acc)
      | isLParenToken kind =
          go (parenDepth + 1) bracketDepth (token : acc) rest
      | isRParenToken kind =
          if parenDepth > 0
            then go (parenDepth - 1) bracketDepth (token : acc) rest
            else Nothing
      | isLBracketToken kind =
          go parenDepth (bracketDepth + 1) (token : acc) rest
      | isRBracketToken kind =
          if bracketDepth > 0
            then go parenDepth (bracketDepth - 1) (token : acc) rest
            else Nothing
      | otherwise =
          go parenDepth bracketDepth (token : acc) rest
      where
        kind = tokenKind token

surfaceSignatureTokenFromToken :: Token -> SurfaceSignatureToken
surfaceSignatureTokenFromToken token =
  case tokenKind token of
    TIdentifier name -> SurfaceSignatureNameToken name
    TInt value -> SurfaceSignatureIntToken value
    TArrow -> SurfaceSignatureArrowToken
    TAt -> SurfaceSignatureAtToken
    TColon -> SurfaceSignatureColonToken
    TLParen -> SurfaceSignatureLParenToken
    TRParen -> SurfaceSignatureRParenToken
    TLBrace -> SurfaceSignatureLBraceToken
    TRBrace -> SurfaceSignatureRBraceToken
    TLBracket -> SurfaceSignatureLBracketToken
    TRBracket -> SurfaceSignatureRBracketToken
    TComma -> SurfaceSignatureCommaToken
    TOperator symbol -> SurfaceSignatureOperatorToken symbol
    _ -> SurfaceSignatureOtherToken (tokenLexeme token)

isArrowToken :: TokenKind -> Bool
isArrowToken kind =
  case kind of
    TArrow -> True
    _ -> False

isLParenToken :: TokenKind -> Bool
isLParenToken kind =
  case kind of
    TLParen -> True
    _ -> False

isRParenToken :: TokenKind -> Bool
isRParenToken kind =
  case kind of
    TRParen -> True
    _ -> False

isLBracketToken :: TokenKind -> Bool
isLBracketToken kind =
  case kind of
    TLBracket -> True
    _ -> False

isRBracketToken :: TokenKind -> Bool
isRBracketToken kind =
  case kind of
    TRBracket -> True
    _ -> False

isRBraceToken :: TokenKind -> Bool
isRBraceToken kind =
  case kind of
    TRBrace -> True
    _ -> False

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

consumeLeftBrace :: [Token] -> Text -> Either Diagnostic [Token]
consumeLeftBrace tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TLBrace} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
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

consumeArrow :: [Token] -> Text -> Either Diagnostic [Token]
consumeArrow tokens endOfInputMessage =
  case tokens of
    Token {tokenKind = TArrow} : rest -> Right rest
    [] -> Left (parseDiagnostic endOfInputMessage)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '->' at "
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

consumeCaseArmPipe :: [Token] -> Either Diagnostic [Token]
consumeCaseArmPipe tokens =
  case tokens of
    Token {tokenKind = TOperator "|"} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected '|' before end of input in case expression")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '|' at "
                <> renderSourceSpan (tokenSpan token)
                <> " to start case arm"
            )
        )

consumeRightParen :: [Token] -> Either Diagnostic [Token]
consumeRightParen tokens =
  case tokens of
    Token {tokenKind = TRParen} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected ')' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected ')' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

consumeRightBracket :: [Token] -> Either Diagnostic [Token]
consumeRightBracket tokens =
  case tokens of
    Token {tokenKind = TRBracket} : rest -> Right rest
    [] -> Left (parseDiagnostic "expected ']' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected ']' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )

parseDiagnostic :: Text -> Diagnostic
parseDiagnostic = mkDiagnostic "E0001"
