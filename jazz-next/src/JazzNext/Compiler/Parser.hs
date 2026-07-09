{-# LANGUAGE OverloadedStrings #-}

-- | Surface parser for the current `jazz-next` language slice. It turns the
-- token stream into a block-wrapped surface AST while enforcing the current
-- statement and operator grammar.
module JazzNext.Compiler.Parser
  ( parseSurfaceExpressionTokens,
    parseSurfaceProgram
  ) where

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
    mkIdentifier,
    mkOperatorBindingIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePattern (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureType (..),
    SurfaceSignatureToken (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  )
import qualified JazzNext.Compiler.Parser.Declaration as Declaration
import JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    isBuiltinOperatorSymbol,
    isReservedOperatorSymbol,
    isValidUserOperatorSymbol,
    lookupOperatorInfoIn
  )
import qualified JazzNext.Compiler.Parser.Pattern as Pattern
import JazzNext.Compiler.Parser.Signature
  ( parseSignaturePayload,
    parseSignatureTypePrefix
  )

type DeclaredOperators = [OperatorInfo]

data OperatorDeclarationFixityKeyword
  = OperatorTierKeyword
  | OperatorPrecedenceKeyword

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
      (fixityKeyword, afterFixityKeyword) <- consumeOperatorFixityKeyword operatorToken afterSymbol
      (operatorInfo, afterFixity) <- parseOperatorDeclarationFixity operatorToken operatorSymbol fixityKeyword afterFixityKeyword
      (operatorInfoWithAssociativity, afterAssociativity) <- parseOptionalOperatorAssociativity operatorInfo afterFixity
      remaining <- consumeOperatorDeclarationDot (operatorDeclarationFixityLabel fixityKeyword) afterAssociativity
      Right (operatorInfoWithAssociativity, remaining)

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

consumeOperatorFixityKeyword :: Token -> [Token] -> Either Diagnostic (OperatorDeclarationFixityKeyword, [Token])
consumeOperatorFixityKeyword operatorToken tokens =
  case tokens of
    Token {tokenKind = TIdentifier "tier"} : rest ->
      Right (OperatorTierKeyword, rest)
    Token {tokenKind = TIdentifier "precedence"} : rest ->
      Right (OperatorPrecedenceKeyword, rest)
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' or 'precedence' in operator declaration at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected 'tier' or 'precedence' before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOperatorDeclarationFixity :: Token -> Text -> OperatorDeclarationFixityKeyword -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationFixity operatorToken operatorSymbol fixityKeyword tokens =
  case fixityKeyword of
    OperatorTierKeyword ->
      parseOperatorDeclarationTier operatorToken operatorSymbol tokens
    OperatorPrecedenceKeyword ->
      parseOperatorDeclarationPrecedence operatorToken operatorSymbol tokens

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

parseOperatorDeclarationPrecedence :: Token -> Text -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOperatorDeclarationPrecedence operatorToken operatorSymbol tokens =
  case tokens of
    Token {tokenKind = TInt precedence} : rest ->
      case declaredOperatorInfoForPrecedence operatorSymbol precedence of
        Just operatorInfo ->
          Right (operatorInfo, rest)
        Nothing ->
          Left
            ( parseDiagnostic
                ( "operator precedence must be between 1 and 99 at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                )
            )
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator precedence 1-99 at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left
        ( parseDiagnostic
            ( "expected operator precedence 1-99 before end of input in operator declaration at "
                <> renderSourceSpan (tokenSpan operatorToken)
            )
        )

parseOptionalOperatorAssociativity :: OperatorInfo -> [Token] -> Either Diagnostic (OperatorInfo, [Token])
parseOptionalOperatorAssociativity operatorInfo tokens =
  case tokens of
    Token {tokenKind = TIdentifier "left"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocLeft}, rest)
    Token {tokenKind = TIdentifier "right"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocRight}, rest)
    Token {tokenKind = TIdentifier "nonassoc"} : rest ->
      Right (operatorInfo {operatorAssociativity = AssocNonAssoc}, rest)
    token@Token {tokenKind = TIdentifier {}} : _ ->
      Left
        ( parseDiagnostic
            ( "expected operator associativity 'left', 'right', or 'nonassoc' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    _ ->
      Right (operatorInfo, tokens)

operatorDeclarationFixityLabel :: OperatorDeclarationFixityKeyword -> Text
operatorDeclarationFixityLabel fixityKeyword =
  case fixityKeyword of
    OperatorTierKeyword -> "tier"
    OperatorPrecedenceKeyword -> "precedence"

consumeOperatorDeclarationDot :: Text -> [Token] -> Either Diagnostic [Token]
consumeOperatorDeclarationDot fixityLabel tokens =
  case tokens of
    Token {tokenKind = TDot} : rest ->
      Right rest
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected '.' after operator declaration "
                <> fixityLabel
                <> " at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
    [] ->
      Left (parseDiagnostic ("expected '.' after operator declaration " <> fixityLabel <> " before end of input"))

parseOperatorBinding :: StatementContext -> Set Text -> DeclaredOperators -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseOperatorBinding context knownAliases declaredOperators operatorToken tokensAfterEquals =
  case context of
    NestedBlockContext ->
      rejectNestedOperatorBinding operatorToken
    TopLevelContext ->
      parseVisibleOperatorBinding
    ModuleBodyContext ->
      parseVisibleOperatorBinding
  where
    parseVisibleOperatorBinding =
      case tokenKind operatorToken of
        TOperator bindingSymbol
          | isBuiltinOperatorSymbol bindingSymbol ->
              Left
                ( parseDiagnostic
                    ( "cannot bind built-in operator '"
                        <> bindingSymbol
                        <> "' at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | not (operatorDeclared bindingSymbol) ->
              Left
                ( parseDiagnostic
                    ( "operator '"
                        <> bindingSymbol
                        <> "' must be declared before binding at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | otherwise -> do
              (valueExpr, afterExpr) <- parseExpr knownAliases declaredOperators tokensAfterEquals
              remaining <- consumeDot afterExpr
              Right
                ( SSLet
                    (mkOperatorBindingIdentifier bindingSymbol)
                    (tokenSpan operatorToken)
                    valueExpr,
                  remaining
                )
        _ ->
          Left
            ( parseDiagnostic
                ( "internal parser error at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                    <> ": expected operator token in operator binding"
                )
            )

    operatorDeclared bindingSymbol =
      any ((== bindingSymbol) . operatorSymbol) declaredOperators

parseOperatorSignature :: StatementContext -> DeclaredOperators -> Token -> [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseOperatorSignature context declaredOperators operatorToken tokensAfterName =
  case context of
    NestedBlockContext ->
      rejectNestedOperatorSignature operatorToken
    TopLevelContext ->
      parseVisibleOperatorSignature
    ModuleBodyContext ->
      parseVisibleOperatorSignature
  where
    parseVisibleOperatorSignature =
      case tokenKind operatorToken of
        TOperator signatureSymbol
          | isBuiltinOperatorSymbol signatureSymbol ->
              Left
                ( parseDiagnostic
                    ( "cannot sign built-in operator '"
                        <> signatureSymbol
                        <> "' at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | not (operatorDeclared signatureSymbol) ->
              Left
                ( parseDiagnostic
                    ( "operator '"
                        <> signatureSymbol
                        <> "' must be declared before signature at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
          | otherwise ->
              parseSignature (mkOperatorBindingIdentifier signatureSymbol) operatorToken tokensAfterName
        _ ->
          Left
            ( parseDiagnostic
                ( "internal parser error at "
                    <> renderSourceSpan (tokenSpan operatorToken)
                    <> ": expected operator token in operator signature"
                )
            )

    operatorDeclared signatureSymbol =
      any ((== signatureSymbol) . operatorSymbol) declaredOperators

-- | Disambiguate statement-level forms before expression parsing so leading
-- identifiers can become signatures or bindings when followed by `::` or `=`.
parseStatement :: StatementContext -> Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic ([SurfaceStatement], [Token])
parseStatement context knownAliases declaredOperators tokens =
  case tokens of
    Token {tokenKind = TLParen} :
      operatorToken@Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      afterName@(Token {tokenKind = TColonColon} : _) ->
        fmap singleStatement (parseOperatorSignature context declaredOperators operatorToken afterName)
    Token {tokenKind = TLParen} :
      operatorToken@Token {tokenKind = TOperator {}} :
      Token {tokenKind = TRParen} :
      Token {tokenKind = TEquals} :
      afterEquals ->
        fmap singleStatement (parseOperatorBinding context knownAliases declaredOperators operatorToken afterEquals)
    abstractionToken@(Token {tokenKind = TIdentifier name}) : rest
      | isDeclarationContext context,
        looksLikeSupportedCapabilityDeclaration name rest ->
          fmap singleStatement
            ( Declaration.parseCapabilityDeclarationTokens
                (parseExpr knownAliases declaredOperators)
                (abstractionToken : rest)
            )
      | isDeclarationContext context,
        looksLikeReservedAbstractionDeclaration name rest ->
          rejectReservedAbstractionSyntax abstractionToken
    moduleToken@(Token {tokenKind = TModule}) : rest ->
      case context of
        TopLevelContext ->
          Declaration.parseModuleStatementTokens
            (parseStatementsUntilBrace ModuleBodyContext Set.empty [])
            (moduleToken : rest)
        ModuleBodyContext ->
          rejectNestedModuleDeclaration moduleToken
        NestedBlockContext ->
          rejectNestedModuleDeclaration moduleToken
    importToken@(Token {tokenKind = TImport}) : rest ->
      fmap singleStatement (Declaration.parseImportStatementTokens (importToken : rest))
    dataToken@(Token {tokenKind = TData}) : rest ->
      case context of
        TopLevelContext ->
          fmap singleStatement (Declaration.parseDataStatementTokens (dataToken : rest))
        ModuleBodyContext ->
          fmap singleStatement (Declaration.parseDataStatementTokens (dataToken : rest))
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
    Token {tokenKind = TIdentifier {}} : rest -> hasOperatorFixityKeywordBeforeTerminator rest
    _ -> False

hasOperatorFixityKeywordBeforeTerminator :: [Token] -> Bool
hasOperatorFixityKeywordBeforeTerminator tokens =
  case tokens of
    [] -> False
    Token {tokenKind = TDot} : _ -> False
    Token {tokenKind = TIdentifier "tier"} : _ -> True
    Token {tokenKind = TIdentifier "precedence"} : _ -> True
    _ : rest -> hasOperatorFixityKeywordBeforeTerminator rest

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

rejectNestedOperatorBinding :: Token -> Either Diagnostic a
rejectNestedOperatorBinding operatorToken =
  Left
    ( parseDiagnostic
        ( "operator bindings are only allowed at file scope or directly in module bodies at "
            <> renderSourceSpan (tokenSpan operatorToken)
        )
    )

rejectNestedOperatorSignature :: Token -> Either Diagnostic a
rejectNestedOperatorSignature operatorToken =
  Left
    ( parseDiagnostic
        ( "operator signatures are only allowed at file scope or directly in module bodies at "
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

parseSurfaceExpressionTokens :: Set Text -> DeclaredOperators -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseSurfaceExpressionTokens = parseExpr

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
    (\rhsStop -> parseExprWithMinPrecedenceUntil knownAliases declaredOperators rhsStop)
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
    (\rhsStop -> parseExprWithoutApplicationWithMinPrecedenceUntil knownAliases declaredOperators rhsStop)
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
      case tokens of
        typeApplicationToken@Token {tokenKind = TAt} : tokensAfterAt -> do
          (typeArgument, remainingAfterTypeArgument) <-
            parseTypeApplicationArgument typeApplicationToken tokensAfterAt
          parseApplicationTailUntil
            knownAliases
            declaredOperators
            stop
            (SETypeApplication functionExpr typeArgument)
            remainingAfterTypeArgument
        _
          | startsPrimaryExprTokens tokens -> do
              (argumentExpr, remainingAfterArgument) <- parsePrimaryExprUntil knownAliases declaredOperators stop tokens
              parseApplicationTailUntil knownAliases declaredOperators stop (SEApply functionExpr argumentExpr) remainingAfterArgument
        _ -> Right (functionExpr, tokens)

parseTypeApplicationArgument :: Token -> [Token] -> Either Diagnostic (SurfaceSignatureType, [Token])
parseTypeApplicationArgument typeApplicationToken tokens =
  case parseSignatureTypePrefix tokens of
    Just parsedTypeArgument -> Right parsedTypeArgument
    Nothing ->
      Left
        ( parseDiagnostic
            ( "unsupported explicit type application argument after '@' at "
                <> renderSourceSpan (tokenSpan typeApplicationToken)
            )
        )

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
  (([Token] -> Bool) -> Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
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
                      let nextMinPrecedence = operatorNextMinPrecedence operatorInfo
                          rhsStop =
                            samePrecedenceNonAssociativeRhsStop declaredOperators operatorInfo stop
                      (rightExpr, remainingAfterRight) <-
                        parseRhs rhsStop nextMinPrecedence tokensAfterOperator
                      rejectNonAssociativeContinuation declaredOperators operatorInfo operatorToken remainingAfterRight
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

operatorNextMinPrecedence :: OperatorInfo -> Int
operatorNextMinPrecedence operatorInfo =
  case operatorAssociativity operatorInfo of
    AssocLeft -> operatorPrecedence operatorInfo + 1
    AssocRight -> operatorPrecedence operatorInfo
    AssocNonAssoc -> operatorPrecedence operatorInfo + 1

rejectNonAssociativeContinuation :: DeclaredOperators -> OperatorInfo -> Token -> [Token] -> Either Diagnostic ()
rejectNonAssociativeContinuation declaredOperators operatorInfo operatorToken remainingTokens =
  case remainingTokens of
    Token {tokenKind = TOperator nextSymbol} : _ ->
      case lookupOperatorInfoIn declaredOperators nextSymbol of
        Just nextInfo
          | operatorPrecedence nextInfo == operatorPrecedence operatorInfo,
            operatorAssociativity operatorInfo == AssocNonAssoc
              || operatorAssociativity nextInfo == AssocNonAssoc ->
              Left
                ( parseDiagnostic
                    ( "non-associative operator '"
                        <> nonAssociativeSymbol nextInfo
                        <> "' cannot be chained without parentheses at "
                        <> renderSourceSpan (tokenSpan operatorToken)
                    )
                )
        _ -> Right ()
    _ -> Right ()
  where
    nonAssociativeSymbol nextInfo
      | operatorAssociativity operatorInfo == AssocNonAssoc = operatorSymbol operatorInfo
      | otherwise = operatorSymbol nextInfo

samePrecedenceNonAssociativeRhsStop :: DeclaredOperators -> OperatorInfo -> ([Token] -> Bool) -> [Token] -> Bool
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
          _ -> False
      _ -> False

-- | Shared precedence climber used by both regular expression parsing and the
-- restricted `if` condition parser.
parseInfixTailWith ::
  (Int -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])) ->
  Int ->
  SurfaceExpr ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseInfixTailWith parseRhs =
  parseInfixTailWithUntil [] neverStop (\_ -> parseRhs)

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
  (casePattern, afterPattern) <- parseCaseArmPattern tokensAfterPipe
  (guardExpr, afterArrow) <- parseOptionalCaseArmGuard afterPattern
  (bodyExpr, remaining) <- parseCaseArmBodyExprWithMinPrecedence Nothing 1 afterArrow
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

    parseCaseArmBodyExprWithMinPrecedence parentOperator minPrecedence bodyTokens = do
      (leftExpr, remainingTokens) <-
        parseApplicationExprUntil knownAliases declaredOperators stopsBeforeCaseArmBoundary bodyTokens
      parseCaseArmBodyInfixTail parentOperator minPrecedence leftExpr remainingTokens

    parseCaseArmBodyInfixTail parentOperator minPrecedence leftExpr bodyTokens
      | stopsBeforeCaseArmTerminator bodyTokens = Right (leftExpr, bodyTokens)
      | otherwise =
          case bodyTokens of
            operatorToken@(Token {tokenKind = TOperator operatorSymbol}) : tokensAfterOperator
              | shouldStopForSectionBoundary tokensAfterOperator ->
                  Right (leftExpr, bodyTokens)
              | operatorSymbol == "|" && caseArmPipeStartsBoundary parentOperator minPrecedence leftExpr tokensAfterOperator ->
                  Right (leftExpr, bodyTokens)
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
                          Right (leftExpr, bodyTokens)
                      | otherwise -> do
                          let nextMinPrecedence = operatorNextMinPrecedence operatorInfo
                          (rightExpr, remainingAfterRight) <-
                            parseCaseArmBodyExprWithMinPrecedence (Just operatorSymbol) nextMinPrecedence tokensAfterOperator
                          rejectNonAssociativeContinuation declaredOperators operatorInfo operatorToken remainingAfterRight
                          parseCaseArmBodyInfixTail
                            parentOperator
                            minPrecedence
                            (SEBinary operatorSymbol leftExpr rightExpr)
                            remainingAfterRight
            _ -> Right (leftExpr, bodyTokens)
      where
        shouldStopForSectionBoundary remainingAfterOperator =
          case remainingAfterOperator of
            Token {tokenKind = TRParen} : _ -> True
            _ -> False

    stopsBeforeCaseArmTerminator allTokens =
      case allTokens of
        Token {tokenKind = TRBrace} : _ -> True
        _ -> False

    caseArmPipeStartsBoundary parentOperator minPrecedence leftExpr tokensAfterPipe =
      case parseCasePattern tokensAfterPipe of
        Right (_, Token {tokenKind = TArrow} : _) -> True
        Right (_, Token {tokenKind = TIf} : afterGuard) ->
          guardTokensEndAtArrow afterGuard
        Right (_, Token {tokenKind = TOperator "|"} : _) ->
          startsDefiniteOrPatternCaseArm tokensAfterPipe
            && not
              ( startsAllLiteralOrPatternCaseArm tokensAfterPipe
                  && casePipeCanContinueExpression parentOperator minPrecedence leftExpr
              )
        Left _
          | startsCasePatternTokens tokensAfterPipe ->
              hasTopLevelArrowBeforeCaseArmBoundary tokensAfterPipe
        _ -> False

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
                          let nextMinPrecedence = operatorNextMinPrecedence operatorInfo
                          (rightExpr, remainingAfterRight) <-
                            parseCaseGuardExprWithMinPrecedence (Just operatorSymbol) nextMinPrecedence tokensAfterOperator
                          rejectNonAssociativeContinuation declaredOperators operatorInfo operatorToken remainingAfterRight
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
               && not (casePipeCanContinueExpression parentOperator minPrecedence leftExpr)
           )

    casePipeCanContinueExpression parentOperator minPrecedence leftExpr =
      case compare minPrecedence caseGuardPipePrecedence of
        LT -> not (leftExprHasBoundaryPrecedenceRoot leftExpr)
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

    -- If a pipe was stopped inside an RHS, keep treating it as a boundary when
    -- control returns to the outer tail.
    leftExprHasBoundaryPrecedenceRoot leftExpr =
      case leftExpr of
        SEBinary operatorSymbol _ _ ->
          case lookupOperatorInfoIn declaredOperators operatorSymbol of
            Just operatorInfo ->
              operatorPrecedence operatorInfo <= caseGuardPipePrecedence
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
        Right (_, Token {tokenKind = TOperator "|"} : _) ->
          startsDefiniteOrPatternCaseArm remainingTokens
        Left _
          | startsCasePatternTokens remainingTokens ->
              hasTopLevelArrowBeforeCaseArmBoundary remainingTokens
        _ -> False

    startsDefiniteUnguardedCaseArmAfterGuardBoundary remainingTokens =
      case parseCaseArmPattern remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} : _) ->
          guardBoundaryPatternIsDefinite casePattern
        _ -> False

    startsDefiniteGuardedCaseArmAfterGuardBoundary remainingTokens =
      case parseCaseArmPattern remainingTokens of
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

    startsDefiniteOrPatternCaseArm remainingTokens =
      case parseCaseArmPattern remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} : _) ->
          orPatternStartsDefiniteArmBoundary casePattern
        Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
          orPatternStartsDefiniteArmBoundary casePattern && guardTokensEndAtArrow afterGuard
        _ -> False

    startsAllLiteralOrPatternCaseArm remainingTokens =
      case parseCaseArmPattern remainingTokens of
        Right (casePattern, Token {tokenKind = TArrow} : _) ->
          orPatternIsAllLiteral casePattern
        Right (casePattern, Token {tokenKind = TIf} : afterGuard) ->
          orPatternIsAllLiteral casePattern && guardTokensEndAtArrow afterGuard
        _ -> False

    orPatternStartsDefiniteArmBoundary casePattern =
      case casePattern of
        SPOr alternatives ->
          any patternIsWildcard alternatives
            || all patternIsVariable alternatives
            || all patternIsLiteral alternatives
            || alternativesBindSameNames alternatives
            || ( case alternatives of
                   firstAlternative : _ -> patternCanStartOrArmBoundary firstAlternative
                   [] -> False
               )
        _ -> False

    orPatternIsAllLiteral casePattern =
      case casePattern of
        SPOr alternatives ->
          not (null alternatives) && all patternIsLiteral alternatives
        _ -> False

    patternCanStartOrArmBoundary casePattern =
      case casePattern of
        SPConstructor {} -> True
        SPList {} -> True
        SPConsList {} -> True
        SPTuple {} -> True
        SPAs {} -> True
        _ -> False

    patternIsWildcard casePattern =
      case casePattern of
        SPWildcard -> True
        _ -> False

    patternIsVariable casePattern =
      case casePattern of
        SPVariable {} -> True
        _ -> False

    patternIsLiteral casePattern =
      case casePattern of
        SPLiteral {} -> True
        _ -> False

    alternativesBindSameNames alternatives =
      case alternatives of
        [] -> False
        firstAlternative : rest ->
          let expectedNames = patternBinderNames firstAlternative
           in not (Set.null expectedNames)
                && all ((== expectedNames) . patternBinderNames) rest

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

    commonPatternBinderNames alternatives =
      case alternatives of
        [] -> Set.empty
        firstAlternative : rest ->
          foldl
            Set.intersection
            (patternBinderNames firstAlternative)
            (map patternBinderNames rest)

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

parseCaseArmPattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCaseArmPattern = Pattern.parseCaseArmPatternTokens

parseCasePattern :: [Token] -> Either Diagnostic (SurfacePattern, [Token])
parseCasePattern = Pattern.parseCasePatternTokens

startsCasePatternTokens :: [Token] -> Bool
startsCasePatternTokens tokens =
  case tokens of
    Token {tokenKind = TInt _} : _ -> True
    Token {tokenKind = TIdentifier _} : _ -> True
    Token {tokenKind = TLBracket} : _ -> True
    Token {tokenKind = TLParen} : _ -> True
    _ -> False

isConstructorIdentifierText :: Text -> Bool
isConstructorIdentifierText name =
  case Text.uncons name of
    Just (firstChar, _) -> isUpper firstChar
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
parseLambdaParameter = Pattern.parseLambdaParameterTokens

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
