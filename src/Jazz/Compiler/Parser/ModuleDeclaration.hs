{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Module headers, exports, imports, and import-alias discovery.
module Jazz.Compiler.Parser.ModuleDeclaration
  ( ModuleBodyParser,
    parseModuleStatementParser,
    parseImportStatementParser,
    registerImportAliases,
    discoverModuleDeclarations,
    discoverModuleDeclarationsDetailed,
    operatorTokenPrefix,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import qualified Data.Text as Text
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
  )
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
    renderModuleExportSelector,
  )
import Jazz.Compiler.Name
  ( NameNamespace (..),
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context (ParserContext)
import Jazz.Compiler.Parser.DeclarationTokens
  ( isReservedLiteralName,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    ParserListKind (..),
    ParserNameRole (..),
    parserFailureDiagnostic,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    isImmediatelyAfter,
  )
import Jazz.Compiler.Parser.Operator (isValidUserOperatorSymbol)
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    foundToken,
    parseAnyToken,
    parseToken,
    peekToken,
    runTokenParserDetailed,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )
import Jazz.Compiler.SourceSpan (spanThrough)
import qualified Text.Megaparsec as MP

type ModuleBodyParser = Parser ([SurfaceStatement], ParserContext)

parseModuleStatementParser :: ModuleBodyParser -> Parser ([SurfaceStatement], ParserContext)
parseModuleStatementParser parseModuleBody = do
  header <- parseModulePrefix
  (statements, context) <- parseModuleBody
  pure (header : statements, context)

-- Header and import grammar is shared by discovery and ordinary parsing.
parseModulePrefix :: Parser SurfaceStatement
parseModulePrefix = do
  moduleToken <- parseToken TModule
  modulePath <- parseModulePath
  next <- peekToken
  moduleExports <- case next of
    Just Token {tokenKind = TLParen} -> parseAnyToken *> (Just <$> parseModuleExportList)
    _ -> pure Nothing
  beforeBody <- peekToken
  case beforeBody of
    Just Token {tokenKind = TLBrace} ->
      SSModule (tokenSpan moduleToken) modulePath moduleExports <$ parseAnyToken
    Nothing -> failTokenParserAt (tokenSpan moduleToken) (ExpectedSyntax "'{'" (ParserEndOfInputAfter "module path"))
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "'{'" (foundToken token))

-- | Discover only module-scope declarations, without parsing any expression or
-- assigning body nodes. Unbalanced or otherwise malformed bodies are left to
-- the full parser; only the shared header/import grammar can fail here.
discoverModuleDeclarations :: [Token] -> Either Diagnostic [SurfaceStatement]
discoverModuleDeclarations = Bifunctor.first parserFailureDiagnostic . discoverModuleDeclarationsDetailed

discoverModuleDeclarationsDetailed :: [Token] -> Either ParserFailure [SurfaceStatement]
discoverModuleDeclarationsDetailed = runTokenParserDetailed "module declarations" $ do
  firstToken <- peekToken
  case firstToken of
    Just Token {tokenKind = TModule} -> do
      header <- parseModulePrefix
      (header :) <$> scan True [] True
    _ -> scan False [] True
  where
    scan wrapped delimiters statementStart = do
      next <- peekToken
      case next of
        Nothing -> pure []
        Just Token {tokenKind = TRBrace} | wrapped && null delimiters -> do
          -- The full parser diagnoses any tokens after the module wrapper.
          _ <- MP.takeWhileP Nothing (const True)
          pure []
        Just Token {tokenKind = TImport} | null delimiters && statementStart -> do
          declaration <- parseImportStatementParser
          (declaration :) <$> scan wrapped [] True
        Just token -> do
          _ <- parseAnyToken
          let kind = tokenKind token
              nextDelimiters = case kind of
                TLParen -> TRParen : delimiters
                TLBracket -> TRBracket : delimiters
                TLBrace -> TRBrace : delimiters
                _ | expected : rest <- delimiters, kind == expected -> rest
                _ -> delimiters
              nextStart = null nextDelimiters && kind == TDot
          scan wrapped nextDelimiters nextStart

parseImportStatementParser :: Parser SurfaceStatement
parseImportStatementParser = do
  importToken <- parseToken TImport
  modulePath <- parseModulePath
  next <- peekToken
  case next of
    Just Token {tokenKind = TDot} ->
      SSImport (tokenSpan importToken) modulePath Nothing Nothing <$ parseAnyToken
    Just asToken@Token {tokenKind = TAs} -> do
      _ <- parseAnyToken
      alias <- peekToken
      case alias of
        Just aliasToken@Token {tokenKind = TIdentifier aliasName}
          | isReservedLiteralName aliasName ->
              failTokenParserAt (tokenSpan aliasToken) (DeclarationFailure (ReservedLiteralName ImportAlias aliasName))
          | otherwise -> do
              _ <- parseAnyToken
              afterAlias <- peekToken
              case afterAlias of
                Just parenToken@Token {tokenKind = TLParen} ->
                  failTokenParserAt (tokenSpan parenToken) (DeclarationFailure ImportAliasCombinedWithSymbolList)
                _ -> SSImport (tokenSpan importToken) modulePath (Just aliasName) Nothing <$ parseToken TDot
        Nothing -> failTokenParserAt (tokenSpan asToken) (ExpectedSyntax "import alias" (ParserEndOfInputAfter "'as'"))
        Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "import alias" (foundToken token))
    Just Token {tokenKind = TLParen} -> do
      _ <- parseAnyToken
      symbols <- parseImportSymbolList
      afterSymbols <- peekToken
      case afterSymbols of
        Just asToken@Token {tokenKind = TAs} ->
          failTokenParserAt (tokenSpan asToken) (DeclarationFailure ImportAliasCombinedWithSymbolList)
        _ -> SSImport (tokenSpan importToken) modulePath Nothing (Just symbols) <$ parseToken TDot
    Nothing -> failTokenParserAt (tokenSpan importToken) (ExpectedSyntax "'.', 'as', or '('" (ParserEndOfInputAfter "import path"))
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "'.', 'as', or '('" (foundToken token))

parseModulePath :: Parser [Text]
parseModulePath = do
  first <- peekToken
  case first of
    Nothing -> failTokenParser (ExpectedSyntax "module path" ParserEndOfInput)
    Just Token {tokenKind = TIdentifier firstSegment} -> parseAnyToken *> go [firstSegment]
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "module path segment" (foundToken token))
  where
    go reversedSegments = do
      remaining <- MP.getInput
      case remaining of
        Token {tokenKind = TColonColon} :< Token {tokenKind = TIdentifier segment} :< _ ->
          MP.takeP Nothing 2 *> go (segment : reversedSegments)
        separator@Token {tokenKind = TColonColon} :< EmptyTokens ->
          failTokenParserAt (tokenSpan separator) (ExpectedSyntax "module path segment" ParserEndOfInput)
        separator@Token {tokenKind = TColonColon} :< token :< _ ->
          failTokenParserAt
            (tokenSpan (if tokenKind token == TDot then separator else token))
            (ExpectedSyntax "module path segment" (foundToken token))
        _ -> pure (reverse reversedSegments)

parseImportSymbolList :: Parser [Text]
parseImportSymbolList = do
  next <- peekToken
  case next of
    Just token@Token {tokenKind = TRParen} ->
      failTokenParserAt (tokenSpan token) (ExpectedSyntax "at least one import symbol" (ParserBeforeToken TRParen ")" Nothing))
    _ -> NonEmpty.toList <$> parseNonEmptyUniqueList ImportSymbolList "import symbol list" (\name -> "'" <> name <> "'") parseImportSymbol

parseModuleExportList :: Parser [ModuleExportSelector]
parseModuleExportList = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TRParen} -> [] <$ parseAnyToken
    _ -> NonEmpty.toList <$> parseNonEmptyUniqueList ModuleExportList "module export list" renderModuleExportSelector parseModuleExport

parseNonEmptyUniqueList :: ParserListKind -> Text -> (item -> Text) -> Parser (item, SourceSpan) -> Parser (NonEmpty.NonEmpty item)
parseNonEmptyUniqueList listKind description renderItem parseItem = do
  (first, _) <- parseItem
  go (first NonEmpty.:| []) (Set.singleton (renderItem first))
  where
    go reversedItems seen = do
      next <- peekToken
      case next of
        Just Token {tokenKind = TComma} -> do
          _ <- parseAnyToken
          (item, spanValue) <- parseItem
          let key = renderItem item
          if Set.member key seen
            then failTokenParserAt spanValue (DeclarationFailure (DuplicateListItem listKind key))
            else go (item NonEmpty.<| reversedItems) (Set.insert key seen)
        Just Token {tokenKind = TRParen} -> NonEmpty.reverse reversedItems <$ parseAnyToken
        Nothing -> failTokenParser (ExpectedSyntax "')'" (ParserEndOfInputIn description))
        Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "',' or ')'" (foundToken token))

parseModuleExport :: Parser (ModuleExportSelector, SourceSpan)
parseModuleExport = do
  tokens <- MP.getInput
  namespace <- case tokens of
    Token {tokenKind = TValue} :< _ -> Just ValueNamespace <$ parseAnyToken
    Token {tokenKind = TIdentifier prefix} :< next :< _
      | Just prefixNamespace <- moduleExportNamespacePrefix prefix,
        isNamespaceTarget next ->
          Just prefixNamespace <$ parseAnyToken
    _ -> pure Nothing
  (name, spanValue) <- parseExportName
  if isParenthesizedOperator name && isJust namespace && namespace /= Just ValueNamespace
    then failTokenParserAt spanValue (ExpectedSyntax "ordinary name for this export namespace" (ParserFoundToken TLParen name))
    else case namespace of
      Just TypeNamespace -> parseTypeModuleExport name spanValue
      _ -> pure (ModuleExportSelector namespace (LocatedModuleExportName name spanValue), spanValue)

isNamespaceTarget :: Token -> Bool
isNamespaceTarget token = case tokenKind token of
  TIdentifier {} -> True
  TLParen -> True
  _ -> False

parseExportName :: Parser (Text, SourceSpan)
parseExportName = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TLParen} :< _ -> parseParenthesizedOperator True
    nameToken@Token {tokenKind = TIdentifier name} :< colonToken@Token {tokenKind = TColonColon} :< memberToken@Token {tokenKind = TIdentifier member} :< _
      | isImmediatelyAfter nameToken colonToken,
        isImmediatelyAfter colonToken memberToken ->
          (name <> "::" <> member, spanThrough (tokenSpan nameToken) (tokenSpan memberToken)) <$ MP.takeP Nothing 3
    Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< _ ->
      (name, spanValue) <$ parseAnyToken
    EmptyTokens -> failTokenParser (ExpectedSyntax "module export name" ParserEndOfInput)
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "module export name" (foundToken token))

isParenthesizedOperator :: Text -> Bool
isParenthesizedOperator = Text.isPrefixOf "("

parseParenthesizedOperator :: Bool -> Parser (Text, SourceSpan)
parseParenthesizedOperator allowQualified = do
  _ <- parseToken TLParen
  tokens <- MP.getInput
  case operatorTokenPrefix tokens of
    Just (operatorToken@Token {tokenKind = TOperator symbol}, count, _)
      | (allowQualified || count == 1),
        isValidUserOperatorSymbol (snd (Text.breakOnEnd "::" symbol)) -> do
          _ <- MP.takeP Nothing count
          _ <- parseToken TRParen
          pure ("(" <> symbol <> ")", tokenSpan operatorToken)
    _ -> do
      next <- peekToken
      case next of
        Nothing -> failTokenParser (ExpectedSyntax "custom operator" ParserEndOfInput)
        Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "custom operator" (foundToken token))

-- | View an adjacent alias-qualified operator as the same Text payload used by
-- an unqualified operator. The token retains the complete authored name span.
operatorTokenPrefix :: TokenStream -> Maybe (Token, Int, TokenStream)
operatorTokenPrefix tokens = case tokens of
  operatorToken@Token {tokenKind = TOperator {}} :< rest -> Just (operatorToken, 1, rest)
  aliasToken@Token {tokenKind = TIdentifier alias} :< colonToken@Token {tokenKind = TColonColon} :< operatorToken@Token {tokenKind = TOperator symbol} :< rest
    | isImmediatelyAfter aliasToken colonToken,
      isImmediatelyAfter colonToken operatorToken ->
        let name = alias <> "::" <> symbol
         in Just (operatorToken {tokenKind = TOperator name, tokenLexeme = name, tokenSpan = spanThrough (tokenSpan aliasToken) (tokenSpan operatorToken)}, 3, rest)
  _ -> Nothing

parseTypeModuleExport :: Text -> SourceSpan -> Parser (ModuleExportSelector, SourceSpan)
parseTypeModuleExport name spanValue = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TLParen} -> do
      _ <- parseAnyToken
      afterOpener <- peekToken
      case afterOpener of
        Just token@Token {tokenKind = TRParen} ->
          failTokenParserAt (tokenSpan token) (ExpectedSyntax "'..' or at least one constructor export" (ParserAtToken TRParen ")"))
        Just dotToken@Token {tokenKind = TDot} ->
          parseAnyToken *> parseAllTypeConstructors name spanValue (tokenSpan dotToken)
        _ -> do
          constructors <- parseNonEmptyUniqueList ConstructorExportList "constructor export group" (\located -> "'" <> locatedModuleExportName located <> "'") parseLocatedModuleExportName
          pure (ModuleTypeExportSelector name spanValue (SelectedTypeConstructors constructors), spanValue)
    _ -> pure (ModuleTypeExportSelector name spanValue AbstractType, spanValue)

parseAllTypeConstructors :: Text -> SourceSpan -> SourceSpan -> Parser (ModuleExportSelector, SourceSpan)
parseAllTypeConstructors name typeSpan allSpan = do
  tokens <- MP.getInput
  case tokens of
    Token {tokenKind = TDot} :< Token {tokenKind = TRParen} :< _ ->
      (ModuleTypeExportSelector name typeSpan (AllTypeConstructors allSpan), typeSpan) <$ MP.takeP Nothing 2
    token :< _ -> failTokenParserAt (tokenSpan token) (DeclarationFailure ConstructorExportGroupRequiresAll)
    EmptyTokens -> failTokenParserAt allSpan (DeclarationFailure ConstructorExportGroupRequiresAll)

parseLocatedModuleExportName :: Parser (LocatedModuleExportName, SourceSpan)
parseLocatedModuleExportName = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TIdentifier name, tokenSpan = spanValue} ->
      (LocatedModuleExportName name spanValue, spanValue) <$ parseAnyToken
    Nothing -> failTokenParser (ExpectedSyntax "constructor export" ParserEndOfInput)
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "constructor export name" (foundToken token))

moduleExportNamespacePrefix :: Text -> Maybe NameNamespace
moduleExportNamespacePrefix prefix = case prefix of
  "constructor" -> Just ConstructorNamespace
  "type" -> Just TypeNamespace
  "class" -> Just CapabilityNamespace
  _ -> Nothing

parseImportSymbol :: Parser (Text, SourceSpan)
parseImportSymbol = do
  next <- peekToken
  case next of
    Just Token {tokenKind = TIdentifier name, tokenSpan = spanValue} ->
      (name, spanValue) <$ parseAnyToken
    Just Token {tokenKind = TLParen} -> parseParenthesizedOperator False
    Nothing -> failTokenParser (ExpectedSyntax "import symbol" ParserEndOfInput)
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "import symbol" (foundToken token))

registerImportAliases :: Set Text -> [SurfaceStatement] -> Set Text
registerImportAliases =
  foldl' registerImportAlias
  where
    registerImportAlias knownAliases statement =
      case statement of
        SSImport _ _ (Just aliasName) Nothing -> Set.insert aliasName knownAliases
        _ -> knownAliases
