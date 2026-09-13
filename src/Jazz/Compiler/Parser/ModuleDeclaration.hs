{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Module headers, exports, imports, and import-alias discovery.
module Jazz.Compiler.Parser.ModuleDeclaration
  ( ModuleBodyParser,
    parseModuleStatementParser,
    parseImportStatementParser,
    registerImportAliases,
    collectImportAliasesUntilEnd,
    collectImportAliasesUntilBrace,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Data.Text
  ( Text,
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
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
import Jazz.Compiler.Parser.DeclarationTokens
  ( isReservedLiteralName,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserEncountered (..),
    ParserFailureReason (..),
    ParserListKind (..),
    ParserNameRole (..),
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    foundToken,
    parseAnyToken,
    parseToken,
    peekToken,
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )
import qualified Text.Megaparsec as MP

type ModuleBodyParser = Parser [SurfaceStatement]

parseModuleStatementParser :: ModuleBodyParser -> Parser [SurfaceStatement]
parseModuleStatementParser parseModuleBody = do
  moduleToken <- parseToken TModule
  modulePath <- parseModulePath
  next <- peekToken
  moduleExports <- case next of
    Just Token {tokenKind = TLParen} -> parseAnyToken *> (Just <$> parseModuleExportList)
    _ -> pure Nothing
  beforeBody <- peekToken
  case beforeBody of
    Just Token {tokenKind = TLBrace} -> do
      _ <- parseAnyToken
      (SSModule (tokenSpan moduleToken) modulePath moduleExports :) <$> parseModuleBody
    Nothing -> failTokenParserAt (tokenSpan moduleToken) (ExpectedSyntax "'{'" (ParserEndOfInputAfter "module path"))
    Just token -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "'{'" (foundToken token))

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
  case tokens of
    Token {tokenKind = TValue} :< Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< _ ->
      (ModuleExportSelector (Just ValueNamespace) name, spanValue) <$ MP.takeP Nothing 2
    Token {tokenKind = TIdentifier prefix} :< Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< _
      | Just TypeNamespace <- moduleExportNamespacePrefix prefix ->
          MP.takeP Nothing 2 *> parseTypeModuleExport name spanValue
      | Just namespace <- moduleExportNamespacePrefix prefix ->
          (ModuleExportSelector (Just namespace) name, spanValue) <$ MP.takeP Nothing 2
    Token {tokenKind = TIdentifier name, tokenSpan = spanValue} :< _ ->
      (ModuleExportSelector Nothing name, spanValue) <$ parseAnyToken
    EmptyTokens -> failTokenParser (ExpectedSyntax "module export name" ParserEndOfInput)
    token :< _ -> failTokenParserAt (tokenSpan token) (ExpectedSyntax "module export name" (foundToken token))

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

collectImportAliasesUntilEnd :: TokenStream -> Set Text
collectImportAliasesUntilEnd = collectImportAliasesInStatementList False

collectImportAliasesUntilBrace :: TokenStream -> Set Text
collectImportAliasesUntilBrace = collectImportAliasesInStatementList True

collectImportAliasesInStatementList :: Bool -> TokenStream -> Set Text
collectImportAliasesInStatementList stopAtRightBrace = go (0 :: Int) Set.empty
  where
    go _ aliases EmptyTokens = aliases
    go depth aliases (token :< rest)
      | stopAtRightBrace && depth == 0 && tokenKind token == TRBrace = aliases
      | otherwise =
          case tokenKind token of
            TImport
              | depth == 0 ->
                  go depth (maybe aliases (`Set.insert` aliases) (collectImportAlias rest)) rest
            TLBrace -> go (depth + 1) aliases rest
            TRBrace -> go (max 0 (depth - 1)) aliases rest
            _ -> go depth aliases rest

    collectImportAlias importTail =
      case importTail of
        EmptyTokens -> Nothing
        Token {tokenKind = TDot} :< _ -> Nothing
        Token {tokenKind = TAs} :< Token {tokenKind = TIdentifier aliasName} :< _ -> Just aliasName
        _ :< rest -> collectImportAlias rest
