{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Module headers, exports, imports, and import-alias discovery.
module Jazz.Compiler.Parser.ModuleDeclaration
  ( ModuleBodyParser,
    parseModuleStatementFromTokens,
    parseImportStatementFromTokens,
    registerImportAliases,
    collectImportAliasesUntilEnd,
    collectImportAliasesUntilBrace,
    rejectNestedModuleDeclaration,
    rejectNestedImportDeclaration,
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
  ( consumeDot,
    isReservedLiteralName,
  )
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    ParserListKind (..),
    ParserNameRole (..),
    parserFailure,
    parserFailureAt,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    pattern EmptyTokens,
    pattern (:<),
  )

type ModuleBodyParser = TokenStream -> Either ParserFailure ([SurfaceStatement], TokenStream)

parseModuleStatementFromTokens ::
  ModuleBodyParser ->
  TokenStream ->
  Either ParserFailure ([SurfaceStatement], TokenStream)
parseModuleStatementFromTokens parseModuleBody tokens =
  case tokens of
    moduleToken@Token {tokenKind = TModule} :< tokensAfterModuleKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
      (moduleExports, beforeModuleBody) <-
        case afterModulePath of
          Token {tokenKind = TLParen} :< afterLeftParen -> do
            (exportNames, remaining) <- parseModuleExportList afterLeftParen
            pure (Just exportNames, remaining)
          _ -> pure (Nothing, afterModulePath)
      case beforeModuleBody of
        Token {tokenKind = TLBrace} :< tokensAfterLeftBrace -> do
          (bodyStatements, remaining) <- parseModuleBody tokensAfterLeftBrace
          pure
            ( SSModule (tokenSpan moduleToken) modulePath moduleExports
                : bodyStatements,
              remaining
            )
        EmptyTokens ->
          Left
            ( parserFailureAt
                (tokenSpan moduleToken)
                (ExpectedSyntax "'{'" (ParserEndOfInputAfter "module path"))
            )
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'{'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "'module'" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'module'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseImportStatementFromTokens :: TokenStream -> Either ParserFailure (SurfaceStatement, TokenStream)
parseImportStatementFromTokens tokens =
  case tokens of
    importToken@Token {tokenKind = TImport} :< tokensAfterImportKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterImportKeyword
      parseImportTail importToken modulePath afterModulePath
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "'import'" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "'import'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseImportTail :: Token -> [Text] -> TokenStream -> Either ParserFailure (SurfaceStatement, TokenStream)
parseImportTail importToken modulePath tokensAfterModulePath =
  case tokensAfterModulePath of
    Token {tokenKind = TDot} :< rest ->
      pure (SSImport (tokenSpan importToken) modulePath Nothing Nothing, rest)
    asToken@Token {tokenKind = TAs} :< rest ->
      case rest of
        aliasToken@Token {tokenKind = TIdentifier aliasName} :< afterAlias
          | isReservedLiteralName aliasName ->
              Left
                ( parserFailureAt
                    (tokenSpan aliasToken)
                    (DeclarationFailure (ReservedLiteralName ImportAlias aliasName))
                )
          | otherwise ->
              case afterAlias of
                parenToken@Token {tokenKind = TLParen} :< _ ->
                  Left
                    ( parserFailureAt
                        (tokenSpan parenToken)
                        (DeclarationFailure ImportAliasCombinedWithSymbolList)
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
        EmptyTokens ->
          Left
            ( parserFailureAt
                (tokenSpan asToken)
                (ExpectedSyntax "import alias" (ParserEndOfInputAfter "'as'"))
            )
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "import alias" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )
    Token {tokenKind = TLParen} :< rest -> do
      (symbols, afterSymbols) <- parseImportSymbolList rest
      case afterSymbols of
        asToken@Token {tokenKind = TAs} :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan asToken)
                (DeclarationFailure ImportAliasCombinedWithSymbolList)
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
    EmptyTokens ->
      Left
        ( parserFailureAt
            (tokenSpan importToken)
            (ExpectedSyntax "'.', 'as', or '('" (ParserEndOfInputAfter "import path"))
        )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "'.', 'as', or '('"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseModulePath :: TokenStream -> Either ParserFailure ([Text], TokenStream)
parseModulePath tokens =
  case tokens of
    EmptyTokens -> Left (parserFailure (ExpectedSyntax "module path" ParserEndOfInput))
    Token {tokenKind = TIdentifier firstSegment} :< rest ->
      go [firstSegment] rest
      where
        go revSegments allTokens =
          case allTokens of
            Token {tokenKind = TColonColon} :< Token {tokenKind = TIdentifier nextSegment} :< remaining ->
              go (nextSegment : revSegments) remaining
            separatorToken@Token {tokenKind = TColonColon} :< EmptyTokens ->
              Left
                ( parserFailureAt
                    (tokenSpan separatorToken)
                    (ExpectedSyntax "module path segment" ParserEndOfInput)
                )
            separatorToken@Token {tokenKind = TColonColon} :< token :< _
              | tokenKind token == TDot ->
                  Left
                    ( parserFailureAt
                        (tokenSpan separatorToken)
                        ( ExpectedSyntax
                            "module path segment"
                            (ParserFoundToken (tokenKind token) (tokenLexeme token))
                        )
                    )
              | otherwise ->
                  Left
                    ( parserFailureAt
                        (tokenSpan token)
                        ( ExpectedSyntax
                            "module path segment"
                            (ParserFoundToken (tokenKind token) (tokenLexeme token))
                        )
                    )
            _ -> Right (reverse revSegments, allTokens)
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            ( ExpectedSyntax
                "module path segment"
                (ParserFoundToken (tokenKind token) (tokenLexeme token))
            )
        )

parseImportSymbolList :: TokenStream -> Either ParserFailure ([Text], TokenStream)
parseImportSymbolList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@Token {tokenKind = TRParen} :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "at least one import symbol" (ParserBeforeToken TRParen ")" Nothing))
        )
    _ ->
      parseNonEmptyUniqueList
        ImportSymbolList
        "import symbol list"
        (\name -> "'" <> name <> "'")
        parseImportSymbol
        tokensAfterLeftParen

parseModuleExportList :: TokenStream -> Either ParserFailure ([ModuleExportSelector], TokenStream)
parseModuleExportList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} :< rest -> Right ([], rest)
    _ ->
      parseNonEmptyUniqueList
        ModuleExportList
        "module export list"
        renderModuleExportSelector
        parseModuleExport
        tokensAfterLeftParen

parseNonEmptyUniqueList ::
  ParserListKind ->
  Text ->
  (item -> Text) ->
  (TokenStream -> Either ParserFailure (item, SourceSpan, TokenStream)) ->
  TokenStream ->
  Either ParserFailure ([item], TokenStream)
parseNonEmptyUniqueList listKind listDescription renderItem parseItem tokens = do
  (firstItem, _, afterFirstItem) <- parseItem tokens
  go [firstItem] (Set.singleton (renderItem firstItem)) afterFirstItem
  where
    go reversedItems seenItems allTokens =
      case allTokens of
        Token {tokenKind = TComma} :< rest -> do
          (nextItem, itemSpan, afterNextItem) <- parseItem rest
          let nextItemKey = renderItem nextItem
          if Set.member nextItemKey seenItems
            then
              Left
                ( parserFailureAt
                    itemSpan
                    (DeclarationFailure (DuplicateListItem listKind (renderItem nextItem)))
                )
            else
              go
                (nextItem : reversedItems)
                (Set.insert nextItemKey seenItems)
                afterNextItem
        Token {tokenKind = TRParen} :< rest -> Right (reverse reversedItems, rest)
        EmptyTokens ->
          Left
            (parserFailure (ExpectedSyntax "')'" (ParserEndOfInputIn listDescription)))
        token :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "',' or ')'" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
            )

parseModuleExport :: TokenStream -> Either ParserFailure (ModuleExportSelector, SourceSpan, TokenStream)
parseModuleExport tokens =
  case tokens of
    Token {tokenKind = TValue}
      :< Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan}
      :< rest ->
        Right
          ( ModuleExportSelector (Just ValueNamespace) exportName exportSpan,
            exportSpan,
            rest
          )
    Token {tokenKind = TIdentifier prefix} :< Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} :< rest
      | Just TypeNamespace <- moduleExportNamespacePrefix prefix ->
          parseTypeModuleExport exportName exportSpan rest
      | Just namespace <- moduleExportNamespacePrefix prefix ->
          Right (ModuleExportSelector (Just namespace) exportName exportSpan, exportSpan, rest)
    Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} :< rest ->
      Right (ModuleExportSelector Nothing exportName exportSpan, exportSpan, rest)
    EmptyTokens -> Left (parserFailure (ExpectedSyntax "module export name" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "module export name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

parseTypeModuleExport :: Text -> SourceSpan -> TokenStream -> Either ParserFailure (ModuleExportSelector, SourceSpan, TokenStream)
parseTypeModuleExport typeName typeSpan tokens =
  case tokens of
    Token {tokenKind = TLParen} :< afterLeftParen ->
      case afterLeftParen of
        token@Token {tokenKind = TRParen} :< _ ->
          Left
            ( parserFailureAt
                (tokenSpan token)
                (ExpectedSyntax "'..' or at least one constructor export" (ParserAtToken TRParen ")"))
            )
        dotToken@Token {tokenKind = TDot} :< afterFirstDot ->
          parseAllTypeConstructors typeName typeSpan (tokenSpan dotToken) afterFirstDot
        _ -> do
          (constructors, remaining) <-
            parseNonEmptyUniqueList
              ConstructorExportList
              "constructor export group"
              (\locatedName -> "'" <> locatedModuleExportName locatedName <> "'")
              parseLocatedModuleExportName
              afterLeftParen
          case NonEmpty.nonEmpty constructors of
            Nothing ->
              Left
                (parserFailureAt typeSpan (ExpectedSyntax "at least one constructor export" ParserImplicitBoundary))
            Just nonEmptyConstructors ->
              Right
                ( ModuleTypeExportSelector typeName typeSpan (SelectedTypeConstructors nonEmptyConstructors),
                  typeSpan,
                  remaining
                )
    _ -> Right (ModuleTypeExportSelector typeName typeSpan AbstractType, typeSpan, tokens)

parseAllTypeConstructors :: Text -> SourceSpan -> SourceSpan -> TokenStream -> Either ParserFailure (ModuleExportSelector, SourceSpan, TokenStream)
parseAllTypeConstructors typeName typeSpan allSpan tokens =
  case tokens of
    Token {tokenKind = TDot} :< Token {tokenKind = TRParen} :< rest ->
      Right
        ( ModuleTypeExportSelector typeName typeSpan (AllTypeConstructors allSpan),
          typeSpan,
          rest
        )
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (DeclarationFailure ConstructorExportGroupRequiresAll)
        )
    EmptyTokens ->
      Left (parserFailureAt allSpan (DeclarationFailure ConstructorExportGroupRequiresAll))

parseLocatedModuleExportName :: TokenStream -> Either ParserFailure (LocatedModuleExportName, SourceSpan, TokenStream)
parseLocatedModuleExportName tokens =
  case tokens of
    Token {tokenKind = TIdentifier constructorName, tokenSpan = constructorSpan} :< rest ->
      Right (LocatedModuleExportName constructorName constructorSpan, constructorSpan, rest)
    EmptyTokens -> Left (parserFailure (ExpectedSyntax "constructor export" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "constructor export name" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

moduleExportNamespacePrefix :: Text -> Maybe NameNamespace
moduleExportNamespacePrefix prefix =
  case prefix of
    "constructor" -> Just ConstructorNamespace
    "type" -> Just TypeNamespace
    "class" -> Just CapabilityNamespace
    _ -> Nothing

parseImportSymbol :: TokenStream -> Either ParserFailure (Text, SourceSpan, TokenStream)
parseImportSymbol tokens =
  case tokens of
    Token {tokenKind = TIdentifier symbolName, tokenSpan = symbolSpan} :< rest ->
      Right (symbolName, symbolSpan, rest)
    EmptyTokens ->
      Left (parserFailure (ExpectedSyntax "import symbol" ParserEndOfInput))
    token :< _ ->
      Left
        ( parserFailureAt
            (tokenSpan token)
            (ExpectedSyntax "import symbol" (ParserFoundToken (tokenKind token) (tokenLexeme token)))
        )

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

rejectNestedModuleDeclaration :: Token -> Either ParserFailure a
rejectNestedModuleDeclaration moduleToken =
  Left
    ( parserFailureAt
        (tokenSpan moduleToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope ModuleDeclaration))
    )

rejectNestedImportDeclaration :: Token -> Either ParserFailure a
rejectNestedImportDeclaration importToken =
  Left
    ( parserFailureAt
        (tokenSpan importToken)
        (DeclarationFailure (DeclarationOutsideAllowedScope ImportDeclaration))
    )
