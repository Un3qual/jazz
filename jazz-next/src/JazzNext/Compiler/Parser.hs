{-# LANGUAGE OverloadedStrings #-}

-- | Public façade for the surface parser. Expression and declaration grammar
-- live in their owner modules; this module only ties their recursive callbacks
-- together and threads scope-local parser context.
module JazzNext.Compiler.Parser
  ( parseStatementsUntilBrace,
    parseSurfaceExpressionTokens,
    parseSurfaceProgram,
    parseSurfaceProgramTokens
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
    renderSourceSpan
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Context
  ( ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
    initialParserContext
  )
import JazzNext.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseStatementParser
  )
import JazzNext.Compiler.Parser.Expression (parseExpressionParser)
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize
  )
import JazzNext.Compiler.Parser.Operator (OperatorInfo)
import JazzNext.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    parseAnyToken,
    peekToken,
    runTokenParser,
    runTokenParserPrefix
  )
import qualified Text.Megaparsec as MP

type StatementParser = ParserContext -> Parser ([SurfaceStatement], ParserContext)

parseSurfaceProgram :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgram source = do
  tokens <- tokenize source
  parseSurfaceProgramTokens tokens

-- | Parse a complete surface program from an already-tokenized stream. This
-- entrypoint keeps lexing and parsing as independently measurable phases.
parseSurfaceProgramTokens :: [Token] -> Either Diagnostic SurfaceExpr
parseSurfaceProgramTokens tokens =
  {-# SCC "jazz-stage:parsing" #-}
  runTokenParser "program" programParser tokens
  where
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser
    programParser =
      SEBlock <$> parseProgramStatements statementParser initialParserContext

-- | Stable prefix parser retained for callers that parse an expression from an
-- already-tokenized stream.
parseSurfaceExpressionTokens ::
  Set Text ->
  [OperatorInfo] ->
  [Token] ->
  Either Diagnostic (SurfaceExpr, [Token])
parseSurfaceExpressionTokens knownAliases declaredOperators =
  runTokenParserPrefix "expression" (expressionParser expressionContext)
  where
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser
    expressionContext =
      ParserContext
        { parserKnownAliases = knownAliases,
          parserDeclaredOperators = declaredOperators,
          parserStatementContext = NestedBlockContext
        }

parseProgramStatements :: StatementParser -> ParserContext -> Parser [SurfaceStatement]
parseProgramStatements parseStatement context = do
  tokens <- MP.lookAhead MP.getInput
  let scopeContext =
        context
          { parserKnownAliases =
              Set.union
                (parserKnownAliases context)
                (collectImportAliasesUntilEnd tokens)
          }
  go False [] scopeContext
  where
    go seenPriorTopLevelForm reversedStatements currentContext = do
      maybeToken <- peekToken
      case maybeToken of
        Nothing -> pure (reverse reversedStatements)
        Just _ -> do
          (statements, nextContext) <- parseStatement currentContext
          case leadingModuleDeclaration statements of
            Just moduleSpan
              | seenPriorTopLevelForm ->
                  failTokenParser
                    ( "module declaration must be the first top-level form at "
                        <> renderSourceSpan moduleSpan
                    )
              | otherwise -> do
                  trailingToken <- peekToken
                  case trailingToken of
                    Nothing ->
                      pure (reverse (reversePrepend statements reversedStatements))
                    Just token ->
                      failTokenParser
                        ( "unexpected token '"
                            <> tokenLexeme token
                            <> "' at "
                            <> renderSourceSpan (tokenSpan token)
                            <> " after module declaration"
                        )
            Nothing ->
              go
                True
                (reversePrepend statements reversedStatements)
                nextContext

parseStatementsUntilBrace :: StatementParser -> StatementBlockParser
parseStatementsUntilBrace parseStatement context = do
  tokens <- MP.lookAhead MP.getInput
  let scopeContext =
        context
          { parserKnownAliases =
              Set.union
                (parserKnownAliases context)
                (collectImportAliasesUntilBrace tokens)
          }
  go [] scopeContext
  where
    go reversedStatements currentContext = do
      maybeToken <- peekToken
      case maybeToken of
        Nothing -> failTokenParser "expected '}' before end of input"
        Just Token {tokenKind = TRBrace} -> do
          _ <- parseAnyToken
          pure (reverse reversedStatements)
        Just _ -> do
          (statements, nextContext) <- parseStatement currentContext
          go (reversePrepend statements reversedStatements) nextContext

reversePrepend :: [a] -> [a] -> [a]
reversePrepend values reversedValues =
  foldl (flip (:)) reversedValues values

leadingModuleDeclaration :: [SurfaceStatement] -> Maybe SourceSpan
leadingModuleDeclaration statements =
  case statements of
    SSModule spanValue _ _ : _ -> Just spanValue
    _ -> Nothing
