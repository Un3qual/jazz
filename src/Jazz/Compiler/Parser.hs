{-# LANGUAGE OverloadedStrings #-}

-- | Public façade for the surface parser. Expression and declaration grammar
-- live in their owner modules; this module only ties their recursive callbacks
-- together and threads scope-local parser context.
module Jazz.Compiler.Parser
  ( parseStatementsUntilBrace,
    parseSurfaceExpressionTokens,
    parseSurfaceProgram,
    parseSurfaceProgramTokens,
    parseSurfaceProgramTokensDetailed,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context
  ( ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
    initialParserContext,
  )
import Jazz.Compiler.Parser.Declaration
  ( collectImportAliasesUntilBrace,
    collectImportAliasesUntilEnd,
    parseStatementParser,
  )
import Jazz.Compiler.Parser.Expression (parseExpressionParser)
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserEncountered (..),
    ParserFailure,
    ParserFailureReason (..),
    parserFailureDiagnostic,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
    tokenize,
  )
import Jazz.Compiler.Parser.Operator
  ( OperatorInfo,
    operatorTableFromDeclarations
  )
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    peekToken,
    runTokenParserDetailed,
    runTokenParserPrefix,
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
parseSurfaceProgramTokens =
  mapLeft parserFailureDiagnostic . parseSurfaceProgramTokensDetailed

parseSurfaceProgramTokensDetailed :: [Token] -> Either ParserFailure SurfaceExpr
parseSurfaceProgramTokensDetailed tokens =
  {-# SCC "jazz-stage:parsing" #-}
  runTokenParserDetailed "program" programParser tokens
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
          parserDeclaredOperators = operatorTableFromDeclarations declaredOperators,
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
                  failTokenParserAt
                    moduleSpan
                    (DeclarationFailure ModuleMustBeFirstTopLevelForm)
              | otherwise -> do
                  trailingToken <- peekToken
                  case trailingToken of
                    Nothing ->
                      pure (reverse (reversePrepend statements reversedStatements))
                    Just token ->
                      failTokenParserAt
                        (tokenSpan token)
                        ( UnexpectedSyntaxAfter
                            (ParserFoundToken (tokenKind token) (tokenLexeme token))
                            "module declaration"
                        )
            Nothing ->
              go
                True
                (reversePrepend statements reversedStatements)
                nextContext

parseStatementsUntilBrace :: StatementParser -> StatementBlockParser
parseStatementsUntilBrace parseStatement context = do
  scopeContext <-
    case parserStatementContext context of
      NestedBlockContext -> pure context
      _ -> do
        tokens <- MP.lookAhead MP.getInput
        pure
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
        Nothing -> failTokenParser (ExpectedSyntax "'}'" ParserEndOfInput)
        Just Token {tokenKind = TRBrace} -> do
          _ <- parseAnyToken
          pure (reverse reversedStatements)
        Just _ -> do
          (statements, nextContext) <- parseStatement currentContext
          go (reversePrepend statements reversedStatements) nextContext

reversePrepend :: [a] -> [a] -> [a]
reversePrepend values reversedValues =
  foldl' (flip (:)) reversedValues values

leadingModuleDeclaration :: [SurfaceStatement] -> Maybe SourceSpan
leadingModuleDeclaration statements =
  case statements of
    SSModule spanValue _ _ : _ -> Just spanValue
    _ -> Nothing

mapLeft :: (errorA -> errorB) -> Either errorA value -> Either errorB value
mapLeft transform result =
  case result of
    Left failure -> Left (transform failure)
    Right value -> Right value
