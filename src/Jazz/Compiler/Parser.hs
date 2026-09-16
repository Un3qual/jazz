{-# LANGUAGE OverloadedStrings #-}

-- | Public façade for the surface parser. Expression and declaration grammar
-- live in their owner modules; this module only ties their recursive callbacks
-- together and threads scope-local parser context.
module Jazz.Compiler.Parser
  ( parseStatementsUntilBrace,
    parseSurfaceProgram,
    parseSurfaceProgramTokens,
    parseSurfaceProgramTokensDetailed,
    parseSurfaceProgramTokensWithContextDetailed,
  )
where

import Data.Bifunctor (first)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Context
  ( ParserContext (..),
    StatementBlockParser,
    initialParserContext,
  )
import Jazz.Compiler.Parser.Declaration
  ( parseStatementParser,
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
import Jazz.Compiler.Parser.ModuleDeclaration
  ( discoverModuleDeclarationsDetailed,
    registerImportAliases,
  )
import Jazz.Compiler.Parser.Operator (OperatorInfo, declaredOperatorsSince)
import Jazz.Compiler.Parser.TokenParser
  ( Parser,
    failTokenParser,
    failTokenParserAt,
    parseAnyToken,
    peekToken,
    runTokenParserDetailed,
    withConsumedSpan,
  )

type StatementParser = ParserContext -> Parser ([SurfaceStatement], ParserContext)

parseSurfaceProgram :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceProgram source = do
  tokens <- tokenize source
  parseSurfaceProgramTokens tokens

-- | Parse a complete surface program from an already-tokenized stream. This
-- entrypoint keeps lexing and parsing as independently measurable phases.
parseSurfaceProgramTokens :: [Token] -> Either Diagnostic SurfaceExpr
parseSurfaceProgramTokens =
  first parserFailureDiagnostic . parseSurfaceProgramTokensDetailed

parseSurfaceProgramTokensDetailed :: [Token] -> Either ParserFailure SurfaceExpr
parseSurfaceProgramTokensDetailed tokens = do
  declarations <- discoverModuleDeclarationsDetailed tokens
  let context = initialParserContext {parserKnownAliases = registerImportAliases Set.empty declarations}
  fst <$> parseSurfaceProgramTokensWithContextDetailed context tokens

-- | Parse a body once with the aliases and operator fixities selected by its
-- dependency resolver. The result retains only this source unit's fixities.
parseSurfaceProgramTokensWithContextDetailed :: ParserContext -> [Token] -> Either ParserFailure (SurfaceExpr, [OperatorInfo])
parseSurfaceProgramTokensWithContextDetailed context tokens =
  {-# SCC "jazz-stage:parsing" #-}
  runTokenParserDetailed "program" programParser tokens
  where
    suppliedOperators = parserDeclaredOperators context
    expressionParser = parseExpressionParser blockParser
    statementParser = parseStatementParser suppliedOperators expressionParser blockParser
    blockParser = parseStatementsUntilBrace statementParser
    programParser = withConsumedSpan (\spanValue (expression, operators) -> (expression {surfaceExprSpan = spanValue}, operators)) $ do
      maybeFirstToken <- peekToken
      (statements, finalContext) <- parseProgramStatements statementParser context
      pure
        ( SurfaceExpr
            (maybe (SourceSpan 1 1) tokenSpan maybeFirstToken)
            (SEBlock statements),
          declaredOperatorsSince suppliedOperators (parserDeclaredOperators finalContext)
        )

-- | Stable prefix parser retained for callers that parse an expression from an
-- already-tokenized stream.
parseProgramStatements :: StatementParser -> ParserContext -> Parser ([SurfaceStatement], ParserContext)
parseProgramStatements parseStatement context =
  go False [] context
  where
    go seenPriorTopLevelForm reversedStatements currentContext = do
      maybeToken <- peekToken
      case maybeToken of
        Nothing -> pure (reverse reversedStatements, currentContext)
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
                      pure (reverse (reversePrepend statements reversedStatements), nextContext)
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
parseStatementsUntilBrace parseStatement context =
  go [] context
  where
    go reversedStatements currentContext = do
      maybeToken <- peekToken
      case maybeToken of
        Nothing -> failTokenParser (ExpectedSyntax "'}'" ParserEndOfInput)
        Just Token {tokenKind = TRBrace} -> do
          _ <- parseAnyToken
          pure (reverse reversedStatements, currentContext)
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
