{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzParserParity
  ( expectedSourceBatchRendering,
    expectedTokenBatchRendering,
    loadExpressionFoundationFixtures,
    runJazzParserSourceBatch,
    runJazzParserTokenBatch,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalLexResult (CanonicalLexSuccess),
    CanonicalSourcePath,
    CanonicalToken,
    canonicalTokenRuntimeValue,
    canonicalizeLexResult,
    normalizeCanonicalSourcePath,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( canonicalParserResultRuntimeValue,
    canonicalSourceResultRuntimeValue,
    canonicalizeParserResult,
    canonicalizeSourceResult,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalSourcePathRuntimeValue,
  )
import JazzNext.Compiler.Driver
  ( RunResult,
    runModuleGraph,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Name
  ( IdentifierLike (identifierText),
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgramTokensDetailed,
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr,
  )
import JazzNext.Compiler.Parser.Failure
  ( ParserFailure,
  )
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    ParserFixtureFamily (ExpressionFoundation),
    lookupParserFixtureFamily,
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure,
    Token,
    tokenizeDetailed,
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

data TokenFixture = TokenFixture CanonicalSourcePath [Token] [CanonicalToken]

loadExpressionFoundationFixtures :: IO [ParserFixture]
loadExpressionFoundationFixtures =
  case lookupParserFixtureFamily ExpressionFoundation of
    Left violations -> fail ("invalid expression-foundation fixture manifest: " <> show violations)
    Right fixtures -> pure fixtures

expectedTokenBatchRendering :: [ParserFixture] -> IO Text
expectedTokenBatchRendering fixtures = do
  tokenFixtures <- loadTokenFixtures fixtures
  pure
    ( renderRuntimeValue
        ( VList
            ( map
                (\(TokenFixture path tokens _) ->
                   canonicalParserResultRuntimeValue
                     (canonicalizeParserResult path (parseSurfaceProgramTokensDetailed tokens))
                )
                tokenFixtures
            )
            Nothing
        )
    )

expectedSourceBatchRendering :: [ParserFixture] -> IO Text
expectedSourceBatchRendering fixtures = do
  values <- mapM expectedSourceFixture fixtures
  pure (renderRuntimeValue (VList values Nothing))
  where
    expectedSourceFixture fixture = do
      path <- normalizeFixturePath fixture
      pure
        ( canonicalSourceResultRuntimeValue
            (canonicalizeSourceResult path (sourceResult fixture))
        )

runJazzParserTokenBatch :: [ParserFixture] -> IO RunResult
runJazzParserTokenBatch fixtures = do
  tokenFixtures <- loadTokenFixtures fixtures
  runParserBatch (map renderTokenCall tokenFixtures)

runJazzParserSourceBatch :: [ParserFixture] -> IO RunResult
runJazzParserSourceBatch fixtures = do
  sourceFixtures <- mapM sourceFixture fixtures
  runParserBatch (map renderSourceCall sourceFixtures)
  where
    sourceFixture fixture = do
      path <- normalizeFixturePath fixture
      pure (path, parserFixtureSource fixture)

loadTokenFixtures :: [ParserFixture] -> IO [TokenFixture]
loadTokenFixtures fixtures = catMaybes <$> mapM loadTokenFixture fixtures
  where
    loadTokenFixture fixture =
      case tokenizeDetailed (parserFixtureSource fixture) of
        Left _ -> pure Nothing
        Right tokens -> do
          path <- normalizeFixturePath fixture
          case canonicalizeLexResult path (Right tokens) of
            CanonicalLexSuccess _ canonicalTokens ->
              pure (Just (TokenFixture path tokens canonicalTokens))
            _ -> fail "canonical token conversion rejected a successful token stream"

sourceResult :: ParserFixture -> Either LexicalFailure (Either ParserFailure SurfaceExpr)
sourceResult fixture =
  case tokenizeDetailed (parserFixtureSource fixture) of
    Left failure -> Left failure
    Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)

normalizeFixturePath :: ParserFixture -> IO CanonicalSourcePath
normalizeFixturePath fixture =
  case normalizeCanonicalSourcePath (parserFixturePath fixture) of
    Left message -> fail (Text.unpack message)
    Right path -> pure path

renderTokenCall :: TokenFixture -> Text
renderTokenCall (TokenFixture path _ tokens) =
  Text.replace
    "__TOKENS__"
    (renderJazzRuntimeValue (VList (map canonicalTokenRuntimeValue tokens) Nothing))
    ( Text.replace
        "__PATH__"
        (renderJazzRuntimeValue (canonicalSourcePathRuntimeValue path))
        """
        parseTokens __PATH__ __TOKENS__
        """
    )

renderSourceCall :: (CanonicalSourcePath, Text) -> Text
renderSourceCall (path, source) =
  Text.replace
    "__SOURCE__"
    (renderJazzRuntimeValue (VText source))
    ( Text.replace
        "__PATH__"
        (renderJazzRuntimeValue (canonicalSourcePathRuntimeValue path))
        """
        parseSource __PATH__ __SOURCE__
        """
    )

renderJazzRuntimeValue :: RuntimeValue -> Text
renderJazzRuntimeValue value =
  case value of
    VInt {} -> renderRuntimeValue value
    VBool {} -> renderRuntimeValue value
    VChar {} -> renderRuntimeValue value
    VText {} -> renderRuntimeValue value
    VList elements _ ->
      "[" <> Text.intercalate ", " (map renderJazzRuntimeValue elements) <> "]"
    VTuple elements ->
      "(" <> Text.intercalate ", " (map renderJazzRuntimeValue elements) <> ")"
    VConstructor _ _ constructorName _ arguments ->
      case arguments of
        [] -> identifierText constructorName
        _ ->
          "("
            <> identifierText constructorName
            <> " "
            <> Text.intercalate " " (map renderJazzRuntimeValue arguments)
            <> ")"
    _ -> error "unsupported runtime value in generated Jazz parser fixture"

runParserBatch :: [Text] -> IO RunResult
runParserBatch calls =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    entrySource =
      Text.replace
        "__CALLS__"
        (Text.intercalate ",\n    " calls)
        """
        module App::Main {
          import LexerTypes.
          import Parser (parseSource, parseTokens).
          [ __CALLS__
          ].
        }

        """
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just entrySource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
