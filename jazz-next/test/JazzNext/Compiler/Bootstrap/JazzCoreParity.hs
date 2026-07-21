{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    expectedControlFlowPatternsBatchRendering,
    expectedFoundationSourceBatchRendering,
    expectedParserSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzFoundationBatch,
    runJazzFoundationSourceBatch,
    runJazzParserSourceBatch,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalCoreComparison
  ( canonicalCoreExprRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( canonicalSourceResultRuntimeValue,
    canonicalizeSourceResult,
    surfaceExprRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( CanonicalSourcePath,
    canonicalConstructor,
    normalizeCanonicalSourcePath,
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
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr,
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgramTokensDetailed,
  )
import JazzNext.Compiler.Parser.Failure
  ( ParserFailure,
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure,
    tokenizeDetailed,
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
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

expectedFoundationBatchRendering :: [SurfaceExpr] -> Either Text Text
expectedFoundationBatchRendering expressions =
  renderRuntimeValue . (`VList` Nothing)
    <$> mapM expectedFoundationResultRuntimeValue expressions

expectedControlFlowPatternsBatchRendering :: [SurfaceExpr] -> Either Text Text
expectedControlFlowPatternsBatchRendering = expectedFoundationBatchRendering

expectedFoundationResultRuntimeValue :: SurfaceExpr -> Either Text RuntimeValue
expectedFoundationResultRuntimeValue expression =
  canonicalConstructor "Just" . pure
    <$> canonicalCoreExprRuntimeValue (lowerSurfaceExpr expression)

expectedFoundationSourceBatchRendering :: [Text] -> Either Text Text
expectedFoundationSourceBatchRendering sources = do
  expressions <- mapM parseFoundationSource sources
  expectedFoundationBatchRendering expressions

expectedParserSourceBatchRendering :: [Text] -> Either Text Text
expectedParserSourceBatchRendering sources = do
  sourcePath <- foundationSourcePath
  pure
    ( renderRuntimeValue
        ( VList
            ( map
                ( canonicalSourceResultRuntimeValue
                    . canonicalizeSourceResult sourcePath
                    . sourceResult
                )
                sources
            )
            Nothing
        )
    )

runJazzFoundationBatch :: [SurfaceExpr] -> IO RunResult
runJazzFoundationBatch expressions =
  runGeneratedBatch
    """
    import CoreLower (lowerFoundationExpression).
    import LexerTypes (CanonicalSpan).
    import Maybe.
    import NonEmpty.
    import ParserTypes.
    """
    (map renderCall expressions)

runJazzControlFlowPatternsBatch :: [SurfaceExpr] -> IO RunResult
runJazzControlFlowPatternsBatch expressions =
  runGeneratedBatch
    """
    import CoreLower (lowerControlFlowPatternsExpression).
    import LexerTypes (CanonicalSpan).
    import Maybe.
    import NonEmpty.
    import ParserTypes.
    """
    (map renderControlFlowPatternsCall expressions)

runJazzFoundationSourceBatch :: [Text] -> IO RunResult
runJazzFoundationSourceBatch sources =
  runGeneratedBatch
    """
    import CoreLower (lowerFoundationExpression).
    import LexerTypes (CanonicalSourcePath).
    import Maybe.
    import Parser (parseSource).
    import ParserTypes (
      CanonicalSourceSuccess,
      CanonicalSourceLexicalFailure,
      CanonicalSourceParserFailure
    ).
    """
    (map renderFoundationSourceCall sources)

runJazzParserSourceBatch :: [Text] -> IO RunResult
runJazzParserSourceBatch sources =
  runGeneratedBatch
    """
    import LexerTypes (CanonicalSourcePath).
    import Parser (parseSource).
    """
    (map renderParserSourceCall sources)

renderCall :: SurfaceExpr -> Text
renderCall expression =
  "lowerFoundationExpression "
    <> renderJazzRuntimeValue (surfaceExprRuntimeValue expression)

renderControlFlowPatternsCall :: SurfaceExpr -> Text
renderControlFlowPatternsCall expression =
  "lowerControlFlowPatternsExpression "
    <> renderJazzRuntimeValue (surfaceExprRuntimeValue expression)

renderFoundationSourceCall :: Text -> Text
renderFoundationSourceCall source =
  Text.replace
    "__SOURCE__"
    (renderJazzRuntimeValue (VText source))
    """
    case parseSource (CanonicalSourcePath "fixtures/core/foundation.jz") __SOURCE__ {
      | CanonicalSourceSuccess path expression -> lowerFoundationExpression expression
      | CanonicalSourceLexicalFailure path failure -> Nothing
      | CanonicalSourceParserFailure path failure -> Nothing
    }
    """

renderParserSourceCall :: Text -> Text
renderParserSourceCall source =
  "parseSource (CanonicalSourcePath \"fixtures/core/foundation.jz\") "
    <> renderJazzRuntimeValue (VText source)

runGeneratedBatch :: Text -> [Text] -> IO RunResult
runGeneratedBatch imports calls =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    entrySource =
      Text.replace
        "__IMPORTS__"
        imports
        ( Text.replace
            "__CALLS__"
            (Text.intercalate ",\n    " calls)
            """
            module App::Main {
              __IMPORTS__
              [ __CALLS__
              ].
            }

            """
        )
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just entrySource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

parseFoundationSource :: Text -> Either Text SurfaceExpr
parseFoundationSource source =
  case sourceResult source of
    Left lexicalFailure -> Left ("foundation source failed lexing: " <> Text.pack (show lexicalFailure))
    Right (Left parserFailure) -> Left ("foundation source failed parsing: " <> Text.pack (show parserFailure))
    Right (Right expression) -> Right expression

sourceResult :: Text -> Either LexicalFailure (Either ParserFailure SurfaceExpr)
sourceResult source =
  case tokenizeDetailed source of
    Left failure -> Left failure
    Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)

foundationSourcePath :: Either Text CanonicalSourcePath
foundationSourcePath = normalizeCanonicalSourcePath "fixtures/core/foundation.jz"

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
    _ -> error "unsupported runtime value in generated Jazz core fixture"

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
