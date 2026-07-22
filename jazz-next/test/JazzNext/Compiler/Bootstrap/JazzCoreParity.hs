{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    expectedCanonicalExpressionBatchRendering,
    expectedCoreCorpusRendering,
    expectedCoreSourceBatchRendering,
    expectedModuleBatchRendering,
    expectedControlFlowPatternsBatchRendering,
    expectedSignaturesDeclarationsOperatorsBatchRendering,
    expectedFoundationSourceBatchRendering,
    expectedControlFlowPatternsSourceBatchRendering,
    expectedSignaturesDeclarationsOperatorsSourceBatchRendering,
    expectedParserSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzCanonicalExpressionBatch,
    runJazzCoreCorpus,
    runJazzCoreSourceBatch,
    runJazzModuleBatch,
    runJazzControlFlowPatternsSourceBatch,
    runJazzFoundationBatch,
    runJazzFoundationSourceBatch,
    runJazzParserSourceBatch,
    runJazzSignaturesDeclarationsOperatorsBatch,
    runJazzSignaturesDeclarationsOperatorsSourceBatch,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalCoreComparison
  ( canonicalCoreExprRuntimeValue,
    canonicalCoreSourceResultRuntimeValue,
    canonicalCoreModuleResultRuntimeValue,
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
    lowerSurfaceModuleDetailed,
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

expectedCanonicalExpressionBatchRendering :: [SurfaceExpr] -> Either Text Text
expectedCanonicalExpressionBatchRendering expressions =
  renderRuntimeValue . (`VList` Nothing)
    <$> mapM (canonicalCoreExprRuntimeValue . lowerSurfaceExpr) expressions

expectedModuleBatchRendering :: [(FilePath, [Text], SurfaceExpr)] -> Either Text Text
expectedModuleBatchRendering inputs =
  renderRuntimeValue . (`VList` Nothing) <$> mapM expectedModuleResult inputs
  where
    expectedModuleResult (sourcePath, expectedPath, expression) =
      canonicalCoreModuleResultRuntimeValue
        (lowerSurfaceModuleDetailed sourcePath expectedPath expression)

expectedCoreSourceBatchRendering :: [(FilePath, [Text], Text)] -> Either Text Text
expectedCoreSourceBatchRendering inputs =
  renderRuntimeValue . (`VList` Nothing) <$> mapM expectedSourceResult inputs
  where
    expectedSourceResult (sourcePath, expectedPath, source) = do
      canonicalSourcePath <- normalizeCanonicalSourcePath sourcePath
      canonicalCoreSourceResultRuntimeValue
        canonicalSourcePath
        (fmap (fmap (lowerSurfaceModuleDetailed sourcePath expectedPath)) (sourceResult source))

expectedCoreCorpusRendering :: [(FilePath, [Text], Text)] -> Either Text Text
expectedCoreCorpusRendering = expectedCoreSourceBatchRendering

expectedControlFlowPatternsBatchRendering :: [SurfaceExpr] -> Either Text Text
expectedControlFlowPatternsBatchRendering = expectedFoundationBatchRendering

expectedSignaturesDeclarationsOperatorsBatchRendering :: [SurfaceExpr] -> Either Text Text
expectedSignaturesDeclarationsOperatorsBatchRendering = expectedFoundationBatchRendering

expectedFoundationResultRuntimeValue :: SurfaceExpr -> Either Text RuntimeValue
expectedFoundationResultRuntimeValue expression =
  canonicalConstructor "Just" . pure
    <$> canonicalCoreExprRuntimeValue (lowerSurfaceExpr expression)

expectedFoundationSourceBatchRendering :: [Text] -> Either Text Text
expectedFoundationSourceBatchRendering sources = do
  expressions <- mapM parseFoundationSource sources
  expectedFoundationBatchRendering expressions

expectedControlFlowPatternsSourceBatchRendering :: [Text] -> Either Text Text
expectedControlFlowPatternsSourceBatchRendering = expectedFoundationSourceBatchRendering

expectedSignaturesDeclarationsOperatorsSourceBatchRendering :: [Text] -> Either Text Text
expectedSignaturesDeclarationsOperatorsSourceBatchRendering = expectedFoundationSourceBatchRendering

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
runJazzFoundationBatch = runJazzLoweringBatch "lowerFoundationExpression"

runJazzCanonicalExpressionBatch :: [SurfaceExpr] -> IO RunResult
runJazzCanonicalExpressionBatch = runJazzLoweringBatch "lowerCanonicalExpression"

runJazzModuleBatch :: [(FilePath, [Text], SurfaceExpr)] -> IO RunResult
runJazzModuleBatch inputs =
  runGeneratedBatch
    """
    import CoreLower (lowerModule).
    import LexerTypes (CanonicalSourcePath, CanonicalSpan).
    import Maybe.
    import NonEmpty.
    import ParserTypes.
    """
    (map renderModuleCall inputs)

runJazzCoreSourceBatch :: [(FilePath, [Text], Text)] -> IO RunResult
runJazzCoreSourceBatch inputs =
  runGeneratedBatch
    """
    import Core (lowerCoreSource).
    import LexerTypes (CanonicalSourcePath).
    """
    (map renderCoreSourceCall inputs)

runJazzCoreCorpus :: [(FilePath, [Text], Text)] -> IO RunResult
runJazzCoreCorpus = runJazzCoreSourceBatch

runJazzControlFlowPatternsBatch :: [SurfaceExpr] -> IO RunResult
runJazzControlFlowPatternsBatch = runJazzLoweringBatch "lowerControlFlowPatternsExpression"

runJazzSignaturesDeclarationsOperatorsBatch :: [SurfaceExpr] -> IO RunResult
runJazzSignaturesDeclarationsOperatorsBatch = runJazzLoweringBatch "lowerSignaturesDeclarationsOperatorsExpression"

runJazzLoweringBatch :: Text -> [SurfaceExpr] -> IO RunResult
runJazzLoweringBatch loweringFunction expressions =
  runGeneratedBatch
    ( Text.replace
        "__LOWER__"
        loweringFunction
        """
    import CoreLower (__LOWER__).
    import LexerTypes (CanonicalSpan).
    import Maybe.
    import NonEmpty.
    import ParserTypes.
    """
    )
    (map (renderLoweringCall loweringFunction) expressions)

runJazzFoundationSourceBatch :: [Text] -> IO RunResult
runJazzFoundationSourceBatch = runJazzLoweringSourceBatch "lowerFoundationExpression"

runJazzControlFlowPatternsSourceBatch :: [Text] -> IO RunResult
runJazzControlFlowPatternsSourceBatch = runJazzLoweringSourceBatch "lowerControlFlowPatternsExpression"

runJazzSignaturesDeclarationsOperatorsSourceBatch :: [Text] -> IO RunResult
runJazzSignaturesDeclarationsOperatorsSourceBatch =
  runJazzLoweringSourceBatch "lowerSignaturesDeclarationsOperatorsExpression"

runJazzLoweringSourceBatch :: Text -> [Text] -> IO RunResult
runJazzLoweringSourceBatch loweringFunction sources =
  runGeneratedBatch
    ( Text.replace
        "__LOWER__"
        loweringFunction
        """
    import CoreLower (__LOWER__).
    import LexerTypes (CanonicalSourcePath).
    import Maybe.
    import Parser (parseSource).
    import ParserTypes (
      CanonicalSourceSuccess,
      CanonicalSourceLexicalFailure,
      CanonicalSourceParserFailure
    ).
    """
    )
    (map (renderLoweringSourceCall loweringFunction) sources)

runJazzParserSourceBatch :: [Text] -> IO RunResult
runJazzParserSourceBatch sources =
  runGeneratedBatch
    """
    import LexerTypes (CanonicalSourcePath).
    import Parser (parseSource).
    """
    (map renderParserSourceCall sources)

renderLoweringCall :: Text -> SurfaceExpr -> Text
renderLoweringCall loweringFunction expression =
  loweringFunction
    <> " "
    <> renderJazzRuntimeValue (surfaceExprRuntimeValue expression)

renderModuleCall :: (FilePath, [Text], SurfaceExpr) -> Text
renderModuleCall (sourcePath, expectedPath, expression) =
  "lowerModule (CanonicalSourcePath "
    <> renderJazzRuntimeValue (VText (Text.pack sourcePath))
    <> ") "
    <> renderJazzRuntimeValue (VList (map VText expectedPath) Nothing)
    <> " "
    <> renderJazzRuntimeValue (surfaceExprRuntimeValue expression)

renderCoreSourceCall :: (FilePath, [Text], Text) -> Text
renderCoreSourceCall (sourcePath, expectedPath, source) =
  "lowerCoreSource (CanonicalSourcePath "
    <> renderJazzRuntimeValue (VText (Text.pack sourcePath))
    <> ") "
    <> renderJazzRuntimeValue (VList (map VText expectedPath) Nothing)
    <> " "
    <> renderJazzRuntimeValue (VText source)

renderLoweringSourceCall :: Text -> Text -> Text
renderLoweringSourceCall loweringFunction source =
  Text.replace
    "__LOWER__"
    loweringFunction
    ( Text.replace
        "__SOURCE__"
        (renderJazzRuntimeValue (VText source))
        """
    case parseSource (CanonicalSourcePath "fixtures/core/foundation.jz") __SOURCE__ {
      | CanonicalSourceSuccess path expression -> __LOWER__ expression
      | CanonicalSourceLexicalFailure path failure -> Nothing
      | CanonicalSourceParserFailure path failure -> Nothing
    }
    """
    )

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
