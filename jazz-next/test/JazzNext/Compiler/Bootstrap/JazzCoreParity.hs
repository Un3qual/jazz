{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedFoundationBatchRendering,
    runJazzFoundationBatch,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalCoreComparison
  ( canonicalCoreExprRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( surfaceExprRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
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

expectedFoundationResultRuntimeValue :: SurfaceExpr -> Either Text RuntimeValue
expectedFoundationResultRuntimeValue expression =
  canonicalConstructor "Just" . pure
    <$> canonicalCoreExprRuntimeValue (lowerSurfaceExpr expression)

runJazzFoundationBatch :: [SurfaceExpr] -> IO RunResult
runJazzFoundationBatch expressions =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    entrySource =
      Text.replace
        "__CALLS__"
        (Text.intercalate ",\n    " (map renderCall expressions))
        """
        module App::Main {
          import CoreLower (lowerFoundationExpression).
          import LexerTypes (CanonicalSpan).
          import Maybe.
          import NonEmpty.
          import ParserTypes.
          [ __CALLS__
          ].
        }

        """
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just entrySource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

renderCall :: SurfaceExpr -> Text
renderCall expression =
  "lowerFoundationExpression "
    <> renderJazzRuntimeValue (surfaceExprRuntimeValue expression)

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
