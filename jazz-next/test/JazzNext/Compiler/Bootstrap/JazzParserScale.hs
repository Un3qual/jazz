{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserScale,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( RunResult,
    runModuleGraphObserved,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (VText),
    renderRuntimeValue,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

runJazzParserScale :: RuntimeObservationRequest -> Int -> IO RunResult
runJazzParserScale observationRequest bindingCount =
  runModuleGraphObserved
    observationRequest
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    entrySource =
      Text.replace
        "__SOURCE__"
        (renderRuntimeValue (VText (generatedParserProgram bindingCount)))
        """
        module App::Main {
          import LexerTypes (CanonicalSourcePath).
          import List (listLength).
          import Parser (parseSource).
          import ParserTypes (CanonicalSourceSuccess, CanonicalSourceLexicalFailure, CanonicalSourceParserFailure, BlockExpression).
          case parseSource (CanonicalSourcePath "fixtures/parser/generated-scale.jz") __SOURCE__ {
            | CanonicalSourceSuccess _ expression -> case expression {
              | BlockExpression statements -> listLength statements
              | other -> 0
            }
            | CanonicalSourceLexicalFailure _ _ -> 0
            | CanonicalSourceParserFailure _ _ -> 0
          }.
        }

        """
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just entrySource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

generatedParserProgram :: Int -> Text
generatedParserProgram bindingCount =
  Text.unlines (map renderBinding [0 .. bindingCount - 1] <> [renderTerminal])
  where
    renderBinding index =
      Text.replace
        "__INDEX__"
        (Text.pack (show index))
        """
        value__INDEX__ = combine [__INDEX__, True, "item"] (__INDEX__, False).
        """
    renderTerminal =
      Text.replace
        "__FINAL_INDEX__"
        (Text.pack (show (bindingCount - 1)))
        """
        value__FINAL_INDEX__.
        """

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
