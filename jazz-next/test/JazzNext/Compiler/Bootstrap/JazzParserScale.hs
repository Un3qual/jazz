{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.JazzParserScale
  ( runJazzParserDeclarationsScale,
    runJazzParserScale,
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

runJazzParserDeclarationsScale :: RuntimeObservationRequest -> IO RunResult
runJazzParserDeclarationsScale observationRequest =
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
        (renderRuntimeValue (VText generatedDeclarationsProgram))
        """
        module App::Main {
          import LexerTypes (CanonicalSourcePath).
          import List (listFoldLeft, listLength).
          import Maybe (Just, Nothing).
          import Parser (parseSource).
          import ParserTypes (
            CanonicalSourceSuccess,
            CanonicalSourceLexicalFailure,
            CanonicalSourceParserFailure,
            BlockExpression,
            QualifiedVariableExpression,
            ModuleStatement,
            SignatureStatement,
            LetStatement,
            DataStatement,
            ImportStatement
          ).
          case parseSource (CanonicalSourcePath "fixtures/parser/generated-declarations-scale.jz") __SOURCE__ {
            | CanonicalSourceSuccess _ expression -> case expression {
              | BlockExpression statements -> {
                counts = listFoldLeft (\\(counts, statement) -> case counts {
                  | (modules, signatures, bindings, dataDeclarations, imports) -> case statement {
                    | ModuleStatement _ _ _ -> (modules + 1, signatures, bindings, dataDeclarations, imports)
                    | SignatureStatement _ _ _ -> (modules, signatures + 1, bindings, dataDeclarations, imports)
                    | LetStatement _ _ expression -> case expression {
                      | QualifiedVariableExpression _ _ -> (modules, signatures, bindings + 1, dataDeclarations, imports)
                      | other -> counts
                    }
                    | DataStatement _ _ _ _ -> (modules, signatures, bindings, dataDeclarations + 1, imports)
                    | ImportStatement _ _ _ _ -> (modules, signatures, bindings, dataDeclarations, imports + 1)
                    | other -> counts
                  }
                }) (0, 0, 0, 0, 0) statements.
                case counts {
                  | (1, 128, 128, 128, 128) -> listLength statements
                  | other -> 0
                }.
              }
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

generatedDeclarationsProgram :: Text
generatedDeclarationsProgram =
  Text.unlines
    ( ["module Generated::Scale {"]
        <> concatMap renderSignatureBinding [0 .. declarationCount - 1]
        <> map renderDataDeclaration [0 .. declarationCount - 1]
        <> map renderImport [0 .. declarationCount - 1]
        <> ["}"]
    )
  where
    declarationCount :: Int
    declarationCount = 128
    replaceIndex template index =
      Text.replace "__INDEX__" (Text.pack (show index)) template
    renderSignatureBinding index =
      [ replaceIndex "value__INDEX__ :: Int." index,
        replaceIndex "value__INDEX__ = Alias__INDEX__::item." index
      ]
    renderDataDeclaration index =
      replaceIndex "data Type__INDEX__ a__INDEX__ = Constructor__INDEX__ a__INDEX__." index
    renderImport index =
      replaceIndex "import Lib::Module__INDEX__ as Alias__INDEX__." index

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
