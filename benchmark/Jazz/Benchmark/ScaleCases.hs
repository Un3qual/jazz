{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseBenchmarks,
    compilerScaleCaseEntryModulePath,
    compilerScaleCaseExpectedOutput,
    compilerScaleCaseIdentifier,
    compilerScaleCaseInterfaceWidth,
    compilerScaleCaseResolutionConfig,
    compilerScaleCaseScenario,
    compilerScaleCaseSize,
    compilerScaleCaseSource,
    compilerScaleCaseSourceCount,
    compilerScaleCases,
    selectCompilerScaleCases,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Profiling (BenchmarkGroup (..))
import System.FilePath ((<.>), (</>))

data CompilerScaleScenario
  = SequentialPolymorphicBindings
  | WideModuleFanout
  deriving (Eq, Ord, Show)

instance NFData CompilerScaleScenario where
  rnf scenario = scenario `seq` ()

data CompilerScaleCase = CompilerScaleCase
  { compilerScaleCaseIdentifier :: Text,
    compilerScaleCaseScenario :: CompilerScaleScenario,
    compilerScaleCaseSize :: Int,
    compilerScaleCaseInterfaceWidth :: Maybe Int,
    compilerScaleCaseBenchmarks :: [BenchmarkGroup],
    compilerScaleCaseEntryModulePath :: [Text],
    compilerScaleCaseResolutionConfig :: ModuleResolutionConfig,
    compilerScaleCaseSources :: Map FilePath Text,
    compilerScaleCaseExpectedOutput :: Text
  }
  deriving (Eq, Show)

instance NFData CompilerScaleCase where
  rnf programCase =
    rnf (compilerScaleCaseIdentifier programCase) `seq`
      rnf (compilerScaleCaseScenario programCase) `seq`
        rnf (compilerScaleCaseSize programCase) `seq`
          rnf (compilerScaleCaseInterfaceWidth programCase) `seq`
            forceBenchmarkGroups (compilerScaleCaseBenchmarks programCase) `seq`
              rnf (compilerScaleCaseEntryModulePath programCase) `seq`
                rnf (moduleRoots (compilerScaleCaseResolutionConfig programCase)) `seq`
                  rnf (moduleExtension (compilerScaleCaseResolutionConfig programCase)) `seq`
                    rnf (compilerScaleCaseSources programCase) `seq`
                      rnf (compilerScaleCaseExpectedOutput programCase)

forceBenchmarkGroups :: [BenchmarkGroup] -> ()
forceBenchmarkGroups = foldr (\benchmarkGroup forced -> benchmarkGroup `seq` forced) ()

compilerScaleCases :: [CompilerScaleCase]
compilerScaleCases =
  map sequentialPolymorphicCase [64, 128, 256, 512]
    <> map (`wideModuleFanoutCase` 16) [8, 16, 32, 64]

compilerScaleCaseSource :: CompilerScaleCase -> FilePath -> Maybe Text
compilerScaleCaseSource programCase path =
  Map.lookup path (compilerScaleCaseSources programCase)

compilerScaleCaseSourceCount :: CompilerScaleCase -> Int
compilerScaleCaseSourceCount = Map.size . compilerScaleCaseSources

selectCompilerScaleCases :: [Text] -> [CompilerScaleCase] -> Either Text [CompilerScaleCase]
selectCompilerScaleCases requestedIdentifiers programCases
  | null requestedIdentifiers = Right programCases
  | not (null missingIdentifiers) =
      Left ("unknown compiler scale case(s): " <> Text.intercalate ", " missingIdentifiers)
  | otherwise =
      Right
        ( filter
            ((`elem` requestedIdentifiers) . compilerScaleCaseIdentifier)
            programCases
        )
  where
    knownIdentifiers = map compilerScaleCaseIdentifier programCases
    missingIdentifiers = filter (`notElem` knownIdentifiers) requestedIdentifiers

scaleModuleRoot :: FilePath
scaleModuleRoot = "compiler-scale"

scaleResolutionConfig :: ModuleResolutionConfig
scaleResolutionConfig =
  ModuleResolutionConfig
    { moduleRoots = [scaleModuleRoot],
      moduleExtension = ".jz"
    }

sequentialPolymorphicCase :: Int -> CompilerScaleCase
sequentialPolymorphicCase bindingCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "sequential-polymorphic-bindings-" <> paddedDecimal 4 bindingCount,
      compilerScaleCaseScenario = SequentialPolymorphicBindings,
      compilerScaleCaseSize = bindingCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, ModulePreparationBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (sequentialPolymorphicSource bindingCount),
      compilerScaleCaseExpectedOutput = "(42, True)"
    }

sequentialPolymorphicSource :: Int -> Text
sequentialPolymorphicSource bindingCount =
  Text.unlines
    ( ["module Main {"]
        <> map ("  " <>) bindings
        <> ["  (" <> finalName <> " 42, " <> finalName <> " True).", "}"]
    )
  where
    bindings = map renderBinding [0 .. bindingCount - 1]
    finalName = bindingName (bindingCount - 1)
    renderBinding index
      | index == 0 = bindingName index <> " = \\(item) -> item."
      | otherwise =
          bindingName index
            <> " = \\(item) -> "
            <> bindingName (index - 1)
            <> " item."
    bindingName index = "identity" <> paddedDecimal 4 index

wideModuleFanoutCase :: Int -> Int -> CompilerScaleCase
wideModuleFanoutCase moduleCount interfaceWidth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "wide-module-fanout-"
          <> paddedDecimal 4 moduleCount
          <> "x"
          <> paddedDecimal 4 interfaceWidth,
      compilerScaleCaseScenario = WideModuleFanout,
      compilerScaleCaseSize = moduleCount,
      compilerScaleCaseInterfaceWidth = Just interfaceWidth,
      compilerScaleCaseBenchmarks = [ModulePreparationBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = wideModuleSources moduleCount interfaceWidth,
      compilerScaleCaseExpectedOutput = "0"
    }

wideModuleSources :: Int -> Int -> Map FilePath Text
wideModuleSources moduleCount interfaceWidth =
  Map.fromList
    ( (scaleModuleRoot </> "Main.jz", wideMainSource moduleCount)
        : [ ( scaleModuleRoot </> "Fanout" </> Text.unpack (moduleName moduleIndex) <.> "jz",
              wideInterfaceSource moduleIndex interfaceWidth
            )
          | moduleIndex <- [0 .. moduleCount - 1]
          ]
    )

wideMainSource :: Int -> Text
wideMainSource moduleCount =
  Text.unlines
    ( ["module Main {"]
        <> ["  import Fanout::" <> moduleName moduleIndex <> "." | moduleIndex <- [0 .. moduleCount - 1]]
        <> ["  " <> valueName 0 0 <> ".", "}"]
    )

wideInterfaceSource :: Int -> Int -> Text
wideInterfaceSource moduleIndex interfaceWidth =
  Text.unlines
    ( ["module Fanout::" <> moduleName moduleIndex <> " {"]
        <> [ "  "
               <> valueName moduleIndex valueIndex
               <> " = "
               <> Text.pack (show valueIndex)
               <> "."
           | valueIndex <- [0 .. interfaceWidth - 1]
           ]
        <> ["}"]
    )

moduleName :: Int -> Text
moduleName index = "Module" <> paddedDecimal 4 index

valueName :: Int -> Int -> Text
valueName moduleIndex valueIndex =
  "fanout"
    <> paddedDecimal 4 moduleIndex
    <> "Value"
    <> paddedDecimal 4 valueIndex

paddedDecimal :: Int -> Int -> Text
paddedDecimal width value = Text.justifyRight width '0' (Text.pack (show value))
