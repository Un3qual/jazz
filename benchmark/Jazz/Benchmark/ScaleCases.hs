{-# LANGUAGE OverloadedStrings #-}

module Jazz.Benchmark.ScaleCases
  ( CompilerScaleCase,
    CompilerScaleScenario (..),
    compilerScaleCaseBenchmarks,
    compilerScaleCaseEntryModulePath,
    compilerScaleCaseEntrySource,
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
import System.FilePath (joinPath, (<.>), (</>))

data CompilerScaleScenario
  = SequentialPolymorphicBindings
  | WideModuleFanout
  | InterleavedRecursiveGroups
  | ConstrainedSignatures
  | DeepNestedLambdas
  | LargeOperatorTables
  | NestedBlocks
  | LongTokenStream
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
    <> map interleavedRecursiveGroupsCase [16, 32, 64, 128]
    <> map constrainedSignaturesCase [32, 64, 128, 256]
    <> map deepNestedLambdasCase [16, 32, 64, 128]
    <> map largeOperatorTablesCase [16, 32, 64, 128]
    <> map nestedBlocksCase [16, 32, 64, 128]
    <> map longTokenStreamCase [1024, 4096, 16384, 65536]

compilerScaleCaseSource :: CompilerScaleCase -> FilePath -> Maybe Text
compilerScaleCaseSource programCase path =
  Map.lookup path (compilerScaleCaseSources programCase)

compilerScaleCaseEntrySource :: CompilerScaleCase -> Maybe Text
compilerScaleCaseEntrySource programCase =
  findEntrySource (moduleRoots resolutionConfig)
  where
    resolutionConfig = compilerScaleCaseResolutionConfig programCase
    relativePath = joinPath (map Text.unpack (compilerScaleCaseEntryModulePath programCase))
    extension = moduleExtension resolutionConfig
    findEntrySource [] = Nothing
    findEntrySource (root : remainingRoots) =
      case compilerScaleCaseSource programCase ((root </> relativePath) <> extension) of
        Nothing -> findEntrySource remainingRoots
        source -> source

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

interleavedRecursiveGroupsCase :: Int -> CompilerScaleCase
interleavedRecursiveGroupsCase groupCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "interleaved-recursive-groups-" <> paddedDecimal 4 groupCount,
      compilerScaleCaseScenario = InterleavedRecursiveGroups,
      compilerScaleCaseSize = groupCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, ModulePreparationBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (interleavedRecursiveGroupsSource groupCount),
      compilerScaleCaseExpectedOutput = "(1, True)"
    }

interleavedRecursiveGroupsSource :: Int -> Text
interleavedRecursiveGroupsSource groupCount =
  Text.unlines
    ( ["module Main {"]
        <> concatMap renderGroup [0 .. groupCount - 1]
        <> [ "  ("
               <> recursiveBindingName "late" finalGroup
               <> ", "
               <> recursiveBindingName "early" finalGroup
               <> ").",
             "}"
           ]
    )
  where
    finalGroup = groupCount - 1
    renderGroup groupIndex =
      [ "  "
          <> recursiveBindingName "left" groupIndex
          <> " = if True then \\(item) -> item else "
          <> recursiveBindingName "right" groupIndex
          <> ".",
        "  "
          <> recursiveBindingName "early" groupIndex
          <> " = "
          <> recursiveBindingName "left" groupIndex
          <> " True.",
        "  "
          <> recursiveBindingName "right" groupIndex
          <> " = if False then \\(item) -> item else "
          <> recursiveBindingName "left" groupIndex
          <> ".",
        "  "
          <> recursiveBindingName "late" groupIndex
          <> " = "
          <> recursiveBindingName "left" groupIndex
          <> " 1."
      ]

recursiveBindingName :: Text -> Int -> Text
recursiveBindingName prefix groupIndex = prefix <> paddedDecimal 4 groupIndex

constrainedSignaturesCase :: Int -> CompilerScaleCase
constrainedSignaturesCase signatureCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "constrained-signatures-" <> paddedDecimal 4 signatureCount,
      compilerScaleCaseScenario = ConstrainedSignatures,
      compilerScaleCaseSize = signatureCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (constrainedSignaturesSource signatureCount),
      compilerScaleCaseExpectedOutput = "(1, True)"
    }

constrainedSignaturesSource :: Int -> Text
constrainedSignaturesSource signatureCount =
  Text.unlines
    ( [ "module Main {",
        "  class ScaleConstraint(a) { }.",
        "  impl ScaleConstraint(Int) { }.",
        "  impl ScaleConstraint(Bool) { }."
      ]
        <> concatMap renderBinding [0 .. signatureCount - 1]
        <> [ "  ("
               <> finalName
               <> " 1, "
               <> finalName
               <> " True).",
             "}"
           ]
    )
  where
    finalName = constrainedBindingName (signatureCount - 1)
    renderBinding index =
      [ "  "
          <> constrainedBindingName index
          <> " :: @{ScaleConstraint(a)}: a -> a.",
        "  "
          <> constrainedBindingName index
          <> " = \\(item) -> item."
      ]

constrainedBindingName :: Int -> Text
constrainedBindingName index = "constrained" <> paddedDecimal 4 index

deepNestedLambdasCase :: Int -> CompilerScaleCase
deepNestedLambdasCase depth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "deep-nested-lambdas-" <> paddedDecimal 4 depth,
      compilerScaleCaseScenario = DeepNestedLambdas,
      compilerScaleCaseSize = depth,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, ModulePreparationBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (deepNestedLambdasSource depth),
      compilerScaleCaseExpectedOutput = "(1, " <> Text.pack (show depth) <> ")"
    }

deepNestedLambdasSource :: Int -> Text
deepNestedLambdasSource depth =
  Text.unlines
    [ "module Main {",
      "  deep = "
        <> Text.concat ["\\(" <> captureName index <> ") -> " | index <- [1 .. depth]]
        <> "("
        <> captureName 1
        <> ", "
        <> captureName depth
        <> ").",
      "  deep " <> Text.unwords (map (Text.pack . show) [1 .. depth]) <> ".",
      "}"
    ]

captureName :: Int -> Text
captureName index = "capture" <> paddedDecimal 4 index

largeOperatorTablesCase :: Int -> CompilerScaleCase
largeOperatorTablesCase operatorCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "large-operator-tables-" <> paddedDecimal 4 operatorCount,
      compilerScaleCaseScenario = LargeOperatorTables,
      compilerScaleCaseSize = operatorCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (largeOperatorTablesSource operatorCount),
      compilerScaleCaseExpectedOutput = ""
    }

largeOperatorTablesSource :: Int -> Text
largeOperatorTablesSource operatorCount =
  Text.unlines
    ( [ "operator "
          <> generatedOperatorSymbol index
          <> " precedence "
          <> Text.pack (show (1 + index `mod` 99))
          <> "."
      | index <- [0 .. operatorCount - 1]
      ]
        <> [ "operatorValue"
               <> paddedDecimal 4 index
               <> " = 1 "
               <> generatedOperatorSymbol index
               <> " 2."
           | index <- [0 .. operatorCount - 1]
           ]
    )

generatedOperatorSymbol :: Int -> Text
generatedOperatorSymbol index =
  "%" <> Text.pack [operatorDigit power | power <- [3, 2, 1, 0]]
  where
    operatorAlphabet = "?^~&"
    operatorDigit :: Int -> Char
    operatorDigit power = operatorAlphabet !! ((index `div` (4 ^ power)) `mod` 4)

nestedBlocksCase :: Int -> CompilerScaleCase
nestedBlocksCase depth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "nested-blocks-" <> paddedDecimal 4 depth,
      compilerScaleCaseScenario = NestedBlocks,
      compilerScaleCaseSize = depth,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (nestedBlocksSource depth),
      compilerScaleCaseExpectedOutput = ""
    }

nestedBlocksSource :: Int -> Text
nestedBlocksSource depth =
  "nested = " <> renderNestedBlock 1 <> ".\nnested.\n"
  where
    renderNestedBlock index
      | index > depth = "0"
      | otherwise =
          "{ local"
            <> paddedDecimal 4 index
            <> " = "
            <> Text.pack (show index)
            <> ". "
            <> renderNestedBlock (index + 1)
            <> ". }"

longTokenStreamCase :: Int -> CompilerScaleCase
longTokenStreamCase tokenCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "long-token-stream-" <> paddedDecimal 5 tokenCount,
      compilerScaleCaseScenario = LongTokenStream,
      compilerScaleCaseSize = tokenCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (longTokenStreamSource tokenCount),
      compilerScaleCaseExpectedOutput = ""
    }

longTokenStreamSource :: Int -> Text
longTokenStreamSource tokenCount =
  Text.unlines
    [ "token" <> paddedDecimal 5 index <> " = 0."
    | index <- [0 .. tokenCount `div` 4 - 1]
    ]

paddedDecimal :: Int -> Int -> Text
paddedDecimal width value = Text.justifyRight width '0' (Text.pack (show value))
