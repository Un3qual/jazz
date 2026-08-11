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
  | SharedInterfaceFanout
  | NestedRuntimeApplications
  | RuntimeImportWidth
  | ResolverFactRich
  | TypedValidationHandoff
  | LoweredTemporaryValidation
  | TypedRecursiveStatementGraph
  | TypedForwardSignedFunctions
  | TypedWideExportProviders
  | WideConstructorApplication
  | CapabilityCandidateWidth
  | HostFreeOpaqueEnvironment
  | AnalyzerDiagnosticChain
  | InterleavedRecursiveGroups
  | RecursiveRebindings
  | ConstrainedSignatures
  | DeepNestedLambdas
  | LargeOperatorTables
  | NestedBlocks
  | AmbiguousCaseArmPipes
  | LongTokenStream
  | IdentifierTokenStream
  | LiteralTokenStream
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
  baseCompilerScaleCases <> runtimeEvidenceCompilerScaleCases

baseCompilerScaleCases :: [CompilerScaleCase]
baseCompilerScaleCases =
  map sequentialPolymorphicCase [64, 128, 256, 512]
    <> map (`wideModuleFanoutCase` 16) [8, 16, 32, 64]
    <> map (`wideModuleFanoutCase` 1) [64, 128, 256, 512]
    <> map (`sharedInterfaceFanoutCase` 16) [16, 32, 64, 128]
    <> map resolverFactRichCase [16, 32, 64, 128]
    <> map typedValidationHandoffCase [64, 128, 256, 512]
    <> map loweredTemporaryValidationCase [64, 256, 1024, 4096]
    <> map typedRecursiveStatementGraphCase [128, 512, 1024, 2048]
    <> map typedForwardSignedFunctionsCase [128, 512, 1024, 2048]
    <> map typedWideExportProvidersCase [128, 512, 1024, 2048]
    <> map wideConstructorApplicationCase [32, 64, 128, 256]
    <> map capabilityCandidateWidthCase [16, 32, 64, 128]
    <> map hostFreeOpaqueEnvironmentCase [64, 256, 1024, 4096]
    <> map analyzerDiagnosticChainCase [64, 128, 256, 512]
    <> map interleavedRecursiveGroupsCase [16, 32, 64, 128]
    <> map recursivePreviewBurstCase [16, 32, 64, 128]
    <> map recursiveRebindingBurstCase [128, 256, 512, 1024]
    <> map constrainedSignaturesCase [32, 64, 128, 256]
    <> map deferredConstraintBurstCase [128, 256, 512, 1024]
    <> map deepNestedLambdasCase [16, 32, 64, 128]
    <> map largeOperatorTablesCase [16, 32, 64, 128]
    <> map nestedBlocksCase [16, 32, 64, 128]
    <> map ambiguousCaseArmPipesCase [64, 128, 256, 512]
    <> map longTokenStreamCase [1024, 4096, 16384, 65536]
    <> map identifierTokenStreamCase [1024, 4096, 16384, 65536]
    <> map literalTokenStreamCase [1024, 4096, 16384, 65536]

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

resolverFactRichCase :: Int -> CompilerScaleCase
resolverFactRichCase declarationCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "resolver-fact-rich-" <> paddedDecimal 4 declarationCount,
      compilerScaleCaseScenario = ResolverFactRich,
      compilerScaleCaseSize = declarationCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ModulePreparationBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.fromList
          [ (scaleModuleRoot </> "Main.jz", resolverFactRichSource declarationCount),
            (scaleModuleRoot </> "Support" </> "Types.jz", resolverFactRichTypesSource),
            (scaleModuleRoot </> "Support" </> "Values.jz", resolverFactRichValuesSource)
          ],
      compilerScaleCaseExpectedOutput = "Token"
    }

resolverFactRichSource :: Int -> Text
resolverFactRichSource declarationCount =
  Text.unlines
    ( [ "module Main {",
        "  import Support::Types as T.",
        "  import Support::Values as V.",
        "  import Support::Values (seed)."
      ]
        <> concatMap renderDeclaration [0 .. declarationCount - 1]
        <> ["  " <> itemName (declarationCount - 1) <> ".", "}"]
    )
  where
    renderDeclaration index =
      [ "  data " <> localTypeName index <> " = " <> localTypeName index <> " T::Token.",
        "  " <> itemName index <> " :: T::Token.",
        "  " <> itemName index <> " = V::identity @T::Token T::Token.",
        "  " <> copyName index <> " = seed."
      ]
    itemName index = "resolvedItem" <> paddedDecimal 4 index
    copyName index = "unqualifiedCopy" <> paddedDecimal 4 index
    localTypeName index = "Local" <> paddedDecimal 4 index

resolverFactRichTypesSource :: Text
resolverFactRichTypesSource =
  "module Support::Types (type Token(..)) { data Token = Token. }"

resolverFactRichValuesSource :: Text
resolverFactRichValuesSource =
  "module Support::Values (identity, seed) { identity = \\(item) -> item. seed = 1. }"

typedValidationHandoffCase :: Int -> CompilerScaleCase
typedValidationHandoffCase expressionCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "typed-validation-handoff-" <> paddedDecimal 4 expressionCount,
      compilerScaleCaseScenario = TypedValidationHandoff,
      compilerScaleCaseSize = expressionCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [TypedLoweringBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }

loweredTemporaryValidationCase :: Int -> CompilerScaleCase
loweredTemporaryValidationCase instructionCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "lowered-temporary-validation-" <> paddedDecimal 4 instructionCount,
      compilerScaleCaseScenario = LoweredTemporaryValidation,
      compilerScaleCaseSize = instructionCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [TypedLoweringBenchmark],
      compilerScaleCaseEntryModulePath = ["LoweredTemporaryValidation"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }

typedRecursiveStatementGraphCase :: Int -> CompilerScaleCase
typedRecursiveStatementGraphCase statementCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "typed-recursive-statement-graph-" <> paddedDecimal 4 statementCount,
      compilerScaleCaseScenario = TypedRecursiveStatementGraph,
      compilerScaleCaseSize = statementCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [TypedLoweringBenchmark],
      compilerScaleCaseEntryModulePath = ["TypedRecursiveStatementGraph"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }

typedWideExportProvidersCase :: Int -> CompilerScaleCase
typedWideExportProvidersCase providerCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "typed-wide-export-providers-" <> paddedDecimal 4 providerCount,
      compilerScaleCaseScenario = TypedWideExportProviders,
      compilerScaleCaseSize = providerCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [TypedLoweringBenchmark],
      compilerScaleCaseEntryModulePath = ["TypedWideExportProviders"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }

wideConstructorApplicationCase :: Int -> CompilerScaleCase
wideConstructorApplicationCase fieldCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "wide-constructor-application-" <> paddedDecimal 4 fieldCount,
      compilerScaleCaseScenario = WideConstructorApplication,
      compilerScaleCaseSize = fieldCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton (scaleModuleRoot </> "Main.jz") (wideConstructorApplicationSource fieldCount),
      compilerScaleCaseExpectedOutput =
        "(<function>, (0, "
          <> Text.pack (show (fieldCount `div` 2))
          <> ", "
          <> Text.pack (show (fieldCount - 1))
          <> "))"
    }

wideConstructorApplicationSource :: Int -> Text
wideConstructorApplicationSource fieldCount =
  Text.unlines
    [ "data Wide = Wide " <> Text.unwords (replicate fieldCount "Int") <> ".",
      "partial = Wide " <> Text.unwords (map renderValue [0 .. partialCount - 1]) <> ".",
      "wideValue = partial " <> Text.unwords (map renderValue [partialCount .. fieldCount - 1]) <> ".",
      "result = case wideValue { | Wide "
        <> Text.unwords (map fieldName [0 .. fieldCount - 1])
        <> " -> ("
        <> fieldName 0
        <> ", "
        <> fieldName partialCount
        <> ", "
        <> fieldName (fieldCount - 1)
        <> ") }.",
      "(partial, result)."
    ]
  where
    partialCount = fieldCount `div` 2
    renderValue = Text.pack . show
    fieldName fieldIndex = "field" <> paddedDecimal 4 fieldIndex

capabilityCandidateWidthCase :: Int -> CompilerScaleCase
capabilityCandidateWidthCase candidateCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "capability-candidate-width-" <> paddedDecimal 4 candidateCount,
      compilerScaleCaseScenario = CapabilityCandidateWidth,
      compilerScaleCaseSize = candidateCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, RuntimeBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (capabilityCandidateWidthSource candidateCount),
      compilerScaleCaseExpectedOutput = Text.pack (show (candidateCount - 1))
    }

capabilityCandidateWidthSource :: Int -> Text
capabilityCandidateWidthSource candidateCount =
  Text.unlines
    ( [ "module Main {",
        "  class CandidateValue(a) {",
        "    candidateValue :: a -> Int.",
        "  }."
      ]
        <> concatMap renderCandidate [0 .. candidateCount - 1]
        <> [ "  CandidateValue::candidateValue "
               <> candidateTargetName (candidateCount - 1)
               <> ".",
             "}"
           ]
    )
  where
    renderCandidate candidateIndex =
      [ "  data " <> targetName <> " = " <> targetName <> ".",
        "  impl CandidateValue(" <> targetName <> ") {",
        "    candidateValue = \\(candidate) -> " <> Text.pack (show candidateIndex) <> ".",
        "  }."
      ]
      where
        targetName = candidateTargetName candidateIndex

candidateTargetName :: Int -> Text
candidateTargetName candidateIndex =
  "CandidateTarget" <> paddedDecimal 4 candidateIndex

hostFreeOpaqueEnvironmentCase :: Int -> CompilerScaleCase
hostFreeOpaqueEnvironmentCase bindingCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "host-free-opaque-environment-" <> paddedDecimal 4 bindingCount,
      compilerScaleCaseScenario = HostFreeOpaqueEnvironment,
      compilerScaleCaseSize = bindingCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [RuntimeBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.fromList
          [ (scaleModuleRoot </> "Shared.jz", "module Shared { seed = 1. }"),
            (scaleModuleRoot </> "Main.jz", hostFreeOpaqueEnvironmentSource bindingCount)
          ],
      compilerScaleCaseExpectedOutput = "1"
    }

hostFreeOpaqueEnvironmentSource :: Int -> Text
hostFreeOpaqueEnvironmentSource bindingCount =
  Text.unlines
    ( ["module Main {", "  import Shared."]
        <> [ "  lazy" <> paddedDecimal 4 bindingIndex <> " = " <> Text.pack (show bindingIndex) <> "."
           | bindingIndex <- [0 .. bindingCount - 1]
           ]
        <> ["  seed.", "}"]
    )

typedForwardSignedFunctionsCase :: Int -> CompilerScaleCase
typedForwardSignedFunctionsCase size =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = identifier,
      compilerScaleCaseScenario = TypedForwardSignedFunctions,
      compilerScaleCaseSize = size,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [TypedLoweringBenchmark],
      compilerScaleCaseEntryModulePath = ["TypedForwardSignedFunctions"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }
  where
    identifier =
      case size of
        128 -> "typed-forward-signed-functions-0128"
        512 -> "typed-forward-signed-functions-0512"
        1024 -> "typed-forward-signed-functions-1024"
        2048 -> "typed-forward-signed-functions-2048"
        _ -> error "unsupported typed forward signed function scale"

analyzerDiagnosticChainCase :: Int -> CompilerScaleCase
analyzerDiagnosticChainCase expressionCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "analyzer-diagnostic-chain-" <> paddedDecimal 4 expressionCount,
      compilerScaleCaseScenario = AnalyzerDiagnosticChain,
      compilerScaleCaseSize = expressionCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.empty,
      compilerScaleCaseExpectedOutput = ""
    }

moduleName :: Int -> Text
moduleName index = "Module" <> paddedDecimal 4 index

valueName :: Int -> Int -> Text
valueName moduleIndex valueIndex =
  "fanout"
    <> paddedDecimal 4 moduleIndex
    <> "Value"
    <> paddedDecimal 4 valueIndex

runtimeEvidenceCompilerScaleCases :: [CompilerScaleCase]
runtimeEvidenceCompilerScaleCases =
  map nestedRuntimeApplicationsCase runtimeEvidenceScaleSizes
    <> map runtimeImportWidthCase runtimeEvidenceScaleSizes

runtimeEvidenceScaleSizes :: [Int]
runtimeEvidenceScaleSizes = [64, 128, 256, 512]

nestedRuntimeApplicationsCase :: Int -> CompilerScaleCase
nestedRuntimeApplicationsCase applicationDepth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "nested-runtime-applications-" <> paddedDecimal 4 applicationDepth,
      compilerScaleCaseScenario = NestedRuntimeApplications,
      compilerScaleCaseSize = applicationDepth,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [RuntimeBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (nestedRuntimeApplicationsSource applicationDepth),
      compilerScaleCaseExpectedOutput = "7"
    }

nestedRuntimeApplicationsSource :: Int -> Text
nestedRuntimeApplicationsSource applicationDepth =
  Text.unlines
    [ "module Main {",
      "  "
        <> Text.replicate applicationDepth "(\\(item) -> item) ("
        <> "7"
        <> Text.replicate applicationDepth ")"
        <> ".",
      "}"
    ]

runtimeImportWidthCase :: Int -> CompilerScaleCase
runtimeImportWidthCase interfaceWidth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "runtime-import-width-" <> paddedDecimal 4 interfaceWidth,
      compilerScaleCaseScenario = RuntimeImportWidth,
      compilerScaleCaseSize = interfaceWidth,
      compilerScaleCaseInterfaceWidth = Just interfaceWidth,
      compilerScaleCaseBenchmarks = [RuntimeBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.fromList
          [ (scaleModuleRoot </> "Shared.jz", runtimeImportWidthDependencySource interfaceWidth),
            (scaleModuleRoot </> "Main.jz", runtimeImportWidthEntrySource)
          ],
      compilerScaleCaseExpectedOutput = "7"
    }

runtimeImportWidthDependencySource :: Int -> Text
runtimeImportWidthDependencySource interfaceWidth =
  Text.unlines
    ( ["module Shared {"]
        <> [ "  "
               <> runtimeImportValueName valueIndex
               <> " = "
               <> (if valueIndex == 0 then "7." else Text.pack (show valueIndex) <> ".")
           | valueIndex <- [0 .. interfaceWidth - 1]
           ]
        <> ["}"]
    )

runtimeImportWidthEntrySource :: Text
runtimeImportWidthEntrySource =
  Text.unlines
    [ "module Main {",
      "  import Shared.",
      "  " <> runtimeImportValueName 0 <> ".",
      "}"
    ]

runtimeImportValueName :: Int -> Text
runtimeImportValueName valueIndex =
  "runtimeImportValue" <> paddedDecimal 4 valueIndex

sharedInterfaceFanoutCase :: Int -> Int -> CompilerScaleCase
sharedInterfaceFanoutCase dependentCount interfaceWidth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "shared-interface-fanout-"
          <> paddedDecimal 4 dependentCount
          <> "x"
          <> paddedDecimal 4 interfaceWidth,
      compilerScaleCaseScenario = SharedInterfaceFanout,
      compilerScaleCaseSize = dependentCount,
      compilerScaleCaseInterfaceWidth = Just interfaceWidth,
      compilerScaleCaseBenchmarks = [ModulePreparationBenchmark, WholeProgramBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = sharedInterfaceSources dependentCount interfaceWidth,
      compilerScaleCaseExpectedOutput = "0"
    }

sharedInterfaceSources :: Int -> Int -> Map FilePath Text
sharedInterfaceSources dependentCount interfaceWidth =
  Map.fromList
    ( [ (scaleModuleRoot </> "Shared.jz", sharedInterfaceSource interfaceWidth),
        (scaleModuleRoot </> "Main.jz", sharedInterfaceMainSource dependentCount)
      ]
        <> [ ( scaleModuleRoot </> "Dependent" </> Text.unpack (moduleName dependentIndex) <.> "jz",
               sharedInterfaceDependentSource dependentIndex
             )
           | dependentIndex <- [0 .. dependentCount - 1]
           ]
    )

sharedInterfaceSource :: Int -> Text
sharedInterfaceSource interfaceWidth =
  Text.unlines
    ( ["module Shared {"]
        <> [ "  "
               <> sharedValueName valueIndex
               <> " = "
               <> Text.pack (show valueIndex)
               <> "."
           | valueIndex <- [0 .. interfaceWidth - 1]
           ]
        <> ["}"]
    )

sharedInterfaceDependentSource :: Int -> Text
sharedInterfaceDependentSource dependentIndex =
  Text.unlines
    [ "module Dependent::" <> moduleName dependentIndex <> " {",
      "  import Shared.",
      "  " <> dependentValueName dependentIndex <> " = " <> sharedValueName 0 <> ".",
      "}"
    ]

sharedInterfaceMainSource :: Int -> Text
sharedInterfaceMainSource dependentCount =
  Text.unlines
    ( ["module Main {"]
        <> [ "  import Dependent::" <> moduleName dependentIndex <> "."
           | dependentIndex <- [0 .. dependentCount - 1]
           ]
        <> ["  " <> dependentValueName 0 <> ".", "}"]
    )

sharedValueName :: Int -> Text
sharedValueName valueIndex = "sharedValue" <> paddedDecimal 4 valueIndex

dependentValueName :: Int -> Text
dependentValueName dependentIndex = "dependentValue" <> paddedDecimal 4 dependentIndex

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

recursivePreviewBurstCase :: Int -> CompilerScaleCase
recursivePreviewBurstCase groupCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "recursive-preview-burst-" <> paddedDecimal 4 groupCount,
      compilerScaleCaseScenario = InterleavedRecursiveGroups,
      compilerScaleCaseSize = groupCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (recursivePreviewBurstSource groupCount),
      compilerScaleCaseExpectedOutput = "(1, True)"
    }

recursivePreviewBurstSource :: Int -> Text
recursivePreviewBurstSource groupCount =
  Text.unlines
    ( ["module Main {"]
        <> concatMap renderGroup [0 .. groupCount - 1]
        <> [ "  ("
               <> recursiveBindingName "late" finalGroup
               <> ", "
               <> recursiveBindingName "previewThree" finalGroup
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
        renderPreview "previewOne" groupIndex,
        renderPreview "previewTwo" groupIndex,
        renderPreview "previewThree" groupIndex,
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
    renderPreview prefix groupIndex =
      "  "
        <> recursiveBindingName prefix groupIndex
        <> " = "
        <> recursiveBindingName "left" groupIndex
        <> " True."

recursiveBindingName :: Text -> Int -> Text
recursiveBindingName prefix groupIndex = prefix <> paddedDecimal 4 groupIndex

recursiveRebindingBurstCase :: Int -> CompilerScaleCase
recursiveRebindingBurstCase bindingCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "recursive-rebinding-burst-" <> paddedDecimal 4 bindingCount,
      compilerScaleCaseScenario = RecursiveRebindings,
      compilerScaleCaseSize = bindingCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (recursiveRebindingBurstSource bindingCount),
      compilerScaleCaseExpectedOutput = Text.pack (show (bindingCount - 1))
    }

recursiveRebindingBurstSource :: Int -> Text
recursiveRebindingBurstSource bindingCount =
  Text.unlines
    ( ["module Main {", "  rebound = 0."]
        <> replicate (bindingCount - 1) "  rebound = rebound + 1."
        <> ["  rebound.", "}"]
    )

constrainedSignaturesCase :: Int -> CompilerScaleCase
constrainedSignaturesCase signatureCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "constrained-signatures-" <> paddedDecimal 4 signatureCount,
      compilerScaleCaseScenario = ConstrainedSignatures,
      compilerScaleCaseSize = signatureCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark, AnalysisBenchmark],
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

deferredConstraintBurstCase :: Int -> CompilerScaleCase
deferredConstraintBurstCase useCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "deferred-constraint-burst-" <> paddedDecimal 4 useCount,
      compilerScaleCaseScenario = ConstrainedSignatures,
      compilerScaleCaseSize = useCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (deferredConstraintBurstSource useCount),
      compilerScaleCaseExpectedOutput =
        "[" <> Text.intercalate ", " (replicate useCount "1") <> "]"
    }

deferredConstraintBurstSource :: Int -> Text
deferredConstraintBurstSource useCount =
  Text.unlines
    [ "module Main {",
      "  class ScaleConstraint(a) { }.",
      "  impl ScaleConstraint(Int) { }.",
      "  constrained :: @{ScaleConstraint(a)}: a -> a.",
      "  constrained = \\(item) -> item.",
      "  ["
        <> Text.intercalate ", " (replicate useCount "constrained 1")
        <> "].",
      "}"
    ]

deepNestedLambdasCase :: Int -> CompilerScaleCase
deepNestedLambdasCase depth =
  CompilerScaleCase
    { compilerScaleCaseIdentifier =
        "deep-nested-lambdas-" <> paddedDecimal 4 depth,
      compilerScaleCaseScenario = DeepNestedLambdas,
      compilerScaleCaseSize = depth,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [AnalysisBenchmark, ModulePreparationBenchmark, WholeProgramBenchmark],
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

ambiguousCaseArmPipesCase :: Int -> CompilerScaleCase
ambiguousCaseArmPipesCase operandCount =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = "ambiguous-case-arm-pipes-" <> paddedDecimal 4 operandCount,
      compilerScaleCaseScenario = AmbiguousCaseArmPipes,
      compilerScaleCaseSize = operandCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources =
        Map.singleton
          (scaleModuleRoot </> "Main.jz")
          (ambiguousCaseArmPipesSource operandCount),
      compilerScaleCaseExpectedOutput = ""
    }

ambiguousCaseArmPipesSource :: Int -> Text
ambiguousCaseArmPipesSource operandCount =
  "ambiguousPipe = case 0 { | _ -> "
    <> Text.intercalate " | " (map (Text.pack . show) [0 .. operandCount - 1])
    <> " }.\n"

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

identifierTokenStreamCase :: Int -> CompilerScaleCase
identifierTokenStreamCase tokenCount =
  tokenStreamCase
    "identifier-token-stream-"
    IdentifierTokenStream
    tokenCount
    ( Text.unlines
        [ "token" <> paddedDecimal 5 index <> " = token" <> paddedDecimal 5 index <> "."
        | index <- [0 .. tokenCount `div` 4 - 1]
        ]
    )

literalTokenStreamCase :: Int -> CompilerScaleCase
literalTokenStreamCase tokenCount =
  tokenStreamCase
    "literal-token-stream-"
    LiteralTokenStream
    tokenCount
    (Text.unlines (replicate (tokenCount `div` 2) "0."))

tokenStreamCase :: Text -> CompilerScaleScenario -> Int -> Text -> CompilerScaleCase
tokenStreamCase identifierPrefix scenario tokenCount source =
  CompilerScaleCase
    { compilerScaleCaseIdentifier = identifierPrefix <> paddedDecimal 5 tokenCount,
      compilerScaleCaseScenario = scenario,
      compilerScaleCaseSize = tokenCount,
      compilerScaleCaseInterfaceWidth = Nothing,
      compilerScaleCaseBenchmarks = [ParseLowerBenchmark],
      compilerScaleCaseEntryModulePath = ["Main"],
      compilerScaleCaseResolutionConfig = scaleResolutionConfig,
      compilerScaleCaseSources = Map.singleton (scaleModuleRoot </> "Main.jz") source,
      compilerScaleCaseExpectedOutput = ""
    }

paddedDecimal :: Int -> Int -> Text
paddedDecimal width value = Text.justifyRight width '0' (Text.pack (show value))
