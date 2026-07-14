{-# LANGUAGE OverloadedStrings #-}

module JazzNext.ProgramCorpus.Types
  ( BenchmarkGroup (..),
    FeatureTag (..),
    ProgramBudgetMetric (..),
    ProgramBudgetViolation (..),
    ProgramBudgets (..),
    ProgramCase (..),
    ProgramCaseDocument (..),
    ProgramCorpus (..),
    ProgramCorpusDocument (..),
    ProgramCorpusViolation (..),
    ProgramPathField (..),
    ProgramTermination (..),
    WorkloadClass (..),
    benchmarkGroupName,
    featureTagName,
    parseBenchmarkGroup,
    parseFeatureTag,
    parseProgramTermination,
    parseWorkloadClass,
    programBudgetMetricName,
    programTerminationName,
    workloadClassName,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import JazzNext.Compiler.Profiling
  ( BenchmarkGroup (..),
    benchmarkGroupName,
  )

data FeatureTag
  = ModulesFeature
  | GenericAdtsFeature
  | PatternsFeature
  | RecursionFeature
  | InferenceFeature
  | CapabilitiesFeature
  | TextFeature
  | ListsFeature
  | DeterministicRuntimeFeature
  deriving (Bounded, Enum, Eq, Ord, Show)

data WorkloadClass
  = FastWorkload
  | FullWorkload
  deriving (Bounded, Enum, Eq, Ord, Show)

data ProgramTermination
  = SuccessfulProgram
  | CompileFailedProgram
  | RuntimeFailedProgram
  deriving (Bounded, Enum, Eq, Ord, Show)

data ProgramBudgets = ProgramBudgets
  { programBudgetSteps :: Word64,
    programBudgetApplications :: Word64,
    programBudgetMaxContinuationDepth :: Word64,
    programBudgetOptionalLimits :: Map ProgramBudgetMetric Word64
  }
  deriving (Eq, Ord, Show)

data ProgramBudgetMetric
  = EvaluatorTransitionsBudget
  | ApplicationsBudget
  | MaximumContinuationDepthBudget
  | ForcedValuesBudget
  | ClosureApplicationsBudget
  | BuiltinApplicationsBudget
  | OperatorApplicationsBudget
  | ConstructorApplicationsBudget
  | MethodApplicationsBudget
  | ClosuresCreatedBudget
  | BindingsCapturedBudget
  | MaximumCaptureWidthBudget
  | ListCellsConstructedBudget
  | TuplesConstructedBudget
  | SaturatedAdtValuesConstructedBudget
  | PatternAttemptsBudget
  | PatternMatchesBudget
  | PatternBindingsBudget
  | BuiltinCallsBudget
  | HostOperationsBudget
  | DeferredCacheHitsBudget
  | DeferredCacheMissesBudget
  | DeferredCacheRecursiveEvaluationsBudget
  deriving (Bounded, Enum, Eq, Ord, Show)

data ProgramBudgetViolation = ProgramBudgetViolation
  { programBudgetViolationCase :: Text,
    programBudgetViolationMetric :: ProgramBudgetMetric,
    programBudgetViolationLimit :: Word64,
    programBudgetViolationActual :: Word64,
    programBudgetViolationPercentageIncrease :: Maybe Rational
  }
  deriving (Eq, Ord, Show)

data ProgramCorpusDocument = ProgramCorpusDocument
  { programCorpusDocumentSchemaVersion :: Int,
    programCorpusDocumentCases :: [ProgramCaseDocument]
  }
  deriving (Eq, Show)

data ProgramCaseDocument = ProgramCaseDocument
  { programCaseDocumentIdentifier :: Text,
    programCaseDocumentDirectory :: FilePath,
    programCaseDocumentEntrySource :: FilePath,
    programCaseDocumentModuleRoot :: FilePath,
    programCaseDocumentExpectedTermination :: Text,
    programCaseDocumentExpectedStdout :: FilePath,
    programCaseDocumentWorkload :: Text,
    programCaseDocumentFeatures :: [Text],
    programCaseDocumentBenchmarks :: [Text],
    programCaseDocumentBudgets :: ProgramBudgets
  }
  deriving (Eq, Show)

data ProgramCorpus = ProgramCorpus
  { programCorpusRoot :: FilePath,
    programCorpusSchemaVersion :: Int,
    programCorpusCases :: [ProgramCase]
  }
  deriving (Eq, Show)

data ProgramCase = ProgramCase
  { programCaseIdentifier :: Text,
    programCasePackageRoot :: FilePath,
    programCaseDirectory :: FilePath,
    programCaseEntrySource :: FilePath,
    programCaseModuleRoot :: FilePath,
    programCaseEntryModulePath :: [Text],
    programCaseExpectedTermination :: ProgramTermination,
    programCaseExpectedStdoutPath :: FilePath,
    programCaseExpectedStdout :: Text,
    programCaseWorkload :: WorkloadClass,
    programCaseFeatures :: [FeatureTag],
    programCaseBenchmarks :: [BenchmarkGroup],
    programCaseBudgets :: ProgramBudgets
  }
  deriving (Eq, Show)

data ProgramPathField
  = CaseDirectoryPath
  | EntrySourcePath
  | ModuleRootPath
  | ExpectedStdoutPath
  deriving (Eq, Ord, Show)

data ProgramCorpusViolation
  = MissingCorpusManifest FilePath
  | ManifestDecodeFailure Text
  | UnsupportedSchemaVersion Int
  | DuplicateCaseIdentifier Text
  | DuplicateCaseDirectory FilePath
  | UnknownTerminationCategory Text Text
  | UnknownWorkloadClass Text Text
  | UnknownFeatureTag Text Text
  | UnknownBenchmarkGroup Text Text
  | AbsoluteCorpusPath Text ProgramPathField FilePath
  | EscapingCorpusPath Text ProgramPathField FilePath
  | MissingCorpusPath Text ProgramPathField FilePath
  | UnreadableCorpusPath Text ProgramPathField FilePath Text
  | EntrySourceOutsideModuleRoot Text FilePath FilePath
  | InvalidEntrySourceExtension Text FilePath
  deriving (Eq, Ord, Show)

parseBenchmarkGroup :: Text -> Maybe BenchmarkGroup
parseBenchmarkGroup = lookupByName benchmarkGroupName

featureTagName :: FeatureTag -> Text
featureTagName feature =
  case feature of
    ModulesFeature -> "modules"
    GenericAdtsFeature -> "generic-adts"
    PatternsFeature -> "patterns"
    RecursionFeature -> "recursion"
    InferenceFeature -> "inference"
    CapabilitiesFeature -> "capabilities"
    TextFeature -> "text"
    ListsFeature -> "lists"
    DeterministicRuntimeFeature -> "deterministic-runtime"

parseFeatureTag :: Text -> Maybe FeatureTag
parseFeatureTag = lookupByName featureTagName

workloadClassName :: WorkloadClass -> Text
workloadClassName workload =
  case workload of
    FastWorkload -> "fast"
    FullWorkload -> "full"

parseWorkloadClass :: Text -> Maybe WorkloadClass
parseWorkloadClass = lookupByName workloadClassName

programTerminationName :: ProgramTermination -> Text
programTerminationName termination =
  case termination of
    SuccessfulProgram -> "success"
    CompileFailedProgram -> "compile-failure"
    RuntimeFailedProgram -> "runtime-failure"

parseProgramTermination :: Text -> Maybe ProgramTermination
parseProgramTermination = lookupByName programTerminationName

programBudgetMetricName :: ProgramBudgetMetric -> Text
programBudgetMetricName metric =
  case metric of
    EvaluatorTransitionsBudget -> "steps"
    ApplicationsBudget -> "applications"
    MaximumContinuationDepthBudget -> "maxContinuationDepth"
    ForcedValuesBudget -> "forcedValues"
    ClosureApplicationsBudget -> "closureApplications"
    BuiltinApplicationsBudget -> "builtinApplications"
    OperatorApplicationsBudget -> "operatorApplications"
    ConstructorApplicationsBudget -> "constructorApplications"
    MethodApplicationsBudget -> "methodApplications"
    ClosuresCreatedBudget -> "closuresCreated"
    BindingsCapturedBudget -> "bindingsCaptured"
    MaximumCaptureWidthBudget -> "maximumCaptureWidth"
    ListCellsConstructedBudget -> "listCellsConstructed"
    TuplesConstructedBudget -> "tuplesConstructed"
    SaturatedAdtValuesConstructedBudget -> "saturatedAdtValuesConstructed"
    PatternAttemptsBudget -> "patternAttempts"
    PatternMatchesBudget -> "patternMatches"
    PatternBindingsBudget -> "patternBindings"
    BuiltinCallsBudget -> "builtinCalls"
    HostOperationsBudget -> "hostOperations"
    DeferredCacheHitsBudget -> "deferredCacheHits"
    DeferredCacheMissesBudget -> "deferredCacheMisses"
    DeferredCacheRecursiveEvaluationsBudget -> "deferredCacheRecursiveEvaluations"

lookupByName :: (Bounded value, Enum value) => (value -> Text) -> Text -> Maybe value
lookupByName renderName requested =
  case filter ((== requested) . renderName) [minBound .. maxBound] of
    value : _ -> Just value
    [] -> Nothing
