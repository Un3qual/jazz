{-# LANGUAGE OverloadedStrings #-}

-- | Stable compiler-stage names shared by benchmarks and profiling tools.
module JazzNext.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    benchmarkGroupName,
    benchmarkGroupStages,
    compilerStageName,
  )
where

import Data.Text (Text)

data BenchmarkGroup
  = ParseLowerBenchmark
  | AnalysisBenchmark
  | ModulePreparationBenchmark
  | RuntimeBenchmark
  | WholeProgramBenchmark
  deriving (Bounded, Enum, Eq, Ord, Show)

data CompilerStage
  = SourceLoadingStage
  | ModuleDiscoveryStage
  | LexingStage
  | ParsingStage
  | LoweringStage
  | ModuleResolutionStage
  | StaticAnalysisStage
  | TypeInferenceStage
  | ConstraintSolvingStage
  | CapabilitySolvingStage
  | RuntimePreparationStage
  | EvaluationStage
  | HostOperationStage
  | DiagnosticRenderingStage
  deriving (Bounded, Enum, Eq, Ord, Show)

benchmarkGroupName :: BenchmarkGroup -> Text
benchmarkGroupName group =
  case group of
    ParseLowerBenchmark -> "parse-lower"
    AnalysisBenchmark -> "analysis"
    ModulePreparationBenchmark -> "module-preparation"
    RuntimeBenchmark -> "runtime"
    WholeProgramBenchmark -> "whole-program"

benchmarkGroupStages :: BenchmarkGroup -> [CompilerStage]
benchmarkGroupStages group =
  case group of
    ParseLowerBenchmark -> [LexingStage, ParsingStage, LoweringStage]
    AnalysisBenchmark -> [StaticAnalysisStage, TypeInferenceStage, ConstraintSolvingStage, CapabilitySolvingStage]
    ModulePreparationBenchmark -> [SourceLoadingStage, ModuleDiscoveryStage, ModuleResolutionStage, RuntimePreparationStage]
    RuntimeBenchmark -> [EvaluationStage, HostOperationStage]
    WholeProgramBenchmark -> [minBound .. maxBound]

compilerStageName :: CompilerStage -> Text
compilerStageName compilerStage =
  case compilerStage of
    SourceLoadingStage -> "source-loading"
    ModuleDiscoveryStage -> "module-discovery"
    LexingStage -> "lexing"
    ParsingStage -> "parsing"
    LoweringStage -> "lowering"
    ModuleResolutionStage -> "module-resolution"
    StaticAnalysisStage -> "static-analysis"
    TypeInferenceStage -> "type-inference"
    ConstraintSolvingStage -> "constraint-solving"
    CapabilitySolvingStage -> "capability-solving"
    RuntimePreparationStage -> "runtime-preparation"
    EvaluationStage -> "evaluation"
    HostOperationStage -> "host-operation"
    DiagnosticRenderingStage -> "diagnostic-rendering"
