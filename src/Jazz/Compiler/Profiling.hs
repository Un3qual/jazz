{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stable compiler-stage names shared by benchmarks and profiling tools.
module Jazz.Compiler.Profiling
  ( BenchmarkGroup (..),
    CompilerStage (..),
    CompilerStageBoundary (..),
    benchmarkGroupName,
    benchmarkGroupStages,
    compilerProfilingEnabled,
    compilerStageMarkerName,
    compilerStageName,
    withCompilerStage,
    withCompilerStageResult,
    withCompilerStageMarkers,
  )
where

import Control.Exception (bracket_)
import Data.Text (Text)
#ifdef JAZZ_GHC_PROFILING
import qualified Data.Text as Text
import Debug.Trace (traceMarkerIO)
#endif

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

data CompilerStageBoundary
  = CompilerStageBegin
  | CompilerStageEnd
  deriving (Eq, Ord, Show)

compilerProfilingEnabled :: Bool
#ifdef JAZZ_GHC_PROFILING
compilerProfilingEnabled = True
#else
compilerProfilingEnabled = False
#endif

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

compilerStageMarkerName :: CompilerStageBoundary -> CompilerStage -> Text
compilerStageMarkerName boundary compilerStage =
  "jazz-stage:"
    <> compilerStageName compilerStage
    <> ":"
    <> case boundary of
      CompilerStageBegin -> "begin"
      CompilerStageEnd -> "end"

-- | Bracket a fully evaluated compiler phase action with eventlog markers.
-- Callers own the phase-specific forcing needed before the action returns.
withCompilerStage :: CompilerStage -> IO value -> IO value
#ifdef JAZZ_GHC_PROFILING
withCompilerStage = withCompilerStageMarkers (traceMarkerIO . Text.unpack)
#else
withCompilerStage _ action = action
#endif

-- | Force the phase-specific result before its end marker in profiling builds.
-- Ordinary builds run the original action without the profiling-only forcing
-- cost, preserving the default compiler path.
withCompilerStageResult :: CompilerStage -> (value -> IO ()) -> IO value -> IO value
#ifdef JAZZ_GHC_PROFILING
withCompilerStageResult compilerStage forceResult action =
  withCompilerStage compilerStage $ do
    result <- action
    forceResult result
    pure result
#else
withCompilerStageResult _ _ action = action
#endif

-- | Injectable form used to verify paired markers without reading a binary
-- eventlog in the ordinary test suite.
withCompilerStageMarkers :: (Text -> IO ()) -> CompilerStage -> IO value -> IO value
withCompilerStageMarkers writeMarker compilerStage =
  bracket_
    (writeMarker (compilerStageMarkerName CompilerStageBegin compilerStage))
    (writeMarker (compilerStageMarkerName CompilerStageEnd compilerStage))
