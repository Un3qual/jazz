{-# LANGUAGE OverloadedStrings #-}

module Jazz.ProgramCorpus.Runner
  ( ProgramCaseResult (..),
    loadProgramCaseEntrySource,
    prepareProgramCase,
    programCaseBudgetViolations,
    programCaseResolutionConfig,
    readProgramCaseSource,
    runProgramCase,
    runProgramCaseObserved,
    readProgramCaseSourceWith,
  )
where

import Control.Exception (IOException, displayException, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E5004))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (ToolingOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Driver
  ( ResolvedPrelude (PreludeBundled),
    RunResult (..),
    buildCompiledProgram,
    runCompileErrors,
    runModuleGraph,
    runModuleGraphObserved,
    runRuntimeErrors,
    runWarnings,
  )
import Jazz.Compiler.ModuleInterface (CompiledProgram)
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationReport (..),
    RuntimeObservationRequest,
    RuntimeStatistics (..),
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.ProgramCorpus.Types
  ( ProgramBudgetMetric (..),
    ProgramBudgetViolation (..),
    ProgramBudgets (..),
    ProgramCase (..),
    ProgramTermination (..),
  )
import System.FilePath ((</>))

data ProgramCaseResult = ProgramCaseResult
  { programCaseResultTermination :: ProgramTermination,
    programCaseResultStdout :: Text,
    programCaseResultDiagnostics :: [Diagnostic],
    programCaseResultWarnings :: [Diagnostic],
    programCaseResultObservation :: Maybe RuntimeObservationReport
  }
  deriving (Eq, Show)

runProgramCase :: ProgramCase -> IO ProgramCaseResult
runProgramCase programCase = do
  runResult <-
    runModuleGraph
      defaultWarningSettings
      (programCaseResolutionConfig programCase)
      (programCaseEntryModulePath programCase)
      readProgramCaseSource
  pure (caseResult runResult)

runProgramCaseObserved :: RuntimeObservationRequest -> ProgramCase -> IO ProgramCaseResult
runProgramCaseObserved observationRequest programCase = do
  runResult <-
    runModuleGraphObserved
      observationRequest
      defaultWarningSettings
      (programCaseResolutionConfig programCase)
      (programCaseEntryModulePath programCase)
      readProgramCaseSource
  pure (caseResult runResult)

programCaseBudgetViolations :: ProgramCase -> RuntimeObservationReport -> [ProgramBudgetViolation]
programCaseBudgetViolations programCase report =
  [ budgetViolation metric limit actual
  | (metric, limit) <- programBudgetLimits (programCaseBudgets programCase),
    let actual = runtimeMetricValue metric (runtimeObservationStatistics report),
    actual > limit
  ]
  where
    budgetViolation metric limit actual =
      ProgramBudgetViolation
        { programBudgetViolationCase = programCaseIdentifier programCase,
          programBudgetViolationMetric = metric,
          programBudgetViolationLimit = limit,
          programBudgetViolationActual = actual,
          programBudgetViolationPercentageIncrease = percentageIncrease limit actual
        }

loadProgramCaseEntrySource :: ProgramCase -> IO (Either Diagnostic Text)
loadProgramCaseEntrySource programCase = do
  let sourcePath = programCaseEntrySource programCase
  readResult <- try (TextIO.readFile sourcePath) :: IO (Either IOException Text)
  pure
    ( case readResult of
        Right source -> Right source
        Left exception ->
          Left
            ( mkErrorDiagnostic
                E5004
                ToolingOrigin
                ( "could not read corpus entry source for case '"
                    <> programCaseIdentifier programCase
                    <> "' at '"
                    <> Text.pack sourcePath
                    <> "': "
                    <> Text.pack (displayException exception)
                )
            )
    )

prepareProgramCase :: ProgramCase -> IO (Either Diagnostic CompiledProgram)
prepareProgramCase programCase =
  buildCompiledProgram
    defaultWarningSettings
    (PreludeBundled bundledPreludeSource)
    (programCaseResolutionConfig programCase)
    (programCaseEntryModulePath programCase)
    readProgramCaseSource

programCaseResolutionConfig :: ProgramCase -> ModuleResolutionConfig
programCaseResolutionConfig programCase =
  ModuleResolutionConfig
    { moduleRoots =
        [ programCaseModuleRoot programCase,
          programCasePackageRoot programCase </> "jazz" </> "stdlib"
        ],
      moduleExtension = ".jz"
    }

readProgramCaseSource :: FilePath -> IO (Maybe Text)
readProgramCaseSource = readProgramCaseSourceWith TextIO.readFile

readProgramCaseSourceWith :: (FilePath -> IO Text) -> FilePath -> IO (Maybe Text)
readProgramCaseSourceWith readSource path = do
  readResult <- try (readSource path) :: IO (Either IOException Text)
  pure (either (const Nothing) Just readResult)

caseResult :: RunResult -> ProgramCaseResult
caseResult result
  | not (null (runCompileErrors result)) =
      ProgramCaseResult
        { programCaseResultTermination = CompileFailedProgram,
          programCaseResultStdout = "",
          programCaseResultDiagnostics = runCompileErrors result,
          programCaseResultWarnings = runWarnings result,
          programCaseResultObservation = runRuntimeObservation result
        }
  | not (null (runRuntimeErrors result)) =
      ProgramCaseResult
        { programCaseResultTermination = RuntimeFailedProgram,
          programCaseResultStdout = "",
          programCaseResultDiagnostics = runRuntimeErrors result,
          programCaseResultWarnings = runWarnings result,
          programCaseResultObservation = runRuntimeObservation result
        }
  | otherwise =
      ProgramCaseResult
        { programCaseResultTermination = SuccessfulProgram,
          programCaseResultStdout = maybe "" (<> "\n") (runOutput result),
          programCaseResultDiagnostics = [],
          programCaseResultWarnings = runWarnings result,
          programCaseResultObservation = runRuntimeObservation result
        }

programBudgetLimits :: ProgramBudgets -> [(ProgramBudgetMetric, Word64)]
programBudgetLimits budgets =
  [ (EvaluatorTransitionsBudget, programBudgetSteps budgets),
    (ApplicationsBudget, programBudgetApplications budgets),
    (MaximumContinuationDepthBudget, programBudgetMaxContinuationDepth budgets)
  ]
    <> Map.toAscList
      ( foldr
          Map.delete
          (programBudgetOptionalLimits budgets)
          [EvaluatorTransitionsBudget, ApplicationsBudget, MaximumContinuationDepthBudget]
      )

runtimeMetricValue :: ProgramBudgetMetric -> RuntimeStatistics -> Word64
runtimeMetricValue metric statistics =
  case metric of
    EvaluatorTransitionsBudget -> runtimeEvaluatorTransitions statistics
    ApplicationsBudget -> runtimeApplications statistics
    MaximumContinuationDepthBudget -> runtimeMaximumContinuationDepth statistics
    ForcedValuesBudget -> runtimeForcedValues statistics
    ClosureApplicationsBudget -> runtimeClosureApplications statistics
    BuiltinApplicationsBudget -> runtimeBuiltinApplications statistics
    OperatorApplicationsBudget -> runtimeOperatorApplications statistics
    ConstructorApplicationsBudget -> runtimeConstructorApplications statistics
    MethodApplicationsBudget -> runtimeMethodApplications statistics
    ClosuresCreatedBudget -> runtimeClosuresCreated statistics
    BindingsCapturedBudget -> runtimeBindingsCaptured statistics
    MaximumCaptureWidthBudget -> runtimeMaximumCaptureWidth statistics
    ListCellsConstructedBudget -> runtimeListCellsConstructed statistics
    TuplesConstructedBudget -> runtimeTuplesConstructed statistics
    SaturatedAdtValuesConstructedBudget -> runtimeSaturatedAdtValuesConstructed statistics
    PatternAttemptsBudget -> runtimePatternAttempts statistics
    PatternMatchesBudget -> runtimePatternMatches statistics
    PatternBindingsBudget -> runtimePatternBindings statistics
    BuiltinCallsBudget -> runtimeBuiltinCalls statistics
    HostOperationsBudget -> runtimeHostOperations statistics
    DeferredCacheHitsBudget -> runtimeDeferredCacheHits statistics
    DeferredCacheMissesBudget -> runtimeDeferredCacheMisses statistics
    DeferredCacheRecursiveEvaluationsBudget -> runtimeDeferredCacheRecursiveEvaluations statistics

percentageIncrease :: Word64 -> Word64 -> Maybe Rational
percentageIncrease limit actual
  | limit == 0 = Nothing
  | otherwise =
      Just
        ( fromIntegral (actual - limit)
            * 100
            / fromIntegral limit
        )
