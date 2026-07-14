module JazzNext.Compiler.Runtime.Observation
  ( RuntimeApplicationKind (..),
    RuntimeConstructionKind (..),
    RuntimeObservationReport (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeObservationState,
    RuntimeStatistics (..),
    RuntimeTermination (..),
    emptyRuntimeStatistics,
    finishRuntimeObservationResult,
    initialRuntimeObservationState,
    recordRuntimeApplication,
    recordRuntimeForcedValue,
    recordRuntimeTransition,
    runtimeObservationEnabled,
    runtimeObservationStatisticsEnabled,
  )
where

import Data.Word (Word64)
import JazzNext.Compiler.Diagnostics (Diagnostic)

data RuntimeObservationRequest
  = RuntimeObservationDisabled
  | RuntimeObservationStatistics
  | RuntimeObservationProfile
  | RuntimeObservationStatisticsAndProfile
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeTermination
  = RuntimeSucceeded
  | RuntimeFailed
  deriving (Eq, Ord, Show)

data RuntimeApplicationKind
  = ClosureApplication
  | BuiltinApplication
  | OperatorApplication
  | ConstructorApplication
  | MethodApplication
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeConstructionKind
  = ClosureConstruction
  | ListCellConstruction
  | TupleConstruction
  | SaturatedAdtConstruction
  deriving (Bounded, Enum, Eq, Ord, Show)

data RuntimeStatistics = RuntimeStatistics
  { runtimeEvaluatorTransitions :: !Word64,
    runtimeForcedValues :: !Word64,
    runtimeApplications :: !Word64,
    runtimeClosureApplications :: !Word64,
    runtimeBuiltinApplications :: !Word64,
    runtimeOperatorApplications :: !Word64,
    runtimeConstructorApplications :: !Word64,
    runtimeMethodApplications :: !Word64,
    runtimeCurrentContinuationDepth :: !Word64,
    runtimeMaximumContinuationDepth :: !Word64,
    runtimeClosuresCreated :: !Word64,
    runtimeBindingsCaptured :: !Word64,
    runtimeMaximumCaptureWidth :: !Word64,
    runtimeListCellsConstructed :: !Word64,
    runtimeTuplesConstructed :: !Word64,
    runtimeSaturatedAdtValuesConstructed :: !Word64,
    runtimePatternAttempts :: !Word64,
    runtimePatternMatches :: !Word64,
    runtimePatternBindings :: !Word64,
    runtimeBuiltinCalls :: !Word64,
    runtimeHostOperations :: !Word64,
    runtimeDeferredCacheHits :: !Word64,
    runtimeDeferredCacheMisses :: !Word64,
    runtimeDeferredCacheRecursiveEvaluations :: !Word64
  }
  deriving (Eq, Show)

data RuntimeObservationReport = RuntimeObservationReport
  { runtimeObservationTermination :: RuntimeTermination,
    runtimeObservationStatistics :: RuntimeStatistics
  }
  deriving (Eq, Show)

data RuntimeObservationResult value = RuntimeObservationResult
  { runtimeObservationOutcome :: Either Diagnostic value,
    runtimeObservationReport :: Maybe RuntimeObservationReport
  }
  deriving (Eq, Show)

data RuntimeObservationState = RuntimeObservationState
  { observationRequest :: !RuntimeObservationRequest,
    observationStatistics :: !RuntimeStatistics
  }

emptyRuntimeStatistics :: RuntimeStatistics
emptyRuntimeStatistics =
  RuntimeStatistics
    { runtimeEvaluatorTransitions = 0,
      runtimeForcedValues = 0,
      runtimeApplications = 0,
      runtimeClosureApplications = 0,
      runtimeBuiltinApplications = 0,
      runtimeOperatorApplications = 0,
      runtimeConstructorApplications = 0,
      runtimeMethodApplications = 0,
      runtimeCurrentContinuationDepth = 0,
      runtimeMaximumContinuationDepth = 0,
      runtimeClosuresCreated = 0,
      runtimeBindingsCaptured = 0,
      runtimeMaximumCaptureWidth = 0,
      runtimeListCellsConstructed = 0,
      runtimeTuplesConstructed = 0,
      runtimeSaturatedAdtValuesConstructed = 0,
      runtimePatternAttempts = 0,
      runtimePatternMatches = 0,
      runtimePatternBindings = 0,
      runtimeBuiltinCalls = 0,
      runtimeHostOperations = 0,
      runtimeDeferredCacheHits = 0,
      runtimeDeferredCacheMisses = 0,
      runtimeDeferredCacheRecursiveEvaluations = 0
    }

initialRuntimeObservationState :: RuntimeObservationRequest -> RuntimeObservationState
initialRuntimeObservationState request =
  RuntimeObservationState
    { observationRequest = request,
      observationStatistics = emptyRuntimeStatistics
    }

runtimeObservationEnabled :: RuntimeObservationState -> Bool
runtimeObservationEnabled = (/= RuntimeObservationDisabled) . observationRequest

recordRuntimeTransition :: Int -> RuntimeObservationState -> RuntimeObservationState
recordRuntimeTransition continuationDepth observationState
  | not (runtimeObservationEnabled observationState) = observationState
  | otherwise =
      observationState
        { observationStatistics =
            statistics
              { runtimeEvaluatorTransitions = runtimeEvaluatorTransitions statistics + 1,
                runtimeCurrentContinuationDepth = depth,
                runtimeMaximumContinuationDepth = max depth (runtimeMaximumContinuationDepth statistics)
              }
        }
  where
    statistics = observationStatistics observationState
    depth = fromIntegral continuationDepth

recordRuntimeForcedValue :: RuntimeObservationState -> RuntimeObservationState
recordRuntimeForcedValue observationState
  | not (runtimeObservationStatisticsEnabled observationState) = observationState
  | otherwise =
      observationState
        { observationStatistics =
            statistics
              { runtimeForcedValues = runtimeForcedValues statistics + 1
              }
        }
  where
    statistics = observationStatistics observationState

recordRuntimeApplication :: RuntimeApplicationKind -> RuntimeObservationState -> RuntimeObservationState
recordRuntimeApplication applicationKind observationState
  | not (runtimeObservationStatisticsEnabled observationState) = observationState
  | otherwise =
      observationState
        { observationStatistics =
            incrementApplicationKind
              applicationKind
              statistics
                { runtimeApplications = runtimeApplications statistics + 1
                }
        }
  where
    statistics = observationStatistics observationState

finishRuntimeObservationResult ::
  Either Diagnostic value ->
  RuntimeObservationState ->
  RuntimeObservationResult value
finishRuntimeObservationResult outcome observationState =
  RuntimeObservationResult
    { runtimeObservationOutcome = outcome,
      runtimeObservationReport =
        if runtimeObservationEnabled observationState
          then
            Just
              RuntimeObservationReport
                { runtimeObservationTermination =
                    case outcome of
                      Left _ -> RuntimeFailed
                      Right _ -> RuntimeSucceeded,
                  runtimeObservationStatistics =
                    (observationStatistics observationState)
                      { runtimeCurrentContinuationDepth = 0
                      }
                }
          else Nothing
    }

runtimeObservationStatisticsEnabled :: RuntimeObservationState -> Bool
runtimeObservationStatisticsEnabled observationState =
  case observationRequest observationState of
    RuntimeObservationStatistics -> True
    RuntimeObservationStatisticsAndProfile -> True
    RuntimeObservationDisabled -> False
    RuntimeObservationProfile -> False

incrementApplicationKind :: RuntimeApplicationKind -> RuntimeStatistics -> RuntimeStatistics
incrementApplicationKind applicationKind statistics =
  case applicationKind of
    ClosureApplication ->
      statistics {runtimeClosureApplications = runtimeClosureApplications statistics + 1}
    BuiltinApplication ->
      statistics {runtimeBuiltinApplications = runtimeBuiltinApplications statistics + 1}
    OperatorApplication ->
      statistics {runtimeOperatorApplications = runtimeOperatorApplications statistics + 1}
    ConstructorApplication ->
      statistics {runtimeConstructorApplications = runtimeConstructorApplications statistics + 1}
    MethodApplication ->
      statistics {runtimeMethodApplications = runtimeMethodApplications statistics + 1}
