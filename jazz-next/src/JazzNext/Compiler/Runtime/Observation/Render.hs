{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Runtime.Observation.Render
  ( decodeRuntimeObservationJson,
    encodeRuntimeObservationJson,
    renderRuntimeObservationHuman,
  )
where

import Data.Aeson
  ( Value,
    eitherDecode,
    withObject,
    (.:),
  )
import Data.Aeson.Encoding
  ( Encoding,
    Series,
    encodingToLazyByteString,
    int,
    pair,
    pairs,
    text,
    word64,
  )
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationReport (..),
    RuntimeStatistics (..),
    RuntimeTermination (..),
  )

runtimeObservationSchemaVersion :: Int
runtimeObservationSchemaVersion = 1

renderRuntimeObservationHuman :: RuntimeObservationReport -> Text
renderRuntimeObservationHuman report =
  Text.unlines
    ( ["Jazz runtime statistics", "termination: " <> terminationName (runtimeObservationTermination report)]
        <> statisticLines (runtimeObservationStatistics report)
    )

encodeRuntimeObservationJson :: RuntimeObservationReport -> ByteString
encodeRuntimeObservationJson = encodingToLazyByteString . reportEncoding

decodeRuntimeObservationJson :: ByteString -> Either String RuntimeObservationReport
decodeRuntimeObservationJson bytes = do
  value <- eitherDecode bytes
  parseEither parseReport value

reportEncoding :: RuntimeObservationReport -> Encoding
reportEncoding report =
  pairs
    ( pair "schemaVersion" (int runtimeObservationSchemaVersion)
        <> pair "termination" (text (terminationName (runtimeObservationTermination report)))
        <> pair "statistics" (statisticsEncoding (runtimeObservationStatistics report))
    )

statisticsEncoding :: RuntimeStatistics -> Encoding
statisticsEncoding statistics =
  pairs (statisticsSeries statistics)

statisticsSeries :: RuntimeStatistics -> Series
statisticsSeries statistics =
  pair "evaluatorTransitions" (word64 (runtimeEvaluatorTransitions statistics))
    <> pair "forcedValues" (word64 (runtimeForcedValues statistics))
    <> pair "applications" (word64 (runtimeApplications statistics))
    <> pair "closureApplications" (word64 (runtimeClosureApplications statistics))
    <> pair "builtinApplications" (word64 (runtimeBuiltinApplications statistics))
    <> pair "operatorApplications" (word64 (runtimeOperatorApplications statistics))
    <> pair "constructorApplications" (word64 (runtimeConstructorApplications statistics))
    <> pair "methodApplications" (word64 (runtimeMethodApplications statistics))
    <> pair "currentContinuationDepth" (word64 (runtimeCurrentContinuationDepth statistics))
    <> pair "maximumContinuationDepth" (word64 (runtimeMaximumContinuationDepth statistics))
    <> pair "closuresCreated" (word64 (runtimeClosuresCreated statistics))
    <> pair "bindingsCaptured" (word64 (runtimeBindingsCaptured statistics))
    <> pair "maximumCaptureWidth" (word64 (runtimeMaximumCaptureWidth statistics))
    <> pair "listCellsConstructed" (word64 (runtimeListCellsConstructed statistics))
    <> pair "tuplesConstructed" (word64 (runtimeTuplesConstructed statistics))
    <> pair "saturatedAdtValuesConstructed" (word64 (runtimeSaturatedAdtValuesConstructed statistics))
    <> pair "patternAttempts" (word64 (runtimePatternAttempts statistics))
    <> pair "patternMatches" (word64 (runtimePatternMatches statistics))
    <> pair "patternBindings" (word64 (runtimePatternBindings statistics))
    <> pair "builtinCalls" (word64 (runtimeBuiltinCalls statistics))
    <> pair "hostOperations" (word64 (runtimeHostOperations statistics))
    <> pair "deferredCacheHits" (word64 (runtimeDeferredCacheHits statistics))
    <> pair "deferredCacheMisses" (word64 (runtimeDeferredCacheMisses statistics))
    <> pair "deferredCacheRecursiveEvaluations" (word64 (runtimeDeferredCacheRecursiveEvaluations statistics))

parseReport :: Value -> Parser RuntimeObservationReport
parseReport = withObject "runtime observation report" $ \object -> do
  schemaVersion <- object .: "schemaVersion"
  if schemaVersion /= runtimeObservationSchemaVersion
    then fail ("unsupported runtime observation schema version: " <> show (schemaVersion :: Int))
    else
      RuntimeObservationReport
        <$> (object .: "termination" >>= parseTermination)
        <*> (object .: "statistics" >>= parseStatistics)

parseTermination :: Text -> Parser RuntimeTermination
parseTermination name =
  case name of
    "succeeded" -> pure RuntimeSucceeded
    "failed" -> pure RuntimeFailed
    _ -> fail ("unknown runtime termination: " <> Text.unpack name)

parseStatistics :: Value -> Parser RuntimeStatistics
parseStatistics = withObject "runtime statistics" $ \object ->
  RuntimeStatistics
    <$> object .: "evaluatorTransitions"
    <*> object .: "forcedValues"
    <*> object .: "applications"
    <*> object .: "closureApplications"
    <*> object .: "builtinApplications"
    <*> object .: "operatorApplications"
    <*> object .: "constructorApplications"
    <*> object .: "methodApplications"
    <*> object .: "currentContinuationDepth"
    <*> object .: "maximumContinuationDepth"
    <*> object .: "closuresCreated"
    <*> object .: "bindingsCaptured"
    <*> object .: "maximumCaptureWidth"
    <*> object .: "listCellsConstructed"
    <*> object .: "tuplesConstructed"
    <*> object .: "saturatedAdtValuesConstructed"
    <*> object .: "patternAttempts"
    <*> object .: "patternMatches"
    <*> object .: "patternBindings"
    <*> object .: "builtinCalls"
    <*> object .: "hostOperations"
    <*> object .: "deferredCacheHits"
    <*> object .: "deferredCacheMisses"
    <*> object .: "deferredCacheRecursiveEvaluations"

statisticLines :: RuntimeStatistics -> [Text]
statisticLines statistics =
  [ statisticLine "evaluator transitions" (runtimeEvaluatorTransitions statistics),
    statisticLine "forced values" (runtimeForcedValues statistics),
    statisticLine "applications" (runtimeApplications statistics),
    statisticLine "closure applications" (runtimeClosureApplications statistics),
    statisticLine "builtin applications" (runtimeBuiltinApplications statistics),
    statisticLine "operator applications" (runtimeOperatorApplications statistics),
    statisticLine "constructor applications" (runtimeConstructorApplications statistics),
    statisticLine "method applications" (runtimeMethodApplications statistics),
    statisticLine "current continuation depth" (runtimeCurrentContinuationDepth statistics),
    statisticLine "maximum continuation depth" (runtimeMaximumContinuationDepth statistics),
    statisticLine "closures created" (runtimeClosuresCreated statistics),
    statisticLine "bindings captured" (runtimeBindingsCaptured statistics),
    statisticLine "maximum capture width" (runtimeMaximumCaptureWidth statistics),
    statisticLine "list cells constructed" (runtimeListCellsConstructed statistics),
    statisticLine "tuples constructed" (runtimeTuplesConstructed statistics),
    statisticLine "saturated ADT values constructed" (runtimeSaturatedAdtValuesConstructed statistics),
    statisticLine "pattern attempts" (runtimePatternAttempts statistics),
    statisticLine "pattern matches" (runtimePatternMatches statistics),
    statisticLine "pattern bindings" (runtimePatternBindings statistics),
    statisticLine "builtin calls" (runtimeBuiltinCalls statistics),
    statisticLine "host operations" (runtimeHostOperations statistics),
    statisticLine "deferred cache hits" (runtimeDeferredCacheHits statistics),
    statisticLine "deferred cache misses" (runtimeDeferredCacheMisses statistics),
    statisticLine "deferred cache recursive evaluations" (runtimeDeferredCacheRecursiveEvaluations statistics)
  ]

statisticLine :: (Show value) => Text -> value -> Text
statisticLine label value = label <> ": " <> Text.pack (show value)

terminationName :: RuntimeTermination -> Text
terminationName termination =
  case termination of
    RuntimeSucceeded -> "succeeded"
    RuntimeFailed -> "failed"
