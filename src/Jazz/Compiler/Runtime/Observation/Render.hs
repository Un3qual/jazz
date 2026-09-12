{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Runtime.Observation.Render
  ( decodeRuntimeObservationJson,
    encodeRuntimeObservationJson,
    renderRuntimeObservationHuman,
  )
where

import Data.Aeson
  ( Key,
    Value,
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
import Data.Word (Word64)
import Jazz.Compiler.Runtime.Observation
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
statisticsSeries =
  foldMap (\(key, _, value) -> pair key (word64 value)) . statisticFields

parseReport :: Value -> Parser RuntimeObservationReport
parseReport = withObject "runtime observation report" $ \object -> do
  schemaVersion <- object .: "schemaVersion"
  if schemaVersion /= runtimeObservationSchemaVersion
    then fail ("unsupported runtime observation schema version: " <> show (schemaVersion :: Int))
    else
      RuntimeObservationReport
        <$> (object .: "termination" >>= parseTermination)
        <*> (object .: "statistics" >>= parseStatistics)
        <*> pure Nothing

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
statisticLines =
  map (\(_, label, value) -> label <> ": " <> Text.pack (show value)) . statisticFields

statisticFields :: RuntimeStatistics -> [(Key, Text, Word64)]
statisticFields statistics =
  [ ("evaluatorTransitions", "evaluator transitions", runtimeEvaluatorTransitions statistics),
    ("forcedValues", "forced values", runtimeForcedValues statistics),
    ("applications", "applications", runtimeApplications statistics),
    ("closureApplications", "closure applications", runtimeClosureApplications statistics),
    ("builtinApplications", "builtin applications", runtimeBuiltinApplications statistics),
    ("operatorApplications", "operator applications", runtimeOperatorApplications statistics),
    ("constructorApplications", "constructor applications", runtimeConstructorApplications statistics),
    ("methodApplications", "method applications", runtimeMethodApplications statistics),
    ("currentContinuationDepth", "current continuation depth", runtimeCurrentContinuationDepth statistics),
    ("maximumContinuationDepth", "maximum continuation depth", runtimeMaximumContinuationDepth statistics),
    ("closuresCreated", "closures created", runtimeClosuresCreated statistics),
    ("bindingsCaptured", "bindings captured", runtimeBindingsCaptured statistics),
    ("maximumCaptureWidth", "maximum capture width", runtimeMaximumCaptureWidth statistics),
    ("listCellsConstructed", "list cells constructed", runtimeListCellsConstructed statistics),
    ("tuplesConstructed", "tuples constructed", runtimeTuplesConstructed statistics),
    ("saturatedAdtValuesConstructed", "saturated ADT values constructed", runtimeSaturatedAdtValuesConstructed statistics),
    ("patternAttempts", "pattern attempts", runtimePatternAttempts statistics),
    ("patternMatches", "pattern matches", runtimePatternMatches statistics),
    ("patternBindings", "pattern bindings", runtimePatternBindings statistics),
    ("builtinCalls", "builtin calls", runtimeBuiltinCalls statistics),
    ("hostOperations", "host operations", runtimeHostOperations statistics),
    ("deferredCacheHits", "deferred cache hits", runtimeDeferredCacheHits statistics),
    ("deferredCacheMisses", "deferred cache misses", runtimeDeferredCacheMisses statistics),
    ("deferredCacheRecursiveEvaluations", "deferred cache recursive evaluations", runtimeDeferredCacheRecursiveEvaluations statistics)
  ]

terminationName :: RuntimeTermination -> Text
terminationName termination =
  case termination of
    RuntimeSucceeded -> "succeeded"
    RuntimeFailed -> "failed"
