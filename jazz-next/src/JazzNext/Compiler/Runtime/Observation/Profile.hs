{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Runtime.Observation.Profile
  ( encodeRuntimeSemanticProfile,
  )
where

import Data.Aeson.Encoding
  ( Encoding,
    encodingToLazyByteString,
    int,
    list,
    pair,
    pairs,
    text,
    word64,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeProfileEvent (..),
    RuntimeProfileFrame (..),
    RuntimeSemanticProfile (..),
    RuntimeTermination (..),
    runtimeCallableDisplayName,
  )

encodeRuntimeSemanticProfile :: RuntimeSemanticProfile -> ByteString
encodeRuntimeSemanticProfile = encodingToLazyByteString . profileDocumentEncoding

profileDocumentEncoding :: RuntimeSemanticProfile -> Encoding
profileDocumentEncoding profile =
  pairs
    ( pair "$schema" (text "https://www.speedscope.app/file-format-schema.json")
        <> pair "shared" (sharedDataEncoding profile)
        <> pair "profiles" (list eventedProfileEncoding [profile])
        <> pair "activeProfileIndex" (int 0)
        <> pair "exporter" (text "jazz-next")
    )

sharedDataEncoding :: RuntimeSemanticProfile -> Encoding
sharedDataEncoding profile =
  pairs
    ( pair
        "frames"
        (list frameEncoding (runtimeSemanticProfileFrames profile))
    )

frameEncoding :: RuntimeProfileFrame -> Encoding
frameEncoding frame =
  pairs
    ( pair
        "name"
        (text (runtimeCallableDisplayName (runtimeProfileFrameIdentity frame)))
    )

eventedProfileEncoding :: RuntimeSemanticProfile -> Encoding
eventedProfileEncoding profile =
  pairs
    ( pair "type" (text "evented")
        <> pair "name" (text (profileName profile))
        <> pair "unit" (text "none")
        <> pair "startValue" (word64 0)
        <> pair "endValue" (word64 (runtimeSemanticProfileEndValue profile))
        <> pair "events" (list eventEncoding (runtimeSemanticProfileEvents profile))
    )

eventEncoding :: RuntimeProfileEvent -> Encoding
eventEncoding event =
  case event of
    RuntimeProfileOpen frameIndex logicalTime ->
      eventFields "O" frameIndex logicalTime
    RuntimeProfileClose frameIndex logicalTime ->
      eventFields "C" frameIndex logicalTime

eventFields :: Text -> Int -> Word64 -> Encoding
eventFields eventType frameIndex logicalTime =
  pairs
    ( pair "type" (text eventType)
        <> pair "frame" (int frameIndex)
        <> pair "at" (word64 logicalTime)
    )

profileName :: RuntimeSemanticProfile -> Text
profileName profile =
  case runtimeSemanticProfileTermination profile of
    RuntimeSucceeded -> "Jazz runtime"
    RuntimeFailed -> "Jazz runtime (incomplete: failed)"
