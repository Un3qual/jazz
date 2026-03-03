{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..),
    renderSourceSpan,
    mkSameScopeRebindingWarning,
    sortWarnings
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    warningCode
  )

data SourceSpan = SourceSpan
  { spanLine :: Int,
    spanColumn :: Int
  }
  deriving (Eq, Ord, Show)

data WarningRecord = WarningRecord
  { warningCategory :: WarningCategory,
    warningCodeText :: Text,
    warningVariableName :: Text,
    warningPrimarySpan :: SourceSpan,
    warningPreviousSpan :: Maybe SourceSpan,
    warningMessage :: Text
  }
  deriving (Eq, Show)

renderSourceSpan :: SourceSpan -> Text
renderSourceSpan spanValue =
  Text.pack (show (spanLine spanValue)) <> ":" <> Text.pack (show (spanColumn spanValue))

mkSameScopeRebindingWarning :: Text -> SourceSpan -> SourceSpan -> WarningRecord
mkSameScopeRebindingWarning variableName primarySpan previousSpan =
  WarningRecord
    { warningCategory = SameScopeRebinding,
      warningCodeText = warningCode SameScopeRebinding,
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = Just previousSpan,
      warningMessage =
        "same-scope rebinding: '"
          <> variableName
          <> "' shadows previous same-scope binding (last declaration wins)"
    }

sortWarnings :: [WarningRecord] -> [WarningRecord]
-- Keep warning output deterministic so tests and CLI behavior are stable.
sortWarnings =
  sortOn
    ( \warning ->
        ( warningPrimarySpan warning,
          warningCategory warning,
          warningVariableName warning
        )
    )
