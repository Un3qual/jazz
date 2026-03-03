module JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..),
    mkSameScopeRebindingWarning,
    sortWarnings
  ) where

import Data.List (sortOn)
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
    warningCodeText :: String,
    warningVariableName :: String,
    warningPrimarySpan :: SourceSpan,
    warningPreviousSpan :: Maybe SourceSpan,
    warningMessage :: String
  }
  deriving (Eq, Show)

mkSameScopeRebindingWarning :: String -> SourceSpan -> SourceSpan -> WarningRecord
mkSameScopeRebindingWarning variableName primarySpan previousSpan =
  WarningRecord
    { warningCategory = SameScopeRebinding,
      warningCodeText = warningCode SameScopeRebinding,
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = Just previousSpan,
      warningMessage =
        "same-scope rebinding for '"
          ++ variableName
          ++ "' (previous binding at "
          ++ show previousSpan
          ++ ")"
    }

sortWarnings :: [WarningRecord] -> [WarningRecord]
sortWarnings =
  sortOn
    ( \warning ->
        ( warningPrimarySpan warning,
          warningCategory warning,
          warningVariableName warning
        )
    )
