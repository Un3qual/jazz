{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    WarningSeverity (..),
    allWarningCategories,
    parseWarningCategory,
    warningCode,
    warningToken
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Warning categories are stable user-facing identifiers. Keep codes/tokens
-- backward-compatible once published in CLI/docs.
data WarningCategory
  = SameScopeRebinding
  | ShadowingOuterScope
  | UnusedBinding
  | DeprecatedSyntax
  deriving (Eq, Ord, Show, Enum, Bounded)

data WarningSeverity
  -- Reserved for phase 2 analyzer diagnostics emission.
  = SeverityWarning
  | SeverityError
  deriving (Eq, Ord, Show)

data WarningMetadata = WarningMetadata
  { metadataCode :: Text,
    metadataToken :: Text
  }

allWarningCategories :: [WarningCategory]
allWarningCategories = [minBound .. maxBound]

warningCode :: WarningCategory -> Text
warningCode = metadataCode . warningMetadata

warningToken :: WarningCategory -> Text
warningToken = metadataToken . warningMetadata

tokenCatalog :: [(Text, WarningCategory)]
tokenCatalog =
  [ (metadataToken (warningMetadata category), category)
    | category <- allWarningCategories
  ]

parseWarningCategory :: Text -> Either Text WarningCategory
parseWarningCategory rawToken =
  case lookup normalizedToken tokenCatalog of
    Just category -> Right category
    Nothing ->
      Left
        ("unknown warning category: " <> normalizedToken <> "; known categories: " <> Text.intercalate ", " (map warningToken allWarningCategories))
  where
    normalizedToken = normalize rawToken

warningMetadata :: WarningCategory -> WarningMetadata
warningMetadata category =
  case category of
    SameScopeRebinding ->
      WarningMetadata
        { metadataCode = "W0001",
          metadataToken = "same-scope-rebinding"
        }
    ShadowingOuterScope ->
      WarningMetadata
        { metadataCode = "W0002",
          metadataToken = "shadowing-outer-scope"
        }
    UnusedBinding ->
      WarningMetadata
        { metadataCode = "W0003",
          metadataToken = "unused-binding"
        }
    DeprecatedSyntax ->
      WarningMetadata
        { metadataCode = "W0004",
          metadataToken = "deprecated-syntax"
        }

normalize :: Text -> Text
normalize = Text.toLower . trim

trim :: Text -> Text
trim = Text.strip
