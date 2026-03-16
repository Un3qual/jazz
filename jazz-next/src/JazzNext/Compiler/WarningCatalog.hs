{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.WarningCatalog
  ( WarningCategory (..),
    allWarningCategories,
    warningCode,
    warningToken
  ) where

import Data.Text (Text)

data WarningCategory
  = SameScopeRebinding
  | ShadowingOuterScope
  | UnusedBinding
  | DeprecatedSyntax
  deriving (Eq, Ord, Show, Enum, Bounded)

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
