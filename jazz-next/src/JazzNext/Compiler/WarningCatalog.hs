{-# LANGUAGE OverloadedStrings #-}

-- | Stable warning metadata catalog. This module is the source of truth for
-- warning enums, CLI tokens, and user-facing codes.
module JazzNext.Compiler.WarningCatalog
  ( WarningCategory (..),
    allWarningCategories,
    warningCode,
    warningToken
  ) where

import Data.Text (Text)

-- | User-visible warning families. Once published, tokens/codes should remain
-- backward compatible.
data WarningCategory
  = SameScopeRebinding
  | ShadowingOuterScope
  | UnusedBinding
  | DeprecatedSyntax
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Internal metadata bundle so code/token definitions stay adjacent to the
-- enum cases they describe.
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

-- | Local metadata table for converting a warning category into its published
-- code and CLI token.
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
