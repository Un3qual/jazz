{-# LANGUAGE OverloadedStrings #-}

-- | Stable warning metadata catalog. This module is the source of truth for
-- warning enums, CLI tokens, and user-facing codes.
module JazzNext.Compiler.WarningCatalog
  ( WarningCategory (..),
    allWarningCategories,
    warningCode,
    warningHasAnalyzerEmitter,
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
    metadataHasAnalyzerEmitter :: Bool,
    metadataToken :: Text
  }

-- | Exhaustive warning inventory in enum order for help text and config
-- validation.
allWarningCategories :: [WarningCategory]
allWarningCategories = [minBound .. maxBound]

-- | Published diagnostic code for a warning category.
warningCode :: WarningCategory -> Text
warningCode = metadataCode . warningMetadata

-- | Whether the active analyzer can currently emit this warning category.
warningHasAnalyzerEmitter :: WarningCategory -> Bool
warningHasAnalyzerEmitter = metadataHasAnalyzerEmitter . warningMetadata

-- | Stable CLI/config token for a warning category.
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
          metadataHasAnalyzerEmitter = True,
          metadataToken = "same-scope-rebinding"
        }
    ShadowingOuterScope ->
      WarningMetadata
        { metadataCode = "W0002",
          metadataHasAnalyzerEmitter = True,
          metadataToken = "shadowing-outer-scope"
        }
    UnusedBinding ->
      WarningMetadata
        { metadataCode = "W0003",
          metadataHasAnalyzerEmitter = True,
          metadataToken = "unused-binding"
        }
    DeprecatedSyntax ->
      WarningMetadata
        { metadataCode = "W0004",
          metadataHasAnalyzerEmitter = False,
          metadataToken = "deprecated-syntax"
        }
