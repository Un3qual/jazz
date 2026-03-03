module JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    WarningSeverity (..),
    allWarningCategories,
    parseWarningCategory,
    warningCode,
    warningToken
  ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate)

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
  { metadataCode :: String,
    metadataToken :: String
  }

allWarningCategories :: [WarningCategory]
allWarningCategories = [minBound .. maxBound]

warningCode :: WarningCategory -> String
warningCode = metadataCode . warningMetadata

warningToken :: WarningCategory -> String
warningToken = metadataToken . warningMetadata

tokenCatalog :: [(String, WarningCategory)]
tokenCatalog =
  [ (metadataToken (warningMetadata category), category)
    | category <- allWarningCategories
  ]

parseWarningCategory :: String -> Either String WarningCategory
parseWarningCategory rawToken =
  case lookup normalizedToken tokenCatalog of
    Just category -> Right category
    Nothing ->
      Left
        ( "unknown warning category: "
            ++ normalizedToken
            ++ "; known categories: "
            ++ intercalate ", " (map warningToken allWarningCategories)
        )
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

normalize :: String -> String
normalize = map toLower . trim

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
