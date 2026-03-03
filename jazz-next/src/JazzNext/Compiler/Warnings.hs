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
  = SeverityWarning
  | SeverityError
  deriving (Eq, Ord, Show)

allWarningCategories :: [WarningCategory]
allWarningCategories = [minBound .. maxBound]

warningCode :: WarningCategory -> String
warningCode category =
  case category of
    SameScopeRebinding -> "W0001"
    ShadowingOuterScope -> "W0002"
    UnusedBinding -> "W0003"
    DeprecatedSyntax -> "W0004"

warningToken :: WarningCategory -> String
warningToken category =
  case category of
    SameScopeRebinding -> "same-scope-rebinding"
    ShadowingOuterScope -> "shadowing-outer-scope"
    UnusedBinding -> "unused-binding"
    DeprecatedSyntax -> "deprecated-syntax"

parseWarningCategory :: String -> Either String WarningCategory
parseWarningCategory rawToken =
  case normalize rawToken of
    "same-scope-rebinding" -> Right SameScopeRebinding
    "shadowing-outer-scope" -> Right ShadowingOuterScope
    "unused-binding" -> Right UnusedBinding
    "deprecated-syntax" -> Right DeprecatedSyntax
    unknown ->
      Left
        ( "unknown warning category: "
            ++ unknown
            ++ "; known categories: "
            ++ intercalate ", " (map warningToken allWarningCategories)
        )

normalize :: String -> String
normalize = map toLower . trim

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
