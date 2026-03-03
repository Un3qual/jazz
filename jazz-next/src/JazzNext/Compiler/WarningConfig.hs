module JazzNext.Compiler.WarningConfig
  ( WarningDirective (..),
    WarningSettings,
    defaultWarningSettings,
    isWarningEnabled,
    isWarningError,
    parseCliWarningDirective,
    parseConfigDirectives,
    parseEnvErrorDirectives,
    parseEnvWarningDirectives,
    resolveWarningSettings
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import JazzNext.Compiler.Warnings
  ( WarningCategory,
    allWarningCategories,
    parseWarningCategory
  )

-- Normalized internal directives produced from CLI/env/config inputs.
data WarningDirective
  = EnableCategory WarningCategory
  | DisableCategory WarningCategory
  | PromoteCategoryToError WarningCategory
  | PromoteAllEnabledToError
  | DisableAllCategories
  deriving (Eq, Show)

data WarningSettings = WarningSettings
  { enabledCategories :: Map WarningCategory Bool,
    errorCategories :: Map WarningCategory Bool,
    allEnabledAreErrors :: Bool
  }
  deriving (Eq, Show)

-- Default contract: warnings are opt-in and non-fatal.
defaultWarningSettings :: WarningSettings
defaultWarningSettings =
  WarningSettings
    { enabledCategories = boolMap False,
      errorCategories = boolMap False,
      allEnabledAreErrors = False
    }

isWarningEnabled :: WarningSettings -> WarningCategory -> Bool
isWarningEnabled settings category =
  Map.findWithDefault False category (enabledCategories settings)

isWarningError :: WarningSettings -> WarningCategory -> Bool
isWarningError settings category =
  isWarningEnabled settings category
    && ( allEnabledAreErrors settings
           || Map.findWithDefault False category (errorCategories settings)
       )

parseCliWarningDirective :: String -> Either String WarningDirective
parseCliWarningDirective rawFlag
  | "-W" `isPrefixOf` rawFlag = parseDirectiveToken (drop 2 rawFlag)
  | otherwise = Left ("invalid warning flag: expected -W<token>, got: " ++ rawFlag)

parseEnvWarningDirectives :: String -> Either String [WarningDirective]
parseEnvWarningDirectives rawValue = do
  -- Env warning flags use +/- category toggles, not -W-prefixed tokens.
  tokens <- parseCommaSeparatedTokens rawValue
  traverse parseEnvWarningToken tokens

parseEnvErrorDirectives :: String -> Either String [WarningDirective]
parseEnvErrorDirectives rawValue = do
  -- Env error flags only support category names (or all).
  tokens <- parseCommaSeparatedTokens rawValue
  traverse parseEnvErrorToken tokens

parseConfigDirectives :: String -> Either String [WarningDirective]
parseConfigDirectives contents = do
  -- Config accepts both one-token-per-line and comma-separated forms, with # comments.
  let tokens = concatMap lineTokens (lines contents)
  traverse parseDirectiveToken tokens

resolveWarningSettings ::
  [String] ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Either String WarningSettings
resolveWarningSettings cliFlags envWarningFlags envErrorFlags configContents = do
  configDirectives <- maybe (Right []) parseConfigDirectives configContents
  envWarningDirectives <- maybe (Right []) parseEnvWarningDirectives envWarningFlags
  envErrorDirectives <- maybe (Right []) parseEnvErrorDirectives envErrorFlags
  cliDirectives <- traverse parseCliWarningDirective cliFlags

  -- Layer precedence is part of the external contract and must remain stable.
  -- PromoteAllEnabledToError only escalates categories that are already enabled;
  -- it does not implicitly enable any categories on its own.
  let applyBatch = foldl applyDirective
      afterConfig = applyBatch defaultWarningSettings configDirectives
      afterEnvWarnings = applyBatch afterConfig envWarningDirectives
      afterEnvErrors = applyBatch afterEnvWarnings envErrorDirectives
      afterCli = applyBatch afterEnvErrors cliDirectives

  pure afterCli

applyDirective :: WarningSettings -> WarningDirective -> WarningSettings
applyDirective settings directive =
  case directive of
    EnableCategory category ->
      settings
        { enabledCategories = Map.insert category True (enabledCategories settings)
        }
    DisableCategory category ->
      settings
        { enabledCategories = Map.insert category False (enabledCategories settings),
          errorCategories = Map.insert category False (errorCategories settings)
        }
    PromoteCategoryToError category ->
      settings
        { enabledCategories = Map.insert category True (enabledCategories settings),
          errorCategories = Map.insert category True (errorCategories settings)
        }
    PromoteAllEnabledToError ->
      settings {allEnabledAreErrors = True}
    DisableAllCategories ->
      settings
        { enabledCategories = boolMap False,
          errorCategories = boolMap False,
          allEnabledAreErrors = False
        }

parseDirectiveToken :: String -> Either String WarningDirective
parseDirectiveToken rawToken
  | token == "" = Left "empty warning token"
  | token == "none" = Right DisableAllCategories
  | token == "error" = Right PromoteAllEnabledToError
  | "error=" `isPrefixOf` token =
      PromoteCategoryToError <$> parseWarningCategory (drop (length "error=") token)
  | "no-" `isPrefixOf` token =
      DisableCategory <$> parseWarningCategory (drop 3 token)
  | otherwise = EnableCategory <$> parseWarningCategory token
  where
    token = trim rawToken

parseEnvWarningToken :: String -> Either String WarningDirective
parseEnvWarningToken rawToken
  | token == "" = Left "empty JAZZ_WARNING_FLAGS token"
  | token == "none" = Right DisableAllCategories
  | "-" `isPrefixOf` token = DisableCategory <$> parseWarningCategory (drop 1 token)
  | otherwise = EnableCategory <$> parseWarningCategory token
  where
    token = trim rawToken

parseEnvErrorToken :: String -> Either String WarningDirective
parseEnvErrorToken rawToken
  | token == "" = Left "empty JAZZ_WARNING_ERROR_FLAGS token"
  | token == "all" = Right PromoteAllEnabledToError
  | otherwise = PromoteCategoryToError <$> parseWarningCategory token
  where
    token = trim rawToken

parseCommaSeparatedTokens :: String -> Either String [String]
parseCommaSeparatedTokens rawValue =
  let rawTokens = splitCommas rawValue
      tokens = map trim rawTokens
   in
    -- Reject empty entries (for example trailing commas) so callers get a
    -- deterministic configuration error instead of silently ignored tokens.
    if all null tokens
        then
          if length tokens > 1
            then Left "empty warning token"
            else Left "expected at least one warning token"
        else
          if any null tokens
            then Left "empty warning token"
            else Right tokens

lineTokens :: String -> [String]
lineTokens line =
  -- Config files allow formatting-oriented empty entries (blank lines, trailing commas).
  let withoutComment = takeWhile (/= '#') line
      pieces = splitCommas withoutComment
   in filter (not . null) (map trim pieces)

splitCommas :: String -> [String]
splitCommas value =
  case break (== ',') value of
    (chunk, []) -> [chunk]
    (chunk, _:rest) -> chunk : splitCommas rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

boolMap :: Bool -> Map WarningCategory Bool
boolMap value = Map.fromList [(category, value) | category <- allWarningCategories]
