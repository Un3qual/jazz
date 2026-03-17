{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkMessageDiagnostic
  )
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

parseCliWarningDirective :: Text -> Either Diagnostic WarningDirective
parseCliWarningDirective rawFlag
  | "-W" `Text.isPrefixOf` rawFlag = parseDirectiveToken (Text.drop 2 rawFlag)
  | otherwise = Left (mkMessageDiagnostic ("invalid warning flag: expected -W<token>, got: " <> rawFlag))

parseEnvWarningDirectives :: Text -> Either Diagnostic [WarningDirective]
parseEnvWarningDirectives rawValue = do
  -- Env warning flags use +/- category toggles, not -W-prefixed tokens.
  tokens <- parseCommaSeparatedTokens rawValue
  traverse parseEnvWarningToken tokens

parseEnvErrorDirectives :: Text -> Either Diagnostic [WarningDirective]
parseEnvErrorDirectives rawValue = do
  -- Env error flags only support category names (or all).
  tokens <- parseCommaSeparatedTokens rawValue
  traverse parseEnvErrorToken tokens

parseConfigDirectives :: Text -> Either Diagnostic [WarningDirective]
parseConfigDirectives contents = do
  -- Config accepts both one-token-per-line and comma-separated forms, with # comments.
  let tokens = concatMap lineTokens (Text.lines contents)
  traverse parseDirectiveToken tokens

resolveWarningSettings ::
  [Text] ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Either Diagnostic WarningSettings
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

parseDirectiveToken :: Text -> Either Diagnostic WarningDirective
parseDirectiveToken rawToken
  | Text.null token = Left (mkMessageDiagnostic "empty warning token")
  | token == "none" = Right DisableAllCategories
  | token == "error" = Right PromoteAllEnabledToError
  | "error=" `Text.isPrefixOf` token =
      PromoteCategoryToError <$> parseWarningCategory (Text.drop (Text.length "error=") token)
  | "no-" `Text.isPrefixOf` token =
      DisableCategory <$> parseWarningCategory (Text.drop 3 token)
  | otherwise = EnableCategory <$> parseWarningCategory token
  where
    token = trim rawToken

parseEnvWarningToken :: Text -> Either Diagnostic WarningDirective
parseEnvWarningToken rawToken
  | Text.null token = Left (mkMessageDiagnostic "empty JAZZ_WARNING_FLAGS token")
  | token == "none" = Right DisableAllCategories
  | "+" `Text.isPrefixOf` token = EnableCategory <$> parseWarningCategory (Text.drop 1 token)
  | "-" `Text.isPrefixOf` token = DisableCategory <$> parseWarningCategory (Text.drop 1 token)
  | otherwise = EnableCategory <$> parseWarningCategory token
  where
    token = trim rawToken

parseEnvErrorToken :: Text -> Either Diagnostic WarningDirective
parseEnvErrorToken rawToken
  | Text.null token = Left (mkMessageDiagnostic "empty JAZZ_WARNING_ERROR_FLAGS token")
  | token == "all" = Right PromoteAllEnabledToError
  | otherwise = PromoteCategoryToError <$> parseWarningCategory token
  where
    token = trim rawToken

parseCommaSeparatedTokens :: Text -> Either Diagnostic [Text]
parseCommaSeparatedTokens rawValue =
  let rawTokens = splitCommas rawValue
      tokens = map trim rawTokens
   in
    -- Reject empty entries (for example trailing commas) so callers get a
    -- deterministic configuration error instead of silently ignored tokens.
    if all Text.null tokens
      then
        if length tokens > 1
          then Left (mkMessageDiagnostic "empty warning token")
          else Left (mkMessageDiagnostic "expected at least one warning token")
      else
        if any Text.null tokens
          then Left (mkMessageDiagnostic "empty warning token")
          else Right tokens

lineTokens :: Text -> [Text]
lineTokens line =
  -- Config files allow formatting-oriented empty entries (blank lines, trailing commas).
  let withoutComment = Text.takeWhile (/= '#') line
      pieces = splitCommas withoutComment
   in filter (not . Text.null) (map trim pieces)

splitCommas :: Text -> [Text]
splitCommas = Text.splitOn ","

trim :: Text -> Text
trim = Text.strip

boolMap :: Bool -> Map WarningCategory Bool
boolMap value = Map.fromList [(category, value) | category <- allWarningCategories]