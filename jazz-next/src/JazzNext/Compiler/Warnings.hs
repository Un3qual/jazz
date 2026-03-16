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
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkMessageDiagnostic
  )
import JazzNext.Compiler.WarningCatalog
  ( WarningCategory (..),
    allWarningCategories,
    warningCode,
    warningToken
  )

-- Warning categories are stable user-facing identifiers. Keep codes/tokens
-- backward-compatible once published in CLI/docs.
data WarningSeverity
  -- Reserved for phase 2 analyzer diagnostics emission.
  = SeverityWarning
  | SeverityError
  deriving (Eq, Ord, Show)

tokenCatalog :: [(Text, WarningCategory)]
tokenCatalog =
  [ (warningToken category, category)
    | category <- allWarningCategories
  ]

parseWarningCategory :: Text -> Either Diagnostic WarningCategory
parseWarningCategory rawToken =
  case lookup normalizedToken tokenCatalog of
    Just category -> Right category
    Nothing ->
      Left
        ( mkMessageDiagnostic
            ( "unknown warning category: "
                <> normalizedToken
                <> "; known categories: "
                <> Text.intercalate ", " (map warningToken allWarningCategories)
            )
        )
  where
    normalizedToken = normalize rawToken

normalize :: Text -> Text
normalize = Text.toLower . trim

trim :: Text -> Text
trim = Text.strip
