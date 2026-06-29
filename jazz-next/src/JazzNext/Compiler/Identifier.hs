{-# LANGUAGE OverloadedStrings #-}

-- | Identifier wrapper that keeps the original text and the purity implied by
-- the current naming convention.
module JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    identifierPurity,
    mkIdentifier,
    mkOperatorBindingIdentifier,
    mkQualifiedIdentifier,
    isOperatorBindingIdentifierText,
    operatorBindingIdentifierText,
    qualifiedIdentifierText,
    splitQualifiedIdentifierText
  ) where

import Data.Char
  ( ord,
    toUpper
  )
import Data.String
  ( IsString (..)
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import JazzNext.Compiler.Purity
  ( Purity,
    namePurity
  )
import Numeric
  ( showHex
  )

-- | Names annotated with the purity implied by their spelling.
data Identifier = Identifier
  { identifierText :: Text,
    identifierPurity :: Purity
  }
  deriving (Eq, Show)

-- | Construct an identifier and derive its purity once so later phases can
-- reuse the classification without re-parsing the name text.
mkIdentifier :: Text -> Identifier
mkIdentifier name =
  Identifier
    { identifierText = name,
      identifierPurity = namePurity name
    }

-- | Render the compiler-owned binding name that backs an executable user
-- operator declaration. The prefix is not source-identifiable, and every
-- operator character is percent-encoded so a trailing @!@ cannot mark the
-- hidden binding as impure.
operatorBindingIdentifierText :: Text -> Text
operatorBindingIdentifierText operatorSymbol =
  operatorBindingIdentifierPrefix <> Text.concatMap encodeOperatorChar operatorSymbol
  where
    encodeOperatorChar char =
      let hexText = Text.pack (map toUpper (showHex (ord char) ""))
       in "%" <> Text.justifyRight 2 '0' hexText

operatorBindingIdentifierPrefix :: Text
operatorBindingIdentifierPrefix = "$operator:"

isOperatorBindingIdentifierText :: Text -> Bool
isOperatorBindingIdentifierText =
  Text.isPrefixOf operatorBindingIdentifierPrefix

mkOperatorBindingIdentifier :: Text -> Identifier
mkOperatorBindingIdentifier =
  mkIdentifier . operatorBindingIdentifierText

-- | Render the canonical textual form used when module import replay exposes a
-- qualified binding to later compiler phases.
qualifiedIdentifierText :: Text -> Text -> Text
qualifiedIdentifierText qualifier member =
  qualifier <> "::" <> member

-- | Build a qualified identifier and classify purity from the rendered
-- qualified text, matching every other identifier construction path.
mkQualifiedIdentifier :: Text -> Text -> Identifier
mkQualifiedIdentifier qualifier member =
  mkIdentifier (qualifiedIdentifierText qualifier member)

-- | Split a canonical qualified identifier into qualifier and member pieces.
-- Nested qualifiers are rejected because the active module system owns exactly
-- one alias/module segment at this boundary.
splitQualifiedIdentifierText :: Text -> Maybe (Text, Text)
splitQualifiedIdentifierText name =
  case Text.breakOn "::" name of
    (qualifier, rest)
      | Text.null qualifier -> Nothing
      | Text.null rest -> Nothing
      | Text.null member -> Nothing
      | Text.isInfixOf "::" member -> Nothing
      | otherwise -> Just (qualifier, member)
      where
        member = Text.drop 2 rest

instance IsString Identifier where
  fromString = mkIdentifier . Text.pack
