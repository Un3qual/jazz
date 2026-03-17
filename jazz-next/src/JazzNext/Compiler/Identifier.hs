{-# LANGUAGE OverloadedStrings #-}

-- | Identifier wrapper that keeps the original text and the purity implied by
-- the current naming convention.
module JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    identifierPurity,
    mkIdentifier
  ) where

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

instance IsString Identifier where
  fromString = mkIdentifier . Text.pack
