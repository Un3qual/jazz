{-# LANGUAGE OverloadedStrings #-}

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

data Identifier = Identifier
  { identifierText :: Text,
    identifierPurity :: Purity
  }
  deriving (Eq, Show)

mkIdentifier :: Text -> Identifier
mkIdentifier name =
  Identifier
    { identifierText = name,
      identifierPurity = namePurity name
    }

instance IsString Identifier where
  fromString = mkIdentifier . Text.pack
