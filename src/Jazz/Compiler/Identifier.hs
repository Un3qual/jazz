{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Compiler identifiers and their source-spelling operations.
module Jazz.Compiler.Identifier
  ( Identifier,
    IdentifierLike (..),
    isIdentifierContinuationCharacter,
    isIdentifierStartCharacter,
    mkIdentifier,
  )
where

import Control.DeepSeq (NFData)
import Data.Char (isAlpha, isAlphaNum)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.Purity (Purity)
import qualified Jazz.Compiler.Purity as Purity

-- | A source identifier paired with the purity implied by its spelling.
data Identifier = Identifier Text Purity
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

class IdentifierLike name where
  identifierText :: name -> Text
  identifierPurity :: name -> Purity

instance IdentifierLike Identifier where
  identifierText (Identifier name _) = name
  identifierPurity (Identifier _ purity) = purity

mkIdentifier :: Text -> Identifier
mkIdentifier name = Identifier name (Purity.namePurity name)

isIdentifierStartCharacter :: Char -> Bool
isIdentifierStartCharacter character = isAlpha character || character == '_'

isIdentifierContinuationCharacter :: Char -> Bool
isIdentifierContinuationCharacter character =
  isAlphaNum character || character == '_' || character == '\'' || character == '!'

instance IsString Identifier where
  fromString = mkIdentifier . Text.pack
