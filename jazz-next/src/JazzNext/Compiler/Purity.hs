{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Purity
  ( Purity (..),
    isImpureName,
    namePurity
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

isImpureName :: Text -> Bool
isImpureName name = Text.isSuffixOf "!" name

namePurity :: Text -> Purity
namePurity name =
  if isImpureName name
    then Impure
    else Pure
