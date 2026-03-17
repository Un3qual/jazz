{-# LANGUAGE OverloadedStrings #-}

-- | Minimal purity classification derived from identifier spelling. The `!`
-- suffix remains the only source of truth for stub-v1 purity rules.
module JazzNext.Compiler.Purity
  ( Purity (..),
    isImpureName,
    namePurity
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Effect marker inferred from a binding or callee name.
data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

isImpureName :: Text -> Bool
isImpureName name = Text.isSuffixOf "!" name

-- | Collapse the current naming convention into a stable enum so downstream
-- phases do not need to know how impurity is spelled.
namePurity :: Text -> Purity
namePurity name =
  if isImpureName name
    then Impure
    else Pure
