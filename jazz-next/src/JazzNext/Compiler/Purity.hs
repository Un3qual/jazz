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

-- | Check the only active impurity marker. The parser permits ordinary text;
-- callers use this predicate when they need the raw spelling rule directly.
isImpureName :: Text -> Bool
isImpureName name = Text.isSuffixOf "!" name

-- | Collapse the current naming convention into a stable enum so downstream
-- phases do not need to know how impurity is spelled.
namePurity :: Text -> Purity
namePurity name =
  if isImpureName name
    then Impure
    else Pure
