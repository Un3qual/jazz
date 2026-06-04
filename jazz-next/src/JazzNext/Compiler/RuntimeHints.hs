{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey (..),
    bindingRuntimeHintKey
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
  )

data BindingRuntimeHintKey = BindingRuntimeHintKey SourceSpan Text
  deriving (Eq, Ord, Show)

bindingRuntimeHintKey :: Identifier -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  BindingRuntimeHintKey bindingSpan (identifierText bindingName)
