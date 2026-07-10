{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey (..),
    bindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
  )

data BindingRuntimeHintKey = BindingRuntimeHintKey (Maybe [Text]) SourceSpan Text
  deriving (Eq, Ord, Show)

bindingRuntimeHintKey :: Identifier -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  bindingRuntimeHintKeyInModule Nothing bindingName bindingSpan

bindingRuntimeHintKeyInModule :: Maybe [Text] -> Identifier -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKeyInModule modulePath bindingName bindingSpan =
  BindingRuntimeHintKey modulePath bindingSpan (identifierText bindingName)
