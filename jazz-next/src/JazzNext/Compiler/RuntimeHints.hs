{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey (..),
    bindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Name (Name)

data BindingRuntimeHintKey
  = BindingRuntimeHintKey (Maybe [Text]) SourceSpan Name
  | ExplicitTypeApplicationRuntimeHintKey (Maybe [Text]) SourceSpan
  deriving (Eq, Ord, Show)

bindingRuntimeHintKey :: Name -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  bindingRuntimeHintKeyInModule Nothing bindingName bindingSpan

bindingRuntimeHintKeyInModule :: Maybe [Text] -> Name -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKeyInModule modulePath bindingName bindingSpan =
  BindingRuntimeHintKey modulePath bindingSpan bindingName

explicitTypeApplicationRuntimeHintKeyInModule :: Maybe [Text] -> SourceSpan -> BindingRuntimeHintKey
explicitTypeApplicationRuntimeHintKeyInModule =
  ExplicitTypeApplicationRuntimeHintKey
