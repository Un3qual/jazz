{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.RuntimeHints
  ( BindingRuntimeHintKey (..),
    bindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule,
    explicitTypeApplicationRuntimeHintKeyInModule,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
  )
import Jazz.Compiler.Name (Name)

data BindingRuntimeHintKey
  = BindingRuntimeHintKey (Maybe [Text]) SourceSpan Name
  | ExplicitTypeApplicationRuntimeHintKey (Maybe [Text]) SourceSpan
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

bindingRuntimeHintKey :: Name -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  bindingRuntimeHintKeyInModule Nothing bindingName bindingSpan

bindingRuntimeHintKeyInModule :: Maybe [Text] -> Name -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKeyInModule modulePath bindingName bindingSpan =
  BindingRuntimeHintKey modulePath bindingSpan bindingName

explicitTypeApplicationRuntimeHintKeyInModule :: Maybe [Text] -> SourceSpan -> BindingRuntimeHintKey
explicitTypeApplicationRuntimeHintKeyInModule =
  ExplicitTypeApplicationRuntimeHintKey
