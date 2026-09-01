{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
import Jazz.Compiler.Name (ResolvedName)

data BindingRuntimeHintKey
  = BindingRuntimeHintKey (Maybe [Text]) SourceSpan ResolvedName
  | ExplicitTypeApplicationRuntimeHintKey (Maybe [Text]) SourceSpan
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

bindingRuntimeHintKey :: ResolvedName -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKey bindingName bindingSpan =
  bindingRuntimeHintKeyInModule Nothing bindingName bindingSpan

bindingRuntimeHintKeyInModule :: Maybe [Text] -> ResolvedName -> SourceSpan -> BindingRuntimeHintKey
bindingRuntimeHintKeyInModule modulePath bindingName bindingSpan =
  BindingRuntimeHintKey modulePath bindingSpan bindingName

explicitTypeApplicationRuntimeHintKeyInModule :: Maybe [Text] -> SourceSpan -> BindingRuntimeHintKey
explicitTypeApplicationRuntimeHintKeyInModule =
  ExplicitTypeApplicationRuntimeHintKey
