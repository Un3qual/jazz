{-# LANGUAGE OverloadedStrings #-}

-- | Typed host capabilities used by effectful runtime evaluation. The record
-- keeps stage-0 Haskell operations outside Jazz values and can be replaced by
-- a native-runtime implementation without changing the public Jazz modules.
module JazzNext.Compiler.RuntimeHost
  ( HostIOCategory (..),
    HostIOFailure (..),
    RuntimeHost (..),
    disabledRuntimeHost,
    hostIOCategoryToken,
    hostIOFailureMessage
  ) where

import Data.Text (Text)

data HostIOCategory
  = HostNotFound
  | HostPermissionDenied
  | HostAlreadyExists
  | HostInvalidData
  | HostResourceExhausted
  | HostInterrupted
  | HostUnsupported
  | HostOther
  deriving (Eq, Ord, Show)

data HostIOFailure = HostIOFailure
  { hostIOFailureCategory :: HostIOCategory,
    hostIOFailureDetail :: Text
  }
  deriving (Eq, Show)

data RuntimeHost m = RuntimeHost
  { runtimeHostReadText :: Text -> m (Either HostIOFailure Text),
    runtimeHostWriteText :: Text -> Text -> m (Either HostIOFailure ()),
    runtimeHostReadStdin :: m (Either HostIOFailure Text),
    runtimeHostWriteStdout :: Text -> m (Either HostIOFailure ()),
    runtimeHostWriteStderr :: Text -> m (Either HostIOFailure ()),
    runtimeHostArguments :: m [Text],
    runtimeHostExit :: Integer -> m ()
  }

disabledRuntimeHost :: Applicative m => RuntimeHost m
disabledRuntimeHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure unsupported,
      runtimeHostWriteText = \_ _ -> pure unsupported,
      runtimeHostReadStdin = pure unsupported,
      runtimeHostWriteStdout = \_ -> pure unsupported,
      runtimeHostWriteStderr = \_ -> pure unsupported,
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure ()
    }
  where
    unsupported = Left (HostIOFailure HostUnsupported (hostIOFailureMessage HostUnsupported))

hostIOCategoryToken :: HostIOCategory -> Text
hostIOCategoryToken category =
  case category of
    HostNotFound -> "not-found"
    HostPermissionDenied -> "permission-denied"
    HostAlreadyExists -> "already-exists"
    HostInvalidData -> "invalid-data"
    HostResourceExhausted -> "resource-exhausted"
    HostInterrupted -> "interrupted"
    HostUnsupported -> "unsupported"
    HostOther -> "other"

hostIOFailureMessage :: HostIOCategory -> Text
hostIOFailureMessage category =
  case category of
    HostNotFound -> "resource not found"
    HostPermissionDenied -> "permission denied"
    HostAlreadyExists -> "resource already exists"
    HostInvalidData -> "input is not valid UTF-8"
    HostResourceExhausted -> "resource exhausted"
    HostInterrupted -> "operation interrupted"
    HostUnsupported -> "operation unsupported"
    HostOther -> "host I/O failed"
