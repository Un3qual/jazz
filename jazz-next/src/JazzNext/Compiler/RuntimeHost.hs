{-# LANGUAGE OverloadedStrings #-}

-- | Typed host capabilities used by effectful runtime evaluation. The record
-- keeps stage-0 Haskell operations outside Jazz values and can be replaced by
-- a native-runtime implementation without changing the public Jazz modules.
module JazzNext.Compiler.RuntimeHost
  ( HostIOCategory (..),
    HostIOFailure (..),
    RuntimeHost (..),
    disabledRuntimeHost,
    productionRuntimeHost,
    hostIOCategoryToken,
    hostIOFailureMessage
  ) where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import GHC.IO.Exception
  ( IOErrorType (Interrupted, UnsupportedOperation)
  )
import JazzNext.Compiler.Profiling
  ( CompilerStage (HostOperationStage),
    withCompilerStage
  )
import System.Environment (getArgs)
import System.Exit
  ( ExitCode (..),
    exitWith
  )
import System.IO
  ( Handle,
    stderr,
    stdin,
    stdout
  )
import System.IO.Error
  ( ioeGetErrorType,
    isAlreadyExistsError,
    isDoesNotExistError,
    isFullError,
    isIllegalOperation,
    isPermissionError,
    tryIOError
  )

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
    runtimeHostExit :: Integer -> m (Either HostIOFailure ())
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
      runtimeHostExit = \_ -> pure unsupported
    }
  where
    unsupported = Left (HostIOFailure HostUnsupported (hostIOFailureMessage HostUnsupported))

productionRuntimeHost :: RuntimeHost IO
productionRuntimeHost =
  RuntimeHost
    { runtimeHostReadText = profileHostOperation . readUtf8File,
      runtimeHostWriteText = \path contents -> profileHostOperation (writeUtf8File path contents),
      runtimeHostReadStdin = profileHostOperation (readUtf8Handle stdin),
      runtimeHostWriteStdout = profileHostOperation . writeUtf8Handle stdout,
      runtimeHostWriteStderr = profileHostOperation . writeUtf8Handle stderr,
      runtimeHostArguments = profileHostOperation (map Text.pack <$> getArgs),
      runtimeHostExit = \status ->
        profileHostOperation
          ( exitWith
              ( if status == 0
                  then ExitSuccess
                  else ExitFailure (fromInteger status)
              )
          )
    }

profileHostOperation :: IO value -> IO value
profileHostOperation action =
  {-# SCC "jazz-stage:host-operation" #-}
  withCompilerStage HostOperationStage action

readUtf8File :: Text -> IO (Either HostIOFailure Text)
readUtf8File path = do
  bytesResult <- captureHostIO (ByteString.readFile (Text.unpack path))
  pure (bytesResult >>= decodeUtf8)

writeUtf8File :: Text -> Text -> IO (Either HostIOFailure ())
writeUtf8File path contents =
  captureHostIO
    (ByteString.writeFile (Text.unpack path) (TextEncoding.encodeUtf8 contents))

readUtf8Handle :: Handle -> IO (Either HostIOFailure Text)
readUtf8Handle handle = do
  bytesResult <- captureHostIO (ByteString.hGetContents handle)
  pure (bytesResult >>= decodeUtf8)

writeUtf8Handle :: Handle -> Text -> IO (Either HostIOFailure ())
writeUtf8Handle handle contents =
  captureHostIO (ByteString.hPut handle (TextEncoding.encodeUtf8 contents))

decodeUtf8 :: ByteString.ByteString -> Either HostIOFailure Text
decodeUtf8 bytes =
  case TextEncoding.decodeUtf8' bytes of
    Left _ -> Left (normalizedHostFailure HostInvalidData)
    Right contents -> Right contents

captureHostIO :: IO value -> IO (Either HostIOFailure value)
captureHostIO action = do
  result <- tryIOError action
  pure $
    case result of
      Left hostError -> Left (normalizedHostFailure (classifyHostIOError hostError))
      Right value -> Right value

classifyHostIOError :: IOError -> HostIOCategory
classifyHostIOError hostError
  | isDoesNotExistError hostError = HostNotFound
  | isPermissionError hostError = HostPermissionDenied
  | isAlreadyExistsError hostError = HostAlreadyExists
  | isFullError hostError = HostResourceExhausted
  | ioeGetErrorType hostError == Interrupted = HostInterrupted
  | isIllegalOperation hostError = HostUnsupported
  | ioeGetErrorType hostError == UnsupportedOperation = HostUnsupported
  | otherwise = HostOther

normalizedHostFailure :: HostIOCategory -> HostIOFailure
normalizedHostFailure category =
  HostIOFailure category (hostIOFailureMessage category)

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
