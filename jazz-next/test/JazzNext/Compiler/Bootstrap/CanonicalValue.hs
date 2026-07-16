{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalValue
  ( CanonicalSourcePath (..),
    CanonicalSpan (..),
    canonicalConstructor,
    canonicalNullaryConstructor,
    canonicalSourcePathRuntimeValue,
    canonicalSpanRuntimeValue,
    canonicalizeSpan,
    normalizeCanonicalSourcePath,
    runtimeIntValue,
  )
where

import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( DataConstructorArgument (DataConstructorArgumentOpaque),
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Name
  ( mkIdentifier,
    sourceName,
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    untypedIntMetadata,
  )

newtype CanonicalSourcePath = CanonicalSourcePath Text
  deriving (Eq, Show)

data CanonicalSpan = CanonicalSpan Int Int
  deriving (Eq, Show)

normalizeCanonicalSourcePath :: FilePath -> Either Text CanonicalSourcePath
normalizeCanonicalSourcePath sourcePath
  | null sourcePath = Left "canonical source path must not be empty"
  | '\\' `elem` sourcePath = Left "canonical source path must use '/' separators"
  | isLogicalAbsolute sourcePath = Left "canonical source path must be relative"
  | any (== "..") segments = Left "canonical source path must not contain '..'"
  | null normalizedSegments = Left "canonical source path must not be empty"
  | otherwise = Right (CanonicalSourcePath (Text.intercalate "/" normalizedSegments))
  where
    segments = Text.splitOn "/" (Text.pack sourcePath)
    normalizedSegments = filter (\segment -> not (Text.null segment) && segment /= ".") segments

isLogicalAbsolute :: FilePath -> Bool
isLogicalAbsolute sourcePath =
  case sourcePath of
    '/' : _ -> True
    drive : ':' : '/' : _ -> isAlpha drive
    _ -> False

canonicalizeSpan :: SourceSpan -> CanonicalSpan
canonicalizeSpan spanValue = CanonicalSpan (spanLine spanValue) (spanColumn spanValue)

canonicalSourcePathRuntimeValue :: CanonicalSourcePath -> RuntimeValue
canonicalSourcePathRuntimeValue (CanonicalSourcePath sourcePath) =
  canonicalConstructor "CanonicalSourcePath" [VText sourcePath]

canonicalSpanRuntimeValue :: CanonicalSpan -> RuntimeValue
canonicalSpanRuntimeValue (CanonicalSpan line column) =
  canonicalConstructor "CanonicalSpan" [runtimeIntValue line, runtimeIntValue column]

canonicalNullaryConstructor :: Text -> RuntimeValue
canonicalNullaryConstructor name = canonicalConstructor name []

canonicalConstructor :: Text -> [RuntimeValue] -> RuntimeValue
canonicalConstructor name arguments =
  VConstructor
    (sourceName (mkIdentifier name))
    []
    (sourceName (mkIdentifier name))
    (replicate (length arguments) DataConstructorArgumentOpaque)
    arguments

runtimeIntValue :: Int -> RuntimeValue
runtimeIntValue value = VInt (fromIntegral value) untypedIntMetadata
