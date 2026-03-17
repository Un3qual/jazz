{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource,
    loadBundledPreludeSource
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BuiltinCatalog
  ( allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName
  )

-- Keep the bundled prelude aligned with the builtin catalog so default source
-- flows and explicit Prelude.jz updates cannot drift independently.
bundledPreludeSource :: Text
bundledPreludeSource =
  Text.unlines $
    map renderKernelBridge allBuiltinSymbols
      <> [""]
      <> map renderPublicAlias allBuiltinSymbols
  where
    renderKernelBridge symbol =
      let kernelName = builtinSymbolKernelName symbol
       in kernelName <> " = " <> kernelName <> "."

    renderPublicAlias symbol =
      builtinSymbolName symbol <> " = " <> builtinSymbolKernelName symbol <> "."

loadBundledPreludeSource :: IO Text
loadBundledPreludeSource =
  pure bundledPreludeSource
