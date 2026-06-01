{-# LANGUAGE OverloadedStrings #-}

-- | Generates the compiler-owned bundled prelude used when callers do not
-- supply an explicit prelude file.
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludePath,
    bundledPreludeSource,
    loadBundledPreludeSource
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BuiltinCatalog
  ( allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName
  )

-- | Repository-relative location of the checked-in bundled prelude mirror.
bundledPreludePath :: FilePath
bundledPreludePath = "jazz-next/stdlib/Prelude.jz"

-- | Pre-generated prelude text that exposes all builtin kernel bridges and
-- their public aliases in a deterministic order.
bundledPreludeSource :: Text
bundledPreludeSource =
  Text.unlines $
    map renderCapabilityClass canonicalCapabilityClassNames
      <> [""]
      <> map renderDefaultCapabilityImpl defaultCapabilityImplFacts
      <> [""]
      <> map renderKernelBridge allBuiltinSymbols
      <> [""]
      <> map renderPublicAlias allBuiltinSymbols
  where
    renderCapabilityClass name =
      "class " <> name <> " { }."

    renderDefaultCapabilityImpl (className, targetType) =
      "impl " <> className <> "(" <> targetType <> ") { }."

    -- Kernel bridge bindings must precede public aliases so alias definitions
    -- can reference already-declared names in the checked-in mirror.
    renderKernelBridge symbol =
      let kernelName = builtinSymbolKernelName symbol
       in kernelName <> " = " <> kernelName <> "."

    renderPublicAlias symbol =
      builtinSymbolName symbol <> " = " <> builtinSymbolKernelName symbol <> "."

canonicalCapabilityClassNames :: [Text]
canonicalCapabilityClassNames =
  [ "Eq",
    "Ord",
    "Num",
    "Integral",
    "Fractional",
    "Showable",
    "Default"
  ]

defaultCapabilityImplFacts :: [(Text, Text)]
defaultCapabilityImplFacts =
  [ ("Eq", "Int"),
    ("Eq", "Float"),
    ("Eq", "Bool"),
    ("Ord", "Int"),
    ("Ord", "Float"),
    ("Num", "Int"),
    ("Num", "Float"),
    ("Integral", "Int"),
    ("Fractional", "Float"),
    ("Default", "Int"),
    ("Default", "Float"),
    ("Default", "Bool"),
    ("Showable", "Int"),
    ("Showable", "Float"),
    ("Showable", "Bool")
  ]

-- | IO wrapper kept for API symmetry with file-backed prelude loading paths.
loadBundledPreludeSource :: IO Text
loadBundledPreludeSource =
  pure bundledPreludeSource
