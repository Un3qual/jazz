{-# LANGUAGE OverloadedStrings #-}

-- | Generates the compiler-owned bundled prelude used when callers do not
-- supply an explicit prelude file.
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource,
    loadBundledPreludeSource
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinOwnership (PreludeTarget),
    allBuiltinSymbols,
    builtinSymbolOwnership,
    builtinSymbolKernelName,
    builtinSymbolName
  )

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
      <> map renderPublicAlias preludeTargetSymbols
      <> map renderDefaultConversionAlias defaultConversionAliases
  where
    renderCapabilityClass name =
      case name of
        "Eq" ->
          Text.intercalate
            "\n"
            [ "class Eq(a) {",
              "equals :: a -> a -> Bool.",
              "}."
            ]
        _ ->
          "class " <> name <> "(a) { }."

    renderDefaultCapabilityImpl (className, targetType) =
      case (className, targetType) of
        ("Eq", targetType')
          | targetType' `elem` ["Int", "Float", "Bool", "Char", "Text", "Float16", "Float32", "Float64"] ->
              renderEqImpl targetType'
        _ ->
          "impl " <> className <> "(" <> targetType <> ") { }."

    renderEqImpl targetType =
      Text.intercalate
        "\n"
        [ "impl Eq(" <> targetType <> ") {",
          "equals = \\(left, right) -> left == right.",
          "}."
        ]

    -- Kernel bridge bindings must precede public aliases so alias definitions
    -- can reference already-declared names in the checked-in mirror.
    renderKernelBridge symbol =
      let kernelName = builtinSymbolKernelName symbol
       in kernelName <> " = " <> kernelName <> "."

    renderPublicAlias symbol =
      builtinSymbolName symbol <> " = " <> builtinSymbolKernelName symbol <> "."

    renderDefaultConversionAlias (aliasName, targetName) =
      aliasName <> " = " <> targetName <> "."

    preludeTargetSymbols =
      filter ((== PreludeTarget) . builtinSymbolOwnership) allBuiltinSymbols

defaultConversionAliases :: [(Text, Text)]
defaultConversionAliases =
  [ ("toInt", "toInt64"),
    ("toFloat", "toFloat64")
  ]

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
  defaultAliasCapabilityImplFacts
    <> concatMap integralNumericCapabilityImplFacts signedIntegerWidthTypes
    <> concatMap integralNumericCapabilityImplFacts unsignedIntegerWidthTypes
    <> concatMap floatingNumericCapabilityImplFacts floatingWidthTypes

defaultAliasCapabilityImplFacts :: [(Text, Text)]
defaultAliasCapabilityImplFacts =
  [ ("Eq", "Int"),
    ("Eq", "Float"),
    ("Eq", "Bool"),
    ("Eq", "Char"),
    ("Eq", "Text"),
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

integralNumericCapabilityImplFacts :: Text -> [(Text, Text)]
integralNumericCapabilityImplFacts targetType =
  map
    (\className -> (className, targetType))
    [ "Eq",
      "Ord",
      "Num",
      "Integral",
      "Default",
      "Showable"
    ]

floatingNumericCapabilityImplFacts :: Text -> [(Text, Text)]
floatingNumericCapabilityImplFacts targetType =
  map
    (\className -> (className, targetType))
    [ "Eq",
      "Ord",
      "Num",
      "Fractional",
      "Default",
      "Showable"
    ]

signedIntegerWidthTypes :: [Text]
signedIntegerWidthTypes =
  [ "Int8",
    "Int16",
    "Int32",
    "Int64"
  ]

unsignedIntegerWidthTypes :: [Text]
unsignedIntegerWidthTypes =
  [ "UInt8",
    "UInt16",
    "UInt32",
    "UInt64"
  ]

floatingWidthTypes :: [Text]
floatingWidthTypes =
  [ "Float16",
    "Float32",
    "Float64"
  ]

-- | IO wrapper kept for API symmetry with file-backed prelude loading paths.
loadBundledPreludeSource :: IO Text
loadBundledPreludeSource =
  pure bundledPreludeSource
