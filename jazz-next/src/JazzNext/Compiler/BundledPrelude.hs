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
    ["data Ordering = LT | EQ | GT.", ""]
      <> map renderCapabilityClass canonicalCapabilityClassNames
      <> ["", compareTextBinding]
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
        "Ord" ->
          Text.intercalate
            "\n"
            [ "class Ord(a) {",
              "compare :: a -> a -> Ordering.",
              "}."
            ]
        "Showable" ->
          Text.intercalate
            "\n"
            [ "class Showable(a) {",
              "show :: a -> Text.",
              "}."
            ]
        "Default" ->
          Text.intercalate
            "\n"
            [ "class Default(a) {",
              "defaultValue :: a.",
              "}."
            ]
        _ ->
          "class " <> name <> "(a) { }."

    renderDefaultCapabilityImpl (className, targetType) =
      case (className, targetType) of
        ("Eq", targetType')
          | targetType' `elem` ["Int", "Float", "Bool", "Char", "Text", "Float16", "Float32", "Float64"] ->
              renderEqImpl targetType'
        ("Ord", "Char") ->
          renderMethodImpl
            "Ord"
            "Char"
            "compare"
            "\\(left, right) -> if __kernel_charToUInt32 left < __kernel_charToUInt32 right then LT else if __kernel_charToUInt32 left > __kernel_charToUInt32 right then GT else EQ"
        ("Ord", "Text") ->
          renderMethodImpl "Ord" "Text" "compare" "__prelude_compareText"
        ("Ord", targetType') ->
          renderMethodImpl
            "Ord"
            targetType'
            "compare"
            "\\(left, right) -> if left < right then LT else if left > right then GT else EQ"
        ("Showable", targetType') ->
          renderMethodImpl "Showable" targetType' "show" "__kernel_renderValue"
        ("Default", targetType') ->
          renderMethodImpl "Default" targetType' "defaultValue" (defaultValueExpression targetType')
        _ ->
          "impl " <> className <> "(" <> targetType <> ") { }."

    renderEqImpl targetType =
      Text.intercalate
        "\n"
        [ "impl Eq(" <> targetType <> ") {",
          "equals = \\(left, right) -> left == right.",
          "}."
        ]

    renderMethodImpl className targetType methodName methodExpression =
      Text.intercalate
        "\n"
        [ "impl " <> className <> "(" <> targetType <> ") {",
          methodName <> " = " <> methodExpression <> ".",
          "}."
        ]

    defaultValueExpression targetType =
      case targetType of
        "Float" -> "0.0"
        "Float16" -> "__kernel_toFloat16 0"
        "Float32" -> "__kernel_toFloat32 0"
        "Float64" -> "__kernel_toFloat64 0"
        "Bool" -> "False"
        "Char" -> "'\\0'"
        "Text" -> "\"\""
        _ -> "0"

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

compareTextBinding :: Text
compareTextBinding =
  Text.intercalate
    "\n"
    [ "__prelude_compareText :: Text -> Text -> Ordering.",
      "__prelude_compareText = \\(left, right) -> case __kernel_textUnconsRaw left {",
      "| [] -> case __kernel_textUnconsRaw right {",
      "  | [] -> EQ",
      "  | [(rightFirst, rightRest)] -> LT",
      "  }",
      "| [(leftFirst, leftRest)] -> case __kernel_textUnconsRaw right {",
      "  | [] -> GT",
      "  | [(rightFirst, rightRest)] -> if __kernel_charToUInt32 leftFirst < __kernel_charToUInt32 rightFirst then LT else if __kernel_charToUInt32 leftFirst > __kernel_charToUInt32 rightFirst then GT else __prelude_compareText leftRest rightRest",
      "  }",
      "}."
    ]

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
    ("Ord", "Char"),
    ("Ord", "Text"),
    ("Num", "Int"),
    ("Num", "Float"),
    ("Integral", "Int"),
    ("Fractional", "Float"),
    ("Default", "Int"),
    ("Default", "Float"),
    ("Default", "Bool"),
    ("Default", "Char"),
    ("Default", "Text"),
    ("Showable", "Int"),
    ("Showable", "Float"),
    ("Showable", "Bool"),
    ("Showable", "Char"),
    ("Showable", "Text")
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
