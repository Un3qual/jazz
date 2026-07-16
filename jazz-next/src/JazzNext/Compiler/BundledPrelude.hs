{-# LANGUAGE OverloadedStrings #-}

-- | Generates the compiler-owned bundled prelude used when callers do not
-- supply an explicit prelude file.
module JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource,
    loadBundledPreludeSource,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( NumericType (..),
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinOwnership (PreludeTarget),
    allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName,
    builtinSymbolOwnership,
    renderNumericTypeName,
  )

-- | Pre-generated prelude text that exposes all builtin kernel bridges and
-- their public aliases in a deterministic order.
bundledPreludeSource :: Text
bundledPreludeSource =
  Text.unlines $
    ["data Ordering = LT | EQ | GT.", ""]
      <> map renderCapabilityClass canonicalCapabilityClasses
      <> ["", compareTextBinding]
      <> [""]
      <> map renderDefaultCapabilityImpl defaultCapabilityImpls
      <> [""]
      <> map renderKernelBridge allBuiltinSymbols
      <> [""]
      <> map renderPublicAlias preludeTargetSymbols
      <> map renderDefaultConversionAlias defaultConversionAliases
  where
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

data CapabilityClass
  = EqualityClass
  | OrderingClass
  | NumericClass
  | IntegralClass
  | FractionalClass
  | ShowableClass
  | DefaultClass

data PreludeTargetType
  = PreludeInt
  | PreludeFloat
  | PreludeBool
  | PreludeChar
  | PreludeText
  | PreludeNumeric NumericType

data OrderedPreludeTargetType
  = OrderedPreludeInt
  | OrderedPreludeFloat
  | OrderedPreludeChar
  | OrderedPreludeText
  | OrderedPreludeNumeric NumericType

data MarkerCapability
  = NumericMarker
  | IntegralMarker
  | FractionalMarker

data DefaultCapabilityImpl
  = EqualityImpl PreludeTargetType
  | OrderingImpl OrderedPreludeTargetType
  | MarkerImpl MarkerCapability PreludeTargetType
  | ShowableImpl PreludeTargetType
  | DefaultImpl PreludeTargetType

renderCapabilityClass :: CapabilityClass -> Text
renderCapabilityClass capabilityClass =
  case capabilityClass of
    EqualityClass ->
      renderClassWithMethod "Eq" "equals :: a -> a -> Bool."
    OrderingClass ->
      renderClassWithMethod "Ord" "compare :: a -> a -> Ordering."
    NumericClass ->
      renderMarkerClass "Num"
    IntegralClass ->
      renderMarkerClass "Integral"
    FractionalClass ->
      renderMarkerClass "Fractional"
    ShowableClass ->
      renderClassWithMethod "Showable" "show :: a -> Text."
    DefaultClass ->
      renderClassWithMethod "Default" "defaultValue :: a."

renderClassWithMethod :: Text -> Text -> Text
renderClassWithMethod className methodSignature =
  Text.intercalate
    "\n"
    [ "class " <> className <> "(a) {",
      methodSignature,
      "}."
    ]

renderMarkerClass :: Text -> Text
renderMarkerClass className =
  "class " <> className <> "(a) { }."

renderDefaultCapabilityImpl :: DefaultCapabilityImpl -> Text
renderDefaultCapabilityImpl capabilityImpl =
  case capabilityImpl of
    EqualityImpl targetType ->
      renderMethodImpl
        "Eq"
        (renderPreludeTargetType targetType)
        "equals"
        "\\(left, right) -> left == right"
    OrderingImpl targetType ->
      renderMethodImpl
        "Ord"
        (renderOrderedPreludeTargetType targetType)
        "compare"
        (orderingExpression targetType)
    MarkerImpl marker targetType ->
      renderEmptyImpl
        (renderMarkerCapability marker)
        (renderPreludeTargetType targetType)
    ShowableImpl targetType ->
      renderMethodImpl
        "Showable"
        (renderPreludeTargetType targetType)
        "show"
        "__kernel_renderValue"
    DefaultImpl targetType ->
      renderMethodImpl
        "Default"
        (renderPreludeTargetType targetType)
        "defaultValue"
        (defaultValueExpression targetType)

renderMethodImpl :: Text -> Text -> Text -> Text -> Text
renderMethodImpl className targetType methodName methodExpression =
  Text.intercalate
    "\n"
    [ "impl " <> className <> "(" <> targetType <> ") {",
      methodName <> " = " <> methodExpression <> ".",
      "}."
    ]

renderEmptyImpl :: Text -> Text -> Text
renderEmptyImpl className targetType =
  "impl " <> className <> "(" <> targetType <> ") { }."

renderPreludeTargetType :: PreludeTargetType -> Text
renderPreludeTargetType targetType =
  case targetType of
    PreludeInt -> "Int"
    PreludeFloat -> "Float"
    PreludeBool -> "Bool"
    PreludeChar -> "Char"
    PreludeText -> "Text"
    PreludeNumeric numericType -> renderNumericTypeName numericType

renderOrderedPreludeTargetType :: OrderedPreludeTargetType -> Text
renderOrderedPreludeTargetType targetType =
  case targetType of
    OrderedPreludeInt -> "Int"
    OrderedPreludeFloat -> "Float"
    OrderedPreludeChar -> "Char"
    OrderedPreludeText -> "Text"
    OrderedPreludeNumeric numericType -> renderNumericTypeName numericType

renderMarkerCapability :: MarkerCapability -> Text
renderMarkerCapability marker =
  case marker of
    NumericMarker -> "Num"
    IntegralMarker -> "Integral"
    FractionalMarker -> "Fractional"

orderingExpression :: OrderedPreludeTargetType -> Text
orderingExpression targetType =
  case targetType of
    OrderedPreludeChar ->
      "\\(left, right) -> if __kernel_charToUInt32 left < __kernel_charToUInt32 right then LT else if __kernel_charToUInt32 left > __kernel_charToUInt32 right then GT else EQ"
    OrderedPreludeText ->
      "__prelude_compareText"
    OrderedPreludeInt ->
      numericOrderingExpression
    OrderedPreludeFloat ->
      numericOrderingExpression
    OrderedPreludeNumeric _ ->
      numericOrderingExpression

numericOrderingExpression :: Text
numericOrderingExpression =
  "\\(left, right) -> if left < right then LT else if left > right then GT else EQ"

defaultValueExpression :: PreludeTargetType -> Text
defaultValueExpression targetType =
  case targetType of
    PreludeInt -> "0"
    PreludeFloat -> "0.0"
    PreludeBool -> "False"
    PreludeChar -> "'\\0'"
    PreludeText -> "\"\""
    PreludeNumeric numericType ->
      case numericType of
        NumericInt8 -> "0"
        NumericInt16 -> "0"
        NumericInt32 -> "0"
        NumericInt64 -> "0"
        NumericUInt8 -> "0"
        NumericUInt16 -> "0"
        NumericUInt32 -> "0"
        NumericUInt64 -> "0"
        NumericFloat16 -> "__kernel_toFloat16 0"
        NumericFloat32 -> "__kernel_toFloat32 0"
        NumericFloat64 -> "__kernel_toFloat64 0"

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

canonicalCapabilityClasses :: [CapabilityClass]
canonicalCapabilityClasses =
  [ EqualityClass,
    OrderingClass,
    NumericClass,
    IntegralClass,
    FractionalClass,
    ShowableClass,
    DefaultClass
  ]

defaultCapabilityImpls :: [DefaultCapabilityImpl]
defaultCapabilityImpls =
  defaultAliasCapabilityImpls
    <> concatMap integralNumericCapabilityImplFacts signedIntegerWidthTypes
    <> concatMap integralNumericCapabilityImplFacts unsignedIntegerWidthTypes
    <> concatMap floatingNumericCapabilityImplFacts floatingWidthTypes

defaultAliasCapabilityImpls :: [DefaultCapabilityImpl]
defaultAliasCapabilityImpls =
  [ EqualityImpl PreludeInt,
    EqualityImpl PreludeFloat,
    EqualityImpl PreludeBool,
    EqualityImpl PreludeChar,
    EqualityImpl PreludeText,
    OrderingImpl OrderedPreludeInt,
    OrderingImpl OrderedPreludeFloat,
    OrderingImpl OrderedPreludeChar,
    OrderingImpl OrderedPreludeText,
    MarkerImpl NumericMarker PreludeInt,
    MarkerImpl NumericMarker PreludeFloat,
    MarkerImpl IntegralMarker PreludeInt,
    MarkerImpl FractionalMarker PreludeFloat,
    DefaultImpl PreludeInt,
    DefaultImpl PreludeFloat,
    DefaultImpl PreludeBool,
    DefaultImpl PreludeChar,
    DefaultImpl PreludeText,
    ShowableImpl PreludeInt,
    ShowableImpl PreludeFloat,
    ShowableImpl PreludeBool,
    ShowableImpl PreludeChar,
    ShowableImpl PreludeText
  ]

integralNumericCapabilityImplFacts :: NumericType -> [DefaultCapabilityImpl]
integralNumericCapabilityImplFacts numericType =
  [ EqualityImpl targetType,
    OrderingImpl (OrderedPreludeNumeric numericType),
    MarkerImpl NumericMarker targetType,
    MarkerImpl IntegralMarker targetType,
    DefaultImpl targetType,
    ShowableImpl targetType
  ]
  where
    targetType = PreludeNumeric numericType

floatingNumericCapabilityImplFacts :: NumericType -> [DefaultCapabilityImpl]
floatingNumericCapabilityImplFacts numericType =
  [ EqualityImpl targetType,
    OrderingImpl (OrderedPreludeNumeric numericType),
    MarkerImpl NumericMarker targetType,
    MarkerImpl FractionalMarker targetType,
    DefaultImpl targetType,
    ShowableImpl targetType
  ]
  where
    targetType = PreludeNumeric numericType

signedIntegerWidthTypes :: [NumericType]
signedIntegerWidthTypes =
  [ NumericInt8,
    NumericInt16,
    NumericInt32,
    NumericInt64
  ]

unsignedIntegerWidthTypes :: [NumericType]
unsignedIntegerWidthTypes =
  [ NumericUInt8,
    NumericUInt16,
    NumericUInt32,
    NumericUInt64
  ]

floatingWidthTypes :: [NumericType]
floatingWidthTypes =
  [ NumericFloat16,
    NumericFloat32,
    NumericFloat64
  ]

-- | IO wrapper kept for API symmetry with file-backed prelude loading paths.
loadBundledPreludeSource :: IO Text
loadBundledPreludeSource =
  pure bundledPreludeSource
