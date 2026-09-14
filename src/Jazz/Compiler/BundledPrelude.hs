{-# LANGUAGE OverloadedStrings #-}

-- | Generates the compiler-owned bundled prelude used when callers do not
-- supply an explicit prelude file.
module Jazz.Compiler.BundledPrelude
  ( bundledPreludeIdentity,
    bundledPreludeSource,
    loadBundledPreludeSource,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinOwnership (PreludeTarget),
    allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName,
    builtinSymbolOwnership,
    renderNumericTypeName,
  )
import Jazz.Compiler.ModuleIdentity
  ( ModuleIdentity,
    mkSourceFile,
    moduleIdentity,
    preludeModulePath,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))

bundledPreludeIdentity :: ModuleIdentity
bundledPreludeIdentity =
  moduleIdentity
    preludeModulePath
    (mkSourceFile "jazz/stdlib/Prelude.jz")

-- | Pre-generated prelude text that exposes all builtin kernel bridges and
-- their public aliases in a deterministic order.
bundledPreludeSource :: Text
bundledPreludeSource =
  Text.unlines $
    [ "data Ordering = LT | EQ | GT.",
      "",
      "not :: Bool -> Bool.",
      "not = \\(condition) -> if condition then False else True.",
      ""
    ]
      <> map renderCapabilityClass canonicalCapabilityClasses
      <> ["", compareTextBinding]
      <> [""]
      <> map renderDefaultCapabilityImpl defaultCapabilityImpls
      <> ["", collectionCapabilityImpls]
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
  | MappingClass
  | ReductionClass
  | CombinationClass

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
      Text.intercalate
        "\n"
        [ "class Equatable(a) {",
          "equals :: a -> a -> Bool.",
          "differs :: a -> a -> Bool.",
          "differs = \\(left, right) -> not (equals left right).",
          "}."
        ]
    OrderingClass ->
      Text.intercalate
        "\n"
        [ "class @{Equatable(a)}: Comparable(a) {",
          "compare :: a -> a -> Ordering.",
          "}."
        ]
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
    MappingClass ->
      Text.intercalate
        "\n"
        [ "class Mappable(f) {",
          "map :: (a -> b) -> f(a) -> f(b).",
          "}."
        ]
    ReductionClass ->
      Text.intercalate
        "\n"
        [ "class Reducible(f) {",
          "foldLeft :: (b -> a -> b) -> b -> f(a) -> b.",
          "foldRight :: (a -> b -> b) -> b -> f(a) -> b.",
          "}."
        ]
    CombinationClass ->
      Text.intercalate
        "\n"
        [ "class Combinable(a) {",
          "combine :: a -> a -> a.",
          "}."
        ]

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
        "Equatable"
        (renderPreludeTargetType targetType)
        "equals"
        "\\(left, right) -> left == right"
    OrderingImpl targetType ->
      renderMethodImpl
        "Comparable"
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
      "  | [(rightFirst, rightRest) | _] -> LT",
      "  }",
      "| [(leftFirst, leftRest) | _] -> case __kernel_textUnconsRaw right {",
      "  | [] -> GT",
      "  | [(rightFirst, rightRest) | _] -> if __kernel_charToUInt32 leftFirst < __kernel_charToUInt32 rightFirst then LT else if __kernel_charToUInt32 leftFirst > __kernel_charToUInt32 rightFirst then GT else __prelude_compareText leftRest rightRest",
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
    DefaultClass,
    MappingClass,
    ReductionClass,
    CombinationClass
  ]

defaultCapabilityImpls :: [DefaultCapabilityImpl]
defaultCapabilityImpls =
  defaultAliasCapabilityImpls
    <> concatMap (numericCapabilityImpls IntegralMarker) (signedIntegerWidthTypes <> unsignedIntegerWidthTypes)
    <> concatMap (numericCapabilityImpls FractionalMarker) floatingWidthTypes

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

numericCapabilityImpls :: MarkerCapability -> NumericType -> [DefaultCapabilityImpl]
numericCapabilityImpls marker numericType =
  [ EqualityImpl targetType,
    OrderingImpl (OrderedPreludeNumeric numericType),
    MarkerImpl NumericMarker targetType,
    MarkerImpl marker targetType,
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

collectionCapabilityImpls :: Text
collectionCapabilityImpls =
  Text.intercalate
    "\n"
    [ "impl @{Equatable(a)}: Equatable([a]) {",
      "equals = \\(left, right) -> case (left, right) {",
      "| ([], []) -> True",
      "| ([x | xs], [y | ys]) -> if equals x y then equals xs ys else False",
      "| _ -> False",
      "}.",
      "}.",
      "impl @{Equatable(a), Equatable(b)}: Equatable((a, b)) {",
      "equals = \\((a, b), (x, y)) -> if equals a x then equals b y else False.",
      "}.",
      "impl @{Equatable(a), Equatable(b), Equatable(c)}: Equatable((a, b, c)) {",
      "equals = \\((a, b, c), (x, y, z)) -> if equals a x then if equals b y then equals c z else False else False.",
      "}.",
      "impl Mappable(List) {",
      "map = __kernel_map.",
      "}.",
      "impl Reducible(List) {",
      "foldLeft = \\(step, initial, values) -> case values {",
      "| [] -> initial",
      "| [first | rest] -> foldLeft step (step initial first) rest",
      "}.",
      "foldRight = \\(step, initial, values) -> foldLeft (\\(acc, item) -> step item acc) initial (__kernel_listReverseRaw values).",
      "}.",
      "impl Combinable([a]) {",
      "combine = \\(left, right) -> foldRight __kernel_listPrependRaw right left.",
      "}.",
      "impl Combinable(Text) {",
      "combine = __kernel_textAppend.",
      "}."
    ]
