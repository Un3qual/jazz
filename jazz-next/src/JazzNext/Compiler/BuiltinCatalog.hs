{-# LANGUAGE OverloadedStrings #-}

-- | Canonical builtin inventory and name-resolution policy shared across
-- analyzer, type inference, runtime, and prelude validation.
module JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinNamesInMode,
    builtinSymbolOwnership,
    builtinSymbolArity,
    builtinSymbolName,
    builtinSymbolKernelName,
    builtinSymbolNumericConversionTarget,
    numericTypeFloatMax,
    numericTypeFloatIntegerBounds,
    numericTypeFromName,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
    numericTypeLiteralIntegerBounds,
    numericTypeSupportsRuntimeArithmetic,
    numericTypeSupportsRuntimeComparison,
    renderNumericTypeName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
    isBuiltinSymbolNameInMode,
    isBuiltinSymbolName,
    isKernelBuiltinSymbolName,
    lookupBuiltinSymbolInMode,
    lookupBuiltinSymbol,
    lookupKernelBuiltinSymbol
  ) where

import Data.List
  ( find
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( NumericType (..)
  )

-- | Selects exactly one builtin naming scheme for a compiler phase: either the
-- kernel bridge names or the older compatibility/public names.
data BuiltinResolutionMode
  = ResolveKernelOnly
  | ResolveCompatibility
  deriving (Eq, Ord, Show)

-- | Declares whether a builtin is conceptually owned by the kernel runtime or
-- should be surfaced through the prelude contract.
data BuiltinOwnership
  = KernelIntrinsic
  | PreludeTarget
  deriving (Eq, Ord, Show)

-- | Stable builtin symbol set shared by all compiler/runtime phases.
data BuiltinSymbol
  = BuiltinMap
  | BuiltinFilter
  | BuiltinHd
  | BuiltinTl
  | BuiltinPrint
  | BuiltinToInt8
  | BuiltinToInt16
  | BuiltinToInt32
  | BuiltinToInt64
  | BuiltinToUInt8
  | BuiltinToUInt16
  | BuiltinToUInt32
  | BuiltinToUInt64
  | BuiltinToFloat16
  | BuiltinToFloat32
  | BuiltinToFloat64
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Exhaustive builtin inventory in declaration order. Generated prelude text
-- and tests rely on this order for reproducible output.
allBuiltinSymbols :: [BuiltinSymbol]
allBuiltinSymbols = [minBound .. maxBound]

-- | Render the accepted builtin names for one resolution mode. The resulting
-- set is intentionally mode-specific so kernel-only phases do not accept
-- compatibility aliases by accident.
builtinNamesInMode :: BuiltinResolutionMode -> Set Text
builtinNamesInMode mode =
  Set.fromList
    [ case mode of
        ResolveKernelOnly -> builtinSymbolKernelName symbol
        ResolveCompatibility -> builtinSymbolName symbol
      | symbol <- allBuiltinSymbols
    ]

-- | Classify the public ownership contract for a builtin independent of the
-- temporary runtime implementation that backs it.
builtinSymbolOwnership :: BuiltinSymbol -> BuiltinOwnership
builtinSymbolOwnership builtinSymbol =
  case builtinSymbol of
    -- Compatibility window: these runtime helpers remain kernel-backed for now,
    -- but the ownership contract marks them as prelude-targeted APIs.
    BuiltinMap -> PreludeTarget
    BuiltinFilter -> PreludeTarget
    BuiltinHd -> PreludeTarget
    BuiltinTl -> PreludeTarget
    BuiltinPrint -> PreludeTarget
    BuiltinToInt8 -> PreludeTarget
    BuiltinToInt16 -> PreludeTarget
    BuiltinToInt32 -> PreludeTarget
    BuiltinToInt64 -> PreludeTarget
    BuiltinToUInt8 -> PreludeTarget
    BuiltinToUInt16 -> PreludeTarget
    BuiltinToUInt32 -> PreludeTarget
    BuiltinToUInt64 -> PreludeTarget
    BuiltinToFloat16 -> PreludeTarget
    BuiltinToFloat32 -> PreludeTarget
    BuiltinToFloat64 -> PreludeTarget

-- | Public compatibility/prelude spelling for a builtin symbol.
builtinSymbolName :: BuiltinSymbol -> Text
builtinSymbolName builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> "map"
    BuiltinFilter -> "filter"
    BuiltinHd -> "hd"
    BuiltinTl -> "tl"
    BuiltinPrint -> "print!"
    BuiltinToInt8 -> "toInt8"
    BuiltinToInt16 -> "toInt16"
    BuiltinToInt32 -> "toInt32"
    BuiltinToInt64 -> "toInt64"
    BuiltinToUInt8 -> "toUInt8"
    BuiltinToUInt16 -> "toUInt16"
    BuiltinToUInt32 -> "toUInt32"
    BuiltinToUInt64 -> "toUInt64"
    BuiltinToFloat16 -> "toFloat16"
    BuiltinToFloat32 -> "toFloat32"
    BuiltinToFloat64 -> "toFloat64"

-- | Kernel bridge spelling reserved for compiler-generated prelude bindings.
builtinSymbolKernelName :: BuiltinSymbol -> Text
builtinSymbolKernelName builtinSymbol =
  kernelBridgeBindingPrefix <> builtinSymbolName builtinSymbol

-- | Runtime arity expected by analyzer/type/runtime calls for each builtin.
builtinSymbolArity :: BuiltinSymbol -> Int
builtinSymbolArity builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> 2
    BuiltinFilter -> 2
    BuiltinHd -> 1
    BuiltinTl -> 1
    BuiltinPrint -> 1
    BuiltinToInt8 -> 1
    BuiltinToInt16 -> 1
    BuiltinToInt32 -> 1
    BuiltinToInt64 -> 1
    BuiltinToUInt8 -> 1
    BuiltinToUInt16 -> 1
    BuiltinToUInt32 -> 1
    BuiltinToUInt64 -> 1
    BuiltinToFloat16 -> 1
    BuiltinToFloat32 -> 1
    BuiltinToFloat64 -> 1

-- | Numeric conversion builtins target one explicit concrete numeric type.
builtinSymbolNumericConversionTarget :: BuiltinSymbol -> Maybe NumericType
builtinSymbolNumericConversionTarget builtinSymbol =
  case builtinSymbol of
    BuiltinToInt8 -> Just NumericInt8
    BuiltinToInt16 -> Just NumericInt16
    BuiltinToInt32 -> Just NumericInt32
    BuiltinToInt64 -> Just NumericInt64
    BuiltinToUInt8 -> Just NumericUInt8
    BuiltinToUInt16 -> Just NumericUInt16
    BuiltinToUInt32 -> Just NumericUInt32
    BuiltinToUInt64 -> Just NumericUInt64
    BuiltinToFloat16 -> Just NumericFloat16
    BuiltinToFloat32 -> Just NumericFloat32
    BuiltinToFloat64 -> Just NumericFloat64
    _ -> Nothing

numericTypeFloatMax :: NumericType -> Maybe Double
numericTypeFloatMax numericType =
  case numericType of
    NumericFloat16 -> Just 65504.0
    NumericFloat32 -> Just 3.4028234663852886e38
    NumericFloat64 -> Just 1.7976931348623157e308
    _ -> Nothing

numericTypeIntegerBounds :: NumericType -> Maybe (Integer, Integer)
numericTypeIntegerBounds numericType =
  case numericType of
    NumericInt8 -> Just (signedLower 8, signedUpper 8)
    NumericInt16 -> Just (signedLower 16, signedUpper 16)
    NumericInt32 -> Just (signedLower 32, signedUpper 32)
    NumericInt64 -> Just (signedLower 64, signedUpper 64)
    NumericUInt8 -> Just (0, unsignedUpper 8)
    NumericUInt16 -> Just (0, unsignedUpper 16)
    NumericUInt32 -> Just (0, unsignedUpper 32)
    NumericUInt64 -> Just (0, unsignedUpper 64)
    NumericFloat16 -> Nothing
    NumericFloat32 -> Nothing
    NumericFloat64 -> Nothing
  where
    signedLower bits = negate (2 ^ (bits - 1))
    signedUpper bits = (2 ^ (bits - 1)) - 1
    unsignedUpper bits = (2 ^ bits) - 1

numericTypeFloatIntegerBounds :: NumericType -> Maybe (Integer, Integer)
numericTypeFloatIntegerBounds numericType =
  case numericTypeFloatMax numericType of
    Just maxMagnitude ->
      Just (ceiling (negate maxMagnitude), floor maxMagnitude)
    Nothing -> Nothing

numericTypeLiteralIntegerBounds :: NumericType -> Maybe (Integer, Integer)
numericTypeLiteralIntegerBounds numericType =
  case numericTypeIntegerBounds numericType of
    Just bounds -> Just bounds
    Nothing -> numericTypeFloatIntegerBounds numericType

numericTypeFromName :: Text -> Maybe NumericType
numericTypeFromName typeName =
  case typeName of
    "Int8" -> Just NumericInt8
    "Int16" -> Just NumericInt16
    "Int32" -> Just NumericInt32
    "Int64" -> Just NumericInt64
    "UInt8" -> Just NumericUInt8
    "UInt16" -> Just NumericUInt16
    "UInt32" -> Just NumericUInt32
    "UInt64" -> Just NumericUInt64
    "Float16" -> Just NumericFloat16
    "Float32" -> Just NumericFloat32
    "Float64" -> Just NumericFloat64
    _ -> Nothing

renderNumericTypeName :: NumericType -> Text
renderNumericTypeName numericType =
  case numericType of
    NumericInt8 -> "Int8"
    NumericInt16 -> "Int16"
    NumericInt32 -> "Int32"
    NumericInt64 -> "Int64"
    NumericUInt8 -> "UInt8"
    NumericUInt16 -> "UInt16"
    NumericUInt32 -> "UInt32"
    NumericUInt64 -> "UInt64"
    NumericFloat16 -> "Float16"
    NumericFloat32 -> "Float32"
    NumericFloat64 -> "Float64"

numericTypeIsIntegral :: NumericType -> Bool
numericTypeIsIntegral numericType =
  case numericType of
    NumericInt8 -> True
    NumericInt16 -> True
    NumericInt32 -> True
    NumericInt64 -> True
    NumericUInt8 -> True
    NumericUInt16 -> True
    NumericUInt32 -> True
    NumericUInt64 -> True
    NumericFloat16 -> False
    NumericFloat32 -> False
    NumericFloat64 -> False

numericTypeSupportsRuntimeArithmetic :: NumericType -> Bool
numericTypeSupportsRuntimeArithmetic numericType =
  numericTypeIsIntegral numericType
    || numericType == NumericFloat16
    || numericType == NumericFloat32
    || numericType == NumericFloat64

numericTypeSupportsRuntimeComparison :: NumericType -> Bool
numericTypeSupportsRuntimeComparison numericType =
  numericTypeSupportsRuntimeArithmetic numericType

-- | Prefix reserved for prelude bindings that directly expose kernel-owned
-- builtin symbols. Example: `__kernel_map = __kernel_map.`
kernelBridgeBindingPrefix :: Text
kernelBridgeBindingPrefix = "__kernel_"

-- | Validate a bridge binding name and, when it names a known kernel builtin,
-- return the canonical kernel target that the bridge must reference.
kernelBridgeTargetName :: Text -> Maybe Text
kernelBridgeTargetName bindingName
  | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName =
      let suffix = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
       in
        if Text.null suffix || not (isKernelBuiltinSymbolName bindingName)
          then Nothing
          else Just bindingName
  | otherwise = Nothing

-- | Resolve a public compatibility/prelude builtin spelling.
lookupBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupBuiltinSymbol name =
  lookupByRenderedName builtinSymbolName name

-- | Resolve a compiler-owned kernel bridge builtin spelling.
lookupKernelBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupKernelBuiltinSymbol name =
  lookupByRenderedName builtinSymbolKernelName name

-- | Resolve builtin names according to the selected mode. This chooses either
-- the kernel-name lookup or the compatibility lookup, never a combined search.
lookupBuiltinSymbolInMode :: BuiltinResolutionMode -> Text -> Maybe BuiltinSymbol
lookupBuiltinSymbolInMode mode name =
  case mode of
    ResolveKernelOnly ->
      lookupKernelBuiltinSymbol name
    ResolveCompatibility ->
      lookupBuiltinSymbol name

-- | Test whether a name is accepted by the selected builtin resolution mode.
isBuiltinSymbolNameInMode :: BuiltinResolutionMode -> Text -> Bool
isBuiltinSymbolNameInMode mode name =
  case lookupBuiltinSymbolInMode mode name of
    Just _ -> True
    Nothing -> False

-- | Test whether a name is a public compatibility/prelude builtin spelling.
isBuiltinSymbolName :: Text -> Bool
isBuiltinSymbolName name =
  case lookupBuiltinSymbol name of
    Just _ -> True
    Nothing -> False

-- | Test whether a name is reserved for the compiler-owned kernel bridge.
isKernelBuiltinSymbolName :: Text -> Bool
isKernelBuiltinSymbolName name =
  case lookupKernelBuiltinSymbol name of
    Just _ -> True
    Nothing -> False

-- | Shared lookup helper used by public-name and kernel-name resolution.
lookupByRenderedName :: (BuiltinSymbol -> Text) -> Text -> Maybe BuiltinSymbol
lookupByRenderedName renderSymbolName name =
  find (\symbol -> renderSymbolName symbol == name) allBuiltinSymbols
