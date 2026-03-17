{-# LANGUAGE OverloadedStrings #-}

-- | Canonical builtin inventory and name-resolution policy shared across
-- analyzer, type inference, runtime, and prelude validation.
module JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolOwnership,
    builtinSymbolArity,
    builtinSymbolName,
    builtinSymbolKernelName,
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
import Data.Text (Text)
import qualified Data.Text as Text

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
  deriving (Eq, Ord, Show, Enum, Bounded)

allBuiltinSymbols :: [BuiltinSymbol]
allBuiltinSymbols = [minBound .. maxBound]

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

builtinSymbolName :: BuiltinSymbol -> Text
builtinSymbolName builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> "map"
    BuiltinFilter -> "filter"
    BuiltinHd -> "hd"
    BuiltinTl -> "tl"
    BuiltinPrint -> "print!"

builtinSymbolKernelName :: BuiltinSymbol -> Text
builtinSymbolKernelName builtinSymbol =
  kernelBridgeBindingPrefix <> builtinSymbolName builtinSymbol

builtinSymbolArity :: BuiltinSymbol -> Int
builtinSymbolArity builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> 2
    BuiltinFilter -> 2
    BuiltinHd -> 1
    BuiltinTl -> 1
    BuiltinPrint -> 1

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

lookupBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupBuiltinSymbol name =
  lookupByRenderedName builtinSymbolName name

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

isBuiltinSymbolNameInMode :: BuiltinResolutionMode -> Text -> Bool
isBuiltinSymbolNameInMode mode name =
  case lookupBuiltinSymbolInMode mode name of
    Just _ -> True
    Nothing -> False

isBuiltinSymbolName :: Text -> Bool
isBuiltinSymbolName name =
  case lookupBuiltinSymbol name of
    Just _ -> True
    Nothing -> False

isKernelBuiltinSymbolName :: Text -> Bool
isKernelBuiltinSymbolName name =
  case lookupKernelBuiltinSymbol name of
    Just _ -> True
    Nothing -> False

-- | Shared lookup helper used by public-name and kernel-name resolution.
lookupByRenderedName :: (BuiltinSymbol -> Text) -> Text -> Maybe BuiltinSymbol
lookupByRenderedName renderSymbolName name =
  find (\symbol -> renderSymbolName symbol == name) allBuiltinSymbols
