{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolArity,
    builtinSymbolName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
    isBuiltinSymbolName,
    lookupBuiltinSymbol
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Canonical builtin inventory shared by analyzer/type/runtime to keep the
-- stdlib boundary contract auditable and drift-resistant.
data BuiltinSymbol
  = BuiltinMap
  | BuiltinHd
  | BuiltinTl
  | BuiltinPrint
  deriving (Eq, Ord, Show, Enum, Bounded)

allBuiltinSymbols :: [BuiltinSymbol]
allBuiltinSymbols = [minBound .. maxBound]

builtinSymbolName :: BuiltinSymbol -> Text
builtinSymbolName builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> "map"
    BuiltinHd -> "hd"
    BuiltinTl -> "tl"
    BuiltinPrint -> "print!"

builtinSymbolArity :: BuiltinSymbol -> Int
builtinSymbolArity builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> 2
    BuiltinHd -> 1
    BuiltinTl -> 1
    BuiltinPrint -> 1

-- Prelude/kernel bridge bindings must use this prefix and point to a known
-- kernel symbol name. Example: __kernel_map = map.
kernelBridgeBindingPrefix :: Text
kernelBridgeBindingPrefix = "__kernel_"

kernelBridgeTargetName :: Text -> Maybe Text
kernelBridgeTargetName bindingName
  | kernelBridgeBindingPrefix `Text.isPrefixOf` bindingName =
      let targetName = Text.drop (Text.length kernelBridgeBindingPrefix) bindingName
       in
        if Text.null targetName
          then Nothing
          else Just targetName
  | otherwise = Nothing

lookupBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupBuiltinSymbol name =
  case name of
    "map" -> Just BuiltinMap
    "hd" -> Just BuiltinHd
    "tl" -> Just BuiltinTl
    "print!" -> Just BuiltinPrint
    _ -> Nothing

isBuiltinSymbolName :: Text -> Bool
isBuiltinSymbolName name =
  case lookupBuiltinSymbol name of
    Just _ -> True
    Nothing -> False
