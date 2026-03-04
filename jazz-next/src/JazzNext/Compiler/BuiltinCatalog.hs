{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolArity,
    builtinSymbolName,
    isBuiltinSymbolName,
    lookupBuiltinSymbol
  ) where

import Data.Text (Text)

-- Canonical builtin inventory shared by analyzer/type/runtime to keep the
-- stdlib boundary contract auditable and drift-resistant.
data BuiltinSymbol
  = BuiltinMap
  | BuiltinHd
  | BuiltinTl
  deriving (Eq, Ord, Show, Enum, Bounded)

allBuiltinSymbols :: [BuiltinSymbol]
allBuiltinSymbols = [minBound .. maxBound]

builtinSymbolName :: BuiltinSymbol -> Text
builtinSymbolName builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> "map"
    BuiltinHd -> "hd"
    BuiltinTl -> "tl"

builtinSymbolArity :: BuiltinSymbol -> Int
builtinSymbolArity builtinSymbol =
  case builtinSymbol of
    BuiltinMap -> 2
    BuiltinHd -> 1
    BuiltinTl -> 1

lookupBuiltinSymbol :: Text -> Maybe BuiltinSymbol
lookupBuiltinSymbol name =
  case name of
    "map" -> Just BuiltinMap
    "hd" -> Just BuiltinHd
    "tl" -> Just BuiltinTl
    _ -> Nothing

isBuiltinSymbolName :: Text -> Bool
isBuiltinSymbolName name =
  case lookupBuiltinSymbol name of
    Just _ -> True
    Nothing -> False
