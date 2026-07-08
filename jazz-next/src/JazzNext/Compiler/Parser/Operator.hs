{-# LANGUAGE OverloadedStrings #-}

-- | Operator metadata used by the surface parser's precedence climber.
module JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    builtinOperatorInfos,
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    isBuiltinOperatorSymbol,
    isReservedOperatorSymbol,
    isStage2OperatorSymbolChar,
    isValidUserOperatorSymbol,
    lookupOperatorInfoIn,
    lookupOperatorInfo
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Associativity used when computing the next precedence floor in the parser.
data Associativity
  = AssocLeft
  | AssocRight
  | AssocNonAssoc
  deriving (Eq, Show)

-- | Published fixity information for a builtin operator.
data OperatorInfo = OperatorInfo
  { operatorSymbol :: Text,
    operatorPrecedence :: Int,
    operatorAssociativity :: Associativity
  }
  deriving (Eq, Show)

-- | Builtin operator table. Precedence levels match the locked v1 tiers in
-- `docs/spec/syntax/operators.md`, where larger numbers bind tighter.
builtinOperatorInfos :: [OperatorInfo]
builtinOperatorInfos =
  [ OperatorInfo "*" 5 AssocLeft,
    OperatorInfo "/" 5 AssocLeft,
    OperatorInfo "+" 4 AssocLeft,
    OperatorInfo "-" 4 AssocLeft,
    OperatorInfo "|" 3 AssocLeft,
    OperatorInfo "==" 2 AssocLeft,
    OperatorInfo "!=" 2 AssocLeft,
    OperatorInfo "<" 2 AssocLeft,
    OperatorInfo "<=" 2 AssocLeft,
    OperatorInfo ">=" 2 AssocLeft,
    OperatorInfo ">" 2 AssocLeft,
    OperatorInfo "$" 1 AssocRight
  ]

-- | Lexer-facing membership check so unsupported operator spellings are
-- rejected before expression parsing.
isBuiltinOperatorSymbol :: Text -> Bool
isBuiltinOperatorSymbol symbol =
  case lookupOperatorInfo symbol of
    Just _ -> True
    Nothing -> False

-- | Lookup helper used by both the lexer and parser so they share the same
-- operator vocabulary and fixity data.
lookupOperatorInfo :: Text -> Maybe OperatorInfo
lookupOperatorInfo symbol = go builtinOperatorInfos
  where
    go infos =
      case infos of
        [] -> Nothing
        info : rest
          | operatorSymbol info == symbol -> Just info
          | otherwise -> go rest

-- | Lookup helper for parser state that extends the builtin table with
-- source-unit-local user declarations.
lookupOperatorInfoIn :: [OperatorInfo] -> Text -> Maybe OperatorInfo
lookupOperatorInfoIn declaredOperators symbol =
  go (declaredOperators <> builtinOperatorInfos)
  where
    go infos =
      case infos of
        [] -> Nothing
        info : rest
          | operatorSymbol info == symbol -> Just info
          | otherwise -> go rest

-- | Stage 2 declaration tiers map from documentation tiers (1 is tightest) to
-- the parser's internal precedence numbers (larger numbers bind tighter).
declaredOperatorInfoForTier :: Text -> Integer -> Maybe OperatorInfo
declaredOperatorInfoForTier symbol tier =
  case tier of
    1 -> Just (OperatorInfo symbol 5 AssocLeft)
    2 -> Just (OperatorInfo symbol 4 AssocLeft)
    3 -> Just (OperatorInfo symbol 3 AssocLeft)
    4 -> Just (OperatorInfo symbol 2 AssocLeft)
    5 -> Just (OperatorInfo symbol 1 AssocRight)
    _ -> Nothing

-- | Custom precedence declarations use the parser's native precedence scale
-- directly. Larger numbers bind tighter and custom associativity defaults left.
declaredOperatorInfoForPrecedence :: Text -> Integer -> Maybe OperatorInfo
declaredOperatorInfoForPrecedence symbol precedence
  | precedence >= 1 && precedence <= 99 =
      Just (OperatorInfo symbol (fromInteger precedence) AssocLeft)
  | otherwise =
      Nothing

-- | Characters allowed in Stage 2 user-defined operator symbols.
isStage2OperatorSymbolChar :: Char -> Bool
isStage2OperatorSymbolChar char =
  char `elem` ("!%&*+-/<>?^|~" :: String)

isValidUserOperatorSymbol :: Text -> Bool
isValidUserOperatorSymbol symbol =
  not (Text.null symbol)
    && Text.all isStage2OperatorSymbolChar symbol
    && not (isBuiltinOperatorSymbol symbol)
    && not (isReservedOperatorSymbol symbol)

isReservedOperatorSymbol :: Text -> Bool
isReservedOperatorSymbol symbol =
  symbol `elem` reservedOperatorSymbols

reservedOperatorSymbols :: [Text]
reservedOperatorSymbols =
  [ "->",
    "=>",
    "//",
    "/*",
    "*/",
    "--"
  ]
