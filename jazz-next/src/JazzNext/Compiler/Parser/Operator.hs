{-# LANGUAGE OverloadedStrings #-}

-- | Operator metadata used by the surface parser's precedence climber.
module JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    builtinOperatorInfos,
    isBuiltinOperatorSymbol,
    lookupOperatorInfo
  ) where

import Data.Text (Text)

-- | Associativity used when computing the next precedence floor in the parser.
data Associativity
  = AssocLeft
  | AssocRight
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
