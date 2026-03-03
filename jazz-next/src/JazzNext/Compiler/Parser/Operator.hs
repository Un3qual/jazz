module JazzNext.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    builtinOperatorInfos,
    isBuiltinOperatorSymbol,
    lookupOperatorInfo
  ) where

data Associativity
  = AssocLeft
  | AssocRight
  deriving (Eq, Show)

data OperatorInfo = OperatorInfo
  { operatorSymbol :: String,
    operatorPrecedence :: Int,
    operatorAssociativity :: Associativity
  }
  deriving (Eq, Show)

-- Precedence levels match the locked v1 tiers in docs/spec/syntax/operators.md.
-- Larger numbers bind tighter in the precedence climber.
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

isBuiltinOperatorSymbol :: String -> Bool
isBuiltinOperatorSymbol symbol =
  case lookupOperatorInfo symbol of
    Just _ -> True
    Nothing -> False

lookupOperatorInfo :: String -> Maybe OperatorInfo
lookupOperatorInfo symbol = go builtinOperatorInfos
  where
    go infos =
      case infos of
        [] -> Nothing
        info : rest
          | operatorSymbol info == symbol -> Just info
          | otherwise -> go rest
