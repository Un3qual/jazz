{-# LANGUAGE OverloadedStrings #-}

-- | Operator metadata used by the surface parser's precedence climber.
module Jazz.Compiler.Parser.Operator
  ( Associativity (..),
    OperatorInfo (..),
    OperatorTable,
    builtinOperatorInfos,
    declaredOperatorInfoForPrecedence,
    declaredOperatorInfoForTier,
    emptyOperatorTable,
    insertDeclaredOperator,
    isBuiltinOperatorSymbol,
    isDeclaredOperator,
    isReservedOperatorSymbol,
    isStage2OperatorSymbolChar,
    isValidUserOperatorSymbol,
    lookupOperatorInfoIn,
    lookupOperatorInfo,
    operatorTableFromDeclarations,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

-- | Builtin operator table. Precedence levels match the public operator
-- reference in `docs/language/operators.md`, where larger numbers bind tighter.
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

-- | Scope-local fixity lookup. Builtins are indexed once; the declared-symbol
-- set remains separate because only user declarations may be bound or signed.
data OperatorTable = OperatorTable
  { operatorInfosBySymbol :: Map Text OperatorInfo,
    declaredOperatorSymbols :: Set Text
  }
  deriving (Eq, Show)

emptyOperatorTable :: OperatorTable
emptyOperatorTable =
  OperatorTable
    { operatorInfosBySymbol = Map.fromList [(operatorSymbol info, info) | info <- builtinOperatorInfos],
      declaredOperatorSymbols = Set.empty
    }

operatorTableFromDeclarations :: [OperatorInfo] -> OperatorTable
operatorTableFromDeclarations = foldr insertDeclaredOperator emptyOperatorTable

insertDeclaredOperator :: OperatorInfo -> OperatorTable -> OperatorTable
insertDeclaredOperator operatorInfo operatorTable =
  operatorTable
    { operatorInfosBySymbol = Map.insert (operatorSymbol operatorInfo) operatorInfo (operatorInfosBySymbol operatorTable),
      declaredOperatorSymbols = Set.insert (operatorSymbol operatorInfo) (declaredOperatorSymbols operatorTable)
    }

isDeclaredOperator :: Text -> OperatorTable -> Bool
isDeclaredOperator symbol = Set.member symbol . declaredOperatorSymbols

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
lookupOperatorInfo symbol = Map.lookup symbol (operatorInfosBySymbol emptyOperatorTable)

-- | Lookup helper for parser state that extends the builtin table with
-- source-unit-local user declarations.
lookupOperatorInfoIn :: OperatorTable -> Text -> Maybe OperatorInfo
lookupOperatorInfoIn operatorTable symbol = Map.lookup symbol (operatorInfosBySymbol operatorTable)

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
