{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Parser.Operator where
import AST
import Parser.Lib

import           Control.Monad.Combinators.Expr
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec

-- allowedOperatorChars :: [[Char]]
-- allowedOperatorChars = ["+", "-", "*", "/", "!", "@", "?", "#", "$", "&", "^", "|", "<", ">", "=", "~", "%"]

builtinInfixOps :: [T.Text]
builtinInfixOps = ["*", "/", ".", "+", "-", "|", "==", "!=", "<", "<=", ">", ">=", "$"]

rawInfixOpP :: Parser Text
rawInfixOpP = choice $ map symbolP builtinInfixOps

-- TODO: Add handling for user-defined operators. (https://stackoverflow.com/questions/56969142/add-operators-during-parsing)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ InfixL (buildInfixExpr "*" )
    , InfixL (buildInfixExpr "/" )
    ]
  , [ InfixL (buildInfixExpr "." ) ]
  , [ InfixL (buildInfixExpr "+" )
    , InfixL (buildInfixExpr "-" )
    ]
  , [ Prefix (EApply (EVar $ Variable "-" Nothing) <$ symbolP "-") ]

  , [ InfixL (buildInfixExpr "|" ) ]
  , [ InfixL (buildInfixExpr "==")
    , InfixL (buildInfixExpr "!=")
    , InfixL (buildInfixExpr "<" )
    , InfixL (buildInfixExpr "<=")
    , InfixL (buildInfixExpr ">=")
    , InfixL (buildInfixExpr ">" )
    ]
  , [ InfixR (EApply <$ maybeDbg "operatorTable::symbol$" (symbolP "$")) ]
  ]

-- Helper function to construct an ASTNode representing an infix expression
buildInfixExpr :: T.Text -> Parser (Expr -> Expr -> Expr)
buildInfixExpr op = (EApply . EApply (EVar $ Variable op Nothing)) <$ maybeDbg ("buildInfixExpr::symbol" ++ T.unpack op) (symbolP op)