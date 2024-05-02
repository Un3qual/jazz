{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Parser.Operator where
import AST
import Parser.Lib

import           Control.Monad.Combinators.Expr
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import GHC.Base (build)

-- allowedOperatorChars :: [[Char]]
-- allowedOperatorChars = ["+", "-", "*", "/", "!", "@", "?", "#", "$", "&", "^", "|", "<", ">", "=", "~", "%"]

builtinInfixOps :: [T.Text]
builtinInfixOps = ["*", "/", ".", "+", "-", "|", "==", "!=", "<", "<=", ">", ">=", "$"]

rawInfixOpP :: Parser Text
rawInfixOpP = choice $ map symbolP builtinInfixOps

-- TODO: Add handling for user-defined operators. (https://stackoverflow.com/questions/56969142/add-operators-during-parsing)

operatorTable :: [[Operator Parser SpannedExpr]]
operatorTable =
  [
    [ InfixL (buildInfixExpr "*" )
    , InfixL (buildInfixExpr "/" )
    ]
  -- , [ InfixL (buildInfixExpr "." ) ]
  , [ InfixL (buildInfixExpr "+" )
    , InfixL (buildInfixExpr "-" )
    ]
  -- , [ Prefix (EApply (EVar $ Variable "-") <$ symbolP "-") ]
  -- , [ Prefix (EApply (EVar $ Variable "-") <$ symbolP "-") ]

  , [ InfixL (buildInfixExpr "|" ) ]
  , [ InfixL (buildInfixExpr "==")
    , InfixL (buildInfixExpr "!=")
    , InfixL (buildInfixExpr "<" )
    , InfixL (buildInfixExpr "<=")
    , InfixL (buildInfixExpr ">=")
    , InfixL (buildInfixExpr ">" )
    ]
  , [ InfixR infixApplyP ] -- "$" operator
  ]

-- Helper function to construct an ASTNode representing an infix expression
buildInfixExpr :: T.Text -> Parser (SpannedExpr -> SpannedExpr -> SpannedExpr)
buildInfixExpr op = do
  (Ann opSp@(Span _ opEnd) opText) <- withSpan $ symbolP op
  let opExpr = Ann opSp (EVar (Variable opText))
  return $ \lhs@(Ann (Span lhsStart _) _) rhs@(Ann (Span _ rhsEnd) _) ->
    Ann (Span lhsStart rhsEnd) (EApply (Ann (Span lhsStart opEnd) (EApply opExpr lhs)) rhs)

infixApplyP :: Parser (SpannedExpr -> SpannedExpr -> SpannedExpr) 
infixApplyP = applyInfix <$ symbolP "$"
  where
    applyInfix lhs@(Ann (Span lhsStart _) _) rhs@(Ann (Span _ rhsEnd) _) = Ann (Span lhsStart rhsEnd) (EApply lhs rhs)
