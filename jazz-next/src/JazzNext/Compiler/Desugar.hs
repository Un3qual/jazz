module JazzNext.Compiler.Desugar
  ( desugarExpr
  ) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )

-- Canonical control-flow lowering.
--
-- Parser/lower currently produce `EIf`, but the analysis/type phases operate on
-- this post-desugared core form.
desugarExpr :: Expr -> Expr
desugarExpr expr =
  case expr of
    EInt value -> EInt value
    EBool value -> EBool value
    EVar name -> EVar name
    EIf conditionExpr thenExpr elseExpr ->
      ECase
        (desugarExpr conditionExpr)
        (desugarExpr thenExpr)
        (desugarExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      ECase
        (desugarExpr conditionExpr)
        (desugarExpr thenExpr)
        (desugarExpr elseExpr)
    EBinary operatorSymbol leftExpr rightExpr ->
      EBinary operatorSymbol (desugarExpr leftExpr) (desugarExpr rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (desugarExpr leftExpr) operatorSymbol
    ESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (desugarExpr rightExpr)
    EScope statements ->
      EScope (map desugarStatement statements)

desugarStatement :: Statement -> Statement
desugarStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (desugarExpr valueExpr)
    SSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SExpr spanValue expr ->
      SExpr spanValue (desugarExpr expr)
