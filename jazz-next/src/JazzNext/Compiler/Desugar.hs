-- | Small desugaring pass for canonical control-flow forms in the core AST.
module JazzNext.Compiler.Desugar
  ( desugarExpr
  ) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )

-- | Canonicalize control-flow nodes without changing statement structure.
-- Parser/lower currently produce `EIf`, but the analysis/type phases operate on
-- the `ECase` form.
desugarExpr :: Expr -> Expr
desugarExpr expr =
  case expr of
    ELit literal -> ELit literal
    EVar name -> EVar name
    ELambda parameterName bodyExpr ->
      ELambda parameterName (desugarExpr bodyExpr)
    EOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
    EList elements -> EList (map desugarExpr elements)
    EApply functionExpr argumentExpr ->
      EApply (desugarExpr functionExpr) (desugarExpr argumentExpr)
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
    EBlock statements ->
      EBlock (map desugarStatement statements)

-- | Statement-level companion to `desugarExpr`.
desugarStatement :: Statement -> Statement
desugarStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (desugarExpr valueExpr)
    SSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SModule spanValue modulePath ->
      SModule spanValue modulePath
    SImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SExpr spanValue expr ->
      SExpr spanValue (desugarExpr expr)
