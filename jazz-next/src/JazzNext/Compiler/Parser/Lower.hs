module JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  ) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  )

-- Converts parser-surface nodes into core nodes while preserving source spans.
-- Control-flow/operator desugaring runs in a dedicated pass after lowering.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  case surfaceExpr of
    SEInt value -> EInt value
    SEBool value -> EBool value
    SEVar name -> EVar name
    SEList elements ->
      EList (map lowerSurfaceExpr elements)
    SEApply functionExpr argumentExpr ->
      EApply (lowerSurfaceExpr functionExpr) (lowerSurfaceExpr argumentExpr)
    SEIf conditionExpr thenExpr elseExpr ->
      EIf
        (lowerSurfaceExpr conditionExpr)
        (lowerSurfaceExpr thenExpr)
        (lowerSurfaceExpr elseExpr)
    SEBinary operatorSymbol leftExpr rightExpr ->
      EBinary
        operatorSymbol
        (lowerSurfaceExpr leftExpr)
        (lowerSurfaceExpr rightExpr)
    SESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (lowerSurfaceExpr leftExpr) operatorSymbol
    SESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (lowerSurfaceExpr rightExpr)
    SEScope statements -> EScope (map lowerSurfaceStatement statements)

lowerSurfaceStatement :: SurfaceStatement -> Statement
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr ->
      SLet name spanValue (lowerSurfaceExpr valueExpr)
    SSSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SSExpr spanValue expr ->
      SExpr spanValue (lowerSurfaceExpr expr)
