module JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  ) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceStatement (..)
  )

-- Converts parser-surface nodes into core nodes while preserving source spans.
-- Control-flow/operator desugaring runs in a dedicated pass after lowering.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  case surfaceExpr of
    SELit literal -> ELit (lowerSurfaceLiteral literal)
    SEVar name -> EVar name
    SEOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
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
    SEBlock statements -> EBlock (map lowerSurfaceStatement statements)

lowerSurfaceLiteral :: SurfaceLiteral -> Literal
lowerSurfaceLiteral literal =
  case literal of
    SLInt value -> LInt value
    SLBool value -> LBool value

lowerSurfaceStatement :: SurfaceStatement -> Statement
lowerSurfaceStatement surfaceStatement =
  case surfaceStatement of
    SSLet name spanValue valueExpr ->
      SLet name spanValue (lowerSurfaceExpr valueExpr)
    SSSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SSModule spanValue modulePath ->
      SModule spanValue modulePath
    SSImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SSExpr spanValue expr ->
      SExpr spanValue (lowerSurfaceExpr expr)
