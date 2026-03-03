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

-- Converts parser-surface nodes into analyzer nodes. This pass currently
-- preserves structure and spans 1:1 while keeping the parse/lower boundary
-- explicit for future desugaring.
lowerSurfaceExpr :: SurfaceExpr -> Expr
lowerSurfaceExpr surfaceExpr =
  case surfaceExpr of
    SEInt value -> EInt value
    SEVar name -> EVar name
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
