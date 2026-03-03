module JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

-- Parser-surface tree. This remains separate from analyzer AST so parsing can
-- evolve (desugaring, richer syntax) without forcing analyzer shape changes.
data SurfaceExpr
  = SEInt Int
  | SEVar String
  | SEScope [SurfaceStatement]
  deriving (Eq, Show)

data SurfaceStatement
  = SSLet String SourceSpan SurfaceExpr
  | SSSignature String SourceSpan String
  | SSExpr SourceSpan SurfaceExpr
  deriving (Eq, Show)
