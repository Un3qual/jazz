module JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

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
