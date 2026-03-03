module JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

data Expr
  = EInt Int
  | EVar String
  | EScope [Statement]
  deriving (Eq, Show)

data Statement
  = SLet String SourceSpan Expr
  | SSignature String SourceSpan String
  | SExpr Expr
  deriving (Eq, Show)
