module JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

-- Analyzer-facing core AST after parser lowering.
data Expr
  = EInt Int
  | EVar String
  | EScope [Statement]
  deriving (Eq, Show)

-- Dot-terminated top-level or block-level forms.
data Statement
  = SLet String SourceSpan Expr
  | SSignature String SourceSpan String
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
