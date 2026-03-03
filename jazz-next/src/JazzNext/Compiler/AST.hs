{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

-- Analyzer-facing core AST after parser lowering.
data Expr
  = EInt Int
  | EBool Bool
  | EVar Text
  | EIf Expr Expr Expr
  -- Internal canonical branch form used after control-flow desugaring.
  | ECase Expr Expr Expr
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EScope [Statement]
  deriving (Eq, Show)

-- Dot-terminated top-level or block-level forms.
data Statement
  = SLet Text SourceSpan Expr
  | SSignature Text SourceSpan Text
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
