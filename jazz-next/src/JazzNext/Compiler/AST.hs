{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier
  )

-- Analyzer-facing core AST after parser lowering.
data Literal
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Expr
  = ELit Literal
  | EVar Identifier
  | EOperatorValue Text
  | EList [Expr]
  | EApply Expr Expr
  | EIf Expr Expr Expr
  -- Internal canonical branch form used after control-flow desugaring.
  | ECase Expr Expr Expr
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EBlock [Statement]
  deriving (Eq, Show)

-- Dot-terminated top-level or block-level forms.
data Statement
  = SLet Identifier SourceSpan Expr
  | SSignature Identifier SourceSpan Text
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
