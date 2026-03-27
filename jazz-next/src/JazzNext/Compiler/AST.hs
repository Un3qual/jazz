{-# LANGUAGE OverloadedStrings #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- small interpreter/runtime slice in `jazz-next`.
module JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..)
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier
  )

-- | Literals currently supported by the lowered core language.
data Literal
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

-- | Core patterns for the first active-path case-expression slice.
data Pattern
  = PWildcard
  | PVariable Identifier
  | PLiteral Literal
  | PConstructor Identifier [Pattern]
  | PList [Pattern]
  deriving (Eq, Show)

-- | One lowered pattern-match arm.
data CaseArm = CaseArm Pattern Expr
  deriving (Eq, Show)

-- | Core expressions after surface syntax has been lowered into the stable
-- analyzer/runtime representation.
data Expr
  = ELit Literal
  | EVar Identifier
  | ELambda Identifier Expr
  | EOperatorValue Text
  | EList [Expr]
  | EApply Expr Expr
  | EIf Expr Expr Expr
  -- Internal canonical branch form used after control-flow desugaring.
  | ECase Expr Expr Expr
  | EPatternCase Expr [CaseArm]
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EBlock [Statement]
  deriving (Eq, Show)

-- | Dot-terminated statements that can appear either at the top level or
-- inside block expressions.
data Statement
  = SLet Identifier SourceSpan Expr
  | SSignature Identifier SourceSpan Text
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
