{-# LANGUAGE OverloadedStrings #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- small interpreter/runtime slice in `jazz-next`.
module JazzNext.Compiler.AST
  ( CaseArm (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
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

-- | Core constructor metadata lowered from parser-owned `data` declarations.
data DataConstructor = DataConstructor Identifier Int
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

-- | Lowered signature payload used by analyzer/type inference.
data SignaturePayload
  = SignatureType SignatureType
  | ConstrainedSignature [SignatureConstraint] ConstraintSignatureType
  | UnsupportedSignature [SignatureToken]
  deriving (Eq, Show)

-- | Lowered representation for constrained signatures. Type inference rejects
-- this payload until constraint semantics are defined, but the parser/lowering
-- pipeline owns its shape.
data SignatureConstraint = SignatureConstraint Identifier [ConstraintSignatureType]
  deriving (Eq, Show)

data ConstraintSignatureType
  = ConstraintTypeName Identifier
  | ConstraintTypeApplication Identifier [ConstraintSignatureType]
  | ConstraintTypeList ConstraintSignatureType
  | ConstraintTypeFunction ConstraintSignatureType ConstraintSignatureType
  deriving (Eq, Show)

-- | Supported monomorphic signature types.
data SignatureType
  = TypeInt
  | TypeBool
  | TypeList SignatureType
  | TypeFunction SignatureType SignatureType
  deriving (Eq, Show)

-- | Tokenized fallback for unsupported signature surfaces.
data SignatureToken
  = SignatureNameToken Text
  | SignatureIntToken Int
  | SignatureArrowToken
  | SignatureAtToken
  | SignatureColonToken
  | SignatureLParenToken
  | SignatureRParenToken
  | SignatureLBraceToken
  | SignatureRBraceToken
  | SignatureLBracketToken
  | SignatureRBracketToken
  | SignatureCommaToken
  | SignatureOperatorToken Text
  | SignatureOtherToken Text
  deriving (Eq, Show)

-- | Dot-terminated statements that can appear either at the top level or
-- inside block expressions.
data Statement
  = SLet Identifier SourceSpan Expr
  | SSignature Identifier SourceSpan SignaturePayload
  | SData SourceSpan Identifier [DataConstructor]
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
