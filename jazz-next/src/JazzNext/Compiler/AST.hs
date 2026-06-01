{-# LANGUAGE OverloadedStrings #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- small interpreter/runtime slice in `jazz-next`.
module JazzNext.Compiler.AST
  ( CaseArm (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    NumericType (..),
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
  = LInt Integer
  | LFloat Double Bool
  | LBool Bool
  deriving (Eq, Show)

-- | Core patterns for the first active-path case-expression slice.
data Pattern
  = PWildcard
  | PVariable Identifier
  | PLiteral Literal
  | PConstructor Identifier [Pattern]
  | PList [Pattern]
  | PConsList Pattern Pattern
  | PTuple [Pattern]
  | PAs Identifier Pattern
  deriving (Eq, Show)

-- | One lowered pattern-match arm.
data CaseArm = CaseArm Pattern Expr
  deriving (Eq, Show)

-- | Core constructor payload metadata lowered from parser-owned `data`
-- declarations. Opaque payloads preserve current arity-only behavior for
-- grouped forms until constructor type schemes own those surfaces.
data DataConstructorArgument
  = DataConstructorArgumentName Identifier
  | DataConstructorArgumentOpaque
  deriving (Eq, Show)

-- | Core constructor metadata lowered from parser-owned `data` declarations.
data DataConstructor = DataConstructor Identifier [DataConstructorArgument]
  deriving (Eq, Show)

-- | Core expressions after surface syntax has been lowered into the stable
-- analyzer/runtime representation.
data Expr
  = ELit Literal
  | EVar Identifier
  | ELambda Identifier Expr
  | EOperatorValue Text
  | EList [Expr]
  | ETuple [Expr]
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

-- | Type grammar fragment allowed inside constraint argument lists. It remains
-- separate from `SignatureType` so unsupported constrained surfaces can keep a
-- faithful shape while the accepted monomorphic signature subset stays small.
data ConstraintSignatureType
  = ConstraintTypeName Identifier
  | ConstraintTypeApplication Identifier [ConstraintSignatureType]
  | ConstraintTypeList ConstraintSignatureType
  | ConstraintTypeTuple [ConstraintSignatureType]
  | ConstraintTypeFunction ConstraintSignatureType ConstraintSignatureType
  deriving (Eq, Show)

-- | Supported monomorphic signature types.
data NumericType
  = NumericInt8
  | NumericInt16
  | NumericInt32
  | NumericInt64
  | NumericUInt8
  | NumericUInt16
  | NumericUInt32
  | NumericUInt64
  | NumericFloat16
  | NumericFloat32
  | NumericFloat64
  deriving (Eq, Ord, Show)

data SignatureType
  = TypeInt
  | TypeFloat
  | TypeNumeric NumericType
  | TypeBool
  | TypeList SignatureType
  | TypeTuple [SignatureType]
  | TypeFunction SignatureType SignatureType
  deriving (Eq, Show)

-- | Tokenized fallback for unsupported signature surfaces. Tokens are stored
-- structurally so diagnostics can remain deterministic without preserving raw
-- source slices.
data SignatureToken
  = SignatureNameToken Text
  | SignatureIntToken Integer
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
  | SData SourceSpan Identifier [Identifier] [DataConstructor]
  | SClass SourceSpan Identifier
  | SImpl SourceSpan Identifier [ConstraintSignatureType]
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving (Eq, Show)
