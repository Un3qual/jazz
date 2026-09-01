{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- small interpreter/runtime slice in `jazz`.
module Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType,
    Pattern (..),
    SignatureConstraint,
    SignaturePayload,
    SignatureToken,
    SignatureType,
    Statement (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
  )
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
  )
import Jazz.Compiler.Name (Name)
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

type NumericType = TypeRepresentation.NumericType

type SignatureType = TypeRepresentation.SignatureType Name Name

type SignatureConstraint = TypeRepresentation.SignatureConstraint Name Name

type SignatureToken = TypeRepresentation.SignatureToken Name

type SignaturePayload = TypeRepresentation.SignaturePayload Name Name Name

-- | Literals currently supported by the lowered core language.
data Literal
  = LInt Integer
  | LFloat Double FractionalLiteralSource (Maybe NumericType)
  | LBool Bool
  | LChar Char
  | LText Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Core patterns for the first active-path case-expression slice.
data Pattern
  = PWildcard
  | PVariable Name
  | PLiteral Literal
  | PConstructor Name [Pattern]
  | PList [Pattern]
  | PConsList Pattern Pattern
  | PTuple [Pattern]
  | PAs Name Pattern
  | POr [Pattern]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | One lowered pattern-match arm.
data CaseArm = CaseArm Pattern (Maybe Expr) Expr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Core constructor metadata lowered from parser-owned `data` declarations.
data DataConstructor = DataConstructor Name [SignatureType]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Core expressions after surface syntax has been lowered into the stable
-- analyzer/runtime representation.
data Expr
  = ELit Literal
  | EVar Name
  | ELambda Name Expr
  | EOperatorValue Text
  | EList [Expr]
  | ETuple [Expr]
  | EApply Expr Expr
  | ETypeApplication Expr SourceSpan SignatureType
  | EIf Expr Expr Expr
  | EPatternCase Expr [CaseArm]
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EBlock [Statement]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ClassMethodSignature = ClassMethodSignature Name SourceSpan SignaturePayload
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ImplMethod = ImplMethod Name SourceSpan Expr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Dot-terminated statements that can appear either at the top level or
-- inside block expressions.
data Statement
  = SLet Name SourceSpan Expr
  | SSignature Name SourceSpan SignaturePayload
  | SData SourceSpan Name [Name] [DataConstructor]
  | SClass SourceSpan Name [Name] [ClassMethodSignature]
  | SImpl SourceSpan Name [SignatureType] [ImplMethod]
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
