{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- interpreter. The phase index makes the name and fact invariants explicit.
module Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNameAt,
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (..),
    CoreSort (..),
    CoreUserNameAt,
    DataConstructor (..),
    Expr (..),
    FactsAt,
    ImplMethod (..),
    Literal (..),
    NumericType,
    Pattern (..),
    SignatureConstraint,
    SignaturePayload,
    SignatureToken,
    SignatureType,
    Statement (..),
    expressionNode,
    patternNode,
    statementNode,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.Diagnostics (SourceSpan)
import Jazz.Compiler.FractionalLiteral (FractionalLiteralSource)
import Jazz.Compiler.Name
  ( Name,
    ResolvedUserName,
    SourceName,
  )
import Jazz.Compiler.SemanticFacts
  ( CoreNodeId (..),
    ExpressionFacts,
    PatternFacts,
    StatementFacts,
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

data CorePhase = Lowered | Resolved | Analyzed

data CoreSort = ExpressionSort | PatternSort | StatementSort

type family CoreUserNameAt (phase :: CorePhase) :: Type where
  CoreUserNameAt 'Lowered = SourceName
  CoreUserNameAt 'Resolved = ResolvedUserName
  CoreUserNameAt 'Analyzed = ResolvedUserName

type CoreNameAt phase = Name (CoreUserNameAt phase)

type family FactsAt (phase :: CorePhase) (sort :: CoreSort) :: Type where
  FactsAt 'Lowered sort = ()
  FactsAt 'Resolved sort = ()
  FactsAt 'Analyzed 'ExpressionSort = ExpressionFacts
  FactsAt 'Analyzed 'PatternSort = PatternFacts
  FactsAt 'Analyzed 'StatementSort = StatementFacts

data CoreNode (phase :: CorePhase) (sort :: CoreSort) = CoreNode
  { coreNodeId :: !CoreNodeId,
    coreNodeSpan :: !SourceSpan,
    coreNodeFacts :: !(FactsAt phase sort)
  }
  deriving stock (Generic)

type role CoreNode nominal nominal

deriving stock instance (Eq (FactsAt phase sort)) => Eq (CoreNode phase sort)

deriving stock instance (Show (FactsAt phase sort)) => Show (CoreNode phase sort)

instance (NFData (FactsAt phase sort)) => NFData (CoreNode phase sort) where
  rnf (CoreNode nodeId spanValue facts) = rnf nodeId `seq` rnf spanValue `seq` rnf facts

type NumericType = TypeRepresentation.NumericType

type SignatureType phase = TypeRepresentation.SignatureType (CoreNameAt phase) (CoreNameAt phase)

type SignatureConstraint phase = TypeRepresentation.SignatureConstraint (CoreNameAt phase) (CoreNameAt phase)

type SignatureToken phase = TypeRepresentation.SignatureToken (CoreNameAt phase)

type SignaturePayload phase = TypeRepresentation.SignaturePayload (CoreNameAt phase) (CoreNameAt phase) (CoreNameAt phase)

data Literal
  = LInt Integer
  | LFloat Double FractionalLiteralSource (Maybe NumericType)
  | LBool Bool
  | LChar Char
  | LText Text
  deriving stock (Eq, Generic, Show)

instance NFData Literal

data Pattern (phase :: CorePhase)
  = PWildcard (CoreNode phase 'PatternSort)
  | PVariable (CoreNode phase 'PatternSort) (CoreNameAt phase)
  | PLiteral (CoreNode phase 'PatternSort) Literal
  | PConstructor (CoreNode phase 'PatternSort) (CoreNameAt phase) [Pattern phase]
  | PList (CoreNode phase 'PatternSort) [Pattern phase]
  | PConsList (CoreNode phase 'PatternSort) (Pattern phase) (Pattern phase)
  | PTuple (CoreNode phase 'PatternSort) [Pattern phase]
  | PAs (CoreNode phase 'PatternSort) (CoreNameAt phase) (Pattern phase)
  | POr (CoreNode phase 'PatternSort) [Pattern phase]
  deriving stock (Generic)

type role Pattern nominal

data CaseArm (phase :: CorePhase)
  = CaseArm
      (CoreNode phase 'ExpressionSort)
      (Pattern phase)
      (Maybe (Expr phase))
      (Expr phase)
  deriving stock (Generic)

type role CaseArm nominal

data DataConstructor (phase :: CorePhase)
  = DataConstructor
      (CoreNode phase 'StatementSort)
      (CoreNameAt phase)
      [SignatureType phase]
  deriving stock (Generic)

type role DataConstructor nominal

data Expr (phase :: CorePhase)
  = ELit (CoreNode phase 'ExpressionSort) Literal
  | EVar (CoreNode phase 'ExpressionSort) (CoreNameAt phase)
  | ELambda (CoreNode phase 'ExpressionSort) (CoreNameAt phase) (Expr phase)
  | EOperatorValue (CoreNode phase 'ExpressionSort) Text
  | EList (CoreNode phase 'ExpressionSort) [Expr phase]
  | ETuple (CoreNode phase 'ExpressionSort) [Expr phase]
  | EApply (CoreNode phase 'ExpressionSort) (Expr phase) (Expr phase)
  | ETypeApplication (CoreNode phase 'ExpressionSort) (Expr phase) SourceSpan (SignatureType phase)
  | EIf (CoreNode phase 'ExpressionSort) (Expr phase) (Expr phase) (Expr phase)
  | EPatternCase (CoreNode phase 'ExpressionSort) (Expr phase) [CaseArm phase]
  | EBinary (CoreNode phase 'ExpressionSort) Text (Expr phase) (Expr phase)
  | ESectionLeft (CoreNode phase 'ExpressionSort) (Expr phase) Text
  | ESectionRight (CoreNode phase 'ExpressionSort) Text (Expr phase)
  | EBlock (CoreNode phase 'ExpressionSort) [Statement phase]
  deriving stock (Generic)

type role Expr nominal

data ClassMethodSignature (phase :: CorePhase)
  = ClassMethodSignature
      (CoreNode phase 'StatementSort)
      (CoreNameAt phase)
      (SignaturePayload phase)
  deriving stock (Generic)

type role ClassMethodSignature nominal

data ImplMethod (phase :: CorePhase)
  = ImplMethod
      (CoreNode phase 'StatementSort)
      (CoreNameAt phase)
      (Expr phase)
  deriving stock (Generic)

type role ImplMethod nominal

data Statement (phase :: CorePhase)
  = SLet (CoreNode phase 'StatementSort) (CoreNameAt phase) (Expr phase)
  | SSignature (CoreNode phase 'StatementSort) (CoreNameAt phase) (SignaturePayload phase)
  | SData (CoreNode phase 'StatementSort) (CoreNameAt phase) [CoreNameAt phase] [DataConstructor phase]
  | SClass (CoreNode phase 'StatementSort) (CoreNameAt phase) [CoreNameAt phase] [ClassMethodSignature phase]
  | SImpl (CoreNode phase 'StatementSort) (CoreNameAt phase) [SignatureType phase] [ImplMethod phase]
  | SModule (CoreNode phase 'StatementSort) [Text]
  | SImport (CoreNode phase 'StatementSort) [Text] (Maybe Text) (Maybe [Text])
  | SExpr (CoreNode phase 'StatementSort) (Expr phase)
  deriving stock (Generic)

type role Statement nominal

expressionNode :: Expr phase -> CoreNode phase 'ExpressionSort
expressionNode expression =
  case expression of
    ELit node _ -> node
    EVar node _ -> node
    ELambda node _ _ -> node
    EOperatorValue node _ -> node
    EList node _ -> node
    ETuple node _ -> node
    EApply node _ _ -> node
    ETypeApplication node _ _ _ -> node
    EIf node _ _ _ -> node
    EPatternCase node _ _ -> node
    EBinary node _ _ _ -> node
    ESectionLeft node _ _ -> node
    ESectionRight node _ _ -> node
    EBlock node _ -> node

patternNode :: Pattern phase -> CoreNode phase 'PatternSort
patternNode pattern = case pattern of
  PWildcard node -> node
  PVariable node _ -> node
  PLiteral node _ -> node
  PConstructor node _ _ -> node
  PList node _ -> node
  PConsList node _ _ -> node
  PTuple node _ -> node
  PAs node _ _ -> node
  POr node _ -> node

statementNode :: Statement phase -> CoreNode phase 'StatementSort
statementNode statement =
  case statement of
    SLet node _ _ -> node
    SSignature node _ _ -> node
    SData node _ _ _ -> node
    SClass node _ _ _ -> node
    SImpl node _ _ _ -> node
    SModule node _ -> node
    SImport node _ _ _ -> node
    SExpr node _ -> node

type CoreEq phase =
  ( Eq (CoreNameAt phase),
    Eq (FactsAt phase 'ExpressionSort),
    Eq (FactsAt phase 'PatternSort),
    Eq (FactsAt phase 'StatementSort)
  )

type CoreShow phase =
  ( Show (CoreNameAt phase),
    Show (FactsAt phase 'ExpressionSort),
    Show (FactsAt phase 'PatternSort),
    Show (FactsAt phase 'StatementSort)
  )

type CoreNFData phase =
  ( NFData (CoreNameAt phase),
    NFData (FactsAt phase 'ExpressionSort),
    NFData (FactsAt phase 'PatternSort),
    NFData (FactsAt phase 'StatementSort)
  )

deriving stock instance (CoreEq phase) => Eq (Pattern phase)

deriving stock instance (CoreShow phase) => Show (Pattern phase)

instance (CoreNFData phase) => NFData (Pattern phase)

deriving stock instance (CoreEq phase) => Eq (CaseArm phase)

deriving stock instance (CoreShow phase) => Show (CaseArm phase)

instance (CoreNFData phase) => NFData (CaseArm phase)

deriving stock instance (CoreEq phase) => Eq (DataConstructor phase)

deriving stock instance (CoreShow phase) => Show (DataConstructor phase)

instance (CoreNFData phase) => NFData (DataConstructor phase)

deriving stock instance (CoreEq phase) => Eq (Expr phase)

deriving stock instance (CoreShow phase) => Show (Expr phase)

instance (CoreNFData phase) => NFData (Expr phase)

deriving stock instance (CoreEq phase) => Eq (ClassMethodSignature phase)

deriving stock instance (CoreShow phase) => Show (ClassMethodSignature phase)

instance (CoreNFData phase) => NFData (ClassMethodSignature phase)

deriving stock instance (CoreEq phase) => Eq (ImplMethod phase)

deriving stock instance (CoreShow phase) => Show (ImplMethod phase)

instance (CoreNFData phase) => NFData (ImplMethod phase)

deriving stock instance (CoreEq phase) => Eq (Statement phase)

deriving stock instance (CoreShow phase) => Show (Statement phase)

instance (CoreNFData phase) => NFData (Statement phase)
