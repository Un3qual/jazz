{-# LANGUAGE OverloadedStrings #-}

-- | Surface AST produced directly by the parser before the program is lowered
-- into the smaller core AST used by later phases.
module JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier
  )

-- | Literals as they appear in parsed source before lowering.
data SurfaceLiteral
  = SLInt Int
  | SLBool Bool
  deriving (Eq, Show)

-- | Surface patterns accepted by the current parser slice for general case
-- expressions.
data SurfacePattern
  = SPWildcard
  | SPVariable Identifier
  | SPLiteral SurfaceLiteral
  | SPConstructor Identifier [SurfacePattern]
  | SPList [SurfacePattern]
  deriving (Eq, Show)

-- | One parser-surface pattern-match arm.
data SurfaceCaseArm = SurfaceCaseArm SurfacePattern SurfaceExpr
  deriving (Eq, Show)

-- | Parser-owned constructor metadata for top-level `data` declarations.
data SurfaceDataConstructor = SurfaceDataConstructor Identifier Int
  deriving (Eq, Show)

-- | Parser-facing expression tree. This remains separate from the core AST so
-- the surface syntax can grow without forcing analyzer/runtime rewrites.
data SurfaceExpr
  = SELit SurfaceLiteral
  | SEVar Identifier
  | SEQualifiedVar Identifier Identifier
  | SELambda [Identifier] SurfaceExpr
  | SEOperatorValue Text
  | SEList [SurfaceExpr]
  | SEApply SurfaceExpr SurfaceExpr
  | SEIf SurfaceExpr SurfaceExpr SurfaceExpr
  | SECase SurfaceExpr [SurfaceCaseArm]
  | SEBinary Text SurfaceExpr SurfaceExpr
  | SESectionLeft SurfaceExpr Text
  | SESectionRight Text SurfaceExpr
  | SEBlock [SurfaceStatement]
  deriving (Eq, Show)

-- | Parser-owned signature payload for the currently supported monomorphic
-- subset. Unsupported surfaces remain tokenized so later phases can keep
-- issuing the stable `E2009` diagnostic without storing joined raw text.
data SurfaceSignaturePayload
  = SurfaceSignatureType SurfaceSignatureType
  | SurfaceConstrainedSignature [SurfaceSignatureConstraint] SurfaceConstrainedSignatureType
  | SurfaceUnsupportedSignature [SurfaceSignatureToken]
  deriving (Eq, Show)

-- | Parser-owned constraint payload for the legacy `@{...}:` surface. It is
-- structured before semantics are implemented so later phases can reject it
-- deterministically without depending on opaque raw text.
data SurfaceSignatureConstraint = SurfaceSignatureConstraint Identifier [SurfaceConstrainedSignatureType]
  deriving (Eq, Show)

-- | Type syntax that can appear inside constrained signatures. This is kept
-- separate from `SurfaceSignatureType` because constrained signatures remain
-- semantically unsupported in the current active-path type checker.
data SurfaceConstrainedSignatureType
  = SurfaceConstrainedTypeName Identifier
  | SurfaceConstrainedTypeApplication Identifier [SurfaceConstrainedSignatureType]
  | SurfaceConstrainedTypeList SurfaceConstrainedSignatureType
  | SurfaceConstrainedTypeFunction SurfaceConstrainedSignatureType SurfaceConstrainedSignatureType
  deriving (Eq, Show)

-- | Monomorphic signature types supported by the active parser/type slice.
data SurfaceSignatureType
  = SurfaceTypeInt
  | SurfaceTypeBool
  | SurfaceTypeList SurfaceSignatureType
  | SurfaceTypeFunction SurfaceSignatureType SurfaceSignatureType
  deriving (Eq, Show)

-- | Tokenized fallback for unsupported signature surfaces.
data SurfaceSignatureToken
  = SurfaceSignatureNameToken Text
  | SurfaceSignatureIntToken Int
  | SurfaceSignatureArrowToken
  | SurfaceSignatureAtToken
  | SurfaceSignatureColonToken
  | SurfaceSignatureLParenToken
  | SurfaceSignatureRParenToken
  | SurfaceSignatureLBraceToken
  | SurfaceSignatureRBraceToken
  | SurfaceSignatureLBracketToken
  | SurfaceSignatureRBracketToken
  | SurfaceSignatureCommaToken
  | SurfaceSignatureOperatorToken Text
  | SurfaceSignatureOtherToken Text
  deriving (Eq, Show)

-- | Statement forms preserved from the parsed surface program.
data SurfaceStatement
  = SSLet Identifier SourceSpan SurfaceExpr
  | SSSignature Identifier SourceSpan SurfaceSignaturePayload
  | SSData SourceSpan Identifier [SurfaceDataConstructor]
  | SSModule SourceSpan [Text]
  | SSImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SSExpr SourceSpan SurfaceExpr
  deriving (Eq, Show)
