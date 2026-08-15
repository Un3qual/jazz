{-# LANGUAGE OverloadedStrings #-}

-- | Surface AST produced directly by the parser before the program is lowered
-- into the smaller core AST used by later phases.
module Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfacePatternLambdaClause (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Jazz.Compiler.Diagnostics
  ( SourceSpan,
  )
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExportSelector,
  )
import Jazz.Compiler.Name
  ( Identifier,
  )

-- | Literals as they appear in parsed source before lowering.
data SurfaceLiteral
  = SLInt Integer
  | SLFloat Double FractionalLiteralSource (Maybe SurfaceNumericType)
  | SLBool Bool
  | SLChar Char
  | SLText Text
  deriving (Eq, Show)

-- | Surface patterns accepted by the current parser slice for general case
-- expressions.
data SurfacePattern
  = SPWildcard
  | SPVariable Identifier
  | SPLiteral SurfaceLiteral
  | SPConstructor Identifier [SurfacePattern]
  | SPList [SurfacePattern]
  | SPConsList SurfacePattern SurfacePattern
  | SPTuple [SurfacePattern]
  | SPAs Identifier SurfacePattern
  | SPOr [SurfacePattern]
  deriving (Eq, Show)

-- | One parser-surface pattern-match arm.
data SurfaceCaseArm = SurfaceCaseArm SurfacePattern (Maybe SurfaceExpr) SurfaceExpr
  deriving (Eq, Show)

-- | Lambda parameters preserve ordinary identifier parameters separately from
-- destructuring patterns so lowering can keep the direct core lambda shape for
-- the common case.
data SurfaceLambdaParameter
  = SurfaceLambdaIdentifier Identifier
  | SurfaceLambdaPattern SurfacePattern
  deriving (Eq, Show)

-- | One ordered head/body pair in a multi-body pattern lambda. Unlike an
-- ordinary lambda parameter list, every head item is a pattern because clause
-- selection is performed by one shared pattern case after lowering.
data SurfacePatternLambdaClause
  = SurfacePatternLambdaClause SourceSpan (NonEmpty SurfacePattern) SurfaceExpr
  deriving (Eq, Show)

-- | Parser-owned constructor metadata for top-level `data` declarations.
data SurfaceDataConstructor = SurfaceDataConstructor Identifier [SurfaceSignatureType]
  deriving (Eq, Show)

-- | Parser-facing expression tree. This remains separate from the core AST so
-- the surface syntax can grow without forcing analyzer/runtime rewrites.
data SurfaceExpr
  = SELit SurfaceLiteral
  | SEVar Identifier
  | SEQualifiedVar Identifier Identifier
  | SELambda (NonEmpty SurfaceLambdaParameter) SurfaceExpr
  | SEPatternLambda (NonEmpty SurfacePatternLambdaClause)
  | SEOperatorValue Text
  | SEList [SurfaceExpr]
  | SETuple [SurfaceExpr]
  | SEApply SurfaceExpr SurfaceExpr
  | SETypeApplication SurfaceExpr SourceSpan SurfaceSignatureType
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
  | SurfaceConstrainedSignature [SurfaceSignatureConstraint] SurfaceSignatureType
  | SurfaceUnsupportedSignature [SurfaceSignatureToken]
  deriving (Eq, Show)

-- | Parser-owned constraint payload for the `@{...}:` surface. It is
-- structured before the full type-class model exists so later phases can
-- reject or narrow it deterministically without depending on opaque raw text.
data SurfaceSignatureConstraint = SurfaceSignatureConstraint Identifier [SurfaceSignatureType]
  deriving (Eq, Show)

-- | Monomorphic signature types supported by the active parser/type slice.
data SurfaceNumericType
  = SurfaceNumericInt8
  | SurfaceNumericInt16
  | SurfaceNumericInt32
  | SurfaceNumericInt64
  | SurfaceNumericUInt8
  | SurfaceNumericUInt16
  | SurfaceNumericUInt32
  | SurfaceNumericUInt64
  | SurfaceNumericFloat16
  | SurfaceNumericFloat32
  | SurfaceNumericFloat64
  deriving (Bounded, Enum, Eq, Ord, Show)

data SurfaceSignatureType
  = SurfaceTypeInt
  | SurfaceTypeFloat
  | SurfaceTypeNumeric SurfaceNumericType
  | SurfaceTypeBool
  | SurfaceTypeChar
  | SurfaceTypeText
  | SurfaceTypeVariable Identifier
  | SurfaceTypeName Identifier
  | SurfaceTypeApplication Identifier [SurfaceSignatureType]
  | SurfaceTypeList SurfaceSignatureType
  | SurfaceTypeTuple [SurfaceSignatureType]
  | SurfaceTypeFunction SurfaceSignatureType SurfaceSignatureType
  deriving (Eq, Show)

-- | Tokenized fallback for unsupported signature surfaces. The parser records
-- enough structure for stable downstream diagnostics while avoiding raw-text
-- coupling between phases.
data SurfaceSignatureToken
  = SurfaceSignatureNameToken Text
  | SurfaceSignatureIntToken Integer
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

data SurfaceClassMethodSignature = SurfaceClassMethodSignature Identifier SourceSpan SurfaceSignaturePayload
  deriving (Eq, Show)

data SurfaceImplMethod = SurfaceImplMethod Identifier SourceSpan SurfaceExpr
  deriving (Eq, Show)

-- | Statement forms preserved from the parsed surface program.
data SurfaceStatement
  = SSLet Identifier SourceSpan SurfaceExpr
  | SSSignature Identifier SourceSpan SurfaceSignaturePayload
  | SSData SourceSpan Identifier [Identifier] [SurfaceDataConstructor]
  | SSClass SourceSpan Identifier [Identifier] [SurfaceClassMethodSignature]
  | SSImpl SourceSpan Identifier [SurfaceSignatureType] [SurfaceImplMethod]
  | SSModule SourceSpan [Text] (Maybe [ModuleExportSelector])
  | SSImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SSExpr SourceSpan SurfaceExpr
  deriving (Eq, Show)
