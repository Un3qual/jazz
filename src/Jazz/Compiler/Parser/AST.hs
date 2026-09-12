{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Surface AST produced directly by the parser before the program is lowered
-- into the smaller core AST used by later phases.
module Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfaceNumericType,
    SurfaceName (..),
    SurfacePatternLambdaClause (..),
    SurfacePattern (..),
    SurfacePatternForm (..),
    SurfaceSignatureConstraint,
    SurfaceSignaturePayload,
    SurfaceSignatureToken,
    SurfaceSignatureType,
    SurfaceStatement (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
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
    IdentifierLike (..),
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation

type SurfaceNumericType = TypeRepresentation.NumericType

type SurfaceSignatureType = TypeRepresentation.SignatureType SurfaceName Identifier

type SurfaceSignatureConstraint = TypeRepresentation.SignatureConstraint SurfaceName Identifier

type SurfaceSignatureToken = TypeRepresentation.SignatureToken Text

type SurfaceSignaturePayload = TypeRepresentation.SignaturePayload SurfaceName Identifier Text

-- | A named type or capability retains the exact component locations at parse
-- time. The member span is also the name span for an unqualified name.
data SurfaceName = SurfaceName
  { surfaceNameIdentifier :: Identifier,
    surfaceNameSpan :: SourceSpan,
    surfaceNameQualifierSpan :: Maybe SourceSpan
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance IdentifierLike SurfaceName where
  identifierText = identifierText . surfaceNameIdentifier
  identifierPurity = identifierPurity . surfaceNameIdentifier

-- | Literals as they appear in parsed source before lowering.
data SurfaceLiteral
  = SLInt Integer
  | SLFloat Double FractionalLiteralSource (Maybe SurfaceNumericType)
  | SLBool Bool
  | SLChar Char
  | SLText Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Surface patterns accepted by the current parser slice for general case
-- expressions.
data SurfacePattern = SurfacePattern
  { surfacePatternSpan :: SourceSpan,
    surfacePatternForm :: SurfacePatternForm
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SurfacePatternForm
  = SPWildcard
  | SPVariable Identifier
  | SPLiteral SurfaceLiteral
  | SPConstructor Identifier [SurfacePattern]
  | SPList [SurfacePattern]
  | SPConsList SurfacePattern SurfacePattern
  | SPTuple [SurfacePattern]
  | SPAs Identifier SurfacePattern
  | SPOr [SurfacePattern]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | One parser-surface pattern-match arm.
data SurfaceCaseArm = SurfaceCaseArm SurfacePattern (Maybe SurfaceExpr) SurfaceExpr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Lambda parameters preserve ordinary identifier parameters separately from
-- destructuring patterns so lowering can keep the direct core lambda shape for
-- the common case.
data SurfaceLambdaParameter
  = SurfaceLambdaIdentifier SourceSpan Identifier
  | SurfaceLambdaPattern SurfacePattern
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | One ordered head/body pair in a multi-body pattern lambda. Unlike an
-- ordinary lambda parameter list, every head item is a pattern because clause
-- selection is performed by one shared pattern case after lowering.
data SurfacePatternLambdaClause
  = SurfacePatternLambdaClause SourceSpan (NonEmpty SurfacePattern) SurfaceExpr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Parser-owned constructor metadata for top-level `data` declarations.
data SurfaceDataConstructor = SurfaceDataConstructor Identifier [SurfaceSignatureType]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Parser-facing expression tree. This remains separate from the core AST so
-- the surface syntax can grow without forcing analyzer/runtime rewrites.
data SurfaceExpr = SurfaceExpr
  { surfaceExprSpan :: SourceSpan,
    surfaceExprForm :: SurfaceExprForm
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SurfaceExprForm
  = SELit SurfaceLiteral
  | SEVar Identifier
  | SEQualifiedVar Identifier Identifier
  | SEQualifiedMethod Identifier Identifier Identifier SourceSpan SourceSpan SourceSpan
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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SurfaceClassMethodSignature = SurfaceClassMethodSignature Identifier SourceSpan SurfaceSignaturePayload
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SurfaceImplMethod = SurfaceImplMethod Identifier SourceSpan SurfaceExpr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Statement forms preserved from the parsed surface program.
data SurfaceStatement
  = SSLet Identifier SourceSpan SurfaceExpr
  | SSSignature Identifier SourceSpan SurfaceSignaturePayload
  | SSData SourceSpan Identifier [Identifier] [SurfaceDataConstructor]
  | SSClass SourceSpan Identifier [Identifier] [SurfaceClassMethodSignature]
  | SSImpl SourceSpan SurfaceName [SurfaceSignatureType] [SurfaceImplMethod]
  | SSModule SourceSpan [Text] (Maybe [ModuleExportSelector])
  | SSImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SSExpr SourceSpan SurfaceExpr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
