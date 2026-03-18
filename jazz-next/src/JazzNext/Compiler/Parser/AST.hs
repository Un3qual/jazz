{-# LANGUAGE OverloadedStrings #-}

-- | Surface AST produced directly by the parser before the program is lowered
-- into the smaller core AST used by later phases.
module JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
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
  deriving (Eq, Show)

-- | One parser-surface pattern-match arm.
data SurfaceCaseArm = SurfaceCaseArm SurfacePattern SurfaceExpr
  deriving (Eq, Show)

-- | Parser-facing expression tree. This remains separate from the core AST so
-- the surface syntax can grow without forcing analyzer/runtime rewrites.
data SurfaceExpr
  = SELit SurfaceLiteral
  | SEVar Identifier
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

-- | Statement forms preserved from the parsed surface program.
data SurfaceStatement
  = SSLet Identifier SourceSpan SurfaceExpr
  | SSSignature Identifier SourceSpan Text
  | SSModule SourceSpan [Text]
  | SSImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SSExpr SourceSpan SurfaceExpr
  deriving (Eq, Show)
