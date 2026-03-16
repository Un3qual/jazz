{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )

-- Parser-surface tree. This remains separate from analyzer AST so parsing can
-- evolve (desugaring, richer syntax) without forcing analyzer shape changes.
data SurfaceExpr
  = SEInt Int
  | SEBool Bool
  | SEVar Text
  | SEOperatorValue Text
  | SEList [SurfaceExpr]
  | SEApply SurfaceExpr SurfaceExpr
  | SEIf SurfaceExpr SurfaceExpr SurfaceExpr
  | SEBinary Text SurfaceExpr SurfaceExpr
  | SESectionLeft SurfaceExpr Text
  | SESectionRight Text SurfaceExpr
  | SEScope [SurfaceStatement]
  deriving (Eq, Show)

data SurfaceStatement
  = SSLet Text SourceSpan SurfaceExpr
  | SSSignature Text SourceSpan Text
  | SSModule SourceSpan [Text]
  | SSImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SSExpr SourceSpan SurfaceExpr
  deriving (Eq, Show)
