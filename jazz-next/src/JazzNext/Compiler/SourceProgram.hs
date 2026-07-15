{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for parsing surface source into lowered compiler programs.
module JazzNext.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    parseSurfaceWithErrorCode,
    scopeStatements
  ) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    prependDiagnosticSummary,
    setDiagnosticErrorCode
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )

parseAndLowerStandaloneSource :: Text -> Either Diagnostic Expr
parseAndLowerStandaloneSource source = do
  surfaceProgram <- parseSurfaceWithErrorCode source
  pure (lowerSurfaceExpr surfaceProgram)

scopeStatements :: Expr -> [Statement]
scopeStatements expr =
  case expr of
    EBlock statements -> statements
    _ -> [SExpr (SourceSpan 1 1) expr]

parseSurfaceWithErrorCode :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceWithErrorCode source =
  case parseSurfaceProgram source of
    Left parseError ->
      Left (setDiagnosticErrorCode E0001 (prependDiagnosticSummary "parse error: " parseError))
    Right surfaceProgram ->
      Right surfaceProgram
