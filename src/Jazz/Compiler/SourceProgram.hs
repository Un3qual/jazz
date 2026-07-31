{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for parsing surface source into lowered compiler programs.
module Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    parseSurfaceWithErrorCode,
    scopeStatements
  ) where

import Data.Text (Text)
import Jazz.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    prependDiagnosticSummary,
    setDiagnosticErrorCode
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr
  )
import Jazz.Compiler.Parser.Lower
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
