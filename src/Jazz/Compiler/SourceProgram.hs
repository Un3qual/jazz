{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared helpers for parsing surface source into lowered compiler programs.
module Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    parseSurfaceWithErrorCode,
    prependLoweredStatements,
    scopeStatements,
  )
where

import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    prependDiagnosticSummary,
    setDiagnosticErrorCode,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr,
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
    reindexLoweredExpr,
  )

parseAndLowerStandaloneSource :: Text -> Either Diagnostic (Expr 'Lowered)
parseAndLowerStandaloneSource source = do
  surfaceProgram <- parseSurfaceWithErrorCode source
  pure (lowerSurfaceExpr surfaceProgram)

-- | Prepend already-lowered declarations and then allocate one identity space
-- for the composed source unit. Parsed programs are blocks; retaining the
-- non-block case makes the helper total without manufacturing a synthetic
-- statement that would duplicate the expression's node identity.
prependLoweredStatements :: [Statement 'Lowered] -> Expr 'Lowered -> Expr 'Lowered
prependLoweredStatements prefix expression =
  reindexLoweredExpr $
    case expression of
      EBlock node statements -> EBlock node (prefix <> statements)
      _ -> expression

scopeStatements :: Expr phase -> [Statement phase]
scopeStatements expr =
  case expr of
    EBlock _ statements -> statements
    _ -> []

parseSurfaceWithErrorCode :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceWithErrorCode source =
  case parseSurfaceProgram source of
    Left parseError ->
      Left (setDiagnosticErrorCode E0001 (prependDiagnosticSummary "parse error: " parseError))
    Right surfaceProgram ->
      Right surfaceProgram
