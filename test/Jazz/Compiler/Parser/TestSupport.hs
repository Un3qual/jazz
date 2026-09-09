{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.TestSupport
  ( lexSource,
    surfaceExprAt,
    surfacePatternAt,
  )
where

import Data.Text (Text)
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm,
    SurfacePattern (..),
    SurfacePatternForm,
  )
import Jazz.Compiler.Parser.Lexer
  ( Token (..),
    tokenize,
  )
import Jazz.Compiler.SourceSpan (sourceSpanStart)
import Jazz.TestHarness
  ( failTest,
  )

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    -- These legacy syntax fixtures assert point-only ASTs. Range contracts use
    -- the production lexer directly in SourceRangesSpec.
    Right tokens -> pure [token {tokenSpan = sourceSpanStart (tokenSpan token)} | token <- tokens]
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)

surfaceExprAt :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
surfaceExprAt line column = SurfaceExpr (SourceSpan line column)

surfacePatternAt :: Int -> Int -> SurfacePatternForm -> SurfacePattern
surfacePatternAt line column = SurfacePattern (SourceSpan line column)
