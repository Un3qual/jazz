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
  ( Token,
    tokenize,
  )
import Jazz.TestHarness
  ( failTest,
  )

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)

surfaceExprAt :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
surfaceExprAt line column = SurfaceExpr (SourceSpan line column)

surfacePatternAt :: Int -> Int -> SurfacePatternForm -> SurfacePattern
surfacePatternAt line column = SurfacePattern (SourceSpan line column)
