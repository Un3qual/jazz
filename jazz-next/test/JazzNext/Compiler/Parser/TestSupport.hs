{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.TestSupport
  ( lexSource
  ) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token,
    tokenize
  )
import JazzNext.TestHarness
  ( failTest
  )

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)
