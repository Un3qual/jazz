{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.TestSupport
  ( lexSource
  ) where

import Data.Text (Text)
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import Jazz.Compiler.Parser.Lexer
  ( Token,
    tokenize
  )
import Jazz.TestHarness
  ( failTest
  )

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)
