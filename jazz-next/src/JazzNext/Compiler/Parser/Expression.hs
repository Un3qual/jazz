{-# LANGUAGE OverloadedStrings #-}

-- | Token-level expression entry point shared with parser decomposition tests.
module JazzNext.Compiler.Parser.Expression
  ( parseExpressionTokens
  ) where

import Data.Set
  ( Set
  )
import Data.Text
  ( Text
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceExpressionTokens
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token
  )
import JazzNext.Compiler.Parser.Operator
  ( OperatorInfo
  )

-- | Parse one expression from a token stream and return the unconsumed suffix.
--
-- This delegates to the main surface parser expression grammar so extracted
-- parser tests and decomposition callers accept the same expression starters,
-- declared operators, and alias-aware nested block behavior as `parseSurfaceProgram`.
parseExpressionTokens :: Set Text -> [OperatorInfo] -> [Token] -> Either Diagnostic (SurfaceExpr, [Token])
parseExpressionTokens = parseSurfaceExpressionTokens
