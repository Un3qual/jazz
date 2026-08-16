{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.AST
  ( Expr (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.TypeInference
  ( inferExpressionDefault,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (inferredExpr))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "CoreNormalization" tests

tests :: [NamedTest]
tests =
  [ ("if remains the canonical boolean conditional", testIfRemainsCanonicalIf),
    ("dollar lowers directly to application", testDollarLowersToApplication)
  ]

testIfRemainsCanonicalIf :: IO ()
testIfRemainsCanonicalIf =
  assertRight "parse if" (parseSurfaceProgram "if True then 1 else 2.") $ \surface -> do
    let lowered = lowerSurfaceExpr surface
    inference <- inferExpressionDefault lowered
    assertEqual "lowered equals inferred" lowered (inferredExpr inference)

testDollarLowersToApplication :: IO ()
testDollarLowersToApplication =
  assertRight "parse dollar" (parseSurfaceProgram "f $ x.") $ \surface ->
    assertEqual
      "canonical dollar"
      (EBlock [SExpr (SourceSpan 1 1) (EApply (EVar "f") (EVar "x"))])
      (lowerSurfaceExpr surface)
