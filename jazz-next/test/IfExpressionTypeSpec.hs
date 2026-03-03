module Main (main) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertJust,
    assertSingleErrorContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "IfExpressionType" tests

tests :: [NamedTest]
tests =
  [ ("if condition must be Bool", testRejectsNonBoolCondition),
    ("if branches must have matching types", testRejectsMismatchedBranchTypes),
    ("if with Bool condition and aligned branches compiles", testAcceptsWellTypedIf)
  ]

testRejectsNonBoolCondition :: IO ()
testRejectsNonBoolCondition = do
  result <- compileExpr defaultWarningSettings nonBoolConditionProgram
  assertSingleErrorContains
    "condition type error"
    "if condition must have type Bool"
    (compileErrors result)

testRejectsMismatchedBranchTypes :: IO ()
testRejectsMismatchedBranchTypes = do
  result <- compileExpr defaultWarningSettings mismatchedBranchProgram
  assertSingleErrorContains
    "branch type mismatch"
    "if branches must have matching types"
    (compileErrors result)

testAcceptsWellTypedIf :: IO ()
testAcceptsWellTypedIf = do
  result <- compileExpr defaultWarningSettings validIfProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

nonBoolConditionProgram :: Expr
nonBoolConditionProgram =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        (EIf (EInt 1) (EInt 2) (EInt 3))
    ]

mismatchedBranchProgram :: Expr
mismatchedBranchProgram =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        (EIf (EBool True) (EInt 1) (EBool False))
    ]

validIfProgram :: Expr
validIfProgram =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        (EIf (EBool True) (EInt 1) (EInt 2))
    ]
