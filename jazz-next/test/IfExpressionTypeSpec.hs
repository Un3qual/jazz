{-# LANGUAGE OverloadedStrings #-}

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
    ("if condition accepts int equality as Bool", testAcceptsEqualityCondition),
    ("if branches must have matching types", testRejectsMismatchedBranchTypes),
    ("if with Bool condition and aligned branches compiles", testAcceptsWellTypedIf),
    ("binary operator rejects mismatched operand types", testRejectsBinaryTypeMismatch)
  ]

testRejectsNonBoolCondition :: IO ()
testRejectsNonBoolCondition = do
  result <- compileExpr defaultWarningSettings nonBoolConditionProgram
  assertSingleErrorContains
    "condition type error"
    "if condition must have type Bool"
    (compileErrors result)

testAcceptsEqualityCondition :: IO ()
testAcceptsEqualityCondition = do
  result <- compileExpr defaultWarningSettings equalityConditionProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

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

mkProgram :: Expr -> Expr
mkProgram expr =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

nonBoolConditionProgram :: Expr
nonBoolConditionProgram =
  mkProgram (EIf (EInt 1) (EInt 2) (EInt 3))

equalityConditionProgram :: Expr
equalityConditionProgram =
  mkProgram (EIf (EBinary "==" (EInt 1) (EInt 2)) (EInt 2) (EInt 3))

mismatchedBranchProgram :: Expr
mismatchedBranchProgram =
  mkProgram (EIf (EBool True) (EInt 1) (EBool False))

validIfProgram :: Expr
validIfProgram =
  mkProgram (EIf (EBool True) (EInt 1) (EInt 2))

testRejectsBinaryTypeMismatch :: IO ()
testRejectsBinaryTypeMismatch = do
  result <- compileExpr defaultWarningSettings binaryTypeMismatchProgram
  assertSingleErrorContains
    "binary type error"
    "cannot apply operator '+'"
    (compileErrors result)

binaryTypeMismatchProgram :: Expr
binaryTypeMismatchProgram =
  mkProgram (EBinary "+" (EInt 1) (EBool True))
