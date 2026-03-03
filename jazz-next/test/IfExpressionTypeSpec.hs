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
    compileExpr,
    compileSource
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
    ("if condition rejects mismatched strict equality operands", testRejectsInvalidEqualityCondition),
    ("if branches must have matching types", testRejectsMismatchedBranchTypes),
    ("if with Bool condition and aligned branches compiles", testAcceptsWellTypedIf),
    ("binary operator rejects mismatched operand types", testRejectsBinaryTypeMismatch),
    ("source pipeline compiles well-typed if expression", testSourcePipelineAcceptsWellTypedIf),
    ("source pipeline reports if condition type errors", testSourcePipelineRejectsNonBoolCondition)
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

testRejectsInvalidEqualityCondition :: IO ()
testRejectsInvalidEqualityCondition = do
  result <- compileExpr defaultWarningSettings invalidEqualityConditionProgram
  assertSingleErrorContains
    "strict equality condition type error"
    "E2004"
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

invalidEqualityConditionProgram :: Expr
invalidEqualityConditionProgram =
  mkProgram (EIf (EBinary "==" (EInt 1) (EBool True)) (EInt 2) (EInt 3))

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

testSourcePipelineAcceptsWellTypedIf :: IO ()
testSourcePipelineAcceptsWellTypedIf = do
  result <- compileSource defaultWarningSettings "x = if True 1 else 2."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSourcePipelineRejectsNonBoolCondition :: IO ()
testSourcePipelineRejectsNonBoolCondition = do
  result <- compileSource defaultWarningSettings "x = if 1 2 else 3."
  assertSingleErrorContains
    "source condition type error"
    "E2001"
    (compileErrors result)
