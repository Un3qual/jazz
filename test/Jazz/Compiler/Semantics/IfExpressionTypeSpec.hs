{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.AST
  ( CorePhase (Lowered),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileExpr,
    compileSource,
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestCore
  ( loweredBlock,
    loweredExpression,
    loweredIf,
    loweredLiteral,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimaryStart,
    runTestSuite,
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
  assertSingleDiagnosticCode
    "condition type error code"
    "E2001"
    (compileErrors result)
  assertSingleDiagnosticContains
    "condition type error"
    "if condition must have type Bool"
    (compileErrors result)
  assertSingleDiagnosticPrimaryStart
    "condition type error primary span"
    (SourceSpan 1 1)
    (compileErrors result)

testAcceptsEqualityCondition :: IO ()
testAcceptsEqualityCondition = do
  result <- compileSource defaultWarningSettings "if 1 == 1 then 2 else 3."
  assertEqual "compile errors" [] (compileErrors result)

testRejectsInvalidEqualityCondition :: IO ()
testRejectsInvalidEqualityCondition = do
  result <- compileSource defaultWarningSettings "if 1 == True then 2 else 3."
  assertSingleDiagnosticContains
    "strict equality condition type error"
    "E2006"
    (compileErrors result)

testRejectsMismatchedBranchTypes :: IO ()
testRejectsMismatchedBranchTypes = do
  result <- compileExpr defaultWarningSettings mismatchedBranchProgram
  assertSingleDiagnosticContains
    "branch type mismatch"
    "if branches must have matching types"
    (compileErrors result)

testAcceptsWellTypedIf :: IO ()
testAcceptsWellTypedIf = do
  result <- compileExpr defaultWarningSettings validIfProgram
  assertEqual "compile errors" [] (compileErrors result)

mkProgram :: Expr 'Lowered -> Expr 'Lowered
mkProgram expr =
  loweredBlock
    [ loweredExpression
        (SourceSpan 1 1)
        expr
    ]

nonBoolConditionProgram :: Expr 'Lowered
nonBoolConditionProgram =
  mkProgram (loweredIf (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2)) (loweredLiteral (LInt 3)))

mismatchedBranchProgram :: Expr 'Lowered
mismatchedBranchProgram =
  mkProgram (loweredIf (loweredLiteral (LBool True)) (loweredLiteral (LInt 1)) (loweredLiteral (LBool False)))

validIfProgram :: Expr 'Lowered
validIfProgram =
  mkProgram (loweredIf (loweredLiteral (LBool True)) (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2)))

testRejectsBinaryTypeMismatch :: IO ()
testRejectsBinaryTypeMismatch = do
  result <- compileSource defaultWarningSettings "1 + True."
  assertSingleDiagnosticContains
    "binary type error"
    "cannot apply function"
    (compileErrors result)

testSourcePipelineAcceptsWellTypedIf :: IO ()
testSourcePipelineAcceptsWellTypedIf = do
  result <- compileSource defaultWarningSettings "x = if True then 1 else 2."
  assertEqual "compile errors" [] (compileErrors result)

testSourcePipelineRejectsNonBoolCondition :: IO ()
testSourcePipelineRejectsNonBoolCondition = do
  result <- compileSource defaultWarningSettings "x = if 1 then 2 else 3."
  assertSingleDiagnosticPrimaryStart
    "source condition type error primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticContains
    "source condition type error"
    "E2001"
    (compileErrors result)
