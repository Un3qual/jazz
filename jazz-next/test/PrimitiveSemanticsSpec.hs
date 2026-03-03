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
main = runTestSuite "PrimitiveSemantics" tests

tests :: [NamedTest]
tests =
  [ ("arithmetic primitives accept Int operands", testAcceptsArithmeticIntOperands),
    ("strict equality accepts same-type Int operands", testAcceptsIntEquality),
    ("strict equality accepts same-type Bool operands", testAcceptsBoolEquality),
    ("strict equality rejects mismatched operand types", testRejectsEqualityTypeMismatch),
    ("strict inequality rejects mismatched operand types", testRejectsInequalityTypeMismatch),
    ("comparison primitives reject non-Int operands", testRejectsComparisonTypeMismatch),
    ("arithmetic primitives reject mismatched operand types", testRejectsArithmeticTypeMismatch)
  ]

testAcceptsArithmeticIntOperands :: IO ()
testAcceptsArithmeticIntOperands = do
  result <- compileExpr defaultWarningSettings arithmeticProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testAcceptsIntEquality :: IO ()
testAcceptsIntEquality = do
  result <- compileExpr defaultWarningSettings intEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testAcceptsBoolEquality :: IO ()
testAcceptsBoolEquality = do
  result <- compileExpr defaultWarningSettings boolEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testRejectsEqualityTypeMismatch :: IO ()
testRejectsEqualityTypeMismatch = do
  result <- compileExpr defaultWarningSettings equalityTypeMismatchProgram
  assertSingleErrorContains
    "strict equality type error"
    "E2004"
    (compileErrors result)

testRejectsInequalityTypeMismatch :: IO ()
testRejectsInequalityTypeMismatch = do
  result <- compileExpr defaultWarningSettings inequalityTypeMismatchProgram
  assertSingleErrorContains
    "strict inequality type error"
    "E2004"
    (compileErrors result)

testRejectsComparisonTypeMismatch :: IO ()
testRejectsComparisonTypeMismatch = do
  result <- compileExpr defaultWarningSettings comparisonTypeMismatchProgram
  assertSingleErrorContains
    "comparison type error"
    "E2003"
    (compileErrors result)

testRejectsArithmeticTypeMismatch :: IO ()
testRejectsArithmeticTypeMismatch = do
  result <- compileExpr defaultWarningSettings arithmeticTypeMismatchProgram
  assertSingleErrorContains
    "arithmetic type error"
    "E2003"
    (compileErrors result)

mkProgram :: Expr -> Expr
mkProgram expr =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

arithmeticProgram :: Expr
arithmeticProgram =
  mkProgram
    ( EBinary
        "+"
        (EBinary "*" (EInt 7) (EInt 6))
        (EBinary "/" (EInt 8) (EInt 2))
    )

intEqualityProgram :: Expr
intEqualityProgram =
  mkProgram (EBinary "==" (EInt 1) (EInt 1))

boolEqualityProgram :: Expr
boolEqualityProgram =
  mkProgram (EBinary "==" (EBool True) (EBool False))

equalityTypeMismatchProgram :: Expr
equalityTypeMismatchProgram =
  mkProgram (EBinary "==" (EInt 1) (EBool True))

inequalityTypeMismatchProgram :: Expr
inequalityTypeMismatchProgram =
  mkProgram (EBinary "!=" (EBool True) (EInt 1))

comparisonTypeMismatchProgram :: Expr
comparisonTypeMismatchProgram =
  mkProgram (EBinary "<" (EBool True) (EBool False))

arithmeticTypeMismatchProgram :: Expr
arithmeticTypeMismatchProgram =
  mkProgram (EBinary "+" (EInt 1) (EBool True))
