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
main = runTestSuite "PrimitiveSemantics" tests

tests :: [NamedTest]
tests =
  [ ("arithmetic primitives accept Int operands", testAcceptsArithmeticIntOperands),
    ("strict equality accepts same-type Int operands", testAcceptsIntEquality),
    ("strict equality accepts same-type Bool operands", testAcceptsBoolEquality),
    ("strict equality rejects mismatched operand types", testRejectsEqualityTypeMismatch),
    ("strict inequality rejects mismatched operand types", testRejectsInequalityTypeMismatch),
    ("comparison primitives reject non-Int operands", testRejectsComparisonTypeMismatch),
    ("arithmetic primitives reject mismatched operand types", testRejectsArithmeticTypeMismatch),
    ("source pipeline accepts hd with list literal argument", testSourcePipelineAcceptsHdListLiteral),
    ("source pipeline accepts map over nested list literals", testSourcePipelineAcceptsMapHdNestedLists),
    ("source pipeline rejects hd with non-list argument", testSourcePipelineRejectsHdNonListArgument),
    ("source pipeline rejects tl with non-list argument", testSourcePipelineRejectsTlNonListArgument),
    ("source pipeline rejects map with non-function mapper", testSourcePipelineRejectsMapNonFunctionMapper),
    ("source pipeline rejects map with non-list collection", testSourcePipelineRejectsMapNonListCollection),
    ("source pipeline accepts equality section application", testSourcePipelineAcceptsEqualitySection),
    ("source pipeline rejects arithmetic section with non-Int operand", testSourcePipelineRejectsArithmeticSectionTypeMismatch),
    ("source pipeline rejects equality section mismatched application", testSourcePipelineRejectsEqualitySectionTypeMismatch),
    ("source pipeline rejects unsupported section operator", testSourcePipelineRejectsUnsupportedSectionOperator),
    ("source pipeline rejects mixed-type list literals", testSourcePipelineRejectsMixedTypeListLiteral)
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

testSourcePipelineAcceptsHdListLiteral :: IO ()
testSourcePipelineAcceptsHdListLiteral = do
  result <- compileSource defaultWarningSettings "x = hd [1, 2, 3]."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSourcePipelineAcceptsMapHdNestedLists :: IO ()
testSourcePipelineAcceptsMapHdNestedLists = do
  result <- compileSource defaultWarningSettings "x = map hd [[1, 2], [3], [4, 5]]."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSourcePipelineRejectsHdNonListArgument :: IO ()
testSourcePipelineRejectsHdNonListArgument = do
  result <- compileSource defaultWarningSettings "x = hd 1."
  assertSingleErrorContains
    "hd argument type mismatch"
    "E2006"
    (compileErrors result)

testSourcePipelineRejectsTlNonListArgument :: IO ()
testSourcePipelineRejectsTlNonListArgument = do
  result <- compileSource defaultWarningSettings "x = tl 1."
  assertSingleErrorContains
    "tl argument type mismatch"
    "E2006"
    (compileErrors result)

testSourcePipelineRejectsMapNonFunctionMapper :: IO ()
testSourcePipelineRejectsMapNonFunctionMapper = do
  result <- compileSource defaultWarningSettings "x = map 1 [1, 2]."
  assertSingleErrorContains
    "map mapper type mismatch"
    "E2006"
    (compileErrors result)

testSourcePipelineRejectsMapNonListCollection :: IO ()
testSourcePipelineRejectsMapNonListCollection = do
  result <- compileSource defaultWarningSettings "x = map hd 1."
  assertSingleErrorContains
    "map collection type mismatch"
    "E2006"
    (compileErrors result)

testSourcePipelineAcceptsEqualitySection :: IO ()
testSourcePipelineAcceptsEqualitySection = do
  result <- compileSource defaultWarningSettings "x = (True ==) False."
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSourcePipelineRejectsArithmeticSectionTypeMismatch :: IO ()
testSourcePipelineRejectsArithmeticSectionTypeMismatch = do
  result <- compileSource defaultWarningSettings "x = (True +) 1."
  assertSingleErrorContains
    "arithmetic section operand mismatch"
    "E2003"
    (compileErrors result)

testSourcePipelineRejectsEqualitySectionTypeMismatch :: IO ()
testSourcePipelineRejectsEqualitySectionTypeMismatch = do
  result <- compileSource defaultWarningSettings "x = (True ==) 1."
  assertSingleErrorContains
    "equality section operand mismatch"
    "E2006"
    (compileErrors result)

testSourcePipelineRejectsUnsupportedSectionOperator :: IO ()
testSourcePipelineRejectsUnsupportedSectionOperator = do
  result <- compileSource defaultWarningSettings "x = ($ 1)."
  assertSingleErrorContains
    "unsupported section operator"
    "E2008"
    (compileErrors result)

testSourcePipelineRejectsMixedTypeListLiteral :: IO ()
testSourcePipelineRejectsMixedTypeListLiteral = do
  result <- compileSource defaultWarningSettings "x = [1, True]."
  assertSingleErrorContains
    "list literal element mismatch"
    "E2007"
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
