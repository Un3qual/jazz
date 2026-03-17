{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
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
    compileSource,
    compileSourceWithPrelude
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
    ("source pipeline accepts filter over list literals", testSourcePipelineAcceptsFilterListLiteral),
    ("source pipeline rejects hd with non-list argument", testSourcePipelineRejectsHdNonListArgument),
    ("source pipeline rejects tl with non-list argument", testSourcePipelineRejectsTlNonListArgument),
    ("source pipeline rejects map with non-function mapper", testSourcePipelineRejectsMapNonFunctionMapper),
    ("source pipeline rejects map with non-list collection", testSourcePipelineRejectsMapNonListCollection),
    ("source pipeline rejects filter with non-function predicate", testSourcePipelineRejectsFilterNonFunctionPredicate),
    ("source pipeline rejects filter with non-list collection", testSourcePipelineRejectsFilterNonListCollection),
    ("source pipeline rejects filter predicate with non-Bool result", testSourcePipelineRejectsFilterPredicateNonBoolResult),
    ("source pipeline accepts equality section application", testSourcePipelineAcceptsEqualitySection),
    ("source pipeline accepts deferred left equality section once constrained", testSourcePipelineAcceptsDeferredLeftEqualitySection),
    ("source pipeline accepts deferred right equality section once constrained", testSourcePipelineAcceptsDeferredRightEqualitySection),
    ("source pipeline rejects arithmetic section with non-Int operand", testSourcePipelineRejectsArithmeticSectionTypeMismatch),
    ("source pipeline rejects equality section mismatched application", testSourcePipelineRejectsEqualitySectionTypeMismatch),
    ("source pipeline rejects deferred equality section constrained to list", testSourcePipelineRejectsDeferredEqualitySectionListConstraint),
    ("source pipeline rejects list equality until runtime support exists", testSourcePipelineRejectsListEquality),
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
testSourcePipelineAcceptsHdListLiteral =
  assertCompilesWithBundledPrelude "x = hd [1, 2, 3]."

testSourcePipelineAcceptsMapHdNestedLists :: IO ()
testSourcePipelineAcceptsMapHdNestedLists =
  assertCompilesWithBundledPrelude "x = map hd [[1, 2], [3], [4, 5]]."

testSourcePipelineAcceptsFilterListLiteral :: IO ()
testSourcePipelineAcceptsFilterListLiteral =
  assertCompilesWithBundledPrelude "x = filter (> 1) [1, 2, 3]."

testSourcePipelineRejectsHdNonListArgument :: IO ()
testSourcePipelineRejectsHdNonListArgument =
  assertCompileErrorWithBundledPrelude
    "x = hd 1."
    "hd argument type mismatch"
    "E2006"

testSourcePipelineRejectsTlNonListArgument :: IO ()
testSourcePipelineRejectsTlNonListArgument =
  assertCompileErrorWithBundledPrelude
    "x = tl 1."
    "tl argument type mismatch"
    "E2006"

testSourcePipelineRejectsMapNonFunctionMapper :: IO ()
testSourcePipelineRejectsMapNonFunctionMapper =
  assertCompileErrorWithBundledPrelude
    "x = map 1 [1, 2]."
    "map mapper type mismatch"
    "E2006"

testSourcePipelineRejectsMapNonListCollection :: IO ()
testSourcePipelineRejectsMapNonListCollection =
  assertCompileErrorWithBundledPrelude
    "x = map hd 1."
    "map collection type mismatch"
    "E2006"

testSourcePipelineRejectsFilterNonFunctionPredicate :: IO ()
testSourcePipelineRejectsFilterNonFunctionPredicate =
  assertCompileErrorWithBundledPrelude
    "x = filter 1 [1, 2]."
    "filter predicate type mismatch"
    "E2006"

testSourcePipelineRejectsFilterNonListCollection :: IO ()
testSourcePipelineRejectsFilterNonListCollection =
  assertCompileErrorWithBundledPrelude
    "x = filter (> 1) 1."
    "filter collection type mismatch"
    "E2006"

testSourcePipelineRejectsFilterPredicateNonBoolResult :: IO ()
testSourcePipelineRejectsFilterPredicateNonBoolResult =
  assertCompileErrorWithBundledPrelude
    "x = filter (+ 1) [1, 2]."
    "filter predicate non-bool mismatch"
    "E2006"

testSourcePipelineAcceptsEqualitySection :: IO ()
testSourcePipelineAcceptsEqualitySection =
  assertCompiles "x = (True ==) False."

testSourcePipelineAcceptsDeferredLeftEqualitySection :: IO ()
testSourcePipelineAcceptsDeferredLeftEqualitySection =
  assertCompilesWithBundledPrelude "x = (hd [] ==) 1."

testSourcePipelineAcceptsDeferredRightEqualitySection :: IO ()
testSourcePipelineAcceptsDeferredRightEqualitySection =
  assertCompilesWithBundledPrelude "x = (== hd []) 1."

testSourcePipelineRejectsArithmeticSectionTypeMismatch :: IO ()
testSourcePipelineRejectsArithmeticSectionTypeMismatch =
  assertCompileError
    "x = (True +) 1."
    "arithmetic section operand mismatch"
    "E2003"

testSourcePipelineRejectsEqualitySectionTypeMismatch :: IO ()
testSourcePipelineRejectsEqualitySectionTypeMismatch =
  assertCompileError
    "x = (True ==) 1."
    "equality section operand mismatch"
    "E2006"

testSourcePipelineRejectsDeferredEqualitySectionListConstraint :: IO ()
testSourcePipelineRejectsDeferredEqualitySectionListConstraint =
  assertCompileErrorWithBundledPrelude
    "x = (hd [] ==) []."
    "deferred equality section must still reject unsupported concrete operand family"
    "E2006"

testSourcePipelineRejectsListEquality :: IO ()
testSourcePipelineRejectsListEquality =
  assertCompileError
    "x = [1] == [1]."
    "list equality unsupported in runtime subset"
    "E2004"

testSourcePipelineRejectsUnsupportedSectionOperator :: IO ()
testSourcePipelineRejectsUnsupportedSectionOperator =
  assertCompileError
    "x = ($ 1)."
    "unsupported section operator"
    "E2008"

testSourcePipelineRejectsMixedTypeListLiteral :: IO ()
testSourcePipelineRejectsMixedTypeListLiteral =
  assertCompileError
    "x = [1, True]."
    "list literal element mismatch"
    "E2007"

assertCompiles :: String -> IO ()
assertCompiles source = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

assertCompilesWithBundledPrelude :: String -> IO ()
assertCompilesWithBundledPrelude source = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

assertCompileError :: String -> String -> String -> IO ()
assertCompileError source failureLabel errorCode = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertSingleErrorContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

assertCompileErrorWithBundledPrelude :: String -> String -> String -> IO ()
assertCompileErrorWithBundledPrelude source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertSingleErrorContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

bundledPreludeSource :: Text.Text
bundledPreludeSource =
  "map = __kernel_map.\n\
  \filter = __kernel_filter.\n\
  \hd = __kernel_hd.\n\
  \tl = __kernel_tl.\n\
  \print! = __kernel_print!."

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
