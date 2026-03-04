{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    assertSingleErrorContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "RuntimeSemantics" tests

tests :: [NamedTest]
tests =
  [ ("if with False condition skips then branch runtime failure", testIfFalseSkipsThenRuntimeFailure),
    ("if with True condition skips else branch runtime failure", testIfTrueSkipsElseRuntimeFailure),
    ("division by zero produces fatal runtime diagnostic", testDivisionByZeroRuntimeError),
    ("left operator section applies at runtime", testLeftOperatorSectionRuntimeSuccess),
    ("right operator section applies at runtime", testRightOperatorSectionRuntimeSuccess),
    ("map + hd evaluates over nested list literals", testMapHdNestedListsRuntimeSuccess),
    ("tl returns the tail of a non-empty list", testTlReturnsTailRuntimeValue),
    ("hd on empty list produces fatal runtime diagnostic", testHdEmptyListRuntimeError),
    ("tl on empty list produces fatal runtime diagnostic", testTlEmptyListRuntimeError),
    ("runtime fallback rejects hd on non-list values", testRuntimeFallbackRejectsHdNonList),
    ("runtime fallback rejects tl on non-list values", testRuntimeFallbackRejectsTlNonList),
    ("runtime fallback rejects map with non-function mapper", testRuntimeFallbackRejectsMapNonFunctionMapper),
    ("runtime fallback rejects map with non-list collection", testRuntimeFallbackRejectsMapNonListCollection),
    ("scope with only declarations has no runtime output", testDeclarationOnlyScopeHasNoOutput),
    ("scope result requires terminal expression", testScopeDeclarationAfterExprClearsResult)
  ]

testIfFalseSkipsThenRuntimeFailure :: IO ()
testIfFalseSkipsThenRuntimeFailure = do
  result <- runSource defaultWarningSettings "if False (1 / 0) else 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2") (runOutput result)

testIfTrueSkipsElseRuntimeFailure :: IO ()
testIfTrueSkipsElseRuntimeFailure = do
  result <- runSource defaultWarningSettings "if True 1 else (1 / 0)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)

testDivisionByZeroRuntimeError :: IO ()
testDivisionByZeroRuntimeError = do
  result <- runSource defaultWarningSettings "1 / 0."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleErrorContains
    "runtime fatal division by zero"
    "E3001"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected division-by-zero runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions division by zero"
        "division by zero"
        runtimeError
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testLeftOperatorSectionRuntimeSuccess :: IO ()
testLeftOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(1 +) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testRightOperatorSectionRuntimeSuccess :: IO ()
testRightOperatorSectionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(+ 1) 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testMapHdNestedListsRuntimeSuccess :: IO ()
testMapHdNestedListsRuntimeSuccess = do
  result <- runSource defaultWarningSettings "map hd [[1, 2], [3], [4, 5]]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3, 4]") (runOutput result)

testTlReturnsTailRuntimeValue :: IO ()
testTlReturnsTailRuntimeValue = do
  result <- runSource defaultWarningSettings "tl [1, 2, 3]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[2, 3]") (runOutput result)

testHdEmptyListRuntimeError :: IO ()
testHdEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "hd []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleErrorContains
    "runtime fatal empty-list hd"
    "E3009"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list hd runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        runtimeError
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testTlEmptyListRuntimeError :: IO ()
testTlEmptyListRuntimeError = do
  result <- runSource defaultWarningSettings "tl []."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleErrorContains
    "runtime fatal empty-list tl"
    "E3010"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected empty-list tl runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions empty list"
        "empty list"
        runtimeError
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRuntimeFallbackRejectsHdNonList :: IO ()
testRuntimeFallbackRejectsHdNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "hd") (EInt 1)))
  assertRuntimeErrorContains "runtime fallback hd non-list" "E3011" result

testRuntimeFallbackRejectsTlNonList :: IO ()
testRuntimeFallbackRejectsTlNonList = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "tl") (EInt 1)))
  assertRuntimeErrorContains "runtime fallback tl non-list" "E3012" result

testRuntimeFallbackRejectsMapNonFunctionMapper :: IO ()
testRuntimeFallbackRejectsMapNonFunctionMapper = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "map") (EInt 1)) (EList [EInt 1])))
  assertRuntimeErrorContains "runtime fallback map mapper" "E3015" result

testRuntimeFallbackRejectsMapNonListCollection :: IO ()
testRuntimeFallbackRejectsMapNonListCollection = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EApply (EVar "map") (EVar "hd")) (EInt 1)))
  assertRuntimeErrorContains "runtime fallback map collection" "E3013" result

testDeclarationOnlyScopeHasNoOutput :: IO ()
testDeclarationOnlyScopeHasNoOutput = do
  result <- runSource defaultWarningSettings "x = 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration-only scope produces no output" Nothing (runOutput result)

testScopeDeclarationAfterExprClearsResult :: IO ()
testScopeDeclarationAfterExprClearsResult = do
  result <- runSource defaultWarningSettings "x = 1. x. y = 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "declaration after expression clears scope result" Nothing (runOutput result)

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

assertRuntimeErrorContains :: Text -> Text -> Either Text (Maybe a) -> IO ()
assertRuntimeErrorContains label expectedCode result =
  case result of
    Left runtimeError ->
      assertContains label expectedCode runtimeError
    Right _ ->
      failTest ("expected runtime error containing " <> expectedCode <> ", but evaluation succeeded")
