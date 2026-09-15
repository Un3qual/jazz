{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Stdlib.OperatorFunctionsTests (operatorFunctionTests) where

import Data.Text (Text)
import Jazz.Compiler.Driver (runCompileErrors, runRuntimeErrors)
import Jazz.Compiler.Stdlib.Shared (assertSuccessfulStdlibOutput, runStdlibSource)
import Jazz.TestHarness (NamedTest, assertEqual, assertSingleDiagnosticCode)

operatorFunctionTests :: [NamedTest]
operatorFunctionTests =
  [ ("all operators dispatch through ordinary methods", testOperatorMethods),
    ("operator names respect lexical bindings", testOperatorScope),
    ("dollar conversions retain runtime validation", testDollarConversionValidation),
    ("arithmetic functions retain runtime overflow checks", testArithmeticOverflow)
  ]
    <> map rejectsMixedNumericOperands ["left + right", "left - right", "left * right", "left / right", "left == right", "left != right", "left < right", "left <= right", "left > right", "left >= right", "(+) left right", "(+ right) left", "(left +) right", "add left right", "equals left right"]
    <> [("ADT equality requires an implementation", rejects "E2009" "data Token = Token Int. Token 1 == Token 1.")]

-- Deliberately nonstructural equality and reversed ordering prove that syntax
-- uses these implementations instead of an independent primitive evaluator.
testOperatorMethods :: IO ()
testOperatorMethods = do
  result <-
    runStdlibSource
      ["OperatorFunctions"]
      """
      module OperatorFunctions {
        data Amount = Amount Int.
        impl Equatable(Amount) { equals = \\(left, right) -> False. }.
        impl Comparable(Amount) { compare = \\(Amount left, Amount right) -> compare right left. }.
        impl Num(Amount) {
          add = \\(Amount left, Amount right) -> Amount (add left right).
          subtract = \\(Amount left, Amount right) -> Amount (subtract left right).
          multiply = \\(Amount left, Amount right) -> Amount (multiply left right).
          divide = \\(Amount left, Amount right) -> Amount (divide left right).
        }.
        left = Amount 12.
        right = Amount 3.
        plus = (+).
        (left + right, left - right, left * right, left / right,
         left == left, left != left, left < right, left <= right, left > right, left >= right,
         plus left right, (left -) right, (- right) left,
         (==) left left, (< right) left, (left >=) right,
         not $ True, ($) not False, ($ True) not, (not $) True, equals left left, compare left right).
      }
      """
  assertSuccessfulStdlibOutput "(Amount(15), Amount(9), Amount(36), Amount(4), False, True, True, True, False, False, Amount(15), Amount(9), Amount(9), False, True, False, False, True, False, False, False, LT)" result

testOperatorScope :: IO ()
testOperatorScope = do
  result <-
    runStdlibSource
      ["OperatorFunctions"]
      """
      module OperatorFunctions {
        add = \\(left, right) -> subtract left right.
        equals = \\(left, right) -> False.
        lessThan = \\(left, right) -> False.
        apply = \\(function, argument) -> not (function argument).
        (8 + 3, (+) 8 3, (8 +) 3, (+ 3) 8, 1 == 1, 1 < 2, not $ True, ($) not True).
      }
      """
  assertSuccessfulStdlibOutput "(5, 5, 5, 5, False, False, True, True)" result

rejectsMixedNumericOperands :: Text -> NamedTest
rejectsMixedNumericOperands expression =
  ( "ordinary numeric argument rules: " <> expression,
    rejects "E2006" ("left :: Int. left = 1. right :: Float. right = 1.0. " <> expression <> ".")
  )

rejects :: Text -> Text -> IO ()
rejects code body = do
  result <- runStdlibSource ["OperatorFunctions"] ("module OperatorFunctions { " <> body <> " }")
  assertSingleDiagnosticCode "operator function diagnostic" code (runCompileErrors result)

testDollarConversionValidation :: IO ()
testDollarConversionValidation = mapM_ check ["toInt8 $ 1.5", "apply toInt8 1.5", "toUInt8 $ 256", "apply toUInt8 256"]
  where
    check expression = do
      result <- runStdlibSource ["OperatorFunctions"] ("module OperatorFunctions { " <> expression <> ". }")
      assertEqual "higher-order conversion compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticCode "higher-order conversion runtime error" "E3024" (runRuntimeErrors result)

testArithmeticOverflow :: IO ()
testArithmeticOverflow = mapM_ check ["200 + 100", "add 200 100", "0 - 1", "subtract 0 1", "16 * 16", "multiply 16 16"]
  where
    check expression = do
      result <- runStdlibSource ["OperatorFunctions"] ("module OperatorFunctions { x :: UInt8. x = " <> expression <> ". x. }")
      assertEqual "arithmetic compile errors" [] (runCompileErrors result)
      assertSingleDiagnosticCode "arithmetic runtime error" "E3025" (runRuntimeErrors result)
