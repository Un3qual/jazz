{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftDiagnosticCodeAndContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "OperatorInvalidSyntax" tests

tests :: [NamedTest]
tests =
  [ ("rejects unsupported percent operator", testRejectsPercentOperator),
    ("rejects unsupported ampersand operator", testRejectsAmpersandOperator),
    ("rejects empty parenthesized expression", testRejectsEmptyParenthesizedExpression),
    ("rejects incomplete infix expression", testRejectsIncompleteInfixExpression)
  ]

testRejectsPercentOperator :: IO ()
testRejectsPercentOperator =
  assertLeftDiagnosticCodeAndContains
    "percent operator"
    "E0001"
    "unexpected character '%'"
    (parseSurfaceProgram "x = 1 % 2.")

testRejectsAmpersandOperator :: IO ()
testRejectsAmpersandOperator =
  assertLeftDiagnosticCodeAndContains
    "ampersand operator"
    "E0001"
    "unexpected character '&'"
    (parseSurfaceProgram "x = a && b.")

testRejectsEmptyParenthesizedExpression :: IO ()
testRejectsEmptyParenthesizedExpression =
  assertLeftDiagnosticCodeAndContains
    "empty parens"
    "E0001"
    "expected expression"
    (parseSurfaceProgram "f = ().")

testRejectsIncompleteInfixExpression :: IO ()
testRejectsIncompleteInfixExpression =
  assertLeftDiagnosticCodeAndContains
    "incomplete infix expression"
    "E0001"
    "expected expression"
    (parseSurfaceProgram "x = 1 +.")
