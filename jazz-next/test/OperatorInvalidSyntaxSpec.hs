{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftContains,
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
  assertLeftContains
    "percent operator"
    "unexpected character '%'"
    (parseSurfaceProgram "x = 1 % 2.")

testRejectsAmpersandOperator :: IO ()
testRejectsAmpersandOperator =
  assertLeftContains
    "ampersand operator"
    "unexpected character '&'"
    (parseSurfaceProgram "x = a && b.")

testRejectsEmptyParenthesizedExpression :: IO ()
testRejectsEmptyParenthesizedExpression =
  assertLeftContains
    "empty parens"
    "expected expression"
    (parseSurfaceProgram "f = ().")

testRejectsIncompleteInfixExpression :: IO ()
testRejectsIncompleteInfixExpression =
  assertLeftContains
    "incomplete infix expression"
    "expected expression"
    (parseSurfaceProgram "x = 1 +.")
