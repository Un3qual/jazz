{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "OperatorInvalidSyntax" tests

tests :: [NamedTest]
tests =
  [ ("rejects unsupported percent operator", testRejectsPercentOperator),
    ("rejects unsupported ampersand operator", testRejectsAmpersandOperator),
    ("rejects malformed right section with missing operand", testRejectsMalformedRightSection),
    ("rejects incomplete infix expression", testRejectsIncompleteInfixExpression)
  ]

testRejectsPercentOperator :: IO ()
testRejectsPercentOperator =
  assertLeftDiagnosticContains
    "percent operator"
    "unexpected character '%'"
    (parseSurfaceProgram "x = 1 % 2.")

testRejectsAmpersandOperator :: IO ()
testRejectsAmpersandOperator =
  assertLeftDiagnosticContains
    "ampersand operator"
    "unexpected character '&'"
    (parseSurfaceProgram "x = a && b.")

testRejectsMalformedRightSection :: IO ()
testRejectsMalformedRightSection =
  assertLeftDiagnosticContains
    "malformed section"
    "expected expression"
    (parseSurfaceProgram "f = (+).")

testRejectsIncompleteInfixExpression :: IO ()
testRejectsIncompleteInfixExpression =
  assertLeftDiagnosticContains
    "incomplete infix expression"
    "expected expression"
    (parseSurfaceProgram "x = 1 +.")
