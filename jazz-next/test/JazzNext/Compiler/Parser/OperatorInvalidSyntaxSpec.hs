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
  [ ("rejects built-in operator declarations", testRejectsBuiltinOperatorDeclaration),
    ("rejects identifier operator declarations", testRejectsIdentifierOperatorDeclaration),
    ("rejects reserved arrow operator declarations", testRejectsReservedArrowOperatorDeclaration),
    ("rejects reserved comment operator declarations", testRejectsReservedCommentOperatorDeclaration),
    ("rejects invalid operator declaration tiers", testRejectsInvalidOperatorDeclarationTier),
    ("rejects duplicate operator declarations", testRejectsDuplicateOperatorDeclaration),
    ("rejects nested operator declarations", testRejectsNestedOperatorDeclaration),
    ("rejects zero custom operator precedence declarations", testRejectsZeroCustomOperatorPrecedenceDeclaration),
    ("rejects high custom operator precedence declarations", testRejectsHighCustomOperatorPrecedenceDeclaration),
    ("rejects invalid operator associativity keyword", testRejectsInvalidOperatorAssociativityKeyword),
    ("rejects non-associative operator chains", testRejectsNonAssociativeOperatorChain),
    ("rejects user operator infix use before declaration", testRejectsUserOperatorInfixUseBeforeDeclaration),
    ("rejects user operator value use before declaration", testRejectsUserOperatorValueUseBeforeDeclaration),
    ("rejects undeclared operator signature", testRejectsUndeclaredOperatorSignature),
    ("rejects built-in operator signature", testRejectsBuiltinOperatorSignature),
    ("rejects undeclared operator binding", testRejectsUndeclaredOperatorBinding),
    ("rejects built-in operator binding", testRejectsBuiltinOperatorBinding),
    ("rejects nested operator binding", testRejectsNestedOperatorBinding),
    ("rejects nested operator signature", testRejectsNestedOperatorSignature),
    ("rejects module declarations after operator declarations", testRejectsModuleAfterOperatorDeclaration),
    ("rejects undeclared percent operator", testRejectsUndeclaredPercentOperator),
    ("rejects undeclared ampersand operator", testRejectsUndeclaredAmpersandOperator),
    ("rejects empty parenthesized expression", testRejectsEmptyParenthesizedExpression),
    ("rejects incomplete infix expression", testRejectsIncompleteInfixExpression)
  ]

testRejectsBuiltinOperatorDeclaration :: IO ()
testRejectsBuiltinOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "built-in operator declaration"
    "E0001"
    "cannot redeclare built-in operator '+'"
    (parseSurfaceProgram "operator + tier 2.")

testRejectsIdentifierOperatorDeclaration :: IO ()
testRejectsIdentifierOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "identifier operator declaration"
    "E0001"
    "expected operator symbol after 'operator'"
    (parseSurfaceProgram "operator abc tier 2.")

testRejectsReservedArrowOperatorDeclaration :: IO ()
testRejectsReservedArrowOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "reserved arrow operator declaration"
    "E0001"
    "reserved operator symbol '->'"
    (parseSurfaceProgram "operator -> tier 5.")

testRejectsReservedCommentOperatorDeclaration :: IO ()
testRejectsReservedCommentOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "reserved comment operator declaration"
    "E0001"
    "reserved operator symbol '--'"
    (parseSurfaceProgram "operator -- tier 1.")

testRejectsInvalidOperatorDeclarationTier :: IO ()
testRejectsInvalidOperatorDeclarationTier =
  assertLeftDiagnosticCodeAndContains
    "invalid operator declaration tier"
    "E0001"
    "operator tier must be between 1 and 5"
    (parseSurfaceProgram "operator %% tier 6.")

testRejectsDuplicateOperatorDeclaration :: IO ()
testRejectsDuplicateOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "duplicate operator declaration"
    "E0001"
    "duplicate operator declaration '%%'"
    (parseSurfaceProgram "operator %% tier 2.\noperator %% tier 3.")

testRejectsNestedOperatorDeclaration :: IO ()
testRejectsNestedOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "nested operator declaration"
    "E0001"
    "operator declarations are only allowed at file scope or directly in module bodies"
    (parseSurfaceProgram "x = { operator %% tier 2. y = 1. }.")

testRejectsZeroCustomOperatorPrecedenceDeclaration :: IO ()
testRejectsZeroCustomOperatorPrecedenceDeclaration =
  assertLeftDiagnosticCodeAndContains
    "zero custom operator precedence declaration"
    "E0001"
    "operator precedence must be between 1 and 99"
    (parseSurfaceProgram "operator %% precedence 0.")

testRejectsHighCustomOperatorPrecedenceDeclaration :: IO ()
testRejectsHighCustomOperatorPrecedenceDeclaration =
  assertLeftDiagnosticCodeAndContains
    "high custom operator precedence declaration"
    "E0001"
    "operator precedence must be between 1 and 99"
    (parseSurfaceProgram "operator %% precedence 100.")

testRejectsInvalidOperatorAssociativityKeyword :: IO ()
testRejectsInvalidOperatorAssociativityKeyword =
  assertLeftDiagnosticCodeAndContains
    "invalid operator associativity keyword"
    "E0001"
    "expected operator associativity 'left', 'right', or 'nonassoc'"
    (parseSurfaceProgram "operator %% tier 2 sideways.")

testRejectsNonAssociativeOperatorChain :: IO ()
testRejectsNonAssociativeOperatorChain =
  assertLeftDiagnosticCodeAndContains
    "non-associative operator chain"
    "E0001"
    "non-associative operator '?>' cannot be chained without parentheses"
    (parseSurfaceProgram "operator ?> precedence 10 nonassoc.\nx = 1 ?> 2 ?> 3.")

testRejectsUserOperatorInfixUseBeforeDeclaration :: IO ()
testRejectsUserOperatorInfixUseBeforeDeclaration =
  assertLeftDiagnosticCodeAndContains
    "user operator infix use before declaration"
    "E0001"
    "operator '%%' must be declared before use"
    (parseSurfaceProgram "x = 1 %% 2.\noperator %% tier 2.")

testRejectsUserOperatorValueUseBeforeDeclaration :: IO ()
testRejectsUserOperatorValueUseBeforeDeclaration =
  assertLeftDiagnosticCodeAndContains
    "user operator value use before declaration"
    "E0001"
    "operator '%%' must be declared before use"
    (parseSurfaceProgram "x = (%%).\noperator %% tier 2.")

testRejectsUndeclaredOperatorSignature :: IO ()
testRejectsUndeclaredOperatorSignature =
  assertLeftDiagnosticCodeAndContains
    "undeclared operator signature"
    "E0001"
    "operator '%%' must be declared before signature"
    (parseSurfaceProgram "(%%) :: Int -> Int -> Int.\n(%%) = \\(left) -> \\(right) -> left + right.")

testRejectsBuiltinOperatorSignature :: IO ()
testRejectsBuiltinOperatorSignature =
  assertLeftDiagnosticCodeAndContains
    "built-in operator signature"
    "E0001"
    "cannot sign built-in operator '+'"
    (parseSurfaceProgram "(+) :: Int -> Int -> Int.\noperator %% tier 2.")

testRejectsUndeclaredOperatorBinding :: IO ()
testRejectsUndeclaredOperatorBinding =
  assertLeftDiagnosticCodeAndContains
    "undeclared operator binding"
    "E0001"
    "operator '%%' must be declared before binding"
    (parseSurfaceProgram "(%%) = \\(left) -> \\(right) -> left + right.")

testRejectsBuiltinOperatorBinding :: IO ()
testRejectsBuiltinOperatorBinding =
  assertLeftDiagnosticCodeAndContains
    "built-in operator binding"
    "E0001"
    "cannot bind built-in operator '+'"
    (parseSurfaceProgram "(+) = \\(left) -> \\(right) -> left + right.")

testRejectsNestedOperatorBinding :: IO ()
testRejectsNestedOperatorBinding =
  assertLeftDiagnosticCodeAndContains
    "nested operator binding"
    "E0001"
    "operator bindings are only allowed at file scope or directly in module bodies"
    (parseSurfaceProgram "operator %% tier 2.\nx = { (%%) = \\(left) -> \\(right) -> left + right. 0. }.")

testRejectsNestedOperatorSignature :: IO ()
testRejectsNestedOperatorSignature =
  assertLeftDiagnosticCodeAndContains
    "nested operator signature"
    "E0001"
    "operator signatures are only allowed at file scope or directly in module bodies"
    (parseSurfaceProgram "operator %% tier 2.\nx = { (%%) :: Int -> Int -> Int. 0. }.")

testRejectsModuleAfterOperatorDeclaration :: IO ()
testRejectsModuleAfterOperatorDeclaration =
  assertLeftDiagnosticCodeAndContains
    "module after operator declaration"
    "E0001"
    "module declaration must be the first top-level form"
    (parseSurfaceProgram "operator %% tier 2.\nmodule Foo { x = 1. }")

testRejectsUndeclaredPercentOperator :: IO ()
testRejectsUndeclaredPercentOperator =
  assertLeftDiagnosticCodeAndContains
    "percent operator"
    "E0001"
    "operator '%' must be declared before use"
    (parseSurfaceProgram "x = 1 % 2.")

testRejectsUndeclaredAmpersandOperator :: IO ()
testRejectsUndeclaredAmpersandOperator =
  assertLeftDiagnosticCodeAndContains
    "ampersand operator"
    "E0001"
    "operator '&&' must be declared before use"
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
