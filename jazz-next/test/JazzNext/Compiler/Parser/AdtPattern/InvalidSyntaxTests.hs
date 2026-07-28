{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.AdtPattern.InvalidSyntaxTests
  ( invalidSyntaxTests
  ) where

import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftDiagnosticContains
  )

invalidSyntaxTests :: [NamedTest]
invalidSyntaxTests =
  [ ("rejects missing guard arrow before guarded constructor arm", testRejectsMissingGuardArrowBeforeGuardedConstructorArm)
    , ("reports missing case body for block-valued scrutinee", testReportsMissingCaseBodyForBlockScrutinee)
    , ("reports block parse error for unterminated fractional block scrutinee", testReportsBlockErrorForUnterminatedFractionalBlockScrutinee)
    , ("reports missing arm arrow for block-valued scrutinee", testReportsMissingArmArrowForBlockScrutinee)
    , ("reports invalid case scrutinee syntax before body diagnostics", testReportsInvalidCaseScrutineeSyntax)
    , ("rejects case expression without leading pipe", testRejectsCaseExpressionWithoutPipe)
    , ("rejects case expression without arm arrow", testRejectsCaseExpressionWithoutArrow)
    , ("rejects data declaration without constructors", testRejectsDataDeclarationWithoutConstructors)
    , ("rejects duplicate constructor names in one data declaration", testRejectsDuplicateConstructorsInDataDeclaration)
    , ("rejects duplicate data type parameters", testRejectsDuplicateDataTypeParameters)
    , ("rejects undeclared generic constructor payload names", testRejectsUndeclaredGenericConstructorPayloadNames)
    , ("rejects undeclared nested constructor payload names", testRejectsUndeclaredNestedConstructorPayloadNames)
    , ("rejects empty named constructor field applications", testRejectsEmptyNamedConstructorFieldApplications)
    , ("rejects unterminated grouped constructor field types", testRejectsUnterminatedGroupedConstructorFieldTypes)
    , ("rejects data declaration with malformed pipe placement", testRejectsDataDeclarationWithMalformedPipePlacement)
    , ("rejects data declaration missing terminator", testRejectsDataDeclarationMissingTerminator)
    , ("rejects malformed parenthesized list-like patterns without tuple diagnostic", testRejectsMalformedParenthesizedListLikePattern)
    , ("rejects malformed list patterns", testRejectsMalformedListPattern)
    , ("rejects malformed later list patterns", testRejectsMalformedLaterListPattern)
    , ("rejects malformed guard expression", testRejectsMalformedGuardExpression)
    , ("rejects malformed or-pattern alternatives", testRejectsMalformedOrPatternAlternative)
  ]

testRejectsMissingGuardArrowBeforeGuardedConstructorArm :: IO ()
testRejectsMissingGuardArrowBeforeGuardedConstructorArm =
  assertLeftDiagnosticContains
    "missing guard arrow before guarded constructor arm"
    "expected '->'"
    (parseSurfaceProgram "x = case m { | item if item < 0 | Just if ok -> item | _ -> m }.")

testReportsMissingCaseBodyForBlockScrutinee :: IO ()
testReportsMissingCaseBodyForBlockScrutinee =
  assertLeftDiagnosticContains
    "block scrutinee missing case body"
    "expected '{', found '.'"
    (parseSurfaceProgram "x = case f { y = 1. y. }.")

testReportsBlockErrorForUnterminatedFractionalBlockScrutinee :: IO ()
testReportsBlockErrorForUnterminatedFractionalBlockScrutinee =
  assertLeftDiagnosticContains
    "unterminated fractional block scrutinee parse error"
    "expected '.'"
    (parseSurfaceProgram "x = case f { y = 1.5 }.")

testReportsMissingArmArrowForBlockScrutinee :: IO ()
testReportsMissingArmArrowForBlockScrutinee =
  assertLeftDiagnosticContains
    "block scrutinee missing arm arrow"
    "expected '->'"
    (parseSurfaceProgram "x = case f { y = 1. y. } { | 1 True }.")

testReportsInvalidCaseScrutineeSyntax :: IO ()
testReportsInvalidCaseScrutineeSyntax =
  assertLeftDiagnosticContains
    "invalid case scrutinee syntax"
    "unexpected token '+'"
    (parseSurfaceProgram "x = case + { | 0 -> True }.")

testRejectsCaseExpressionWithoutPipe :: IO ()
testRejectsCaseExpressionWithoutPipe =
  assertLeftDiagnosticContains
    "missing case-arm pipe"
    "expected '|'"
    (parseSurfaceProgram "x = case n { 0 -> True }.")

testRejectsCaseExpressionWithoutArrow :: IO ()
testRejectsCaseExpressionWithoutArrow =
  assertLeftDiagnosticContains
    "missing case-arm arrow"
    "expected '->'"
    (parseSurfaceProgram "x = case n { | 0 True }.")

testRejectsDataDeclarationWithoutConstructors :: IO ()
testRejectsDataDeclarationWithoutConstructors =
  assertLeftDiagnosticContains
    "empty data constructor list"
    "expected constructor declaration"
    (parseSurfaceProgram "data Maybe = .")

testRejectsDuplicateConstructorsInDataDeclaration :: IO ()
testRejectsDuplicateConstructorsInDataDeclaration =
  assertLeftDiagnosticContains
    "duplicate data constructor"
    "duplicate constructor declaration 'Nothing'"
    (parseSurfaceProgram "data Maybe = Nothing | Nothing Int.")

testRejectsDuplicateDataTypeParameters :: IO ()
testRejectsDuplicateDataTypeParameters =
  assertLeftDiagnosticContains
    "duplicate data type parameter diagnostic"
    "duplicate type parameter 'a' in data declaration"
    (parseSurfaceProgram "data Pair a a = Pair a a.")

testRejectsUndeclaredGenericConstructorPayloadNames :: IO ()
testRejectsUndeclaredGenericConstructorPayloadNames =
  assertLeftDiagnosticContains
    "undeclared generic constructor payload diagnostic"
    "constructor payload type parameter 'b' is not declared in data type 'Maybe'"
    (parseSurfaceProgram "data Maybe a = Just b.")

testRejectsUndeclaredNestedConstructorPayloadNames :: IO ()
testRejectsUndeclaredNestedConstructorPayloadNames =
  assertLeftDiagnosticContains
    "undeclared nested constructor payload diagnostic"
    "constructor payload type parameter 'missing' is not declared in data type 'Tree'"
    (parseSurfaceProgram "data Tree a = Branch Tree(missing) Tree(a).")

testRejectsEmptyNamedConstructorFieldApplications :: IO ()
testRejectsEmptyNamedConstructorFieldApplications =
  assertLeftDiagnosticContains
    "empty named constructor field application"
    "expected"
    (parseSurfaceProgram "data Tree a = Branch Tree().")

testRejectsUnterminatedGroupedConstructorFieldTypes :: IO ()
testRejectsUnterminatedGroupedConstructorFieldTypes =
  assertLeftDiagnosticContains
    "unterminated grouped constructor field type"
    "expected ')'"
    (parseSurfaceProgram "data Callback a b = Callback (a -> b.")

testRejectsDataDeclarationWithMalformedPipePlacement :: IO ()
testRejectsDataDeclarationWithMalformedPipePlacement =
  assertLeftDiagnosticContains
    "malformed constructor separator"
    "expected constructor declaration"
    (parseSurfaceProgram "data Maybe = Just Int | .")

testRejectsDataDeclarationMissingTerminator :: IO ()
testRejectsDataDeclarationMissingTerminator =
  assertLeftDiagnosticContains
    "missing data declaration terminator"
    "expected '.'"
    (parseSurfaceProgram "data Maybe = Just Int | Nothing")

testRejectsMalformedParenthesizedListLikePattern :: IO ()
testRejectsMalformedParenthesizedListLikePattern =
  assertLeftDiagnosticContains
    "malformed parenthesized list-like pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case pair { | (left, [right) ]) -> left | _ -> 0 }.")

testRejectsMalformedListPattern :: IO ()
testRejectsMalformedListPattern =
  assertLeftDiagnosticContains
    "malformed list pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case values { | [head tail] -> head }.")

testRejectsMalformedLaterListPattern :: IO ()
testRejectsMalformedLaterListPattern =
  assertLeftDiagnosticContains
    "malformed later list pattern"
    "expected ',' or ']'"
    (parseSurfaceProgram "x = case values { | 0 -> 1 | [head tail] -> head }.")

testRejectsMalformedGuardExpression :: IO ()
testRejectsMalformedGuardExpression =
  assertLeftDiagnosticContains
    "malformed guard expression"
    "expected guard expression"
    (parseSurfaceProgram "x = case subject { | item if -> item }.")

testRejectsMalformedOrPatternAlternative :: IO ()
testRejectsMalformedOrPatternAlternative =
  assertLeftDiagnosticContains
    "malformed or-pattern alternative"
    "expected case pattern"
    (parseSurfaceProgram "x = case subject { | Just item | -> item }.")
