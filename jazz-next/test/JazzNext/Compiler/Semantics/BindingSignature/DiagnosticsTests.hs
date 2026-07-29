{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.DiagnosticsTests
  ( diagnosticTests
  ) where

import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( compileErrors,
    compileExpr,
    compileSource
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject
  )
import JazzNext.Compiler.Semantics.BindingSignature.Shared

diagnosticTests :: [NamedTest]
diagnosticTests =
  [ ("signature type mismatch is rejected", testSignatureTypeMismatch)
    , ("source pipeline rejects generic signature specialization", testSourceRejectsGenericSignatureSpecialization)
    , ("source pipeline rejects generic signature variable collapse", testSourceRejectsGenericSignatureVariableCollapse)
    , ("source pipeline rejects generic named signature specialization", testSourceRejectsGenericNamedSignatureSpecialization)
    , ("signature separated from binding by expression is rejected", testSignatureSeparatedFromBinding)
    , ("signature must match immediate binding name", testSignatureNameMismatch)
    , ("use-before-definition is rejected", testUseBeforeDefinition)
    , ("source pipeline treats capability declarations as signature separators", testSourceRejectsSignatureSeparatedByCapabilityDeclaration)
    , ("source pipeline rejects separated signature", testSourceRejectsSeparatedSignature)
    , ("source pipeline rejects signature name mismatch", testSourceRejectsSignatureNameMismatch)
    , ("source pipeline rejects signature type mismatch", testSourceRejectsSignatureTypeMismatch)
    , ("source pipeline rejects out-of-range width-specific integer literals", testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals)
    , ("source pipeline rejects out-of-range width-specific branch literals", testSourceRejectsOutOfRangeWidthSpecificBranchLiterals)
    , ("source pipeline rejects out-of-range width-specific literal arithmetic", testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic)
    , ("source pipeline rejects out-of-range width-specific section literals", testSourceRejectsOutOfRangeWidthSpecificSectionLiterals)
    , ("source pipeline rejects mixed-width numeric operator signatures", testSourceRejectsMixedWidthNumericOperatorSignatures)
    , ("source pipeline keeps float signatures distinct from integer literals", testSourceRejectsFloatSignatureForIntegerLiteral)
    , ("source pipeline rejects integral fractional literal targets", testSourceRejectsIntegralFractionalLiteralTargets)
    , ("source pipeline rejects tuple signature mismatch", testSourceRejectsTupleSignatureMismatch)
    , ("source pipeline rejects tuple signature arity mismatch", testSourceRejectsTupleSignatureArityMismatch)
    , ("source pipeline rejects forward capability facts for constrained signature", testSourceRejectsForwardCapabilityFactsForConstrainedSignature)
    , ("source pipeline rejects type-application constrained signature argument", testSourceRejectsTypeApplicationConstrainedSignatureArgument)
    , ("source pipeline rejects function constrained signature argument", testSourceRejectsFunctionConstrainedSignatureArgument)
    , ("source pipeline keeps unsupported constrained signature spans on signatures", testSourceRejectsUnsupportedConstrainedSignatureSpans)
    , ("source pipeline rejects list signature mismatch", testSourceRejectsListSignatureMismatch)
    , ("source pipeline rejects unknown named signature type", testSourceRejectsUnknownNamedSignatureType)
    , ("source pipeline rejects named signature type arity mismatch", testSourceRejectsNamedSignatureTypeArityMismatch)
    , ("source pipeline rejects partial named signature type", testSourceRejectsPartialNamedSignatureType)
    , ("source pipeline preserves local type declaration order", testSourcePreservesLocalTypeDeclarationOrder)
    , ("source pipeline rejects unsupported signature surface", testSourceRejectsUnsupportedSignatureSurface)
    , ("source pipeline rejects missing use-site facts for variable constrained signatures", testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures)
    , ("source pipeline rejects ambiguous variable constrained signature use", testSourceRejectsAmbiguousVariableConstrainedSignatureUse)
    , ("source pipeline rejects unsupported variable constrained signature contract", testSourceRejectsUnsupportedVariableConstrainedSignatureContract)
    , ("source pipeline rejects constrained signature surface with E2009", testSourceRejectsConstrainedSignatureSurface)
    , ("signature mismatch keeps declared type for downstream checks", testSignatureMismatchKeepsDeclaredTypeDownstream)
    , ("mismatched pending signature does not monomorphize following binding", testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding)
  ]

testSourceRejectsGenericSignatureSpecialization :: IO ()
testSourceRejectsGenericSignatureSpecialization =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    bad :: a -> a.
    bad = \\(x) -> 1.
    """
    "declared as"

testSourceRejectsGenericSignatureVariableCollapse :: IO ()
testSourceRejectsGenericSignatureVariableCollapse =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    bad :: a -> b -> a.
    bad = \\(x, y) -> y.
    """
    "declared as"

testSourceRejectsGenericNamedSignatureSpecialization :: IO ()
testSourceRejectsGenericNamedSignatureSpecialization =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    bad :: Box(a) -> Box(a).
    bad = \\(x) -> Box 1.
    """
    "declared as Box"

testSignatureTypeMismatch :: IO ()
testSignatureTypeMismatch = do
  result <- compileExpr defaultWarningSettings signatureTypeMismatchProgram
  assertSingleDiagnosticCode
    "signature type mismatch error"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature type mismatch subject"
    "x"
    (compileErrors result)

testSignatureSeparatedFromBinding :: IO ()
testSignatureSeparatedFromBinding = do
  result <- compileExpr defaultWarningSettings separatedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must be immediately followed by a matching binding"
    (compileErrors result)

testSignatureNameMismatch :: IO ()
testSignatureNameMismatch = do
  result <- compileExpr defaultWarningSettings mismatchedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must annotate the next binding with the same name"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature mismatch subject"
    "x"
    (compileErrors result)

testUseBeforeDefinition :: IO ()
testUseBeforeDefinition = do
  result <- compileExpr defaultWarningSettings useBeforeDefinitionProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'x'"
    (compileErrors result)

testSourceRejectsSignatureSeparatedByCapabilityDeclaration :: IO ()
testSourceRejectsSignatureSeparatedByCapabilityDeclaration =
  assertSourceErrorContains """
  x :: Int.
  class Eq(a) { }.
  x = 1.
  """ "E1002"

testSourceRejectsSeparatedSignature :: IO ()
testSourceRejectsSeparatedSignature =
  assertSourceErrorContains """
  x :: Int.
  1.
  x = 2.
  """ "E1002"

testSourceRejectsSignatureNameMismatch :: IO ()
testSourceRejectsSignatureNameMismatch =
  assertSourceErrorContains """
  x :: Int.
  y = 2.
  """ "E1003"

testSourceRejectsSignatureTypeMismatch :: IO ()
testSourceRejectsSignatureTypeMismatch = do
  result <- compileSource defaultWarningSettings """
  x :: Int.
  x = True.
  """
  assertSingleDiagnosticCode
    "source signature type mismatch code"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "source signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "source signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "source signature type mismatch subject"
    "x"
    (compileErrors result)

testSourceRejectsUnknownNamedSignatureType :: IO ()
testSourceRejectsUnknownNamedSignatureType =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    candidate :: Unknown.
    candidate = 1.
    """
    "unknown named type 'Unknown'"

testSourceRejectsNamedSignatureTypeArityMismatch :: IO ()
testSourceRejectsNamedSignatureTypeArityMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    candidate :: Box(Int, Bool).
    candidate = Box 1.
    """
    "type 'Box' expects 1 argument(s), found 2"

testSourceRejectsPartialNamedSignatureType :: IO ()
testSourceRejectsPartialNamedSignatureType =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    candidate :: Box.
    candidate = Box 1.
    """
    "type 'Box' expects 1 argument(s), found 0"

testSourcePreservesLocalTypeDeclarationOrder :: IO ()
testSourcePreservesLocalTypeDeclarationOrder =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    candidate :: Box(Int).
    candidate = 1.
    data Box a = Box a.
    """
    "unknown named type 'Box'"

testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals = do
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = 300.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: Int8.
  x = 128.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: UInt64.
  x = 18446744073709551616.
  """ "E2005"
  assertSourceSingleErrorContains """
  xs :: [UInt8].
  xs = [1, 300].
  """ "E2005"

testSourceRejectsOutOfRangeWidthSpecificBranchLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificBranchLiterals = do
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = if True then 1 else 300.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = case 0 { | 0 -> 1 | _ -> 300 }.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: (UInt8, UInt8).
  x = if True then (1, 1) else (2, 300).
  """ "E2005"
  assertSourceSingleErrorContains """
  f :: UInt8 -> UInt8.
  f = if True then (\\(x) -> 1) else (\\(x) -> 300).
  """ "E2005"

testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic :: IO ()
testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic = do
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = 1 + 300.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = 200 + 100.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = 0 - 1.
  """ "E2005"
  assertSourceSingleErrorContains """
  x :: UInt8.
  x = 16 * 16.
  """ "E2005"

testSourceRejectsOutOfRangeWidthSpecificSectionLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificSectionLiterals = do
  assertSourceSingleErrorContains """
  inc :: UInt8 -> UInt8.
  inc = (+ 300).
  """ "E2005"
  assertSourceSingleErrorContains """
  inc :: UInt8 -> UInt8.
  inc = (300 +).
  """ "E2005"

testSourceRejectsMixedWidthNumericOperatorSignatures :: IO ()
testSourceRejectsMixedWidthNumericOperatorSignatures =
  assertSourceSingleErrorContains """
  add :: Int8 -> UInt8 -> Int8.
  add = (+).
  """ "E2005"

testSourceRejectsFloatSignatureForIntegerLiteral :: IO ()
testSourceRejectsFloatSignatureForIntegerLiteral =
  assertSourceSingleErrorContains """
  x :: Float64.
  x = 1.
  """ "E2005"

testSourceRejectsIntegralFractionalLiteralTargets :: IO ()
testSourceRejectsIntegralFractionalLiteralTargets = do
  assertSourceSingleErrorContains """
  x :: Int.
  x = 1.5.
  """ "E2005"

testSourceRejectsTupleSignatureMismatch :: IO ()
testSourceRejectsTupleSignatureMismatch = do
  result <- compileSource defaultWarningSettings """
  pair :: (Int, Bool).
  pair = (1, 2).
  """
  assertSingleDiagnosticCode
    "source tuple signature mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsTupleSignatureArityMismatch :: IO ()
testSourceRejectsTupleSignatureArityMismatch = do
  result <- compileSource defaultWarningSettings """
  pair :: (Int, Bool).
  pair = (1, True, 3).
  """
  assertSingleDiagnosticCode
    "source tuple signature arity mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsForwardCapabilityFactsForConstrainedSignature :: IO ()
testSourceRejectsForwardCapabilityFactsForConstrainedSignature =
  assertSourceSingleErrorContainsWithoutPrelude """
  x :: @{Eq(Int)}: Int.
  x = 1.
  class Eq(a) { }.
  impl Eq(Int) { }.
  """ "missing class declaration 'Eq'"

testSourceRejectsTypeApplicationConstrainedSignatureArgument :: IO ()
testSourceRejectsTypeApplicationConstrainedSignatureArgument =
  assertSourceSingleErrorContains """
  x :: @{Eq(Maybe(Int))}: Int.
  x = 1.
  """ "E2009"

testSourceRejectsFunctionConstrainedSignatureArgument :: IO ()
testSourceRejectsFunctionConstrainedSignatureArgument =
  assertSourceSingleErrorContains """
  x :: @{Eq(Int -> Int)}: Int.
  x = 1.
  """ "E2009"

testSourceRejectsUnsupportedConstrainedSignatureSpans :: IO ()
testSourceRejectsUnsupportedConstrainedSignatureSpans = do
  let assertSignatureSpan signatureSource =
        -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
        assertSourceSingleErrorCodeAndPrimarySpan
          ("prefix = 0.\n" <> signatureSource <> "\n")
          "E2009"
          (SourceSpan 2 1)
  assertSignatureSpan """
  x :: @{Unknown(Int)}: Int.
  x = 1.
  """
  assertSignatureSpan """
  x :: @{Eq(Int, Bool)}: Int.
  x = 1.
  """
  assertSignatureSpan """
  x :: @{Eq(Maybe(Int))}: Int.
  x = 1.
  """
  assertSignatureSpan """
  x :: @{Eq(Int -> Int)}: Int.
  x = 1.
  """
  assertSignatureSpan """
  f :: @{Eq(a), Eq(a)}: a -> a.
  f = \\(x) -> x.
  """
  assertSignatureSpan """
  f :: @{Eq(a)}: Int -> Int.
  f = \\(x) -> x.
  """

testSourceRejectsListSignatureMismatch :: IO ()
testSourceRejectsListSignatureMismatch = do
  result <- compileSource defaultWarningSettings """
  x :: [Bool].
  x = [1].
  """
  assertSingleDiagnosticCode
    "source list signature mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsUnsupportedSignatureSurface :: IO ()
testSourceRejectsUnsupportedSignatureSurface =
  assertSourceSingleErrorContains """
  x :: forall a.
  x = 1.
  """ "E2009"

testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures :: IO ()
testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    id :: @{Eq(a)}: a -> a.
    id = \\(x) -> x.
    ok = id 1.
    bad = id True.
    """
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsAmbiguousVariableConstrainedSignatureUse :: IO ()
testSourceRejectsAmbiguousVariableConstrainedSignatureUse =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    id :: @{Eq(a)}: a -> a.
    id = \\(x) -> x.
    ambiguous = id [].
    """
    "ambiguous/defaulting explicit constraint"

testSourceRejectsUnsupportedVariableConstrainedSignatureContract :: IO ()
testSourceRejectsUnsupportedVariableConstrainedSignatureContract = do
  result <- compileSource defaultWarningSettings """
  f :: @{Eq(a)}: b -> b.
  f = \\(x) -> x.
  """
  assertSingleDiagnosticCode
    "source unsupported variable constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unsupported variable constrained signature contract"
    "type-variable constrained signatures require every constrained variable to appear in the signature body"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unsupported variable constrained signature payload"
    "@{Eq(a)}: b -> b"
    (compileErrors result)

testSourceRejectsConstrainedSignatureSurface :: IO ()
testSourceRejectsConstrainedSignatureSurface = do
  result <- compileSource defaultWarningSettings """
  f :: @{Eq(a), Ord(b)}: a -> c.
  f = \\(x) -> x.
  """
  assertSingleDiagnosticCode
    "source constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source constrained signature payload"
    "@{Eq(a), Ord(b)}: a -> c"
    (compileErrors result)

testSignatureMismatchKeepsDeclaredTypeDownstream :: IO ()
testSignatureMismatchKeepsDeclaredTypeDownstream =
  assertSourceSingleErrorContains """
  x :: Int.
  x = True.
  y = x + 1.
  """ "E2005"

testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding :: IO ()
testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding =
  assertSourceSingleErrorContains """
  x :: Int.
  id = \\(candidate) -> candidate.
  intValue = id 1.
  boolValue = id True.
  """ "E1003"
