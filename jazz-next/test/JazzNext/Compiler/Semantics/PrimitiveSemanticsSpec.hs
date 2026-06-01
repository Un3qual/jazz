{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
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
    assertSingleDiagnosticContains,
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
    ("source pipeline accepts structural list equality", testSourcePipelineAcceptsStructuralListEquality),
    ("source pipeline accepts structural tuple equality", testSourcePipelineAcceptsStructuralTupleEquality),
    ("source pipeline accepts structural ADT equality", testSourcePipelineAcceptsStructuralAdtEquality),
    ("source pipeline accepts structural equality sections", testSourcePipelineAcceptsStructuralEqualitySections),
    ("source pipeline rejects structural equality with function elements", testSourcePipelineRejectsStructuralFunctionEquality),
    ("source pipeline rejects structural ADT equality with function payloads", testSourcePipelineRejectsStructuralAdtFunctionEquality),
    ("source pipeline rejects structural ADT equality for partial constructors", testSourcePipelineRejectsStructuralAdtPartialConstructorEquality),
    ("source pipeline rejects structural ADT equality across different types", testSourcePipelineRejectsStructuralAdtTypeMismatch),
    ("source pipeline preserves numeric width through left integer literal arithmetic", testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral),
    ("source pipeline preserves numeric width through left integer literal section", testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection),
    ("source pipeline preserves numeric width through right integer literal section", testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection),
    ("source pipeline rejects left arithmetic section with non-numeric operand", testSourcePipelineRejectsLeftArithmeticSectionTypeMismatch),
    ("source pipeline rejects right arithmetic section with non-numeric operand", testSourcePipelineRejectsRightArithmeticSectionTypeMismatch),
    ("source pipeline rejects equality section mismatched application", testSourcePipelineRejectsEqualitySectionTypeMismatch),
    ("source pipeline rejects deferred equality section constrained to unresolved list", testSourcePipelineRejectsDeferredEqualitySectionUnresolvedListConstraint),
    ("source pipeline rejects unsupported section operator", testSourcePipelineRejectsUnsupportedSectionOperator),
    ("source pipeline accepts bare operator value", testSourcePipelineAcceptsBareOperatorValue),
    ("source pipeline accepts bare operator value application", testSourcePipelineAcceptsBareOperatorValueApplication),
    ("source pipeline accepts explicit partial application of bare operator value", testSourcePipelineAcceptsExplicitPartialOperatorApplication),
    ("source pipeline rejects mixed-type list literals", testSourcePipelineRejectsMixedTypeListLiteral),
    ("source pipeline accepts target-named integer conversions", testSourcePipelineAcceptsTargetNamedIntegerConversions),
    ("source pipeline accepts target-named float conversions", testSourcePipelineAcceptsTargetNamedFloatConversions),
    ("source pipeline accepts Float64 fractional literal defaults", testSourcePipelineAcceptsFloat64FractionalLiteralDefaults),
    ("source pipeline accepts explicitly targeted Float16 and Float32 fractional literals", testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals),
    ("source pipeline accepts same-width Float64 arithmetic", testSourcePipelineAcceptsSameWidthFloat64Arithmetic),
    ("source pipeline accepts same-width Float64 operator values", testSourcePipelineAcceptsSameWidthFloat64OperatorValues),
    ("source pipeline accepts same-width Float16 and Float32 arithmetic", testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic),
    ("source pipeline accepts same-width Float64 comparison and equality", testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality),
    ("source pipeline accepts same-width Float16 and Float32 comparison and equality", testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality),
    ("source pipeline accepts same-width Float64 comparison/equality operator values", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues),
    ("source pipeline accepts same-width Float64 comparison/equality sections", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections),
    ("source pipeline rejects mixed-width float comparison and equality", testSourcePipelineRejectsMixedWidthFloatComparisonEquality),
    ("source pipeline rejects implicit Float16 and Float32 comparison and equality", testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality),
    ("source pipeline rejects implicit integer and fractional literal mixing", testSourcePipelineRejectsImplicitIntegerFractionalMixing),
    ("source pipeline rejects mixed-width float arithmetic", testSourcePipelineRejectsMixedWidthFloatArithmetic),
    ("source pipeline rejects mixed-width and implicit Float16/Float32 arithmetic", testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic),
    ("source pipeline rejects out-of-range literal conversions", testSourcePipelineRejectsOutOfRangeLiteralConversions),
    ("source pipeline rejects non-integral fractional literal conversions", testSourcePipelineRejectsNonIntegralFractionalLiteralConversions),
    ("source pipeline rejects rounded non-integral fractional literal conversions", testSourcePipelineRejectsRoundedNonIntegralFractionalLiteralConversions),
    ("source pipeline accepts integral-boundary fractional literal conversions", testSourcePipelineAcceptsIntegralBoundaryFractionalLiteralConversions),
    ("source pipeline rejects out-of-range float-target literal conversions", testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions),
    ("source pipeline rejects source-exact float-target literal overflow", testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow),
    ("source pipeline rejects source-exact negative float-target literal overflow", testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow),
    ("source pipeline rejects dollar-applied fractional literal conversions", testSourcePipelineRejectsDollarAppliedFractionalLiteralConversions),
    ("source pipeline rejects typed prelude alias literal overflow", testSourcePipelineRejectsTypedPreludeAliasLiteralOverflow),
    ("source pipeline ignores conversion literal checks for shadowed names", testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames),
    ("source pipeline freshens prelude conversion aliases", testSourcePipelineFreshensPreludeConversionAliases),
    ("source pipeline keeps locally shadowed kernel aliases ordinary", testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary),
    ("source pipeline rejects non-numeric conversion source", testSourcePipelineRejectsNonNumericConversionSource)
  ]

testAcceptsArithmeticIntOperands :: IO ()
testAcceptsArithmeticIntOperands = do
  result <- compileExpr defaultWarningSettings arithmeticProgram
  assertEqual "compile errors" [] (compileErrors result)

testAcceptsIntEquality :: IO ()
testAcceptsIntEquality = do
  result <- compileExpr defaultWarningSettings intEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)

testAcceptsBoolEquality :: IO ()
testAcceptsBoolEquality = do
  result <- compileExpr defaultWarningSettings boolEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)

testRejectsEqualityTypeMismatch :: IO ()
testRejectsEqualityTypeMismatch = do
  result <- compileExpr defaultWarningSettings equalityTypeMismatchProgram
  assertSingleDiagnosticContains
    "strict equality type error"
    "E2004"
    (compileErrors result)

testRejectsInequalityTypeMismatch :: IO ()
testRejectsInequalityTypeMismatch = do
  result <- compileExpr defaultWarningSettings inequalityTypeMismatchProgram
  assertSingleDiagnosticContains
    "strict inequality type error"
    "E2004"
    (compileErrors result)

testRejectsComparisonTypeMismatch :: IO ()
testRejectsComparisonTypeMismatch = do
  result <- compileExpr defaultWarningSettings comparisonTypeMismatchProgram
  assertSingleDiagnosticContains
    "comparison type error"
    "E2003"
    (compileErrors result)

testRejectsArithmeticTypeMismatch :: IO ()
testRejectsArithmeticTypeMismatch = do
  result <- compileExpr defaultWarningSettings arithmeticTypeMismatchProgram
  assertSingleDiagnosticContains
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

testSourcePipelineAcceptsStructuralListEquality :: IO ()
testSourcePipelineAcceptsStructuralListEquality =
  assertCompiles
    "same = [1, 2] == [1, 2].\nnested = [[True], [False]] != [[True], [True]]."

testSourcePipelineAcceptsStructuralTupleEquality :: IO ()
testSourcePipelineAcceptsStructuralTupleEquality =
  assertCompiles
    "same = (1, True) == (1, True).\nnested = (1, (True, 2)) != (1, (True, 3))."

testSourcePipelineAcceptsStructuralAdtEquality :: IO ()
testSourcePipelineAcceptsStructuralAdtEquality = do
  assertCompiles
    "data Maybe = Nothing | Just value.\nleft = Just 1.\nright = Just 1.\nsame = left == right.\ndifferent = left != Nothing.\neqOp = (==).\nsameViaOp = eqOp left right.\nsameViaLeftSection = (left ==) right.\nsameViaRightSection = (== right) left."
  assertCompiles
    "data Box a = Box a.\nleft = Box [1, 2].\nright = Box [1, 2].\nsame = left == right."

testSourcePipelineAcceptsStructuralEqualitySections :: IO ()
testSourcePipelineAcceptsStructuralEqualitySections =
  assertCompiles
    "listEq = (== [1, 2]) [1, 2].\ntupleNe = ((1, True) !=) (1, False)."

testSourcePipelineRejectsStructuralFunctionEquality :: IO ()
testSourcePipelineRejectsStructuralFunctionEquality = do
  result <- compileSource defaultWarningSettings "f = \\(x) -> x.\nx = [f] == [f]."
  assertSingleDiagnosticContains
    "function-valued structural equality code"
    "E2004"
    (compileErrors result)
  assertSingleDiagnosticContains
    "function-valued structural equality summary"
    "lists and tuples containing equality-supported elements"
    (compileErrors result)

testSourcePipelineRejectsStructuralAdtFunctionEquality :: IO ()
testSourcePipelineRejectsStructuralAdtFunctionEquality = do
  result <- compileSource defaultWarningSettings "data Box a = Box a.\nf = \\(x) -> x.\nleft = Box f.\nright = Box f.\nx = left == right."
  assertSingleDiagnosticContains
    "function-valued ADT equality code"
    "E2004"
    (compileErrors result)
  assertSingleDiagnosticContains
    "function-valued ADT equality summary"
    "ADTs containing equality-supported constructor payloads"
    (compileErrors result)

testSourcePipelineRejectsStructuralAdtPartialConstructorEquality :: IO ()
testSourcePipelineRejectsStructuralAdtPartialConstructorEquality =
  assertCompileError
    "data Box a = Box a.\nx = Box == Box."
    "partial constructor equality"
    "E2004"

testSourcePipelineRejectsStructuralAdtTypeMismatch :: IO ()
testSourcePipelineRejectsStructuralAdtTypeMismatch =
  assertCompileError
    "data Lefty = Lefty.\ndata Righty = Righty.\nx = Lefty == Righty."
    "different ADT type equality"
    "E2004"

testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral :: IO ()
testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral =
  assertCompiles "y :: UInt8.\ny = 2.\nx = 1 + y.\nz :: UInt8.\nz = x."

testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection :: IO ()
testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection =
  assertCompiles "y :: UInt8.\ny = 2.\nf = (1 +).\nz :: UInt8.\nz = f y."

testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection :: IO ()
testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection =
  assertCompiles "y :: UInt8.\ny = 2.\nf = (+ 1).\nz :: UInt8.\nz = f y."

testSourcePipelineRejectsLeftArithmeticSectionTypeMismatch :: IO ()
testSourcePipelineRejectsLeftArithmeticSectionTypeMismatch = do
  result <- compileSource defaultWarningSettings "x = (True +) 1."
  assertSingleDiagnosticContains
    "left arithmetic section operand mismatch code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "left arithmetic section operand mismatch summary"
    "requires a numeric operand, found Bool"
    (compileErrors result)

testSourcePipelineRejectsRightArithmeticSectionTypeMismatch :: IO ()
testSourcePipelineRejectsRightArithmeticSectionTypeMismatch = do
  result <- compileSource defaultWarningSettings "x = (+ True) 1."
  assertSingleDiagnosticContains
    "right arithmetic section operand mismatch code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "right arithmetic section operand mismatch summary"
    "requires a numeric operand, found Bool"
    (compileErrors result)

testSourcePipelineRejectsEqualitySectionTypeMismatch :: IO ()
testSourcePipelineRejectsEqualitySectionTypeMismatch =
  assertCompileError
    "x = (True ==) 1."
    "equality section operand mismatch"
    "E2006"

testSourcePipelineRejectsDeferredEqualitySectionUnresolvedListConstraint :: IO ()
testSourcePipelineRejectsDeferredEqualitySectionUnresolvedListConstraint =
  assertCompileErrorWithBundledPrelude
    "x = (hd [] ==) []."
    "deferred equality section must still reject unresolved list equality"
    "E2006"

testSourcePipelineRejectsUnsupportedSectionOperator :: IO ()
testSourcePipelineRejectsUnsupportedSectionOperator =
  assertCompileError
    "x = ($ 1)."
    "unsupported section operator"
    "E2008"

testSourcePipelineAcceptsBareOperatorValue :: IO ()
testSourcePipelineAcceptsBareOperatorValue =
  assertCompiles "x = (+)."

testSourcePipelineAcceptsBareOperatorValueApplication :: IO ()
testSourcePipelineAcceptsBareOperatorValueApplication =
  assertCompiles "x = (+) 1 2."

testSourcePipelineAcceptsExplicitPartialOperatorApplication :: IO ()
testSourcePipelineAcceptsExplicitPartialOperatorApplication =
  assertCompiles "x = ((+) 1) 2."

testSourcePipelineRejectsMixedTypeListLiteral :: IO ()
testSourcePipelineRejectsMixedTypeListLiteral =
  assertCompileError
    "x = [1, True]."
    "list literal element mismatch"
    "E2007"

testSourcePipelineAcceptsTargetNamedIntegerConversions :: IO ()
testSourcePipelineAcceptsTargetNamedIntegerConversions =
  assertCompilesWithBundledPrelude "x :: UInt8.\nx = toUInt8 255.\ny :: Int16.\ny = toInt16 x."

testSourcePipelineAcceptsTargetNamedFloatConversions :: IO ()
testSourcePipelineAcceptsTargetNamedFloatConversions =
  assertCompilesWithBundledPrelude "x :: Float64.\nx = toFloat64 1."

testSourcePipelineAcceptsFloat64FractionalLiteralDefaults :: IO ()
testSourcePipelineAcceptsFloat64FractionalLiteralDefaults =
  assertCompiles "x = 1.5.\ny :: Float64.\ny = x."

testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals :: IO ()
testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals =
  assertCompiles
    "x16 :: Float16.\nx16 = 1.5.\ny16 :: Float16.\ny16 = x16.\nx32 :: Float32.\nx32 = 2.25.\ny32 :: Float32.\ny32 = x32."

testSourcePipelineAcceptsSameWidthFloat64Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat64Arithmetic =
  assertCompilesWithBundledPrelude
    "x :: Float64.\nx = ((1.5 + 2.25) - toFloat64 1) * (6.0 / 2.0)."

testSourcePipelineAcceptsSameWidthFloat64OperatorValues :: IO ()
testSourcePipelineAcceptsSameWidthFloat64OperatorValues =
  assertCompilesWithBundledPrelude
    "x :: Float64.\nx = (+) (toFloat64 1) (toFloat64 2)."

testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic =
  assertCompilesWithBundledPrelude
    "a16 :: Float16.\na16 = toFloat16 1.\nb16 :: Float16.\nb16 = toFloat16 2.\nc16 :: Float16.\nc16 = toFloat16 6.\nd16 :: Float16.\nd16 = toFloat16 3.\nx16 :: Float16.\nx16 = ((a16 + b16) * (c16 / d16)) - b16.\na32 :: Float32.\na32 = toFloat32 1.\nb32 :: Float32.\nb32 = toFloat32 2.\nc32 :: Float32.\nc32 = toFloat32 6.\nd32 :: Float32.\nd32 = toFloat32 3.\nx32 :: Float32.\nx32 = ((a32 + b32) * (c32 / d32)) - b32."

testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality =
  assertCompiles
    "lt = 1.5 < 2.0.\nle = 2.0 <= 2.0.\ngt = 3.0 > 2.0.\nge = 3.0 >= 3.0.\neq = 2.0 == 2.0.\nne = 2.0 != 3.0."

testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality :: IO ()
testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality =
  assertCompilesWithBundledPrelude
    "a16 :: Float16.\na16 = toFloat16 1.\nb16 :: Float16.\nb16 = toFloat16 2.\nlt16 = a16 < b16.\nle16 = a16 <= a16.\ngt16 = b16 > a16.\nge16 = b16 >= b16.\neq16 = a16 == a16.\nne16 = a16 != b16.\na32 :: Float32.\na32 = toFloat32 1.\nb32 :: Float32.\nb32 = toFloat32 2.\nlt32 = a32 < b32.\nle32 = a32 <= a32.\ngt32 = b32 > a32.\nge32 = b32 >= b32.\neq32 = a32 == a32.\nne32 = a32 != b32."

testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues =
  assertCompiles
    "lt = (<) 1.5 2.0.\nle = (<=) 2.0 2.0.\ngt = (>) 3.0 2.0.\nge = (>=) 3.0 3.0.\neq = (==) 2.0 2.0.\nne = (!=) 2.0 3.0."

testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections =
  assertCompiles
    "lt = (1.5 <) 2.0.\nle = (2.0 <=) 2.0.\ngt = (> 2.0) 3.0.\nge = (>= 3.0) 3.0.\neq = (2.0 ==) 2.0.\nne = (!= 3.0) 2.0."

testSourcePipelineRejectsMixedWidthFloatComparisonEquality :: IO ()
testSourcePipelineRejectsMixedWidthFloatComparisonEquality = do
  assertCompileError
    "x = 1 == 1.5."
    "mixed Int/Float64 equality"
    "E2004"
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nright :: Float32.\nright = toFloat32 1.\nx = left == right."
    "mixed Float16/Float32 equality"
    "E2004"
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nright :: Float32.\nright = toFloat32 2.\nx = left < right."
    "mixed Float16/Float32 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nright :: Float64.\nright = toFloat64 1.\nx = left < right."
    "mixed Float16/Float64 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left :: Float32.\nleft = toFloat32 1.\nright :: Float64.\nright = toFloat64 1.\nx = left == right."
    "mixed Float32/Float64 equality"
    "E2004"

testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality :: IO ()
testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality = do
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nx = left < 1."
    "implicit integer-to-Float16 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left :: Float32.\nleft = toFloat32 1.\nx = left == 1."
    "implicit integer-to-Float32 equality"
    "E2004"

testSourcePipelineRejectsImplicitIntegerFractionalMixing :: IO ()
testSourcePipelineRejectsImplicitIntegerFractionalMixing =
  assertCompileError
    "x = 1 + 1.5."
    "mixed integer/fractional operator"
    "E2003"

testSourcePipelineRejectsMixedWidthFloatArithmetic :: IO ()
testSourcePipelineRejectsMixedWidthFloatArithmetic =
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nx :: Float64.\nx = left + 2.0."
    "mixed-width float arithmetic"
    "E2003"

testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic :: IO ()
testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic = do
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nright :: Float32.\nright = toFloat32 2.\nx = left + right."
    "mixed Float16/Float32 arithmetic"
    "E2003"
  assertCompileError
    "left :: Float16.\nleft = 1.5.\nright :: Float32.\nright = 2.25.\nx = left + right."
    "mixed targeted Float16/Float32 arithmetic"
    "E2003"
  assertCompileError
    "left :: Float16.\nleft = 1.5.\nx = left + 1.25."
    "implicit fractional literal-to-Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left :: Float16.\nleft = toFloat16 1.\nx = left + 1."
    "implicit integer-to-Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left :: Float32.\nleft = toFloat32 1.\nx = left + 1."
    "implicit integer-to-Float32 arithmetic"
    "E2003"

testSourcePipelineRejectsOutOfRangeLiteralConversions :: IO ()
testSourcePipelineRejectsOutOfRangeLiteralConversions =
  assertCompileErrorWithBundledPrelude
    "x = toUInt8 256."
    "out-of-range literal conversion"
    "E2006"

testSourcePipelineRejectsNonIntegralFractionalLiteralConversions :: IO ()
testSourcePipelineRejectsNonIntegralFractionalLiteralConversions =
  assertCompileErrorWithBundledPrelude
    "x = toInt8 1.5."
    "non-integral fractional literal conversion"
    "E2006"

testSourcePipelineRejectsRoundedNonIntegralFractionalLiteralConversions :: IO ()
testSourcePipelineRejectsRoundedNonIntegralFractionalLiteralConversions =
  assertCompileErrorWithBundledPrelude
    "x = toInt8 0.99999999999999999."
    "rounded non-integral fractional literal conversion"
    "E2006"

testSourcePipelineAcceptsIntegralBoundaryFractionalLiteralConversions :: IO ()
testSourcePipelineAcceptsIntegralBoundaryFractionalLiteralConversions =
  assertCompilesWithBundledPrelude
    "x = toInt64 9223372036854775807.0.\ny = toUInt64 18446744073709551615.0."

testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions :: IO ()
testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions = do
  assertCompileErrorWithBundledPrelude
    "x = toFloat16 70000."
    "out-of-range float-target literal conversion"
    "E2006"
  assertCompileError
    "x :: Float16.\nx = 70000.0."
    "out-of-range Float16 literal target"
    "E2006"
  assertCompileError
    "x :: Float32.\nx = 1000000000000000000000000000000000000000.0."
    "out-of-range Float32 literal target"
    "E2006"

testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow :: IO ()
testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow = do
  assertCompileErrorWithBundledPrelude
    "x = toFloat16 65504.000000000000000001."
    "source-exact float-target literal overflow"
    "E2006"
  assertCompileError
    "x :: Float16.\nx = 65504.000000000000000001."
    "source-exact Float16 literal target overflow"
    "E2006"

testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow :: IO ()
testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow = do
  result <- compileExpr defaultWarningSettings sourceExactNegativeFloatTargetOverflowProgram
  assertSingleDiagnosticContains
    "source-exact negative float-target literal overflow"
    "E2006"
    (compileErrors result)

testSourcePipelineRejectsDollarAppliedFractionalLiteralConversions :: IO ()
testSourcePipelineRejectsDollarAppliedFractionalLiteralConversions = do
  assertCompileErrorWithBundledPrelude
    "x = toInt8 $ 1.5."
    "dollar-applied non-integral fractional literal conversion"
    "E2006"
  assertCompileErrorWithBundledPrelude
    "x = toFloat16 $ 65504.000000000000000001."
    "dollar-applied source-exact float-target literal overflow"
    "E2006"

testSourcePipelineRejectsTypedPreludeAliasLiteralOverflow :: IO ()
testSourcePipelineRejectsTypedPreludeAliasLiteralOverflow =
  assertCompileErrorWithPrelude
    "toFloat16 :: Float -> Float16.\ntoFloat16 = __kernel_toFloat16."
    "x = toFloat16 65504.000000000000000001."
    "typed prelude alias source-exact literal overflow"
    "E2006"

testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames :: IO ()
testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames =
  assertCompiles "toUInt8 = \\(x) -> x.\nx = toUInt8 256."

testSourcePipelineFreshensPreludeConversionAliases :: IO ()
testSourcePipelineFreshensPreludeConversionAliases =
  assertCompilesWithBundledPrelude
    "a :: Int16.\na = toInt16 1.\nb :: UInt8.\nb = toUInt8 a.\nc :: UInt16.\nc = toUInt16 2.\nd :: UInt8.\nd = toUInt8 c."

testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary :: IO ()
testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary =
  assertCompilesWithBundledPrelude
    "x = {\n__kernel_toUInt8 = \\(value) -> value.\nalias = __kernel_toUInt8.\nalias 256.\n}."

testSourcePipelineRejectsNonNumericConversionSource :: IO ()
testSourcePipelineRejectsNonNumericConversionSource =
  assertCompileErrorWithBundledPrelude
    "flag = True.\nx = toInt8 flag."
    "non-numeric conversion argument"
    "E2006"

assertCompiles :: String -> IO ()
assertCompiles source = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)

assertCompilesWithBundledPrelude :: String -> IO ()
assertCompilesWithBundledPrelude source = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)

assertCompileError :: String -> String -> String -> IO ()
assertCompileError source failureLabel errorCode = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

assertCompileErrorWithBundledPrelude :: String -> String -> String -> IO ()
assertCompileErrorWithBundledPrelude source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

assertCompileErrorWithPrelude :: String -> String -> String -> String -> IO ()
assertCompileErrorWithPrelude preludeSource source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just (Text.pack preludeSource)) (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

mkProgram :: Expr -> Expr
mkProgram expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

arithmeticProgram :: Expr
arithmeticProgram =
  mkProgram
    ( EBinary
        "+"
        (EBinary "*" (ELit (LInt 7)) (ELit (LInt 6)))
        (EBinary "/" (ELit (LInt 8)) (ELit (LInt 2)))
    )

sourceExactNegativeFloatTargetOverflowProgram :: Expr
sourceExactNegativeFloatTargetOverflowProgram =
  mkProgram
    ( EApply
        (EVar "__kernel_toFloat16")
        (ELit (LFloat (-65504.0) (mkFractionalLiteralSource (-65504) 1 18)))
    )

intEqualityProgram :: Expr
intEqualityProgram =
  mkProgram (EBinary "==" (ELit (LInt 1)) (ELit (LInt 1)))

boolEqualityProgram :: Expr
boolEqualityProgram =
  mkProgram (EBinary "==" (ELit (LBool True)) (ELit (LBool False)))

equalityTypeMismatchProgram :: Expr
equalityTypeMismatchProgram =
  mkProgram (EBinary "==" (ELit (LInt 1)) (ELit (LBool True)))

inequalityTypeMismatchProgram :: Expr
inequalityTypeMismatchProgram =
  mkProgram (EBinary "!=" (ELit (LBool True)) (ELit (LInt 1)))

comparisonTypeMismatchProgram :: Expr
comparisonTypeMismatchProgram =
  mkProgram (EBinary "<" (ELit (LBool True)) (ELit (LBool False)))

arithmeticTypeMismatchProgram :: Expr
arithmeticTypeMismatchProgram =
  mkProgram (EBinary "+" (ELit (LInt 1)) (ELit (LBool True)))
