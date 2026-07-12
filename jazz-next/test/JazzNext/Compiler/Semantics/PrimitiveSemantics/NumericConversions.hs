{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.PrimitiveSemantics.NumericConversions
  ( integerWidthTests,
    numericConversionTests
  )
where

import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCompileError,
    assertCompileErrorWithBundledPrelude,
    assertCompileErrorWithPrelude,
    assertCompiles,
    assertCompilesWithBundledPrelude,
    mkProgram
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertSingleDiagnosticContains
  )

integerWidthTests :: [NamedTest]
integerWidthTests =
  [ ("source pipeline preserves numeric width through left integer literal arithmetic", testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral),
    ("source pipeline preserves numeric width through left integer literal section", testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection),
    ("source pipeline preserves numeric width through right integer literal section", testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection),
    ("source pipeline rejects left arithmetic section with non-numeric operand", testSourcePipelineRejectsLeftArithmeticSectionTypeMismatch),
    ("source pipeline rejects right arithmetic section with non-numeric operand", testSourcePipelineRejectsRightArithmeticSectionTypeMismatch)
  ]

numericConversionTests :: [NamedTest]
numericConversionTests =
  [ ("source pipeline accepts target-named integer conversions", testSourcePipelineAcceptsTargetNamedIntegerConversions),
    ("source pipeline accepts target-named float conversions", testSourcePipelineAcceptsTargetNamedFloatConversions),
    ("source pipeline accepts default prelude conversion aliases", testSourcePipelineAcceptsDefaultPreludeConversionAliases),
    ("source pipeline accepts Float64 fractional literal defaults", testSourcePipelineAcceptsFloat64FractionalLiteralDefaults),
    ("source pipeline accepts explicitly targeted Float16 and Float32 fractional literals", testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals),
    ("source pipeline accepts suffixed Float16/Float32/Float64 fractional literal arithmetic", testSourcePipelineAcceptsSuffixedFractionalLiteralArithmetic),
    ("source pipeline accepts same-width Float64 arithmetic", testSourcePipelineAcceptsSameWidthFloat64Arithmetic),
    ("source pipeline accepts Float64-domain integer literal arithmetic", testSourcePipelineAcceptsFloat64DomainIntegerLiteralArithmetic),
    ("source pipeline accepts direct typed integer to Float64 arithmetic", testSourcePipelineAcceptsDirectTypedIntegerFloat64Arithmetic),
    ("source pipeline accepts same-width Float64 operator values", testSourcePipelineAcceptsSameWidthFloat64OperatorValues),
    ("source pipeline accepts direct typed integer to Float64 operator values and sections", testSourcePipelineAcceptsDirectTypedIntegerFloat64OperatorValuesSections),
    ("source pipeline accepts dollar alias typed integer to Float64 operator values", testSourcePipelineAcceptsDollarAliasTypedIntegerFloat64OperatorValues),
    ("source pipeline preserves dollar-produced operator aliases", testSourcePipelinePreservesDollarProducedOperatorAliases),
    ("source pipeline accepts dollar-applied typed integer to Float64 sections", testSourcePipelineAcceptsDollarAppliedTypedIntegerFloat64Sections),
    ("source pipeline accepts same-width Float16 and Float32 arithmetic", testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic),
    ("source pipeline accepts targeted Float16 and Float32 arithmetic", testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic),
    ("source pipeline accepts Float16 and Float32 arithmetic boundary values", testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues),
    ("source pipeline accepts same-width Float64 comparison and equality", testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality),
    ("source pipeline accepts Float64-domain integer literal comparison and equality", testSourcePipelineAcceptsFloat64DomainIntegerLiteralComparisonEquality),
    ("source pipeline accepts direct typed integer to Float64 comparison and equality", testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality),
    ("source pipeline accepts direct typed integer to Float64 comparison/equality operator aliases", testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases),
    ("source pipeline accepts same-width Float16 and Float32 comparison and equality", testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality),
    ("source pipeline accepts same-width Float64 comparison/equality operator values", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues),
    ("source pipeline accepts same-width Float64 comparison/equality sections", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections),
    ("source pipeline rejects mixed-width float comparison and equality", testSourcePipelineRejectsMixedWidthFloatComparisonEquality),
    ("source pipeline rejects implicit Float16 and Float32 comparison and equality", testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality),
    ("source pipeline rejects typed integer to Float16 and Float32 promotion", testSourcePipelineRejectsTypedIntegerNarrowFloatPromotion),
    ("source pipeline rejects non-literal integer result Float64-domain arithmetic", testSourcePipelineRejectsNonLiteralIntegerResultFloat64DomainArithmetic),
    ("source pipeline rejects first-class integer Float64-domain sections", testSourcePipelineRejectsFirstClassIntegerFloat64DomainSections),
    ("source pipeline rejects user-defined operator integer to Float64 promotion", testSourcePipelineRejectsUserDefinedOperatorIntegerFloat64Promotion),
    ("source pipeline rejects mixed-width float arithmetic", testSourcePipelineRejectsMixedWidthFloatArithmetic),
    ("source pipeline rejects mixed-width and implicit Float16/Float32 arithmetic", testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic),
    ("source pipeline rejects suffixed fractional literal mixed-width/default arithmetic", testSourcePipelineRejectsSuffixedFractionalLiteralMixedWidthArithmetic),
    ("source pipeline rejects out-of-range literal conversions", testSourcePipelineRejectsOutOfRangeLiteralConversions),
    ("source pipeline rejects non-integral fractional literal conversions", testSourcePipelineRejectsNonIntegralFractionalLiteralConversions),
    ("source pipeline rejects rounded non-integral fractional literal conversions", testSourcePipelineRejectsRoundedNonIntegralFractionalLiteralConversions),
    ("source pipeline accepts integral-boundary fractional literal conversions", testSourcePipelineAcceptsIntegralBoundaryFractionalLiteralConversions),
    ("source pipeline rejects out-of-range float-target literal conversions", testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions),
    ("source pipeline rejects source-exact float-target literal overflow", testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow),
    ("source pipeline rejects suffixed fractional literal target overflow", testSourcePipelineRejectsSuffixedFractionalLiteralTargetOverflow),
    ("source pipeline rejects source-exact negative float-target literal overflow", testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow),
    ("core pipeline rejects targeted Float64 fractional literal overflow", testCorePipelineRejectsTargetedFloat64FractionalLiteralOverflow),
    ("source pipeline rejects dollar-applied fractional literal conversions", testSourcePipelineRejectsDollarAppliedFractionalLiteralConversions),
    ("source pipeline rejects typed prelude alias literal overflow", testSourcePipelineRejectsTypedPreludeAliasLiteralOverflow),
    ("source pipeline ignores conversion literal checks for shadowed names", testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames),
    ("source pipeline freshens prelude conversion aliases", testSourcePipelineFreshensPreludeConversionAliases),
    ("source pipeline keeps locally shadowed kernel aliases ordinary", testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary),
    ("source pipeline rejects non-numeric conversion source", testSourcePipelineRejectsNonNumericConversionSource)
  ]

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

testSourcePipelineAcceptsSuffixedFractionalLiteralArithmetic :: IO ()
testSourcePipelineAcceptsSuffixedFractionalLiteralArithmetic =
  assertCompiles
    "x16 = 1.5f16 + 2.5f16.\nx32 = 1.5f32 + 2.5f32.\nx64 = 1.5f64 + 2.5f64."

testSourcePipelineAcceptsSameWidthFloat64Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat64Arithmetic =
  assertCompilesWithBundledPrelude
    "x :: Float64.\nx = ((1.5 + 2.25) - toFloat64 1) * (6.0 / 2.0)."

testSourcePipelineAcceptsFloat64DomainIntegerLiteralArithmetic :: IO ()
testSourcePipelineAcceptsFloat64DomainIntegerLiteralArithmetic =
  assertCompilesWithBundledPrelude
    "defaultLeft :: Float.\ndefaultLeft = 1 + 1.5.\ndefaultRight :: Float.\ndefaultRight = 1.5 + 2.\ndefaultSub :: Float.\ndefaultSub = 5 - 2.5.\ndefaultMul :: Float.\ndefaultMul = 2 * 1.5.\ndefaultDiv :: Float.\ndefaultDiv = 6 / 2.0.\nexplicitLeft :: Float64.\nexplicitLeft = 1 + toFloat64 1.\nexplicitRight :: Float64.\nexplicitRight = toFloat64 1 + 2.\nexplicitSub :: Float64.\nexplicitSub = 5 - toFloat64 2.\nexplicitMul :: Float64.\nexplicitMul = toFloat64 2 * 3.\nexplicitDiv :: Float64.\nexplicitDiv = 6 / toFloat64 2."

testSourcePipelineAcceptsDirectTypedIntegerFloat64Arithmetic :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64Arithmetic =
  assertCompilesWithBundledPrelude
    "defaultInt :: Int.\ndefaultInt = 4.\nwideInt :: Int64.\nwideInt = toInt64 6.\nnarrowInt :: Int8.\nnarrowInt = toInt8 3.\ndefaultFloat :: Float.\ndefaultFloat = 1.5.\nexplicitFloat :: Float64.\nexplicitFloat = toFloat64 2.\naddDefault :: Float.\naddDefault = defaultInt + defaultFloat.\naddExplicit :: Float64.\naddExplicit = explicitFloat + wideInt.\nsubDefault :: Float.\nsubDefault = defaultInt - defaultFloat.\nsubExplicit :: Float64.\nsubExplicit = explicitFloat - narrowInt.\nmulDefault :: Float.\nmulDefault = narrowInt * defaultFloat.\nmulExplicit :: Float64.\nmulExplicit = wideInt * explicitFloat.\ndivDefault :: Float.\ndivDefault = defaultInt / defaultFloat.\ndivExplicit :: Float64.\ndivExplicit = explicitFloat / wideInt."

testSourcePipelineAcceptsSameWidthFloat64OperatorValues :: IO ()
testSourcePipelineAcceptsSameWidthFloat64OperatorValues =
  assertCompilesWithBundledPrelude
    "x :: Float64.\nx = (+) (toFloat64 1) (toFloat64 2)."

testSourcePipelineAcceptsDirectTypedIntegerFloat64OperatorValuesSections :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64OperatorValuesSections =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\ndirect :: Float64.\ndirect = (+) integer floating.\nliteralDirect :: Float.\nliteralDirect = (+) 1 1.5.\ndollarDirect :: Float64.\ndollarDirect = ($) (+) integer floating.\nadd = (+).\naliased :: Float64.\naliased = add integer floating.\nleftSection :: Float64.\nleftSection = (integer +) floating.\nrightSection :: Float64.\nrightSection = (+ floating) integer.\nliteralLeft :: Float.\nliteralLeft = (1 +) 1.5.\nliteralRight :: Float.\nliteralRight = (+ 1.5) 1."

testSourcePipelineAcceptsDollarAliasTypedIntegerFloat64OperatorValues :: IO ()
testSourcePipelineAcceptsDollarAliasTypedIntegerFloat64OperatorValues =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\napply = ($).\nresult :: Float64.\nresult = apply (+) integer floating."

testSourcePipelinePreservesDollarProducedOperatorAliases :: IO ()
testSourcePipelinePreservesDollarProducedOperatorAliases =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\napply = ($).\nop = apply (+).\nresult :: Float64.\nresult = op integer floating."

testSourcePipelineAcceptsDollarAppliedTypedIntegerFloat64Sections :: IO ()
testSourcePipelineAcceptsDollarAppliedTypedIntegerFloat64Sections =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\nleftSection :: Float64.\nleftSection = ($) (integer +) floating.\nrightSection :: Float64.\nrightSection = ($) (+ floating) integer."

testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic =
  assertCompilesWithBundledPrelude
    "a16 :: Float16.\na16 = toFloat16 8.\nb16 :: Float16.\nb16 = toFloat16 2.\nadd16 :: Float16.\nadd16 = a16 + b16.\nsub16 :: Float16.\nsub16 = a16 - b16.\nmul16 :: Float16.\nmul16 = a16 * b16.\ndiv16 :: Float16.\ndiv16 = a16 / b16.\na32 :: Float32.\na32 = toFloat32 8.\nb32 :: Float32.\nb32 = toFloat32 2.\nadd32 :: Float32.\nadd32 = a32 + b32.\nsub32 :: Float32.\nsub32 = a32 - b32.\nmul32 :: Float32.\nmul32 = a32 * b32.\ndiv32 :: Float32.\ndiv32 = a32 / b32."

testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic :: IO ()
testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic =
  assertCompiles
    "a16 :: Float16.\na16 = 8.0.\nb16 :: Float16.\nb16 = 2.0.\nadd16 :: Float16.\nadd16 = a16 + b16.\nsub16 :: Float16.\nsub16 = a16 - b16.\nmul16 :: Float16.\nmul16 = a16 * b16.\ndiv16 :: Float16.\ndiv16 = a16 / b16.\na32 :: Float32.\na32 = 8.0.\nb32 :: Float32.\nb32 = 2.0.\nadd32 :: Float32.\nadd32 = a32 + b32.\nsub32 :: Float32.\nsub32 = a32 - b32.\nmul32 :: Float32.\nmul32 = a32 * b32.\ndiv32 :: Float32.\ndiv32 = a32 / b32."

testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues :: IO ()
testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues =
  assertCompilesWithBundledPrelude
    "max16 :: Float16.\nmax16 = toFloat16 65504.\nzero16 :: Float16.\nzero16 = toFloat16 0.\nstaysMax16 :: Float16.\nstaysMax16 = max16 + zero16.\nminSub16 :: Float16.\nminSub16 = toFloat16 0.000000059604644775390625.\nscaled16 :: Float16.\nscaled16 = minSub16 * toFloat16 2.\nedge32 :: Float32.\nedge32 = toFloat32 65504.\nzero32 :: Float32.\nzero32 = toFloat32 0.\nstaysEdge32 :: Float32.\nstaysEdge32 = edge32 + zero32."

testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality =
  assertCompiles
    "lt = 1.5 < 2.0.\nle = 2.0 <= 2.0.\ngt = 3.0 > 2.0.\nge = 3.0 >= 3.0.\neq = 2.0 == 2.0.\nne = 2.0 != 3.0."

testSourcePipelineAcceptsFloat64DomainIntegerLiteralComparisonEquality :: IO ()
testSourcePipelineAcceptsFloat64DomainIntegerLiteralComparisonEquality =
  assertCompilesWithBundledPrelude
    "literalLeft = 1 < 1.5.\nliteralEquality = 1 == 1.0.\nleftFloat :: Float64.\nleftFloat = toFloat64 1.\nrightFloat :: Float64.\nrightFloat = toFloat64 2.\nexplicitLeft = 1 < rightFloat.\nexplicitRight = leftFloat < 2.\nexplicitEqualityLeft = 1 == rightFloat.\nexplicitEqualityRight = leftFloat == 1.\nconvertedEquality = toFloat64 1 == 1."

testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality =
  assertCompilesWithBundledPrelude
    "defaultInt :: Int.\ndefaultInt = 2.\nwideInt :: Int64.\nwideInt = toInt64 3.\nnarrowInt :: Int8.\nnarrowInt = toInt8 1.\ndefaultFloat :: Float.\ndefaultFloat = 2.0.\nexplicitFloat :: Float64.\nexplicitFloat = toFloat64 3.\nltDefault = narrowInt < defaultFloat.\nleExplicit = wideInt <= explicitFloat.\ngtDefault = defaultFloat > narrowInt.\ngeExplicit = explicitFloat >= wideInt.\neqDefault = defaultInt == defaultFloat.\nneExplicit = explicitFloat != narrowInt."

testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 1.\neqAlias = (==).\nneAlias = (!=).\neqMixed = eqAlias integer floating.\nneMixed = neAlias floating integer."

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

testSourcePipelineRejectsTypedIntegerNarrowFloatPromotion :: IO ()
testSourcePipelineRejectsTypedIntegerNarrowFloatPromotion = do
  assertCompileErrorWithBundledPrelude
    "integer :: Int.\ninteger = 1.\nfloat16 :: Float16.\nfloat16 = toFloat16 1.\nx = integer + float16."
    "typed Int mixed with Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloat32 :: Float32.\nfloat32 = toFloat32 1.\nx = float32 * integer."
    "typed Int64 mixed with Float32 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "integer :: Int8.\ninteger = toInt8 1.\nfloat16 :: Float16.\nfloat16 = toFloat16 1.\nx = integer < float16."
    "typed Int8 mixed with Float16 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "integer :: Int16.\ninteger = toInt16 1.\nfloat32 :: Float32.\nfloat32 = toFloat32 1.\nx = float32 == integer."
    "typed Int16 mixed with Float32 equality"
    "E2004"

testSourcePipelineRejectsNonLiteralIntegerResultFloat64DomainArithmetic :: IO ()
testSourcePipelineRejectsNonLiteralIntegerResultFloat64DomainArithmetic =
  assertCompileError
    "id = \\(x) -> x.\nx = id 2 + 1.5."
    "non-literal integer result Float64-domain arithmetic"
    "E2003"

testSourcePipelineRejectsFirstClassIntegerFloat64DomainSections :: IO ()
testSourcePipelineRejectsFirstClassIntegerFloat64DomainSections = do
  assertCompileError
    "section = (1 +).\nx = section 1.5."
    "integer literal Float64-domain left section binding"
    "E2006"
  assertCompileError
    "section = (+ 1.5).\nx = section 1."
    "integer literal Float64-domain right section binding"
    "E2006"
  assertCompileErrorWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\nsection = (integer +).\nx = section floating."
    "typed integer Float64-domain left section binding"
    "E2006"
  assertCompileErrorWithBundledPrelude
    "integer :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\nsection = (+ floating).\nx = section integer."
    "typed integer Float64-domain right section binding"
    "E2006"

testSourcePipelineRejectsUserDefinedOperatorIntegerFloat64Promotion :: IO ()
testSourcePipelineRejectsUserDefinedOperatorIntegerFloat64Promotion =
  assertCompileErrorWithBundledPrelude
    "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\ninteger :: Int64.\ninteger = toInt64 1.\nfloating :: Float64.\nfloating = toFloat64 2.\nx = integer %% floating."
    "user-defined operator integer Float64-domain application"
    "E2006"

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

testSourcePipelineRejectsSuffixedFractionalLiteralMixedWidthArithmetic :: IO ()
testSourcePipelineRejectsSuffixedFractionalLiteralMixedWidthArithmetic = do
  assertCompileError
    "x = 1.5f16 + 2.5."
    "suffixed Float16/default Float arithmetic"
    "E2003"
  assertCompileError
    "x = 1.5f16 + 2.5f32."
    "suffixed Float16/Float32 arithmetic"
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

testSourcePipelineAcceptsDefaultPreludeConversionAliases :: IO ()
testSourcePipelineAcceptsDefaultPreludeConversionAliases =
  assertCompilesWithBundledPrelude
    "integer :: Int64.\ninteger = toInt 9223372036854775807.0.\nfloating :: Float64.\nfloating = toFloat 1."

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

testSourcePipelineRejectsSuffixedFractionalLiteralTargetOverflow :: IO ()
testSourcePipelineRejectsSuffixedFractionalLiteralTargetOverflow =
  assertCompileError
    "x = 65504.000000000000000001f16."
    "suffixed Float16 literal target overflow"
    "E2006"

testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow :: IO ()
testSourcePipelineRejectsSourceExactNegativeFloatTargetLiteralOverflow = do
  result <- compileExpr defaultWarningSettings sourceExactNegativeFloatTargetOverflowProgram
  assertSingleDiagnosticContains
    "source-exact negative float-target literal overflow"
    "E2006"
    (compileErrors result)

testCorePipelineRejectsTargetedFloat64FractionalLiteralOverflow :: IO ()
testCorePipelineRejectsTargetedFloat64FractionalLiteralOverflow = do
  result <- compileExpr defaultWarningSettings targetedFloat64OverflowProgram
  assertSingleDiagnosticContains
    "targeted Float64 fractional literal overflow"
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

sourceExactNegativeFloatTargetOverflowProgram :: Expr
sourceExactNegativeFloatTargetOverflowProgram =
  mkProgram
    ( EApply
        (EVar "__kernel_toFloat16")
        (ELit (LFloat (-65504.0) (mkFractionalLiteralSource (-65504) 1 18) Nothing))
    )

targetedFloat64OverflowProgram :: Expr
targetedFloat64OverflowProgram =
  EBlock
    [ SSignature
        "x"
        (SourceSpan 1 1)
        (SignatureType (TypeNumeric NumericFloat64)),
      SLet
        "x"
        (SourceSpan 2 1)
        (ELit (LFloat literalValue literalSource Nothing))
    ]
  where
    literalValue = 1 / 0 :: Double
    literalSource =
      mkFractionalLiteralSource
        ((floor (1.7976931348623157e308 :: Double) :: Integer) + 1)
        0
        1
