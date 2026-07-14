{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.PrimitiveSemantics.NumericConversions
  ( integerWidthTests,
    numericConversionTests
  )
where

import qualified Data.Text as Text
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
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCompileError,
    assertCompileErrorWithBundledPrelude,
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

assertCompileErrorWithPrelude :: String -> String -> String -> String -> IO ()
assertCompileErrorWithPrelude preludeSource source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just (Text.pack preludeSource)) (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral :: IO ()
testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteral =
  assertCompiles """
  y :: UInt8.
  y = 2.
  x = 1 + y.
  z :: UInt8.
  z = x.
  """

testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection :: IO ()
testSourcePipelinePreservesNumericWidthWithLeftIntegerLiteralSection =
  assertCompiles """
  y :: UInt8.
  y = 2.
  f = (1 +).
  z :: UInt8.
  z = f y.
  """

testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection :: IO ()
testSourcePipelinePreservesNumericWidthWithRightIntegerLiteralSection =
  assertCompiles """
  y :: UInt8.
  y = 2.
  f = (+ 1).
  z :: UInt8.
  z = f y.
  """

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
  assertCompilesWithBundledPrelude """
  x :: UInt8.
  x = toUInt8 255.
  y :: Int16.
  y = toInt16 x.
  """

testSourcePipelineAcceptsTargetNamedFloatConversions :: IO ()
testSourcePipelineAcceptsTargetNamedFloatConversions =
  assertCompilesWithBundledPrelude """
  x :: Float64.
  x = toFloat64 1.
  """

testSourcePipelineAcceptsFloat64FractionalLiteralDefaults :: IO ()
testSourcePipelineAcceptsFloat64FractionalLiteralDefaults =
  assertCompiles """
  x = 1.5.
  y :: Float64.
  y = x.
  """

testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals :: IO ()
testSourcePipelineAcceptsTargetedFloat16Float32FractionalLiterals =
  assertCompiles
    """
    x16 :: Float16.
    x16 = 1.5.
    y16 :: Float16.
    y16 = x16.
    x32 :: Float32.
    x32 = 2.25.
    y32 :: Float32.
    y32 = x32.
    """

testSourcePipelineAcceptsSuffixedFractionalLiteralArithmetic :: IO ()
testSourcePipelineAcceptsSuffixedFractionalLiteralArithmetic =
  assertCompiles
    """
    x16 = 1.5f16 + 2.5f16.
    x32 = 1.5f32 + 2.5f32.
    x64 = 1.5f64 + 2.5f64.
    """

testSourcePipelineAcceptsSameWidthFloat64Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat64Arithmetic =
  assertCompilesWithBundledPrelude
    """
    x :: Float64.
    x = ((1.5 + 2.25) - toFloat64 1) * (6.0 / 2.0).
    """

testSourcePipelineAcceptsFloat64DomainIntegerLiteralArithmetic :: IO ()
testSourcePipelineAcceptsFloat64DomainIntegerLiteralArithmetic =
  assertCompilesWithBundledPrelude
    """
    defaultLeft :: Float.
    defaultLeft = 1 + 1.5.
    defaultRight :: Float.
    defaultRight = 1.5 + 2.
    defaultSub :: Float.
    defaultSub = 5 - 2.5.
    defaultMul :: Float.
    defaultMul = 2 * 1.5.
    defaultDiv :: Float.
    defaultDiv = 6 / 2.0.
    explicitLeft :: Float64.
    explicitLeft = 1 + toFloat64 1.
    explicitRight :: Float64.
    explicitRight = toFloat64 1 + 2.
    explicitSub :: Float64.
    explicitSub = 5 - toFloat64 2.
    explicitMul :: Float64.
    explicitMul = toFloat64 2 * 3.
    explicitDiv :: Float64.
    explicitDiv = 6 / toFloat64 2.
    """

testSourcePipelineAcceptsDirectTypedIntegerFloat64Arithmetic :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64Arithmetic =
  assertCompilesWithBundledPrelude
    """
    defaultInt :: Int.
    defaultInt = 4.
    wideInt :: Int64.
    wideInt = toInt64 6.
    narrowInt :: Int8.
    narrowInt = toInt8 3.
    defaultFloat :: Float.
    defaultFloat = 1.5.
    explicitFloat :: Float64.
    explicitFloat = toFloat64 2.
    addDefault :: Float.
    addDefault = defaultInt + defaultFloat.
    addExplicit :: Float64.
    addExplicit = explicitFloat + wideInt.
    subDefault :: Float.
    subDefault = defaultInt - defaultFloat.
    subExplicit :: Float64.
    subExplicit = explicitFloat - narrowInt.
    mulDefault :: Float.
    mulDefault = narrowInt * defaultFloat.
    mulExplicit :: Float64.
    mulExplicit = wideInt * explicitFloat.
    divDefault :: Float.
    divDefault = defaultInt / defaultFloat.
    divExplicit :: Float64.
    divExplicit = explicitFloat / wideInt.
    """

testSourcePipelineAcceptsSameWidthFloat64OperatorValues :: IO ()
testSourcePipelineAcceptsSameWidthFloat64OperatorValues =
  assertCompilesWithBundledPrelude
    """
    x :: Float64.
    x = (+) (toFloat64 1) (toFloat64 2).
    """

testSourcePipelineAcceptsDirectTypedIntegerFloat64OperatorValuesSections :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64OperatorValuesSections =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    direct :: Float64.
    direct = (+) integer floating.
    literalDirect :: Float.
    literalDirect = (+) 1 1.5.
    dollarDirect :: Float64.
    dollarDirect = ($) (+) integer floating.
    add = (+).
    aliased :: Float64.
    aliased = add integer floating.
    leftSection :: Float64.
    leftSection = (integer +) floating.
    rightSection :: Float64.
    rightSection = (+ floating) integer.
    literalLeft :: Float.
    literalLeft = (1 +) 1.5.
    literalRight :: Float.
    literalRight = (+ 1.5) 1.
    """

testSourcePipelineAcceptsDollarAliasTypedIntegerFloat64OperatorValues :: IO ()
testSourcePipelineAcceptsDollarAliasTypedIntegerFloat64OperatorValues =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    apply = ($).
    result :: Float64.
    result = apply (+) integer floating.
    """

testSourcePipelinePreservesDollarProducedOperatorAliases :: IO ()
testSourcePipelinePreservesDollarProducedOperatorAliases =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    apply = ($).
    op = apply (+).
    result :: Float64.
    result = op integer floating.
    """

testSourcePipelineAcceptsDollarAppliedTypedIntegerFloat64Sections :: IO ()
testSourcePipelineAcceptsDollarAppliedTypedIntegerFloat64Sections =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    leftSection :: Float64.
    leftSection = ($) (integer +) floating.
    rightSection :: Float64.
    rightSection = ($) (+ floating) integer.
    """

testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic :: IO ()
testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic =
  assertCompilesWithBundledPrelude
    """
    a16 :: Float16.
    a16 = toFloat16 8.
    b16 :: Float16.
    b16 = toFloat16 2.
    add16 :: Float16.
    add16 = a16 + b16.
    sub16 :: Float16.
    sub16 = a16 - b16.
    mul16 :: Float16.
    mul16 = a16 * b16.
    div16 :: Float16.
    div16 = a16 / b16.
    a32 :: Float32.
    a32 = toFloat32 8.
    b32 :: Float32.
    b32 = toFloat32 2.
    add32 :: Float32.
    add32 = a32 + b32.
    sub32 :: Float32.
    sub32 = a32 - b32.
    mul32 :: Float32.
    mul32 = a32 * b32.
    div32 :: Float32.
    div32 = a32 / b32.
    """

testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic :: IO ()
testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic =
  assertCompiles
    """
    a16 :: Float16.
    a16 = 8.0.
    b16 :: Float16.
    b16 = 2.0.
    add16 :: Float16.
    add16 = a16 + b16.
    sub16 :: Float16.
    sub16 = a16 - b16.
    mul16 :: Float16.
    mul16 = a16 * b16.
    div16 :: Float16.
    div16 = a16 / b16.
    a32 :: Float32.
    a32 = 8.0.
    b32 :: Float32.
    b32 = 2.0.
    add32 :: Float32.
    add32 = a32 + b32.
    sub32 :: Float32.
    sub32 = a32 - b32.
    mul32 :: Float32.
    mul32 = a32 * b32.
    div32 :: Float32.
    div32 = a32 / b32.
    """

testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues :: IO ()
testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues =
  assertCompilesWithBundledPrelude
    """
    max16 :: Float16.
    max16 = toFloat16 65504.
    zero16 :: Float16.
    zero16 = toFloat16 0.
    staysMax16 :: Float16.
    staysMax16 = max16 + zero16.
    minSub16 :: Float16.
    minSub16 = toFloat16 0.000000059604644775390625.
    scaled16 :: Float16.
    scaled16 = minSub16 * toFloat16 2.
    edge32 :: Float32.
    edge32 = toFloat32 65504.
    zero32 :: Float32.
    zero32 = toFloat32 0.
    staysEdge32 :: Float32.
    staysEdge32 = edge32 + zero32.
    """

testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality =
  assertCompiles
    """
    lt = 1.5 < 2.0.
    le = 2.0 <= 2.0.
    gt = 3.0 > 2.0.
    ge = 3.0 >= 3.0.
    eq = 2.0 == 2.0.
    ne = 2.0 != 3.0.
    """

testSourcePipelineAcceptsFloat64DomainIntegerLiteralComparisonEquality :: IO ()
testSourcePipelineAcceptsFloat64DomainIntegerLiteralComparisonEquality =
  assertCompilesWithBundledPrelude
    """
    literalLeft = 1 < 1.5.
    literalEquality = 1 == 1.0.
    leftFloat :: Float64.
    leftFloat = toFloat64 1.
    rightFloat :: Float64.
    rightFloat = toFloat64 2.
    explicitLeft = 1 < rightFloat.
    explicitRight = leftFloat < 2.
    explicitEqualityLeft = 1 == rightFloat.
    explicitEqualityRight = leftFloat == 1.
    convertedEquality = toFloat64 1 == 1.
    """

testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality =
  assertCompilesWithBundledPrelude
    """
    defaultInt :: Int.
    defaultInt = 2.
    wideInt :: Int64.
    wideInt = toInt64 3.
    narrowInt :: Int8.
    narrowInt = toInt8 1.
    defaultFloat :: Float.
    defaultFloat = 2.0.
    explicitFloat :: Float64.
    explicitFloat = toFloat64 3.
    ltDefault = narrowInt < defaultFloat.
    leExplicit = wideInt <= explicitFloat.
    gtDefault = defaultFloat > narrowInt.
    geExplicit = explicitFloat >= wideInt.
    eqDefault = defaultInt == defaultFloat.
    neExplicit = explicitFloat != narrowInt.
    """

testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases :: IO ()
testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 1.
    eqAlias = (==).
    neAlias = (!=).
    eqMixed = eqAlias integer floating.
    neMixed = neAlias floating integer.
    """

testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality :: IO ()
testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality =
  assertCompilesWithBundledPrelude
    """
    a16 :: Float16.
    a16 = toFloat16 1.
    b16 :: Float16.
    b16 = toFloat16 2.
    lt16 = a16 < b16.
    le16 = a16 <= a16.
    gt16 = b16 > a16.
    ge16 = b16 >= b16.
    eq16 = a16 == a16.
    ne16 = a16 != b16.
    a32 :: Float32.
    a32 = toFloat32 1.
    b32 :: Float32.
    b32 = toFloat32 2.
    lt32 = a32 < b32.
    le32 = a32 <= a32.
    gt32 = b32 > a32.
    ge32 = b32 >= b32.
    eq32 = a32 == a32.
    ne32 = a32 != b32.
    """

testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues =
  assertCompiles
    """
    lt = (<) 1.5 2.0.
    le = (<=) 2.0 2.0.
    gt = (>) 3.0 2.0.
    ge = (>=) 3.0 3.0.
    eq = (==) 2.0 2.0.
    ne = (!=) 2.0 3.0.
    """

testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections :: IO ()
testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections =
  assertCompiles
    """
    lt = (1.5 <) 2.0.
    le = (2.0 <=) 2.0.
    gt = (> 2.0) 3.0.
    ge = (>= 3.0) 3.0.
    eq = (2.0 ==) 2.0.
    ne = (!= 3.0) 2.0.
    """

testSourcePipelineRejectsMixedWidthFloatComparisonEquality :: IO ()
testSourcePipelineRejectsMixedWidthFloatComparisonEquality = do
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    right :: Float32.
    right = toFloat32 1.
    x = left == right.
    """
    "mixed Float16/Float32 equality"
    "E2004"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    right :: Float32.
    right = toFloat32 2.
    x = left < right.
    """
    "mixed Float16/Float32 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    right :: Float64.
    right = toFloat64 1.
    x = left < right.
    """
    "mixed Float16/Float64 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float32.
    left = toFloat32 1.
    right :: Float64.
    right = toFloat64 1.
    x = left == right.
    """
    "mixed Float32/Float64 equality"
    "E2004"

testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality :: IO ()
testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality = do
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    x = left < 1.
    """
    "implicit integer-to-Float16 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float32.
    left = toFloat32 1.
    x = left == 1.
    """
    "implicit integer-to-Float32 equality"
    "E2004"

testSourcePipelineRejectsTypedIntegerNarrowFloatPromotion :: IO ()
testSourcePipelineRejectsTypedIntegerNarrowFloatPromotion = do
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int.
    integer = 1.
    float16 :: Float16.
    float16 = toFloat16 1.
    x = integer + float16.
    """
    "typed Int mixed with Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    float32 :: Float32.
    float32 = toFloat32 1.
    x = float32 * integer.
    """
    "typed Int64 mixed with Float32 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int8.
    integer = toInt8 1.
    float16 :: Float16.
    float16 = toFloat16 1.
    x = integer < float16.
    """
    "typed Int8 mixed with Float16 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int16.
    integer = toInt16 1.
    float32 :: Float32.
    float32 = toFloat32 1.
    x = float32 == integer.
    """
    "typed Int16 mixed with Float32 equality"
    "E2004"

testSourcePipelineRejectsNonLiteralIntegerResultFloat64DomainArithmetic :: IO ()
testSourcePipelineRejectsNonLiteralIntegerResultFloat64DomainArithmetic =
  assertCompileError
    """
    id = \\(x) -> x.
    x = id 2 + 1.5.
    """
    "non-literal integer result Float64-domain arithmetic"
    "E2003"

testSourcePipelineRejectsFirstClassIntegerFloat64DomainSections :: IO ()
testSourcePipelineRejectsFirstClassIntegerFloat64DomainSections = do
  assertCompileError
    """
    section = (1 +).
    x = section 1.5.
    """
    "integer literal Float64-domain left section binding"
    "E2006"
  assertCompileError
    """
    section = (+ 1.5).
    x = section 1.
    """
    "integer literal Float64-domain right section binding"
    "E2006"
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    section = (integer +).
    x = section floating.
    """
    "typed integer Float64-domain left section binding"
    "E2006"
  assertCompileErrorWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    section = (+ floating).
    x = section integer.
    """
    "typed integer Float64-domain right section binding"
    "E2006"

testSourcePipelineRejectsUserDefinedOperatorIntegerFloat64Promotion :: IO ()
testSourcePipelineRejectsUserDefinedOperatorIntegerFloat64Promotion =
  assertCompileErrorWithBundledPrelude
    """
    operator %% tier 2.
    (%%) = \\(left) -> \\(right) -> left + right.
    integer :: Int64.
    integer = toInt64 1.
    floating :: Float64.
    floating = toFloat64 2.
    x = integer %% floating.
    """
    "user-defined operator integer Float64-domain application"
    "E2006"

testSourcePipelineRejectsMixedWidthFloatArithmetic :: IO ()
testSourcePipelineRejectsMixedWidthFloatArithmetic =
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    x :: Float64.
    x = left + 2.0.
    """
    "mixed-width float arithmetic"
    "E2003"

testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic :: IO ()
testSourcePipelineRejectsMixedWidthAndImplicitFloat16Float32Arithmetic = do
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    right :: Float32.
    right = toFloat32 2.
    x = left + right.
    """
    "mixed Float16/Float32 arithmetic"
    "E2003"
  assertCompileError
    """
    left :: Float16.
    left = 1.5.
    right :: Float32.
    right = 2.25.
    x = left + right.
    """
    "mixed targeted Float16/Float32 arithmetic"
    "E2003"
  assertCompileError
    """
    left :: Float16.
    left = 1.5.
    x = left + 1.25.
    """
    "implicit fractional literal-to-Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float16.
    left = toFloat16 1.
    x = left + 1.
    """
    "implicit integer-to-Float16 arithmetic"
    "E2003"
  assertCompileErrorWithBundledPrelude
    """
    left :: Float32.
    left = toFloat32 1.
    x = left + 1.
    """
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
    """
    x = toInt64 9223372036854775807.0.
    y = toUInt64 18446744073709551615.0.
    """

testSourcePipelineAcceptsDefaultPreludeConversionAliases :: IO ()
testSourcePipelineAcceptsDefaultPreludeConversionAliases =
  assertCompilesWithBundledPrelude
    """
    integer :: Int64.
    integer = toInt 9223372036854775807.0.
    floating :: Float64.
    floating = toFloat 1.
    """

testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions :: IO ()
testSourcePipelineRejectsOutOfRangeFloatTargetLiteralConversions = do
  assertCompileErrorWithBundledPrelude
    "x = toFloat16 70000."
    "out-of-range float-target literal conversion"
    "E2006"
  assertCompileError
    """
    x :: Float16.
    x = 70000.0.
    """
    "out-of-range Float16 literal target"
    "E2006"
  assertCompileError
    """
    x :: Float32.
    x = 1000000000000000000000000000000000000000.0.
    """
    "out-of-range Float32 literal target"
    "E2006"

testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow :: IO ()
testSourcePipelineRejectsSourceExactFloatTargetLiteralOverflow = do
  assertCompileErrorWithBundledPrelude
    "x = toFloat16 65504.000000000000000001."
    "source-exact float-target literal overflow"
    "E2006"
  assertCompileError
    """
    x :: Float16.
    x = 65504.000000000000000001.
    """
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
    """
    toFloat16 :: Float -> Float16.
    toFloat16 = __kernel_toFloat16.
    """
    "x = toFloat16 65504.000000000000000001."
    "typed prelude alias source-exact literal overflow"
    "E2006"

testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames :: IO ()
testSourcePipelineIgnoresConversionLiteralChecksForShadowedNames =
  assertCompiles """
  toUInt8 = \\(x) -> x.
  x = toUInt8 256.
  """

testSourcePipelineFreshensPreludeConversionAliases :: IO ()
testSourcePipelineFreshensPreludeConversionAliases =
  assertCompilesWithBundledPrelude
    """
    a :: Int16.
    a = toInt16 1.
    b :: UInt8.
    b = toUInt8 a.
    c :: UInt16.
    c = toUInt16 2.
    d :: UInt8.
    d = toUInt8 c.
    """

testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary :: IO ()
testSourcePipelineKeepsLocallyShadowedKernelAliasesOrdinary =
  assertCompilesWithBundledPrelude
    """
    x = {
    __kernel_toUInt8 = \\(value) -> value.
    alias = __kernel_toUInt8.
    alias 256.
    }.
    """

testSourcePipelineRejectsNonNumericConversionSource :: IO ()
testSourcePipelineRejectsNonNumericConversionSource =
  assertCompileErrorWithBundledPrelude
    """
    flag = True.
    x = toInt8 flag.
    """
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
