{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.NumericTests
  ( numericTests
  ) where


import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Name (Name, qualifiedName)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKey
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltins
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertLeftDiagnosticCodeAndContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
    runTestSuite
  )
import System.Timeout
  ( timeout
  )
import JazzNext.Compiler.Semantics.Runtime.Shared

numericTests :: [NamedTest]
numericTests =
  [ ("division by zero produces fatal runtime diagnostic", testDivisionByZeroRuntimeError)
    , ("right section differs from ordinary partial application for division", testRightSectionDiffersFromOrdinaryPartialApplication)
    , ("target-named integer conversion evaluates at runtime", testIntegerConversionRuntimeSuccess)
    , ("target-named integer conversion preserves source-exact integral Float literal", testIntegerConversionSourceExactIntegralFloatRuntimeSuccess)
    , ("default integer conversion alias preserves source-exact integral Float literal", testDefaultIntegerConversionAliasRuntimeSuccess)
    , ("Float64 signature preserves source-exact integral conversion", testFloat64SignaturePreservesSourceExactIntegralConversion)
    , ("Float16 signature converts from rounded runtime value", testFloat16SignatureConvertsFromRoundedRuntimeValue)
    , ("width-specific integer arithmetic checks preserved result bounds", testWidthSpecificIntegerArithmeticBoundsRuntimeError)
    , ("target-named float conversion evaluates at runtime", testFloatConversionRuntimeSuccess)
    , ("default float conversion alias evaluates at runtime", testDefaultFloatConversionAliasRuntimeSuccess)
    , ("dynamic integer-to-Float64 overflow checks source magnitude", testDynamicIntegerToFloat64OverflowRuntimeError)
    , ("fractional literal evaluates and renders at runtime", testFractionalLiteralRuntimeSuccess)
    , ("Float64 arithmetic evaluates at runtime", testFloat64ArithmeticRuntimeSuccess)
    , ("Float64-domain integer literal arithmetic evaluates at runtime", testFloat64DomainIntegerLiteralArithmeticRuntimeSuccess)
    , ("direct typed integer to Float64 arithmetic evaluates at runtime", testDirectTypedIntegerFloat64ArithmeticRuntimeSuccess)
    , ("Float16 arithmetic preserves target width at runtime", testFloat16ArithmeticPreservesRuntimeWidth)
    , ("Float32 arithmetic preserves target width at runtime", testFloat32ArithmeticPreservesRuntimeWidth)
    , ("runtime fallback rejects targeted Float16/Float32 mixed with untyped Float arithmetic", testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic)
    , ("runtime fallback handles direct integer and Float64 mixed-domain arithmetic", testRuntimeFallbackHandlesIntegerFloat64MixedDomainArithmetic)
    , ("runtime fallback rejects mixed targeted float comparison and equality", testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality)
    , ("runtime fallback handles untyped integer and Float64 comparison/equality", testRuntimeFallbackHandlesUntypedIntegerFloat64ComparisonEquality)
    , ("targeted Float16 and Float32 fractional literals round at runtime", testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue)
    , ("suffixed Float16 and Float32 fractional literals round at runtime", testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue)
    , ("Float16 arithmetic overflow produces runtime diagnostic", testFloat16ArithmeticOverflowRuntimeError)
    , ("Float64 arithmetic overflow produces runtime diagnostic", testFloat64ArithmeticOverflowRuntimeError)
    , ("Float64 comparison and equality evaluate at runtime", testFloat64ComparisonEqualityRuntimeSuccess)
    , ("direct typed integer to Float64 comparison and equality evaluate at runtime", testDirectTypedIntegerFloat64ComparisonEqualityRuntimeSuccess)
    , ("Float16 and Float32 comparison and equality evaluate at runtime", testFloat16Float32ComparisonEqualityRuntimeSuccess)
    , ("targeted Float16 and Float32 fractional literals evaluate through comparison and equality", testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess)
    , ("runtime fallback rejects mixed targeted integer equality", testRuntimeFallbackRejectsMixedTargetedIntegerEquality)
    , ("runtime fallback rejects mixed targeted integer comparison", testRuntimeFallbackRejectsMixedTargetedIntegerComparison)
    , ("Float16 conversion rounds to target precision", testFloat16ConversionRoundsRuntimeValue)
    , ("dynamic integer conversion range failure reports deterministic diagnostic", testDynamicIntegerConversionRangeRuntimeError)
    , ("runtime fallback rejects non-numeric conversion values", testRuntimeFallbackRejectsNonNumericConversionValue)
    , ("typed numeric sections preserve captured operand flexibility", testTypedNumericSectionPreservesCapturedOperandFlexibility)
    , ("defaulted integer binding hints reject values outside Int64 range", testDefaultedIntegerBindingHintRejectsOutsideInt64Range)
  ]

testDivisionByZeroRuntimeError :: IO ()
testDivisionByZeroRuntimeError = do
  result <- runSource defaultWarningSettings "1 / 0."
  let runtimeErrors = runRuntimeErrors result
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "runtime fatal division by zero"
    "E3001"
    runtimeErrors
  case runtimeErrors of
    [] ->
      fail "expected division-by-zero runtime error, but got no runtime errors"
    runtimeError : _ ->
      assertContains
        "runtime fatal mentions division by zero"
        "division by zero"
        (renderDiagnostic runtimeError)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRightSectionDiffersFromOrdinaryPartialApplication :: IO ()
testRightSectionDiffersFromOrdinaryPartialApplication = do
  rightSectionResult <- runSource defaultWarningSettings "(/ 2) 10."
  partialApplicationResult <- runSource defaultWarningSettings "((/) 2) 10."
  assertEqual "right section compile errors" [] (runCompileErrors rightSectionResult)
  assertEqual "right section runtime errors" [] (runRuntimeErrors rightSectionResult)
  assertEqual "right section runtime output" (Just "5") (runOutput rightSectionResult)
  assertEqual "partial application compile errors" [] (runCompileErrors partialApplicationResult)
  assertEqual "partial application runtime errors" [] (runRuntimeErrors partialApplicationResult)
  assertEqual "partial application runtime output" (Just "0") (runOutput partialApplicationResult)

testIntegerConversionRuntimeSuccess :: IO ()
testIntegerConversionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toUInt8 255."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "255") (runOutput result)

testIntegerConversionSourceExactIntegralFloatRuntimeSuccess :: IO ()
testIntegerConversionSourceExactIntegralFloatRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toInt64 9223372036854775807.0."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testDefaultIntegerConversionAliasRuntimeSuccess :: IO ()
testDefaultIntegerConversionAliasRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toInt 9223372036854775807.0."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testFloat64SignaturePreservesSourceExactIntegralConversion :: IO ()
testFloat64SignaturePreservesSourceExactIntegralConversion = do
  result <- runSource defaultWarningSettings "value :: Float64.\nvalue = 9223372036854775807.0.\ntoInt64 value."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testFloat16SignatureConvertsFromRoundedRuntimeValue :: IO ()
testFloat16SignatureConvertsFromRoundedRuntimeValue = do
  result <- runSource defaultWarningSettings "value :: Float16.\nvalue = 2049.0.\ntoInt64 value."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048") (runOutput result)

testWidthSpecificIntegerArithmeticBoundsRuntimeError :: IO ()
testWidthSpecificIntegerArithmeticBoundsRuntimeError = do
  result <- runSource defaultWarningSettings "value = toUInt8 255.\nvalue + 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "UInt8 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "UInt8 arithmetic overflow runtime text"
    "outside UInt8 range"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloatConversionRuntimeSuccess :: IO ()
testFloatConversionRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toFloat64 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1.0") (runOutput result)

testDefaultFloatConversionAliasRuntimeSuccess :: IO ()
testDefaultFloatConversionAliasRuntimeSuccess = do
  result <- runSource defaultWarningSettings "toFloat 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1.0") (runOutput result)

testDynamicIntegerToFloat64OverflowRuntimeError :: IO ()
testDynamicIntegerToFloat64OverflowRuntimeError = do
  let justAboveFloat64MaxInteger = show ((floor (1.7976931348623157e308 :: Double) :: Integer) + 1)
      source = Text.pack ("id = \\(value) -> value.\ntoFloat64 (id " <> justAboveFloat64MaxInteger <> ").")
  result <- runSource defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "dynamic integer-to-Float64 overflow runtime code"
    "E3024"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "dynamic integer-to-Float64 overflow runtime text"
    "finite Float64"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFractionalLiteralRuntimeSuccess :: IO ()
testFractionalLiteralRuntimeSuccess = do
  result <- runSource defaultWarningSettings "1.25."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1.25") (runOutput result)

testFloat64ArithmeticRuntimeSuccess :: IO ()
testFloat64ArithmeticRuntimeSuccess = do
  result <- runSource defaultWarningSettings "((7.5 - 1.5) * 2.0) / 3.0 + 0.25."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "4.25") (runOutput result)

testFloat64DomainIntegerLiteralArithmeticRuntimeSuccess :: IO ()
testFloat64DomainIntegerLiteralArithmeticRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(1 + 1.5, 1.5 + 2, 5 - 2.5, 2 * 1.5, 1 / 2.0)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2.5, 3.5, 2.5, 3.0, 0.5)") (runOutput result)

testDirectTypedIntegerFloat64ArithmeticRuntimeSuccess :: IO ()
testDirectTypedIntegerFloat64ArithmeticRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      "integer :: Int64.\ninteger = toInt64 6.\nnarrow :: Int8.\nnarrow = toInt8 3.\nfloating :: Float64.\nfloating = toFloat64 2.\ndefaultInt :: Int.\ndefaultInt = 4.\ndefaultFloat :: Float.\ndefaultFloat = 1.5.\n(integer + floating, floating + integer, integer - floating, floating - narrow, integer * floating, narrow * defaultFloat, integer / floating, floating / integer, defaultInt + defaultFloat)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(8.0, 8.0, 4.0, -1.0, 12.0, 4.5, 3.0, 0.3333333333333333, 5.5)") (runOutput result)

testFloat16ArithmeticPreservesRuntimeWidth :: IO ()
testFloat16ArithmeticPreservesRuntimeWidth = do
  result <- runSource defaultWarningSettings "left :: Float16.\nleft = 2048.0.\none :: Float16.\none = 1.0.\nthree :: Float16.\nthree = 3.0.\nmulLeft :: Float16.\nmulLeft = 683.0.\nadd16 :: Float16.\nadd16 = left + one.\nsub16 :: Float16.\nsub16 = add16 - one.\nmul16 :: Float16.\nmul16 = mulLeft * three.\ndiv16 :: Float16.\ndiv16 = add16 / one.\n(add16, sub16, mul16, div16)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 2047.0, 2048.0, 2048.0)") (runOutput result)

testFloat32ArithmeticPreservesRuntimeWidth :: IO ()
testFloat32ArithmeticPreservesRuntimeWidth = do
  result <- runSource defaultWarningSettings "one :: Float32.\none = 1.0.\nepsilon :: Float32.\nepsilon = 0.00000001.\nadd32 :: Float32.\nadd32 = one + epsilon.\nsub32 :: Float32.\nsub32 = add32 - epsilon.\nmul32 :: Float32.\nmul32 = one * add32.\ndiv32 :: Float32.\ndiv32 = add32 / one.\n(add32, sub32, mul32, div32)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1.0, 1.0, 1.0, 1.0)") (runOutput result)

testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic :: IO ()
testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic = do
  assertRuntimeErrorContains
    "runtime fallback Float16 plus untyped Float"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" (targetedFloat "__kernel_toFloat16") untypedFloatOne)))
  assertRuntimeErrorContains
    "runtime fallback untyped Float plus Float32"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" untypedFloatOne (targetedFloat "__kernel_toFloat32"))))

testRuntimeFallbackHandlesIntegerFloat64MixedDomainArithmetic :: IO ()
testRuntimeFallbackHandlesIntegerFloat64MixedDomainArithmetic = do
  case evaluateRuntimeExpr (runtimeExpr (EBinary "+" (targetedInt "__kernel_toInt64") (targetedFloat "__kernel_toFloat64"))) of
    Right (Just (VFloat value _)) ->
      assertEqual "runtime fallback typed Int64 plus Float64" 2.0 value
    Right otherValue ->
      failTest ("expected Float64-domain runtime value, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected Float64-domain runtime success, got " <> renderDiagnostic runtimeError)
  assertRuntimeErrorContains
    "runtime fallback untyped Int plus Float16"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" (ELit (LInt 1)) (targetedFloat "__kernel_toFloat16"))))
  assertRuntimeErrorContains
    "runtime fallback integer-to-Float64 arithmetic overflow"
    "E3024"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "+" tooLargeFloat64Integer (targetedFloat "__kernel_toFloat64"))))

testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality = do
  assertRuntimeErrorContains
    "runtime fallback Float16 less-than Float32"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat32"))))
  assertRuntimeErrorContains
    "runtime fallback Float16 equality Float64"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat64"))))
  assertRuntimeErrorContains
    "runtime fallback Float32 inequality untyped Float"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "!=" (targetedFloat "__kernel_toFloat32") untypedFloatOne)))

testRuntimeFallbackHandlesUntypedIntegerFloat64ComparisonEquality :: IO ()
testRuntimeFallbackHandlesUntypedIntegerFloat64ComparisonEquality = do
  assertRuntimeBool
    "runtime fallback untyped Int less-than untyped Float"
    True
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<" (ELit (LInt 1)) untypedFloatTwo)))
  assertRuntimeBool
    "runtime fallback untyped Int equality Float64"
    True
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" (ELit (LInt 1)) (targetedFloat "__kernel_toFloat64"))))

testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue :: IO ()
testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "x16 :: Float16.\nx16 = 2049.0.\nx32 :: Float32.\nx32 = 1.00000001.\ny16 :: @{}: Float16.\ny16 = 2049.0.\ny32 :: @{}: Float32.\ny32 = 1.00000001.\n(x16, x32, y16, y32)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0, 2048.0, 1.0)") (runOutput result)

testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue :: IO ()
testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "x16 = 2049.0f16.\nx32 = 1.00000001f32.\nx64 = 1.5f64.\n(x16, x32, x64)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0, 1.5)") (runOutput result)

testFloat16ArithmeticOverflowRuntimeError :: IO ()
testFloat16ArithmeticOverflowRuntimeError = do
  result <- runSource defaultWarningSettings "left = toFloat16 65504.\nright = toFloat16 65504.\nleft + right."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "Float16 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "Float16 arithmetic overflow runtime text"
    "finite Float16"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloat64ArithmeticOverflowRuntimeError :: IO ()
testFloat64ArithmeticOverflowRuntimeError = do
  let hugeInteger = "1" <> replicate 200 '0'
      source =
        Text.pack
          ( "left = toFloat64 "
              <> hugeInteger
              <> ".\nright = toFloat64 "
              <> hugeInteger
              <> ".\nleft * right."
          )
  result <- runSource defaultWarningSettings source
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "Float64 arithmetic overflow runtime code"
    "E3025"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "Float64 arithmetic overflow runtime text"
    "non-finite Float result"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testFloat64ComparisonEqualityRuntimeSuccess :: IO ()
testFloat64ComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "lt = 1.5 < 2.0.\nle = 2.0 <= 2.0.\ngt = 3.0 > 2.0.\nge = 3.0 >= 3.0.\neq = 2.0 == 2.0.\nne = 2.0 != 3.0.\n[lt, le, gt, ge, eq, ne]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True]") (runOutput result)

testDirectTypedIntegerFloat64ComparisonEqualityRuntimeSuccess :: IO ()
testDirectTypedIntegerFloat64ComparisonEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      "integer :: Int64.\ninteger = toInt64 2.\nnarrow :: Int8.\nnarrow = toInt8 1.\nfloating :: Float64.\nfloating = toFloat64 2.\ndefaultFloat :: Float.\ndefaultFloat = 3.0.\n[integer < defaultFloat, integer <= floating, defaultFloat > narrow, floating >= integer, integer == floating, defaultFloat != integer]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True]") (runOutput result)
  assertRuntimeErrorContains
    "runtime fallback untyped integer-to-Float64 comparison overflow"
    "E3024"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" tooLargeFloat64Integer (targetedFloat "__kernel_toFloat64"))))

testFloat16Float32ComparisonEqualityRuntimeSuccess :: IO ()
testFloat16Float32ComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "a16 = toFloat16 1.\nb16 = toFloat16 2.\na32 = toFloat32 1.\nb32 = toFloat32 2.\nlt16 = a16 < b16.\nle16 = a16 <= a16.\ngt16 = b16 > a16.\nge16 = b16 >= b16.\neq16 = a16 == a16.\nne16 = a16 != b16.\nlt32 = a32 < b32.\nle32 = a32 <= a32.\ngt32 = b32 > a32.\nge32 = b32 >= b32.\neq32 = a32 == a32.\nne32 = a32 != b32.\n[lt16, le16, gt16, ge16, eq16, ne16, lt32, le32, gt32, ge32, eq32, ne32]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess :: IO ()
testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess = do
  result <- runSource defaultWarningSettings "a16 :: Float16.\na16 = 1.5.\nb16 :: Float16.\nb16 = 2.25.\na32 :: Float32.\na32 = 1.5.\nb32 :: Float32.\nb32 = 2.25.\n[a16 < b16, a16 <= a16, b16 > a16, b16 >= b16, a16 == a16, a16 != b16, a32 < b32, a32 <= a32, b32 > a32, b32 >= b32, a32 == a32, a32 != b32]."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testRuntimeFallbackRejectsMixedTargetedIntegerEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerEquality =
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer equality"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "==" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))

testRuntimeFallbackRejectsMixedTargetedIntegerComparison :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerComparison = do
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer less-than"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))
  assertRuntimeErrorContains
    "runtime fallback targeted UInt8 less-or-equal untyped out-of-range Int"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary "<=" (targetedInt "__kernel_toUInt8") (ELit (LInt 256)))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-than"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary ">" (targetedInt "__kernel_toUInt8") (targetedInt "__kernel_toInt16"))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-or-equal"
    "E3007"
    (evaluateRuntimeExpr (runtimeExpr (EBinary ">=" (targetedInt "__kernel_toUInt16") (targetedInt "__kernel_toInt8"))))

testFloat16ConversionRoundsRuntimeValue :: IO ()
testFloat16ConversionRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "toFloat16 2049."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048.0") (runOutput result)

testDynamicIntegerConversionRangeRuntimeError :: IO ()
testDynamicIntegerConversionRangeRuntimeError = do
  result <- runSource defaultWarningSettings "x :: Int.\nx = 256.\ntoUInt8 x."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "dynamic conversion range runtime code"
    "E3024"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "dynamic conversion range runtime text"
    "outside UInt8 range"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)

testRuntimeFallbackRejectsNonNumericConversionValue :: IO ()
testRuntimeFallbackRejectsNonNumericConversionValue = do
  let result = evaluateRuntimeExpr (runtimeExpr (EApply (EVar "__kernel_toInt8") (ELit (LBool True))))
  assertRuntimeErrorContains "runtime fallback conversion non-numeric" "E3024" result

testTypedNumericSectionPreservesCapturedOperandFlexibility :: IO ()
testTypedNumericSectionPreservesCapturedOperandFlexibility = do
  result <-
    runSource
      defaultWarningSettings
      ( "add8 :: UInt8 -> UInt8.\n"
          <> "add8 = (+ 1).\n"
          <> "add8 (toUInt8 2)."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDefaultedIntegerBindingHintRejectsOutsideInt64Range :: IO ()
testDefaultedIntegerBindingHintRejectsOutsideInt64Range = do
  result <- runSource defaultWarningSettings "value = 18446744073709551616.\nvalue."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticContains
    "defaulted integer runtime code"
    "E3024"
    (runRuntimeErrors result)
  assertSingleDiagnosticContains
    "defaulted integer runtime text"
    "outside Int64 range"
    (runRuntimeErrors result)
  assertEqual "runtime output is suppressed on runtime failure" Nothing (runOutput result)
