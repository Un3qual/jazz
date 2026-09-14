{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.NumericTests
  ( numericTests,
  )
where

import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Driver
  ( runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.ResolvedFixture
import Jazz.Compiler.Semantics.Runtime.Shared
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticContains,
  )

numericTests :: [NamedTest]
numericTests =
  [ ("division by zero produces fatal runtime diagnostic", testDivisionByZeroRuntimeError),
    ("right section differs from ordinary partial application for division", testRightSectionDiffersFromOrdinaryPartialApplication),
    ("target-named integer conversion evaluates at runtime", testIntegerConversionRuntimeSuccess),
    ("target-named integer conversion preserves source-exact integral Float literal", testIntegerConversionSourceExactIntegralFloatRuntimeSuccess),
    ("default integer conversion alias preserves source-exact integral Float literal", testDefaultIntegerConversionAliasRuntimeSuccess),
    ("Float64 signature preserves source-exact integral conversion", testFloat64SignaturePreservesSourceExactIntegralConversion),
    ("Float16 signature converts from rounded runtime itemValue", testFloat16SignatureConvertsFromRoundedRuntimeValue),
    ("width-specific integer arithmetic checks preserved result bounds", testWidthSpecificIntegerArithmeticBoundsRuntimeError),
    ("target-named float conversion evaluates at runtime", testFloatConversionRuntimeSuccess),
    ("default float conversion alias evaluates at runtime", testDefaultFloatConversionAliasRuntimeSuccess),
    ("dynamic integer-to-Float64 overflow checks source magnitude", testDynamicIntegerToFloat64OverflowRuntimeError),
    ("fractional literal evaluates and renders at runtime", testFractionalLiteralRuntimeSuccess),
    ("Float64 arithmetic evaluates at runtime", testFloat64ArithmeticRuntimeSuccess),
    ("Float64 literal arithmetic evaluates at runtime", testFloat64LiteralArithmeticRuntimeSuccess),
    ("explicit integer to Float64 arithmetic evaluates at runtime", testExplicitIntegerFloat64ArithmeticRuntimeSuccess),
    ("Float16 arithmetic preserves target width at runtime", testFloat16ArithmeticPreservesRuntimeWidth),
    ("Float32 arithmetic preserves target width at runtime", testFloat32ArithmeticPreservesRuntimeWidth),
    ("runtime fallback rejects targeted Float16/Float32 mixed with untyped Float arithmetic", testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic),
    ("kernel rejects mixed integer and Float64 arithmetic", testKernelRejectsMixedIntegerFloatArithmetic),
    ("runtime fallback rejects mixed targeted float comparison and equality", testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality),
    ("kernel rejects mixed integer and Float64 comparison/equality", testKernelRejectsMixedIntegerFloatComparison),
    ("targeted Float16 and Float32 fractional literals round at runtime", testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue),
    ("suffixed Float16 and Float32 fractional literals round at runtime", testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue),
    ("Float16 arithmetic overflow produces runtime diagnostic", testFloat16ArithmeticOverflowRuntimeError),
    ("Float64 arithmetic overflow produces runtime diagnostic", testFloat64ArithmeticOverflowRuntimeError),
    ("Float64 comparison and equality evaluate at runtime", testFloat64ComparisonEqualityRuntimeSuccess),
    ("explicit integer to Float64 comparison and equality evaluate at runtime", testExplicitIntegerFloat64ComparisonEqualityRuntimeSuccess),
    ("Float16 and Float32 comparison and equality evaluate at runtime", testFloat16Float32ComparisonEqualityRuntimeSuccess),
    ("targeted Float16 and Float32 fractional literals evaluate through comparison and equality", testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess),
    ("runtime fallback rejects mixed targeted integer equality", testRuntimeFallbackRejectsMixedTargetedIntegerEquality),
    ("runtime fallback rejects mixed targeted integer comparison", testRuntimeFallbackRejectsMixedTargetedIntegerComparison),
    ("Float16 conversion rounds to target precision", testFloat16ConversionRoundsRuntimeValue),
    ("dynamic integer conversion range failure reports deterministic diagnostic", testDynamicIntegerConversionRangeRuntimeError),
    ("runtime fallback rejects non-numeric conversion values", testRuntimeFallbackRejectsNonNumericConversionValue),
    ("typed numeric sections preserve captured operand flexibility", testTypedNumericSectionPreservesCapturedOperandFlexibility),
    ("defaulted integer binding hints reject values outside Int64 range", testDefaultedIntegerBindingHintRejectsOutsideInt64Range)
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
  result <-
    runSource
      defaultWarningSettings
      """
      itemValue :: Float64.
      itemValue = 9223372036854775807.0.
      toInt64 itemValue.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "9223372036854775807") (runOutput result)

testFloat16SignatureConvertsFromRoundedRuntimeValue :: IO ()
testFloat16SignatureConvertsFromRoundedRuntimeValue = do
  result <-
    runSource
      defaultWarningSettings
      """
      itemValue :: Float16.
      itemValue = 2049.0.
      toInt64 itemValue.
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048") (runOutput result)

testWidthSpecificIntegerArithmeticBoundsRuntimeError :: IO ()
testWidthSpecificIntegerArithmeticBoundsRuntimeError = do
  result <-
    runSource
      defaultWarningSettings
      """
      itemValue = toUInt8 255.
      itemValue + 1.
      """
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
      -- Explicit fragments are intentional: this program embeds a generated boundary itemValue.
      source = Text.pack ("id = \\(itemValue) -> itemValue.\ntoFloat64 (id " <> justAboveFloat64MaxInteger <> ").")
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

testFloat64LiteralArithmeticRuntimeSuccess :: IO ()
testFloat64LiteralArithmeticRuntimeSuccess = do
  result <- runSource defaultWarningSettings "(1.0 + 1.5, 1.5 + 2.0, 5.0 - 2.5, 2.0 * 1.5, 1.0 / 2.0)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2.5, 3.5, 2.5, 3.0, 0.5)") (runOutput result)

testExplicitIntegerFloat64ArithmeticRuntimeSuccess :: IO ()
testExplicitIntegerFloat64ArithmeticRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      integer :: Int64.
      integer = toInt64 6.
      narrow :: Int8.
      narrow = toInt8 3.
      floating :: Float64.
      floating = toFloat64 2.
      defaultInt :: Int.
      defaultInt = 4.
      defaultFloat :: Float.
      defaultFloat = 1.5.
      ((toFloat64 integer) + floating, floating + (toFloat64 integer), (toFloat64 integer) - floating, floating - (toFloat64 narrow), (toFloat64 integer) * floating, (toFloat64 narrow) * defaultFloat, (toFloat64 integer) / floating, floating / (toFloat64 integer), (toFloat64 defaultInt) + defaultFloat).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(8.0, 8.0, 4.0, -1.0, 12.0, 4.5, 3.0, 0.3333333333333333, 5.5)") (runOutput result)

testFloat16ArithmeticPreservesRuntimeWidth :: IO ()
testFloat16ArithmeticPreservesRuntimeWidth = do
  result <-
    runSource
      defaultWarningSettings
      """
      left :: Float16.
      left = 2048.0.
      one :: Float16.
      one = 1.0.
      three :: Float16.
      three = 3.0.
      mulLeft :: Float16.
      mulLeft = 683.0.
      add16 :: Float16.
      add16 = left + one.
      sub16 :: Float16.
      sub16 = add16 - one.
      mul16 :: Float16.
      mul16 = mulLeft * three.
      div16 :: Float16.
      div16 = add16 / one.
      (add16, sub16, mul16, div16).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 2047.0, 2048.0, 2048.0)") (runOutput result)

testFloat32ArithmeticPreservesRuntimeWidth :: IO ()
testFloat32ArithmeticPreservesRuntimeWidth = do
  result <-
    runSource
      defaultWarningSettings
      """
      one :: Float32.
      one = 1.0.
      epsilon :: Float32.
      epsilon = 0.00000001.
      add32 :: Float32.
      add32 = one + epsilon.
      sub32 :: Float32.
      sub32 = add32 - epsilon.
      mul32 :: Float32.
      mul32 = one * add32.
      div32 :: Float32.
      div32 = add32 / one.
      (add32, sub32, mul32, div32).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1.0, 1.0, 1.0, 1.0)") (runOutput result)

testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic :: IO ()
testRuntimeFallbackRejectsTargetedNarrowFloatUntypedFloatArithmetic = do
  assertRuntimeErrorContains
    "runtime fallback Float16 plus untyped Float"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "+" (targetedFloat "__kernel_toFloat16") untypedFloatOne)))
  assertRuntimeErrorContains
    "runtime fallback untyped Float plus Float32"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "+" untypedFloatOne (targetedFloat "__kernel_toFloat32"))))

testKernelRejectsMixedIntegerFloatArithmetic :: IO ()
testKernelRejectsMixedIntegerFloatArithmetic = do
  assertRuntimeErrorContains
    "kernel arithmetic rejects mixed Int64 and Float64"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "+" (targetedInt "__kernel_toInt64") (targetedFloat "__kernel_toFloat64"))))
  assertRuntimeErrorContains
    "runtime fallback untyped Int plus Float16"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "+" (expressionLiteral (LInt 1)) (targetedFloat "__kernel_toFloat16"))))
  assertRuntimeErrorContains
    "kernel rejects mixed arithmetic before conversion"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "+" tooLargeFloat64Integer (targetedFloat "__kernel_toFloat64"))))

testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedFloatComparisonEquality = do
  assertRuntimeErrorContains
    "runtime fallback Float16 less-than Float32"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "<" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat32"))))
  assertRuntimeErrorContains
    "runtime fallback Float16 equality Float64"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "==" (targetedFloat "__kernel_toFloat16") (targetedFloat "__kernel_toFloat64"))))
  assertRuntimeErrorContains
    "runtime fallback Float32 inequality untyped Float"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "!=" (targetedFloat "__kernel_toFloat32") untypedFloatOne)))

testKernelRejectsMixedIntegerFloatComparison :: IO ()
testKernelRejectsMixedIntegerFloatComparison = do
  assertRuntimeErrorContains
    "runtime fallback untyped Int less-than untyped Float"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "<" (expressionLiteral (LInt 1)) untypedFloatTwo)))
  assertRuntimeErrorContains
    "runtime fallback untyped Int equality Float64"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "==" (expressionLiteral (LInt 1)) (targetedFloat "__kernel_toFloat64"))))

testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue :: IO ()
testTargetedFloat16Float32FractionalLiteralRoundsRuntimeValue = do
  result <-
    runSource
      defaultWarningSettings
      """
      x16 :: Float16.
      x16 = 2049.0.
      x32 :: Float32.
      x32 = 1.00000001.
      y16 :: @{}: Float16.
      y16 = 2049.0.
      y32 :: @{}: Float32.
      y32 = 1.00000001.
      (x16, x32, y16, y32).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0, 2048.0, 1.0)") (runOutput result)

testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue :: IO ()
testSuffixedFloat16Float32FractionalLiteralRoundsRuntimeValue = do
  result <-
    runSource
      defaultWarningSettings
      """
      x16 = 2049.0f16.
      x32 = 1.00000001f32.
      x64 = 1.5f64.
      (x16, x32, x64).
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(2048.0, 1.0, 1.5)") (runOutput result)

testFloat16ArithmeticOverflowRuntimeError :: IO ()
testFloat16ArithmeticOverflowRuntimeError = do
  result <-
    runSource
      defaultWarningSettings
      """
      left = toFloat16 65504.
      right = toFloat16 65504.
      left + right.
      """
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
      -- Explicit fragments are intentional: this program embeds a generated stress itemValue.
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
  result <-
    runSource
      defaultWarningSettings
      """
      lt = 1.5 < 2.0.
      le = 2.0 <= 2.0.
      gt = 3.0 > 2.0.
      ge = 3.0 >= 3.0.
      eq = 2.0 == 2.0.
      ne = 2.0 != 3.0.
      [lt, le, gt, ge, eq, ne].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True]") (runOutput result)

testExplicitIntegerFloat64ComparisonEqualityRuntimeSuccess :: IO ()
testExplicitIntegerFloat64ComparisonEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      integer :: Int64.
      integer = toInt64 2.
      narrow :: Int8.
      narrow = toInt8 1.
      floating :: Float64.
      floating = toFloat64 2.
      defaultFloat :: Float.
      defaultFloat = 3.0.
      [(toFloat64 integer) < defaultFloat, (toFloat64 integer) <= floating, defaultFloat > (toFloat64 narrow), floating >= (toFloat64 integer), (toFloat64 integer) == floating, defaultFloat != (toFloat64 integer)].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True]") (runOutput result)
  assertRuntimeErrorContains
    "kernel rejects mixed equality before conversion"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "==" tooLargeFloat64Integer (targetedFloat "__kernel_toFloat64"))))

testFloat16Float32ComparisonEqualityRuntimeSuccess :: IO ()
testFloat16Float32ComparisonEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      a16 = toFloat16 1.
      b16 = toFloat16 2.
      a32 = toFloat32 1.
      b32 = toFloat32 2.
      lt16 = a16 < b16.
      le16 = a16 <= a16.
      gt16 = b16 > a16.
      ge16 = b16 >= b16.
      eq16 = a16 == a16.
      ne16 = a16 != b16.
      lt32 = a32 < b32.
      le32 = a32 <= a32.
      gt32 = b32 > a32.
      ge32 = b32 >= b32.
      eq32 = a32 == a32.
      ne32 = a32 != b32.
      [lt16, le16, gt16, ge16, eq16, ne16, lt32, le32, gt32, ge32, eq32, ne32].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess :: IO ()
testTargetedFloat16Float32FractionalLiteralComparisonEqualityRuntimeSuccess = do
  result <-
    runSource
      defaultWarningSettings
      """
      a16 :: Float16.
      a16 = 1.5.
      b16 :: Float16.
      b16 = 2.25.
      a32 :: Float32.
      a32 = 1.5.
      b32 :: Float32.
      b32 = 2.25.
      [a16 < b16, a16 <= a16, b16 > a16, b16 >= b16, a16 == a16, a16 != b16, a32 < b32, a32 <= a32, b32 > a32, b32 >= b32, a32 == a32, a32 != b32].
      """
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[True, True, True, True, True, True, True, True, True, True, True, True]") (runOutput result)

testRuntimeFallbackRejectsMixedTargetedIntegerEquality :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerEquality =
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer equality"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "==" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))

testRuntimeFallbackRejectsMixedTargetedIntegerComparison :: IO ()
testRuntimeFallbackRejectsMixedTargetedIntegerComparison = do
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer less-than"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "<" (targetedInt "__kernel_toInt8") (targetedInt "__kernel_toUInt8"))))
  assertRuntimeErrorContains
    "runtime fallback targeted UInt8 less-or-equal untyped out-of-range Int"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary "<=" (targetedInt "__kernel_toUInt8") (expressionLiteral (LInt 256)))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-than"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary ">" (targetedInt "__kernel_toUInt8") (targetedInt "__kernel_toInt16"))))
  assertRuntimeErrorContains
    "runtime fallback mixed targeted integer greater-or-equal"
    "E3007"
    (evaluateFixture (runtimeExpr (expressionKernelBinary ">=" (targetedInt "__kernel_toUInt16") (targetedInt "__kernel_toInt8"))))

testFloat16ConversionRoundsRuntimeValue :: IO ()
testFloat16ConversionRoundsRuntimeValue = do
  result <- runSource defaultWarningSettings "toFloat16 2049."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "2048.0") (runOutput result)

testDynamicIntegerConversionRangeRuntimeError :: IO ()
testDynamicIntegerConversionRangeRuntimeError = do
  result <-
    runSource
      defaultWarningSettings
      """
      x :: Int.
      x = 256.
      toUInt8 x.
      """
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
  let result = evaluateFixture (runtimeExpr (expressionApply (expressionVariable "__kernel_toInt8") (expressionLiteral (LBool True))))
  assertRuntimeErrorContains "runtime fallback conversion non-numeric" "E3024" result

testTypedNumericSectionPreservesCapturedOperandFlexibility :: IO ()
testTypedNumericSectionPreservesCapturedOperandFlexibility = do
  result <-
    runSource
      defaultWarningSettings
      ( """
        add8 :: UInt8 -> UInt8.
        add8 = (+ 1).
        add8 (toUInt8 2).
        """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testDefaultedIntegerBindingHintRejectsOutsideInt64Range :: IO ()
testDefaultedIntegerBindingHintRejectsOutsideInt64Range = do
  result <-
    runSource
      defaultWarningSettings
      """
      itemValue = 18446744073709551616.
      itemValue.
      """
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
