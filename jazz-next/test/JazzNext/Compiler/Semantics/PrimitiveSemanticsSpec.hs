{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
  ( SourceSpan (..),
    renderDiagnostic
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
    assertContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
    runTestSuite
  )
import System.Timeout
  ( timeout
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
    ("source pipeline accepts deferred direct equality once constrained", testSourcePipelineAcceptsDeferredDirectEquality),
    ("source pipeline accepts structural list equality", testSourcePipelineAcceptsStructuralListEquality),
    ("source pipeline accepts structural tuple equality", testSourcePipelineAcceptsStructuralTupleEquality),
    ("source pipeline accepts structural ADT equality", testSourcePipelineAcceptsStructuralAdtEquality),
    ("source pipeline accepts self-referential structural ADT equality", testSourcePipelineAcceptsSelfReferentialStructuralAdtEquality),
    ("source pipeline accepts structural equality sections", testSourcePipelineAcceptsStructuralEqualitySections),
    ("source pipeline rejects structural equality with function elements", testSourcePipelineRejectsStructuralFunctionEquality),
    ("source pipeline rejects structural ADT equality with function payloads", testSourcePipelineRejectsStructuralAdtFunctionEquality),
    ("source pipeline rejects duplicate ADT declarations before structural equality", testSourcePipelineRejectsDuplicateAdtDeclarationBeforeStructuralEquality),
    ("source pipeline rejects structural ADT equality for partial constructors", testSourcePipelineRejectsStructuralAdtPartialConstructorEquality),
    ("source pipeline rejects structural ADT equality across different types", testSourcePipelineRejectsStructuralAdtTypeMismatch),
    ("source pipeline rejects equality over operator-section callable values", testSourcePipelineRejectsOperatorSectionCallableEquality),
    ("source pipeline rejects equality over bare operator callable values", testSourcePipelineRejectsBareOperatorCallableEquality),
    ("source pipeline rejects equality over bundled callable values", testSourcePipelineRejectsBundledCallableEquality),
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
    ("source pipeline keeps builtin pipe off declared operator binding path", testSourcePipelineKeepsBuiltinPipeOffDeclaredOperatorBindingPath),
    ("source pipeline accepts declared user operator infix binding", testSourcePipelineAcceptsDeclaredUserOperatorInfixBinding),
    ("source pipeline accepts declared user operator signature", testSourcePipelineAcceptsDeclaredUserOperatorSignature),
    ("source pipeline rejects declared user operator signature mismatch", testSourcePipelineRejectsDeclaredUserOperatorSignatureMismatch),
    ("source pipeline rejects non-adjacent declared user operator signature", testSourcePipelineRejectsNonAdjacentDeclaredUserOperatorSignature),
    ("source pipeline accepts declared user operator value application", testSourcePipelineAcceptsDeclaredUserOperatorValueApplication),
    ("source pipeline rejects declared user operator without binding", testSourcePipelineRejectsDeclaredUserOperatorWithoutBinding),
    ("source pipeline rejects non-callable declared user operator binding", testSourcePipelineRejectsNonCallableDeclaredUserOperatorBinding),
    ("source pipeline rejects mixed-type list literals", testSourcePipelineRejectsMixedTypeListLiteral),
    ("source pipeline accepts target-named integer conversions", testSourcePipelineAcceptsTargetNamedIntegerConversions),
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
    ("source pipeline accepts same-width Float16 and Float32 arithmetic", testSourcePipelineAcceptsSameWidthFloat16Float32Arithmetic),
    ("source pipeline accepts targeted Float16 and Float32 arithmetic", testSourcePipelineAcceptsTargetedFloat16Float32Arithmetic),
    ("source pipeline accepts Float16 and Float32 arithmetic boundary values", testSourcePipelineAcceptsFloat16Float32ArithmeticBoundaryValues),
    ("source pipeline accepts same-width Float64 comparison and equality", testSourcePipelineAcceptsSameWidthFloat64ComparisonEquality),
    ("source pipeline accepts direct typed integer to Float64 comparison and equality", testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEquality),
    ("source pipeline accepts direct typed integer to Float64 comparison/equality operator aliases", testSourcePipelineAcceptsDirectTypedIntegerFloat64ComparisonEqualityOperatorAliases),
    ("source pipeline accepts same-width Float16 and Float32 comparison and equality", testSourcePipelineAcceptsSameWidthFloat16Float32ComparisonEquality),
    ("source pipeline accepts same-width Float64 comparison/equality operator values", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualityOperatorValues),
    ("source pipeline accepts same-width Float64 comparison/equality sections", testSourcePipelineAcceptsSameWidthFloat64ComparisonEqualitySections),
    ("source pipeline rejects mixed-width float comparison and equality", testSourcePipelineRejectsMixedWidthFloatComparisonEquality),
    ("source pipeline rejects implicit Float16 and Float32 comparison and equality", testSourcePipelineRejectsImplicitFloat16Float32ComparisonEquality),
    ("source pipeline rejects implicit integer and Float64 comparison and equality", testSourcePipelineRejectsImplicitIntegerFloat64ComparisonEquality),
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

testSourcePipelineAcceptsSelfReferentialStructuralAdtEquality :: IO ()
testSourcePipelineAcceptsSelfReferentialStructuralAdtEquality = do
  maybeResult <-
    timeout
      2000000
      ( compileSource
          defaultWarningSettings
          "data IntList = Nil | Cons value rest.\nleft = Cons 1 Nil.\nright = Cons 1 Nil.\nsame = left == right."
      )
  case maybeResult of
    Nothing ->
      failTest "expected self-referential ADT equality support check to terminate, but compilation timed out"
    Just result ->
      assertEqual "compile errors" [] (compileErrors result)

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

testSourcePipelineRejectsDuplicateAdtDeclarationBeforeStructuralEquality :: IO ()
testSourcePipelineRejectsDuplicateAdtDeclarationBeforeStructuralEquality = do
  result <-
    compileSource
      defaultWarningSettings
      "data Box a = Box a.\ndata Box a = Empty.\nf = Box (\\(x) -> x).\ng = Box (\\(x) -> x).\nok = f == g."
  assertContains
    "duplicate ADT declaration before equality metadata overwrite"
    "E2014"
    (Text.unlines (map renderDiagnostic (compileErrors result)))

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

testSourcePipelineRejectsOperatorSectionCallableEquality :: IO ()
testSourcePipelineRejectsOperatorSectionCallableEquality = do
  assertCallableEqualityRejected
    "left operator section equality"
    "left = (1 +).\nright = (1 +).\nsame = left == right."
  assertCallableEqualityRejected
    "right operator section inequality"
    "left = (+ 1).\nright = (+ 1).\ndifferent = left != right."

testSourcePipelineRejectsBareOperatorCallableEquality :: IO ()
testSourcePipelineRejectsBareOperatorCallableEquality = do
  assertCallableEqualityRejected
    "bare arithmetic operator equality"
    "same = (+) == (+)."
  assertCallableEqualityRejected
    "bare equality operator inequality"
    "different = (==) != (==)."

testSourcePipelineRejectsBundledCallableEquality :: IO ()
testSourcePipelineRejectsBundledCallableEquality = do
  assertCallableEqualityRejectedWithBundledPrelude
    "bundled builtin equality"
    "same = hd == hd."
  assertCallableEqualityRejectedWithBundledPrelude
    "bundled builtin inequality"
    "different = map != map."

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

testSourcePipelineAcceptsDeferredDirectEquality :: IO ()
testSourcePipelineAcceptsDeferredDirectEquality =
  assertCompilesWithBundledPrelude
    "value = hd [].\nsame = value == value.\nsum = value + 1.\nsum."

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

testSourcePipelineKeepsBuiltinPipeOffDeclaredOperatorBindingPath :: IO ()
testSourcePipelineKeepsBuiltinPipeOffDeclaredOperatorBindingPath = do
  result <- compileSource defaultWarningSettings "x = True | False."
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "builtin pipe diagnostic code" "E2003" rendered
      assertContains "builtin pipe diagnostic text" "cannot apply operator '|'" rendered
      if "E2010" `Text.isInfixOf` rendered || "has no executable binding" `Text.isInfixOf` rendered
        then failTest "builtin pipe incorrectly used declared-operator missing-binding path"
        else pure ()
    _ -> failTest "expected exactly one builtin pipe type diagnostic"

testSourcePipelineAcceptsDeclaredUserOperatorInfixBinding :: IO ()
testSourcePipelineAcceptsDeclaredUserOperatorInfixBinding =
  assertCompiles
    "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left + right.\nx = 1 %% 2."

testSourcePipelineAcceptsDeclaredUserOperatorSignature :: IO ()
testSourcePipelineAcceptsDeclaredUserOperatorSignature =
  assertCompiles
    "operator %% tier 2.\n(%%) :: Int -> Int -> Int.\n(%%) = \\(left) -> \\(right) -> left + right.\nx = 1 %% 2."

testSourcePipelineRejectsDeclaredUserOperatorSignatureMismatch :: IO ()
testSourcePipelineRejectsDeclaredUserOperatorSignatureMismatch =
  assertCompileError
    "operator %% tier 2.\n(%%) :: Int -> Int -> Bool.\n(%%) = \\(left) -> \\(right) -> left + right.\nx = 1 %% 2."
    "declared user operator signature mismatch"
    "E2005"

testSourcePipelineRejectsNonAdjacentDeclaredUserOperatorSignature :: IO ()
testSourcePipelineRejectsNonAdjacentDeclaredUserOperatorSignature =
  assertCompileError
    "operator %% tier 2.\n(%%) :: Int -> Int -> Int.\ngap = 0.\n(%%) = \\(left) -> \\(right) -> toFloat64 1.\nx = 1 %% 2."
    "declared user operator signature adjacency"
    "must annotate the next binding with the same name"

testSourcePipelineAcceptsDeclaredUserOperatorValueApplication :: IO ()
testSourcePipelineAcceptsDeclaredUserOperatorValueApplication =
  assertCompiles
    "operator %% tier 2.\n(%%) = \\(left) -> \\(right) -> left == right.\nx = (%%) 1 1."

testSourcePipelineRejectsDeclaredUserOperatorWithoutBinding :: IO ()
testSourcePipelineRejectsDeclaredUserOperatorWithoutBinding = do
  result <- compileSource defaultWarningSettings "operator %% tier 2.\nx = 1 %% 2."
  assertSingleDiagnosticContains
    "declared user operator missing binding code"
    "E2010"
    (compileErrors result)
  assertSingleDiagnosticContains
    "declared user operator missing binding text"
    "operator '%%' has no executable binding"
    (compileErrors result)

testSourcePipelineRejectsNonCallableDeclaredUserOperatorBinding :: IO ()
testSourcePipelineRejectsNonCallableDeclaredUserOperatorBinding = do
  result <- compileSource defaultWarningSettings "operator %% tier 2.\n(%%) = 1.\nx = 1 %% 2."
  assertSingleDiagnosticContains
    "declared user operator non-callable binding code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "declared user operator non-callable binding text"
    "cannot apply function of type"
    (compileErrors result)

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

testSourcePipelineRejectsImplicitIntegerFloat64ComparisonEquality :: IO ()
testSourcePipelineRejectsImplicitIntegerFloat64ComparisonEquality = do
  assertCompileError
    "x = 1 < 1.5."
    "integer literal Float64-domain comparison"
    "E2003"
  assertCompileError
    "x = 1 == 1.0."
    "integer literal Float64-domain equality"
    "E2004"
  assertCompileErrorWithBundledPrelude
    "left = toFloat64 1.\nx = left < 2."
    "implicit integer-to-Float64 comparison"
    "E2003"
  assertCompileErrorWithBundledPrelude
    "left = toFloat64 1.\nx = left == 1."
    "implicit integer-to-Float64 equality"
    "E2004"
  assertCompileErrorWithBundledPrelude
    "x = toFloat64 1 == 1."
    "toFloat64 integer literal equality"
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

assertCallableEqualityRejected :: String -> Text.Text -> IO ()
assertCallableEqualityRejected failureLabel source = do
  result <- compileSource defaultWarningSettings source
  assertCallableEqualityDiagnostic failureLabel result

assertCallableEqualityRejectedWithBundledPrelude :: String -> Text.Text -> IO ()
assertCallableEqualityRejectedWithBundledPrelude failureLabel source = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) source
  assertCallableEqualityDiagnostic failureLabel result

assertCallableEqualityDiagnostic :: String -> CompileResult -> IO ()
assertCallableEqualityDiagnostic failureLabel result = do
  assertSingleDiagnosticContains
    (Text.pack (failureLabel <> " code"))
    "E2004"
    (compileErrors result)
  assertSingleDiagnosticContains
    (Text.pack (failureLabel <> " callable text"))
    "callable values are not equality-supported"
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
