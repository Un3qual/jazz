{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.PrimitiveSemantics.EqualityOperator
  ( basicEqualityTests,
    primitiveMismatchTests,
    structuralEqualityTests,
    operatorTests
  )
where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..)
  )
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource
  )
import JazzNext.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCallableEqualityRejected,
    assertCallableEqualityRejectedWithBundledPrelude,
    assertCompileError,
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
    assertContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest
  )
import System.Timeout
  ( timeout
  )

basicEqualityTests :: [NamedTest]
basicEqualityTests =
  [ ("strict equality accepts same-type Int operands", testAcceptsIntEquality),
    ("strict equality accepts same-type Bool operands", testAcceptsBoolEquality),
    ("source pipeline accepts Char and Text equality", testSourcePipelineAcceptsCharTextEquality),
    ("source pipeline rejects Char/Text equality mismatch", testSourcePipelineRejectsCharTextMismatch),
    ("source pipeline accepts Char/Text equality values and sections", testSourcePipelineAcceptsCharTextEqualityValuesAndSections),
    ("source pipeline types Char and Text literal patterns", testSourcePipelineTypesCharTextPatterns)
  ]

primitiveMismatchTests :: [NamedTest]
primitiveMismatchTests =
  [ ("strict equality rejects mismatched operand types", testRejectsEqualityTypeMismatch),
    ("strict inequality rejects mismatched operand types", testRejectsInequalityTypeMismatch),
    ("comparison primitives reject non-Int operands", testRejectsComparisonTypeMismatch)
  ]

structuralEqualityTests :: [NamedTest]
structuralEqualityTests =
  [ ("source pipeline accepts equality section application", testSourcePipelineAcceptsEqualitySection),
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
    ("source pipeline rejects equality over bundled callable values", testSourcePipelineRejectsBundledCallableEquality)
  ]

operatorTests :: [NamedTest]
operatorTests =
  [ ("source pipeline rejects equality section mismatched application", testSourcePipelineRejectsEqualitySectionTypeMismatch),
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
    ("source pipeline rejects non-callable declared user operator binding", testSourcePipelineRejectsNonCallableDeclaredUserOperatorBinding)
  ]

testAcceptsIntEquality :: IO ()
testAcceptsIntEquality = do
  result <- compileExpr defaultWarningSettings intEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)

testAcceptsBoolEquality :: IO ()
testAcceptsBoolEquality = do
  result <- compileExpr defaultWarningSettings boolEqualityProgram
  assertEqual "compile errors" [] (compileErrors result)

testSourcePipelineAcceptsCharTextEquality :: IO ()
testSourcePipelineAcceptsCharTextEquality = do
  assertCompiles "same = 'a' == 'a'. different = 'a' != 'b'."
  assertCompiles "same = \"Jazz\" == \"Jazz\". different = \"Jazz\" != \"jazz\"."

testSourcePipelineRejectsCharTextMismatch :: IO ()
testSourcePipelineRejectsCharTextMismatch = do
  result <- compileSource defaultWarningSettings "bad = 'a' == \"a\"."
  assertSingleDiagnosticContains "Char/Text mismatch" "E2004" (compileErrors result)

testSourcePipelineAcceptsCharTextEqualityValuesAndSections :: IO ()
testSourcePipelineAcceptsCharTextEqualityValuesAndSections =
  assertCompiles
    "eq = (==). char = eq 'a' 'a'. text = (\"Jazz\" ==) \"Jazz\". other = (!= \"Jazz\") \"jazz\"."

testSourcePipelineTypesCharTextPatterns :: IO ()
testSourcePipelineTypesCharTextPatterns = do
  assertCompiles "x = case 'a' { | 'a' -> True | _ -> False }."
  assertCompiles "x = case \"Jazz\" { | \"Jazz\" -> True | _ -> False }."
  result <- compileSource defaultWarningSettings "x = case 'a' { | \"a\" -> True | _ -> False }."
  assertSingleDiagnosticContains "Char/Text pattern mismatch" "E2011" (compileErrors result)

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
