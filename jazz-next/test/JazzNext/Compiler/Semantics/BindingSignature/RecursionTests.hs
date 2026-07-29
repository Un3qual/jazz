{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.RecursionTests
  ( recursionTests
  ) where

import JazzNext.Compiler.Driver
  ( compileErrors,
    compileExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticContains
  )
import JazzNext.Compiler.Semantics.BindingSignature.Shared

recursionTests :: [NamedTest]
recursionTests =
  [ ("self-recursive binding is accepted", testSelfRecursiveBinding)
    , ("mutual recursion group is accepted", testMutualRecursionGroup)
    , ("three-node mutual recursion group is accepted", testThreeNodeMutualRecursionGroup)
    , ("non-recursive forward reference in bindings is rejected", testNonRecursiveForwardReference)
    , ("rebinding cannot retroactively create recursion group", testRebindingDoesNotCreateRetroactiveRecursion)
    , ("source pipeline preserves inferred method constraints across mutual recursion", testSourcePreservesInferredMethodConstraintsAcrossMutualRecursion)
    , ("source pipeline keeps nested recursive helper inferred method obligations scoped", testSourceKeepsNestedRecursiveHelperInferredMethodObligationsScoped)
    , ("source pipeline instantiates recursive binding schemes per use", testSourceInstantiatesRecursiveBindingSchemesPerUse)
    , ("source pipeline instantiates mutual recursive binding schemes per use", testSourceInstantiatesMutualRecursiveBindingSchemesPerUse)
    , ("source pipeline instantiates interleaved mutual recursive schemes per use", testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse)
    , ("source pipeline keeps later rebinding over recursive scheme", testSourceKeepsLaterRebindingOverRecursiveScheme)
    , ("source pipeline rejects interleaved use constrained by later recursive member", testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember)
    , ("source pipeline types recursive guards against prior rebinding", testSourceTypesRecursiveGuardsAgainstPriorRebinding)
    , ("source pipeline defers partial recursive previews past intervening dependencies", testSourceDefersPartialRecursivePreviewsPastInterveningDependencies)
    , ("source pipeline previews through intervening recursive group members", testSourcePreviewsThroughInterveningRecursiveGroupMembers)
    , ("source pipeline rejects non-recursive forward reference", testSourceRejectsNonRecursiveForwardReference)
    , ("source pipeline rejects retroactive rebinding recursion", testSourceRejectsRetroactiveRebindingRecursion)
    , ("source pipeline accepts mutual recursion group", testSourceAcceptsMutualRecursionGroup)
    , ("source pipeline instantiates recursive constrained signatures per use", testSourceInstantiatesRecursiveConstrainedSignaturePerUse)
    , ("source pipeline discards speculative deferred constraints from recursive previews", testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews)
    , ("source pipeline does not duplicate inferred constraints from recursive previews", testSourceDoesNotDuplicateInferredConstraintsFromRecursivePreviews)
    , ("source pipeline reports signed recursive rhs type errors", testSourceReportsSignedRecursiveRhsTypeError)
  ]

testSelfRecursiveBinding :: IO ()
testSelfRecursiveBinding = do
  result <- compileExpr defaultWarningSettings selfRecursiveProgram
  assertEqual "compile errors" [] (compileErrors result)

testMutualRecursionGroup :: IO ()
testMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings mutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)

testThreeNodeMutualRecursionGroup :: IO ()
testThreeNodeMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings threeNodeMutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)

testNonRecursiveForwardReference :: IO ()
testNonRecursiveForwardReference = do
  result <- compileExpr defaultWarningSettings nonRecursiveForwardReferenceProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

testRebindingDoesNotCreateRetroactiveRecursion :: IO ()
testRebindingDoesNotCreateRetroactiveRecursion = do
  result <- compileExpr defaultWarningSettings retroactiveRebindingProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

testSourcePreservesInferredMethodConstraintsAcrossMutualRecursion :: IO ()
testSourcePreservesInferredMethodConstraintsAcrossMutualRecursion =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> True.
    }.
    impl C(Bool) {
    m = \\(x) -> False.
    }.
    left = if True then \\(x) -> C::m x else right.
    right = if False then \\(x) -> C::m x else left.
    intResult = left 1.
    boolResult = right True.
    """
    )

testSourceKeepsNestedRecursiveHelperInferredMethodObligationsScoped :: IO ()
testSourceKeepsNestedRecursiveHelperInferredMethodObligationsScoped =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> True.
    }.
    outer = { f = if True then \\(x) -> g x else g. g = if False then \\(y) -> C::m y else f. 1. }.
    outer.
    """
    )

testSourceInstantiatesRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesRecursiveBindingSchemesPerUse =
  assertSourceOk """
  choose = if True then \\(x) -> x else choose.
  intValue = choose 1.
  boolValue = choose True.
  """

testSourceInstantiatesMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesMutualRecursiveBindingSchemesPerUse =
  assertSourceOk """
  left = if True then \\(x) -> x else right.
  right = if False then \\(x) -> x else left.
  intValue = left 1.
  boolValue = right True.
  """

testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse =
  assertSourceOk """
  left = if True then \\(x) -> x else right.
  intValue = left 1.
  right = if False then \\(x) -> x else left.
  boolValue = right True.
  """

testSourceKeepsLaterRebindingOverRecursiveScheme :: IO ()
testSourceKeepsLaterRebindingOverRecursiveScheme =
  assertSourceSingleErrorContains
    """
    left = if True then \\(x) -> x else right.
    right = if False then \\(x) -> x else left.
    left = \\(x) -> x + 1.
    bad = left True.
    """
    "cannot apply function of type Int64 -> Int64 to argument of type Bool"

testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember :: IO ()
testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember =
  assertSourceSingleErrorContains
    """
    left = if True then \\(x) -> x else right.
    bad = left True.
    right = \\(x) -> left (x + 1).
    """
    "cannot apply function of type Int64 -> Int64 to argument of type Bool"

testSourceTypesRecursiveGuardsAgainstPriorRebinding :: IO ()
testSourceTypesRecursiveGuardsAgainstPriorRebinding =
  assertSourceOk """
  f = \\(x) -> x.
  f = case 0 { | 0 if f True -> \\(y) -> y | _ -> \\(y) -> y }.
  candidate = f 1.
  candidate.
  """

testSourceDefersPartialRecursivePreviewsPastInterveningDependencies :: IO ()
testSourceDefersPartialRecursivePreviewsPastInterveningDependencies =
  assertSourceErrorContains
    """
    left = if True then \\(x) -> x else right.
    early = left True.
    helper = \\(x) -> x + 1.
    right = \\(x) -> left (helper x).
    early.
    """
    "cannot apply function"

testSourcePreviewsThroughInterveningRecursiveGroupMembers :: IO ()
testSourcePreviewsThroughInterveningRecursiveGroupMembers =
  assertSourceOk """
  left = if True then \\(x) -> x else right.
  early = left True.
  middle = if True then \\(x) -> x else left.
  right = if False then middle else left.
  late = left 1.
  late.
  """

testSourceRejectsNonRecursiveForwardReference :: IO ()
testSourceRejectsNonRecursiveForwardReference =
  assertSourceErrorContains """
  x = y.
  y = 1.
  x.
  """ "E1001"

testSourceRejectsRetroactiveRebindingRecursion :: IO ()
testSourceRejectsRetroactiveRebindingRecursion =
  assertSourceErrorContains """
  x = y.
  y = 1.
  y = x.
  x.
  """ "E1001"

testSourceAcceptsMutualRecursionGroup :: IO ()
testSourceAcceptsMutualRecursionGroup =
  assertSourceOk """
  even = odd.
  odd = even.
  even.
  """

testSourceInstantiatesRecursiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesRecursiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Bool) { }.
  choose :: @{Eq(a)}: a -> a.
  choose = if True then \\(x) -> x else choose.
  intValue = choose 1.
  boolValue = choose True.
  """

testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews :: IO ()
testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews = do
  result <- compileExpr defaultWarningSettings speculativePreviewDeferredConstraintProgram
  assertEqual "compile errors" [] (compileErrors result)

testSourceDoesNotDuplicateInferredConstraintsFromRecursivePreviews :: IO ()
testSourceDoesNotDuplicateInferredConstraintsFromRecursivePreviews = do
  result <- compileExpr defaultWarningSettings speculativePreviewInferredConstraintProgram
  assertSingleDiagnosticContains "compile errors" "missing impl fact 'C(Bool)'" (compileErrors result)

testSourceReportsSignedRecursiveRhsTypeError :: IO ()
testSourceReportsSignedRecursiveRhsTypeError =
  assertSourceSingleErrorContains """
  x :: Bool.
  x = x + 1.
  """ "E2003"
