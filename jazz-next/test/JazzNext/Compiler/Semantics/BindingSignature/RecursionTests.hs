{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.RecursionTests
  ( recursionTests
  ) where

import JazzNext.Compiler.Driver
  ( CompileResult (..),
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
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> True.\n}.\n"
        <> "impl C(Bool) {\nm = \\(x) -> False.\n}.\n"
        <> "left = if True then \\(x) -> C::m x else right.\n"
        <> "right = if False then \\(x) -> C::m x else left.\n"
        <> "intResult = left 1.\n"
        <> "boolResult = right True."
    )

testSourceKeepsNestedRecursiveHelperInferredMethodObligationsScoped :: IO ()
testSourceKeepsNestedRecursiveHelperInferredMethodObligationsScoped =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> True.\n}.\n"
        <> "outer = { f = if True then \\(x) -> g x else g. g = if False then \\(y) -> C::m y else f. 1. }.\n"
        <> "outer."
    )

testSourceInstantiatesRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesRecursiveBindingSchemesPerUse =
  assertSourceOk "choose = if True then \\(x) -> x else choose.\nintValue = choose 1.\nboolValue = choose True."

testSourceInstantiatesMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesMutualRecursiveBindingSchemesPerUse =
  assertSourceOk "left = if True then \\(x) -> x else right.\nright = if False then \\(x) -> x else left.\nintValue = left 1.\nboolValue = right True."

testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse =
  assertSourceOk "left = if True then \\(x) -> x else right.\nintValue = left 1.\nright = if False then \\(x) -> x else left.\nboolValue = right True."

testSourceKeepsLaterRebindingOverRecursiveScheme :: IO ()
testSourceKeepsLaterRebindingOverRecursiveScheme =
  assertSourceSingleErrorContains
    "left = if True then \\(x) -> x else right.\nright = if False then \\(x) -> x else left.\nleft = \\(x) -> x + 1.\nbad = left True."
    "cannot apply function of type Int64 -> Int64 to argument of type Bool"

testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember :: IO ()
testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember =
  assertSourceSingleErrorContains
    "left = if True then \\(x) -> x else right.\nbad = left True.\nright = \\(x) -> left (x + 1)."
    "cannot apply function of type Int64 -> Int64 to argument of type Bool"

testSourceTypesRecursiveGuardsAgainstPriorRebinding :: IO ()
testSourceTypesRecursiveGuardsAgainstPriorRebinding =
  assertSourceOk "f = \\(x) -> x.\nf = case 0 { | 0 if f True -> \\(y) -> y | _ -> \\(y) -> y }.\nvalue = f 1.\nvalue."

testSourceDefersPartialRecursivePreviewsPastInterveningDependencies :: IO ()
testSourceDefersPartialRecursivePreviewsPastInterveningDependencies =
  assertSourceErrorContains
    "left = if True then \\(x) -> x else right.\nearly = left True.\nhelper = \\(x) -> x + 1.\nright = \\(x) -> left (helper x).\nearly."
    "cannot apply function"

testSourcePreviewsThroughInterveningRecursiveGroupMembers :: IO ()
testSourcePreviewsThroughInterveningRecursiveGroupMembers =
  assertSourceOk "left = if True then \\(x) -> x else right.\nearly = left True.\nmiddle = if True then \\(x) -> x else left.\nright = if False then middle else left.\nlate = left 1.\nlate."

testSourceRejectsNonRecursiveForwardReference :: IO ()
testSourceRejectsNonRecursiveForwardReference =
  assertSourceErrorContains "x = y.\ny = 1.\nx." "E1001"

testSourceRejectsRetroactiveRebindingRecursion :: IO ()
testSourceRejectsRetroactiveRebindingRecursion =
  assertSourceErrorContains "x = y.\ny = 1.\ny = x.\nx." "E1001"

testSourceAcceptsMutualRecursionGroup :: IO ()
testSourceAcceptsMutualRecursionGroup =
  assertSourceOk "even = odd.\nodd = even.\neven."

testSourceInstantiatesRecursiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesRecursiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nchoose :: @{Eq(a)}: a -> a.\nchoose = if True then \\(x) -> x else choose.\nintValue = choose 1.\nboolValue = choose True."

testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews :: IO ()
testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews = do
  result <- compileExpr defaultWarningSettings speculativePreviewDeferredConstraintProgram
  assertEqual "compile errors" [] (compileErrors result)

testSourceReportsSignedRecursiveRhsTypeError :: IO ()
testSourceReportsSignedRecursiveRhsTypeError =
  assertSourceSingleErrorContains "x :: Bool.\nx = x + 1." "E2003"
