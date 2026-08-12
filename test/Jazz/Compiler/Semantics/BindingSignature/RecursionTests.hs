{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Jazz.Compiler.Semantics.BindingSignature.RecursionTests
  ( recursionTests
  ) where

import Control.Exception
  ( ErrorCall,
    try
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import Jazz.Compiler.Analyzer
  ( AnalysisInputs (..),
    AnalysisResult (..)
  )
import qualified Jazz.Compiler.Analyzer as Analyzer
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly)
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    isErrorDiagnostic
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileExpr
  )
import Jazz.Compiler.RecursiveBindings
  ( prepareRecursiveScope
  )
import Jazz.Compiler.Semantics.BindingSignature.Shared
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest
  )
import Language.Haskell.TH
  ( lookupValueName
  )

$( do
     legacyEntryPoint <- lookupValueName "Analyzer.analyzeProgramWithInputsAndScopeFacts"
     case legacyEntryPoint of
       Nothing -> pure []
       Just _ ->
         fail
           "Analyzer.analyzeProgramWithInputsAndScopeFacts must remain unavailable; use the owned PreparedRecursiveScope entry point"
 )

recursionTests :: [NamedTest]
recursionTests =
  [ ("self-recursive binding is accepted", testSelfRecursiveBinding)
    , ("mutual recursion group is accepted", testMutualRecursionGroup)
    , ("three-node mutual recursion group is accepted", testThreeNodeMutualRecursionGroup)
    , ("non-recursive forward reference in bindings is rejected", testNonRecursiveForwardReference)
    , ("prepared analyzer scopes cannot cross-pair statements and facts", testPreparedScopesCannotCrossPairStatementsAndFacts)
    , ("prepared analyzer scopes rederive facts for current outer bindings", testPreparedAnalyzerScopeRederivesForOuterBindings)
    , ("ordinary roots stay lazy while owned prepared statements detach", testAnalyzerRootLaziness)
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
    , ("source pipeline previews overlapping interleaved recursive groups", testSourcePreviewsOverlappingInterleavedRecursiveGroups)
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

testPreparedScopesCannotCrossPairStatementsAndFacts :: IO ()
testPreparedScopesCannotCrossPairStatementsAndFacts = do
  AnalysisResult recursiveExpr recursiveDiagnostics <-
    Analyzer.analyzeProgramWithInputsAndPreparedScope
      analysisInputs
      Set.empty
      (prepareRecursiveScope Set.empty recursiveStatements)
  AnalysisResult forwardExpr forwardDiagnostics <-
    Analyzer.analyzeProgramWithInputsAndPreparedScope
      analysisInputs
      Set.empty
      (prepareRecursiveScope Set.empty forwardStatements)
  assertEqual
    "recursive prepared expression"
    (EBlock recursiveStatements)
    recursiveExpr
  assertEqual
    "recursive prepared diagnostics"
    []
    (filter isErrorDiagnostic recursiveDiagnostics)
  assertEqual
    "forward prepared expression"
    (EBlock forwardStatements)
    forwardExpr
  assertSingleDiagnosticContains
    "prepared scope forward reference"
    "unbound variable 'y'"
    (filter isErrorDiagnostic forwardDiagnostics)
  where
    recursiveStatements =
      [ SLet "left" (SourceSpan 1 1) (EVar "right"),
        SLet "right" (SourceSpan 2 1) (EVar "left"),
        SExpr (SourceSpan 3 1) (EVar "left")
      ]
    forwardStatements =
      [ SLet "x" (SourceSpan 1 1) (EVar "y"),
        SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
        SExpr (SourceSpan 3 1) (EVar "x")
      ]

testPreparedAnalyzerScopeRederivesForOuterBindings :: IO ()
testPreparedAnalyzerScopeRederivesForOuterBindings = do
  ordinaryResult <-
    Analyzer.analyzeProgramWithInputs
      analysisInputs
      Set.empty
      (EBlock statements)
  preparedResult <-
    Analyzer.analyzeProgramWithInputsAndPreparedScope
      analysisInputs
      Set.empty
      (prepareRecursiveScope (Set.singleton "self") statements)
  assertEqual "prepared scope under current inputs" ordinaryResult preparedResult
  where
    statements =
      [ SLet "self" (SourceSpan 1 1) (EVar "self"),
        SExpr (SourceSpan 2 1) (EVar "self")
      ]

testAnalyzerRootLaziness :: IO ()
testAnalyzerRootLaziness = do
  ordinaryOutcome <-
    try
      ( Analyzer.analyzeProgramWithInputs
          analysisInputs
          Set.empty
          (error "ordinary analyzer root was forced")
      ) :: IO (Either ErrorCall AnalysisResult)
  case ordinaryOutcome of
    Left _ -> failTest "expected ordinary analyzer roots to remain lazy"
    Right _ -> pure ()

  preparedOutcome <-
    try
      ( Analyzer.analyzeProgramWithInputsAndPreparedScope
          analysisInputs
          Set.empty
          (prepareRecursiveScope Set.empty (error "prepared statements were retained lazily"))
      ) :: IO (Either ErrorCall AnalysisResult)
  case preparedOutcome of
    Left _ -> pure ()
    Right _ -> failTest "expected the analyzer boundary to force its prepared statements"

analysisInputs :: AnalysisInputs
analysisInputs =
  AnalysisInputs
    { analysisBuiltinMode = ResolveKernelOnly,
      analysisWarningSettings = defaultWarningSettings,
      analysisImportedValues = Map.empty,
      analysisForwardFunctions = Map.empty,
      analysisImportedClasses = Set.empty,
      analysisModulePath = Nothing
    }

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

testSourcePreviewsOverlappingInterleavedRecursiveGroups :: IO ()
testSourcePreviewsOverlappingInterleavedRecursiveGroups =
  assertSourceOk """
  aLeft = if True then \\(x) -> x else aRight.
  bLeft = if True then \\(x) -> x else bRight.
  probe = (aLeft True, bLeft 1).
  aRight = if False then \\(x) -> x else aLeft.
  bRight = if False then \\(x) -> x else bLeft.
  probe.
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
