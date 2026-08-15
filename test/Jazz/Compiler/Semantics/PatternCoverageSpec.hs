{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (..))
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    diagnosticCode,
    diagnosticSummary,
    isErrorDiagnostic,
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileSource,
    compileWarnings,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ConstructorNamespace),
    mkIdentifier,
    resolvedImportedName,
  )
import Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    PatternCoverageFailure (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    emptyConstructorInventory,
    renderCoveragePattern,
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    InferenceResult (..),
    inferExpressionWithInputs,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    TypeBinding (..),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings,
    resolveWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )
import System.Timeout (timeout)

main :: IO ()
main = runTestSuite "PatternCoverage" tests

tests :: [NamedTest]
tests =
  [ ("empty Bool match reports the first missing constructor", testEmptyBoolMatch),
    ("both Bool constructors are exhaustive", testCompleteBoolMatch),
    ("duplicate Bool arm is unreachable", testDuplicateBoolArm),
    ("open integer literals require a fallback", testOpenIntegerDomain),
    ("unguarded wildcard makes a later arm unreachable", testWildcardShadowing),
    ("guarded arms do not contribute coverage", testGuardedArmDoesNotCover),
    ("guarded arms do not shadow later arms", testGuardedArmDoesNotShadow),
    ("unit has one exhaustive tuple pattern", testUnitCoverage),
    ("tuple coverage decomposes element domains", testTupleCoverage),
    ("nil and cons cover every list", testListCoverage),
    ("missing cons reports a list witness", testMissingListCons),
    ("visible ADT constructors form a closed domain", testAdtCoverage),
    ("missing ADT constructor reports its fields", testMissingAdtConstructor),
    ("ADT payloads decompose nested constructor spaces", testNestedAdtCoverage),
    ("hidden ADT constructors keep the domain open", testHiddenAdtConstructor),
    ("exact lists specialize through cons cells", testExactListShadowing),
    ("as-patterns contribute their inner coverage", testAsPatternCoverage),
    ("or-pattern alternatives form a coverage union", testOrPatternCoverage),
    ("nested or-pattern products stay symbolic", testNestedOrPatternProductCoverage),
    ("jointly exhaustive product alternatives stay symbolic", testJointlyExhaustiveProductAlternatives),
    ("duplicate non-total alternatives stay symbolic", testDuplicateNonTotalAlternatives),
    ("partly useful or-pattern arm stays reachable", testPartlyUsefulOrPattern),
    ("wholly covered or-pattern arm is unreachable", testCoveredOrPattern),
    ("source pipeline accepts an exhaustive match", testCompleteSourceMatch),
    ("source pipeline rejects a non-exhaustive match", testIncompleteSourceMatch),
    ("source pipeline rejects an unreachable arm", testUnreachableSourceArm),
    ("nested source matches retain traversal order", testNestedSourceMatches),
    ("existing type errors suppress coverage cascades", testCoverageSuppression),
    ("source pipeline closes locally declared ADTs", testLocalAdtCoverage),
    ("pattern lambdas share source coverage", testPatternLambdaCoverage),
    ("guarded source arms do not contribute coverage", testGuardedSourceCoverage),
    ("recursive inference records one source match", testRecursiveMatchRecordedOnce),
    ("nested constructor witnesses render unambiguously", testNestedWitnessRendering),
    ("imported witnesses render source-accessible constructor names", testImportedWitnessRendering),
    ("source reachability covers every strict arm case", testStrictSourceReachability),
    ("repeated guarded arms remain reachable", testRepeatedGuardedSourceArms),
    ("warning-only diagnostics do not suppress coverage", testWarningsDoNotSuppressCoverage),
    ("hidden imported constructors stay out of witnesses", testHiddenImportedConstructorCoverage)
  ]

testEmptyBoolMatch :: IO ()
testEmptyBoolMatch =
  assertCoverage
    "empty Bool"
    TBoolType
    []
    [NonExhaustivePattern (PLiteral (LBool False))]

testCompleteBoolMatch :: IO ()
testCompleteBoolMatch =
  assertCoverage
    "complete Bool"
    TBoolType
    [arm (PLiteral (LBool False)), arm (PLiteral (LBool True))]
    []

testDuplicateBoolArm :: IO ()
testDuplicateBoolArm =
  assertCoverage
    "duplicate Bool"
    TBoolType
    [arm (PLiteral (LBool False)), arm (PLiteral (LBool False)), arm PWildcard]
    [UnreachablePatternArm 2]

testOpenIntegerDomain :: IO ()
testOpenIntegerDomain =
  assertCoverage
    "open integer"
    TIntType
    [arm (PLiteral (LInt 0))]
    [NonExhaustivePattern PWildcard]

testWildcardShadowing :: IO ()
testWildcardShadowing =
  assertCoverage
    "wildcard shadowing"
    TIntType
    [arm PWildcard, arm (PLiteral (LInt 1))]
    [UnreachablePatternArm 2]

testGuardedArmDoesNotCover :: IO ()
testGuardedArmDoesNotCover =
  assertCoverage
    "guarded coverage"
    TBoolType
    [guardedArm (PLiteral (LBool False)), arm (PLiteral (LBool True))]
    [NonExhaustivePattern (PLiteral (LBool False))]

testGuardedArmDoesNotShadow :: IO ()
testGuardedArmDoesNotShadow =
  assertCoverage
    "guarded shadowing"
    TBoolType
    [ guardedArm (PLiteral (LBool False)),
      arm (PLiteral (LBool False)),
      arm (PLiteral (LBool True))
    ]
    []

testUnitCoverage :: IO ()
testUnitCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "unit"
    (TTupleType [])
    [arm (PTuple [])]
    []

testTupleCoverage :: IO ()
testTupleCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "tuple"
    (TTupleType [TBoolType, TBoolType])
    [ arm (PTuple [PLiteral (LBool False), PWildcard]),
      arm (PTuple [PLiteral (LBool True), PWildcard])
    ]
    []

testListCoverage :: IO ()
testListCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "list"
    (TListType TIntType)
    [arm (PList []), arm (PConsList PWildcard PWildcard)]
    []

testMissingListCons :: IO ()
testMissingListCons =
  assertCoverageWith
    emptyConstructorInventory
    "missing list cons"
    (TListType TIntType)
    [arm (PList [])]
    [NonExhaustivePattern (PConsList PWildcard PWildcard)]

testAdtCoverage :: IO ()
testAdtCoverage =
  assertCoverageWith
    maybeInventory
    "ADT"
    maybeIntType
    [ arm (PConstructor "Nothing" []),
      arm (PConstructor "Just" [PWildcard])
    ]
    []

testMissingAdtConstructor :: IO ()
testMissingAdtConstructor =
  assertCoverageWith
    maybeInventory
    "missing ADT constructor"
    maybeIntType
    [arm (PConstructor "Nothing" [])]
    [NonExhaustivePattern (PConstructor "Just" [PWildcard])]

testNestedAdtCoverage :: IO ()
testNestedAdtCoverage =
  assertCoverageWith
    maybeInventory
    "nested ADT"
    (TDataType "Maybe" [TBoolType])
    [ arm (PConstructor "Nothing" []),
      arm (PConstructor "Just" [PLiteral (LBool False)])
    ]
    [NonExhaustivePattern (PConstructor "Just" [PLiteral (LBool True)])]

testHiddenAdtConstructor :: IO ()
testHiddenAdtConstructor =
  assertCoverageWith
    hiddenMaybeInventory
    "hidden ADT constructor"
    maybeIntType
    [arm (PConstructor "Nothing" [])]
    [NonExhaustivePattern PWildcard]

testExactListShadowing :: IO ()
testExactListShadowing =
  assertCoverageWith
    emptyConstructorInventory
    "exact list shadowing"
    (TListType TBoolType)
    [ arm (PList []),
      arm (PConsList PWildcard PWildcard),
      arm (PList [PLiteral (LBool True)])
    ]
    [UnreachablePatternArm 3]

testAsPatternCoverage :: IO ()
testAsPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "as-pattern"
    TBoolType
    [ arm (PAs "whole" (PLiteral (LBool False))),
      arm (PLiteral (LBool True))
    ]
    []

testOrPatternCoverage :: IO ()
testOrPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "or-pattern"
    TBoolType
    [arm (POr [PLiteral (LBool False), PLiteral (LBool True)])]
    []

testNestedOrPatternProductCoverage :: IO ()
testNestedOrPatternProductCoverage = do
  completed <-
    timeout 5000000 $
      evaluate
        ( null
            ( analyzePatternCoverage
                emptyConstructorInventory
                (TTupleType (replicate fieldCount TBoolType))
                [arm productPattern]
            )
        )
  assertEqual "nested or-pattern product" (Just True) completed
  where
    fieldCount = 30
    booleanAlternative =
      POr [PLiteral (LBool False), PLiteral (LBool True)]
    productPattern = PTuple (replicate fieldCount booleanAlternative)

testJointlyExhaustiveProductAlternatives :: IO ()
testJointlyExhaustiveProductAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( null
            ( analyzePatternCoverage
                emptyConstructorInventory
                (TTupleType (replicate fieldCount productType))
                [arm (PTuple (replicate fieldCount productAlternative))]
            )
        )
  assertEqual "jointly exhaustive product alternatives" (Just True) completed
  where
    fieldCount = 30
    productType = TTupleType [TBoolType, TBoolType]
    productAlternative =
      POr
        [ PTuple [PLiteral (LBool False), PWildcard],
          PTuple [PLiteral (LBool True), PWildcard]
        ]

testDuplicateNonTotalAlternatives :: IO ()
testDuplicateNonTotalAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( UnreachablePatternArm 2
            `elem` analyzePatternCoverage
              emptyConstructorInventory
              (TTupleType (replicate fieldCount TBoolType))
              [arm repeatedPattern, arm repeatedPattern]
        )
  assertEqual "duplicate non-total alternatives" (Just True) completed
  where
    fieldCount = 30
    duplicateFalse =
      POr [PLiteral (LBool False), PLiteral (LBool False)]
    repeatedPattern = PTuple (replicate fieldCount duplicateFalse)

testPartlyUsefulOrPattern :: IO ()
testPartlyUsefulOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "partly useful or-pattern"
    TBoolType
    [ arm (PLiteral (LBool False)),
      arm (POr [PLiteral (LBool False), PLiteral (LBool True)])
    ]
    []

testCoveredOrPattern :: IO ()
testCoveredOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "covered or-pattern"
    TBoolType
    [ arm (PLiteral (LBool False)),
      arm (PLiteral (LBool True)),
      arm (POr [PLiteral (LBool False), PLiteral (LBool True)])
    ]
    [UnreachablePatternArm 3]

testCompleteSourceMatch :: IO ()
testCompleteSourceMatch = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | False -> 0 | True -> 1 }."
  assertEqual "complete source diagnostics" [] (compileErrors result)

testIncompleteSourceMatch :: IO ()
testIncompleteSourceMatch = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | True -> 1 }."
  assertEqual
    "incomplete source diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: False")]
    (diagnosticIdentities (compileErrors result))

testUnreachableSourceArm :: IO ()
testUnreachableSourceArm = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | False -> 0 | False -> 1 | True -> 2 }."
  assertEqual
    "unreachable source diagnostics"
    [("E2019", "pattern arm 2 is unreachable because earlier unguarded arms cover it")]
    (diagnosticIdentities (compileErrors result))

testNestedSourceMatches :: IO ()
testNestedSourceMatches = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | True -> case False { | False -> 0 } }."
  assertEqual
    "nested source diagnostics"
    [ ("E2018", "non-exhaustive pattern match; missing pattern: False"),
      ("E2018", "non-exhaustive pattern match; missing pattern: True")
    ]
    (diagnosticIdentities (compileErrors result))

testCoverageSuppression :: IO ()
testCoverageSuppression = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | 0 -> 1 }."
  assertEqual
    "coverage suppression diagnostics"
    ["E2011"]
    (map (diagnosticCodeText . diagnosticCode) (compileErrors result))

diagnosticIdentities :: [Diagnostic] -> [(Text, Text)]
diagnosticIdentities =
  map
    ( \diagnostic ->
        ( diagnosticCodeText (diagnosticCode diagnostic),
          diagnosticSummary diagnostic
        )
    )

testLocalAdtCoverage :: IO ()
testLocalAdtCoverage = do
  completeResult <-
    compileSource
      defaultWarningSettings
      "data Maybe a = Nothing | Just a. x = case Just True { | Nothing -> 0 | Just False -> 1 | Just True -> 2 }."
  assertEqual "complete local ADT diagnostics" [] (compileErrors completeResult)
  incompleteResult <-
    compileSource
      defaultWarningSettings
      "data Maybe a = Nothing | Just a. x = case Just True { | Just item -> 1 }."
  assertEqual
    "incomplete local ADT diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: Nothing")]
    (diagnosticIdentities (compileErrors incompleteResult))

testPatternLambdaCoverage :: IO ()
testPatternLambdaCoverage = do
  result <-
    compileSource
      defaultWarningSettings
      "choose = \\(True) -> 1. x = choose True."
  assertEqual
    "pattern lambda diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: False")]
    (diagnosticIdentities (compileErrors result))

testGuardedSourceCoverage :: IO ()
testGuardedSourceCoverage = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | False if True -> 0 | True -> 1 }."
  assertEqual
    "guarded source diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: False")]
    (diagnosticIdentities (compileErrors result))

testRecursiveMatchRecordedOnce :: IO ()
testRecursiveMatchRecordedOnce = do
  result <-
    compileSource
      defaultWarningSettings
      "f = \\(item) -> case item { | True -> if item then 1 else f item }. x = f True."
  assertEqual
    "recursive source diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: False")]
    (diagnosticIdentities (compileErrors result))

testNestedWitnessRendering :: IO ()
testNestedWitnessRendering =
  assertEqual
    "nested witness"
    "Pair (Just _) [(_, _) | _]"
    ( renderCoveragePattern
        ( PConstructor
            "Pair"
            [ PConstructor "Just" [PWildcard],
              PConsList (PTuple [PWildcard, PWildcard]) PWildcard
            ]
        )
    )

testImportedWitnessRendering :: IO ()
testImportedWitnessRendering =
  assertEqual
    "imported witness"
    "Second _"
    ( renderCoveragePattern
        ( PConstructor
            (resolvedImportedName ["Lib", "Choice"] ConstructorNamespace (mkIdentifier "Second"))
            [PWildcard]
        )
    )

testStrictSourceReachability :: IO ()
testStrictSourceReachability =
  mapM_
    assertUnreachableArm
    [ ( "constructor arm",
        2,
        "data Maybe a = Nothing | Just a. x = case Just 1 { | Just _ -> 0 | Just item -> 1 | Nothing -> 2 }."
      ),
      ( "exact list arm",
        3,
        "x = case [1] { | [] -> 0 | [_ | _] -> 1 | [item] -> 2 }."
      ),
      ( "guarded arm",
        2,
        "x = case True { | _ -> 0 | True if False -> 1 }."
      )
    ]
  where
    assertUnreachableArm :: (Text, Int, Text) -> IO ()
    assertUnreachableArm (label, armIndex, source) = do
      result <- compileSource defaultWarningSettings source
      assertEqual
        (label <> " diagnostics")
        [("E2019", "pattern arm " <> Text.pack (show armIndex) <> " is unreachable because earlier unguarded arms cover it")]
        (diagnosticIdentities (compileErrors result))

testRepeatedGuardedSourceArms :: IO ()
testRepeatedGuardedSourceArms = do
  result <-
    compileSource
      defaultWarningSettings
      "x = case True { | True if False -> 0 | True if True -> 1 | _ -> 2 }."
  assertEqual "repeated guarded diagnostics" [] (compileErrors result)

testWarningsDoNotSuppressCoverage :: IO ()
testWarningsDoNotSuppressCoverage = do
  result <-
    compileSource
      unusedWarningSettings
      "unused = 1. case True { | True -> 1 }."
  assertEqual
    "coverage alongside warnings"
    ["E2018"]
    (map (diagnosticCodeText . diagnosticCode) (compileErrors result))
  assertEqual "warning remains present" False (null (compileWarnings result))

unusedWarningSettings :: WarningSettings
unusedWarningSettings =
  case resolveWarningSettings ["-Wunused-binding"] Nothing Nothing Nothing of
    Right settings -> settings
    Left diagnostic -> error (show diagnostic)

testHiddenImportedConstructorCoverage :: IO ()
testHiddenImportedConstructorCoverage = do
  result <-
    inferExpressionWithInputs
      hiddenConstructorInputs
      ( EPatternCase
          (EVar "subject")
          [arm (PConstructor "Nothing" [])]
      )
  assertEqual
    "hidden constructor pipeline diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: _")]
    (diagnosticIdentities (filter isErrorDiagnostic (inferredDiagnostics result)))

hiddenConstructorInputs :: InferenceInputs
hiddenConstructorInputs =
  InferenceInputs
    { inferenceBuiltinMode = ResolveKernelOnly,
      inferenceWarningSettings = defaultWarningSettings,
      inferenceImportedTypes =
        Map.fromList
          [ ("subject", PlainTypeBinding maybeIntType),
            ("Nothing", ConstructorTypeBinding "Maybe" ["a"] [])
          ],
      inferenceImportedDataTypes =
        Map.singleton
          "Maybe"
          ( DataTypeBinding
              ["a"]
              [ [],
                [ConstructorArgumentParameter "a"]
              ]
          ),
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

assertCoverage :: Text -> ExpressionType -> [CaseArm] -> [PatternCoverageFailure] -> IO ()
assertCoverage label expressionType arms expected =
  assertCoverageWith emptyConstructorInventory label expressionType arms expected

assertCoverageWith :: ConstructorInventory -> Text -> ExpressionType -> [CaseArm] -> [PatternCoverageFailure] -> IO ()
assertCoverageWith inventory label expressionType arms expected =
  assertEqual label expected (analyzePatternCoverage inventory expressionType arms)

arm :: Pattern -> CaseArm
arm patternValue = CaseArm patternValue Nothing (ELit (LInt 0))

guardedArm :: Pattern -> CaseArm
guardedArm patternValue =
  CaseArm patternValue (Just (ELit (LBool True))) (ELit (LInt 0))

maybeIntType :: ExpressionType
maybeIntType = TDataType "Maybe" [TIntType]

maybeInventory :: ConstructorInventory
maybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            ["a"]
            [ [],
              [ConstructorArgumentParameter "a"]
            ]
        )
    )
    ( Map.fromList
        [ ("Nothing", ConstructorTypeBinding "Maybe" ["a"] []),
          ("Just", ConstructorTypeBinding "Maybe" ["a"] [ConstructorArgumentParameter "a"])
        ]
    )

hiddenMaybeInventory :: ConstructorInventory
hiddenMaybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            ["a"]
            [ [],
              [ConstructorArgumentParameter "a"]
            ]
        )
    )
    (Map.singleton "Nothing" (ConstructorTypeBinding "Maybe" ["a"] []))
