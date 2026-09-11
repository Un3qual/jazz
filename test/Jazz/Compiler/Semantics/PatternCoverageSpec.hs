{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (..),
    CoreSort (..),
    Expr (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.CoreIdentity (ResolvedReference (UnresolvedReference))
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticCode,
    diagnosticSummary,
    isErrorDiagnostic,
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileSource,
    compileWarnings,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (mkModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Name
  ( NameNamespace (ConstructorNamespace, TypeNamespace, ValueNamespace),
    ResolvedName,
    mkIdentifier,
    resolvedImportedName,
    resolvedLocalName,
    sourceName,
  )
import Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    CoveragePattern,
    PatternCoverageFailure (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    constructorInventoryFromBindingsWithWitnessNames,
    emptyConstructorInventory,
    renderCoveragePattern,
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    inferExpressionWithInputs,
  )
import Jazz.Compiler.TypeInference.Result (inferredDiagnostics)
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType,
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeEnvKey (..),
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
import System.Environment (getArgs)
import System.Timeout (timeout)

main :: IO ()
main = do
  args <- getArgs
  runTestSuite "PatternCoverage" (tests <> if "--skip-performance" `elem` args then [] else performanceTests)

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

performanceTests :: [NamedTest]
performanceTests =
  [ ("large repeated integer arms preserve diagnostic order", testLargeRepeatedIntegerArmOrder),
    ("nested or-pattern products stay symbolic", testNestedOrPatternProductCoverage),
    ("jointly exhaustive product alternatives stay symbolic", testJointlyExhaustiveProductAlternatives),
    ("duplicate non-total alternatives stay symbolic", testDuplicateNonTotalAlternatives),
    ("repeated distinct non-total alternatives stay symbolic", testRepeatedDistinctNonTotalAlternatives),
    ("reordered non-total alternatives share canonical coverage", testReorderedNonTotalAlternatives),
    ("constructor inventories materialize only reachable data types", testTypeScopedConstructorInventory)
  ]

testEmptyBoolMatch :: IO ()
testEmptyBoolMatch =
  assertCoverage
    "empty Bool"
    SemanticBool
    []
    [missing "False"]

testCompleteBoolMatch :: IO ()
testCompleteBoolMatch =
  assertCoverage
    "complete Bool"
    SemanticBool
    [arm (literalPattern (LBool False)), arm (literalPattern (LBool True))]
    []

testDuplicateBoolArm :: IO ()
testDuplicateBoolArm =
  assertCoverage
    "duplicate Bool"
    SemanticBool
    [arm (literalPattern (LBool False)), arm (literalPattern (LBool False)), arm wildcardPattern]
    [unreachable 2]

testOpenIntegerDomain :: IO ()
testOpenIntegerDomain =
  assertCoverage
    "open integer"
    SemanticInt
    [arm (literalPattern (LInt 0))]
    [missing "_"]

testLargeRepeatedIntegerArmOrder :: IO ()
testLargeRepeatedIntegerArmOrder =
  assertCoverage
    "large repeated integer arms"
    SemanticInt
    (map (arm . literalPattern . LInt) ([0 .. 63] <> replicate 1024 0))
    (map unreachable [65 .. 1088] <> [missing "_"])

testWildcardShadowing :: IO ()
testWildcardShadowing =
  assertCoverage
    "wildcard shadowing"
    SemanticInt
    [arm wildcardPattern, arm (literalPattern (LInt 1))]
    [unreachable 2]

testGuardedArmDoesNotCover :: IO ()
testGuardedArmDoesNotCover =
  assertCoverage
    "guarded coverage"
    SemanticBool
    [guardedArm (literalPattern (LBool False)), arm (literalPattern (LBool True))]
    [missing "False"]

testGuardedArmDoesNotShadow :: IO ()
testGuardedArmDoesNotShadow =
  assertCoverage
    "guarded shadowing"
    SemanticBool
    [ guardedArm (literalPattern (LBool False)),
      arm (literalPattern (LBool False)),
      arm (literalPattern (LBool True))
    ]
    []

testUnitCoverage :: IO ()
testUnitCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "unit"
    (SemanticTuple [])
    [arm (tuplePattern [])]
    []

testTupleCoverage :: IO ()
testTupleCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "tuple"
    (SemanticTuple [SemanticBool, SemanticBool])
    [ arm (tuplePattern [literalPattern (LBool False), wildcardPattern]),
      arm (tuplePattern [literalPattern (LBool True), wildcardPattern])
    ]
    []

testListCoverage :: IO ()
testListCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "list"
    (SemanticList SemanticInt)
    [arm (listPattern []), arm (consListPattern wildcardPattern wildcardPattern)]
    []

testMissingListCons :: IO ()
testMissingListCons =
  assertCoverageWith
    emptyConstructorInventory
    "missing list cons"
    (SemanticList SemanticInt)
    [arm (listPattern [])]
    [missing "[_ | _]"]

testAdtCoverage :: IO ()
testAdtCoverage =
  assertCoverageWith
    maybeInventory
    "ADT"
    maybeIntType
    [ arm (constructorPattern "Nothing" []),
      arm (constructorPattern "Just" [wildcardPattern])
    ]
    []

testMissingAdtConstructor :: IO ()
testMissingAdtConstructor =
  assertCoverageWith
    maybeInventory
    "missing ADT constructor"
    maybeIntType
    [arm (constructorPattern "Nothing" [])]
    [missing "Just _"]

testNestedAdtCoverage :: IO ()
testNestedAdtCoverage =
  assertCoverageWith
    maybeInventory
    "nested ADT"
    (SemanticData (resolvedTypeName "Maybe") [SemanticBool])
    [ arm (constructorPattern "Nothing" []),
      arm (constructorPattern "Just" [literalPattern (LBool False)])
    ]
    [missing "Just True"]

testHiddenAdtConstructor :: IO ()
testHiddenAdtConstructor =
  assertCoverageWith
    hiddenMaybeInventory
    "hidden ADT constructor"
    maybeIntType
    [arm (constructorPattern "Nothing" [])]
    [missing "_"]

testExactListShadowing :: IO ()
testExactListShadowing =
  assertCoverageWith
    emptyConstructorInventory
    "exact list shadowing"
    (SemanticList SemanticBool)
    [ arm (listPattern []),
      arm (consListPattern wildcardPattern wildcardPattern),
      arm (listPattern [literalPattern (LBool True)])
    ]
    [unreachable 3]

testAsPatternCoverage :: IO ()
testAsPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "as-pattern"
    SemanticBool
    [ arm (asPattern "whole" (literalPattern (LBool False))),
      arm (literalPattern (LBool True))
    ]
    []

testOrPatternCoverage :: IO ()
testOrPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "or-pattern"
    SemanticBool
    [arm (orPattern [literalPattern (LBool False), literalPattern (LBool True)])]
    []

testNestedOrPatternProductCoverage :: IO ()
testNestedOrPatternProductCoverage = do
  completed <-
    timeout 5000000 $
      evaluate
        ( null
            ( analyzePatternCoverage
                emptyConstructorInventory
                (SemanticTuple (replicate fieldCount SemanticBool))
                [arm productPattern]
            )
        )
  assertEqual "nested or-pattern product" (Just True) completed
  where
    fieldCount = 30
    booleanAlternative =
      orPattern [literalPattern (LBool False), literalPattern (LBool True)]
    productPattern = tuplePattern (replicate fieldCount booleanAlternative)

testJointlyExhaustiveProductAlternatives :: IO ()
testJointlyExhaustiveProductAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( null
            ( analyzePatternCoverage
                emptyConstructorInventory
                (SemanticTuple (replicate fieldCount productType))
                [arm (tuplePattern (replicate fieldCount productAlternative))]
            )
        )
  assertEqual "jointly exhaustive product alternatives" (Just True) completed
  where
    fieldCount = 30
    productType = SemanticTuple [SemanticBool, SemanticBool]
    productAlternative =
      orPattern
        [ tuplePattern [literalPattern (LBool False), wildcardPattern],
          tuplePattern [literalPattern (LBool True), wildcardPattern]
        ]

testDuplicateNonTotalAlternatives :: IO ()
testDuplicateNonTotalAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( UnreachablePatternArm 2
            `elem` analyzePatternCoverage
              emptyConstructorInventory
              (SemanticTuple (replicate fieldCount SemanticBool))
              [arm repeatedPattern, arm repeatedPattern]
        )
  assertEqual "duplicate non-total alternatives" (Just True) completed
  where
    fieldCount = 30
    duplicateFalse =
      orPattern [literalPattern (LBool False), literalPattern (LBool False)]
    repeatedPattern = tuplePattern (replicate fieldCount duplicateFalse)

testRepeatedDistinctNonTotalAlternatives :: IO ()
testRepeatedDistinctNonTotalAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( UnreachablePatternArm 2
            `elem` analyzePatternCoverage
              emptyConstructorInventory
              (SemanticTuple (replicate fieldCount SemanticInt))
              [arm repeatedPattern, arm repeatedPattern]
        )
  assertEqual "repeated distinct non-total alternatives" (Just True) completed
  where
    fieldCount = 30
    zeroOrOne =
      orPattern [literalPattern (LInt 0), literalPattern (LInt 1)]
    repeatedPattern = tuplePattern (replicate fieldCount zeroOrOne)

testReorderedNonTotalAlternatives :: IO ()
testReorderedNonTotalAlternatives = do
  completed <-
    timeout 5000000 $
      evaluate
        ( UnreachablePatternArm 2
            `elem` analyzePatternCoverage
              emptyConstructorInventory
              (SemanticTuple (replicate fieldCount SemanticInt))
              [arm firstPattern, arm reorderedPattern]
        )
  assertEqual "reordered non-total alternatives" (Just True) completed
  where
    fieldCount = 30
    zeroOrOne =
      orPattern [literalPattern (LInt 0), literalPattern (LInt 1)]
    oneOrZero =
      orPattern [literalPattern (LInt 1), literalPattern (LInt 0)]
    firstPattern = tuplePattern (replicate fieldCount zeroOrOne)
    reorderedPattern = tuplePattern (replicate fieldCount oneOrZero)

testTypeScopedConstructorInventory :: IO ()
testTypeScopedConstructorInventory = do
  _ <- evaluate (Map.size dataTypes)
  completed <-
    timeout 5000000 $
      evaluate
        ( and
            [ null
                ( analyzePatternCoverage
                    (constructorInventoryFromBindings dataTypes (fixtureTypes (environment siteIndex)))
                    targetType
                    [arm (constructorPattern "Only" [])]
                )
            | siteIndex <- [1 .. siteCount]
            ]
        )
  assertEqual "type-scoped constructor inventory" (Just True) completed
  where
    siteCount :: Int
    siteCount = 300

    dataTypeCount :: Int
    dataTypeCount = 100000
    targetType = SemanticData (resolvedTypeName "Target") []
    dataTypes =
      Map.insert
        "Target"
        (DataTypeBinding [] [[]])
        ( Map.fromList
            [ ("Unused" <> Text.pack (show dataTypeIndex), DataTypeBinding [] [[]])
            | dataTypeIndex <- [1 .. dataTypeCount]
            ]
        )
    environment siteIndex =
      Map.insert
        (resolvedLocalName ValueNamespace (mkIdentifier ("value" <> Text.pack (show siteIndex))))
        (PlainTypeBinding SemanticInt)
        (Map.singleton (resolvedLocalName ConstructorNamespace (mkIdentifier "Only")) (ConstructorTypeBinding (resolvedTypeName "Target") [] []))

testPartlyUsefulOrPattern :: IO ()
testPartlyUsefulOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "partly useful or-pattern"
    SemanticBool
    [ arm (literalPattern (LBool False)),
      arm (orPattern [literalPattern (LBool False), literalPattern (LBool True)])
    ]
    []

testCoveredOrPattern :: IO ()
testCoveredOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "covered or-pattern"
    SemanticBool
    [ arm (literalPattern (LBool False)),
      arm (literalPattern (LBool True)),
      arm (orPattern [literalPattern (LBool False), literalPattern (LBool True)])
    ]
    [unreachable 3]

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
    "[(True, True) | []]"
    ( renderCoveragePattern
        ( firstMissing
            emptyConstructorInventory
            (SemanticList (SemanticTuple [SemanticBool, SemanticBool]))
            [ arm (listPattern []),
              arm (consListPattern (tuplePattern [literalPattern (LBool False), wildcardPattern]) wildcardPattern),
              arm (consListPattern (tuplePattern [literalPattern (LBool True), literalPattern (LBool False)]) wildcardPattern)
            ]
        )
    )

testImportedWitnessRendering :: IO ()
testImportedWitnessRendering =
  assertEqual
    "imported witness"
    "Only"
    (renderCoveragePattern (firstMissing importedWitnessInventory (SemanticData (resolvedTypeName "Choice") []) []))
  where
    importedName =
      resolvedImportedName
        (mkModulePath (mkIdentifier "Lib" :| [mkIdentifier "Choice"]))
        ConstructorNamespace
        (mkIdentifier "Only")
    sourceWitness = sourceName (mkIdentifier "Only")
    importedWitnessInventory =
      constructorInventoryFromBindingsWithWitnessNames
        (Map.singleton importedName sourceWitness)
        (Map.singleton "Choice" (DataTypeBinding [] [[]]))
        (fixtureTypes (Map.singleton importedName (ConstructorTypeBinding (resolvedTypeName "Choice") [] [])))

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
      (resolvedPatternCase (constructorPattern "Nothing" []))
  assertEqual
    "hidden constructor pipeline diagnostics"
    [("E2018", "non-exhaustive pattern match; missing pattern: _")]
    (diagnosticIdentities (filter isErrorDiagnostic (inferredDiagnostics result)))

hiddenConstructorInputs :: InferenceInputs
hiddenConstructorInputs =
  InferenceInputs
    { inferenceWarningSettings = defaultWarningSettings,
      inferenceExternalUses = Set.empty,
      inferenceImportedTypes =
        fixtureTypes $
          Map.fromList
            [ (resolvedLocalName ValueNamespace (mkIdentifier "subject"), PlainTypeBinding maybeIntType),
              (resolvedLocalName ConstructorNamespace (mkIdentifier "Nothing"), ConstructorTypeBinding (resolvedTypeName "Maybe") [resolvedLocalName TypeNamespace (mkIdentifier "a")] [])
            ],
      inferenceImportedDataTypes =
        Map.singleton
          "Maybe"
          ( DataTypeBinding
              [resolvedLocalName TypeNamespace (mkIdentifier "a")]
              [ [],
                [ConstructorArgumentType (SemanticVariable "a")]
              ]
          ),
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Nothing
    }

data ExpectedCoverageFailure
  = ExpectedMissing Text
  | ExpectedUnreachable Int
  deriving (Eq, Show)

missing :: Text -> ExpectedCoverageFailure
missing = ExpectedMissing

unreachable :: Int -> ExpectedCoverageFailure
unreachable = ExpectedUnreachable

coverageFailureView :: PatternCoverageFailure -> ExpectedCoverageFailure
coverageFailureView failure =
  case failure of
    NonExhaustivePattern patternValue -> ExpectedMissing (renderCoveragePattern patternValue)
    UnreachablePatternArm armIndex -> ExpectedUnreachable armIndex

assertCoverage :: Text -> ExpressionType -> [CaseArm 'Resolved] -> [ExpectedCoverageFailure] -> IO ()
assertCoverage label expressionType arms expected =
  assertCoverageWith emptyConstructorInventory label expressionType arms expected

assertCoverageWith :: ConstructorInventory -> Text -> ExpressionType -> [CaseArm 'Resolved] -> [ExpectedCoverageFailure] -> IO ()
assertCoverageWith inventory label expressionType arms expected =
  assertEqual label expected (map coverageFailureView (analyzePatternCoverage inventory expressionType arms))

arm :: Pattern 'Lowered -> CaseArm 'Resolved
arm patternValue = resolveCaseArm patternValue Nothing

guardedArm :: Pattern 'Lowered -> CaseArm 'Resolved
guardedArm patternValue =
  resolveCaseArm patternValue (Just (ELit loweredExpressionNode (LBool True)))

resolveCaseArm :: Pattern 'Lowered -> Maybe (Expr 'Lowered) -> CaseArm 'Resolved
resolveCaseArm patternValue maybeGuard =
  case resolveExpression loweredCaseExpression of
    EPatternCase _ _ [resolvedArm] -> resolvedArm
    _ -> error "expected one resolved case arm"
  where
    loweredCaseExpression =
      EPatternCase
        loweredExpressionNode
        (ELit loweredExpressionNode (LBool True))
        [CaseArm loweredExpressionNode patternValue maybeGuard (ELit loweredExpressionNode (LInt 0))]

resolvedPatternCase :: Pattern 'Lowered -> Expr 'Resolved
resolvedPatternCase patternValue =
  case resolveExpression loweredCaseExpression of
    resolved@EPatternCase {} -> resolved
    _ -> error "expected resolved pattern case"
  where
    loweredCaseExpression =
      EPatternCase
        loweredExpressionNode
        (EVar loweredExpressionNode (sourceName (mkIdentifier "subject")))
        [CaseArm loweredExpressionNode patternValue Nothing (ELit loweredExpressionNode (LInt 0))]

resolveExpression :: Expr 'Lowered -> Expr 'Resolved
resolveExpression expression =
  case resolveStandaloneExprNames (exportInventory []) expression of
    Right resolved -> resolved
    Left diagnostics -> error (show diagnostics)

wildcardPattern :: Pattern 'Lowered
wildcardPattern = PWildcard loweredPatternNode

literalPattern :: Literal -> Pattern 'Lowered
literalPattern = PLiteral loweredPatternNode

constructorPattern :: Text -> [Pattern 'Lowered] -> Pattern 'Lowered
constructorPattern name = PConstructor loweredPatternNode (sourceName (mkIdentifier name))

listPattern :: [Pattern 'Lowered] -> Pattern 'Lowered
listPattern = PList loweredPatternNode

consListPattern :: Pattern 'Lowered -> Pattern 'Lowered -> Pattern 'Lowered
consListPattern = PConsList loweredPatternNode

tuplePattern :: [Pattern 'Lowered] -> Pattern 'Lowered
tuplePattern = PTuple loweredPatternNode

asPattern :: Text -> Pattern 'Lowered -> Pattern 'Lowered
asPattern name = PAs loweredPatternNode (sourceName (mkIdentifier name))

orPattern :: [Pattern 'Lowered] -> Pattern 'Lowered
orPattern = POr loweredPatternNode

loweredPatternNode :: CoreNode 'Lowered 'PatternSort
loweredPatternNode = CoreNode (CoreNodeId 0) (SourceSpan 1 1) ()

loweredExpressionNode :: CoreNode 'Lowered 'ExpressionSort
loweredExpressionNode = CoreNode (CoreNodeId 1) (SourceSpan 1 1) ()

firstMissing :: ConstructorInventory -> ExpressionType -> [CaseArm 'Resolved] -> CoveragePattern
firstMissing inventory expressionType arms =
  case analyzePatternCoverage inventory expressionType arms of
    NonExhaustivePattern patternValue : _ -> patternValue
    failures -> error ("expected missing pattern, got " <> show failures)

maybeIntType :: ExpressionType
maybeIntType = SemanticData (resolvedTypeName "Maybe") [SemanticInt]

maybeInventory :: ConstructorInventory
maybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            [maybeTypeParameter]
            [ [],
              [ConstructorArgumentType (SemanticVariable "a")]
            ]
        )
    )
    ( fixtureTypes $
        Map.fromList
          [ (resolvedLocalName ConstructorNamespace (mkIdentifier "Nothing"), ConstructorTypeBinding (resolvedTypeName "Maybe") [maybeTypeParameter] []),
            (resolvedLocalName ConstructorNamespace (mkIdentifier "Just"), ConstructorTypeBinding (resolvedTypeName "Maybe") [maybeTypeParameter] [ConstructorArgumentType (SemanticVariable "a")])
          ]
    )

hiddenMaybeInventory :: ConstructorInventory
hiddenMaybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            [maybeTypeParameter]
            [ [],
              [ConstructorArgumentType (SemanticVariable "a")]
            ]
        )
    )
    (fixtureTypes $ Map.singleton (resolvedLocalName ConstructorNamespace (mkIdentifier "Nothing")) (ConstructorTypeBinding (resolvedTypeName "Maybe") [maybeTypeParameter] []))

maybeTypeParameter :: ResolvedName
maybeTypeParameter = resolvedLocalName TypeNamespace (mkIdentifier "a")

resolvedTypeName :: Text -> ResolvedName
resolvedTypeName = resolvedLocalName TypeNamespace . mkIdentifier

-- These partial inference inputs intentionally have no defining source unit;
-- the expected results are diagnostics, never a successful analyzed tree.
fixtureTypes :: Map.Map ResolvedName TypeBinding -> TypeEnv
fixtureTypes = Map.mapKeys (\name -> TypeEnvKey (UnresolvedReference name) name)
