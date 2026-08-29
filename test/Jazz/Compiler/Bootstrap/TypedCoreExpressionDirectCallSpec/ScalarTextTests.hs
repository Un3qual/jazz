{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ScalarTextTests where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CaseArm (..), Expr (..), Literal (..), Pattern (..), Statement (..))
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( diagnosticCode,
    isErrorDiagnostic,
  )
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleGraph (CoreModule (..), ResolvedModule (..))
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Elaboration.Types (InferredExpr (..))
import Jazz.Compiler.TypeInference.Pattern (InferredPatternCaseArm (..), inferPatternCaseTypeWithResults)
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.State (initialInferState)
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (TBoolType),
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

isPatternCaseBlock :: LoweredBlock -> Bool
isPatternCaseBlock (LoweredBlock (LoweredBlockId name) _ _ _) =
  "case$" `Text.isPrefixOf` name

functionId :: Text -> LoweredFunctionId
functionId name = LoweredFunctionId ("App::Main::" <> name)

blockId :: Text -> LoweredBlockId
blockId = LoweredBlockId

parameter :: Text -> LoweredRepresentation -> LoweredParameter
parameter name representation = LoweredParameter (LoweredParameterId name) representation

functionParameter :: Text -> LoweredRepresentation -> LoweredOperand
functionParameter name representation =
  LoweredFunctionParameterOperand (LoweredParameterId name) representation

blockParameter :: Text -> LoweredRepresentation -> LoweredOperand
blockParameter name representation =
  LoweredBlockParameterOperand (LoweredParameterId name) representation

temporary :: Int -> LoweredRepresentation -> LoweredOperand
temporary index representation =
  LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index))) representation

boolImmediate :: Bool -> LoweredOperand
boolImmediate = LoweredImmediateOperand . LoweredBoolImmediate

intImmediate :: Integer -> LoweredOperand
intImmediate = LoweredImmediateOperand . LoweredSignedIntegerImmediate LoweredIntegerWidth64

intRepresentation :: LoweredRepresentation
intRepresentation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64

testFunctionResultNegativeTerminators :: IO ()
testFunctionResultNegativeTerminators = do
  let (typedProgram, expectedProgram) = functionBodyConsumedCallExpectedProgram
  assertEqual
    "function-body consumed calls use valid typed core"
    []
    (validateTypedProgram typedProgram)
  assertEqual
    "function-body consumed calls use valid Lowered IR"
    []
    (validateLoweredProgram expectedProgram)
  assertEqual
    "function-body consumed direct and closure calls lower exactly"
    (LoweredIRSucceeded expectedProgram)
    (lowerTypedCoreExpressionDirectCall typedProgram)

testFunctionBodyPartialApplicationResult :: IO ()
testFunctionBodyPartialApplicationResult = do
  let (typedProgram, expectedProgram) = functionBodyPartialApplicationExpectedProgram
  assertEqual
    "function-body partial application uses valid typed core"
    []
    (validateTypedProgram typedProgram)
  assertEqual
    "function-body partial application uses valid Lowered IR"
    []
    (validateLoweredProgram expectedProgram)
  assertEqual
    "function-body partial application lowers exactly"
    (LoweredIRSucceeded expectedProgram)
    (lowerTypedCoreExpressionDirectCall typedProgram)

testNestedTailControlFlow :: IO ()
testNestedTailControlFlow =
  mapM_ assertExact nestedTailControlFlowExpectedLoweredPrograms
  where
    assertExact (name, expectedProgram) = do
      firstProduction <- produceFixture (producerEdgeFixture name)
      secondProduction <- produceFixture (producerEdgeFixture name)
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          assertEqual
            (name <> " exact nested tail lowering")
            (LoweredIRSucceeded expectedProgram)
            (lowerTypedCoreExpressionDirectCall typedProgram)
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

testScalarPatternCaseProduction :: IO ()
testScalarPatternCaseProduction =
  mapM_ assertProduced scalarPatternCaseExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture =
            if name == "pattern-case"
              then fixtureByName name
              else producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual
        (name <> " exact typed production")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstProduction)
      assertEqual (name <> " typed validation") [] (validateTypedProgram expectedProgram)
      case lookup name expectedLowerings of
        Just expectedLowering ->
          assertEqual
            (name <> " exact scalar pattern-case lowering")
            (LoweredIRSucceeded expectedLowering)
            (lowerTypedCoreExpressionDirectCall expectedProgram)
        Nothing ->
          case lowerTypedCoreExpressionDirectCall expectedProgram of
            LoweredIRSucceeded loweredProgram ->
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
            lowering ->
              failTest (name <> " did not lower: " <> Text.pack (show lowering))
    expectedLowerings =
      [(name, lowered) | (name, _, lowered) <- scalarPatternCaseExpectedLoweredPrograms]

testScalarPatternCaseLowererBoundary :: IO ()
testScalarPatternCaseLowererBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name scalarPatternCaseLowererBoundaryPrograms of
        Nothing -> failTest (name <> " pattern-case lowerer boundary program is missing")
        Just programValue -> do
          let firstLowering = lowerTypedCoreExpressionDirectCall programValue
              secondLowering = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstLowering secondLowering
          assertEqual
            (name <> " exact lowerer rejection")
            (LoweredIRUnsupported expectedFailures)
            firstLowering

    expectedResults =
      [ ( "pattern-case-constructor-lowerer",
          [patternFailure [1] [0, 0]]
        ),
        unsupportedPattern "pattern-case-list-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-tuple-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-as-lowerer" [0] [0, 0],
        unsupportedPattern "pattern-case-or-lowerer" [0] [0, 0],
        incompleteCase "pattern-case-final-literal-lowerer",
        incompleteCase "pattern-case-final-guarded-catch-all-lowerer",
        incompleteCase "pattern-case-unguarded-non-final-wildcard-lowerer",
        incompleteCase "pattern-case-unguarded-non-final-variable-lowerer"
      ]
    unsupportedPattern name statementPath patternPath =
      (name, [patternFailure statementPath patternPath])
    patternFailure statementPath patternPath =
      LoweredIRLoweringFailure
        (TypedPatternPath ["App", "Main"] statementPath patternPath)
        LoweredIRUnsupportedPattern
        LoweredIRNoFailureDetail
    incompleteCase name =
      ( name,
        [ LoweredIRLoweringFailure
            (TypedExpressionPath ["App", "Main"] [0] [0])
            LoweredIRIncompletePatternCase
            LoweredIRNoFailureDetail
        ]
      )

testScalarPatternCaseProducerBoundaries :: IO ()
testScalarPatternCaseProducerBoundaries = do
  mapM_ assertSourceBoundary expectedSourceFailures
  mapM_ assertDiagnosticBoundary expectedDiagnosticFailures
  assertEmptyArmBoundary
  where
    expectedSourceFailures =
      [ ("pattern-case-managed-scrutinee", [profileFailure 0]),
        ("pattern-case-constructor-pattern", [profileFailure 1]),
        ( "pattern-case-list-pattern",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            profileFailure 0
          ]
        ),
        ("pattern-case-tuple-pattern", [profileFailure 0]),
        ("pattern-case-as-pattern", [profileFailure 0]),
        ("pattern-case-or-pattern", [profileFailure 0])
      ]
    expectedDiagnosticFailures =
      [ ("pattern-case-final-guarded-catch-all", "E2018"),
        ("pattern-case-missing-final-catch-all", "E2018"),
        ("pattern-case-unguarded-non-final-wildcard", "E2019"),
        ("pattern-case-unguarded-non-final-variable", "E2019"),
        ("pattern-case-non-bool-guard", "E2001"),
        ("pattern-case-incompatible-arm-results", "E2012")
      ]

    assertSourceBoundary (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable rejection") firstProduction secondProduction
      assertEqual
        (name <> " exact producer-profile rejection")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstProduction)

    assertDiagnosticBoundary (name, expectedDiagnosticCode) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstProduction)
      assertEqual (name <> " repeatable diagnostic block") firstProduction secondProduction
      assertEqual
        (name <> " remains owned by inference diagnostics")
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstProduction)
      assertEqual
        (name <> " exact inference diagnostic")
        [expectedDiagnosticCode]
        [ diagnosticCodeText (diagnosticCode diagnostic)
        | diagnostic <- inferredDiagnostics ordinary,
          isErrorDiagnostic diagnostic
        ]

    assertEmptyArmBoundary = do
      let fixture = fixtureByName "unit-entry"
      resolvedModule <- resolveFixtureModule fixture
      let emptyCaseModule =
            resolvedModule
              { resolvedModuleCore =
                  replaceTerminalExpression
                    (EPatternCase (ELit (LBool True)) [])
                    (resolvedModuleCore resolvedModule)
              }
      firstProduction <- produceResolvedFixture fixture emptyCaseModule
      secondProduction <- produceResolvedFixture fixture emptyCaseModule
      assertEqual "empty pattern-case repeatable rejection" firstProduction secondProduction
      assertEqual
        "empty pattern-case exact diagnostic rejection"
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstProduction)
      assertEqual
        "empty pattern-case exact diagnostic"
        ["E2018"]
        [ diagnosticCodeText (diagnosticCode diagnostic)
        | diagnostic <- inferredDiagnostics (typedCoreProductionInferenceResult firstProduction),
          isErrorDiagnostic diagnostic
        ]

    replaceTerminalExpression replacement coreModule =
      case coreModuleExpr coreModule of
        EBlock [SExpr spanValue _] ->
          coreModule {coreModuleExpr = EBlock [SExpr spanValue replacement]}
        other ->
          error ("unexpected unit fixture core shape: " <> show other)

    profileFailure statementIndex =
      expressionFailure
        statementIndex
        []
        TypedCorePatternCaseUnsupported
        TypedCorePatternCaseDetail

    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testScalarPatternCaseArmResultPositions :: IO ()
testScalarPatternCaseArmResultPositions =
  assertEqual
    "pattern inference retains one result slot per authored arm"
    [PLiteral (LInt 1), PWildcard]
    (map inferredArmPattern armResults)
  where
    (_, _, armResults) =
      inferPatternCaseTypeWithResults
        inferChild
        ResolveKernelOnly
        Map.empty
        TBoolType
        initialInferState
        [ CaseArm (PLiteral (LInt 1)) Nothing (ELit (LBool False)),
          CaseArm PWildcard Nothing (ELit (LBool True))
        ]
    inferChild _ _ state _ =
      (InferredExpr (Just TBoolType) Nothing [], state)
    inferredArmPattern (InferredPatternCaseArm pattern _ _) = pattern

testScalarPatternCaseAnalysisProduction :: IO ()
testScalarPatternCaseAnalysisProduction =
  mapM_ assertProduced scalarPatternCaseAnalysisExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      assertEqual
        (name <> " exact analysis-preserving production")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstProduction)
      assertEqual (name <> " typed validation") [] (validateTypedProgram expectedProgram)

testScalarPatternCaseTransportLowering :: IO ()
testScalarPatternCaseTransportLowering = do
  mapM_
    assertExpectationKeys
    [ ("control flow", map fst expectedPatternCaseControlFlows),
      ("transport shape", map fst expectedPatternCaseTransportShapes),
      ("join operations", map fst expectedPatternCaseJoinOperations),
      ("closure call count", map fst expectedClosureCallCounts)
    ]
  mapM_ assertTransported names
  where
    assertExpectationKeys (label, keys) =
      assertEqual
        (label <> " expectation keys are exercised")
        []
        [key | key <- keys, key `notElem` names]
    names =
      [ "pattern-case-in-conditional-branch",
        "conditional-in-pattern-case-guard",
        "pattern-case-in-pattern-case-body",
        "pattern-case-scrutinee-pattern-case",
        "pattern-case-ambient-scalar",
        "pattern-case-captured-scalar",
        "scalar-pattern-case-closure-result",
        "scalar-pattern-case-tail-function",
        "pattern-case-call-argument"
      ]
    assertTransported name = do
      let fixture = producerEdgeFixture name
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              case lookup name expectedPatternCaseControlFlows of
                Just expectedControlFlow ->
                  assertEqual
                    (name <> " exact pattern-case control flow")
                    expectedControlFlow
                    (patternCaseControlFlow loweredProgram)
                Nothing -> pure ()
              case lookup name expectedPatternCaseTransportShapes of
                Just expectedTransportShape ->
                  assertEqual
                    (name <> " exact pattern-case transport shape")
                    expectedTransportShape
                    (patternCaseTransportShape loweredProgram)
                Nothing -> pure ()
              case lookup name expectedPatternCaseJoinOperations of
                Just (joinBlockId, expectedOperations) ->
                  assertEqual
                    (name <> " exact post-join operations")
                    [expectedOperations]
                    (blockOperations loweredProgram joinBlockId)
                Nothing -> pure ()
              case lookup name expectedClosureCallCounts of
                Just expectedCount ->
                  assertEqual
                    (name <> " closure application count")
                    expectedCount
                    (closureCallCount loweredProgram)
                Nothing -> pure ()
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

patternCaseControlFlow :: LoweredProgram -> [(LoweredFunctionId, [LoweredBlock])]
patternCaseControlFlow (LoweredProgram _ _ _ functions _) =
  [ (loweredFunctionId, blocks)
  | LoweredFunction loweredFunctionId _ _ _ blocks _ <- functions,
    any isPatternCaseBlock blocks
  ]

patternCaseTransportShape :: LoweredProgram -> [(LoweredFunctionId, [(LoweredBlockId, [LoweredRepresentation], [(LoweredBlockId, [LoweredRepresentation])])])]
patternCaseTransportShape (LoweredProgram _ _ _ functions _) =
  [ (loweredFunctionId, map blockShape caseBlocks)
  | LoweredFunction loweredFunctionId _ _ _ blocks _ <- functions,
    let caseBlocks = filter isPatternCaseBlock blocks,
    not (null caseBlocks)
  ]
  where
    blockShape (LoweredBlock loweredBlockId parameters _ terminator) =
      (loweredBlockId, map parameterRepresentation parameters, maybe [] successorShapes terminator)
    parameterRepresentation (LoweredParameter _ representation) = representation
    successorShapes terminator =
      case terminator of
        LoweredJump target arguments -> [(target, map operandRepresentation arguments)]
        LoweredBranch _ trueTarget trueArguments falseTarget falseArguments ->
          [ (trueTarget, map operandRepresentation trueArguments),
            (falseTarget, map operandRepresentation falseArguments)
          ]
        _ -> []
    operandRepresentation operand =
      case operand of
        LoweredFunctionParameterOperand _ representation -> representation
        LoweredBlockParameterOperand _ representation -> representation
        LoweredTemporaryOperand _ representation -> representation
        LoweredImmediateOperand immediate ->
          case immediate of
            LoweredUnitImmediate -> LoweredUnitRepresentation
            LoweredBoolImmediate {} -> LoweredBoolRepresentation
            LoweredSignedIntegerImmediate width _ -> LoweredSignedIntegerRepresentation width
            LoweredUnsignedIntegerImmediate width _ -> LoweredUnsignedIntegerRepresentation width
            LoweredFloatImmediate width _ -> LoweredFloatRepresentation width
            LoweredCharImmediate {} -> LoweredCharRepresentation

blockOperations :: LoweredProgram -> LoweredBlockId -> [[LoweredOperation]]
blockOperations (LoweredProgram _ _ _ functions _) targetBlockId =
  [ operations
  | LoweredFunction _ _ _ _ blocks _ <- functions,
    LoweredBlock loweredBlockId _ instructions _ <- blocks,
    loweredBlockId == targetBlockId,
    let operations = [operation | LoweredInstruction _ _ operation <- instructions]
  ]

closureCallCount :: LoweredProgram -> Int
closureCallCount (LoweredProgram _ _ _ functions _) =
  length
    [ ()
    | LoweredFunction _ _ _ _ blocks _ <- functions,
      LoweredBlock _ _ instructions _ <- blocks,
      LoweredInstruction _ _ LoweredClosureCall {} <- instructions
    ]

expectedPatternCaseJoinOperations :: [(Text, (LoweredBlockId, [LoweredOperation]))]
expectedPatternCaseJoinOperations =
  [ ( "pattern-case-ambient-scalar",
      ( LoweredBlockId "case$s1$3$e1$0$join",
        [ LoweredPrimitiveOperation
            (LoweredArithmeticPrimitive LoweredAdd)
            [ LoweredBlockParameterOperand (LoweredParameterId "result") intRepresentation,
              LoweredBlockParameterOperand (LoweredParameterId "live1") intRepresentation
            ]
        ]
      )
    ),
    ( "pattern-case-call-argument",
      ( LoweredBlockId "case$s1$0$e2$0,1$join",
        [ LoweredClosureCall
            (LoweredBlockParameterOperand (LoweredParameterId "live1") closureRepresentation)
            [LoweredBlockParameterOperand (LoweredParameterId "result") intRepresentation]
        ]
      )
    )
  ]
  where
    closureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [intRepresentation] intRepresentation)

expectedClosureCallCounts :: [(Text, Int)]
expectedClosureCallCounts =
  [ ("pattern-case-captured-scalar", 1),
    ("scalar-pattern-case-closure-result", 1),
    ("pattern-case-call-argument", 1)
  ]

expectedPatternCaseTransportShapes :: [(Text, [(LoweredFunctionId, [(LoweredBlockId, [LoweredRepresentation], [(LoweredBlockId, [LoweredRepresentation])])])])]
expectedPatternCaseTransportShapes =
  [ ( "pattern-case-ambient-scalar",
      [ ( entryFunction,
          [ shape ambientPrefix "$a0$guard" intPair [("$a0$body", intPair), ("$a1$body", intPair)],
            shape ambientPrefix "$a0$body" intPair [("$join", intPair)],
            shape ambientPrefix "$a1$body" intPair [("$join", intPair)],
            shape ambientPrefix "$join" intPair []
          ]
        )
      ]
    ),
    ( "pattern-case-captured-scalar",
      [ ( LoweredFunctionId "App::Main::choose",
          [ shape capturedPrefix "$a0$guard" intSingle [("$a0$body", intSingle), ("$a1$body", intSingle)],
            shape capturedPrefix "$a0$body" intSingle [],
            shape capturedPrefix "$a1$body" intSingle []
          ]
        )
      ]
    ),
    ( "scalar-pattern-case-closure-result",
      [ ( LoweredFunctionId "App::Main::choose",
          [ shape closureResultPrefix "$a0$body" [] [],
            shape closureResultPrefix "$a1$body" [] []
          ]
        )
      ]
    ),
    ( "pattern-case-call-argument",
      [ ( entryFunction,
          [ shape callArgumentPrefix "$a0$body" closureSingle [("$join", closureAndInt)],
            shape callArgumentPrefix "$a1$body" closureSingle [("$join", closureAndInt)],
            shape callArgumentPrefix "$join" closureAndInt []
          ]
        )
      ]
    )
  ]
  where
    entryFunction = LoweredFunctionId "App::Main::$entry"
    intSingle = [intRepresentation]
    intPair = [intRepresentation, intRepresentation]
    closureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [intRepresentation] intRepresentation)
    closureSingle = [closureRepresentation]
    closureAndInt = [closureRepresentation, intRepresentation]
    ambientPrefix = "case$s1$3$e1$0"
    capturedPrefix = "case$s1$1$e2$0,0"
    closureResultPrefix = "case$s1$0$e2$0,0"
    callArgumentPrefix = "case$s1$0$e2$0,1"
    shape prefix suffix parameters successors =
      ( LoweredBlockId (prefix <> suffix),
        parameters,
        [(LoweredBlockId (prefix <> targetSuffix), arguments) | (targetSuffix, arguments) <- successors]
      )

expectedPatternCaseControlFlows :: [(Text, [(LoweredFunctionId, [LoweredBlock])])]
expectedPatternCaseControlFlows =
  [ ( "pattern-case-in-conditional-branch",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) outerThenBlockId [] outerElseBlockId [])),
            LoweredBlock
              outerThenBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      nestedFirstBodyBlockId
                      []
                      nestedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              nestedFirstBodyBlockId
              []
              []
              (Just (LoweredJump nestedJoinBlockId [intImmediate 10])),
            LoweredBlock
              nestedFinalBodyBlockId
              []
              []
              (Just (LoweredJump nestedJoinBlockId [intImmediate 20])),
            LoweredBlock
              nestedJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump outerJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              outerElseBlockId
              []
              []
              (Just (LoweredJump outerJoinBlockId [intImmediate 30])),
            LoweredBlock
              outerJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "conditional-in-pattern-case-guard",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      guardedArmGuardBlockId
                      []
                      guardedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              guardedArmGuardBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) guardThenBlockId [] guardElseBlockId [])),
            LoweredBlock
              guardThenBlockId
              []
              []
              (Just (LoweredJump guardJoinBlockId [boolImmediate False])),
            LoweredBlock
              guardElseBlockId
              []
              []
              (Just (LoweredJump guardJoinBlockId [boolImmediate True])),
            LoweredBlock
              guardJoinBlockId
              [parameter "result" LoweredBoolRepresentation]
              []
              ( Just
                  ( LoweredBranch
                      (blockParameter "result" LoweredBoolRepresentation)
                      guardedArmBodyBlockId
                      []
                      guardedFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              guardedArmBodyBlockId
              []
              []
              (Just (LoweredJump guardedJoinBlockId [intImmediate 10])),
            LoweredBlock
              guardedFinalBodyBlockId
              []
              []
              (Just (LoweredJump guardedJoinBlockId [intImmediate 20])),
            LoweredBlock
              guardedJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "pattern-case-in-pattern-case-body",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (boolImmediate True) (boolImmediate True)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      outerCaseFirstBodyBlockId
                      []
                      outerCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              outerCaseFirstBodyBlockId
              []
              [comparisonInstruction 1 (intImmediate 1) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      bodyCaseFirstBodyBlockId
                      []
                      bodyCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              bodyCaseFirstBodyBlockId
              []
              []
              (Just (LoweredJump bodyCaseJoinBlockId [intImmediate 10])),
            LoweredBlock
              bodyCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump bodyCaseJoinBlockId [intImmediate 20])),
            LoweredBlock
              bodyCaseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump outerCaseJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              outerCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump outerCaseJoinBlockId [intImmediate 30])),
            LoweredBlock
              outerCaseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "pattern-case-scrutinee-pattern-case",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (boolImmediate True) (boolImmediate True)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      scrutineeCaseFirstBodyBlockId
                      []
                      scrutineeCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              scrutineeCaseFirstBodyBlockId
              []
              []
              (Just (LoweredJump scrutineeCaseJoinBlockId [intImmediate 1])),
            LoweredBlock
              scrutineeCaseFinalBodyBlockId
              []
              []
              (Just (LoweredJump scrutineeCaseJoinBlockId [intImmediate 2])),
            LoweredBlock
              scrutineeCaseJoinBlockId
              [parameter "result" intRepresentation]
              [ comparisonInstruction
                  1
                  (blockParameter "result" intRepresentation)
                  (intImmediate 1)
              ]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      scrutineeOuterFirstBodyBlockId
                      [blockParameter "result" intRepresentation]
                      scrutineeOuterFinalBodyBlockId
                      [blockParameter "result" intRepresentation]
                  )
              ),
            LoweredBlock
              scrutineeOuterFirstBodyBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredJump scrutineeOuterJoinBlockId [intImmediate 10])),
            LoweredBlock
              scrutineeOuterFinalBodyBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredJump scrutineeOuterJoinBlockId [intImmediate 20])),
            LoweredBlock
              scrutineeOuterJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    ),
    ( "scalar-pattern-case-tail-function",
      [ ( functionId "loop",
          [ LoweredBlock
              entryBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 0)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseFirstBodyBlockId
                      []
                      tailCaseSecondTestBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseFirstBodyBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 0))),
            LoweredBlock
              tailCaseSecondTestBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 1)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseSecondBodyBlockId
                      []
                      tailCaseGuardBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseSecondBodyBlockId
              []
              []
              (Just (LoweredDirectTailCall (functionId "loop") [intImmediate 0])),
            LoweredBlock
              tailCaseGuardBlockId
              []
              [comparisonInstruction 1 (functionParameter "arg1" intRepresentation) (intImmediate 2)]
              ( Just
                  ( LoweredBranch
                      (temporary 1 LoweredBoolRepresentation)
                      tailCaseGuardBodyBlockId
                      []
                      tailCaseFinalBodyBlockId
                      []
                  )
              ),
            LoweredBlock
              tailCaseGuardBodyBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg1" intRepresentation))),
            LoweredBlock
              tailCaseFinalBodyBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 3)))
          ]
        )
      ]
    )
  ]
  where
    comparisonInstruction :: Int -> LoweredOperand -> LoweredOperand -> LoweredInstruction
    comparisonInstruction index left right =
      LoweredInstruction
        (LoweredTemporaryId ("t" <> Text.pack (show index)))
        LoweredBoolRepresentation
        ( LoweredPrimitiveOperation
            (LoweredComparisonPrimitive LoweredEqual)
            [left, right]
        )
    entryBlockId = blockId "entry"
    outerThenBlockId = blockId "if$s1$0$e1$0$then"
    outerElseBlockId = blockId "if$s1$0$e1$0$else"
    outerJoinBlockId = blockId "if$s1$0$e1$0$join"
    nestedFirstBodyBlockId = blockId "case$s1$0$e2$0,1$a0$body"
    nestedFinalBodyBlockId = blockId "case$s1$0$e2$0,1$a1$body"
    nestedJoinBlockId = blockId "case$s1$0$e2$0,1$join"
    guardedArmGuardBlockId = blockId "case$s1$0$e1$0$a0$guard"
    guardedArmBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    guardedFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    guardedJoinBlockId = blockId "case$s1$0$e1$0$join"
    guardThenBlockId = blockId "if$s1$0$e3$0,1,0$then"
    guardElseBlockId = blockId "if$s1$0$e3$0,1,0$else"
    guardJoinBlockId = blockId "if$s1$0$e3$0,1,0$join"
    outerCaseFirstBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    outerCaseFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    outerCaseJoinBlockId = blockId "case$s1$0$e1$0$join"
    bodyCaseFirstBodyBlockId = blockId "case$s1$0$e3$0,1,1$a0$body"
    bodyCaseFinalBodyBlockId = blockId "case$s1$0$e3$0,1,1$a1$body"
    bodyCaseJoinBlockId = blockId "case$s1$0$e3$0,1,1$join"
    scrutineeCaseFirstBodyBlockId = blockId "case$s1$0$e2$0,0$a0$body"
    scrutineeCaseFinalBodyBlockId = blockId "case$s1$0$e2$0,0$a1$body"
    scrutineeCaseJoinBlockId = blockId "case$s1$0$e2$0,0$join"
    scrutineeOuterFirstBodyBlockId = blockId "case$s1$0$e1$0$a0$body"
    scrutineeOuterFinalBodyBlockId = blockId "case$s1$0$e1$0$a1$body"
    scrutineeOuterJoinBlockId = blockId "case$s1$0$e1$0$join"
    tailCaseFirstBodyBlockId = blockId "case$s1$1$e2$0,0$a0$body"
    tailCaseSecondTestBlockId = blockId "case$s1$1$e2$0,0$a1$test"
    tailCaseSecondBodyBlockId = blockId "case$s1$1$e2$0,0$a1$body"
    tailCaseGuardBlockId = blockId "case$s1$1$e2$0,0$a2$guard"
    tailCaseGuardBodyBlockId = blockId "case$s1$1$e2$0,0$a2$body"
    tailCaseFinalBodyBlockId = blockId "case$s1$1$e2$0,0$a3$body"

testConditionalProfileCoverage :: IO ()
testConditionalProfileCoverage =
  mapM_ assertConditionalProfile names
  where
    names =
      [ "conditional-function-parameter",
        "conditional-captured-scalar",
        "conditional-tail-call-function",
        "conditional-closure-result-application",
        "nested-conditionals"
      ]
    assertConditionalProfile name = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstProduction <- produceFixture fixture
      secondProduction <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstProduction)
      assertEqual (name <> " repeatable production") firstProduction secondProduction
      case typedCoreProductionStatus firstProduction of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              case lookup name expectedConditionalControlFlows of
                Just expectedControlFlow ->
                  assertEqual
                    (name <> " exact conditional control flow")
                    expectedControlFlow
                    (conditionalControlFlow loweredProgram)
                Nothing -> failTest (name <> " is missing a conditional control-flow expectation")
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

conditionalControlFlow :: LoweredProgram -> [(LoweredFunctionId, [LoweredBlock])]
conditionalControlFlow (LoweredProgram _ _ _ functions _) =
  [ (loweredFunctionId, blocks)
  | LoweredFunction loweredFunctionId _ _ _ blocks _ <- functions,
    any containsBranch blocks
  ]
  where
    containsBranch (LoweredBlock _ _ _ (Just (LoweredBranch _ _ _ _ _))) = True
    containsBranch _ = False

expectedConditionalControlFlows :: [(Text, [(LoweredFunctionId, [LoweredBlock])])]
expectedConditionalControlFlows =
  [ ( "conditional-function-parameter",
      [ ( functionId "choose",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      parameterThenBlockId
                      []
                      parameterElseBlockId
                      []
                  )
              ),
            LoweredBlock
              parameterThenBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg2" intRepresentation))),
            LoweredBlock
              parameterElseBlockId
              []
              []
              (Just (LoweredReturn (intImmediate 0)))
          ]
        )
      ]
    ),
    ( "conditional-captured-scalar",
      [ ( functionId "choose",
          [ LoweredBlock
              entryBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  intRepresentation
                  ( LoweredProjectField
                      capturedSeedLayoutId
                      0
                      (functionParameter "environment" (LoweredManagedReferenceRepresentation capturedSeedLayoutId))
                  )
              ]
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      capturedThenBlockId
                      [temporary 1 intRepresentation]
                      capturedElseBlockId
                      [temporary 1 intRepresentation]
                  )
              ),
            LoweredBlock
              capturedThenBlockId
              [parameter "live1" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "live1" intRepresentation))),
            LoweredBlock
              capturedElseBlockId
              [parameter "live1" intRepresentation]
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  intRepresentation
                  ( LoweredPrimitiveOperation
                      (LoweredArithmeticPrimitive LoweredAdd)
                      [blockParameter "live1" intRepresentation, intImmediate 2]
                  )
              ]
              (Just (LoweredReturn (temporary 1 intRepresentation)))
          ]
        )
      ]
    ),
    ( "conditional-tail-call-function",
      [ ( functionId "loop",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (functionParameter "arg1" LoweredBoolRepresentation)
                      tailThenBlockId
                      []
                      tailElseBlockId
                      []
                  )
              ),
            LoweredBlock
              tailThenBlockId
              []
              []
              (Just (LoweredReturn (functionParameter "arg2" intRepresentation))),
            LoweredBlock
              tailElseBlockId
              []
              []
              ( Just
                  ( LoweredDirectTailCall
                      (functionId "loop")
                      [boolImmediate True, functionParameter "arg2" intRepresentation]
                  )
              )
          ]
        )
      ]
    ),
    ( "conditional-closure-result-application",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              ( Just
                  ( LoweredBranch
                      (boolImmediate True)
                      closureThenBlockId
                      []
                      closureElseBlockId
                      []
                  )
              ),
            LoweredBlock
              closureThenBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  (LoweredManagedReferenceRepresentation identityLayoutId)
                  (LoweredConstructProduct identityLayoutId []),
                LoweredInstruction
                  (LoweredTemporaryId "t2")
                  boolClosureRepresentation
                  ( LoweredConstructClosure
                      (functionId "identity")
                      (temporary 1 (LoweredManagedReferenceRepresentation identityLayoutId))
                  )
              ]
              (Just (LoweredJump closureJoinBlockId [temporary 2 boolClosureRepresentation])),
            LoweredBlock
              closureElseBlockId
              []
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  (LoweredManagedReferenceRepresentation alwaysFalseLayoutId)
                  (LoweredConstructProduct alwaysFalseLayoutId []),
                LoweredInstruction
                  (LoweredTemporaryId "t2")
                  boolClosureRepresentation
                  ( LoweredConstructClosure
                      (functionId "alwaysFalse")
                      (temporary 1 (LoweredManagedReferenceRepresentation alwaysFalseLayoutId))
                  )
              ]
              (Just (LoweredJump closureJoinBlockId [temporary 2 boolClosureRepresentation])),
            LoweredBlock
              closureJoinBlockId
              [parameter "result" boolClosureRepresentation]
              [ LoweredInstruction
                  (LoweredTemporaryId "t1")
                  LoweredBoolRepresentation
                  ( LoweredClosureCall
                      (blockParameter "result" boolClosureRepresentation)
                      [boolImmediate True]
                  )
              ]
              (Just (LoweredReturn (temporary 1 LoweredBoolRepresentation)))
          ]
        )
      ]
    ),
    ( "nested-conditionals",
      [ ( functionId "$entry",
          [ LoweredBlock
              entryBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) nestedConditionThenBlockId [] nestedConditionElseBlockId [])),
            LoweredBlock
              nestedConditionThenBlockId
              []
              []
              (Just (LoweredJump nestedConditionJoinBlockId [boolImmediate False])),
            LoweredBlock
              nestedConditionElseBlockId
              []
              []
              (Just (LoweredJump nestedConditionJoinBlockId [boolImmediate True])),
            LoweredBlock
              nestedConditionJoinBlockId
              [parameter "result" LoweredBoolRepresentation]
              []
              ( Just
                  ( LoweredBranch
                      (blockParameter "result" LoweredBoolRepresentation)
                      nestedOuterThenBlockId
                      []
                      nestedOuterElseBlockId
                      []
                  )
              ),
            LoweredBlock
              nestedOuterThenBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate True) nestedThenThenBlockId [] nestedThenElseBlockId [])),
            LoweredBlock
              nestedThenThenBlockId
              []
              []
              (Just (LoweredJump nestedThenJoinBlockId [intImmediate 1])),
            LoweredBlock
              nestedThenElseBlockId
              []
              []
              (Just (LoweredJump nestedThenJoinBlockId [intImmediate 2])),
            LoweredBlock
              nestedThenJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump nestedOuterJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              nestedOuterElseBlockId
              []
              []
              (Just (LoweredBranch (boolImmediate False) nestedElseThenBlockId [] nestedElseElseBlockId [])),
            LoweredBlock
              nestedElseThenBlockId
              []
              []
              (Just (LoweredJump nestedElseJoinBlockId [intImmediate 3])),
            LoweredBlock
              nestedElseElseBlockId
              []
              []
              (Just (LoweredJump nestedElseJoinBlockId [intImmediate 4])),
            LoweredBlock
              nestedElseJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredJump nestedOuterJoinBlockId [blockParameter "result" intRepresentation])),
            LoweredBlock
              nestedOuterJoinBlockId
              [parameter "result" intRepresentation]
              []
              (Just (LoweredReturn (blockParameter "result" intRepresentation)))
          ]
        )
      ]
    )
  ]
  where
    boolClosureRepresentation :: LoweredRepresentation
    boolClosureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)
    entryBlockId = blockId "entry"
    parameterThenBlockId = blockId "if$s1$1$e3$0,0,0$then"
    parameterElseBlockId = blockId "if$s1$1$e3$0,0,0$else"
    capturedSeedLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n6:choose"
    capturedThenBlockId = blockId "if$s1$3$e2$0,0$then"
    capturedElseBlockId = blockId "if$s1$3$e2$0,0$else"
    tailThenBlockId = blockId "if$s1$1$e3$0,0,0$then"
    tailElseBlockId = blockId "if$s1$1$e3$0,0,0$else"
    identityLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$1$n8:identity"
    alwaysFalseLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n11:alwaysFalse"
    closureThenBlockId = blockId "if$s1$4$e2$0,0$then"
    closureElseBlockId = blockId "if$s1$4$e2$0,0$else"
    closureJoinBlockId = blockId "if$s1$4$e2$0,0$join"
    nestedConditionThenBlockId = blockId "if$s1$0$e2$0,0$then"
    nestedConditionElseBlockId = blockId "if$s1$0$e2$0,0$else"
    nestedConditionJoinBlockId = blockId "if$s1$0$e2$0,0$join"
    nestedOuterThenBlockId = blockId "if$s1$0$e1$0$then"
    nestedOuterElseBlockId = blockId "if$s1$0$e1$0$else"
    nestedOuterJoinBlockId = blockId "if$s1$0$e1$0$join"
    nestedThenThenBlockId = blockId "if$s1$0$e2$0,1$then"
    nestedThenElseBlockId = blockId "if$s1$0$e2$0,1$else"
    nestedThenJoinBlockId = blockId "if$s1$0$e2$0,1$join"
    nestedElseThenBlockId = blockId "if$s1$0$e2$0,2$then"
    nestedElseElseBlockId = blockId "if$s1$0$e2$0,2$else"
    nestedElseJoinBlockId = blockId "if$s1$0$e2$0,2$join"

testScalarBindingProduction :: IO ()
testScalarBindingProduction = do
  mapM_ assertProduced scalarBindingExpectedPrograms
  assertFailedBindingHidden
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

    assertFailedBindingHidden = do
      let fixture = producerEdgeFixture "scalar-binding-failed-initializer-hidden"
          expected =
            TypedCoreProductionUnsupported
              [ expressionFailure 0 [0] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_toFloat64"),
                expressionFailure 1 [] TypedCoreCaptureUnsupported (TypedCoreNameDetail "failed")
              ]
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual "failed scalar binding repeatable rejection" firstRun secondRun
      assertEqual "failed scalar binding remains hidden" expected (typedCoreProductionStatus firstRun)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testManagedTextProduction :: IO ()
testManagedTextProduction =
  mapM_ assertProduced managedTextExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testManagedTextOperationProduction :: IO ()
testManagedTextOperationProduction =
  mapM_ assertProduced managedTextOperationExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testManagedTextOperationLowering :: IO ()
testManagedTextOperationLowering =
  mapM_ assertLowered managedTextOperationExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact service lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " valid expected Lowered IR") [] (validateLoweredProgram expectedProgram)

testManagedTextKernelBoundaries :: IO ()
testManagedTextKernelBoundaries = do
  mapM_ assertKernelUnsupported unsupportedExpectations
  assertBlockedByDiagnostics "managed-text-oversaturated-length"
  where
    unsupportedExpectations =
      [ ( "managed-text-bare-length",
          TypedCoreCallableValueUnsupported,
          TypedCoreNameDetail "__kernel_textLength"
        ),
        ( "managed-text-partial-append",
          TypedCoreCallArityUnsupported,
          TypedCoreArityDetail 2 1
        ),
        ( "managed-text-partial-append-char",
          TypedCoreCallArityUnsupported,
          TypedCoreArityDetail 2 1
        )
      ]
    assertKernelUnsupported (name, kind, detail) = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  kind
                  detail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " exact producer boundary") expected (typedCoreProductionStatus firstRun)
    assertBlockedByDiagnostics name = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " diagnostic precedence") TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus firstRun)

testManagedTextProfileExclusions :: IO ()
testManagedTextProfileExclusions = do
  mapM_ assertExcluded exactExclusions
  mapM_ assertManifestExclusion ["text-value", "resolved-import", "imported-direct-call"]
  where
    exactExclusions =
      [ ( "managed-text-literal-pattern",
          [expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]
        ),
        ( "managed-text-uncons",
          [expressionFailure 0 [] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_textUnconsRaw")]
        ),
        ( "managed-text-from-chars",
          [expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
        ),
        ( "managed-text-concat",
          [expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
        ),
        ( "managed-text-read-io",
          [expressionFailure 0 [] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_readTextRaw!")]
        ),
        ( "managed-text-write-io",
          [expressionFailure 0 [] TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_writeTextRaw!")]
        )
      ]
    assertExcluded (name, failures) = do
      let fixture = producerEdgeFixture name
          expected = TypedCoreProductionUnsupported failures
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable exclusion") firstRun secondRun
      assertEqual (name <> " exact exclusion") expected (typedCoreProductionStatus firstRun)
    assertManifestExclusion name = do
      let fixture = fixtureByName name
          expected =
            case lookup name rejectedManifestExpectedStatuses of
              Just status -> status
              Nothing -> error ("managed Text manifest exclusion is missing: " <> Text.unpack name)
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable established exclusion") firstRun secondRun
      assertEqual (name <> " exact established exclusion") expected (typedCoreProductionStatus firstRun)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testManagedTextServiceValidationOwnership :: IO ()
testManagedTextServiceValidationOwnership = do
  assertEqual
    "managed Text malformed service signature"
    [ LoweredIRValidationFailure
        (LoweredInstructionPath entryFunctionId entryBlockId 2)
        LoweredRuntimeCallSignatureMismatch
        (LoweredRepresentationDetail LoweredBoolRepresentation textRepresentation)
    ]
    (validateLoweredProgram malformedSignatureProgram)
  assertEqual
    "managed Text missing service reference"
    [ LoweredIRValidationFailure
        (LoweredInstructionPath entryFunctionId entryBlockId 2)
        LoweredUnknownRuntimeService
        (LoweredIdentifierDetail "jazz.runtime.text.equal.v1")
    ]
    (validateLoweredProgram missingServiceProgram)
  where
    textLayoutId = LoweredLayoutId "jazz.layout.text.v1"
    textLayout = LoweredLayout textLayoutId LoweredTextLayout
    textRepresentation = LoweredManagedReferenceRepresentation textLayoutId
    equalityServiceId = LoweredRuntimeServiceId "jazz.runtime.text.equal.v1"
    entryFunctionId = LoweredFunctionId "App::Main::$entry"
    entryBlockId = LoweredBlockId "entry"
    malformedSignatureProgram = serviceProgram [malformedService]
    missingServiceProgram = serviceProgram []
    malformedService =
      LoweredRuntimeService
        equalityServiceId
        (LoweredCallSignature [LoweredBoolRepresentation, textRepresentation] LoweredBoolRepresentation)
    serviceProgram services =
      LoweredProgram
        (LoweredIRVersion 1)
        [textLayout]
        services
        [ LoweredFunction
            entryFunctionId
            Nothing
            []
            LoweredBoolRepresentation
            [ LoweredBlock
                entryBlockId
                []
                [ textInstruction 1 "left",
                  textInstruction 2 "right",
                  LoweredInstruction
                    (LoweredTemporaryId "t3")
                    LoweredBoolRepresentation
                    ( LoweredRuntimeCall
                        equalityServiceId
                        [ textTemporary 1,
                          textTemporary 2
                        ]
                    )
                ]
                (Just (LoweredReturn (LoweredTemporaryOperand (LoweredTemporaryId "t3") LoweredBoolRepresentation)))
            ]
            entryBlockId
        ]
        entryFunctionId
    textInstruction :: Int -> Text -> LoweredInstruction
    textInstruction index value =
      LoweredInstruction
        (LoweredTemporaryId ("t" <> Text.pack (show index)))
        textRepresentation
        (LoweredConstructText textLayoutId value)
    textTemporary :: Int -> LoweredOperand
    textTemporary index =
      LoweredTemporaryOperand
        (LoweredTemporaryId ("t" <> Text.pack (show index)))
        textRepresentation
