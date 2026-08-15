{-# LANGUAGE OverloadedStrings #-}

-- | Call, capture, currying, recursion, and closure artifacts.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.CallsCaptures where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedText
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Scalar
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.TypedCore

scalarBindingExpectedPrograms :: [(Text, TypedProgram)]
scalarBindingExpectedPrograms =
  [ ("scalar-binding-literal", scalarBindingLiteralProgram),
    ("scalar-binding-ordered-reuse", scalarBindingOrderedReuseProgram),
    ("scalar-binding-direct-call-result", scalarBindingDirectCallResultProgram)
  ]

scalarBindingExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
scalarBindingExpectedLoweredPrograms =
  [ ( "scalar-binding-literal",
      scalarBindingLiteralProgram,
      expectedCallableLoweredProgram
        []
        int64Representation
        [expectedPrimitiveInstruction 1 int64Representation (LoweredArithmeticPrimitive LoweredAdd) [loweredInt64 40, loweredInt64 2]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "scalar-binding-ordered-reuse",
      scalarBindingOrderedReuseProgram,
      expectedCallableLoweredProgram
        []
        int64Representation
        [expectedPrimitiveInstruction 1 int64Representation (LoweredArithmeticPrimitive LoweredAdd) [loweredInt64 40, loweredInt64 2]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "scalar-binding-direct-call-result",
      scalarBindingDirectCallResultProgram,
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "identity"
            [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
            LoweredBoolRepresentation
            []
            (loweredParameter 1 LoweredBoolRepresentation)
        ]
        LoweredBoolRepresentation
        [expectedDirectCallInstruction 1 LoweredBoolRepresentation "identity" [loweredImmediate (LoweredBoolImmediate True)]]
        (loweredTemporary 1 LoweredBoolRepresentation)
    )
  ]

functionBodyConsumedCallExpectedProgram :: (TypedProgram, LoweredProgram)
functionBodyConsumedCallExpectedProgram =
  ( typedProgram,
    expectedCallableLoweredProgram
      [ expectedLocalFunction
          "identity"
          [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
          LoweredBoolRepresentation
          []
          (loweredParameter 1 LoweredBoolRepresentation),
        expectedLocalFunction
          "consumeDirect"
          [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
          LoweredBoolRepresentation
          [ expectedDirectCallInstruction 1 LoweredBoolRepresentation "identity" [loweredImmediate (LoweredBoolImmediate True)],
            expectedPrimitiveInstruction
              2
              LoweredBoolRepresentation
              (LoweredComparisonPrimitive LoweredEqual)
              [loweredTemporary 1 LoweredBoolRepresentation, loweredImmediate (LoweredBoolImmediate False)]
          ]
          (loweredTemporary 2 LoweredBoolRepresentation),
        expectedLocalFunction
          "consumeClosure"
          [LoweredParameter (LoweredParameterId "arg1") boolClosureRepresentation]
          LoweredBoolRepresentation
          [ expectedClosureCallInstruction
              1
              LoweredBoolRepresentation
              (loweredParameter 1 boolClosureRepresentation)
              [loweredImmediate (LoweredBoolImmediate True)],
            expectedPrimitiveInstruction
              2
              LoweredBoolRepresentation
              (LoweredComparisonPrimitive LoweredEqual)
              [loweredTemporary 1 LoweredBoolRepresentation, loweredImmediate (LoweredBoolImmediate False)]
          ]
          (loweredTemporary 2 LoweredBoolRepresentation)
      ]
      LoweredBoolRepresentation
      []
      (loweredImmediate (LoweredBoolImmediate True))
  )
  where
    typedProgram =
      expectedFunctionProgram
        []
        [ ExpectedFunction
            "identity"
            [("item", boolInfo)]
            boolInfo
            TypedDirectCallableShape
            (variableExpr "item" boolInfo),
          ExpectedFunction
            "consumeDirect"
            [("ignored", boolInfo)]
            boolInfo
            TypedDirectCallableShape
            ( binaryExpr
                boolInfo
                "=="
                (directCall "identity" [boolInfo] boolInfo [boolExpr True])
                (boolExpr False)
            ),
          ExpectedFunction
            "consumeClosure"
            [("function", boolCallableInfo)]
            boolInfo
            TypedDirectCallableShape
            ( binaryExpr
                boolInfo
                "=="
                (directCall "function" [boolInfo] boolInfo [boolExpr True])
                (boolExpr False)
            )
        ]
        (boolExpr True)

lexicalCaptureExpectedPrograms :: [(Text, TypedProgram)]
lexicalCaptureExpectedPrograms =
  [ ("capturing-function", capturingProducerProgram),
    ("anonymous-lambda-result", anonymousLambdaResultProgram)
  ]

curriedApplicationExpectedPrograms :: [(Text, TypedProgram)]
curriedApplicationExpectedPrograms =
  [ ("partial-direct-call", curriedPartialApplicationProgram),
    ("curried-partial-application", curriedPartialApplicationProgram),
    ("curried-callable-oversaturation", curriedCallableOversaturationProgram),
    ("curried-partial-higher-order-consumer", curriedPartialHigherOrderProgram),
    ("inline-curried-lambda-call", inlineCurriedLambdaProgram),
    ("curried-named-function-value", curriedNamedFunctionValueProgram)
  ]

curriedPartialApplicationProgram :: TypedProgram
curriedPartialApplicationProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [combineFunction {expectedFunctionShape = TypedClosureCallableShape}]
    ( TypedApplyExpr
        remainingInfo
        (variableExpr "combine" combineInfo)
        (intExpr 1)
    )
  where
    combineInfo = stagedFunctionInfo [("left", intInfo), ("right", intInfo)] intInfo
    remainingInfo = stagedFunctionInfo [("right", intInfo)] intInfo

functionBodyPartialApplicationExpectedProgram :: (TypedProgram, LoweredProgram)
functionBodyPartialApplicationExpectedProgram =
  ( typedProgram,
    expectedClosureCallableLoweredProgram
      curriedCombineLayouts
      ( curriedCombineFunctions
          <> [ expectedLocalFunction
                 "partial"
                 [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
                 curriedCombineInnerClosureRepresentation
                 [ expectedEmptyEnvironmentInstruction 1 curriedCombineOuterLayoutId,
                   LoweredInstruction
                     (LoweredTemporaryId "t2")
                     curriedCombineOuterClosureRepresentation
                     ( LoweredConstructClosure
                         (LoweredFunctionId "App::Main::combine")
                         (loweredTemporary 1 (LoweredManagedReferenceRepresentation curriedCombineOuterLayoutId))
                     ),
                   expectedClosureCallInstruction
                     3
                     curriedCombineInnerClosureRepresentation
                     (loweredTemporary 2 curriedCombineOuterClosureRepresentation)
                     [loweredInt64 1]
                 ]
                 (loweredTemporary 3 curriedCombineInnerClosureRepresentation)
             ]
      )
      LoweredBoolRepresentation
      []
      (loweredImmediate (LoweredBoolImmediate True))
  )
  where
    typedProgram =
      expectedFunctionProgramWithLineOffset
        1
        []
        [ combineFunction {expectedFunctionShape = TypedClosureCallableShape},
          ExpectedFunction
            "partial"
            [("ignored", boolInfo)]
            remainingInfo
            TypedDirectCallableShape
            ( TypedApplyExpr
                remainingInfo
                (variableExpr "combine" combineInfo)
                (intExpr 1)
            )
        ]
        (boolExpr True)
    combineInfo = stagedFunctionInfo [("left", intInfo), ("right", intInfo)] intInfo
    remainingInfo = stagedFunctionInfo [("right", intInfo)] intInfo

nestedTailControlFlowExpectedLoweredPrograms :: [(Text, LoweredProgram)]
nestedTailControlFlowExpectedLoweredPrograms =
  [ ( "nested-tail-if-alternatives",
      expectedCallableLoweredProgram
        [ LoweredFunction
            (functionId "chooseNestedIf")
            Nothing
            [ parameter "arg1" LoweredBoolRepresentation,
              parameter "arg2" LoweredBoolRepresentation,
              parameter "arg3" intRepresentation
            ]
            intRepresentation
            [ LoweredBlock
                entryBlockId
                []
                []
                (Just (LoweredBranch (functionParameter "arg1" LoweredBoolRepresentation) outerIfThenBlockId [] outerIfElseBlockId [])),
              LoweredBlock
                outerIfThenBlockId
                []
                []
                (Just (LoweredBranch (functionParameter "arg2" LoweredBoolRepresentation) nestedIfThenBlockId [] nestedIfElseBlockId [])),
              LoweredBlock
                nestedIfThenBlockId
                []
                []
                (Just (LoweredReturn (functionParameter "arg3" intRepresentation))),
              LoweredBlock
                nestedIfElseBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 1))),
              LoweredBlock
                outerIfElseBlockId
                []
                [comparisonInstruction (functionParameter "arg3" intRepresentation) (intImmediate 0)]
                (Just (LoweredBranch boolTemporary nestedIfCaseFirstBodyBlockId [] nestedIfCaseFinalBodyBlockId [])),
              LoweredBlock
                nestedIfCaseFirstBodyBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 2))),
              LoweredBlock
                nestedIfCaseFinalBodyBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 3)))
            ]
            entryBlockId
        ]
        intRepresentation
        [ expectedDirectCallInstruction
            1
            intRepresentation
            "chooseNestedIf"
            [boolImmediate True, boolImmediate False, intImmediate 9]
        ]
        (loweredTemporary 1 intRepresentation)
    ),
    ( "nested-tail-case-bodies",
      expectedCallableLoweredProgram
        [ LoweredFunction
            (functionId "chooseNestedCase")
            Nothing
            [ parameter "arg1" intRepresentation,
              parameter "arg2" LoweredBoolRepresentation
            ]
            intRepresentation
            [ LoweredBlock
                entryBlockId
                []
                [comparisonInstruction (functionParameter "arg1" intRepresentation) (intImmediate 0)]
                (Just (LoweredBranch boolTemporary outerCaseFirstBodyBlockId [] outerCaseFinalBodyBlockId [])),
              LoweredBlock
                outerCaseFirstBodyBlockId
                []
                []
                (Just (LoweredBranch (functionParameter "arg2" LoweredBoolRepresentation) caseIfThenBlockId [] caseIfElseBlockId [])),
              LoweredBlock
                caseIfThenBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 1))),
              LoweredBlock
                caseIfElseBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 2))),
              LoweredBlock
                outerCaseFinalBodyBlockId
                []
                [comparisonInstruction (functionParameter "arg1" intRepresentation) (intImmediate 1)]
                (Just (LoweredBranch boolTemporary nestedCaseFirstBodyBlockId [] nestedCaseFinalBodyBlockId [])),
              LoweredBlock
                nestedCaseFirstBodyBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 3))),
              LoweredBlock
                nestedCaseFinalBodyBlockId
                []
                []
                (Just (LoweredReturn (intImmediate 4)))
            ]
            entryBlockId
        ]
        intRepresentation
        [expectedDirectCallInstruction 1 intRepresentation "chooseNestedCase" [intImmediate 0, boolImmediate True]]
        (loweredTemporary 1 intRepresentation)
    )
  ]
  where
    functionId name = LoweredFunctionId ("App::Main::" <> name)
    parameter name representation = LoweredParameter (LoweredParameterId name) representation
    functionParameter name representation =
      LoweredFunctionParameterOperand (LoweredParameterId name) representation
    boolImmediate = loweredImmediate . LoweredBoolImmediate
    intImmediate = loweredImmediate . LoweredSignedIntegerImmediate LoweredIntegerWidth64
    boolTemporary = loweredTemporary 1 LoweredBoolRepresentation
    comparisonInstruction left right =
      LoweredInstruction
        (LoweredTemporaryId "t1")
        LoweredBoolRepresentation
        (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [left, right])
    intRepresentation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    entryBlockId = LoweredBlockId "entry"
    outerIfThenBlockId = LoweredBlockId "if$s1$1$e4$0,0,0,0$then"
    outerIfElseBlockId = LoweredBlockId "if$s1$1$e4$0,0,0,0$else"
    nestedIfThenBlockId = LoweredBlockId "if$s1$1$e5$0,0,0,0,1$then"
    nestedIfElseBlockId = LoweredBlockId "if$s1$1$e5$0,0,0,0,1$else"
    nestedIfCaseFirstBodyBlockId = LoweredBlockId "case$s1$1$e5$0,0,0,0,2$a0$body"
    nestedIfCaseFinalBodyBlockId = LoweredBlockId "case$s1$1$e5$0,0,0,0,2$a1$body"
    outerCaseFirstBodyBlockId = LoweredBlockId "case$s1$1$e3$0,0,0$a0$body"
    outerCaseFinalBodyBlockId = LoweredBlockId "case$s1$1$e3$0,0,0$a1$body"
    caseIfThenBlockId = LoweredBlockId "if$s1$1$e5$0,0,0,1,1$then"
    caseIfElseBlockId = LoweredBlockId "if$s1$1$e5$0,0,0,1,1$else"
    nestedCaseFirstBodyBlockId = LoweredBlockId "case$s1$1$e5$0,0,0,2,1$a0$body"
    nestedCaseFinalBodyBlockId = LoweredBlockId "case$s1$1$e5$0,0,0,2,1$a1$body"

curriedCallableOversaturationProgram :: TypedProgram
curriedCallableOversaturationProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [identity, choose]
    ( TypedApplyExpr
        intInfo
        ( TypedApplyExpr
            intCallableInfo
            (variableExpr "choose" chooseInfo)
            (boolExpr False)
        )
        (intExpr 2)
    )
  where
    intCallableInfo = stagedFunctionInfo [("item", intInfo)] intInfo
    chooseInfo = functionInfo [("ignored", boolInfo)] intCallableInfo
    identity =
      ExpectedFunction
        "identity"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        (variableExpr "item" intInfo)
    choose =
      ExpectedFunction
        "choose"
        [("ignored", boolInfo)]
        intCallableInfo
        TypedDirectCallableShape
        (variableExpr "identity" intCallableInfo)

curriedPartialHigherOrderProgram :: TypedProgram
curriedPartialHigherOrderProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [combine, apply]
    ( directCall
        "apply"
        [remainingInfo]
        intInfo
        [ TypedApplyExpr
            remainingInfo
            (variableExpr "combine" combineInfo)
            (intExpr 1)
        ]
    )
  where
    remainingInfo = stagedFunctionInfo [("right", intInfo)] intInfo
    combineInfo = stagedFunctionInfo [("left", intInfo), ("right", intInfo)] intInfo
    combine = combineFunction {expectedFunctionShape = TypedClosureCallableShape}
    apply =
      ExpectedFunction
        "apply"
        [("function", remainingInfo)]
        intInfo
        TypedDirectCallableShape
        ( TypedApplyExpr
            intInfo
            (variableExpr "function" remainingInfo)
            (intExpr 2)
        )

inlineCurriedLambdaProgram :: TypedProgram
inlineCurriedLambdaProgram =
  expectedRootProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedApplyExpr
            intInfo
            ( TypedApplyExpr
                remainingInfo
                ( TypedLambdaExpr
                    lambdaInfo
                    leftBinder
                    leftName
                    ( TypedLambdaExpr
                        remainingInfo
                        rightBinder
                        rightName
                        ( binaryExpr
                            intInfo
                            "+"
                            (boundVariableExpr leftName intInfo leftBinder)
                            (boundVariableExpr rightName intInfo rightBinder)
                        )
                    )
                )
                (intExpr 20)
            )
            (intExpr 22)
        )
    ]
    intInfo
  where
    leftName = resolvedName "left"
    leftBinder = TypedBinderId (modulePath, [0, 0, 0], leftName)
    rightName = resolvedName "right"
    rightBinder = TypedBinderId (modulePath, [0, 0, 0, 0], rightName)
    remainingInfo = stagedFunctionInfo [("right", intInfo)] intInfo
    lambdaInfo = stagedFunctionInfo [("left", intInfo), ("right", intInfo)] intInfo

curriedNamedFunctionValueProgram :: TypedProgram
curriedNamedFunctionValueProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [combineFunction {expectedFunctionShape = TypedClosureCallableShape}]
    (variableExpr "combine" combineInfo)
  where
    combineInfo = stagedFunctionInfo [("left", intInfo), ("right", intInfo)] intInfo

curriedApplicationExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
curriedApplicationExpectedLoweredPrograms =
  [ ( "partial-direct-call",
      curriedPartialApplicationProgram,
      curriedPartialApplicationLoweredProgram
    ),
    ( "curried-partial-application",
      curriedPartialApplicationProgram,
      curriedPartialApplicationLoweredProgram
    ),
    ( "curried-callable-oversaturation",
      curriedCallableOversaturationProgram,
      curriedCallableOversaturationLoweredProgram
    ),
    ( "curried-partial-higher-order-consumer",
      curriedPartialHigherOrderProgram,
      curriedPartialHigherOrderLoweredProgram
    ),
    ( "inline-curried-lambda-call",
      inlineCurriedLambdaProgram,
      inlineCurriedLambdaLoweredProgram
    ),
    ( "curried-named-function-value",
      curriedNamedFunctionValueProgram,
      curriedNamedFunctionValueLoweredProgram
    )
  ]

curriedPartialApplicationLoweredProgram :: LoweredProgram
curriedPartialApplicationLoweredProgram =
  expectedClosureCallableLoweredProgram
    curriedCombineLayouts
    curriedCombineFunctions
    curriedCombineInnerClosureRepresentation
    [ expectedEmptyEnvironmentInstruction 1 curriedCombineOuterLayoutId,
      LoweredInstruction
        (LoweredTemporaryId "t2")
        curriedCombineOuterClosureRepresentation
        ( LoweredConstructClosure
            (LoweredFunctionId "App::Main::combine")
            (loweredTemporary 1 (LoweredManagedReferenceRepresentation curriedCombineOuterLayoutId))
        ),
      expectedClosureCallInstruction
        3
        curriedCombineInnerClosureRepresentation
        (loweredTemporary 2 curriedCombineOuterClosureRepresentation)
        [loweredInt64 1]
    ]
    (loweredTemporary 3 curriedCombineInnerClosureRepresentation)

curriedNamedFunctionValueLoweredProgram :: LoweredProgram
curriedNamedFunctionValueLoweredProgram =
  expectedClosureCallableLoweredProgram
    curriedCombineLayouts
    curriedCombineFunctions
    curriedCombineOuterClosureRepresentation
    [ expectedEmptyEnvironmentInstruction 1 curriedCombineOuterLayoutId,
      LoweredInstruction
        (LoweredTemporaryId "t2")
        curriedCombineOuterClosureRepresentation
        ( LoweredConstructClosure
            (LoweredFunctionId "App::Main::combine")
            (loweredTemporary 1 (LoweredManagedReferenceRepresentation curriedCombineOuterLayoutId))
        )
    ]
    (loweredTemporary 2 curriedCombineOuterClosureRepresentation)

curriedPartialHigherOrderLoweredProgram :: LoweredProgram
curriedPartialHigherOrderLoweredProgram =
  expectedClosureCallableLoweredProgram
    curriedCombineLayouts
    ( curriedCombineFunctions
        <> [ expectedTailLocalFunction
               "apply"
               [LoweredParameter (LoweredParameterId "arg1") curriedCombineInnerClosureRepresentation]
               int64Representation
               []
               (LoweredClosureTailCall (loweredParameter 1 curriedCombineInnerClosureRepresentation) [loweredInt64 2])
           ]
    )
    int64Representation
    [ expectedEmptyEnvironmentInstruction 1 curriedCombineOuterLayoutId,
      LoweredInstruction
        (LoweredTemporaryId "t2")
        curriedCombineOuterClosureRepresentation
        ( LoweredConstructClosure
            (LoweredFunctionId "App::Main::combine")
            (loweredTemporary 1 (LoweredManagedReferenceRepresentation curriedCombineOuterLayoutId))
        ),
      expectedClosureCallInstruction
        3
        curriedCombineInnerClosureRepresentation
        (loweredTemporary 2 curriedCombineOuterClosureRepresentation)
        [loweredInt64 1],
      expectedDirectCallInstruction
        4
        int64Representation
        "apply"
        [loweredTemporary 3 curriedCombineInnerClosureRepresentation]
    ]
    (loweredTemporary 4 int64Representation)

curriedCombineLayouts :: [LoweredLayout]
curriedCombineLayouts =
  [ LoweredLayout curriedCombineOuterLayoutId (LoweredClosureEnvironmentLayout []),
    LoweredLayout curriedCombineInnerLayoutId (LoweredClosureEnvironmentLayout [int64Representation])
  ]

curriedCombineFunctions :: [LoweredFunction]
curriedCombineFunctions =
  [ LoweredFunction
      (LoweredFunctionId "App::Main::combine")
      (Just (layoutEnvironmentParameter curriedCombineOuterLayoutId))
      [LoweredParameter (LoweredParameterId "arg1") int64Representation]
      curriedCombineInnerClosureRepresentation
      [ LoweredBlock
          (LoweredBlockId "entry")
          []
          [ LoweredInstruction
              (LoweredTemporaryId "t1")
              (LoweredManagedReferenceRepresentation curriedCombineInnerLayoutId)
              ( LoweredConstructProduct
                  curriedCombineInnerLayoutId
                  [loweredParameter 1 int64Representation]
              ),
            LoweredInstruction
              (LoweredTemporaryId "t2")
              curriedCombineInnerClosureRepresentation
              ( LoweredConstructClosure
                  curriedCombineInnerFunctionId
                  (loweredTemporary 1 (LoweredManagedReferenceRepresentation curriedCombineInnerLayoutId))
              )
          ]
          (Just (LoweredReturn (loweredTemporary 2 curriedCombineInnerClosureRepresentation)))
      ]
      (LoweredBlockId "entry"),
    LoweredFunction
      curriedCombineInnerFunctionId
      (Just (layoutEnvironmentParameter curriedCombineInnerLayoutId))
      [LoweredParameter (LoweredParameterId "arg1") int64Representation]
      int64Representation
      [ LoweredBlock
          (LoweredBlockId "entry")
          []
          [ LoweredInstruction
              (LoweredTemporaryId "t1")
              int64Representation
              (LoweredProjectField curriedCombineInnerLayoutId 0 (layoutEnvironmentOperand curriedCombineInnerLayoutId)),
            expectedPrimitiveInstruction
              2
              int64Representation
              (LoweredArithmeticPrimitive LoweredAdd)
              [loweredTemporary 1 int64Representation, loweredParameter 1 int64Representation]
          ]
          (Just (LoweredReturn (loweredTemporary 2 int64Representation)))
      ]
      (LoweredBlockId "entry")
  ]

curriedCombineOuterLayoutId, curriedCombineInnerLayoutId :: LoweredLayoutId
curriedCombineOuterLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$1$n7:combine"
curriedCombineInnerLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p3$1,0,0$n5:right"

curriedCombineInnerFunctionId :: LoweredFunctionId
curriedCombineInnerFunctionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p3$1,0,0$n5:right"

curriedCombineInnerClosureRepresentation, curriedCombineOuterClosureRepresentation :: LoweredRepresentation
curriedCombineInnerClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [int64Representation] int64Representation)
curriedCombineOuterClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [int64Representation] curriedCombineInnerClosureRepresentation)

curriedCallableOversaturationLoweredProgram :: LoweredProgram
curriedCallableOversaturationLoweredProgram =
  expectedClosureCallableLoweredProgram
    [LoweredLayout identityLayoutId (LoweredClosureEnvironmentLayout [])]
    [ LoweredFunction
        (LoweredFunctionId "App::Main::identity")
        (Just (layoutEnvironmentParameter identityLayoutId))
        [LoweredParameter (LoweredParameterId "arg1") int64Representation]
        int64Representation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            (Just (LoweredReturn (loweredParameter 1 int64Representation)))
        ]
        (LoweredBlockId "entry"),
      LoweredFunction
        (LoweredFunctionId "App::Main::choose")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        callableRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ expectedEmptyEnvironmentInstruction 1 identityLayoutId,
              LoweredInstruction
                (LoweredTemporaryId "t2")
                callableRepresentation
                ( LoweredConstructClosure
                    (LoweredFunctionId "App::Main::identity")
                    (loweredTemporary 1 (LoweredManagedReferenceRepresentation identityLayoutId))
                )
            ]
            (Just (LoweredReturn (loweredTemporary 2 callableRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    int64Representation
    [ expectedDirectCallInstruction
        1
        callableRepresentation
        "choose"
        [loweredImmediate (LoweredBoolImmediate False)],
      expectedClosureCallInstruction
        2
        int64Representation
        (loweredTemporary 1 callableRepresentation)
        [loweredInt64 2]
    ]
    (loweredTemporary 2 int64Representation)
  where
    identityLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$1$n8:identity"
    callableRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [int64Representation] int64Representation)

inlineCurriedLambdaLoweredProgram :: LoweredProgram
inlineCurriedLambdaLoweredProgram =
  expectedClosureCallableLoweredProgram
    [ LoweredLayout outerLayoutId (LoweredClosureEnvironmentLayout []),
      LoweredLayout innerLayoutId (LoweredClosureEnvironmentLayout [int64Representation])
    ]
    [ LoweredFunction
        outerFunctionId
        (Just (layoutEnvironmentParameter outerLayoutId))
        [LoweredParameter (LoweredParameterId "arg1") int64Representation]
        innerClosureRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                (LoweredManagedReferenceRepresentation innerLayoutId)
                (LoweredConstructProduct innerLayoutId [loweredParameter 1 int64Representation]),
              LoweredInstruction
                (LoweredTemporaryId "t2")
                innerClosureRepresentation
                ( LoweredConstructClosure
                    innerFunctionId
                    (loweredTemporary 1 (LoweredManagedReferenceRepresentation innerLayoutId))
                )
            ]
            (Just (LoweredReturn (loweredTemporary 2 innerClosureRepresentation)))
        ]
        (LoweredBlockId "entry"),
      LoweredFunction
        innerFunctionId
        (Just (layoutEnvironmentParameter innerLayoutId))
        [LoweredParameter (LoweredParameterId "arg1") int64Representation]
        int64Representation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                int64Representation
                (LoweredProjectField innerLayoutId 0 (layoutEnvironmentOperand innerLayoutId)),
              expectedPrimitiveInstruction
                2
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredTemporary 1 int64Representation, loweredParameter 1 int64Representation]
            ]
            (Just (LoweredReturn (loweredTemporary 2 int64Representation)))
        ]
        (LoweredBlockId "entry")
    ]
    int64Representation
    [ expectedEmptyEnvironmentInstruction 1 outerLayoutId,
      LoweredInstruction
        (LoweredTemporaryId "t2")
        outerClosureRepresentation
        ( LoweredConstructClosure
            outerFunctionId
            (loweredTemporary 1 (LoweredManagedReferenceRepresentation outerLayoutId))
        ),
      expectedClosureCallInstruction
        3
        innerClosureRepresentation
        (loweredTemporary 2 outerClosureRepresentation)
        [loweredInt64 20],
      expectedClosureCallInstruction
        4
        int64Representation
        (loweredTemporary 3 innerClosureRepresentation)
        [loweredInt64 22]
    ]
    (loweredTemporary 4 int64Representation)
  where
    outerLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p3$0,0,0$n4:left"
    innerLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p4$0,0,0,0$n5:right"
    outerFunctionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p3$0,0,0$n4:left"
    innerFunctionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p4$0,0,0,0$n5:right"
    innerClosureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [int64Representation] int64Representation)
    outerClosureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [int64Representation] innerClosureRepresentation)

layoutEnvironmentParameter :: LoweredLayoutId -> LoweredParameter
layoutEnvironmentParameter layoutId =
  LoweredParameter
    (LoweredParameterId "environment")
    (LoweredManagedReferenceRepresentation layoutId)

layoutEnvironmentOperand :: LoweredLayoutId -> LoweredOperand
layoutEnvironmentOperand layoutId =
  LoweredFunctionParameterOperand
    (LoweredParameterId "environment")
    (LoweredManagedReferenceRepresentation layoutId)

capturingProducerProgram :: TypedProgram
capturingProducerProgram =
  expectedRootProgram
    [ TypedSignatureStatement seedSignatureBinder seedName (TypedSpan 2 1) (scalarScheme seedSignatureBinder intInfo),
      TypedLetStatement seedBinder seedName (TypedSpan 3 1) (scalarScheme seedBinder intInfo) (intExpr 1),
      TypedSignatureStatement addSeedSignatureBinder addSeedName (TypedSpan 4 1) (callableScheme addSeedSignatureBinder),
      TypedLetStatement
        addSeedBinder
        addSeedName
        (TypedSpan 5 1)
        (callableScheme addSeedBinder)
        ( TypedLambdaExpr
            addSeedInfo
            itemBinder
            itemName
            ( binaryExpr
                intInfo
                "+"
                (boundVariableExpr itemName intInfo itemBinder)
                (boundVariableExpr seedName intInfo seedBinder)
            )
        ),
      TypedExpressionStatement
        (TypedSpan 6 1)
        ( TypedApplyExpr
            intInfo
            (boundVariableExpr addSeedName addSeedInfo addSeedBinder)
            (intExpr 41)
        )
    ]
    intInfo
  where
    seedName = resolvedName "seed"
    seedSignatureBinder = TypedBinderId (modulePath, [0], seedName)
    seedBinder = TypedBinderId (modulePath, [1], seedName)
    addSeedName = resolvedName "addSeed"
    addSeedSignatureBinder = TypedBinderId (modulePath, [2], addSeedName)
    addSeedBinder = TypedBinderId (modulePath, [3], addSeedName)
    itemName = resolvedName "item"
    itemBinder = TypedBinderId (modulePath, [3, 0], itemName)
    addSeedInfo = functionInfo [("item", intInfo)] intInfo
    callableScheme owner =
      TypedScheme owner [] [] [] (typedExpressionType addSeedInfo) (typedExpressionRecipe addSeedInfo) (Just TypedClosureCallableShape)

anonymousLambdaResultProgram :: TypedProgram
anonymousLambdaResultProgram =
  expectedRootProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedLambdaExpr
            lambdaInfo
            flagBinder
            flagName
            ( binaryExpr
                boolInfo
                "=="
                (boundVariableExpr flagName boolInfo flagBinder)
                (boolExpr True)
            )
        )
    ]
    lambdaInfo
  where
    flagName = resolvedName "flag"
    flagBinder = TypedBinderId (modulePath, [0], flagName)
    lambdaInfo = stagedFunctionInfo [("flag", boolInfo)] boolInfo

lexicalCaptureExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
lexicalCaptureExpectedLoweredPrograms =
  [ ("capturing-function", capturingProducerProgram, capturingExpectedLoweredProgram),
    ("anonymous-lambda-result", anonymousLambdaResultProgram, anonymousLambdaExpectedLoweredProgram)
  ]

capturingExpectedLoweredProgram :: LoweredProgram
capturingExpectedLoweredProgram =
  expectedClosureCallableLoweredProgram
    [LoweredLayout layoutId (LoweredClosureEnvironmentLayout [int64Representation])]
    [ LoweredFunction
        (LoweredFunctionId "App::Main::addSeed")
        (Just environmentParameter)
        [LoweredParameter (LoweredParameterId "arg1") int64Representation]
        int64Representation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                int64Representation
                (LoweredProjectField layoutId 0 environmentOperand),
              expectedPrimitiveInstruction
                2
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredTemporary 1 int64Representation]
            ]
            (Just (LoweredReturn (loweredTemporary 2 int64Representation)))
        ]
        (LoweredBlockId "entry")
    ]
    int64Representation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        (LoweredManagedReferenceRepresentation layoutId)
        (LoweredConstructProduct layoutId [loweredInt64 1]),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        closureRepresentation
        (LoweredConstructClosure (LoweredFunctionId "App::Main::addSeed") (loweredTemporary 1 (LoweredManagedReferenceRepresentation layoutId))),
      expectedClosureCallInstruction 3 int64Representation (loweredTemporary 2 closureRepresentation) [loweredInt64 41]
    ]
    (loweredTemporary 3 int64Representation)
  where
    layoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n7:addSeed"
    environmentParameter = LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)
    environmentOperand = LoweredFunctionParameterOperand (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)
    closureRepresentation =
      LoweredClosureRepresentation (LoweredCallSignature [int64Representation] int64Representation)

anonymousLambdaExpectedLoweredProgram :: LoweredProgram
anonymousLambdaExpectedLoweredProgram =
  expectedClosureCallableLoweredProgram
    [LoweredLayout layoutId (LoweredClosureEnvironmentLayout [])]
    [ LoweredFunction
        functionId
        (Just (LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)))
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        LoweredBoolRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ expectedPrimitiveInstruction
                1
                LoweredBoolRepresentation
                (LoweredComparisonPrimitive LoweredEqual)
                [loweredParameter 1 LoweredBoolRepresentation, loweredImmediate (LoweredBoolImmediate True)]
            ]
            (Just (LoweredReturn (loweredTemporary 1 LoweredBoolRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    closureRepresentation
    [ expectedEmptyEnvironmentInstruction 1 layoutId,
      LoweredInstruction
        (LoweredTemporaryId "t2")
        closureRepresentation
        (LoweredConstructClosure functionId (loweredTemporary 1 (LoweredManagedReferenceRepresentation layoutId)))
    ]
    (loweredTemporary 2 closureRepresentation)
  where
    functionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p1$0$n4:flag"
    layoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$0$n4:flag"
    closureRepresentation =
      LoweredClosureRepresentation (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)

scalarBindingLiteralProgram :: TypedProgram
scalarBindingLiteralProgram =
  expectedRootProgram
    [ TypedLetStatement seedBinder seedName (TypedSpan 2 1) seedScheme (TypedLiteralExpr inferredIntInfo (TypedIntegerLiteral "40")),
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( binaryExpr
            inferredIntInfo
            "+"
            (boundVariableExpr seedName inferredIntInfo seedBinder)
            (TypedLiteralExpr inferredIntInfo (TypedIntegerLiteral "2"))
        )
    ]
    inferredIntInfo
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = scalarScheme seedBinder inferredIntInfo

scalarBindingOrderedReuseProgram :: TypedProgram
scalarBindingOrderedReuseProgram =
  expectedRootProgram
    [ TypedSignatureStatement signatureBinder seedName (TypedSpan 2 1) signatureScheme,
      TypedLetStatement seedBinder seedName (TypedSpan 3 1) seedScheme (intExpr 40),
      TypedLetStatement
        answerBinder
        answerName
        (TypedSpan 4 1)
        answerScheme
        (binaryExpr intInfo "+" (boundVariableExpr seedName intInfo seedBinder) (intExpr 2)),
      TypedExpressionStatement (TypedSpan 5 1) (boundVariableExpr answerName intInfo answerBinder)
    ]
    intInfo
  where
    seedName = resolvedName "seed"
    signatureBinder = TypedBinderId (modulePath, [0], seedName)
    signatureScheme = scalarScheme signatureBinder intInfo
    seedBinder = TypedBinderId (modulePath, [1], seedName)
    seedScheme = scalarScheme seedBinder intInfo
    answerName = resolvedName "answer"
    answerBinder = TypedBinderId (modulePath, [2], answerName)
    answerScheme = scalarScheme answerBinder intInfo

scalarBindingDirectCallResultProgram :: TypedProgram
scalarBindingDirectCallResultProgram =
  expectedRootProgram
    ( functionStatements
        <> [ TypedLetStatement answerBinder answerName (TypedSpan 4 1) answerScheme boundCall,
             TypedExpressionStatement (TypedSpan 5 1) (boundVariableExpr answerName boolInfo answerBinder)
           ]
    )
    boolInfo
  where
    function = ExpectedFunction "identity" [("item", boolInfo)] boolInfo TypedDirectCallableShape (variableExpr "item" boolInfo)
    functionName = resolvedName "identity"
    functionBinder = TypedBinderId (modulePath, [1], functionName)
    functionStatements =
      map
        (bindExpectedStatementVariables (Map.singleton functionName functionBinder))
        (expectedFunctionStatementsAtLineOffset 1 0 1 function)
    answerName = resolvedName "answer"
    answerBinder = TypedBinderId (modulePath, [2], answerName)
    answerScheme = scalarScheme answerBinder boolInfo
    boundCall =
      bindExpectedExpressionVariables
        (Map.singleton functionName functionBinder)
        (directCall "identity" [boolInfo] boolInfo [boolExpr True])

directCallExpectedPrograms :: [(Text, TypedProgram)]
directCallExpectedPrograms =
  [ ( "explicit-numeric-widths",
      expectedFunctionProgram (map expectedFunctionName explicitNumericFunctions) explicitNumericFunctions (TypedTupleExpr unitInfo [])
    ),
    ( "scalar-parameter-return",
      expectedFunctionProgram
        ["identity"]
        [identityFunction]
        (directCall "identity" [intInfo] intInfo [intExpr 42])
    ),
    ( "single-argument-direct-call",
      expectedFunctionProgram
        ["increment"]
        [incrementFunction]
        (directCall "increment" [intInfo] intInfo [intExpr 41])
    ),
    ( "curried-multi-argument-direct-call",
      expectedFunctionProgram
        ["combine"]
        [combineFunction]
        (directCall "combine" [intInfo, intInfo] intInfo [intExpr 20, intExpr 22])
    ),
    ( "three-argument-direct-call",
      expectedFunctionProgram
        ["sumThree"]
        [sumThreeFunction]
        (directCall "sumThree" [intInfo, intInfo, intInfo] intInfo [intExpr 10, intExpr 20, intExpr 12])
    ),
    ( "forward-direct-call-dag",
      expectedFunctionProgram
        ["first", "second"]
        [firstFunction, incrementNamed "second"]
        (directCall "first" [intInfo] intInfo [intExpr 41])
    ),
    ( "nested-direct-calls",
      expectedFunctionProgram
        ["increment", "double"]
        [incrementFunction, doubleFunction]
        ( directCall
            "double"
            [intInfo]
            intInfo
            [directCall "increment" [intInfo] intInfo [intExpr 20]]
        )
    ),
    ( "dollar-direct-call",
      expectedFunctionProgram
        ["increment"]
        [incrementFunction]
        (directCall "increment" [intInfo] intInfo [intExpr 41])
    ),
    ( "exported-direct-function",
      expectedFunctionProgramWithLineOffset
        1
        ["increment"]
        [incrementFunction]
        (directCall "increment" [intInfo] intInfo [intExpr 41])
    )
  ]

directRecursionExpectedPrograms :: [(Text, TypedProgram)]
directRecursionExpectedPrograms =
  [ ("self-recursive-function", selfRecursiveExpectedProgram),
    ("mutually-recursive-functions", mutuallyRecursiveExpectedProgram)
  ]

closureRecursionExpectedPrograms :: [(Text, TypedProgram)]
closureRecursionExpectedPrograms =
  [ ("closure-value-mutual-recursion", closureValueMutualRecursiveExpectedProgram),
    ("closure-value-self-recursion", closureValueSelfRecursiveExpectedProgram),
    ("capturing-self-recursion", capturingSelfRecursiveExpectedProgram),
    ("capturing-mutual-recursion", capturingMutualRecursiveExpectedProgram)
  ]

closureValueMutualRecursiveExpectedProgram :: TypedProgram
closureValueMutualRecursiveExpectedProgram =
  expectedFunctionProgramWithLineOffsetAndRecursiveGroups
    1
    [["left", "right"]]
    []
    [applyFunction, closurePassingLeftFunction, closurePassingRightFunction]
    (directCall "left" [boolInfo] boolInfo [boolExpr False])

closureValueSelfRecursiveExpectedProgram :: TypedProgram
closureValueSelfRecursiveExpectedProgram =
  expectedFunctionProgramWithLineOffsetAndRecursiveGroups
    1
    [["loop"]]
    []
    [applyFunction, closurePassingLoopFunction]
    (directCall "loop" [boolInfo] boolInfo [boolExpr False])

capturingSelfRecursiveExpectedProgram :: TypedProgram
capturingSelfRecursiveExpectedProgram =
  expectedCapturedRecursiveProgram
    [["loop"]]
    [capturingLoopFunction]
    (directCall "loop" [intInfo] intInfo [intExpr 1])

capturingMutualRecursiveExpectedProgram :: TypedProgram
capturingMutualRecursiveExpectedProgram =
  expectedCapturedRecursiveProgram
    [["left", "right"]]
    [capturingLeftFunction, capturingRightFunction]
    (directCall "left" [intInfo] intInfo [intExpr 1])

expectedCapturedRecursiveProgram :: [[Text]] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedCapturedRecursiveProgram groupNames functions terminalExpression =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        recursiveGroups
        statements
        (typedExpressionInfo boundTerminalExpression)
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    functionOwners =
      Map.fromList
        [ ( resolvedName (expectedFunctionName function),
            TypedBinderId (modulePath, [functionOffset * 2 + 2], resolvedName (expectedFunctionName function))
          )
        | (functionOffset, function) <- zip [0 ..] functions
        ]
    bindings = Map.insert seedName seedBinder functionOwners
    functionStatements =
      concat
        [ map
            (bindExpectedStatementVariables bindings)
            (expectedFunctionStatementsAtLineOffset 1 signatureIndex bindingIndex function)
        | (functionOffset, function) <- zip [0 ..] functions,
          let signatureIndex = functionOffset * 2 + 1,
          let bindingIndex = signatureIndex + 1
        ]
    recursiveGroups =
      [ TypedRecursiveGroup [functionOwners Map.! resolvedName name | name <- names]
      | names <- groupNames
      ]
    boundTerminalExpression = bindExpectedExpressionVariables bindings terminalExpression
    statements =
      TypedLetStatement seedBinder seedName (TypedSpan 2 1) seedScheme (intExpr 1)
        : functionStatements
          <> [TypedExpressionStatement (TypedSpan (length functionStatements + 3) 1) boundTerminalExpression]

closureRecursionExpectedLoweredPrograms ::
  [(Text, TypedProgram, LoweredProgram)]
closureRecursionExpectedLoweredPrograms =
  [ ( "closure-value-mutual-recursion",
      closureValueMutualRecursiveExpectedProgram,
      expectedClosureCallableLoweredProgram
        [recursiveGroupLayout]
        [ expectedBoolApplyFunction,
          expectedRecursivePassingFunction "left" "right" recursiveGroupLayoutId,
          expectedRecursivePassingFunction "right" "left" recursiveGroupLayoutId
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 recursiveGroupLayoutId,
          expectedClosureWithEnvironmentInstruction 2 "left" (loweredTemporary 1 recursiveGroupEnvironmentRepresentation),
          expectedClosureWithEnvironmentInstruction 3 "right" (loweredTemporary 1 recursiveGroupEnvironmentRepresentation),
          expectedClosureCallInstruction 4 LoweredBoolRepresentation (loweredTemporary 2 boolClosureRepresentation) [loweredImmediate (LoweredBoolImmediate False)]
        ]
        (loweredTemporary 4 LoweredBoolRepresentation)
    ),
    ( "closure-value-self-recursion",
      closureValueSelfRecursiveExpectedProgram,
      expectedClosureCallableLoweredProgram
        [recursiveGroupLayout]
        [ expectedBoolApplyFunction,
          expectedRecursivePassingFunction "loop" "loop" recursiveGroupLayoutId
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 recursiveGroupLayoutId,
          expectedClosureWithEnvironmentInstruction 2 "loop" (loweredTemporary 1 recursiveGroupEnvironmentRepresentation),
          expectedClosureCallInstruction 3 LoweredBoolRepresentation (loweredTemporary 2 boolClosureRepresentation) [loweredImmediate (LoweredBoolImmediate False)]
        ]
        (loweredTemporary 3 LoweredBoolRepresentation)
    ),
    ( "capturing-self-recursion",
      capturingSelfRecursiveExpectedProgram,
      expectedCapturedRecursiveLoweredProgram
        [expectedCapturedRecursiveFunction "loop" "loop" capturingRecursiveLayoutId]
        ["loop"]
    ),
    ( "capturing-mutual-recursion",
      capturingMutualRecursiveExpectedProgram,
      expectedCapturedRecursiveLoweredProgram
        [ expectedCapturedRecursiveFunction "left" "right" capturingRecursiveLayoutId,
          expectedCapturedRecursiveFunction "right" "left" capturingRecursiveLayoutId
        ]
        ["left", "right"]
    )
  ]

capturingRecursiveLayoutId :: LoweredLayoutId
capturingRecursiveLayoutId = LoweredLayoutId "$jz1$recursive-env$m2$3:App$4:Main$p1$2$n5:group"

capturingRecursiveLayout :: LoweredLayout
capturingRecursiveLayout = LoweredLayout capturingRecursiveLayoutId (LoweredClosureEnvironmentLayout [int64Representation])

expectedCapturedRecursiveLoweredProgram :: [LoweredFunction] -> [Text] -> LoweredProgram
expectedCapturedRecursiveLoweredProgram recursiveFunctions functionNames =
  expectedClosureCallableLoweredProgram
    [capturingRecursiveLayout]
    recursiveFunctions
    int64Representation
    ( [ expectedEnvironmentInstruction 1 capturingRecursiveLayoutId [loweredInt64 1]
      ]
        <> zipWith
          ( \index functionName ->
              expectedClosureWithEnvironmentInstructionFor
                index
                functionName
                intClosureRepresentation
                (loweredTemporary 1 (LoweredManagedReferenceRepresentation capturingRecursiveLayoutId))
          )
          [2 ..]
          functionNames
        <> [ expectedClosureCallInstruction
               (length recursiveFunctions + 2)
               int64Representation
               (loweredTemporary 2 intClosureRepresentation)
               [loweredInt64 1]
           ]
    )
    (loweredTemporary (length recursiveFunctions + 2) int64Representation)

expectedCapturedRecursiveFunction :: Text -> Text -> LoweredLayoutId -> LoweredFunction
expectedCapturedRecursiveFunction functionName peerName layoutId =
  LoweredFunction
    (LoweredFunctionId ("App::Main::" <> functionName))
    (Just (LoweredParameter (LoweredParameterId "environment") environmentRepresentation))
    [LoweredParameter (LoweredParameterId "arg1") int64Representation]
    int64Representation
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction
            (LoweredTemporaryId "t1")
            int64Representation
            (LoweredProjectField layoutId 0 environmentOperand),
          expectedClosureWithEnvironmentInstructionFor 2 peerName intClosureRepresentation environmentOperand,
          expectedPrimitiveInstruction 3 int64Representation (LoweredArithmeticPrimitive LoweredAdd) [loweredParameter 1 int64Representation, loweredTemporary 1 int64Representation]
        ]
        (Just (LoweredClosureTailCall (loweredTemporary 2 intClosureRepresentation) [loweredTemporary 3 int64Representation]))
    ]
    (LoweredBlockId "entry")
  where
    environmentRepresentation = LoweredManagedReferenceRepresentation layoutId
    environmentOperand = LoweredFunctionParameterOperand (LoweredParameterId "environment") environmentRepresentation

directRecursionExpectedLoweredPrograms ::
  [(Text, TypedProgram, LoweredProgram)]
directRecursionExpectedLoweredPrograms =
  [ ( "self-recursive-function",
      selfRecursiveExpectedProgram,
      expectedCallableLoweredProgram
        [ expectedTailLocalFunction
            "loop"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            []
            (loweredDirectTailCall "loop" [loweredParameter 1 int64Representation])
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "loop" [loweredInt64 1]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "mutually-recursive-functions",
      mutuallyRecursiveExpectedProgram,
      expectedCallableLoweredProgram
        [ expectedTailLocalFunction
            "left"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            []
            (loweredDirectTailCall "right" [loweredParameter 1 int64Representation]),
          expectedTailLocalFunction
            "right"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            []
            (loweredDirectTailCall "left" [loweredParameter 1 int64Representation])
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "left" [loweredInt64 1]]
        (loweredTemporary 1 int64Representation)
    )
  ]

selfRecursiveExpectedProgram :: TypedProgram
selfRecursiveExpectedProgram =
  expectedFunctionProgramWithLineOffsetAndRecursiveGroups
    1
    [["loop"]]
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (directCall "loop" [intInfo] intInfo [intExpr 1])

mutuallyRecursiveExpectedProgram :: TypedProgram
mutuallyRecursiveExpectedProgram =
  expectedFunctionProgramWithLineOffsetAndRecursiveGroups
    1
    [["left", "right"]]
    []
    [ ExpectedFunction
        "left"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (directCall "right" [intInfo] intInfo [variableExpr "item" intInfo]),
      ExpectedFunction
        "right"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (directCall "left" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (directCall "left" [intInfo] intInfo [intExpr 1])

closedCallableExpectedPrograms :: [(Text, TypedProgram)]
closedCallableExpectedPrograms =
  [ ( "named-function-value",
      expectedFunctionProgramWithLineOffset
        1
        []
        [boolIdentityFunction]
        (variableExpr "identity" boolCallableInfo)
    ),
    ( "higher-order-call",
      expectedFunctionProgramWithLineOffset
        1
        []
        [applyFunction, boolIdentityFunction]
        (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
    ),
    ( "closure-result",
      expectedFunctionProgramWithLineOffset
        1
        []
        [boolIdentityFunction, chooseFunction]
        (directCall "choose" [boolInfo] boolCallableInfo [boolExpr False])
    ),
    ( "callable-parameter-shadows-named-function",
      expectedFunctionProgramWithLineOffset
        1
        []
        [boolCombineFunction, applyCombineParameterFunction, boolIdentityFunction]
        (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
    ),
    ( "callable-parameter-shadows-enclosing-function",
      expectedFunctionProgramWithLineOffset
        1
        []
        [selfShadowingApplyFunction, boolIdentityFunction]
        (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
    ),
    ( "mixed-direct-and-value-use",
      expectedFunctionProgramWithLineOffset
        1
        []
        [applyFunction, boolIdentityFunction]
        ( binaryExpr
            boolInfo
            "=="
            (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
            (directCall "identity" [boolInfo] boolInfo [boolExpr True])
        )
    ),
    ( "callable-parameter-value-shadows-enclosing-function",
      expectedFunctionProgramWithLineOffset
        1
        []
        [applyFunction, shadowingForwardFunction, boolIdentityFunction]
        (directCall "forward" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
    )
  ]

directCallExpectedLoweredPrograms :: [(Text, LoweredProgram)]
directCallExpectedLoweredPrograms =
  [ ( "explicit-numeric-widths",
      expectedCallableLoweredProgram
        [ expectedLiteralFunction "asInt8" (LoweredSignedIntegerRepresentation LoweredIntegerWidth8) (LoweredSignedIntegerImmediate LoweredIntegerWidth8 1),
          expectedLiteralFunction "asInt16" (LoweredSignedIntegerRepresentation LoweredIntegerWidth16) (LoweredSignedIntegerImmediate LoweredIntegerWidth16 2),
          expectedLiteralFunction "asInt32" (LoweredSignedIntegerRepresentation LoweredIntegerWidth32) (LoweredSignedIntegerImmediate LoweredIntegerWidth32 3),
          expectedLiteralFunction "asInt64" int64Representation (LoweredSignedIntegerImmediate LoweredIntegerWidth64 4),
          expectedLiteralFunction "asUInt8" (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth8) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 5),
          expectedLiteralFunction "asUInt16" (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth16) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth16 6),
          expectedLiteralFunction "asUInt32" (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth32) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth32 7),
          expectedLiteralFunction "asUInt64" (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 8),
          expectedLiteralFunction "asFloat16" (LoweredFloatRepresentation LoweredFloatWidth16) (LoweredFloatImmediate LoweredFloatWidth16 "1.5"),
          expectedLiteralFunction "asFloat32" (LoweredFloatRepresentation LoweredFloatWidth32) (LoweredFloatImmediate LoweredFloatWidth32 "2.5"),
          expectedLiteralFunction "asFloat64" float64Representation (LoweredFloatImmediate LoweredFloatWidth64 "3.5")
        ]
        LoweredUnitRepresentation
        []
        (loweredImmediate LoweredUnitImmediate)
    ),
    ( "scalar-parameter-return",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "identity"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            []
            (loweredParameter 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "identity" [loweredInt64 42]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "single-argument-direct-call",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "increment"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredInt64 1]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "increment" [loweredInt64 41]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "curried-multi-argument-direct-call",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "combine"
            [ LoweredParameter (LoweredParameterId "arg1") int64Representation,
              LoweredParameter (LoweredParameterId "arg2") int64Representation
            ]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredParameter 2 int64Representation]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "combine" [loweredInt64 20, loweredInt64 22]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "three-argument-direct-call",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "sumThree"
            [ LoweredParameter (LoweredParameterId "arg1") int64Representation,
              LoweredParameter (LoweredParameterId "arg2") int64Representation,
              LoweredParameter (LoweredParameterId "arg3") int64Representation
            ]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredParameter 2 int64Representation],
              expectedPrimitiveInstruction
                2
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredTemporary 1 int64Representation, loweredParameter 3 int64Representation]
            ]
            (loweredTemporary 2 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "sumThree" [loweredInt64 10, loweredInt64 20, loweredInt64 12]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "forward-direct-call-dag",
      expectedCallableLoweredProgram
        [ expectedTailLocalFunction
            "first"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            []
            (loweredDirectTailCall "second" [loweredParameter 1 int64Representation]),
          expectedLocalFunction
            "second"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredInt64 1]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "first" [loweredInt64 41]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "nested-direct-calls",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "increment"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredInt64 1]
            ]
            (loweredTemporary 1 int64Representation),
          expectedLocalFunction
            "double"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredParameter 1 int64Representation]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [ expectedDirectCallInstruction 1 int64Representation "increment" [loweredInt64 20],
          expectedDirectCallInstruction 2 int64Representation "double" [loweredTemporary 1 int64Representation]
        ]
        (loweredTemporary 2 int64Representation)
    ),
    ( "dollar-direct-call",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "increment"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredInt64 1]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "increment" [loweredInt64 41]]
        (loweredTemporary 1 int64Representation)
    ),
    ( "exported-direct-function",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "increment"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [ expectedPrimitiveInstruction
                1
                int64Representation
                (LoweredArithmeticPrimitive LoweredAdd)
                [loweredParameter 1 int64Representation, loweredInt64 1]
            ]
            (loweredTemporary 1 int64Representation)
        ]
        int64Representation
        [expectedDirectCallInstruction 1 int64Representation "increment" [loweredInt64 41]]
        (loweredTemporary 1 int64Representation)
    )
  ]

closedCallableExpectedLoweredPrograms :: [(Text, LoweredProgram)]
closedCallableExpectedLoweredPrograms =
  [ ( "named-function-value",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt1]
        [expectedBoolIdentityClosure identityLayoutIdAt1]
        boolClosureRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt1,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt1
        ]
        (loweredTemporary 2 boolClosureRepresentation)
    ),
    ( "higher-order-call",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt3]
        [ expectedBoolApplyFunction,
          expectedBoolIdentityClosure identityLayoutIdAt3
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt3,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt3,
          expectedDirectCallInstruction 3 LoweredBoolRepresentation "apply" [loweredTemporary 2 boolClosureRepresentation]
        ]
        (loweredTemporary 3 LoweredBoolRepresentation)
    ),
    ( "closure-result",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt1]
        [ expectedBoolIdentityClosure identityLayoutIdAt1,
          expectedLocalFunction
            "choose"
            [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
            boolClosureRepresentation
            [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt1,
              expectedClosureInstruction 2 "identity" identityLayoutIdAt1
            ]
            (loweredTemporary 2 boolClosureRepresentation)
        ]
        boolClosureRepresentation
        [expectedDirectCallInstruction 1 boolClosureRepresentation "choose" [loweredImmediate (LoweredBoolImmediate False)]]
        (loweredTemporary 1 boolClosureRepresentation)
    ),
    ( "callable-parameter-shadows-named-function",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt5]
        [ expectedBoolCombineFunction,
          expectedBoolApplyFunction,
          expectedBoolIdentityClosure identityLayoutIdAt5
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt5,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt5,
          expectedDirectCallInstruction 3 LoweredBoolRepresentation "apply" [loweredTemporary 2 boolClosureRepresentation]
        ]
        (loweredTemporary 3 LoweredBoolRepresentation)
    ),
    ( "callable-parameter-shadows-enclosing-function",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt3]
        [ expectedBoolApplyFunction,
          expectedBoolIdentityClosure identityLayoutIdAt3
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt3,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt3,
          expectedDirectCallInstruction 3 LoweredBoolRepresentation "apply" [loweredTemporary 2 boolClosureRepresentation]
        ]
        (loweredTemporary 3 LoweredBoolRepresentation)
    ),
    ( "mixed-direct-and-value-use",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt3]
        [ expectedBoolApplyFunction,
          expectedBoolIdentityClosure identityLayoutIdAt3
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt3,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt3,
          expectedDirectCallInstruction 3 LoweredBoolRepresentation "apply" [loweredTemporary 2 boolClosureRepresentation],
          expectedEmptyEnvironmentInstruction 4 identityLayoutIdAt3,
          expectedClosureInstruction 5 "identity" identityLayoutIdAt3,
          expectedClosureCallInstruction 6 LoweredBoolRepresentation (loweredTemporary 5 boolClosureRepresentation) [loweredImmediate (LoweredBoolImmediate True)],
          expectedPrimitiveInstruction
            7
            LoweredBoolRepresentation
            (LoweredComparisonPrimitive LoweredEqual)
            [loweredTemporary 3 LoweredBoolRepresentation, loweredTemporary 6 LoweredBoolRepresentation]
        ]
        (loweredTemporary 7 LoweredBoolRepresentation)
    ),
    ( "callable-parameter-value-shadows-enclosing-function",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt5]
        [ expectedBoolApplyFunction,
          expectedBoolForwardFunction,
          expectedBoolIdentityClosure identityLayoutIdAt5
        ]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt5,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt5,
          expectedDirectCallInstruction 3 LoweredBoolRepresentation "forward" [loweredTemporary 2 boolClosureRepresentation]
        ]
        (loweredTemporary 3 LoweredBoolRepresentation)
    )
  ]

independentClosureExpectedLoweredPrograms :: [(Text, LoweredProgram)]
independentClosureExpectedLoweredPrograms =
  [ ( "closure-valued-parameter",
      expectedClosureCallableLoweredProgram
        []
        [expectedBoolApplyFunction]
        LoweredBoolRepresentation
        []
        (loweredImmediate (LoweredBoolImmediate True))
    ),
    ( "closure-valued-result",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt1]
        [ expectedBoolIdentityClosure identityLayoutIdAt1,
          expectedLocalFunction
            "choose"
            [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
            boolClosureRepresentation
            [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt1,
              expectedClosureInstruction 2 "identity" identityLayoutIdAt1
            ]
            (loweredTemporary 2 boolClosureRepresentation)
        ]
        LoweredBoolRepresentation
        []
        (loweredImmediate (LoweredBoolImmediate True))
    ),
    ( "closure-shaped-named-function",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt1]
        [expectedBoolIdentityClosure identityLayoutIdAt1]
        boolClosureRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt1,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt1
        ]
        (loweredTemporary 2 boolClosureRepresentation)
    ),
    ( "closure-shaped-named-application",
      expectedClosureCallableLoweredProgram
        [identityLayoutAt1]
        [expectedBoolIdentityClosure identityLayoutIdAt1]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 identityLayoutIdAt1,
          expectedClosureInstruction 2 "identity" identityLayoutIdAt1,
          expectedPrimitiveInstruction
            3
            LoweredBoolRepresentation
            (LoweredComparisonPrimitive LoweredEqual)
            [loweredImmediate (LoweredBoolImmediate True), loweredImmediate (LoweredBoolImmediate False)],
          expectedClosureCallInstruction
            4
            LoweredBoolRepresentation
            (loweredTemporary 2 boolClosureRepresentation)
            [loweredTemporary 3 LoweredBoolRepresentation]
        ]
        (loweredTemporary 4 LoweredBoolRepresentation)
    ),
    ( "callable-parameter-shadows-top-level-lowerer",
      expectedClosureCallableLoweredProgram
        []
        [expectedBoolCombineFunction, expectedBoolApplyFunction]
        LoweredBoolRepresentation
        []
        (loweredImmediate (LoweredBoolImmediate True))
    ),
    ( "callable-parameter-value-shadows-enclosing-function-lowerer",
      expectedClosureCallableLoweredProgram
        []
        [expectedBoolApplyFunction, expectedBoolForwardFunction]
        LoweredBoolRepresentation
        []
        (loweredImmediate (LoweredBoolImmediate True))
    ),
    ( "closure-shaped-self-recursive-function",
      expectedClosureCallableLoweredProgram
        [recursiveGroupLayoutAt1]
        [expectedRecursiveCallingFunction "loop" "loop" recursiveGroupLayoutIdAt1]
        LoweredBoolRepresentation
        [ expectedEmptyEnvironmentInstruction 1 recursiveGroupLayoutIdAt1,
          expectedClosureWithEnvironmentInstructionFor 2 "loop" intClosureRepresentation (loweredTemporary 1 recursiveGroupEnvironmentRepresentationAt1)
        ]
        (loweredImmediate (LoweredBoolImmediate True))
    )
  ]

recursiveGroupLayoutId, recursiveGroupLayoutIdAt1 :: LoweredLayoutId
recursiveGroupLayoutId = LoweredLayoutId "$jz1$recursive-env$m2$3:App$4:Main$p1$3$n5:group"
recursiveGroupLayoutIdAt1 = LoweredLayoutId "$jz1$recursive-env$m2$3:App$4:Main$p1$1$n5:group"

recursiveGroupLayout, recursiveGroupLayoutAt1 :: LoweredLayout
recursiveGroupLayout = LoweredLayout recursiveGroupLayoutId (LoweredClosureEnvironmentLayout [])
recursiveGroupLayoutAt1 = LoweredLayout recursiveGroupLayoutIdAt1 (LoweredClosureEnvironmentLayout [])

recursiveGroupEnvironmentRepresentation, recursiveGroupEnvironmentRepresentationAt1 :: LoweredRepresentation
recursiveGroupEnvironmentRepresentation = LoweredManagedReferenceRepresentation recursiveGroupLayoutId
recursiveGroupEnvironmentRepresentationAt1 = LoweredManagedReferenceRepresentation recursiveGroupLayoutIdAt1

expectedRecursivePassingFunction :: Text -> Text -> LoweredLayoutId -> LoweredFunction
expectedRecursivePassingFunction functionName peerName layoutId =
  LoweredFunction
    (LoweredFunctionId ("App::Main::" <> functionName))
    (Just (LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)))
    [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
    LoweredBoolRepresentation
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [expectedClosureWithEnvironmentInstruction 1 peerName environmentOperand]
        (Just (loweredDirectTailCall "apply" [loweredTemporary 1 boolClosureRepresentation]))
    ]
    (LoweredBlockId "entry")
  where
    environmentOperand =
      LoweredFunctionParameterOperand
        (LoweredParameterId "environment")
        (LoweredManagedReferenceRepresentation layoutId)

expectedRecursiveCallingFunction :: Text -> Text -> LoweredLayoutId -> LoweredFunction
expectedRecursiveCallingFunction functionName peerName layoutId =
  LoweredFunction
    (LoweredFunctionId ("App::Main::" <> functionName))
    (Just (LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)))
    [LoweredParameter (LoweredParameterId "arg1") int64Representation]
    int64Representation
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [expectedClosureWithEnvironmentInstructionFor 1 peerName intClosureRepresentation environmentOperand]
        (Just (LoweredClosureTailCall (loweredTemporary 1 intClosureRepresentation) [loweredParameter 1 int64Representation]))
    ]
    (LoweredBlockId "entry")
  where
    environmentOperand =
      LoweredFunctionParameterOperand
        (LoweredParameterId "environment")
        (LoweredManagedReferenceRepresentation layoutId)

intClosureRepresentation :: LoweredRepresentation
intClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [int64Representation] int64Representation)

identityLayoutAt1, identityLayoutAt3, identityLayoutAt5 :: LoweredLayout
identityLayoutAt1 = LoweredLayout identityLayoutIdAt1 (LoweredClosureEnvironmentLayout [])
identityLayoutAt3 = LoweredLayout identityLayoutIdAt3 (LoweredClosureEnvironmentLayout [])
identityLayoutAt5 = LoweredLayout identityLayoutIdAt5 (LoweredClosureEnvironmentLayout [])

rfcClosureEnvironmentIdentityProgram :: (TypedProgram, LoweredProgram)
rfcClosureEnvironmentIdentityProgram = (typedProgram, loweredProgram)
  where
    rfcModulePath = ["Main"]
    functionName = TypedResolvedName TypedCurrentModule TypedValueNamespace "identity"
    functionBinder = TypedBinderId (rfcModulePath, [0], functionName)
    parameterName = TypedResolvedName TypedCurrentModule TypedValueNamespace "item"
    parameterBinder = TypedBinderId (rfcModulePath, [0, 0], parameterName)
    functionSchemeValue =
      TypedScheme
        functionBinder
        []
        []
        []
        (typedExpressionType boolCallableInfo)
        (typedExpressionRecipe boolCallableInfo)
        (Just TypedClosureCallableShape)
    typedProgram =
      TypedProgram
        Nothing
        [ TypedModule
            rfcModulePath
            (TypedSourcePath "src/Main.jz")
            []
            []
            (TypedModuleInterface [] [] [] [])
            []
            [ TypedLetStatement
                functionBinder
                functionName
                (TypedSpan 1 1)
                functionSchemeValue
                ( TypedLambdaExpr
                    boolCallableInfo
                    parameterBinder
                    parameterName
                    (TypedVariableExpr boolInfo parameterName (Just parameterBinder))
                ),
              TypedExpressionStatement
                (TypedSpan 2 1)
                (TypedVariableExpr boolCallableInfo functionName (Just functionBinder))
            ]
            boolCallableInfo
        ]
        rfcModulePath
    layoutId = LoweredLayoutId "$jz1$closure-env$m1$4:Main$p1$0$n8:identity"
    closureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)
    functionId = LoweredFunctionId "Main::identity"
    entryFunctionId = LoweredFunctionId "Main::$entry"
    loweredProgram =
      LoweredProgram
        (LoweredIRVersion 1)
        [LoweredLayout layoutId (LoweredClosureEnvironmentLayout [])]
        []
        [ LoweredFunction
            functionId
            (Just (LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)))
            [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
            LoweredBoolRepresentation
            [ LoweredBlock
                (LoweredBlockId "entry")
                []
                []
                (Just (LoweredReturn (LoweredFunctionParameterOperand (LoweredParameterId "arg1") LoweredBoolRepresentation)))
            ]
            (LoweredBlockId "entry"),
          LoweredFunction
            entryFunctionId
            Nothing
            []
            closureRepresentation
            [ LoweredBlock
                (LoweredBlockId "entry")
                []
                [ LoweredInstruction
                    (LoweredTemporaryId "t1")
                    (LoweredManagedReferenceRepresentation layoutId)
                    (LoweredConstructProduct layoutId []),
                  LoweredInstruction
                    (LoweredTemporaryId "t2")
                    closureRepresentation
                    ( LoweredConstructClosure
                        functionId
                        (LoweredTemporaryOperand (LoweredTemporaryId "t1") (LoweredManagedReferenceRepresentation layoutId))
                    )
                ]
                (Just (LoweredReturn (LoweredTemporaryOperand (LoweredTemporaryId "t2") closureRepresentation)))
            ]
            (LoweredBlockId "entry")
        ]
        entryFunctionId

identityLayoutIdAt1, identityLayoutIdAt3, identityLayoutIdAt5 :: LoweredLayoutId
identityLayoutIdAt1 = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$1$n8:identity"
identityLayoutIdAt3 = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n8:identity"
identityLayoutIdAt5 = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$5$n8:identity"

boolClosureRepresentation :: LoweredRepresentation
boolClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)

expectedClosureCallableLoweredProgram ::
  [LoweredLayout] ->
  [LoweredFunction] ->
  LoweredRepresentation ->
  [LoweredInstruction] ->
  LoweredOperand ->
  LoweredProgram
expectedClosureCallableLoweredProgram layouts functions resultRepresentation instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    layouts
    []
    ( functions
        <> [ LoweredFunction
               loweredEntryFunctionId
               Nothing
               []
               resultRepresentation
               [LoweredBlock (LoweredBlockId "entry") [] instructions (Just (LoweredReturn resultOperand))]
               (LoweredBlockId "entry")
           ]
    )
    loweredEntryFunctionId

expectedBoolIdentityClosure :: LoweredLayoutId -> LoweredFunction
expectedBoolIdentityClosure layoutId =
  LoweredFunction
    (LoweredFunctionId "App::Main::identity")
    (Just (LoweredParameter (LoweredParameterId "environment") (LoweredManagedReferenceRepresentation layoutId)))
    [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
    LoweredBoolRepresentation
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        []
        (Just (LoweredReturn (loweredParameter 1 LoweredBoolRepresentation)))
    ]
    (LoweredBlockId "entry")

expectedBoolApplyFunction :: LoweredFunction
expectedBoolApplyFunction =
  expectedTailLocalFunction
    "apply"
    [LoweredParameter (LoweredParameterId "arg1") boolClosureRepresentation]
    LoweredBoolRepresentation
    []
    (LoweredClosureTailCall (loweredParameter 1 boolClosureRepresentation) [loweredImmediate (LoweredBoolImmediate True)])

expectedBoolForwardFunction :: LoweredFunction
expectedBoolForwardFunction =
  expectedTailLocalFunction
    "forward"
    [LoweredParameter (LoweredParameterId "arg1") boolClosureRepresentation]
    LoweredBoolRepresentation
    []
    (loweredDirectTailCall "apply" [loweredParameter 1 boolClosureRepresentation])

expectedBoolCombineFunction :: LoweredFunction
expectedBoolCombineFunction =
  expectedLocalFunction
    "combine"
    [ LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation,
      LoweredParameter (LoweredParameterId "arg2") LoweredBoolRepresentation
    ]
    LoweredBoolRepresentation
    []
    (loweredParameter 1 LoweredBoolRepresentation)

expectedEmptyEnvironmentInstruction :: Int -> LoweredLayoutId -> LoweredInstruction
expectedEmptyEnvironmentInstruction index layoutId =
  expectedEnvironmentInstruction index layoutId []

expectedEnvironmentInstruction :: Int -> LoweredLayoutId -> [LoweredOperand] -> LoweredInstruction
expectedEnvironmentInstruction index layoutId fields =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    (LoweredManagedReferenceRepresentation layoutId)
    (LoweredConstructProduct layoutId fields)

expectedClosureInstruction :: Int -> Text -> LoweredLayoutId -> LoweredInstruction
expectedClosureInstruction index functionName layoutId =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    boolClosureRepresentation
    ( LoweredConstructClosure
        (LoweredFunctionId ("App::Main::" <> functionName))
        (loweredTemporary (index - 1) (LoweredManagedReferenceRepresentation layoutId))
    )

expectedClosureWithEnvironmentInstruction :: Int -> Text -> LoweredOperand -> LoweredInstruction
expectedClosureWithEnvironmentInstruction index functionName environmentOperand =
  expectedClosureWithEnvironmentInstructionFor index functionName boolClosureRepresentation environmentOperand

expectedClosureWithEnvironmentInstructionFor :: Int -> Text -> LoweredRepresentation -> LoweredOperand -> LoweredInstruction
expectedClosureWithEnvironmentInstructionFor index functionName closureRepresentation environmentOperand =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    closureRepresentation
    (LoweredConstructClosure (LoweredFunctionId ("App::Main::" <> functionName)) environmentOperand)

expectedCallableLoweredProgram ::
  [LoweredFunction] ->
  LoweredRepresentation ->
  [LoweredInstruction] ->
  LoweredOperand ->
  LoweredProgram
expectedCallableLoweredProgram functions resultRepresentation instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    []
    []
    ( functions
        <> [ LoweredFunction
               loweredEntryFunctionId
               Nothing
               []
               resultRepresentation
               [LoweredBlock (LoweredBlockId "entry") [] instructions (Just (LoweredReturn resultOperand))]
               (LoweredBlockId "entry")
           ]
    )
    loweredEntryFunctionId

expectedTailLocalFunction ::
  Text ->
  [LoweredParameter] ->
  LoweredRepresentation ->
  [LoweredInstruction] ->
  LoweredTerminator ->
  LoweredFunction
expectedTailLocalFunction name parameters resultRepresentation instructions terminator =
  LoweredFunction
    (LoweredFunctionId ("App::Main::" <> name))
    Nothing
    parameters
    resultRepresentation
    [LoweredBlock (LoweredBlockId "entry") [] instructions (Just terminator)]
    (LoweredBlockId "entry")

expectedLiteralFunction :: Text -> LoweredRepresentation -> LoweredImmediate -> LoweredFunction
expectedLiteralFunction name resultRepresentation immediateValue =
  expectedLocalFunction
    name
    [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
    resultRepresentation
    []
    (loweredImmediate immediateValue)

loweredDirectTailCall :: Text -> [LoweredOperand] -> LoweredTerminator
loweredDirectTailCall functionName operands =
  LoweredDirectTailCall (LoweredFunctionId ("App::Main::" <> functionName)) operands
