{-# LANGUAGE OverloadedStrings #-}

-- | Source fixtures for the managed product and local-variant profile.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedProductsVariants where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.TypedCore

managedProductVariantFixtures :: [(Text, Fixture)]
managedProductVariantFixtures =
  [ ("managed-tuple", sourceFixtureNoExports "managed-tuple" managedTupleSource),
    ("managed-option", sourceFixtureNoExports "managed-option" managedOptionSource),
    ("managed-exported-option", sourceFixture "managed-exported-option" managedExportedOptionSource),
    ("managed-tree", sourceFixtureNoExports "managed-tree" managedTreeSource),
    ("managed-pair-binding", sourceFixtureNoExports "managed-pair-binding" managedPairBindingSource),
    ("managed-pair-identity", sourceFixtureNoExports "managed-pair-identity" managedPairIdentitySource),
    ("managed-pair-direct-tail", sourceFixtureNoExports "managed-pair-direct-tail" managedPairDirectTailSource),
    ("managed-pair-recursive-capture", sourceFixtureNoExports "managed-pair-recursive-capture" managedPairRecursiveCaptureSource),
    ("managed-pair-conditional-join", sourceFixtureNoExports "managed-pair-conditional-join" managedPairConditionalJoinSource),
    ("managed-pair-scalar-case-join", sourceFixtureNoExports "managed-pair-scalar-case-join" managedPairScalarCaseJoinSource),
    ("managed-box-capture", sourceFixtureNoExports "managed-box-capture" managedBoxCaptureSource),
    ( "managed-as-constructor-tuple-pattern",
      sourceFixtureNoExports "managed-as-constructor-tuple-pattern" managedAsConstructorTuplePatternSource
    ),
    ( "managed-top-level-or-pattern",
      sourceFixtureNoExports "managed-top-level-or-pattern" managedTopLevelOrPatternSource
    ),
    ( "managed-list-pattern-boundary",
      sourceFixtureNoExports "managed-list-pattern-boundary" managedListPatternBoundarySource
    ),
    ( "managed-cons-pattern-boundary",
      sourceFixtureNoExports "managed-cons-pattern-boundary" managedConsPatternBoundarySource
    ),
    ( "managed-text-literal-pattern-boundary",
      sourceFixtureNoExports "managed-text-literal-pattern-boundary" managedTextLiteralPatternBoundarySource
    ),
    ( "managed-nested-or-pattern-boundary",
      sourceFixtureNoExports "managed-nested-or-pattern-boundary" managedNestedOrPatternBoundarySource
    ),
    ( "managed-pattern-lambda-boundary",
      sourceFixtureNoExports "managed-pattern-lambda-boundary" managedPatternLambdaBoundarySource
    ),
    ( "managed-tuple-child-failure",
      sourceFixtureNoExports "managed-tuple-child-failure" retainedTupleChildFailureSource
    ),
    ( "managed-data-sibling-failure",
      sourceFixtureNoExports "managed-data-sibling-failure" retainedDataSiblingFailureSource
    ),
    ( "managed-bare-constructor-failure",
      sourceFixtureNoExports "managed-bare-constructor-failure" bareConstructorSource
    ),
    ( "managed-partial-constructor-failure",
      sourceFixtureNoExports "managed-partial-constructor-failure" partialConstructorSource
    ),
    ( "managed-list-field-failure",
      sourceFixtureNoExports "managed-list-field-failure" listFieldSource
    ),
    ( "managed-unresolved-constructor-failure",
      sourceFixtureNoExports "managed-unresolved-constructor-failure" unresolvedConstructorSource
    ),
    ( "managed-list-construction-failure",
      sourceFixtureNoExports "managed-list-construction-failure" listConstructionSource
    ),
    ( "managed-tuple-equality-failure",
      sourceFixtureNoExports "managed-tuple-equality-failure" tupleEqualitySource
    ),
    ( "managed-variant-equality-failure",
      sourceFixtureNoExports "managed-variant-equality-failure" variantEqualitySource
    ),
    ( "managed-tuple-pattern-failure",
      sourceFixtureNoExports "managed-tuple-pattern-failure" tuplePatternSource
    ),
    ( "managed-constructor-pattern-failure",
      sourceFixtureNoExports "managed-constructor-pattern-failure" constructorPatternSource
    )
  ]

managedProductVariantFixture :: Text -> Fixture
managedProductVariantFixture name =
  case lookup name managedProductVariantFixtures of
    Just fixture -> fixture
    Nothing -> error "managed product/variant fixture is missing"

managedProductVariantExpectedPrograms :: [(Text, TypedProgram)]
managedProductVariantExpectedPrograms =
  [ ("managed-tuple", managedTupleProgram),
    ("managed-option", managedOptionProgram),
    ("managed-exported-option", managedExportedOptionProgram),
    ("managed-tree", managedTreeProgram),
    ("managed-pair-binding", managedPairBindingProgram),
    ("managed-pair-identity", managedPairIdentityProgram),
    ("managed-pair-direct-tail", managedPairDirectTailProgram),
    ("managed-pair-recursive-capture", managedPairRecursiveCaptureProgram),
    ("managed-pair-conditional-join", managedPairConditionalJoinProgram),
    ("managed-pair-scalar-case-join", managedPairScalarCaseJoinProgram),
    ("managed-box-capture", managedBoxCaptureProgram),
    ("managed-as-constructor-tuple-pattern", managedAsConstructorTuplePatternProgram),
    ("managed-top-level-or-pattern", managedTopLevelOrPatternProgram)
  ]

managedPatternProfileAcceptedPrograms :: [(Text, TypedProgram)]
managedPatternProfileAcceptedPrograms =
  [ ("managed-closed-variant-pattern-profile", managedTopLevelOrPatternProgram),
    ("managed-total-tuple-pattern-profile", managedTotalTuplePatternProgram)
  ]

managedPatternProfileRejectedPrograms :: [(Text, TypedProgram)]
managedPatternProfileRejectedPrograms =
  [ ("managed-missing-constructor-pattern-profile", managedMissingConstructorPatternProgram),
    ("managed-other-missing-constructor-pattern-profile", managedOtherMissingConstructorPatternProgram),
    ("managed-guarded-constructors-pattern-profile", managedGuardedConstructorsPatternProgram),
    ("managed-incomplete-tuple-pattern-profile", managedIncompleteTuplePatternProgram),
    ("managed-bool-literals-without-catch-all-pattern-profile", managedBoolLiteralPatternProgram),
    ("managed-nested-constructor-tuple-pattern-profile", managedAsConstructorTuplePatternProgram),
    ("managed-list-pattern-profile", managedListPatternProgram),
    ("managed-nested-or-pattern-profile", managedNestedOrPatternProgram),
    ("managed-text-literal-pattern-profile", managedTextLiteralPatternProgram)
  ]

managedProductVariantExpectedLoweredPrograms :: [(Text, LoweredProgram)]
managedProductVariantExpectedLoweredPrograms =
  [ ("managed-tuple", managedTupleLoweredProgram),
    ("managed-option", managedOptionLoweredProgram),
    ("managed-exported-option", managedOptionLoweredProgram),
    ("managed-tree", managedTreeLoweredProgram),
    ("managed-pair-binding", managedTupleLoweredProgram),
    ("managed-pair-identity", managedPairIdentityLoweredProgram),
    ("managed-pair-direct-tail", managedPairDirectTailLoweredProgram),
    ("managed-pair-recursive-capture", managedPairRecursiveCaptureLoweredProgram),
    ("managed-pair-conditional-join", managedPairConditionalJoinLoweredProgram),
    ("managed-pair-scalar-case-join", managedPairScalarCaseJoinLoweredProgram),
    ("managed-box-capture", managedBoxCaptureLoweredProgram)
  ]

managedProductVariantIndependentExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
managedProductVariantIndependentExpectedLoweredPrograms =
  [ ("managed-none", managedNoneProgram, managedNoneLoweredProgram),
    ("managed-tuple-variant", managedTupleVariantProgram, managedTupleVariantLoweredProgram),
    ("managed-text-variant", managedTextVariantProgram, managedTextVariantLoweredProgram),
    ("managed-closure-variant", managedClosureVariantProgram, managedClosureVariantLoweredProgram),
    ("managed-product-variant", managedProductVariantProgram, managedProductVariantLoweredProgram),
    ("managed-nested-variant", managedNestedVariantProgram, managedNestedVariantLoweredProgram)
  ]

managedProductVariantManifestExpectedPrograms :: [(Text, TypedProgram)]
managedProductVariantManifestExpectedPrograms =
  [ ("non-unit-tuple", manifestTupleProgram),
    ("data-value", manifestDataProgram)
  ]

managedProductVariantManifestExpectedLoweredPrograms :: [(Text, LoweredProgram)]
managedProductVariantManifestExpectedLoweredPrograms =
  [ ("non-unit-tuple", manifestTupleLoweredProgram),
    ("data-value", manifestDataLoweredProgram)
  ]

managedTupleLoweredProgram :: LoweredProgram
managedTupleLoweredProgram =
  managedLoweredProgram
    [textLayout, tupleLayout]
    tupleRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "two"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleRepresentation
        ( LoweredConstructProduct
            tupleLayoutId
            [intOperand 1, temporaryOperand 1 textRepresentation]
        )
    ]
    (temporaryOperand 2 tupleRepresentation)

managedOptionLoweredProgram :: LoweredProgram
managedOptionLoweredProgram =
  managedLoweredProgram
    [optionLayout]
    optionRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        optionRepresentation
        (LoweredConstructVariant optionLayoutId 1 [intOperand 7])
    ]
    (temporaryOperand 1 optionRepresentation)

managedNoneLoweredProgram :: LoweredProgram
managedNoneLoweredProgram =
  managedLoweredProgram
    [optionLayout]
    optionRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        optionRepresentation
        (LoweredConstructVariant optionLayoutId 0 [])
    ]
    (temporaryOperand 1 optionRepresentation)

managedTupleVariantLoweredProgram :: LoweredProgram
managedTupleVariantLoweredProgram =
  managedLoweredProgram
    [tupleVariantLayout, optionLayout]
    tupleVariantRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        optionRepresentation
        (LoweredConstructVariant optionLayoutId 1 [intOperand 7]),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleVariantRepresentation
        ( LoweredConstructProduct
            tupleVariantLayoutId
            [temporaryOperand 1 optionRepresentation, intOperand 8]
        )
    ]
    (temporaryOperand 2 tupleVariantRepresentation)

managedTextVariantLoweredProgram :: LoweredProgram
managedTextVariantLoweredProgram =
  managedLoweredProgram
    [textLayout, textBoxLayout]
    textBoxRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "inside"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        textBoxRepresentation
        (LoweredConstructVariant textBoxLayoutId 0 [temporaryOperand 1 textRepresentation])
    ]
    (temporaryOperand 2 textBoxRepresentation)

managedClosureVariantLoweredProgram :: LoweredProgram
managedClosureVariantLoweredProgram =
  managedLoweredProgramWithFunctions
    [closureBoxLayout, closureEnvironmentLayout]
    [ LoweredFunction
        closureFunctionId
        ( Just
            ( LoweredParameter
                (LoweredParameterId "environment")
                closureEnvironmentRepresentation
            )
        )
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        LoweredBoolRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredReturn
                    (LoweredFunctionParameterOperand (LoweredParameterId "arg1") LoweredBoolRepresentation)
                )
            )
        ]
        (LoweredBlockId "entry")
    ]
    closureBoxRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        closureEnvironmentRepresentation
        (LoweredConstructProduct closureEnvironmentLayoutId []),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        boolClosureRepresentation
        ( LoweredConstructClosure
            closureFunctionId
            (temporaryOperand 1 closureEnvironmentRepresentation)
        ),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        closureBoxRepresentation
        (LoweredConstructVariant closureBoxLayoutId 0 [temporaryOperand 2 boolClosureRepresentation])
    ]
    (temporaryOperand 3 closureBoxRepresentation)

managedProductVariantLoweredProgram :: LoweredProgram
managedProductVariantLoweredProgram =
  managedLoweredProgram
    [textLayout, productBoxLayout, tupleLayout]
    productBoxRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "two"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleRepresentation
        (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        productBoxRepresentation
        (LoweredConstructVariant productBoxLayoutId 0 [temporaryOperand 2 tupleRepresentation])
    ]
    (temporaryOperand 3 productBoxRepresentation)

managedNestedVariantLoweredProgram :: LoweredProgram
managedNestedVariantLoweredProgram =
  managedLoweredProgram
    [outerLayout, optionLayout]
    outerRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        optionRepresentation
        (LoweredConstructVariant optionLayoutId 1 [intOperand 7]),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        outerRepresentation
        (LoweredConstructVariant outerLayoutId 0 [temporaryOperand 1 optionRepresentation])
    ]
    (temporaryOperand 2 outerRepresentation)

managedTreeLoweredProgram :: LoweredProgram
managedTreeLoweredProgram =
  managedLoweredProgram
    [treeLayout]
    treeRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        treeRepresentation
        (LoweredConstructVariant treeLayoutId 0 [intOperand 1]),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        treeRepresentation
        (LoweredConstructVariant treeLayoutId 0 [intOperand 2]),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        treeRepresentation
        ( LoweredConstructVariant
            treeLayoutId
            1
            [temporaryOperand 1 treeRepresentation, temporaryOperand 2 treeRepresentation]
        )
    ]
    (temporaryOperand 3 treeRepresentation)

managedPairIdentityLoweredProgram :: LoweredProgram
managedPairIdentityLoweredProgram =
  managedLoweredProgramWithFunctions
    [textLayout, tupleLayout]
    [ LoweredFunction
        (LoweredFunctionId "App::Main::identity")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") tupleRepresentation]
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredReturn
                    (LoweredFunctionParameterOperand (LoweredParameterId "arg1") tupleRepresentation)
                )
            )
        ]
        (LoweredBlockId "entry")
    ]
    tupleRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "two"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleRepresentation
        (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        tupleRepresentation
        (LoweredDirectCall (LoweredFunctionId "App::Main::identity") [temporaryOperand 2 tupleRepresentation])
    ]
    (temporaryOperand 3 tupleRepresentation)

managedPairDirectTailLoweredProgram :: LoweredProgram
managedPairDirectTailLoweredProgram =
  managedLoweredProgramWithFunctions
    [textLayout, tupleLayout]
    [ loweredIdentityFunction,
      LoweredFunction
        (LoweredFunctionId "App::Main::forward")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") tupleRepresentation]
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredDirectTailCall
                    (LoweredFunctionId "App::Main::identity")
                    [LoweredFunctionParameterOperand (LoweredParameterId "arg1") tupleRepresentation]
                )
            )
        ]
        (LoweredBlockId "entry")
    ]
    tupleRepresentation
    pairConstructionInstructions
    (temporaryOperand 3 tupleRepresentation)
  where
    loweredIdentityFunction =
      LoweredFunction
        (LoweredFunctionId "App::Main::identity")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") tupleRepresentation]
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            (Just (LoweredReturn (LoweredFunctionParameterOperand (LoweredParameterId "arg1") tupleRepresentation)))
        ]
        (LoweredBlockId "entry")
    pairConstructionInstructions =
      [ LoweredInstruction
          (LoweredTemporaryId "t1")
          textRepresentation
          (LoweredConstructText textLayoutId "two"),
        LoweredInstruction
          (LoweredTemporaryId "t2")
          tupleRepresentation
          (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
        LoweredInstruction
          (LoweredTemporaryId "t3")
          tupleRepresentation
          (LoweredDirectCall (LoweredFunctionId "App::Main::forward") [temporaryOperand 2 tupleRepresentation])
      ]

managedPairRecursiveCaptureLoweredProgram :: LoweredProgram
managedPairRecursiveCaptureLoweredProgram =
  managedLoweredProgramWithFunctions
    [textLayout, tupleLayout, recursivePairEnvironmentLayout]
    [ LoweredFunction
        (LoweredFunctionId "App::Main::loop")
        (Just (LoweredParameter (LoweredParameterId "environment") recursivePairEnvironmentRepresentation))
        [LoweredParameter (LoweredParameterId "arg1") tupleRepresentation]
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                tupleRepresentation
                ( LoweredProjectField
                    recursivePairEnvironmentLayoutId
                    0
                    (LoweredFunctionParameterOperand (LoweredParameterId "environment") recursivePairEnvironmentRepresentation)
                ),
              LoweredInstruction
                (LoweredTemporaryId "t2")
                pairClosureRepresentation
                ( LoweredConstructClosure
                    (LoweredFunctionId "App::Main::loop")
                    (LoweredFunctionParameterOperand (LoweredParameterId "environment") recursivePairEnvironmentRepresentation)
                )
            ]
            (Just (LoweredClosureTailCall (temporaryOperand 2 pairClosureRepresentation) [temporaryOperand 1 tupleRepresentation]))
        ]
        (LoweredBlockId "entry")
    ]
    tupleRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "two"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleRepresentation
        (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        recursivePairEnvironmentRepresentation
        (LoweredConstructProduct recursivePairEnvironmentLayoutId [temporaryOperand 2 tupleRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t4")
        pairClosureRepresentation
        (LoweredConstructClosure (LoweredFunctionId "App::Main::loop") (temporaryOperand 3 recursivePairEnvironmentRepresentation)),
      LoweredInstruction
        (LoweredTemporaryId "t5")
        tupleRepresentation
        (LoweredClosureCall (temporaryOperand 4 pairClosureRepresentation) [temporaryOperand 2 tupleRepresentation])
    ]
    (temporaryOperand 5 tupleRepresentation)

managedPairConditionalJoinLoweredProgram :: LoweredProgram
managedPairConditionalJoinLoweredProgram =
  managedPairJoinLoweredProgram
    ( LoweredBranch
        (LoweredImmediateOperand (LoweredBoolImmediate True))
        conditionalThenBlockId
        []
        conditionalElseBlockId
        []
    )
    conditionalThenBlockId
    conditionalElseBlockId
    conditionalJoinBlockId
  where
    conditionalThenBlockId = LoweredBlockId "if$s1$2$e2$0,1$then"
    conditionalElseBlockId = LoweredBlockId "if$s1$2$e2$0,1$else"
    conditionalJoinBlockId = LoweredBlockId "if$s1$2$e2$0,1$join"

managedPairScalarCaseJoinLoweredProgram :: LoweredProgram
managedPairScalarCaseJoinLoweredProgram =
  managedPairJoinLoweredProgram
    ( LoweredBranch
        (temporaryOperand 1 LoweredBoolRepresentation)
        trueBodyBlockId
        []
        fallbackBodyBlockId
        []
    )
    trueBodyBlockId
    fallbackBodyBlockId
    joinBlockId
  where
    trueBodyBlockId = LoweredBlockId "case$s1$2$e2$0,1$a0$body"
    fallbackBodyBlockId = LoweredBlockId "case$s1$2$e2$0,1$a1$body"
    joinBlockId = LoweredBlockId "case$s1$2$e2$0,1$join"

managedPairJoinLoweredProgram :: LoweredTerminator -> LoweredBlockId -> LoweredBlockId -> LoweredBlockId -> LoweredProgram
managedPairJoinLoweredProgram entryTerminator firstBlockId secondBlockId joinBlockId =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout, tupleLayout]
    []
    [ loweredIdentityFunction,
      LoweredFunction
        (LoweredFunctionId "App::Main::$entry")
        Nothing
        []
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            entryInstructions
            (Just entryTerminator),
          pairBranch firstBlockId 1 "one",
          pairBranch secondBlockId 2 "two",
          LoweredBlock
            joinBlockId
            [LoweredParameter (LoweredParameterId "result") tupleRepresentation]
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                tupleRepresentation
                ( LoweredDirectCall
                    (LoweredFunctionId "App::Main::identity")
                    [LoweredBlockParameterOperand (LoweredParameterId "result") tupleRepresentation]
                )
            ]
            (Just (LoweredReturn (temporaryOperand 1 tupleRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    (LoweredFunctionId "App::Main::$entry")
  where
    entryInstructions =
      case entryTerminator of
        LoweredBranch (LoweredTemporaryOperand {}) _ _ _ _ ->
          [ LoweredInstruction
              (LoweredTemporaryId "t1")
              LoweredBoolRepresentation
              ( LoweredPrimitiveOperation
                  (LoweredComparisonPrimitive LoweredEqual)
                  [LoweredImmediateOperand (LoweredBoolImmediate True), LoweredImmediateOperand (LoweredBoolImmediate True)]
              )
          ]
        _ -> []
    loweredIdentityFunction =
      LoweredFunction
        (LoweredFunctionId "App::Main::identity")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") tupleRepresentation]
        tupleRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            (Just (LoweredReturn (LoweredFunctionParameterOperand (LoweredParameterId "arg1") tupleRepresentation)))
        ]
        (LoweredBlockId "entry")
    pairBranch blockId value textValue =
      LoweredBlock
        blockId
        []
        [ LoweredInstruction
            (LoweredTemporaryId "t1")
            textRepresentation
            (LoweredConstructText textLayoutId textValue),
          LoweredInstruction
            (LoweredTemporaryId "t2")
            tupleRepresentation
            (LoweredConstructProduct tupleLayoutId [intOperand value, temporaryOperand 1 textRepresentation])
        ]
        (Just (LoweredJump joinBlockId [temporaryOperand 2 tupleRepresentation]))

managedBoxCaptureLoweredProgram :: LoweredProgram
managedBoxCaptureLoweredProgram =
  managedLoweredProgramWithFunctions
    [textLayout, captureBoxLayout, tupleLayout, captureEnvironmentLayout]
    [ LoweredFunction
        (LoweredFunctionId "App::Main::capture")
        ( Just
            ( LoweredParameter
                (LoweredParameterId "environment")
                captureEnvironmentRepresentation
            )
        )
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        captureBoxRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                captureBoxRepresentation
                ( LoweredProjectField
                    captureEnvironmentLayoutId
                    0
                    ( LoweredFunctionParameterOperand
                        (LoweredParameterId "environment")
                        captureEnvironmentRepresentation
                    )
                )
            ]
            (Just (LoweredReturn (temporaryOperand 1 captureBoxRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    captureBoxRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        textRepresentation
        (LoweredConstructText textLayoutId "two"),
      LoweredInstruction
        (LoweredTemporaryId "t2")
        tupleRepresentation
        (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t3")
        captureBoxRepresentation
        (LoweredConstructVariant captureBoxLayoutId 0 [temporaryOperand 2 tupleRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t4")
        captureEnvironmentRepresentation
        (LoweredConstructProduct captureEnvironmentLayoutId [temporaryOperand 3 captureBoxRepresentation]),
      LoweredInstruction
        (LoweredTemporaryId "t5")
        captureClosureRepresentation
        ( LoweredConstructClosure
            (LoweredFunctionId "App::Main::capture")
            (temporaryOperand 4 captureEnvironmentRepresentation)
        ),
      LoweredInstruction
        (LoweredTemporaryId "t6")
        captureBoxRepresentation
        ( LoweredClosureCall
            (temporaryOperand 5 captureClosureRepresentation)
            [LoweredImmediateOperand (LoweredBoolImmediate True)]
        )
    ]
    (temporaryOperand 6 captureBoxRepresentation)

manifestTupleLoweredProgram :: LoweredProgram
manifestTupleLoweredProgram =
  managedLoweredProgram
    [manifestTupleLayout]
    manifestTupleRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        manifestTupleRepresentation
        (LoweredConstructProduct manifestTupleLayoutId [intOperand 1, intOperand 2])
    ]
    (temporaryOperand 1 manifestTupleRepresentation)

manifestDataLoweredProgram :: LoweredProgram
manifestDataLoweredProgram =
  managedLoweredProgram
    [manifestDataLayout]
    manifestDataRepresentation
    [ LoweredInstruction
        (LoweredTemporaryId "t1")
        manifestDataRepresentation
        (LoweredConstructVariant manifestDataLayoutId 0 [])
    ]
    (temporaryOperand 1 manifestDataRepresentation)

managedLoweredProgram :: [LoweredLayout] -> LoweredRepresentation -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
managedLoweredProgram layouts resultRepresentation instructions resultOperand =
  managedLoweredProgramWithFunctions layouts [] resultRepresentation instructions resultOperand

managedLoweredProgramWithFunctions :: [LoweredLayout] -> [LoweredFunction] -> LoweredRepresentation -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
managedLoweredProgramWithFunctions layouts functions resultRepresentation instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    layouts
    []
    ( functions
        <> [ LoweredFunction
               (LoweredFunctionId "App::Main::$entry")
               Nothing
               []
               resultRepresentation
               [ LoweredBlock
                   (LoweredBlockId "entry")
                   []
                   instructions
                   (Just (LoweredReturn resultOperand))
               ]
               (LoweredBlockId "entry")
           ]
    )
    (LoweredFunctionId "App::Main::$entry")

int64Representation :: LoweredRepresentation
int64Representation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64

intOperand :: Integer -> LoweredOperand
intOperand = LoweredImmediateOperand . LoweredSignedIntegerImmediate LoweredIntegerWidth64

temporaryOperand :: Int -> LoweredRepresentation -> LoweredOperand
temporaryOperand index =
  LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index)))

textLayoutId, tupleLayoutId, optionLayoutId, treeLayoutId, tupleVariantLayoutId, textBoxLayoutId, closureBoxLayoutId, closureEnvironmentLayoutId, productBoxLayoutId, outerLayoutId, captureBoxLayoutId, captureEnvironmentLayoutId, recursivePairEnvironmentLayoutId, manifestTupleLayoutId, manifestDataLayoutId :: LoweredLayoutId
textLayoutId = LoweredLayoutId "jazz.layout.text.v1"
tupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$4:text"
optionLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$3:int"
treeLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$4:Tree$args1$3:int"
tupleVariantLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$54:variant$module2$3:App$4:Main$name$6:Option$args1$3:int$8:signed64"
textBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$7:TextBox$args0"
closureBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$10:ClosureBox$args0"
closureEnvironmentLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p3$1,0,1$n4:flag"
productBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$10:ProductBox$args0"
outerLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$5:Outer$args0"
captureBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$3:Box$args0"
captureEnvironmentLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$3$n7:capture"
recursivePairEnvironmentLayoutId = LoweredLayoutId "$jz1$recursive-env$m2$3:App$4:Main$p1$2$n5:group"
manifestTupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$8:signed64"
manifestDataLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$11:ManifestBox$args0"

textRepresentation, tupleRepresentation, optionRepresentation, treeRepresentation, tupleVariantRepresentation, textBoxRepresentation, closureBoxRepresentation, closureEnvironmentRepresentation, productBoxRepresentation, outerRepresentation, captureBoxRepresentation, captureEnvironmentRepresentation, recursivePairEnvironmentRepresentation, manifestTupleRepresentation, manifestDataRepresentation :: LoweredRepresentation
textRepresentation = LoweredManagedReferenceRepresentation textLayoutId
tupleRepresentation = LoweredManagedReferenceRepresentation tupleLayoutId
optionRepresentation = LoweredManagedReferenceRepresentation optionLayoutId
treeRepresentation = LoweredManagedReferenceRepresentation treeLayoutId
tupleVariantRepresentation = LoweredManagedReferenceRepresentation tupleVariantLayoutId
textBoxRepresentation = LoweredManagedReferenceRepresentation textBoxLayoutId
closureBoxRepresentation = LoweredManagedReferenceRepresentation closureBoxLayoutId
closureEnvironmentRepresentation = LoweredManagedReferenceRepresentation closureEnvironmentLayoutId
productBoxRepresentation = LoweredManagedReferenceRepresentation productBoxLayoutId
outerRepresentation = LoweredManagedReferenceRepresentation outerLayoutId
captureBoxRepresentation = LoweredManagedReferenceRepresentation captureBoxLayoutId
captureEnvironmentRepresentation = LoweredManagedReferenceRepresentation captureEnvironmentLayoutId
recursivePairEnvironmentRepresentation = LoweredManagedReferenceRepresentation recursivePairEnvironmentLayoutId
manifestTupleRepresentation = LoweredManagedReferenceRepresentation manifestTupleLayoutId
manifestDataRepresentation = LoweredManagedReferenceRepresentation manifestDataLayoutId

textLayout, tupleLayout, optionLayout, treeLayout, tupleVariantLayout, textBoxLayout, closureBoxLayout, closureEnvironmentLayout, productBoxLayout, outerLayout, captureBoxLayout, captureEnvironmentLayout, recursivePairEnvironmentLayout, manifestTupleLayout, manifestDataLayout :: LoweredLayout
textLayout = LoweredLayout textLayoutId LoweredTextLayout
tupleLayout = LoweredLayout tupleLayoutId (LoweredProductLayout [int64Representation, textRepresentation])
optionLayout =
  LoweredLayout
    optionLayoutId
    (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [int64Representation]])
treeLayout =
  LoweredLayout
    treeLayoutId
    ( LoweredVariantLayouts
        [ LoweredVariantLayout 0 [int64Representation],
          LoweredVariantLayout 1 [treeRepresentation, treeRepresentation]
        ]
    )
tupleVariantLayout = LoweredLayout tupleVariantLayoutId (LoweredProductLayout [optionRepresentation, int64Representation])
textBoxLayout = LoweredLayout textBoxLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 [textRepresentation]])
closureBoxLayout = LoweredLayout closureBoxLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 [boolClosureRepresentation]])
closureEnvironmentLayout = LoweredLayout closureEnvironmentLayoutId (LoweredClosureEnvironmentLayout [])
productBoxLayout = LoweredLayout productBoxLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 [tupleRepresentation]])
outerLayout = LoweredLayout outerLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 [optionRepresentation]])
captureBoxLayout = LoweredLayout captureBoxLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 [tupleRepresentation]])
captureEnvironmentLayout = LoweredLayout captureEnvironmentLayoutId (LoweredClosureEnvironmentLayout [captureBoxRepresentation])
recursivePairEnvironmentLayout = LoweredLayout recursivePairEnvironmentLayoutId (LoweredClosureEnvironmentLayout [tupleRepresentation])
manifestTupleLayout = LoweredLayout manifestTupleLayoutId (LoweredProductLayout [int64Representation, int64Representation])
manifestDataLayout = LoweredLayout manifestDataLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 []])

boolClosureRepresentation :: LoweredRepresentation
boolClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)

closureFunctionId :: LoweredFunctionId
closureFunctionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p3$1,0,1$n4:flag"

captureClosureRepresentation :: LoweredRepresentation
captureClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [LoweredBoolRepresentation] captureBoxRepresentation)

pairClosureRepresentation :: LoweredRepresentation
pairClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [tupleRepresentation] tupleRepresentation)

manifestTupleProgram :: TypedProgram
manifestTupleProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        (TypedTupleExpr tupleInfo [intExpr 1, intExpr 2])
    ]
    tupleInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedIntType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64])
        []
        []

manifestDataProgram :: TypedProgram
manifestDataProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (TypedVariableExpr manifestBoxInfo manifestBoxName (Just manifestBoxBinder))
    ]
    manifestBoxInfo
  where
    dataName = typeName "ManifestBox"
    manifestBoxName = constructorName "ManifestBox"
    manifestBoxBinder = constructorBinder 0 manifestBoxName
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        dataName
        []
        [TypedConstructorDeclaration manifestBoxBinder manifestBoxName [] []]
    manifestBoxInfo = variantInfo dataName []

managedTupleProgram :: TypedProgram
managedTupleProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        (TypedTupleExpr tupleInfo [intExpr 1, textExpr "two"])
    ]
    tupleInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedTextType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe])
        []
        []

managedAsConstructorTuplePatternProgram :: TypedProgram
managedAsConstructorTuplePatternProgram =
  managedProgram
    [ TypedDataStatement maybeDeclaration,
      TypedLetStatement
        subjectBinder
        subjectName
        (TypedSpan 3 1)
        (valueScheme subjectBinder maybeTupleInfo)
        (constructorCall justBinder justName maybeTupleInfo [tupleInfo] [subjectTuple]),
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( TypedPatternCaseExpr
            int64Info
            (TypedVariableExpr maybeTupleInfo subjectName (Just subjectBinder))
            [ TypedCaseArm
                ( TypedAsPattern
                    maybeTupleInfo
                    wholeBinder
                    wholeName
                    ( TypedConstructorPattern
                        maybeTupleInfo
                        justName
                        [ TypedTuplePattern
                            tupleInfo
                            [ TypedVariablePattern int64Info itemBinder itemName,
                              TypedLiteralPattern boolInfo (TypedBooleanLiteral True)
                            ]
                        ]
                    )
                )
                Nothing
                (TypedVariableExpr int64Info itemName (Just itemBinder)),
              TypedCaseArm
                (TypedConstructorPattern maybeTupleInfo nothingName [])
                Nothing
                (int64Expr 0),
              TypedCaseArm
                ( TypedConstructorPattern
                    maybeTupleInfo
                    justName
                    [ TypedTuplePattern
                        tupleInfo
                        [ TypedWildcardPattern int64Info,
                          TypedLiteralPattern boolInfo (TypedBooleanLiteral False)
                        ]
                    ]
                )
                Nothing
                (int64Expr 1)
            ]
        )
    ]
    int64Info
  where
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    maybeName = typeName "Maybe"
    nothingName = constructorName "Nothing"
    justName = constructorName "Just"
    nothingBinder = constructorBinder 0 nothingName
    justBinder = constructorBinder 1 justName
    maybeDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        maybeName
        [parameter]
        [ TypedConstructorDeclaration nothingBinder nothingName [] [],
          TypedConstructorDeclaration justBinder justName [parameterType] [TypedRepresentationParameterRecipe parameter]
        ]
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedNumericType TypedInt64Type, TypedBoolType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedBoolRecipe])
        []
        []
    maybeTupleInfo = variantInfo maybeName [typedExpressionType tupleInfo]
    subjectName = valueName "subject"
    subjectBinder = statementBinder 1 subjectName
    subjectTuple = TypedTupleExpr tupleInfo [int64Expr 41, boolExpr True]
    wholeName = valueName "whole"
    wholeBinder = TypedBinderId (modulePath, [2, 0], wholeName)
    itemName = valueName "item"
    itemBinder = TypedBinderId (modulePath, [2, 0, 0, 0, 0], itemName)
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedTopLevelOrPatternProgram :: TypedProgram
managedTopLevelOrPatternProgram =
  managedProgram
    [ TypedDataStatement choiceDeclaration,
      TypedLetStatement
        subjectBinder
        subjectName
        (TypedSpan 3 1)
        (valueScheme subjectBinder choiceIntInfo)
        (constructorCall rightBinder rightName choiceIntInfo [int64Info] [int64Expr 7]),
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( TypedPatternCaseExpr
            int64Info
            (TypedVariableExpr choiceIntInfo subjectName (Just subjectBinder))
            [ TypedCaseArm
                ( TypedOrPattern
                    choiceIntInfo
                    [ TypedConstructorPattern choiceIntInfo leftName [TypedVariablePattern int64Info leftItemBinder itemName],
                      TypedConstructorPattern choiceIntInfo rightName [TypedVariablePattern int64Info rightItemBinder itemName]
                    ]
                )
                Nothing
                (TypedVariableExpr int64Info itemName (Just leftItemBinder))
            ]
        )
    ]
    int64Info
  where
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    choiceName = typeName "Choice"
    leftName = constructorName "Left"
    rightName = constructorName "Right"
    leftBinder = constructorBinder 0 leftName
    rightBinder = constructorBinder 1 rightName
    choiceDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        choiceName
        [parameter]
        [ TypedConstructorDeclaration leftBinder leftName [parameterType] [TypedRepresentationParameterRecipe parameter],
          TypedConstructorDeclaration rightBinder rightName [parameterType] [TypedRepresentationParameterRecipe parameter]
        ]
    choiceIntInfo = variantInfo choiceName [TypedNumericType TypedInt64Type]
    subjectName = valueName "subject"
    subjectBinder = statementBinder 1 subjectName
    itemName = valueName "item"
    leftItemBinder = TypedBinderId (modulePath, [2, 0, 0, 0], itemName)
    rightItemBinder = TypedBinderId (modulePath, [2, 0, 1, 0], itemName)
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedMissingConstructorPatternProgram :: TypedProgram
managedMissingConstructorPatternProgram =
  rewriteTerminalPatternArms retainFirstAlternative managedTopLevelOrPatternProgram
  where
    retainFirstAlternative [TypedCaseArm (TypedOrPattern _ (firstAlternative : _)) maybeGuard body] =
      [TypedCaseArm firstAlternative maybeGuard body]
    retainFirstAlternative _ = error "managed top-level or-pattern fixture must retain one or-pattern arm"

managedOtherMissingConstructorPatternProgram :: TypedProgram
managedOtherMissingConstructorPatternProgram =
  rewriteTerminalPatternArms retainSecondAlternative managedTopLevelOrPatternProgram
  where
    retainSecondAlternative [TypedCaseArm (TypedOrPattern _ (_ : secondAlternative : _)) maybeGuard body] =
      case (secondAlternative, body) of
        ( TypedConstructorPattern _ _ [TypedVariablePattern _ binder _],
          TypedVariableExpr info name _
          ) ->
            [TypedCaseArm secondAlternative maybeGuard (TypedVariableExpr info name (Just binder))]
        _ -> error "managed second constructor alternative must bind the arm result"
    retainSecondAlternative _ = error "managed top-level or-pattern fixture must retain one or-pattern arm"

managedGuardedConstructorsPatternProgram :: TypedProgram
managedGuardedConstructorsPatternProgram =
  rewriteTerminalPatternArms guardOnlyArm managedTopLevelOrPatternProgram
  where
    guardOnlyArm [TypedCaseArm patternValue Nothing body] =
      [TypedCaseArm patternValue (Just (boolExpr True)) body]
    guardOnlyArm _ = error "managed top-level or-pattern fixture must retain one unguarded arm"

managedTotalTuplePatternProgram :: TypedProgram
managedTotalTuplePatternProgram = managedTuplePatternProfileProgram True

managedIncompleteTuplePatternProgram :: TypedProgram
managedIncompleteTuplePatternProgram = managedTuplePatternProfileProgram False

managedBoolLiteralPatternProgram :: TypedProgram
managedBoolLiteralPatternProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            int64Info
            (boolExpr True)
            [ TypedCaseArm
                (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
                Nothing
                (int64Expr 1),
              TypedCaseArm
                (TypedLiteralPattern boolInfo (TypedBooleanLiteral False))
                Nothing
                (int64Expr 0)
            ]
        )
    ]
    int64Info
  where
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedTuplePatternProfileProgram :: Bool -> TypedProgram
managedTuplePatternProfileProgram totalNestedFields =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            int64Info
            (TypedTupleExpr tupleInfo [int64Expr 1, boolExpr True])
            [ TypedCaseArm
                ( TypedTuplePattern
                    tupleInfo
                    [ if totalNestedFields
                        then TypedWildcardPattern int64Info
                        else TypedLiteralPattern int64Info (TypedIntegerLiteral "1"),
                      TypedWildcardPattern boolInfo
                    ]
                )
                Nothing
                (int64Expr 1)
            ]
        )
    ]
    int64Info
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedNumericType TypedInt64Type, TypedBoolType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedBoolRecipe])
        []
        []
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedListPatternProgram :: TypedProgram
managedListPatternProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            int64Info
            (TypedListExpr listInfo [int64Expr 1])
            [ TypedCaseArm
                (TypedListPattern listInfo [TypedWildcardPattern int64Info])
                Nothing
                (int64Expr 1),
              TypedCaseArm
                (TypedWildcardPattern listInfo)
                Nothing
                (int64Expr 2)
            ]
        )
    ]
    int64Info
  where
    listInfo =
      TypedNodeInfo
        (TypedListType (TypedNumericType TypedInt64Type))
        (TypedManagedListRecipe (TypedSignedIntegerRecipe 64))
        []
        []
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedNestedOrPatternProgram :: TypedProgram
managedNestedOrPatternProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            int64Info
            (monomorphicConstructorCall boxBinder boxConstructor boxInfo [boolInfo] [boolExpr True])
            [ TypedCaseArm
                ( TypedConstructorPattern
                    boxInfo
                    boxConstructor
                    [ TypedOrPattern
                        boolInfo
                        [ TypedLiteralPattern boolInfo (TypedBooleanLiteral True),
                          TypedLiteralPattern boolInfo (TypedBooleanLiteral False)
                        ]
                    ]
                )
                Nothing
                (int64Expr 1),
              TypedCaseArm (TypedWildcardPattern boxInfo) Nothing (int64Expr 2)
            ]
        )
    ]
    int64Info
  where
    boxName = typeName "BoolBox"
    boxConstructor = constructorName "BoolBox"
    boxBinder = constructorBinder 0 boxConstructor
    boxInfo = variantInfo boxName []
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        boxName
        []
        [TypedConstructorDeclaration boxBinder boxConstructor [TypedBoolType] [TypedBoolRecipe]]
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

managedTextLiteralPatternProgram :: TypedProgram
managedTextLiteralPatternProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            int64Info
            (monomorphicConstructorCall boxBinder boxConstructor boxInfo [textInfo] [textExpr "inside"])
            [ TypedCaseArm
                ( TypedConstructorPattern
                    boxInfo
                    boxConstructor
                    [TypedLiteralPattern textInfo (TypedTextLiteral "inside")]
                )
                Nothing
                (int64Expr 1),
              TypedCaseArm (TypedWildcardPattern boxInfo) Nothing (int64Expr 2)
            ]
        )
    ]
    int64Info
  where
    boxName = typeName "TextBox"
    boxConstructor = constructorName "TextBox"
    boxBinder = constructorBinder 0 boxConstructor
    boxInfo = variantInfo boxName []
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        boxName
        []
        [TypedConstructorDeclaration boxBinder boxConstructor [TypedTextType] [TypedManagedTextRecipe]]
    int64Info = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []
    int64Expr :: Integer -> TypedExpr
    int64Expr value = TypedLiteralExpr int64Info (TypedIntegerLiteral (Text.pack (show value)))

rewriteTerminalPatternArms :: ([TypedCaseArm] -> [TypedCaseArm]) -> TypedProgram -> TypedProgram
rewriteTerminalPatternArms rewriteArms programValue =
  case programValue of
    TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
      case reverse statements of
        TypedExpressionStatement spanValue (TypedPatternCaseExpr info scrutinee arms) : reversedPrefix ->
          TypedProgram
            prelude
            [ TypedModule
                path
                source
                imports
                exports
                interface
                recursiveGroups
                ( reverse reversedPrefix
                    <> [ TypedExpressionStatement
                           spanValue
                           (TypedPatternCaseExpr info scrutinee (rewriteArms arms))
                       ]
                )
                moduleInfo
            ]
            entryPath
        _ -> error "managed pattern fixture must end in a pattern-case expression"
    _ -> error "managed pattern fixture must contain one module"

managedOptionProgram :: TypedProgram
managedOptionProgram = optionProgram [] (TypedModuleInterface [] [] [] [])

managedNoneProgram :: TypedProgram
managedNoneProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement (TypedSpan 3 1) noneExpression
    ]
    (typedExpressionInfo noneExpression)
  where
    noneExpression = constructorCall noneBinder noneName optionIntInfo [] []

managedTupleVariantProgram :: TypedProgram
managedTupleVariantProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedTupleExpr
            tupleInfo
            [constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 7], intExpr 8]
        )
    ]
    tupleInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [typedExpressionType optionIntInfo, TypedIntType])
        (TypedManagedProductRecipe [typedExpressionRecipe optionIntInfo, TypedSignedIntegerRecipe 64])
        []
        []

managedTextVariantProgram :: TypedProgram
managedTextVariantProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (monomorphicConstructorCall binder constructor boxInfo [textInfo] [textExpr "inside"])
    ]
    boxInfo
  where
    name = typeName "TextBox"
    constructor = constructorName "TextBox"
    binder = constructorBinder 0 constructor
    boxInfo = variantInfo name []
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        name
        []
        [TypedConstructorDeclaration binder constructor [TypedTextType] [TypedManagedTextRecipe]]

managedClosureVariantProgram :: TypedProgram
managedClosureVariantProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (monomorphicConstructorCall binder constructor boxInfo [closureInfo] [closureExpression])
    ]
    boxInfo
  where
    name = typeName "ClosureBox"
    constructor = constructorName "ClosureBox"
    binder = constructorBinder 0 constructor
    boxInfo = variantInfo name []
    parameterName = TypedResolvedName TypedCurrentModule TypedValueNamespace "flag"
    parameterBinder = TypedBinderId (modulePath, [1, 0, 1], parameterName)
    closureInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType TypedBoolType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
        []
        []
    closureExpression =
      TypedLambdaExpr
        closureInfo
        parameterBinder
        parameterName
        (TypedVariableExpr boolInfo parameterName (Just parameterBinder))
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        name
        []
        [ TypedConstructorDeclaration
            binder
            constructor
            [typedExpressionType closureInfo]
            [typedExpressionRecipe closureInfo]
        ]

managedProductVariantProgram :: TypedProgram
managedProductVariantProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (monomorphicConstructorCall binder constructor boxInfo [tupleInfo] [tupleExpression])
    ]
    boxInfo
  where
    name = typeName "ProductBox"
    constructor = constructorName "ProductBox"
    binder = constructorBinder 0 constructor
    boxInfo = variantInfo name []
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedTextType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe])
        []
        []
    tupleExpression = TypedTupleExpr tupleInfo [intExpr 1, textExpr "two"]
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        name
        []
        [ TypedConstructorDeclaration
            binder
            constructor
            [typedExpressionType tupleInfo]
            [typedExpressionRecipe tupleInfo]
        ]

managedNestedVariantProgram :: TypedProgram
managedNestedVariantProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedDataStatement outerDeclaration,
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( monomorphicConstructorCall
            outerBinder
            outerConstructor
            outerInfo
            [optionIntInfo]
            [constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 7]]
        )
    ]
    outerInfo
  where
    outerName = typeName "Outer"
    outerConstructor = constructorName "Outer"
    outerBinder = catalogConstructorBinder 1 0 outerConstructor
    outerInfo = variantInfo outerName []
    outerDeclaration =
      TypedDataDeclaration
        (TypedSpan 3 1)
        outerName
        []
        [ TypedConstructorDeclaration
            outerBinder
            outerConstructor
            [typedExpressionType optionIntInfo]
            [typedExpressionRecipe optionIntInfo]
        ]

managedExportedOptionProgram :: TypedProgram
managedExportedOptionProgram =
  optionProgram
    [ TypedModuleExport TypedTypeNamespace "Option",
      TypedModuleExport TypedConstructorNamespace "None",
      TypedModuleExport TypedConstructorNamespace "Some"
    ]
    (TypedModuleInterface [] [TypedDataInterface optionDeclaration] [] [])

optionProgram :: [TypedModuleExport] -> TypedModuleInterface -> TypedProgram
optionProgram exports interface =
  managedProgramWithInterface
    exports
    interface
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 7])
    ]
    optionIntInfo

optionParameter :: TypedTypeParameterId
optionParameter = TypedTypeParameterId 0

optionName, noneName, someName :: TypedCoreName
optionName = typeName "Option"
noneName = constructorName "None"
someName = constructorName "Some"

noneBinder, someBinder :: TypedBinderId
noneBinder = constructorBinder 0 noneName
someBinder = constructorBinder 1 someName

optionDeclaration :: TypedDataDeclaration
optionDeclaration =
  TypedDataDeclaration
    (TypedSpan 2 1)
    optionName
    [optionParameter]
    [ TypedConstructorDeclaration noneBinder noneName [] [],
      TypedConstructorDeclaration
        someBinder
        someName
        [TypedTypeParameterType optionParameter]
        [TypedRepresentationParameterRecipe optionParameter]
    ]

optionIntInfo :: TypedNodeInfo
optionIntInfo = variantInfo optionName [TypedIntType]

managedTreeProgram :: TypedProgram
managedTreeProgram =
  managedProgram
    [ TypedDataStatement treeDeclaration,
      TypedExpressionStatement (TypedSpan 3 1) branchExpression
    ]
    treeIntInfo
  where
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    treeName = typeName "Tree"
    leafName = constructorName "Leaf"
    branchName = constructorName "Branch"
    leafBinder = constructorBinder 0 leafName
    branchBinder = constructorBinder 1 branchName
    genericTreeType = TypedDataType treeName [parameterType]
    genericTreeRecipe = TypedManagedVariantRecipe treeName [parameterType]
    treeDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        treeName
        [parameter]
        [ TypedConstructorDeclaration
            leafBinder
            leafName
            [parameterType]
            [TypedRepresentationParameterRecipe parameter],
          TypedConstructorDeclaration
            branchBinder
            branchName
            [genericTreeType, genericTreeType]
            [genericTreeRecipe, genericTreeRecipe]
        ]
    treeIntInfo = variantInfo treeName [TypedIntType]
    leaf value = constructorCall leafBinder leafName treeIntInfo [intInfo] [intExpr value]
    branchExpression =
      constructorCall
        branchBinder
        branchName
        treeIntInfo
        [treeIntInfo, treeIntInfo]
        [leaf 1, leaf 2]

managedPairBindingProgram :: TypedProgram
managedPairBindingProgram =
  managedProgram
    [ TypedLetStatement
        pairBinder
        pairName
        (TypedSpan 2 1)
        (valueScheme pairBinder managedPairBindingInfo)
        managedPairBindingExpression,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (TypedVariableExpr managedPairBindingInfo pairName (Just pairBinder))
    ]
    managedPairBindingInfo
  where
    pairName = TypedResolvedName TypedCurrentModule TypedValueNamespace "pair"
    pairBinder = TypedBinderId (modulePath, [0], pairName)

managedPairIdentityProgram :: TypedProgram
managedPairIdentityProgram =
  managedProgram
    [ TypedSignatureStatement
        signatureBinder
        identityName
        (TypedSpan 2 1)
        (callableScheme signatureBinder TypedDirectCallableShape identityInfo),
      TypedLetStatement
        identityBinder
        identityName
        (TypedSpan 3 1)
        (callableScheme identityBinder TypedDirectCallableShape identityInfo)
        ( TypedLambdaExpr
            identityInfo
            parameterBinder
            parameterName
            (TypedVariableExpr managedPairInfo parameterName (Just parameterBinder))
        ),
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( TypedApplyExpr
            managedPairInfo
            (TypedVariableExpr identityInfo identityName (Just identityBinder))
            managedPairExpression
        )
    ]
    managedPairInfo
  where
    identityName = TypedResolvedName TypedCurrentModule TypedValueNamespace "identity"
    signatureBinder = TypedBinderId (modulePath, [0], identityName)
    identityBinder = TypedBinderId (modulePath, [1], identityName)
    parameterName = TypedResolvedName TypedCurrentModule TypedValueNamespace "item"
    parameterBinder = TypedBinderId (modulePath, [1, 0], parameterName)
    identityInfo =
      TypedNodeInfo
        (TypedFunctionType (typedExpressionType managedPairInfo) (typedExpressionType managedPairInfo))
        (TypedClosureRecipe [typedExpressionRecipe managedPairInfo] (typedExpressionRecipe managedPairInfo))
        []
        []

managedPairDirectTailProgram :: TypedProgram
managedPairDirectTailProgram =
  managedProgram
    [ signatureStatement 0 2 identityName,
      identityStatement,
      signatureStatement 2 4 forwardName,
      forwardStatement,
      TypedExpressionStatement
        (TypedSpan 6 1)
        ( TypedApplyExpr
            managedPairInfo
            (TypedVariableExpr pairFunctionInfo forwardName (Just forwardBinder))
            managedPairExpression
        )
    ]
    managedPairInfo
  where
    identityName = valueName "identity"
    identityBinder = statementBinder 1 identityName
    identityParameterName = valueName "item"
    identityParameterBinder = TypedBinderId (modulePath, [1, 0], identityParameterName)
    forwardName = valueName "forward"
    forwardBinder = statementBinder 3 forwardName
    forwardParameterName = valueName "item"
    forwardParameterBinder = TypedBinderId (modulePath, [3, 0], forwardParameterName)
    signatureStatement statementIndex line name =
      let binder = statementBinder statementIndex name
       in TypedSignatureStatement binder name (TypedSpan line 1) (callableScheme binder TypedDirectCallableShape pairFunctionInfo)
    identityStatement =
      TypedLetStatement
        identityBinder
        identityName
        (TypedSpan 3 1)
        (callableScheme identityBinder TypedDirectCallableShape pairFunctionInfo)
        ( TypedLambdaExpr
            pairFunctionInfo
            identityParameterBinder
            identityParameterName
            (TypedVariableExpr managedPairInfo identityParameterName (Just identityParameterBinder))
        )
    forwardStatement =
      TypedLetStatement
        forwardBinder
        forwardName
        (TypedSpan 5 1)
        (callableScheme forwardBinder TypedDirectCallableShape pairFunctionInfo)
        ( TypedLambdaExpr
            pairFunctionInfo
            forwardParameterBinder
            forwardParameterName
            ( TypedApplyExpr
                managedPairInfo
                (TypedVariableExpr pairFunctionInfo identityName (Just identityBinder))
                (TypedVariableExpr managedPairInfo forwardParameterName (Just forwardParameterBinder))
            )
        )

managedPairRecursiveCaptureProgram :: TypedProgram
managedPairRecursiveCaptureProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedRecursiveGroup [loopBinder]]
        [ TypedLetStatement
            pairBinder
            pairName
            (TypedSpan 2 1)
            (valueScheme pairBinder managedPairInfo)
            managedPairExpression,
          TypedSignatureStatement
            loopSignatureBinder
            loopName
            (TypedSpan 3 1)
            (callableScheme loopSignatureBinder TypedClosureCallableShape pairFunctionInfo),
          TypedLetStatement
            loopBinder
            loopName
            (TypedSpan 4 1)
            (callableScheme loopBinder TypedClosureCallableShape pairFunctionInfo)
            ( TypedLambdaExpr
                pairFunctionInfo
                parameterBinder
                parameterName
                loopCall
            ),
          TypedExpressionStatement (TypedSpan 5 1) loopCall
        ]
        managedPairInfo
    ]
    modulePath
  where
    pairName = valueName "pair"
    pairBinder = statementBinder 0 pairName
    loopName = valueName "loop"
    loopSignatureBinder = statementBinder 1 loopName
    loopBinder = statementBinder 2 loopName
    parameterName = valueName "item"
    parameterBinder = TypedBinderId (modulePath, [2, 0], parameterName)
    loopCall =
      TypedApplyExpr
        managedPairInfo
        (TypedVariableExpr pairFunctionInfo loopName (Just loopBinder))
        (TypedVariableExpr managedPairInfo pairName (Just pairBinder))

managedPairConditionalJoinProgram :: TypedProgram
managedPairConditionalJoinProgram =
  managedPairJoinProgram
    ( TypedIfExpr
        managedPairInfo
        (boolExpr True)
        (managedPairExpressionWith 1 "one")
        (managedPairExpressionWith 2 "two")
    )

managedPairScalarCaseJoinProgram :: TypedProgram
managedPairScalarCaseJoinProgram =
  managedPairJoinProgram
    ( TypedPatternCaseExpr
        managedPairInfo
        (boolExpr True)
        [ TypedCaseArm
            (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
            Nothing
            (managedPairExpressionWith 1 "one"),
          TypedCaseArm
            (TypedWildcardPattern boolInfo)
            Nothing
            (managedPairExpressionWith 2 "two")
        ]
    )

managedPairJoinProgram :: TypedExpr -> TypedProgram
managedPairJoinProgram argument =
  managedProgram
    [ TypedSignatureStatement
        signatureBinder
        identityName
        (TypedSpan 2 1)
        (callableScheme signatureBinder TypedDirectCallableShape pairFunctionInfo),
      TypedLetStatement
        identityBinder
        identityName
        (TypedSpan 3 1)
        (callableScheme identityBinder TypedDirectCallableShape pairFunctionInfo)
        ( TypedLambdaExpr
            pairFunctionInfo
            parameterBinder
            parameterName
            (TypedVariableExpr managedPairInfo parameterName (Just parameterBinder))
        ),
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( TypedApplyExpr
            managedPairInfo
            (TypedVariableExpr pairFunctionInfo identityName (Just identityBinder))
            argument
        )
    ]
    managedPairInfo
  where
    identityName = valueName "identity"
    signatureBinder = statementBinder 0 identityName
    identityBinder = statementBinder 1 identityName
    parameterName = valueName "item"
    parameterBinder = TypedBinderId (modulePath, [1, 0], parameterName)

managedBoxCaptureProgram :: TypedProgram
managedBoxCaptureProgram =
  managedProgram
    [ TypedDataStatement boxDeclaration,
      TypedLetStatement
        boxBinder
        boxName
        (TypedSpan 3 1)
        (valueScheme boxBinder boxInfo)
        boxExpression,
      TypedSignatureStatement
        captureSignatureBinder
        captureName
        (TypedSpan 4 1)
        (callableScheme captureSignatureBinder TypedClosureCallableShape captureInfo),
      TypedLetStatement
        captureBinder
        captureName
        (TypedSpan 5 1)
        (callableScheme captureBinder TypedClosureCallableShape captureInfo)
        ( TypedLambdaExpr
            captureInfo
            ignoredBinder
            ignoredName
            (TypedVariableExpr boxInfo boxName (Just boxBinder))
        ),
      TypedExpressionStatement
        (TypedSpan 6 1)
        ( TypedApplyExpr
            boxInfo
            (TypedVariableExpr captureInfo captureName (Just captureBinder))
            (boolExpr True)
        )
    ]
    boxInfo
  where
    dataName = typeName "Box"
    constructorNameValue = constructorName "Box"
    constructorBinderValue = catalogConstructorBinder 0 0 constructorNameValue
    boxInfo = variantInfo dataName []
    boxDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        dataName
        []
        [ TypedConstructorDeclaration
            constructorBinderValue
            constructorNameValue
            [typedExpressionType managedPairInfo]
            [typedExpressionRecipe managedPairInfo]
        ]
    boxName = TypedResolvedName TypedCurrentModule TypedValueNamespace "box"
    boxBinder = TypedBinderId (modulePath, [1], boxName)
    boxExpression =
      monomorphicConstructorCall
        constructorBinderValue
        constructorNameValue
        boxInfo
        [managedPairInfo]
        [managedPairExpression]
    captureName = TypedResolvedName TypedCurrentModule TypedValueNamespace "capture"
    captureSignatureBinder = TypedBinderId (modulePath, [2], captureName)
    captureBinder = TypedBinderId (modulePath, [3], captureName)
    ignoredName = TypedResolvedName TypedCurrentModule TypedValueNamespace "ignored"
    ignoredBinder = TypedBinderId (modulePath, [3, 0], ignoredName)
    captureInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (typedExpressionType boxInfo))
        (TypedClosureRecipe [TypedBoolRecipe] (typedExpressionRecipe boxInfo))
        []
        []

managedPairInfo :: TypedNodeInfo
managedPairInfo =
  TypedNodeInfo
    (TypedTupleType [TypedIntType, TypedTextType])
    (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe])
    []
    []

managedPairExpression :: TypedExpr
managedPairExpression = TypedTupleExpr managedPairInfo [intExpr 1, textExpr "two"]

managedPairExpressionWith :: Integer -> Text -> TypedExpr
managedPairExpressionWith value textValue =
  TypedTupleExpr managedPairInfo [intExpr value, textExpr textValue]

pairFunctionInfo :: TypedNodeInfo
pairFunctionInfo =
  TypedNodeInfo
    (TypedFunctionType (typedExpressionType managedPairInfo) (typedExpressionType managedPairInfo))
    (TypedClosureRecipe [typedExpressionRecipe managedPairInfo] (typedExpressionRecipe managedPairInfo))
    []
    []

managedPairBindingInfo :: TypedNodeInfo
managedPairBindingInfo =
  TypedNodeInfo
    (TypedTupleType [TypedNumericType TypedInt64Type, TypedTextType])
    (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe])
    []
    []

managedPairBindingExpression :: TypedExpr
managedPairBindingExpression =
  TypedTupleExpr
    managedPairBindingInfo
    [ TypedLiteralExpr
        (TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] [])
        (TypedIntegerLiteral "1"),
      textExpr "two"
    ]

valueName :: Text -> TypedCoreName
valueName = TypedResolvedName TypedCurrentModule TypedValueNamespace

statementBinder :: Int -> TypedCoreName -> TypedBinderId
statementBinder statementIndex name = TypedBinderId (modulePath, [statementIndex], name)

valueScheme :: TypedBinderId -> TypedNodeInfo -> TypedScheme
valueScheme owner info =
  TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info) Nothing

callableScheme :: TypedBinderId -> TypedCallableShape -> TypedNodeInfo -> TypedScheme
callableScheme owner shape info =
  TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info) (Just shape)

managedProgram :: [TypedStatement] -> TypedNodeInfo -> TypedProgram
managedProgram statements moduleInfo =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        statements
        moduleInfo
    ]
    modulePath

managedProgramWithInterface :: [TypedModuleExport] -> TypedModuleInterface -> [TypedStatement] -> TypedNodeInfo -> TypedProgram
managedProgramWithInterface exports interface statements moduleInfo =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        exports
        interface
        []
        statements
        moduleInfo
    ]
    modulePath

constructorCall :: TypedBinderId -> TypedCoreName -> TypedNodeInfo -> [TypedNodeInfo] -> [TypedExpr] -> TypedExpr
constructorCall owner name resultInfo fieldInfos arguments =
  constructorCallWithInstantiations [instantiation] owner name resultInfo fieldInfos arguments
  where
    typeArguments =
      case typedExpressionType resultInfo of
        TypedDataType _ argumentsValue ->
          zipWith TypedTypeArgument [TypedTypeParameterId index | index <- [0 ..]] argumentsValue
        _ -> []
    instantiation = TypedInstantiation owner typeArguments Nothing

constructorCallWithInstantiations :: [TypedInstantiation] -> TypedBinderId -> TypedCoreName -> TypedNodeInfo -> [TypedNodeInfo] -> [TypedExpr] -> TypedExpr
constructorCallWithInstantiations instantiations owner name resultInfo fieldInfos arguments =
  case fieldInfos of
    [] -> TypedVariableExpr (withInstantiations resultInfo) name (Just owner)
    _ -> saturated constructorExpression fieldInfos arguments
  where
    constructorInfo =
      TypedNodeInfo
        (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldInfos)
        (TypedClosureRecipe (map typedExpressionRecipe fieldInfos) (typedExpressionRecipe resultInfo))
        instantiations
        []
    constructorExpression = TypedVariableExpr constructorInfo name (Just owner)

    withInstantiations (TypedNodeInfo typeValue recipe _ evidence) =
      TypedNodeInfo typeValue recipe instantiations evidence

    saturated function remainingFields remainingArguments =
      case (remainingFields, remainingArguments) of
        (_ : fieldRest, argument : argumentRest) ->
          let applicationInfo =
                case fieldRest of
                  [] -> resultInfo
                  _ ->
                    TypedNodeInfo
                      (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldRest)
                      (TypedClosureRecipe (map typedExpressionRecipe fieldRest) (typedExpressionRecipe resultInfo))
                      []
                      []
           in saturated (TypedApplyExpr applicationInfo function argument) fieldRest argumentRest
        ([], []) -> function
        _ -> error "constructor fixture must be exactly saturated"

variantInfo :: TypedCoreName -> [TypedType] -> TypedNodeInfo
variantInfo name arguments =
  TypedNodeInfo
    (TypedDataType name arguments)
    (TypedManagedVariantRecipe name arguments)
    []
    []

typeName :: Text -> TypedCoreName
typeName = TypedResolvedName TypedCurrentModule TypedTypeNamespace

constructorName :: Text -> TypedCoreName
constructorName = TypedResolvedName TypedCurrentModule TypedConstructorNamespace

constructorBinder :: Int -> TypedCoreName -> TypedBinderId
constructorBinder constructorIndex name = TypedBinderId (modulePath, [0, constructorIndex], name)

managedExportedOptionSource :: Text
managedExportedOptionSource =
  Text.unlines
    [ "module App::Main (type Option(..)) {",
      "data Option a = None | Some a.",
      "Some 7.",
      "}"
    ]

managedAsConstructorTuplePatternSource, managedTopLevelOrPatternSource, managedListPatternBoundarySource, managedConsPatternBoundarySource, managedTextLiteralPatternBoundarySource, managedNestedOrPatternBoundarySource, managedPatternLambdaBoundarySource :: Text
managedAsConstructorTuplePatternSource =
  Text.unlines
    [ "data Maybe a = Nothing | Just a.",
      "subject = Just (41, True).",
      "case subject {",
      "  | whole @ Just (item, True) -> item",
      "  | Nothing -> 0",
      "  | Just (_, False) -> 1",
      "}."
    ]
managedTopLevelOrPatternSource =
  Text.unlines
    [ "data Choice a = Left a | Right a.",
      "subject = Right 7.",
      "case subject {",
      "  | Left item | Right item -> item",
      "}."
    ]
managedListPatternBoundarySource = "case [1] { | [item] -> item | _ -> 0 }."
managedConsPatternBoundarySource = "case [1, 2] { | [head | tail] -> head | _ -> 0 }."
managedTextLiteralPatternBoundarySource = "case \"managed\" { | \"managed\" -> 1 | _ -> 0 }."
managedNestedOrPatternBoundarySource =
  Text.unlines
    [ "data Choice a = Left a | Right a.",
      "data Holder a = Holder Choice(a).",
      "subject = Holder (Right 7).",
      "case subject { | Holder (Left item | Right item) -> item }."
    ]
managedPatternLambdaBoundarySource =
  Text.unlines
    [ "data Choice a = Left a | Right a.",
      "choose = \\|(Left item) -> item | (Right item) -> item.",
      "choose (Right 7)."
    ]

managedPairBindingSource, managedPairIdentitySource, managedPairDirectTailSource, managedPairRecursiveCaptureSource, managedPairConditionalJoinSource, managedPairScalarCaseJoinSource, managedBoxCaptureSource :: Text
managedPairBindingSource =
  Text.unlines
    [ "pair = (1, \"two\").",
      "pair."
    ]
managedPairIdentitySource =
  Text.unlines
    [ "identity :: (Int, Text) -> (Int, Text).",
      "identity = \\(item) -> item.",
      "identity (1, \"two\")."
    ]
managedPairDirectTailSource =
  Text.unlines
    [ "identity :: (Int, Text) -> (Int, Text).",
      "identity = \\(item) -> item.",
      "forward :: (Int, Text) -> (Int, Text).",
      "forward = \\(item) -> identity item.",
      "forward (1, \"two\")."
    ]
managedPairRecursiveCaptureSource =
  Text.unlines
    [ "pair = (1, \"two\").",
      "loop :: (Int, Text) -> (Int, Text).",
      "loop = \\(item) -> loop pair.",
      "loop pair."
    ]
managedPairConditionalJoinSource =
  Text.unlines
    [ "identity :: (Int, Text) -> (Int, Text).",
      "identity = \\(item) -> item.",
      "identity (if True then (1, \"one\") else (2, \"two\"))."
    ]
managedPairScalarCaseJoinSource =
  Text.unlines
    [ "identity :: (Int, Text) -> (Int, Text).",
      "identity = \\(item) -> item.",
      "identity (case True { | True -> (1, \"one\") | _ -> (2, \"two\") })."
    ]
managedBoxCaptureSource =
  Text.unlines
    [ "data Box = Box (Int, Text).",
      "box = Box (1, \"two\").",
      "capture :: Bool -> Box.",
      "capture = \\(ignored) -> box.",
      "capture True."
    ]

bareConstructorSource, partialConstructorSource, listFieldSource, unresolvedConstructorSource, listConstructionSource, tupleEqualitySource, variantEqualitySource, tuplePatternSource, constructorPatternSource :: Text
bareConstructorSource = Text.unlines ["data Box = Box Int.", "Box."]
partialConstructorSource = Text.unlines ["data Pair a b = Pair a b.", "Pair 1."]
listFieldSource = Text.unlines ["data Box = Box List(Int).", "Box [1]."]
unresolvedConstructorSource = Text.unlines ["data Option a = None | Some a.", "None."]
listConstructionSource = "[1]."
tupleEqualitySource = "(1, \"left\") == (1, \"right\")."
variantEqualitySource = Text.unlines ["data Box = Box Int.", "Box 1 == Box 2."]
tuplePatternSource = "case (1, 2) { | (left, right) -> left }."
constructorPatternSource =
  Text.unlines
    [ "data Option a = None | Some a.",
      "case Some 1 { | Some item -> item | None -> 0 }."
    ]

managedLayoutCatalogProgram :: TypedProgram
managedLayoutCatalogProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedDataStatement leftDeclaration,
          TypedDataStatement rightDeclaration,
          TypedDataStatement catalogOptionDeclaration,
          TypedDataStatement catalogTreeDeclaration,
          TypedDataStatement evenDeclaration,
          TypedDataStatement oddDeclaration,
          expression 7 productExpression,
          expression 8 productExpression,
          expression 9 (monomorphicConstructorCall leftBinder leftConstructor leftInfo [boolInfo] [boolExpr True]),
          expression 10 (monomorphicConstructorCall rightBinder rightConstructor rightInfo [boolInfo] [boolExpr False]),
          expression 11 (constructorCall catalogSomeBinder catalogSomeName optionBoolInfo [boolInfo] [boolExpr True]),
          expression 12 (constructorCall catalogSomeBinder catalogSomeName optionTextInfo [textInfo] [textExpr "value"]),
          expression 13 (constructorCall catalogLeafBinder catalogLeafName catalogTreeIntInfo [intInfo] [intExpr 1]),
          expression 14 (monomorphicConstructorCall zeroBinder zeroName evenInfo [] [])
        ]
        evenInfo
    ]
    modulePath
  where
    expression line value = TypedExpressionStatement (TypedSpan line 1) value
    productInfo =
      TypedNodeInfo
        (TypedTupleType [TypedBoolType, TypedTextType])
        (TypedManagedProductRecipe [TypedBoolRecipe, TypedManagedTextRecipe])
        []
        []
    productExpression = TypedTupleExpr productInfo [boolExpr True, textExpr "product"]

    leftName = typeName "LeftBox"
    leftConstructor = constructorName "LeftBox"
    leftBinder = catalogConstructorBinder 0 0 leftConstructor
    leftDeclaration =
      TypedDataDeclaration
        (TypedSpan 1 1)
        leftName
        []
        [TypedConstructorDeclaration leftBinder leftConstructor [TypedBoolType] [TypedBoolRecipe]]
    leftInfo = variantInfo leftName []

    rightName = typeName "RightBox"
    rightConstructor = constructorName "RightBox"
    rightBinder = catalogConstructorBinder 1 0 rightConstructor
    rightDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        rightName
        []
        [TypedConstructorDeclaration rightBinder rightConstructor [TypedBoolType] [TypedBoolRecipe]]
    rightInfo = variantInfo rightName []

    catalogParameter = TypedTypeParameterId 0
    catalogOptionName = typeName "Option"
    catalogNoneName = constructorName "None"
    catalogSomeName = constructorName "Some"
    catalogNoneBinder = catalogConstructorBinder 2 0 catalogNoneName
    catalogSomeBinder = catalogConstructorBinder 2 1 catalogSomeName
    catalogOptionDeclaration =
      TypedDataDeclaration
        (TypedSpan 3 1)
        catalogOptionName
        [catalogParameter]
        [ TypedConstructorDeclaration catalogNoneBinder catalogNoneName [] [],
          TypedConstructorDeclaration
            catalogSomeBinder
            catalogSomeName
            [TypedTypeParameterType catalogParameter]
            [TypedRepresentationParameterRecipe catalogParameter]
        ]
    optionBoolInfo = variantInfo catalogOptionName [TypedBoolType]
    optionTextInfo = variantInfo catalogOptionName [TypedTextType]

    catalogTreeName = typeName "Tree"
    catalogLeafName = constructorName "Leaf"
    catalogBranchName = constructorName "Branch"
    catalogLeafBinder = catalogConstructorBinder 3 0 catalogLeafName
    catalogBranchBinder = catalogConstructorBinder 3 1 catalogBranchName
    genericTreeType = TypedDataType catalogTreeName [TypedTypeParameterType catalogParameter]
    genericTreeRecipe = TypedManagedVariantRecipe catalogTreeName [TypedTypeParameterType catalogParameter]
    catalogTreeDeclaration =
      TypedDataDeclaration
        (TypedSpan 4 1)
        catalogTreeName
        [catalogParameter]
        [ TypedConstructorDeclaration
            catalogLeafBinder
            catalogLeafName
            [TypedTypeParameterType catalogParameter]
            [TypedRepresentationParameterRecipe catalogParameter],
          TypedConstructorDeclaration
            catalogBranchBinder
            catalogBranchName
            [genericTreeType, genericTreeType]
            [genericTreeRecipe, genericTreeRecipe]
        ]
    catalogTreeIntInfo = variantInfo catalogTreeName [TypedIntType]

    evenName = typeName "Even"
    oddName = typeName "Odd"
    evenConstructorName = constructorName "Even"
    zeroName = constructorName "Zero"
    oddConstructorName = constructorName "Odd"
    evenBinder = catalogConstructorBinder 4 0 evenConstructorName
    zeroBinder = catalogConstructorBinder 4 1 zeroName
    oddBinder = catalogConstructorBinder 5 0 oddConstructorName
    evenDeclaration =
      TypedDataDeclaration
        (TypedSpan 5 1)
        evenName
        []
        [ TypedConstructorDeclaration evenBinder evenConstructorName [TypedDataType oddName []] [TypedManagedVariantRecipe oddName []],
          TypedConstructorDeclaration zeroBinder zeroName [] []
        ]
    oddDeclaration =
      TypedDataDeclaration
        (TypedSpan 6 1)
        oddName
        []
        [TypedConstructorDeclaration oddBinder oddConstructorName [TypedDataType evenName []] [TypedManagedVariantRecipe evenName []]]
    evenInfo = variantInfo evenName []

catalogConstructorBinder :: Int -> Int -> TypedCoreName -> TypedBinderId
catalogConstructorBinder statementIndex constructorIndex name =
  TypedBinderId (modulePath, [statementIndex, constructorIndex], name)

monomorphicConstructorCall :: TypedBinderId -> TypedCoreName -> TypedNodeInfo -> [TypedNodeInfo] -> [TypedExpr] -> TypedExpr
monomorphicConstructorCall = constructorCallWithInstantiations []
