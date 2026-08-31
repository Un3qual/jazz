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
    ),
    ( "managed-nested-constructor-tuple-pattern",
      sourceFixtureNoExports "managed-nested-constructor-tuple-pattern" nestedConstructorTuplePatternSource
    ),
    ( "managed-as-constructor-pattern",
      sourceFixtureNoExports "managed-as-constructor-pattern" asConstructorPatternSource
    ),
    ( "managed-or-constructor-pattern",
      sourceFixtureNoExports "managed-or-constructor-pattern" orConstructorPatternSource
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
    ("managed-tuple-pattern-failure", managedTuplePatternProgram),
    ("managed-constructor-pattern-failure", managedConstructorPatternProgram),
    ("managed-nested-constructor-tuple-pattern", managedNestedConstructorTuplePatternProgram),
    ("managed-as-constructor-pattern", managedAsConstructorPatternProgram),
    ("managed-or-constructor-pattern", managedOrConstructorPatternProgram)
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
    ("managed-box-capture", managedBoxCaptureLoweredProgram),
    ("managed-tuple-pattern-failure", managedTuplePatternLoweredProgram),
    ("managed-constructor-pattern-failure", managedConstructorPatternLoweredProgram),
    ("managed-nested-constructor-tuple-pattern", managedNestedConstructorTuplePatternLoweredProgram),
    ("managed-as-constructor-pattern", managedAsConstructorPatternLoweredProgram),
    ("managed-or-constructor-pattern", managedOrConstructorPatternLoweredProgram)
  ]

managedProductVariantIndependentExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
managedProductVariantIndependentExpectedLoweredPrograms =
  [ ("managed-none", managedNoneProgram, managedNoneLoweredProgram),
    ("managed-tuple-variant", managedTupleVariantProgram, managedTupleVariantLoweredProgram),
    ("managed-text-variant", managedTextVariantProgram, managedTextVariantLoweredProgram),
    ("managed-closure-variant", managedClosureVariantProgram, managedClosureVariantLoweredProgram),
    ("managed-product-variant", managedProductVariantProgram, managedProductVariantLoweredProgram),
    ("managed-nested-variant", managedNestedVariantProgram, managedNestedVariantLoweredProgram),
    ("managed-nested-pattern-fallthrough", managedNestedPatternFallthroughProgram, managedNestedPatternFallthroughLoweredProgram),
    ("managed-recursive-constructor-pattern", managedRecursiveConstructorPatternProgram, managedRecursiveConstructorPatternLoweredProgram),
    ("managed-multiple-literal-tuple-pattern", managedMultipleLiteralTuplePatternProgram, managedMultipleLiteralTuplePatternLoweredProgram),
    ("managed-total-nested-constructor-pattern", managedTotalNestedConstructorPatternProgram, managedTotalNestedConstructorPatternLoweredProgram),
    ("managed-mixed-pattern-fallthrough", managedMixedPatternFallthroughProgram, managedMixedPatternFallthroughLoweredProgram),
    ("managed-as-guard-transport", managedAsGuardTransportProgram, managedAsGuardTransportLoweredProgram)
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

managedTuplePatternLoweredProgram :: LoweredProgram
managedTuplePatternLoweredProgram =
  managedPatternLoweredProgram
    [manifestTupleLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") manifestTupleRepresentation (LoweredConstructProduct manifestTupleLayoutId [intOperand 1, intOperand 2]),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField manifestTupleLayoutId 0 (temporaryOperand 1 manifestTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") int64Representation (LoweredProjectField manifestTupleLayoutId 1 (temporaryOperand 1 manifestTupleRepresentation))
        ]
        (Just (LoweredJump tupleBodyBlockId [temporaryOperand 1 manifestTupleRepresentation, temporaryOperand 2 int64Representation, temporaryOperand 3 int64Representation])),
      LoweredBlock
        tupleBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") manifestTupleRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation,
          LoweredParameter (LoweredParameterId "pattern2") int64Representation
        ]
        []
        (Just (LoweredJump tupleJoinBlockId [blockOperand "pattern1" int64Representation])),
      resultJoinBlock tupleJoinBlockId int64Representation
    ]
    int64Representation
  where
    tupleBodyBlockId = LoweredBlockId "case$s1$0$e1$0$a0$body"
    tupleJoinBlockId = LoweredBlockId "case$s1$0$e1$0$join"

managedConstructorPatternLoweredProgram :: LoweredProgram
managedConstructorPatternLoweredProgram =
  managedPatternLoweredProgram
    [optionLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredConstructVariant optionLayoutId 1 [intOperand 1]),
          LoweredInstruction (LoweredTemporaryId "t2") tagRepresentation (LoweredProjectVariantTag optionLayoutId (temporaryOperand 1 optionRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 1 optionRepresentation) [LoweredSwitchCase 1 someSelectedBlockId [temporaryOperand 1 optionRepresentation], LoweredSwitchCase 0 noneBodyBlockId [temporaryOperand 1 optionRepresentation]] Nothing)),
      LoweredBlock
        someSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        [ LoweredInstruction
            (LoweredTemporaryId "t1")
            int64Representation
            (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "live1" optionRepresentation))
        ]
        (Just (LoweredJump someBodyBlockId [blockOperand "live1" optionRepresentation, temporaryOperand 1 int64Representation])),
      LoweredBlock
        someBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation, LoweredParameter (LoweredParameterId "pattern1") int64Representation]
        []
        (Just (LoweredJump optionJoinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        noneBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        []
        (Just (LoweredJump optionJoinBlockId [intOperand 0])),
      resultJoinBlock optionJoinBlockId int64Representation
    ]
    int64Representation
  where
    someSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$selected"
    someBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    noneBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    optionJoinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedNestedConstructorTuplePatternLoweredProgram :: LoweredProgram
managedNestedConstructorTuplePatternLoweredProgram =
  managedPatternLoweredProgram
    [textLayout, optionPairLayout, tupleLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") textRepresentation (LoweredConstructText textLayoutId "one"),
          LoweredInstruction (LoweredTemporaryId "t2") tupleRepresentation (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t3") optionPairRepresentation (LoweredConstructVariant optionPairLayoutId 1 [temporaryOperand 2 tupleRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t4") tagRepresentation (LoweredProjectVariantTag optionPairLayoutId (temporaryOperand 3 optionPairRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 3 optionPairRepresentation) [LoweredSwitchCase 1 nestedSelectedBlockId [temporaryOperand 3 optionPairRepresentation], LoweredSwitchCase 0 nestedNoneBlockId [temporaryOperand 3 optionPairRepresentation]] Nothing)),
      LoweredBlock
        nestedSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        [ LoweredInstruction (LoweredTemporaryId "t1") tupleRepresentation (LoweredProjectVariantField optionPairLayoutId 1 0 (blockOperand "live1" optionPairRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField tupleLayoutId 0 (temporaryOperand 1 tupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") textRepresentation (LoweredProjectField tupleLayoutId 1 (temporaryOperand 1 tupleRepresentation))
        ]
        (Just (LoweredJump nestedBodyBlockId [blockOperand "live1" optionPairRepresentation, temporaryOperand 2 int64Representation, temporaryOperand 3 textRepresentation])),
      LoweredBlock
        nestedBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionPairRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation,
          LoweredParameter (LoweredParameterId "pattern2") textRepresentation
        ]
        []
        (Just (LoweredJump nestedJoinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        nestedNoneBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        []
        (Just (LoweredJump nestedJoinBlockId [intOperand 0])),
      resultJoinBlock nestedJoinBlockId int64Representation
    ]
    int64Representation
  where
    nestedSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$selected"
    nestedBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    nestedNoneBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    nestedJoinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedAsConstructorPatternLoweredProgram :: LoweredProgram
managedAsConstructorPatternLoweredProgram =
  managedPatternLoweredProgram
    [optionLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredConstructVariant optionLayoutId 1 [intOperand 1]),
          LoweredInstruction (LoweredTemporaryId "t2") tagRepresentation (LoweredProjectVariantTag optionLayoutId (temporaryOperand 1 optionRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 1 optionRepresentation) [LoweredSwitchCase 1 someSelectedBlockId [temporaryOperand 1 optionRepresentation], LoweredSwitchCase 0 noneBodyBlockId [temporaryOperand 1 optionRepresentation]] Nothing)),
      LoweredBlock
        someSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        [LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "live1" optionRepresentation))]
        (Just (LoweredJump someBodyBlockId [blockOperand "live1" optionRepresentation, blockOperand "live1" optionRepresentation, temporaryOperand 1 int64Representation])),
      LoweredBlock
        someBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern2") int64Representation
        ]
        []
        (Just (LoweredJump optionJoinBlockId [blockOperand "pattern2" int64Representation])),
      LoweredBlock
        noneBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        []
        (Just (LoweredJump optionJoinBlockId [intOperand 0])),
      resultJoinBlock optionJoinBlockId int64Representation
    ]
    int64Representation
  where
    someSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$selected"
    someBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    noneBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    optionJoinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedOrConstructorPatternLoweredProgram :: LoweredProgram
managedOrConstructorPatternLoweredProgram =
  managedPatternLoweredProgram
    [choiceLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [LoweredInstruction (LoweredTemporaryId "t1") choiceRepresentation (LoweredConstructVariant choiceLayoutId 1 [intOperand 2, intOperand 20])]
        (Just (LoweredJump leftSelectedBlockId [temporaryOperand 1 choiceRepresentation])),
      alternativeBlock leftSelectedBlockId leftMatchBlockId rightSelectedBlockId 0,
      matchedAlternativeBlock leftMatchBlockId rightSelectedBlockId 0 1,
      alternativeBlock rightSelectedBlockId rightMatchBlockId fallbackBodyBlockId 1,
      matchedAlternativeBlock rightMatchBlockId fallbackBodyBlockId 1 2,
      LoweredBlock
        bodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") choiceRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation
        ]
        []
        (Just (LoweredJump joinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        fallbackBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") choiceRepresentation]
        []
        (Just (LoweredJump joinBlockId [intOperand 0])),
      resultJoinBlock joinBlockId int64Representation
    ]
    int64Representation
  where
    leftSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$alternative0"
    leftMatchBlockId = LoweredBlockId "case$s1$1$e1$0$a0$alternative0$match1"
    rightSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$alternative1"
    rightMatchBlockId = LoweredBlockId "case$s1$1$e1$0$a0$alternative1$match1"
    bodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    fallbackBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    joinBlockId = LoweredBlockId "case$s1$1$e1$0$join"
    alternativeBlock blockId matchBlockId failureBlockId tag =
      LoweredBlock
        blockId
        [LoweredParameter (LoweredParameterId "live1") choiceRepresentation]
        [LoweredInstruction (LoweredTemporaryId "t1") tagRepresentation (LoweredProjectVariantTag choiceLayoutId (blockOperand "live1" choiceRepresentation))]
        ( Just
            ( LoweredSwitch
                (blockOperand "live1" choiceRepresentation)
                [LoweredSwitchCase tag matchBlockId [blockOperand "live1" choiceRepresentation, blockOperand "live1" choiceRepresentation]]
                (Just (LoweredSwitchDefault failureBlockId [blockOperand "live1" choiceRepresentation]))
            )
        )
    matchedAlternativeBlock blockId failureBlockId tag literal =
      LoweredBlock
        blockId
        [ LoweredParameter (LoweredParameterId "live1") choiceRepresentation,
          LoweredParameter (LoweredParameterId "match1") choiceRepresentation
        ]
        [ LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField choiceLayoutId tag 0 (blockOperand "match1" choiceRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectVariantField choiceLayoutId tag 1 (blockOperand "match1" choiceRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [temporaryOperand 1 int64Representation, intOperand literal])
        ]
        (Just (LoweredBranch (temporaryOperand 3 LoweredBoolRepresentation) bodyBlockId [blockOperand "live1" choiceRepresentation, temporaryOperand 2 int64Representation] failureBlockId [blockOperand "live1" choiceRepresentation]))

managedNestedPatternFallthroughLoweredProgram :: LoweredProgram
managedNestedPatternFallthroughLoweredProgram =
  managedPatternLoweredProgram
    [textLayout, optionPairLayout, tupleLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") textRepresentation (LoweredConstructText textLayoutId "one"),
          LoweredInstruction (LoweredTemporaryId "t2") tupleRepresentation (LoweredConstructProduct tupleLayoutId [intOperand 1, temporaryOperand 1 textRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t3") optionPairRepresentation (LoweredConstructVariant optionPairLayoutId 1 [temporaryOperand 2 tupleRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t4") tagRepresentation (LoweredProjectVariantTag optionPairLayoutId (temporaryOperand 3 optionPairRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 3 optionPairRepresentation) [LoweredSwitchCase 1 firstSelectedBlockId [temporaryOperand 3 optionPairRepresentation], LoweredSwitchCase 0 noneBodyBlockId [temporaryOperand 3 optionPairRepresentation]] Nothing)),
      LoweredBlock
        firstSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        [ LoweredInstruction (LoweredTemporaryId "t1") tupleRepresentation (LoweredProjectVariantField optionPairLayoutId 1 0 (blockOperand "live1" optionPairRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField tupleLayoutId 0 (temporaryOperand 1 tupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") textRepresentation (LoweredProjectField tupleLayoutId 1 (temporaryOperand 1 tupleRepresentation)),
          LoweredInstruction
            (LoweredTemporaryId "t4")
            LoweredBoolRepresentation
            (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [temporaryOperand 2 int64Representation, intOperand 0])
        ]
        (Just (LoweredBranch (temporaryOperand 4 LoweredBoolRepresentation) firstBodyBlockId [blockOperand "live1" optionPairRepresentation] fallbackSelectedBlockId [blockOperand "live1" optionPairRepresentation])),
      LoweredBlock
        firstBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        []
        (Just (LoweredJump nestedFallthroughJoinBlockId [intOperand 99])),
      LoweredBlock
        fallbackSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        [ LoweredInstruction (LoweredTemporaryId "t1") tupleRepresentation (LoweredProjectVariantField optionPairLayoutId 1 0 (blockOperand "live1" optionPairRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField tupleLayoutId 0 (temporaryOperand 1 tupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") textRepresentation (LoweredProjectField tupleLayoutId 1 (temporaryOperand 1 tupleRepresentation))
        ]
        (Just (LoweredJump fallbackBodyBlockId [blockOperand "live1" optionPairRepresentation, temporaryOperand 2 int64Representation])),
      LoweredBlock
        fallbackBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation, LoweredParameter (LoweredParameterId "pattern1") int64Representation]
        []
        (Just (LoweredJump nestedFallthroughJoinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        noneBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionPairRepresentation]
        []
        (Just (LoweredJump nestedFallthroughJoinBlockId [intOperand 0])),
      resultJoinBlock nestedFallthroughJoinBlockId int64Representation
    ]
    int64Representation
  where
    firstSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$selected"
    firstBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    fallbackSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a1$selected"
    fallbackBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    noneBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a2$body"
    nestedFallthroughJoinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedRecursiveConstructorPatternLoweredProgram :: LoweredProgram
managedRecursiveConstructorPatternLoweredProgram =
  managedPatternLoweredProgram
    [optionOptionLayout, optionLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredConstructVariant optionLayoutId 1 [intOperand 1]),
          LoweredInstruction (LoweredTemporaryId "t2") optionOptionRepresentation (LoweredConstructVariant optionOptionLayoutId 1 [temporaryOperand 1 optionRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t3") tagRepresentation (LoweredProjectVariantTag optionOptionLayoutId (temporaryOperand 2 optionOptionRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 2 optionOptionRepresentation) [LoweredSwitchCase 1 outerSelectedBlockId [temporaryOperand 2 optionOptionRepresentation], LoweredSwitchCase 0 noneBodyBlockId [temporaryOperand 2 optionOptionRepresentation]] Nothing)),
      LoweredBlock
        outerSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation]
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredProjectVariantField optionOptionLayoutId 1 0 (blockOperand "live1" optionOptionRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") tagRepresentation (LoweredProjectVariantTag optionLayoutId (temporaryOperand 1 optionRepresentation))
        ]
        ( Just
            ( LoweredSwitch
                (temporaryOperand 1 optionRepresentation)
                [LoweredSwitchCase 1 nestedSelectedBlockId [blockOperand "live1" optionOptionRepresentation, temporaryOperand 1 optionRepresentation]]
                (Just (LoweredSwitchDefault fallbackSelectedBlockId [blockOperand "live1" optionOptionRepresentation]))
            )
        ),
      LoweredBlock
        nestedSelectedBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation,
          LoweredParameter (LoweredParameterId "match1") optionRepresentation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "match1" optionRepresentation))]
        (Just (LoweredJump firstBodyBlockId [blockOperand "live1" optionOptionRepresentation, temporaryOperand 1 int64Representation])),
      LoweredBlock
        firstBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation
        ]
        []
        (Just (LoweredJump recursiveJoinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        fallbackSelectedBlockId
        [LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation]
        [LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredProjectVariantField optionOptionLayoutId 1 0 (blockOperand "live1" optionOptionRepresentation))]
        (Just (LoweredJump fallbackBodyBlockId [blockOperand "live1" optionOptionRepresentation])),
      LoweredBlock
        fallbackBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation]
        []
        (Just (LoweredJump recursiveJoinBlockId [intOperand 0])),
      LoweredBlock
        noneBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionOptionRepresentation]
        []
        (Just (LoweredJump recursiveJoinBlockId [intOperand 0])),
      resultJoinBlock recursiveJoinBlockId int64Representation
    ]
    int64Representation
  where
    outerSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$selected"
    nestedSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$match1"
    firstBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    fallbackSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a1$selected"
    fallbackBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    noneBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a2$body"
    recursiveJoinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedMultipleLiteralTuplePatternLoweredProgram :: LoweredProgram
managedMultipleLiteralTuplePatternLoweredProgram =
  managedPatternLoweredProgram
    [manifestTupleLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") manifestTupleRepresentation (LoweredConstructProduct manifestTupleLayoutId [intOperand 1, intOperand 2]),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField manifestTupleLayoutId 0 (temporaryOperand 1 manifestTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t3") int64Representation (LoweredProjectField manifestTupleLayoutId 1 (temporaryOperand 1 manifestTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t4") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [temporaryOperand 2 int64Representation, intOperand 0])
        ]
        (Just (LoweredBranch (temporaryOperand 4 LoweredBoolRepresentation) firstMatchBlockId [temporaryOperand 1 manifestTupleRepresentation, temporaryOperand 3 int64Representation] fallbackTestBlockId [temporaryOperand 1 manifestTupleRepresentation])),
      LoweredBlock
        firstMatchBlockId
        [ LoweredParameter (LoweredParameterId "live1") manifestTupleRepresentation,
          LoweredParameter (LoweredParameterId "match1") int64Representation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [blockOperand "match1" int64Representation, intOperand 0])]
        (Just (LoweredBranch (temporaryOperand 1 LoweredBoolRepresentation) firstBodyBlockId [blockOperand "live1" manifestTupleRepresentation] fallbackTestBlockId [blockOperand "live1" manifestTupleRepresentation])),
      LoweredBlock
        firstBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") manifestTupleRepresentation]
        []
        (Just (LoweredJump tupleLiteralJoinBlockId [intOperand 99])),
      LoweredBlock
        fallbackTestBlockId
        [LoweredParameter (LoweredParameterId "live1") manifestTupleRepresentation]
        [ LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectField manifestTupleLayoutId 0 (blockOperand "live1" manifestTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") int64Representation (LoweredProjectField manifestTupleLayoutId 1 (blockOperand "live1" manifestTupleRepresentation))
        ]
        (Just (LoweredJump fallbackBodyBlockId [blockOperand "live1" manifestTupleRepresentation, temporaryOperand 1 int64Representation, temporaryOperand 2 int64Representation])),
      LoweredBlock
        fallbackBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") manifestTupleRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation,
          LoweredParameter (LoweredParameterId "pattern2") int64Representation
        ]
        []
        (Just (LoweredJump tupleLiteralJoinBlockId [blockOperand "pattern1" int64Representation])),
      resultJoinBlock tupleLiteralJoinBlockId int64Representation
    ]
    int64Representation
  where
    firstMatchBlockId = LoweredBlockId "case$s1$0$e1$0$a0$match1"
    firstBodyBlockId = LoweredBlockId "case$s1$0$e1$0$a0$body"
    fallbackTestBlockId = LoweredBlockId "case$s1$0$e1$0$a1$test"
    fallbackBodyBlockId = LoweredBlockId "case$s1$0$e1$0$a1$body"
    tupleLiteralJoinBlockId = LoweredBlockId "case$s1$0$e1$0$join"

managedTotalNestedConstructorPatternLoweredProgram :: LoweredProgram
managedTotalNestedConstructorPatternLoweredProgram =
  managedPatternLoweredProgram
    [textLayout, singleConstructorTupleLayout, textBoxLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") textRepresentation (LoweredConstructText textLayoutId "inside"),
          LoweredInstruction (LoweredTemporaryId "t2") textBoxRepresentation (LoweredConstructVariant textBoxLayoutId 0 [temporaryOperand 1 textRepresentation]),
          LoweredInstruction (LoweredTemporaryId "t3") singleConstructorTupleRepresentation (LoweredConstructProduct singleConstructorTupleLayoutId [temporaryOperand 2 textBoxRepresentation, intOperand 2]),
          LoweredInstruction (LoweredTemporaryId "t4") textBoxRepresentation (LoweredProjectField singleConstructorTupleLayoutId 0 (temporaryOperand 3 singleConstructorTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t5") int64Representation (LoweredProjectField singleConstructorTupleLayoutId 1 (temporaryOperand 3 singleConstructorTupleRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t6") tagRepresentation (LoweredProjectVariantTag textBoxLayoutId (temporaryOperand 4 textBoxRepresentation))
        ]
        ( Just
            ( LoweredSwitch
                (temporaryOperand 4 textBoxRepresentation)
                [LoweredSwitchCase 0 nestedSelectedBlockId [temporaryOperand 3 singleConstructorTupleRepresentation, temporaryOperand 4 textBoxRepresentation, temporaryOperand 5 int64Representation]]
                Nothing
            )
        ),
      LoweredBlock
        nestedSelectedBlockId
        [ LoweredParameter (LoweredParameterId "live1") singleConstructorTupleRepresentation,
          LoweredParameter (LoweredParameterId "match1") textBoxRepresentation,
          LoweredParameter (LoweredParameterId "match2") int64Representation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") textRepresentation (LoweredProjectVariantField textBoxLayoutId 0 0 (blockOperand "match1" textBoxRepresentation))]
        (Just (LoweredJump bodyBlockId [blockOperand "live1" singleConstructorTupleRepresentation, temporaryOperand 1 textRepresentation])),
      LoweredBlock
        bodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") singleConstructorTupleRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") textRepresentation
        ]
        []
        (Just (LoweredJump joinBlockId [blockOperand "pattern1" textRepresentation])),
      resultJoinBlock joinBlockId textRepresentation
    ]
    textRepresentation
  where
    nestedSelectedBlockId = LoweredBlockId "case$s1$1$e1$0$a0$match1"
    bodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    joinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedMixedPatternFallthroughLoweredProgram :: LoweredProgram
managedMixedPatternFallthroughLoweredProgram =
  managedPatternLoweredProgram
    [optionLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredConstructVariant optionLayoutId 1 [intOperand 1]),
          LoweredInstruction (LoweredTemporaryId "t2") tagRepresentation (LoweredProjectVariantTag optionLayoutId (temporaryOperand 1 optionRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 1 optionRepresentation) [LoweredSwitchCase 1 firstMatchBlockId [temporaryOperand 1 optionRepresentation, temporaryOperand 1 optionRepresentation]] (Just (LoweredSwitchDefault secondTestBlockId [temporaryOperand 1 optionRepresentation])))),
      LoweredBlock
        firstMatchBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "match1") optionRepresentation
        ]
        [ LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "match1" optionRepresentation)),
          LoweredInstruction (LoweredTemporaryId "t2") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [temporaryOperand 1 int64Representation, intOperand 0])
        ]
        (Just (LoweredBranch (temporaryOperand 2 LoweredBoolRepresentation) firstBodyBlockId [blockOperand "live1" optionRepresentation] secondTestBlockId [blockOperand "live1" optionRepresentation])),
      bodyBlock firstBodyBlockId (intOperand 10),
      LoweredBlock
        secondTestBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        [LoweredInstruction (LoweredTemporaryId "t1") tagRepresentation (LoweredProjectVariantTag optionLayoutId (blockOperand "live1" optionRepresentation))]
        (Just (LoweredSwitch (blockOperand "live1" optionRepresentation) [LoweredSwitchCase 1 secondMatchBlockId [blockOperand "live1" optionRepresentation, blockOperand "live1" optionRepresentation]] (Just (LoweredSwitchDefault noneTestBlockId [blockOperand "live1" optionRepresentation])))),
      LoweredBlock
        secondMatchBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "match1") optionRepresentation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "match1" optionRepresentation))]
        (Just (LoweredJump secondGuardBlockId [blockOperand "live1" optionRepresentation, temporaryOperand 1 int64Representation])),
      LoweredBlock
        secondGuardBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [blockOperand "pattern1" int64Representation, intOperand 0])]
        (Just (LoweredBranch (temporaryOperand 1 LoweredBoolRepresentation) secondBodyBlockId [blockOperand "live1" optionRepresentation, blockOperand "pattern1" int64Representation] noneTestBlockId [blockOperand "live1" optionRepresentation])),
      LoweredBlock
        secondBodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") int64Representation
        ]
        []
        (Just (LoweredJump joinBlockId [blockOperand "pattern1" int64Representation])),
      LoweredBlock
        noneTestBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        [LoweredInstruction (LoweredTemporaryId "t1") tagRepresentation (LoweredProjectVariantTag optionLayoutId (blockOperand "live1" optionRepresentation))]
        (Just (LoweredSwitch (blockOperand "live1" optionRepresentation) [LoweredSwitchCase 0 noneMatchBlockId [blockOperand "live1" optionRepresentation, blockOperand "live1" optionRepresentation]] (Just (LoweredSwitchDefault catchAllBodyBlockId [blockOperand "live1" optionRepresentation])))),
      LoweredBlock
        noneMatchBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "match1") optionRepresentation
        ]
        []
        (Just (LoweredJump noneBodyBlockId [blockOperand "live1" optionRepresentation])),
      bodyBlock noneBodyBlockId (intOperand 30),
      bodyBlock catchAllBodyBlockId (intOperand 40),
      resultJoinBlock joinBlockId int64Representation
    ]
    int64Representation
  where
    firstMatchBlockId = LoweredBlockId "case$s1$1$e1$0$a0$match1"
    firstBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    secondTestBlockId = LoweredBlockId "case$s1$1$e1$0$a1$test"
    secondMatchBlockId = LoweredBlockId "case$s1$1$e1$0$a1$match1"
    secondGuardBlockId = LoweredBlockId "case$s1$1$e1$0$a1$guard"
    secondBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    noneTestBlockId = LoweredBlockId "case$s1$1$e1$0$a2$test"
    noneMatchBlockId = LoweredBlockId "case$s1$1$e1$0$a2$match1"
    noneBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a2$body"
    catchAllBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a3$body"
    joinBlockId = LoweredBlockId "case$s1$1$e1$0$join"
    bodyBlock blockId result =
      LoweredBlock
        blockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        []
        (Just (LoweredJump joinBlockId [result]))

managedAsGuardTransportLoweredProgram :: LoweredProgram
managedAsGuardTransportLoweredProgram =
  managedPatternLoweredProgram
    [optionLayout]
    [ LoweredBlock
        (LoweredBlockId "entry")
        []
        [ LoweredInstruction (LoweredTemporaryId "t1") optionRepresentation (LoweredConstructVariant optionLayoutId 1 [intOperand 1]),
          LoweredInstruction (LoweredTemporaryId "t2") tagRepresentation (LoweredProjectVariantTag optionLayoutId (temporaryOperand 1 optionRepresentation))
        ]
        (Just (LoweredSwitch (temporaryOperand 1 optionRepresentation) [LoweredSwitchCase 1 matchBlockId [temporaryOperand 1 optionRepresentation, temporaryOperand 1 optionRepresentation, temporaryOperand 1 optionRepresentation]] (Just (LoweredSwitchDefault fallbackBodyBlockId [temporaryOperand 1 optionRepresentation])))),
      LoweredBlock
        matchBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pending1") optionRepresentation,
          LoweredParameter (LoweredParameterId "match1") optionRepresentation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") int64Representation (LoweredProjectVariantField optionLayoutId 1 0 (blockOperand "match1" optionRepresentation))]
        (Just (LoweredJump guardBlockId [blockOperand "live1" optionRepresentation, blockOperand "pending1" optionRepresentation, temporaryOperand 1 int64Representation])),
      LoweredBlock
        guardBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern2") int64Representation
        ]
        [LoweredInstruction (LoweredTemporaryId "t1") LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredComparisonPrimitive LoweredEqual) [blockOperand "pattern2" int64Representation, intOperand 1])]
        (Just (LoweredBranch (temporaryOperand 1 LoweredBoolRepresentation) bodyBlockId [blockOperand "live1" optionRepresentation, blockOperand "pattern1" optionRepresentation, blockOperand "pattern2" int64Representation] fallbackBodyBlockId [blockOperand "live1" optionRepresentation])),
      LoweredBlock
        bodyBlockId
        [ LoweredParameter (LoweredParameterId "live1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern1") optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern2") int64Representation
        ]
        []
        (Just (LoweredJump joinBlockId [blockOperand "pattern1" optionRepresentation])),
      LoweredBlock
        fallbackBodyBlockId
        [LoweredParameter (LoweredParameterId "live1") optionRepresentation]
        []
        (Just (LoweredJump joinBlockId [blockOperand "live1" optionRepresentation])),
      resultJoinBlock joinBlockId optionRepresentation
    ]
    optionRepresentation
  where
    matchBlockId = LoweredBlockId "case$s1$1$e1$0$a0$match1"
    guardBlockId = LoweredBlockId "case$s1$1$e1$0$a0$guard"
    bodyBlockId = LoweredBlockId "case$s1$1$e1$0$a0$body"
    fallbackBodyBlockId = LoweredBlockId "case$s1$1$e1$0$a1$body"
    joinBlockId = LoweredBlockId "case$s1$1$e1$0$join"

managedPatternLoweredProgram :: [LoweredLayout] -> [LoweredBlock] -> LoweredRepresentation -> LoweredProgram
managedPatternLoweredProgram layouts blocks resultRepresentation =
  LoweredProgram
    (LoweredIRVersion 1)
    layouts
    []
    [LoweredFunction (LoweredFunctionId "App::Main::$entry") Nothing [] resultRepresentation blocks (LoweredBlockId "entry")]
    (LoweredFunctionId "App::Main::$entry")

resultJoinBlock :: LoweredBlockId -> LoweredRepresentation -> LoweredBlock
resultJoinBlock blockId representation =
  LoweredBlock blockId [LoweredParameter (LoweredParameterId "result") representation] [] (Just (LoweredReturn (blockOperand "result" representation)))

blockOperand :: Text -> LoweredRepresentation -> LoweredOperand
blockOperand name = LoweredBlockParameterOperand (LoweredParameterId name)

tagRepresentation :: LoweredRepresentation
tagRepresentation = LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64

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

textLayoutId, tupleLayoutId, optionLayoutId, optionOptionLayoutId, optionPairLayoutId, choiceLayoutId, treeLayoutId, tupleVariantLayoutId, textBoxLayoutId, closureBoxLayoutId, closureEnvironmentLayoutId, productBoxLayoutId, outerLayoutId, captureBoxLayoutId, captureEnvironmentLayoutId, recursivePairEnvironmentLayoutId, manifestTupleLayoutId, manifestDataLayoutId, singleConstructorTupleLayoutId :: LoweredLayoutId
textLayoutId = LoweredLayoutId "jazz.layout.text.v1"
tupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$4:text"
optionLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$3:int"
optionOptionLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$51:data$module2$3:App$4:Main$name$6:Option$args1$3:int"
optionPairLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$19:tuple2$3:int$4:text"
choiceLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Choice$args0"
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
singleConstructorTupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$49:variant$module2$3:App$4:Main$name$7:TextBox$args0$8:signed64"

textRepresentation, tupleRepresentation, optionRepresentation, optionOptionRepresentation, optionPairRepresentation, choiceRepresentation, treeRepresentation, tupleVariantRepresentation, textBoxRepresentation, closureBoxRepresentation, closureEnvironmentRepresentation, productBoxRepresentation, outerRepresentation, captureBoxRepresentation, captureEnvironmentRepresentation, recursivePairEnvironmentRepresentation, manifestTupleRepresentation, manifestDataRepresentation, singleConstructorTupleRepresentation :: LoweredRepresentation
textRepresentation = LoweredManagedReferenceRepresentation textLayoutId
tupleRepresentation = LoweredManagedReferenceRepresentation tupleLayoutId
optionRepresentation = LoweredManagedReferenceRepresentation optionLayoutId
optionOptionRepresentation = LoweredManagedReferenceRepresentation optionOptionLayoutId
optionPairRepresentation = LoweredManagedReferenceRepresentation optionPairLayoutId
choiceRepresentation = LoweredManagedReferenceRepresentation choiceLayoutId
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
singleConstructorTupleRepresentation = LoweredManagedReferenceRepresentation singleConstructorTupleLayoutId

textLayout, tupleLayout, optionLayout, optionOptionLayout, optionPairLayout, choiceLayout, treeLayout, tupleVariantLayout, textBoxLayout, closureBoxLayout, closureEnvironmentLayout, productBoxLayout, outerLayout, captureBoxLayout, captureEnvironmentLayout, recursivePairEnvironmentLayout, manifestTupleLayout, manifestDataLayout, singleConstructorTupleLayout :: LoweredLayout
textLayout = LoweredLayout textLayoutId LoweredTextLayout
tupleLayout = LoweredLayout tupleLayoutId (LoweredProductLayout [int64Representation, textRepresentation])
optionLayout =
  LoweredLayout
    optionLayoutId
    (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [int64Representation]])
optionOptionLayout =
  LoweredLayout
    optionOptionLayoutId
    (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [optionRepresentation]])
optionPairLayout =
  LoweredLayout
    optionPairLayoutId
    (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [tupleRepresentation]])
choiceLayout =
  LoweredLayout
    choiceLayoutId
    (LoweredVariantLayouts [LoweredVariantLayout 0 [int64Representation, int64Representation], LoweredVariantLayout 1 [int64Representation, int64Representation]])
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
singleConstructorTupleLayout = LoweredLayout singleConstructorTupleLayoutId (LoweredProductLayout [textBoxRepresentation, int64Representation])

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

managedTuplePatternProgram :: TypedProgram
managedTuplePatternProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            intInfo
            (TypedTupleExpr tupleInfo [intExpr 1, intExpr 2])
            [TypedCaseArm tuplePattern Nothing (boundVariableExpr leftName intInfo leftBinder)]
        )
    ]
    intInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedIntType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64])
        []
        []
    leftName = valueName "left"
    rightName = valueName "right"
    leftBinder = patternBinder [0, 0, 0] leftName
    rightBinder = patternBinder [0, 0, 1] rightName
    tuplePattern =
      TypedTuplePattern
        tupleInfo
        [ TypedVariablePattern intInfo leftBinder leftName,
          TypedVariablePattern intInfo rightBinder rightName
        ]

managedConstructorPatternProgram :: TypedProgram
managedConstructorPatternProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 1])
            [ TypedCaseArm
                (TypedConstructorPattern optionIntInfo someName [TypedVariablePattern intInfo itemBinder itemName])
                Nothing
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm (TypedConstructorPattern optionIntInfo noneName []) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    itemName = valueName "item"
    itemBinder = patternBinder [1, 0, 0] itemName

managedNestedConstructorTuplePatternProgram :: TypedProgram
managedNestedConstructorTuplePatternProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionTupleInfo [managedPairInfo] [managedPairExpressionWith 1 "one"])
            [ TypedCaseArm
                ( TypedConstructorPattern
                    optionTupleInfo
                    someName
                    [ TypedTuplePattern
                        managedPairInfo
                        [ TypedVariablePattern intInfo numberBinder numberName,
                          TypedVariablePattern textInfo labelBinder labelName
                        ]
                    ]
                )
                Nothing
                (boundVariableExpr numberName intInfo numberBinder),
              TypedCaseArm (TypedConstructorPattern optionTupleInfo noneName []) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    optionTupleInfo = variantInfo optionName [typedExpressionType managedPairInfo]
    numberName = valueName "number"
    labelName = valueName "label"
    numberBinder = patternBinder [1, 0, 0, 0] numberName
    labelBinder = patternBinder [1, 0, 0, 1] labelName

managedNestedPatternFallthroughProgram :: TypedProgram
managedNestedPatternFallthroughProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionTupleInfo [managedPairInfo] [managedPairExpressionWith 1 "one"])
            [ TypedCaseArm
                ( TypedConstructorPattern
                    optionTupleInfo
                    someName
                    [TypedTuplePattern managedPairInfo [TypedLiteralPattern intInfo (TypedIntegerLiteral "0"), TypedWildcardPattern textInfo]]
                )
                Nothing
                (intExpr 99),
              TypedCaseArm
                ( TypedConstructorPattern
                    optionTupleInfo
                    someName
                    [TypedTuplePattern managedPairInfo [TypedVariablePattern intInfo fallbackBinder fallbackName, TypedWildcardPattern textInfo]]
                )
                Nothing
                (boundVariableExpr fallbackName intInfo fallbackBinder),
              TypedCaseArm (TypedConstructorPattern optionTupleInfo noneName []) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    optionTupleInfo = variantInfo optionName [typedExpressionType managedPairInfo]
    fallbackName = valueName "fallback"
    fallbackBinder = patternBinder [1, 1, 0, 0] fallbackName

managedRecursiveConstructorPatternProgram :: TypedProgram
managedRecursiveConstructorPatternProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionOptionInfo [optionIntInfo] [constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 1]])
            [ TypedCaseArm
                ( TypedConstructorPattern
                    optionOptionInfo
                    someName
                    [TypedConstructorPattern optionIntInfo someName [TypedVariablePattern intInfo itemBinder itemName]]
                )
                Nothing
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm
                (TypedConstructorPattern optionOptionInfo someName [TypedWildcardPattern optionIntInfo])
                Nothing
                (intExpr 0),
              TypedCaseArm (TypedConstructorPattern optionOptionInfo noneName []) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    optionOptionInfo = variantInfo optionName [typedExpressionType optionIntInfo]
    itemName = valueName "item"
    itemBinder = patternBinder [1, 0, 0, 0] itemName

managedMultipleLiteralTuplePatternProgram :: TypedProgram
managedMultipleLiteralTuplePatternProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            intInfo
            (TypedTupleExpr tupleInfo [intExpr 1, intExpr 2])
            [ TypedCaseArm
                (TypedTuplePattern tupleInfo [literalPattern 0, literalPattern 0])
                Nothing
                (intExpr 99),
              TypedCaseArm
                (TypedTuplePattern tupleInfo [TypedVariablePattern intInfo leftBinder leftName, TypedVariablePattern intInfo rightBinder rightName])
                Nothing
                (boundVariableExpr leftName intInfo leftBinder)
            ]
        )
    ]
    intInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedIntType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64])
        []
        []
    literalPattern :: Int -> TypedPattern
    literalPattern value = TypedLiteralPattern intInfo (TypedIntegerLiteral (Text.pack (show value)))
    leftName = valueName "left"
    rightName = valueName "right"
    leftBinder = patternBinder [0, 1, 0] leftName
    rightBinder = patternBinder [0, 1, 1] rightName

managedTotalNestedConstructorPatternProgram :: TypedProgram
managedTotalNestedConstructorPatternProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            textInfo
            ( TypedTupleExpr
                tupleInfo
                [monomorphicConstructorCall binder constructor boxInfo [textInfo] [textExpr "inside"], intExpr 2]
            )
            [ TypedCaseArm
                ( TypedTuplePattern
                    tupleInfo
                    [ TypedConstructorPattern boxInfo constructor [TypedVariablePattern textInfo itemBinder itemName],
                      TypedWildcardPattern intInfo
                    ]
                )
                Nothing
                (boundVariableExpr itemName textInfo itemBinder)
            ]
        )
    ]
    textInfo
  where
    name = typeName "TextBox"
    constructor = constructorName "TextBox"
    binder = constructorBinder 0 constructor
    boxInfo = variantInfo name []
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [typedExpressionType boxInfo, TypedIntType])
        (TypedManagedProductRecipe [typedExpressionRecipe boxInfo, TypedSignedIntegerRecipe 64])
        []
        []
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        name
        []
        [TypedConstructorDeclaration binder constructor [TypedTextType] [TypedManagedTextRecipe]]
    itemName = valueName "item"
    itemBinder = patternBinder [1, 0, 0, 0] itemName

managedMixedPatternFallthroughProgram :: TypedProgram
managedMixedPatternFallthroughProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 1])
            [ TypedCaseArm
                (TypedConstructorPattern optionIntInfo someName [TypedLiteralPattern intInfo (TypedIntegerLiteral "0")])
                Nothing
                (intExpr 10),
              TypedCaseArm
                (TypedConstructorPattern optionIntInfo someName [TypedVariablePattern intInfo itemBinder itemName])
                ( Just
                    ( TypedBinaryExpr
                        boolInfo
                        (TypedBuiltinOperator "==")
                        (boundVariableExpr itemName intInfo itemBinder)
                        (intExpr 0)
                    )
                )
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm (TypedConstructorPattern optionIntInfo noneName []) Nothing (intExpr 30),
              TypedCaseArm (TypedWildcardPattern optionIntInfo) Nothing (intExpr 40)
            ]
        )
    ]
    intInfo
  where
    itemName = valueName "item"
    itemBinder = patternBinder [1, 1, 0] itemName

managedAsGuardTransportProgram :: TypedProgram
managedAsGuardTransportProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            optionIntInfo
            (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 1])
            [ TypedCaseArm
                ( TypedAsPattern
                    optionIntInfo
                    wholeBinder
                    wholeName
                    (TypedConstructorPattern optionIntInfo someName [TypedVariablePattern intInfo itemBinder itemName])
                )
                ( Just
                    ( TypedBinaryExpr
                        boolInfo
                        (TypedBuiltinOperator "==")
                        (boundVariableExpr itemName intInfo itemBinder)
                        (intExpr 1)
                    )
                )
                (boundVariableExpr wholeName optionIntInfo wholeBinder),
              TypedCaseArm
                (TypedVariablePattern optionIntInfo fallbackBinder fallbackName)
                Nothing
                (boundVariableExpr fallbackName optionIntInfo fallbackBinder)
            ]
        )
    ]
    optionIntInfo
  where
    wholeName = valueName "whole"
    itemName = valueName "item"
    fallbackName = valueName "fallback"
    wholeBinder = patternBinder [1, 0] wholeName
    itemBinder = patternBinder [1, 0, 0, 0] itemName
    fallbackBinder = patternBinder [1, 1] fallbackName

managedAsConstructorPatternProgram :: TypedProgram
managedAsConstructorPatternProgram =
  managedProgram
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 1])
            [ TypedCaseArm
                ( TypedAsPattern
                    optionIntInfo
                    wholeBinder
                    wholeName
                    (TypedConstructorPattern optionIntInfo someName [TypedVariablePattern intInfo itemBinder itemName])
                )
                Nothing
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm (TypedConstructorPattern optionIntInfo noneName []) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    wholeName = valueName "whole"
    itemName = valueName "item"
    wholeBinder = patternBinder [1, 0] wholeName
    itemBinder = patternBinder [1, 0, 0, 0] itemName

managedOrConstructorPatternProgram :: TypedProgram
managedOrConstructorPatternProgram =
  managedProgram
    [ TypedDataStatement choiceDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedPatternCaseExpr
            intInfo
            (monomorphicConstructorCall rightBinder rightName choiceInfo [intInfo, intInfo] [intExpr 2, intExpr 20])
            [ TypedCaseArm
                ( TypedOrPattern
                    choiceInfo
                    [ TypedConstructorPattern choiceInfo leftName [TypedLiteralPattern intInfo (TypedIntegerLiteral "1"), TypedVariablePattern intInfo itemBinder itemName],
                      TypedConstructorPattern choiceInfo rightName [TypedLiteralPattern intInfo (TypedIntegerLiteral "2"), TypedVariablePattern intInfo itemBinder itemName]
                    ]
                )
                Nothing
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm (TypedWildcardPattern choiceInfo) Nothing (intExpr 0)
            ]
        )
    ]
    intInfo
  where
    choiceName = typeName "Choice"
    leftName = constructorName "Left"
    rightName = constructorName "Right"
    leftBinder = constructorBinder 0 leftName
    rightBinder = constructorBinder 1 rightName
    choiceInfo = variantInfo choiceName []
    choiceDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        choiceName
        []
        [ TypedConstructorDeclaration leftBinder leftName [TypedIntType, TypedIntType] [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64],
          TypedConstructorDeclaration rightBinder rightName [TypedIntType, TypedIntType] [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64]
        ]
    itemName = valueName "item"
    itemBinder = patternBinder [1, 0, 0, 1] itemName

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

patternBinder :: [Int] -> TypedCoreName -> TypedBinderId
patternBinder path name = TypedBinderId (modulePath, path, name)

boundVariableExpr :: TypedCoreName -> TypedNodeInfo -> TypedBinderId -> TypedExpr
boundVariableExpr name info owner = TypedVariableExpr info name (Just owner)

managedExportedOptionSource :: Text
managedExportedOptionSource =
  Text.unlines
    [ "module App::Main (type Option(..)) {",
      "data Option a = None | Some a.",
      "Some 7.",
      "}"
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

bareConstructorSource, partialConstructorSource, listFieldSource, unresolvedConstructorSource, listConstructionSource, tupleEqualitySource, variantEqualitySource, tuplePatternSource, constructorPatternSource, nestedConstructorTuplePatternSource, asConstructorPatternSource, orConstructorPatternSource :: Text
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
nestedConstructorTuplePatternSource =
  Text.unlines
    [ "data Option a = None | Some a.",
      "case Some (1, \"one\") { | Some (number, label) -> number | None -> 0 }."
    ]
asConstructorPatternSource =
  Text.unlines
    [ "data Option a = None | Some a.",
      "case Some 1 { | whole @ Some item -> item | None -> 0 }."
    ]
orConstructorPatternSource =
  Text.unlines
    [ "data Choice = Left Int Int | Right Int Int.",
      "case Right 2 20 { | Left 1 item | Right 2 item -> item | _ -> 0 }."
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
