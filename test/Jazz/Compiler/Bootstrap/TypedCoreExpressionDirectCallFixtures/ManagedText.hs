{-# LANGUAGE OverloadedStrings #-}

-- | Managed Text producer and lowering artifacts.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedText where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Scalar
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinTextAppend, BuiltinTextAppendChar, BuiltinTextLength),
    builtinSymbolKernelName,
  )
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (TextAppendCharService, TextAppendService, TextEqualService, TextLengthService),
    runtimeServiceContract,
  )
import Jazz.Compiler.TypedCore

managedTextProducerFixtures :: [(Text, Fixture)]
managedTextProducerFixtures =
  [ ("managed-text-literal", sourceFixtureNoExports "managed-text-literal" managedTextLiteralSource),
    ("managed-scalar-binding", sourceFixtureNoExports "managed-scalar-binding" managedScalarBindingSource),
    ("managed-text-identity", sourceFixtureNoExports "managed-text-identity" managedTextIdentitySource),
    ("managed-text-capture-transport", sourceFixtureNoExports "managed-text-capture-transport" managedTextCaptureTransportSource),
    ("managed-text-conditional-result", sourceFixtureNoExports "managed-text-conditional-result" managedTextConditionalResultSource),
    ("managed-text-scalar-case-result", sourceFixtureNoExports "managed-text-scalar-case-result" managedTextScalarCaseResultSource)
  ]

managedTextExpectedPrograms :: [(Text, TypedProgram)]
managedTextExpectedPrograms =
  [ ( "managed-text-literal",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (textExpr "managed")]
        textInfo
    ),
    ("managed-scalar-binding", managedScalarBindingProgram),
    ("managed-text-identity", managedTextIdentityProgram),
    ("managed-text-capture-transport", managedTextCaptureTransportProgram),
    ("managed-text-conditional-result", managedTextConditionalResultProgram),
    ("managed-text-scalar-case-result", managedTextScalarCaseResultProgram),
    ("unsupported-managed-capture", managedTextCaptureProgram),
    ("partial-call-managed-argument", managedTextPartialApplicationProgram)
  ]

managedTextOperationProducerFixtures :: [(Text, Fixture)]
managedTextOperationProducerFixtures =
  [ ("managed-text-equality", sourceFixtureNoExports "managed-text-equality" managedTextEqualitySource),
    ("managed-text-inequality", sourceFixtureNoExports "managed-text-inequality" managedTextInequalitySource),
    ("managed-text-length", sourceFixtureNoExports "managed-text-length" managedTextLengthSource),
    ("managed-text-append", sourceFixtureNoExports "managed-text-append" managedTextAppendSource),
    ("managed-text-append-char", sourceFixtureNoExports "managed-text-append-char" managedTextAppendCharSource),
    ("managed-text-combined-operations", sourceFixtureNoExports "managed-text-combined-operations" managedTextCombinedOperationsSource),
    ("managed-text-duplicate-equality", sourceFixtureNoExports "managed-text-duplicate-equality" managedTextDuplicateEqualitySource),
    ("managed-text-conditional-append", sourceFixtureNoExports "managed-text-conditional-append" managedTextConditionalAppendSource),
    ( "managed-text-builtin-local-function-shadow",
      sourceFixtureNoExports
        "managed-text-builtin-local-function-shadow"
        ( Text.unlines
            [ "__kernel_textLength :: Bool -> Bool.",
              "__kernel_textLength = \\(item) -> item.",
              "__kernel_textLength True."
            ]
        )
    ),
    ( "managed-text-builtin-callable-parameter-shadow",
      sourceFixtureNoExports
        "managed-text-builtin-callable-parameter-shadow"
        ( Text.unlines
            [ "apply :: (Bool -> Bool) -> Bool.",
              "apply = \\(__kernel_textLength) -> __kernel_textLength True.",
              "identity :: Bool -> Bool.",
              "identity = \\(item) -> item.",
              "apply identity."
            ]
        )
    )
  ]

managedTextOperationExpectedPrograms :: [(Text, TypedProgram)]
managedTextOperationExpectedPrograms =
  [ ( "managed-text-equality",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (binaryExpr boolInfo "==" (textExpr "left") (textExpr "right"))]
        boolInfo
    ),
    ( "managed-text-inequality",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (binaryExpr boolInfo "!=" (textExpr "left") (textExpr "right"))]
        boolInfo
    ),
    ( "managed-text-length",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (managedTextBuiltinCall BuiltinTextLength [textInfo] intInfo [textExpr "Jazz"])]
        intInfo
    ),
    ( "managed-text-append",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (managedTextBuiltinCall BuiltinTextAppend [textInfo, textInfo] textInfo [textExpr "Jazz", textExpr "!"])]
        textInfo
    ),
    ( "managed-text-append-char",
      expectedRootProgram
        [TypedExpressionStatement (TypedSpan 2 1) (managedTextBuiltinCall BuiltinTextAppendChar [textInfo, charInfo] textInfo [textExpr "Jazz", charExpr '!'])]
        textInfo
    ),
    ( "managed-text-combined-operations",
      expectedRootProgram
        [ TypedExpressionStatement (TypedSpan 2 1) (binaryExpr boolInfo "==" (textExpr "left") (textExpr "right")),
          TypedExpressionStatement (TypedSpan 3 1) (managedTextBuiltinCall BuiltinTextLength [textInfo] intInfo [textExpr "Jazz"]),
          TypedExpressionStatement (TypedSpan 4 1) (managedTextBuiltinCall BuiltinTextAppend [textInfo, textInfo] textInfo [textExpr "Jazz", textExpr "!"]),
          TypedExpressionStatement (TypedSpan 5 1) (managedTextBuiltinCall BuiltinTextAppendChar [textInfo, charInfo] textInfo [textExpr "Jazz", charExpr '!'])
        ]
        textInfo
    ),
    ( "managed-text-duplicate-equality",
      expectedRootProgram
        [ TypedExpressionStatement (TypedSpan 2 1) (binaryExpr boolInfo "==" (textExpr "left") (textExpr "right")),
          TypedExpressionStatement (TypedSpan 3 1) (binaryExpr boolInfo "==" (textExpr "left") (textExpr "right"))
        ]
        boolInfo
    ),
    ( "managed-text-conditional-append",
      expectedRootProgram
        [ TypedExpressionStatement
            (TypedSpan 2 1)
            ( managedTextBuiltinCall
                BuiltinTextAppend
                [textInfo, textInfo]
                textInfo
                [ TypedIfExpr textInfo (boolExpr True) (textExpr "left") (textExpr "other"),
                  textExpr "right"
                ]
            )
        ]
        textInfo
    ),
    ( "managed-text-builtin-local-function-shadow",
      expectedFunctionProgramWithLineOffset
        1
        []
        [ ExpectedFunction
            "__kernel_textLength"
            [("item", boolInfo)]
            boolInfo
            TypedDirectCallableShape
            (variableExpr "item" boolInfo)
        ]
        (directCall "__kernel_textLength" [boolInfo] boolInfo [boolExpr True])
    ),
    ( "managed-text-builtin-callable-parameter-shadow",
      expectedFunctionProgramWithLineOffset
        1
        []
        [ ExpectedFunction
            "apply"
            [("__kernel_textLength", boolCallableInfo)]
            boolInfo
            TypedDirectCallableShape
            (directCall "__kernel_textLength" [boolInfo] boolInfo [boolExpr True]),
          boolIdentityFunction
        ]
        (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "identity" boolCallableInfo])
    )
  ]

managedTextKernelBoundaryFixtures :: [(Text, Fixture)]
managedTextKernelBoundaryFixtures =
  [ ("managed-text-bare-length", sourceFixtureNoExports "managed-text-bare-length" managedTextBareLengthSource),
    ("managed-text-partial-append", sourceFixtureNoExports "managed-text-partial-append" managedTextPartialAppendSource),
    ("managed-text-partial-append-char", sourceFixtureNoExports "managed-text-partial-append-char" managedTextPartialAppendCharSource),
    ("managed-text-oversaturated-length", sourceFixtureNoExports "managed-text-oversaturated-length" managedTextOversaturatedLengthSource)
  ]

managedTextExclusionFixtures :: [(Text, Fixture)]
managedTextExclusionFixtures =
  [ ("managed-text-literal-pattern", sourceFixtureNoExports "managed-text-literal-pattern" managedTextLiteralPatternSource),
    ("managed-text-uncons", sourceFixtureNoExports "managed-text-uncons" managedTextUnconsSource),
    ("managed-text-from-chars", sourceFixtureNoExports "managed-text-from-chars" managedTextFromCharsSource),
    ("managed-text-concat", sourceFixtureNoExports "managed-text-concat" managedTextConcatSource),
    ("managed-text-read-io", sourceFixtureNoExports "managed-text-read-io" managedTextReadIOSource),
    ("managed-text-write-io", sourceFixtureNoExports "managed-text-write-io" managedTextWriteIOSource)
  ]

managedTextOperationExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
managedTextOperationExpectedLoweredPrograms =
  [ operation "managed-text-equality" expectedManagedTextEqualityLoweredProgram,
    operation "managed-text-inequality" expectedManagedTextInequalityLoweredProgram,
    operation "managed-text-length" expectedManagedTextLengthLoweredProgram,
    operation "managed-text-append" expectedManagedTextAppendLoweredProgram,
    operation "managed-text-append-char" expectedManagedTextAppendCharLoweredProgram,
    operation "managed-text-combined-operations" expectedManagedTextCombinedOperationsLoweredProgram,
    operation "managed-text-duplicate-equality" expectedManagedTextDuplicateEqualityLoweredProgram,
    operation "managed-text-conditional-append" expectedManagedTextConditionalAppendLoweredProgram
  ]
  where
    operation name expectedProgram =
      case lookup name managedTextOperationExpectedPrograms of
        Just typedProgram -> (name, typedProgram, expectedProgram)
        Nothing -> error ("managed Text operation expectation is missing: " <> Text.unpack name)

expectedManagedTextEqualityLoweredProgram :: LoweredProgram
expectedManagedTextEqualityLoweredProgram =
  expectedManagedTextOperationProgram
    LoweredBoolRepresentation
    [runtimeServiceContract TextEqualService]
    [ expectedTextInstruction 1 "left",
      expectedTextInstruction 2 "right",
      expectedRuntimeCallInstruction
        3
        LoweredBoolRepresentation
        TextEqualService
        [loweredTemporary 1 textRepresentation, loweredTemporary 2 textRepresentation]
    ]
    (loweredTemporary 3 LoweredBoolRepresentation)

expectedManagedTextInequalityLoweredProgram :: LoweredProgram
expectedManagedTextInequalityLoweredProgram =
  expectedManagedTextOperationProgram
    LoweredBoolRepresentation
    [runtimeServiceContract TextEqualService]
    [ expectedTextInstruction 1 "left",
      expectedTextInstruction 2 "right",
      expectedRuntimeCallInstruction
        3
        LoweredBoolRepresentation
        TextEqualService
        [loweredTemporary 1 textRepresentation, loweredTemporary 2 textRepresentation],
      expectedPrimitiveInstruction
        4
        LoweredBoolRepresentation
        (LoweredBooleanPrimitive LoweredBooleanNot)
        [loweredTemporary 3 LoweredBoolRepresentation]
    ]
    (loweredTemporary 4 LoweredBoolRepresentation)

expectedManagedTextLengthLoweredProgram :: LoweredProgram
expectedManagedTextLengthLoweredProgram =
  expectedManagedTextOperationProgram
    int64Representation
    [runtimeServiceContract TextLengthService]
    [ expectedTextInstruction 1 "Jazz",
      expectedRuntimeCallInstruction
        2
        int64Representation
        TextLengthService
        [loweredTemporary 1 textRepresentation]
    ]
    (loweredTemporary 2 int64Representation)

expectedManagedTextAppendLoweredProgram :: LoweredProgram
expectedManagedTextAppendLoweredProgram =
  expectedManagedTextOperationProgram
    textRepresentation
    [runtimeServiceContract TextAppendService]
    [ expectedTextInstruction 1 "Jazz",
      expectedTextInstruction 2 "!",
      expectedRuntimeCallInstruction
        3
        textRepresentation
        TextAppendService
        [loweredTemporary 1 textRepresentation, loweredTemporary 2 textRepresentation]
    ]
    (loweredTemporary 3 textRepresentation)

expectedManagedTextAppendCharLoweredProgram :: LoweredProgram
expectedManagedTextAppendCharLoweredProgram =
  expectedManagedTextOperationProgram
    textRepresentation
    [runtimeServiceContract TextAppendCharService]
    [ expectedTextInstruction 1 "Jazz",
      expectedRuntimeCallInstruction
        2
        textRepresentation
        TextAppendCharService
        [loweredTemporary 1 textRepresentation, loweredImmediate (LoweredCharImmediate '!')]
    ]
    (loweredTemporary 2 textRepresentation)

expectedManagedTextCombinedOperationsLoweredProgram :: LoweredProgram
expectedManagedTextCombinedOperationsLoweredProgram =
  expectedManagedTextOperationProgram
    textRepresentation
    [ runtimeServiceContract TextEqualService,
      runtimeServiceContract TextLengthService,
      runtimeServiceContract TextAppendService,
      runtimeServiceContract TextAppendCharService
    ]
    [ expectedTextInstruction 1 "left",
      expectedTextInstruction 2 "right",
      expectedRuntimeCallInstruction 3 LoweredBoolRepresentation TextEqualService [loweredTemporary 1 textRepresentation, loweredTemporary 2 textRepresentation],
      expectedTextInstruction 4 "Jazz",
      expectedRuntimeCallInstruction 5 int64Representation TextLengthService [loweredTemporary 4 textRepresentation],
      expectedTextInstruction 6 "Jazz",
      expectedTextInstruction 7 "!",
      expectedRuntimeCallInstruction 8 textRepresentation TextAppendService [loweredTemporary 6 textRepresentation, loweredTemporary 7 textRepresentation],
      expectedTextInstruction 9 "Jazz",
      expectedRuntimeCallInstruction 10 textRepresentation TextAppendCharService [loweredTemporary 9 textRepresentation, loweredImmediate (LoweredCharImmediate '!')]
    ]
    (loweredTemporary 10 textRepresentation)

expectedManagedTextDuplicateEqualityLoweredProgram :: LoweredProgram
expectedManagedTextDuplicateEqualityLoweredProgram =
  expectedManagedTextOperationProgram
    LoweredBoolRepresentation
    [runtimeServiceContract TextEqualService]
    [ expectedTextInstruction 1 "left",
      expectedTextInstruction 2 "right",
      expectedRuntimeCallInstruction 3 LoweredBoolRepresentation TextEqualService [loweredTemporary 1 textRepresentation, loweredTemporary 2 textRepresentation],
      expectedTextInstruction 4 "left",
      expectedTextInstruction 5 "right",
      expectedRuntimeCallInstruction 6 LoweredBoolRepresentation TextEqualService [loweredTemporary 4 textRepresentation, loweredTemporary 5 textRepresentation]
    ]
    (loweredTemporary 6 LoweredBoolRepresentation)

expectedManagedTextConditionalAppendLoweredProgram :: LoweredProgram
expectedManagedTextConditionalAppendLoweredProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout]
    [runtimeServiceContract TextAppendService]
    [ LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredBranch
                    (loweredImmediate (LoweredBoolImmediate True))
                    thenBlockId
                    []
                    elseBlockId
                    []
                )
            ),
          LoweredBlock
            thenBlockId
            []
            [expectedTextInstruction 1 "left"]
            (Just (LoweredJump joinBlockId [loweredTemporary 1 textRepresentation])),
          LoweredBlock
            elseBlockId
            []
            [expectedTextInstruction 1 "other"]
            (Just (LoweredJump joinBlockId [loweredTemporary 1 textRepresentation])),
          LoweredBlock
            joinBlockId
            [LoweredParameter (LoweredParameterId "result") textRepresentation]
            [ expectedTextInstruction 1 "right",
              expectedRuntimeCallInstruction
                2
                textRepresentation
                TextAppendService
                [ LoweredBlockParameterOperand (LoweredParameterId "result") textRepresentation,
                  loweredTemporary 1 textRepresentation
                ]
            ]
            (Just (LoweredReturn (loweredTemporary 2 textRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId
  where
    thenBlockId = LoweredBlockId "if$s1$0$e3$0,0,1$then"
    elseBlockId = LoweredBlockId "if$s1$0$e3$0,0,1$else"
    joinBlockId = LoweredBlockId "if$s1$0$e3$0,0,1$join"

expectedManagedTextOperationProgram :: LoweredRepresentation -> [LoweredRuntimeService] -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
expectedManagedTextOperationProgram resultRepresentation services instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout]
    services
    [ LoweredFunction
        loweredEntryFunctionId
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
    loweredEntryFunctionId

expectedRuntimeCallInstruction :: Int -> LoweredRepresentation -> RuntimeServiceKey -> [LoweredOperand] -> LoweredInstruction
expectedRuntimeCallInstruction index representation serviceKey operands =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    representation
    (LoweredRuntimeCall serviceId operands)
  where
    LoweredRuntimeService serviceId _ = runtimeServiceContract serviceKey

managedTextExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
managedTextExpectedLoweredPrograms =
  [ ( "managed-text-literal",
      managedTextProgram "managed-text-literal",
      expectedManagedTextLiteralLoweredProgram "managed"
    ),
    ( "managed-scalar-binding",
      managedTextProgram "managed-scalar-binding",
      expectedManagedTextLiteralLoweredProgram "managed"
    ),
    ( "managed-text-identity",
      managedTextProgram "managed-text-identity",
      expectedManagedTextIdentityLoweredProgram
    ),
    ( "managed-text-capture-transport",
      managedTextProgram "managed-text-capture-transport",
      expectedManagedTextCaptureLoweredProgram
    ),
    ( "managed-text-conditional-result",
      managedTextProgram "managed-text-conditional-result",
      expectedManagedTextConditionalLoweredProgram
    ),
    ( "managed-text-scalar-case-result",
      managedTextProgram "managed-text-scalar-case-result",
      expectedManagedTextScalarCaseLoweredProgram
    )
  ]
  where
    managedTextProgram name =
      case lookup name managedTextExpectedPrograms of
        Just program -> program
        Nothing -> error ("managed Text producer expectation is missing: " <> Text.unpack name)

expectedManagedTextLiteralLoweredProgram :: Text -> LoweredProgram
expectedManagedTextLiteralLoweredProgram value =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout]
    []
    [ LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                textRepresentation
                (LoweredConstructText textLayoutId value)
            ]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId

textLayoutId :: LoweredLayoutId
textLayoutId = LoweredLayoutId "jazz.layout.text.v1"

textLayout :: LoweredLayout
textLayout = LoweredLayout textLayoutId LoweredTextLayout

textRepresentation :: LoweredRepresentation
textRepresentation = LoweredManagedReferenceRepresentation textLayoutId

expectedManagedTextIdentityLoweredProgram :: LoweredProgram
expectedManagedTextIdentityLoweredProgram =
  expectedManagedTextProgram
    [ expectedLocalFunction
        "identity"
        [LoweredParameter (LoweredParameterId "arg1") textRepresentation]
        textRepresentation
        []
        (loweredParameter 1 textRepresentation)
    ]
    textRepresentation
    [ expectedTextInstruction 1 "Jazz",
      expectedDirectCallInstruction
        2
        textRepresentation
        "identity"
        [loweredTemporary 1 textRepresentation]
    ]
    (loweredTemporary 2 textRepresentation)

expectedManagedTextCaptureLoweredProgram :: LoweredProgram
expectedManagedTextCaptureLoweredProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    [ textLayout,
      LoweredLayout captureLayoutId (LoweredClosureEnvironmentLayout [textRepresentation])
    ]
    []
    [ LoweredFunction
        (LoweredFunctionId "App::Main::capture")
        ( Just
            ( LoweredParameter
                (LoweredParameterId "environment")
                captureEnvironmentRepresentation
            )
        )
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                textRepresentation
                ( LoweredProjectField
                    captureLayoutId
                    0
                    ( LoweredFunctionParameterOperand
                        (LoweredParameterId "environment")
                        captureEnvironmentRepresentation
                    )
                )
            ]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation)))
        ]
        (LoweredBlockId "entry"),
      LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ expectedTextInstruction 1 "managed",
              LoweredInstruction
                (LoweredTemporaryId "t2")
                captureEnvironmentRepresentation
                ( LoweredConstructProduct
                    captureLayoutId
                    [loweredTemporary 1 textRepresentation]
                ),
              LoweredInstruction
                (LoweredTemporaryId "t3")
                captureClosureRepresentation
                ( LoweredConstructClosure
                    (LoweredFunctionId "App::Main::capture")
                    (loweredTemporary 2 captureEnvironmentRepresentation)
                ),
              expectedClosureCallInstruction
                4
                textRepresentation
                (loweredTemporary 3 captureClosureRepresentation)
                [loweredImmediate (LoweredBoolImmediate True)]
            ]
            (Just (LoweredReturn (loweredTemporary 4 textRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId
  where
    captureLayoutId =
      LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p1$2$n7:capture"
    captureEnvironmentRepresentation =
      LoweredManagedReferenceRepresentation captureLayoutId
    captureClosureRepresentation =
      LoweredClosureRepresentation
        (LoweredCallSignature [LoweredBoolRepresentation] textRepresentation)

expectedManagedTextConditionalLoweredProgram :: LoweredProgram
expectedManagedTextConditionalLoweredProgram =
  expectedManagedTextProgram
    [ LoweredFunction
        (LoweredFunctionId "App::Main::choose")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredBranch
                    (loweredParameter 1 LoweredBoolRepresentation)
                    thenBlockId
                    []
                    elseBlockId
                    []
                )
            ),
          LoweredBlock
            thenBlockId
            []
            [expectedTextInstruction 1 "yes"]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation))),
          LoweredBlock
            elseBlockId
            []
            [expectedTextInstruction 1 "no"]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    textRepresentation
    [ expectedDirectCallInstruction
        1
        textRepresentation
        "choose"
        [loweredImmediate (LoweredBoolImmediate True)]
    ]
    (loweredTemporary 1 textRepresentation)
  where
    thenBlockId = LoweredBlockId "if$s1$1$e2$0,0$then"
    elseBlockId = LoweredBlockId "if$s1$1$e2$0,0$else"

expectedManagedTextScalarCaseLoweredProgram :: LoweredProgram
expectedManagedTextScalarCaseLoweredProgram =
  expectedManagedTextProgram
    [ LoweredFunction
        (LoweredFunctionId "App::Main::choose")
        Nothing
        [LoweredParameter (LoweredParameterId "arg1") LoweredBoolRepresentation]
        textRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ expectedPrimitiveInstruction
                1
                LoweredBoolRepresentation
                (LoweredComparisonPrimitive LoweredEqual)
                [ loweredParameter 1 LoweredBoolRepresentation,
                  loweredImmediate (LoweredBoolImmediate True)
                ]
            ]
            ( Just
                ( LoweredBranch
                    (loweredTemporary 1 LoweredBoolRepresentation)
                    trueBodyBlockId
                    []
                    fallbackBodyBlockId
                    []
                )
            ),
          LoweredBlock
            trueBodyBlockId
            []
            [expectedTextInstruction 1 "yes"]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation))),
          LoweredBlock
            fallbackBodyBlockId
            []
            [expectedTextInstruction 1 "no"]
            (Just (LoweredReturn (loweredTemporary 1 textRepresentation)))
        ]
        (LoweredBlockId "entry")
    ]
    textRepresentation
    [ expectedDirectCallInstruction
        1
        textRepresentation
        "choose"
        [loweredImmediate (LoweredBoolImmediate True)]
    ]
    (loweredTemporary 1 textRepresentation)
  where
    trueBodyBlockId = LoweredBlockId "case$s1$1$e2$0,0$a0$body"
    fallbackBodyBlockId = LoweredBlockId "case$s1$1$e2$0,0$a1$body"

expectedManagedTextProgram :: [LoweredFunction] -> LoweredRepresentation -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
expectedManagedTextProgram functions resultRepresentation instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout]
    []
    ( functions
        <> [ LoweredFunction
               loweredEntryFunctionId
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
    loweredEntryFunctionId

expectedTextInstruction :: Int -> Text -> LoweredInstruction
expectedTextInstruction index value =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    textRepresentation
    (LoweredConstructText textLayoutId value)

expectedManagedTextBindingAfterConditionalProgram :: LoweredProgram
expectedManagedTextBindingAfterConditionalProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    [textLayout]
    []
    [ LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        LoweredBoolRepresentation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredBranch
                    (loweredImmediate (LoweredBoolImmediate True))
                    thenBlockId
                    []
                    elseBlockId
                    []
                )
            ),
          LoweredBlock
            thenBlockId
            []
            []
            (Just (LoweredJump joinBlockId [loweredInt64 1])),
          LoweredBlock
            elseBlockId
            []
            []
            (Just (LoweredJump joinBlockId [loweredInt64 2])),
          LoweredBlock
            joinBlockId
            [LoweredParameter (LoweredParameterId "result") int64Representation]
            [ LoweredInstruction
                (LoweredTemporaryId "t1")
                textRepresentation
                (LoweredConstructText textLayoutId "later")
            ]
            (Just (LoweredReturn (loweredImmediate (LoweredBoolImmediate True))))
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId
  where
    thenBlockId = LoweredBlockId "if$s1$0$e1$0$then"
    elseBlockId = LoweredBlockId "if$s1$0$e1$0$else"
    joinBlockId = LoweredBlockId "if$s1$0$e1$0$join"

managedScalarBindingProgram :: TypedProgram
managedScalarBindingProgram =
  expectedRootProgram
    [ TypedLetStatement messageBinder messageName (TypedSpan 2 1) messageScheme (textExpr "managed"),
      TypedExpressionStatement (TypedSpan 3 1) (boundVariableExpr messageName textInfo messageBinder)
    ]
    textInfo
  where
    messageName = resolvedName "message"
    messageBinder = TypedBinderId (modulePath, [0], messageName)
    messageScheme = scalarScheme messageBinder textInfo

managedTextIdentityProgram :: TypedProgram
managedTextIdentityProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [ ExpectedFunction
        "identity"
        [("item", textInfo)]
        textInfo
        TypedDirectCallableShape
        (variableExpr "item" textInfo)
    ]
    (directCall "identity" [textInfo] textInfo [textExpr "Jazz"])

managedTextCaptureTransportProgram :: TypedProgram
managedTextCaptureTransportProgram =
  expectedRootProgram
    ( TypedLetStatement messageBinder messageName (TypedSpan 2 1) messageScheme (textExpr "managed")
        : functionStatements
          <> [ TypedExpressionStatement
                 (TypedSpan 5 1)
                 ( bindExpectedExpressionVariables
                     (Map.singleton captureName captureBinder)
                     (directCall "capture" [boolInfo] textInfo [boolExpr True])
                 )
             ]
    )
    textInfo
  where
    messageName = resolvedName "message"
    messageBinder = TypedBinderId (modulePath, [0], messageName)
    messageScheme = scalarScheme messageBinder textInfo
    captureName = resolvedName "capture"
    captureBinder = TypedBinderId (modulePath, [2], captureName)
    function =
      ExpectedFunction
        "capture"
        [("ignored", boolInfo)]
        textInfo
        TypedClosureCallableShape
        (variableExpr "message" textInfo)
    functionStatements =
      map
        (bindExpectedStatementVariables (Map.fromList [(messageName, messageBinder), (captureName, captureBinder)]))
        (expectedFunctionStatementsAtLineOffset 1 1 2 function)

managedTextConditionalResultProgram :: TypedProgram
managedTextConditionalResultProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [ ExpectedFunction
        "choose"
        [("flag", boolInfo)]
        textInfo
        TypedDirectCallableShape
        (TypedIfExpr textInfo (variableExpr "flag" boolInfo) (textExpr "yes") (textExpr "no"))
    ]
    (directCall "choose" [boolInfo] textInfo [boolExpr True])

managedTextScalarCaseResultProgram :: TypedProgram
managedTextScalarCaseResultProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [ ExpectedFunction
        "choose"
        [("flag", boolInfo)]
        textInfo
        TypedDirectCallableShape
        ( TypedPatternCaseExpr
            textInfo
            (variableExpr "flag" boolInfo)
            [ TypedCaseArm
                (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
                Nothing
                (textExpr "yes"),
              TypedCaseArm
                (TypedWildcardPattern boolInfo)
                Nothing
                (textExpr "no")
            ]
        )
    ]
    (directCall "choose" [boolInfo] textInfo [boolExpr True])

managedTextCaptureProgram :: TypedProgram
managedTextCaptureProgram =
  expectedRootProgram
    ( TypedLetStatement messageBinder messageName (TypedSpan 2 1) messageScheme (textExpr "managed")
        : functionStatements
          <> [TypedExpressionStatement (TypedSpan 5 1) (TypedTupleExpr unitInfo [])]
    )
    unitInfo
  where
    messageName = resolvedName "message"
    messageBinder = TypedBinderId (modulePath, [0], messageName)
    messageScheme = scalarScheme messageBinder textInfo
    checkName = resolvedName "check"
    checkBinder = TypedBinderId (modulePath, [2], checkName)
    function =
      ExpectedFunction
        "check"
        [("ignored", boolInfo)]
        boolInfo
        TypedClosureCallableShape
        (binaryExpr boolInfo "==" (variableExpr "message" textInfo) (variableExpr "message" textInfo))
    functionStatements =
      map
        (bindExpectedStatementVariables (Map.fromList [(messageName, messageBinder), (checkName, checkBinder)]))
        (expectedFunctionStatementsAtLineOffset 1 1 2 function)

managedTextPartialApplicationProgram :: TypedProgram
managedTextPartialApplicationProgram =
  expectedFunctionProgramWithLineOffset
    1
    []
    [keepRightFunction]
    ( TypedApplyExpr
        remainingInfo
        (variableExpr "keepRight" keepRightInfo)
        (textExpr "managed")
    )
  where
    keepRightFunction =
      ExpectedFunction
        "keepRight"
        [("ignored", textInfo), ("right", intInfo)]
        intInfo
        TypedClosureCallableShape
        (variableExpr "right" intInfo)
    keepRightInfo = stagedFunctionInfo [("ignored", textInfo), ("right", intInfo)] intInfo
    remainingInfo = stagedFunctionInfo [("right", intInfo)] intInfo

managedTextBuiltinCall :: BuiltinSymbol -> [TypedNodeInfo] -> TypedNodeInfo -> [TypedExpr] -> TypedExpr
managedTextBuiltinCall symbol parameterInfos resultInfo arguments =
  saturatedCall
    "managed Text builtin expectation"
    (TypedVariableExpr (functionInfo (zip (repeat "") parameterInfos) resultInfo) (TypedBuiltinName (builtinSymbolKernelName symbol)) Nothing)
    parameterInfos
    resultInfo
    arguments
