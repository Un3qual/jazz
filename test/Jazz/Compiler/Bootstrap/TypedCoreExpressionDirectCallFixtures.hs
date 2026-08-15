{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( Fixture (..),
    fixtureNames,
    acceptedFixtureNames,
    rejectedFixtureNames,
    priorScalarDirectCallFixtureNames,
    fixtures,
    expectedUnitProgram,
    scalarExpectedLoweredPrograms,
    explicitNumericScalarLoweringPrograms,
    fullUInt64ScalarLoweringPrograms,
    nestedScalarTypedProgram,
    expectedNestedScalarLoweredProgram,
    scalarFixtures,
    scalarExpectedPrograms,
    scalarPatternCaseExpectedPrograms,
    scalarPatternCaseExpectedLoweredPrograms,
    scalarPatternCaseAnalysisExpectedPrograms,
    scalarPatternCaseLowererBoundaryPrograms,
    scalarBindingExpectedPrograms,
    scalarBindingExpectedLoweredPrograms,
    managedTextProducerFixtures,
    managedTextExpectedPrograms,
    lexicalCaptureExpectedPrograms,
    lexicalCaptureExpectedLoweredPrograms,
    curriedApplicationExpectedPrograms,
    curriedApplicationExpectedLoweredPrograms,
    directCallExpectedPrograms,
    directRecursionExpectedPrograms,
    directRecursionExpectedLoweredPrograms,
    closureRecursionExpectedPrograms,
    closureRecursionExpectedLoweredPrograms,
    closedCallableExpectedPrograms,
    directCallExpectedLoweredPrograms,
    closedCallableExpectedLoweredPrograms,
    independentClosureExpectedLoweredPrograms,
    functionBodyConsumedCallExpectedProgram,
    functionBodyPartialApplicationExpectedProgram,
    nestedTailControlFlowExpectedLoweredPrograms,
    rfcClosureEnvironmentIdentityProgram,
    lowererBoundaryPrograms,
    validIndependentLowererPrograms,
    invalidLowererBoundaryPrograms,
    independentLowererPrograms,
    lowererStructuralBoundaryPrograms,
    reviewLowererBoundaryPrograms,
    producerEdgeFixtures,
    scalarBindingProducerFixtures,
    ordinaryForwardVisibilityFixture,
    forwardVisibilityNegativeFixtures,
    rejectedScalarFixtures,
    resolveFixture,
    resolveFixtureWithLookup,
    explicitNumericTypes,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.LoweredIR
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    resolveProgram,
  )
import Jazz.Compiler.TypeInference (InferenceInputs (..))
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (TFunctionType, TIntType),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.WarningConfig (defaultWarningSettings)

data Fixture = Fixture
  { fixtureName :: Text,
    fixtureInputs :: InferenceInputs,
    fixtureSourcePath :: TypedSourcePath,
    fixtureSourceFiles :: Map.Map FilePath Text
  }

fixtureNames :: [Text]
fixtureNames = map fixtureName fixtures

acceptedFixtureNames :: [Text]
acceptedFixtureNames = map fixtureName acceptedFixtures

rejectedFixtureNames :: [Text]
rejectedFixtureNames = map fixtureName rejectedFixtures

priorScalarDirectCallFixtureNames :: [Text]
priorScalarDirectCallFixtureNames =
  [ "unit-entry",
    "bool-entry",
    "char-entry",
    "default-int-entry",
    "default-float-entry",
    "explicit-numeric-widths",
    "arithmetic-operators",
    "ordering-operators",
    "equality-operators",
    "scalar-parameter-return",
    "single-argument-direct-call",
    "curried-multi-argument-direct-call",
    "forward-direct-call-dag",
    "nested-direct-calls",
    "dollar-direct-call",
    "exported-direct-function",
    "source-diagnostic",
    "invalid-portable-source-path",
    "resolved-import",
    "ambient-prelude-input",
    "text-value",
    "list-value",
    "non-unit-tuple",
    "data-value",
    "conditional",
    "pattern-case",
    "local-block-binding",
    "named-function-value",
    "partial-direct-call",
    "oversaturated-direct-call",
    "capturing-function",
    "self-recursive-function",
    "mutually-recursive-functions",
    "polymorphic-or-evidence-function",
    "imported-direct-call",
    "user-defined-operator-call"
  ]

fixtures :: [Fixture]
fixtures = acceptedFixtures <> rejectedFixtures

acceptedFixtures :: [Fixture]
acceptedFixtures =
  [ sourceFixture "unit-entry" unitEntrySource,
    sourceFixture "bool-entry" boolEntrySource,
    sourceFixture "char-entry" charEntrySource,
    sourceFixture "default-int-entry" defaultIntEntrySource,
    sourceFixture "default-float-entry" defaultFloatEntrySource,
    sourceFixture "explicit-numeric-widths" explicitNumericWidthsSource,
    sourceFixture "arithmetic-operators" arithmeticOperatorsSource,
    sourceFixture "ordering-operators" orderingOperatorsSource,
    sourceFixture "equality-operators" equalityOperatorsSource,
    sourceFixtureNoExports "conditional" conditionalSource,
    sourceFixtureNoExports "pattern-case" patternCaseSource,
    sourceFixture "scalar-parameter-return" scalarParameterReturnSource,
    sourceFixture "single-argument-direct-call" singleArgumentDirectCallSource,
    sourceFixture "curried-multi-argument-direct-call" curriedMultiArgumentDirectCallSource,
    sourceFixture "three-argument-direct-call" threeArgumentDirectCallSource,
    sourceFixture "forward-direct-call-dag" forwardDirectCallDagSource,
    sourceFixture "nested-direct-calls" nestedDirectCallsSource,
    sourceFixture "dollar-direct-call" dollarDirectCallSource,
    sourceFixture "exported-direct-function" exportedDirectFunctionSource,
    sourceFixtureNoExports "named-function-value" namedFunctionValueSource,
    sourceFixtureNoExports "higher-order-call" higherOrderCallSource,
    sourceFixtureNoExports "closure-result" closureResultSource,
    sourceFixtureNoExports "callable-parameter-shadows-named-function" callableParameterShadowsNamedFunctionSource,
    sourceFixtureNoExports "callable-parameter-shadows-enclosing-function" callableParameterShadowsEnclosingFunctionSource,
    sourceFixtureNoExports "mixed-direct-and-value-use" mixedDirectAndValueUseSource,
    sourceFixtureNoExports "callable-parameter-value-shadows-enclosing-function" callableParameterValueShadowsEnclosingFunctionSource,
    sourceFixtureNoExports "capturing-function" capturingFunctionSource,
    sourceFixtureNoExports "partial-direct-call" partialDirectCallSource,
    sourceFixtureNoExports "self-recursive-function" selfRecursiveFunctionSource,
    sourceFixtureNoExports "mutually-recursive-functions" mutuallyRecursiveFunctionsSource,
    sourceFixtureNoExports "closure-value-mutual-recursion" closureValueMutualRecursionSource,
    sourceFixtureNoExports "closure-value-self-recursion" closureValueSelfRecursionSource,
    sourceFixtureNoExports "capturing-self-recursion" capturingSelfRecursionSource,
    sourceFixtureNoExports "capturing-mutual-recursion" capturingMutualRecursionSource
  ]

rejectedFixtures :: [Fixture]
rejectedFixtures =
  [ sourceFixture "source-diagnostic" sourceDiagnosticSource,
    (sourceFixture "invalid-portable-source-path" unitEntrySource)
      { fixtureSourcePath = TypedSourcePath "/private/host/Main.jz"
      },
    sourceFixtureWithFiles "resolved-import" emptyInputs resolvedImportSource resolvedImportSourceFiles,
    (sourceFixture "ambient-prelude-input" unitEntrySource)
      { fixtureInputs = ambientPreludeInputs
      },
    sourceFixtureNoExports "text-value" textValueSource,
    sourceFixtureNoExports "list-value" listValueSource,
    sourceFixtureNoExports "non-unit-tuple" nonUnitTupleSource,
    sourceFixtureNoExports "data-value" dataValueSource,
    sourceFixtureNoExports "local-block-binding" localBlockBindingSource,
    sourceFixtureNoExports "oversaturated-direct-call" oversaturatedDirectCallSource,
    sourceFixtureNoExports "later-capture-mutual-recursion" laterCaptureMutualRecursionSource,
    sourceFixtureNoExports "transitive-later-capture-mutual-recursion" transitiveLaterCaptureMutualRecursionSource,
    sourceFixtureNoExports "interleaved-rebound-capture-mutual-recursion" interleavedReboundCaptureMutualRecursionSource,
    sourceFixtureNoExports "polymorphic-or-evidence-function" polymorphicFunctionSource,
    (sourceFixture "imported-direct-call" importedDirectCallSource)
      { fixtureInputs =
          emptyInputs
            { inferenceImportedTypes =
                Map.singleton
                  "foreign"
                  (PlainTypeBinding (TFunctionType TIntType TIntType))
            }
      },
    sourceFixtureNoExports "user-defined-operator-call" userDefinedOperatorCallSource
  ]

forwardVisibilityNegativeFixtures :: [Fixture]
forwardVisibilityNegativeFixtures =
  [ sourceFixture "forward-polymorphic-function-invisibility" forwardPolymorphicFunctionSource,
    sourceFixture "forward-constrained-function-invisibility" forwardConstrainedFunctionSource,
    sourceFixture "forward-signed-scalar-invisibility" forwardSignedScalarSource,
    sourceFixture "forward-unsigned-lambda-invisibility" forwardUnsignedLambdaSource,
    sourceFixtureNoExports "nested-forward-signed-function-invisibility" nestedForwardSignedFunctionSource
  ]

ordinaryForwardVisibilityFixture :: Fixture
ordinaryForwardVisibilityFixture =
  sourceFixture "ordinary-unsigned-forward-caller-invisibility" ordinaryUnsignedForwardCallerSource

expectedUnitProgram :: TypedProgram
expectedUnitProgram = TypedProgram Nothing [entryModule] modulePath

scalarExpectedLoweredPrograms :: [(Text, LoweredProgram)]
scalarExpectedLoweredPrograms =
  [ ("unit-entry", expectedLoweredProgram LoweredUnitRepresentation [] (loweredImmediate LoweredUnitImmediate)),
    ("bool-entry", expectedLoweredProgram LoweredBoolRepresentation [] (loweredImmediate (LoweredBoolImmediate True))),
    ("char-entry", expectedLoweredProgram LoweredCharRepresentation [] (loweredImmediate (LoweredCharImmediate 'j'))),
    ("default-int-entry", expectedLoweredProgram int64Representation [] (loweredInt64 7)),
    ("default-float-entry", expectedLoweredProgram float64Representation [] (loweredImmediate (LoweredFloatImmediate LoweredFloatWidth64 "1.05"))),
    ("conditional", expectedConditionalLoweredProgram),
    ( "arithmetic-operators",
      expectedLoweredProgram
        int64Representation
        [ expectedPrimitiveInstruction 1 int64Representation (LoweredArithmeticPrimitive LoweredAdd) [loweredInt64 1, loweredInt64 2],
          expectedPrimitiveInstruction 2 int64Representation (LoweredArithmeticPrimitive LoweredSubtract) [loweredInt64 3, loweredInt64 1],
          expectedPrimitiveInstruction 3 int64Representation (LoweredArithmeticPrimitive LoweredMultiply) [loweredInt64 2, loweredInt64 4],
          expectedPrimitiveInstruction 4 int64Representation (LoweredArithmeticPrimitive LoweredDivide) [loweredInt64 8, loweredInt64 2]
        ]
        (loweredTemporary 4 int64Representation)
    ),
    ( "ordering-operators",
      expectedLoweredProgram
        LoweredBoolRepresentation
        [ expectedPrimitiveInstruction 1 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredLessThan) [loweredInt64 1, loweredInt64 2],
          expectedPrimitiveInstruction 2 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredLessThanOrEqual) [loweredInt64 2, loweredInt64 2],
          expectedPrimitiveInstruction 3 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredGreaterThan) [loweredInt64 3, loweredInt64 2],
          expectedPrimitiveInstruction 4 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredGreaterThanOrEqual) [loweredInt64 3, loweredInt64 3]
        ]
        (loweredTemporary 4 LoweredBoolRepresentation)
    ),
    ( "equality-operators",
      expectedLoweredProgram
        LoweredBoolRepresentation
        [ expectedPrimitiveInstruction 1 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredEqual) [loweredInt64 1, loweredInt64 1],
          expectedPrimitiveInstruction 2 LoweredBoolRepresentation (LoweredComparisonPrimitive LoweredNotEqual) [loweredInt64 1, loweredInt64 2]
        ]
        (loweredTemporary 2 LoweredBoolRepresentation)
    )
  ]

expectedConditionalLoweredProgram :: LoweredProgram
expectedConditionalLoweredProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    []
    []
    [ LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        int64Representation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            ( Just
                ( LoweredBranch
                    (loweredImmediate (LoweredBoolImmediate True))
                    conditionalThenBlockId
                    []
                    conditionalElseBlockId
                    []
                )
            ),
          LoweredBlock
            conditionalThenBlockId
            []
            []
            (Just (LoweredJump conditionalJoinBlockId [loweredInt64 1])),
          LoweredBlock
            conditionalElseBlockId
            []
            []
            (Just (LoweredJump conditionalJoinBlockId [loweredInt64 2])),
          LoweredBlock
            conditionalJoinBlockId
            [LoweredParameter (LoweredParameterId "result") int64Representation]
            []
            ( Just
                ( LoweredReturn
                    ( LoweredBlockParameterOperand
                        (LoweredParameterId "result")
                        int64Representation
                    )
                )
            )
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId
  where
    conditionalThenBlockId = LoweredBlockId "if$s1$0$e1$0$then"
    conditionalElseBlockId = LoweredBlockId "if$s1$0$e1$0$else"
    conditionalJoinBlockId = LoweredBlockId "if$s1$0$e1$0$join"

explicitNumericScalarLoweringPrograms :: [(Text, TypedProgram, LoweredProgram)]
explicitNumericScalarLoweringPrograms =
  [ expectedNumericInteger "Int8" TypedInt8Type (TypedSignedIntegerRecipe 8) (LoweredSignedIntegerRepresentation LoweredIntegerWidth8) (LoweredSignedIntegerImmediate LoweredIntegerWidth8 1) 1,
    expectedNumericInteger "Int16" TypedInt16Type (TypedSignedIntegerRecipe 16) (LoweredSignedIntegerRepresentation LoweredIntegerWidth16) (LoweredSignedIntegerImmediate LoweredIntegerWidth16 2) 2,
    expectedNumericInteger "Int32" TypedInt32Type (TypedSignedIntegerRecipe 32) (LoweredSignedIntegerRepresentation LoweredIntegerWidth32) (LoweredSignedIntegerImmediate LoweredIntegerWidth32 3) 3,
    expectedNumericInteger "Int64" TypedInt64Type (TypedSignedIntegerRecipe 64) (LoweredSignedIntegerRepresentation LoweredIntegerWidth64) (LoweredSignedIntegerImmediate LoweredIntegerWidth64 4) 4,
    expectedNumericInteger "UInt8" TypedUInt8Type (TypedUnsignedIntegerRecipe 8) (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth8) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 5) 5,
    expectedNumericInteger "UInt16" TypedUInt16Type (TypedUnsignedIntegerRecipe 16) (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth16) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth16 6) 6,
    expectedNumericInteger "UInt32" TypedUInt32Type (TypedUnsignedIntegerRecipe 32) (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth32) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth32 7) 7,
    expectedNumericInteger "UInt64" TypedUInt64Type (TypedUnsignedIntegerRecipe 64) (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 8) 8,
    expectedNumericFloat "Float16" TypedFloat16Type (TypedFloatRecipe 16) (LoweredFloatRepresentation LoweredFloatWidth16) (LoweredFloatImmediate LoweredFloatWidth16 "1.5") "1" "5",
    expectedNumericFloat "Float32" TypedFloat32Type (TypedFloatRecipe 32) (LoweredFloatRepresentation LoweredFloatWidth32) (LoweredFloatImmediate LoweredFloatWidth32 "2.5") "2" "5",
    expectedNumericFloat "Float64" TypedFloat64Type (TypedFloatRecipe 64) (LoweredFloatRepresentation LoweredFloatWidth64) (LoweredFloatImmediate LoweredFloatWidth64 "3.5") "3" "5"
  ]
  where
    expectedNumericInteger ::
      Text ->
      TypedNumericType ->
      TypedRepresentationRecipe ->
      LoweredRepresentation ->
      LoweredImmediate ->
      Integer ->
      (Text, TypedProgram, LoweredProgram)
    expectedNumericInteger name numericType recipe representation immediateValue value =
      let info = TypedNodeInfo (TypedNumericType numericType) recipe [] []
       in ( name,
            expectedScalarProgram info (TypedLiteralExpr info (TypedIntegerLiteral (Text.pack (show value)))),
            expectedLoweredProgram representation [] (loweredImmediate immediateValue)
          )
    expectedNumericFloat ::
      Text ->
      TypedNumericType ->
      TypedRepresentationRecipe ->
      LoweredRepresentation ->
      LoweredImmediate ->
      Text ->
      Text ->
      (Text, TypedProgram, LoweredProgram)
    expectedNumericFloat name numericType recipe representation immediateValue whole fractional =
      let info = TypedNodeInfo (TypedNumericType numericType) recipe [] []
       in ( name,
            expectedScalarProgram info (TypedLiteralExpr info (TypedFractionalLiteral whole fractional (Just numericType))),
            expectedLoweredProgram representation [] (loweredImmediate immediateValue)
          )

fullUInt64ScalarLoweringPrograms :: [(Text, TypedProgram, LoweredProgram)]
fullUInt64ScalarLoweringPrograms =
  [ fullUInt64Program "first-upper-half-uint64" "9223372036854775808" 9223372036854775808,
    fullUInt64Program "maximum-uint64" "18446744073709551615" 18446744073709551615
  ]
  where
    fullUInt64Program name source value =
      let info =
            TypedNodeInfo
              (TypedNumericType TypedUInt64Type)
              (TypedUnsignedIntegerRecipe 64)
              []
              []
          representation =
            LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64
       in ( name,
            expectedScalarProgram info (TypedLiteralExpr info (TypedIntegerLiteral source)),
            expectedLoweredProgram
              representation
              []
              (loweredImmediate (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 value))
          )

nestedScalarTypedProgram :: TypedProgram
nestedScalarTypedProgram =
  expectedScalarProgram
    intInfo
    ( binaryExpr
        intInfo
        "*"
        (binaryExpr intInfo "+" (intExpr 1) (intExpr 2))
        (binaryExpr intInfo "-" (intExpr 5) (intExpr 3))
    )

expectedNestedScalarLoweredProgram :: LoweredProgram
expectedNestedScalarLoweredProgram =
  expectedLoweredProgram
    int64Representation
    [ expectedPrimitiveInstruction 1 int64Representation (LoweredArithmeticPrimitive LoweredAdd) [loweredInt64 1, loweredInt64 2],
      expectedPrimitiveInstruction 2 int64Representation (LoweredArithmeticPrimitive LoweredSubtract) [loweredInt64 5, loweredInt64 3],
      expectedPrimitiveInstruction
        3
        int64Representation
        (LoweredArithmeticPrimitive LoweredMultiply)
        [loweredTemporary 1 int64Representation, loweredTemporary 2 int64Representation]
    ]
    (loweredTemporary 3 int64Representation)

expectedLoweredProgram :: LoweredRepresentation -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
expectedLoweredProgram resultRepresentation instructions resultOperand =
  LoweredProgram
    (LoweredIRVersion 1)
    []
    []
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

expectedPrimitiveInstruction :: Int -> LoweredRepresentation -> LoweredPrimitive -> [LoweredOperand] -> LoweredInstruction
expectedPrimitiveInstruction index representation primitive operands =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    representation
    (LoweredPrimitiveOperation primitive operands)

loweredEntryFunctionId :: LoweredFunctionId
loweredEntryFunctionId = LoweredFunctionId "App::Main::$entry"

loweredImmediate :: LoweredImmediate -> LoweredOperand
loweredImmediate = LoweredImmediateOperand

loweredInt64 :: Integer -> LoweredOperand
loweredInt64 = loweredImmediate . LoweredSignedIntegerImmediate LoweredIntegerWidth64

loweredTemporary :: Int -> LoweredRepresentation -> LoweredOperand
loweredTemporary index =
  LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index)))

int64Representation :: LoweredRepresentation
int64Representation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64

float64Representation :: LoweredRepresentation
float64Representation = LoweredFloatRepresentation LoweredFloatWidth64

scalarFixtures :: [Fixture]
scalarFixtures = map fixtureByName ["bool-entry", "char-entry", "default-int-entry", "default-float-entry", "arithmetic-operators", "ordering-operators", "equality-operators"]

scalarExpectedPrograms :: [(Text, TypedProgram)]
scalarExpectedPrograms =
  [ ("bool-entry", expectedScalarProgram boolInfo (boolExpr True)),
    ("char-entry", expectedScalarProgram charInfo (charExpr 'j')),
    ("default-int-entry", expectedScalarProgram intInfo (intExpr 7)),
    ("default-float-entry", expectedScalarProgram floatInfo (floatExpr 1 "05" Nothing)),
    ( "arithmetic-operators",
      expectedScalarStatements
        [ binaryExpr intInfo "+" (intExpr 1) (intExpr 2),
          binaryExpr intInfo "-" (intExpr 3) (intExpr 1),
          binaryExpr intInfo "*" (intExpr 2) (intExpr 4),
          binaryExpr intInfo "/" (intExpr 8) (intExpr 2)
        ]
    ),
    ( "ordering-operators",
      expectedScalarStatements
        [ binaryExpr boolInfo "<" (intExpr 1) (intExpr 2),
          binaryExpr boolInfo "<=" (intExpr 2) (intExpr 2),
          binaryExpr boolInfo ">" (intExpr 3) (intExpr 2),
          binaryExpr boolInfo ">=" (intExpr 3) (intExpr 3)
        ]
    ),
    ( "equality-operators",
      expectedScalarStatements
        [ binaryExpr boolInfo "==" (intExpr 1) (intExpr 1),
          binaryExpr boolInfo "!=" (intExpr 1) (intExpr 2)
        ]
    ),
    ( "conditional",
      TypedProgram
        Nothing
        [ TypedModule
            modulePath
            validSourcePath
            []
            []
            (TypedModuleInterface [] [] [] [])
            []
            [ TypedExpressionStatement
                (TypedSpan 2 1)
                (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2))
            ]
            intInfo
        ]
        modulePath
    )
  ]

scalarPatternCaseExpectedPrograms :: [(Text, TypedProgram)]
scalarPatternCaseExpectedPrograms =
  [ ("pattern-case", scalarPatternCaseExpectedProgram),
    ("scalar-pattern-case", scalarPatternCaseExpectedProgram),
    ("scalar-pattern-case-variable-guards", scalarPatternCaseVariableGuardsProgram),
    ("scalar-pattern-case-repeated-literal-guards", scalarPatternCaseRepeatedLiteralGuardsProgram)
  ]

scalarPatternCaseExpectedProgram :: TypedProgram
scalarPatternCaseExpectedProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedExpressionStatement
            (TypedSpan 2 1)
            ( TypedPatternCaseExpr
                intInfo
                (boolExpr True)
                [ TypedCaseArm
                    (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
                    Nothing
                    (intExpr 1),
                  TypedCaseArm
                    (TypedWildcardPattern boolInfo)
                    Nothing
                    (intExpr 2)
                ]
            )
        ]
        intInfo
    ]
    modulePath

scalarPatternCaseVariableGuardsProgram :: TypedProgram
scalarPatternCaseVariableGuardsProgram =
  expectedRootProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            intInfo
            (intExpr 2)
            [ TypedCaseArm
                (TypedVariablePattern intInfo itemBinder itemName)
                ( Just
                    ( binaryExpr
                        boolInfo
                        ">"
                        (boundVariableExpr itemName intInfo itemBinder)
                        (intExpr 2)
                    )
                )
                (boundVariableExpr itemName intInfo itemBinder),
              TypedCaseArm
                (TypedVariablePattern intInfo fallbackBinder fallbackName)
                Nothing
                ( binaryExpr
                    intInfo
                    "+"
                    (boundVariableExpr fallbackName intInfo fallbackBinder)
                    (intExpr 1)
                )
            ]
        )
    ]
    intInfo
  where
    itemName = resolvedName "item"
    itemBinder = TypedBinderId (modulePath, [0, 0], itemName)
    fallbackName = resolvedName "fallback"
    fallbackBinder = TypedBinderId (modulePath, [0, 1], fallbackName)

scalarPatternCaseExpectedLoweredPrograms :: [(Text, TypedProgram, LoweredProgram)]
scalarPatternCaseExpectedLoweredPrograms =
  [ ("pattern-case", scalarPatternCaseExpectedProgram, scalarPatternCaseExpectedLoweredProgram),
    ("scalar-pattern-case", scalarPatternCaseExpectedProgram, scalarPatternCaseExpectedLoweredProgram)
  ]

scalarPatternCaseExpectedLoweredProgram :: LoweredProgram
scalarPatternCaseExpectedLoweredProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    []
    []
    [ LoweredFunction
        loweredEntryFunctionId
        Nothing
        []
        int64Representation
        [ LoweredBlock
            (LoweredBlockId "entry")
            []
            [ expectedPrimitiveInstruction
                1
                LoweredBoolRepresentation
                (LoweredComparisonPrimitive LoweredEqual)
                [ loweredImmediate (LoweredBoolImmediate True),
                  loweredImmediate (LoweredBoolImmediate True)
                ]
            ]
            ( Just
                ( LoweredBranch
                    (loweredTemporary 1 LoweredBoolRepresentation)
                    firstBodyBlockId
                    []
                    finalBodyBlockId
                    []
                )
            ),
          LoweredBlock
            firstBodyBlockId
            []
            []
            (Just (LoweredJump joinBlockId [loweredInt64 1])),
          LoweredBlock
            finalBodyBlockId
            []
            []
            (Just (LoweredJump joinBlockId [loweredInt64 2])),
          LoweredBlock
            joinBlockId
            [LoweredParameter (LoweredParameterId "result") int64Representation]
            []
            ( Just
                ( LoweredReturn
                    ( LoweredBlockParameterOperand
                        (LoweredParameterId "result")
                        int64Representation
                    )
                )
            )
        ]
        (LoweredBlockId "entry")
    ]
    loweredEntryFunctionId
  where
    firstBodyBlockId = LoweredBlockId "case$s1$0$e1$0$a0$body"
    finalBodyBlockId = LoweredBlockId "case$s1$0$e1$0$a1$body"
    joinBlockId = LoweredBlockId "case$s1$0$e1$0$join"

scalarPatternCaseRepeatedLiteralGuardsProgram :: TypedProgram
scalarPatternCaseRepeatedLiteralGuardsProgram =
  expectedRootProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        ( TypedPatternCaseExpr
            intInfo
            (intExpr 2)
            [ TypedCaseArm
                (TypedLiteralPattern intInfo (TypedIntegerLiteral "2"))
                (Just (boolExpr False))
                (intExpr 10),
              TypedCaseArm
                (TypedLiteralPattern intInfo (TypedIntegerLiteral "2"))
                (Just (boolExpr True))
                (intExpr 20),
              TypedCaseArm
                (TypedWildcardPattern intInfo)
                Nothing
                (intExpr 30)
            ]
        )
    ]
    intInfo

scalarPatternCaseAnalysisExpectedPrograms :: [(Text, TypedProgram)]
scalarPatternCaseAnalysisExpectedPrograms =
  [ ("scalar-pattern-case-capture", scalarPatternCaseCaptureProgram),
    ("scalar-pattern-case-closure-result", scalarPatternCaseClosureResultProgram)
  ]

scalarPatternCaseLowererBoundaryPrograms :: [(Text, TypedProgram)]
scalarPatternCaseLowererBoundaryPrograms =
  [ ("pattern-case-constructor-lowerer", constructorPatternCaseLowererProgram),
    ("pattern-case-list-lowerer", unsupportedPatternCaseProgram listScrutinee listPattern),
    ("pattern-case-tuple-lowerer", unsupportedPatternCaseProgram tupleScrutinee tuplePattern),
    ("pattern-case-as-lowerer", unsupportedPatternCaseProgram (boolExpr True) asPattern),
    ("pattern-case-or-lowerer", unsupportedPatternCaseProgram (boolExpr True) orPattern),
    ("pattern-case-final-literal-lowerer", incompletePatternCaseProgram [literalArm]),
    ("pattern-case-final-guarded-catch-all-lowerer", incompletePatternCaseProgram [guardedWildcardArm]),
    ( "pattern-case-unguarded-non-final-wildcard-lowerer",
      incompletePatternCaseProgram [unguardedWildcardArm, finalWildcardArm]
    ),
    ( "pattern-case-unguarded-non-final-variable-lowerer",
      incompletePatternCaseProgram [unguardedVariableArm, finalWildcardArm]
    )
  ]
  where
    listInfo =
      TypedNodeInfo
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
        []
        []
    listScrutinee = TypedListExpr listInfo []
    listPattern = TypedListPattern listInfo []
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedBoolType, TypedBoolType])
        (TypedManagedProductRecipe [TypedBoolRecipe, TypedBoolRecipe])
        []
        []
    tupleScrutinee = TypedTupleExpr tupleInfo [boolExpr True, boolExpr False]
    tuplePattern =
      TypedTuplePattern
        tupleInfo
        [TypedWildcardPattern boolInfo, TypedWildcardPattern boolInfo]
    asName = resolvedName "whole"
    asBinder = TypedBinderId (modulePath, [0, 0, 0], asName)
    asPattern = TypedAsPattern boolInfo asBinder asName (TypedWildcardPattern boolInfo)
    orPattern =
      TypedOrPattern
        boolInfo
        [ TypedLiteralPattern boolInfo (TypedBooleanLiteral True),
          TypedLiteralPattern boolInfo (TypedBooleanLiteral False)
        ]
    literalArm =
      TypedCaseArm
        (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
        Nothing
        (intExpr 1)
    guardedWildcardArm =
      TypedCaseArm
        (TypedWildcardPattern boolInfo)
        (Just (boolExpr True))
        (intExpr 1)
    unguardedWildcardArm =
      TypedCaseArm
        (TypedWildcardPattern boolInfo)
        Nothing
        (intExpr 1)
    variableName = resolvedName "matched"
    variableBinder = TypedBinderId (modulePath, [0, 0, 0], variableName)
    unguardedVariableArm =
      TypedCaseArm
        (TypedVariablePattern boolInfo variableBinder variableName)
        Nothing
        (intExpr 1)
    finalWildcardArm =
      TypedCaseArm
        (TypedWildcardPattern boolInfo)
        Nothing
        (intExpr 2)

unsupportedPatternCaseProgram :: TypedExpr -> TypedPattern -> TypedProgram
unsupportedPatternCaseProgram scrutinee patternValue =
  expectedScalarProgram
    intInfo
    ( TypedPatternCaseExpr
        intInfo
        scrutinee
        [ TypedCaseArm patternValue Nothing (intExpr 1),
          TypedCaseArm
            (TypedWildcardPattern (typedExpressionInfo scrutinee))
            Nothing
            (intExpr 2)
        ]
    )

incompletePatternCaseProgram :: [TypedCaseArm] -> TypedProgram
incompletePatternCaseProgram arms =
  expectedScalarProgram
    intInfo
    (TypedPatternCaseExpr intInfo (boolExpr True) arms)

constructorPatternCaseLowererProgram :: TypedProgram
constructorPatternCaseLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedDataStatement declaration,
          TypedExpressionStatement
            (TypedSpan 2 1)
            ( TypedPatternCaseExpr
                intInfo
                scrutinee
                [ TypedCaseArm
                    (TypedConstructorPattern dataInfo constructorName [])
                    Nothing
                    (intExpr 1),
                  TypedCaseArm
                    (TypedWildcardPattern dataInfo)
                    Nothing
                    (intExpr 2)
                ]
            )
        ]
        intInfo
    ]
    modulePath
  where
    dataName =
      TypedResolvedName
        TypedCurrentModule
        TypedTypeNamespace
        "Choice"
    constructorName =
      TypedResolvedName
        TypedCurrentModule
        TypedConstructorNamespace
        "Chosen"
    constructorBinder = TypedBinderId (modulePath, [0, 0], constructorName)
    declaration =
      TypedDataDeclaration
        (TypedSpan 1 1)
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [] []]
    dataInfo =
      TypedNodeInfo
        (TypedDataType dataName [])
        (TypedManagedVariantRecipe dataName [])
        []
        []
    scrutinee = TypedVariableExpr dataInfo constructorName (Just constructorBinder)

scalarPatternCaseCaptureProgram :: TypedProgram
scalarPatternCaseCaptureProgram =
  expectedRootProgram
    [ TypedLetStatement
        seedBinder
        seedName
        (TypedSpan 2 1)
        (scalarScheme seedBinder inferredIntInfo)
        (inferredIntExpr 40),
      TypedLetStatement
        chooseBinder
        chooseName
        (TypedSpan 3 1)
        (patternCaseCallableScheme chooseBinder TypedClosureCallableShape chooseInfo)
        ( TypedLambdaExpr
            chooseInfo
            itemBinder
            itemName
            ( TypedPatternCaseExpr
                inferredIntInfo
                (boundVariableExpr itemName inferredIntInfo itemBinder)
                [ TypedCaseArm
                    (TypedVariablePattern inferredIntInfo currentBinder currentName)
                    ( Just
                        ( binaryExpr
                            boolInfo
                            ">"
                            (boundVariableExpr currentName inferredIntInfo currentBinder)
                            (inferredIntExpr 0)
                        )
                    )
                    ( binaryExpr
                        inferredIntInfo
                        "+"
                        (boundVariableExpr currentName inferredIntInfo currentBinder)
                        (boundVariableExpr seedName inferredIntInfo seedBinder)
                    ),
                  TypedCaseArm
                    (TypedWildcardPattern inferredIntInfo)
                    Nothing
                    (boundVariableExpr seedName inferredIntInfo seedBinder)
                ]
            )
        ),
      TypedExpressionStatement
        (TypedSpan 4 1)
        ( TypedApplyExpr
            inferredIntInfo
            (boundVariableExpr chooseName chooseInfo chooseBinder)
            (inferredIntExpr 2)
        )
    ]
    inferredIntInfo
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    chooseName = resolvedName "choose"
    chooseBinder = TypedBinderId (modulePath, [1], chooseName)
    itemName = resolvedName "item"
    itemBinder = TypedBinderId (modulePath, [1, 0], itemName)
    currentName = resolvedName "current"
    currentBinder = TypedBinderId (modulePath, [1, 0, 0, 0], currentName)
    chooseInfo = stagedFunctionInfo [("item", inferredIntInfo)] inferredIntInfo
    inferredIntExpr :: Integer -> TypedExpr
    inferredIntExpr value =
      TypedLiteralExpr inferredIntInfo (TypedIntegerLiteral (Text.pack (show value)))

scalarPatternCaseClosureResultProgram :: TypedProgram
scalarPatternCaseClosureResultProgram =
  expectedRootProgram
    [ TypedLetStatement
        chooseBinder
        chooseName
        (TypedSpan 2 1)
        (patternCaseCallableScheme chooseBinder TypedDirectCallableShape chooseInfo)
        ( TypedLambdaExpr
            chooseInfo
            flagBinder
            flagName
            ( TypedPatternCaseExpr
                remainingInfo
                (boundVariableExpr flagName boolInfo flagBinder)
                [ TypedCaseArm
                    (TypedLiteralPattern boolInfo (TypedBooleanLiteral True))
                    Nothing
                    ( TypedLambdaExpr
                        remainingInfo
                        trueItemBinder
                        itemName
                        (boundVariableExpr itemName intInfo trueItemBinder)
                    ),
                  TypedCaseArm
                    (TypedWildcardPattern boolInfo)
                    Nothing
                    ( TypedLambdaExpr
                        remainingInfo
                        falseItemBinder
                        itemName
                        (intExpr 0)
                    )
                ]
            )
        ),
      TypedExpressionStatement
        (TypedSpan 3 1)
        ( TypedApplyExpr
            inferredIntInfo
            ( TypedApplyExpr
                remainingInferredInfo
                (boundVariableExpr chooseName chooseInfo chooseBinder)
                (boolExpr True)
            )
            (TypedLiteralExpr inferredIntInfo (TypedIntegerLiteral "7"))
        )
    ]
    inferredIntInfo
  where
    chooseName = resolvedName "choose"
    chooseBinder = TypedBinderId (modulePath, [0], chooseName)
    flagName = resolvedName "flag"
    flagBinder = TypedBinderId (modulePath, [0, 0], flagName)
    itemName = resolvedName "item"
    trueItemBinder = TypedBinderId (modulePath, [0, 0, 0, 1, 1], itemName)
    falseItemBinder = TypedBinderId (modulePath, [0, 0, 0, 2, 1], itemName)
    remainingInfo = stagedFunctionInfo [("item", intInfo)] intInfo
    remainingInferredInfo = stagedFunctionInfo [("item", inferredIntInfo)] inferredIntInfo
    chooseInfo = functionInfo [("flag", boolInfo)] remainingInfo

patternCaseCallableScheme :: TypedBinderId -> TypedCallableShape -> TypedNodeInfo -> TypedScheme
patternCaseCallableScheme owner callableShape info =
  TypedScheme
    owner
    []
    []
    []
    (typedExpressionType info)
    (typedExpressionRecipe info)
    (Just callableShape)

scalarBindingProducerFixtures :: [(Text, Fixture)]
scalarBindingProducerFixtures =
  [ ("scalar-binding-literal", sourceFixtureNoExports "scalar-binding-literal" scalarBindingLiteralSource),
    ("scalar-binding-ordered-reuse", sourceFixtureNoExports "scalar-binding-ordered-reuse" scalarBindingOrderedReuseSource),
    ("scalar-binding-direct-call-result", sourceFixtureNoExports "scalar-binding-direct-call-result" scalarBindingDirectCallResultSource),
    ("scalar-binding-failed-initializer-hidden", sourceFixtureNoExports "scalar-binding-failed-initializer-hidden" scalarBindingFailedInitializerSource)
  ]

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
    ("partial-call-managed-argument-failure", managedTextPartialApplicationProgram)
  ]

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

expectedRootProgram :: [TypedStatement] -> TypedNodeInfo -> TypedProgram
expectedRootProgram statements moduleInfo =
  TypedProgram
    Nothing
    [TypedModule modulePath validSourcePath [] [] (TypedModuleInterface [] [] [] []) [] statements moduleInfo]
    modulePath

scalarScheme :: TypedBinderId -> TypedNodeInfo -> TypedScheme
scalarScheme owner info =
  TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info) Nothing

boundVariableExpr :: TypedCoreName -> TypedNodeInfo -> TypedBinderId -> TypedExpr
boundVariableExpr name info owner = TypedVariableExpr info name (Just owner)

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

expectedClosureCallInstruction :: Int -> LoweredRepresentation -> LoweredOperand -> [LoweredOperand] -> LoweredInstruction
expectedClosureCallInstruction index representation functionOperand operands =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    representation
    (LoweredClosureCall functionOperand operands)

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

expectedLocalFunction ::
  Text ->
  [LoweredParameter] ->
  LoweredRepresentation ->
  [LoweredInstruction] ->
  LoweredOperand ->
  LoweredFunction
expectedLocalFunction name parameters resultRepresentation instructions resultOperand =
  LoweredFunction
    (LoweredFunctionId ("App::Main::" <> name))
    Nothing
    parameters
    resultRepresentation
    [LoweredBlock (LoweredBlockId "entry") [] instructions (Just (LoweredReturn resultOperand))]
    (LoweredBlockId "entry")

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

expectedDirectCallInstruction :: Int -> LoweredRepresentation -> Text -> [LoweredOperand] -> LoweredInstruction
expectedDirectCallInstruction index representation functionName operands =
  LoweredInstruction
    (LoweredTemporaryId ("t" <> Text.pack (show index)))
    representation
    (LoweredDirectCall (LoweredFunctionId ("App::Main::" <> functionName)) operands)

loweredDirectTailCall :: Text -> [LoweredOperand] -> LoweredTerminator
loweredDirectTailCall functionName operands =
  LoweredDirectTailCall (LoweredFunctionId ("App::Main::" <> functionName)) operands

loweredParameter :: Int -> LoweredRepresentation -> LoweredOperand
loweredParameter index =
  LoweredFunctionParameterOperand (LoweredParameterId ("arg" <> Text.pack (show index)))

lowererBoundaryPrograms :: [(Text, TypedProgram)]
lowererBoundaryPrograms =
  [ ("scalar-binding-unsupported-rhs", invalidScalarBindingRhsProgram),
    ("combined-statement-failure-order", combinedStatementFailureOrderLowererProgram),
    ("recursion-descendant-failure-order", recursionDescendantFailureOrderLowererProgram),
    ("interleaved-capture-mutual-recursion", interleavedCaptureMutualRecursiveLowererProgram),
    ("closure-valued-parameter", closureValuedParameterLowererProgram),
    ("closure-valued-result", closureValuedResultLowererProgram),
    ("closure-shaped-named-function", closureShapeLowererProgram),
    ("closure-shaped-named-application", closureShapeApplicationLowererProgram),
    ("callable-parameter-shadows-top-level-lowerer", callableParameterShadowsTopLevelLowererProgram),
    ("callable-parameter-value-shadows-enclosing-function-lowerer", callableParameterValueShadowsEnclosingFunctionLowererProgram),
    ("non-concrete-closure-representation", nonConcreteClosureRepresentationLowererProgram),
    ("duplicate-parameter-function", duplicateParameterLowererProgram),
    ("self-recursive-duplicate-parameter-function", selfRecursiveDuplicateParameterLowererProgram),
    ("duplicate-function-identity", duplicateFunctionLowererProgram),
    ("capturing-function", capturingLowererProgram),
    ("closure-shaped-self-recursive-function", closureShapedSelfRecursiveLowererProgram),
    ("nested-lambda-closure-value-self-recursion", nestedLambdaClosureValueSelfRecursiveLowererProgram),
    ("imported-direct-call", importedDirectCallLowererProgram)
  ]

validIndependentLowererPrograms :: [(Text, TypedProgram)]
validIndependentLowererPrograms =
  [ (name, programValue)
  | (name, programValue, _) <- scalarBindingExpectedLoweredPrograms
  ]
    <> [ (name, programValue)
       | (name, programValue, _) <- directRecursionExpectedLoweredPrograms
       ]
    <> [ (name, programValue)
       | (name, programValue, _) <- closureRecursionExpectedLoweredPrograms
       ]
    <> lowererBoundaryPrograms
    <> lowererStructuralBoundaryPrograms
    <> scalarPatternCaseLowererBoundaryPrograms

invalidLowererBoundaryPrograms :: [(Text, TypedProgram)]
invalidLowererBoundaryPrograms =
  [ ("closure-shape-flattened-recipe", closureShapeFlattenedRecipeLowererProgram),
    ("direct-shape-staged-recipe", directShapeStagedRecipeLowererProgram),
    ("callable-shape-body-disagreement", callableShapeBodyDisagreementLowererProgram),
    ("variable-binder-reference-mismatch", variableBinderReferenceMismatchLowererProgram),
    ("direct-flattened-representation", directFlattenedRepresentationLowererProgram),
    ("direct-shaped-closure-value-self-recursion", directShapedClosureValueSelfRecursiveLowererProgram),
    ("shape-rejected-self-recursion", shapeRejectedSelfRecursiveLowererProgram),
    ("shape-rejected-mutual-recursion", shapeRejectedMutualRecursiveLowererProgram),
    ("shape-rejected-binder-shadow-control", shapeRejectedBinderShadowControlLowererProgram),
    ("bare-function-value", bareFunctionLowererProgram),
    ("partial-direct-call", partialCallLowererProgram)
  ]

independentLowererPrograms :: [(Text, TypedProgram)]
independentLowererPrograms =
  validIndependentLowererPrograms <> invalidLowererBoundaryPrograms

lowererStructuralBoundaryPrograms :: [(Text, TypedProgram)]
lowererStructuralBoundaryPrograms =
  [ ("managed-scalar-entry", managedScalarLowererProgram),
    ("conditional-entry", conditionalLowererProgram)
  ]

reviewLowererBoundaryPrograms :: [(Text, TypedProgram)]
reviewLowererBoundaryPrograms =
  [ ("lifted-lambda-failure-preorder", liftedLambdaFailurePreorderProgram),
    ("exported-scalar-lifted-lambda-name-collision", exportedScalarLiftedLambdaNameCollisionProgram)
  ]

liftedLambdaFailurePreorderProgram :: TypedProgram
liftedLambdaFailurePreorderProgram =
  expectedScalarProgram
    intInfo
    ( TypedApplyExpr
        intInfo
        ( TypedLambdaExpr
            lambdaInfo
            parameterBinder
            parameterName
            (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2))
        )
        (TypedIfExpr intInfo (boolExpr True) (intExpr 3) (intExpr 4))
    )
  where
    parameterName = resolvedName "item"
    parameterBinder = TypedBinderId (modulePath, [0, 0], parameterName)
    lambdaInfo = stagedFunctionInfo [("item", intInfo)] intInfo

exportedScalarLiftedLambdaNameCollisionProgram :: TypedProgram
exportedScalarLiftedLambdaNameCollisionProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        [TypedModuleExport TypedValueNamespace "item"]
        (TypedModuleInterface [TypedValueInterface itemName itemScheme] [] [] [])
        []
        [ TypedLetStatement itemBinder itemName (TypedSpan 1 1) itemScheme (intExpr 1),
          TypedExpressionStatement
            (TypedSpan 2 1)
            (TypedLambdaExpr lambdaInfo parameterBinder itemName (boundVariableExpr itemName intInfo parameterBinder))
        ]
        lambdaInfo
    ]
    modulePath
  where
    itemName = resolvedName "item"
    itemBinder = TypedBinderId (modulePath, [0], itemName)
    itemScheme = scalarScheme itemBinder intInfo
    parameterBinder = TypedBinderId (modulePath, [1], itemName)
    lambdaInfo = stagedFunctionInfo [("item", intInfo)] intInfo

managedScalarLowererProgram :: TypedProgram
managedScalarLowererProgram =
  expectedScalarProgram
    textInfo
    (TypedLiteralExpr textInfo (TypedTextLiteral "managed"))

conditionalLowererProgram :: TypedProgram
conditionalLowererProgram =
  expectedScalarProgram
    intInfo
    (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2))

closureShapeLowererProgram :: TypedProgram
closureShapeLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction]
    (variableExpr "identity" boolCallableInfo)

closureShapeApplicationLowererProgram :: TypedProgram
closureShapeApplicationLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction]
    ( directCall
        "identity"
        [boolInfo]
        boolInfo
        [binaryExpr boolInfo "==" (boolExpr True) (boolExpr False)]
    )

callableParameterShadowsTopLevelLowererProgram :: TypedProgram
callableParameterShadowsTopLevelLowererProgram =
  expectedFunctionProgram
    []
    [boolCombineFunction, applyCombineParameterFunction]
    (boolExpr True)

callableParameterValueShadowsEnclosingFunctionLowererProgram :: TypedProgram
callableParameterValueShadowsEnclosingFunctionLowererProgram =
  expectedFunctionProgram
    []
    [applyFunction, shadowingForwardFunction]
    (boolExpr True)

closureValuedParameterLowererProgram :: TypedProgram
closureValuedParameterLowererProgram =
  expectedFunctionProgram
    []
    [applyFunction]
    (boolExpr True)

closureValuedResultLowererProgram :: TypedProgram
closureValuedResultLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction, chooseFunction]
    (boolExpr True)

directFlattenedRepresentationLowererProgram :: TypedProgram
directFlattenedRepresentationLowererProgram =
  expectedFunctionProgram
    []
    [boolCombineFunction]
    (variableExpr "combine" (functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo))

nonConcreteClosureRepresentationLowererProgram :: TypedProgram
nonConcreteClosureRepresentationLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedSignatureStatement signatureBinder functionName (TypedSpan 1 1) (polymorphicScheme signatureBinder),
          TypedLetStatement
            bindingBinder
            functionName
            (TypedSpan 2 1)
            (polymorphicScheme bindingBinder)
            ( TypedLambdaExpr
                polymorphicInfo
                parameterBinder
                parameterName
                (TypedVariableExpr parameterInfo parameterName (Just parameterBinder))
            ),
          TypedExpressionStatement (TypedSpan 3 1) (boolExpr True)
        ]
        boolInfo
    ]
    modulePath
  where
    typeParameter = TypedTypeParameterId 0
    parameterName = resolvedName "item"
    functionName = resolvedName "identity"
    signatureBinder = TypedBinderId (modulePath, [0], functionName)
    bindingBinder = TypedBinderId (modulePath, [1], functionName)
    parameterBinder = TypedBinderId (modulePath, [1, 0], parameterName)
    parameterInfo =
      TypedNodeInfo
        (TypedTypeParameterType typeParameter)
        (TypedRepresentationParameterRecipe typeParameter)
        []
        []
    polymorphicInfo =
      TypedNodeInfo
        (TypedFunctionType (typedExpressionType parameterInfo) (typedExpressionType parameterInfo))
        ( TypedClosureRecipe
            [typedExpressionRecipe parameterInfo]
            (typedExpressionRecipe parameterInfo)
        )
        []
        []
    polymorphicScheme owner =
      TypedScheme
        owner
        [typeParameter]
        []
        []
        (typedExpressionType polymorphicInfo)
        (typedExpressionRecipe polymorphicInfo)
        (Just TypedClosureCallableShape)

callableShapeBodyDisagreementLowererProgram :: TypedProgram
callableShapeBodyDisagreementLowererProgram =
  rewriteChooserShape
    ( expectedFunctionProgram
        []
        [ boolCombineFunction,
          ExpectedFunction
            "choose"
            [("ignored", boolInfo)]
            binaryCallableInfo
            TypedClosureCallableShape
            (variableExpr "combine" binaryCallableInfo)
        ]
        (boolExpr True)
    )
  where
    binaryCallableInfo = functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo
    stagedChooserInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (typedExpressionType binaryCallableInfo))
        ( TypedClosureRecipe
            [TypedBoolRecipe]
            (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
        )
        []
        []
    rewriteChooserShape programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [ TypedModule
                path
                source
                imports
                exports
                interface
                recursiveGroups
                (map rewriteStatement statements)
                moduleInfo
            ]
            entryPath
        _ -> error "callable shape/body disagreement lowerer fixture changed shape"
    chooserName = resolvedName "choose"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue
          | name == chooserName ->
              TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body)
          | name == chooserName ->
              TypedLetStatement
                owner
                name
                spanValue
                (rewriteScheme schemeValue)
                (TypedLambdaExpr stagedChooserInfo parameterOwner parameterName body)
        _ -> statement
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe stagedChooserInfo) shape

closureShapeFlattenedRecipeLowererProgram :: TypedProgram
closureShapeFlattenedRecipeLowererProgram =
  rewriteRootRecipe
    ( expectedFunctionProgram
        []
        [boolCombineFunction {expectedFunctionShape = TypedClosureCallableShape}]
        (boolExpr True)
    )
  where
    flattenedInfo = functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo
    rewriteRootRecipe programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [TypedModule path source imports exports interface recursiveGroups (map rewriteStatement statements) moduleInfo]
            entryPath
        _ -> error "closure flattened-recipe lowerer fixture changed shape"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue ->
          TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body) ->
          TypedLetStatement
            owner
            name
            spanValue
            (rewriteScheme schemeValue)
            (TypedLambdaExpr flattenedInfo parameterOwner parameterName body)
        other -> other
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe flattenedInfo) shape

directShapeStagedRecipeLowererProgram :: TypedProgram
directShapeStagedRecipeLowererProgram =
  rewriteRootRecipe
    (expectedFunctionProgram [] [boolCombineFunction] (boolExpr True))
  where
    stagedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
        []
        []
    rewriteRootRecipe programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [TypedModule path source imports exports interface recursiveGroups (map rewriteStatement statements) moduleInfo]
            entryPath
        _ -> error "direct staged-recipe lowerer fixture changed shape"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue ->
          TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body) ->
          TypedLetStatement
            owner
            name
            spanValue
            (rewriteScheme schemeValue)
            (TypedLambdaExpr stagedInfo parameterOwner parameterName body)
        other -> other
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe stagedInfo) shape

variableBinderReferenceMismatchLowererProgram :: TypedProgram
variableBinderReferenceMismatchLowererProgram =
  case expectedFunctionProgram [] [boolIdentityFunction] (variableExpr "identity" boolCallableInfo) of
    TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
      TypedProgram
        prelude
        [TypedModule path source imports exports interface recursiveGroups (map corruptTerminal statements) moduleInfo]
        entryPath
    _ -> error "variable binder-reference lowerer fixture changed shape"
  where
    wrongBinder = TypedBinderId (modulePath, [999], resolvedName "identity")
    corruptTerminal statement =
      case statement of
        TypedExpressionStatement spanValue (TypedVariableExpr info name _) ->
          TypedExpressionStatement spanValue (TypedVariableExpr info name (Just wrongBinder))
        other -> other

duplicateParameterLowererProgram :: TypedProgram
duplicateParameterLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "chooseSecond"
        [("item", intInfo), ("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "item" intInfo)
    ]
    (directCall "chooseSecond" [intInfo, intInfo] intInfo [intExpr 1, intExpr 2])

selfRecursiveDuplicateParameterLowererProgram :: TypedProgram
selfRecursiveDuplicateParameterLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo), ("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        ( directCall
            "loop"
            [intInfo, intInfo]
            intInfo
            [variableExpr "item" intInfo, variableExpr "item" intInfo]
        )
    ]
    (directCall "loop" [intInfo, intInfo] intInfo [intExpr 1, intExpr 2])

duplicateFunctionLowererProgram :: TypedProgram
duplicateFunctionLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "identity"
        [("first", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "first" intInfo),
      ExpectedFunction
        "identity"
        [("second", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "second" intInfo)
    ]
    (directCall "identity" [intInfo] intInfo [intExpr 1])

invalidScalarBindingRhsProgram :: TypedProgram
invalidScalarBindingRhsProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedLetStatement
            seedBinder
            seedName
            (TypedSpan 1 1)
            seedScheme
            (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2)),
          TypedExpressionStatement (TypedSpan 2 1) (intExpr 1)
        ]
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing

combinedStatementFailureOrderLowererProgram :: TypedProgram
combinedStatementFailureOrderLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedLetStatement
            seedBinder
            seedName
            (TypedSpan 1 1)
            seedScheme
            (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2)),
          TypedLetStatement
            messageBinder
            messageName
            (TypedSpan 2 1)
            messageScheme
            (TypedLiteralExpr textInfo (TypedTextLiteral "later")),
          TypedExpressionStatement (TypedSpan 3 1) (boolExpr True)
        ]
        boolInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    messageName = resolvedName "message"
    messageBinder = TypedBinderId (modulePath, [1], messageName)
    messageScheme = TypedScheme messageBinder [] [] [] TypedTextType TypedManagedTextRecipe Nothing

recursionDescendantFailureOrderLowererProgram :: TypedProgram
recursionDescendantFailureOrderLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedRecursiveGroup [loopBinder]]
        ( scalarStatement
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 1 2 loopFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 4 1)
                   (bindExpectedExpressionVariables bindings (directCall "loop" [intInfo] intInfo [intExpr 1]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    loopName = resolvedName "loop"
    loopBinder = TypedBinderId (modulePath, [2], loopName)
    bindings = Map.fromList [(seedName, seedBinder), (loopName, loopBinder)]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    scalarStatement =
      [TypedLetStatement seedBinder seedName (TypedSpan 1 1) seedScheme (intExpr 1)]
    loopFunction =
      ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        ( binaryExpr
            intInfo
            "+"
            (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
            (variableExpr "seed" intInfo)
        )

interleavedCaptureMutualRecursiveLowererProgram :: TypedProgram
interleavedCaptureMutualRecursiveLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedRecursiveGroup [leftBinder, rightBinder]]
        ( map
            (bindExpectedStatementVariables bindings)
            (expectedFunctionStatements 0 1 leftFunction)
            <> [TypedLetStatement seedBinder seedName (TypedSpan 3 1) seedScheme (intExpr 1)]
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 3 4 rightFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 6 1)
                   (bindExpectedExpressionVariables bindings (directCall "left" [intInfo] intInfo [intExpr 1]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    leftName = resolvedName "left"
    leftBinder = TypedBinderId (modulePath, [1], leftName)
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [2], seedName)
    rightName = resolvedName "right"
    rightBinder = TypedBinderId (modulePath, [4], rightName)
    bindings =
      Map.fromList
        [ (leftName, leftBinder),
          (seedName, seedBinder),
          (rightName, rightBinder)
        ]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    leftFunction =
      ExpectedFunction
        "left"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        (directCall "right" [intInfo] intInfo [variableExpr "item" intInfo])
    rightFunction =
      ExpectedFunction
        "right"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        ( directCall
            "left"
            [intInfo]
            intInfo
            [binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo)]
        )

capturingLowererProgram :: TypedProgram
capturingLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        ( scalarStatement
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 1 2 addSeedFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 4 1)
                   (bindExpectedExpressionVariables bindings (directCall "addSeed" [intInfo] intInfo [intExpr 41]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    addSeedName = resolvedName "addSeed"
    addSeedBinder = TypedBinderId (modulePath, [2], addSeedName)
    bindings = Map.fromList [(seedName, seedBinder), (addSeedName, addSeedBinder)]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    scalarStatement =
      [TypedLetStatement seedBinder seedName (TypedSpan 1 1) seedScheme (intExpr 1)]
    addSeedFunction =
      ExpectedFunction
        "addSeed"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo))

closureShapedSelfRecursiveLowererProgram :: TypedProgram
closureShapedSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (boolExpr True)

directShapedClosureValueSelfRecursiveLowererProgram :: TypedProgram
directShapedClosureValueSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ applyFunction,
      closurePassingLoopFunction {expectedFunctionShape = TypedDirectCallableShape}
    ]
    (boolExpr True)

nestedLambdaClosureValueSelfRecursiveLowererProgram :: TypedProgram
nestedLambdaClosureValueSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [applyFunction, nestedLambdaClosurePassingLoopFunction]
    (boolExpr True)

shapeRejectedSelfRecursiveLowererProgram :: TypedProgram
shapeRejectedSelfRecursiveLowererProgram =
  shapeRejectedCycleLowererProgram [("loop", "loop")]

shapeRejectedMutualRecursiveLowererProgram :: TypedProgram
shapeRejectedMutualRecursiveLowererProgram =
  shapeRejectedCycleLowererProgram [("left", "right"), ("right", "left")]

shapeRejectedCycleLowererProgram :: [(Text, Text)] -> TypedProgram
shapeRejectedCycleLowererProgram functions =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [ TypedRecursiveGroup
            [ binders Map.! resolvedName name
            | (name, _) <- functions
            ]
        ]
        (concatMap functionStatements indexedFunctions <> [TypedExpressionStatement (TypedSpan (length functions * 2 + 1) 1) (boolExpr True)])
        boolInfo
    ]
    modulePath
  where
    indexedFunctions = zip [0 ..] functions
    binders =
      Map.fromList
        [ (resolvedName name, TypedBinderId (modulePath, [bindingIndex], resolvedName name))
        | (functionIndex, (name, _)) <- indexedFunctions,
          let bindingIndex = functionIndex * 2 + 1
        ]
    functionStatements (functionIndex, (name, target)) =
      let signatureIndex = functionIndex * 2
          bindingIndex = signatureIndex + 1
          function = ExpectedFunction name [("item", boolInfo)] boolInfo TypedDirectCallableShape (boolExpr True)
          functionName = resolvedName name
          signatureBinder = TypedBinderId (modulePath, [signatureIndex], functionName)
          bindingBinder = TypedBinderId (modulePath, [bindingIndex], functionName)
       in [ TypedSignatureStatement
              signatureBinder
              functionName
              (TypedSpan (signatureIndex + 1) 1)
              (functionScheme signatureIndex function),
            bindExpectedStatementVariables
              binders
              ( TypedLetStatement
                  bindingBinder
                  functionName
                  (TypedSpan (bindingIndex + 1) 1)
                  (functionScheme bindingIndex function)
                  (shapeRejectedConditionalBody bindingIndex target)
              )
          ]
    shapeRejectedConditionalBody statementIndex target =
      TypedIfExpr
        boolCallableInfo
        (boolExpr True)
        (branchLambda statementIndex 1 target)
        (branchLambda statementIndex 2 target)
    branchLambda statementIndex branchIndex target =
      let parameterName = resolvedName "item"
          parameterBinder = TypedBinderId (modulePath, [statementIndex, 0, branchIndex], parameterName)
       in TypedLambdaExpr
            boolCallableInfo
            parameterBinder
            parameterName
            (directCall target [boolInfo] boolInfo [TypedVariableExpr boolInfo parameterName (Just parameterBinder)])

shapeRejectedBinderShadowControlLowererProgram :: TypedProgram
shapeRejectedBinderShadowControlLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedSignatureStatement signatureBinder functionName (TypedSpan 1 1) (functionScheme 0 function),
          TypedLetStatement
            bindingBinder
            functionName
            (TypedSpan 2 1)
            (functionScheme 1 function)
            ( TypedIfExpr
                functionNodeInfo
                (boolExpr True)
                (branchLambda 1)
                (branchLambda 2)
            ),
          TypedExpressionStatement (TypedSpan 3 1) (boolExpr True)
        ]
        boolInfo
    ]
    modulePath
  where
    functionName = resolvedName "loop"
    function = ExpectedFunction "loop" [("loop", boolCallableInfo)] boolInfo TypedDirectCallableShape (boolExpr True)
    signatureBinder = TypedBinderId (modulePath, [0], functionName)
    bindingBinder = TypedBinderId (modulePath, [1], functionName)
    functionNodeInfo = functionInfo [("loop", boolCallableInfo)] boolInfo
    branchLambda branchIndex =
      let parameterBinder = TypedBinderId (modulePath, [1, 0, branchIndex], functionName)
       in TypedLambdaExpr
            functionNodeInfo
            parameterBinder
            functionName
            (TypedApplyExpr boolInfo (TypedVariableExpr boolCallableInfo functionName (Just parameterBinder)) (boolExpr True))

bareFunctionLowererProgram :: TypedProgram
bareFunctionLowererProgram =
  expectedFunctionProgram
    []
    [identityFunction]
    (TypedVariableExpr (functionInfo [("item", intInfo)] intInfo) (resolvedName "identity") Nothing)

partialCallLowererProgram :: TypedProgram
partialCallLowererProgram =
  expectedFunctionProgram
    []
    [combineFunction]
    ( TypedApplyExpr
        (functionInfo [("right", intInfo)] intInfo)
        (TypedVariableExpr (functionInfo [("left", intInfo), ("right", intInfo)] intInfo) (resolvedName "combine") Nothing)
        (intExpr 1)
    )

importedDirectCallLowererProgram :: TypedProgram
importedDirectCallLowererProgram =
  TypedProgram Nothing [providerModule, entry] modulePath
  where
    providerPath = ["Library", "Functions"]
    providerName = TypedResolvedName TypedCurrentModule TypedValueNamespace "foreign"
    importedName = TypedResolvedName (TypedImportedModule providerPath) TypedValueNamespace "foreign"
    providerOwner = TypedBinderId (providerPath, [0], providerName)
    providerParameterName = TypedResolvedName TypedCurrentModule TypedValueNamespace "item"
    providerParameterBinder = TypedBinderId (providerPath, [0, 0], providerParameterName)
    providerInfo = functionInfo [("item", intInfo)] intInfo
    providerScheme =
      TypedScheme
        providerOwner
        []
        []
        []
        (TypedFunctionType TypedIntType TypedIntType)
        (TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64))
        (Just TypedDirectCallableShape)
    providerModule =
      TypedModule
        providerPath
        (TypedSourcePath "src/Library/Functions.jz")
        []
        [TypedModuleExport TypedValueNamespace "foreign"]
        (TypedModuleInterface [TypedValueInterface providerName providerScheme] [] [] [])
        []
        [ TypedLetStatement
            providerOwner
            providerName
            (TypedSpan 1 1)
            providerScheme
            ( TypedLambdaExpr
                providerInfo
                providerParameterBinder
                providerParameterName
                (TypedVariableExpr intInfo providerParameterName (Just providerParameterBinder))
            )
        ]
        unitInfo
    callExpression =
      TypedApplyExpr
        intInfo
        (TypedVariableExpr providerInfo importedName (Just providerOwner))
        (intExpr 1)
    entry =
      TypedModule
        modulePath
        validSourcePath
        [TypedResolvedImport (TypedSpan 1 1) providerPath Nothing (Just ["foreign"])]
        []
        (TypedModuleInterface [] [] [] [])
        []
        [TypedExpressionStatement (TypedSpan 1 1) callExpression]
        intInfo

rejectedScalarFixtures :: [Fixture]
rejectedScalarFixtures = map fixtureByName ["text-value", "list-value", "non-unit-tuple", "data-value", "local-block-binding"]

producerEdgeFixtures :: [(Text, Fixture)]
producerEdgeFixtures =
  scalarBindingProducerFixtures
    <> managedTextProducerFixtures
    <> [ ( "scalar-pattern-case",
           sourceFixtureNoExports
             "scalar-pattern-case"
             "case True { | True -> 1 | _ -> 2 }."
         ),
         ( "scalar-pattern-case-variable-guards",
           sourceFixtureNoExports
             "scalar-pattern-case-variable-guards"
             "case 2 { | item if item > 2 -> item | fallback -> fallback + 1 }."
         ),
         ( "scalar-pattern-case-repeated-literal-guards",
           sourceFixtureNoExports
             "scalar-pattern-case-repeated-literal-guards"
             "case 2 { | 2 if False -> 10 | 2 if True -> 20 | _ -> 30 }."
         ),
         ( "scalar-pattern-case-capture",
           sourceFixtureNoExports
             "scalar-pattern-case-capture"
             ( Text.unlines
                 [ "seed = 40.",
                   "choose = \\(item) -> case item { | current if current > 0 -> current + seed | _ -> seed }.",
                   "choose 2."
                 ]
             )
         ),
         ( "scalar-pattern-case-closure-result",
           sourceFixtureNoExports
             "scalar-pattern-case-closure-result"
             ( Text.unlines
                 [ "choose = \\(flag) -> case flag { | True -> \\(item) -> item | _ -> \\(item) -> 0 }.",
                   "(choose True) 7."
                 ]
             )
         ),
         ( "scalar-pattern-case-tail-function",
           sourceFixtureNoExports
             "scalar-pattern-case-tail-function"
             ( Text.unlines
                 [ "loop :: Int -> Int.",
                   "loop = \\(item) -> case item { | 0 -> 0 | 1 -> loop 0 | next if next == 2 -> next | _ -> 3 }.",
                   "loop 1."
                 ]
             )
         ),
         ( "nested-tail-if-alternatives",
           sourceFixtureNoExports
             "nested-tail-if-alternatives"
             ( Text.unlines
                 [ "chooseNestedIf :: Bool -> Bool -> Int -> Int.",
                   "chooseNestedIf = \\(outer, inner, item) -> if outer then (if inner then item else 1) else (case item { | 0 -> 2 | _ -> 3 }).",
                   "chooseNestedIf True False 9."
                 ]
             )
         ),
         ( "nested-tail-case-bodies",
           sourceFixtureNoExports
             "nested-tail-case-bodies"
             ( Text.unlines
                 [ "chooseNestedCase :: Int -> Bool -> Int.",
                   "chooseNestedCase = \\(item, flag) -> case item { | 0 -> if flag then 1 else 2 | _ -> case item { | 1 -> 3 | _ -> 4 } }.",
                   "chooseNestedCase 0 True."
                 ]
             )
         ),
         ( "pattern-case-in-conditional-branch",
           sourceFixtureNoExports
             "pattern-case-in-conditional-branch"
             "if True then case 1 { | 1 -> 10 | _ -> 20 } else 30."
         ),
         ( "conditional-in-pattern-case-guard",
           sourceFixtureNoExports
             "conditional-in-pattern-case-guard"
             "case 1 { | 1 if if True then False else True -> 10 | _ -> 20 }."
         ),
         ( "pattern-case-in-pattern-case-body",
           sourceFixtureNoExports
             "pattern-case-in-pattern-case-body"
             "case True { | True -> case 1 { | 1 -> 10 | _ -> 20 } | _ -> 30 }."
         ),
         ( "pattern-case-scrutinee-pattern-case",
           sourceFixtureNoExports
             "pattern-case-scrutinee-pattern-case"
             "case (case True { | True -> 1 | _ -> 2 }) { | 1 -> 10 | _ -> 20 }."
         ),
         ( "pattern-case-ambient-scalar",
           sourceFixtureNoExports
             "pattern-case-ambient-scalar"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "seed = identity 1.",
                   "chosen = case seed { | item if item == seed -> item | _ -> 0 }.",
                   "chosen + seed."
                 ]
             )
         ),
         ( "pattern-case-captured-scalar",
           sourceFixtureNoExports
             "pattern-case-captured-scalar"
             ( Text.unlines
                 [ "seed = 1.",
                   "choose = \\(item) -> case item { | current if current == seed -> current | _ -> seed }.",
                   "choose seed."
                 ]
             )
         ),
         ( "pattern-case-call-argument",
           sourceFixtureNoExports
             "pattern-case-call-argument"
             "(\\(item) -> item) (case 1 { | 1 -> 2 | _ -> 3 })."
         ),
         ( "pattern-case-final-guarded-catch-all",
           sourceFixtureNoExports
             "pattern-case-final-guarded-catch-all"
             "case True { | _ if True -> 1 }."
         ),
         ( "pattern-case-missing-final-catch-all",
           sourceFixtureNoExports
             "pattern-case-missing-final-catch-all"
             "case True { | True -> 1 }."
         ),
         ( "pattern-case-unguarded-non-final-wildcard",
           sourceFixtureNoExports
             "pattern-case-unguarded-non-final-wildcard"
             "case True { | _ -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-unguarded-non-final-variable",
           sourceFixtureNoExports
             "pattern-case-unguarded-non-final-variable"
             "case True { | item -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-managed-scrutinee",
           sourceFixtureNoExports
             "pattern-case-managed-scrutinee"
             "case \"managed\" { | _ -> 1 }."
         ),
         ( "pattern-case-constructor-pattern",
           sourceFixtureNoExports
             "pattern-case-constructor-pattern"
             "data Maybe = Nothing. case Nothing { | Nothing -> 1 }."
         ),
         ( "pattern-case-list-pattern",
           sourceFixtureNoExports
             "pattern-case-list-pattern"
             "case [1] { | [1] -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-tuple-pattern",
           sourceFixtureNoExports
             "pattern-case-tuple-pattern"
             "case (True, False) { | (True, False) -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-as-pattern",
           sourceFixtureNoExports
             "pattern-case-as-pattern"
             "case True { | whole @ True -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-or-pattern",
           sourceFixtureNoExports
             "pattern-case-or-pattern"
             "case True { | True | False -> 1 }."
         ),
         ( "pattern-case-non-bool-guard",
           sourceFixtureNoExports
             "pattern-case-non-bool-guard"
             "case True { | True if 1 -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-incompatible-arm-results",
           sourceFixtureNoExports
             "pattern-case-incompatible-arm-results"
             "case True { | True -> 1 | _ -> False }."
         ),
         ("conditional-function-parameter", sourceFixtureNoExports "conditional-function-parameter" conditionalFunctionParameterSource),
         ("conditional-captured-scalar", sourceFixtureNoExports "conditional-captured-scalar" conditionalCapturedScalarSource),
         ("conditional-tail-call-function", sourceFixtureNoExports "conditional-tail-call-function" conditionalTailCallFunctionSource),
         ("conditional-closure-result-application", sourceFixtureNoExports "conditional-closure-result-application" conditionalClosureResultApplicationSource),
         ("nested-conditionals", sourceFixtureNoExports "nested-conditionals" nestedConditionalsSource),
         ("empty-module", sourceFixtureNoExports "empty-module" ""),
         ( "default-exported-polymorphic-callable",
           sourceFixture
             "default-exported-polymorphic-callable"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "identity :: a -> a.",
                   "identity = \\(item) -> item.",
                   "()."
                 ]
             )
         ),
         ( "self-recursive-function-rebinding",
           sourceFixtureNoExports
             "self-recursive-function-rebinding"
             ( Text.unlines
                 [ "loop :: Int -> Int.",
                   "loop = \\(item) -> loop item.",
                   "loop :: Int -> Int.",
                   "loop = \\(item) -> loop (if True then item else item).",
                   "loop 1."
                 ]
             )
         ),
         ( "later-callable-rebinding-calls-nearest-prior",
           sourceFixtureNoExports
             "later-callable-rebinding-calls-nearest-prior"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> identity item.",
                   "identity True."
                 ]
             )
         ),
         ( "intervening-scalar-canonical-ownership",
           sourceFixtureNoExports
             "intervening-scalar-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "multiple-intervening-scalars-canonical-ownership",
           sourceFixtureNoExports
             "multiple-intervening-scalars-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "a = False.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "interleaved-callable-scalar-canonical-ownership",
           sourceFixtureNoExports
             "interleaved-callable-scalar-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = False.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "three-same-name-nearest-prior-mutual-recursion",
           sourceFixtureNoExports
             "three-same-name-nearest-prior-mutual-recursion"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> peer item.",
                   "peer :: Bool -> Bool.",
                   "peer = \\(item) -> identity item.",
                   "True."
                 ]
             )
         ),
         ( "canonical-self-recursion-no-prior",
           sourceFixtureNoExports
             "canonical-self-recursion-no-prior"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> loop item.",
                   "True."
                 ]
             )
         ),
         ( "canonical-mutual-recursion-peers",
           sourceFixtureNoExports
             "canonical-mutual-recursion-peers"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> left item.",
                   "True."
                 ]
             )
         ),
         ( "nested-lambda-direct-recursion",
           sourceFixtureNoExports
             "nested-lambda-direct-recursion"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> apply (\\(nested) -> loop nested).",
                   "loop False."
                 ]
             )
         ),
         ( "nearest-rebinding-mutual-control",
           sourceFixtureNoExports
             "nearest-rebinding-mutual-control"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> left item.",
                   "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "True."
                 ]
             )
         ),
         ( "rebinding-parameter-shadow-control",
           sourceFixtureNoExports
             "rebinding-parameter-shadow-control"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(apply) -> apply True.",
                   "True."
                 ]
             )
         ),
         ( "rebinding-local-shadow-control",
           sourceFixtureNoExports
             "rebinding-local-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-self-alias-recursion",
           sourceFixtureNoExports
             "rejected-self-alias-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = loop.",
                   "True."
                 ]
             )
         ),
         ( "rejected-mutual-alias-recursion",
           sourceFixtureNoExports
             "rejected-mutual-alias-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = right.",
                   "right :: Bool -> Bool.",
                   "right = left.",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-conditional-mutual-recursion",
           sourceFixtureNoExports
             "rejected-alias-conditional-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = right.",
                   "right :: Bool -> Bool.",
                   "right = if True then left else left.",
                   "True."
                 ]
             )
         ),
         ( "rejected-operator-alias-self-recursion",
           sourceFixtureNoExports
             "rejected-operator-alias-self-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = (%%).",
                   "0."
                 ]
             )
         ),
         ( "rejected-eager-operator-conditional-control",
           sourceFixtureNoExports
             "rejected-eager-operator-conditional-control"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Bool -> Bool -> Bool.",
                   "(%%) = if True %% False then (%%) else (%%).",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-parameter-shadow-control",
           sourceFixtureNoExports
             "rejected-alias-parameter-shadow-control"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = (\\(loop) -> loop) identity.",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-local-shadow-control",
           sourceFixtureNoExports
             "rejected-alias-local-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = {",
                   "  loop :: Bool -> Bool.",
                   "  loop = \\(item) -> item.",
                   "  loop.",
                   "}.",
                   "True."
                 ]
             )
         ),
         ( "rejected-eager-self-before-callable-result-control",
           sourceFixtureNoExports
             "rejected-eager-self-before-callable-result-control"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = { f True. \\(x) -> x. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-nearest-prior-callable-rebinding-recursion",
           sourceFixtureNoExports
             "rejected-block-nearest-prior-callable-rebinding-recursion"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = { inner :: Bool -> Bool. inner = \\(x) -> f x. inner = inner. inner. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-conditional-self-recursion",
           sourceFixtureNoExports
             "rejected-conditional-self-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> if item then loop False else item.",
                   "loop True."
                 ]
             )
         ),
         ( "rejected-block-conditional-mutual-recursion",
           sourceFixtureNoExports
             "rejected-block-conditional-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> { right item. }.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> if item then left False else item.",
                   "left True."
                 ]
             )
         ),
         ( "rejected-block-parameter-shadow-control",
           sourceFixtureNoExports
             "rejected-block-parameter-shadow-control"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "forward :: (Bool -> Bool) -> Bool.",
                   "forward = \\(forward) -> { apply forward. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-later-shadow-control",
           sourceFixtureNoExports
             "rejected-block-later-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop item. loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-initializer-self-recursion",
           sourceFixtureNoExports
             "rejected-block-initializer-self-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop = loop item. item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-initializer-mutual-recursion",
           sourceFixtureNoExports
             "rejected-block-initializer-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> { right = right item. item. }.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = left item. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-prior-outer-alias-mutual-recursion",
           sourceFixtureNoExports
             "nested-prior-outer-alias-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = left. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-prior-outer-conditional-alias-mutual-recursion",
           sourceFixtureNoExports
             "nested-prior-outer-conditional-alias-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = if item then left else left. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-self-recursive-lambda-local-ownership",
           sourceFixtureNoExports
             "nested-self-recursive-lambda-local-ownership"
             ( Text.unlines
                 [ "owner :: Bool -> Bool.",
                   "owner = \\(item) -> { loop = \\(nested) -> loop nested. item. }.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> owner item.",
                   "True."
                 ]
             )
         ),
         ( "accepted-then-rejected-callable-rebinding",
           sourceFixtureNoExports
             "accepted-then-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-recursive-callable-rebinding-order",
           sourceFixtureNoExports
             "rejected-recursive-callable-rebinding-order"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> g item else \\(item) -> g item.",
                   "g :: Bool -> Bool.",
                   "g = \\(item) -> f item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-then-accepted-callable-rebinding",
           sourceFixtureNoExports
             "rejected-then-accepted-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "repeated-rejected-callable-rebinding",
           sourceFixtureNoExports
             "repeated-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if False then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "scalar-then-rejected-callable-control",
           sourceFixtureNoExports
             "scalar-then-rejected-callable-control"
             ( Text.unlines
                 [ "f = True.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "accepted-scalar-rejected-callable-rebinding",
           sourceFixtureNoExports
             "accepted-scalar-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f = True.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-scalar-accepted-callable-rebinding",
           sourceFixtureNoExports
             "rejected-scalar-accepted-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f = True.",
                   "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-later-signed-shadow-control",
           sourceFixtureNoExports
             "rejected-block-later-signed-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> {",
                   "  observed = loop item.",
                   "  loop :: Bool -> Bool.",
                   "  loop = \\(nested) -> nested.",
                   "  loop item.",
                   "}.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-local-shadow-cycle-control",
           sourceFixtureNoExports
             "rejected-block-local-shadow-cycle-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> forward item.",
                   "forward :: Bool -> Bool.",
                   "forward = \\(item) -> { loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-parameter-shadow-cycle-control",
           sourceFixtureNoExports
             "rejected-block-parameter-shadow-cycle-control"
             ( Text.unlines
                 [ "forward :: (Bool -> Bool) -> Bool.",
                   "forward = \\(loop) -> { loop True. }.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> forward identity.",
                   "True."
                 ]
             )
         ),
         ( "rejected-operator-value-self-recursion",
           sourceFixtureNoExports
             "rejected-operator-value-self-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> (%%) left right.",
                   "0."
                 ]
             )
         ),
         ( "rejected-infix-operator-mutual-recursion",
           sourceFixtureNoExports
             "rejected-infix-operator-mutual-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "operator ~~ tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left ~~ right.",
                   "(~~) :: Int -> Int -> Int.",
                   "(~~) = \\(left, right) -> left %% right.",
                   "0."
                 ]
             )
         ),
         ( "rejected-section-operator-mutual-recursion",
           sourceFixtureNoExports
             "rejected-section-operator-mutual-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "operator ~~ tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> (left ~~) right.",
                   "(~~) :: Int -> Int -> Int.",
                   "(~~) = \\(left, right) -> (%% right) left.",
                   "0."
                 ]
             )
         ),
         ( "unit-forward-function",
           sourceFixtureNoExports
             "unit-forward-function"
             ( Text.unlines
                 [ "first :: () -> ().",
                   "first = \\(item) -> second item.",
                   "second :: () -> ().",
                   "second = \\(item) -> item.",
                   "first ()."
                 ]
             )
         ),
         ( "curried-first-argument-capture",
           sourceFixtureNoExports
             "curried-first-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "use :: Int -> Int.",
                   "use = \\(item) -> combine seed item.",
                   "use 1."
                 ]
             )
         ),
         ( "partial-call-argument-capture",
           sourceFixtureNoExports
             "partial-call-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine seed."
                 ]
             )
         ),
         ( "partial-call-managed-argument-failure",
           sourceFixtureNoExports
             "partial-call-managed-argument-failure"
             ( Text.unlines
                 [ "keepRight :: Text -> Int -> Int.",
                   "keepRight = \\(ignored, right) -> right.",
                   "keepRight \"managed\"."
                 ]
             )
         ),
         ( "closure-use-argument-failure-order",
           sourceFixtureNoExports
             "closure-use-argument-failure-order"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "apply :: (Int -> Int) -> Int.",
                   "apply = \\(function) -> function seed.",
                   "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "apply identity.",
                   "[1]."
                 ]
             )
         ),
         ( "non-local-call-argument-capture",
           sourceFixtureNoExports
             "non-local-call-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "__kernel_toFloat64 seed."
                 ]
             )
         ),
         ( "higher-order-parameter",
           sourceFixtureNoExports
             "higher-order-parameter"
             ( Text.unlines
                 [ "ignore :: (Int -> Int) -> Int.",
                   "ignore = \\(function) -> 1.",
                   "1."
                 ]
             )
         ),
         ( "narrow-literal-direct-call",
           sourceFixtureNoExports
             "narrow-literal-direct-call"
             ( Text.unlines
                 [ "narrowIdentity :: Int8 -> Int8.",
                   "narrowIdentity = \\(item) -> item.",
                   "narrowIdentity 1."
                 ]
             )
         ),
         ( "narrow-composite-function-result",
           sourceFixtureNoExports
             "narrow-composite-function-result"
             ( Text.unlines
                 [ "narrowSum :: Bool -> Int8.",
                   "narrowSum = \\(ignored) -> 1 + 2.",
                   "narrowSum True."
                 ]
             )
         ),
         ( "narrow-comparison-operand",
           sourceFixtureNoExports
             "narrow-comparison-operand"
             ( Text.unlines
                 [ "isSmall :: Int8 -> Bool.",
                   "isSmall = \\(item) -> item < 2.",
                   "isSmall 1."
                 ]
             )
         ),
         ( "narrow-root-binary-direct-call",
           sourceFixtureNoExports
             "narrow-root-binary-direct-call"
             ( Text.unlines
                 [ "narrowIdentity :: Int8 -> Int8.",
                   "narrowIdentity = \\(item) -> item.",
                   "narrowIdentity 1 + 2."
                 ]
             )
         ),
         ( "equivalent-scalar-alias-specialization",
           sourceFixtureNoExports
             "equivalent-scalar-alias-specialization"
             ( Text.unlines
                 [ "asInt :: Bool -> Int.",
                   "asInt = \\(ignored) -> 1.",
                   "asInt64 :: Bool -> Int64.",
                   "asInt64 = \\(flag) -> asInt flag.",
                   "acceptInt64 :: Int64 -> Int64.",
                   "acceptInt64 = \\(item) -> item.",
                   "useInt64 :: Bool -> Int64.",
                   "useInt64 = \\(flag) -> acceptInt64 (asInt flag).",
                   "asFloat :: Bool -> Float.",
                   "asFloat = \\(ignored) -> 1.5.",
                   "asFloat64 :: Bool -> Float64.",
                   "asFloat64 = \\(flag) -> asFloat flag.",
                   "acceptFloat64 :: Float64 -> Float64.",
                   "acceptFloat64 = \\(item) -> item.",
                   "acceptFloat64 (asFloat True)."
                 ]
             )
         ),
         ( "earlier-caller-transitive-recursive-capture",
           sourceFixtureNoExports
             "earlier-caller-transitive-recursive-capture"
             ( Text.unlines
                 [ "caller :: Int -> Int.",
                   "caller = \\(item) -> loop item.",
                   "seed = 1.",
                   "loop :: Int -> Int.",
                   "loop = \\(item) -> loop seed.",
                   "caller 1."
                 ]
             )
         ),
         ( "unused-user-defined-operator",
           sourceFixtureNoExports
             "unused-user-defined-operator"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left + right.",
                   "()."
                 ]
             )
         ),
         ( "root-data-failure-accumulation",
           sourceFixtureNoExports
             "root-data-failure-accumulation"
             ( Text.unlines
                 [ "[1].",
                   "data Box = Box.",
                   "()."
                 ]
             )
         ),
         ( "anonymous-lambda-result",
           sourceFixtureNoExports
             "anonymous-lambda-result"
             "\\(flag) -> flag == True."
         ),
         ( "inline-anonymous-lambda-call",
           sourceFixtureNoExports
             "inline-anonymous-lambda-call"
             "(\\(item) -> item + 1) 41."
         ),
         ( "curried-partial-application",
           sourceFixtureNoExports
             "curried-partial-application"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine 1."
                 ]
             )
         ),
         ( "curried-callable-oversaturation",
           sourceFixtureNoExports
             "curried-callable-oversaturation"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "choose :: Bool -> (Int -> Int).",
                   "choose = \\(ignored) -> identity.",
                   "choose False 2."
                 ]
             )
         ),
         ( "curried-partial-higher-order-consumer",
           sourceFixtureNoExports
             "curried-partial-higher-order-consumer"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "apply :: (Int -> Int) -> Int.",
                   "apply = \\(function) -> function 2.",
                   "apply (combine 1)."
                 ]
             )
         ),
         ( "inline-curried-lambda-call",
           sourceFixtureNoExports
             "inline-curried-lambda-call"
             "(\\(left, right) -> left + right) 20 22."
         ),
         ( "curried-named-function-value",
           sourceFixtureNoExports
             "curried-named-function-value"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine."
                 ]
             )
         ),
         ( "non-callable-oversaturation-diagnostic",
           sourceFixtureNoExports
             "non-callable-oversaturation-diagnostic"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "identity 1 2."
                 ]
             )
         ),
         ( "nested-scalar-capture",
           sourceFixtureNoExports
             "nested-scalar-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "\\(outer) -> \\(item) -> item + outer + seed."
                 ]
             )
         ),
         ( "nested-shadow-capture-order",
           sourceFixtureNoExports
             "nested-shadow-capture-order"
             ( Text.unlines
                 [ "left :: Int.",
                   "left = 10.",
                   "right :: Int.",
                   "right = 20.",
                   "\\(outer) -> \\(left) -> right + outer + left + right."
                 ]
             )
         ),
         ( "nested-closure-valued-capture",
           sourceFixtureNoExports
             "nested-closure-valued-capture"
             "\\(predicate) -> \\(item) -> predicate (item == True) == True."
         ),
         ( "named-nested-captured-closure-call",
           sourceFixtureNoExports
             "named-nested-captured-closure-call"
             ( Text.unlines
                 [ "seed :: Bool.",
                   "seed = True.",
                   "makePredicate :: (Bool -> Bool) -> Bool -> Bool.",
                   "makePredicate = \\(predicate) -> \\(item) -> predicate item == seed.",
                   "True."
                 ]
             )
         ),
         ( "transitive-named-closure-capture",
           sourceFixtureNoExports
             "transitive-named-closure-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "addSeed :: Int -> Int.",
                   "addSeed = \\(item) -> item + seed.",
                   "callAddSeed :: Int -> Int.",
                   "callAddSeed = \\(item) -> addSeed item.",
                   "callAddSeed 41."
                 ]
             )
         ),
         ( "unsupported-managed-capture",
           sourceFixtureNoExports
             "unsupported-managed-capture"
             ( Text.unlines
                 [ "message = \"managed\".",
                   "check :: Bool -> Bool.",
                   "check = \\(ignored) -> message == message.",
                   "()."
                 ]
             )
         ),
         ( "signed-function-only",
           sourceFixtureNoExports
             "signed-function-only"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item."
                 ]
             )
         ),
         ( "missing-result-failure-accumulation",
           sourceFixtureNoExports
             "missing-result-failure-accumulation"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "addSeed :: Int -> Int.",
                   "addSeed = \\(item) -> item + seed."
                 ]
             )
         ),
         ( "nested-unsupported-children",
           sourceFixtureNoExports
             "nested-unsupported-children"
             "if True then [1] else [2]."
         ),
         ( "pattern-case-unsupported-children",
           sourceFixtureNoExports
             "pattern-case-unsupported-children"
             "case [1] { | _ -> [2] }."
         ),
         ( "guarded-pattern-case-unsupported-children",
           sourceFixtureNoExports
             "guarded-pattern-case-unsupported-children"
             "case [1] { | _ if { ignored = [2]. True. } -> [3] | _ -> [4] }."
         ),
         ( "nested-block-unsupported-child",
           sourceFixtureNoExports
             "nested-block-unsupported-child"
             "{ ignored = [1]. [2]. }."
         ),
         ( "unsupported-binary-child",
           sourceFixtureNoExports
             "unsupported-binary-child"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left + right.",
                   "(if True then 1 else 2) %% (if True then 3 else 4)."
                 ]
             )
         ),
         ( "left-section-unsupported-child",
           sourceFixtureNoExports
             "left-section-unsupported-child"
             "((if True then 1 else 2) +)."
         ),
         ( "right-section-unsupported-child",
           sourceFixtureNoExports
             "right-section-unsupported-child"
             "(+ (if True then 1 else 2))."
         ),
         ( "type-application-composite",
           sourceFixtureNoExports
             "type-application-composite"
             ( Text.unlines
                 [ "identity :: a -> a.",
                   "identity = \\(item) -> item.",
                   "identity @Int 1."
                 ]
             )
         ),
         ( "signed-function-rebinding",
           sourceFixtureNoExports
             "signed-function-rebinding"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "identity :: Int -> Int.",
                   "identity = \\(item) -> item + 1.",
                   "identity 1."
                 ]
             )
         ),
         ( "duplicate-leading-parameters",
           sourceFixtureNoExports
             "duplicate-leading-parameters"
             ( Text.unlines
                 [ "chooseSecond :: Int -> Int -> Int.",
                   "chooseSecond = \\(item, item) -> item.",
                   "chooseSecond 1 2."
                 ]
             )
         ),
         ( "curried-shadowed-parameter",
           sourceFixtureNoExports
             "curried-shadowed-parameter"
             ( Text.unlines
                 [ "chooseSecond :: Int -> Int -> Int.",
                   "chooseSecond = \\(item) -> \\(item) -> item.",
                   "chooseSecond 1 2."
                 ]
             )
         ),
         ( "out-of-range-signed-function-literal",
           sourceFixtureNoExports
             "out-of-range-signed-function-literal"
             ( Text.unlines
                 [ "invalid :: Bool -> Int8.",
                   "invalid = \\(ignored) -> 999.",
                   "invalid True."
                 ]
             )
         ),
         ( "class-impl-declarations",
           sourceFixtureNoExports
             "class-impl-declarations"
             ( Text.unlines
                 [ "class Marker(a) { }.",
                   "impl Marker(Int) { }.",
                   "1."
                 ]
             )
         ),
         ( "impl-method-profile-failure",
           sourceFixtureNoExports
             "impl-method-profile-failure"
             ( Text.unlines
                 [ "class Items(a) { items :: a -> [Int]. }.",
                   "impl Items(Int) { items = \\(item) -> [item]. }.",
                   "()."
                 ]
             )
         ),
         ( "unsupported-binding-child-failure",
           sourceFixtureNoExports
             "unsupported-binding-child-failure"
             ( Text.unlines
                 [ "seed = [1].",
                   "()."
                 ]
             )
         ),
         ( "invalid-forward-signed-function",
           sourceFixtureNoExports
             "invalid-forward-signed-function"
             ( Text.unlines
                 [ "first :: Int -> Int.",
                   "first = \\(item) -> later item.",
                   "later :: Int -> Int.",
                   "later = \\(item) -> item True.",
                   "first 1."
                 ]
             )
         ),
         ( "qualified-method-profile-rejection",
           sourceFixtureNoExports
             "qualified-method-profile-rejection"
             ( Text.unlines
                 [ "class Choice(a) { pick :: a -> Bool. }.",
                   "impl Choice(Int) { pick = \\(candidate) -> True. }.",
                   "impl Choice(Bool) { pick = \\(candidate) -> False. }.",
                   "Choice::pick 1."
                 ]
             )
         ),
         ( "out-of-range-default-integer",
           sourceFixtureNoExports
             "out-of-range-default-integer"
             "9223372036854775808."
         ),
         ( "out-of-range-default-integer-binary",
           sourceFixtureNoExports
             "out-of-range-default-integer-binary"
             "9223372036854775807 + 1."
         ),
         ( "integer-literal-float64-promotion",
           sourceFixtureNoExports
             "integer-literal-float64-promotion"
             "1 + 2.0."
         ),
         ( "integer-literal-float64-equality",
           sourceFixtureNoExports
             "integer-literal-float64-equality"
             "1 == 2.0."
         ),
         ( "signed-parameter-float64-promotion",
           sourceFixtureNoExports
             "signed-parameter-float64-promotion"
             ( Text.unlines
                 [ "promote :: Int -> Float64 -> Float64.",
                   "promote = \\(whole, fractional) -> whole + fractional.",
                   "promote 1 2.0."
                 ]
             )
         )
       ]

explicitNumericTypes :: [Text]
explicitNumericTypes =
  [ "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "Float16",
    "Float32",
    "Float64"
  ]

sourceFixture :: Text -> Text -> Fixture
sourceFixture name source =
  Fixture
    name
    emptyInputs
    validSourcePath
    (Map.singleton sourceFilePath source)

sourceFixtureWithFiles :: Text -> InferenceInputs -> Text -> Map.Map FilePath Text -> Fixture
sourceFixtureWithFiles name inputs source additionalSources =
  Fixture
    name
    inputs
    validSourcePath
    (Map.insert sourceFilePath source additionalSources)

sourceFixtureNoExports :: Text -> Text -> Fixture
sourceFixtureNoExports name source =
  sourceFixture name (emptyExportModuleSource source)

resolveFixture :: Fixture -> IO (Either Diagnostic ModuleGraph.ResolvedModule)
resolveFixture fixture =
  resolveFixtureWithLookup fixture (pure . (`Map.lookup` fixtureSourceFiles fixture))

resolveFixtureWithLookup :: Fixture -> (FilePath -> IO (Maybe Text)) -> IO (Either Diagnostic ModuleGraph.ResolvedModule)
resolveFixtureWithLookup fixture loadSource =
  fmap (fmap resolverEntryModule) $
    resolveProgram
      fixtureResolverConfig
      (inferenceBuiltinMode (fixtureInputs fixture))
      Set.empty
      Set.empty
      loadSource
      modulePath
  where
    resolverEntryModule program =
      case filter ((== modulePath) . ModuleGraph.resolvedModulePath) (ModuleGraph.resolvedProgramModules program) of
        [resolvedModule] -> resolvedModule
        _ -> error "typed-core fixture resolver did not produce one entry module"

fixtureResolverConfig :: ModuleResolutionConfig
fixtureResolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

sourceFilePath :: FilePath
sourceFilePath = "src/App/Main.jz"

emptyExportModuleSource :: Text -> Text
emptyExportModuleSource source =
  "module App::Main () {\n" <> source <> "\n}"

fixtureByName :: Text -> Fixture
fixtureByName name =
  case filter ((== name) . fixtureName) fixtures of
    [fixture] -> fixture
    _ -> error ("typed-core fixture is missing or duplicated: " <> Text.unpack name)

explicitNumericWidthsSource :: Text
explicitNumericWidthsSource =
  Text.unlines
    [ "asInt8 :: Bool -> Int8.",
      "asInt8 = \\(ignored) -> 1.",
      "asInt16 :: Bool -> Int16.",
      "asInt16 = \\(ignored) -> 2.",
      "asInt32 :: Bool -> Int32.",
      "asInt32 = \\(ignored) -> 3.",
      "asInt64 :: Bool -> Int64.",
      "asInt64 = \\(ignored) -> 4.",
      "asUInt8 :: Bool -> UInt8.",
      "asUInt8 = \\(ignored) -> 5.",
      "asUInt16 :: Bool -> UInt16.",
      "asUInt16 = \\(ignored) -> 6.",
      "asUInt32 :: Bool -> UInt32.",
      "asUInt32 = \\(ignored) -> 7.",
      "asUInt64 :: Bool -> UInt64.",
      "asUInt64 = \\(ignored) -> 8.",
      "asFloat16 :: Bool -> Float16.",
      "asFloat16 = \\(ignored) -> 1.5.",
      "asFloat32 :: Bool -> Float32.",
      "asFloat32 = \\(ignored) -> 2.5.",
      "asFloat64 :: Bool -> Float64.",
      "asFloat64 = \\(ignored) -> 3.5.",
      "()."
    ]

scalarParameterReturnSource :: Text
scalarParameterReturnSource =
  Text.unlines
    [ "identity :: Int -> Int.",
      "identity = \\(item) -> item.",
      "identity 42."
    ]

singleArgumentDirectCallSource :: Text
singleArgumentDirectCallSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "increment 41."
    ]

curriedMultiArgumentDirectCallSource :: Text
curriedMultiArgumentDirectCallSource =
  Text.unlines
    [ "combine :: Int -> Int -> Int.",
      "combine = \\(left, right) -> left + right.",
      "combine 20 22."
    ]

threeArgumentDirectCallSource :: Text
threeArgumentDirectCallSource =
  Text.unlines
    [ "sumThree :: Int -> Int -> Int -> Int.",
      "sumThree = \\(first, second, third) -> first + second + third.",
      "sumThree 10 20 12."
    ]

forwardDirectCallDagSource :: Text
forwardDirectCallDagSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> second item.",
      "second :: Int -> Int.",
      "second = \\(item) -> item + 1.",
      "first 41."
    ]

ordinaryUnsignedForwardCallerSource :: Text
ordinaryUnsignedForwardCallerSource =
  Text.unlines
    [ "caller = \\(item) -> later item.",
      "later :: Int -> Int.",
      "later = \\(item) -> item.",
      "caller 1."
    ]

forwardPolymorphicFunctionSource :: Text
forwardPolymorphicFunctionSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later :: a -> a.",
      "later = \\(item) -> item.",
      "first 1."
    ]

forwardConstrainedFunctionSource :: Text
forwardConstrainedFunctionSource =
  Text.unlines
    [ "class Eq(a) { }.",
      "impl Eq(Int) { }.",
      "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later :: @{Eq(Int)}: Int -> Int.",
      "later = \\(item) -> item.",
      "first 1."
    ]

forwardSignedScalarSource :: Text
forwardSignedScalarSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> item + later.",
      "later :: Int.",
      "later = 1.",
      "first 1."
    ]

forwardUnsignedLambdaSource :: Text
forwardUnsignedLambdaSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later = \\(item) -> item.",
      "first 1."
    ]

nestedForwardSignedFunctionSource :: Text
nestedForwardSignedFunctionSource =
  Text.unlines
    [ "{",
      "  caller :: Int -> Int.",
      "  caller = \\(item) -> later True.",
      "  later :: Int -> Int.",
      "  later = \\(item) -> item.",
      "  caller 1.",
      "}."
    ]

nestedDirectCallsSource :: Text
nestedDirectCallsSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "double :: Int -> Int.",
      "double = \\(item) -> item + item.",
      "double (increment 20)."
    ]

dollarDirectCallSource :: Text
dollarDirectCallSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "increment $ 41."
    ]

exportedDirectFunctionSource :: Text
exportedDirectFunctionSource =
  "module App::Main (value increment) {\n"
    <> singleArgumentDirectCallSource
    <> "}\n"

namedFunctionValueSource :: Text
namedFunctionValueSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "identity."
    ]

higherOrderCallSource :: Text
higherOrderCallSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

closureResultSource :: Text
closureResultSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "choose :: Bool -> Bool -> Bool.",
      "choose = \\(ignored) -> identity.",
      "choose False."
    ]

callableParameterShadowsNamedFunctionSource :: Text
callableParameterShadowsNamedFunctionSource =
  Text.unlines
    [ "combine :: Bool -> Bool -> Bool.",
      "combine = \\(left, right) -> left.",
      "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(combine) -> combine True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

callableParameterShadowsEnclosingFunctionSource :: Text
callableParameterShadowsEnclosingFunctionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(apply) -> apply True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

mixedDirectAndValueUseSource :: Text
mixedDirectAndValueUseSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity == identity True."
    ]

callableParameterValueShadowsEnclosingFunctionSource :: Text
callableParameterValueShadowsEnclosingFunctionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "forward :: (Bool -> Bool) -> Bool.",
      "forward = \\(forward) -> apply forward.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "forward identity."
    ]

partialDirectCallSource :: Text
partialDirectCallSource =
  Text.unlines
    [ "combine :: Int -> Int -> Int.",
      "combine = \\(left, right) -> left + right.",
      "combine 1."
    ]

oversaturatedDirectCallSource :: Text
oversaturatedDirectCallSource =
  Text.unlines
    [ "makeAdder :: Int -> Int -> Int.",
      "makeAdder = \\(left) -> (left +).",
      "makeAdder 1 2."
    ]

capturingFunctionSource :: Text
capturingFunctionSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 1.",
      "addSeed :: Int -> Int.",
      "addSeed = \\(item) -> item + seed.",
      "addSeed 41."
    ]

selfRecursiveFunctionSource :: Text
selfRecursiveFunctionSource =
  Text.unlines
    [ "loop :: Int -> Int.",
      "loop = \\(item) -> loop item.",
      "loop 1."
    ]

mutuallyRecursiveFunctionsSource :: Text
mutuallyRecursiveFunctionsSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "right :: Int -> Int.",
      "right = \\(item) -> left item.",
      "left 1."
    ]

closureValueSelfRecursionSource :: Text
closureValueSelfRecursionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "loop :: Bool -> Bool.",
      "loop = \\(item) -> apply loop.",
      "loop False."
    ]

closureValueMutualRecursionSource :: Text
closureValueMutualRecursionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "left :: Bool -> Bool.",
      "left = \\(item) -> apply right.",
      "right :: Bool -> Bool.",
      "right = \\(item) -> apply left.",
      "left False."
    ]

capturingSelfRecursionSource :: Text
capturingSelfRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "loop :: Int -> Int.",
      "loop = \\(item) -> loop (item + seed).",
      "loop 1."
    ]

capturingMutualRecursionSource :: Text
capturingMutualRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "left :: Int -> Int.",
      "left = \\(item) -> right (item + seed).",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

laterCaptureMutualRecursionSource :: Text
laterCaptureMutualRecursionSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "seed = 1.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

transitiveLaterCaptureMutualRecursionSource :: Text
transitiveLaterCaptureMutualRecursionSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "seed = 1.",
      "helper :: Int -> Int.",
      "helper = \\(item) -> item + seed.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (helper item).",
      "left 1."
    ]

interleavedReboundCaptureMutualRecursionSource :: Text
interleavedReboundCaptureMutualRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "left :: Int -> Int.",
      "left = \\(item) -> right (item + seed).",
      "seed = 2.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

polymorphicFunctionSource :: Text
polymorphicFunctionSource =
  Text.unlines
    [ "identity :: a -> a.",
      "identity = \\(item) -> item.",
      "identity 1."
    ]

importedDirectCallSource :: Text
importedDirectCallSource =
  Text.unlines
    [ "foreign :: Int -> Int.",
      "foreign = \\(item) -> item.",
      "foreign 1."
    ]

userDefinedOperatorCallSource :: Text
userDefinedOperatorCallSource =
  Text.unlines
    [ "operator %% tier 2.",
      "(%%) :: Int -> Int -> Int.",
      "(%%) = \\(left, right) -> left + right.",
      "1 %% 2."
    ]

emptyInputs :: InferenceInputs
emptyInputs =
  InferenceInputs
    { inferenceBuiltinMode = ResolveKernelOnly,
      inferenceWarningSettings = defaultWarningSettings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Just modulePath
    }

ambientPreludeInputs :: InferenceInputs
ambientPreludeInputs = emptyInputs {inferenceImportedClassNames = Set.singleton "PreludeClass"}

modulePath :: [Text]
modulePath = ["App", "Main"]

validSourcePath :: TypedSourcePath
validSourcePath = TypedSourcePath "src/App/Main.jz"

unitEntrySource, boolEntrySource, charEntrySource, defaultIntEntrySource, defaultFloatEntrySource, managedTextLiteralSource :: Text
unitEntrySource = "()."
boolEntrySource = "True."
charEntrySource = "'j'."
defaultIntEntrySource = "7."
defaultFloatEntrySource = "1.05."
managedTextLiteralSource = "\"managed\"."

scalarBindingLiteralSource, scalarBindingOrderedReuseSource, scalarBindingDirectCallResultSource, managedScalarBindingSource, scalarBindingFailedInitializerSource, managedTextIdentitySource, managedTextCaptureTransportSource, managedTextConditionalResultSource, managedTextScalarCaseResultSource :: Text
scalarBindingLiteralSource =
  Text.unlines
    [ "seed = 40.",
      "seed + 2."
    ]
scalarBindingOrderedReuseSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 40.",
      "answer = seed + 2.",
      "answer."
    ]
scalarBindingDirectCallResultSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "answer = identity True.",
      "answer."
    ]
managedScalarBindingSource =
  Text.unlines
    [ "message = \"managed\".",
      "message."
    ]
managedTextIdentitySource =
  Text.unlines
    [ "identity :: Text -> Text.",
      "identity = \\(item) -> item.",
      "identity \"Jazz\"."
    ]
managedTextCaptureTransportSource =
  Text.unlines
    [ "message = \"managed\".",
      "capture :: Bool -> Text.",
      "capture = \\(ignored) -> message.",
      "capture True."
    ]
managedTextConditionalResultSource =
  Text.unlines
    [ "choose :: Bool -> Text.",
      "choose = \\(flag) -> if flag then \"yes\" else \"no\".",
      "choose True."
    ]
managedTextScalarCaseResultSource =
  Text.unlines
    [ "choose :: Bool -> Text.",
      "choose = \\(flag) -> case flag { | True -> \"yes\" | _ -> \"no\" }.",
      "choose True."
    ]
scalarBindingFailedInitializerSource =
  Text.unlines
    [ "failed = __kernel_toFloat64 1.",
      "failed."
    ]

arithmeticOperatorsSource, orderingOperatorsSource, equalityOperatorsSource :: Text
arithmeticOperatorsSource = Text.unlines ["1 + 2.", "3 - 1.", "2 * 4.", "8 / 2."]
orderingOperatorsSource = Text.unlines ["1 < 2.", "2 <= 2.", "3 > 2.", "3 >= 3."]
equalityOperatorsSource = Text.unlines ["1 == 1.", "1 != 2."]

sourceDiagnosticSource, textValueSource, listValueSource, nonUnitTupleSource, dataValueSource, conditionalSource, patternCaseSource, localBlockBindingSource :: Text
sourceDiagnosticSource = "missing."
textValueSource = Text.unlines ["\"managed\".", "[1]."]
listValueSource = "[1]."
nonUnitTupleSource = "(1, 2)."
dataValueSource = Text.unlines ["data Box = Box.", "Box."]
conditionalSource = "if True then 1 else 2."
patternCaseSource = "case True { | True -> 1 | _ -> 2 }."
localBlockBindingSource = "{ item = 1. item. }."

conditionalFunctionParameterSource, conditionalCapturedScalarSource, conditionalTailCallFunctionSource, conditionalClosureResultApplicationSource, nestedConditionalsSource :: Text
conditionalFunctionParameterSource =
  Text.unlines
    [ "choose :: Bool -> Int -> Int.",
      "choose = \\(flag, item) -> if flag then item else 0.",
      "choose True 7."
    ]
conditionalCapturedScalarSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 40.",
      "choose :: Bool -> Int.",
      "choose = \\(flag) -> if flag then seed else seed + 2.",
      "apply :: (Bool -> Int) -> Int.",
      "apply = \\(function) -> function True.",
      "apply choose."
    ]
conditionalTailCallFunctionSource =
  Text.unlines
    [ "loop :: Bool -> Int -> Int.",
      "loop = \\(stop, item) -> if stop then item else loop True item.",
      "loop False 7."
    ]
conditionalClosureResultApplicationSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "alwaysFalse :: Bool -> Bool.",
      "alwaysFalse = \\(item) -> False.",
      "(if True then identity else alwaysFalse) True."
    ]
nestedConditionalsSource =
  "if (if True then False else True) then (if True then 1 else 2) else (if False then 3 else 4)."

resolvedImportSource :: Text
resolvedImportSource = Text.unlines ["import Library::Value.", "()."]

resolvedImportSourceFiles :: Map.Map FilePath Text
resolvedImportSourceFiles = Map.singleton "src/Library/Value.jz" "answer = 1."

entryModule :: TypedModule
entryModule =
  TypedModule
    modulePath
    validSourcePath
    []
    []
    (TypedModuleInterface [] [] [] [])
    []
    [TypedExpressionStatement (TypedSpan 1 1) (TypedTupleExpr unitInfo [])]
    unitInfo

unitInfo :: TypedNodeInfo
unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

boolInfo, boolCallableInfo, charInfo, intInfo, floatInfo, textInfo :: TypedNodeInfo
boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
boolCallableInfo =
  TypedNodeInfo
    (TypedFunctionType TypedBoolType TypedBoolType)
    (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
    []
    []
charInfo = TypedNodeInfo TypedCharType TypedCharRecipe [] []
intInfo = TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] []

inferredIntInfo :: TypedNodeInfo
inferredIntInfo = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []

floatInfo = TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] []

textInfo = TypedNodeInfo TypedTextType TypedManagedTextRecipe [] []

boolExpr :: Bool -> TypedExpr
boolExpr value = TypedLiteralExpr boolInfo (TypedBooleanLiteral value)

charExpr :: Char -> TypedExpr
charExpr value = TypedLiteralExpr charInfo (TypedCharacterLiteral value)

textExpr :: Text -> TypedExpr
textExpr value = TypedLiteralExpr textInfo (TypedTextLiteral value)

intExpr :: Integer -> TypedExpr
intExpr value = TypedLiteralExpr intInfo (TypedIntegerLiteral (Text.pack (show value)))

floatExpr :: Integer -> Text -> Maybe TypedNumericType -> TypedExpr
floatExpr whole fractional maybeNumericType = TypedLiteralExpr floatInfo (TypedFractionalLiteral (Text.pack (show whole)) fractional maybeNumericType)

binaryExpr :: TypedNodeInfo -> Text -> TypedExpr -> TypedExpr -> TypedExpr
binaryExpr resultInfo operator left right = TypedBinaryExpr resultInfo (TypedBuiltinOperator operator) left right

expectedScalarProgram :: TypedNodeInfo -> TypedExpr -> TypedProgram
expectedScalarProgram moduleInfo expression =
  TypedProgram Nothing [TypedModule modulePath validSourcePath [] [] (TypedModuleInterface [] [] [] []) [] [TypedExpressionStatement (TypedSpan 1 1) expression] moduleInfo] modulePath

expectedScalarStatements :: [TypedExpr] -> TypedProgram
expectedScalarStatements expressions =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        (zipWith (\line expression -> TypedExpressionStatement (TypedSpan line 1) expression) [1 ..] expressions)
        (typedExpressionInfo (last expressions))
    ]
    modulePath

data ExpectedFunction = ExpectedFunction
  { expectedFunctionName :: Text,
    expectedFunctionParameters :: [(Text, TypedNodeInfo)],
    expectedFunctionResult :: TypedNodeInfo,
    expectedFunctionShape :: TypedCallableShape,
    expectedFunctionBody :: TypedExpr
  }

identityFunction :: ExpectedFunction
identityFunction =
  ExpectedFunction
    "identity"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (variableExpr "item" intInfo)

boolIdentityFunction :: ExpectedFunction
boolIdentityFunction =
  ExpectedFunction
    "identity"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (variableExpr "item" boolInfo)

applyFunction :: ExpectedFunction
applyFunction =
  ExpectedFunction
    "apply"
    [("function", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "function" [boolInfo] boolInfo [boolExpr True])

boolCombineFunction :: ExpectedFunction
boolCombineFunction =
  ExpectedFunction
    "combine"
    [("left", boolInfo), ("right", boolInfo)]
    boolInfo
    TypedDirectCallableShape
    (variableExpr "left" boolInfo)

applyCombineParameterFunction :: ExpectedFunction
applyCombineParameterFunction =
  ExpectedFunction
    "apply"
    [("combine", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "combine" [boolInfo] boolInfo [boolExpr True])

selfShadowingApplyFunction :: ExpectedFunction
selfShadowingApplyFunction =
  ExpectedFunction
    "apply"
    [("apply", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "apply" [boolInfo] boolInfo [boolExpr True])

shadowingForwardFunction :: ExpectedFunction
shadowingForwardFunction =
  ExpectedFunction
    "forward"
    [("forward", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "forward" boolCallableInfo])

closurePassingLoopFunction :: ExpectedFunction
closurePassingLoopFunction =
  ExpectedFunction
    "loop"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "loop" boolCallableInfo])

nestedLambdaClosurePassingLoopFunction :: ExpectedFunction
nestedLambdaClosurePassingLoopFunction =
  ExpectedFunction
    "loop"
    [("item", boolInfo)]
    boolInfo
    TypedDirectCallableShape
    ( directCall
        "apply"
        [boolCallableInfo]
        boolInfo
        [ TypedLambdaExpr
            boolCallableInfo
            nestedParameterBinder
            nestedParameterName
            (directCall "loop" [boolInfo] boolInfo [variableExpr "nested" boolInfo])
        ]
    )
  where
    nestedParameterName = resolvedName "nested"
    nestedParameterBinder = TypedBinderId (modulePath, [3, 0, 0, 1], nestedParameterName)

closurePassingLeftFunction :: ExpectedFunction
closurePassingLeftFunction =
  ExpectedFunction
    "left"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "right" boolCallableInfo])

closurePassingRightFunction :: ExpectedFunction
closurePassingRightFunction =
  ExpectedFunction
    "right"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "left" boolCallableInfo])

capturingLoopFunction :: ExpectedFunction
capturingLoopFunction = capturingRecursiveFunction "loop" "loop"

capturingLeftFunction :: ExpectedFunction
capturingLeftFunction = capturingRecursiveFunction "left" "right"

capturingRightFunction :: ExpectedFunction
capturingRightFunction = capturingRecursiveFunction "right" "left"

capturingRecursiveFunction :: Text -> Text -> ExpectedFunction
capturingRecursiveFunction functionName peerName =
  ExpectedFunction
    functionName
    [("item", intInfo)]
    intInfo
    TypedClosureCallableShape
    ( directCall
        peerName
        [intInfo]
        intInfo
        [binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo)]
    )

chooseFunction :: ExpectedFunction
chooseFunction =
  ExpectedFunction
    "choose"
    [("ignored", boolInfo)]
    boolCallableInfo
    TypedDirectCallableShape
    (variableExpr "identity" boolCallableInfo)

incrementFunction :: ExpectedFunction
incrementFunction = incrementNamed "increment"

incrementNamed :: Text -> ExpectedFunction
incrementNamed name =
  ExpectedFunction
    name
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "item" intInfo) (intExpr 1))

combineFunction :: ExpectedFunction
combineFunction =
  ExpectedFunction
    "combine"
    [("left", intInfo), ("right", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "left" intInfo) (variableExpr "right" intInfo))

sumThreeFunction :: ExpectedFunction
sumThreeFunction =
  ExpectedFunction
    "sumThree"
    [("first", intInfo), ("second", intInfo), ("third", intInfo)]
    intInfo
    TypedDirectCallableShape
    ( binaryExpr
        intInfo
        "+"
        (binaryExpr intInfo "+" (variableExpr "first" intInfo) (variableExpr "second" intInfo))
        (variableExpr "third" intInfo)
    )

firstFunction :: ExpectedFunction
firstFunction =
  ExpectedFunction
    "first"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (directCall "second" [intInfo] intInfo [variableExpr "item" intInfo])

doubleFunction :: ExpectedFunction
doubleFunction =
  ExpectedFunction
    "double"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "item" intInfo))

explicitNumericFunctions :: [ExpectedFunction]
explicitNumericFunctions =
  [ numericFunction "asInt8" TypedInt8Type (TypedSignedIntegerRecipe 8) (TypedIntegerLiteral "1"),
    numericFunction "asInt16" TypedInt16Type (TypedSignedIntegerRecipe 16) (TypedIntegerLiteral "2"),
    numericFunction "asInt32" TypedInt32Type (TypedSignedIntegerRecipe 32) (TypedIntegerLiteral "3"),
    numericFunction "asInt64" TypedInt64Type (TypedSignedIntegerRecipe 64) (TypedIntegerLiteral "4"),
    numericFunction "asUInt8" TypedUInt8Type (TypedUnsignedIntegerRecipe 8) (TypedIntegerLiteral "5"),
    numericFunction "asUInt16" TypedUInt16Type (TypedUnsignedIntegerRecipe 16) (TypedIntegerLiteral "6"),
    numericFunction "asUInt32" TypedUInt32Type (TypedUnsignedIntegerRecipe 32) (TypedIntegerLiteral "7"),
    numericFunction "asUInt64" TypedUInt64Type (TypedUnsignedIntegerRecipe 64) (TypedIntegerLiteral "8"),
    numericFunction "asFloat16" TypedFloat16Type (TypedFloatRecipe 16) (TypedFractionalLiteral "1" "5" (Just TypedFloat16Type)),
    numericFunction "asFloat32" TypedFloat32Type (TypedFloatRecipe 32) (TypedFractionalLiteral "2" "5" (Just TypedFloat32Type)),
    numericFunction "asFloat64" TypedFloat64Type (TypedFloatRecipe 64) (TypedFractionalLiteral "3" "5" (Just TypedFloat64Type))
  ]
  where
    numericFunction name numericType recipe literal =
      let resultInfo = TypedNodeInfo (TypedNumericType numericType) recipe [] []
       in ExpectedFunction
            name
            [("ignored", boolInfo)]
            resultInfo
            TypedDirectCallableShape
            (TypedLiteralExpr resultInfo literal)

typedRecursiveGroupSchemaSentinel :: TypedRecursiveGroup
typedRecursiveGroupSchemaSentinel =
  TypedRecursiveGroup
    [TypedBinderId (modulePath, [1], resolvedName "loop")]

expectedFunctionProgram :: [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgram = expectedFunctionProgramWithRecursiveGroups []

expectedFunctionProgramWithRecursiveGroups ::
  [[Text]] ->
  [Text] ->
  [ExpectedFunction] ->
  TypedExpr ->
  TypedProgram
expectedFunctionProgramWithRecursiveGroups = expectedFunctionProgramWithLineOffsetAndRecursiveGroups 0

expectedFunctionProgramWithLineOffset :: Int -> [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgramWithLineOffset lineOffset = expectedFunctionProgramWithLineOffsetAndRecursiveGroups lineOffset []

expectedFunctionProgramWithLineOffsetAndRecursiveGroups :: Int -> [[Text]] -> [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgramWithLineOffsetAndRecursiveGroups lineOffset recursiveGroupNames exportedNames functions terminalExpression =
  typedRecursiveGroupSchemaSentinel `seq`
    TypedProgram
      Nothing
      [ TypedModule
          modulePath
          validSourcePath
          []
          [TypedModuleExport TypedValueNamespace name | name <- exportedNames]
          typedInterface
          recursiveGroups
          statements
          (typedExpressionInfo boundTerminalExpression)
      ]
      modulePath
  where
    functionOwners =
      Map.fromList
        [ ( resolvedName (expectedFunctionName function),
            TypedBinderId (modulePath, [functionOffset * 2 + 1], resolvedName (expectedFunctionName function))
          )
        | (functionOffset, function) <- zip [0 ..] functions
        ]
    functionStatements =
      concat
        [ map
            (bindExpectedStatementVariables functionOwners)
            (expectedFunctionStatementsAtLineOffset lineOffset signatureIndex bindingIndex function)
        | (functionOffset, function) <- zip [0 ..] functions,
          let signatureIndex = functionOffset * 2,
          let bindingIndex = signatureIndex + 1
        ]
    terminalIndex = length functionStatements
    boundTerminalExpression = bindExpectedExpressionVariables functionOwners terminalExpression
    recursiveGroups =
      [ TypedRecursiveGroup
          [ functionOwners Map.! resolvedName name
          | name <- names
          ]
      | names <- recursiveGroupNames
      ]
    statements =
      functionStatements
        <> [TypedExpressionStatement (TypedSpan (lineOffset + terminalIndex + 1) 1) boundTerminalExpression]
    typedInterface =
      TypedModuleInterface
        [ TypedValueInterface
            (resolvedName name)
            (functionScheme bindingIndex function)
        | name <- exportedNames,
          (functionOffset, function) <- zip [0 ..] functions,
          expectedFunctionName function == name,
          let bindingIndex = functionOffset * 2 + 1
        ]
        []
        []
        []

expectedFunctionStatements :: Int -> Int -> ExpectedFunction -> [TypedStatement]
expectedFunctionStatements = expectedFunctionStatementsAtLineOffset 0

expectedFunctionStatementsAtLineOffset :: Int -> Int -> Int -> ExpectedFunction -> [TypedStatement]
expectedFunctionStatementsAtLineOffset lineOffset signatureIndex bindingIndex function =
  [ TypedSignatureStatement
      signatureOwner
      functionName
      (TypedSpan (lineOffset + signatureIndex + 1) 1)
      (functionScheme signatureIndex function),
    TypedLetStatement
      bindingOwner
      functionName
      (TypedSpan (lineOffset + bindingIndex + 1) 1)
      (functionScheme bindingIndex function)
      (lambdaExpression bindingIndex [0] (expectedFunctionParameters function))
  ]
  where
    functionName = resolvedName (expectedFunctionName function)
    signatureOwner = TypedBinderId (modulePath, [signatureIndex], functionName)
    bindingOwner = TypedBinderId (modulePath, [bindingIndex], functionName)

    lambdaExpression statementIndex childPath parameters =
      case parameters of
        [] -> expectedFunctionBody function
        (parameterName, _) : rest ->
          let typedParameterName = resolvedName parameterName
              parameterBinder = TypedBinderId (modulePath, statementIndex : childPath, typedParameterName)
           in TypedLambdaExpr
                (expectedFunctionInfo function parameters)
                parameterBinder
                typedParameterName
                (lambdaExpression statementIndex (childPath <> [0]) rest)

functionScheme :: Int -> ExpectedFunction -> TypedScheme
functionScheme statementIndex function =
  let functionName = resolvedName (expectedFunctionName function)
      owner = TypedBinderId (modulePath, [statementIndex], functionName)
      info = expectedFunctionInfo function (expectedFunctionParameters function)
   in TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info) (Just (expectedFunctionShape function))

expectedFunctionInfo :: ExpectedFunction -> [(Text, TypedNodeInfo)] -> TypedNodeInfo
expectedFunctionInfo function parameters =
  case expectedFunctionShape function of
    TypedDirectCallableShape -> functionInfo parameters (expectedFunctionResult function)
    TypedClosureCallableShape -> stagedFunctionInfo parameters (expectedFunctionResult function)

functionInfo :: [(Text, TypedNodeInfo)] -> TypedNodeInfo -> TypedNodeInfo
functionInfo parameters resultInfo =
  TypedNodeInfo
    (foldr (TypedFunctionType . typedExpressionType . snd) (typedExpressionType resultInfo) parameters)
    ( case parameters of
        [] -> typedExpressionRecipe resultInfo
        _ ->
          TypedClosureRecipe
            (map (typedExpressionRecipe . snd) parameters)
            (typedExpressionRecipe resultInfo)
    )
    []
    []

stagedFunctionInfo :: [(Text, TypedNodeInfo)] -> TypedNodeInfo -> TypedNodeInfo
stagedFunctionInfo parameters resultInfo =
  TypedNodeInfo
    (foldr (TypedFunctionType . typedExpressionType . snd) (typedExpressionType resultInfo) parameters)
    ( foldr
        (\(_, parameterInfo) resultRecipe -> TypedClosureRecipe [typedExpressionRecipe parameterInfo] resultRecipe)
        (typedExpressionRecipe resultInfo)
        parameters
    )
    []
    []

directCall :: Text -> [TypedNodeInfo] -> TypedNodeInfo -> [TypedExpr] -> TypedExpr
directCall functionName parameterInfos resultInfo arguments =
  go
    (TypedVariableExpr (functionInfo (zip (repeat "") parameterInfos) resultInfo) (resolvedName functionName) Nothing)
    parameterInfos
    arguments
  where
    go functionExpression remainingParameters remainingArguments =
      case (remainingParameters, remainingArguments) of
        (_ : parameterRest, argument : argumentRest) ->
          let applicationInfo =
                case parameterRest of
                  [] -> resultInfo
                  _ -> stagedFunctionInfo (zip (repeat "") parameterRest) resultInfo
           in go (TypedApplyExpr applicationInfo functionExpression argument) parameterRest argumentRest
        ([], []) -> functionExpression
        _ -> error "expected direct call must be fully saturated"

resolvedName :: Text -> TypedCoreName
resolvedName = TypedResolvedName TypedCurrentModule TypedValueNamespace

variableExpr :: Text -> TypedNodeInfo -> TypedExpr
variableExpr name info = TypedVariableExpr info (resolvedName name) Nothing

bindExpectedStatementVariables :: Map.Map TypedCoreName TypedBinderId -> TypedStatement -> TypedStatement
bindExpectedStatementVariables bindings statement =
  case statement of
    TypedLetStatement owner name spanValue schemeValue expression ->
      TypedLetStatement owner name spanValue schemeValue (bindExpectedExpressionVariables bindings expression)
    TypedExpressionStatement spanValue expression ->
      TypedExpressionStatement spanValue (bindExpectedExpressionVariables bindings expression)
    other -> other

bindExpectedExpressionVariables :: Map.Map TypedCoreName TypedBinderId -> TypedExpr -> TypedExpr
bindExpectedExpressionVariables bindings expression =
  case expression of
    TypedLiteralExpr {} -> expression
    TypedVariableExpr info name _ -> TypedVariableExpr info name (Map.lookup name bindings)
    TypedLambdaExpr info owner name body ->
      TypedLambdaExpr info owner name (bindExpectedExpressionVariables (Map.insert name owner bindings) body)
    TypedOperatorValueExpr {} -> expression
    TypedListExpr info values -> TypedListExpr info (map recurse values)
    TypedTupleExpr info values -> TypedTupleExpr info (map recurse values)
    TypedApplyExpr info function argument -> TypedApplyExpr info (recurse function) (recurse argument)
    TypedTypeApplicationExpr info function spanValue typeValue -> TypedTypeApplicationExpr info (recurse function) spanValue typeValue
    TypedIfExpr info condition consequent alternative -> TypedIfExpr info (recurse condition) (recurse consequent) (recurse alternative)
    TypedPatternCaseExpr info scrutinee arms ->
      TypedPatternCaseExpr info (recurse scrutinee) (map bindArm arms)
    TypedBinaryExpr info operator left right -> TypedBinaryExpr info operator (recurse left) (recurse right)
    TypedLeftSectionExpr info left operator -> TypedLeftSectionExpr info (recurse left) operator
    TypedRightSectionExpr info operator right -> TypedRightSectionExpr info operator (recurse right)
    TypedBlockExpr info statements -> TypedBlockExpr info (map (bindExpectedStatementVariables bindings) statements)
  where
    recurse = bindExpectedExpressionVariables bindings
    bindArm (TypedCaseArm patternValue guard result) =
      TypedCaseArm patternValue (recurse <$> guard) (recurse result)

typedExpressionType :: TypedNodeInfo -> TypedType
typedExpressionType (TypedNodeInfo expressionType _ _ _) = expressionType

typedExpressionRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
typedExpressionRecipe (TypedNodeInfo _ recipe _ _) = recipe
