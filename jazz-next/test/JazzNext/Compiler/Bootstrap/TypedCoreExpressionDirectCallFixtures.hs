{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( Fixture (..),
    fixtureNames,
    acceptedFixtureNames,
    rejectedFixtureNames,
    fixtures,
    expectedUnitProgram,
    scalarExpectedLoweredPrograms,
    explicitNumericScalarLoweringPrograms,
    fullUInt64ScalarLoweringPrograms,
    nestedScalarTypedProgram,
    expectedNestedScalarLoweredProgram,
    scalarFixtures,
    scalarExpectedPrograms,
    directCallExpectedPrograms,
    directCallExpectedLoweredPrograms,
    lowererBoundaryPrograms,
    lowererStructuralBoundaryPrograms,
    ordinaryForwardVisibilityFixture,
    forwardVisibilityNegativeFixtures,
    rejectedScalarFixtures,
    resolveFixture,
    resolveFixtureWithLookup,
    admittedOperators,
    explicitNumericTypes,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.Diagnostics (Diagnostic)
import JazzNext.Compiler.LoweredIR
import qualified JazzNext.Compiler.ModuleGraph as ModuleGraph
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    resolveProgram,
  )
import JazzNext.Compiler.TypeInference (InferenceInputs (..))
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (TFunctionType, TIntType),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts,
  )
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)

data Fixture = Fixture
  { fixtureName :: Text,
    fixtureInputs :: InferenceInputs,
    fixtureSourcePath :: TypedSourcePath,
    fixtureSourceFiles :: Map.Map FilePath Text
  }

fixtureNames :: [Text]
fixtureNames = acceptedFixtureNames <> rejectedFixtureNames

acceptedFixtureNames :: [Text]
acceptedFixtureNames =
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
    "exported-direct-function"
  ]

rejectedFixtureNames :: [Text]
rejectedFixtureNames =
  [ "source-diagnostic",
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
    "bare-function-value",
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
fixtures =
  [ sourceFixture "unit-entry" unitEntrySource,
    sourceFixture "bool-entry" boolEntrySource,
    sourceFixture "char-entry" charEntrySource,
    sourceFixture "default-int-entry" defaultIntEntrySource,
    sourceFixture "default-float-entry" defaultFloatEntrySource,
    sourceFixture "explicit-numeric-widths" explicitNumericWidthsSource,
    sourceFixture "arithmetic-operators" arithmeticOperatorsSource,
    sourceFixture "ordering-operators" orderingOperatorsSource,
    sourceFixture "equality-operators" equalityOperatorsSource,
    sourceFixture "scalar-parameter-return" scalarParameterReturnSource,
    sourceFixture "single-argument-direct-call" singleArgumentDirectCallSource,
    sourceFixture "curried-multi-argument-direct-call" curriedMultiArgumentDirectCallSource,
    sourceFixture "forward-direct-call-dag" forwardDirectCallDagSource,
    sourceFixture "nested-direct-calls" nestedDirectCallsSource,
    sourceFixture "dollar-direct-call" dollarDirectCallSource,
    sourceFixture "exported-direct-function" exportedDirectFunctionSource,
    sourceFixture "source-diagnostic" sourceDiagnosticSource,
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
    sourceFixtureNoExports "conditional" conditionalSource,
    sourceFixtureNoExports "pattern-case" patternCaseSource,
    sourceFixtureNoExports "local-block-binding" localBlockBindingSource,
    sourceFixtureNoExports "bare-function-value" bareFunctionValueSource,
    sourceFixtureNoExports "partial-direct-call" partialDirectCallSource,
    sourceFixtureNoExports "oversaturated-direct-call" oversaturatedDirectCallSource,
    sourceFixtureNoExports "capturing-function" capturingFunctionSource,
    sourceFixtureNoExports "self-recursive-function" selfRecursiveFunctionSource,
    sourceFixtureNoExports "mutually-recursive-functions" mutuallyRecursiveFunctionsSource,
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
    sourceFixture "forward-unsigned-lambda-invisibility" forwardUnsignedLambdaSource
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
    ("default-float-entry", expectedLoweredProgram float64Representation [] (loweredImmediate (LoweredFloatImmediate LoweredFloatWidth64 "1.5"))),
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
    ("default-float-entry", expectedScalarProgram floatInfo (floatExpr 1 5 Nothing)),
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
    )
  ]

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
      expectedFunctionProgram
        ["increment"]
        [incrementFunction]
        (directCall "increment" [intInfo] intInfo [intExpr 41])
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
    ( "forward-direct-call-dag",
      expectedCallableLoweredProgram
        [ expectedLocalFunction
            "first"
            [LoweredParameter (LoweredParameterId "arg1") int64Representation]
            int64Representation
            [expectedDirectCallInstruction 1 int64Representation "second" [loweredParameter 1 int64Representation]]
            (loweredTemporary 1 int64Representation),
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

loweredParameter :: Int -> LoweredRepresentation -> LoweredOperand
loweredParameter index =
  LoweredFunctionParameterOperand (LoweredParameterId ("arg" <> Text.pack (show index)))

lowererBoundaryPrograms :: [(Text, TypedProgram)]
lowererBoundaryPrograms =
  [ ("invalid-function-shape", scalarBindingProgram),
    ("duplicate-parameter-function", duplicateParameterLowererProgram),
    ("capturing-function", capturingLowererProgram),
    ("self-recursive-function", selfRecursiveLowererProgram),
    ("mutually-recursive-functions", mutuallyRecursiveLowererProgram),
    ("bare-function-value", bareFunctionLowererProgram),
    ("partial-direct-call", partialCallLowererProgram),
    ("imported-direct-call", importedDirectCallLowererProgram)
  ]

lowererStructuralBoundaryPrograms :: [(Text, TypedProgram)]
lowererStructuralBoundaryPrograms =
  [ ("managed-scalar-entry", managedScalarLowererProgram),
    ("conditional-entry", conditionalLowererProgram)
  ]

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

duplicateParameterLowererProgram :: TypedProgram
duplicateParameterLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "chooseSecond"
        [("item", intInfo), ("item", intInfo)]
        intInfo
        (variableExpr "item" intInfo)
    ]
    (directCall "chooseSecond" [intInfo, intInfo] intInfo [intExpr 1, intExpr 2])

scalarBindingProgram :: TypedProgram
scalarBindingProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [ TypedLetStatement
            seedBinder
            seedName
            (TypedSpan 1 1)
            seedScheme
            (intExpr 1),
          TypedExpressionStatement (TypedSpan 2 1) (intExpr 1)
        ]
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64)

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
        ( scalarStatement
            <> expectedFunctionStatements 1 2 addSeedFunction
            <> [ TypedExpressionStatement
                   (TypedSpan 4 1)
                   (directCall "addSeed" [intInfo] intInfo [intExpr 41])
               ]
        )
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64)
    scalarStatement =
      [TypedLetStatement seedBinder seedName (TypedSpan 1 1) seedScheme (intExpr 1)]
    addSeedFunction =
      ExpectedFunction
        "addSeed"
        [("item", intInfo)]
        intInfo
        (binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo))

selfRecursiveLowererProgram :: TypedProgram
selfRecursiveLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (directCall "loop" [intInfo] intInfo [intExpr 1])

mutuallyRecursiveLowererProgram :: TypedProgram
mutuallyRecursiveLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "left"
        [("item", intInfo)]
        intInfo
        (directCall "right" [intInfo] intInfo [variableExpr "item" intInfo]),
      ExpectedFunction
        "right"
        [("item", intInfo)]
        intInfo
        (directCall "left" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (directCall "left" [intInfo] intInfo [intExpr 1])

bareFunctionLowererProgram :: TypedProgram
bareFunctionLowererProgram =
  expectedFunctionProgram
    []
    [identityFunction]
    (TypedVariableExpr (functionInfo [("item", intInfo)] intInfo) (resolvedName "identity"))

partialCallLowererProgram :: TypedProgram
partialCallLowererProgram =
  expectedFunctionProgram
    []
    [combineFunction]
    ( TypedApplyExpr
        (functionInfo [("right", intInfo)] intInfo)
        (TypedVariableExpr (functionInfo [("left", intInfo), ("right", intInfo)] intInfo) (resolvedName "combine"))
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
    providerModule =
      TypedModule
        providerPath
        (TypedSourcePath "src/Library/Functions.jz")
        []
        [TypedModuleExport TypedValueNamespace "foreign"]
        (TypedModuleInterface [TypedValueInterface providerName providerScheme] [] [] [])
        [ TypedLetStatement
            providerOwner
            providerName
            (TypedSpan 1 1)
            providerScheme
            ( TypedLambdaExpr
                providerInfo
                providerParameterBinder
                providerParameterName
                (TypedVariableExpr intInfo providerParameterName)
            )
        ]
        unitInfo
    callExpression =
      TypedApplyExpr
        intInfo
        (TypedVariableExpr providerInfo importedName)
        (intExpr 1)
    entry =
      TypedModule
        modulePath
        validSourcePath
        [TypedResolvedImport (TypedSpan 1 1) providerPath Nothing (Just ["foreign"])]
        []
        (TypedModuleInterface [] [] [] [])
        [TypedExpressionStatement (TypedSpan 1 1) callExpression]
        intInfo

rejectedScalarFixtures :: [Fixture]
rejectedScalarFixtures = map fixtureByName ["text-value", "list-value", "non-unit-tuple", "data-value", "conditional", "pattern-case", "local-block-binding"]

admittedOperators :: [Text]
admittedOperators = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]

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
exportedDirectFunctionSource = singleArgumentDirectCallSource

bareFunctionValueSource :: Text
bareFunctionValueSource =
  Text.unlines
    [ "identity :: Int -> Int.",
      "identity = \\(item) -> item.",
      "identity."
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

unitEntrySource, boolEntrySource, charEntrySource, defaultIntEntrySource, defaultFloatEntrySource :: Text
unitEntrySource = "()."
boolEntrySource = "True."
charEntrySource = "'j'."
defaultIntEntrySource = "7."
defaultFloatEntrySource = "1.5."

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
patternCaseSource = "case True { | _ -> 1 }."
localBlockBindingSource = "{ item = 1. item. }."

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
    [TypedExpressionStatement (TypedSpan 1 1) (TypedTupleExpr unitInfo [])]
    unitInfo

unitInfo :: TypedNodeInfo
unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

boolInfo, charInfo, intInfo, floatInfo, textInfo :: TypedNodeInfo
boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
charInfo = TypedNodeInfo TypedCharType TypedCharRecipe [] []
intInfo = TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] []
floatInfo = TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] []
textInfo = TypedNodeInfo TypedTextType TypedManagedTextRecipe [] []

boolExpr :: Bool -> TypedExpr
boolExpr value = TypedLiteralExpr boolInfo (TypedBooleanLiteral value)

charExpr :: Char -> TypedExpr
charExpr value = TypedLiteralExpr charInfo (TypedCharacterLiteral value)

intExpr :: Integer -> TypedExpr
intExpr value = TypedLiteralExpr intInfo (TypedIntegerLiteral (Text.pack (show value)))

floatExpr :: Integer -> Integer -> Maybe TypedNumericType -> TypedExpr
floatExpr whole fractional maybeNumericType = TypedLiteralExpr floatInfo (TypedFractionalLiteral (Text.pack (show whole)) (Text.pack (show fractional)) maybeNumericType)

binaryExpr :: TypedNodeInfo -> Text -> TypedExpr -> TypedExpr -> TypedExpr
binaryExpr resultInfo operator left right = TypedBinaryExpr resultInfo (TypedBuiltinOperator operator) left right

expectedScalarProgram :: TypedNodeInfo -> TypedExpr -> TypedProgram
expectedScalarProgram moduleInfo expression =
  TypedProgram Nothing [TypedModule modulePath validSourcePath [] [] (TypedModuleInterface [] [] [] []) [TypedExpressionStatement (TypedSpan 1 1) expression] moduleInfo] modulePath

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
        (zipWith (\line expression -> TypedExpressionStatement (TypedSpan line 1) expression) [1 ..] expressions)
        (typedExpressionInfo (last expressions))
    ]
    modulePath

data ExpectedFunction = ExpectedFunction
  { expectedFunctionName :: Text,
    expectedFunctionParameters :: [(Text, TypedNodeInfo)],
    expectedFunctionResult :: TypedNodeInfo,
    expectedFunctionBody :: TypedExpr
  }

identityFunction :: ExpectedFunction
identityFunction =
  ExpectedFunction
    "identity"
    [("item", intInfo)]
    intInfo
    (variableExpr "item" intInfo)

incrementFunction :: ExpectedFunction
incrementFunction = incrementNamed "increment"

incrementNamed :: Text -> ExpectedFunction
incrementNamed name =
  ExpectedFunction
    name
    [("item", intInfo)]
    intInfo
    (binaryExpr intInfo "+" (variableExpr "item" intInfo) (intExpr 1))

combineFunction :: ExpectedFunction
combineFunction =
  ExpectedFunction
    "combine"
    [("left", intInfo), ("right", intInfo)]
    intInfo
    (binaryExpr intInfo "+" (variableExpr "left" intInfo) (variableExpr "right" intInfo))

firstFunction :: ExpectedFunction
firstFunction =
  ExpectedFunction
    "first"
    [("item", intInfo)]
    intInfo
    (directCall "second" [intInfo] intInfo [variableExpr "item" intInfo])

doubleFunction :: ExpectedFunction
doubleFunction =
  ExpectedFunction
    "double"
    [("item", intInfo)]
    intInfo
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
            (TypedLiteralExpr resultInfo literal)

expectedFunctionProgram :: [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgram exportedNames functions terminalExpression =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        [TypedModuleExport TypedValueNamespace name | name <- sort exportedNames]
        typedInterface
        statements
        (typedExpressionInfo terminalExpression)
    ]
    modulePath
  where
    functionStatements =
      concat
        [ expectedFunctionStatements signatureIndex bindingIndex function
        | (functionOffset, function) <- zip [0 ..] functions,
          let signatureIndex = functionOffset * 2,
          let bindingIndex = signatureIndex + 1
        ]
    terminalIndex = length functionStatements
    statements =
      functionStatements
        <> [TypedExpressionStatement (TypedSpan (terminalIndex + 1) 1) terminalExpression]
    typedInterface =
      TypedModuleInterface
        [ TypedValueInterface
            (resolvedName name)
            (functionScheme bindingIndex function)
        | name <- sort exportedNames,
          (functionOffset, function) <- zip [0 ..] functions,
          expectedFunctionName function == name,
          let bindingIndex = functionOffset * 2 + 1
        ]
        []
        []
        []

expectedFunctionStatements :: Int -> Int -> ExpectedFunction -> [TypedStatement]
expectedFunctionStatements signatureIndex bindingIndex function =
  [ TypedSignatureStatement
      signatureOwner
      functionName
      (TypedSpan (signatureIndex + 1) 1)
      (functionScheme signatureIndex function),
    TypedLetStatement
      bindingOwner
      functionName
      (TypedSpan (bindingIndex + 1) 1)
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
                (functionInfo parameters (expectedFunctionResult function))
                parameterBinder
                typedParameterName
                (lambdaExpression statementIndex (childPath <> [0]) rest)

functionScheme :: Int -> ExpectedFunction -> TypedScheme
functionScheme statementIndex function =
  let functionName = resolvedName (expectedFunctionName function)
      owner = TypedBinderId (modulePath, [statementIndex], functionName)
      info = functionInfo (expectedFunctionParameters function) (expectedFunctionResult function)
   in TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info)

functionInfo :: [(Text, TypedNodeInfo)] -> TypedNodeInfo -> TypedNodeInfo
functionInfo parameters resultInfo =
  TypedNodeInfo
    (foldr (TypedFunctionType . typedExpressionType . snd) (typedExpressionType resultInfo) parameters)
    (TypedClosureRecipe (map (typedExpressionRecipe . snd) parameters) (typedExpressionRecipe resultInfo))
    []
    []

directCall :: Text -> [TypedNodeInfo] -> TypedNodeInfo -> [TypedExpr] -> TypedExpr
directCall functionName parameterInfos resultInfo arguments =
  go
    (TypedVariableExpr (functionInfo (zip (repeat "") parameterInfos) resultInfo) (resolvedName functionName))
    parameterInfos
    arguments
  where
    go functionExpression remainingParameters remainingArguments =
      case (remainingParameters, remainingArguments) of
        (_ : parameterRest, argument : argumentRest) ->
          let applicationInfo =
                case parameterRest of
                  [] -> resultInfo
                  _ -> functionInfo (zip (repeat "") parameterRest) resultInfo
           in go (TypedApplyExpr applicationInfo functionExpression argument) parameterRest argumentRest
        ([], []) -> functionExpression
        _ -> error "expected direct call must be fully saturated"

resolvedName :: Text -> TypedCoreName
resolvedName = TypedResolvedName TypedCurrentModule TypedValueNamespace

variableExpr :: Text -> TypedNodeInfo -> TypedExpr
variableExpr name info = TypedVariableExpr info (resolvedName name)

typedExpressionType :: TypedNodeInfo -> TypedType
typedExpressionType (TypedNodeInfo expressionType _ _ _) = expressionType

typedExpressionRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
typedExpressionRecipe (TypedNodeInfo _ recipe _ _) = recipe

typedExpressionInfo :: TypedExpr -> TypedNodeInfo
typedExpressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedApplyExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedTupleExpr info _ -> info
    _ -> error "scalar fixture expected a scalar expression"
