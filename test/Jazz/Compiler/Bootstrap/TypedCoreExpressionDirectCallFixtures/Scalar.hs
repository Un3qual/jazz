{-# LANGUAGE OverloadedStrings #-}

-- | Unit, scalar, numeric, binding, and pattern-case artifacts.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Scalar where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.TypedCore

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
