{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.CallTests where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.BoundaryTests
import Jazz.Compiler.Diagnostics (isErrorDiagnostic)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

testManagedTextLowering :: IO ()
testManagedTextLowering =
  mapM_ assertLowered (managedTextExpectedLoweredPrograms <> lowererBoundaryExpectedLoweredPrograms)
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact managed Text lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " valid expected Lowered IR") [] (validateLoweredProgram expectedProgram)

testLexicalCaptureProduction :: IO ()
testLexicalCaptureProduction =
  mapM_ assertProduced lexicalCaptureExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture =
            case lookup name producerEdgeFixtures of
              Just edgeFixture -> edgeFixture
              Nothing -> fixtureByName name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable lexical production") firstRun secondRun
      assertEqual
        (name <> " exact lexical typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected lexical typed validation") [] (validateTypedProgram expectedProgram)

testCurriedApplicationProduction :: IO ()
testCurriedApplicationProduction =
  mapM_ assertProduced curriedApplicationExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      let fixture = curriedApplicationFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact typed program")
        (TypedCoreProductionSucceeded expectedProgram)
        (typedCoreProductionStatus firstRun)
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)

testCurriedApplicationLowering :: IO ()
testCurriedApplicationLowering =
  mapM_ assertLowered curriedApplicationExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed input") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual (name <> " exact lowered program") (LoweredIRSucceeded expectedProgram) firstRun
      case firstRun of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
        other -> failTest (name <> " did not lower: " <> Text.pack (show other))

curriedApplicationFixture :: Text -> Fixture
curriedApplicationFixture name
  | name `elem` fixtureNames = fixtureByName name
  | otherwise = producerEdgeFixture name

testNonCallableOversaturationDiagnostic :: IO ()
testNonCallableOversaturationDiagnostic = do
  let fixture = producerEdgeFixture "non-callable-oversaturation-diagnostic"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "non-callable oversaturation inference compatibility" ordinary (typedCoreProductionInferenceResult firstRun)
  assertEqual "non-callable oversaturation repeatability" firstRun secondRun
  assertEqual
    "non-callable oversaturation has one ordinary type error"
    1
    (length (filter isErrorDiagnostic (inferredDiagnostics ordinary)))
  assertEqual
    "non-callable oversaturation blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)
  assertEqual "blocked oversaturation has no validation proof" Nothing (typedCoreProductionValidatedProgram firstRun)

testLexicalCaptureLowering :: IO ()
testLexicalCaptureLowering =
  mapM_ assertLowered lexicalCaptureExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " is valid lexical typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lexical lowering") firstRun secondRun
      assertEqual (name <> " exact lexical lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " expected lexical lowered validation") [] (validateLoweredProgram expectedProgram)

testLexicalCaptureFixtureMatrix :: IO ()
testLexicalCaptureFixtureMatrix = do
  mapM_ assertSupported lexicalABIs
  where
    assertSupported (name, expectedBinders, expectedLayouts, expectedFunctionIds, expectedNestedEnvironment) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable typed production") firstRun secondRun
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded typedProgram -> do
          assertEqual (name <> " typed validation") [] (validateTypedProgram typedProgram)
          assertEqual (name <> " exact lambda binders") expectedBinders (typedLambdaBinders typedProgram)
          let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
              secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " repeatable lowering") firstLowering secondLowering
          case firstLowering of
            LoweredIRSucceeded loweredProgram -> do
              assertEqual (name <> " lowered validation") [] (validateLoweredProgram loweredProgram)
              assertEqual (name <> " exact environment layouts") expectedLayouts (loweredLayouts loweredProgram)
              assertEqual (name <> " exact function identities") expectedFunctionIds (loweredFunctionIds loweredProgram)
              case expectedNestedEnvironment of
                Just (nestedLayoutId, fields) ->
                  assertEqual (name <> " exact nested environment field order") [fields] (constructedEnvironmentFields nestedLayoutId loweredProgram)
                Nothing -> pure ()
            other -> failTest (name <> " did not lower: " <> Text.pack (show other))
        other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

    lexicalABIs :: [(Text, [TypedBinderId], [LoweredLayout], [LoweredFunctionId], Maybe (LoweredLayoutId, [LoweredOperand]))]
    lexicalABIs =
      [ ( "inline-anonymous-lambda-call",
          [binder [0, 0] "item"],
          [layout [0, 0] "item" []],
          [lambdaFunction [0, 0] "item", loweredEntryFunctionId],
          Nothing
        ),
        ( "nested-scalar-capture",
          [binder [2] "outer", binder [2, 0] "item"],
          [layout [2] "outer" [int64Representation], layout [2, 0] "item" [int64Representation, int64Representation]],
          [lambdaFunction [2] "outer", lambdaFunction [2, 0] "item", loweredEntryFunctionId],
          Just
            ( layoutId [2, 0] "item",
              [ loweredParameter 1 int64Representation,
                loweredTemporary 1 int64Representation
              ]
            )
        ),
        ( "nested-shadow-capture-order",
          [binder [4] "outer", binder [4, 0] "left"],
          [layout [4] "outer" [int64Representation], layout [4, 0] "left" [int64Representation, int64Representation]],
          [lambdaFunction [4] "outer", lambdaFunction [4, 0] "left", loweredEntryFunctionId],
          Just
            ( layoutId [4, 0] "left",
              [ loweredTemporary 1 int64Representation,
                loweredParameter 1 int64Representation
              ]
            )
        ),
        ( "nested-closure-valued-capture",
          [binder [0] "predicate", binder [0, 0] "item"],
          [layout [0] "predicate" [], layout [0, 0] "item" [boolClosureRepresentation]],
          [lambdaFunction [0] "predicate", lambdaFunction [0, 0] "item", loweredEntryFunctionId],
          Just
            ( layoutId [0, 0] "item",
              [loweredParameter 1 boolClosureRepresentation]
            )
        )
      ]

    binder :: [Int] -> Text -> TypedBinderId
    binder path name =
      TypedBinderId (["App", "Main"], path, TypedResolvedName TypedCurrentModule TypedValueNamespace name)
    layout :: [Int] -> Text -> [LoweredRepresentation] -> LoweredLayout
    layout path name fields = LoweredLayout (layoutId path name) (LoweredClosureEnvironmentLayout fields)
    layoutId :: [Int] -> Text -> LoweredLayoutId
    layoutId path name = LoweredLayoutId (generatedIdentity "closure-env" path name)
    lambdaFunction :: [Int] -> Text -> LoweredFunctionId
    lambdaFunction path name = LoweredFunctionId (generatedIdentity "lambda-fn" path name)
    generatedIdentity :: Text -> [Int] -> Text -> Text
    generatedIdentity domain path name =
      "$jz1$"
        <> domain
        <> "$m2$3:App$4:Main$p"
        <> Text.pack (show (length path))
        <> "$"
        <> Text.intercalate "," (map (Text.pack . show) path)
        <> "$n"
        <> Text.pack (show (Text.length name))
        <> ":"
        <> name
    int64Representation = LoweredSignedIntegerRepresentation LoweredIntegerWidth64
    boolClosureRepresentation =
      LoweredClosureRepresentation (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)
    loweredEntryFunctionId = LoweredFunctionId "App::Main::$entry"
    loweredParameter :: Int -> LoweredRepresentation -> LoweredOperand
    loweredParameter index representation =
      LoweredFunctionParameterOperand (LoweredParameterId ("arg" <> Text.pack (show index))) representation
    loweredTemporary :: Int -> LoweredRepresentation -> LoweredOperand
    loweredTemporary index representation =
      LoweredTemporaryOperand (LoweredTemporaryId ("t" <> Text.pack (show index))) representation

    typedLambdaBinders (TypedProgram _ modules _) =
      concat
        [ concatMap statementLambdaBinders statements
        | TypedModule _ _ _ _ _ _ statements _ <- modules
        ]
    statementLambdaBinders statement =
      case statement of
        TypedLetStatement _ _ _ _ expression -> expressionLambdaBinders expression
        TypedExpressionStatement _ expression -> expressionLambdaBinders expression
        _ -> []
    expressionLambdaBinders expression =
      case expression of
        TypedLambdaExpr _ parameterBinder _ body -> parameterBinder : expressionLambdaBinders body
        TypedApplyExpr _ function argument -> expressionLambdaBinders function <> expressionLambdaBinders argument
        TypedBinaryExpr _ _ left right -> expressionLambdaBinders left <> expressionLambdaBinders right
        _ -> []
    loweredLayouts (LoweredProgram _ layouts _ _ _) = layouts
    loweredFunctionIds (LoweredProgram _ _ _ functions _) =
      [functionId | LoweredFunction functionId _ _ _ _ _ <- functions]
    constructedEnvironmentFields targetLayout (LoweredProgram _ _ _ functions _) =
      [ fields
      | LoweredFunction _ _ _ _ blocks _ <- functions,
        LoweredBlock _ _ instructions _ <- blocks,
        LoweredInstruction _ _ (LoweredConstructProduct layoutValue fields) <- instructions,
        layoutValue == targetLayout
      ]

testClosureCaptureReviewRegression :: Text -> IO ()
testClosureCaptureReviewRegression name = do
  let fixture = producerEdgeFixture name
  firstProduction <- produceFixture fixture
  secondProduction <- produceFixture fixture
  assertEqual (name <> " repeatable typed production") firstProduction secondProduction
  case typedCoreProductionStatus firstProduction of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      let firstLowering = lowerTypedCoreExpressionDirectCall typedProgram
          secondLowering = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " repeatable lowering") firstLowering secondLowering
      case firstLowering of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (name <> " valid lowered IR") [] (validateLoweredProgram loweredProgram)
        other -> failTest (name <> " did not lower: " <> Text.pack (show other))
    other -> failTest (name <> " did not produce typed core: " <> Text.pack (show other))

testLiftedLambdaFailurePreorder :: IO ()
testLiftedLambdaFailurePreorder =
  case lookup "lifted-lambda-failure-preorder" reviewLowererBoundaryPrograms of
    Just programValue -> do
      let firstRun = lowerTypedCoreExpressionDirectCall programValue
          secondRun = lowerTypedCoreExpressionDirectCall programValue
      assertEqual "lifted conditional fixture is valid typed core" [] (validateTypedProgram programValue)
      assertEqual "lifted conditional lowering is repeatable" firstRun secondRun
      case firstRun of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "lifted conditional lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("lifted conditional did not lower: " <> Text.pack (show other))
    Nothing -> failTest "lifted conditional regression fixture is missing"

testLiftedLambdaMetadataAlias :: IO ()
testLiftedLambdaMetadataAlias =
  case lookup "exported-scalar-lifted-lambda-name-collision" reviewLowererBoundaryPrograms of
    Just programValue -> do
      assertEqual "metadata collision fixture is valid typed core" [] (validateTypedProgram programValue)
      assertEqual
        "lifted lambda names cannot satisfy scalar module metadata"
        ( LoweredIRUnsupported
            [ LoweredIRLoweringFailure
                (TypedModulePath ["App", "Main"])
                LoweredIRUnsupportedModule
                LoweredIRNoFailureDetail
            ]
        )
        (lowerTypedCoreExpressionDirectCall programValue)
    Nothing -> failTest "metadata collision regression fixture is missing"

testScalarBindingLowering :: IO ()
testScalarBindingLowering =
  mapM_ assertLowered scalarBindingExpectedLoweredPrograms
  where
    assertLowered (name, typedProgram, expectedProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable scalar lowering") firstRun secondRun
      assertEqual (name <> " exact scalar lowering") (LoweredIRSucceeded expectedProgram) firstRun
      assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)

testThreeArgumentDirectLeadingLambdaRecipe :: IO ()
testThreeArgumentDirectLeadingLambdaRecipe =
  case lookup "three-argument-direct-call" directCallExpectedPrograms of
    Just programValue ->
      assertEqual
        "three-argument direct leading-lambda validation"
        []
        (validateTypedProgram programValue)
    Nothing -> failTest "three-argument direct-call typed program is missing"

testRfcClosureEnvironmentIdentity :: IO ()
testRfcClosureEnvironmentIdentity = do
  let (typedProgram, expectedProgram) = rfcClosureEnvironmentIdentityProgram
      firstRun = lowerTypedCoreExpressionDirectCall typedProgram
      secondRun = lowerTypedCoreExpressionDirectCall typedProgram
  assertEqual "RFC identity typed-core validation" [] (validateTypedProgram typedProgram)
  assertEqual "RFC identity repeatable lowering" firstRun secondRun
  assertEqual "RFC identity exact lowering" (LoweredIRSucceeded expectedProgram) firstRun

testSupportedClosureLowererBoundary :: IO ()
testSupportedClosureLowererBoundary =
  mapM_ assertSupported independentClosureExpectedLoweredPrograms
  where
    assertSupported (name, expectedProgram) =
      case lookup name lowererBoundaryPrograms of
        Nothing -> failTest (name <> " supported lowerer program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable closure lowering") firstRun secondRun
          assertEqual (name <> " exact closure lowering") (LoweredIRSucceeded expectedProgram) firstRun
          assertEqual (name <> " expected lowered validation") [] (validateLoweredProgram expectedProgram)

testLowererCallableBoundary :: IO ()
testLowererCallableBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name lowererBoundaryPrograms of
        Nothing -> failTest (name <> " lowerer boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstRun secondRun
          assertEqual (name <> " exact lowerer rejection") (LoweredIRUnsupported expectedFailures) firstRun

    expectedResults =
      [ ( "recursion-descendant-failure-order",
          [ expressionFailure
              2
              [0, 0, 1]
              LoweredIRCaptureUnsupported
              (LoweredIRNameFailureDetail (currentName "seed"))
          ]
        ),
        ( "interleaved-capture-mutual-recursion",
          [ statementFailure
              4
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "right"))
          ]
        ),
        ( "non-concrete-closure-representation",
          [ statementFailure
              1
              LoweredIRInvalidFunctionShape
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "duplicate-parameter-function",
          [ expressionFailure
              1
              [0, 0]
              LoweredIRDuplicateParameterIdentity
              (LoweredIRNameFailureDetail (currentName "item"))
          ]
        ),
        ( "self-recursive-duplicate-parameter-function",
          [ statementFailure
              1
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop")),
            expressionFailure
              1
              [0, 0]
              LoweredIRDuplicateParameterIdentity
              (LoweredIRNameFailureDetail (currentName "item"))
          ]
        ),
        ( "duplicate-function-identity",
          [ statementFailure
              3
              LoweredIRDuplicateFunctionIdentity
              (LoweredIRNameFailureDetail (currentName "identity"))
          ]
        ),
        ( "nested-lambda-closure-value-self-recursion",
          [ statementFailure
              3
              LoweredIRRecursiveFunctionUnsupported
              (LoweredIRNameFailureDetail (currentName "loop"))
          ]
        ),
        ( "imported-direct-call",
          [ LoweredIRLoweringFailure
              TypedProgramPath
              LoweredIRUnsupportedProgram
              LoweredIRNoFailureDetail,
            LoweredIRLoweringFailure
              (TypedModulePath ["App", "Main"])
              LoweredIRUnsupportedModule
              LoweredIRNoFailureDetail,
            expressionFailure
              0
              [0]
              LoweredIRNonLocalCallUnsupported
              ( LoweredIRNameFailureDetail
                  (TypedResolvedName (TypedImportedModule ["Library", "Functions"]) TypedValueNamespace "foreign")
              )
          ]
        )
      ]
    statementFailure index kind detail =
      LoweredIRLoweringFailure
        (TypedStatementPath ["App", "Main"] [index])
        kind
        detail
    expressionFailure statementIndex childPath kind detail =
      LoweredIRLoweringFailure
        (TypedExpressionPath ["App", "Main"] [statementIndex] childPath)
        kind
        detail
    currentName = TypedResolvedName TypedCurrentModule TypedValueNamespace

testInvalidLowererTypedCoreBoundary :: IO ()
testInvalidLowererTypedCoreBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name invalidLowererBoundaryPrograms of
        Nothing -> failTest (name <> " invalid lowerer boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " repeatable typed-core rejection") firstRun secondRun
          assertEqual (name <> " exact typed-core rejection") (LoweredIRTypedCoreFailures expectedFailures) firstRun

    expectedResults =
      [ ( "closure-shape-flattened-recipe",
          [ callableShapeFailure 0,
            callableShapeFailure 1,
            TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [1] [0])
              TypedCallableRecipeMismatch
              ( TypedRecipeDetail
                  (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
                  (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)
              )
          ]
        ),
        ( "direct-shape-staged-recipe",
          [callableShapeFailure 1]
        ),
        ( "callable-shape-body-disagreement",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0])
              TypedLambdaResultMismatch
              ( TypedRecipeDetail
                  (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
                  (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)
              ),
            TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0, 0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        ),
        ( "variable-binder-reference-mismatch",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedBinderReferenceMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [999], currentName "identity")))
          ]
        ),
        ( "direct-flattened-representation",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        ),
        ( "direct-shaped-closure-value-self-recursion",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [3] [0, 0, 1])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [3], currentName "loop")))
          ]
        ),
        ( "shape-rejected-self-recursion",
          [callableShapeFailureFor 1 "loop"]
        ),
        ( "shape-rejected-mutual-recursion",
          [ callableShapeFailureFor 1 "left",
            callableShapeFailureFor 3 "right"
          ]
        ),
        ( "shape-rejected-binder-shadow-control",
          [callableShapeFailureFor 1 "loop"]
        ),
        ( "bare-function-value",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "identity")))
          ]
        ),
        ( "partial-direct-call",
          [ TypedCoreValidationFailure
              (TypedExpressionPath ["App", "Main"] [2] [0, 0])
              TypedCallableShapeMismatch
              (TypedBinderDetail (TypedBinderId (["App", "Main"], [1], currentName "combine")))
          ]
        )
      ]
    callableShapeFailure statementIndex =
      callableShapeFailureFor statementIndex "combine"
    callableShapeFailureFor statementIndex identifier =
      TypedCoreValidationFailure
        (TypedStatementPath ["App", "Main"] [statementIndex])
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            (TypedBinderId (["App", "Main"], [statementIndex], currentName identifier))
        )
    currentName = TypedResolvedName TypedCurrentModule TypedValueNamespace

testLowererStructuralBoundary :: IO ()
testLowererStructuralBoundary =
  mapM_ assertBoundary expectedResults
  where
    assertBoundary (name, expectedFailures) =
      case lookup name lowererStructuralBoundaryPrograms of
        Nothing -> failTest (name <> " lowerer structural boundary program is missing")
        Just programValue -> do
          let firstRun = lowerTypedCoreExpressionDirectCall programValue
              secondRun = lowerTypedCoreExpressionDirectCall programValue
          assertEqual (name <> " is permanently valid typed core") [] (validateTypedProgram programValue)
          assertEqual (name <> " repeatable lowerer rejection") firstRun secondRun
          assertEqual (name <> " exact lowerer rejection") (LoweredIRUnsupported expectedFailures) firstRun

    expectedResults =
      [ ( "managed-pattern-scrutinee",
          [ LoweredIRLoweringFailure
              (TypedExpressionPath ["App", "Main"] [0] [0])
              LoweredIRUnsupportedPattern
              LoweredIRNoFailureDetail
          ]
        )
      ]

testForwardVisibilityBoundary :: IO ()
testForwardVisibilityBoundary = do
  ordinary <- inferFixture ordinaryForwardVisibilityFixture
  firstRun <- produceFixture ordinaryForwardVisibilityFixture
  secondRun <- produceFixture ordinaryForwardVisibilityFixture
  assertUnboundLater "ordinary unsigned forward caller" ordinary
  assertEqual
    "ordinary unsigned forward caller inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "ordinary unsigned forward caller repeatable production" firstRun secondRun
  assertEqual
    "ordinary unsigned forward caller blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)
  mapM_ assertInvisible forwardVisibilityNegativeFixtures
  where
    assertInvisible fixture = do
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertUnboundLater (fixtureName fixture <> " ordinary invisibility") ordinary
      assertEqual
        (fixtureName fixture <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual
        (fixtureName fixture <> " repeatable production")
        firstRun
        secondRun
      assertEqual
        (fixtureName fixture <> " blocks typed-core production")
        TypedCoreProductionBlockedByDiagnostics
        (typedCoreProductionStatus firstRun)

testUnitForwardVisibility :: IO ()
testUnitForwardVisibility = do
  let fixture = producerEdgeFixture "unit-forward-function"
  result <- produceFixture fixture
  assertEqual
    "unit forward function diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult result)))
  case typedCoreProductionStatus result of
    TypedCoreProductionSucceeded programValue ->
      assertEqual "unit forward function typed-core validation" [] (validateTypedProgram programValue)
    _ -> failTest "unit forward function did not produce typed core"

testCurriedArgumentCapture :: IO ()
testCurriedArgumentCapture = do
  let fixture = producerEdgeFixture "curried-first-argument-capture"
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "captured direct-call argument repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual "captured direct-call argument typed validation" [] (validateTypedProgram typedProgram)
      case lowerTypedCoreExpressionDirectCall typedProgram of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "captured direct-call argument lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("captured direct-call argument did not lower: " <> Text.pack (show other))
    other -> failTest ("captured direct-call argument did not produce typed core: " <> Text.pack (show other))

testPartialApplicationArgumentCapture :: IO ()
testPartialApplicationArgumentCapture = do
  let fixture = producerEdgeFixture "partial-call-argument-capture"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "partial-call argument inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "partial-call argument repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded typedProgram -> do
      assertEqual "partial-call argument typed validation" [] (validateTypedProgram typedProgram)
      case lowerTypedCoreExpressionDirectCall typedProgram of
        LoweredIRSucceeded loweredProgram ->
          assertEqual "partial-call argument lowered validation" [] (validateLoweredProgram loweredProgram)
        other -> failTest ("partial-call argument did not lower: " <> Text.pack (show other))
    other -> failTest ("partial-call argument did not produce typed core: " <> Text.pack (show other))

testNonLocalCallArgumentFailureAccumulation :: IO ()
testNonLocalCallArgumentFailureAccumulation = do
  let fixture = producerEdgeFixture "non-local-call-argument-capture"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [])
              TypedCoreNonLocalCallUnsupported
              (TypedCoreNameDetail "__kernel_toFloat64")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "non-local-call argument failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "non-local-call argument failure repeatability" firstRun secondRun
  assertEqual "non-local-call argument failure accumulation" expected (typedCoreProductionStatus firstRun)

testClosureUseArgumentFailureOrder :: IO ()
testClosureUseArgumentFailureOrder = do
  let fixture = producerEdgeFixture "closure-use-argument-failure-order"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 7 [])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "closure-use argument failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "closure-use argument failure repeatability" firstRun secondRun
  assertEqual "closure-use argument and later sibling failure order" expected (typedCoreProductionStatus firstRun)

testClosureShapeClassificationCollapse :: IO ()
testClosureShapeClassificationCollapse = do
  let fixture = fixtureByName "mixed-direct-and-value-use"
      expectedShapes =
        [ ("apply", TypedDirectCallableShape),
          ("apply", TypedDirectCallableShape),
          ("identity", TypedClosureCallableShape),
          ("identity", TypedClosureCallableShape)
        ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "mixed callable-use classification repeatability" firstRun secondRun
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded (TypedProgram _ [TypedModule _ _ _ _ _ _ statements _] _) ->
      assertEqual "mixed callable-use scheme classifications" expectedShapes (callableSchemeShapes statements)
    status -> failTest ("mixed callable-use fixture did not produce typed core: " <> Text.pack (show status))
  where
    callableSchemeShapes statements =
      [ (identifier, shape)
      | statement <- statements,
        (name, TypedScheme _ _ _ _ _ _ (Just shape)) <- case statement of
          TypedSignatureStatement _ name _ schemeValue -> [(name, schemeValue)]
          TypedLetStatement _ name _ schemeValue _ -> [(name, schemeValue)]
          _ -> [],
        TypedResolvedName TypedCurrentModule TypedValueNamespace identifier <- [name]
      ]
