{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManagedProductsVariantsTests where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.LowererBoundary
  ( managedPatternAnalysisBoundaryPrograms,
    managedPatternTransportPrograms,
  )
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedProductsVariants
  ( optionLayout,
    optionLayoutId,
    textRepresentation,
    treeLayout,
    tupleLayout,
  )
import qualified Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedProductsVariants as ManagedProductsVariants
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source (intInfo, sourceFixture, sourceFixtureNoExports)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
import Jazz.Compiler.LoweredIR.Lower.ManagedPatterns
import Jazz.Compiler.LoweredIR.Lower.Requirements
  ( collectRuntimeRequirements,
    requiredRuntimeLayouts,
    requirementsForManagedLayouts,
  )
import Jazz.Compiler.LoweredIR.Lower.Types (RuntimeRequirements (..))
import Jazz.Compiler.LoweredIR.RuntimeServiceCatalog (textLayout)
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate
  ( validateTypedProgram,
    validatedTypedProgram,
  )
import Jazz.TestHarness (assertEqual, failTest)

testManagedProductVariantRetention :: IO ()
testManagedProductVariantRetention = do
  assertBoundary
    "managed-tuple-child-failure"
    [expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-data-sibling-failure"
    [expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-bare-constructor-failure"
    [expressionFailure 1 [] TypedCoreCallableValueUnsupported (TypedCoreNameDetail "Box")]
  assertBoundary
    "managed-partial-constructor-failure"
    [expressionFailure 1 [] TypedCoreCallArityUnsupported (TypedCoreArityDetail 2 1)]
  assertBoundary
    "managed-list-field-failure"
    [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
      expressionFailure 1 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
    ]
  assertBoundary
    "managed-unresolved-constructor-failure"
    [expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail]
  assertBoundary
    "managed-list-construction-failure"
    [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
  assertBoundary
    "managed-tuple-equality-failure"
    [expressionFailure 0 [] TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail]
  assertBoundary
    "managed-variant-equality-failure"
    [expressionFailure 1 [] TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail]

testManagedPatternProducerExclusions :: IO ()
testManagedPatternProducerExclusions =
  assertProductionUnsupported
    "managed nested Text pattern remains producer-owned"
    [expressionFailure 0 [0, 1] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]
    . typedCoreProductionStatus
    =<< produceFixture
      ( sourceFixtureNoExports
          "managed-nested-text-pattern"
          "case (1, \"one\") { | (number, \"one\") -> number | _ -> 0 }."
      )

testManagedPatternLowererBoundary :: IO ()
testManagedPatternLowererBoundary = do
  testManagedPatternAnalysisBoundaries
  testManagedPatternPureAnalysis

testManagedPatternAnalysisBoundaries :: IO ()
testManagedPatternAnalysisBoundaries =
  mapM_ assertPatternBoundary expectedResults
  where
    assertPatternBoundary (name, expectedFailures) =
      case lookup name managedPatternAnalysisBoundaryPrograms of
        Nothing -> failTest (name <> " managed pattern analysis boundary is missing")
        Just typedProgram -> do
          assertEqual (name <> " valid arbitrary Typed Core") [] (validateTypedProgram typedProgram)
          assertUnsupportedLowering
            (name <> " exact managed pattern boundary")
            expectedFailures
            (lowerTypedCoreExpressionDirectCall typedProgram)

    expectedResults =
      [ ("managed-incomplete-constructor-case", [expressionFailureAt 1 LoweredIRIncompletePatternCase]),
        ("managed-guarded-complete-constructor-case", [expressionFailureAt 1 LoweredIRIncompletePatternCase]),
        ("managed-incomplete-nested-case", [expressionFailureAt 1 LoweredIRIncompletePatternCase]),
        ("managed-unsupported-list-pattern", [patternFailureAt 0 [0, 0]]),
        ("managed-unsupported-text-pattern", [patternFailureAt 0 [0, 0]]),
        ("managed-unsupported-nested-or-pattern", [patternFailureAt 0 [0, 0, 0]]),
        ("managed-non-final-irrefutable-or", [expressionFailureAt 1 LoweredIRIncompletePatternCase])
      ]
    expressionFailureAt statementIndex kind =
      LoweredIRLoweringFailure
        (TypedExpressionPath ["App", "Main"] [statementIndex] [0])
        kind
        LoweredIRNoFailureDetail
    patternFailureAt statementIndex patternPath =
      LoweredIRLoweringFailure
        (TypedPatternPath ["App", "Main"] [statementIndex] patternPath)
        LoweredIRUnsupportedPattern
        LoweredIRNoFailureDetail

testManagedPatternPureAnalysis :: IO ()
testManagedPatternPureAnalysis = do
  constructorCatalog <- catalogFor ManagedProductsVariants.managedConstructorPatternProgram
  let expectedNames = [ManagedProductsVariants.noneName, ManagedProductsVariants.someName]
  case managedPatternConstructorsFor constructorCatalog ManagedProductsVariants.optionIntInfo of
    Nothing -> failTest "managed constructor catalog did not resolve Option Int"
    Just constructors -> do
      assertEqual "constructor catalog preserves source names" expectedNames (map managedPatternConstructorName constructors)
      assertEqual "constructor catalog preserves source tags" [0, 1] (map (managedConstructorTag . managedPatternConstructorLayout) constructors)
      assertEqual "constructor catalog specializes field infos" [[], [intInfo]] (map managedPatternConstructorFields constructors)
      assertEqual "constructor catalog reuses concrete layout" [optionLayoutId, optionLayoutId] (map (managedConstructorLayoutId . managedPatternConstructorLayout) constructors)
  case managedPatternConstructorFor constructorCatalog ManagedProductsVariants.someName ManagedProductsVariants.optionIntInfo of
    Just constructor ->
      assertEqual "constructor lookup specializes one field" [intInfo] (managedPatternConstructorFields constructor)
    Nothing -> failTest "managed constructor lookup did not resolve Some Int"

  constructorPlan <- analyzeProgram ManagedProductsVariants.managedConstructorPatternProgram
  case constructorPlan of
    ManagedPatternArm (ManagedConstructor someConstructor [ManagedVariable _ itemBinder]) Nothing _
      :| [ManagedPatternArm (ManagedConstructor noneConstructor []) Nothing _] -> do
        assertEqual "source arm order retains Some first" 1 (managedConstructorTag (managedPatternConstructorLayout someConstructor))
        assertEqual "source arm order retains None second" 0 (managedConstructorTag (managedPatternConstructorLayout noneConstructor))
        assertEqual "constructor binder contract is retained" (ManagedProductsVariants.patternBinder [1, 0, 0] (ManagedProductsVariants.valueName "item")) itemBinder
    other -> failTest ("unexpected constructor analysis plan: " <> Text.pack (show other))

  tuplePlan <- analyzeProgram ManagedProductsVariants.managedTuplePatternProgram
  case tuplePlan of
    ManagedPatternArm (ManagedTuple _ layoutId [ManagedVariable _ leftBinder, ManagedVariable _ rightBinder]) Nothing _ :| [] -> do
      assertEqual
        "tuple plan uses the concrete product layout"
        (LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$8:signed64")
        layoutId
      assertEqual
        "tuple plan preserves left-to-right children"
        [ ManagedProductsVariants.patternBinder [0, 0, 0] (ManagedProductsVariants.valueName "left"),
          ManagedProductsVariants.patternBinder [0, 0, 1] (ManagedProductsVariants.valueName "right")
        ]
        [leftBinder, rightBinder]
    other -> failTest ("unexpected tuple analysis plan: " <> Text.pack (show other))

  orPlan <- analyzeProgram ManagedProductsVariants.managedOrConstructorPatternProgram
  case orPlan of
    ManagedPatternArm (ManagedOr _ (leftPattern :| [rightPattern])) Nothing _ :| [ManagedPatternArm ManagedWildcard {} Nothing _] ->
      assertEqual
        "top-level alternatives share one binder contract"
        (managedPatternBinders leftPattern)
        (managedPatternBinders rightPattern)
    other -> failTest ("unexpected or-pattern analysis plan: " <> Text.pack (show other))

  distinctOrProgram <- namedPatternBoundary "managed-distinct-or-binders"
  assertEqual "distinct or-binder fixture remains valid Typed Core" [] (validateTypedProgram distinctOrProgram)
  distinctOrPlan <- analyzeProgram distinctOrProgram
  case distinctOrPlan of
    ManagedPatternArm (ManagedOr _ (leftPattern :| [rightPattern])) Nothing _ :| [ManagedPatternArm ManagedWildcard {} Nothing _] -> do
      let firstBinder = ManagedProductsVariants.patternBinder [1, 0, 0, 1] (ManagedProductsVariants.valueName "item")
      assertEqual "first alternative retains its canonical binder" [firstBinder] (managedPatternBinders leftPattern)
      assertEqual "later alternative uses the first binder identity" [firstBinder] (managedPatternBinders rightPattern)
    other -> failTest ("unexpected distinct-binder or-pattern plan: " <> Text.pack (show other))
  where
    catalogFor typedProgram =
      case typedProgram of
        TypedProgram _ [typedModule] _ ->
          case collectManagedLayoutCatalog typedModule of
            Right catalog -> pure catalog
            Left failures -> failTest ("managed catalog collection failed: " <> Text.pack (show failures))
        _ -> failTest "managed pattern fixture must contain one module"
    namedPatternBoundary name =
      case lookup name managedPatternAnalysisBoundaryPrograms of
        Just programValue -> pure programValue
        Nothing -> failTest (name <> " managed pattern boundary is missing")
    analyzeProgram typedProgram =
      case typedProgram of
        TypedProgram _ [TypedModule modulePath _ _ _ _ _ statements _] _ -> do
          catalog <- catalogFor typedProgram
          case [(statementIndex, expression) | (statementIndex, TypedExpressionStatement _ expression@TypedPatternCaseExpr {}) <- zip [0 ..] statements] of
            [(statementIndex, TypedPatternCaseExpr _ scrutinee arms)] ->
              case analyzeManagedPatternCase catalog modulePath [statementIndex] [0] scrutinee arms of
                Right plan -> pure plan
                Left failure -> failTest ("managed pattern analysis failed: " <> Text.pack (show failure))
            _ -> failTest "managed pattern fixture must contain one case expression"
        _ -> failTest "managed pattern fixture must contain one module"
    managedPatternBinders patternValue =
      case patternValue of
        ManagedVariable _ binder -> [binder]
        ManagedConstructor _ children -> concatMap managedPatternBinders children
        ManagedTuple _ _ children -> concatMap managedPatternBinders children
        ManagedAs _ binder nested -> binder : managedPatternBinders nested
        ManagedOr _ alternatives -> concatMap managedPatternBinders alternatives
        _ -> []

testManagedProductVariantProduction :: IO ()
testManagedProductVariantProduction =
  mapM_ assertProduced managedProductVariantExpectedPrograms
  where
    assertProduced (name, expectedProgram) = do
      firstRun <- produceFixture (managedProductVariantFixture name)
      secondRun <- produceFixture (managedProductVariantFixture name)
      assertEqual (name <> " repeatable exact production") firstRun secondRun
      assertEqual (name <> " expected typed validation") [] (validateTypedProgram expectedProgram)
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded validatedProgram ->
          assertEqual (name <> " exact typed program") expectedProgram (validatedTypedProgram validatedProgram)
        status -> failTest (name <> " did not produce typed core: " <> Text.pack (show status))

testManagedProductVariantLowering :: IO ()
testManagedProductVariantLowering =
  mapM_ assertProducedLowered managedProductVariantExpectedLoweredPrograms
    >> mapM_ assertLowered managedProductVariantIndependentExpectedLoweredPrograms
    >> testManagedPatternTransport
  where
    assertProducedLowered (name, expectedLoweredProgram) =
      case lookup name managedProductVariantExpectedPrograms of
        Nothing -> failTest (name <> " is missing its typed-program expectation")
        Just typedProgram -> assertLowered (name, typedProgram, expectedLoweredProgram)
    assertLowered (name, typedProgram, expectedLoweredProgram) = do
      let lowering = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertSuccessfulLowering
        (name <> " exact managed product/variant lowering")
        expectedLoweredProgram
        lowering
      assertEqual
        (name <> " valid expected Lowered IR")
        []
        (validateLoweredProgram expectedLoweredProgram)

testManagedPatternTransport :: IO ()
testManagedPatternTransport = do
  mapM_ assertTransportLowers managedPatternTransportPrograms
  directProgram <- transportProgram "managed-direct-function-result"
  directLowered <- successfulProgram "managed direct function result" directProgram
  assertEqual
    "direct managed function bodies finish with returns and no result join"
    [ ( LoweredBlockId "case$s1$2$e2$0,0$a0$body",
        [ LoweredParameter (LoweredParameterId "pattern1") ManagedProductsVariants.optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern2") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64)
        ],
        Just (LoweredReturn (LoweredBlockParameterOperand (LoweredParameterId "pattern2") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64)))
      ),
      ( LoweredBlockId "case$s1$2$e2$0,0$a1$body",
        [],
        Just (LoweredReturn (LoweredImmediateOperand (LoweredSignedIntegerImmediate LoweredIntegerWidth64 0)))
      )
    ]
    (selectedBlocks (LoweredFunctionId "App::Main::select") ["$a0$body", "$a1$body"] directLowered)

  capturedProgram <- transportProgram "managed-captured-scalar-arm"
  capturedLowered <- successfulProgram "managed captured scalar arm" capturedProgram
  assertEqual
    "closure managed arm transports capture and scrutinee but returns directly"
    [ ( LoweredBlockId "case$s1$3$e2$0,0$a0$body",
        [ LoweredParameter (LoweredParameterId "live1") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64),
          LoweredParameter (LoweredParameterId "pattern1") ManagedProductsVariants.optionRepresentation,
          LoweredParameter (LoweredParameterId "pattern2") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64)
        ],
        Just (LoweredReturn (LoweredBlockParameterOperand (LoweredParameterId "live1") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64)))
      ),
      ( LoweredBlockId "case$s1$3$e2$0,0$a1$body",
        [LoweredParameter (LoweredParameterId "live1") (LoweredSignedIntegerRepresentation LoweredIntegerWidth64)],
        Just (LoweredReturn (LoweredImmediateOperand (LoweredSignedIntegerImmediate LoweredIntegerWidth64 0)))
      )
    ]
    (selectedBlocks (LoweredFunctionId "App::Main::selectCaptured") ["$a0$body", "$a1$body"] capturedLowered)

  nestedProgram <- transportProgram "managed-nested-case-arm"
  nestedLowered <- successfulProgram "managed nested case arm" nestedProgram
  assertEqual
    "nested managed cases preserve independent ProduceValue joins"
    [ LoweredBlockId "case$s1$1$e3$0,1,1$join",
      LoweredBlockId "case$s1$1$e1$0$join"
    ]
    (joinBlockIds (LoweredFunctionId "App::Main::$entry") nestedLowered)

  closureProgram <- transportProgram "managed-closure-result-application"
  closureLowered <- successfulProgram "managed closure result application" closureProgram
  assertEqual
    "closure-valued managed result joins before application"
    ( LoweredBlock
        (LoweredBlockId "case$s1$1$e2$0,0$join")
        [LoweredParameter (LoweredParameterId "result") ManagedProductsVariants.boolClosureRepresentation]
        [ LoweredInstruction
            (LoweredTemporaryId "t1")
            LoweredBoolRepresentation
            ( LoweredClosureCall
                (LoweredBlockParameterOperand (LoweredParameterId "result") ManagedProductsVariants.boolClosureRepresentation)
                [LoweredImmediateOperand (LoweredBoolImmediate True)]
            )
        ]
        (Just (LoweredReturn (LoweredTemporaryOperand (LoweredTemporaryId "t1") LoweredBoolRepresentation)))
    )
    (namedBlock (LoweredFunctionId "App::Main::$entry") (LoweredBlockId "case$s1$1$e2$0,0$join") closureLowered)

  let requirements = collectRuntimeRequirements (onlyModule ManagedProductsVariants.managedTotalNestedConstructorPatternProgram)
  assertEqual
    "recursive managed patterns discover only the existing Text layout and no service"
    (RuntimeRequirements True mempty)
    requirements
  assertEqual "recursive managed pattern requirements retain the existing Text layout" [textLayout] (requiredRuntimeLayouts requirements)
  where
    assertTransportLowers (name, typedProgram) = do
      assertEqual (name <> " valid transport Typed Core") [] (validateTypedProgram typedProgram)
      _ <- successfulProgram name typedProgram
      pure ()
    transportProgram name =
      case lookup name managedPatternTransportPrograms of
        Just programValue -> pure programValue
        Nothing -> failTest (name <> " transport fixture is missing")
    successfulProgram label typedProgram =
      case lowerTypedCoreExpressionDirectCall typedProgram of
        LoweredIRSucceeded validatedProgram -> pure (validatedLoweredProgram validatedProgram)
        other -> failTest (label <> " expected successful lowering, got " <> Text.pack (show other))
    onlyModule (TypedProgram _ [moduleValue] _) = moduleValue
    onlyModule _ = error "managed runtime requirements fixture must contain one module"
    selectedBlocks functionId suffixes programValue =
      [ (blockId, parameters, terminator)
      | LoweredBlock blockId parameters _ terminator <- functionBlocks functionId programValue,
        any (`Text.isSuffixOf` loweredBlockIdText blockId) suffixes
      ]
    joinBlockIds functionId programValue =
      [ blockId
      | LoweredBlock blockId _ _ _ <- functionBlocks functionId programValue,
        "$join" `Text.isSuffixOf` loweredBlockIdText blockId
      ]
    namedBlock functionId expectedId programValue =
      case [block | block@(LoweredBlock blockId _ _ _) <- functionBlocks functionId programValue, blockId == expectedId] of
        [block] -> block
        _ -> error "managed transport block is missing"
    functionBlocks expectedId (LoweredProgram _ _ _ functions _) =
      case [blocks | LoweredFunction functionId _ _ _ blocks _ <- functions, functionId == expectedId] of
        [blocks] -> blocks
        _ -> error "managed transport function is missing"
    loweredBlockIdText (LoweredBlockId value) = value

testManagedConstructionLowererBoundaries :: IO ()
testManagedConstructionLowererBoundaries =
  mapM_ assertLowererBoundary expectedResults
  where
    assertLowererBoundary (name, expectedFailures) =
      case lookup name managedConstructionLowererBoundaryPrograms of
        Nothing -> failTest (name <> " managed construction boundary is missing")
        Just typedProgram -> do
          let lowering = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " valid arbitrary Typed Core") [] (validateTypedProgram typedProgram)
          assertUnsupportedLowering (name <> " exact lowerer boundary") expectedFailures lowering

    expectedResults =
      [ ( "managed-bare-nonnullary-constructor-lowerer",
          [ lowererExpressionFailure
              1
              LoweredIRCallableValueUnsupported
              (LoweredIRNameFailureDetail (TypedResolvedName TypedCurrentModule TypedConstructorNamespace "Some"))
          ]
        ),
        ( "managed-partial-constructor-lowerer",
          [ lowererExpressionFailure
              1
              LoweredIRCallArityUnsupported
              (LoweredIRArityFailureDetail 2 1)
          ]
        ),
        ( "managed-unsupported-field-recipe-lowerer",
          [ lowererExpressionFailure
              1
              LoweredIRUnsupportedRepresentation
              (LoweredIRRecipeFailureDetail (TypedManagedListRecipe (TypedSignedIntegerRecipe 64)))
          ]
        ),
        ( "managed-unsupported-phantom-list-argument-lowerer",
          [ lowererExpressionFailure
              1
              LoweredIRUnsupportedRepresentation
              ( LoweredIRRecipeFailureDetail
                  ( TypedManagedVariantRecipe
                      (TypedResolvedName TypedCurrentModule TypedTypeNamespace "Phantom")
                      [SemanticList SemanticInt]
                  )
              )
          ]
        ),
        ( "managed-unsupported-nested-phantom-list-argument-lowerer",
          [ lowererExpressionFailure
              2
              LoweredIRUnsupportedRepresentation
              ( LoweredIRRecipeFailureDetail
                  ( TypedManagedVariantRecipe
                      (TypedResolvedName TypedCurrentModule TypedTypeNamespace "Phantom")
                      [ SemanticData
                          (TypedResolvedName TypedCurrentModule TypedTypeNamespace "Inner")
                          [SemanticList SemanticInt]
                      ]
                  )
              )
          ]
        ),
        ( "managed-product-equality-lowerer",
          [ LoweredIRLoweringFailure
              (TypedExpressionPath ["App", "Main"] [0] [0])
              LoweredIRUnsupportedRepresentation
              (LoweredIRRecipeFailureDetail (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe]))
          ]
        ),
        ( "managed-variant-equality-lowerer",
          [ lowererExpressionFailure
              1
              LoweredIRUnsupportedRepresentation
              ( LoweredIRRecipeFailureDetail
                  ( TypedManagedVariantRecipe
                      (TypedResolvedName TypedCurrentModule TypedTypeNamespace "Option")
                      [SemanticInt]
                  )
              )
          ]
        )
      ]
    lowererExpressionFailure statementIndex kind detail =
      LoweredIRLoweringFailure
        (TypedExpressionPath ["App", "Main"] [statementIndex] [0])
        kind
        detail

testManagedConstructorClosureCapture :: IO ()
testManagedConstructorClosureCapture =
  assertCompleteProduction
    "constructor closure capture"
    ( sourceFixtureNoExports
        "constructor-closure-capture"
        ( Text.unlines
            [ "data Box = Box Int.",
              "\\(item) -> Box item."
            ]
        )
    )

testManagedGenericConstructorFieldSpecialization :: IO ()
testManagedGenericConstructorFieldSpecialization =
  assertCompleteProduction
    "generic constructor field specialization"
    ( sourceFixtureNoExports
        "generic-constructor-field-specialization"
        ( Text.unlines
            [ "data Option a = None | Some a.",
              "item :: Option(UInt8).",
              "item = Some 1.",
              "item."
            ]
        )
    )

testManagedConstructorSourceOrder :: IO ()
testManagedConstructorSourceOrder =
  assertCompleteProduction
    "constructor source order"
    ( sourceFixtureNoExports
        "constructor-source-order"
        ( Text.unlines
            [ "data A = C Int.",
              "first = C 1.",
              "data B = C Text.",
              "second = C \"two\".",
              "data D = C Bool.",
              "third = C True.",
              "(first, second, third)."
            ]
        )
    )

testManagedConstructorRebindingExport :: IO ()
testManagedConstructorRebindingExport = do
  let fixture =
        sourceFixture
          "constructor-rebinding-export"
          ( Text.unlines
              [ "module App::Main (constructor C) {",
                "data A = C Int.",
                "data B = C Text.",
                "C \"two\".",
                "}"
              ]
          )
  assertCompleteProduction "constructor rebinding export" fixture
  production <- produceFixture fixture
  case typedCoreProductionStatus production of
    TypedCoreProductionSucceeded validatedProgram ->
      assertEqual
        "constructor export retains only its source-visible declaration"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "B"]
        (interfaceDataNames (validatedTypedProgram validatedProgram))
    status -> failTest ("constructor rebinding export did not produce typed core: " <> Text.pack (show status))

testManagedStandaloneConstructorDependencyRebindingExport :: IO ()
testManagedStandaloneConstructorDependencyRebindingExport = do
  let fixture =
        sourceFixture
          "standalone-constructor-dependency-rebinding-export"
          ( Text.unlines
              [ "module App::Main (constructor C) {",
                "data A = C Int.",
                "a = C 1.",
                "data B = C A.",
                "C a.",
                "}"
              ]
          )
      abstractTypeFixture =
        sourceFixture
          "abstract-type-standalone-constructor-dependency-rebinding-export"
          ( Text.unlines
              [ "module App::Main (type A, constructor C) {",
                "data A = C Int.",
                "a = C 1.",
                "data B = C A.",
                "C a.",
                "}"
              ]
          )
  assertCompleteProduction "standalone constructor dependency rebinding export" fixture
  production <- produceFixture fixture
  case typedCoreProductionStatus production of
    TypedCoreProductionSucceeded validatedProgram ->
      assertEqual
        "standalone constructor export retains its source-visible owner and private dependency"
        [ TypedResolvedName TypedCurrentModule TypedTypeNamespace "A",
          TypedResolvedName TypedCurrentModule TypedTypeNamespace "B"
        ]
        (interfaceDataNames (validatedTypedProgram validatedProgram))
    status -> failTest ("standalone constructor dependency rebinding export did not produce typed core: " <> Text.pack (show status))
  abstractTypeProduction <- produceFixture abstractTypeFixture
  assertProductionUnsupported
    "abstract type and standalone constructor reject ownership that the unchanged schema cannot represent"
    [ TypedCoreProductionFailure
        (TypedCoreProductionModulePath ["App", "Main"])
        TypedCoreUnsupportedExport
        (TypedCoreNameDetail "C")
    ]
    (typedCoreProductionStatus abstractTypeProduction)

testManagedTypeSelectorRebindingExport :: IO ()
testManagedTypeSelectorRebindingExport = do
  let fixture =
        sourceFixture
          "type-selector-constructor-rebinding-export"
          ( Text.unlines
              [ "module App::Main (type A(..)) {",
                "data A = C Int.",
                "data B = C Text.",
                "C \"two\".",
                "}"
              ]
          )
  assertCompleteProduction "type-selector constructor rebinding export" fixture
  production <- produceFixture fixture
  case typedCoreProductionStatus production of
    TypedCoreProductionSucceeded validatedProgram ->
      assertEqual
        "type selector retains its declared constructor owner"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "A"]
        (interfaceDataNames (validatedTypedProgram validatedProgram))
    status -> failTest ("type-selector constructor rebinding export did not produce typed core: " <> Text.pack (show status))

testManagedPrivateDataInterfaceDependencies :: IO ()
testManagedPrivateDataInterfaceDependencies = do
  let constructorFixture =
        sourceFixture
          "managed-private-data-interface-dependencies"
          ( Text.unlines
              [ "module App::Main (type Public(..)) {",
                "data Hidden = Hidden Int.",
                "data Public = Public Hidden.",
                "Public (Hidden 1).",
                "}"
              ]
          )
      valueFixture =
        sourceFixture
          "managed-private-value-interface-dependency"
          ( Text.unlines
              [ "module App::Main (value make) {",
                "data Hidden = Hidden Int.",
                "make :: Int -> Hidden.",
                "make = \\(item) -> Hidden item.",
                "make 1.",
                "}"
              ]
          )
  assertCompleteProduction "managed private constructor dependency" constructorFixture
  constructorProduction <- produceFixture constructorFixture
  case typedCoreProductionStatus constructorProduction of
    TypedCoreProductionSucceeded validatedProgram ->
      assertEqual
        "private constructor dependencies remain metadata without becoming exports"
        [ TypedResolvedName TypedCurrentModule TypedTypeNamespace "Hidden",
          TypedResolvedName TypedCurrentModule TypedTypeNamespace "Public"
        ]
        (interfaceDataNames (validatedTypedProgram validatedProgram))
    status -> failTest ("private constructor dependency fixture did not produce typed core: " <> Text.pack (show status))
  assertCompleteProduction "managed private value dependency" valueFixture
  valueProduction <- produceFixture valueFixture
  case typedCoreProductionStatus valueProduction of
    TypedCoreProductionSucceeded validatedProgram ->
      assertEqual
        "private value dependencies remain metadata without becoming exports"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "Hidden"]
        (interfaceDataNames (validatedTypedProgram validatedProgram))
    status -> failTest ("private value dependency fixture did not produce typed core: " <> Text.pack (show status))

testManagedNestedVariantProductModuleIdentity :: IO ()
testManagedNestedVariantProductModuleIdentity = do
  appLayoutIds <- layoutIdsFor ["App", "Main"]
  libLayoutIds <- layoutIdsFor ["Lib", "Main"]
  assertEqual
    "App nested variant product identity"
    [ LoweredLayoutId "jazz.layout.product.v1$fields2$45:variant$module2$3:App$4:Main$name$3:Box$args0$8:signed64",
      LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$3:Box$args0"
    ]
    appLayoutIds
  assertEqual
    "Lib nested variant product identity"
    [ LoweredLayoutId "jazz.layout.product.v1$fields2$45:variant$module2$3:Lib$4:Main$name$3:Box$args0$8:signed64",
      LoweredLayoutId "jazz.layout.variant.v1$module2$3:Lib$4:Main$name$3:Box$args0"
    ]
    libLayoutIds
  where
    layoutIdsFor modulePath = do
      let programValue = moduleIdentityProgram modulePath
      assertEqual "module identity fixture validates" [] (validateTypedProgram programValue)
      case programValue of
        TypedProgram _ [moduleValue] _ ->
          case collectManagedLayoutCatalog moduleValue of
            Left failures -> failTest ("module identity catalog failed: " <> Text.pack (show failures))
            Right catalog -> pure [layoutId | LoweredLayout layoutId _ <- orderedManagedLayouts catalog]
        _ -> failTest "module identity fixture must contain exactly one module"

    moduleIdentityProgram modulePath =
      let boxTypeName = TypedResolvedName TypedCurrentModule TypedTypeNamespace "Box"
          boxConstructorName = TypedResolvedName TypedCurrentModule TypedConstructorNamespace "Box"
          boxBinder = TypedBinderId (modulePath, [0, 0], boxConstructorName)
          boxInfo = TypedNodeInfo (SemanticData boxTypeName []) (TypedManagedVariantRecipe boxTypeName []) [] []
          productInfo =
            TypedNodeInfo
              (SemanticTuple [SemanticData boxTypeName [], SemanticInt])
              (TypedManagedProductRecipe [TypedManagedVariantRecipe boxTypeName [], TypedSignedIntegerRecipe 64])
              []
              []
          declaration =
            TypedDataDeclaration
              (TypedSpan 1 1)
              boxTypeName
              []
              [TypedConstructorDeclaration boxBinder boxConstructorName [] []]
          moduleValue =
            TypedModule
              modulePath
              (TypedSourcePath "src/App/Main.jz")
              []
              []
              (TypedModuleInterface [] [] [] [])
              []
              [ TypedDataStatement declaration,
                TypedExpressionStatement
                  (TypedSpan 2 1)
                  ( TypedTupleExpr
                      productInfo
                      [ TypedVariableExpr boxInfo boxConstructorName (Just boxBinder),
                        TypedLiteralExpr
                          (TypedNodeInfo SemanticInt (TypedSignedIntegerRecipe 64) [] [])
                          (TypedIntegerLiteral "1")
                      ]
                  )
              ]
              productInfo
       in TypedProgram Nothing [moduleValue] modulePath

testManagedStructuredFailureAccumulation :: IO ()
testManagedStructuredFailureAccumulation = do
  let fixture =
        sourceFixtureNoExports
          "structured-failure-accumulation"
          ( Text.unlines
              [ "data A = A List(Int).",
                "data B = B List(Int).",
                "[1]."
              ]
          )
      expectedFailures =
        [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
          statementFailure 1 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
          expressionFailure 2 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
        ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "structured failure accumulation is repeatable" firstRun secondRun
  assertProductionUnsupported
    "structured failure accumulation preserves source order"
    expectedFailures
    (typedCoreProductionStatus firstRun)

testManagedStructuredModuleFailureOrder :: IO ()
testManagedStructuredModuleFailureOrder = do
  production <-
    produceFixture
      ( sourceFixtureNoExports
          "structured-module-failure-order"
          "data A = A List(Int)."
      )
  assertProductionUnsupported
    "structured declaration failures precede missing module result failures"
    [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
      TypedCoreProductionFailure
        (TypedCoreProductionModulePath ["App", "Main"])
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail
    ]
    (typedCoreProductionStatus production)

interfaceDataNames :: TypedProgram -> [TypedCoreName]
interfaceDataNames (TypedProgram _ [TypedModule _ _ _ _ (TypedModuleInterface _ datas _ _) _ _ _] _) =
  [name | TypedDataInterface (TypedDataDeclaration _ name _ _) <- datas]
interfaceDataNames _ = []

testManagedProductVariantLayoutCatalog :: IO ()
testManagedProductVariantLayoutCatalog = do
  assertCatalog "managed-tuple" [tupleLayout]
  assertCatalog "managed-option" [optionLayout]
  assertCatalog "managed-exported-option" [optionLayout]
  assertCatalog "managed-tree" [treeLayout]
  assertEqual "combined managed catalog fixture validates" [] (validateTypedProgram managedLayoutCatalogProgram)
  case collectManagedLayoutCatalog (onlyModule managedLayoutCatalogProgram) of
    Left failures -> failTest ("combined managed catalog failed: " <> Text.pack (show failures))
    Right catalog ->
      let layouts = orderedManagedLayouts catalog
       in assertEqual
            "combined managed catalog preserves discovery order and semantic identity"
            combinedLayouts
            layouts
            >> assertEqual
              "runtime layouts precede managed discovery order"
              (LoweredLayout (LoweredLayoutId "jazz.layout.text.v1") LoweredTextLayout : combinedLayouts)
              (requiredRuntimeLayouts (requirementsForManagedLayouts layouts) <> layouts)
  let optionModule = onlyModule (expectedProgram "managed-option")
      someBinder = optionSomeBinder optionModule
  case collectManagedLayoutCatalog optionModule of
    Left failures -> failTest ("managed-option catalog failed: " <> Text.pack (show failures))
    Right catalog ->
      assertEqual
        "managed-option Some constructor layout"
        ( Just
            ManagedConstructorLayout
              { managedConstructorLayoutId = optionLayoutId,
                managedConstructorTag = 1,
                managedConstructorFields = [LoweredSignedIntegerRepresentation LoweredIntegerWidth64]
              }
        )
        ( constructorLayoutFor
            catalog
            someBinder
            [ TypedInstantiation
                someBinder
                [TypedTypeArgument (TypedTypeParameterId 0) SemanticInt]
                Nothing
            ]
        )
  where
    assertCatalog name expectedLayouts = do
      let programValue = expectedProgram name
      assertEqual (name <> " catalog fixture validates") [] (validateTypedProgram programValue)
      case collectManagedLayoutCatalog (onlyModule programValue) of
        Left failures -> failTest (name <> " catalog failed: " <> Text.pack (show failures))
        Right catalog -> assertEqual (name <> " exact managed layouts") expectedLayouts (orderedManagedLayouts catalog)

    expectedProgram name =
      case lookup name managedProductVariantExpectedPrograms of
        Just programValue -> programValue
        Nothing -> error "managed catalog expected program is missing"

    onlyModule (TypedProgram _ [moduleValue] _) = moduleValue
    onlyModule _ = error "managed catalog fixture must contain exactly one module"

    optionSomeBinder (TypedModule _ _ _ _ _ _ (TypedDataStatement (TypedDataDeclaration _ _ _ [_, TypedConstructorDeclaration binder _ _ _]) : _) _) = binder
    optionSomeBinder _ = error "managed-option fixture must retain Some as its second constructor"

    productBoolTextId = LoweredLayoutId "jazz.layout.product.v1$fields2$4:bool$4:text"
    leftBoxId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$7:LeftBox$args0"
    rightBoxId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$8:RightBox$args0"
    optionBoolId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$4:bool"
    optionTextId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$4:text"
    evenId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$4:Even$args0"
    oddId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$3:Odd$args0"
    combinedLayouts =
      [ LoweredLayout productBoolTextId (LoweredProductLayout [LoweredBoolRepresentation, textRepresentation]),
        LoweredLayout leftBoxId (LoweredVariantLayouts [LoweredVariantLayout 0 [LoweredBoolRepresentation]]),
        LoweredLayout rightBoxId (LoweredVariantLayouts [LoweredVariantLayout 0 [LoweredBoolRepresentation]]),
        LoweredLayout
          optionBoolId
          (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [LoweredBoolRepresentation]]),
        LoweredLayout
          optionTextId
          (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [textRepresentation]]),
        treeLayout,
        LoweredLayout
          evenId
          ( LoweredVariantLayouts
              [ LoweredVariantLayout 0 [LoweredManagedReferenceRepresentation oddId],
                LoweredVariantLayout 1 []
              ]
          ),
        LoweredLayout
          oddId
          (LoweredVariantLayouts [LoweredVariantLayout 0 [LoweredManagedReferenceRepresentation evenId]])
      ]

assertBoundary :: Text -> [TypedCoreProductionFailure] -> IO ()
assertBoundary name expectedFailures = do
  let fixture = managedProductVariantFixture name
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual (name <> " inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
  assertEqual (name <> " repeatable production") firstRun secondRun
  assertProductionUnsupported
    (name <> " exact producer boundary")
    expectedFailures
    (typedCoreProductionStatus firstRun)

expressionFailure :: Int -> [Int] -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> TypedCoreProductionFailure
expressionFailure statementIndex childPath kind detail =
  TypedCoreProductionFailure
    (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
    kind
    detail

statementFailure :: Int -> TypedCoreProductionFailureKind -> TypedCoreProductionFailureDetail -> TypedCoreProductionFailure
statementFailure statementIndex kind detail =
  TypedCoreProductionFailure
    (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
    kind
    detail
