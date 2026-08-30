{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManagedProductsVariantsTests where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedProductsVariants
  ( optionIntInfo,
    optionLayout,
    optionLayoutId,
    someName,
    textRepresentation,
    treeLayout,
    tupleLayout,
  )
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source (sourceFixture, sourceFixtureNoExports)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics (diagnosticCode)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
import Jazz.Compiler.LoweredIR.Lower.Requirements
  ( requiredRuntimeLayouts,
    requirementsForManagedLayouts,
  )
import Jazz.Compiler.LoweredIR.Lower.Shapes (analyzeTypedModule)
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
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
  testManagedPatternProducerBoundaries

testManagedPatternProducerBoundaries :: IO ()
testManagedPatternProducerBoundaries = do
  assertNestedOrSourceBoundary
  mapM_ (uncurry assertBoundary) producerBoundaryExpectations
  where
    producerBoundaryExpectations =
      [ ( "managed-list-pattern-boundary",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "managed-cons-pattern-boundary",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "managed-text-literal-pattern-boundary",
          [expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail]
        ),
        ( "managed-pattern-lambda-boundary",
          [ statementFailure 1 TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail "choose"),
            expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
            expressionFailure 1 [0] TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
            expressionFailure 2 [] TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail
          ]
        )
      ]
    assertNestedOrSourceBoundary = do
      resolution <- resolveFixture (managedProductVariantFixture "managed-nested-or-pattern-boundary")
      case resolution of
        Left diagnostic ->
          assertEqual
            "managed-nested-or-pattern boundary code"
            "E4004"
            (diagnosticCodeText (diagnosticCode diagnostic))
        Right _ -> failTest "managed-nested-or-pattern-boundary unexpectedly resolved"

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
        TypedCoreProductionSucceeded actualProgram ->
          assertEqual (name <> " exact typed program") expectedProgram actualProgram
        status -> failTest (name <> " did not produce typed core: " <> Text.pack (show status))

testManagedProductVariantLowering :: IO ()
testManagedProductVariantLowering =
  mapM_ assertProducedLowered managedProductVariantExpectedLoweredPrograms
    >> mapM_ assertLowered managedProductVariantIndependentExpectedLoweredPrograms
  where
    assertProducedLowered (name, expectedLoweredProgram) =
      case lookup name managedProductVariantExpectedPrograms of
        Nothing -> failTest (name <> " is missing its typed-program expectation")
        Just typedProgram -> assertLowered (name, typedProgram, expectedLoweredProgram)
    assertLowered (name, typedProgram, expectedLoweredProgram) = do
      let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
          secondRun = lowerTypedCoreExpressionDirectCall typedProgram
      assertEqual (name <> " valid typed core") [] (validateTypedProgram typedProgram)
      assertEqual (name <> " repeatable lowering") firstRun secondRun
      assertEqual
        (name <> " exact managed product/variant lowering")
        (LoweredIRSucceeded expectedLoweredProgram)
        firstRun
      assertEqual
        (name <> " valid expected Lowered IR")
        []
        (validateLoweredProgram expectedLoweredProgram)

testManagedConstructionLowererBoundaries :: IO ()
testManagedConstructionLowererBoundaries =
  mapM_ assertLowererBoundary expectedResults
    >> testManagedPatternLowererProfile
  where
    assertLowererBoundary (name, expectedFailures) =
      case lookup name managedConstructionLowererBoundaryPrograms of
        Nothing -> failTest (name <> " managed construction boundary is missing")
        Just typedProgram -> do
          let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
              secondRun = lowerTypedCoreExpressionDirectCall typedProgram
          assertEqual (name <> " valid arbitrary Typed Core") [] (validateTypedProgram typedProgram)
          assertEqual (name <> " repeatable rejection") firstRun secondRun
          assertEqual (name <> " exact lowerer boundary") (LoweredIRUnsupported expectedFailures) firstRun

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
                      [TypedListType TypedIntType]
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
                      [ TypedDataType
                          (TypedResolvedName TypedCurrentModule TypedTypeNamespace "Inner")
                          [TypedListType TypedIntType]
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
                      [TypedIntType]
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

testManagedPatternLowererProfile :: IO ()
testManagedPatternLowererProfile = do
  mapM_ assertAccepted acceptedNames
  mapM_ assertRejected expectedRejections
  where
    assertAccepted name =
      case lookup name managedConstructionLowererBoundaryPrograms of
        Nothing -> failTest (name <> " managed pattern acceptance fixture is missing")
        Just typedProgram -> do
          assertEqual (name <> " valid arbitrary Typed Core") [] (validateTypedProgram typedProgram)
          case analyzeTypedModule (onlyModule typedProgram) of
            Left failures -> failTest (name <> " did not enter the managed pattern profile: " <> Text.pack (show failures))
            Right _ -> pure ()

    assertRejected (name, expectedFailure) =
      case lookup name managedConstructionLowererBoundaryPrograms of
        Nothing -> failTest (name <> " managed pattern rejection fixture is missing")
        Just typedProgram -> do
          assertEqual (name <> " valid arbitrary Typed Core") [] (validateTypedProgram typedProgram)
          assertEqual
            (name <> " exact managed pattern profile rejection")
            (Left [expectedFailure])
            (case analyzeTypedModule (onlyModule typedProgram) of Left failures -> Left failures; Right _ -> Right ())

    acceptedNames =
      [ "managed-closed-variant-pattern-profile",
        "managed-nested-constructor-tuple-pattern-profile",
        "managed-total-tuple-pattern-profile"
      ]

    expectedRejections =
      [ incomplete "managed-missing-constructor-pattern-profile" 2,
        incomplete "managed-guarded-constructors-pattern-profile" 2,
        incomplete "managed-incomplete-tuple-pattern-profile" 0,
        unsupported "managed-list-pattern-profile" 0 [0, 0],
        unsupported "managed-nested-or-pattern-profile" 1 [0, 0, 0],
        unsupported "managed-text-literal-pattern-profile" 1 [0, 0, 0]
      ]

    incomplete name statementIndex =
      ( name,
        LoweredIRLoweringFailure
          (TypedExpressionPath ["App", "Main"] [statementIndex] [0])
          LoweredIRIncompletePatternCase
          LoweredIRNoFailureDetail
      )
    unsupported name statementIndex patternPath =
      ( name,
        LoweredIRLoweringFailure
          (TypedPatternPath ["App", "Main"] [statementIndex] patternPath)
          LoweredIRUnsupportedPattern
          LoweredIRNoFailureDetail
      )
    onlyModule (TypedProgram _ [moduleValue] _) = moduleValue
    onlyModule _ = error "managed pattern profile fixture must contain exactly one module"

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
    TypedCoreProductionSucceeded programValue ->
      assertEqual
        "constructor export retains only its source-visible declaration"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "B"]
        (interfaceDataNames programValue)
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
    TypedCoreProductionSucceeded programValue ->
      assertEqual
        "standalone constructor export retains its source-visible owner and private dependency"
        [ TypedResolvedName TypedCurrentModule TypedTypeNamespace "A",
          TypedResolvedName TypedCurrentModule TypedTypeNamespace "B"
        ]
        (interfaceDataNames programValue)
    status -> failTest ("standalone constructor dependency rebinding export did not produce typed core: " <> Text.pack (show status))
  abstractTypeProduction <- produceFixture abstractTypeFixture
  assertEqual
    "abstract type and standalone constructor reject ownership that the unchanged schema cannot represent"
    ( TypedCoreProductionUnsupported
        [ TypedCoreProductionFailure
            (TypedCoreProductionModulePath ["App", "Main"])
            TypedCoreUnsupportedExport
            (TypedCoreNameDetail "C")
        ]
    )
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
    TypedCoreProductionSucceeded programValue ->
      assertEqual
        "type selector retains its declared constructor owner"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "A"]
        (interfaceDataNames programValue)
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
    TypedCoreProductionSucceeded programValue ->
      assertEqual
        "private constructor dependencies remain metadata without becoming exports"
        [ TypedResolvedName TypedCurrentModule TypedTypeNamespace "Hidden",
          TypedResolvedName TypedCurrentModule TypedTypeNamespace "Public"
        ]
        (interfaceDataNames programValue)
    status -> failTest ("private constructor dependency fixture did not produce typed core: " <> Text.pack (show status))
  assertCompleteProduction "managed private value dependency" valueFixture
  valueProduction <- produceFixture valueFixture
  case typedCoreProductionStatus valueProduction of
    TypedCoreProductionSucceeded programValue ->
      assertEqual
        "private value dependencies remain metadata without becoming exports"
        [TypedResolvedName TypedCurrentModule TypedTypeNamespace "Hidden"]
        (interfaceDataNames programValue)
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
          boxInfo = TypedNodeInfo (TypedDataType boxTypeName []) (TypedManagedVariantRecipe boxTypeName []) [] []
          productInfo =
            TypedNodeInfo
              (TypedTupleType [TypedDataType boxTypeName [], TypedIntType])
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
                          (TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] [])
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
  assertEqual
    "structured failure accumulation preserves source order"
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus firstRun)

testManagedStructuredModuleFailureOrder :: IO ()
testManagedStructuredModuleFailureOrder = do
  production <-
    produceFixture
      ( sourceFixtureNoExports
          "structured-module-failure-order"
          "data A = A List(Int)."
      )
  assertEqual
    "structured declaration failures precede missing module result failures"
    ( TypedCoreProductionUnsupported
        [ statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail,
          TypedCoreProductionFailure
            (TypedCoreProductionModulePath ["App", "Main"])
            TypedCoreUnsupportedRootExpression
            TypedCoreUnsupportedRootDetail
        ]
    )
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
                [TypedTypeArgument (TypedTypeParameterId 0) TypedIntType]
                Nothing
            ]
        )
        >> assertEqual
          "managed-option Some constructor pattern layout"
          ( Just
              ManagedConstructorLayout
                { managedConstructorLayoutId = optionLayoutId,
                  managedConstructorTag = 1,
                  managedConstructorFields = [LoweredSignedIntegerRepresentation LoweredIntegerWidth64]
                }
          )
          (constructorPatternLayoutFor catalog optionIntInfo someName)
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
  assertEqual
    (name <> " exact producer boundary")
    (TypedCoreProductionUnsupported expectedFailures)
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
