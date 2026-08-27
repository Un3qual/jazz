{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.ManagedProductsVariantsTests where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
import Jazz.Compiler.LoweredIR.Lower.Requirements
  ( requiredRuntimeLayouts,
    requirementsForManagedLayouts,
  )
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
    [statementFailure 0 TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail]
  assertBoundary
    "managed-unresolved-constructor-failure"
    [expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreDataValueDetail]

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

    tupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$4:text"
    optionLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$3:int"
    treeLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$4:Tree$args1$3:int"
    textRepresentation = LoweredManagedReferenceRepresentation (LoweredLayoutId "jazz.layout.text.v1")
    tupleLayout =
      LoweredLayout
        tupleLayoutId
        (LoweredProductLayout [LoweredSignedIntegerRepresentation LoweredIntegerWidth64, textRepresentation])
    optionLayout =
      LoweredLayout
        optionLayoutId
        ( LoweredVariantLayouts
            [ LoweredVariantLayout 0 [],
              LoweredVariantLayout 1 [LoweredSignedIntegerRepresentation LoweredIntegerWidth64]
            ]
        )
    treeRepresentation = LoweredManagedReferenceRepresentation treeLayoutId
    treeLayout =
      LoweredLayout
        treeLayoutId
        ( LoweredVariantLayouts
            [ LoweredVariantLayout 0 [LoweredSignedIntegerRepresentation LoweredIntegerWidth64],
              LoweredVariantLayout 1 [treeRepresentation, treeRepresentation]
            ]
        )
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
