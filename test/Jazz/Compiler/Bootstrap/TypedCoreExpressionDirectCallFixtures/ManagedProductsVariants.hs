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
    ("managed-tree", managedTreeProgram)
  ]

managedProductVariantExpectedLoweredPrograms :: [(Text, LoweredProgram)]
managedProductVariantExpectedLoweredPrograms =
  [ ("managed-tuple", managedTupleLoweredProgram),
    ("managed-option", managedOptionLoweredProgram),
    ("managed-exported-option", managedOptionLoweredProgram),
    ("managed-tree", managedTreeLoweredProgram)
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

textLayoutId, tupleLayoutId, optionLayoutId, treeLayoutId, tupleVariantLayoutId, textBoxLayoutId, closureBoxLayoutId, closureEnvironmentLayoutId, productBoxLayoutId, outerLayoutId, manifestTupleLayoutId, manifestDataLayoutId :: LoweredLayoutId
textLayoutId = LoweredLayoutId "jazz.layout.text.v1"
tupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$4:text"
optionLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$6:Option$args1$3:int"
treeLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$4:Tree$args1$3:int"
tupleVariantLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$28:variant$6:Option$args1$3:int$8:signed64"
textBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$7:TextBox$args0"
closureBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$10:ClosureBox$args0"
closureEnvironmentLayoutId = LoweredLayoutId "$jz1$closure-env$m2$3:App$4:Main$p3$1,0,1$n4:flag"
productBoxLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$10:ProductBox$args0"
outerLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$5:Outer$args0"
manifestTupleLayoutId = LoweredLayoutId "jazz.layout.product.v1$fields2$8:signed64$8:signed64"
manifestDataLayoutId = LoweredLayoutId "jazz.layout.variant.v1$module2$3:App$4:Main$name$3:Box$args0"

textRepresentation, tupleRepresentation, optionRepresentation, treeRepresentation, tupleVariantRepresentation, textBoxRepresentation, closureBoxRepresentation, closureEnvironmentRepresentation, productBoxRepresentation, outerRepresentation, manifestTupleRepresentation, manifestDataRepresentation :: LoweredRepresentation
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
manifestTupleRepresentation = LoweredManagedReferenceRepresentation manifestTupleLayoutId
manifestDataRepresentation = LoweredManagedReferenceRepresentation manifestDataLayoutId

textLayout, tupleLayout, optionLayout, treeLayout, tupleVariantLayout, textBoxLayout, closureBoxLayout, closureEnvironmentLayout, productBoxLayout, outerLayout, manifestTupleLayout, manifestDataLayout :: LoweredLayout
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
manifestTupleLayout = LoweredLayout manifestTupleLayoutId (LoweredProductLayout [int64Representation, int64Representation])
manifestDataLayout = LoweredLayout manifestDataLayoutId (LoweredVariantLayouts [LoweredVariantLayout 0 []])

boolClosureRepresentation :: LoweredRepresentation
boolClosureRepresentation =
  LoweredClosureRepresentation
    (LoweredCallSignature [LoweredBoolRepresentation] LoweredBoolRepresentation)

closureFunctionId :: LoweredFunctionId
closureFunctionId = LoweredFunctionId "$jz1$lambda-fn$m2$3:App$4:Main$p3$1,0,1$n4:flag"

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
        (TypedVariableExpr boxInfo boxName (Just boxBinder))
    ]
    boxInfo
  where
    dataName = typeName "Box"
    boxName = constructorName "Box"
    boxBinder = constructorBinder 0 boxName
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        dataName
        []
        [TypedConstructorDeclaration boxBinder boxName [] []]
    boxInfo = variantInfo dataName []

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
  case fieldInfos of
    [] -> TypedVariableExpr instantiatedResultInfo name (Just owner)
    _ -> saturated constructorExpression fieldInfos arguments
  where
    typeArguments =
      case typedExpressionType resultInfo of
        TypedDataType _ argumentsValue ->
          zipWith TypedTypeArgument [TypedTypeParameterId index | index <- [0 ..]] argumentsValue
        _ -> []
    instantiation = TypedInstantiation owner typeArguments Nothing
    instantiatedResultInfo = addInstantiation resultInfo instantiation
    constructorInfo =
      TypedNodeInfo
        (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldInfos)
        (TypedClosureRecipe (map typedExpressionRecipe fieldInfos) (typedExpressionRecipe resultInfo))
        [instantiation]
        []
    constructorExpression = TypedVariableExpr constructorInfo name (Just owner)

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

addInstantiation :: TypedNodeInfo -> TypedInstantiation -> TypedNodeInfo
addInstantiation (TypedNodeInfo typeValue recipe _ evidence) instantiation =
  TypedNodeInfo typeValue recipe [instantiation] evidence

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

bareConstructorSource, partialConstructorSource, listFieldSource, unresolvedConstructorSource :: Text
bareConstructorSource = Text.unlines ["data Box = Box Int.", "Box."]
partialConstructorSource = Text.unlines ["data Pair a b = Pair a b.", "Pair 1."]
listFieldSource = Text.unlines ["data Box = Box List(Int).", "Box [1]."]
unresolvedConstructorSource = Text.unlines ["data Option a = None | Some a.", "None."]

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
monomorphicConstructorCall owner name resultInfo fieldInfos arguments =
  case fieldInfos of
    [] -> TypedVariableExpr resultInfo name (Just owner)
    _ -> saturated constructorExpression fieldInfos arguments
  where
    constructorInfo =
      TypedNodeInfo
        (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldInfos)
        (TypedClosureRecipe (map typedExpressionRecipe fieldInfos) (typedExpressionRecipe resultInfo))
        []
        []
    constructorExpression = TypedVariableExpr constructorInfo name (Just owner)
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
        _ -> error "monomorphic constructor fixture must be exactly saturated"
