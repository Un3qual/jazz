{-# LANGUAGE OverloadedStrings #-}

-- | Fixed valid and invalid validator fixture manifests.
module Jazz.Compiler.Bootstrap.TypedCoreContract.FixedFixtures where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreContract.ReviewFixtures
import Jazz.Compiler.TypedCore

data ValidFixture = ValidFixture
  { validFixtureName :: Text,
    validFixtureProgram :: TypedProgram
  }

expectedValidFixtureNames :: [Text]
expectedValidFixtureNames =
  [ "scalar-aliases-widths",
    "resolved-name-origins",
    "builtin-generated-names",
    "list-tuple-data-recipes",
    "callable-recipes",
    "staged-callable-parameter",
    "staged-callable-data-field",
    "monomorphic-binding",
    "generalized-binding",
    "implicit-instantiation",
    "explicit-instantiation",
    "explicit-capability-evidence",
    "inferred-capability-evidence",
    "qualified-method-selection",
    "partial-method-candidates",
    "patterns-binders",
    "or-pattern-alignment",
    "callable-shapes-binder-references",
    "multi-module-interface",
    "lexical-capture",
    "curried-applications"
  ]

validFixtures :: [ValidFixture]
validFixtures =
  [ ValidFixture name (validProgram name)
  | name <- expectedValidFixtureNames
  ]

validProgram :: Text -> TypedProgram
validProgram fixtureName =
  case fixtureName of
    "scalar-aliases-widths" -> scalarAliasesWidthsProgram
    "resolved-name-origins" -> resolvedNameOriginsProgram
    "builtin-generated-names" -> builtinGeneratedNamesProgram
    "list-tuple-data-recipes" -> listTupleDataRecipesProgram
    "callable-recipes" -> callableRecipesProgram
    "staged-callable-parameter" -> stagedCallableParameterProgram
    "staged-callable-data-field" -> stagedCallableDataFieldProgram
    "monomorphic-binding" -> monomorphicBindingProgram
    "generalized-binding" -> generalizedBindingProgram
    "implicit-instantiation" -> implicitInstantiationProgram
    "explicit-instantiation" -> explicitInstantiationProgram
    "explicit-capability-evidence" -> explicitCapabilityEvidenceProgram
    "inferred-capability-evidence" -> inferredCapabilityEvidenceProgram
    "qualified-method-selection" -> qualifiedMethodSelectionProgram
    "partial-method-candidates" -> partialMethodCandidatesProgram
    "patterns-binders" -> patternsBindersProgram
    "or-pattern-alignment" -> orPatternAlignmentProgram
    "callable-shapes-binder-references" -> callableShapesBinderReferencesProgram
    "multi-module-interface" -> multiModuleInterfaceProgram
    "lexical-capture" -> lexicalCaptureProgram
    "curried-applications" -> curriedApplicationsProgram
    _ -> error "unknown valid typed-core fixture"

scalarAliasesWidthsProgram :: TypedProgram
scalarAliasesWidthsProgram =
  programWith
    "scalar-aliases-widths"
    (zipWith expressionStatement [1 ..] scalarExpressions)
    emptyInterface
    textInfo
  where
    scalarExpressions =
      [ literalExpr TypedIntType (TypedSignedIntegerRecipe 64) (TypedIntegerLiteral "1"),
        literalExpr TypedFloatType (TypedFloatRecipe 64) (TypedFractionalLiteral "1" "5" Nothing),
        numericLiteral TypedInt8Type (TypedSignedIntegerRecipe 8),
        numericLiteral TypedInt16Type (TypedSignedIntegerRecipe 16),
        numericLiteral TypedInt32Type (TypedSignedIntegerRecipe 32),
        numericLiteral TypedInt64Type (TypedSignedIntegerRecipe 64),
        numericLiteral TypedUInt8Type (TypedUnsignedIntegerRecipe 8),
        numericLiteral TypedUInt16Type (TypedUnsignedIntegerRecipe 16),
        numericLiteral TypedUInt32Type (TypedUnsignedIntegerRecipe 32),
        numericLiteral TypedUInt64Type (TypedUnsignedIntegerRecipe 64),
        fractionalLiteral TypedFloat16Type 16,
        fractionalLiteral TypedFloat32Type 32,
        fractionalLiteral TypedFloat64Type 64,
        literalExpr TypedBoolType TypedBoolRecipe (TypedBooleanLiteral True),
        literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'j'),
        literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "jazz")
      ]
    numericLiteral numericType recipe =
      literalExpr (TypedNumericType numericType) recipe (TypedIntegerLiteral "7")
    fractionalLiteral numericType width =
      literalExpr
        (TypedNumericType numericType)
        (TypedFloatRecipe width)
        (TypedFractionalLiteral "2" "25" (Just numericType))

lexicalCaptureProgram :: TypedProgram
lexicalCaptureProgram =
  programWith
    fixture
    [ TypedLetStatement seedBinder seedName span1 seedScheme (TypedLiteralExpr intInfo (TypedIntegerLiteral "1")),
      TypedLetStatement functionBinder functionName span1 functionScheme functionExpression,
      TypedExpressionStatement span1 applicationExpression
    ]
    emptyInterface
    intInfo
  where
    fixture = "lexical-capture"
    modulePath = fixtureModulePath fixture
    seedName = fixtureValueName "seed"
    seedBinder = binder modulePath [0] seedName
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    functionName = fixtureValueName "addSeed"
    functionBinder = binder modulePath [1] functionName
    parameterName = fixtureValueName "item"
    parameterBinder = binder modulePath [1, 0] parameterName
    intInfo = info TypedIntType (TypedSignedIntegerRecipe 64)
    callableType = TypedFunctionType TypedIntType TypedIntType
    callableRecipe = TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64)
    callableInfo = info callableType callableRecipe
    functionScheme = TypedScheme functionBinder [] [] [] callableType callableRecipe (Just TypedClosureCallableShape)
    functionExpression =
      TypedLambdaExpr
        callableInfo
        parameterBinder
        parameterName
        ( TypedBinaryExpr
            intInfo
            (TypedBuiltinOperator "+")
            (TypedVariableExpr intInfo parameterName (Just parameterBinder))
            (TypedVariableExpr intInfo seedName (Just seedBinder))
        )
    applicationExpression =
      TypedApplyExpr
        intInfo
        (TypedVariableExpr callableInfo functionName (Just functionBinder))
        (TypedLiteralExpr intInfo (TypedIntegerLiteral "41"))

curriedApplicationsProgram :: TypedProgram
curriedApplicationsProgram =
  programWith
    fixture
    [ TypedLetStatement combineBinder combineName span1 combineScheme combineExpression,
      expressionStatement 2 partialApplication,
      TypedLetStatement identityBinder identityName span1 identityScheme identityExpression,
      TypedLetStatement chooseBinder chooseName span1 chooseScheme chooseExpression,
      expressionStatement 5 callableOversaturation
    ]
    emptyInterface
    intInfo
  where
    fixture = "curried-applications"
    modulePath = fixtureModulePath fixture
    intRecipe = TypedSignedIntegerRecipe 64
    intInfo = info TypedIntType intRecipe
    intToIntType = TypedFunctionType TypedIntType TypedIntType
    intToIntRecipe = TypedClosureRecipe [intRecipe] intRecipe
    intToIntInfo = info intToIntType intToIntRecipe
    combineType = TypedFunctionType TypedIntType intToIntType
    combineRecipe = TypedClosureRecipe [intRecipe] intToIntRecipe
    combineInfo = info combineType combineRecipe
    combineName = fixtureValueName "combine"
    combineBinder = binder modulePath [0] combineName
    leftName = fixtureValueName "left"
    leftBinder = binder modulePath [0, 0] leftName
    rightName = fixtureValueName "right"
    rightBinder = binder modulePath [0, 0, 0] rightName
    combineScheme = TypedScheme combineBinder [] [] [] combineType combineRecipe (Just TypedClosureCallableShape)
    combineExpression =
      TypedLambdaExpr
        combineInfo
        leftBinder
        leftName
        ( TypedLambdaExpr
            intToIntInfo
            rightBinder
            rightName
            (fixtureBoundVariableExpr leftBinder intInfo leftName)
        )
    partialApplication =
      TypedApplyExpr
        intToIntInfo
        (fixtureBoundVariableExpr combineBinder combineInfo combineName)
        (TypedLiteralExpr intInfo (TypedIntegerLiteral "1"))
    identityName = fixtureValueName "identity"
    identityBinder = binder modulePath [2] identityName
    itemName = fixtureValueName "item"
    itemBinder = binder modulePath [2, 0] itemName
    identityScheme = TypedScheme identityBinder [] [] [] intToIntType intToIntRecipe (Just TypedClosureCallableShape)
    identityExpression = TypedLambdaExpr intToIntInfo itemBinder itemName (fixtureBoundVariableExpr itemBinder intInfo itemName)
    chooseType = TypedFunctionType TypedBoolType intToIntType
    chooseRecipe = TypedClosureRecipe [TypedBoolRecipe] intToIntRecipe
    chooseInfo = info chooseType chooseRecipe
    chooseName = fixtureValueName "choose"
    chooseBinder = binder modulePath [3] chooseName
    ignoredName = fixtureValueName "ignored"
    ignoredBinder = binder modulePath [3, 0] ignoredName
    chooseScheme = TypedScheme chooseBinder [] [] [] chooseType chooseRecipe (Just TypedDirectCallableShape)
    chooseExpression =
      TypedLambdaExpr
        chooseInfo
        ignoredBinder
        ignoredName
        (fixtureBoundVariableExpr identityBinder intToIntInfo identityName)
    callableOversaturation =
      TypedApplyExpr
        intInfo
        ( TypedApplyExpr
            intToIntInfo
            (fixtureBoundVariableExpr chooseBinder chooseInfo chooseName)
            falseExpr
        )
        (TypedLiteralExpr intInfo (TypedIntegerLiteral "2"))

resolvedNameOriginsProgram :: TypedProgram
resolvedNameOriginsProgram =
  TypedProgram (Just preludeModule) [libraryModule, entryModule] entryPath
  where
    entryPath = (fixtureModulePath "resolved-name-origins")
    libraryPath = (fixtureLibraryPath "Data")
    localValue = resolved TypedCurrentModule TypedValueNamespace "localValue"
    localBinder = binder entryPath [0] localValue
    localScheme = fixtureScheme localBinder [] [] [] TypedTextType TypedManagedTextRecipe
    importedSome = resolved (TypedImportedModule libraryPath) TypedConstructorNamespace "Some"
    importedLibraryType = resolved (TypedImportedModule libraryPath) TypedTypeNamespace "Option"
    localSome = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    libraryType = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someBinder = binder libraryPath [0, 0] localSome
    libraryData = TypedDataDeclaration span1 libraryType [] [TypedConstructorDeclaration someBinder localSome [TypedTextType] [TypedManagedTextRecipe]]
    preludeList = resolved TypedAmbientPrelude TypedTypeNamespace "List"
    preludeData =
      dataDeclarationWithNullaryConstructor
        ["Prelude"]
        [0, 0]
        preludeList
        [TypedTypeParameterId 0]
    printable = resolved TypedCurrentModule TypedCapabilityNamespace "Printable"
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/Data.jz")
        []
        [TypedModuleExport TypedConstructorNamespace "Some"]
        (TypedModuleInterface [] [TypedDataInterface libraryData] [] [])
        [TypedDataStatement libraryData]
        textInfo
    preludeModule =
      typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedTypeNamespace "List"]
        (TypedModuleInterface [] [TypedDataInterface preludeData] [] [])
        [TypedDataStatement preludeData]
        textInfo
    importedSomeResultType = TypedDataType importedLibraryType []
    importedSomeInfo =
      info
        (TypedFunctionType TypedTextType importedSomeResultType)
        (TypedClosureRecipe [TypedManagedTextRecipe] (TypedManagedVariantRecipe importedLibraryType []))
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Some"])]
        []
        emptyInterface
        ( [ TypedLetStatement
              localBinder
              localValue
              span1
              localScheme
              (TypedLiteralExpr textInfo (TypedTextLiteral "local")),
            TypedClassStatement (TypedClassDeclaration span1 printable [TypedTypeParameterId 0] [])
          ]
            <> [ expressionStatement 1 (fixtureBoundVariableExpr someBinder importedSomeInfo importedSome),
                 expressionStatement 2 (fixtureBoundVariableExpr localBinder textInfo localValue)
               ]
        )
        textInfo

builtinGeneratedNamesProgram :: TypedProgram
builtinGeneratedNamesProgram =
  programWith
    "builtin-generated-names"
    ( expressionStatement 1 (fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map"))
        : zipWith expressionStatement [2 ..] generatedLambdas
    )
    emptyInterface
    functionInfo
  where
    modulePath = (fixtureModulePath "builtin-generated-names")
    generatedNames =
      [ TypedGeneratedName (TypedLambdaPatternArgument 1),
        TypedGeneratedName (TypedOperatorBinding "$operator:%7E"),
        TypedGeneratedName TypedOperatorSectionFunction,
        TypedGeneratedName TypedOperatorSectionLeft,
        TypedGeneratedName TypedOperatorSectionRight
      ]
    functionInfo =
      info
        (TypedFunctionType TypedTextType TypedTextType)
        (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
    generatedLambdas =
      [ TypedLambdaExpr
          functionInfo
          (binder modulePath [index] name)
          name
          (fixtureBoundVariableExpr (binder modulePath [index] name) textInfo name)
      | (index, name) <- zip [0 ..] generatedNames
      ]

listTupleDataRecipesProgram :: TypedProgram
listTupleDataRecipesProgram =
  programWith
    "list-tuple-data-recipes"
    [ TypedDataStatement optionDeclaration,
      expressionStatement 1 (TypedTupleExpr unitInfo []),
      expressionStatement 2 (TypedTupleExpr pairInfo [trueExpr, falseExpr]),
      expressionStatement 3 (TypedListExpr boolListInfo [trueExpr, falseExpr]),
      expressionStatement 4 (fixtureVariableExpr optionConstructorInfo optionConstructor)
    ]
    emptyInterface
    optionConstructorInfo
  where
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    optionConstructor = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    optionParameter = TypedTypeParameterId 0
    optionConstructorOwner =
      binder (fixtureModulePath "list-tuple-data-recipes") [0, 0] optionConstructor
    optionDeclaration =
      TypedDataDeclaration
        span1
        optionName
        [optionParameter]
        [ TypedConstructorDeclaration
            optionConstructorOwner
            optionConstructor
            [TypedTypeParameterType optionParameter]
            [TypedRepresentationParameterRecipe optionParameter]
        ]
    optionConstructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
        [TypedInstantiation optionConstructorOwner [TypedTypeArgument optionParameter TypedBoolType] Nothing]
        []

callableRecipesProgram :: TypedProgram
callableRecipesProgram =
  programWith
    "callable-recipes"
    [expressionStatement 1 lambda]
    emptyInterface
    callableInfo
  where
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder (fixtureModulePath "callable-recipes") [0] argumentName
    innerArgumentName = resolved TypedCurrentModule TypedValueNamespace "innerArgument"
    innerArgumentBinder = binder (fixtureModulePath "callable-recipes") [0, 0] innerArgumentName
    functionType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    callableInfo =
      info
        functionType
        (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
    innerInfo =
      info
        (TypedFunctionType TypedCharType TypedTextType)
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    lambda =
      TypedLambdaExpr
        callableInfo
        argumentBinder
        argumentName
        (TypedLambdaExpr innerInfo innerArgumentBinder innerArgumentName (TypedLiteralExpr textInfo (TypedTextLiteral "ok")))

stagedCallableParameterProgram :: TypedProgram
stagedCallableParameterProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ TypedLetStatement functionBinder functionName span1 functionScheme functionExpression,
      expressionStatement 2 (TypedVariableExpr functionInfo functionName (Just functionBinder))
    ]
    emptyInterface
    functionInfo
    modulePath
  where
    fixture = "staged-callable-parameter"
    modulePath = fixtureModulePath fixture
    functionName = fixtureValueName "apply"
    functionBinder = binder modulePath [0] functionName
    parameterName = fixtureValueName "callable"
    parameterBinder = binder modulePath [0, 0] parameterName
    parameterType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    parameterRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    functionType = TypedFunctionType parameterType TypedBoolType
    functionRecipe = TypedClosureRecipe [parameterRecipe] TypedBoolRecipe
    functionInfo = info functionType functionRecipe
    functionScheme = TypedScheme functionBinder [] [] [] functionType functionRecipe (Just TypedClosureCallableShape)
    functionExpression = TypedLambdaExpr functionInfo parameterBinder parameterName trueExpr

stagedCallableDataFieldProgram :: TypedProgram
stagedCallableDataFieldProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [TypedDataStatement declaration]
    emptyInterface
    boolInfo
    (fixtureModulePath fixture)
  where
    fixture = "staged-callable-data-field"
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Handler"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Handler"
    constructorBinder = fixtureBinder fixture 0 constructorName
    fieldType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    fieldRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [fieldType] [fieldRecipe]]

callableShapesBinderReferencesProgram :: TypedProgram
callableShapesBinderReferencesProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    statements
    emptyInterface
    boolInfo
    modulePath
  where
    fixture = "callable-shapes-binder-references"
    modulePath = fixtureModulePath fixture
    directName = fixtureValueName "direct"
    directOwner = binder modulePath [0] directName
    directOuterName = fixtureValueName "directOuter"
    directOuterBinder = binder modulePath [0, 0] directOuterName
    directInnerName = fixtureValueName "directInner"
    directInnerBinder = binder modulePath [0, 0, 0] directInnerName
    directTerminalName = fixtureValueName "directTerminal"
    directTerminalBinder = binder modulePath [0, 0, 0, 0] directTerminalName
    closureName = fixtureValueName "closure"
    closureOwner = binder modulePath [1] closureName
    closureOuterName = fixtureValueName "closureOuter"
    closureOuterBinder = binder modulePath [1, 0] closureOuterName
    closureInnerName = fixtureValueName "closureInner"
    closureInnerBinder = binder modulePath [1, 0, 0] closureInnerName
    scalarName = fixtureValueName "scalar"
    scalarOwner = binder modulePath [2] scalarName
    directType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType))
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    directInfo = info directType directRecipe
    closureType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)
    closureRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
    closureInfo = info closureType closureRecipe
    directScheme = TypedScheme directOwner [] [] [] directType directRecipe (Just TypedDirectCallableShape)
    closureScheme = TypedScheme closureOwner [] [] [] closureType closureRecipe (Just TypedClosureCallableShape)
    scalarScheme = TypedScheme scalarOwner [] [] [] TypedBoolType TypedBoolRecipe Nothing
    directExpression =
      TypedLambdaExpr
        directInfo
        directOuterBinder
        directOuterName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)) (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe))
            directInnerBinder
            directInnerName
            ( TypedLambdaExpr
                boolToBoolInfo
                directTerminalBinder
                directTerminalName
                (TypedVariableExpr boolInfo directTerminalName (Just directTerminalBinder))
            )
        )
    closureExpression =
      TypedLambdaExpr
        closureInfo
        closureOuterBinder
        closureOuterName
        ( TypedLambdaExpr
            boolToBoolInfo
            closureInnerBinder
            closureInnerName
            (TypedVariableExpr boolInfo closureInnerName (Just closureInnerBinder))
        )
    statements =
      [ TypedLetStatement directOwner directName span1 directScheme directExpression,
        TypedLetStatement closureOwner closureName span1 closureScheme closureExpression,
        TypedLetStatement scalarOwner scalarName span1 scalarScheme trueExpr,
        expressionStatement
          1
          ( TypedApplyExpr
              boolInfo
              ( TypedApplyExpr
                  boolToBoolInfo
                  ( TypedApplyExpr
                      (info (TypedFunctionType TypedBoolType boolToBoolType) (TypedClosureRecipe [TypedBoolRecipe] boolToBoolRecipe))
                      (TypedVariableExpr directInfo directName (Just directOwner))
                      trueExpr
                  )
                  trueExpr
              )
              trueExpr
          )
      ]

monomorphicBindingProgram :: TypedProgram
monomorphicBindingProgram =
  singleModuleProgram
    fixture
    relativeSource
    [TypedModuleExport TypedValueNamespace "enabled"]
    [TypedLetStatement valueBinder valueName span1 scheme trueExpr]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    boolInfo
    (fixtureModulePath fixture)
  where
    fixture = "monomorphic-binding"
    valueName = resolved TypedCurrentModule TypedValueNamespace "enabled"
    valueBinder = binder (fixtureModulePath fixture) [0] valueName
    scheme = fixtureScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

generalizedBindingProgram :: TypedProgram
generalizedBindingProgram =
  singleModuleProgram
    fixture
    relativeSource
    [TypedModuleExport TypedValueNamespace "choose"]
    [TypedLetStatement valueBinder valueName span1 scheme valueExpression]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    boolInfo
    (fixtureModulePath fixture)
  where
    fixture = "generalized-binding"
    valueName = resolved TypedCurrentModule TypedValueNamespace "choose"
    valueBinder = binder (fixtureModulePath fixture) [0] valueName
    parameter0 = TypedTypeParameterId 0
    parameter1 = TypedTypeParameterId 1
    polymorphicType =
      TypedFunctionType
        (TypedTypeParameterType parameter0)
        (TypedFunctionType (TypedTypeParameterType parameter1) (TypedTypeParameterType parameter0))
    polymorphicRecipe =
      TypedClosureRecipe
        [ TypedRepresentationParameterRecipe parameter0,
          TypedRepresentationParameterRecipe parameter1
        ]
        (TypedRepresentationParameterRecipe parameter0)
    firstArgumentName = fixtureValueName "first"
    secondArgumentName = fixtureValueName "second"
    firstArgumentBinder = binder (fixtureModulePath fixture) [0, 0] firstArgumentName
    innerType =
      TypedFunctionType
        (TypedTypeParameterType parameter1)
        (TypedTypeParameterType parameter0)
    innerRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter1]
        (TypedRepresentationParameterRecipe parameter0)
    valueExpression =
      TypedLambdaExpr
        (info polymorphicType polymorphicRecipe)
        firstArgumentBinder
        firstArgumentName
        ( TypedLambdaExpr
            (info innerType innerRecipe)
            (binder (fixtureModulePath fixture) [0, 0, 0] secondArgumentName)
            secondArgumentName
            ( fixtureBoundVariableExpr
                firstArgumentBinder
                (info (TypedTypeParameterType parameter0) (TypedRepresentationParameterRecipe parameter0))
                firstArgumentName
            )
        )
    scheme =
      fixtureScheme
        valueBinder
        [parameter0, parameter1]
        []
        [ TypedNumericPrimitiveConstraint TypedAnyNumericConstraint (TypedTypeParameterType parameter0),
          TypedStrictEqualityPrimitiveConstraint (TypedTypeParameterType parameter1),
          TypedNumericPrimitiveConstraint
            (TypedIntegralLiteralNumericConstraint "0" "255")
            (TypedTypeParameterType parameter0)
        ]
        polymorphicType
        polymorphicRecipe

implicitInstantiationProgram :: TypedProgram
implicitInstantiationProgram = instantiationProgram "implicit-instantiation" Nothing

explicitInstantiationProgram :: TypedProgram
explicitInstantiationProgram = instantiationProgram "explicit-instantiation" (Just (TypedSpan 3 11))

explicitCapabilityEvidenceProgram :: TypedProgram
explicitCapabilityEvidenceProgram = evidenceProgram "explicit-capability-evidence" (Just (TypedEvidenceParameterId 0))

inferredCapabilityEvidenceProgram :: TypedProgram
inferredCapabilityEvidenceProgram = evidenceProgram "inferred-capability-evidence" Nothing

evidenceProgram :: Text -> Maybe TypedEvidenceParameterId -> TypedProgram
evidenceProgram fixture parameterId =
  withFixturePrelude
    ( programWith
        fixture
        [ TypedLetStatement valueBinder valueName span1 scheme trueExpr,
          expressionStatement 1 expression
        ]
        emptyInterface
        (expressionInfoForFixture expression)
    )
  where
    capability =
      TypedCapabilityConstraint
        (preludeCapability "Equal")
        (case parameterId of Nothing -> Just "Equal.equal"; Just _ -> Nothing)
        TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder (fixtureModulePath fixture) [0] valueName
    evidenceUse =
      TypedEvidenceUse
        (TypedEvidenceParameterRef valueBinder <$> parameterId)
        capability
        implId
        (case parameterId of Nothing -> Just (TypedMethodId implId "equal"); Just _ -> Nothing)
    scheme =
      case parameterId of
        Nothing -> monoScheme valueBinder
        Just evidenceId -> fixtureScheme valueBinder [] [TypedEvidenceParameter evidenceId capability] [] TypedBoolType TypedBoolRecipe
    instantiations =
      case parameterId of
        Nothing -> []
        Just _ -> [TypedInstantiation valueBinder [] Nothing]
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe instantiations [TypedSelectedEvidence evidenceUse])
        (case parameterId of Nothing -> TypedBuiltinName "Equal::equal"; Just _ -> valueName)

qualifiedMethodSelectionProgram :: TypedProgram
qualifiedMethodSelectionProgram =
  withFixturePrelude
    ( programWith
        fixture
        [expressionStatement 1 expression]
        emptyInterface
        (expressionInfoForFixture expression)
    )
  where
    fixture = "qualified-method-selection"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName "Equal::equal")

partialMethodCandidatesProgram :: TypedProgram
partialMethodCandidatesProgram =
  withFixturePrelude
    ( programWith
        fixture
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                secondImpl
                [method, fixtureImplMethod (fixtureModulePath fixture) [0, 1] secondImpl "render"]
            ),
          expressionStatement 1 expression
        ]
        emptyInterface
        (expressionInfoForFixture expression)
    )
  where
    fixture = "partial-method-candidates"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    constraint = TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.map") TypedTextType
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId (fixtureModulePath fixture) capabilityName [TypedTextType]
    candidates =
      [ TypedEvidenceCandidate firstImpl (Just (TypedMethodId firstImpl "map")),
        TypedEvidenceCandidate secondImpl (Just (TypedMethodId secondImpl "map"))
      ]
    methodName = resolved TypedCurrentModule TypedValueNamespace "map"
    method =
      TypedMethodDefinition
        (TypedMethodId secondImpl "map")
        (binder (fixtureModulePath fixture) [0] methodName)
        methodName
        span1
        methodExpression
    methodExpression = builtinMapDirectExpression (fixtureModulePath fixture) [0]
    expression =
      fixtureVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [TypedEvidenceCandidates constraint candidates])
        (TypedBuiltinName "map")

patternsBindersProgram :: TypedProgram
patternsBindersProgram =
  programWith fixture statements emptyInterface boolInfo
  where
    fixture = "patterns-binders"
    modulePath = (fixtureModulePath fixture)
    variablePattern index =
      let name = resolved TypedCurrentModule TypedValueNamespace ("item" <> Text.pack (show index))
       in TypedVariablePattern boolInfo (binder modulePath [index] name) name
    asName = resolved TypedCurrentModule TypedValueNamespace "asValue"
    asPattern = TypedAsPattern boolInfo (binder modulePath [6] asName) asName (TypedWildcardPattern boolInfo)
    orPatternName = resolved TypedCurrentModule TypedValueNamespace "value7"
    orPatternBinder lexicalIndex =
      TypedVariablePattern
        boolInfo
        (binder modulePath [lexicalIndex] orPatternName)
        orPatternName
    boolCase patternValue =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm patternValue (Just trueExpr) falseExpr]
    boolListPatternInfo = info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe)
    boolListExpr = TypedListExpr boolListPatternInfo [trueExpr]
    listCase patternValue =
      TypedPatternCaseExpr
        boolInfo
        boolListExpr
        [TypedCaseArm patternValue Nothing trueExpr]
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    optionParameter = TypedTypeParameterId 0
    optionConstructorOwner =
      binder (fixtureModulePath fixture) [0, 0] someName
    optionDeclaration =
      TypedDataDeclaration
        span1
        optionName
        [optionParameter]
        [ TypedConstructorDeclaration
            optionConstructorOwner
            someName
            [TypedTypeParameterType optionParameter]
            [TypedRepresentationParameterRecipe optionParameter]
        ]
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
        [TypedInstantiation optionConstructorOwner [TypedTypeArgument optionParameter TypedBoolType] Nothing]
        []
    optionScrutinee = TypedApplyExpr optionInfo (fixtureVariableExpr constructorInfo someName) trueExpr
    statements =
      TypedDataStatement optionDeclaration
        : zipWith
          expressionStatement
          [1 ..]
          [ boolCase (TypedWildcardPattern boolInfo),
            boolCase (variablePattern 1),
            boolCase (TypedLiteralPattern boolInfo (TypedBooleanLiteral True)),
            TypedPatternCaseExpr
              boolInfo
              optionScrutinee
              [TypedCaseArm (TypedConstructorPattern optionInfo someName [variablePattern 2]) Nothing trueExpr],
            listCase (TypedListPattern boolListPatternInfo [variablePattern 3]),
            listCase (TypedConsListPattern boolListPatternInfo (variablePattern 4) (TypedListPattern boolListPatternInfo [])),
            TypedPatternCaseExpr
              boolInfo
              (TypedTupleExpr pairInfo [trueExpr, falseExpr])
              [TypedCaseArm (TypedTuplePattern pairInfo [variablePattern 5, TypedWildcardPattern boolInfo]) Nothing trueExpr],
            boolCase asPattern,
            boolCase (TypedOrPattern boolInfo [orPatternBinder 7, orPatternBinder 8])
          ]

orPatternAlignmentProgram :: TypedProgram
orPatternAlignmentProgram =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    fixture = "or-pattern-alignment"
    valueName = resolved TypedCurrentModule TypedValueNamespace "matched"
    firstBinder = binder (fixtureModulePath fixture) [0] valueName
    secondBinder = binder (fixtureModulePath fixture) [1] valueName
    firstAlternative = TypedVariablePattern boolInfo firstBinder valueName
    secondAlternative = TypedVariablePattern boolInfo secondBinder valueName
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedOrPattern boolInfo [firstAlternative, secondAlternative]) Nothing trueExpr]

multiModuleInterfaceProgram :: TypedProgram
multiModuleInterfaceProgram =
  TypedProgram
    (Just preludeModule)
    [libraryModule, entryModule]
    ["App", "Main"]
  where
    preludeName = resolved TypedAmbientPrelude TypedValueNamespace "truth"
    preludeBinder = binder ["Prelude"] [0] preludeName
    preludeScheme = fixtureScheme preludeBinder [] [] [] TypedBoolType TypedBoolRecipe
    preludeModule =
      typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedValueNamespace "truth"]
        (TypedModuleInterface [TypedValueInterface preludeName preludeScheme] [] [] [])
        [TypedLetStatement preludeBinder preludeName span1 preludeScheme trueExpr]
        boolInfo
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorBinder = binder (fixtureLibraryPath "Flag") [0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedBoolRecipe]]
    libraryModule =
      typedModule
        (fixtureLibraryPath "Flag")
        (TypedSourcePath "src/Library/Flag.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Flag", TypedModuleExport TypedConstructorNamespace "Flag"]
        (TypedModuleInterface [] [TypedDataInterface declaration] [] [])
        [TypedDataStatement declaration]
        (info (TypedDataType dataName []) (TypedManagedVariantRecipe dataName []))
    entryModule =
      typedModule
        ["App", "Main"]
        (TypedSourcePath "src/App/Main.jz")
        [ TypedResolvedImport span1 ["Prelude"] Nothing (Just ["truth"]),
          TypedResolvedImport span1 (fixtureLibraryPath "Flag") (Just "FlagModule") Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

aliasedCapabilityLocalClassProgram :: TypedProgram
aliasedCapabilityLocalClassProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = fixtureLibraryPath "AliasedCapability"
    entryPath = fixtureModulePath "review-aliased-capability-local-class"
    parameter = TypedTypeParameterId 0
    libraryClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    libraryDeclaration =
      TypedClassDeclaration span1 libraryClassName [parameter] []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/AliasedCapability.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        (TypedModuleInterface [] [] [TypedClassInterface libraryDeclaration] [])
        [TypedClassStatement libraryDeclaration]
        unitInfo
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    localDeclaration =
      TypedClassDeclaration span1 localClassName [parameter] []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath (Just "Library") Nothing]
        []
        emptyInterface
        [TypedClassStatement localDeclaration, expressionStatement 2 trueExpr]
        boolInfo

recursivePhantomDataEqualityProgram :: TypedProgram
recursivePhantomDataEqualityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-recursive-phantom-data-equality"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Phantom"
    endName = resolved TypedCurrentModule TypedConstructorNamespace "End"
    moreName = resolved TypedCurrentModule TypedConstructorNamespace "More"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    recursiveField = TypedDataType dataName [parameterType]
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] endName)
            endName
            []
            [],
          TypedConstructorDeclaration
            (binder modulePath [0, 1] moreName)
            moreName
            [recursiveField]
            [TypedManagedVariantRecipe dataName [parameterType]]
        ]
    valueName = fixtureValueName "phantomEquality"
    valueOwner = binder modulePath [1] valueName
    targetType = TypedDataType dataName [boolToBoolType]
    scheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint targetType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

publishedImplDataName :: TypedCoreName
publishedImplDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Hidden"

publishedImplDataMetadataProgram :: TypedProgram
publishedImplDataMetadataProgram =
  singleModuleProgram fixture relativeSource exports statements interface boolInfo modulePath
  where
    fixture = "review-published-impl-data-metadata"
    modulePath = fixtureModulePath fixture
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Hidden"
    dataDeclaration =
      TypedDataDeclaration
        span1
        publishedImplDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Default"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    implId =
      TypedImplId
        modulePath
        capabilityName
        [TypedDataType publishedImplDataName []]
    exports = [TypedModuleExport TypedCapabilityNamespace "Default"]
    statements =
      [ TypedDataStatement dataDeclaration,
        TypedClassStatement capabilityDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 4 trueExpr
      ]
    interface =
      TypedModuleInterface
        []
        []
        [TypedClassInterface capabilityDeclaration]
        [TypedImplInterface implId]

denseImportDagProgram :: Int -> TypedProgram
denseImportDagProgram moduleCount =
  TypedProgram Nothing modules entryPath
  where
    modulePath moduleIndex = ["Dense", "M" <> Text.pack (show moduleIndex)]
    entryPath = modulePath (moduleCount - 1)
    modules = [denseModule moduleIndex | moduleIndex <- [0 .. moduleCount - 1]]
    denseModule moduleIndex =
      typedModule
        (modulePath moduleIndex)
        (TypedSourcePath ("src/Dense/M" <> Text.pack (show moduleIndex) <> ".jz"))
        [ TypedResolvedImport span1 (modulePath importedIndex) Nothing Nothing
        | importedIndex <- [0 .. moduleIndex - 1]
        ]
        []
        emptyInterface
        []
        unitInfo

denseBindingDagProgram :: Int -> TypedProgram
denseBindingDagProgram bindingCount =
  singleModuleProgram
    fixture
    relativeSource
    []
    (bindings <> [expressionStatement bindingCount terminalExpression])
    emptyInterface
    boolInfo
    modulePath
  where
    fixture = "review-dense-binding-dag"
    modulePath = fixtureModulePath fixture
    names =
      [ resolved
          TypedCurrentModule
          TypedValueNamespace
          ("item" <> Text.pack (show index))
      | index <- [0 .. bindingCount - 1]
      ]
    owners =
      [ binder modulePath [index] name
      | (index, name) <- zip [0 ..] names
      ]
    bindings =
      [ TypedLetStatement
          owner
          name
          span1
          (monoScheme owner)
          (denseExpression (take index (zip owners names)))
      | (index, name) <- zip [0 ..] names,
        let owner = binder modulePath [index] name
      ]
    denseExpression =
      foldr
        ( \(owner, name) rest ->
            TypedIfExpr
              boolInfo
              (fixtureBoundVariableExpr owner boolInfo name)
              rest
              falseExpr
        )
        trueExpr
    terminalExpression =
      case reverse (zip owners names) of
        (owner, name) : _ -> fixtureBoundVariableExpr owner boolInfo name
        [] -> trueExpr

invalidResolvedOperatorSymbolsProgram :: TypedProgram
invalidResolvedOperatorSymbolsProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath
  where
    fixture = "review-invalid-resolved-operator-symbols"
    modulePath = fixtureModulePath fixture
    aliasedName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    encodedName = TypedGeneratedName (TypedOperatorBinding "$operator:%61")
    builtinName = TypedGeneratedName (TypedOperatorBinding "$operator:%2B")
    reservedName = TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E")
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    operatorScheme lexicalIndex name =
      fixtureScheme
        (binder modulePath [lexicalIndex] name)
        []
        []
        []
        operatorType
        operatorRecipe
    operatorDefinition lexicalIndex name =
      TypedLetStatement
        (binder modulePath [lexicalIndex] name)
        name
        span1
        (operatorScheme lexicalIndex name)
        (boolBinaryFunctionExpression modulePath [lexicalIndex])
    operatorUse name symbol =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator name symbol)
    statements =
      [ operatorDefinition 0 aliasedName,
        operatorDefinition 1 encodedName,
        operatorDefinition 2 builtinName,
        operatorDefinition 3 reservedName,
        expressionStatement 4 (operatorUse aliasedName "a"),
        expressionStatement 5 (operatorUse encodedName "a"),
        expressionStatement 6 (operatorUse builtinName "+"),
        expressionStatement 7 (operatorUse reservedName "->")
      ]

ambiguousQualifiedMethodSelectionProgram :: TypedProgram
ambiguousQualifiedMethodSelectionProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-ambiguous-qualified-method-selection"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    selectedEvidence targetType =
      let implId = TypedImplId ["Prelude"] capabilityName [targetType]
       in TypedSelectedEvidence
            ( TypedEvidenceUse
                Nothing
                (TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") targetType)
                implId
                (Just (TypedMethodId implId "equal"))
            )
    expression =
      fixtureVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            []
            [selectedEvidence TypedBoolType, selectedEvidence TypedCharType]
        )
        (TypedBuiltinName "Equal::equal")

repeatedEqualityDagProgram :: Int -> TypedProgram
repeatedEqualityDagProgram depth =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-repeated-equality-dag"
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    dataName :: Int -> TypedCoreName
    dataName index =
      resolved
        TypedCurrentModule
        TypedTypeNamespace
        ("D" <> Text.pack (show index))
    constructorName :: Int -> TypedCoreName
    constructorName index =
      resolved
        TypedCurrentModule
        TypedConstructorNamespace
        ("D" <> Text.pack (show index))
    dataType :: Int -> TypedType
    dataType index = TypedDataType (dataName index) [parameterType]
    declaration :: Int -> TypedDataDeclaration
    declaration index =
      TypedDataDeclaration
        span1
        (dataName index)
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [index, 0] (constructorName index))
            (constructorName index)
            fields
            recipes
        ]
      where
        (fields, recipes)
          | index == depth =
              ([parameterType], [TypedRepresentationParameterRecipe parameter])
          | otherwise =
              ( [dataType (index + 1), dataType (index + 1)],
                [ TypedManagedVariantRecipe (dataName (index + 1)) [parameterType],
                  TypedManagedVariantRecipe (dataName (index + 1)) [parameterType]
                ]
              )
    valueName = fixtureValueName "equal"
    valueOwner = binder modulePath [depth + 1] valueName
    targetType = TypedDataType (dataName 0) [TypedBoolType]
    valueScheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint targetType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      map (TypedDataStatement . declaration) [0 .. depth]
        <> [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]

recursivePhantomEqualityDagProgram :: Int -> TypedProgram
recursivePhantomEqualityDagProgram depth =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-recursive-phantom-equality-dag"
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    dataName :: Int -> TypedCoreName
    dataName index =
      resolved
        TypedCurrentModule
        TypedTypeNamespace
        ("P" <> Text.pack (show index))
    endName :: Int -> TypedCoreName
    endName index =
      resolved
        TypedCurrentModule
        TypedConstructorNamespace
        ("End" <> Text.pack (show index))
    stepName :: Int -> TypedCoreName
    stepName index =
      resolved
        TypedCurrentModule
        TypedConstructorNamespace
        ("Step" <> Text.pack (show index))
    nextIndex :: Int -> Int
    nextIndex index
      | index == depth = 0
      | otherwise = index + 1
    nextArgument :: Int -> TypedType
    nextArgument index
      | index == depth = TypedListType parameterType
      | otherwise = parameterType
    nextType :: Int -> TypedType
    nextType index =
      TypedDataType
        (dataName (nextIndex index))
        [nextArgument index]
    declaration :: Int -> TypedDataDeclaration
    declaration index =
      TypedDataDeclaration
        span1
        (dataName index)
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [index, 0] (endName index))
            (endName index)
            []
            [],
          TypedConstructorDeclaration
            (binder modulePath [index, 1] (stepName index))
            (stepName index)
            [nextType index, nextType index]
            [ TypedManagedVariantRecipe (dataName (nextIndex index)) [nextArgument index],
              TypedManagedVariantRecipe (dataName (nextIndex index)) [nextArgument index]
            ]
        ]
    valueName = fixtureValueName "equal"
    valueOwner = binder modulePath [depth + 1] valueName
    targetType = TypedDataType (dataName 0) [TypedBoolType]
    valueScheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint targetType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      map (TypedDataStatement . declaration) [0 .. depth]
        <> [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]

data InvalidFixture = InvalidFixture
  { invalidFixtureName :: Text,
    invalidFixtureProgram :: TypedProgram,
    invalidFixtureFailures :: [TypedCoreValidationFailure]
  }

expectedInvalidFixtureNames :: [Text]
expectedInvalidFixtureNames =
  [ "unresolved-source-name",
    "unresolved-qualified-name",
    "absolute-source-path",
    "duplicate-module-path",
    "unknown-entry-module",
    "duplicate-binder",
    "unknown-binder",
    "duplicate-or-noncanonical-type-parameter",
    "free-type-parameter",
    "free-representation-parameter",
    "invalid-integer-width",
    "type-representation-mismatch",
    "data-recipe-declaration",
    "callable-recipe-signature",
    "callable-zero-argument-stage",
    "flattened-callable-parameter-scheme",
    "flattened-callable-parameter-lambda",
    "flattened-anonymous-lambda-recipe",
    "flattened-nested-lambda-recipe",
    "callable-missing-shape",
    "combined-callable-failure-order",
    "scalar-carrying-shape",
    "missing-binder-reference",
    "unknown-binder-reference",
    "binder-reference-contract-mismatch",
    "application-function-shape",
    "application-argument-type",
    "collection-child-recipe-staging",
    "constructor-pattern-field-recipe-staging",
    "direct-callable-value-use",
    "flattened-callable-data-field",
    "flattened-operator-section-recipe",
    "resolved-operator-section-operand-recipe",
    "direct-binding-without-leading-lambda",
    "binary-operator-result-recipe-staging",
    "underapplied-direct-binary-operator",
    "underapplied-direct-operator-sections",
    "builtin-application-operator-result-recipe-staging",
    "pattern-arm-recipe-join",
    "if-branch-recipe-join",
    "direct-lambda-tail-recipe-progression",
    "application-argument-recipe-staging",
    "application-result-type",
    "application-result-recipe-staging",
    "oversaturation-after-non-callable-result",
    "if-condition-type",
    "if-branch-type",
    "pattern-scrutinee-type",
    "pattern-guard-type",
    "pattern-arm-result-type",
    "or-pattern-binder-contract",
    "duplicate-or-noncanonical-evidence-parameter",
    "instantiation-contract",
    "missing-or-duplicate-evidence",
    "ambiguous-or-invisible-evidence",
    "method-or-interface-identity"
  ]

invalidFixtures :: [InvalidFixture]
invalidFixtures =
  [ unresolvedSourceNameFixture,
    unresolvedQualifiedNameFixture,
    absoluteSourcePathFixture,
    duplicateModulePathFixture,
    unknownEntryModuleFixture,
    duplicateBinderFixture,
    unknownBinderFixture,
    duplicateTypeParameterFixture,
    freeTypeParameterFixture,
    freeRepresentationParameterFixture,
    invalidIntegerWidthFixture,
    typeRepresentationMismatchFixture,
    dataRecipeDeclarationFixture,
    callableRecipeSignatureFixture,
    callableZeroArgumentStageFixture,
    flattenedCallableParameterSchemeFixture,
    flattenedCallableParameterLambdaFixture,
    flattenedAnonymousLambdaRecipeFixture,
    flattenedNestedLambdaRecipeFixture,
    callableMissingShapeFixture,
    combinedCallableFailureOrderFixture,
    scalarCarryingShapeFixture,
    missingBinderReferenceFixture,
    unknownBinderReferenceFixture,
    binderReferenceContractMismatchFixture,
    applicationFunctionShapeFixture,
    applicationArgumentTypeFixture,
    collectionChildRecipeStagingFixture,
    constructorPatternFieldRecipeStagingFixture,
    directCallableValueUseFixture,
    flattenedCallableDataFieldFixture,
    flattenedOperatorSectionRecipeFixture,
    resolvedOperatorSectionOperandRecipeFixture,
    directBindingWithoutLeadingLambdaFixture,
    binaryOperatorResultRecipeStagingFixture,
    underappliedDirectBinaryOperatorFixture,
    underappliedDirectOperatorSectionsFixture,
    builtinApplicationOperatorResultRecipeStagingFixture,
    patternArmRecipeJoinFixture,
    ifBranchRecipeJoinFixture,
    directLambdaTailRecipeProgressionFixture,
    applicationArgumentRecipeStagingFixture,
    applicationResultTypeFixture,
    applicationResultRecipeStagingFixture,
    oversaturationAfterNonCallableResultFixture,
    ifConditionTypeFixture,
    ifBranchTypeFixture,
    patternScrutineeTypeFixture,
    patternGuardTypeFixture,
    patternArmResultTypeFixture,
    orPatternBinderContractFixture,
    duplicateEvidenceParameterFixture,
    instantiationContractFixture,
    missingOrDuplicateEvidenceFixture,
    ambiguousOrInvisibleEvidenceFixture,
    methodOrInterfaceIdentityFixture
  ]

unresolvedSourceNameFixture :: InvalidFixture
unresolvedSourceNameFixture =
  expressionFixture
    "unresolved-source-name"
    (fixtureVariableExpr boolInfo unresolvedName)
    [expressionFailure "unresolved-source-name" TypedUnresolvedName (TypedNameDetail unresolvedName)]
  where
    unresolvedName = TypedUnresolvedSourceName "missing"

unresolvedQualifiedNameFixture :: InvalidFixture
unresolvedQualifiedNameFixture =
  expressionFixture
    "unresolved-qualified-name"
    (fixtureVariableExpr boolInfo unresolvedName)
    [expressionFailure "unresolved-qualified-name" TypedUnresolvedName (TypedNameDetail unresolvedName)]
  where
    unresolvedName = TypedUnresolvedQualifiedName "Missing" "item"

absoluteSourcePathFixture :: InvalidFixture
absoluteSourcePathFixture =
  InvalidFixture
    fixture
    (singleModuleProgram fixture (TypedSourcePath "/absolute/Main.jz") [] [] emptyInterface boolInfo (fixtureModulePath fixture))
    [moduleFailure fixture TypedInvalidSourcePath (TypedTextDetail "/absolute/Main.jz")]
  where
    fixture = "absolute-source-path"

duplicateModulePathFixture :: InvalidFixture
duplicateModulePathFixture =
  InvalidFixture
    fixture
    (TypedProgram Nothing [moduleValue, moduleValue] modulePath)
    [TypedCoreValidationFailure (TypedModulePath modulePath) TypedDuplicateModule (TypedTextDetail "Fixture::duplicate_module_path")]
  where
    fixture = "duplicate-module-path"
    modulePath = (fixtureModulePath fixture)
    moduleValue = typedModule modulePath (TypedSourcePath "src/Fixture/duplicate-module-path.jz") [] [] emptyInterface [] boolInfo

unknownEntryModuleFixture :: InvalidFixture
unknownEntryModuleFixture =
  InvalidFixture
    fixture
    (singleModuleProgram fixture relativeSource [] [] emptyInterface boolInfo ["Missing", "Entry"])
    [TypedCoreValidationFailure TypedProgramPath TypedUnknownEntryModule (TypedTextDetail "Missing::Entry")]
  where
    fixture = "unknown-entry-module"

duplicateBinderFixture :: InvalidFixture
duplicateBinderFixture =
  InvalidFixture fixture program [statementFailure fixture 1 TypedDuplicateBinder (TypedBinderDetail valueBinder)]
  where
    fixture = "duplicate-binder"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = monoScheme valueBinder
    statement = TypedLetStatement valueBinder valueName span1 scheme trueExpr
    program = singleModuleProgram fixture relativeSource [] [statement, statement] emptyInterface boolInfo (fixtureModulePath fixture)

unknownBinderFixture :: InvalidFixture
unknownBinderFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedUnknownBinder (TypedBinderDetail schemeBinder)]
  where
    fixture = "unknown-binder"
    valueName = fixtureValueName "item"
    statementBinder = fixtureBinder fixture 0 valueName
    schemeBinder = fixtureBinder fixture 1 valueName
    statement = TypedLetStatement statementBinder valueName span1 (monoScheme schemeBinder) trueExpr
    program = singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo (fixtureModulePath fixture)

duplicateTypeParameterFixture :: InvalidFixture
duplicateTypeParameterFixture =
  InvalidFixture fixture program failures
  where
    fixture = "duplicate-or-noncanonical-type-parameter"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    parameters = [TypedTypeParameterId 0, TypedTypeParameterId 0, TypedTypeParameterId 3]
    scheme = fixtureScheme valueBinder parameters [] [] TypedBoolType TypedBoolRecipe
    program = signatureProgram fixture valueBinder valueName scheme
    failures =
      [ statementFailure fixture 0 TypedDuplicateTypeParameter (TypedTypeParameterDetail (TypedTypeParameterId 0)),
        statementFailure fixture 0 TypedInvalidTypeParameterOrder (TypedIndexDetail 1),
        statementFailure fixture 0 TypedInvalidTypeParameterOrder (TypedIndexDetail 2)
      ]

freeTypeParameterFixture :: InvalidFixture
freeTypeParameterFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedUnboundTypeParameter (TypedTypeParameterDetail parameterId)]
  where
    fixture = "free-type-parameter"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme valueBinder [] [] [] (TypedTypeParameterType parameterId) TypedBoolRecipe
    program = signatureProgram fixture valueBinder valueName scheme

freeRepresentationParameterFixture :: InvalidFixture
freeRepresentationParameterFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedUnboundRepresentationParameter (TypedTypeParameterDetail parameterId)]
  where
    fixture = "free-representation-parameter"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme valueBinder [] [] [] TypedBoolType (TypedRepresentationParameterRecipe parameterId)
    program = signatureProgram fixture valueBinder valueName scheme

invalidIntegerWidthFixture :: InvalidFixture
invalidIntegerWidthFixture =
  expressionFixture
    fixture
    (literalExpr TypedIntType recipe (TypedIntegerLiteral "1"))
    [expressionFailure fixture TypedInvalidRepresentationWidth (TypedIndexDetail 7)]
  where
    fixture = "invalid-integer-width"
    recipe = TypedSignedIntegerRecipe 7

typeRepresentationMismatchFixture :: InvalidFixture
typeRepresentationMismatchFixture =
  expressionFixture
    fixture
    (literalExpr TypedBoolType (TypedSignedIntegerRecipe 64) (TypedBooleanLiteral True))
    [expressionFailure fixture TypedTypeRepresentationMismatch (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64))]
  where
    fixture = "type-representation-mismatch"

dataRecipeDeclarationFixture :: InvalidFixture
dataRecipeDeclarationFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedDataRecipeMismatch (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64))]
  where
    fixture = "data-recipe-declaration"
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorBinder = fixtureBinder fixture 0 constructorName
    declaration =
      TypedDataDeclaration span1 dataName [] [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedSignedIntegerRecipe 64]]
    program = singleModuleProgram fixture relativeSource [] [TypedDataStatement declaration] emptyInterface boolInfo (fixtureModulePath fixture)

flattenedCallableDataFieldFixture :: InvalidFixture
flattenedCallableDataFieldFixture =
  InvalidFixture
    fixture
    program
    [statementFailure fixture 0 TypedDataRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "flattened-callable-data-field"
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Handler"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Handler"
    constructorBinder = fixtureBinder fixture 0 constructorName
    fieldType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    expectedRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    actualRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [fieldType] [actualRecipe]]
    program = singleModuleProgram fixture relativeSource [] [TypedDataStatement declaration] emptyInterface boolInfo (fixtureModulePath fixture)

callableRecipeSignatureFixture :: InvalidFixture
callableRecipeSignatureFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedCallableRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "callable-recipe-signature"
    valueName = fixtureValueName "callable"
    valueBinder = fixtureBinder fixture 0 valueName
    expectedRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    actualRecipe = TypedClosureRecipe [TypedCharRecipe] TypedBoolRecipe
    scheme = fixtureScheme valueBinder [] [] [] (TypedFunctionType TypedBoolType TypedBoolType) actualRecipe
    program = signatureProgram fixture valueBinder valueName scheme

callableZeroArgumentStageFixture :: InvalidFixture
callableZeroArgumentStageFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedCallableRecipeMismatch (TypedRecipeDetail boolToBoolRecipe actualRecipe)]
  where
    fixture = "callable-zero-argument-stage"
    valueName = fixtureValueName "callable"
    valueBinder = fixtureBinder fixture 0 valueName
    actualRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [] TypedBoolRecipe)
    scheme = TypedScheme valueBinder [] [] [] boolToBoolType actualRecipe (Just TypedDirectCallableShape)
    program = signatureProgram fixture valueBinder valueName scheme

flattenedCallableParameterSchemeFixture :: InvalidFixture
flattenedCallableParameterSchemeFixture =
  InvalidFixture
    fixture
    program
    [statementFailure fixture 0 TypedCallableRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "flattened-callable-parameter-scheme"
    valueName = fixtureValueName "apply"
    valueBinder = fixtureBinder fixture 0 valueName
    parameterType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    parameterRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    flattenedParameterRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    functionType = TypedFunctionType parameterType TypedBoolType
    expectedRecipe = TypedClosureRecipe [parameterRecipe] TypedBoolRecipe
    actualRecipe = TypedClosureRecipe [flattenedParameterRecipe] TypedBoolRecipe
    scheme = TypedScheme valueBinder [] [] [] functionType actualRecipe (Just TypedClosureCallableShape)
    program = signatureProgram fixture valueBinder valueName scheme

flattenedCallableParameterLambdaFixture :: InvalidFixture
flattenedCallableParameterLambdaFixture =
  expressionFixture
    fixture
    expression
    [expressionFailure fixture TypedCallableRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "flattened-callable-parameter-lambda"
    modulePath = fixtureModulePath fixture
    parameterName = fixtureValueName "callable"
    parameterBinder = binder modulePath [0] parameterName
    parameterType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    parameterRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    flattenedParameterRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    functionType = TypedFunctionType parameterType TypedBoolType
    expectedRecipe = TypedClosureRecipe [parameterRecipe] TypedBoolRecipe
    actualRecipe = TypedClosureRecipe [flattenedParameterRecipe] TypedBoolRecipe
    expression = TypedLambdaExpr (info functionType actualRecipe) parameterBinder parameterName trueExpr

flattenedAnonymousLambdaRecipeFixture :: InvalidFixture
flattenedAnonymousLambdaRecipeFixture =
  expressionFixture
    fixture
    expression
    [expressionFailure fixture TypedCallableRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "flattened-anonymous-lambda-recipe"
    modulePath = fixtureModulePath fixture
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0] innerName
    functionType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    expectedRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    actualRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    expression =
      TypedLambdaExpr
        (info functionType actualRecipe)
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            innerBinder
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "ok"))
        )

flattenedNestedLambdaRecipeFixture :: InvalidFixture
flattenedNestedLambdaRecipeFixture =
  InvalidFixture fixture (expressionFixtureProgram fixture expression) failures
  where
    fixture = "flattened-nested-lambda-recipe"
    modulePath = fixtureModulePath fixture
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0] innerName
    terminalName = fixtureValueName "terminal"
    terminalBinder = binder modulePath [0, 0, 0] terminalName
    innerType = TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType)
    expectedInnerRecipe = TypedClosureRecipe [TypedCharRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    actualInnerRecipe = TypedClosureRecipe [TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    outerType = TypedFunctionType TypedBoolType innerType
    outerRecipe = TypedClosureRecipe [TypedBoolRecipe] expectedInnerRecipe
    expression =
      TypedLambdaExpr
        (info outerType outerRecipe)
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info innerType actualInnerRecipe)
            innerBinder
            innerName
            ( TypedLambdaExpr
                (info (TypedFunctionType TypedBoolType TypedTextType) (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe))
                terminalBinder
                terminalName
                (TypedLiteralExpr textInfo (TypedTextLiteral "ok"))
            )
        )
    failures =
      [ TypedCoreValidationFailure
          (TypedExpressionPath modulePath [0] [0, 0])
          TypedCallableRecipeMismatch
          (TypedRecipeDetail expectedInnerRecipe actualInnerRecipe)
      ]

callableMissingShapeFixture :: InvalidFixture
callableMissingShapeFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedCallableShapeMismatch (TypedBinderDetail valueBinder)]
  where
    fixture = "callable-missing-shape"
    valueName = fixtureValueName "callable"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = TypedScheme valueBinder [] [] [] boolToBoolType boolToBoolRecipe Nothing
    program = signatureProgram fixture valueBinder valueName scheme

combinedCallableFailureOrderFixture :: InvalidFixture
combinedCallableFailureOrderFixture =
  InvalidFixture fixture program failures
  where
    fixture = "combined-callable-failure-order"
    modulePath = fixtureModulePath fixture
    functionName = fixtureValueName "callable"
    functionBinder = fixtureBinder fixture 0 functionName
    argumentName = fixtureValueName "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    functionScheme =
      TypedScheme
        functionBinder
        []
        []
        []
        boolToBoolType
        boolToBoolRecipe
        Nothing
    functionExpression =
      TypedLambdaExpr
        boolToBoolInfo
        argumentBinder
        argumentName
        (TypedVariableExpr boolInfo argumentName Nothing)
    laterExpression =
      TypedLiteralExpr boolInfo (TypedCharacterLiteral 'x')
    statements =
      [ TypedLetStatement
          functionBinder
          functionName
          span1
          functionScheme
          functionExpression,
        expressionStatement 2 laterExpression
      ]
    program =
      singleModuleProgram
        fixture
        relativeSource
        []
        statements
        emptyInterface
        boolInfo
        modulePath
    failures =
      [ statementFailure fixture 0 TypedCallableShapeMismatch (TypedBinderDetail functionBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [0] [0, 0])
          TypedBinderReferenceMismatch
          (TypedBinderDetail argumentBinder),
        expressionFailureAt
          fixture
          1
          TypedLiteralTypeMismatch
          (TypedTypeDetail TypedCharType TypedBoolType)
      ]

scalarCarryingShapeFixture :: InvalidFixture
scalarCarryingShapeFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedCallableShapeMismatch (TypedBinderDetail valueBinder)]
  where
    fixture = "scalar-carrying-shape"
    valueName = fixtureValueName "scalar"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe (Just TypedDirectCallableShape)
    program = signatureProgram fixture valueBinder valueName scheme

missingBinderReferenceFixture :: InvalidFixture
missingBinderReferenceFixture =
  InvalidFixture fixture program [expressionFailureAt fixture 1 TypedBinderReferenceMismatch (TypedBinderDetail valueBinder)]
  where
    fixture = "missing-binder-reference"
    valueName = fixtureValueName "local"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe Nothing
    statements =
      [ TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 (TypedVariableExpr boolInfo valueName Nothing)
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo (fixtureModulePath fixture)

unknownBinderReferenceFixture :: InvalidFixture
unknownBinderReferenceFixture =
  InvalidFixture fixture program [expressionFailureAt fixture 1 TypedBinderReferenceMismatch (TypedBinderDetail unknownBinder)]
  where
    fixture = "unknown-binder-reference"
    valueName = fixtureValueName "local"
    valueBinder = fixtureBinder fixture 0 valueName
    unknownBinder = fixtureBinder fixture 9 valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe Nothing
    statements =
      [ TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 (TypedVariableExpr boolInfo valueName (Just unknownBinder))
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo (fixtureModulePath fixture)

binderReferenceContractMismatchFixture :: InvalidFixture
binderReferenceContractMismatchFixture =
  InvalidFixture fixture program [expressionFailureAt fixture 1 TypedBinderReferenceMismatch (TypedBinderDetail valueBinder)]
  where
    fixture = "binder-reference-contract-mismatch"
    valueName = fixtureValueName "local"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe Nothing
    mismatchedInfo = info TypedCharType TypedCharRecipe
    statements =
      [ TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 (TypedVariableExpr mismatchedInfo valueName (Just valueBinder))
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface mismatchedInfo (fixtureModulePath fixture)

applicationFunctionShapeFixture :: InvalidFixture
applicationFunctionShapeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationFunctionMismatch (TypedTypeDetail (TypedFunctionType TypedBoolType TypedBoolType) TypedBoolType)]
  where
    fixture = "application-function-shape"
    expression = TypedApplyExpr boolInfo trueExpr falseExpr

applicationArgumentTypeFixture :: InvalidFixture
applicationArgumentTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
  where
    fixture = "application-argument-type"
    functionName = resolved TypedCurrentModule TypedValueNamespace "argument"
    functionExpr = TypedLambdaExpr boolToBoolInfo (binder (fixtureModulePath fixture) [0, 0] functionName) functionName trueExpr
    argumentExpr = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedApplyExpr boolInfo functionExpr argumentExpr

collectionChildRecipeStagingFixture :: InvalidFixture
collectionChildRecipeStagingFixture =
  InvalidFixture fixture program failures
  where
    fixture = "collection-child-recipe-staging"
    modulePath = fixtureModulePath fixture
    directName = fixtureValueName "direct"
    directBinder = binder modulePath [0] directName
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0] innerName
    callableType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    stagedRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    directInfo = info callableType directRecipe
    directScheme = TypedScheme directBinder [] [] [] callableType directRecipe (Just TypedDirectCallableShape)
    directExpression =
      TypedLambdaExpr
        directInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            innerBinder
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "ok"))
        )
    directReference = TypedVariableExpr directInfo directName (Just directBinder)
    listInfo = info (TypedListType callableType) (TypedManagedListRecipe stagedRecipe)
    tupleInfo = info (TypedTupleType [callableType, callableType]) (TypedManagedProductRecipe [stagedRecipe, stagedRecipe])
    statements =
      [ TypedLetStatement directBinder directName span1 directScheme directExpression,
        expressionStatement 2 (TypedListExpr listInfo [directReference]),
        expressionStatement 3 (TypedTupleExpr tupleInfo [directReference, directReference]),
        expressionStatement 4 trueExpr
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
    failures =
      [ TypedCoreValidationFailure
          (TypedExpressionPath modulePath [1] [0])
          TypedCollectionShapeMismatch
          (TypedRecipeDetail stagedRecipe directRecipe),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [1] [0, 0])
          TypedCallableShapeMismatch
          (TypedBinderDetail directBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0])
          TypedCollectionShapeMismatch
          (TypedRecipeDetail stagedRecipe directRecipe),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0])
          TypedCollectionShapeMismatch
          (TypedRecipeDetail stagedRecipe directRecipe),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0, 0])
          TypedCallableShapeMismatch
          (TypedBinderDetail directBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0, 1])
          TypedCallableShapeMismatch
          (TypedBinderDetail directBinder)
      ]

constructorPatternFieldRecipeStagingFixture :: InvalidFixture
constructorPatternFieldRecipeStagingFixture =
  InvalidFixture fixture program failures
  where
    fixture = "constructor-pattern-field-recipe-staging"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Handler"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Handler"
    constructorBinder = binder modulePath [0, 0] constructorName
    callableType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    stagedRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    dataType = TypedDataType dataName []
    dataRecipe = TypedManagedVariantRecipe dataName []
    dataInfo = info dataType dataRecipe
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [callableType] [stagedRecipe]]
    constructorInfo = info (TypedFunctionType callableType dataType) (TypedClosureRecipe [stagedRecipe] dataRecipe)
    handlerOuterName = fixtureValueName "handlerOuter"
    handlerOuterBinder = binder modulePath [1, 0, 0] handlerOuterName
    handlerInnerName = fixtureValueName "handlerInner"
    handlerInnerBinder = binder modulePath [1, 0, 0, 0] handlerInnerName
    handlerExpression =
      TypedLambdaExpr
        (info callableType stagedRecipe)
        handlerOuterBinder
        handlerOuterName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            handlerInnerBinder
            handlerInnerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "handled"))
        )
    scrutinee =
      TypedApplyExpr
        dataInfo
        (TypedVariableExpr constructorInfo constructorName (Just constructorBinder))
        handlerExpression
    fieldName = fixtureValueName "field"
    fieldBinder = binder modulePath [1, 0, 1] fieldName
    patternValue =
      TypedConstructorPattern
        dataInfo
        constructorName
        [TypedVariablePattern (info callableType directRecipe) fieldBinder fieldName]
    caseExpression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]
    statements = [TypedDataStatement declaration, expressionStatement 2 caseExpression]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
    failures =
      [ TypedCoreValidationFailure
          (TypedPatternPath modulePath [1] [0, 0, 0])
          TypedPatternScrutineeMismatch
          (TypedRecipeDetail stagedRecipe directRecipe)
      ]

directCallableValueUseFixture :: InvalidFixture
directCallableValueUseFixture =
  InvalidFixture fixture program failures
  where
    fixture = "direct-callable-value-use"
    modulePath = fixtureModulePath fixture
    directName = fixtureValueName "direct"
    directBinder = binder modulePath [0] directName
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0] innerName
    directType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    directInfo = info directType directRecipe
    directScheme = TypedScheme directBinder [] [] [] directType directRecipe (Just TypedDirectCallableShape)
    directExpression =
      TypedLambdaExpr
        directInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            innerBinder
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "done"))
        )
    directReference = TypedVariableExpr directInfo directName (Just directBinder)
    partialInfo = info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    partialApplication = TypedApplyExpr partialInfo directReference trueExpr
    completeApplication =
      TypedApplyExpr
        textInfo
        partialApplication
        (literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x'))
    statements =
      [ TypedLetStatement directBinder directName span1 directScheme directExpression,
        expressionStatement 2 directReference,
        expressionStatement 3 partialApplication,
        expressionStatement 4 completeApplication
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
    failures =
      [ TypedCoreValidationFailure
          (TypedExpressionPath modulePath [1] [0])
          TypedCallableShapeMismatch
          (TypedBinderDetail directBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0, 0])
          TypedCallableShapeMismatch
          (TypedBinderDetail directBinder)
      ]

directBindingWithoutLeadingLambdaFixture :: InvalidFixture
directBindingWithoutLeadingLambdaFixture =
  InvalidFixture
    fixture
    program
    [statementFailure fixture 0 TypedCallableShapeMismatch (TypedBinderDetail functionBinder)]
  where
    fixture = "direct-binding-without-leading-lambda"
    modulePath = fixtureModulePath fixture
    functionName = fixtureValueName "choose"
    functionBinder = binder modulePath [0] functionName
    argumentType = TypedBoolType
    resultType = TypedBoolType
    functionType = TypedFunctionType argumentType resultType
    functionRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    functionInfo = info functionType functionRecipe
    functionScheme =
      TypedScheme
        functionBinder
        []
        []
        []
        functionType
        functionRecipe
        (Just TypedDirectCallableShape)
    branch branchIndex =
      TypedLambdaExpr
        functionInfo
        (binder modulePath [0, branchIndex] argumentName)
        argumentName
        trueExpr
    argumentName = fixtureValueName "item"
    functionExpression = TypedIfExpr functionInfo trueExpr (branch 0) (branch 1)
    statements =
      [ TypedLetStatement functionBinder functionName span1 functionScheme functionExpression,
        expressionStatement 2 trueExpr
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath

flattenedOperatorSectionRecipeFixture :: InvalidFixture
flattenedOperatorSectionRecipeFixture =
  InvalidFixture fixture program failures
  where
    fixture = "flattened-operator-section-recipe"
    modulePath = fixtureModulePath fixture
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    operatorBinder = binder modulePath [0] operatorName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType))
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    operatorInfo = info operatorType operatorRecipe
    operatorScheme = TypedScheme operatorBinder [] [] [] operatorType operatorRecipe (Just TypedDirectCallableShape)
    firstName = fixtureValueName "first"
    firstBinder = binder modulePath [0, 0] firstName
    secondName = fixtureValueName "second"
    secondBinder = binder modulePath [0, 0, 0] secondName
    thirdName = fixtureValueName "third"
    thirdBinder = binder modulePath [0, 0, 0, 0] thirdName
    operatorExpression =
      TypedLambdaExpr
        operatorInfo
        firstBinder
        firstName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType)) (TypedClosureRecipe [TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe))
            secondBinder
            secondName
            ( TypedLambdaExpr
                (info (TypedFunctionType TypedBoolType TypedTextType) (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe))
                thirdBinder
                thirdName
                (TypedLiteralExpr textInfo (TypedTextLiteral "section"))
            )
        )
    operator = TypedResolvedOperator operatorName "~"
    leftType = TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType)
    leftRecipe = TypedClosureRecipe [TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    expectedLeftRecipe = TypedClosureRecipe [TypedCharRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    rightType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedTextType)
    rightRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    expectedRightRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    statements =
      [ TypedLetStatement operatorBinder operatorName span1 operatorScheme operatorExpression,
        expressionStatement 2 (TypedLeftSectionExpr (info leftType leftRecipe) trueExpr operator),
        expressionStatement
          3
          ( TypedRightSectionExpr
              (info rightType rightRecipe)
              operator
              (literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x'))
          ),
        expressionStatement 4 (TypedOperatorValueExpr operatorInfo operator),
        expressionStatement 5 trueExpr
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
    failures =
      [ expressionFailureAt fixture 1 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder),
        expressionFailureAt fixture 1 TypedCallableRecipeMismatch (TypedRecipeDetail expectedLeftRecipe leftRecipe),
        expressionFailureAt fixture 2 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder),
        expressionFailureAt fixture 2 TypedCallableRecipeMismatch (TypedRecipeDetail expectedRightRecipe rightRecipe),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [3] [0])
          TypedCallableShapeMismatch
          (TypedBinderDetail operatorBinder)
      ]

resolvedOperatorSectionOperandRecipeFixture :: InvalidFixture
resolvedOperatorSectionOperandRecipeFixture =
  InvalidFixture fixture program failures
  where
    fixture = "resolved-operator-section-operand-recipe"
    modulePath = fixtureModulePath fixture
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    operatorBinder = binder modulePath [0] operatorName
    leftType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    stagedLeftRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe]
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    flattenedLeftRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    rightType = TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType)
    stagedRightRecipe =
      TypedClosureRecipe
        [TypedCharRecipe]
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    flattenedRightRecipe = TypedClosureRecipe [TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    operatorType = TypedFunctionType leftType (TypedFunctionType rightType TypedBoolType)
    operatorRecipe = TypedClosureRecipe [stagedLeftRecipe, stagedRightRecipe] TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    operatorScheme =
      TypedScheme
        operatorBinder
        []
        []
        []
        operatorType
        operatorRecipe
        (Just TypedDirectCallableShape)
    leftParameterName = fixtureValueName "left"
    leftParameterBinder = binder modulePath [0, 0] leftParameterName
    rightParameterName = fixtureValueName "right"
    rightParameterBinder = binder modulePath [0, 0, 0] rightParameterName
    operatorExpression =
      TypedLambdaExpr
        operatorInfo
        leftParameterBinder
        leftParameterName
        ( TypedLambdaExpr
            (info (TypedFunctionType rightType TypedBoolType) (TypedClosureRecipe [stagedRightRecipe] TypedBoolRecipe))
            rightParameterBinder
            rightParameterName
            falseExpr
        )
    capturedLeft = callableExpression [1, 0] TypedBoolType TypedCharType flattenedLeftRecipe
    capturedRight = callableExpression [2, 0] TypedCharType TypedBoolType flattenedRightRecipe
    callableExpression lexicalPrefix firstType secondType callableRecipe =
      TypedLambdaExpr
        (info (TypedFunctionType firstType (TypedFunctionType secondType TypedTextType)) callableRecipe)
        (binder modulePath (lexicalPrefix <> [0]) outerName)
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType secondType TypedTextType) (TypedClosureRecipe [expectedRecipeFor secondType] TypedManagedTextRecipe))
            (binder modulePath (lexicalPrefix <> [0, 0]) innerName)
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "captured"))
        )
      where
        outerName = fixtureValueName "outer"
        innerName = fixtureValueName "inner"
    expectedRecipeFor typeValue
      | typeValue == TypedBoolType = TypedBoolRecipe
      | otherwise = TypedCharRecipe
    operator = TypedResolvedOperator operatorName "~"
    leftSectionInfo = info (TypedFunctionType rightType TypedBoolType) (TypedClosureRecipe [stagedRightRecipe] TypedBoolRecipe)
    rightSectionInfo = info (TypedFunctionType leftType TypedBoolType) (TypedClosureRecipe [stagedLeftRecipe] TypedBoolRecipe)
    statements =
      [ TypedLetStatement operatorBinder operatorName span1 operatorScheme operatorExpression,
        expressionStatement 2 (TypedLeftSectionExpr leftSectionInfo capturedLeft operator),
        expressionStatement 3 (TypedRightSectionExpr rightSectionInfo operator capturedRight),
        expressionStatement 4 trueExpr
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
    failures =
      [ expressionFailureAt fixture 1 TypedApplicationArgumentMismatch (TypedRecipeDetail stagedLeftRecipe flattenedLeftRecipe),
        expressionFailureAt fixture 1 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [1] [0, 0])
          TypedCallableRecipeMismatch
          (TypedRecipeDetail stagedLeftRecipe flattenedLeftRecipe),
        expressionFailureAt fixture 2 TypedApplicationArgumentMismatch (TypedRecipeDetail stagedRightRecipe flattenedRightRecipe),
        expressionFailureAt fixture 2 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder),
        TypedCoreValidationFailure
          (TypedExpressionPath modulePath [2] [0, 0])
          TypedCallableRecipeMismatch
          (TypedRecipeDetail stagedRightRecipe flattenedRightRecipe)
      ]

binaryOperatorResultRecipeStagingFixture :: InvalidFixture
binaryOperatorResultRecipeStagingFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt
        fixture
        1
        TypedApplicationResultMismatch
        (TypedRecipeDetail stagedResultRecipe flattenedResultRecipe)
    ]
  where
    fixture = "binary-operator-result-recipe-staging"
    modulePath = fixtureModulePath fixture
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    operatorBinder = binder modulePath [0] operatorName
    leftName = fixtureValueName "left"
    leftBinder = binder modulePath [0, 0] leftName
    rightName = fixtureValueName "right"
    rightBinder = binder modulePath [0, 0, 0] rightName
    resultOuterName = fixtureValueName "resultOuter"
    resultOuterBinder = binder modulePath [0, 0, 0, 0] resultOuterName
    resultInnerName = fixtureValueName "resultInner"
    resultInnerBinder = binder modulePath [0, 0, 0, 0, 0] resultInnerName
    resultType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    stagedResultRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    flattenedResultRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType resultType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe]
        (TypedClosureRecipe [TypedCharRecipe] stagedResultRecipe)
    operatorInfo = info operatorType operatorRecipe
    operatorScheme =
      TypedScheme
        operatorBinder
        []
        []
        []
        operatorType
        operatorRecipe
        (Just TypedClosureCallableShape)
    operatorExpression =
      TypedLambdaExpr
        operatorInfo
        leftBinder
        leftName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType resultType) (TypedClosureRecipe [TypedCharRecipe] stagedResultRecipe))
            rightBinder
            rightName
            ( TypedLambdaExpr
                (info resultType stagedResultRecipe)
                resultOuterBinder
                resultOuterName
                ( TypedLambdaExpr
                    (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
                    resultInnerBinder
                    resultInnerName
                    (TypedLiteralExpr textInfo (TypedTextLiteral "result"))
                )
            )
        )
    binaryExpression =
      TypedBinaryExpr
        (info resultType flattenedResultRecipe)
        (TypedResolvedOperator operatorName "~")
        trueExpr
        (literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x'))
    statements =
      [ TypedLetStatement operatorBinder operatorName span1 operatorScheme operatorExpression,
        expressionStatement 2 binaryExpression
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface (typedExpressionInfo binaryExpression) modulePath

builtinApplicationOperatorResultRecipeStagingFixture :: InvalidFixture
builtinApplicationOperatorResultRecipeStagingFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailure
        fixture
        TypedApplicationResultMismatch
        (TypedRecipeDetail stagedResultRecipe flattenedResultRecipe)
    ]
  where
    fixture = "builtin-application-operator-result-recipe-staging"
    modulePath = fixtureModulePath fixture
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    middleName = fixtureValueName "middle"
    middleBinder = binder modulePath [0, 0, 0] middleName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0, 0] innerName
    resultType = TypedFunctionType TypedCharType (TypedFunctionType TypedTextType TypedIntType)
    stagedResultRecipe = TypedClosureRecipe [TypedCharRecipe] (TypedClosureRecipe [TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64))
    flattenedResultRecipe = TypedClosureRecipe [TypedCharRecipe, TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64)
    functionType = TypedFunctionType TypedBoolType resultType
    functionInfo = info functionType (TypedClosureRecipe [TypedBoolRecipe] stagedResultRecipe)
    functionExpression =
      TypedLambdaExpr
        functionInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info resultType stagedResultRecipe)
            middleBinder
            middleName
            ( TypedLambdaExpr
                (info (TypedFunctionType TypedTextType TypedIntType) (TypedClosureRecipe [TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64)))
                innerBinder
                innerName
                (literalExpr TypedIntType (TypedSignedIntegerRecipe 64) (TypedIntegerLiteral "1"))
            )
        )
    expression =
      TypedBinaryExpr
        (info resultType flattenedResultRecipe)
        (TypedBuiltinOperator "$")
        functionExpression
        trueExpr
    program = expressionFixtureProgram fixture expression

underappliedDirectBinaryOperatorFixture :: InvalidFixture
underappliedDirectBinaryOperatorFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt
        fixture
        1
        TypedCallableShapeMismatch
        (TypedBinderDetail operatorBinder)
    ]
  where
    fixture = "underapplied-direct-binary-operator"
    modulePath = fixtureModulePath fixture
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    operatorBinder = binder modulePath [0] operatorName
    firstName = fixtureValueName "first"
    firstBinder = binder modulePath [0, 0] firstName
    secondName = fixtureValueName "second"
    secondBinder = binder modulePath [0, 0, 0] secondName
    thirdName = fixtureValueName "third"
    thirdBinder = binder modulePath [0, 0, 0, 0] thirdName
    resultType = TypedFunctionType TypedTextType TypedIntType
    resultRecipe = TypedClosureRecipe [TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64)
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType resultType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe, TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64)
    operatorInfo = info operatorType operatorRecipe
    operatorScheme =
      TypedScheme
        operatorBinder
        []
        []
        []
        operatorType
        operatorRecipe
        (Just TypedDirectCallableShape)
    operatorExpression =
      TypedLambdaExpr
        operatorInfo
        firstBinder
        firstName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType resultType) (TypedClosureRecipe [TypedCharRecipe, TypedManagedTextRecipe] (TypedSignedIntegerRecipe 64)))
            secondBinder
            secondName
            ( TypedLambdaExpr
                (info resultType resultRecipe)
                thirdBinder
                thirdName
                (literalExpr TypedIntType (TypedSignedIntegerRecipe 64) (TypedIntegerLiteral "1"))
            )
        )
    expression =
      TypedBinaryExpr
        (info resultType resultRecipe)
        (TypedResolvedOperator operatorName "~")
        trueExpr
        (literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x'))
    statements =
      [ TypedLetStatement operatorBinder operatorName span1 operatorScheme operatorExpression,
        expressionStatement 2 expression
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface (typedExpressionInfo expression) modulePath

underappliedDirectOperatorSectionsFixture :: InvalidFixture
underappliedDirectOperatorSectionsFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt fixture 1 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder),
      expressionFailureAt fixture 2 TypedCallableShapeMismatch (TypedBinderDetail operatorBinder)
    ]
  where
    fixture = "underapplied-direct-operator-sections"
    modulePath = fixtureModulePath fixture
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    operatorBinder = binder modulePath [0] operatorName
    leftName = fixtureValueName "left"
    leftBinder = binder modulePath [0, 0] leftName
    rightName = fixtureValueName "right"
    rightBinder = binder modulePath [0, 0, 0] rightName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    operatorInfo = info operatorType operatorRecipe
    operatorScheme =
      TypedScheme
        operatorBinder
        []
        []
        []
        operatorType
        operatorRecipe
        (Just TypedDirectCallableShape)
    operatorExpression =
      TypedLambdaExpr
        operatorInfo
        leftBinder
        leftName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            rightBinder
            rightName
            (TypedLiteralExpr textInfo (TypedTextLiteral "section"))
        )
    operator = TypedResolvedOperator operatorName "~"
    leftSectionInfo =
      info
        (TypedFunctionType TypedCharType TypedTextType)
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    rightSectionInfo =
      info
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    statements =
      [ TypedLetStatement operatorBinder operatorName span1 operatorScheme operatorExpression,
        expressionStatement 2 (TypedLeftSectionExpr leftSectionInfo trueExpr operator),
        expressionStatement
          3
          ( TypedRightSectionExpr
              rightSectionInfo
              operator
              (literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x'))
          ),
        expressionStatement 4 trueExpr
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath

applicationArgumentRecipeStagingFixture :: InvalidFixture
applicationArgumentRecipeStagingFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt fixture 1 TypedApplicationArgumentMismatch (TypedRecipeDetail stagedCallableRecipe directCallableRecipe),
      TypedCoreValidationFailure
        (TypedExpressionPath modulePath [1] [0, 1])
        TypedCallableShapeMismatch
        (TypedBinderDetail directBinder)
    ]
  where
    fixture = "application-argument-recipe-staging"
    modulePath = fixtureModulePath fixture
    directName = fixtureValueName "direct"
    directBinder = binder modulePath [0] directName
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0] innerName
    directType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directCallableRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    stagedCallableRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    directInfo = info directType directCallableRecipe
    directScheme = TypedScheme directBinder [] [] [] directType directCallableRecipe (Just TypedDirectCallableShape)
    directExpression =
      TypedLambdaExpr
        directInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            innerBinder
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "ok"))
        )
    callableName = fixtureValueName "callable"
    callableBinder = binder modulePath [1, 0, 0] callableName
    applyType = TypedFunctionType directType TypedBoolType
    applyInfo = info applyType (TypedClosureRecipe [stagedCallableRecipe] TypedBoolRecipe)
    functionExpression = TypedLambdaExpr applyInfo callableBinder callableName trueExpr
    argumentExpression = TypedVariableExpr directInfo directName (Just directBinder)
    application = TypedApplyExpr boolInfo functionExpression argumentExpression
    statements =
      [ TypedLetStatement directBinder directName span1 directScheme directExpression,
        expressionStatement 2 application
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath

applicationResultTypeFixture :: InvalidFixture
applicationResultTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
  where
    fixture = "application-result-type"
    functionName = resolved TypedCurrentModule TypedValueNamespace "argument"
    functionExpr = TypedLambdaExpr boolToBoolInfo (binder (fixtureModulePath fixture) [0, 0] functionName) functionName trueExpr
    expression = TypedApplyExpr textInfo functionExpr trueExpr

applicationResultRecipeStagingFixture :: InvalidFixture
applicationResultRecipeStagingFixture =
  expressionFixture
    fixture
    expression
    [expressionFailure fixture TypedApplicationResultMismatch (TypedRecipeDetail expectedResultRecipe actualResultRecipe)]
  where
    fixture = "application-result-recipe-staging"
    modulePath = fixtureModulePath fixture
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    middleName = fixtureValueName "middle"
    middleBinder = binder modulePath [0, 0, 0] middleName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0, 0] innerName
    resultType = TypedFunctionType TypedBoolType boolToBoolType
    expectedResultRecipe = TypedClosureRecipe [TypedBoolRecipe] boolToBoolRecipe
    actualResultRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    functionInfo =
      info
        (TypedFunctionType TypedBoolType resultType)
        (TypedClosureRecipe [TypedBoolRecipe] expectedResultRecipe)
    resultInfo = info resultType expectedResultRecipe
    functionExpr =
      TypedLambdaExpr
        functionInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            resultInfo
            middleBinder
            middleName
            (TypedLambdaExpr boolToBoolInfo innerBinder innerName trueExpr)
        )
    expression = TypedApplyExpr (info resultType actualResultRecipe) functionExpr trueExpr

directLambdaTailRecipeProgressionFixture :: InvalidFixture
directLambdaTailRecipeProgressionFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt fixture 0 TypedLambdaResultMismatch (TypedRecipeDetail expectedTailRecipe actualTailRecipe),
      TypedCoreValidationFailure
        (TypedExpressionPath modulePath [1] [0])
        TypedCallableShapeMismatch
        (TypedBinderDetail functionBinder)
    ]
  where
    fixture = "direct-lambda-tail-recipe-progression"
    modulePath = fixtureModulePath fixture
    functionName = fixtureValueName "direct"
    functionBinder = binder modulePath [0] functionName
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    middleName = fixtureValueName "middle"
    middleBinder = binder modulePath [0, 0, 0] middleName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0, 0] innerName
    functionType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType))
    functionRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    expectedTailRecipe = TypedClosureRecipe [TypedCharRecipe, TypedBoolRecipe] TypedManagedTextRecipe
    actualTailRecipe = TypedClosureRecipe [TypedCharRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    functionInfo = info functionType functionRecipe
    functionScheme = TypedScheme functionBinder [] [] [] functionType functionRecipe (Just TypedDirectCallableShape)
    functionExpression =
      TypedLambdaExpr
        functionInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType (TypedFunctionType TypedBoolType TypedTextType)) actualTailRecipe)
            middleBinder
            middleName
            ( TypedLambdaExpr
                (info (TypedFunctionType TypedBoolType TypedTextType) (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe))
                innerBinder
                innerName
                (TypedLiteralExpr textInfo (TypedTextLiteral "ok"))
            )
        )
    statements =
      [ TypedLetStatement functionBinder functionName span1 functionScheme functionExpression,
        expressionStatement 2 (TypedVariableExpr functionInfo functionName (Just functionBinder))
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface functionInfo modulePath

oversaturationAfterNonCallableResultFixture :: InvalidFixture
oversaturationAfterNonCallableResultFixture =
  expressionFixture
    fixture
    expression
    [ expressionFailure
        fixture
        TypedApplicationFunctionMismatch
        (TypedTypeDetail boolToBoolType TypedBoolType)
    ]
  where
    fixture = "oversaturation-after-non-callable-result"
    argumentName = fixtureValueName "argument"
    argumentBinder = binder (fixtureModulePath fixture) [0, 0] argumentName
    functionExpr =
      TypedLambdaExpr
        boolToBoolInfo
        argumentBinder
        argumentName
        (fixtureBoundVariableExpr argumentBinder boolInfo argumentName)
    completeApplication = TypedApplyExpr boolInfo functionExpr trueExpr
    expression = TypedApplyExpr boolInfo completeApplication falseExpr

ifConditionTypeFixture :: InvalidFixture
ifConditionTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedConditionalConditionMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
  where
    fixture = "if-condition-type"
    condition = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedIfExpr boolInfo condition trueExpr falseExpr

ifBranchTypeFixture :: InvalidFixture
ifBranchTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedConditionalBranchMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
  where
    fixture = "if-branch-type"
    elseExpression = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedIfExpr boolInfo trueExpr trueExpr elseExpression

ifBranchRecipeJoinFixture :: InvalidFixture
ifBranchRecipeJoinFixture =
  InvalidFixture
    fixture
    program
    [ expressionFailureAt fixture 2 TypedConditionalBranchMismatch (TypedRecipeDetail directRecipe closureRecipe),
      TypedCoreValidationFailure
        (TypedExpressionPath modulePath [2] [0, 1])
        TypedCallableShapeMismatch
        (TypedBinderDetail directBinder)
    ]
  where
    fixture = "if-branch-recipe-join"
    modulePath = fixtureModulePath fixture
    callableType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    closureRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    directInfo = info callableType directRecipe
    closureInfo = info callableType closureRecipe
    directName = fixtureValueName "direct"
    directBinder = binder modulePath [0] directName
    closureName = fixtureValueName "closure"
    closureBinder = binder modulePath [1] closureName
    directOuterName = fixtureValueName "directOuter"
    directOuterBinder = binder modulePath [0, 0] directOuterName
    directInnerName = fixtureValueName "directInner"
    directInnerBinder = binder modulePath [0, 0, 0] directInnerName
    closureOuterName = fixtureValueName "closureOuter"
    closureOuterBinder = binder modulePath [1, 0] closureOuterName
    closureInnerName = fixtureValueName "closureInner"
    closureInnerBinder = binder modulePath [1, 0, 0] closureInnerName
    directExpression =
      TypedLambdaExpr
        directInfo
        directOuterBinder
        directOuterName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            directInnerBinder
            directInnerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "direct"))
        )
    closureExpression =
      TypedLambdaExpr
        closureInfo
        closureOuterBinder
        closureOuterName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            closureInnerBinder
            closureInnerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "closure"))
        )
    statements =
      [ TypedLetStatement directBinder directName span1 (TypedScheme directBinder [] [] [] callableType directRecipe (Just TypedDirectCallableShape)) directExpression,
        TypedLetStatement closureBinder closureName span1 (TypedScheme closureBinder [] [] [] callableType closureRecipe (Just TypedClosureCallableShape)) closureExpression,
        expressionStatement
          3
          ( TypedIfExpr
              closureInfo
              trueExpr
              (TypedVariableExpr directInfo directName (Just directBinder))
              (TypedVariableExpr closureInfo closureName (Just closureBinder))
          )
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface closureInfo modulePath

patternScrutineeTypeFixture :: InvalidFixture
patternScrutineeTypeFixture =
  expressionFixture fixture expression [patternFailure fixture TypedPatternScrutineeMismatch (TypedTypeDetail TypedCharType TypedBoolType)]
  where
    fixture = "pattern-scrutinee-type"
    patternValue = TypedWildcardPattern boolInfo
    scrutinee = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]

patternGuardTypeFixture :: InvalidFixture
patternGuardTypeFixture =
  expressionFixture fixture expression [patternFailure fixture TypedPatternGuardMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
  where
    fixture = "pattern-guard-type"
    guard = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedWildcardPattern boolInfo) (Just guard) trueExpr]

patternArmResultTypeFixture :: InvalidFixture
patternArmResultTypeFixture =
  expressionFixture fixture expression [patternFailure fixture TypedPatternArmResultMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
  where
    fixture = "pattern-arm-result-type"
    result = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedWildcardPattern boolInfo) Nothing result]

patternArmRecipeJoinFixture :: InvalidFixture
patternArmRecipeJoinFixture =
  InvalidFixture
    fixture
    program
    [ TypedCoreValidationFailure
        (TypedPatternPath modulePath [1] [0, 0])
        TypedPatternArmResultMismatch
        (TypedRecipeDetail closureRecipe directRecipe),
      TypedCoreValidationFailure
        (TypedExpressionPath modulePath [1] [0, 1])
        TypedCallableShapeMismatch
        (TypedBinderDetail directBinder)
    ]
  where
    fixture = "pattern-arm-recipe-join"
    modulePath = fixtureModulePath fixture
    callableType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    directRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe
    closureRecipe = TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    directInfo = info callableType directRecipe
    closureInfo = info callableType closureRecipe
    directName = fixtureValueName "direct"
    directBinder = binder modulePath [0] directName
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath [0, 0] outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath [0, 0, 0] innerName
    directExpression =
      TypedLambdaExpr
        directInfo
        outerBinder
        outerName
        ( TypedLambdaExpr
            (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
            innerBinder
            innerName
            (TypedLiteralExpr textInfo (TypedTextLiteral "direct"))
        )
    caseExpression =
      TypedPatternCaseExpr
        closureInfo
        trueExpr
        [ TypedCaseArm
            (TypedWildcardPattern boolInfo)
            Nothing
            (TypedVariableExpr directInfo directName (Just directBinder))
        ]
    statements =
      [ TypedLetStatement directBinder directName span1 (TypedScheme directBinder [] [] [] callableType directRecipe (Just TypedDirectCallableShape)) directExpression,
        expressionStatement 2 caseExpression
      ]
    program = singleModuleProgram fixture relativeSource [] statements emptyInterface closureInfo modulePath

orPatternBinderContractFixture :: InvalidFixture
orPatternBinderContractFixture =
  expressionFixture fixture expression [patternFailure fixture TypedOrPatternBinderMismatch (TypedBinderDetail secondBinder)]
  where
    fixture = "or-pattern-binder-contract"
    firstName = fixtureValueName "first"
    secondName = fixtureValueName "second"
    firstBinder = fixtureBinder fixture 0 firstName
    secondBinder = fixtureBinder fixture 1 secondName
    patternValue =
      TypedOrPattern
        boolInfo
        [ TypedVariablePattern boolInfo firstBinder firstName,
          TypedVariablePattern boolInfo secondBinder secondName
        ]
    expression = TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm patternValue Nothing trueExpr]

duplicateEvidenceParameterFixture :: InvalidFixture
duplicateEvidenceParameterFixture =
  InvalidFixture fixture program failures
  where
    fixture = "duplicate-or-noncanonical-evidence-parameter"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence =
      [ TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
        TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
        TypedEvidenceParameter (TypedEvidenceParameterId 3) constraint
      ]
    scheme = fixtureScheme valueBinder [] evidence [] TypedBoolType TypedBoolRecipe
    program = withFixturePrelude (signatureProgram fixture valueBinder valueName scheme)
    failures =
      [ statementFailure fixture 0 TypedDuplicateEvidenceParameter (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
        statementFailure fixture 0 TypedDuplicateEvidenceParameter (TypedEvidenceParameterDetail (TypedEvidenceParameterId 3)),
        statementFailure fixture 0 TypedInvalidEvidenceParameterOrder (TypedIndexDetail 1),
        statementFailure fixture 0 TypedInvalidEvidenceParameterOrder (TypedIndexDetail 2)
      ]

instantiationContractFixture :: InvalidFixture
instantiationContractFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedInstantiationMismatch (TypedBinderDetail unknownOwner)]
  where
    fixture = "instantiation-contract"
    unknownName = fixtureValueName "unknown"
    unknownOwner = fixtureBinder fixture 9 unknownName
    instantiation = TypedInstantiation unknownOwner [] Nothing
    expression = TypedVariableExpr (TypedNodeInfo builtinMapType builtinMapValueRecipe [instantiation] []) (TypedBuiltinName "map") Nothing

missingOrDuplicateEvidenceFixture :: InvalidFixture
missingOrDuplicateEvidenceFixture =
  InvalidFixture fixture program failures
  where
    fixture = "missing-or-duplicate-evidence"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    duplicateOwner = fixtureBinder fixture 9 (fixtureValueName "duplicateEvidence")
    use =
      TypedEvidenceUse
        (Just (TypedEvidenceParameterRef duplicateOwner (TypedEvidenceParameterId 0)))
        constraint
        implId
        Nothing
    missingExpression =
      TypedLambdaExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint []])
        (fixtureBinder fixture 0 missingArgument)
        missingArgument
        trueExpr
    missingArgument = fixtureValueName "missingArgument"
    duplicateExpression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (TypedBooleanLiteral True)
    program = withFixturePrelude (singleModuleProgram fixture relativeSource [] [expressionStatement 1 missingExpression, expressionStatement 2 duplicateExpression] emptyInterface boolInfo (fixtureModulePath fixture))
    failures =
      [ expressionFailureAt fixture 0 TypedMissingEvidence (TypedTextDetail "Equal"),
        expressionFailureAt fixture 1 TypedDuplicateEvidence (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
        expressionFailureAt fixture 1 TypedInstantiationMismatch (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
        expressionFailureAt fixture 1 TypedInstantiationMismatch (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
      ]

ambiguousOrInvisibleEvidenceFixture :: InvalidFixture
ambiguousOrInvisibleEvidenceFixture =
  InvalidFixture fixture program failures
  where
    fixture = "ambiguous-or-invisible-evidence"
    constraint = TypedCapabilityConstraint (preludeCapability "Render") Nothing TypedTextType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId (fixtureLibraryPath "Render") capabilityName [TypedTextType]
    invisibleImpl = TypedImplId ["Hidden", "Render"] capabilityName [TypedTextType]
    ambiguousExpression =
      TypedLiteralExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedEvidenceCandidates constraint [TypedEvidenceCandidate firstImpl Nothing, TypedEvidenceCandidate secondImpl Nothing]])
        (TypedTextLiteral "ambiguous")
    invisibleUse = TypedEvidenceUse Nothing constraint invisibleImpl Nothing
    invisibleExpression =
      TypedLiteralExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedSelectedEvidence invisibleUse])
        (TypedTextLiteral "invisible")
    program = withFixturePrelude (singleModuleProgram fixture relativeSource [] [expressionStatement 1 ambiguousExpression, expressionStatement 2 invisibleExpression] emptyInterface textInfo (fixtureModulePath fixture))
    failures =
      [ expressionFailureAt fixture 0 TypedAmbiguousEvidence (TypedArityDetail 1 2),
        expressionFailureAt fixture 1 TypedInvisibleImpl (TypedImplDetail invisibleImpl)
      ]

methodOrInterfaceIdentityFixture :: InvalidFixture
methodOrInterfaceIdentityFixture =
  InvalidFixture fixture program failures
  where
    fixture = "method-or-interface-identity"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    otherImpl = TypedImplId ["Prelude"] capabilityName [TypedCharType]
    mismatchedMethod = TypedMethodId otherImpl "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just mismatchedMethod)
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBooleanLiteral True)
    valueName = fixtureValueName "published"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = monoScheme valueBinder
    statement = TypedLetStatement valueBinder valueName span1 scheme trueExpr
    program =
      withFixturePrelude
        ( singleModuleProgram
            fixture
            relativeSource
            [TypedModuleExport TypedValueNamespace "published"]
            [expressionStatement 1 expression, statement]
            emptyInterface
            boolInfo
            (fixtureModulePath fixture)
        )
    failures =
      [ TypedCoreValidationFailure (TypedInterfacePath (fixtureModulePath fixture)) TypedModuleInterfaceMismatch (TypedNameDetail valueName),
        expressionFailureAt fixture 0 TypedMethodSelectionMismatch (TypedImplDetail otherImpl)
      ]

expressionFixture :: Text -> TypedExpr -> [TypedCoreValidationFailure] -> InvalidFixture
expressionFixture fixture expression failures =
  InvalidFixture
    fixture
    (singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface (expressionInfoForFixture expression) (fixtureModulePath fixture))
    failures
