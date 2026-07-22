{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedProgramRuntimeValue,
  )
import JazzNext.Compiler.Runtime (renderRuntimeValue)
import JazzNext.Compiler.TypedCore
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzTypedCoreContract" tests

tests :: [NamedTest]
tests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("renders the complete valid contract deterministically", testValidContractRendering),
    ("renders every typed-core outcome deterministically", testOutcomeRendering)
  ]

testValidFixtureManifest :: IO ()
testValidFixtureManifest = do
  assertEqual "valid fixture names" expectedValidFixtureNames (map validFixtureName validFixtures)
  assertEqual "valid fixture count" 16 (length validFixtures)

testValidContractRendering :: IO ()
testValidContractRendering = do
  let first = map (renderRuntimeValue . canonicalTypedProgramRuntimeValue . validFixtureProgram) validFixtures
      second = map (renderRuntimeValue . canonicalTypedProgramRuntimeValue . validFixtureProgram) validFixtures
  assertEqual "complete valid typed-core rendering" first second

testOutcomeRendering :: IO ()
testOutcomeRendering = do
  let failure = TypedCoreValidationFailure TypedProgramPath TypedUnknownEntryModule TypedNoValidationDetail
      outcomes =
        [ TypedCoreBlockedByDiagnostics,
          TypedCoreInvariantFailures [failure],
          TypedCoreSucceeded scalarAliasesWidthsProgram
        ]
      first = map (renderRuntimeValue . canonicalTypedCoreOutcomeRuntimeValue) outcomes
      second = map (renderRuntimeValue . canonicalTypedCoreOutcomeRuntimeValue) outcomes
  assertEqual "typed-core outcome constructor count" 3 (length outcomes)
  assertEqual "typed-core outcome rendering" first second

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
    "multi-module-interface"
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
    "multi-module-interface" -> multiModuleInterfaceProgram
    _ -> error "unknown valid typed-core fixture"

programWith :: Text -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> TypedProgram
programWith fixtureName statements interface moduleInfo =
  TypedProgram
    Nothing
    [ TypedModule
        ["Fixture", fixtureName]
        (TypedSourcePath ("src/Fixture/" <> fixtureName <> ".jz"))
        []
        []
        interface
        statements
        moduleInfo
    ]
    ["Fixture", fixtureName]

scalarAliasesWidthsProgram :: TypedProgram
scalarAliasesWidthsProgram =
  programWith
    "scalar-aliases-widths"
    (zipWith expressionStatement [1 ..] scalarExpressions)
    emptyInterface
    boolInfo
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

resolvedNameOriginsProgram :: TypedProgram
resolvedNameOriginsProgram =
  programWith
    "resolved-name-origins"
    (zipWith expressionStatement [1 ..] (map (TypedVariableExpr textInfo) names))
    emptyInterface
    textInfo
  where
    names =
      [ resolved TypedCurrentModule TypedValueNamespace "localValue",
        resolved (TypedImportedModule ["Library", "Data"]) TypedConstructorNamespace "Some",
        resolved TypedAmbientPrelude TypedTypeNamespace "List",
        resolved TypedCurrentModule TypedCapabilityNamespace "Printable"
      ]

builtinGeneratedNamesProgram :: TypedProgram
builtinGeneratedNamesProgram =
  programWith
    "builtin-generated-names"
    (zipWith expressionStatement [1 ..] (map (TypedVariableExpr textInfo) names))
    emptyInterface
    textInfo
  where
    names =
      [ TypedBuiltinName "map",
        TypedGeneratedName (TypedLambdaPatternArgument 0),
        TypedGeneratedName (TypedOperatorBinding "+"),
        TypedGeneratedName TypedOperatorSectionFunction,
        TypedGeneratedName TypedOperatorSectionLeft,
        TypedGeneratedName TypedOperatorSectionRight
      ]

listTupleDataRecipesProgram :: TypedProgram
listTupleDataRecipesProgram =
  programWith
    "list-tuple-data-recipes"
    [ expressionStatement 1 (TypedTupleExpr unitInfo []),
      expressionStatement 2 (TypedTupleExpr pairInfo [trueExpr, falseExpr]),
      expressionStatement 3 (TypedListExpr boolListInfo [trueExpr, falseExpr]),
      expressionStatement 4 (TypedVariableExpr optionInfo optionConstructor)
    ]
    emptyInterface
    optionInfo
  where
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    optionConstructor = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    optionInfo =
      info
        (TypedDataType optionName [TypedBoolType])
        (TypedManagedVariantRecipe optionName [TypedBoolType])

callableRecipesProgram :: TypedProgram
callableRecipesProgram =
  programWith
    "callable-recipes"
    [expressionStatement 1 lambda]
    emptyInterface
    callableInfo
  where
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder ["Fixture", "callable-recipes"] [0] argumentName
    functionType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    callableInfo =
      info functionType (TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe)
    innerInfo =
      info
        (TypedFunctionType TypedCharType TypedTextType)
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    lambda =
      TypedLambdaExpr
        callableInfo
        argumentBinder
        argumentName
        (TypedLambdaExpr innerInfo argumentBinder argumentName (TypedLiteralExpr textInfo (TypedTextLiteral "ok")))

monomorphicBindingProgram :: TypedProgram
monomorphicBindingProgram =
  programWith
    fixture
    [TypedLetStatement valueBinder valueName span1 scheme trueExpr]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    boolInfo
  where
    fixture = "monomorphic-binding"
    valueName = resolved TypedCurrentModule TypedValueNamespace "enabled"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

generalizedBindingProgram :: TypedProgram
generalizedBindingProgram =
  programWith
    fixture
    [TypedSignatureStatement valueBinder valueName span1 scheme]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    polymorphicInfo
  where
    fixture = "generalized-binding"
    valueName = resolved TypedCurrentModule TypedValueNamespace "choose"
    valueBinder = binder ["Fixture", fixture] [0] valueName
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
    polymorphicInfo = info polymorphicType polymorphicRecipe
    scheme =
      TypedScheme
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

instantiationProgram :: Text -> Maybe TypedSpan -> TypedProgram
instantiationProgram fixture explicitSpan =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder ["Fixture", fixture] [0] name
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument (TypedTypeParameterId 0) TypedBoolType]
        explicitSpan
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) name

explicitCapabilityEvidenceProgram :: TypedProgram
explicitCapabilityEvidenceProgram = evidenceProgram "explicit-capability-evidence" (Just (TypedEvidenceParameterId 0))

inferredCapabilityEvidenceProgram :: TypedProgram
inferredCapabilityEvidenceProgram = evidenceProgram "inferred-capability-evidence" Nothing

evidenceProgram :: Text -> Maybe TypedEvidenceParameterId -> TypedProgram
evidenceProgram fixture parameterId =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    capability = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    evidenceUse = TypedEvidenceUse parameterId capability implId Nothing
    expression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (resolved TypedCurrentModule TypedValueNamespace "same")

qualifiedMethodSelectionProgram :: TypedProgram
qualifiedMethodSelectionProgram =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    fixture = "qualified-method-selection"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (resolved (TypedImportedModule ["Prelude"]) TypedValueNamespace "equal")

partialMethodCandidatesProgram :: TypedProgram
partialMethodCandidatesProgram =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    fixture = "partial-method-candidates"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    constraint = TypedCapabilityConstraint "Render" (Just "Render.render") TypedTextType
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId ["Fixture", fixture] capabilityName [TypedTextType]
    candidates =
      [ TypedEvidenceCandidate firstImpl (Just (TypedMethodId firstImpl "render")),
        TypedEvidenceCandidate secondImpl (Just (TypedMethodId secondImpl "render"))
      ]
    expression =
      TypedVariableExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedEvidenceCandidates constraint candidates])
        (resolved TypedCurrentModule TypedValueNamespace "render")

patternsBindersProgram :: TypedProgram
patternsBindersProgram =
  programWith fixture statements emptyInterface boolInfo
  where
    fixture = "patterns-binders"
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    variablePattern = TypedVariablePattern boolInfo valueBinder valueName
    asPattern = TypedAsPattern boolInfo valueBinder valueName (TypedWildcardPattern boolInfo)
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
    tupleCase =
      TypedPatternCaseExpr
        boolInfo
        (TypedTupleExpr pairInfo [trueExpr, falseExpr])
        [TypedCaseArm (TypedTuplePattern pairInfo [variablePattern, TypedWildcardPattern boolInfo]) Nothing trueExpr]
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
    constructorCase =
      TypedPatternCaseExpr
        boolInfo
        (TypedVariableExpr optionInfo someName)
        [TypedCaseArm (TypedConstructorPattern optionInfo someName [variablePattern]) Nothing trueExpr]
    statements =
      zipWith
        expressionStatement
        [1 ..]
        [ boolCase (TypedWildcardPattern boolInfo),
          boolCase variablePattern,
          boolCase (TypedLiteralPattern boolInfo (TypedBooleanLiteral True)),
          constructorCase,
          listCase (TypedListPattern boolListPatternInfo [variablePattern]),
          listCase (TypedConsListPattern boolListPatternInfo variablePattern (TypedListPattern boolListPatternInfo [])),
          tupleCase,
          boolCase asPattern,
          boolCase (TypedOrPattern boolInfo [variablePattern, variablePattern])
        ]

orPatternAlignmentProgram :: TypedProgram
orPatternAlignmentProgram =
  programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo
  where
    fixture = "or-pattern-alignment"
    valueName = resolved TypedCurrentModule TypedValueNamespace "matched"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    alternative = TypedVariablePattern boolInfo valueBinder valueName
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedOrPattern boolInfo [alternative, alternative]) Nothing trueExpr]

multiModuleInterfaceProgram :: TypedProgram
multiModuleInterfaceProgram =
  TypedProgram
    (Just preludeModule)
    [libraryModule, entryModule]
    ["App", "Main"]
  where
    preludeName = resolved TypedAmbientPrelude TypedValueNamespace "truth"
    preludeBinder = binder ["Prelude"] [0] preludeName
    preludeScheme = TypedScheme preludeBinder [] [] [] TypedBoolType TypedBoolRecipe
    preludeModule =
      TypedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedValueNamespace "truth"]
        (TypedModuleInterface [TypedValueInterface preludeName preludeScheme] [] [] [])
        [TypedLetStatement preludeBinder preludeName span1 preludeScheme trueExpr]
        boolInfo
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorBinder = binder ["Library", "Flag"] [0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedBoolRecipe]]
    libraryModule =
      TypedModule
        ["Library", "Flag"]
        (TypedSourcePath "src/Library/Flag.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Flag", TypedModuleExport TypedConstructorNamespace "Flag"]
        (TypedModuleInterface [] [TypedDataInterface declaration] [] [])
        [TypedDataStatement declaration]
        (info (TypedDataType dataName []) (TypedManagedVariantRecipe dataName []))
    entryModule =
      TypedModule
        ["App", "Main"]
        (TypedSourcePath "src/App/Main.jz")
        [ TypedResolvedImport span1 ["Prelude"] Nothing (Just ["truth"]),
          TypedResolvedImport span1 ["Library", "Flag"] (Just "FlagModule") Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

emptyInterface :: TypedModuleInterface
emptyInterface = TypedModuleInterface [] [] [] []

span1 :: TypedSpan
span1 = TypedSpan 1 1

expressionStatement :: Int -> TypedExpr -> TypedStatement
expressionStatement line expression = TypedExpressionStatement (TypedSpan line 1) expression

literalExpr :: TypedType -> TypedRepresentationRecipe -> TypedLiteral -> TypedExpr
literalExpr typeValue recipe literal = TypedLiteralExpr (info typeValue recipe) literal

info :: TypedType -> TypedRepresentationRecipe -> TypedNodeInfo
info typeValue recipe = TypedNodeInfo typeValue recipe [] []

boolInfo :: TypedNodeInfo
boolInfo = info TypedBoolType TypedBoolRecipe

textInfo :: TypedNodeInfo
textInfo = info TypedTextType TypedManagedTextRecipe

unitInfo :: TypedNodeInfo
unitInfo = info (TypedTupleType []) (TypedManagedProductRecipe [])

pairInfo :: TypedNodeInfo
pairInfo = info (TypedTupleType [TypedBoolType, TypedBoolType]) (TypedManagedProductRecipe [TypedBoolRecipe, TypedBoolRecipe])

boolListInfo :: TypedNodeInfo
boolListInfo = info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe)

trueExpr :: TypedExpr
trueExpr = TypedLiteralExpr boolInfo (TypedBooleanLiteral True)

falseExpr :: TypedExpr
falseExpr = TypedLiteralExpr boolInfo (TypedBooleanLiteral False)

resolved :: TypedNameOrigin -> TypedNameNamespace -> Text -> TypedCoreName
resolved = TypedResolvedName

binder :: [Text] -> [Int] -> TypedCoreName -> TypedBinderId
binder modulePath lexicalPath name = TypedBinderId (modulePath, lexicalPath, name)
