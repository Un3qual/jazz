{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedProgramRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    decodeCanonicalTypedValidationFailuresRuntimeValue,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.Name (identifierText)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
  )
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )
import JazzNext.TestSource (readCheckedInJazzProjectModuleSource)

main :: IO ()
main = runTestSuite "JazzTypedCoreContract" tests

tests :: [NamedTest]
tests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("renders the complete valid contract deterministically", testValidContractRendering),
    ("renders every typed-core outcome deterministically", testOutcomeRendering),
    ("accepts every fixed valid program", testValidPrograms),
    ("audits the fixed invalid fixture manifest", testInvalidFixtureManifest),
    ("reports every fixed invalid program exactly", testInvalidPrograms),
    ("audits the combined fixed fixture count", testCombinedFixtureCount),
    ("validates the complete fixture family deterministically", testValidationDeterminism),
    ("round-trips canonical validation failures through the checked adapter", testCheckedValidationAdapterRoundTrip),
    ("matches Haskell validation for all 44 Jazz fixtures twice", testJazzValidationParity)
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

testValidPrograms :: IO ()
testValidPrograms =
  mapM_
    (\fixture -> assertEqual (validFixtureName fixture <> " valid failures") [] (validateTypedProgram (validFixtureProgram fixture)))
    validFixtures

testInvalidFixtureManifest :: IO ()
testInvalidFixtureManifest = do
  assertEqual "invalid fixture names" expectedInvalidFixtureNames (map invalidFixtureName invalidFixtures)
  assertEqual "invalid fixture count" 28 (length invalidFixtures)

testInvalidPrograms :: IO ()
testInvalidPrograms =
  mapM_
    ( \fixture ->
        assertEqual
          (invalidFixtureName fixture <> " invalid failures")
          (invalidFixtureFailures fixture)
          (validateTypedProgram (invalidFixtureProgram fixture))
    )
    invalidFixtures

testCombinedFixtureCount :: IO ()
testCombinedFixtureCount =
  assertEqual "combined fixture count" 44 (length validFixtures + length invalidFixtures)

testValidationDeterminism :: IO ()
testValidationDeterminism = do
  let programs = map validFixtureProgram validFixtures <> map invalidFixtureProgram invalidFixtures
      first = map validateTypedProgram programs
      second = map validateTypedProgram programs
  assertEqual "complete validation output" first second

testCheckedValidationAdapterRoundTrip :: IO ()
testCheckedValidationAdapterRoundTrip =
  mapM_
    ( \fixture ->
        assertEqual
          (invalidFixtureName fixture <> " checked validation round-trip")
          (Right (invalidFixtureFailures fixture))
          (decodeCanonicalTypedValidationFailuresRuntimeValue (canonicalTypedValidationFailuresRuntimeValue (invalidFixtureFailures fixture)))
    )
    invalidFixtures

testJazzValidationParity :: IO ()
testJazzValidationParity = do
  let programs = map validFixtureProgram validFixtures <> map invalidFixtureProgram invalidFixtures
      expected =
        renderRuntimeValue
          ( VList
              [ VTuple
                  [ canonicalTypedProgramRuntimeValue program,
                    canonicalTypedValidationFailuresRuntimeValue (validateTypedProgram program)
                  ]
                | program <- programs
              ]
              Nothing
          )
  first <- runJazzValidationBatch programs
  second <- runJazzValidationBatch programs
  assertJazzOutput "Jazz validation first run" expected first
  assertJazzOutput "Jazz validation second run" expected second
  assertEqual "Jazz validation deterministic output" (runOutput first) (runOutput second)

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

runJazzValidationBatch :: [TypedProgram] -> IO RunResult
runJazzValidationBatch programs =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just (jazzValidationBatchSource programs))
        _ -> readCheckedInJazzProjectModuleSource sourcePath

jazzValidationBatchSource :: [TypedProgram] -> Text
jazzValidationBatchSource programs =
  Text.unlines
    [ "module App::Main {",
      "  import List (listMap).",
      "  import Maybe.",
      "  import TypedCoreTypes.",
      "  import TypedCoreValidate (validateProgram).",
      "  listMap",
      "    (\\(program) -> (program, validateProgram program))",
      "    [" <> Text.intercalate ", " (map (renderJazzRuntimeValue . canonicalTypedProgramRuntimeValue) programs) <> "].",
      "}",
      ""
    ]

assertJazzOutput :: Text -> Text -> RunResult -> IO ()
assertJazzOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

renderJazzRuntimeValue :: RuntimeValue -> Text
renderJazzRuntimeValue value =
  case value of
    VInt integer _
      | integer < 0 -> "(0 - " <> Text.pack (show (abs integer)) <> ")"
      | otherwise -> renderRuntimeValue value
    VBool {} -> renderRuntimeValue value
    VChar {} -> renderRuntimeValue value
    VText {} -> renderRuntimeValue value
    VList elements _ -> "[" <> Text.intercalate ", " (map renderJazzRuntimeValue elements) <> "]"
    VTuple elements -> "(" <> Text.intercalate ", " (map renderJazzRuntimeValue elements) <> ")"
    VConstructor _ _ constructorName _ arguments ->
      case arguments of
        [] -> identifierText constructorName
        _ -> "(" <> identifierText constructorName <> " " <> Text.intercalate " " (map renderJazzRuntimeValue arguments) <> ")"
    _ -> error "unsupported runtime value in generated typed-core fixture"

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
    boolInfo
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
  programWith fixture [TypedSignatureStatement owner name span1 scheme, expressionStatement 2 expression] emptyInterface boolInfo
  where
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder ["Fixture", fixture] [0] name
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument (TypedTypeParameterId 0) TypedBoolType]
        explicitSpan
    parameterId = TypedTypeParameterId 0
    scheme =
      TypedScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameterId) (TypedTypeParameterType parameterId))
        (TypedClosureRecipe [TypedRepresentationParameterRecipe parameterId] (TypedRepresentationParameterRecipe parameterId))
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
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint candidates])
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
    "application-function-shape",
    "application-argument-type",
    "application-result-type",
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
    applicationFunctionShapeFixture,
    applicationArgumentTypeFixture,
    applicationResultTypeFixture,
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
    (TypedVariableExpr boolInfo unresolvedName)
    [expressionFailure "unresolved-source-name" TypedUnresolvedName (TypedNameDetail unresolvedName)]
  where
    unresolvedName = TypedUnresolvedSourceName "missing"

unresolvedQualifiedNameFixture :: InvalidFixture
unresolvedQualifiedNameFixture =
  expressionFixture
    "unresolved-qualified-name"
    (TypedVariableExpr boolInfo unresolvedName)
    [expressionFailure "unresolved-qualified-name" TypedUnresolvedName (TypedNameDetail unresolvedName)]
  where
    unresolvedName = TypedUnresolvedQualifiedName "Missing" "value"

absoluteSourcePathFixture :: InvalidFixture
absoluteSourcePathFixture =
  InvalidFixture
    fixture
    (singleModuleProgram fixture (TypedSourcePath "/absolute/Main.jz") [] [] emptyInterface boolInfo ["Fixture", fixture])
    [moduleFailure fixture TypedInvalidSourcePath (TypedTextDetail "/absolute/Main.jz")]
  where
    fixture = "absolute-source-path"

duplicateModulePathFixture :: InvalidFixture
duplicateModulePathFixture =
  InvalidFixture
    fixture
    (TypedProgram Nothing [moduleValue, moduleValue] modulePath)
    [TypedCoreValidationFailure (TypedModulePath modulePath) TypedDuplicateModule (TypedTextDetail "Fixture::duplicate-module-path")]
  where
    fixture = "duplicate-module-path"
    modulePath = ["Fixture", fixture]
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
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = monoScheme valueBinder
    statement = TypedLetStatement valueBinder valueName span1 scheme trueExpr
    program = singleModuleProgram fixture relativeSource [] [statement, statement] emptyInterface boolInfo ["Fixture", fixture]

unknownBinderFixture :: InvalidFixture
unknownBinderFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedUnknownBinder (TypedBinderDetail schemeBinder)]
  where
    fixture = "unknown-binder"
    valueName = fixtureValueName "value"
    statementBinder = fixtureBinder fixture 0 valueName
    schemeBinder = fixtureBinder fixture 1 valueName
    statement = TypedLetStatement statementBinder valueName span1 (monoScheme schemeBinder) trueExpr
    program = singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo ["Fixture", fixture]

duplicateTypeParameterFixture :: InvalidFixture
duplicateTypeParameterFixture =
  InvalidFixture fixture program failures
  where
    fixture = "duplicate-or-noncanonical-type-parameter"
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    parameters = [TypedTypeParameterId 0, TypedTypeParameterId 0, TypedTypeParameterId 3]
    scheme = TypedScheme valueBinder parameters [] [] TypedBoolType TypedBoolRecipe
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
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    parameterId = TypedTypeParameterId 0
    scheme = TypedScheme valueBinder [] [] [] (TypedTypeParameterType parameterId) TypedBoolRecipe
    program = signatureProgram fixture valueBinder valueName scheme

freeRepresentationParameterFixture :: InvalidFixture
freeRepresentationParameterFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedUnboundRepresentationParameter (TypedTypeParameterDetail parameterId)]
  where
    fixture = "free-representation-parameter"
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    parameterId = TypedTypeParameterId 0
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType (TypedRepresentationParameterRecipe parameterId)
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
    program = singleModuleProgram fixture relativeSource [] [TypedDataStatement declaration] emptyInterface boolInfo ["Fixture", fixture]

callableRecipeSignatureFixture :: InvalidFixture
callableRecipeSignatureFixture =
  InvalidFixture fixture program [statementFailure fixture 0 TypedCallableRecipeMismatch (TypedRecipeDetail expectedRecipe actualRecipe)]
  where
    fixture = "callable-recipe-signature"
    valueName = fixtureValueName "callable"
    valueBinder = fixtureBinder fixture 0 valueName
    expectedRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    actualRecipe = TypedClosureRecipe [TypedCharRecipe] TypedBoolRecipe
    scheme = TypedScheme valueBinder [] [] [] (TypedFunctionType TypedBoolType TypedBoolType) actualRecipe
    program = signatureProgram fixture valueBinder valueName scheme

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
    functionExpr = TypedVariableExpr boolToBoolInfo (fixtureValueName "function")
    argumentExpr = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedApplyExpr boolInfo functionExpr argumentExpr

applicationResultTypeFixture :: InvalidFixture
applicationResultTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
  where
    fixture = "application-result-type"
    functionExpr = TypedVariableExpr boolToBoolInfo (fixtureValueName "function")
    expression = TypedApplyExpr textInfo functionExpr trueExpr

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
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    evidence =
      [ TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
        TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
        TypedEvidenceParameter (TypedEvidenceParameterId 3) constraint
      ]
    scheme = TypedScheme valueBinder [] evidence [] TypedBoolType TypedBoolRecipe
    program = signatureProgram fixture valueBinder valueName scheme
    failures =
      [ statementFailure fixture 0 TypedDuplicateEvidenceParameter (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
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
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) unknownName

missingOrDuplicateEvidenceFixture :: InvalidFixture
missingOrDuplicateEvidenceFixture =
  InvalidFixture fixture program failures
  where
    fixture = "missing-or-duplicate-evidence"
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    use = TypedEvidenceUse (Just (TypedEvidenceParameterId 0)) constraint implId Nothing
    missingExpression =
      TypedVariableExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint []])
        (fixtureValueName "missing")
    duplicateExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (fixtureValueName "duplicate")
    program = singleModuleProgram fixture relativeSource [] [expressionStatement 1 missingExpression, expressionStatement 2 duplicateExpression] emptyInterface boolInfo ["Fixture", fixture]
    failures =
      [ expressionFailureAt fixture 0 TypedMissingEvidence (TypedTextDetail "Equal"),
        expressionFailureAt fixture 1 TypedDuplicateEvidence (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
      ]

ambiguousOrInvisibleEvidenceFixture :: InvalidFixture
ambiguousOrInvisibleEvidenceFixture =
  InvalidFixture fixture program failures
  where
    fixture = "ambiguous-or-invisible-evidence"
    constraint = TypedCapabilityConstraint "Render" Nothing TypedTextType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId ["Library", "Render"] capabilityName [TypedTextType]
    invisibleImpl = TypedImplId ["Hidden", "Render"] capabilityName [TypedTextType]
    ambiguousExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedEvidenceCandidates constraint [TypedEvidenceCandidate firstImpl Nothing, TypedEvidenceCandidate secondImpl Nothing]])
        (fixtureValueName "ambiguous")
    invisibleUse = TypedEvidenceUse Nothing constraint invisibleImpl Nothing
    invisibleExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedSelectedEvidence invisibleUse])
        (fixtureValueName "invisible")
    program = singleModuleProgram fixture relativeSource [] [expressionStatement 1 ambiguousExpression, expressionStatement 2 invisibleExpression] emptyInterface textInfo ["Fixture", fixture]
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
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    otherImpl = TypedImplId ["Prelude"] capabilityName [TypedCharType]
    mismatchedMethod = TypedMethodId otherImpl "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just mismatchedMethod)
    expression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (fixtureValueName "equal")
    valueName = fixtureValueName "published"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme = monoScheme valueBinder
    statement = TypedLetStatement valueBinder valueName span1 scheme trueExpr
    program =
      singleModuleProgram
        fixture
        relativeSource
        [TypedModuleExport TypedValueNamespace "published"]
        [expressionStatement 1 expression, statement]
        emptyInterface
        boolInfo
        ["Fixture", fixture]
    failures =
      [ TypedCoreValidationFailure (TypedInterfacePath ["Fixture", fixture]) TypedModuleInterfaceMismatch (TypedNameDetail valueName),
        expressionFailureAt fixture 0 TypedMethodSelectionMismatch (TypedImplDetail otherImpl)
      ]

expressionFixture :: Text -> TypedExpr -> [TypedCoreValidationFailure] -> InvalidFixture
expressionFixture fixture expression failures =
  InvalidFixture
    fixture
    (singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo ["Fixture", fixture])
    failures

signatureProgram :: Text -> TypedBinderId -> TypedCoreName -> TypedScheme -> TypedProgram
signatureProgram fixture valueBinder valueName scheme =
  singleModuleProgram
    fixture
    relativeSource
    []
    [TypedSignatureStatement valueBinder valueName span1 scheme]
    emptyInterface
    boolInfo
    ["Fixture", fixture]

singleModuleProgram :: Text -> TypedSourcePath -> [TypedModuleExport] -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> [Text] -> TypedProgram
singleModuleProgram fixture sourcePath exports statements interface moduleInfo entryModule =
  TypedProgram
    Nothing
    [typedModule ["Fixture", fixture] sourcePath [] exports interface statements moduleInfo]
    entryModule

typedModule :: [Text] -> TypedSourcePath -> [TypedResolvedImport] -> [TypedModuleExport] -> TypedModuleInterface -> [TypedStatement] -> TypedNodeInfo -> TypedModule
typedModule = TypedModule

relativeSource :: TypedSourcePath
relativeSource = TypedSourcePath "src/Fixture/Main.jz"

fixtureValueName :: Text -> TypedCoreName
fixtureValueName = resolved TypedCurrentModule TypedValueNamespace

fixtureBinder :: Text -> Int -> TypedCoreName -> TypedBinderId
fixtureBinder fixture lexicalIndex = binder ["Fixture", fixture] [lexicalIndex]

monoScheme :: TypedBinderId -> TypedScheme
monoScheme valueBinder = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

boolToBoolType :: TypedType
boolToBoolType = TypedFunctionType TypedBoolType TypedBoolType

boolToBoolRecipe :: TypedRepresentationRecipe
boolToBoolRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe

boolToBoolInfo :: TypedNodeInfo
boolToBoolInfo = info boolToBoolType boolToBoolRecipe

moduleFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
moduleFailure fixture = TypedCoreValidationFailure (TypedModulePath ["Fixture", fixture])

statementFailure :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
statementFailure fixture statementIndex = TypedCoreValidationFailure (TypedStatementPath ["Fixture", fixture] statementIndex)

expressionFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailure fixture = expressionFailureAt fixture 0

expressionFailureAt :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailureAt fixture statementIndex =
  TypedCoreValidationFailure (TypedExpressionPath ["Fixture", fixture] statementIndex [0])

patternFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
patternFailure fixture =
  TypedCoreValidationFailure (TypedPatternPath ["Fixture", fixture] 0 [0])
