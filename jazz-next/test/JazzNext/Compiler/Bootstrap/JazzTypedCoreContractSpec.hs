{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedProgramRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    decodeCanonicalTypedValidationFailuresRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
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
    assertContains,
    assertEqual,
    failTest,
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
    ("rejects unknown validation constructors", testCheckedValidationAdapterUnknownConstructor),
    ("rejects wrong validation constructor arity", testCheckedValidationAdapterWrongArity),
    ("rejects wrong validation field categories", testCheckedValidationAdapterWrongFieldCategory),
    ("rejects malformed nested binder identities", testCheckedValidationAdapterMalformedBinder),
    ("rejects malformed nested impl identities", testCheckedValidationAdapterMalformedImpl),
    ("rejects host-specific name identities", testCheckedValidationAdapterHostName),
    ("rejects runtime values in structural fields", testCheckedValidationAdapterRuntimeValue),
    ("rejects absolute source-path constructors in structural fields", testCheckedValidationAdapterAbsoluteSourcePath),
    ("audits fixture uniqueness and complete validation-kind coverage", testFixtureCoverage),
    ("rejects malformed nested block contracts at unique statement paths", testNestedBlockValidationRegressions),
    ("enforces typed-core scope and visibility contracts", testScopeAndVisibilityRegressions),
    ("enforces typed-core value-shape contracts", testValueShapeRegressions),
    ("enforces follow-up typed-core boundary contracts", testReviewFollowupRegressions),
    ("enforces latest typed-core review contracts", testLatestReviewRegressions),
    ("matches Haskell validation for every fixed and review fixture twice", testJazzValidationParity)
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

testCheckedValidationAdapterUnknownConstructor :: IO ()
testCheckedValidationAdapterUnknownConstructor =
  assertTextLeftContains
    "unknown validation constructor"
    "validation failure expected constructor 'TypedCoreValidationFailure', got 'UnexpectedFailure'"
    (decodeCanonicalTypedValidationFailuresRuntimeValue (VList [canonicalNullaryConstructor "UnexpectedFailure"] Nothing))

testCheckedValidationAdapterWrongArity :: IO ()
testCheckedValidationAdapterWrongArity =
  assertTextLeftContains
    "wrong validation failure arity"
    "TypedCoreValidationFailure expected 3 field(s), got 2"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( VList
            [ canonicalConstructor
                "TypedCoreValidationFailure"
                [canonicalNullaryConstructor "TypedProgramPath", canonicalNullaryConstructor "TypedUnknownEntryModule"]
            ]
            Nothing
        )
    )

testCheckedValidationAdapterWrongFieldCategory :: IO ()
testCheckedValidationAdapterWrongFieldCategory =
  assertTextLeftContains
    "wrong validation path category"
    "validation path expected a constructor, got Text"
    (decodeCanonicalTypedValidationFailuresRuntimeValue (singleCanonicalFailure (VText "not-a-path") (canonicalNullaryConstructor "TypedUnknownEntryModule") (canonicalNullaryConstructor "TypedNoValidationDetail")))

testCheckedValidationAdapterMalformedBinder :: IO ()
testCheckedValidationAdapterMalformedBinder =
  assertTextLeftContains
    "malformed binder identity"
    "TypedBinderId expected 3 field(s), got 0"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( singleCanonicalFailure
            (canonicalNullaryConstructor "TypedProgramPath")
            (canonicalNullaryConstructor "TypedDuplicateBinder")
            (canonicalConstructor "TypedBinderDetail" [canonicalNullaryConstructor "TypedBinderId"])
        )
    )

testCheckedValidationAdapterMalformedImpl :: IO ()
testCheckedValidationAdapterMalformedImpl =
  assertTextLeftContains
    "malformed impl identity"
    "typed-core name expected a constructor, got Text"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( singleCanonicalFailure
            (canonicalNullaryConstructor "TypedProgramPath")
            (canonicalNullaryConstructor "TypedInvisibleImpl")
            ( canonicalConstructor
                "TypedImplDetail"
                [canonicalConstructor "TypedImplId" [VList [VText "Prelude"] Nothing, VText "host-capability", VList [] Nothing]]
            )
        )
    )

testCheckedValidationAdapterHostName :: IO ()
testCheckedValidationAdapterHostName =
  assertTextLeftContains
    "host-specific name identity"
    "typed-core name expected a constructor, got operator"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( singleCanonicalFailure
            (canonicalNullaryConstructor "TypedProgramPath")
            (canonicalNullaryConstructor "TypedUnresolvedName")
            (canonicalConstructor "TypedNameDetail" [VOperator "host-name" []])
        )
    )

testCheckedValidationAdapterRuntimeValue :: IO ()
testCheckedValidationAdapterRuntimeValue =
  assertTextLeftContains
    "runtime value structural field"
    "representation recipe expected a constructor, got left section"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( singleCanonicalFailure
            (canonicalNullaryConstructor "TypedProgramPath")
            (canonicalNullaryConstructor "TypedTypeRepresentationMismatch")
            (canonicalConstructor "TypedRecipeDetail" [canonicalNullaryConstructor "TypedBoolRecipe", VSectionLeft "+" (VBool True)])
        )
    )

testCheckedValidationAdapterAbsoluteSourcePath :: IO ()
testCheckedValidationAdapterAbsoluteSourcePath =
  assertTextLeftContains
    "absolute source path structural field"
    "unknown typed-core name constructor 'TypedSourcePath'"
    ( decodeCanonicalTypedValidationFailuresRuntimeValue
        ( singleCanonicalFailure
            (canonicalNullaryConstructor "TypedProgramPath")
            (canonicalNullaryConstructor "TypedUnresolvedName")
            (canonicalConstructor "TypedNameDetail" [canonicalConstructor "TypedSourcePath" [VText "/private/host/Main.jz"]])
        )
    )

singleCanonicalFailure :: RuntimeValue -> RuntimeValue -> RuntimeValue -> RuntimeValue
singleCanonicalFailure path kind detail =
  VList [canonicalConstructor "TypedCoreValidationFailure" [path, kind, detail]] Nothing

assertTextLeftContains :: (Show value) => Text -> Text -> Either Text value -> IO ()
assertTextLeftContains label expected result =
  case result of
    Left actual -> assertContains label expected actual
    Right value -> failTest (label <> ": expected Left, got Right " <> Text.pack (show value))

testFixtureCoverage :: IO ()
testFixtureCoverage = do
  let names = map validFixtureName validFixtures <> map invalidFixtureName invalidFixtures
      observedKinds =
        [kind | fixture <- invalidFixtures, TypedCoreValidationFailure _ kind _ <- invalidFixtureFailures fixture]
          <> [kind | program <- reviewRegressionPrograms, TypedCoreValidationFailure _ kind _ <- validateTypedProgram program]
  assertEqual "fixture names are unique" (length names) (length (nub names))
  assertEqual "uncovered validation kinds" [] (filter (`notElem` observedKinds) allValidationKinds)

allValidationKinds :: [TypedCoreValidationKind]
allValidationKinds = [minBound .. maxBound]

testJazzValidationParity :: IO ()
testJazzValidationParity = do
  let programs = map validFixtureProgram validFixtures <> map invalidFixtureProgram invalidFixtures <> reviewRegressionPrograms
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

testNestedBlockValidationRegressions :: IO ()
testNestedBlockValidationRegressions = do
  assertEqual
    "nested expression path is distinct from its containing block"
    nestedPathFailures
    (validateTypedProgram nestedPathProgram)
  assertEqual
    "block declarations receive complete validation"
    nestedDeclarationFailures
    (validateTypedProgram nestedDeclarationProgram)
  assertEqual
    "block-local binder identities remain unique"
    nestedDuplicateBinderFailures
    (validateTypedProgram nestedDuplicateBinderProgram)

reviewRegressionPrograms :: [TypedProgram]
reviewRegressionPrograms =
  [ nestedPathProgram,
    nestedDeclarationProgram,
    nestedDuplicateBinderProgram,
    guardedCasePathProgram,
    generalizedLetScopeProgram,
    importedInstantiationProgram,
    invisibleSiblingImplProgram,
    selectedEvidenceTargetProgram,
    invisibleVariableProgram,
    selectedMethodContractProgram,
    enclosingImplMethodProgram,
    bindingValueProgram,
    lambdaResultProgram,
    literalTypeProgram,
    collectionShapeProgram,
    dataTypeArityProgram,
    tuplePatternShapeProgram,
    moduleResultProgram,
    schemeDataTypeProgram,
    driveAbsoluteProgram,
    instantiationDataTypeProgram,
    literalPatternProgram,
    invisibleOperatorProgram,
    expressionDuplicateBinderProgram,
    privateInterfaceLeakProgram,
    constructorPatternContractProgram,
    nonListPatternProgram,
    explicitTypeApplicationContractProgram,
    variableSchemeContractProgram,
    missingImportProgram,
    candidateConstraintProgram,
    invalidVariableNamespaceProgram,
    binderNameContractProgram,
    blockLocalGeneralizedSchemeProgram,
    blockLocalMonomorphicSchemeProgram,
    implMethodNameProgram,
    blockResultProgram,
    nestedCasePatternPathProgram,
    operatorSchemeProgram,
    selectiveImportProgram,
    classParameterScopeProgram,
    evidenceParameterContractProgram,
    implCapabilityNamespaceProgram
  ]

nestedPathProgram :: TypedProgram
nestedPathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-path"
    modulePath = ["Fixture", fixture]
    nestedName = TypedUnresolvedSourceName "nested"
    block =
      TypedBlockExpr
        (info TypedBoolType (TypedSignedIntegerRecipe 64))
        [expressionStatement 2 (TypedVariableExpr boolInfo nestedName)]

nestedPathFailures :: [TypedCoreValidationFailure]
nestedPathFailures =
  [ TypedCoreValidationFailure
      (TypedExpressionPath ["Fixture", "review-nested-path"] 0 [0])
      TypedTypeRepresentationMismatch
      (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64)),
    TypedCoreValidationFailure
      (TypedExpressionPath ["Fixture", "review-nested-path"] 1 [0])
      TypedUnresolvedName
      (TypedNameDetail (TypedUnresolvedSourceName "nested"))
  ]

nestedDeclarationProgram :: TypedProgram
nestedDeclarationProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-declaration"
    modulePath = ["Fixture", fixture]
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorBinder = binder modulePath [0, 0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedSignedIntegerRecipe 64]]
    block = TypedBlockExpr boolInfo [TypedDataStatement declaration]

nestedDeclarationFailures :: [TypedCoreValidationFailure]
nestedDeclarationFailures =
  [ TypedCoreValidationFailure
      (TypedStatementPath ["Fixture", "review-nested-declaration"] 1)
      TypedDataRecipeMismatch
      (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64))
  ]

nestedDuplicateBinderProgram :: TypedProgram
nestedDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-duplicate-binder"
    modulePath = ["Fixture", fixture]
    name = resolved TypedCurrentModule TypedValueNamespace "duplicate"
    duplicateBinder = binder modulePath [0, 0] name
    scheme = monoScheme duplicateBinder
    block =
      TypedBlockExpr
        boolInfo
        [ TypedLetStatement duplicateBinder name span1 scheme trueExpr,
          TypedLetStatement duplicateBinder name span1 scheme trueExpr
        ]

nestedDuplicateBinderFailures :: [TypedCoreValidationFailure]
nestedDuplicateBinderFailures =
  [ TypedCoreValidationFailure
      (TypedStatementPath ["Fixture", "review-nested-duplicate-binder"] 2)
      TypedDuplicateBinder
      (TypedBinderDetail (binder ["Fixture", "review-nested-duplicate-binder"] [0, 0] (resolved TypedCurrentModule TypedValueNamespace "duplicate")))
  ]

guardedCasePathProgram :: TypedProgram
guardedCasePathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo modulePath
  where
    fixture = "review-guarded-case-path"
    modulePath = ["Fixture", fixture]
    unresolved name = TypedVariableExpr boolInfo (TypedUnresolvedSourceName name)
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm (TypedWildcardPattern boolInfo) (Just (unresolved "guard")) (unresolved "first-result"),
          TypedCaseArm (TypedWildcardPattern boolInfo) Nothing (unresolved "second-result")
        ]

testScopeAndVisibilityRegressions :: IO ()
testScopeAndVisibilityRegressions = do
  assertEqual "generalized let body uses its scheme scope" [] (validateTypedProgram generalizedLetScopeProgram)
  assertEqual "imported schemes support instantiation" [] (validateTypedProgram importedInstantiationProgram)
  assertEqual
    "sibling impls are not visible without an import"
    [ expressionFailureAt "review-invisible-sibling-impl" 1 TypedInvisibleImpl (TypedImplDetail invisibleSiblingImplId)
    ]
    (validateTypedProgram invisibleSiblingImplProgram)
  assertEqual
    "selected evidence targets its own constraint"
    [ expressionFailureAt "review-selected-evidence-target" 2 TypedMethodSelectionMismatch (TypedTypeDetail TypedBoolType TypedCharType)
    ]
    (validateTypedProgram selectedEvidenceTargetProgram)
  assertEqual
    "resolved variables still require visible definitions"
    [ expressionFailure "review-invisible-variable" TypedInvisibleName (TypedNameDetail invisibleVariableName)
    ]
    (validateTypedProgram invisibleVariableProgram)
  assertEqual
    "selected methods match the capability method contract"
    [ expressionFailureAt "review-selected-method-contract" 2 TypedMethodSelectionMismatch (TypedTextDetail "Equal.equal"),
      expressionFailureAt "review-selected-method-contract" 3 TypedMethodSelectionMismatch (TypedTextDetail "Equal.equal")
    ]
    (validateTypedProgram selectedMethodContractProgram)
  assertEqual
    "impl method ids retain their enclosing impl identity"
    [ statementFailure "review-enclosing-impl-method" 0 TypedMethodSelectionMismatch (TypedImplDetail enclosingOtherImplId)
    ]
    (validateTypedProgram enclosingImplMethodProgram)

generalizedLetScopeProgram :: TypedProgram
generalizedLetScopeProgram =
  singleModuleProgram fixture relativeSource [TypedModuleExport TypedValueNamespace "identity"] [statement] interface boolInfo modulePath
  where
    fixture = "review-generalized-let-scope"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    valueBinder = binder modulePath [0] valueName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    functionInfo = info functionType functionRecipe
    scheme = TypedScheme valueBinder [parameterId] [] [] functionType functionRecipe
    expression = TypedLambdaExpr functionInfo argumentBinder argumentName (TypedVariableExpr (info parameterType parameterRecipe) argumentName)
    statement = TypedLetStatement valueBinder valueName span1 scheme expression
    interface = TypedModuleInterface [TypedValueInterface valueName scheme] [] [] []

importedInstantiationProgram :: TypedProgram
importedInstantiationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "Identity"]
    entryPath = ["Fixture", "review-imported-instantiation"]
    localName = resolved TypedCurrentModule TypedValueNamespace "identity"
    importedName = resolved (TypedImportedModule libraryPath) TypedValueNamespace "identity"
    owner = binder libraryPath [0] localName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    scheme =
      TypedScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/Identity.jz")
        []
        [TypedModuleExport TypedValueNamespace "identity"]
        (TypedModuleInterface [TypedValueInterface localName scheme] [] [] [])
        [TypedSignatureStatement owner localName span1 scheme]
        boolInfo
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) importedName
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["identity"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

invisibleSiblingImplId :: TypedImplId
invisibleSiblingImplId =
  TypedImplId ["Hidden", "Evidence"] (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal") [TypedBoolType]

invisibleSiblingImplProgram :: TypedProgram
invisibleSiblingImplProgram = TypedProgram Nothing [hiddenModule, entryModule] entryPath
  where
    fixture = "review-invisible-sibling-impl"
    hiddenPath = ["Hidden", "Evidence"]
    entryPath = ["Fixture", fixture]
    hiddenDeclaration = TypedImplDeclaration span1 invisibleSiblingImplId []
    hiddenModule =
      typedModule
        hiddenPath
        (TypedSourcePath "src/Hidden/Evidence.jz")
        []
        []
        (TypedModuleInterface [] [] [] [TypedImplInterface invisibleSiblingImplId])
        [TypedImplStatement hiddenDeclaration]
        boolInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder entryPath [0] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint invisibleSiblingImplId Nothing
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [TypedSignatureStatement valueBinder valueName span1 scheme, expressionStatement 1 expression]
        boolInfo

selectedEvidenceTargetProgram :: TypedProgram
selectedEvidenceTargetProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-selected-evidence-target"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedCharType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint implId Nothing
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        TypedSignatureStatement valueBinder valueName span1 scheme,
        expressionStatement 1 expression
      ]

invisibleVariableName :: TypedCoreName
invisibleVariableName = resolved TypedCurrentModule TypedValueNamespace "missing"

invisibleVariableProgram :: TypedProgram
invisibleVariableProgram =
  expressionFixtureProgram "review-invisible-variable" (TypedVariableExpr boolInfo invisibleVariableName)

selectedMethodContractProgram :: TypedProgram
selectedMethodContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-selected-method-contract"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "equal"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    withoutMethod = TypedEvidenceUse Nothing constraint implId Nothing
    wrongMethod = TypedEvidenceUse Nothing constraint implId (Just (TypedMethodId implId "other"))
    selected evidence = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        TypedSignatureStatement valueBinder valueName span1 scheme,
        expressionStatement 1 (selected withoutMethod),
        expressionStatement 2 (selected wrongMethod)
      ]

enclosingOtherImplId :: TypedImplId
enclosingOtherImplId =
  TypedImplId ["Fixture", "review-enclosing-impl-method"] (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render") [TypedCharType]

enclosingImplMethodProgram :: TypedProgram
enclosingImplMethodProgram =
  singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-enclosing-impl-method"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] methodName
    method = TypedMethodDefinition (TypedMethodId enclosingOtherImplId "render") methodBinder methodName span1 trueExpr
    declaration = TypedImplDeclaration span1 implId [method]

testValueShapeRegressions :: IO ()
testValueShapeRegressions = do
  assertEqual
    "let RHS matches its published scheme"
    [statementFailure "review-binding-value" 0 TypedBindingValueMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
    (validateTypedProgram bindingValueProgram)
  assertEqual
    "lambda body matches its annotated result"
    [expressionFailure "review-lambda-result" TypedLambdaResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
    (validateTypedProgram lambdaResultProgram)
  assertEqual
    "literal payload matches its annotated type"
    [expressionFailure "review-literal-type" TypedLiteralTypeMismatch (TypedTypeDetail TypedTextType TypedBoolType)]
    (validateTypedProgram literalTypeProgram)
  assertEqual
    "collection children match their parent shape"
    [expressionFailure "review-collection-shape" TypedCollectionShapeMismatch (TypedTypeDetail TypedBoolType TypedCharType)]
    (validateTypedProgram collectionShapeProgram)
  assertEqual
    "data type applications match visible declaration arity"
    [expressionFailureAt "review-data-type-arity" 1 TypedDataTypeMismatch (TypedArityDetail 1 0)]
    (validateTypedProgram dataTypeArityProgram)
  assertEqual
    "tuple pattern arity is exact"
    [patternFailure "review-tuple-pattern-shape" TypedPatternShapeMismatch (TypedArityDetail 2 1)]
    (validateTypedProgram tuplePatternShapeProgram)
  assertEqual
    "module result matches its terminal expression"
    [moduleFailure "review-module-result" TypedModuleResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
    (validateTypedProgram moduleResultProgram)
  assertEqual
    "scheme data types require visible declarations"
    [statementFailure "review-scheme-data-type" 0 TypedDataTypeMismatch (TypedNameDetail missingSchemeDataName)]
    (validateTypedProgram schemeDataTypeProgram)
  assertEqual
    "drive-absolute source paths are rejected consistently"
    [moduleFailure "review-drive-absolute" TypedInvalidSourcePath (TypedTextDetail "C:/Fixture/Main.jz")]
    (validateTypedProgram driveAbsoluteProgram)

bindingValueProgram :: TypedProgram
bindingValueProgram =
  singleModuleProgram fixture relativeSource [] [TypedLetStatement valueBinder valueName span1 (monoScheme valueBinder) value] emptyInterface boolInfo modulePath
  where
    fixture = "review-binding-value"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    valueBinder = binder modulePath [0] valueName
    value = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "wrong")

lambdaResultProgram :: TypedProgram
lambdaResultProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-lambda-result"
    modulePath = ["Fixture", fixture]
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    expression =
      TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath [0, 0] argumentName)
        argumentName
        (literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "wrong"))

literalTypeProgram :: TypedProgram
literalTypeProgram = expressionFixtureProgram "review-literal-type" (TypedLiteralExpr boolInfo (TypedTextLiteral "wrong"))

collectionShapeProgram :: TypedProgram
collectionShapeProgram =
  expressionFixtureProgram
    "review-collection-shape"
    (TypedListExpr boolListInfo [literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')])

dataTypeArityProgram :: TypedProgram
dataTypeArityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface dataInfo modulePath
  where
    fixture = "review-data-type-arity"
    modulePath = ["Fixture", fixture]
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration = TypedDataDeclaration span1 dataName [TypedTypeParameterId 0] []
    dataInfo = info (TypedDataType dataName []) (TypedManagedVariantRecipe dataName [])
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (TypedVariableExpr dataInfo (TypedBuiltinName "box"))
      ]

tuplePatternShapeProgram :: TypedProgram
tuplePatternShapeProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-tuple-pattern-shape"
    expression =
      TypedPatternCaseExpr
        boolInfo
        (TypedTupleExpr pairInfo [trueExpr, falseExpr])
        [TypedCaseArm (TypedTuplePattern pairInfo [TypedWildcardPattern boolInfo]) Nothing trueExpr]

moduleResultProgram :: TypedProgram
moduleResultProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 terminal] emptyInterface boolInfo ["Fixture", fixture]
  where
    fixture = "review-module-result"
    terminal = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "result")

missingSchemeDataName :: TypedCoreName
missingSchemeDataName = resolved TypedCurrentModule TypedTypeNamespace "Missing"

schemeDataTypeProgram :: TypedProgram
schemeDataTypeProgram =
  singleModuleProgram fixture relativeSource [] [TypedSignatureStatement valueBinder valueName span1 scheme] emptyInterface boolInfo modulePath
  where
    fixture = "review-scheme-data-type"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    valueBinder = binder modulePath [0] valueName
    resultType = TypedDataType missingSchemeDataName []
    scheme = TypedScheme valueBinder [] [] [] resultType (TypedManagedVariantRecipe missingSchemeDataName [])

driveAbsoluteProgram :: TypedProgram
driveAbsoluteProgram =
  singleModuleProgram fixture (TypedSourcePath "C:/Fixture/Main.jz") [] [] emptyInterface boolInfo ["Fixture", fixture]
  where
    fixture = "review-drive-absolute"

testReviewFollowupRegressions :: IO ()
testReviewFollowupRegressions = do
  assertEqual
    "instantiation arguments require visible data declarations"
    [expressionFailureAt "review-instantiation-data-type" 1 TypedDataTypeMismatch (TypedNameDetail missingInstantiationDataName)]
    (validateTypedProgram instantiationDataTypeProgram)
  assertEqual
    "literal patterns match their annotated payload kind"
    [patternFailure "review-literal-pattern" TypedLiteralTypeMismatch (TypedTypeDetail TypedTextType TypedBoolType)]
    (validateTypedProgram literalPatternProgram)
  assertEqual
    "resolved operators require visible value bindings"
    [expressionFailure "review-invisible-operator" TypedInvisibleName (TypedNameDetail invisibleOperatorName)]
    (validateTypedProgram invisibleOperatorProgram)
  assertEqual
    "expression binders participate in module-wide duplicate checks"
    [ expressionFailureAt
        "review-expression-duplicate-binder"
        1
        TypedDuplicateBinder
        (TypedBinderDetail expressionDuplicateBinder)
    ]
    (validateTypedProgram expressionDuplicateBinderProgram)
  assertEqual
    "module interfaces cannot publish unexported values"
    [ TypedCoreValidationFailure
        (TypedInterfacePath privateInterfaceLibraryPath)
        TypedModuleInterfaceMismatch
        (TypedNameDetail privateInterfaceLocalName),
      TypedCoreValidationFailure
        (TypedExpressionPath privateInterfaceEntryPath 0 [0])
        TypedInvisibleName
        (TypedNameDetail privateInterfaceImportedName)
    ]
    (validateTypedProgram privateInterfaceLeakProgram)
  assertEqual
    "constructor patterns match declared field arity and types"
    [ TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-constructor-pattern-contract"] 1 [0, 0])
        TypedPatternShapeMismatch
        (TypedArityDetail 1 0),
      TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-constructor-pattern-contract"] 1 [0, 1, 0])
        TypedPatternScrutineeMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram constructorPatternContractProgram)
  assertEqual
    "list patterns require list scrutinees"
    [patternFailure "review-non-list-pattern" TypedPatternShapeMismatch (TypedTypeDetail (TypedListType TypedBoolType) TypedBoolType)]
    (validateTypedProgram nonListPatternProgram)
  assertEqual
    "explicit type applications require a matching generalized instantiation"
    [ expressionFailureAt
        "review-explicit-type-application-contract"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail explicitTypeApplicationOwner)
    ]
    (validateTypedProgram explicitTypeApplicationContractProgram)
  assertEqual
    "monomorphic variable nodes match their binding schemes"
    [ expressionFailureAt
        "review-variable-scheme-contract"
        1
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram variableSchemeContractProgram)
  assertEqual
    "resolved imports require modules present in the program"
    [moduleFailure "review-missing-import" TypedModuleInterfaceMismatch (TypedTextDetail "Missing::Library")]
    (validateTypedProgram missingImportProgram)
  assertEqual
    "candidate evidence matches capability and method constraints"
    [ expressionFailureAt "review-candidate-constraint" 2 TypedMethodSelectionMismatch (TypedTextDetail "Equal"),
      expressionFailureAt "review-candidate-constraint" 2 TypedMethodSelectionMismatch (TypedTextDetail "Equal.equal"),
      expressionFailureAt "review-candidate-constraint" 3 TypedMethodSelectionMismatch (TypedTextDetail "Equal.equal")
    ]
    (validateTypedProgram candidateConstraintProgram)
  assertEqual
    "variable expressions reject non-value-producing namespaces"
    [expressionFailureAt "review-variable-namespace" 1 TypedInvisibleName (TypedNameDetail invalidVariableNamespaceName)]
    (validateTypedProgram invalidVariableNamespaceProgram)

testLatestReviewRegressions :: IO ()
testLatestReviewRegressions = do
  assertEqual
    "declaration binder ids match their published names"
    [statementFailure "review-binder-name-contract" 0 TypedUnknownBinder (TypedBinderDetail binderNameContractBinder)]
    (validateTypedProgram binderNameContractProgram)
  assertEqual
    "block-local generalized schemes support instantiation"
    []
    (validateTypedProgram blockLocalGeneralizedSchemeProgram)
  assertEqual
    "block-local monomorphic schemes constrain later uses"
    [ expressionFailureAt
        "review-block-local-monomorphic-scheme"
        2
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram blockLocalMonomorphicSchemeProgram)
  assertEqual
    "impl method ids match their published method names"
    [statementFailure "review-impl-method-name" 0 TypedMethodSelectionMismatch (TypedTextDetail "equal")]
    (validateTypedProgram implMethodNameProgram)
  assertEqual
    "block results match terminal expression contracts"
    [ expressionFailure
        "review-block-result"
        TypedBlockResultMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram blockResultProgram)
  assertEqual
    "nested case patterns retain their containing expression paths"
    [ TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-nested-case-pattern-path"] 0 [0, 1, 0])
        TypedLiteralTypeMismatch
        (TypedTypeDetail TypedTextType TypedBoolType)
    ]
    (validateTypedProgram nestedCasePatternPathProgram)
  assertEqual
    "resolved operator uses match their published schemes"
    operatorSchemeFailures
    (validateTypedProgram operatorSchemeProgram)
  assertEqual
    "selective imports name exported interface entries"
    [moduleFailure "review-selective-import" TypedModuleInterfaceMismatch (TypedTextDetail "missing")]
    (validateTypedProgram selectiveImportProgram)
  assertEqual
    "class parameters remain in method scheme scope"
    []
    (validateTypedProgram classParameterScopeProgram)
  assertEqual
    "selected evidence matches instantiated scheme parameters"
    [ expressionFailureAt
        "review-evidence-parameter-contract"
        1
        TypedInstantiationMismatch
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 7)),
      expressionFailureAt
        "review-evidence-parameter-contract"
        2
        TypedInstantiationMismatch
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram evidenceParameterContractProgram)
  assertEqual
    "impl capabilities use visible capability names"
    [ statementFailure "review-impl-capability-namespace" 0 TypedInvisibleName (TypedNameDetail invalidImplCapabilityName),
      statementFailure "review-impl-capability-namespace" 1 TypedInvisibleName (TypedNameDetail invisibleImplCapabilityName)
    ]
    (validateTypedProgram implCapabilityNamespaceProgram)

missingInstantiationDataName :: TypedCoreName
missingInstantiationDataName = resolved TypedCurrentModule TypedTypeNamespace "Missing"

instantiationDataTypeProgram :: TypedProgram
instantiationDataTypeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-instantiation-data-type"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "phantom"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = TypedScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId (TypedDataType missingInstantiationDataName [])] Nothing
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    statements = [TypedSignatureStatement owner valueName span1 scheme, expressionStatement 2 expression]

literalPatternProgram :: TypedProgram
literalPatternProgram =
  expressionFixtureProgram
    "review-literal-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedLiteralPattern boolInfo (TypedTextLiteral "wrong")) Nothing falseExpr]
    )

invisibleOperatorName :: TypedCoreName
invisibleOperatorName = resolved (TypedImportedModule ["Hidden", "Operators"]) TypedValueNamespace "plus"

invisibleOperatorProgram :: TypedProgram
invisibleOperatorProgram = TypedProgram Nothing [hiddenModule, entryModule] entryPath
  where
    hiddenPath = ["Hidden", "Operators"]
    entryPath = ["Fixture", "review-invisible-operator"]
    localName = resolved TypedCurrentModule TypedValueNamespace "plus"
    localBinder = binder hiddenPath [0] localName
    scheme = monoScheme localBinder
    hiddenModule =
      typedModule
        hiddenPath
        (TypedSourcePath "src/Hidden/Operators.jz")
        []
        [TypedModuleExport TypedValueNamespace "plus"]
        (TypedModuleInterface [TypedValueInterface localName scheme] [] [] [])
        [TypedSignatureStatement localBinder localName span1 scheme]
        boolInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 (TypedOperatorValueExpr boolInfo (TypedResolvedOperator invisibleOperatorName "+"))]
        boolInfo

expressionDuplicateBinder :: TypedBinderId
expressionDuplicateBinder =
  binder
    ["Fixture", "review-expression-duplicate-binder"]
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "value")

expressionDuplicateBinderProgram :: TypedProgram
expressionDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-expression-duplicate-binder"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    scheme = monoScheme expressionDuplicateBinder
    lambda = TypedLambdaExpr boolToBoolInfo expressionDuplicateBinder valueName (TypedVariableExpr boolInfo valueName)
    statements = [TypedSignatureStatement expressionDuplicateBinder valueName span1 scheme, expressionStatement 2 lambda]

privateInterfaceLibraryPath :: [Text]
privateInterfaceLibraryPath = ["Private", "Library"]

privateInterfaceEntryPath :: [Text]
privateInterfaceEntryPath = ["Fixture", "review-private-interface"]

privateInterfaceLocalName :: TypedCoreName
privateInterfaceLocalName = resolved TypedCurrentModule TypedValueNamespace "secret"

privateInterfaceImportedName :: TypedCoreName
privateInterfaceImportedName = resolved (TypedImportedModule privateInterfaceLibraryPath) TypedValueNamespace "secret"

privateInterfaceLeakProgram :: TypedProgram
privateInterfaceLeakProgram = TypedProgram Nothing [libraryModule, entryModule] privateInterfaceEntryPath
  where
    owner = binder privateInterfaceLibraryPath [0] privateInterfaceLocalName
    scheme = monoScheme owner
    libraryModule =
      typedModule
        privateInterfaceLibraryPath
        (TypedSourcePath "src/Private/Library.jz")
        []
        []
        (TypedModuleInterface [TypedValueInterface privateInterfaceLocalName scheme] [] [] [])
        [TypedSignatureStatement owner privateInterfaceLocalName span1 scheme]
        boolInfo
    entryModule =
      typedModule
        privateInterfaceEntryPath
        relativeSource
        [TypedResolvedImport span1 privateInterfaceLibraryPath Nothing Nothing]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr boolInfo privateInterfaceImportedName)]
        boolInfo

constructorPatternContractProgram :: TypedProgram
constructorPatternContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-constructor-pattern-contract"
    modulePath = ["Fixture", fixture]
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    declaration =
      TypedDataDeclaration
        span1
        optionName
        [parameterId]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] someName)
            someName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
    scrutinee = TypedVariableExpr optionInfo someName
    expression =
      TypedPatternCaseExpr
        boolInfo
        scrutinee
        [ TypedCaseArm (TypedConstructorPattern optionInfo someName []) Nothing trueExpr,
          TypedCaseArm (TypedConstructorPattern optionInfo someName [TypedWildcardPattern textInfo]) Nothing falseExpr
        ]
    statements = [TypedDataStatement declaration, expressionStatement 2 expression]

nonListPatternProgram :: TypedProgram
nonListPatternProgram =
  expressionFixtureProgram
    "review-non-list-pattern"
    (TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedListPattern boolInfo []) Nothing falseExpr])

explicitTypeApplicationOwner :: TypedBinderId
explicitTypeApplicationOwner =
  binder
    ["Fixture", "review-explicit-type-application-contract"]
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "value")

explicitTypeApplicationContractProgram :: TypedProgram
explicitTypeApplicationContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-explicit-type-application-contract"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    scheme = monoScheme explicitTypeApplicationOwner
    expression = TypedTypeApplicationExpr boolInfo (TypedVariableExpr boolInfo valueName) span1 TypedBoolType
    statements = [TypedSignatureStatement explicitTypeApplicationOwner valueName span1 scheme, expressionStatement 2 expression]

variableSchemeContractProgram :: TypedProgram
variableSchemeContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-variable-scheme-contract"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    valueBinder = binder modulePath [0] valueName
    statements =
      [ TypedSignatureStatement valueBinder valueName span1 (monoScheme valueBinder),
        expressionStatement 2 (TypedVariableExpr textInfo valueName)
      ]

missingImportProgram :: TypedProgram
missingImportProgram =
  typedProgram
  where
    fixture = "review-missing-import"
    modulePath = ["Fixture", fixture]
    typedProgram =
      TypedProgram
        Nothing
        [ typedModule
            modulePath
            relativeSource
            [TypedResolvedImport span1 ["Missing", "Library"] Nothing Nothing]
            []
            emptyInterface
            []
            boolInfo
        ]
        modulePath

candidateConstraintProgram :: TypedProgram
candidateConstraintProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-candidate-constraint"
    modulePath = ["Fixture", fixture]
    renderName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    equalName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    renderImpl = TypedImplId modulePath renderName [TypedBoolType]
    equalImpl = TypedImplId modulePath equalName [TypedBoolType]
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    renderCandidate = TypedEvidenceCandidate renderImpl (Just (TypedMethodId renderImpl "render"))
    wrongMethodCandidate = TypedEvidenceCandidate equalImpl (Just (TypedMethodId equalImpl "other"))
    candidateExpression candidate =
      TypedVariableExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (TypedBuiltinName "candidate")
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 renderImpl []),
        TypedImplStatement (TypedImplDeclaration span1 equalImpl []),
        expressionStatement 3 (candidateExpression renderCandidate),
        expressionStatement 4 (candidateExpression wrongMethodCandidate)
      ]

invalidVariableNamespaceName :: TypedCoreName
invalidVariableNamespaceName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

invalidVariableNamespaceProgram :: TypedProgram
invalidVariableNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-variable-namespace"
    modulePath = ["Fixture", fixture]
    declaration = TypedDataDeclaration span1 invalidVariableNamespaceName [] []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 2 (TypedVariableExpr boolInfo invalidVariableNamespaceName)
      ]

binderNameContractBinder :: TypedBinderId
binderNameContractBinder =
  binder
    ["Fixture", "review-binder-name-contract"]
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "embedded")

binderNameContractProgram :: TypedProgram
binderNameContractProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-binder-name-contract"
    modulePath = ["Fixture", fixture]
    publishedName = resolved TypedCurrentModule TypedValueNamespace "published"
    scheme = monoScheme binderNameContractBinder
    statement = TypedLetStatement binderNameContractBinder publishedName span1 scheme trueExpr

blockLocalGeneralizedSchemeProgram :: TypedProgram
blockLocalGeneralizedSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-block-local-generalized-scheme"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = TypedScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    use = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    block = TypedBlockExpr boolInfo [TypedSignatureStatement owner valueName span1 scheme, expressionStatement 2 use]

blockLocalMonomorphicSchemeProgram :: TypedProgram
blockLocalMonomorphicSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface textInfo modulePath
  where
    fixture = "review-block-local-monomorphic-scheme"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    scheme = monoScheme owner
    use = TypedVariableExpr textInfo valueName
    block = TypedBlockExpr textInfo [TypedSignatureStatement owner valueName span1 scheme, expressionStatement 2 use]

implMethodNameProgram :: TypedProgram
implMethodNameProgram =
  singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-method-name"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    publishedName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] publishedName
    method = TypedMethodDefinition (TypedMethodId implId "equal") methodBinder publishedName span1 trueExpr
    declaration = TypedImplDeclaration span1 implId [method]

blockResultProgram :: TypedProgram
blockResultProgram =
  expressionFixtureProgram
    "review-block-result"
    (TypedBlockExpr boolInfo [expressionStatement 2 (literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "text"))])

nestedCasePatternPathProgram :: TypedProgram
nestedCasePatternPathProgram =
  expressionFixtureProgram fixture (TypedIfExpr boolInfo trueExpr nestedCase falseExpr)
  where
    fixture = "review-nested-case-pattern-path"
    nestedCase =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedLiteralPattern boolInfo (TypedTextLiteral "wrong")) Nothing falseExpr]

operatorSchemeProgram :: TypedProgram
operatorSchemeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textToTextInfo modulePath
  where
    fixture = "review-operator-scheme"
    modulePath = ["Fixture", fixture]
    operatorName = resolved TypedCurrentModule TypedValueNamespace "plus"
    owner = binder modulePath [0] operatorName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    scheme = TypedScheme owner [] [] [] operatorType operatorRecipe
    operator = TypedResolvedOperator operatorName "+"
    textExpr = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "text")
    textToTextInfo = info (TypedFunctionType TypedTextType TypedTextType) (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
    statements =
      [ TypedSignatureStatement owner operatorName span1 scheme,
        expressionStatement 2 (TypedBinaryExpr textInfo operator textExpr textExpr),
        expressionStatement 3 (TypedLeftSectionExpr textToTextInfo textExpr operator),
        expressionStatement 4 (TypedRightSectionExpr textToTextInfo operator textExpr)
      ]

operatorSchemeFailures :: [TypedCoreValidationFailure]
operatorSchemeFailures =
  [ operatorFailure 1 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 1 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 1 TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 2 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 2 TypedApplicationResultMismatch (TypedTypeDetail boolToBoolType (TypedFunctionType TypedTextType TypedTextType)),
    operatorFailure 3 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 3 TypedApplicationResultMismatch (TypedTypeDetail boolToBoolType (TypedFunctionType TypedTextType TypedTextType))
  ]
  where
    operatorFailure statementIndex =
      TypedCoreValidationFailure (TypedExpressionPath ["Fixture", "review-operator-scheme"] statementIndex [0])

selectiveImportProgram :: TypedProgram
selectiveImportProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "Selective"]
    entryPath = ["Fixture", "review-selective-import"]
    localName = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder libraryPath [0] localName
    scheme = monoScheme owner
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/Selective.jz")
        []
        [TypedModuleExport TypedValueNamespace "identity"]
        (TypedModuleInterface [TypedValueInterface localName scheme] [] [] [])
        [TypedSignatureStatement owner localName span1 scheme]
        boolInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["missing"])]
        []
        emptyInterface
        []
        boolInfo

classParameterScopeProgram :: TypedProgram
classParameterScopeProgram =
  singleModuleProgram fixture relativeSource [] [TypedClassStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-class-parameter-scope"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodBinder = binder modulePath [0, 0] methodName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodScheme = TypedScheme methodBinder [] [] [] methodType methodRecipe
    declaration = TypedClassDeclaration span1 capabilityName [parameterId] [TypedMethodSignature methodName span1 methodScheme]

evidenceParameterContractProgram :: TypedProgram
evidenceParameterContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-evidence-parameter-contract"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    evidenceId = TypedEvidenceParameterId 0
    generalizedConstraint = TypedCapabilityConstraint "Equal" Nothing (TypedTypeParameterType parameterId)
    scheme = TypedScheme owner [parameterId] [TypedEvidenceParameter evidenceId generalizedConstraint] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    selected selectedId constraint targetType =
      TypedSelectedEvidence
        (TypedEvidenceUse (Just selectedId) constraint (TypedImplId ["Prelude"] capabilityName [targetType]) Nothing)
    expression selection = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [selection]) valueName
    statements =
      [ TypedSignatureStatement owner valueName span1 scheme,
        expressionStatement 2 (expression (selected (TypedEvidenceParameterId 7) (TypedCapabilityConstraint "Equal" Nothing TypedBoolType) TypedBoolType)),
        expressionStatement 3 (expression (selected evidenceId (TypedCapabilityConstraint "Equal" Nothing TypedCharType) TypedCharType))
      ]

invalidImplCapabilityName :: TypedCoreName
invalidImplCapabilityName = resolved TypedAmbientPrelude TypedValueNamespace "Equal"

invisibleImplCapabilityName :: TypedCoreName
invisibleImplCapabilityName = resolved (TypedImportedModule ["Hidden", "Capabilities"]) TypedCapabilityNamespace "Render"

implCapabilityNamespaceProgram :: TypedProgram
implCapabilityNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-capability-namespace"
    modulePath = ["Fixture", fixture]
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 (TypedImplId modulePath invalidImplCapabilityName [TypedBoolType]) []),
        TypedImplStatement (TypedImplDeclaration span1 (TypedImplId modulePath invisibleImplCapabilityName [TypedBoolType]) [])
      ]

expressionFixtureProgram :: Text -> TypedExpr -> TypedProgram
expressionFixtureProgram fixture expression =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface (expressionInfoForFixture expression) ["Fixture", fixture]

expressionInfoForFixture :: TypedExpr -> TypedNodeInfo
expressionInfoForFixture expression =
  case expression of
    TypedLiteralExpr valueInfo _ -> valueInfo
    TypedVariableExpr valueInfo _ -> valueInfo
    TypedLambdaExpr valueInfo _ _ _ -> valueInfo
    TypedOperatorValueExpr valueInfo _ -> valueInfo
    TypedListExpr valueInfo _ -> valueInfo
    TypedTupleExpr valueInfo _ -> valueInfo
    TypedApplyExpr valueInfo _ _ -> valueInfo
    TypedTypeApplicationExpr valueInfo _ _ _ -> valueInfo
    TypedIfExpr valueInfo _ _ _ -> valueInfo
    TypedPatternCaseExpr valueInfo _ _ -> valueInfo
    TypedBinaryExpr valueInfo _ _ _ -> valueInfo
    TypedLeftSectionExpr valueInfo _ _ -> valueInfo
    TypedRightSectionExpr valueInfo _ _ -> valueInfo
    TypedBlockExpr valueInfo _ -> valueInfo

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

resolvedNameOriginsProgram :: TypedProgram
resolvedNameOriginsProgram =
  TypedProgram (Just preludeModule) [libraryModule, entryModule] entryPath
  where
    entryPath = ["Fixture", "resolved-name-origins"]
    libraryPath = ["Library", "Data"]
    localValue = resolved TypedCurrentModule TypedValueNamespace "localValue"
    localBinder = binder entryPath [0] localValue
    localScheme = TypedScheme localBinder [] [] [] TypedTextType TypedManagedTextRecipe
    importedSome = resolved (TypedImportedModule libraryPath) TypedConstructorNamespace "Some"
    localSome = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    libraryType = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someBinder = binder libraryPath [0, 0] localSome
    libraryData = TypedDataDeclaration span1 libraryType [] [TypedConstructorDeclaration someBinder localSome [TypedTextType] [TypedManagedTextRecipe]]
    preludeList = resolved TypedAmbientPrelude TypedTypeNamespace "List"
    preludeData = TypedDataDeclaration span1 preludeList [TypedTypeParameterId 0] []
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
    names = [importedSome, localValue]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Some"])]
        []
        emptyInterface
        ( [ TypedSignatureStatement localBinder localValue span1 localScheme,
            TypedClassStatement (TypedClassDeclaration span1 printable [] [])
          ]
            <> zipWith expressionStatement [1 ..] (map (TypedVariableExpr textInfo) names)
        )
        textInfo

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
    [ TypedDataStatement optionDeclaration,
      expressionStatement 1 (TypedTupleExpr unitInfo []),
      expressionStatement 2 (TypedTupleExpr pairInfo [trueExpr, falseExpr]),
      expressionStatement 3 (TypedListExpr boolListInfo [trueExpr, falseExpr]),
      expressionStatement 4 (TypedVariableExpr optionInfo optionConstructor)
    ]
    emptyInterface
    optionInfo
  where
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    optionConstructor = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    optionParameter = TypedTypeParameterId 0
    optionDeclaration =
      TypedDataDeclaration
        span1
        optionName
        [optionParameter]
        [ TypedConstructorDeclaration
            (binder ["Fixture", "list-tuple-data-recipes"] [0, 0] optionConstructor)
            optionConstructor
            [TypedTypeParameterType optionParameter]
            [TypedRepresentationParameterRecipe optionParameter]
        ]
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
    innerArgumentName = resolved TypedCurrentModule TypedValueNamespace "inner-argument"
    innerArgumentBinder = binder ["Fixture", "callable-recipes"] [0, 0] innerArgumentName
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
        (TypedLambdaExpr innerInfo innerArgumentBinder innerArgumentName (TypedLiteralExpr textInfo (TypedTextLiteral "ok")))

monomorphicBindingProgram :: TypedProgram
monomorphicBindingProgram =
  singleModuleProgram
    fixture
    relativeSource
    [TypedModuleExport TypedValueNamespace "enabled"]
    [TypedLetStatement valueBinder valueName span1 scheme trueExpr]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    boolInfo
    ["Fixture", fixture]
  where
    fixture = "monomorphic-binding"
    valueName = resolved TypedCurrentModule TypedValueNamespace "enabled"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    scheme = TypedScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

generalizedBindingProgram :: TypedProgram
generalizedBindingProgram =
  singleModuleProgram
    fixture
    relativeSource
    [TypedModuleExport TypedValueNamespace "choose"]
    [TypedSignatureStatement valueBinder valueName span1 scheme]
    (TypedModuleInterface [TypedValueInterface valueName scheme] [] [] [])
    boolInfo
    ["Fixture", fixture]
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
    instantiatedInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression =
      case explicitSpan of
        Nothing -> TypedVariableExpr instantiatedInfo name
        Just explicitApplicationSpan ->
          TypedTypeApplicationExpr
            instantiatedInfo
            (TypedVariableExpr boolInfo name)
            explicitApplicationSpan
            TypedBoolType

explicitCapabilityEvidenceProgram :: TypedProgram
explicitCapabilityEvidenceProgram = evidenceProgram "explicit-capability-evidence" (Just (TypedEvidenceParameterId 0))

inferredCapabilityEvidenceProgram :: TypedProgram
inferredCapabilityEvidenceProgram = evidenceProgram "inferred-capability-evidence" Nothing

evidenceProgram :: Text -> Maybe TypedEvidenceParameterId -> TypedProgram
evidenceProgram fixture parameterId =
  programWith fixture [TypedSignatureStatement valueBinder valueName span1 scheme, expressionStatement 1 expression] emptyInterface boolInfo
  where
    capability = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    evidenceUse = TypedEvidenceUse parameterId capability implId Nothing
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    scheme =
      case parameterId of
        Nothing -> monoScheme valueBinder
        Just evidenceId -> TypedScheme valueBinder [] [TypedEvidenceParameter evidenceId capability] [] TypedBoolType TypedBoolRecipe
    instantiations =
      case parameterId of
        Nothing -> []
        Just _ -> [TypedInstantiation valueBinder [] Nothing]
    expression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe instantiations [TypedSelectedEvidence evidenceUse])
        valueName

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
        (TypedBuiltinName "equal")

partialMethodCandidatesProgram :: TypedProgram
partialMethodCandidatesProgram =
  programWith
    fixture
    [ TypedImplStatement (TypedImplDeclaration span1 secondImpl []),
      expressionStatement 1 expression
    ]
    emptyInterface
    boolToBoolInfo
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
        (TypedBuiltinName "render")

patternsBindersProgram :: TypedProgram
patternsBindersProgram =
  programWith fixture statements emptyInterface boolInfo
  where
    fixture = "patterns-binders"
    modulePath = ["Fixture", fixture]
    variablePattern index =
      let name = resolved TypedCurrentModule TypedValueNamespace ("value-" <> Text.pack (show index))
       in TypedVariablePattern boolInfo (binder modulePath [index] name) name
    asName = resolved TypedCurrentModule TypedValueNamespace "as-value"
    asPattern = TypedAsPattern boolInfo (binder modulePath [6] asName) asName (TypedWildcardPattern boolInfo)
    orPatternBinder = variablePattern 7
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
    optionDeclaration =
      TypedDataDeclaration
        span1
        optionName
        [optionParameter]
        [ TypedConstructorDeclaration
            (binder ["Fixture", fixture] [0, 0] someName)
            someName
            [TypedTypeParameterType optionParameter]
            [TypedRepresentationParameterRecipe optionParameter]
        ]
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
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
              (TypedVariableExpr optionInfo someName)
              [TypedCaseArm (TypedConstructorPattern optionInfo someName [variablePattern 2]) Nothing trueExpr],
            listCase (TypedListPattern boolListPatternInfo [variablePattern 3]),
            listCase (TypedConsListPattern boolListPatternInfo (variablePattern 4) (TypedListPattern boolListPatternInfo [])),
            TypedPatternCaseExpr
              boolInfo
              (TypedTupleExpr pairInfo [trueExpr, falseExpr])
              [TypedCaseArm (TypedTuplePattern pairInfo [variablePattern 5, TypedWildcardPattern boolInfo]) Nothing trueExpr],
            boolCase asPattern,
            boolCase (TypedOrPattern boolInfo [orPatternBinder, orPatternBinder])
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
unitInfo = info (TypedTupleType []) TypedUnitRecipe

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
    functionExpr = TypedVariableExpr boolToBoolInfo (TypedBuiltinName "function")
    argumentExpr = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedApplyExpr boolInfo functionExpr argumentExpr

applicationResultTypeFixture :: InvalidFixture
applicationResultTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
  where
    fixture = "application-result-type"
    functionExpr = TypedVariableExpr boolToBoolInfo (TypedBuiltinName "function")
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
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) (TypedBuiltinName "unknown")

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
        (TypedBuiltinName "missing")
    duplicateExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (TypedBuiltinName "duplicate")
    program = singleModuleProgram fixture relativeSource [] [expressionStatement 1 missingExpression, expressionStatement 2 duplicateExpression] emptyInterface boolInfo ["Fixture", fixture]
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
    constraint = TypedCapabilityConstraint "Render" Nothing TypedTextType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId ["Library", "Render"] capabilityName [TypedTextType]
    invisibleImpl = TypedImplId ["Hidden", "Render"] capabilityName [TypedTextType]
    ambiguousExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedEvidenceCandidates constraint [TypedEvidenceCandidate firstImpl Nothing, TypedEvidenceCandidate secondImpl Nothing]])
        (TypedBuiltinName "ambiguous")
    invisibleUse = TypedEvidenceUse Nothing constraint invisibleImpl Nothing
    invisibleExpression =
      TypedVariableExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedSelectedEvidence invisibleUse])
        (TypedBuiltinName "invisible")
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
        (TypedBuiltinName "equal")
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
    (singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface (expressionInfoForFixture expression) ["Fixture", fixture])
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
  TypedCoreValidationFailure (TypedPatternPath ["Fixture", fixture] 0 [0, 0])
