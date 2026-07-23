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
    ("enforces newest typed-core review contracts", testNewestReviewRegressions),
    ("retains data metadata for selectively imported values", testSelectedValueDataMetadata),
    ("filters private impls from selective imports", testSelectiveImportImplLeak),
    ("requires selected evidence methods to exist", testSelectedEvidenceMethodExistence),
    ("rejects duplicate impl methods", testDuplicateImplMethods),
    ("retains outer type scope in nested bindings", testNestedOuterTypeScope),
    ("keeps impl methods out of ordinary value scope", testImplMethodValueVisibility),
    ("validates builtin operator contracts", testBuiltinOperatorContracts),
    ("limits candidate deferral to qualified methods", testOrdinaryFunctionCandidateAmbiguity),
    ("validates numeric primitive constraint targets", testNumericPrimitiveConstraintTargets),
    ("requires polymorphic variable instantiations", testMissingPolymorphicInstantiation),
    ("rejects unsupported strict equality constraints", testUnsupportedStrictEqualityConstraint),
    ("checks builtin and generated name visibility", testUncheckedSpecialNames),
    ("exports class methods as values", testClassMethodExport),
    ("matches fractional literal suffix widths", testFractionalLiteralSuffix),
    ("checks Prelude evidence implementations", testMissingPreludeImpl),
    ("retains type scope for evidence", testEvidenceTypeScope),
    ("checks constructor pattern data ownership", testWrongConstructorPatternType),
    ("checks local impl module ownership", testForeignOwnedLocalImpl),
    ("imports type-exported capability metadata", testImportedTypeCapabilityMetadata),
    ("rejects callable builtin equality", testCallableBuiltinEquality),
    ("enforces current typed-core review contracts", testCurrentReviewRegressions),
    ("enforces latest bot-reviewed typed-core contracts", testLatestBotReviewRegressions),
    ("enforces newest bot-reviewed typed-core contracts", testNewestBotReviewRegressions),
    ("enforces post-newest bot-reviewed typed-core contracts", testPostNewestBotReviewRegressions),
    ("checks fractional literals against their selected floating widths", testFractionalLiteralBounds),
    ("rejects local classes that collide with visible classes", testVisibleClassCollisions),
    ("retains method data metadata for selective class imports", testSelectedClassDataDependency),
    ("resolves shadowed schemes through lexical scope", testLexicalSchemeShadowing),
    ("rejects method candidates after full application", testFullyAppliedMethodCandidates),
    ("rejects duplicate unbound selected evidence", testDuplicateUnboundEvidence),
    ("generalizes imported class methods as values", testGeneralizedClassMethodImport),
    ("rejects colliding imported class identifiers", testImportedClassCollision),
    ("preserves block statement scope order", testForwardBlockReference),
    ("preserves proven recursive block peers", testRecursiveBlockPeers),
    ("rejects malformed generalized literal bounds", testMalformedLiteralConstraintBounds),
    ("preserves instantiated evidence order", testEvidenceSelectionOrder),
    ("keeps private capability metadata out of source visibility", testPrivateCapabilityMetadataVisibility),
    ("matches module-qualified method keys at the final separator", testModuleQualifiedMethodKey),
    ("retains imported data dependencies through exported schemes", testImportedDataDependencyMetadata),
    ("closes selected data contracts over field metadata", testTransitiveDataContractDependency),
    ("rejects imported capability dependencies that lose identity", testImportedCapabilityDependency),
    ("keeps metadata-only impls out of evidence visibility", testMetadataOnlyImplVisibility),
    ("rejects expression-only metadata on patterns", testPatternExpressionMetadata),
    ("allows phantom data arguments in strict equality", testPhantomDataEquality),
    ("preserves same-scope value rebinding", testSameScopeValueRebinding),
    ("preserves top-level statement scope order", testForwardModuleReference),
    ("rejects cyclic resolved imports", testCyclicResolvedImports),
    ("keeps bare signatures out of executable value scope", testBareSignatureVisibility),
    ("exports only the active rebinding scheme", testActiveRebindingExport),
    ("accepts constructor-owned instantiations", testConstructorInstantiation),
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
    implCapabilityNamespaceProgram,
    missingInstantiatedEvidenceProgram,
    constructorExpressionContractProgram,
    unrelatedTypeApplicationProgram,
    lexicalBinderContractProgram,
    generalizedVariableContractProgram,
    enclosingInstantiationScopeProgram,
    implMethodContractProgram,
    dataDeclarationNamespaceProgram,
    duplicateDeclarationProgram,
    importedImplQualificationProgram,
    implTargetArityProgram,
    localDeclarationOriginProgram,
    selectedValueDataMetadataProgram,
    selectiveImportImplLeakProgram,
    selectedEvidenceMethodExistenceProgram,
    duplicateImplMethodProgram,
    nestedOuterTypeScopeProgram,
    implMethodValueVisibilityProgram,
    builtinOperatorContractProgram,
    ordinaryFunctionCandidateAmbiguityProgram,
    invalidNumericPrimitiveConstraintProgram,
    missingPolymorphicInstantiationProgram,
    unsupportedStrictEqualityConstraintProgram,
    uncheckedSpecialNameProgram,
    classMethodExportProgram,
    fractionalLiteralSuffixProgram,
    missingPreludeImplProgram,
    evidenceTypeScopeProgram,
    wrongConstructorPatternTypeProgram,
    foreignOwnedLocalImplProgram,
    importedTypeCapabilityMetadataProgram,
    callableBuiltinEqualityProgram,
    moduleInfoStructuralEqualityProgram,
    typeApplicationResultContractProgram,
    capabilityConstraintVisibilityProgram,
    unconstrainedNumericParameterProgram,
    unconstrainedEqualityParameterProgram,
    duplicatePatternNameProgram,
    nonTuplePatternProgram,
    ownerAmbiguousEvidenceProgram,
    reorderedOrPatternProgram,
    emptyPatternCaseProgram,
    typeVisibleImplImportProgram,
    methodVisibleImplImportProgram,
    integralLiteralRangeProgram,
    nestedStrictEqualityConstraintProgram,
    canonicalQualifiedMethodKeyProgram,
    wrongQualifiedMethodKeyProgram,
    builtinValueContractProgram,
    missingInterfaceMetadataProgram,
    unterminatedBlockProgram,
    constrainedMonomorphicUseProgram,
    unrelatedKnownInstantiationProgram,
    explicitHeadParameterProgram,
    classArityProgram,
    classMethodSchemeShapeProgram,
    duplicateImplDeclarationProgram,
    emptyOrPatternProgram,
    nonBindingTypeApplicationProgram,
    mismatchedResolvedOperatorProgram,
    dataInterfaceDependencyProgram,
    classMethodInterfaceDependencyProgram,
    instantiatedPrimitiveConstraintProgram,
    typeApplicationExtraOwnerProgram,
    constrainedResolvedOperatorProgram,
    missingModuleResultProgram,
    emptyDataDeclarationProgram,
    laterOrPatternBinderCollisionProgram,
    concreteIntegerBoundsProgram,
    incompleteImplProgram,
    duplicateInstantiationProgram,
    fractionalLiteralBoundsProgram,
    visibleClassCollisionProgram,
    selectedClassDataDependencyProgram,
    lexicalSchemeShadowingProgram,
    fullyAppliedMethodCandidatesProgram,
    duplicateUnboundEvidenceProgram,
    generalizedClassMethodImportProgram,
    importedClassCollisionProgram,
    forwardBlockReferenceProgram,
    recursiveBlockPeerProgram,
    malformedLiteralConstraintBoundsProgram,
    evidenceSelectionOrderProgram,
    privateCapabilityMetadataVisibilityProgram,
    moduleQualifiedMethodKeyProgram,
    importedDataDependencyProgram,
    transitiveDataContractDependencyProgram,
    importedCapabilityDependencyProgram,
    metadataOnlyImplVisibilityProgram,
    patternExpressionMetadataProgram,
    phantomDataEqualityProgram,
    sameScopeValueRebindingProgram,
    forwardModuleReferenceProgram,
    cyclicImportProgram,
    bareSignatureVisibilityProgram,
    activeRebindingExportProgram,
    constructorInstantiationProgram
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
      (TypedExpressionPath ["Fixture", "review-nested-declaration"] 0 [0])
      TypedBlockResultMismatch
      TypedNoValidationDetail,
    TypedCoreValidationFailure
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
          TypedLetStatement duplicateBinder name span1 scheme trueExpr,
          expressionStatement 3 trueExpr
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
        [TypedLetStatement owner localName span1 scheme (polymorphicIdentityExpression libraryPath [0] parameterId)]
        boolInfo
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    instantiatedType = TypedFunctionType TypedBoolType TypedBoolType
    instantiatedRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    instantiatedInfo = TypedNodeInfo instantiatedType instantiatedRecipe [instantiation] []
    expression = TypedVariableExpr instantiatedInfo importedName
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["identity"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        instantiatedInfo

invisibleSiblingImplId :: TypedImplId
invisibleSiblingImplId =
  TypedImplId ["Hidden", "Evidence"] (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal") [TypedBoolType]

invisibleSiblingImplProgram :: TypedProgram
invisibleSiblingImplProgram = TypedProgram (Just fixturePrelude) [hiddenModule, entryModule] entryPath
  where
    fixture = "review-invisible-sibling-impl"
    hiddenPath = ["Hidden", "Evidence"]
    entryPath = ["Fixture", fixture]
    hiddenDeclaration =
      TypedImplDeclaration
        span1
        invisibleSiblingImplId
        [ fixtureImplMethod hiddenPath [0, 0] invisibleSiblingImplId "equal",
          fixtureImplMethod hiddenPath [0, 1] invisibleSiblingImplId "other"
        ]
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
        [TypedLetStatement valueBinder valueName span1 scheme trueExpr, expressionStatement 1 expression]
        boolInfo

selectedEvidenceTargetProgram :: TypedProgram
selectedEvidenceTargetProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
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
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [ fixtureImplMethod modulePath [0, 0] implId "equal",
                fixtureImplMethod modulePath [0, 1] implId "other"
              ]
          ),
        TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

invisibleVariableName :: TypedCoreName
invisibleVariableName = resolved TypedCurrentModule TypedValueNamespace "missing"

invisibleVariableProgram :: TypedProgram
invisibleVariableProgram =
  expressionFixtureProgram "review-invisible-variable" (TypedVariableExpr boolInfo invisibleVariableName)

selectedMethodContractProgram :: TypedProgram
selectedMethodContractProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
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
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [ fixtureImplMethod modulePath [0, 0] implId "equal",
                fixtureImplMethod modulePath [0, 1] implId "other"
              ]
          ),
        TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 (selected withoutMethod),
        expressionStatement 2 (selected wrongMethod)
      ]

enclosingOtherImplId :: TypedImplId
enclosingOtherImplId =
  TypedImplId ["Fixture", "review-enclosing-impl-method"] (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render") [TypedCharType]

enclosingImplMethodProgram :: TypedProgram
enclosingImplMethodProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-enclosing-impl-method"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] methodName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    methodExpression = TypedLambdaExpr boolToBoolInfo (binder modulePath [0, 0, 0] argumentName) argumentName trueExpr
    method = TypedMethodDefinition (TypedMethodId enclosingOtherImplId "render") methodBinder methodName span1 methodExpression
    declaration =
      TypedImplDeclaration
        span1
        implId
        [method, fixtureImplMethod modulePath [0, 1] implId "map"]

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
    [ expressionFailureAt "review-data-type-arity" 1 TypedDataTypeMismatch (TypedArityDetail 1 0),
      expressionFailureAt "review-data-type-arity" 1 TypedBlockResultMismatch TypedNoValidationDetail
    ]
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
    declaration =
      dataDeclarationWithNullaryConstructor
        modulePath
        [0, 0]
        dataName
        [TypedTypeParameterId 0]
    dataInfo = info (TypedDataType dataName []) (TypedManagedVariantRecipe dataName [])
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (TypedBlockExpr dataInfo [])
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
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
      expressionFailureAt
        "review-evidence-parameter-contract"
        1
        TypedInstantiationMismatch
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 7)),
      expressionFailureAt
        "review-evidence-parameter-contract"
        2
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
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

testNewestReviewRegressions :: IO ()
testNewestReviewRegressions = do
  assertEqual
    "instantiated evidence obligations must all be selected"
    [ expressionFailureAt
        "review-missing-instantiated-evidence"
        1
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
      expressionFailureAt
        "review-missing-instantiated-evidence"
        1
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 1))
    ]
    (validateTypedProgram missingInstantiatedEvidenceProgram)
  assertEqual
    "constructor expressions match their declaration contracts"
    [ expressionFailureAt
        "review-constructor-expression-contract"
        1
        TypedBindingValueMismatch
        ( TypedTypeDetail
            (TypedFunctionType TypedBoolType constructorExpressionResultType)
            TypedBoolType
        )
    ]
    (validateTypedProgram constructorExpressionContractProgram)
  assertEqual
    "explicit type applications are owned by their callee"
    [ expressionFailureAt
        "review-unrelated-type-application"
        1
        TypedInstantiationMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram unrelatedTypeApplicationProgram)
  assertEqual
    "lexical variable uses match their binder contracts"
    [ TypedCoreValidationFailure
        (TypedExpressionPath ["Fixture", "review-lexical-binder-contract"] 0 [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram lexicalBinderContractProgram)
  assertEqual
    "generalized variable uses match substituted scheme results"
    [ expressionFailureAt
        "review-generalized-variable-contract"
        1
        TypedBindingValueMismatch
        ( TypedTypeDetail
            (TypedFunctionType TypedBoolType TypedBoolType)
            TypedBoolType
        )
    ]
    (validateTypedProgram generalizedVariableContractProgram)
  assertEqual
    "instantiation arguments retain the enclosing type scope"
    []
    (validateTypedProgram enclosingInstantiationScopeProgram)
  assertEqual
    "impl method bodies match instantiated class signatures"
    [ statementFailure
        "review-impl-method-contract"
        1
        TypedBindingValueMismatch
        ( TypedTypeDetail
            (TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType))
            TypedTextType
        )
    ]
    (validateTypedProgram implMethodContractProgram)
  assertEqual
    "data declarations use the type namespace"
    [ statementFailure
        "review-data-declaration-namespace"
        0
        TypedInvisibleName
        (TypedNameDetail invalidDataDeclarationName)
    ]
    (validateTypedProgram dataDeclarationNamespaceProgram)
  assertEqual
    "duplicate resolved declarations are rejected before context construction"
    [ statementFailure
        "review-duplicate-declaration"
        1
        TypedDuplicateDeclaration
        (TypedNameDetail duplicateDeclarationName)
    ]
    (validateTypedProgram duplicateDeclarationProgram)
  assertEqual
    "imported impl identities are qualified into the importing module"
    []
    (validateTypedProgram importedImplQualificationProgram)
  assertEqual
    "impl target lists match capability arity"
    [ statementFailure
        "review-impl-target-arity"
        0
        TypedMethodSelectionMismatch
        (TypedArityDetail 1 2),
      expressionFailureAt
        "review-impl-target-arity"
        1
        TypedMethodSelectionMismatch
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram implTargetArityProgram)
  assertEqual
    "local declarations cannot claim imported name origins"
    [ statementFailure
        "review-local-declaration-origin"
        0
        TypedInvisibleName
        (TypedNameDetail localDeclarationOriginName)
    ]
    (validateTypedProgram localDeclarationOriginProgram)

testSelectedValueDataMetadata :: IO ()
testSelectedValueDataMetadata =
  assertEqual
    "selective value imports retain referenced data metadata"
    []
    (validateTypedProgram selectedValueDataMetadataProgram)

testSelectiveImportImplLeak :: IO ()
testSelectiveImportImplLeak =
  assertEqual
    "selective imports do not expose private impl interfaces"
    [ expressionFailureAt
        "review-selective-import-impl-leak"
        0
        TypedInvisibleImpl
        (TypedImplDetail selectiveImportLeakedImpl)
    ]
    (validateTypedProgram selectiveImportImplLeakProgram)

testSelectedEvidenceMethodExistence :: IO ()
testSelectedEvidenceMethodExistence =
  assertEqual
    "selected method evidence requires capability and impl method contracts"
    [ expressionFailureAt
        "review-selected-evidence-method-existence"
        2
        TypedMethodSelectionMismatch
        (TypedTextDetail "equal"),
      expressionFailureAt
        "review-selected-evidence-method-existence"
        2
        TypedMethodSelectionMismatch
        (TypedTextDetail "equal")
    ]
    (validateTypedProgram selectedEvidenceMethodExistenceProgram)

testDuplicateImplMethods :: IO ()
testDuplicateImplMethods =
  assertEqual
    "impl declarations reject duplicate method definitions"
    [ statementFailure
        "review-duplicate-impl-method"
        0
        TypedDuplicateDeclaration
        (TypedNameDetail duplicateImplMethodName)
    ]
    (validateTypedProgram duplicateImplMethodProgram)

testNestedOuterTypeScope :: IO ()
testNestedOuterTypeScope =
  assertEqual
    "nested local schemes retain enclosing type parameters"
    []
    (validateTypedProgram nestedOuterTypeScopeProgram)

testImplMethodValueVisibility :: IO ()
testImplMethodValueVisibility =
  assertEqual
    "impl method names are not ordinary visible values"
    [ expressionFailureAt
        "review-impl-method-value-visibility"
        1
        TypedInvisibleName
        (TypedNameDetail implMethodVisibleName)
    ]
    (validateTypedProgram implMethodValueVisibilityProgram)

testBuiltinOperatorContracts :: IO ()
testBuiltinOperatorContracts =
  assertEqual
    "builtin operators match the supported typed catalog"
    [ expressionFailureAt
        "review-builtin-operator-contract"
        0
        TypedBindingValueMismatch
        (TypedTextDetail "%%"),
      expressionFailureAt
        "review-builtin-operator-contract"
        1
        TypedApplicationResultMismatch
        (TypedTypeDetail TypedIntType TypedBoolType)
    ]
    (validateTypedProgram builtinOperatorContractProgram)

testOrdinaryFunctionCandidateAmbiguity :: IO ()
testOrdinaryFunctionCandidateAmbiguity =
  assertEqual
    "candidate ambiguity is deferred only for qualified methods"
    [ expressionFailureAt
        "review-ordinary-function-candidate-ambiguity"
        0
        TypedAmbiguousEvidence
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram ordinaryFunctionCandidateAmbiguityProgram)

testNumericPrimitiveConstraintTargets :: IO ()
testNumericPrimitiveConstraintTargets =
  assertEqual
    "numeric primitive constraints reject nonnumeric targets"
    [ statementFailure
        "review-invalid-numeric-primitive-constraint"
        0
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType TypedTextType)
    ]
    (validateTypedProgram invalidNumericPrimitiveConstraintProgram)

testMissingPolymorphicInstantiation :: IO ()
testMissingPolymorphicInstantiation =
  assertEqual
    "polymorphic variable uses require an instantiation"
    [ expressionFailureAt
        "review-missing-polymorphic-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail missingPolymorphicInstantiationOwner)
    ]
    (validateTypedProgram missingPolymorphicInstantiationProgram)

testUnsupportedStrictEqualityConstraint :: IO ()
testUnsupportedStrictEqualityConstraint =
  assertEqual
    "strict equality constraints reject callable and callable-containing data types"
    [ statementFailure
        "review-unsupported-strict-equality-constraint"
        1
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType boolToBoolType),
      statementFailure
        "review-unsupported-strict-equality-constraint"
        2
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType unsupportedEqualityDataType)
    ]
    (validateTypedProgram unsupportedStrictEqualityConstraintProgram)

testUncheckedSpecialNames :: IO ()
testUncheckedSpecialNames =
  assertEqual
    "builtin names come from the catalog and generated names remain lexical"
    [ expressionFailureAt
        "review-unchecked-special-name"
        0
        TypedInvisibleName
        (TypedNameDetail (TypedBuiltinName "doesNotExist")),
      expressionFailureAt
        "review-unchecked-special-name"
        1
        TypedInvisibleName
        (TypedNameDetail (TypedGeneratedName TypedOperatorSectionFunction))
    ]
    (validateTypedProgram uncheckedSpecialNameProgram)

testClassMethodExport :: IO ()
testClassMethodExport =
  assertEqual
    "class methods can be exported and selectively imported as values"
    []
    (validateTypedProgram classMethodExportProgram)

testFractionalLiteralSuffix :: IO ()
testFractionalLiteralSuffix =
  assertEqual
    "fractional literal suffixes match their typed numeric width"
    [ expressionFailure
        "review-fractional-literal-suffix"
        TypedLiteralTypeMismatch
        (TypedTypeDetail (TypedNumericType TypedFloat16Type) (TypedNumericType TypedFloat64Type))
    ]
    (validateTypedProgram fractionalLiteralSuffixProgram)

testMissingPreludeImpl :: IO ()
testMissingPreludeImpl =
  assertEqual
    "Prelude evidence selects a declared visible implementation"
    [ expressionFailure
        "review-missing-prelude-impl"
        TypedInvisibleImpl
        (TypedImplDetail missingPreludeImplId)
    ]
    (validateTypedProgram missingPreludeImplProgram)

testEvidenceTypeScope :: IO ()
testEvidenceTypeScope =
  assertEqual
    "evidence validation retains the enclosing type scope"
    [ expressionFailureAt
        "review-evidence-type-scope"
        0
        TypedMethodSelectionMismatch
        (TypedTypeDetail (TypedTypeParameterType evidenceTypeScopeParameter) TypedBoolType)
    ]
    (validateTypedProgram evidenceTypeScopeProgram)

testWrongConstructorPatternType :: IO ()
testWrongConstructorPatternType =
  assertEqual
    "constructor patterns belong to their annotated data type"
    [ TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-wrong-constructor-pattern-type"] 1 [0, 0])
        TypedPatternShapeMismatch
        (TypedTypeDetail wrongConstructorDataType TypedBoolType)
    ]
    (validateTypedProgram wrongConstructorPatternTypeProgram)

testForeignOwnedLocalImpl :: IO ()
testForeignOwnedLocalImpl =
  assertEqual
    "local impl identities use the declaring module path"
    [ statementFailure
        "review-foreign-owned-local-impl"
        1
        TypedInvisibleImpl
        (TypedImplDetail foreignOwnedLocalImplId)
    ]
    (validateTypedProgram foreignOwnedLocalImplProgram)

testImportedTypeCapabilityMetadata :: IO ()
testImportedTypeCapabilityMetadata =
  assertEqual
    "type-exported classes retain capability metadata when imported"
    [ statementFailure
        "review-imported-type-capability-metadata"
        0
        TypedMethodSelectionMismatch
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram importedTypeCapabilityMetadataProgram)

testCallableBuiltinEquality :: IO ()
testCallableBuiltinEquality =
  assertEqual
    "builtin equality rejects callable operand types"
    [ expressionFailure
        "review-callable-builtin-equality"
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType boolToBoolType)
    ]
    (validateTypedProgram callableBuiltinEqualityProgram)

testCurrentReviewRegressions :: IO ()
testCurrentReviewRegressions = do
  assertEqual
    "module info uses complete structural equality before skipping validation"
    [ moduleFailure
        "review-module-info-structural-equality"
        TypedInstantiationMismatch
        (TypedBinderDetail moduleInfoStructuralEqualityUnknownOwner)
    ]
    (validateTypedProgram moduleInfoStructuralEqualityProgram)
  assertEqual
    "explicit type application nodes match the instantiated callee contract"
    [ expressionFailureAt
        "review-type-application-result-contract"
        1
        TypedApplicationResultMismatch
        (TypedTypeDetail boolToBoolType TypedTextType)
    ]
    (validateTypedProgram typeApplicationResultContractProgram)
  assertEqual
    "capability constraints resolve visible classes and methods"
    [ statementFailure
        "review-capability-constraint-visibility"
        0
        TypedInvisibleName
        (TypedTextDetail "Missing"),
      statementFailure
        "review-capability-constraint-visibility"
        0
        TypedMethodSelectionMismatch
        (TypedTextDetail "Equal.missing")
    ]
    (validateTypedProgram capabilityConstraintVisibilityProgram)
  assertEqual
    "numeric operators over type parameters require a published primitive constraint"
    [ TypedCoreValidationFailure
        (TypedExpressionPath ["Fixture", "review-unconstrained-numeric-parameter"] 0 [0, 0])
        TypedBindingValueMismatch
        (TypedTextDetail "+")
    ]
    (validateTypedProgram unconstrainedNumericParameterProgram)
  assertEqual
    "equality over type parameters requires a published primitive constraint"
    [ TypedCoreValidationFailure
        (TypedExpressionPath ["Fixture", "review-unconstrained-equality-parameter"] 0 [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType (TypedTypeParameterType (TypedTypeParameterId 0)))
    ]
    (validateTypedProgram unconstrainedEqualityParameterProgram)
  assertEqual
    "one pattern cannot bind the same resolved name twice"
    [ patternFailure
        "review-duplicate-pattern-name"
        TypedDuplicateBinder
        (TypedBinderDetail duplicatePatternNameSecondBinder)
    ]
    (validateTypedProgram duplicatePatternNameProgram)
  assertEqual
    "tuple patterns require tuple-typed nodes"
    [ patternFailure
        "review-non-tuple-pattern"
        TypedPatternShapeMismatch
        (TypedTypeDetail (TypedTupleType []) TypedBoolType)
    ]
    (validateTypedProgram nonTuplePatternProgram)
  assertEqual
    "evidence selections are owned by one generalized callee"
    [ expressionFailureAt
        "review-owner-ambiguous-evidence"
        2
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
      expressionFailureAt
        "review-owner-ambiguous-evidence"
        2
        TypedInstantiationMismatch
        (TypedBinderDetail ownerAmbiguousSecondOwner)
    ]
    (validateTypedProgram ownerAmbiguousEvidenceProgram)
  assertEqual
    "or-pattern contracts compare binder names independently of traversal order"
    []
    (validateTypedProgram reorderedOrPatternProgram)
  assertEqual
    "pattern cases require at least one arm"
    [ expressionFailure
        "review-empty-pattern-case"
        TypedPatternShapeMismatch
        (TypedArityDetail 1 0)
    ]
    (validateTypedProgram emptyPatternCaseProgram)
  assertEqual
    "type-visible classes import their public impls"
    []
    (validateTypedProgram typeVisibleImplImportProgram)
  assertEqual
    "method-visible classes import their public impls"
    []
    (validateTypedProgram methodVisibleImplImportProgram)
  assertEqual
    "integral literal constraints fit the selected numeric width"
    [ statementFailure
        "review-integral-literal-range"
        0
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType (TypedNumericType TypedUInt8Type))
    ]
    (validateTypedProgram integralLiteralRangeProgram)

testLatestBotReviewRegressions :: IO ()
testLatestBotReviewRegressions = do
  assertEqual
    "nested operand types do not inherit an enclosing strict-equality constraint"
    [ TypedCoreValidationFailure
        (TypedExpressionPath ["Fixture", "review-nested-strict-equality-constraint"] 0 [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType nestedStrictEqualityOperandType)
    ]
    (validateTypedProgram nestedStrictEqualityConstraintProgram)
  assertEqual
    "canonical qualified method keys match their selected method"
    []
    (validateTypedProgram canonicalQualifiedMethodKeyProgram)
  assertEqual
    "qualified method keys verify their capability qualifier"
    [ expressionFailure
        "review-wrong-qualified-method-key"
        TypedMethodSelectionMismatch
        (TypedTextDetail "Other.equal")
    ]
    (validateTypedProgram wrongQualifiedMethodKeyProgram)
  assertEqual
    "builtin variables match the builtin catalog value contract"
    [ expressionFailure
        "review-builtin-value-contract"
        TypedBindingValueMismatch
        (TypedTypeDetail (TypedFunctionType TypedTextType TypedIntType) TypedBoolType)
    ]
    (validateTypedProgram builtinValueContractProgram)
  assertEqual
    "value interfaces carry local data metadata referenced by their schemes"
    [ TypedCoreValidationFailure
        (TypedInterfacePath ["Library", "MissingMetadata"])
        TypedModuleInterfaceMismatch
        (TypedNameDetail missingInterfaceMetadataDataName)
    ]
    (validateTypedProgram missingInterfaceMetadataProgram)
  assertEqual
    "value blocks require a terminal expression"
    [ expressionFailure
        "review-unterminated-block"
        TypedBlockResultMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram unterminatedBlockProgram)
  assertEqual
    "constrained monomorphic uses require instantiation and evidence metadata"
    [ expressionFailureAt
        "review-constrained-monomorphic-use"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail constrainedMonomorphicOwner),
      expressionFailureAt
        "review-constrained-monomorphic-use"
        1
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram constrainedMonomorphicUseProgram)
  assertEqual
    "expression instantiations belong to a scheme referenced by that expression"
    [ expressionFailureAt
        "review-unrelated-known-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail unrelatedKnownInstantiationOwner)
    ]
    (validateTypedProgram unrelatedKnownInstantiationProgram)

testNewestBotReviewRegressions :: IO ()
testNewestBotReviewRegressions = do
  assertEqual
    "explicit type applications bind the first quantified parameter"
    [ expressionFailureAt
        "review-explicit-head-parameter"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail explicitHeadParameterOwner)
    ]
    (validateTypedProgram explicitHeadParameterProgram)
  assertEqual
    "class declarations remain unary"
    [ statementFailure
        "review-class-arity"
        0
        TypedMethodSelectionMismatch
        (TypedArityDetail 1 0),
      statementFailure
        "review-class-arity"
        1
        TypedMethodSelectionMismatch
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram classArityProgram)
  assertEqual
    "class method schemes cannot add local parameters or obligations"
    [ statementFailure
        "review-class-method-scheme-shape"
        0
        TypedBindingValueMismatch
        (TypedArityDetail 0 1),
      statementFailure
        "review-class-method-scheme-shape"
        0
        TypedBindingValueMismatch
        (TypedArityDetail 0 1),
      statementFailure
        "review-class-method-scheme-shape"
        0
        TypedBindingValueMismatch
        (TypedArityDetail 0 1)
    ]
    (validateTypedProgram classMethodSchemeShapeProgram)
  assertEqual
    "duplicate impl identities are rejected before dictionary publication"
    [ statementFailure
        "review-duplicate-impl-declaration"
        2
        TypedDuplicateDeclaration
        (TypedImplDetail duplicateImplDeclarationId)
    ]
    (validateTypedProgram duplicateImplDeclarationProgram)
  assertEqual
    "or-patterns require at least one alternative"
    [ patternFailure
        "review-empty-or-pattern"
        TypedPatternShapeMismatch
        (TypedArityDetail 1 0)
    ]
    (validateTypedProgram emptyOrPatternProgram)
  assertEqual
    "explicit type applications reject ordinary application results"
    [ expressionFailureAt
        "review-non-binding-type-application"
        1
        TypedInstantiationMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram nonBindingTypeApplicationProgram)
  assertEqual
    "resolved operator bindings match their operator symbols"
    [ expressionFailureAt
        "review-mismatched-resolved-operator"
        1
        TypedBindingValueMismatch
        (TypedTextDetail "-")
    ]
    (validateTypedProgram mismatchedResolvedOperatorProgram)
  assertEqual
    "data interfaces retain local field-type metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath ["Library", "DataDependency"])
        TypedModuleInterfaceMismatch
        (TypedNameDetail dataInterfaceDependencyHiddenName)
    ]
    (validateTypedProgram dataInterfaceDependencyProgram)
  assertEqual
    "class method interfaces retain local type metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath ["Library", "ClassMethodDependency"])
        TypedModuleInterfaceMismatch
        (TypedNameDetail classMethodInterfaceDependencyDataName)
    ]
    (validateTypedProgram classMethodInterfaceDependencyProgram)

testPostNewestBotReviewRegressions :: IO ()
testPostNewestBotReviewRegressions = do
  assertEqual
    "instantiations satisfy their substituted primitive constraints"
    [ expressionFailureAt
        "review-instantiated-primitive-constraints"
        2
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType TypedBoolType),
      expressionFailureAt
        "review-instantiated-primitive-constraints"
        3
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType boolToBoolType)
    ]
    (validateTypedProgram instantiatedPrimitiveConstraintProgram)
  assertEqual
    "type applications reject extra unrelated instantiation owners"
    [ expressionFailureAt
        "review-type-application-extra-owner"
        2
        TypedInstantiationMismatch
        (TypedBinderDetail typeApplicationExtraOwner)
    ]
    (validateTypedProgram typeApplicationExtraOwnerProgram)
  assertEqual
    "resolved operators require evidence for constrained schemes"
    [ expressionFailureAt
        "review-constrained-resolved-operator"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail constrainedResolvedOperatorOwner),
      expressionFailureAt
        "review-constrained-resolved-operator"
        1
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram constrainedResolvedOperatorProgram)
  assertEqual
    "modules require a terminal result contract"
    [ moduleFailure
        "review-missing-module-result"
        TypedModuleResultMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram missingModuleResultProgram)
  assertEqual
    "data declarations require at least one constructor"
    [ statementFailure
        "review-empty-data-declaration"
        0
        TypedDataRecipeMismatch
        (TypedArityDetail 1 0)
    ]
    (validateTypedProgram emptyDataDeclarationProgram)
  assertEqual
    "duplicate binder scanning visits every or-pattern alternative"
    [ TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-later-or-pattern-binder-collision"] 1 [0, 0, 1])
        TypedDuplicateBinder
        (TypedBinderDetail laterOrPatternCollidingBinder)
    ]
    (validateTypedProgram laterOrPatternBinderCollisionProgram)
  assertEqual
    "concrete integer literals fit their selected integral widths"
    [ expressionFailureAt
        "review-concrete-integer-bounds"
        0
        TypedLiteralTypeMismatch
        (TypedTypeDetail TypedIntType (TypedNumericType TypedUInt8Type)),
      expressionFailureAt
        "review-concrete-integer-bounds"
        1
        TypedLiteralTypeMismatch
        (TypedTypeDetail TypedIntType (TypedNumericType TypedUInt8Type))
    ]
    (validateTypedProgram concreteIntegerBoundsProgram)
  assertEqual
    "impl declarations provide every class method"
    [ statementFailure
        "review-incomplete-impl"
        1
        TypedMethodSelectionMismatch
        (TypedTextDetail "map"),
      statementFailure
        "review-incomplete-impl"
        1
        TypedMethodSelectionMismatch
        (TypedTextDetail "render")
    ]
    (validateTypedProgram incompleteImplProgram)
  assertEqual
    "one expression cannot instantiate the same owner twice"
    [ expressionFailureAt
        "review-duplicate-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail duplicateInstantiationOwner)
    ]
    (validateTypedProgram duplicateInstantiationProgram)

testFractionalLiteralBounds :: IO ()
testFractionalLiteralBounds =
  assertEqual
    "fractional literals fit their selected floating widths"
    [ expressionFailureAt
        "review-fractional-literal-bounds"
        1
        TypedLiteralTypeMismatch
        (TypedTypeDetail (TypedNumericType TypedFloat16Type) (TypedNumericType TypedFloat16Type)),
      expressionFailureAt
        "review-fractional-literal-bounds"
        2
        TypedLiteralTypeMismatch
        (TypedTypeDetail (TypedNumericType TypedFloat16Type) (TypedNumericType TypedFloat16Type)),
      expressionFailureAt
        "review-fractional-literal-bounds"
        4
        TypedLiteralTypeMismatch
        (TypedTypeDetail (TypedNumericType TypedFloat32Type) (TypedNumericType TypedFloat32Type)),
      expressionFailureAt
        "review-fractional-literal-bounds"
        6
        TypedLiteralTypeMismatch
        (TypedTypeDetail (TypedNumericType TypedFloat64Type) (TypedNumericType TypedFloat64Type))
    ]
    (validateTypedProgram fractionalLiteralBoundsProgram)

testVisibleClassCollisions :: IO ()
testVisibleClassCollisions =
  assertEqual
    "local classes do not collide with visible Prelude or imported classes"
    [ statementFailure
        "review-visible-class-collision"
        0
        TypedDuplicateDeclaration
        (TypedNameDetail visibleClassCollisionPreludeName),
      statementFailure
        "review-visible-class-collision"
        1
        TypedDuplicateDeclaration
        (TypedNameDetail visibleClassCollisionImportedName)
    ]
    (validateTypedProgram visibleClassCollisionProgram)

testSelectedClassDataDependency :: IO ()
testSelectedClassDataDependency =
  assertEqual
    "selective class imports retain data metadata used by method contracts"
    []
    (validateTypedProgram selectedClassDataDependencyProgram)

testLexicalSchemeShadowing :: IO ()
testLexicalSchemeShadowing =
  assertEqual
    "nearest block schemes shadow outer schemes by lexical name"
    []
    (validateTypedProgram lexicalSchemeShadowingProgram)

testFullyAppliedMethodCandidates :: IO ()
testFullyAppliedMethodCandidates =
  assertEqual
    "fully applied qualified methods require a unique selected body"
    [ expressionFailureAt
        "review-fully-applied-method-candidates"
        1
        TypedAmbiguousEvidence
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram fullyAppliedMethodCandidatesProgram)

testDuplicateUnboundEvidence :: IO ()
testDuplicateUnboundEvidence =
  assertEqual
    "unbound selected evidence is unique per resolved constraint"
    [ expressionFailure
        "review-duplicate-unbound-evidence"
        TypedDuplicateEvidence
        (TypedTextDetail "Equal")
    ]
    (validateTypedProgram duplicateUnboundEvidenceProgram)

testGeneralizedClassMethodImport :: IO ()
testGeneralizedClassMethodImport =
  assertEqual
    "imported class methods quantify their class parameters as values"
    []
    (validateTypedProgram generalizedClassMethodImportProgram)

testImportedClassCollision :: IO ()
testImportedClassCollision =
  assertEqual
    "constraints reject colliding imported class identifiers"
    [ statementFailure
        "review-imported-class-collision"
        0
        TypedDuplicateDeclaration
        (TypedTextDetail "Clash")
    ]
    (validateTypedProgram importedClassCollisionProgram)

testForwardBlockReference :: IO ()
testForwardBlockReference =
  assertEqual
    "block expressions cannot see later non-recursive declarations"
    [ TypedCoreValidationFailure
        (TypedExpressionPath ["Fixture", "review-forward-block-reference"] 1 [0])
        TypedInvisibleName
        (TypedNameDetail forwardBlockReferenceName)
    ]
    (validateTypedProgram forwardBlockReferenceProgram)

testRecursiveBlockPeers :: IO ()
testRecursiveBlockPeers =
  assertEqual
    "ordered block scope preserves proven recursive peers"
    []
    (validateTypedProgram recursiveBlockPeerProgram)

testMalformedLiteralConstraintBounds :: IO ()
testMalformedLiteralConstraintBounds =
  assertEqual
    "generalized literal constraints validate decimal syntax and bound order"
    [ statementFailure
        "review-malformed-literal-constraint-bounds"
        0
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType (TypedTypeParameterType (TypedTypeParameterId 0))),
      statementFailure
        "review-malformed-literal-constraint-bounds"
        1
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType (TypedTypeParameterType (TypedTypeParameterId 0)))
    ]
    (validateTypedProgram malformedLiteralConstraintBoundsProgram)

testEvidenceSelectionOrder :: IO ()
testEvidenceSelectionOrder =
  assertEqual
    "selected evidence follows the generalized scheme obligation order"
    [ expressionFailureAt
        "review-evidence-selection-order"
        1
        TypedInstantiationMismatch
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 1)),
      expressionFailureAt
        "review-evidence-selection-order"
        1
        TypedInstantiationMismatch
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram evidenceSelectionOrderProgram)

testPrivateCapabilityMetadataVisibility :: IO ()
testPrivateCapabilityMetadataVisibility =
  assertEqual
    "dependency-only capability metadata does not collide with a local class"
    []
    (validateTypedProgram privateCapabilityMetadataVisibilityProgram)

testModuleQualifiedMethodKey :: IO ()
testModuleQualifiedMethodKey =
  assertEqual
    "module-qualified capability names use the final method separator"
    []
    (validateTypedProgram moduleQualifiedMethodKeyProgram)

testImportedDataDependencyMetadata :: IO ()
testImportedDataDependencyMetadata =
  assertEqual
    "exported schemes retain imported data metadata"
    []
    (validateTypedProgram importedDataDependencyProgram)

testTransitiveDataContractDependency :: IO ()
testTransitiveDataContractDependency =
  assertEqual
    "selected data contracts retain transitive field metadata"
    []
    (validateTypedProgram transitiveDataContractDependencyProgram)

testImportedCapabilityDependency :: IO ()
testImportedCapabilityDependency =
  assertEqual
    "interfaces reject constraints whose imported capability identity cannot be retained"
    [ TypedCoreValidationFailure
        (TypedInterfacePath importedCapabilityFacadePath)
        TypedModuleInterfaceMismatch
        (TypedNameDetail (resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq"))
    ]
    (validateTypedProgram importedCapabilityDependencyProgram)

testMetadataOnlyImplVisibility :: IO ()
testMetadataOnlyImplVisibility =
  assertEqual
    "dependency-only capability metadata does not expose impl evidence"
    [ expressionFailureAt
        "review-metadata-only-impl-visibility"
        0
        TypedInvisibleName
        (TypedNameDetail metadataOnlyImportedCapabilityName),
      expressionFailureAt
        "review-metadata-only-impl-visibility"
        0
        TypedInvisibleImpl
        (TypedImplDetail metadataOnlyImportedImpl)
    ]
    (validateTypedProgram metadataOnlyImplVisibilityProgram)

testPatternExpressionMetadata :: IO ()
testPatternExpressionMetadata =
  assertEqual
    "patterns reject expression-only instantiation and evidence metadata"
    [ TypedCoreValidationFailure
        (TypedPatternPath ["Fixture", "review-pattern-expression-metadata"] 3 [0, 0])
        TypedPatternShapeMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram patternExpressionMetadataProgram)

testPhantomDataEquality :: IO ()
testPhantomDataEquality =
  assertEqual
    "phantom data arguments do not determine structural equality support"
    []
    (validateTypedProgram phantomDataEqualityProgram)

testSameScopeValueRebinding :: IO ()
testSameScopeValueRebinding =
  assertEqual
    "ordinary value rebinding remains valid and last-wins"
    []
    (validateTypedProgram sameScopeValueRebindingProgram)

testForwardModuleReference :: IO ()
testForwardModuleReference =
  assertEqual
    "non-recursive module bindings cannot reference later declarations"
    [ expressionFailureAt
        "review-forward-module-reference"
        0
        TypedInvisibleName
        (TypedNameDetail (fixtureValueName "later"))
    ]
    (validateTypedProgram forwardModuleReferenceProgram)

testCyclicResolvedImports :: IO ()
testCyclicResolvedImports =
  assertEqual
    "resolved module graphs are acyclic"
    [ TypedCoreValidationFailure
        (TypedModulePath cyclicImportFirstPath)
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Cycle::Second"),
      TypedCoreValidationFailure
        (TypedModulePath cyclicImportSecondPath)
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Cycle::First")
    ]
    (validateTypedProgram cyclicImportProgram)

testBareSignatureVisibility :: IO ()
testBareSignatureVisibility =
  assertEqual
    "a signature without a value body is not an executable binding"
    [ expressionFailureAt
        "review-bare-signature-visibility"
        1
        TypedInvisibleName
        (TypedNameDetail bareSignatureValueName)
    ]
    (validateTypedProgram bareSignatureVisibilityProgram)

testActiveRebindingExport :: IO ()
testActiveRebindingExport =
  assertEqual
    "an interface cannot publish a shadowed rebinding contract"
    [ TypedCoreValidationFailure
        (TypedInterfacePath ["Fixture", "review-active-rebinding-export"])
        TypedModuleInterfaceMismatch
        (TypedNameDetail activeRebindingExportName)
    ]
    (validateTypedProgram activeRebindingExportProgram)

testConstructorInstantiation :: IO ()
testConstructorInstantiation =
  assertEqual
    "generic constructors own their instantiation metadata"
    []
    (validateTypedProgram constructorInstantiationProgram)

cyclicImportFirstPath :: [Text]
cyclicImportFirstPath = ["Cycle", "First"]

cyclicImportSecondPath :: [Text]
cyclicImportSecondPath = ["Cycle", "Second"]

cyclicImportProgram :: TypedProgram
cyclicImportProgram =
  TypedProgram
    Nothing
    [ moduleWithImport cyclicImportFirstPath cyclicImportSecondPath,
      moduleWithImport cyclicImportSecondPath cyclicImportFirstPath
    ]
    cyclicImportFirstPath
  where
    moduleWithImport modulePath importPath =
      typedModule
        modulePath
        (TypedSourcePath ("src/" <> Text.intercalate "/" modulePath <> ".jz"))
        [TypedResolvedImport span1 importPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo

bareSignatureValueName :: TypedCoreName
bareSignatureValueName =
  resolved TypedCurrentModule TypedValueNamespace "declaredOnly"

bareSignatureVisibilityProgram :: TypedProgram
bareSignatureVisibilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-bare-signature-visibility"
    modulePath = ["Fixture", fixture]
    owner = binder modulePath [0] bareSignatureValueName
    statements =
      [ TypedSignatureStatement owner bareSignatureValueName span1 (monoScheme owner),
        expressionStatement 1 (TypedVariableExpr boolInfo bareSignatureValueName)
      ]

activeRebindingExportName :: TypedCoreName
activeRebindingExportName =
  resolved TypedCurrentModule TypedValueNamespace "value"

activeRebindingExportProgram :: TypedProgram
activeRebindingExportProgram =
  singleModuleProgram fixture relativeSource exports statements interface unitInfo modulePath
  where
    fixture = "review-active-rebinding-export"
    modulePath = ["Fixture", fixture]
    firstOwner = binder modulePath [0] activeRebindingExportName
    secondOwner = binder modulePath [1] activeRebindingExportName
    firstScheme = monoScheme firstOwner
    secondScheme =
      TypedScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    exports = [TypedModuleExport TypedValueNamespace "value"]
    statements =
      [ TypedLetStatement firstOwner activeRebindingExportName span1 firstScheme trueExpr,
        TypedLetStatement
          secondOwner
          activeRebindingExportName
          span1
          secondScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "latest"))
      ]
    interface =
      TypedModuleInterface
        [TypedValueInterface activeRebindingExportName firstScheme]
        []
        []
        []

constructorInstantiationProgram :: TypedProgram
constructorInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface constructorInfo modulePath
  where
    fixture = "review-constructor-instantiation"
    modulePath = ["Fixture", fixture]
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    constructorOwner = binder modulePath [0, 0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameterId]
        [ TypedConstructorDeclaration
            constructorOwner
            constructorName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    instantiation =
      TypedInstantiation
        constructorOwner
        [TypedTypeArgument parameterId TypedBoolType]
        Nothing
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType dataName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe dataName [TypedBoolType]))
        [instantiation]
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (TypedVariableExpr constructorInfo constructorName)
      ]

lexicalSchemeShadowingProgram :: TypedProgram
lexicalSchemeShadowingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-lexical-scheme-shadowing"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    outerOwner = binder modulePath [0] valueName
    innerOwner = binder modulePath [1, 0] valueName
    innerUse = TypedVariableExpr textInfo valueName
    block =
      TypedBlockExpr
        textInfo
        [ TypedLetStatement
            innerOwner
            valueName
            span1
            (TypedScheme innerOwner [] [] [] TypedTextType TypedManagedTextRecipe)
            (TypedLiteralExpr textInfo (TypedTextLiteral "inner")),
          expressionStatement 2 innerUse
        ]
    statements =
      [ TypedLetStatement outerOwner valueName span1 (monoScheme outerOwner) trueExpr,
        expressionStatement 1 block
      ]

fullyAppliedMethodCandidatesProgram :: TypedProgram
fullyAppliedMethodCandidatesProgram =
  withFixturePrelude
    ( singleModuleProgram
        fixture
        relativeSource
        []
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                secondImpl
                [ fixtureImplMethod modulePath [0, 0] secondImpl "render",
                  fixtureImplMethod modulePath [0, 1] secondImpl "map"
                ]
            ),
          expressionStatement 1 expression
        ]
        emptyInterface
        listTextInfo
        modulePath
    )
  where
    fixture = "review-fully-applied-method-candidates"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId modulePath capabilityName [TypedTextType]
    constraint = TypedCapabilityConstraint "Render" (Just "Render.map") TypedTextType
    candidates =
      [ TypedEvidenceCandidate firstImpl (Just (TypedMethodId firstImpl "map")),
        TypedEvidenceCandidate secondImpl (Just (TypedMethodId secondImpl "map"))
      ]
    boolToTextType = TypedFunctionType TypedBoolType TypedTextType
    boolToTextRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe
    mapperName = resolved TypedCurrentModule TypedValueNamespace "mapperArgument"
    mapper =
      TypedLambdaExpr
        (info boolToTextType boolToTextRecipe)
        (binder modulePath [1, 0] mapperName)
        mapperName
        (TypedLiteralExpr textInfo (TypedTextLiteral "mapped"))
    intermediateType =
      TypedFunctionType
        (TypedListType TypedBoolType)
        (TypedListType TypedTextType)
    intermediateRecipe =
      TypedClosureRecipe
        [TypedManagedListRecipe TypedBoolRecipe]
        (TypedManagedListRecipe TypedManagedTextRecipe)
    intermediate =
      TypedApplyExpr
        (info intermediateType intermediateRecipe)
        (TypedVariableExpr builtinMapInfo (TypedBuiltinName "map"))
        mapper
    argument =
      TypedListExpr
        (info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe))
        [trueExpr]
    listTextInfo =
      TypedNodeInfo
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
        []
        [TypedEvidenceCandidates constraint candidates]
    expression = TypedApplyExpr listTextInfo intermediate argument

duplicateUnboundEvidenceProgram :: TypedProgram
duplicateUnboundEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-unbound-evidence"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    use = TypedEvidenceUse Nothing constraint implId Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (TypedBooleanLiteral True)

generalizedClassMethodImportProgram :: TypedProgram
generalizedClassMethodImportProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "GeneralizedClassMethod"]
    entryPath = ["Fixture", "review-generalized-class-method-import"]
    parameter = TypedTypeParameterId 0
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    methodName = resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder libraryPath [0, 0] methodName
    methodType =
      TypedFunctionType
        (TypedTypeParameterType parameter)
        TypedTextType
    methodRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        TypedManagedTextRecipe
    methodScheme =
      TypedScheme methodOwner [] [] [] methodType methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/GeneralizedClassMethod.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Display",
          TypedModuleExport TypedValueNamespace "display"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [TypedClassStatement classDeclaration]
        unitInfo
    importedMethodName =
      resolved (TypedImportedModule libraryPath) TypedValueNamespace "display"
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
        [ TypedInstantiation
            methodOwner
            [TypedTypeArgument parameter TypedBoolType]
            Nothing
        ]
        []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["display"])]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr instantiatedInfo importedMethodName)]
        instantiatedInfo

importedClassCollisionProgram :: TypedProgram
importedClassCollisionProgram =
  TypedProgram Nothing [firstLibrary, secondLibrary, entryModule] entryPath
  where
    firstPath = ["Library", "FirstClash"]
    secondPath = ["Library", "SecondClash"]
    entryPath = ["Fixture", "review-imported-class-collision"]
    parameter = TypedTypeParameterId 0
    libraryModule libraryPath =
      let className = resolved TypedCurrentModule TypedCapabilityNamespace "Clash"
          declaration = TypedClassDeclaration span1 className [parameter] []
       in typedModule
            libraryPath
            (TypedSourcePath ("src/" <> Text.intercalate "/" libraryPath <> ".jz"))
            []
            [TypedModuleExport TypedCapabilityNamespace "Clash"]
            (TypedModuleInterface [] [] [TypedClassInterface declaration] [])
            [TypedClassStatement declaration]
            unitInfo
    firstLibrary = libraryModule firstPath
    secondLibrary = libraryModule secondPath
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder entryPath [0] valueName
    constraint = TypedCapabilityConstraint "Clash" Nothing TypedBoolType
    scheme =
      TypedScheme
        valueOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 firstPath Nothing (Just ["Clash"]),
          TypedResolvedImport span1 secondPath Nothing (Just ["Clash"])
        ]
        []
        emptyInterface
        [ TypedSignatureStatement valueOwner valueName span1 scheme,
          expressionStatement 1 trueExpr
        ]
        boolInfo

forwardBlockReferenceName :: TypedCoreName
forwardBlockReferenceName =
  resolved TypedCurrentModule TypedValueNamespace "later"

forwardBlockReferenceProgram :: TypedProgram
forwardBlockReferenceProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-forward-block-reference"
    modulePath = ["Fixture", fixture]
    owner = binder modulePath [0, 1] forwardBlockReferenceName
    block =
      TypedBlockExpr
        boolInfo
        [ expressionStatement 2 (TypedVariableExpr boolInfo forwardBlockReferenceName),
          TypedLetStatement owner forwardBlockReferenceName span1 (monoScheme owner) trueExpr,
          expressionStatement 3 trueExpr
        ]

recursiveBlockPeerProgram :: TypedProgram
recursiveBlockPeerProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-recursive-block-peers"
    modulePath = ["Fixture", fixture]
    leftName = resolved TypedCurrentModule TypedValueNamespace "left"
    rightName = resolved TypedCurrentModule TypedValueNamespace "right"
    leftOwner = binder modulePath [0, 0] leftName
    rightOwner = binder modulePath [0, 1] rightName
    recursiveLambda ownerPath argumentName peerName =
      TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath ownerPath argumentName)
        argumentName
        ( TypedApplyExpr
            boolInfo
            (TypedVariableExpr boolToBoolInfo peerName)
            (TypedVariableExpr boolInfo argumentName)
        )
    leftArgument = resolved TypedCurrentModule TypedValueNamespace "leftArgument"
    rightArgument = resolved TypedCurrentModule TypedValueNamespace "rightArgument"
    leftStatement =
      TypedLetStatement
        leftOwner
        leftName
        span1
        (TypedScheme leftOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 0, 0] leftArgument rightName)
    rightStatement =
      TypedLetStatement
        rightOwner
        rightName
        span1
        (TypedScheme rightOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 1, 0] rightArgument leftName)
    block =
      TypedBlockExpr
        boolToBoolInfo
        [ leftStatement,
          rightStatement,
          expressionStatement 3 (TypedVariableExpr boolToBoolInfo leftName)
        ]

malformedLiteralConstraintBoundsProgram :: TypedProgram
malformedLiteralConstraintBoundsProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-literal-constraint-bounds"
    modulePath = ["Fixture", fixture]
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    signature statementIndex suffix lower upper =
      let valueName = resolved TypedCurrentModule TypedValueNamespace suffix
          owner = binder modulePath [statementIndex] valueName
          scheme =
            TypedScheme
              owner
              [parameter]
              []
              [TypedNumericPrimitiveConstraint (TypedIntegralLiteralNumericConstraint lower upper) parameterType]
              TypedBoolType
              TypedBoolRecipe
       in TypedSignatureStatement owner valueName span1 scheme
    statements =
      [ signature 0 "reversed" "10" "2",
        signature 1 "nonDecimal" "zero" "10"
      ]

evidenceSelectionOrderProgram :: TypedProgram
evidenceSelectionOrderProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-evidence-selection-order"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    owner = binder modulePath [0] valueName
    firstParameter = TypedEvidenceParameterId 0
    secondParameter = TypedEvidenceParameterId 1
    firstConstraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    secondConstraint = TypedCapabilityConstraint "Equal" Nothing TypedCharType
    scheme =
      TypedScheme
        owner
        []
        [ TypedEvidenceParameter firstParameter firstConstraint,
          TypedEvidenceParameter secondParameter secondConstraint
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    selection parameter constraint target =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef owner parameter))
            constraint
            (TypedImplId ["Prelude"] capabilityName [target])
            Nothing
        )
    expression =
      TypedVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [TypedInstantiation owner [] Nothing]
            [ selection secondParameter secondConstraint TypedCharType,
              selection firstParameter firstConstraint TypedBoolType
            ]
        )
        valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

privateCapabilityMetadataVisibilityProgram :: TypedProgram
privateCapabilityMetadataVisibilityProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "PrivateCapabilityMetadata"]
    entryPath = ["Fixture", "review-private-capability-metadata-visibility"]
    parameter = TypedTypeParameterId 0
    libraryCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    libraryClass =
      TypedClassDeclaration span1 libraryCapabilityName [parameter] []
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder libraryPath [1] valueName
    valueScheme =
      TypedScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint "PrivateEq" Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/PrivateCapabilityMetadata.jz")
        []
        [TypedModuleExport TypedValueNamespace "constrained"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            []
            [TypedClassInterface libraryClass]
            []
        )
        [ TypedClassStatement libraryClass,
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["constrained"])]
        []
        emptyInterface
        [ TypedClassStatement
            (TypedClassDeclaration span1 localCapabilityName [parameter] []),
          expressionStatement 1 trueExpr
        ]
        boolInfo

moduleQualifiedMethodKeyProgram :: TypedProgram
moduleQualifiedMethodKeyProgram =
  TypedProgram (Just preludeModule) [entryModule] entryPath
  where
    preludePath = ["Prelude"]
    entryPath = ["Fixture", "review-module-qualified-method-key"]
    capabilityIdentifier = "Lib::Api::Make"
    qualifiedMethod = "Lib::Api::Make::make"
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace capabilityIdentifier
    methodName = resolved TypedCurrentModule TypedValueNamespace "make"
    methodOwner = binder preludePath [0, 0] methodName
    methodScheme = monoScheme methodOwner
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId preludePath capabilityName [TypedBoolType]
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "make")
        (binder preludePath [1, 0] methodName)
        methodName
        span1
        trueExpr
    preludeModule =
      typedModule
        preludePath
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace capabilityIdentifier]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface implId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo
    importedCapabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace capabilityIdentifier
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint capabilityIdentifier (Just qualifiedMethod) TypedBoolType)
        (TypedImplId preludePath importedCapabilityName [TypedBoolType])
        ( Just
            ( TypedMethodId
                (TypedImplId preludePath importedCapabilityName [TypedBoolType])
                "make"
            )
        )
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 0 expression]
        boolInfo

importedDataDependencyProgram :: TypedProgram
importedDataDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule, entryModule] entryPath
  where
    providerPath = ["Library", "ImportedDataProvider"]
    facadePath = ["Library", "ImportedDataFacade"]
    entryPath = ["Fixture", "review-imported-data-dependency"]
    providerBoxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    importedBoxName =
      resolved (TypedImportedModule providerPath) TypedTypeNamespace "Box"
    importedBoxConstructor =
      resolved (TypedImportedModule providerPath) TypedConstructorNamespace "Box"
    boxDeclaration =
      dataDeclarationWithNullaryConstructor
        providerPath
        [0, 0]
        providerBoxName
        []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ImportedDataProvider.jz")
        []
        [ TypedModuleExport TypedTypeNamespace "Box",
          TypedModuleExport TypedConstructorNamespace "Box"
        ]
        (TypedModuleInterface [] [TypedDataInterface boxDeclaration] [] [])
        [TypedDataStatement boxDeclaration]
        unitInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    importedValueName =
      resolved (TypedImportedModule facadePath) TypedValueNamespace "published"
    valueOwner = binder facadePath [0] valueName
    boxType = TypedDataType importedBoxName []
    boxRecipe = TypedManagedVariantRecipe importedBoxName []
    valueScheme =
      TypedScheme valueOwner [] [] [] boxType boxRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/ImportedDataFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["Box"])]
        [TypedModuleExport TypedValueNamespace "published"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [ TypedLetStatement
            valueOwner
            valueName
            span1
            valueScheme
            (TypedVariableExpr (info boxType boxRecipe) importedBoxConstructor)
        ]
        unitInfo
    entryInfo = info boxType boxRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr entryInfo importedValueName)]
        entryInfo

transitiveDataContractDependencyProgram :: TypedProgram
transitiveDataContractDependencyProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "TransitiveDataContract"]
    entryPath = ["Fixture", "review-transitive-data-contract-dependency"]
    hiddenName = resolved TypedCurrentModule TypedTypeNamespace "Hidden"
    boxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    boxConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    hiddenType = TypedDataType hiddenName []
    boxType = TypedDataType boxName []
    hiddenDeclaration =
      dataDeclarationWithNullaryConstructor libraryPath [0, 0] hiddenName []
    boxDeclaration =
      TypedDataDeclaration
        span1
        boxName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [1, 0] boxConstructorName)
            boxConstructorName
            [hiddenType]
            [TypedManagedVariantRecipe hiddenName []]
        ]
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    importedValueName =
      resolved (TypedImportedModule libraryPath) TypedValueNamespace "published"
    valueOwner = binder libraryPath [2] valueName
    parameter = TypedTypeParameterId 0
    valueScheme =
      TypedScheme
        valueOwner
        [parameter]
        []
        [TypedStrictEqualityPrimitiveConstraint boxType]
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TransitiveDataContract.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            [ TypedDataInterface hiddenDeclaration,
              TypedDataInterface boxDeclaration
            ]
            []
            []
        )
        [ TypedDataStatement hiddenDeclaration,
          TypedDataStatement boxDeclaration,
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    instantiation =
      TypedInstantiation
        valueOwner
        [TypedTypeArgument parameter TypedBoolType]
        Nothing
    entryInfo =
      TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr entryInfo importedValueName)]
        entryInfo

importedCapabilityFacadePath :: [Text]
importedCapabilityFacadePath = ["Library", "ImportedCapabilityFacade"]

importedCapabilityDependencyProgram :: TypedProgram
importedCapabilityDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule] importedCapabilityFacadePath
  where
    providerPath = ["Library", "ImportedCapabilityProvider"]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq"
    parameter = TypedTypeParameterId 0
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ImportedCapabilityProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ForeignEq"]
        (TypedModuleInterface [] [] [TypedClassInterface capability] [])
        [TypedClassStatement capability]
        unitInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    valueOwner = binder importedCapabilityFacadePath [0] valueName
    valueScheme =
      TypedScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint "ForeignEq" Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        importedCapabilityFacadePath
        (TypedSourcePath "src/Library/ImportedCapabilityFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["ForeignEq"])]
        [TypedModuleExport TypedValueNamespace "published"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        unitInfo

metadataOnlyImportedCapabilityName :: TypedCoreName
metadataOnlyImportedCapabilityName =
  resolved
    (TypedImportedModule ["Library", "MetadataOnlyImpl"])
    TypedCapabilityNamespace
    "PrivateEq"

metadataOnlyImportedImpl :: TypedImplId
metadataOnlyImportedImpl =
  TypedImplId
    ["Library", "MetadataOnlyImpl"]
    metadataOnlyImportedCapabilityName
    [TypedBoolType]

metadataOnlyImplVisibilityProgram :: TypedProgram
metadataOnlyImplVisibilityProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "MetadataOnlyImpl"]
    entryPath = ["Fixture", "review-metadata-only-impl-visibility"]
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    localImpl =
      TypedImplId libraryPath capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder libraryPath [2] valueName
    constraint =
      TypedCapabilityConstraint "PrivateEq" Nothing TypedBoolType
    valueScheme =
      TypedScheme
        valueOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MetadataOnlyImpl.jz")
        []
        [TypedModuleExport TypedValueNamespace "constrained"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            []
            [TypedClassInterface capability]
            [TypedImplInterface localImpl]
        )
        [ TypedClassStatement capability,
          TypedImplStatement (TypedImplDeclaration span1 localImpl []),
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    evidenceUse =
      TypedEvidenceUse Nothing constraint metadataOnlyImportedImpl Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["constrained"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

patternExpressionMetadataProgram :: TypedProgram
patternExpressionMetadataProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-pattern-expression-metadata"
    modulePath = ["Fixture", fixture]
    genericName = fixtureValueName "generic"
    genericOwner = binder modulePath [0] genericName
    parameter = TypedTypeParameterId 0
    genericScheme =
      TypedScheme genericOwner [parameter] [] [] TypedBoolType TypedBoolRecipe
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PatternMarker"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint "PatternMarker" Nothing TypedBoolType
    patternInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation genericOwner [TypedTypeArgument parameter TypedBoolType] Nothing]
        [TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)]
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedWildcardPattern patternInfo) Nothing trueExpr]
    statements =
      [ TypedLetStatement genericOwner genericName span1 genericScheme trueExpr,
        TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 4 expression
      ]

phantomDataEqualityProgram :: TypedProgram
phantomDataEqualityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-phantom-data-equality"
    modulePath = ["Fixture", fixture]
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Phantom"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Phantom"
    parameter = TypedTypeParameterId 0
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    valueName = fixtureValueName "phantomEquality"
    valueOwner = binder modulePath [1] valueName
    phantomFunctionType = TypedDataType dataName [boolToBoolType]
    scheme =
      TypedScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint phantomFunctionType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedSignatureStatement valueOwner valueName span1 scheme
      ]

sameScopeValueRebindingProgram :: TypedProgram
sameScopeValueRebindingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-same-scope-value-rebinding"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "value"
    firstOwner = binder modulePath [0] valueName
    secondOwner = binder modulePath [1] valueName
    firstScheme =
      TypedScheme firstOwner [] [] [] TypedBoolType TypedBoolRecipe
    secondScheme =
      TypedScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedLetStatement firstOwner valueName span1 firstScheme trueExpr,
        TypedLetStatement
          secondOwner
          valueName
          span1
          secondScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "latest")),
        expressionStatement 3 (TypedVariableExpr textInfo valueName)
      ]

forwardModuleReferenceProgram :: TypedProgram
forwardModuleReferenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-forward-module-reference"
    modulePath = ["Fixture", fixture]
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstOwner = binder modulePath [0] firstName
    laterOwner = binder modulePath [1] laterName
    statements =
      [ TypedLetStatement
          firstOwner
          firstName
          span1
          (monoScheme firstOwner)
          (TypedVariableExpr boolInfo laterName),
        TypedLetStatement laterOwner laterName span1 (monoScheme laterOwner) trueExpr,
        expressionStatement 3 (TypedVariableExpr boolInfo firstName)
      ]

missingPolymorphicInstantiationOwner :: TypedBinderId
missingPolymorphicInstantiationOwner =
  fixtureBinder
    "review-missing-polymorphic-instantiation"
    0
    (fixtureValueName "identity")

missingPolymorphicInstantiationProgram :: TypedProgram
missingPolymorphicInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-missing-polymorphic-instantiation"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "identity"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      TypedScheme
        missingPolymorphicInstantiationOwner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    statements =
      [ TypedLetStatement
          missingPolymorphicInstantiationOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 (TypedVariableExpr boolToBoolInfo valueName)
      ]

unsupportedEqualityDataName :: TypedCoreName
unsupportedEqualityDataName = resolved TypedCurrentModule TypedTypeNamespace "CallableBox"

unsupportedEqualityDataType :: TypedType
unsupportedEqualityDataType = TypedDataType unsupportedEqualityDataName []

unsupportedStrictEqualityConstraintProgram :: TypedProgram
unsupportedStrictEqualityConstraintProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unsupported-strict-equality-constraint"
    modulePath = ["Fixture", fixture]
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "CallableBox"
    dataDeclaration =
      TypedDataDeclaration
        span1
        unsupportedEqualityDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            [boolToBoolType]
            [boolToBoolRecipe]
        ]
    functionName = fixtureValueName "functionEquality"
    functionOwner = binder modulePath [1] functionName
    dataName = fixtureValueName "dataEquality"
    dataOwner = binder modulePath [2] dataName
    constrained owner target =
      TypedSignatureStatement
        owner
        (case owner of TypedBinderId (_, _, name) -> name)
        span1
        (TypedScheme owner [] [] [TypedStrictEqualityPrimitiveConstraint target] TypedBoolType TypedBoolRecipe)
    statements =
      [ TypedDataStatement dataDeclaration,
        constrained functionOwner boolToBoolType,
        constrained dataOwner unsupportedEqualityDataType
      ]

uncheckedSpecialNameProgram :: TypedProgram
uncheckedSpecialNameProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface builtinMapInfo ["Fixture", fixture]
  where
    fixture = "review-unchecked-special-name"
    statements =
      [ expressionStatement 1 (TypedVariableExpr boolInfo (TypedBuiltinName "doesNotExist")),
        expressionStatement 2 (TypedVariableExpr boolInfo (TypedGeneratedName TypedOperatorSectionFunction)),
        expressionStatement 3 (TypedVariableExpr builtinMapInfo (TypedBuiltinName "map"))
      ]

classMethodExportProgram :: TypedProgram
classMethodExportProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "ClassMethodExport"]
    entryPath = ["Fixture", "review-class-method-export"]
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    methodScheme = TypedScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    declaration = TypedClassDeclaration span1 className [TypedTypeParameterId 0] [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/ClassMethodExport.jz")
        []
        [ TypedModuleExport TypedTypeNamespace "Render",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface declaration] [])
        [TypedClassStatement declaration]
        boolInfo
    importedMethod = resolved (TypedImportedModule libraryPath) TypedValueNamespace "render"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["render"])]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr boolToBoolInfo importedMethod)]
        boolToBoolInfo

fractionalLiteralSuffixProgram :: TypedProgram
fractionalLiteralSuffixProgram =
  expressionFixtureProgram
    "review-fractional-literal-suffix"
    ( TypedLiteralExpr
        (info (TypedNumericType TypedFloat64Type) (TypedFloatRecipe 64))
        (TypedFractionalLiteral "1" "5" (Just TypedFloat16Type))
    )

fixturePrelude :: TypedModule
fixturePrelude =
  typedModule
    ["Prelude"]
    (TypedSourcePath "src/Prelude.jz")
    []
    [ TypedModuleExport TypedCapabilityNamespace "Equal",
      TypedModuleExport TypedCapabilityNamespace "Render"
    ]
    ( TypedModuleInterface
        []
        []
        [TypedClassInterface equalityClass, TypedClassInterface renderClass]
        [TypedImplInterface boolImpl, TypedImplInterface charImpl, TypedImplInterface textRenderImpl]
    )
    [ TypedClassStatement equalityClass,
      TypedClassStatement renderClass,
      TypedImplStatement (TypedImplDeclaration span1 boolImpl [equalImplMethod, otherBoolImplMethod]),
      TypedImplStatement (TypedImplDeclaration span1 charImpl [equalCharImplMethod, otherCharImplMethod]),
      TypedImplStatement (TypedImplDeclaration span1 textRenderImpl [renderImplMethod, mapImplMethod])
    ]
    boolInfo
  where
    parameter = TypedTypeParameterId 0
    equalClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    renderClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    equalName = resolved TypedCurrentModule TypedValueNamespace "equal"
    otherName = resolved TypedCurrentModule TypedValueNamespace "other"
    renderName = resolved TypedCurrentModule TypedValueNamespace "render"
    mapName = resolved TypedCurrentModule TypedValueNamespace "map"
    equalOwner = binder ["Prelude"] [0, 0] equalName
    otherOwner = binder ["Prelude"] [0, 1] otherName
    renderOwner = binder ["Prelude"] [1, 0] renderName
    mapOwner = binder ["Prelude"] [1, 1] mapName
    equalityClass =
      TypedClassDeclaration
        span1
        equalClassName
        [parameter]
        [ TypedMethodSignature equalName span1 (monoScheme equalOwner),
          TypedMethodSignature otherName span1 (TypedScheme otherOwner [] [] [] boolToBoolType boolToBoolRecipe)
        ]
    renderClass =
      TypedClassDeclaration
        span1
        renderClassName
        [parameter]
        [ TypedMethodSignature renderName span1 (TypedScheme renderOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedMethodSignature mapName span1 (TypedScheme mapOwner [] [] [] boolToBoolType boolToBoolRecipe)
        ]
    boolImpl = TypedImplId ["Prelude"] equalClassName [TypedBoolType]
    charImpl = TypedImplId ["Prelude"] equalClassName [TypedCharType]
    textRenderImpl = TypedImplId ["Prelude"] renderClassName [TypedTextType]
    equalImplMethod = TypedMethodDefinition (TypedMethodId boolImpl "equal") (binder ["Prelude"] [2, 0] equalName) equalName span1 trueExpr
    otherBoolArgument = resolved TypedCurrentModule TypedValueNamespace "otherBoolArgument"
    otherBoolExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [2, 1, 0] otherBoolArgument) otherBoolArgument trueExpr
    otherBoolImplMethod = TypedMethodDefinition (TypedMethodId boolImpl "other") (binder ["Prelude"] [2, 1] otherName) otherName span1 otherBoolExpression
    equalCharImplMethod = TypedMethodDefinition (TypedMethodId charImpl "equal") (binder ["Prelude"] [3, 0] equalName) equalName span1 trueExpr
    otherCharArgument = resolved TypedCurrentModule TypedValueNamespace "otherCharArgument"
    otherCharExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [3, 1, 0] otherCharArgument) otherCharArgument trueExpr
    otherCharImplMethod = TypedMethodDefinition (TypedMethodId charImpl "other") (binder ["Prelude"] [3, 1] otherName) otherName span1 otherCharExpression
    renderArgument = resolved TypedCurrentModule TypedValueNamespace "renderArgument"
    renderExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [4, 0, 0] renderArgument) renderArgument trueExpr
    renderImplMethod = TypedMethodDefinition (TypedMethodId textRenderImpl "render") (binder ["Prelude"] [4, 0] renderName) renderName span1 renderExpression
    mapArgument = resolved TypedCurrentModule TypedValueNamespace "mapArgument"
    mapExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [4, 1, 0] mapArgument) mapArgument trueExpr
    mapImplMethod = TypedMethodDefinition (TypedMethodId textRenderImpl "map") (binder ["Prelude"] [4, 1] mapName) mapName span1 mapExpression

fixtureImplMethod :: [Text] -> [Int] -> TypedImplId -> Text -> TypedMethodDefinition
fixtureImplMethod modulePath methodPath implId methodKey =
  TypedMethodDefinition
    (TypedMethodId implId methodKey)
    (binder modulePath methodPath methodName)
    methodName
    span1
    methodExpression
  where
    methodName = resolved TypedCurrentModule TypedValueNamespace methodKey
    argumentName = resolved TypedCurrentModule TypedValueNamespace (methodKey <> "Argument")
    methodExpression
      | methodKey == "equal" = trueExpr
      | otherwise =
          TypedLambdaExpr
            boolToBoolInfo
            (binder modulePath (methodPath <> [0]) argumentName)
            argumentName
            trueExpr

withFixturePrelude :: TypedProgram -> TypedProgram
withFixturePrelude (TypedProgram _ modules entryModule) =
  TypedProgram (Just fixturePrelude) modules entryModule

missingPreludeImplId :: TypedImplId
missingPreludeImplId =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedTextType]

missingPreludeImplProgram :: TypedProgram
missingPreludeImplProgram =
  TypedProgram (Just fixturePrelude) [entryModule] modulePath
  where
    fixture = "review-missing-prelude-impl"
    modulePath = ["Fixture", fixture]
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedTextType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint missingPreludeImplId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule = typedModule modulePath relativeSource [] [] emptyInterface [expressionStatement 1 expression] boolInfo

evidenceTypeScopeParameter :: TypedTypeParameterId
evidenceTypeScopeParameter = TypedTypeParameterId 0

evidenceTypeScopeProgram :: TypedProgram
evidenceTypeScopeProgram =
  TypedProgram (Just fixturePrelude) [entryModule] modulePath
  where
    fixture = "review-evidence-type-scope"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "generic"
    owner = binder modulePath [0] valueName
    parameterType = TypedTypeParameterType evidenceTypeScopeParameter
    scheme = TypedScheme owner [evidenceTypeScopeParameter] [] [] TypedBoolType TypedBoolRecipe
    implId =
      TypedImplId
        ["Prelude"]
        (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
        [TypedBoolType]
    constraint = TypedCapabilityConstraint "Equal" Nothing parameterType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule =
      typedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [TypedLetStatement owner valueName span1 scheme expression]
        boolInfo

wrongConstructorDataName :: TypedCoreName
wrongConstructorDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

wrongConstructorDataType :: TypedType
wrongConstructorDataType = TypedDataType wrongConstructorDataName []

wrongConstructorPatternTypeProgram :: TypedProgram
wrongConstructorPatternTypeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-wrong-constructor-pattern-type"
    modulePath = ["Fixture", fixture]
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "On"
    declaration =
      TypedDataDeclaration
        span1
        wrongConstructorDataName
        []
        [TypedConstructorDeclaration (binder modulePath [0, 0] constructorName) constructorName [] []]
    patternValue = TypedConstructorPattern boolInfo constructorName []
    expression = TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm patternValue Nothing trueExpr]
    statements = [TypedDataStatement declaration, expressionStatement 1 expression]

foreignOwnedLocalImplId :: TypedImplId
foreignOwnedLocalImplId =
  TypedImplId
    ["Other", "Owner"]
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

foreignOwnedLocalImplProgram :: TypedProgram
foreignOwnedLocalImplProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-foreign-owned-local-impl"
    modulePath = ["Fixture", fixture]
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    statements =
      [ TypedClassStatement (TypedClassDeclaration span1 className [TypedTypeParameterId 0] []),
        TypedImplStatement (TypedImplDeclaration span1 foreignOwnedLocalImplId [])
      ]

importedTypeCapabilityMetadataProgram :: TypedProgram
importedTypeCapabilityMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "TypeCapability"]
    entryPath = ["Fixture", "review-imported-type-capability-metadata"]
    localClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedClassName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Render"
    parameter = TypedTypeParameterId 0
    declaration = TypedClassDeclaration span1 localClassName [parameter] []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TypeCapability.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Render"]
        (TypedModuleInterface [] [] [TypedClassInterface declaration] [])
        [TypedClassStatement declaration]
        boolInfo
    implId = TypedImplId entryPath importedClassName [TypedBoolType, TypedCharType]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Render"])]
        []
        emptyInterface
        [TypedImplStatement (TypedImplDeclaration span1 implId [])]
        boolInfo

callableBuiltinEqualityProgram :: TypedProgram
callableBuiltinEqualityProgram =
  expressionFixtureProgram "review-callable-builtin-equality" expression
  where
    modulePath = ["Fixture", "review-callable-builtin-equality"]
    argumentName index = resolved TypedCurrentModule TypedValueNamespace ("argument" <> Text.pack (show index))
    function index =
      let name = argumentName index
       in TypedLambdaExpr boolToBoolInfo (binder modulePath [index] name) name trueExpr
    expression = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") (function 0) (function 1)

moduleInfoStructuralEqualityUnknownOwner :: TypedBinderId
moduleInfoStructuralEqualityUnknownOwner =
  fixtureBinder
    "review-module-info-structural-equality"
    9
    (fixtureValueName "unknown")

moduleInfoStructuralEqualityProgram :: TypedProgram
moduleInfoStructuralEqualityProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 trueExpr] emptyInterface moduleInfo modulePath
  where
    fixture = "review-module-info-structural-equality"
    modulePath = ["Fixture", fixture]
    moduleInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation moduleInfoStructuralEqualityUnknownOwner [] Nothing]
        []

typeApplicationResultContractProgram :: TypedProgram
typeApplicationResultContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface applicationInfo modulePath
  where
    fixture = "review-type-application-result-contract"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      TypedScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation = TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] (Just span1)
    calleeInfo = TypedNodeInfo boolToBoolType boolToBoolRecipe [instantiation] []
    applicationInfo = TypedNodeInfo TypedTextType TypedManagedTextRecipe [instantiation] []
    expression =
      TypedTypeApplicationExpr
        applicationInfo
        (TypedVariableExpr calleeInfo valueName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

capabilityConstraintVisibilityProgram :: TypedProgram
capabilityConstraintVisibilityProgram =
  withFixturePrelude (signatureProgram fixture owner valueName scheme)
  where
    fixture = "review-capability-constraint-visibility"
    valueName = fixtureValueName "constrained"
    owner = fixtureBinder fixture 0 valueName
    evidence =
      [ TypedEvidenceParameter
          (TypedEvidenceParameterId 0)
          (TypedCapabilityConstraint "Missing" (Just "Missing.m") TypedBoolType),
        TypedEvidenceParameter
          (TypedEvidenceParameterId 1)
          (TypedCapabilityConstraint "Equal" (Just "Equal.missing") TypedBoolType)
      ]
    scheme = TypedScheme owner [] evidence [] TypedBoolType TypedBoolRecipe

unconstrainedNumericParameterProgram :: TypedProgram
unconstrainedNumericParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-numeric-parameter"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "numeric"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argument = TypedVariableExpr parameterInfo argumentName
    body = TypedBinaryExpr parameterInfo (TypedBuiltinOperator "+") argument argument
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        (binder modulePath [0, 0] argumentName)
        argumentName
        body
    scheme = TypedScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

unconstrainedEqualityParameterProgram :: TypedProgram
unconstrainedEqualityParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-equality-parameter"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "equal"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterInfo =
      info
        (TypedTypeParameterType parameter)
        (TypedRepresentationParameterRecipe parameter)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argument = TypedVariableExpr parameterInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    functionType = TypedFunctionType (TypedTypeParameterType parameter) TypedBoolType
    functionRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        TypedBoolRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        (binder modulePath [0, 0] argumentName)
        argumentName
        body
    scheme = TypedScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

duplicatePatternNameSecondBinder :: TypedBinderId
duplicatePatternNameSecondBinder =
  binder
    ["Fixture", "review-duplicate-pattern-name"]
    [0, 1]
    (fixtureValueName "duplicate")

duplicatePatternNameProgram :: TypedProgram
duplicatePatternNameProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-duplicate-pattern-name"
    modulePath = ["Fixture", fixture]
    duplicateName = fixtureValueName "duplicate"
    firstBinder = binder modulePath [0, 0] duplicateName
    patternValue =
      TypedTuplePattern
        pairInfo
        [ TypedVariablePattern boolInfo firstBinder duplicateName,
          TypedVariablePattern boolInfo duplicatePatternNameSecondBinder duplicateName
        ]
    scrutinee = TypedTupleExpr pairInfo [trueExpr, falseExpr]
    expression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]

nonTuplePatternProgram :: TypedProgram
nonTuplePatternProgram =
  expressionFixtureProgram
    "review-non-tuple-pattern"
    (TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedTuplePattern boolInfo []) Nothing trueExpr])

ownerAmbiguousEvidenceProgram :: TypedProgram
ownerAmbiguousEvidenceProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-owner-ambiguous-evidence"
    modulePath = ["Fixture", fixture]
    firstName = fixtureValueName "first"
    secondName = fixtureValueName "second"
    firstOwner = fixtureBinder fixture 0 firstName
    secondOwner = ownerAmbiguousSecondOwner
    parameter = TypedTypeParameterId 0
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    scheme owner =
      TypedScheme
        owner
        [parameter]
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    instantiate owner =
      TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] Nothing
    implId =
      TypedImplId
        ["Prelude"]
        (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
        [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        (Just (TypedEvidenceParameterRef firstOwner (TypedEvidenceParameterId 0)))
        constraint
        implId
        Nothing
    expressionInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiate firstOwner, instantiate secondOwner]
        [TypedSelectedEvidence evidenceUse]
    expression = TypedVariableExpr expressionInfo firstName
    statements =
      [ TypedLetStatement firstOwner firstName span1 (scheme firstOwner) trueExpr,
        TypedLetStatement secondOwner secondName span1 (scheme secondOwner) trueExpr,
        expressionStatement 2 expression
      ]

ownerAmbiguousSecondOwner :: TypedBinderId
ownerAmbiguousSecondOwner =
  fixtureBinder
    "review-owner-ambiguous-evidence"
    1
    (fixtureValueName "second")

reorderedOrPatternProgram :: TypedProgram
reorderedOrPatternProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-reordered-or-pattern"
    modulePath = ["Fixture", fixture]
    leftName = fixtureValueName "left"
    rightName = fixtureValueName "right"
    variable lexicalPath name =
      TypedVariablePattern boolInfo (binder modulePath lexicalPath name) name
    firstAlternative =
      TypedTuplePattern
        pairInfo
        [variable [0, 0] leftName, variable [0, 1] rightName]
    secondAlternative =
      TypedTuplePattern
        pairInfo
        [variable [1, 0] rightName, variable [1, 1] leftName]
    patternValue = TypedOrPattern pairInfo [firstAlternative, secondAlternative]
    scrutinee = TypedTupleExpr pairInfo [trueExpr, falseExpr]
    expression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]

emptyPatternCaseProgram :: TypedProgram
emptyPatternCaseProgram =
  expressionFixtureProgram
    "review-empty-pattern-case"
    (TypedPatternCaseExpr boolInfo trueExpr [])

typeVisibleImplImportProgram :: TypedProgram
typeVisibleImplImportProgram =
  visibleClassImplImportProgram
    "review-type-visible-impl-import"
    [TypedModuleExport TypedTypeNamespace "Render"]
    ["Render"]

methodVisibleImplImportProgram :: TypedProgram
methodVisibleImplImportProgram =
  visibleClassImplImportProgram
    "review-method-visible-impl-import"
    [TypedModuleExport TypedValueNamespace "render"]
    ["render"]

visibleClassImplImportProgram :: Text -> [TypedModuleExport] -> [Text] -> TypedProgram
visibleClassImplImportProgram fixture exports selectedNames =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", fixture]
    entryPath = ["Fixture", fixture]
    localClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedClassName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    parameter = TypedTypeParameterId 0
    methodScheme = TypedScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        localClassName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath localClassName [TypedBoolType]
    importedImplId = TypedImplId libraryPath importedClassName [TypedBoolType]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/VisibleClassImpl.jz")
        []
        exports
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                localImplId
                [fixtureImplMethod libraryPath [1, 0] localImplId "render"]
            )
        ]
        boolInfo
    constraint = TypedCapabilityConstraint "Render" Nothing TypedBoolType
    evidence =
      TypedSelectedEvidence
        (TypedEvidenceUse Nothing constraint importedImplId Nothing)
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence])
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just selectedNames)]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

integralLiteralRangeProgram :: TypedProgram
integralLiteralRangeProgram =
  signatureProgram fixture owner valueName scheme
  where
    fixture = "review-integral-literal-range"
    valueName = fixtureValueName "bounded"
    owner = fixtureBinder fixture 0 valueName
    scheme =
      TypedScheme
        owner
        []
        []
        [ TypedNumericPrimitiveConstraint
            (TypedIntegralLiteralNumericConstraint "0" "300")
            (TypedNumericType TypedUInt8Type)
        ]
        TypedBoolType
        TypedBoolRecipe

nestedStrictEqualityOperandType :: TypedType
nestedStrictEqualityOperandType =
  TypedListType
    ( TypedTupleType
        [ TypedTypeParameterType (TypedTypeParameterId 0),
          TypedBoolType
        ]
    )

nestedStrictEqualityConstraintProgram :: TypedProgram
nestedStrictEqualityConstraintProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-strict-equality-constraint"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "compare"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    constrainedType = TypedTupleType [parameterType, TypedBoolType]
    operandType = nestedStrictEqualityOperandType
    operandRecipe =
      TypedManagedListRecipe
        (TypedManagedProductRecipe [TypedRepresentationParameterRecipe parameter, TypedBoolRecipe])
    operandInfo = info operandType operandRecipe
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentBinder = binder modulePath [0, 0] argumentName
    argument = TypedVariableExpr operandInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    lambdaType = TypedFunctionType operandType TypedBoolType
    lambdaRecipe = TypedClosureRecipe [operandRecipe] TypedBoolRecipe
    expression = TypedLambdaExpr (info lambdaType lambdaRecipe) argumentBinder argumentName body
    scheme =
      TypedScheme
        owner
        [parameter]
        []
        [TypedStrictEqualityPrimitiveConstraint constrainedType]
        lambdaType
        lambdaRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

qualifiedMethodKeyProgram :: Text -> Text -> TypedProgram
qualifiedMethodKeyProgram fixture methodKey =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint "Equal" (Just methodKey) TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBooleanLiteral True)

canonicalQualifiedMethodKeyProgram :: TypedProgram
canonicalQualifiedMethodKeyProgram =
  qualifiedMethodKeyProgram "review-canonical-qualified-method-key" "Equal::equal"

wrongQualifiedMethodKeyProgram :: TypedProgram
wrongQualifiedMethodKeyProgram =
  qualifiedMethodKeyProgram "review-wrong-qualified-method-key" "Other.equal"

builtinValueContractProgram :: TypedProgram
builtinValueContractProgram =
  expressionFixtureProgram
    "review-builtin-value-contract"
    (TypedVariableExpr boolInfo (TypedBuiltinName "__kernel_textLength"))

missingInterfaceMetadataDataName :: TypedCoreName
missingInterfaceMetadataDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Box"

missingInterfaceMetadataProgram :: TypedProgram
missingInterfaceMetadataProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = ["Library", "MissingMetadata"]
    valueName = resolved TypedCurrentModule TypedValueNamespace "boxed"
    valueBinder = binder libraryPath [0] valueName
    dataType = TypedDataType missingInterfaceMetadataDataName []
    dataRecipe = TypedManagedVariantRecipe missingInterfaceMetadataDataName []
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Box"
    valueScheme = TypedScheme valueBinder [] [] [] dataType dataRecipe
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [1, 0]
        missingInterfaceMetadataDataName
        []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MissingMetadata.jz")
        []
        [TypedModuleExport TypedValueNamespace "boxed"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [ TypedLetStatement
            valueBinder
            valueName
            span1
            valueScheme
            (TypedVariableExpr (info dataType dataRecipe) constructorName),
          TypedDataStatement dataDeclaration
        ]
        boolInfo

unterminatedBlockProgram :: TypedProgram
unterminatedBlockProgram =
  expressionFixtureProgram
    "review-unterminated-block"
    (TypedBlockExpr boolInfo [])

constrainedMonomorphicOwner :: TypedBinderId
constrainedMonomorphicOwner =
  fixtureBinder
    "review-constrained-monomorphic-use"
    0
    (fixtureValueName "same")

constrainedMonomorphicUseProgram :: TypedProgram
constrainedMonomorphicUseProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-constrained-monomorphic-use"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "same"
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    scheme =
      TypedScheme
        constrainedMonomorphicOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    expression = TypedVariableExpr boolInfo valueName
    statements =
      [ TypedLetStatement constrainedMonomorphicOwner valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

unrelatedKnownInstantiationOwner :: TypedBinderId
unrelatedKnownInstantiationOwner =
  fixtureBinder
    "review-unrelated-known-instantiation"
    0
    (fixtureValueName "known")

unrelatedKnownInstantiationProgram :: TypedProgram
unrelatedKnownInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unrelated-known-instantiation"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "known"
    instantiation = TypedInstantiation unrelatedKnownInstantiationOwner [] Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [])
        (TypedBooleanLiteral True)
    statements =
      [ TypedLetStatement unrelatedKnownInstantiationOwner valueName span1 (monoScheme unrelatedKnownInstantiationOwner) trueExpr,
        expressionStatement 1 expression
      ]

explicitHeadParameterOwner :: TypedBinderId
explicitHeadParameterOwner =
  fixtureBinder
    "review-explicit-head-parameter"
    0
    (fixtureValueName "choose")

explicitHeadParameterProgram :: TypedProgram
explicitHeadParameterProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface instantiatedInfo modulePath
  where
    fixture = "review-explicit-head-parameter"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "choose"
    firstParameter = TypedTypeParameterId 0
    secondParameter = TypedTypeParameterId 1
    parameterType = TypedTypeParameterType firstParameter
    parameterRecipe = TypedRepresentationParameterRecipe firstParameter
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      TypedScheme
        explicitHeadParameterOwner
        [firstParameter, secondParameter]
        []
        []
        functionType
        functionRecipe
    instantiation =
      TypedInstantiation
        explicitHeadParameterOwner
        [ TypedTypeArgument firstParameter TypedTextType,
          TypedTypeArgument secondParameter TypedBoolType
        ]
        (Just span1)
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedTextType TypedTextType)
        (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
        [instantiation]
        []
    expression =
      TypedTypeApplicationExpr
        instantiatedInfo
        (TypedVariableExpr instantiatedInfo valueName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement
          explicitHeadParameterOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] firstParameter),
        expressionStatement 1 expression
      ]

classArityProgram :: TypedProgram
classArityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-class-arity"
    modulePath = ["Fixture", fixture]
    zeroName = resolved TypedCurrentModule TypedCapabilityNamespace "Zero"
    multiName = resolved TypedCurrentModule TypedCapabilityNamespace "Multi"
    statements =
      [ TypedClassStatement (TypedClassDeclaration span1 zeroName [] []),
        TypedClassStatement
          ( TypedClassDeclaration
              span1
              multiName
              [TypedTypeParameterId 0, TypedTypeParameterId 1]
              []
          )
      ]

classMethodSchemeShapeProgram :: TypedProgram
classMethodSchemeShapeProgram =
  singleModuleProgram fixture relativeSource [] [TypedClassStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-class-method-scheme-shape"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    method name scheme =
      TypedMethodSignature
        name
        span1
        scheme
    methodName suffix = resolved TypedCurrentModule TypedValueNamespace suffix
    methodBinder index name = binder modulePath [0, index] name
    localName = methodName "local"
    evidenceName = methodName "evidence"
    primitiveName = methodName "primitive"
    localScheme =
      TypedScheme
        (methodBinder 0 localName)
        [parameter]
        []
        []
        parameterType
        parameterRecipe
    evidenceScheme =
      TypedScheme
        (methodBinder 1 evidenceName)
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint "Marker" Nothing parameterType)
        ]
        []
        parameterType
        parameterRecipe
    primitiveScheme =
      TypedScheme
        (methodBinder 2 primitiveName)
        []
        []
        [TypedStrictEqualityPrimitiveConstraint parameterType]
        parameterType
        parameterRecipe
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [ method localName localScheme,
          method evidenceName evidenceScheme,
          method primitiveName primitiveScheme
        ]

duplicateImplDeclarationId :: TypedImplId
duplicateImplDeclarationId =
  TypedImplId
    ["Fixture", "review-duplicate-impl-declaration"]
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

duplicateImplDeclarationProgram :: TypedProgram
duplicateImplDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-impl-declaration"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    implStatement =
      TypedImplStatement
        (TypedImplDeclaration span1 duplicateImplDeclarationId [])
    statements =
      [TypedClassStatement declaration, implStatement, implStatement]

emptyOrPatternProgram :: TypedProgram
emptyOrPatternProgram =
  expressionFixtureProgram
    "review-empty-or-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedOrPattern boolInfo []) Nothing trueExpr]
    )

nonBindingTypeApplicationProgram :: TypedProgram
nonBindingTypeApplicationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-binding-type-application"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      TypedScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter TypedBoolType]
        (Just span1)
    calleeInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiation]
        []
    resultInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiation]
        []
    applied =
      TypedApplyExpr
        resultInfo
        (TypedVariableExpr calleeInfo valueName)
        trueExpr
    expression =
      TypedTypeApplicationExpr
        resultInfo
        applied
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

mismatchedResolvedOperatorProgram :: TypedProgram
mismatchedResolvedOperatorProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath
  where
    fixture = "review-mismatched-resolved-operator"
    modulePath = ["Fixture", fixture]
    operatorName =
      TypedGeneratedName
        (TypedOperatorBinding "$operator:%2B")
    owner = binder modulePath [0] operatorName
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    scheme =
      TypedScheme
        owner
        []
        []
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator operatorName "-")
    statements =
      [ TypedLetStatement owner operatorName span1 scheme (boolBinaryFunctionExpression modulePath [0]),
        expressionStatement 1 expression
      ]

dataInterfaceDependencyHiddenName :: TypedCoreName
dataInterfaceDependencyHiddenName =
  resolved TypedCurrentModule TypedTypeNamespace "Hidden"

dataInterfaceDependencyProgram :: TypedProgram
dataInterfaceDependencyProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = ["Library", "DataDependency"]
    boxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    boxConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    hiddenType =
      TypedDataType dataInterfaceDependencyHiddenName []
    hiddenDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        dataInterfaceDependencyHiddenName
        []
    boxDeclaration =
      TypedDataDeclaration
        span1
        boxName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [1, 0] boxConstructorName)
            boxConstructorName
            [hiddenType]
            [TypedManagedVariantRecipe dataInterfaceDependencyHiddenName []]
        ]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/DataDependency.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Box"]
        (TypedModuleInterface [] [TypedDataInterface boxDeclaration] [] [])
        [ TypedDataStatement hiddenDeclaration,
          TypedDataStatement boxDeclaration
        ]
        boolInfo

classMethodInterfaceDependencyDataName :: TypedCoreName
classMethodInterfaceDependencyDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Box"

classMethodInterfaceDependencyProgram :: TypedProgram
classMethodInterfaceDependencyProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = ["Library", "ClassMethodDependency"]
    boxDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        classMethodInterfaceDependencyDataName
        []
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    boxType =
      TypedDataType classMethodInterfaceDependencyDataName []
    boxRecipe =
      TypedManagedVariantRecipe
        classMethodInterfaceDependencyDataName
        []
    methodScheme =
      TypedScheme
        (binder libraryPath [1, 0] methodName)
        []
        []
        []
        (TypedFunctionType boxType parameterType)
        ( TypedClosureRecipe
            [boxRecipe]
            (TypedRepresentationParameterRecipe parameter)
        )
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/ClassMethodDependency.jz")
        []
        [TypedModuleExport TypedValueNamespace "render"]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [ TypedDataStatement boxDeclaration,
          TypedClassStatement classDeclaration
        ]
        boolInfo

instantiatedPrimitiveConstraintProgram :: TypedProgram
instantiatedPrimitiveConstraintProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-instantiated-primitive-constraints"
    modulePath = ["Fixture", fixture]
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    numericName = fixtureValueName "numeric"
    numericOwner = binder modulePath [0] numericName
    equalityName = fixtureValueName "equality"
    equalityOwner = binder modulePath [1] equalityName
    constrainedScheme owner primitiveConstraint =
      TypedScheme
        owner
        [parameter]
        []
        [primitiveConstraint]
        TypedBoolType
        TypedBoolRecipe
    instantiatedUse owner name typeArgument =
      TypedVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [TypedInstantiation owner [TypedTypeArgument parameter typeArgument] Nothing]
            []
        )
        name
    statements =
      [ TypedLetStatement
          numericOwner
          numericName
          span1
          (constrainedScheme numericOwner (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType))
          trueExpr,
        TypedLetStatement
          equalityOwner
          equalityName
          span1
          (constrainedScheme equalityOwner (TypedStrictEqualityPrimitiveConstraint parameterType))
          trueExpr,
        expressionStatement 2 (instantiatedUse numericOwner numericName TypedBoolType),
        expressionStatement 3 (instantiatedUse equalityOwner equalityName boolToBoolType)
      ]

typeApplicationExtraOwner :: TypedBinderId
typeApplicationExtraOwner =
  fixtureBinder
    "review-type-application-extra-owner"
    1
    (fixtureValueName "other")

typeApplicationExtraOwnerProgram :: TypedProgram
typeApplicationExtraOwnerProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-type-application-extra-owner"
    modulePath = ["Fixture", fixture]
    parameter = TypedTypeParameterId 0
    functionName = fixtureValueName "function"
    functionOwner = binder modulePath [0] functionName
    otherName = fixtureValueName "other"
    scheme owner =
      TypedScheme owner [parameter] [] [] TypedBoolType TypedBoolRecipe
    instantiate owner maybeSpan =
      TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] maybeSpan
    functionInstantiation = instantiate functionOwner (Just span1)
    otherInstantiation = instantiate typeApplicationExtraOwner Nothing
    functionInfo =
      TypedNodeInfo TypedBoolType TypedBoolRecipe [functionInstantiation] []
    applicationInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [functionInstantiation, otherInstantiation]
        []
    expression =
      TypedTypeApplicationExpr
        applicationInfo
        (TypedVariableExpr functionInfo functionName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement functionOwner functionName span1 (scheme functionOwner) trueExpr,
        TypedLetStatement typeApplicationExtraOwner otherName span1 (scheme typeApplicationExtraOwner) trueExpr,
        expressionStatement 2 expression
      ]

constrainedResolvedOperatorOwner :: TypedBinderId
constrainedResolvedOperatorOwner =
  binder
    ["Fixture", "review-constrained-resolved-operator"]
    [0]
    constrainedResolvedOperatorName

constrainedResolvedOperatorName :: TypedCoreName
constrainedResolvedOperatorName =
  TypedGeneratedName (TypedOperatorBinding "$operator:%2B")

constrainedResolvedOperatorProgram :: TypedProgram
constrainedResolvedOperatorProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath)
  where
    fixture = "review-constrained-resolved-operator"
    modulePath = ["Fixture", fixture]
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    scheme =
      TypedScheme
        constrainedResolvedOperatorOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator constrainedResolvedOperatorName "+")
    statements =
      [ TypedLetStatement
          constrainedResolvedOperatorOwner
          constrainedResolvedOperatorName
          span1
          scheme
          (boolBinaryFunctionExpression modulePath [0]),
        expressionStatement 2 expression
      ]

missingModuleResultProgram :: TypedProgram
missingModuleResultProgram =
  TypedProgram
    Nothing
    [ TypedModule
        ["Fixture", fixture]
        relativeSource
        []
        []
        emptyInterface
        [TypedSignatureStatement owner name span1 (monoScheme owner)]
        boolInfo
    ]
    ["Fixture", fixture]
  where
    fixture = "review-missing-module-result"
    name = fixtureValueName "value"
    owner = fixtureBinder fixture 0 name

emptyDataDeclarationProgram :: TypedProgram
emptyDataDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-data-declaration"
    modulePath = ["Fixture", fixture]
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Never"
    statements =
      [ TypedDataStatement (TypedDataDeclaration span1 dataName [] []),
        expressionStatement 2 trueExpr
      ]

laterOrPatternCollidingBinder :: TypedBinderId
laterOrPatternCollidingBinder =
  fixtureBinder
    "review-later-or-pattern-binder-collision"
    0
    (fixtureValueName "matched")

laterOrPatternBinderCollisionProgram :: TypedProgram
laterOrPatternBinderCollisionProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-later-or-pattern-binder-collision"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "matched"
    firstBinder = binder modulePath [1, 0] valueName
    firstAlternative = TypedVariablePattern boolInfo firstBinder valueName
    secondAlternative =
      TypedVariablePattern boolInfo laterOrPatternCollidingBinder valueName
    patternValue =
      TypedOrPattern boolInfo [firstAlternative, secondAlternative]
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm patternValue Nothing trueExpr]
    statements =
      [ TypedSignatureStatement
          laterOrPatternCollidingBinder
          valueName
          span1
          (monoScheme laterOrPatternCollidingBinder),
        expressionStatement 2 expression
      ]

concreteIntegerBoundsProgram :: TypedProgram
concreteIntegerBoundsProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ expressionStatement 1 (integerExpression "300"),
      expressionStatement 2 (integerExpression "-1")
    ]
    emptyInterface
    integerInfo
    ["Fixture", fixture]
  where
    fixture = "review-concrete-integer-bounds"
    integerInfo =
      info
        (TypedNumericType TypedUInt8Type)
        (TypedUnsignedIntegerRecipe 8)
    integerExpression value =
      TypedLiteralExpr integerInfo (TypedIntegerLiteral value)

incompleteImplProgram :: TypedProgram
incompleteImplProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-incomplete-impl"
    modulePath = ["Fixture", fixture]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    renderName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    mapName =
      resolved TypedCurrentModule TypedValueNamespace "map"
    parameter = TypedTypeParameterId 0
    methodScheme methodOwner =
      TypedScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        ( TypedClosureRecipe
            [TypedRepresentationParameterRecipe parameter]
            TypedManagedTextRecipe
        )
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [ TypedMethodSignature renderName span1 (methodScheme (binder modulePath [0, 0] renderName)),
          TypedMethodSignature mapName span1 (methodScheme (binder modulePath [0, 1] mapName))
        ]
    implId =
      TypedImplId modulePath capabilityName [TypedBoolType]
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 3 trueExpr
      ]

duplicateInstantiationOwner :: TypedBinderId
duplicateInstantiationOwner =
  fixtureBinder
    "review-duplicate-instantiation"
    0
    (fixtureValueName "value")

duplicateInstantiationProgram :: TypedProgram
duplicateInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-instantiation"
    modulePath = ["Fixture", fixture]
    valueName = fixtureValueName "value"
    parameter = TypedTypeParameterId 0
    scheme =
      TypedScheme
        duplicateInstantiationOwner
        [parameter]
        []
        []
        TypedBoolType
        TypedBoolRecipe
    instantiate typeValue =
      TypedInstantiation
        duplicateInstantiationOwner
        [TypedTypeArgument parameter typeValue]
        Nothing
    expression =
      TypedVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [instantiate TypedBoolType, instantiate TypedTextType]
            []
        )
        valueName
    statements =
      [ TypedLetStatement
          duplicateInstantiationOwner
          valueName
          span1
          scheme
          trueExpr,
        expressionStatement 2 expression
      ]

fractionalLiteralBoundsProgram :: TypedProgram
fractionalLiteralBoundsProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ expressionStatement 1 (fractionalExpression TypedFloat16Type 16 "65504" "0"),
      expressionStatement 2 (fractionalExpression TypedFloat16Type 16 "65504" "1"),
      expressionStatement 3 (fractionalExpression TypedFloat16Type 16 "-65504" "1"),
      expressionStatement 4 (fractionalExpression TypedFloat32Type 32 float32Maximum "0"),
      expressionStatement 5 (fractionalExpression TypedFloat32Type 32 float32Maximum "1"),
      expressionStatement 6 (fractionalExpression TypedFloat64Type 64 float64Maximum "0"),
      expressionStatement 7 (fractionalExpression TypedFloat64Type 64 float64Maximum "1")
    ]
    emptyInterface
    (floatInfo TypedFloat64Type 64)
    ["Fixture", fixture]
  where
    fixture = "review-fractional-literal-bounds"
    float32Maximum =
      "340282346638528859811704183484516925440"
    float64Maximum =
      "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"
    floatInfo numericType width =
      info
        (TypedNumericType numericType)
        (TypedFloatRecipe width)
    fractionalExpression numericType width whole fractional =
      TypedLiteralExpr
        (floatInfo numericType width)
        (TypedFractionalLiteral whole fractional (Just numericType))

visibleClassCollisionPreludeName :: TypedCoreName
visibleClassCollisionPreludeName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Render"

visibleClassCollisionImportedName :: TypedCoreName
visibleClassCollisionImportedName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Visible"

visibleClassCollisionProgram :: TypedProgram
visibleClassCollisionProgram =
  TypedProgram (Just fixturePrelude) [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "VisibleClass"]
    entryPath = ["Fixture", "review-visible-class-collision"]
    parameter = TypedTypeParameterId 0
    libraryClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    libraryDeclaration =
      TypedClassDeclaration span1 libraryClassName [parameter] []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/VisibleClass.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        (TypedModuleInterface [] [] [TypedClassInterface libraryDeclaration] [])
        [TypedClassStatement libraryDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Visible"])]
        []
        emptyInterface
        [ TypedClassStatement
            ( TypedClassDeclaration
                span1
                visibleClassCollisionPreludeName
                [parameter]
                []
            ),
          TypedClassStatement
            ( TypedClassDeclaration
                span1
                visibleClassCollisionImportedName
                [parameter]
                []
            ),
          expressionStatement 3 trueExpr
        ]
        boolInfo

selectedClassDataDependencyProgram :: TypedProgram
selectedClassDataDependencyProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "SelectedClassData"]
    entryPath = ["Fixture", "review-selected-class-data-dependency"]
    dataName =
      resolved TypedCurrentModule TypedTypeNamespace "Box"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    dataDeclaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "RoundTrip"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "roundTrip"
    classParameter = TypedTypeParameterId 0
    localBoxType = TypedDataType dataName []
    localBoxRecipe = TypedManagedVariantRecipe dataName []
    methodOwner = binder libraryPath [1, 0] methodName
    methodScheme =
      TypedScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType localBoxType localBoxType)
        (TypedClosureRecipe [localBoxRecipe] localBoxRecipe)
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [classParameter]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/SelectedClassData.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "RoundTrip"]
        ( TypedModuleInterface
            []
            [TypedDataInterface dataDeclaration]
            [TypedClassInterface classDeclaration]
            []
        )
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration
        ]
        unitInfo
    importedCapabilityName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "RoundTrip"
    importedDataName =
      resolved
        (TypedImportedModule libraryPath)
        TypedTypeNamespace
        "Box"
    importedBoxType = TypedDataType importedDataName []
    importedBoxRecipe = TypedManagedVariantRecipe importedDataName []
    methodType = TypedFunctionType importedBoxType importedBoxType
    methodRecipe = TypedClosureRecipe [importedBoxRecipe] importedBoxRecipe
    methodInfo = info methodType methodRecipe
    implId = TypedImplId entryPath importedCapabilityName [TypedBoolType]
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "roundTrip"
    methodBinder = binder entryPath [0, 0] localMethodName
    parameterName =
      resolved TypedCurrentModule TypedValueNamespace "value"
    parameterBinder = binder entryPath [0, 0, 0] parameterName
    body =
      TypedVariableExpr
        (info importedBoxType importedBoxRecipe)
        parameterName
    methodBody =
      TypedLambdaExpr
        methodInfo
        parameterBinder
        parameterName
        body
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "roundTrip")
        methodBinder
        localMethodName
        span1
        methodBody
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["RoundTrip"])]
        []
        emptyInterface
        [ TypedImplStatement
            (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo

selectedValueDataMetadataProgram :: TypedProgram
selectedValueDataMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "SelectedValueData"]
    entryPath = ["Fixture", "review-selected-value-data-metadata"]
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    importedDataName = resolved (TypedImportedModule libraryPath) TypedTypeNamespace "Box"
    localValueName = resolved TypedCurrentModule TypedValueNamespace "boxed"
    importedValueName = resolved (TypedImportedModule libraryPath) TypedValueNamespace "boxed"
    localConstructorName = resolved TypedCurrentModule TypedConstructorNamespace "Box"
    valueBinder = binder libraryPath [0] localValueName
    dataType = TypedDataType localDataName []
    dataRecipe = TypedManagedVariantRecipe localDataName []
    importedType = TypedDataType importedDataName []
    importedRecipe = TypedManagedVariantRecipe importedDataName []
    valueScheme = TypedScheme valueBinder [] [] [] dataType dataRecipe
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [1, 0]
        localDataName
        []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/SelectedValueData.jz")
        []
        [TypedModuleExport TypedValueNamespace "boxed"]
        (TypedModuleInterface [TypedValueInterface localValueName valueScheme] [TypedDataInterface dataDeclaration] [] [])
        [ TypedLetStatement
            valueBinder
            localValueName
            span1
            valueScheme
            (TypedVariableExpr (info dataType dataRecipe) localConstructorName),
          TypedDataStatement dataDeclaration
        ]
        boolInfo
    entryInfo = info importedType importedRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["boxed"])]
        []
        emptyInterface
        [expressionStatement 1 (TypedVariableExpr entryInfo importedValueName)]
        entryInfo

selectiveImportLeakedImpl :: TypedImplId
selectiveImportLeakedImpl =
  TypedImplId
    ["Library", "PrivateImpl"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedBoolType]

selectiveImportImplLeakProgram :: TypedProgram
selectiveImportImplLeakProgram = TypedProgram (Just fixturePrelude) [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "PrivateImpl"]
    entryPath = ["Fixture", "review-selective-import-impl-leak"]
    localValueName = resolved TypedCurrentModule TypedValueNamespace "published"
    valueBinder = binder libraryPath [0] localValueName
    valueScheme = monoScheme valueBinder
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/PrivateImpl.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        (TypedModuleInterface [TypedValueInterface localValueName valueScheme] [] [] [TypedImplInterface selectiveImportLeakedImpl])
        [ TypedLetStatement valueBinder localValueName span1 valueScheme trueExpr,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                selectiveImportLeakedImpl
                [ fixtureImplMethod libraryPath [1, 0] selectiveImportLeakedImpl "equal",
                  fixtureImplMethod libraryPath [1, 1] selectiveImportLeakedImpl "other"
                ]
            )
        ]
        boolInfo
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint selectiveImportLeakedImpl Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

selectedEvidenceMethodExistenceProgram :: TypedProgram
selectedEvidenceMethodExistenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-selected-evidence-method-existence"
    modulePath = ["Fixture", fixture]
    parameter = TypedTypeParameterId 0
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    capability = TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    methodId = TypedMethodId implId "equal"
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId (Just methodId))
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 1 expression
      ]

duplicateImplMethodName :: TypedCoreName
duplicateImplMethodName = resolved TypedCurrentModule TypedValueNamespace "equal"

duplicateImplMethodProgram :: TypedProgram
duplicateImplMethodProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-duplicate-impl-method"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    method lexicalIndex =
      TypedMethodDefinition
        methodId
        (binder modulePath [lexicalIndex] duplicateImplMethodName)
        duplicateImplMethodName
        span1
        trueExpr
    declaration =
      TypedImplDeclaration
        span1
        implId
        [ method 0,
          method 1,
          fixtureImplMethod modulePath [2] implId "other"
        ]

nestedOuterTypeScopeProgram :: TypedProgram
nestedOuterTypeScopeProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-outer-type-scope"
    modulePath = ["Fixture", fixture]
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerBinder = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    argumentUse = TypedVariableExpr parameterInfo argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localBinder = binder modulePath [0, 0, 0] localName
    localScheme = TypedScheme localBinder [] [] [] parameterType parameterRecipe
    localBinding =
      TypedLetStatement
        localBinder
        localName
        span1
        localScheme
        argumentUse
    localUse = expressionStatement 2 (TypedVariableExpr parameterInfo localName)
    block = TypedBlockExpr parameterInfo [localBinding, localUse]
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentBinder
        argumentName
        block
    outerScheme = TypedScheme outerBinder [parameter] [] [] functionType functionRecipe
    topLevelBinding = TypedLetStatement outerBinder outerName span1 outerScheme expression

implMethodVisibleName :: TypedCoreName
implMethodVisibleName = resolved TypedCurrentModule TypedValueNamespace "equal"

implMethodValueVisibilityProgram :: TypedProgram
implMethodValueVisibilityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-value-visibility"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    method =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        (binder modulePath [0] implMethodVisibleName)
        implMethodVisibleName
        span1
        trueExpr
    statements =
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [method, fixtureImplMethod modulePath [0, 1] implId "other"]
          ),
        expressionStatement 2 (TypedVariableExpr boolInfo implMethodVisibleName)
      ]

builtinOperatorContractProgram :: TypedProgram
builtinOperatorContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface intInfo modulePath
  where
    fixture = "review-builtin-operator-contract"
    modulePath = ["Fixture", fixture]
    intInfo = info TypedIntType (TypedSignedIntegerRecipe 64)
    one = TypedLiteralExpr intInfo (TypedIntegerLiteral "1")
    invalidUnknown = TypedOperatorValueExpr boolToBoolInfo (TypedBuiltinOperator "%%")
    invalidResult = TypedBinaryExpr boolInfo (TypedBuiltinOperator "+") one one
    validResult = TypedBinaryExpr intInfo (TypedBuiltinOperator "+") one one
    statements = [expressionStatement 1 invalidUnknown, expressionStatement 2 invalidResult, expressionStatement 3 validResult]

ordinaryFunctionCandidateAmbiguityProgram :: TypedProgram
ordinaryFunctionCandidateAmbiguityProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-ordinary-function-candidate-ambiguity"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    constraint = TypedCapabilityConstraint "Render" (Just "Render.render") TypedTextType
    candidate = TypedEvidenceCandidate implId (Just (TypedMethodId implId "render"))
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    expression =
      TypedLambdaExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint [candidate, candidate]])
        (binder modulePath [0] argumentName)
        argumentName
        trueExpr

invalidNumericPrimitiveConstraintProgram :: TypedProgram
invalidNumericPrimitiveConstraintProgram =
  signatureProgram fixture valueBinder valueName scheme
  where
    fixture = "review-invalid-numeric-primitive-constraint"
    valueName = fixtureValueName "value"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme =
      TypedScheme
        valueBinder
        []
        []
        [TypedNumericPrimitiveConstraint TypedAnyNumericConstraint TypedTextType]
        TypedBoolType
        TypedBoolRecipe

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
    statements = [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 expression]

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
invisibleOperatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%2B")

invisibleOperatorProgram :: TypedProgram
invisibleOperatorProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo entryPath
  where
    fixture = "review-invisible-operator"
    entryPath = ["Fixture", "review-invisible-operator"]
    expression = TypedOperatorValueExpr boolInfo (TypedResolvedOperator invisibleOperatorName "+")

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
    statements = [TypedLetStatement expressionDuplicateBinder valueName span1 scheme trueExpr, expressionStatement 2 lambda]

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
        [TypedLetStatement owner privateInterfaceLocalName span1 scheme trueExpr]
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
    constructorInfo =
      info
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
    scrutinee = TypedApplyExpr optionInfo (TypedVariableExpr constructorInfo someName) trueExpr
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
    statements = [TypedLetStatement explicitTypeApplicationOwner valueName span1 scheme trueExpr, expressionStatement 2 expression]

variableSchemeContractProgram :: TypedProgram
variableSchemeContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-variable-scheme-contract"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "value"
    valueBinder = binder modulePath [0] valueName
    statements =
      [ TypedLetStatement valueBinder valueName span1 (monoScheme valueBinder) trueExpr,
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
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath)
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
    candidateExpression lexicalIndex candidate =
      TypedLambdaExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (binder modulePath [lexicalIndex] candidateArgumentName)
        candidateArgumentName
        trueExpr
    candidateArgumentName = resolved TypedCurrentModule TypedValueNamespace "candidateArgument"
    method implId methodKey lexicalIndex =
      let methodName = resolved TypedCurrentModule TypedValueNamespace methodKey
          argumentName = resolved TypedCurrentModule TypedValueNamespace (methodKey <> "Argument")
       in TypedMethodDefinition
            (TypedMethodId implId methodKey)
            (binder modulePath [lexicalIndex] methodName)
            methodName
            span1
            (TypedLambdaExpr boolToBoolInfo (binder modulePath [lexicalIndex, 0] argumentName) argumentName trueExpr)
    statements =
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              renderImpl
              [method renderImpl "render" 0, fixtureImplMethod modulePath [0, 1] renderImpl "map"]
          ),
        TypedImplStatement
          ( TypedImplDeclaration
              span1
              equalImpl
              [method equalImpl "other" 1, fixtureImplMethod modulePath [1, 1] equalImpl "equal"]
          ),
        expressionStatement 3 (candidateExpression 2 renderCandidate),
        expressionStatement 4 (candidateExpression 3 wrongMethodCandidate)
      ]

invalidVariableNamespaceName :: TypedCoreName
invalidVariableNamespaceName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

invalidVariableNamespaceProgram :: TypedProgram
invalidVariableNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-variable-namespace"
    modulePath = ["Fixture", fixture]
    declaration =
      dataDeclarationWithNullaryConstructor
        modulePath
        [0, 0]
        invalidVariableNamespaceName
        []
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
    block = TypedBlockExpr boolInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

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
    block = TypedBlockExpr textInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

implMethodNameProgram :: TypedProgram
implMethodNameProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-name"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    publishedName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] publishedName
    method = TypedMethodDefinition (TypedMethodId implId "equal") methodBinder publishedName span1 trueExpr
    declaration =
      TypedImplDeclaration
        span1
        implId
        [method, fixtureImplMethod modulePath [0, 1] implId "other"]

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
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%2B")
    owner = binder modulePath [0] operatorName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    scheme = TypedScheme owner [] [] [] operatorType operatorRecipe
    operator = TypedResolvedOperator operatorName "+"
    textExpr = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "text")
    textToTextInfo = info (TypedFunctionType TypedTextType TypedTextType) (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
    statements =
      [ TypedLetStatement owner operatorName span1 scheme (boolBinaryFunctionExpression modulePath [0]),
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
        [TypedLetStatement owner localName span1 scheme trueExpr]
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
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
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
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef owner selectedId))
            constraint
            (TypedImplId ["Prelude"] capabilityName [targetType])
            Nothing
        )
    expression selection = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [selection]) valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
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

missingInstantiatedEvidenceProgram :: TypedProgram
missingInstantiatedEvidenceProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-missing-instantiated-evidence"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    evidenceId = TypedEvidenceParameterId 0
    laterEvidenceId = TypedEvidenceParameterId 1
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    scheme = TypedScheme owner [] [TypedEvidenceParameter evidenceId constraint, TypedEvidenceParameter laterEvidenceId constraint] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [] Nothing
    expression = TypedVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    statements = [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 expression]

constructorExpressionDataName :: TypedCoreName
constructorExpressionDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

constructorExpressionResultType :: TypedType
constructorExpressionResultType = TypedDataType constructorExpressionDataName []

constructorExpressionContractProgram :: TypedProgram
constructorExpressionContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-constructor-expression-contract"
    modulePath = ["Fixture", fixture]
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    declaration =
      TypedDataDeclaration
        span1
        constructorExpressionDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            [TypedBoolType]
            [TypedBoolRecipe]
        ]
    statements = [TypedDataStatement declaration, expressionStatement 2 (TypedVariableExpr boolInfo constructorName)]

unrelatedTypeApplicationProgram :: TypedProgram
unrelatedTypeApplicationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface applicationInfo modulePath
  where
    fixture = "review-unrelated-type-application"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "unrelated"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      TypedScheme
        owner
        [parameterId]
        []
        []
        functionType
        functionRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] (Just span1)
    applicationInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression = TypedTypeApplicationExpr applicationInfo trueExpr span1 TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameterId),
        expressionStatement 2 expression
      ]

lexicalBinderContractProgram :: TypedProgram
lexicalBinderContractProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-lexical-binder-contract"
    modulePath = ["Fixture", fixture]
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0] argumentName
    lambdaInfo =
      info
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    expression = TypedLambdaExpr lambdaInfo argumentBinder argumentName (TypedVariableExpr textInfo argumentName)

generalizedVariableContractProgram :: TypedProgram
generalizedVariableContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface badUseInfo modulePath
  where
    fixture = "review-generalized-variable-contract"
    modulePath = ["Fixture", fixture]
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder modulePath [0] valueName
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
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    badUseInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression = TypedVariableExpr badUseInfo valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameterId),
        expressionStatement 2 expression
      ]

enclosingInstantiationScopeProgram :: TypedProgram
enclosingInstantiationScopeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-enclosing-instantiation-scope"
    modulePath = ["Fixture", fixture]
    identityName = resolved TypedCurrentModule TypedValueNamespace "identity"
    identityOwner = binder modulePath [0] identityName
    identityParameter = TypedTypeParameterId 0
    identityParameterType = TypedTypeParameterType identityParameter
    identityParameterRecipe = TypedRepresentationParameterRecipe identityParameter
    identityScheme =
      TypedScheme
        identityOwner
        [identityParameter]
        []
        []
        (TypedFunctionType identityParameterType identityParameterType)
        (TypedClosureRecipe [identityParameterRecipe] identityParameterRecipe)
    wrapperName = resolved TypedCurrentModule TypedValueNamespace "wrapper"
    wrapperOwner = binder modulePath [1] wrapperName
    wrapperParameter = TypedTypeParameterId 0
    wrapperParameterType = TypedTypeParameterType wrapperParameter
    wrapperParameterRecipe = TypedRepresentationParameterRecipe wrapperParameter
    wrapperType = TypedFunctionType wrapperParameterType wrapperParameterType
    wrapperRecipe = TypedClosureRecipe [wrapperParameterRecipe] wrapperParameterRecipe
    wrapperScheme = TypedScheme wrapperOwner [wrapperParameter] [] [] wrapperType wrapperRecipe
    instantiation = TypedInstantiation identityOwner [TypedTypeArgument identityParameter wrapperParameterType] Nothing
    expression = TypedVariableExpr (TypedNodeInfo wrapperType wrapperRecipe [instantiation] []) identityName
    statements =
      [ TypedLetStatement
          identityOwner
          identityName
          span1
          identityScheme
          (polymorphicIdentityExpression modulePath [0] identityParameter),
        TypedLetStatement wrapperOwner wrapperName span1 wrapperScheme expression
      ]

implMethodContractProgram :: TypedProgram
implMethodContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-method-contract"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodOwner = binder modulePath [0, 0] methodName
    methodScheme = TypedScheme methodOwner [] [] [] methodType methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameterId]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    implMethod =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        (TypedLiteralExpr textInfo (TypedTextLiteral "wrong"))
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId [implMethod])
      ]

invalidDataDeclarationName :: TypedCoreName
invalidDataDeclarationName = resolved TypedCurrentModule TypedValueNamespace "Flag"

dataDeclarationNamespaceProgram :: TypedProgram
dataDeclarationNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-data-declaration-namespace"
    modulePath = ["Fixture", fixture]
    statements =
      [ TypedDataStatement
          ( dataDeclarationWithNullaryConstructor
              modulePath
              [0, 0]
              invalidDataDeclarationName
              []
          )
      ]

duplicateDeclarationName :: TypedCoreName
duplicateDeclarationName = resolved TypedCurrentModule TypedValueNamespace "duplicate"

duplicateDeclarationProgram :: TypedProgram
duplicateDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-declaration"
    modulePath = ["Fixture", fixture]
    firstOwner = binder modulePath [0] duplicateDeclarationName
    secondOwner = binder modulePath [1] duplicateDeclarationName
    statements =
      [ TypedSignatureStatement firstOwner duplicateDeclarationName span1 (monoScheme firstOwner),
        TypedSignatureStatement secondOwner duplicateDeclarationName span1 (monoScheme secondOwner)
      ]

importedImplQualificationProgram :: TypedProgram
importedImplQualificationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = ["Library", "QualifiedImpl"]
    entryPath = ["Fixture", "review-imported-impl-qualification"]
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    localCapabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Mark"
    parameterId = TypedTypeParameterId 0
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        localDataName
        []
    classDeclaration = TypedClassDeclaration span1 localCapabilityName [parameterId] []
    localImplId = TypedImplId libraryPath localCapabilityName [TypedDataType localDataName []]
    libraryInterface =
      TypedModuleInterface
        []
        [TypedDataInterface dataDeclaration]
        [TypedClassInterface classDeclaration]
        [TypedImplInterface localImplId]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/QualifiedImpl.jz")
        []
        [ TypedModuleExport TypedTypeNamespace "Flag",
          TypedModuleExport TypedCapabilityNamespace "Mark"
        ]
        libraryInterface
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [])
        ]
        boolInfo
    importedDataName = resolved (TypedImportedModule libraryPath) TypedTypeNamespace "Flag"
    importedCapabilityName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Mark"
    importedTargetType = TypedDataType importedDataName []
    importedImplId = TypedImplId libraryPath importedCapabilityName [importedTargetType]
    constraint = TypedCapabilityConstraint "Mark" Nothing importedTargetType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint importedImplId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing Nothing]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

implTargetArityProgram :: TypedProgram
implTargetArityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-target-arity"
    modulePath = ["Fixture", fixture]
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType, TypedCharType]
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 2 expression
      ]

localDeclarationOriginBinder :: TypedBinderId
localDeclarationOriginBinder =
  binder
    ["Fixture", "review-local-declaration-origin"]
    [0]
    localDeclarationOriginName

localDeclarationOriginName :: TypedCoreName
localDeclarationOriginName = resolved (TypedImportedModule ["Other", "Module"]) TypedValueNamespace "foreign"

localDeclarationOriginProgram :: TypedProgram
localDeclarationOriginProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-local-declaration-origin"
    modulePath = ["Fixture", fixture]
    scheme = monoScheme localDeclarationOriginBinder
    statements =
      [ TypedLetStatement
          localDeclarationOriginBinder
          localDeclarationOriginName
          span1
          scheme
          trueExpr
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
    [ typedModule
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
            <> [ expressionStatement 1 (TypedVariableExpr importedSomeInfo importedSome),
                 expressionStatement 2 (TypedVariableExpr textInfo localValue)
               ]
        )
        textInfo

builtinGeneratedNamesProgram :: TypedProgram
builtinGeneratedNamesProgram =
  programWith
    "builtin-generated-names"
    ( expressionStatement 1 (TypedVariableExpr builtinMapInfo (TypedBuiltinName "map"))
        : zipWith expressionStatement [2 ..] generatedLambdas
    )
    emptyInterface
    functionInfo
  where
    modulePath = ["Fixture", "builtin-generated-names"]
    generatedNames =
      [ TypedGeneratedName (TypedLambdaPatternArgument 0),
        TypedGeneratedName (TypedOperatorBinding "$operator:%2B"),
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
          (TypedVariableExpr textInfo name)
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
      expressionStatement 4 (TypedVariableExpr optionConstructorInfo optionConstructor)
    ]
    emptyInterface
    optionConstructorInfo
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
    optionConstructorInfo =
      info
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))

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
    [TypedLetStatement valueBinder valueName span1 scheme valueExpression]
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
    firstArgumentName = fixtureValueName "first"
    secondArgumentName = fixtureValueName "second"
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
        (binder ["Fixture", fixture] [0, 0] firstArgumentName)
        firstArgumentName
        ( TypedLambdaExpr
            (info innerType innerRecipe)
            (binder ["Fixture", fixture] [0, 0, 0] secondArgumentName)
            secondArgumentName
            ( TypedVariableExpr
                (info (TypedTypeParameterType parameter0) (TypedRepresentationParameterRecipe parameter0))
                firstArgumentName
            )
        )
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
  programWith
    fixture
    [ TypedLetStatement
        owner
        name
        span1
        scheme
        (polymorphicIdentityExpression ["Fixture", fixture] [0] parameterId),
      expressionStatement 2 expression
    ]
    emptyInterface
    instantiatedInfo
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
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType TypedBoolType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
        [instantiation]
        []
    expression =
      case explicitSpan of
        Nothing -> TypedVariableExpr instantiatedInfo name
        Just explicitApplicationSpan ->
          TypedTypeApplicationExpr
            instantiatedInfo
            (TypedVariableExpr instantiatedInfo name)
            explicitApplicationSpan
            TypedBoolType

explicitCapabilityEvidenceProgram :: TypedProgram
explicitCapabilityEvidenceProgram = evidenceProgram "explicit-capability-evidence" (Just (TypedEvidenceParameterId 0))

inferredCapabilityEvidenceProgram :: TypedProgram
inferredCapabilityEvidenceProgram = evidenceProgram "inferred-capability-evidence" Nothing

evidenceProgram :: Text -> Maybe TypedEvidenceParameterId -> TypedProgram
evidenceProgram fixture parameterId =
  withFixturePrelude (programWith fixture [TypedLetStatement valueBinder valueName span1 scheme trueExpr, expressionStatement 1 expression] emptyInterface boolInfo)
  where
    capability = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder ["Fixture", fixture] [0] valueName
    evidenceUse =
      TypedEvidenceUse
        (TypedEvidenceParameterRef valueBinder <$> parameterId)
        capability
        implId
        Nothing
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
  withFixturePrelude (programWith fixture [expressionStatement 1 expression] emptyInterface boolInfo)
  where
    fixture = "qualified-method-selection"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint "Equal" (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse]) (TypedBooleanLiteral True)

partialMethodCandidatesProgram :: TypedProgram
partialMethodCandidatesProgram =
  withFixturePrelude
    ( programWith
        fixture
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                secondImpl
                [method, fixtureImplMethod ["Fixture", fixture] [0, 1] secondImpl "render"]
            ),
          expressionStatement 1 expression
        ]
        emptyInterface
        builtinMapInfo
    )
  where
    fixture = "partial-method-candidates"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    constraint = TypedCapabilityConstraint "Render" (Just "Render.map") TypedTextType
    firstImpl = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    secondImpl = TypedImplId ["Fixture", fixture] capabilityName [TypedTextType]
    candidates =
      [ TypedEvidenceCandidate firstImpl (Just (TypedMethodId firstImpl "map")),
        TypedEvidenceCandidate secondImpl (Just (TypedMethodId secondImpl "map"))
      ]
    methodName = resolved TypedCurrentModule TypedValueNamespace "map"
    method =
      TypedMethodDefinition
        (TypedMethodId secondImpl "map")
        (binder ["Fixture", fixture] [0] methodName)
        methodName
        span1
        methodExpression
    methodArgument = resolved TypedCurrentModule TypedValueNamespace "methodArgument"
    methodExpression = TypedLambdaExpr boolToBoolInfo (binder ["Fixture", fixture] [0, 0] methodArgument) methodArgument trueExpr
    expression =
      TypedVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [TypedEvidenceCandidates constraint candidates])
        (TypedBuiltinName "map")

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
    orPatternName = resolved TypedCurrentModule TypedValueNamespace "value-7"
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
    constructorInfo =
      info
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
    optionScrutinee = TypedApplyExpr optionInfo (TypedVariableExpr constructorInfo someName) trueExpr
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
    firstBinder = binder ["Fixture", fixture] [0] valueName
    secondBinder = binder ["Fixture", fixture] [1] valueName
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
    preludeScheme = TypedScheme preludeBinder [] [] [] TypedBoolType TypedBoolRecipe
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
    constructorBinder = binder ["Library", "Flag"] [0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedBoolRecipe]]
    libraryModule =
      typedModule
        ["Library", "Flag"]
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

dataDeclarationWithNullaryConstructor :: [Text] -> [Int] -> TypedCoreName -> [TypedTypeParameterId] -> TypedDataDeclaration
dataDeclarationWithNullaryConstructor modulePath lexicalPath dataName parameters =
  TypedDataDeclaration
    span1
    dataName
    parameters
    [ TypedConstructorDeclaration
        (binder modulePath lexicalPath constructorName)
        constructorName
        []
        []
    ]
  where
    constructorName =
      case dataName of
        TypedResolvedName origin _ identifier ->
          TypedResolvedName origin TypedConstructorNamespace identifier
        other -> other

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
    functionName = resolved TypedCurrentModule TypedValueNamespace "argument"
    functionExpr = TypedLambdaExpr boolToBoolInfo (binder ["Fixture", fixture] [0, 0] functionName) functionName trueExpr
    argumentExpr = literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')
    expression = TypedApplyExpr boolInfo functionExpr argumentExpr

applicationResultTypeFixture :: InvalidFixture
applicationResultTypeFixture =
  expressionFixture fixture expression [expressionFailure fixture TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType)]
  where
    fixture = "application-result-type"
    functionName = resolved TypedCurrentModule TypedValueNamespace "argument"
    functionExpr = TypedLambdaExpr boolToBoolInfo (binder ["Fixture", fixture] [0, 0] functionName) functionName trueExpr
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
    program = withFixturePrelude (signatureProgram fixture valueBinder valueName scheme)
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
    expression = TypedVariableExpr (TypedNodeInfo builtinMapType builtinMapRecipe [instantiation] []) (TypedBuiltinName "map")

missingOrDuplicateEvidenceFixture :: InvalidFixture
missingOrDuplicateEvidenceFixture =
  InvalidFixture fixture program failures
  where
    fixture = "missing-or-duplicate-evidence"
    constraint = TypedCapabilityConstraint "Equal" Nothing TypedBoolType
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
    program = withFixturePrelude (singleModuleProgram fixture relativeSource [] [expressionStatement 1 missingExpression, expressionStatement 2 duplicateExpression] emptyInterface boolInfo ["Fixture", fixture])
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
      TypedLiteralExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedEvidenceCandidates constraint [TypedEvidenceCandidate firstImpl Nothing, TypedEvidenceCandidate secondImpl Nothing]])
        (TypedTextLiteral "ambiguous")
    invisibleUse = TypedEvidenceUse Nothing constraint invisibleImpl Nothing
    invisibleExpression =
      TypedLiteralExpr
        (TypedNodeInfo TypedTextType TypedManagedTextRecipe [] [TypedSelectedEvidence invisibleUse])
        (TypedTextLiteral "invisible")
    program = withFixturePrelude (singleModuleProgram fixture relativeSource [] [expressionStatement 1 ambiguousExpression, expressionStatement 2 invisibleExpression] emptyInterface textInfo ["Fixture", fixture])
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
            ["Fixture", fixture]
        )
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
typedModule modulePath sourcePath imports exports interface statements moduleInfo =
  TypedModule
    modulePath
    sourcePath
    imports
    exports
    interface
    statements
    (if hasTerminalExpression statements then moduleInfo else unitInfo)

hasTerminalExpression :: [TypedStatement] -> Bool
hasTerminalExpression statements =
  case reverse statements of
    TypedExpressionStatement {} : _ -> True
    _ -> False

polymorphicIdentityExpression :: [Text] -> [Int] -> TypedTypeParameterId -> TypedExpr
polymorphicIdentityExpression modulePath lexicalPath parameterId =
  TypedLambdaExpr
    functionInfo
    (binder modulePath (lexicalPath <> [0]) argumentName)
    argumentName
    (TypedVariableExpr parameterInfo argumentName)
  where
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    parameterInfo = info parameterType parameterRecipe
    functionInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)

boolBinaryFunctionExpression :: [Text] -> [Int] -> TypedExpr
boolBinaryFunctionExpression modulePath lexicalPath =
  TypedLambdaExpr
    binaryInfo
    (binder modulePath (lexicalPath <> [0]) leftName)
    leftName
    ( TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath (lexicalPath <> [0, 0]) rightName)
        rightName
        trueExpr
    )
  where
    leftName = resolved TypedCurrentModule TypedValueNamespace "left"
    rightName = resolved TypedCurrentModule TypedValueNamespace "right"
    binaryInfo =
      info
        (TypedFunctionType TypedBoolType boolToBoolType)
        (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)

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

builtinMapType :: TypedType
builtinMapType =
  TypedFunctionType
    (TypedFunctionType TypedBoolType TypedTextType)
    (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedTextType))

builtinMapRecipe :: TypedRepresentationRecipe
builtinMapRecipe =
  TypedClosureRecipe
    [ TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe,
      TypedManagedListRecipe TypedBoolRecipe
    ]
    (TypedManagedListRecipe TypedManagedTextRecipe)

builtinMapInfo :: TypedNodeInfo
builtinMapInfo = info builtinMapType builtinMapRecipe

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
