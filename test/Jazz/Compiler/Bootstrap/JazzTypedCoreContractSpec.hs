{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( CanonicalTypedCoreStructure,
    canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedProgramRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    decodeCanonicalTypedCoreStructure,
    decodeCanonicalTypedValidationFailuresRuntimeValue,
  )
import Jazz.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName,
    builtinSymbolOwnership,
  )
import Jazz.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )
import Jazz.TestSource (readCheckedInJazzProjectModuleSource)
import System.Timeout (timeout)

main :: IO ()
main = runTestSuite "JazzTypedCoreContract" tests

tests :: [NamedTest]
tests =
  coreTests <> map fst reviewRegressionGroups <> [("matches Haskell validation for every fixed and review fixture twice", testJazzValidationParity)]

coreTests :: [NamedTest]
coreTests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("encodes every typed-core outcome constructor", testOutcomeEncoding),
    ("accepts every fixed valid program", testValidPrograms),
    ("audits the fixed invalid fixture manifest", testInvalidFixtureManifest),
    ("reports every fixed invalid program exactly", testInvalidPrograms),
    ("audits the combined fixed fixture count", testCombinedFixtureCount),
    ("round-trips canonical validation failures through the checked adapter", testCheckedValidationAdapterRoundTrip),
    ("rejects unknown validation constructors", testCheckedValidationAdapterUnknownConstructor),
    ("rejects wrong validation constructor arity", testCheckedValidationAdapterWrongArity),
    ("rejects wrong validation field categories", testCheckedValidationAdapterWrongFieldCategory),
    ("rejects malformed nested binder identities", testCheckedValidationAdapterMalformedBinder),
    ("rejects malformed nested impl identities", testCheckedValidationAdapterMalformedImpl),
    ("rejects host-specific name identities", testCheckedValidationAdapterHostName),
    ("rejects runtime values in structural fields", testCheckedValidationAdapterRuntimeValue),
    ("rejects absolute source-path constructors in structural fields", testCheckedValidationAdapterAbsoluteSourcePath),
    ("audits fixture uniqueness and complete validation-kind coverage", testFixtureCoverage)
  ]

testValidFixtureManifest :: IO ()
testValidFixtureManifest = do
  assertEqual "valid fixture names" expectedValidFixtureNames (map validFixtureName validFixtures)
  assertEqual "valid fixture count" 19 (length validFixtures)

testOutcomeEncoding :: IO ()
testOutcomeEncoding = do
  let failure = TypedCoreValidationFailure TypedProgramPath TypedUnknownEntryModule TypedNoValidationDetail
      outcomes =
        [ TypedCoreBlockedByDiagnostics,
          TypedCoreInvariantFailures [failure],
          TypedCoreSucceeded scalarAliasesWidthsProgram
        ]
      expected =
        [ canonicalNullaryConstructor "TypedCoreBlockedByDiagnostics",
          canonicalConstructor
            "TypedCoreInvariantFailures"
            [canonicalTypedValidationFailuresRuntimeValue [failure]],
          canonicalConstructor
            "TypedCoreSucceeded"
            [canonicalTypedProgramRuntimeValue scalarAliasesWidthsProgram]
        ]
  assertEqual
    "typed-core outcome encoding"
    expected
    (map canonicalTypedCoreOutcomeRuntimeValue outcomes)

testValidPrograms :: IO ()
testValidPrograms =
  mapM_
    (\fixture -> assertEqual (validFixtureName fixture <> " valid failures") [] (validateTypedProgram (validFixtureProgram fixture)))
    validFixtures

testInvalidFixtureManifest :: IO ()
testInvalidFixtureManifest = do
  assertEqual "invalid fixture names" expectedInvalidFixtureNames (map invalidFixtureName invalidFixtures)
  assertEqual "invalid fixture count" 46 (length invalidFixtures)

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
  assertEqual "combined fixture count" 65 (length validFixtures + length invalidFixtures)

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
  let validNames = map validFixtureName validFixtures
      invalidNames = map invalidFixtureName invalidFixtures
      names = validNames <> invalidNames
      observedKinds =
        [kind | fixture <- invalidFixtures, TypedCoreValidationFailure _ kind _ <- invalidFixtureFailures fixture]
          <> [kind | program <- reviewRegressionPrograms, TypedCoreValidationFailure _ kind _ <- validateTypedProgram program]
  assertEqual "valid and invalid fixture names are disjoint" [] [name | name <- validNames, name `elem` invalidNames]
  assertEqual "fixed fixture manifests are exhaustive" (expectedValidFixtureNames <> expectedInvalidFixtureNames) names
  assertEqual "fixture names are unique" (length names) (length (nub names))
  assertEqual "review regression programs are unique" (length reviewRegressionPrograms) (length (nub reviewRegressionPrograms))
  assertEqual "uncovered validation kinds" [] (filter (`notElem` observedKinds) allValidationKinds)

allValidationKinds :: [TypedCoreValidationKind]
allValidationKinds = [minBound .. maxBound]

testJazzValidationParity :: IO ()
testJazzValidationParity = do
  let programs = map validFixtureProgram validFixtures <> map invalidFixtureProgram invalidFixtures <> reviewRegressionPrograms
      expectedRuntimeValue =
        VList
          [ VTuple
              [ canonicalTypedProgramRuntimeValue program,
                canonicalTypedValidationFailuresRuntimeValue (validateTypedProgram program)
              ]
          | program <- programs
          ]
          Nothing
      expected = decodeCanonicalTypedCoreStructure expectedRuntimeValue
  first <- runJazzValidationBatch programs
  second <- runJazzValidationBatch programs
  assertJazzStructure "Jazz validation first run" expected first
  assertJazzStructure "Jazz validation second run" expected second
  assertEqual "Jazz validation deterministic structure" (checkedRunStructure first) (checkedRunStructure second)

testNestedBlockValidationRegressions :: IO ()
testNestedBlockValidationRegressions = do
  assertEqual
    "nested expression path is distinct from its containing block"
    nestedPathFailures
    (validateTypedProgram nestedPathProgram)
  assertEqual
    "block declarations fail at the statement-scope boundary"
    nestedDeclarationFailures
    (validateTypedProgram nestedDeclarationProgram)
  assertEqual
    "block-local binder identities remain unique"
    nestedDuplicateBinderFailures
    (validateTypedProgram nestedDuplicateBinderProgram)
  assertEqual
    "guarded case children use one flattened expression path"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-guarded-case-path") [0] [0, 1])
        TypedUnresolvedName
        (TypedNameDetail (TypedUnresolvedSourceName "guard")),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-guarded-case-path") [0] [0, 2])
        TypedUnresolvedName
        (TypedNameDetail (TypedUnresolvedSourceName "first-result")),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-guarded-case-path") [0] [0, 3])
        TypedUnresolvedName
        (TypedNameDetail (TypedUnresolvedSourceName "second-result"))
    ]
    (validateTypedProgram guardedCasePathProgram)

reviewRegressionGroups :: [(NamedTest, [TypedProgram])]
-- The lone-surrogate character regression remains Haskell-only because its
-- invalid scalar cannot be encoded in the Jazz source used by hosted parity.
reviewRegressionGroups =
  [ (("accepts semantic scalar aliases at application boundaries", testApplicationScalarAliasCompatibility), [applicationScalarAliasProgram]),
    (("rejects malformed nested block contracts at unique statement paths", testNestedBlockValidationRegressions), [nestedPathProgram, nestedDeclarationProgram, nestedDuplicateBinderProgram, guardedCasePathProgram]),
    (("enforces typed-core scope and visibility contracts", testScopeAndVisibilityRegressions), [generalizedLetScopeProgram, importedInstantiationProgram, invisibleSiblingImplProgram, selectedEvidenceTargetProgram, invisibleVariableProgram, selectedMethodContractProgram, enclosingImplMethodProgram]),
    (("enforces typed-core value-shape contracts", testValueShapeRegressions), [bindingValueProgram, lambdaResultProgram, literalTypeProgram, collectionShapeProgram, dataTypeArityProgram, tuplePatternShapeProgram, moduleResultProgram, schemeDataTypeProgram, driveAbsoluteProgram]),
    (("enforces follow-up typed-core boundary contracts", testReviewFollowupRegressions), [instantiationDataTypeProgram, literalPatternProgram, invisibleOperatorProgram, expressionDuplicateBinderProgram, privateInterfaceLeakProgram, constructorPatternContractProgram, nonListPatternProgram, explicitTypeApplicationContractProgram, variableSchemeContractProgram, missingImportProgram, candidateConstraintProgram, invalidVariableNamespaceProgram]),
    (("enforces latest typed-core review contracts", testLatestReviewRegressions), [binderNameContractProgram, blockLocalGeneralizedSchemeProgram, blockLocalMonomorphicSchemeProgram, implMethodNameProgram, blockResultProgram, nestedCasePatternPathProgram, operatorSchemeProgram, selectiveImportProgram, classParameterScopeProgram, evidenceParameterContractProgram, implCapabilityNamespaceProgram]),
    (("enforces newest typed-core review contracts", testNewestReviewRegressions), [missingInstantiatedEvidenceProgram, constructorExpressionContractProgram, unrelatedTypeApplicationProgram, lexicalBinderContractProgram, generalizedVariableContractProgram, enclosingInstantiationScopeProgram, implMethodContractProgram, dataDeclarationNamespaceProgram, duplicateDeclarationProgram, importedImplQualificationProgram, implTargetArityProgram, localDeclarationOriginProgram]),
    (("retains data metadata for selectively imported values", testSelectedValueDataMetadata), [selectedValueDataMetadataProgram]),
    (("filters private impls from selective imports", testSelectiveImportImplLeak), [selectiveImportImplLeakProgram]),
    (("requires selected evidence methods to exist", testSelectedEvidenceMethodExistence), [selectedEvidenceMethodExistenceProgram]),
    (("rejects duplicate impl methods", testDuplicateImplMethods), [duplicateImplMethodProgram]),
    (("retains outer type scope in nested bindings", testNestedOuterTypeScope), [nestedOuterTypeScopeProgram]),
    (("keeps impl methods out of ordinary value scope", testImplMethodValueVisibility), [implMethodValueVisibilityProgram]),
    (("validates builtin operator contracts", testBuiltinOperatorContracts), [builtinOperatorContractProgram]),
    (("limits candidate deferral to qualified methods", testOrdinaryFunctionCandidateAmbiguity), [ordinaryFunctionCandidateAmbiguityProgram]),
    (("validates numeric primitive constraint targets", testNumericPrimitiveConstraintTargets), [invalidNumericPrimitiveConstraintProgram]),
    (("requires polymorphic variable instantiations", testMissingPolymorphicInstantiation), [missingPolymorphicInstantiationProgram]),
    (("rejects unsupported strict equality constraints", testUnsupportedStrictEqualityConstraint), [unsupportedStrictEqualityConstraintProgram]),
    (("checks builtin and generated name visibility", testUncheckedSpecialNames), [uncheckedSpecialNameProgram]),
    (("exports target-independent class methods with dispatch", testClassMethodExport), [classMethodExportProgram, missingTargetIndependentClassMethodDispatchProgram]),
    (("matches fractional literal suffix widths", testFractionalLiteralSuffix), [fractionalLiteralSuffixProgram]),
    (("checks Prelude evidence implementations", testMissingPreludeImpl), [missingPreludeImplProgram]),
    (("retains type scope for evidence", testEvidenceTypeScope), [evidenceTypeScopeProgram]),
    (("checks constructor pattern data ownership", testWrongConstructorPatternType), [wrongConstructorPatternTypeProgram]),
    (("checks local impl module ownership", testForeignOwnedLocalImpl), [foreignOwnedLocalImplProgram]),
    (("imports type-exported capability metadata", testImportedTypeCapabilityMetadata), [importedTypeCapabilityMetadataProgram]),
    (("rejects callable builtin equality", testCallableBuiltinEquality), [callableBuiltinEqualityProgram]),
    (("enforces current typed-core review contracts", testCurrentReviewRegressions), [moduleInfoStructuralEqualityProgram, typeApplicationResultContractProgram, capabilityConstraintVisibilityProgram, unconstrainedNumericParameterProgram, unconstrainedEqualityParameterProgram, duplicatePatternNameProgram, duplicateOrPatternContractProgram, nonTuplePatternProgram, ownerAmbiguousEvidenceProgram, reorderedOrPatternProgram, emptyPatternCaseProgram, typeVisibleImplImportProgram, methodVisibleImplImportProgram, integralLiteralRangeProgram]),
    (("enforces latest bot-reviewed typed-core contracts", testLatestBotReviewRegressions), [nestedStrictEqualityConstraintProgram, canonicalQualifiedMethodKeyProgram, wrongQualifiedMethodKeyProgram, builtinValueContractProgram, missingInterfaceMetadataProgram, unterminatedBlockProgram, constrainedMonomorphicUseProgram, unrelatedKnownInstantiationProgram]),
    (("enforces newest bot-reviewed typed-core contracts", testNewestBotReviewRegressions), [explicitHeadParameterProgram, classArityProgram, classMethodSchemeShapeProgram, duplicateImplDeclarationProgram, emptyOrPatternProgram, nonBindingTypeApplicationProgram, mismatchedResolvedOperatorProgram, dataInterfaceDependencyProgram, classMethodInterfaceDependencyProgram]),
    (("enforces post-newest bot-reviewed typed-core contracts", testPostNewestBotReviewRegressions), [instantiatedPrimitiveConstraintProgram, typeApplicationExtraOwnerProgram, constrainedResolvedOperatorProgram, missingModuleResultProgram, emptyDataDeclarationProgram, laterOrPatternBinderCollisionProgram, concreteIntegerBoundsProgram, incompleteImplProgram, duplicateInstantiationProgram]),
    (("rejects globally reserved typed-core names", testReservedValueTypedCoreBoundary), [reservedValueIdentifierProgram, reservedValueModulePathProgram]),
    (("checks fractional literals against their selected floating widths", testFractionalLiteralBounds), [fractionalLiteralBoundsProgram]),
    (("rejects local classes that collide with visible classes", testVisibleClassCollisions), [visibleClassCollisionProgram]),
    (("retains method data metadata across selective class facades", testSelectedClassDataDependency), [selectedClassDataDependencyProgram]),
    (("resolves shadowed schemes through lexical scope", testLexicalSchemeShadowing), [lexicalSchemeShadowingProgram]),
    (("rejects method candidates after full application", testFullyAppliedMethodCandidates), [fullyAppliedMethodCandidatesProgram]),
    (("rejects duplicate unbound selected evidence", testDuplicateUnboundEvidence), [duplicateUnboundEvidenceProgram]),
    (("generalizes imported class methods without losing dispatch", testGeneralizedClassMethodImport), [generalizedClassMethodImportProgram, missingImportedClassMethodDispatchProgram]),
    (("rejects colliding imported class identifiers", testImportedClassCollision), [importedClassCollisionProgram]),
    (("preserves block statement scope order", testForwardBlockReference), [forwardBlockReferenceProgram]),
    (("preserves proven recursive block peers", testRecursiveBlockPeers), [recursiveBlockPeerProgram]),
    (("rejects malformed generalized literal bounds", testMalformedLiteralConstraintBounds), [malformedLiteralConstraintBoundsProgram]),
    (("preserves instantiated evidence order", testEvidenceSelectionOrder), [evidenceSelectionOrderProgram]),
    (("keeps private capability metadata out of source visibility", testPrivateCapabilityMetadataVisibility), [privateCapabilityMetadataVisibilityProgram]),
    (("matches module-qualified method keys to their full capability origin", testModuleQualifiedMethodKey), [moduleQualifiedMethodKeyProgram, forgedModuleQualifiedMethodKeyProgram]),
    (("retains imported data dependencies through exported schemes", testImportedDataDependencyMetadata), [importedDataDependencyProgram]),
    (("closes selected data contracts over field metadata", testTransitiveDataContractDependency), [transitiveDataContractDependencyProgram]),
    (("rejects imported capability dependencies that lose identity", testImportedCapabilityDependency), [importedCapabilityDependencyProgram]),
    (("keeps metadata-only impls out of evidence visibility", testMetadataOnlyImplVisibility), [metadataOnlyImplVisibilityProgram]),
    (("rejects expression-only metadata on patterns", testPatternExpressionMetadata), [patternExpressionMetadataProgram]),
    (("allows phantom data arguments in strict equality", testPhantomDataEquality), [phantomDataEqualityProgram]),
    (("preserves same-scope value rebinding", testSameScopeValueRebinding), [sameScopeValueRebindingProgram]),
    (("locks narrow forward signed-function visibility", testForwardSignedFunctionVisibility), map snd forwardSignedVisibilityPrograms),
    (("keeps forward signed-function visibility out of nested blocks", testNestedForwardSignedFunctionInvisibility), [nestedForwardSignedFunctionProgram]),
    (("preserves top-level statement scope order", testForwardModuleReference), [forwardModuleReferenceProgram]),
    (("rejects cyclic resolved imports", testCyclicResolvedImports), [cyclicImportProgram]),
    (("keeps bare signatures out of executable value scope", testBareSignatureVisibility), [bareSignatureVisibilityProgram]),
    (("exports only the active rebinding scheme", testActiveRebindingExport), [activeRebindingExportProgram]),
    (("accepts constructor-owned instantiations", testConstructorInstantiation), [constructorInstantiationProgram]),
    (("requires dependency-first resolved module ordering", testResolvedModuleOrder), [resolvedModuleOrderProgram]),
    (("rejects empty resolved identifiers", testEmptyResolvedIdentifier), [emptyResolvedIdentifierProgram]),
    (("forbids explicit spans on implicit instantiations", testExplicitSpanOnVariable), [explicitSpanOnVariableProgram]),
    (("limits single evidence candidates to method deferral", testSingleEvidenceCandidate), [singleEvidenceCandidateProgram]),
    (("rejects structurally empty module paths", testEmptyModulePath), [emptyModulePathProgram]),
    (("requires the ambient prelude slot to identify Prelude", testAmbientPreludePath), [wrongPreludeSlotProgram]),
    (("checks adjacent signatures against their bindings", testSignatureBindingContract), [signatureBindingMismatchProgram, signatureBindingShapeMismatchProgram]),
    (("derives callable parameter contracts from lambda recipes", testLambdaCallableParameterRecipeContract), [lambdaCallableParameterRecipeProgram]),
    (("accepts explicit type application on qualified methods", testQualifiedMethodTypeApplication), [qualifiedMethodTypeApplicationProgram]),
    (("enforces final typed-core review contracts", testFinalReviewRegressions), [aliasShapedSelfRecursionProgram, qualifiedMethodValueContractProgram, eagerSelfReferenceProgram]),
    (("enforces post-final typed-core review contracts", testPostFinalReviewRegressions), [importNameCollisionProgram, localClassMethodVisibilityProgram, syntheticBinderShadowingProgram, implFreeClassParameterProgram, duplicateQualifiedMethodCandidateProgram, metadataOnlySourceTypeProgram]),
    (("keeps retained capabilities out of source while allowing inferred schemes", testMethodOnlyCapabilityVisibility), [inferredMethodOnlyCapabilityVisibilityProgram, explicitMethodOnlyCapabilityVisibilityProgram]),
    (("includes capabilities in import collision checks", testCapabilityImportCollision), [capabilityImportCollisionProgram]),
    (("rejects nested type-parameter ordinal shadowing", testNestedTypeParameterShadowing), [nestedTypeParameterShadowingProgram]),
    (("rejects type-only explicit import selectors", testTypeOnlyImportSelector), [typeOnlyImportSelectorProgram]),
    (("rejects unbound selected evidence on ordinary nodes", testOrdinaryUnboundEvidence), [ordinaryUnboundEvidenceProgram]),
    (("preserves nested local generalization under generics", testNestedLocalGeneralization), [nestedLocalGeneralizationProgram]),
    (("rejects non-concrete impl targets", testNonConcreteImplTarget), [nonConcreteImplTargetProgram]),
    (("rejects module-scope declarations inside blocks", testBlockDeclarationScope), [blockDeclarationScopeProgram]),
    (("rejects non-lexical module path segments", testModulePathIdentifierSegments), [delimiterModulePathProgram, slashModulePathProgram, reservedModulePathProgram]),
    (("requires module metadata to equal the terminal node", testModuleMetadataIdentity), [moduleMetadataIdentityProgram]),
    (("rejects stray qualified type-application instantiations", testQualifiedTypeApplicationInstantiation), [qualifiedTypeApplicationInstantiationProgram]),
    (("keeps local class methods out of active scheme lookup", testLocalClassMethodSchemeIsolation), [localClassMethodAfterValueProgram, localClassMethodBeforeValueProgram]),
    (("retains imported capability evidence through selective values", testRetainedCapabilityEvidence), [retainedCapabilityEvidenceProgram]),
    (("matches retained evidence parameters to their capability", testRetainedCapabilityEvidenceOrigin), [retainedCapabilityWrongImplProgram]),
    (("requires polymorphic constructor instantiation metadata", testMissingConstructorInstantiation), [missingConstructorInstantiationProgram]),
    (("publishes impl identities for retained capabilities", testMissingPublishedImpl), [missingPublishedImplProgram]),
    (("terminates equality checks for expanding recursive types", testExpandingRecursiveEquality), [expandingRecursiveEqualityProgram]),
    (("checks non-recursive equality fields after recursion", testRecursiveEqualityCallableField), [recursiveEqualityCallableFieldProgram]),
    (("rejects orphan signatures", testOrphanSignature), [orphanSignatureProgram]),
    (("checks transformed recursive equality payloads", testRecursiveEqualityNestedCallable), [recursiveEqualityNestedCallableProgram]),
    (("rejects current-module imported origins", testImportedCurrentOrigin), [importedCurrentOriginProgram]),
    (("keeps retained capabilities out of explicit exports", testRetainedCapabilityExport), [retainedCapabilityExportProgram]),
    (("rejects duplicate import aliases", testImportAliasCollision), [importAliasCollisionProgram]),
    (("preserves class-before-impl ordering", testImplBeforeClass), [implBeforeClassProgram]),
    (("matches evidence capability origins", testEvidenceCapabilityOrigin), [evidenceCapabilityOriginProgram]),
    (("rejects malformed generated-name payloads", testMalformedGeneratedNames), [malformedGeneratedNamesProgram]),
    (("reserves Prelude for the explicit slot", testRegularPreludeModule), [regularPreludeModuleProgram]),
    (("keeps retained class methods out of explicit exports", testRetainedClassMethodExport), [retainedClassMethodExportProgram]),
    (("rejects malformed resolved-name payloads", testMalformedResolvedIdentifiers), [malformedResolvedIdentifiersProgram]),
    (("normalizes duplicate Prelude impl identities", testNormalizedPreludeImplDuplicates), [normalizedPreludeImplDuplicatesProgram]),
    (("rejects malformed import aliases", testMalformedImportAlias), [malformedImportAliasProgram]),
    (("rejects duplicate module exports", testDuplicateModuleExports), [duplicateModuleExportsProgram]),
    (("rejects non-positive source spans", testInvalidSourceSpans), [invalidImportSpanProgram, invalidStatementSpansProgram, invalidDeclarationSpansProgram, invalidExpressionSpansProgram]),
    (("enforces canonical typed-core inventories", testUnresolvedReviewRegressions), [emptyImportSelectorProgram, duplicateImportSelectorProgram, aliasAndSelectorImportProgram, distinctClassMethodProgram, duplicateEvidenceConstraintProgram, singletonTupleTypeProgram, preludeAmbientDataDependencyProgram, duplicateModuleInterfaceEntriesProgram, sameNamedCapabilityDependencyProgram, sameNamedRetainedCapabilityProgram]),
    (("allows local classes beside alias-qualified imports", testAliasedCapabilityLocalClass), [aliasedCapabilityLocalClassProgram]),
    (("preserves phantom parameters through equality recursion", testRecursivePhantomDataEquality), [recursivePhantomDataEqualityProgram]),
    (("retains data metadata for published impl targets", testPublishedImplDataMetadata), [publishedImplDataMetadataProgram]),
    (("validates converging import graphs without path explosion", testDenseImportDag), [denseImportDagProgram 10]),
    (("computes dense recursive groups without repeated graph searches", testDenseBindingDag), [denseBindingDagProgram 10]),
    (("rejects invalid resolved operator symbols", testInvalidResolvedOperatorSymbols), [invalidResolvedOperatorSymbolsProgram]),
    (("requires one selected body for qualified methods", testAmbiguousQualifiedMethodSelection), [ambiguousQualifiedMethodSelectionProgram]),
    (("validates repeated equality subgraphs once", testRepeatedEqualityDag), [repeatedEqualityDagProgram 10]),
    (("validates recursive phantom equality graphs without path explosion", testRecursivePhantomEqualityDag), [recursivePhantomEqualityDagProgram 8]),
    (("requires capability metadata for published impls", testPublishedImplCapabilityMetadata), [publishedImplWithoutCapabilityMetadataProgram]),
    (("keeps selected impls inside deferred candidate sets", testDeferredCandidateSelection), [deferredCandidateSelectionProgram]),
    (("rejects duplicate deferred evidence obligations", testDuplicateDeferredEvidence), [duplicateDeferredEvidenceProgram]),
    (("rejects ambiguous value exports", testAmbiguousValueExport), [ambiguousValueExportProgram]),
    (("entails polymorphic primitive instantiations", testPrimitiveInstantiationEntailment), [unentailedPrimitiveInstantiationProgram]),
    (("requires source-addressable class methods", testGeneratedClassMethodName), [generatedClassMethodNameProgram]),
    (("rejects singleton or-pattern nodes", testSingletonOrPattern), [singletonOrPatternProgram]),
    (("rejects fractional literal patterns", testFractionalLiteralPattern), [fractionalPatternProgram]),
    (("enforces constructor-like identifier casing", testConstructorLikeIdentifierCasing), [lowercaseConstructorLikeNamesProgram]),
    (("orders duplicate name failures before impl failures", testDuplicateDeclarationOrdering), [duplicateDeclarationOrderingProgram]),
    (("covers every builtin catalog contract in hosted parity", testBuiltinCatalogParity), [builtinCatalogProgram]),
    (("rejects deferred evidence after the target argument", testAppliedTargetCandidateDeferral), [appliedTargetCandidateDeferralProgram]),
    (("binds capability exports to local class interfaces", testLocalCapabilityExportIdentity), [localCapabilityExportIdentityProgram]),
    (("enforces exact retained classes and structural identities", testStructuralIdentityRegressions), [invalidRetainedClassSpanProgram, duplicateRetainedClassMethodProgram, negativeBinderPathProgram, wrongDataNamespaceProgram, wrongConstructorNamespaceProgram]),
    (("preserves deferred evidence across applications", testDroppedDeferredEvidence), [droppedDeferredEvidenceProgram]),
    (("preserves deferred candidate order across applications", testReorderedDeferredEvidence), [reorderedDeferredEvidenceProgram]),
    (("preserves selected evidence across applications", testSelectedEvidenceProgression), [selectedEvidenceProgressionProgram]),
    (("keeps explicit instantiations consistent with their callees", testExplicitInstantiationProgression), [mismatchedExplicitInstantiationProgram]),
    (("validates evidence capability identities", testForgedEvidenceCapability), [forgedEvidenceCapabilityProgram]),
    (("rejects empty monomorphic instantiations", testEmptyMonomorphicInstantiation), [emptyMonomorphicInstantiationProgram])
  ]

testApplicationScalarAliasCompatibility :: IO ()
testApplicationScalarAliasCompatibility =
  assertEqual
    "Int/Int64 and Float/Float64 remain compatible application types"
    []
    (validateTypedProgram applicationScalarAliasProgram)

testLambdaCallableParameterRecipeContract :: IO ()
testLambdaCallableParameterRecipeContract =
  assertEqual
    "staged callable lambda parameter recipe"
    []
    (validateTypedProgram lambdaCallableParameterRecipeProgram)

lambdaCallableParameterRecipeProgram :: TypedProgram
lambdaCallableParameterRecipeProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [expressionStatement 1 lambda]
    emptyInterface
    lambdaInfo
    modulePath
  where
    fixture = "review-lambda-callable-parameter-recipe"
    modulePath = fixtureModulePath fixture
    parameterName = fixtureValueName "function"
    parameterBinder = binder modulePath [0] parameterName
    parameterType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedCharType TypedTextType)
    parameterRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe]
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    parameterInfo = info parameterType parameterRecipe
    lambdaInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    lambda =
      TypedLambdaExpr
        lambdaInfo
        parameterBinder
        parameterName
        (fixtureBoundVariableExpr parameterBinder parameterInfo parameterName)

applicationScalarAliasProgram :: TypedProgram
applicationScalarAliasProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ intBinding,
      expressionStatement 2 intApplication,
      floatBinding,
      expressionStatement 4 floatApplication
    ]
    emptyInterface
    floatAliasInfo
    modulePath
  where
    fixture = "review-application-scalar-alias"
    modulePath = fixtureModulePath fixture
    int64Type = TypedNumericType TypedInt64Type
    int64Recipe = TypedSignedIntegerRecipe 64
    float64Type = TypedNumericType TypedFloat64Type
    float64Recipe = TypedFloatRecipe 64
    floatAliasInfo = info TypedFloatType float64Recipe
    (intBinding, intApplication) =
      aliasApplication
        0
        "identityInt64"
        int64Type
        TypedIntType
        int64Recipe
        (TypedIntegerLiteral "1")
    (floatBinding, floatApplication) =
      aliasApplication
        2
        "identityFloat64"
        float64Type
        TypedFloatType
        float64Recipe
        (TypedFractionalLiteral "1" "5" Nothing)

    aliasApplication statementIndex nameText explicitType aliasType recipe literal =
      let name = fixtureValueName nameText
          owner = binder modulePath [statementIndex] name
          argumentName = fixtureValueName (nameText <> "Argument")
          argumentOwner = binder modulePath [statementIndex, 0] argumentName
          explicitInfo = info explicitType recipe
          functionType = TypedFunctionType explicitType explicitType
          functionRecipe = TypedClosureRecipe [recipe] recipe
          functionInfo = info functionType functionRecipe
          scheme = fixtureScheme owner [] [] [] functionType functionRecipe
          binding =
            TypedLetStatement
              owner
              name
              span1
              scheme
              (TypedLambdaExpr functionInfo argumentOwner argumentName (fixtureBoundVariableExpr argumentOwner explicitInfo argumentName))
          aliasInfo = info aliasType recipe
          application =
            TypedApplyExpr
              aliasInfo
              (fixtureBoundVariableExpr owner functionInfo name)
              (TypedLiteralExpr aliasInfo literal)
       in (binding, application)

reviewRegressionPrograms :: [TypedProgram]
reviewRegressionPrograms = concatMap snd reviewRegressionGroups

testAliasedCapabilityLocalClass :: IO ()
testAliasedCapabilityLocalClass =
  assertEqual
    "alias-qualified capabilities do not reserve unqualified local class names"
    []
    (validateTypedProgram aliasedCapabilityLocalClassProgram)

testRecursivePhantomDataEquality :: IO ()
testRecursivePhantomDataEquality =
  assertEqual
    "recursive phantom data arguments do not determine structural equality support"
    []
    (validateTypedProgram recursivePhantomDataEqualityProgram)

testPublishedImplDataMetadata :: IO ()
testPublishedImplDataMetadata =
  assertEqual
    "published impl targets retain their local data metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-published-impl-data-metadata"))
        TypedModuleInterfaceMismatch
        (TypedNameDetail publishedImplDataName)
    ]
    (validateTypedProgram publishedImplDataMetadataProgram)

testDenseImportDag :: IO ()
testDenseImportDag = do
  assertEqual
    "small converging import graph is acyclic"
    []
    (validateTypedProgram (denseImportDagProgram 10))
  result <- timeout 2000000 (evaluate (length (validateTypedProgram (denseImportDagProgram 160))))
  case result of
    Nothing -> failTest "large converging import graph exceeded the two-second validation budget"
    Just failureCount -> assertEqual "large converging import graph failures" 0 failureCount

testDenseBindingDag :: IO ()
testDenseBindingDag = do
  assertEqual
    "small dense binding graph is non-recursive"
    []
    (validateTypedProgram (denseBindingDagProgram 10))
  result <- timeout 2000000 (evaluate (length (validateTypedProgram (denseBindingDagProgram 200))))
  case result of
    Nothing -> failTest "large dense binding graph exceeded the two-second validation budget"
    Just failureCount -> assertEqual "large dense binding graph failures" 0 failureCount

testInvalidResolvedOperatorSymbols :: IO ()
testInvalidResolvedOperatorSymbols =
  assertEqual
    "resolved operators reject non-user symbols before dispatch"
    [ statementFailure
        "review-invalid-resolved-operator-symbols"
        1
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%61"))
        ),
      statementFailure
        "review-invalid-resolved-operator-symbols"
        2
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2B"))
        ),
      statementFailure
        "review-invalid-resolved-operator-symbols"
        3
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E"))
        ),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        4
        TypedBindingValueMismatch
        (TypedTextDetail "a"),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        5
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%61"))
        ),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        5
        TypedBindingValueMismatch
        (TypedTextDetail "a"),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        6
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2B"))
        ),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        6
        TypedBindingValueMismatch
        (TypedTextDetail "+"),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        7
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E"))
        ),
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        7
        TypedBindingValueMismatch
        (TypedTextDetail "->")
    ]
    (validateTypedProgram invalidResolvedOperatorSymbolsProgram)

testAmbiguousQualifiedMethodSelection :: IO ()
testAmbiguousQualifiedMethodSelection =
  assertEqual
    "a qualified method value has exactly one selected dispatch body"
    [ expressionFailure
        "review-ambiguous-qualified-method-selection"
        TypedAmbiguousEvidence
        (TypedArityDetail 1 2)
    ]
    (validateTypedProgram ambiguousQualifiedMethodSelectionProgram)

testRepeatedEqualityDag :: IO ()
testRepeatedEqualityDag = do
  assertEqual
    "small repeated equality subgraphs remain supported"
    []
    (validateTypedProgram (repeatedEqualityDagProgram 10))
  result <- timeout 2000000 (evaluate (length (validateTypedProgram (repeatedEqualityDagProgram 25))))
  case result of
    Nothing -> failTest "large repeated equality graph exceeded the two-second validation budget"
    Just failureCount -> assertEqual "large repeated equality graph failures" 0 failureCount

testRecursivePhantomEqualityDag :: IO ()
testRecursivePhantomEqualityDag = do
  assertEqual
    "small recursive phantom equality graphs remain supported"
    []
    (validateTypedProgram (recursivePhantomEqualityDagProgram 8))
  result <- timeout 2000000 (evaluate (length (validateTypedProgram (recursivePhantomEqualityDagProgram 25))))
  case result of
    Nothing -> failTest "large recursive phantom equality graph exceeded the two-second validation budget"
    Just failureCount -> assertEqual "large recursive phantom equality graph failures" 0 failureCount

testPublishedImplCapabilityMetadata :: IO ()
testPublishedImplCapabilityMetadata =
  assertEqual
    "an impl interface requires the matching class interface"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-published-impl-capability-metadata"))
        TypedModuleInterfaceMismatch
        (TypedImplDetail publishedImplWithoutCapabilityMetadataId)
    ]
    (validateTypedProgram publishedImplWithoutCapabilityMetadataProgram)

testDeferredCandidateSelection :: IO ()
testDeferredCandidateSelection =
  assertEqual
    "application evidence selects only an immediate pre-target candidate"
    [ expressionFailureAt
        "review-deferred-candidate-selection"
        1
        TypedMethodSelectionMismatch
        (TypedImplDetail deferredCandidateSelectedImpl),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-deferred-candidate-selection") [1] [0, 0])
        TypedAmbiguousEvidence
        (TypedArityDetail 1 1)
    ]
    (validateTypedProgram deferredCandidateSelectionProgram)

testDuplicateDeferredEvidence :: IO ()
testDuplicateDeferredEvidence =
  assertEqual
    "deferred evidence obligations are unique across one node"
    [ expressionFailure
        "review-duplicate-deferred-evidence"
        TypedDuplicateEvidence
        (TypedTextDetail "Render.map")
    ]
    (validateTypedProgram duplicateDeferredEvidenceProgram)

testAmbiguousValueExport :: IO ()
testAmbiguousValueExport =
  assertEqual
    "one value export resolves to exactly one interface provider"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-ambiguous-value-export"))
        TypedModuleInterfaceMismatch
        (TypedNameDetail ambiguousValueExportName)
    ]
    (validateTypedProgram ambiguousValueExportProgram)

testPrimitiveInstantiationEntailment :: IO ()
testPrimitiveInstantiationEntailment =
  assertEqual
    "polymorphic instantiations require enclosing primitive entailment"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-unentailed-primitive-instantiation") [2] [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType (TypedTypeParameterType (TypedTypeParameterId 0))),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-unentailed-primitive-instantiation") [3] [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType (TypedTypeParameterType (TypedTypeParameterId 0))),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-unentailed-primitive-instantiation") [7] [0, 0])
        TypedBindingValueMismatch
        (TypedTypeDetail TypedIntType (TypedTypeParameterType (TypedTypeParameterId 0)))
    ]
    (validateTypedProgram unentailedPrimitiveInstantiationProgram)

testGeneratedClassMethodName :: IO ()
testGeneratedClassMethodName =
  assertEqual
    "class methods require identifier-bearing source names"
    [ statementFailure
        "review-generated-class-method-name"
        0
        TypedUnresolvedName
        (TypedNameDetail generatedClassMethodName)
    ]
    (validateTypedProgram generatedClassMethodNameProgram)

testSingletonOrPattern :: IO ()
testSingletonOrPattern =
  assertEqual
    "or-pattern nodes require at least two alternatives"
    [ patternFailure
        "review-singleton-or-pattern"
        TypedPatternShapeMismatch
        (TypedArityDetail 2 1)
    ]
    (validateTypedProgram singletonOrPatternProgram)

testFractionalLiteralPattern :: IO ()
testFractionalLiteralPattern =
  assertEqual
    "fractional literal patterns remain outside the canonical typed-core contract"
    [ patternFailure
        "review-fractional-pattern"
        TypedPatternShapeMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram fractionalPatternProgram)

testConstructorLikeIdentifierCasing :: IO ()
testConstructorLikeIdentifierCasing =
  assertEqual
    "constructor-like namespaces retain their uppercase source invariant"
    [ statementFailure
        "review-lowercase-constructor-like-names"
        0
        TypedUnresolvedName
        (TypedNameDetail lowercaseTypeName),
      statementFailure
        "review-lowercase-constructor-like-names"
        0
        TypedUnresolvedName
        (TypedNameDetail lowercaseConstructorName),
      statementFailure
        "review-lowercase-constructor-like-names"
        1
        TypedUnresolvedName
        (TypedNameDetail lowercaseCapabilityName)
    ]
    (validateTypedProgram lowercaseConstructorLikeNamesProgram)

testDuplicateDeclarationOrdering :: IO ()
testDuplicateDeclarationOrdering =
  assertEqual
    "duplicate declaration failures keep names before impl identities"
    [ statementFailure
        "review-duplicate-declaration-ordering"
        4
        TypedDuplicateDeclaration
        (TypedNameDetail duplicateOrderingDataName),
      statementFailure
        "review-duplicate-declaration-ordering"
        4
        TypedDuplicateDeclaration
        (TypedNameDetail duplicateOrderingConstructorName),
      statementFailure
        "review-duplicate-declaration-ordering"
        2
        TypedDuplicateDeclaration
        (TypedImplDetail duplicateOrderingImplId)
    ]
    (validateTypedProgram duplicateDeclarationOrderingProgram)

testBuiltinCatalogParity :: IO ()
testBuiltinCatalogParity =
  assertEqual
    "every catalog builtin name and value contract validates"
    []
    (validateTypedProgram builtinCatalogProgram)

testAppliedTargetCandidateDeferral :: IO ()
testAppliedTargetCandidateDeferral =
  assertEqual
    "candidate evidence cannot survive application of the class-target argument"
    [ expressionFailureAt
        "review-applied-target-candidate-deferral"
        2
        TypedAmbiguousEvidence
        (TypedArityDetail 1 1)
    ]
    (validateTypedProgram appliedTargetCandidateDeferralProgram)

testLocalCapabilityExportIdentity :: IO ()
testLocalCapabilityExportIdentity =
  assertEqual
    "a capability export requires the matching declared local class interface"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-local-capability-export-identity"))
        TypedModuleInterfaceMismatch
        ( TypedNameDetail
            (resolved TypedCurrentModule TypedCapabilityNamespace "Visible")
        )
    ]
    (validateTypedProgram localCapabilityExportIdentityProgram)

testStructuralIdentityRegressions :: IO ()
testStructuralIdentityRegressions =
  assertEqual
    "exact retained metadata, nonnegative binder paths, and namespace-preserving lookups"
    expected
    [(label, validateTypedProgram program) | (label, program) <- programs]
  where
    programs :: [(Text, TypedProgram)]
    programs =
      [ ("retained class spans", invalidRetainedClassSpanProgram),
        ("retained class method cardinality", duplicateRetainedClassMethodProgram),
        ("binder lexical paths", negativeBinderPathProgram),
        ("data namespaces", wrongDataNamespaceProgram),
        ("constructor namespaces", wrongConstructorNamespaceProgram)
      ]
    expected :: [(Text, [TypedCoreValidationFailure])]
    expected =
      [ ( "retained class spans",
          [retainedClassMetadataFailure "review-invalid-retained-class-span"]
        ),
        ( "retained class method cardinality",
          [retainedClassMetadataFailure "review-duplicate-retained-class-method"]
        ),
        ( "binder lexical paths",
          [ statementFailure
              "review-negative-binder-path"
              0
              TypedUnknownBinder
              (TypedBinderDetail negativeBinderPathOwner)
          ]
        ),
        ( "data namespaces",
          [ statementFailure
              "review-wrong-data-namespace"
              1
              TypedDataTypeMismatch
              (TypedNameDetail wrongDataNamespaceName)
          ]
        ),
        ( "constructor namespaces",
          [ TypedCoreValidationFailure
              (TypedPatternPath (fixtureModulePath "review-wrong-constructor-namespace") [1] [0, 0])
              TypedInvisibleName
              (TypedNameDetail wrongConstructorNamespaceName)
          ]
        )
      ]

testDroppedDeferredEvidence :: IO ()
testDroppedDeferredEvidence =
  assertEqual
    "application nodes cannot drop unresolved callee evidence"
    [ expressionFailureAt
        "review-dropped-deferred-evidence"
        2
        TypedMissingEvidence
        (TypedTextDetail "Build.build")
    ]
    (validateTypedProgram droppedDeferredEvidenceProgram)

testReorderedDeferredEvidence :: IO ()
testReorderedDeferredEvidence =
  assertEqual
    "application nodes preserve the solver's deferred candidate order"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-reordered-deferred-evidence") [1] [0, 0])
        TypedMissingEvidence
        ( TypedTextDetail
            ( Text.intercalate
                "::"
                (fixtureLibraryPath "ReorderedDeferredEvidence" <> ["Build", "build"])
            )
        )
    ]
    (validateTypedProgram reorderedDeferredEvidenceProgram)

testSelectedEvidenceProgression :: IO ()
testSelectedEvidenceProgression =
  assertEqual
    "application nodes preserve an already selected method body"
    [ expressionFailureAt
        "review-selected-evidence-progression"
        1
        TypedMethodSelectionMismatch
        (TypedImplDetail selectedEvidenceProgressionOriginalImpl)
    ]
    (validateTypedProgram selectedEvidenceProgressionProgram)

testExplicitInstantiationProgression :: IO ()
testExplicitInstantiationProgression =
  assertEqual
    "explicit type applications preserve the callee specialization"
    [ expressionFailureAt
        "review-mismatched-explicit-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail mismatchedExplicitInstantiationOwner)
    ]
    (validateTypedProgram mismatchedExplicitInstantiationProgram)

testForgedEvidenceCapability :: IO ()
testForgedEvidenceCapability =
  assertEqual
    "evidence constraints reject imported-self capability identities"
    [ expressionFailureAt
        "review-forged-evidence-capability"
        2
        TypedInvisibleName
        (TypedNameDetail forgedEvidenceCapabilityName)
    ]
    (validateTypedProgram forgedEvidenceCapabilityProgram)

testEmptyMonomorphicInstantiation :: IO ()
testEmptyMonomorphicInstantiation =
  assertEqual
    "empty instantiations require a generalized or evidence-constrained owner"
    [ expressionFailureAt
        "review-empty-monomorphic-instantiation"
        2
        TypedInstantiationMismatch
        (TypedBinderDetail emptyMonomorphicValueOwner),
      expressionFailureAt
        "review-empty-monomorphic-instantiation"
        3
        TypedInstantiationMismatch
        (TypedBinderDetail emptyMonomorphicConstructorOwner)
    ]
    (validateTypedProgram emptyMonomorphicInstantiationProgram)

droppedDeferredEvidenceProgram :: TypedProgram
droppedDeferredEvidenceProgram =
  targetCandidateApplicationProgram
    "review-dropped-deferred-evidence"
    False

reorderedDeferredEvidenceProgram :: TypedProgram
reorderedDeferredEvidenceProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    fixture = "review-reordered-deferred-evidence"
    providerPath = fixtureLibraryPath "ReorderedDeferredEvidence"
    entryPath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    providerCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Build"
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Build"
    providerMethodName =
      resolved TypedCurrentModule TypedValueNamespace "build"
    methodOwner = binder providerPath [0, 0] providerMethodName
    genericMethodType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType (TypedTypeParameterType parameter) TypedBoolType)
    genericMethodRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedRepresentationParameterRecipe parameter]
        TypedBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        providerCapabilityName
        [parameter]
        [ TypedMethodSignature
            providerMethodName
            span1
            ( fixtureScheme
                methodOwner
                []
                []
                []
                genericMethodType
                genericMethodRecipe
            )
        ]
    providerImpl =
      TypedImplId providerPath providerCapabilityName [TypedTextType]
    importedProviderImpl =
      TypedImplId providerPath importedCapabilityName [TypedTextType]
    localImpl =
      TypedImplId entryPath importedCapabilityName [TypedTextType]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ReorderedDeferredEvidence.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Build"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface providerImpl]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                providerImpl
                [methodDefinition providerPath [1, 0] providerImpl]
            )
        ]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing Nothing]
        []
        emptyInterface
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                localImpl
                [methodDefinition entryPath [0, 0] localImpl]
            ),
          expressionStatement 1 expression
        ]
        resultInfo
    specializedMethodType =
      TypedFunctionType TypedBoolType (TypedFunctionType TypedTextType TypedBoolType)
    specializedMethodRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedManagedTextRecipe]
        TypedBoolRecipe
    specializedMethodInfo =
      info specializedMethodType specializedMethodRecipe
    textToBoolType = TypedFunctionType TypedTextType TypedBoolType
    textToBoolRecipe =
      TypedClosureRecipe [TypedManagedTextRecipe] TypedBoolRecipe
    textToBoolInfo = info textToBoolType textToBoolRecipe
    flagName = resolved TypedCurrentModule TypedValueNamespace "flag"
    targetName = resolved TypedCurrentModule TypedValueNamespace "target"
    methodDefinition modulePath methodPath implId =
      TypedMethodDefinition
        (TypedMethodId implId "build")
        (binder modulePath methodPath providerMethodName)
        providerMethodName
        span1
        ( TypedLambdaExpr
            specializedMethodInfo
            (binder modulePath (methodPath <> [0]) flagName)
            flagName
            ( TypedLambdaExpr
                textToBoolInfo
                (binder modulePath (methodPath <> [0, 0]) targetName)
                targetName
                trueExpr
            )
        )
    constraint =
      TypedCapabilityConstraint
        importedCapabilityName
        (Just qualifiedMethodKey)
        TypedTextType
    qualifiedMethodKey =
      Text.intercalate "::" (providerPath <> ["Build", "build"])
    candidates =
      [ TypedEvidenceCandidate
          importedProviderImpl
          (Just (TypedMethodId importedProviderImpl "build")),
        TypedEvidenceCandidate
          localImpl
          (Just (TypedMethodId localImpl "build"))
      ]
    functionInfo =
      TypedNodeInfo
        specializedMethodType
        specializedMethodRecipe
        []
        [TypedEvidenceCandidates constraint candidates]
    intermediateInfo =
      TypedNodeInfo
        textToBoolType
        textToBoolRecipe
        []
        [TypedEvidenceCandidates constraint (reverse candidates)]
    resultInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        []
        [ TypedSelectedEvidence
            ( TypedEvidenceUse
                Nothing
                constraint
                importedProviderImpl
                (Just (TypedMethodId importedProviderImpl "build"))
            )
        ]
    intermediate =
      TypedApplyExpr
        intermediateInfo
        ( fixtureVariableExpr
            functionInfo
            (TypedBuiltinName qualifiedMethodKey)
        )
        trueExpr
    expression =
      TypedApplyExpr
        resultInfo
        intermediate
        (TypedLiteralExpr textInfo (TypedTextLiteral "target"))

forgedEvidenceCapabilityName :: TypedCoreName
forgedEvidenceCapabilityName =
  resolved
    (TypedImportedModule (fixtureModulePath "review-forged-evidence-capability"))
    TypedCapabilityNamespace
    "Check"

forgedEvidenceCapabilityProgram :: TypedProgram
forgedEvidenceCapabilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-forged-evidence-capability"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Check"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "check"
    methodOwner = binder modulePath [0, 0] methodName
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 (monoScheme methodOwner)]
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "check")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        trueExpr
    constraint =
      TypedCapabilityConstraint
        forgedEvidenceCapabilityName
        (Just qualifiedMethodKey)
        TypedBoolType
    qualifiedMethodKey =
      Text.intercalate "::" (modulePath <> ["Check", "check"])
    evidenceUse =
      TypedEvidenceUse
        Nothing
        constraint
        implId
        (Just (TypedMethodId implId "check"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName qualifiedMethodKey)
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 implId [methodDefinition]),
        expressionStatement 2 expression,
        expressionStatement 3 trueExpr
      ]

emptyMonomorphicValueOwner :: TypedBinderId
emptyMonomorphicValueOwner =
  binder
    (fixtureModulePath "review-empty-monomorphic-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

emptyMonomorphicConstructorOwner :: TypedBinderId
emptyMonomorphicConstructorOwner =
  binder
    (fixtureModulePath "review-empty-monomorphic-instantiation")
    [1, 0]
    (resolved TypedCurrentModule TypedConstructorNamespace "Flag")

emptyMonomorphicInstantiationProgram :: TypedProgram
emptyMonomorphicInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-monomorphic-instantiation"
    modulePath = fixtureModulePath fixture
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation emptyMonomorphicValueOwner [] Nothing]
        []
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorType = TypedDataType dataName []
    constructorInfo =
      TypedNodeInfo
        constructorType
        (TypedManagedVariantRecipe dataName [])
        [TypedInstantiation emptyMonomorphicConstructorOwner [] Nothing]
        []
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            emptyMonomorphicConstructorOwner
            constructorName
            []
            []
        ]
    statements =
      [ TypedLetStatement
          emptyMonomorphicValueOwner
          valueName
          span1
          (monoScheme emptyMonomorphicValueOwner)
          trueExpr,
        TypedDataStatement declaration,
        expressionStatement 2 (fixtureVariableExpr valueInfo valueName),
        expressionStatement
          3
          (fixtureVariableExpr constructorInfo constructorName),
        expressionStatement 4 trueExpr
      ]

retainedClassMetadataFailure :: Text -> TypedCoreValidationFailure
retainedClassMetadataFailure fixture =
  TypedCoreValidationFailure
    (TypedInterfacePath (fixtureModulePath fixture))
    TypedModuleInterfaceMismatch
    (TypedNameDetail retainedClassMetadataName)

retainedClassMetadataName :: TypedCoreName
retainedClassMetadataName =
  resolved
    (TypedImportedModule retainedClassMetadataProviderPath)
    TypedCapabilityNamespace
    "Display"

retainedClassMetadataProviderPath :: [Text]
retainedClassMetadataProviderPath =
  fixtureLibraryPath "RetainedClassMetadataProvider"

invalidRetainedClassSpanProgram :: TypedProgram
invalidRetainedClassSpanProgram =
  retainedClassMetadataProgram
    "review-invalid-retained-class-span"
    invalidSpan
    False

duplicateRetainedClassMethodProgram :: TypedProgram
duplicateRetainedClassMethodProgram =
  retainedClassMetadataProgram
    "review-duplicate-retained-class-method"
    span1
    True

retainedClassMetadataProgram :: Text -> TypedSpan -> Bool -> TypedProgram
retainedClassMetadataProgram fixture retainedSpan duplicateMethod =
  TypedProgram Nothing [providerModule, facadeModule] facadePath
  where
    providerPath = retainedClassMetadataProviderPath
    facadePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder providerPath [0, 0] localMethodName
    methodScheme = fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    localMethod = TypedMethodSignature localMethodName span1 methodScheme
    localClass =
      TypedClassDeclaration span1 localClassName [parameter] [localMethod]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedClassMetadataProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Display"]
        (TypedModuleInterface [] [] [TypedClassInterface localClass] [])
        [TypedClassStatement localClass]
        unitInfo
    retainedMethodName =
      resolved
        (TypedImportedModule providerPath)
        TypedValueNamespace
        "display"
    retainedMethod =
      TypedMethodSignature retainedMethodName span1 methodScheme
    retainedMethods
      | duplicateMethod = [retainedMethod, retainedMethod]
      | otherwise = [retainedMethod]
    retainedClass =
      TypedClassDeclaration
        retainedSpan
        retainedClassMetadataName
        [parameter]
        retainedMethods
    facadeModule =
      typedModule
        facadePath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["Display"])]
        []
        (TypedModuleInterface [] [] [TypedClassInterface retainedClass] [])
        []
        unitInfo

negativeBinderPathOwner :: TypedBinderId
negativeBinderPathOwner =
  binder
    (fixtureModulePath "review-negative-binder-path")
    [-1]
    (fixtureValueName "answer")

negativeBinderPathProgram :: TypedProgram
negativeBinderPathProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface unitInfo modulePath
  where
    fixture = "review-negative-binder-path"
    modulePath = fixtureModulePath fixture
    valueName = fixtureValueName "answer"
    statement =
      TypedLetStatement
        negativeBinderPathOwner
        valueName
        span1
        (monoScheme negativeBinderPathOwner)
        trueExpr

wrongDataNamespaceName :: TypedCoreName
wrongDataNamespaceName =
  resolved TypedCurrentModule TypedValueNamespace "Box"

wrongDataNamespaceProgram :: TypedProgram
wrongDataNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-wrong-data-namespace"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration =
      dataDeclarationWithNullaryConstructor modulePath [0, 0] dataName []
    valueName = fixtureValueName "item"
    owner = binder modulePath [1] valueName
    invalidType = TypedDataType wrongDataNamespaceName []
    invalidRecipe = TypedManagedVariantRecipe wrongDataNamespaceName []
    scheme = fixtureScheme owner [] [] [] invalidType invalidRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedSignatureStatement owner valueName span1 scheme
      ]

wrongConstructorNamespaceName :: TypedCoreName
wrongConstructorNamespaceName =
  resolved TypedCurrentModule TypedValueNamespace "Box"

wrongConstructorNamespaceProgram :: TypedProgram
wrongConstructorNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-wrong-constructor-namespace"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration =
      dataDeclarationWithNullaryConstructor modulePath [0, 0] dataName []
    patternValue =
      TypedConstructorPattern boolInfo wrongConstructorNamespaceName []
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm patternValue Nothing trueExpr]
    statements =
      [TypedDataStatement declaration, expressionStatement 1 expression]

appliedTargetCandidateDeferralProgram :: TypedProgram
appliedTargetCandidateDeferralProgram =
  targetCandidateApplicationProgram
    "review-applied-target-candidate-deferral"
    True

targetCandidateApplicationProgram :: Text -> Bool -> TypedProgram
targetCandidateApplicationProgram fixture retainCandidate =
  singleModuleProgram fixture relativeSource [] statements emptyInterface resultInfo modulePath
  where
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Build"
    methodName = resolved TypedCurrentModule TypedValueNamespace "build"
    methodOwner = binder modulePath [0, 0] methodName
    genericMethodType = TypedFunctionType parameterType boolToBoolType
    genericMethodRecipe =
      TypedClosureRecipe [parameterRecipe, TypedBoolRecipe] TypedBoolRecipe
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        genericMethodType
        genericMethodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId modulePath capabilityName [TypedTextType]
    targetName = resolved TypedCurrentModule TypedValueNamespace "target"
    resultName = resolved TypedCurrentModule TypedValueNamespace "result"
    specializedMethodType = TypedFunctionType TypedTextType boolToBoolType
    specializedMethodRecipe =
      TypedClosureRecipe
        [TypedManagedTextRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    methodExpression =
      TypedLambdaExpr
        (info specializedMethodType specializedMethodRecipe)
        (binder modulePath [1, 0, 0] targetName)
        targetName
        ( TypedLambdaExpr
            boolToBoolInfo
            (binder modulePath [1, 0, 0, 0] resultName)
            resultName
            trueExpr
        )
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "build")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        methodExpression
    constraint =
      TypedCapabilityConstraint capabilityName (Just "Build.build") TypedTextType
    candidate =
      TypedEvidenceCandidate implId (Just (TypedMethodId implId "build"))
    selection = TypedEvidenceCandidates constraint [candidate]
    functionInfo =
      TypedNodeInfo specializedMethodType specializedMethodRecipe [] [selection]
    resultInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        []
        (if retainCandidate then [selection] else [])
    expression =
      TypedApplyExpr
        resultInfo
        ( fixtureVariableExpr
            functionInfo
            (TypedBuiltinName "Build::build")
        )
        (TypedLiteralExpr textInfo (TypedTextLiteral "target"))
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 implId [methodDefinition]),
        expressionStatement 2 expression
      ]

localCapabilityExportIdentityProgram :: TypedProgram
localCapabilityExportIdentityProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = fixtureLibraryPath "CapabilityExportIdentity"
    entryPath = fixtureModulePath "review-local-capability-export-identity"
    parameter = TypedTypeParameterId 0
    providerClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    providerDeclaration =
      TypedClassDeclaration span1 providerClassName [parameter] []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/CapabilityExportIdentity.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface providerDeclaration]
            []
        )
        [TypedClassStatement providerDeclaration]
        unitInfo
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    localDeclaration =
      TypedClassDeclaration span1 localClassName [parameter] []
    retainedClassName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Visible"
    retainedDeclaration =
      TypedClassDeclaration span1 retainedClassName [parameter] []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport
            span1
            providerPath
            (Just "Library")
            Nothing
        ]
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface retainedDeclaration]
            []
        )
        [TypedClassStatement localDeclaration]
        unitInfo

duplicateDeferredEvidenceProgram :: TypedProgram
duplicateDeferredEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-deferred-evidence"
    candidate =
      fixtureRenderCandidate (fixtureRenderImpl ["Prelude"])
    selection =
      TypedEvidenceCandidates fixtureRenderConstraint [candidate]
    expression =
      fixtureVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [selection, selection])
        (TypedBuiltinName "map")

ambiguousValueExportName :: TypedCoreName
ambiguousValueExportName =
  resolved TypedCurrentModule TypedValueNamespace "render"

ambiguousValueExportProgram :: TypedProgram
ambiguousValueExportProgram =
  singleModuleProgram fixture relativeSource exports statements interface boolInfo modulePath
  where
    fixture = "review-ambiguous-value-export"
    modulePath = fixtureModulePath fixture
    valueOwner = binder modulePath [0] ambiguousValueExportName
    valueScheme = monoScheme valueOwner
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    methodOwner = binder modulePath [1, 0] ambiguousValueExportName
    methodScheme = monoScheme methodOwner
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature ambiguousValueExportName span1 methodScheme]
    exports = [TypedModuleExport TypedValueNamespace "render"]
    statements =
      [ TypedLetStatement valueOwner ambiguousValueExportName span1 valueScheme trueExpr,
        TypedClassStatement classDeclaration,
        expressionStatement 2 trueExpr
      ]
    interface =
      TypedModuleInterface
        [TypedValueInterface ambiguousValueExportName valueScheme]
        []
        [TypedClassInterface classDeclaration]
        []

unentailedPrimitiveInstantiationProgram :: TypedProgram
unentailedPrimitiveInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unentailed-primitive-instantiation"
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    numericName = fixtureValueName "numeric"
    numericOwner = binder modulePath [0] numericName
    equalityName = fixtureValueName "equality"
    equalityOwner = binder modulePath [1] equalityName
    outerScheme owner =
      fixtureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType TypedBoolType)
        (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
    constrainedOuterScheme owner constraint =
      fixtureScheme
        owner
        [parameter]
        []
        [constraint]
        (TypedFunctionType parameterType TypedBoolType)
        (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
    constrainedScheme owner constraint =
      fixtureScheme owner [parameter] [] [constraint] TypedBoolType TypedBoolRecipe
    instantiate owner =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter parameterType]
        Nothing
    outerExpression statementIndex owner name =
      TypedLambdaExpr
        ( info
            (TypedFunctionType parameterType TypedBoolType)
            (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
        )
        (binder modulePath [statementIndex, 0] (fixtureValueName "argument"))
        (fixtureValueName "argument")
        ( fixtureVariableExpr
            (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiate owner] [])
            name
        )
    numericOuterName = fixtureValueName "numericOuter"
    numericOuterOwner = binder modulePath [2] numericOuterName
    equalityOuterName = fixtureValueName "equalityOuter"
    equalityOuterOwner = binder modulePath [3] equalityOuterName
    entailedNumericOuterName = fixtureValueName "entailedNumericOuter"
    entailedNumericOuterOwner = binder modulePath [4] entailedNumericOuterName
    entailedEqualityOuterName = fixtureValueName "entailedEqualityOuter"
    entailedEqualityOuterOwner = binder modulePath [5] entailedEqualityOuterName
    integralName = fixtureValueName "integral"
    integralOwner = binder modulePath [6] integralName
    arithmeticOuterName = fixtureValueName "arithmeticOuter"
    arithmeticOuterOwner = binder modulePath [7] arithmeticOuterName
    statements =
      [ TypedLetStatement
          numericOwner
          numericName
          span1
          ( constrainedScheme
              numericOwner
              (TypedNumericPrimitiveConstraint TypedRuntimeArithmeticNumericConstraint parameterType)
          )
          trueExpr,
        TypedLetStatement
          equalityOwner
          equalityName
          span1
          (constrainedScheme equalityOwner (TypedStrictEqualityPrimitiveConstraint parameterType))
          trueExpr,
        TypedLetStatement
          numericOuterOwner
          numericOuterName
          span1
          (outerScheme numericOuterOwner)
          (outerExpression 2 numericOwner numericName),
        TypedLetStatement
          equalityOuterOwner
          equalityOuterName
          span1
          (outerScheme equalityOuterOwner)
          (outerExpression 3 equalityOwner equalityName),
        TypedLetStatement
          entailedNumericOuterOwner
          entailedNumericOuterName
          span1
          ( constrainedOuterScheme
              entailedNumericOuterOwner
              (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType)
          )
          (outerExpression 4 numericOwner numericName),
        TypedLetStatement
          entailedEqualityOuterOwner
          entailedEqualityOuterName
          span1
          ( constrainedOuterScheme
              entailedEqualityOuterOwner
              (TypedStrictEqualityPrimitiveConstraint parameterType)
          )
          (outerExpression 5 equalityOwner equalityName),
        TypedLetStatement
          integralOwner
          integralName
          span1
          ( constrainedScheme
              integralOwner
              (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType)
          )
          trueExpr,
        TypedLetStatement
          arithmeticOuterOwner
          arithmeticOuterName
          span1
          ( constrainedOuterScheme
              arithmeticOuterOwner
              (TypedNumericPrimitiveConstraint TypedRuntimeArithmeticNumericConstraint parameterType)
          )
          (outerExpression 7 integralOwner integralName),
        expressionStatement 8 trueExpr
      ]

generatedClassMethodName :: TypedCoreName
generatedClassMethodName =
  TypedGeneratedName TypedOperatorSectionFunction

generatedClassMethodNameProgram :: TypedProgram
generatedClassMethodNameProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-generated-class-method-name"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "GeneratedMethod"
    methodOwner = binder modulePath [0, 0] generatedClassMethodName
    methodScheme = monoScheme methodOwner
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature generatedClassMethodName span1 methodScheme]
    statements =
      [ TypedClassStatement declaration,
        expressionStatement 1 trueExpr
      ]

singletonOrPatternProgram :: TypedProgram
singletonOrPatternProgram =
  expressionFixtureProgram
    "review-singleton-or-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm
            (TypedOrPattern boolInfo [TypedWildcardPattern boolInfo])
            Nothing
            trueExpr
        ]
    )

fractionalPatternProgram :: TypedProgram
fractionalPatternProgram =
  expressionFixtureProgram
    "review-fractional-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        fractionalExpression
        [ TypedCaseArm
            (TypedLiteralPattern fractionalInfo fractionalLiteral)
            Nothing
            trueExpr
        ]
    )
  where
    fractionalInfo =
      info
        (TypedNumericType TypedFloat64Type)
        (TypedFloatRecipe 64)
    fractionalLiteral =
      TypedFractionalLiteral "1" "5" (Just TypedFloat64Type)
    fractionalExpression =
      TypedLiteralExpr fractionalInfo fractionalLiteral

lowercaseTypeName :: TypedCoreName
lowercaseTypeName =
  resolved TypedCurrentModule TypedTypeNamespace "lower"

lowercaseConstructorName :: TypedCoreName
lowercaseConstructorName =
  resolved TypedCurrentModule TypedConstructorNamespace "lowerConstructor"

lowercaseCapabilityName :: TypedCoreName
lowercaseCapabilityName =
  resolved TypedCurrentModule TypedCapabilityNamespace "lowerCapability"

lowercaseConstructorLikeNamesProgram :: TypedProgram
lowercaseConstructorLikeNamesProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-lowercase-constructor-like-names"
    modulePath = fixtureModulePath fixture
    dataDeclaration =
      TypedDataDeclaration
        span1
        lowercaseTypeName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] lowercaseConstructorName)
            lowercaseConstructorName
            []
            []
        ]
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        lowercaseCapabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedDataStatement dataDeclaration,
        TypedClassStatement capabilityDeclaration,
        expressionStatement 2 trueExpr
      ]

duplicateOrderingDataName :: TypedCoreName
duplicateOrderingDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Duplicate"

duplicateOrderingConstructorName :: TypedCoreName
duplicateOrderingConstructorName =
  resolved TypedCurrentModule TypedConstructorNamespace "Duplicate"

duplicateOrderingImplId :: TypedImplId
duplicateOrderingImplId =
  TypedImplId
    (fixtureModulePath "review-duplicate-declaration-ordering")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

duplicateDeclarationOrderingProgram :: TypedProgram
duplicateDeclarationOrderingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-declaration-ordering"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    dataDeclaration statementIndex =
      TypedDataDeclaration
        span1
        duplicateOrderingDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [statementIndex, 0] duplicateOrderingConstructorName)
            duplicateOrderingConstructorName
            []
            []
        ]
    statements =
      [ TypedClassStatement capabilityDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 duplicateOrderingImplId []),
        TypedImplStatement (TypedImplDeclaration span1 duplicateOrderingImplId []),
        TypedDataStatement (dataDeclaration 3),
        TypedDataStatement (dataDeclaration 4),
        expressionStatement 5 trueExpr
      ]

builtinCatalogProgram :: TypedProgram
builtinCatalogProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface terminalInfo modulePath
  where
    fixture = "review-builtin-catalog-parity"
    modulePath = fixtureModulePath fixture
    builtinExpressions =
      [ fixtureVariableExpr (builtinCatalogInfo symbol) (TypedBuiltinName name)
      | symbol <- allBuiltinSymbols,
        name <- builtinAcceptedNames symbol
      ]
    statements =
      zipWith expressionStatement [1 ..] builtinExpressions
    terminalInfo = builtinCatalogInfo BuiltinExit

builtinAcceptedNames :: BuiltinSymbol -> [Text]
builtinAcceptedNames symbol =
  case builtinSymbolOwnership symbol of
    PreludeTarget ->
      [builtinSymbolName symbol, builtinSymbolKernelName symbol]
    KernelIntrinsic ->
      [builtinSymbolKernelName symbol]

builtinCatalogInfo :: BuiltinSymbol -> TypedNodeInfo
builtinCatalogInfo symbol =
  case symbol of
    BuiltinMap -> builtinMapInfo
    BuiltinFilter ->
      info
        ( TypedFunctionType
            (TypedFunctionType TypedBoolType TypedBoolType)
            (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedBoolType))
        )
        ( TypedClosureRecipe
            [boolToBoolRecipe, TypedManagedListRecipe TypedBoolRecipe]
            (TypedManagedListRecipe TypedBoolRecipe)
        )
    BuiltinHd ->
      functionInfo
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
        TypedBoolType
        TypedBoolRecipe
    BuiltinTl -> boolListTransformInfo
    BuiltinPrint -> boolToBoolInfo
    BuiltinToInt8 -> numericConversionInfo TypedInt8Type (TypedSignedIntegerRecipe 8)
    BuiltinToInt16 -> numericConversionInfo TypedInt16Type (TypedSignedIntegerRecipe 16)
    BuiltinToInt32 -> numericConversionInfo TypedInt32Type (TypedSignedIntegerRecipe 32)
    BuiltinToInt64 -> numericConversionInfo TypedInt64Type (TypedSignedIntegerRecipe 64)
    BuiltinToUInt8 -> numericConversionInfo TypedUInt8Type (TypedUnsignedIntegerRecipe 8)
    BuiltinToUInt16 -> numericConversionInfo TypedUInt16Type (TypedUnsignedIntegerRecipe 16)
    BuiltinToUInt32 -> numericConversionInfo TypedUInt32Type (TypedUnsignedIntegerRecipe 32)
    BuiltinToUInt64 -> numericConversionInfo TypedUInt64Type (TypedUnsignedIntegerRecipe 64)
    BuiltinToFloat16 -> numericConversionInfo TypedFloat16Type (TypedFloatRecipe 16)
    BuiltinToFloat32 -> numericConversionInfo TypedFloat32Type (TypedFloatRecipe 32)
    BuiltinToFloat64 -> numericConversionInfo TypedFloat64Type (TypedFloatRecipe 64)
    BuiltinListPrependRaw ->
      info
        ( TypedFunctionType
            TypedBoolType
            (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedBoolType))
        )
        ( TypedClosureRecipe
            [TypedBoolRecipe, TypedManagedListRecipe TypedBoolRecipe]
            (TypedManagedListRecipe TypedBoolRecipe)
        )
    BuiltinListReverseRaw -> boolListTransformInfo
    BuiltinCharToUInt32 ->
      functionInfo
        TypedCharType
        TypedCharRecipe
        (TypedNumericType TypedUInt32Type)
        (TypedUnsignedIntegerRecipe 32)
    BuiltinCharFromUInt32Raw ->
      functionInfo
        (TypedNumericType TypedUInt32Type)
        (TypedUnsignedIntegerRecipe 32)
        (TypedListType TypedCharType)
        (TypedManagedListRecipe TypedCharRecipe)
    BuiltinCharIsAlpha -> charPredicateInfo
    BuiltinCharIsAlphaNum -> charPredicateInfo
    BuiltinCharIsDigit -> charPredicateInfo
    BuiltinCharIsSpace -> charPredicateInfo
    BuiltinCharIsHexDigit -> charPredicateInfo
    BuiltinCharIsLower -> charPredicateInfo
    BuiltinCharIsUpper -> charPredicateInfo
    BuiltinCharToLower -> charTransformInfo
    BuiltinCharToUpper -> charTransformInfo
    BuiltinTextLength ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        TypedIntType
        (TypedSignedIntegerRecipe 64)
    BuiltinTextUnconsRaw ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        (TypedListType (TypedTupleType [TypedCharType, TypedTextType]))
        ( TypedManagedListRecipe
            (TypedManagedProductRecipe [TypedCharRecipe, TypedManagedTextRecipe])
        )
    BuiltinTextAppend -> textBinaryInfo TypedTextType TypedManagedTextRecipe
    BuiltinTextAppendChar -> textBinaryInfo TypedCharType TypedCharRecipe
    BuiltinTextFromChars ->
      functionInfo
        (TypedListType TypedCharType)
        (TypedManagedListRecipe TypedCharRecipe)
        TypedTextType
        TypedManagedTextRecipe
    BuiltinTextConcat ->
      functionInfo
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
        TypedTextType
        TypedManagedTextRecipe
    BuiltinRenderValue ->
      functionInfo
        TypedBoolType
        TypedBoolRecipe
        TypedTextType
        TypedManagedTextRecipe
    BuiltinReadTextRaw ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe
    BuiltinWriteTextRaw ->
      info
        (TypedFunctionType TypedTextType (TypedFunctionType TypedTextType hostIOOutcomeType))
        (TypedClosureRecipe [TypedManagedTextRecipe, TypedManagedTextRecipe] hostIOOutcomeRecipe)
    BuiltinReadStdinRaw ->
      functionInfo
        (TypedTupleType [])
        TypedUnitRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe
    BuiltinWriteStdoutRaw -> textToHostIOInfo
    BuiltinWriteStderrRaw -> textToHostIOInfo
    BuiltinArguments ->
      functionInfo
        (TypedTupleType [])
        TypedUnitRecipe
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
    BuiltinExit ->
      functionInfo
        TypedIntType
        (TypedSignedIntegerRecipe 64)
        (TypedTupleType [])
        TypedUnitRecipe
  where
    functionInfo argumentType argumentRecipe resultType resultRecipe =
      info
        (TypedFunctionType argumentType resultType)
        (TypedClosureRecipe [argumentRecipe] resultRecipe)
    numericConversionInfo targetType targetRecipe =
      functionInfo
        TypedIntType
        (TypedSignedIntegerRecipe 64)
        (TypedNumericType targetType)
        targetRecipe
    boolListTransformInfo =
      functionInfo
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
    charPredicateInfo =
      functionInfo TypedCharType TypedCharRecipe TypedBoolType TypedBoolRecipe
    charTransformInfo =
      functionInfo TypedCharType TypedCharRecipe TypedCharType TypedCharRecipe
    textBinaryInfo secondType secondRecipe =
      info
        (TypedFunctionType TypedTextType (TypedFunctionType secondType TypedTextType))
        (TypedClosureRecipe [TypedManagedTextRecipe, secondRecipe] TypedManagedTextRecipe)
    hostIOOutcomeType =
      TypedTupleType
        [TypedBoolType, TypedTextType, TypedTextType, TypedTextType]
    hostIOOutcomeRecipe =
      TypedManagedProductRecipe
        [TypedBoolRecipe, TypedManagedTextRecipe, TypedManagedTextRecipe, TypedManagedTextRecipe]
    textToHostIOInfo =
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe

publishedImplWithoutCapabilityMetadataId :: TypedImplId
publishedImplWithoutCapabilityMetadataId =
  TypedImplId
    (fixtureModulePath "review-published-impl-capability-metadata")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Published")
    [TypedBoolType]

publishedImplWithoutCapabilityMetadataProgram :: TypedProgram
publishedImplWithoutCapabilityMetadataProgram =
  singleModuleProgram fixture relativeSource [] statements interface unitInfo modulePath
  where
    fixture = "review-published-impl-capability-metadata"
    modulePath = fixtureModulePath fixture
    capability =
      TypedClassDeclaration
        span1
        (resolved TypedCurrentModule TypedCapabilityNamespace "Published")
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 publishedImplWithoutCapabilityMetadataId [])
      ]
    interface =
      TypedModuleInterface
        []
        []
        []
        [TypedImplInterface publishedImplWithoutCapabilityMetadataId]

deferredCandidateSelectedImpl :: TypedImplId
deferredCandidateSelectedImpl =
  fixtureRenderImpl (fixtureModulePath "review-deferred-candidate-selection")

deferredCandidateSelectionProgram :: TypedProgram
deferredCandidateSelectionProgram =
  qualifiedMapDispatchProgram
    fixture
    [ TypedEvidenceCandidates
        fixtureRenderConstraint
        [fixtureRenderCandidate (fixtureRenderImpl ["Prelude"])]
    ]
    [ TypedSelectedEvidence
        ( TypedEvidenceUse
            Nothing
            fixtureRenderConstraint
            deferredCandidateSelectedImpl
            (Just (TypedMethodId deferredCandidateSelectedImpl "map"))
        )
    ]
  where
    fixture = "review-deferred-candidate-selection"

selectedEvidenceProgressionOriginalImpl :: TypedImplId
selectedEvidenceProgressionOriginalImpl =
  fixtureRenderImpl ["Prelude"]

selectedEvidenceProgressionProgram :: TypedProgram
selectedEvidenceProgressionProgram =
  qualifiedMapDispatchProgram
    fixture
    [selected selectedEvidenceProgressionOriginalImpl]
    [selected (fixtureRenderImpl (fixtureModulePath fixture))]
  where
    fixture = "review-selected-evidence-progression"
    selected implId =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            Nothing
            fixtureRenderConstraint
            implId
            (Just (TypedMethodId implId "map"))
        )

mismatchedExplicitInstantiationOwner :: TypedBinderId
mismatchedExplicitInstantiationOwner =
  binder
    (fixtureModulePath "review-mismatched-explicit-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "identity")

mismatchedExplicitInstantiationProgram :: TypedProgram
mismatchedExplicitInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface outerInfo modulePath
  where
    fixture = "review-mismatched-explicit-instantiation"
    modulePath = fixtureModulePath fixture
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = mismatchedExplicitInstantiationOwner
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiate typeArgument =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter typeArgument]
        (Just span1)
    outerInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiate TypedBoolType]
        []
    calleeInfo =
      TypedNodeInfo
        (TypedFunctionType TypedCharType TypedCharType)
        (TypedClosureRecipe [TypedCharRecipe] TypedCharRecipe)
        [instantiate TypedCharType]
        []
    expression =
      TypedTypeApplicationExpr
        outerInfo
        (fixtureVariableExpr calleeInfo name)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement
          owner
          name
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

nestedPathProgram :: TypedProgram
nestedPathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-path"
    modulePath = (fixtureModulePath fixture)
    nestedName = TypedUnresolvedSourceName "nested"
    block =
      TypedBlockExpr
        (info TypedBoolType (TypedSignedIntegerRecipe 64))
        [expressionStatement 2 (fixtureVariableExpr boolInfo nestedName)]

nestedPathFailures :: [TypedCoreValidationFailure]
nestedPathFailures =
  [ TypedCoreValidationFailure
      (TypedExpressionPath (fixtureModulePath "review-nested-path") [0] [0])
      TypedTypeRepresentationMismatch
      (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64)),
    TypedCoreValidationFailure
      (TypedExpressionPath (fixtureModulePath "review-nested-path") [0, 0, 0] [0])
      TypedUnresolvedName
      (TypedNameDetail (TypedUnresolvedSourceName "nested"))
  ]

nestedDeclarationProgram :: TypedProgram
nestedDeclarationProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-declaration"
    modulePath = (fixtureModulePath fixture)
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
      (TypedExpressionPath (fixtureModulePath "review-nested-declaration") [0] [0])
      TypedBlockResultMismatch
      TypedNoValidationDetail,
    TypedCoreValidationFailure
      (TypedStatementPath (fixtureModulePath "review-nested-declaration") [0, 0, 0])
      TypedBlockResultMismatch
      (TypedTextDetail "data declaration")
  ]

nestedDuplicateBinderProgram :: TypedProgram
nestedDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-duplicate-binder"
    modulePath = (fixtureModulePath fixture)
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
      (TypedStatementPath (fixtureModulePath "review-nested-duplicate-binder") [0, 0, 1])
      TypedDuplicateBinder
      (TypedBinderDetail (binder (fixtureModulePath "review-nested-duplicate-binder") [0, 0] (resolved TypedCurrentModule TypedValueNamespace "duplicate")))
  ]

guardedCasePathProgram :: TypedProgram
guardedCasePathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo modulePath
  where
    fixture = "review-guarded-case-path"
    modulePath = (fixtureModulePath fixture)
    unresolved name = fixtureVariableExpr boolInfo (TypedUnresolvedSourceName name)
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
    modulePath = (fixtureModulePath fixture)
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
    scheme = fixtureScheme valueBinder [parameterId] [] [] functionType functionRecipe
    expression = TypedLambdaExpr functionInfo argumentBinder argumentName (fixtureBoundVariableExpr argumentBinder (info parameterType parameterRecipe) argumentName)
    statement = TypedLetStatement valueBinder valueName span1 scheme expression
    interface = TypedModuleInterface [TypedValueInterface valueName scheme] [] [] []

importedInstantiationProgram :: TypedProgram
importedInstantiationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "Identity")
    entryPath = (fixtureModulePath "review-imported-instantiation")
    localName = resolved TypedCurrentModule TypedValueNamespace "identity"
    importedName = resolved (TypedImportedModule libraryPath) TypedValueNamespace "identity"
    owner = binder libraryPath [0] localName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    scheme =
      fixtureScheme
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
    expression = fixtureBoundVariableExpr owner instantiatedInfo importedName
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

retainedPreludeEqualClass :: TypedClassDeclaration
retainedPreludeEqualClass = fixtureEqualClass TypedAmbientPrelude

fixtureEqualClass :: TypedNameOrigin -> TypedClassDeclaration
fixtureEqualClass origin =
  TypedClassDeclaration
    span1
    (resolved origin TypedCapabilityNamespace "Equal")
    [TypedTypeParameterId 0]
    [ TypedMethodSignature
        (resolved origin TypedValueNamespace "equal")
        span1
        (monoScheme equalOwner),
      TypedMethodSignature
        (resolved origin TypedValueNamespace "other")
        span1
        (fixtureScheme otherOwner [] [] [] boolToBoolType boolToBoolRecipe)
    ]
  where
    equalOwner =
      binder
        ["Prelude"]
        [0, 0]
        (resolved TypedCurrentModule TypedValueNamespace "equal")
    otherOwner =
      binder
        ["Prelude"]
        [0, 1]
        (resolved TypedCurrentModule TypedValueNamespace "other")

invisibleSiblingImplProgram :: TypedProgram
invisibleSiblingImplProgram = TypedProgram (Just fixturePrelude) [hiddenModule, entryModule] entryPath
  where
    fixture = "review-invisible-sibling-impl"
    hiddenPath = ["Hidden", "Evidence"]
    entryPath = (fixtureModulePath fixture)
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
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface retainedPreludeEqualClass]
            [TypedImplInterface invisibleSiblingImplId]
        )
        [TypedImplStatement hiddenDeclaration]
        boolInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder entryPath [0] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint invisibleSiblingImplId Nothing
    expression = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
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
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedCharType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint implId Nothing
    expression = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
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
  expressionFixtureProgram "review-invisible-variable" (fixtureVariableExpr boolInfo invisibleVariableName)

selectedMethodContractProgram :: TypedProgram
selectedMethodContractProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-selected-method-contract"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "equal"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint capabilityName (Just "Equal.equal") TypedBoolType
    withoutMethod = TypedEvidenceUse Nothing constraint implId Nothing
    wrongMethod = TypedEvidenceUse Nothing constraint implId (Just (TypedMethodId implId "other"))
    selected evidence = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
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
  TypedImplId (fixtureModulePath "review-enclosing-impl-method") (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render") [TypedCharType]

enclosingImplMethodProgram :: TypedProgram
enclosingImplMethodProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-enclosing-impl-method"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId modulePath capabilityName [TypedTextType]
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
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    value = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "wrong")

lambdaResultProgram :: TypedProgram
lambdaResultProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-lambda-result"
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
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
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 terminal] emptyInterface boolInfo (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    resultType = TypedDataType missingSchemeDataName []
    scheme = fixtureScheme valueBinder [] [] [] resultType (TypedManagedVariantRecipe missingSchemeDataName [])

driveAbsoluteProgram :: TypedProgram
driveAbsoluteProgram =
  singleModuleProgram fixture (TypedSourcePath "C:/Fixture/Main.jz") [] [] emptyInterface boolInfo (fixtureModulePath fixture)
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
        (TypedExpressionPath privateInterfaceEntryPath [0] [0])
        TypedInvisibleName
        (TypedNameDetail privateInterfaceImportedName)
    ]
    (validateTypedProgram privateInterfaceLeakProgram)
  assertEqual
    "constructor patterns match declared field arity and types"
    [ TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-constructor-pattern-contract") [1] [0, 0])
        TypedPatternShapeMismatch
        (TypedArityDetail 1 0),
      TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-constructor-pattern-contract") [1] [0, 1, 0])
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
        TypedBinderReferenceMismatch
        (TypedBinderDetail (binder (fixtureModulePath "review-variable-scheme-contract") [0] (fixtureValueName "item")))
    ]
    (validateTypedProgram variableSchemeContractProgram)
  assertEqual
    "resolved imports require modules present in the program"
    [moduleFailure "review-missing-import" TypedModuleInterfaceMismatch (TypedTextDetail "Missing::Library")]
    (validateTypedProgram missingImportProgram)
  assertEqual
    "candidate evidence matches capability and method constraints"
    [ expressionFailureAt "review-candidate-constraint" 0 TypedMethodSelectionMismatch (TypedNameDetail (preludeCapability "Equal")),
      expressionFailureAt "review-candidate-constraint" 0 TypedMethodSelectionMismatch (TypedTypeDetail TypedTextType TypedBoolType),
      expressionFailureAt "review-candidate-constraint" 0 TypedMethodSelectionMismatch (TypedTextDetail "Render.map"),
      expressionFailureAt "review-candidate-constraint" 1 TypedMethodSelectionMismatch (TypedTextDetail "Render.map")
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
    [ expressionFailureAtPath
        "review-block-local-monomorphic-scheme"
        [0, 0, 1]
        TypedBinderReferenceMismatch
        (TypedBinderDetail (binder (fixtureModulePath "review-block-local-monomorphic-scheme") [0, 0] (fixtureValueName "local")))
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
        (TypedPatternPath (fixtureModulePath "review-nested-case-pattern-path") [0] [0, 1, 0])
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
        ( TypedBinderDetail
            ( fixtureBinder
                "review-unrelated-type-application"
                0
                (fixtureValueName "unrelated")
            )
        ),
      expressionFailureAt
        "review-unrelated-type-application"
        1
        TypedInstantiationMismatch
        TypedNoValidationDetail
    ]
    (validateTypedProgram unrelatedTypeApplicationProgram)
  assertEqual
    "lexical variable uses match their binder contracts"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-lexical-binder-contract") [0] [0, 0])
        TypedBinderReferenceMismatch
        (TypedBinderDetail (binder (fixtureModulePath "review-lexical-binder-contract") [0] (fixtureValueName "argument")))
    ]
    (validateTypedProgram lexicalBinderContractProgram)
  assertEqual
    "generalized variable uses match substituted scheme results"
    [ expressionFailureAt
        "review-generalized-variable-contract"
        1
        TypedBinderReferenceMismatch
        (TypedBinderDetail (binder (fixtureModulePath "review-generalized-variable-contract") [0] (fixtureValueName "identity")))
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
        (TypedNameDetail duplicateDeclarationName),
      statementFailure
        "review-duplicate-declaration"
        0
        TypedBindingValueMismatch
        (TypedNameDetail duplicateDeclarationName),
      statementFailure
        "review-duplicate-declaration"
        1
        TypedBindingValueMismatch
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
testClassMethodExport = do
  assertEqual
    "target-independent class methods can be exported and selectively imported with a selected body"
    []
    (validateTypedProgram classMethodExportProgram)
  assertEqual
    "target-independent imported class methods cannot omit their dispatch body"
    [ expressionFailure
        "review-missing-target-independent-class-method-dispatch"
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram missingTargetIndependentClassMethodDispatchProgram)

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
        (TypedPatternPath (fixtureModulePath "review-wrong-constructor-pattern-type") [1] [0, 0])
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
    "classes exported in the type namespace publish neither an export nor capability metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "TypeCapability"))
        TypedModuleInterfaceMismatch
        (TypedNameDetail (resolved TypedCurrentModule TypedTypeNamespace "Render")),
      moduleFailure
        "review-imported-type-capability-metadata"
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Render"),
      statementFailure
        "review-imported-type-capability-metadata"
        0
        TypedInvisibleName
        ( TypedNameDetail
            ( resolved
                (TypedImportedModule (fixtureLibraryPath "TypeCapability"))
                TypedCapabilityNamespace
                "Render"
            )
        )
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
        (TypedNameDetail (preludeCapability "Missing")),
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
        (TypedExpressionPath (fixtureModulePath "review-unconstrained-numeric-parameter") [0] [0, 0])
        TypedBindingValueMismatch
        (TypedTextDetail "+")
    ]
    (validateTypedProgram unconstrainedNumericParameterProgram)
  assertEqual
    "equality over type parameters requires a published primitive constraint"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-unconstrained-equality-parameter") [0] [0, 0])
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
    "or-pattern contract mismatches preserve duplicate-name diagnostics"
    [ TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-duplicate-or-pattern-contract") [0] [0, 0])
        TypedDuplicateBinder
        ( TypedBinderDetail
            ( binder
                (fixtureModulePath "review-duplicate-or-pattern-contract")
                [0, 1]
                (fixtureValueName "duplicate")
            )
        ),
      TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-duplicate-or-pattern-contract") [0] [0, 0])
        TypedOrPatternBinderMismatch
        ( TypedBinderDetail
            ( binder
                (fixtureModulePath "review-duplicate-or-pattern-contract")
                [1, 1]
                (fixtureValueName "duplicate")
            )
        ),
      TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-duplicate-or-pattern-contract") [0] [0, 0, 1, 1])
        TypedPatternScrutineeMismatch
        (TypedTypeDetail TypedTextType TypedBoolType)
    ]
    (validateTypedProgram duplicateOrPatternContractProgram)
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
    "or-pattern contracts preserve positional binder associations"
    [ patternFailure
        "review-reordered-or-pattern"
        TypedOrPatternBinderMismatch
        (TypedBinderDetail reorderedOrPatternMismatchBinder)
    ]
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
    "type-only class exports are rejected and cannot expose evidence metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "review-type-visible-impl-import"))
        TypedModuleInterfaceMismatch
        (TypedNameDetail (resolved TypedCurrentModule TypedTypeNamespace "Render")),
      moduleFailure
        "review-type-visible-impl-import"
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Render"),
      expressionFailure
        "review-type-visible-impl-import"
        TypedInvisibleName
        ( TypedNameDetail
            ( resolved
                (TypedImportedModule (fixtureLibraryPath "review-type-visible-impl-import"))
                TypedCapabilityNamespace
                "Render"
            )
        ),
      expressionFailure
        "review-type-visible-impl-import"
        TypedInvisibleImpl
        ( TypedImplDetail
            ( TypedImplId
                (fixtureLibraryPath "review-type-visible-impl-import")
                ( resolved
                    (TypedImportedModule (fixtureLibraryPath "review-type-visible-impl-import"))
                    TypedCapabilityNamespace
                    "Render"
                )
                [TypedBoolType]
            )
        )
    ]
    (validateTypedProgram typeVisibleImplImportProgram)
  assertEqual
    "method-only imports do not turn ordinary nodes into evidence consumers"
    [ expressionFailure
        "review-method-visible-impl-import"
        TypedInvisibleName
        ( TypedNameDetail
            ( resolved
                (TypedImportedModule (fixtureLibraryPath "review-method-visible-impl-import"))
                TypedCapabilityNamespace
                "Render"
            )
        )
    ]
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
        (TypedExpressionPath (fixtureModulePath "review-nested-strict-equality-constraint") [0] [0, 0])
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
        (TypedInterfacePath (fixtureLibraryPath "MissingMetadata"))
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
        TypedDuplicateTypeParameter
        (TypedTypeParameterDetail (TypedTypeParameterId 0)),
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
    "or-patterns require at least two alternatives"
    [ patternFailure
        "review-empty-or-pattern"
        TypedPatternShapeMismatch
        (TypedArityDetail 2 0)
    ]
    (validateTypedProgram emptyOrPatternProgram)
  assertEqual
    "explicit type applications reject ordinary application results"
    [ expressionFailureAt
        "review-non-binding-type-application"
        1
        TypedInstantiationMismatch
        ( TypedBinderDetail
            ( fixtureBinder
                "review-non-binding-type-application"
                0
                (fixtureValueName "identity")
            )
        ),
      expressionFailureAt
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
        (TypedTextDetail "^")
    ]
    (validateTypedProgram mismatchedResolvedOperatorProgram)
  assertEqual
    "data interfaces retain local field-type metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "DataDependency"))
        TypedModuleInterfaceMismatch
        (TypedNameDetail dataInterfaceDependencyHiddenName)
    ]
    (validateTypedProgram dataInterfaceDependencyProgram)
  assertEqual
    "class method interfaces retain local type metadata"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "ClassMethodDependency"))
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
        (TypedPatternPath (fixtureModulePath "review-later-or-pattern-binder-collision") [1] [0, 0, 1])
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

testReservedValueTypedCoreBoundary :: IO ()
testReservedValueTypedCoreBoundary = do
  assertEqual
    "typed-core source identifiers reject the globally reserved value keyword"
    [ statementFailure
        "review-reserved-value-identifier"
        0
        TypedUnresolvedName
        (TypedNameDetail reservedValueIdentifierName)
    ]
    (validateTypedProgram reservedValueIdentifierProgram)
  assertEqual
    "typed-core module paths reject the globally reserved value keyword"
    [ TypedCoreValidationFailure
        (TypedModulePath reservedValueModulePath)
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Fixture::value")
    ]
    (validateTypedProgram reservedValueModulePathProgram)

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
    "retained class methods resolve provider data without exposing the retained capability to source"
    [ statementFailure
        "review-selected-class-data-dependency"
        0
        TypedInvisibleName
        (TypedNameDetail selectedClassDataDependencyCapabilityName),
      statementFailure
        "review-selected-class-data-dependency"
        0
        TypedInvisibleName
        (TypedNameDetail selectedClassDataDependencyCapabilityName)
    ]
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
        (TypedTextDetail "Equal.equal")
    ]
    (validateTypedProgram duplicateUnboundEvidenceProgram)

testGeneralizedClassMethodImport :: IO ()
testGeneralizedClassMethodImport = do
  assertEqual
    "imported class methods quantify their class parameters and select a dispatch body"
    []
    (validateTypedProgram generalizedClassMethodImportProgram)
  assertEqual
    "imported class methods cannot omit their dispatch body"
    [ expressionFailure
        "review-missing-imported-class-method-dispatch"
        TypedMissingEvidence
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0))
    ]
    (validateTypedProgram missingImportedClassMethodDispatchProgram)

testImportedClassCollision :: IO ()
testImportedClassCollision =
  assertEqual
    "resolved constraints do not repeat import-collision diagnostics"
    [ moduleFailure
        "review-imported-class-collision"
        TypedDuplicateDeclaration
        (TypedTextDetail "Clash")
    ]
    (validateTypedProgram importedClassCollisionProgram)

testForwardBlockReference :: IO ()
testForwardBlockReference =
  assertEqual
    "block expressions cannot see later non-recursive declarations"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-forward-block-reference") [0, 0, 0] [0])
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
testModuleQualifiedMethodKey = do
  assertEqual
    "module-qualified capability names use their complete imported origin"
    []
    (validateTypedProgram moduleQualifiedMethodKeyProgram)
  assertEqual
    "module-qualified method keys reject a foreign capability origin"
    [ expressionFailure
        "review-forged-module-qualified-method-key"
        TypedMethodSelectionMismatch
        (TypedTextDetail "Other::Make::make"),
      expressionFailure
        "review-forged-module-qualified-method-key"
        TypedInvisibleName
        (TypedNameDetail (TypedBuiltinName "Other::Make::make"))
    ]
    (validateTypedProgram forgedModuleQualifiedMethodKeyProgram)

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
        ( TypedNameDetail
            ( resolved
                (TypedImportedModule (fixtureLibraryPath "ImportedCapabilityProvider"))
                TypedCapabilityNamespace
                "ForeignEq"
            )
        )
    ]
    (validateTypedProgram importedCapabilityDependencyProgram)

testMetadataOnlyImplVisibility :: IO ()
testMetadataOnlyImplVisibility =
  assertEqual
    "dependency-only capability metadata does not expose its source name"
    [ expressionFailureAt
        "review-metadata-only-impl-visibility"
        0
        TypedInvisibleName
        (TypedNameDetail metadataOnlyImportedCapabilityName)
    ]
    (validateTypedProgram metadataOnlyImplVisibilityProgram)

testPatternExpressionMetadata :: IO ()
testPatternExpressionMetadata =
  assertEqual
    "patterns reject expression-only instantiation and evidence metadata"
    [ TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-pattern-expression-metadata") [3] [0, 0])
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
    "signed value rebinding remains valid and last-wins"
    []
    (validateTypedProgram sameScopeValueRebindingProgram)

testForwardSignedFunctionVisibility :: IO ()
testForwardSignedFunctionVisibility = do
  let expectedNames =
        [ "forward-signed-function-visibility",
          "forward-signed-scalar-invisibility",
          "forward-unsigned-function-invisibility",
          "forward-signed-function-hidden-from-unsigned-caller",
          "forward-signed-function-hidden-from-scalar-expression"
        ]
      expectedResults =
        [ [],
          [ forwardVisibilityFailure
              "forward-signed-scalar-invisibility"
              [0, 0]
              (fixtureValueName "later")
          ],
          [ forwardVisibilityFailure
              "forward-unsigned-function-invisibility"
              [0, 0, 0]
              (fixtureValueName "later")
          ],
          [ forwardVisibilityFailureAt
              "forward-signed-function-hidden-from-unsigned-caller"
              0
              [0, 0, 0]
              (fixtureValueName "later")
          ],
          [ forwardVisibilityFailureAt
              "forward-signed-function-hidden-from-scalar-expression"
              0
              [0, 0]
              (fixtureValueName "later")
          ]
        ]
      actualResults = map (validateTypedProgram . snd) forwardSignedVisibilityPrograms
  assertEqual "supplemental forward visibility names" expectedNames (map fst forwardSignedVisibilityPrograms)
  assertEqual "supplemental forward visibility first run" expectedResults actualResults
  assertEqual
    "supplemental forward visibility second run"
    expectedResults
    (map (validateTypedProgram . snd) forwardSignedVisibilityPrograms)

testNestedForwardSignedFunctionInvisibility :: IO ()
testNestedForwardSignedFunctionInvisibility = do
  let expected =
        [ TypedCoreValidationFailure
            (TypedExpressionPath (fixtureModulePath fixture) [0, 0, 1] [0, 0, 0])
            TypedInvisibleName
            (TypedNameDetail (fixtureValueName "later"))
        ]
      actual = validateTypedProgram nestedForwardSignedFunctionProgram
  assertEqual "nested forward signed function first run" expected actual
  assertEqual
    "nested forward signed function second run"
    expected
    (validateTypedProgram nestedForwardSignedFunctionProgram)
  where
    fixture = "review-nested-forward-signed-function-invisibility"

forwardVisibilityFailure :: Text -> [Int] -> TypedCoreName -> TypedCoreValidationFailure
forwardVisibilityFailure fixture expressionPath name =
  forwardVisibilityFailureAt fixture 1 expressionPath name

forwardVisibilityFailureAt :: Text -> Int -> [Int] -> TypedCoreName -> TypedCoreValidationFailure
forwardVisibilityFailureAt fixture statementIndex expressionPath name =
  TypedCoreValidationFailure
    (TypedExpressionPath (fixtureModulePath fixture) [statementIndex] expressionPath)
    TypedInvisibleName
    (TypedNameDetail name)

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
    [ statementFailure
        "review-bare-signature-visibility"
        0
        TypedBindingValueMismatch
        (TypedNameDetail bareSignatureValueName),
      expressionFailureAt
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
        (TypedInterfacePath (fixtureModulePath "review-active-rebinding-export"))
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

testResolvedModuleOrder :: IO ()
testResolvedModuleOrder =
  assertEqual
    "resolved dependencies precede their importers"
    [ TypedCoreValidationFailure
        (TypedModulePath resolvedModuleOrderImporterPath)
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Dependency::Library")
    ]
    (validateTypedProgram resolvedModuleOrderProgram)

testEmptyResolvedIdentifier :: IO ()
testEmptyResolvedIdentifier =
  assertEqual
    "resolved identifier payloads are nonempty"
    [ statementFailure
        "review-empty-resolved-identifier"
        0
        TypedUnresolvedName
        (TypedNameDetail emptyResolvedIdentifierName),
      expressionFailureAt
        "review-empty-resolved-identifier"
        1
        TypedUnresolvedName
        (TypedNameDetail emptyResolvedIdentifierName)
    ]
    (validateTypedProgram emptyResolvedIdentifierProgram)

testExplicitSpanOnVariable :: IO ()
testExplicitSpanOnVariable =
  assertEqual
    "explicit instantiation spans belong only to type-application nodes"
    [ expressionFailureAt
        "review-explicit-span-on-variable"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail explicitSpanOnVariableOwner)
    ]
    (validateTypedProgram explicitSpanOnVariableProgram)

testSingleEvidenceCandidate :: IO ()
testSingleEvidenceCandidate =
  assertEqual
    "candidate evidence remains legal only while a qualified method is deferred"
    [ expressionFailure
        "review-single-evidence-candidate"
        TypedAmbiguousEvidence
        (TypedArityDetail 1 1)
    ]
    (validateTypedProgram singleEvidenceCandidateProgram)

testEmptyModulePath :: IO ()
testEmptyModulePath =
  assertEqual
    "module identities contain at least one nonempty path segment"
    [ TypedCoreValidationFailure
        (TypedModulePath [])
        TypedModuleInterfaceMismatch
        (TypedTextDetail "")
    ]
    (validateTypedProgram emptyModulePathProgram)

testAmbientPreludePath :: IO ()
testAmbientPreludePath =
  assertEqual
    "the ambient prelude slot has the canonical Prelude identity"
    [ TypedCoreValidationFailure
        TypedPreludePath
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Library::WrongPrelude")
    ]
    (validateTypedProgram wrongPreludeSlotProgram)

testSignatureBindingContract :: IO ()
testSignatureBindingContract = do
  assertEqual
    "an attached signature and binding publish one scheme contract"
    [ statementFailure
        "review-signature-binding-mismatch"
        0
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType TypedTextType)
    ]
    (validateTypedProgram signatureBindingMismatchProgram)
  assertEqual
    "an attached signature preserves callable-shape mismatch diagnostics"
    [ statementFailure
        "review-signature-binding-shape-mismatch"
        0
        TypedCallableShapeMismatch
        (TypedBinderDetail shapeBindingOwner)
    ]
    (validateTypedProgram signatureBindingShapeMismatchProgram)

testQualifiedMethodTypeApplication :: IO ()
testQualifiedMethodTypeApplication =
  assertEqual
    "qualified method type applications resolve through selected method evidence"
    []
    (validateTypedProgram qualifiedMethodTypeApplicationProgram)

testFinalReviewRegressions :: IO ()
testFinalReviewRegressions = do
  assertEqual
    "alias-shaped self references are visible in their own binding"
    []
    (validateTypedProgram aliasShapedSelfRecursionProgram)
  assertEqual
    "qualified method values match their selected class method contract"
    [ expressionFailure
        "review-qualified-method-value-contract"
        TypedBindingValueMismatch
        (TypedTypeDetail builtinMapType boolToBoolType)
    ]
    (validateTypedProgram qualifiedMethodValueContractProgram)
  assertEqual
    "eager self references remain outside recursive binding scope"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-eager-self-reference") [0] [0, 0])
        TypedInvisibleName
        (TypedNameDetail eagerSelfReferenceName)
    ]
    (validateTypedProgram eagerSelfReferenceProgram)

testPostFinalReviewRegressions :: IO ()
testPostFinalReviewRegressions = do
  assertEqual
    "resolved imports reject source-visible value and type collisions"
    [ moduleFailure
        "review-import-name-collision"
        TypedDuplicateDeclaration
        (TypedTextDetail "shared"),
      moduleFailure
        "review-import-name-collision"
        TypedDuplicateDeclaration
        (TypedTextDetail "Box")
    ]
    (validateTypedProgram importNameCollisionProgram)
  assertEqual
    "local class methods remain outside ordinary value scope"
    [ expressionFailureAt
        "review-local-class-method-visibility"
        1
        TypedInvisibleName
        (TypedNameDetail localClassMethodName)
    ]
    (validateTypedProgram localClassMethodVisibilityProgram)
  assertEqual
    "same-scope rebinding follows declaration order rather than binder payloads"
    []
    (validateTypedProgram syntheticBinderShadowingProgram)
  assertEqual
    "concrete impl bodies cannot retain substituted class parameters"
    [ TypedUnboundTypeParameter,
      TypedUnboundTypeParameter,
      TypedUnboundRepresentationParameter,
      TypedUnboundRepresentationParameter,
      TypedUnboundTypeParameter,
      TypedUnboundRepresentationParameter
    ]
    (validationKinds implFreeClassParameterProgram)
  assertEqual
    "qualified method candidate sets reject duplicate impl-method identities"
    [ expressionFailure
        "review-duplicate-qualified-method-candidate"
        TypedDuplicateEvidence
        (TypedImplDetail duplicateQualifiedMethodCandidateImpl)
    ]
    (validateTypedProgram duplicateQualifiedMethodCandidateProgram)
  assertEqual
    "metadata-only imported data types remain unavailable to source declarations"
    [ statementFailure
        "review-metadata-only-source-type"
        0
        TypedInvisibleName
        (TypedNameDetail metadataOnlyImportedTypeName)
    ]
    (validateTypedProgram metadataOnlySourceTypeProgram)
  assertEqual
    "character literals and literal patterns require Unicode scalar values"
    [ expressionFailureAt
        "review-non-scalar-character"
        0
        TypedLiteralTypeMismatch
        (TypedTextDetail "non-scalar character"),
      TypedCoreValidationFailure
        (TypedPatternPath (fixtureModulePath "review-non-scalar-character") [1] [0, 0])
        TypedLiteralTypeMismatch
        (TypedTextDetail "non-scalar character")
    ]
    (validateTypedProgram nonScalarCharacterProgram)

testMethodOnlyCapabilityVisibility :: IO ()
testMethodOnlyCapabilityVisibility = do
  assertEqual
    "inferred schemes may retain capability metadata hidden from source"
    []
    (validateTypedProgram inferredMethodOnlyCapabilityVisibilityProgram)
  assertEqual
    "explicit signatures still require source-visible capability names"
    [ statementFailure
        "review-method-only-capability-visibility"
        0
        TypedInvisibleName
        ( TypedNameDetail
            ( resolved
                (TypedImportedModule (fixtureLibraryPath "MethodOnlyCapability"))
                TypedCapabilityNamespace
                "Render"
            )
        )
    ]
    (validateTypedProgram explicitMethodOnlyCapabilityVisibilityProgram)

testCapabilityImportCollision :: IO ()
testCapabilityImportCollision =
  assertEqual
    "capability imports share the source symbol collision domain"
    [ moduleFailure
        "review-capability-import-collision"
        TypedDuplicateDeclaration
        (TypedTextDetail "Shared")
    ]
    (validateTypedProgram capabilityImportCollisionProgram)

testNestedTypeParameterShadowing :: IO ()
testNestedTypeParameterShadowing =
  assertEqual
    "nested schemes cannot reuse enclosing type-parameter ordinals"
    [TypedDuplicateTypeParameter]
    (validationKinds nestedTypeParameterShadowingProgram)

testTypeOnlyImportSelector :: IO ()
testTypeOnlyImportSelector =
  assertEqual
    "explicit imports reject type-only selectors"
    [ moduleFailure
        "review-type-only-import-selector"
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Box")
    ]
    (validateTypedProgram typeOnlyImportSelectorProgram)

testOrdinaryUnboundEvidence :: IO ()
testOrdinaryUnboundEvidence =
  assertEqual
    "ordinary nodes reject selected evidence with no consuming obligation"
    [ expressionFailure
        "review-ordinary-unbound-evidence"
        TypedMethodSelectionMismatch
        (TypedTextDetail "Equal.equal")
    ]
    (validateTypedProgram ordinaryUnboundEvidenceProgram)

testNestedLocalGeneralization :: IO ()
testNestedLocalGeneralization =
  assertEqual
    "nested schemes allocate ordinals after enclosing type parameters"
    []
    (validateTypedProgram nestedLocalGeneralizationProgram)

testNonConcreteImplTarget :: IO ()
testNonConcreteImplTarget =
  assertEqual
    "impl declarations require recursively concrete target types"
    [ statementFailure
        "review-non-concrete-impl-target"
        1
        TypedMethodSelectionMismatch
        (TypedImplDetail nonConcreteImplTargetId)
    ]
    (validateTypedProgram nonConcreteImplTargetProgram)

testBlockDeclarationScope :: IO ()
testBlockDeclarationScope =
  assertEqual
    "blocks reject declarations that the source grammar reserves for module scope"
    [ statementFailureAtPath
        "review-block-declaration-scope"
        [0, 0, 0]
        TypedBlockResultMismatch
        (TypedTextDetail "data declaration"),
      statementFailureAtPath
        "review-block-declaration-scope"
        [0, 0, 1]
        TypedBlockResultMismatch
        (TypedTextDetail "class declaration"),
      statementFailureAtPath
        "review-block-declaration-scope"
        [0, 0, 2]
        TypedBlockResultMismatch
        (TypedTextDetail "impl declaration")
    ]
    (validateTypedProgram blockDeclarationScopeProgram)

testModulePathIdentifierSegments :: IO ()
testModulePathIdentifierSegments =
  assertEqual
    "module paths contain only lexer-produced identifier segments"
    [ TypedCoreValidationFailure
        (TypedModulePath ["A::B"])
        TypedModuleInterfaceMismatch
        (TypedTextDetail "A::B"),
      TypedCoreValidationFailure
        (TypedModulePath ["App/Main"])
        TypedModuleInterfaceMismatch
        (TypedTextDetail "App/Main"),
      TypedCoreValidationFailure
        (TypedModulePath ["if"])
        TypedModuleInterfaceMismatch
        (TypedTextDetail "if")
    ]
    (concatMap validateTypedProgram [delimiterModulePathProgram, slashModulePathProgram, reservedModulePathProgram])

testModuleMetadataIdentity :: IO ()
testModuleMetadataIdentity =
  assertEqual
    "module metadata rejects source-impossible instantiation metadata"
    [ moduleFailure
        "review-module-metadata-identity"
        TypedInstantiationMismatch
        (TypedBinderDetail moduleMetadataIdentityOwner)
    ]
    (validateTypedProgram moduleMetadataIdentityProgram)

testQualifiedTypeApplicationInstantiation :: IO ()
testQualifiedTypeApplicationInstantiation =
  assertEqual
    "qualified type applications cannot retain unrelated ordinary instantiations"
    [ expressionFailureAt
        "review-qualified-type-application-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail qualifiedTypeApplicationInstantiationOwner)
    ]
    (validateTypedProgram qualifiedTypeApplicationInstantiationProgram)

testLocalClassMethodSchemeIsolation :: IO ()
testLocalClassMethodSchemeIsolation =
  mapM_
    (\program -> assertEqual "local class methods never replace active value schemes" [] (validateTypedProgram program))
    [localClassMethodAfterValueProgram, localClassMethodBeforeValueProgram]

testRetainedCapabilityEvidence :: IO ()
testRetainedCapabilityEvidence =
  assertEqual
    "selective constrained-value imports retain capability and impl metadata without exposing the capability name"
    []
    (validateTypedProgram retainedCapabilityEvidenceProgram)

testRetainedCapabilityEvidenceOrigin :: IO ()
testRetainedCapabilityEvidenceOrigin =
  assertEqual
    "retained evidence parameters reject an implementation of another capability"
    [ expressionFailureAt
        "review-retained-capability-wrong-impl"
        3
        TypedMethodSelectionMismatch
        (TypedNameDetail retainedCapabilityWrongImplName)
    ]
    (validateTypedProgram retainedCapabilityWrongImplProgram)

testMissingConstructorInstantiation :: IO ()
testMissingConstructorInstantiation =
  assertEqual
    "generic constructor values publish their owner instantiation"
    [ expressionFailureAt
        "review-missing-constructor-instantiation"
        1
        TypedInstantiationMismatch
        (TypedBinderDetail missingConstructorInstantiationOwner)
    ]
    (validateTypedProgram missingConstructorInstantiationProgram)

testMissingPublishedImpl :: IO ()
testMissingPublishedImpl =
  assertEqual
    "interfaces publish every local impl for a retained capability"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-missing-published-impl"))
        TypedModuleInterfaceMismatch
        (TypedImplDetail missingPublishedImplId)
    ]
    (validateTypedProgram missingPublishedImplProgram)

testExpandingRecursiveEquality :: IO ()
testExpandingRecursiveEquality =
  assertEqual
    "equality support terminates when recursive instantiations keep expanding"
    []
    (validateTypedProgram expandingRecursiveEqualityProgram)

testRecursiveEqualityCallableField :: IO ()
testRecursiveEqualityCallableField =
  assertEqual
    "recursion detection still checks non-recursive constructor fields"
    [ statementFailure
        "review-recursive-equality-callable-field"
        1
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType recursiveEqualityCallableType)
    ]
    (validateTypedProgram recursiveEqualityCallableFieldProgram)

testOrphanSignature :: IO ()
testOrphanSignature =
  assertEqual
    "signatures require an adjacent same-name binding"
    [ statementFailure
        "review-orphan-signature"
        0
        TypedBindingValueMismatch
        (TypedNameDetail orphanSignatureName)
    ]
    (validateTypedProgram orphanSignatureProgram)

testRecursiveEqualityNestedCallable :: IO ()
testRecursiveEqualityNestedCallable =
  assertEqual
    "recursive equality checks transformed payload arguments"
    [ statementFailure
        "review-recursive-equality-nested-callable"
        1
        TypedBindingValueMismatch
        (TypedTypeDetail TypedBoolType recursiveEqualityNestedCallableType)
    ]
    (validateTypedProgram recursiveEqualityNestedCallableProgram)

testImportedCurrentOrigin :: IO ()
testImportedCurrentOrigin =
  assertEqual
    "imported origins cannot identify the current module"
    [ expressionFailureAt
        "review-imported-current-origin"
        1
        TypedInvisibleName
        (TypedNameDetail importedCurrentOriginName)
    ]
    (validateTypedProgram importedCurrentOriginProgram)

testRetainedCapabilityExport :: IO ()
testRetainedCapabilityExport =
  assertEqual
    "retained imported capabilities cannot become explicit exports"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "RetainedCapabilityFacade"))
        TypedModuleInterfaceMismatch
        ( TypedNameDetail
            (resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq")
        )
    ]
    (validateTypedProgram retainedCapabilityExportProgram)

testImportAliasCollision :: IO ()
testImportAliasCollision =
  assertEqual
    "import aliases have their own collision scope"
    [ moduleFailure
        "review-import-alias-collision"
        TypedDuplicateDeclaration
        (TypedTextDetail "Ops")
    ]
    (validateTypedProgram importAliasCollisionProgram)

testImplBeforeClass :: IO ()
testImplBeforeClass =
  assertEqual
    "impl validation cannot see a later class declaration"
    [ statementFailure
        "review-impl-before-class"
        0
        TypedInvisibleName
        (TypedNameDetail implBeforeClassCapabilityName)
    ]
    (validateTypedProgram implBeforeClassProgram)

testEvidenceCapabilityOrigin :: IO ()
testEvidenceCapabilityOrigin =
  assertEqual
    "bound evidence keeps the capability origin associated with its owner"
    [ expressionFailure
        "review-evidence-capability-origin"
        TypedMethodSelectionMismatch
        (TypedNameDetail evidenceCapabilityWrongName)
    ]
    (validateTypedProgram evidenceCapabilityOriginProgram)

testMalformedGeneratedNames :: IO ()
testMalformedGeneratedNames =
  assertEqual
    "compiler-generated names validate their payloads"
    [ expressionFailureAt
        "review-malformed-generated-names"
        0
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedLambdaPatternArgument 0))
        ),
      statementFailure
        "review-malformed-generated-names"
        1
        TypedUnresolvedName
        (TypedNameDetail (TypedGeneratedName (TypedOperatorBinding ""))),
      statementFailure
        "review-malformed-generated-names"
        2
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "operator:%2B"))
        ),
      statementFailure
        "review-malformed-generated-names"
        3
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:garbage"))
        ),
      statementFailure
        "review-malformed-generated-names"
        4
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%GG"))
        ),
      statementFailure
        "review-malformed-generated-names"
        5
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2B"))
        ),
      statementFailure
        "review-malformed-generated-names"
        6
        TypedUnresolvedName
        ( TypedNameDetail
            (TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E"))
        )
    ]
    (validateTypedProgram malformedGeneratedNamesProgram)

testRegularPreludeModule :: IO ()
testRegularPreludeModule =
  assertEqual
    "the Prelude path belongs only to the explicit prelude slot"
    [ TypedCoreValidationFailure
        (TypedModulePath ["Prelude"])
        TypedModuleInterfaceMismatch
        (TypedTextDetail "Prelude")
    ]
    (validateTypedProgram regularPreludeModuleProgram)

testRetainedClassMethodExport :: IO ()
testRetainedClassMethodExport =
  assertEqual
    "retained imported class methods cannot become explicit exports"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureLibraryPath "RetainedMethodFacade"))
        TypedModuleInterfaceMismatch
        ( TypedNameDetail
            (resolved TypedCurrentModule TypedValueNamespace "display")
        )
    ]
    (validateTypedProgram retainedClassMethodExportProgram)

testMalformedResolvedIdentifiers :: IO ()
testMalformedResolvedIdentifiers =
  assertEqual
    "resolved identifiers obey source spelling and reserved-token rules"
    [ statementFailure
        "review-malformed-resolved-identifiers"
        0
        TypedUnresolvedName
        (TypedNameDetail malformedWhitespaceName),
      statementFailure
        "review-malformed-resolved-identifiers"
        1
        TypedUnresolvedName
        (TypedNameDetail malformedReservedName),
      statementFailure
        "review-malformed-resolved-identifiers"
        2
        TypedUnresolvedName
        (TypedNameDetail malformedQualifiedName)
    ]
    (validateTypedProgram malformedResolvedIdentifiersProgram)

testNormalizedPreludeImplDuplicates :: IO ()
testNormalizedPreludeImplDuplicates =
  assertEqual
    "Prelude impl duplicates compare canonical capability and target identities"
    [ TypedCoreValidationFailure
        (TypedStatementPath ["Prelude"] [2])
        TypedDuplicateDeclaration
        (TypedImplDetail normalizedPreludeAmbientImpl)
    ]
    (validateTypedProgram normalizedPreludeImplDuplicatesProgram)

testMalformedImportAlias :: IO ()
testMalformedImportAlias =
  assertEqual
    "import aliases obey source identifier rules"
    [ moduleFailure
        "review-malformed-import-alias"
        TypedUnresolvedName
        (TypedTextDetail "True")
    ]
    (validateTypedProgram malformedImportAliasProgram)

testDuplicateModuleExports :: IO ()
testDuplicateModuleExports =
  assertEqual
    "module exports are unique within each namespace"
    [ TypedCoreValidationFailure
        (TypedInterfacePath (fixtureModulePath "review-duplicate-module-exports"))
        TypedDuplicateDeclaration
        ( TypedNameDetail
            (resolved TypedCurrentModule TypedValueNamespace "answer")
        )
    ]
    (validateTypedProgram duplicateModuleExportsProgram)

testInvalidSourceSpans :: IO ()
testInvalidSourceSpans = do
  assertEqual
    "all retained source spans use positive one-based coordinates"
    expectedPaths
    (map validationPaths invalidSourceSpanPrograms)
  assertEqual
    "invalid spans use the dedicated validation kind"
    (map (map (const TypedInvalidSpan)) expectedPaths)
    (map validationKinds invalidSourceSpanPrograms)
  where
    invalidSourceSpanPrograms =
      [ invalidImportSpanProgram,
        invalidStatementSpansProgram,
        invalidDeclarationSpansProgram,
        invalidExpressionSpansProgram
      ]
    expectedPaths =
      [ [TypedModulePath (fixtureModulePath "review-invalid-import-span")],
        map
          (TypedStatementPath (fixtureModulePath "review-invalid-statement-spans") . pure)
          [0, 1, 2],
        concatMap
          (replicate 3 . TypedStatementPath ["Prelude"] . pure)
          [0 .. 4],
        [ TypedExpressionPath (fixtureModulePath "review-invalid-expression-spans") [1] [0],
          TypedExpressionPath (fixtureModulePath "review-invalid-expression-spans") [1] [0],
          TypedExpressionPath (fixtureModulePath "review-invalid-expression-spans") [1] [0, 0]
        ]
      ]
    validationPaths program =
      [path | TypedCoreValidationFailure path _ _ <- validateTypedProgram program]

testUnresolvedReviewRegressions :: IO ()
testUnresolvedReviewRegressions =
  assertEqual
    "canonical inventory regressions"
    expected
    [ (label, validateTypedProgram program)
    | (label, program) <- programs
    ]
  where
    programs :: [(Text, TypedProgram)]
    programs =
      [ ("nonempty import selectors", emptyImportSelectorProgram),
        ("unique import selectors", duplicateImportSelectorProgram),
        ("exclusive import alias and selectors", aliasAndSelectorImportProgram),
        ("class-scoped method identities", distinctClassMethodProgram),
        ("unique evidence obligations", duplicateEvidenceConstraintProgram),
        ("tuple arity", singletonTupleTypeProgram),
        ("Prelude data closure", preludeAmbientDataDependencyProgram),
        ("unique interface entries", duplicateModuleInterfaceEntriesProgram),
        ("resolved capability dependencies", sameNamedCapabilityDependencyProgram),
        ("metadata-only same-named capabilities", sameNamedRetainedCapabilityProgram)
      ]
    expected :: [(Text, [TypedCoreValidationFailure])]
    expected =
      [ ( "nonempty import selectors",
          [ moduleFailure
              "review-empty-import-selector"
              TypedModuleInterfaceMismatch
              (TypedArityDetail 1 0)
          ]
        ),
        ( "unique import selectors",
          [ moduleFailure
              "review-duplicate-import-selector"
              TypedDuplicateDeclaration
              (TypedTextDetail "item")
          ]
        ),
        ( "exclusive import alias and selectors",
          [ moduleFailure
              "review-alias-and-selector-import"
              TypedModuleInterfaceMismatch
              (TypedTextDetail "alias and selectors")
          ]
        ),
        ("class-scoped method identities", []),
        ( "unique evidence obligations",
          [ statementFailure
              "review-duplicate-evidence-constraint"
              0
              TypedDuplicateEvidenceParameter
              (TypedEvidenceParameterDetail (TypedEvidenceParameterId 1))
          ]
        ),
        ( "tuple arity",
          [ expressionFailure
              "review-singleton-tuple-type"
              TypedCollectionShapeMismatch
              (TypedArityDetail 2 1)
          ]
        ),
        ( "Prelude data closure",
          [ TypedCoreValidationFailure
              (TypedInterfacePath ["Prelude"])
              TypedModuleInterfaceMismatch
              (TypedNameDetail preludeAmbientDependencyName)
          ]
        ),
        ( "unique interface entries",
          [ TypedCoreValidationFailure
              (TypedInterfacePath duplicateInterfaceModulePath)
              TypedDuplicateDeclaration
              (TypedNameDetail duplicateInterfaceValueName),
            TypedCoreValidationFailure
              (TypedInterfacePath duplicateInterfaceModulePath)
              TypedDuplicateDeclaration
              (TypedNameDetail duplicateInterfaceDataName),
            TypedCoreValidationFailure
              (TypedInterfacePath duplicateInterfaceModulePath)
              TypedDuplicateDeclaration
              (TypedNameDetail duplicateInterfaceClassName),
            TypedCoreValidationFailure
              (TypedInterfacePath duplicateInterfaceModulePath)
              TypedDuplicateDeclaration
              (TypedImplDetail duplicateInterfaceImplId)
          ]
        ),
        ( "resolved capability dependencies",
          [ TypedCoreValidationFailure
              (TypedInterfacePath sameNamedCapabilityFacadePath)
              TypedModuleInterfaceMismatch
              (TypedNameDetail sameNamedImportedCapabilityName)
          ]
        ),
        ( "metadata-only same-named capabilities",
          [ expressionFailure
              "review-same-named-retained-capability"
              TypedInvisibleName
              (TypedNameDetail sameNamedImportedCapabilityName),
            expressionFailure
              "review-same-named-retained-capability"
              TypedInvisibleImpl
              (TypedImplDetail sameNamedImportedImplId)
          ]
        )
      ]

validationKinds :: TypedProgram -> [TypedCoreValidationKind]
validationKinds program =
  [kind | TypedCoreValidationFailure _ kind _ <- validateTypedProgram program]

emptyImportSelectorProgram :: TypedProgram
emptyImportSelectorProgram =
  importSelectorShapeProgram "review-empty-import-selector" []

duplicateImportSelectorProgram :: TypedProgram
duplicateImportSelectorProgram =
  importSelectorShapeProgram "review-duplicate-import-selector" ["item", "item"]

aliasAndSelectorImportProgram :: TypedProgram
aliasAndSelectorImportProgram =
  importSelectorProgram
    "review-alias-and-selector-import"
    (Just "Library")
    ["item"]

importSelectorShapeProgram :: Text -> [Text] -> TypedProgram
importSelectorShapeProgram fixture selectedNames =
  importSelectorProgram fixture Nothing selectedNames

importSelectorProgram :: Text -> Maybe Text -> [Text] -> TypedProgram
importSelectorProgram fixture alias selectedNames =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath fixture)
    entryPath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueOwner = binder libraryPath [0] valueName
    valueScheme = monoScheme valueOwner
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath ("src/Library/" <> fixture <> ".jz"))
        []
        [TypedModuleExport TypedValueNamespace "item"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath alias (Just selectedNames)]
        []
        emptyInterface
        []
        unitInfo

sameNamedCapabilityProviderPath :: [Text]
sameNamedCapabilityProviderPath =
  fixtureLibraryPath "SameNamedCapabilityProvider"

sameNamedImportedCapabilityName :: TypedCoreName
sameNamedImportedCapabilityName =
  resolved
    (TypedImportedModule sameNamedCapabilityProviderPath)
    TypedCapabilityNamespace
    "Shared"

sameNamedImportedImplId :: TypedImplId
sameNamedImportedImplId =
  TypedImplId
    sameNamedCapabilityProviderPath
    sameNamedImportedCapabilityName
    [TypedBoolType]

sameNamedCapabilityProviderModule :: TypedModule
sameNamedCapabilityProviderModule =
  sameNamedCapabilityProviderModuleAt sameNamedCapabilityProviderPath

sameNamedCapabilityProviderModuleAt :: [Text] -> TypedModule
sameNamedCapabilityProviderModuleAt providerPath =
  typedModule
    providerPath
    (TypedSourcePath "src/Library/SameNamedCapabilityProvider.jz")
    []
    [TypedModuleExport TypedValueNamespace "source"]
    ( TypedModuleInterface
        [TypedValueInterface sourceName sourceScheme]
        []
        [TypedClassInterface capability]
        [TypedImplInterface localImplId]
    )
    [ TypedClassStatement capability,
      TypedImplStatement (TypedImplDeclaration span1 localImplId []),
      TypedLetStatement sourceOwner sourceName span1 sourceScheme trueExpr
    ]
    unitInfo
  where
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    localImplId =
      TypedImplId
        providerPath
        capabilityName
        [TypedBoolType]
    sourceName =
      resolved TypedCurrentModule TypedValueNamespace "source"
    sourceOwner =
      binder providerPath [2] sourceName
    sourceScheme =
      fixtureScheme
        sourceOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint capabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe

sameNamedVisibleCapabilityProviderModuleAt :: [Text] -> TypedModule
sameNamedVisibleCapabilityProviderModuleAt providerPath =
  typedModule
    providerPath
    (TypedSourcePath "src/Library/SameNamedVisibleCapabilityProvider.jz")
    []
    [TypedModuleExport TypedCapabilityNamespace "Shared"]
    (TypedModuleInterface [] [] [TypedClassInterface capability] [])
    [TypedClassStatement capability]
    unitInfo
  where
    capability =
      TypedClassDeclaration
        span1
        (resolved TypedCurrentModule TypedCapabilityNamespace "Shared")
        [TypedTypeParameterId 0]
        []

sameNamedCapabilityFacadePath :: [Text]
sameNamedCapabilityFacadePath =
  fixtureLibraryPath "SameNamedCapabilityFacadeMissing"

sameNamedCapabilityDependencyProgram :: TypedProgram
sameNamedCapabilityDependencyProgram =
  TypedProgram
    Nothing
    [ sameNamedVisibleCapabilityProviderModuleAt sameNamedCapabilityProviderPath,
      secondProviderModule,
      facadeModule
    ]
    sameNamedCapabilityFacadePath
  where
    secondProviderPath =
      fixtureLibraryPath "SameNamedCapabilityProviderTwo"
    secondProviderModule =
      sameNamedVisibleCapabilityProviderModuleAt secondProviderPath
    secondImportedCapabilityName =
      resolved
        (TypedImportedModule secondProviderPath)
        TypedCapabilityNamespace
        "Shared"
    retainedWrongCapability =
      TypedClassDeclaration
        span1
        secondImportedCapabilityName
        [TypedTypeParameterId 0]
        []
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner =
      binder sameNamedCapabilityFacadePath [0] publishedName
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            ( TypedCapabilityConstraint
                sameNamedImportedCapabilityName
                Nothing
                TypedBoolType
            )
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        sameNamedCapabilityFacadePath
        (TypedSourcePath "src/Library/SameNamedCapabilityFacadeMissing.jz")
        [ TypedResolvedImport
            span1
            sameNamedCapabilityProviderPath
            (Just "First")
            Nothing,
          TypedResolvedImport
            span1
            secondProviderPath
            (Just "Second")
            Nothing
        ]
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface retainedWrongCapability]
            []
        )
        [ TypedLetStatement
            publishedOwner
            publishedName
            span1
            publishedScheme
            trueExpr
        ]
        unitInfo

sameNamedRetainedCapabilityProgram :: TypedProgram
sameNamedRetainedCapabilityProgram =
  TypedProgram
    Nothing
    [sameNamedCapabilityProviderModule, facadeModule, entryModule]
    entryPath
  where
    facadePath =
      fixtureLibraryPath "SameNamedCapabilityFacade"
    entryPath =
      fixtureModulePath "review-same-named-retained-capability"
    parameter = TypedTypeParameterId 0
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    localCapability =
      TypedClassDeclaration span1 localCapabilityName [parameter] []
    retainedCapability =
      TypedClassDeclaration span1 sameNamedImportedCapabilityName [parameter] []
    constraint =
      TypedCapabilityConstraint
        sameNamedImportedCapabilityName
        Nothing
        TypedBoolType
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/SameNamedCapabilityFacade.jz")
        [ TypedResolvedImport
            span1
            sameNamedCapabilityProviderPath
            Nothing
            (Just ["source"])
        ]
        [TypedModuleExport TypedCapabilityNamespace "Shared"]
        ( TypedModuleInterface
            []
            []
            [ TypedClassInterface localCapability,
              TypedClassInterface retainedCapability
            ]
            [TypedImplInterface sameNamedImportedImplId]
        )
        [TypedClassStatement localCapability]
        unitInfo
    evidenceUse =
      TypedEvidenceUse
        Nothing
        constraint
        sameNamedImportedImplId
        Nothing
    expression =
      TypedLiteralExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            []
            [TypedSelectedEvidence evidenceUse]
        )
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["Shared"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

distinctClassMethodProgram :: TypedProgram
distinctClassMethodProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-distinct-class-methods"
    modulePath = (fixtureModulePath fixture)
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    classDeclaration statementIndex classIdentifier =
      let methodOwner = binder modulePath [statementIndex, 0] methodName
          methodScheme = monoScheme methodOwner
       in TypedClassDeclaration
            span1
            (resolved TypedCurrentModule TypedCapabilityNamespace classIdentifier)
            [TypedTypeParameterId 0]
            [TypedMethodSignature methodName span1 methodScheme]
    statements =
      [ TypedClassStatement (classDeclaration 0 "Render"),
        TypedClassStatement (classDeclaration 1 "Debug")
      ]

duplicateEvidenceConstraintProgram :: TypedProgram
duplicateEvidenceConstraintProgram =
  withFixturePrelude (signatureProgram fixture valueOwner valueName valueScheme)
  where
    fixture = "review-duplicate-evidence-constraint"
    valueName = fixtureValueName "item"
    valueOwner = fixtureBinder fixture 0 valueName
    constraint =
      TypedCapabilityConstraint
        (preludeCapability "Equal")
        Nothing
        TypedBoolType
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
          TypedEvidenceParameter (TypedEvidenceParameterId 1) constraint
        ]
        []
        TypedBoolType
        TypedBoolRecipe

singletonTupleTypeProgram :: TypedProgram
singletonTupleTypeProgram =
  expressionFixtureProgram
    "review-singleton-tuple-type"
    (TypedTupleExpr singletonInfo [trueExpr])
  where
    singletonInfo =
      info
        (TypedTupleType [TypedBoolType])
        (TypedManagedProductRecipe [TypedBoolRecipe])

preludeAmbientDependencyName :: TypedCoreName
preludeAmbientDependencyName =
  resolved TypedAmbientPrelude TypedTypeNamespace "Payload"

preludeAmbientDataDependencyProgram :: TypedProgram
preludeAmbientDataDependencyProgram =
  TypedProgram (Just preludeModule) [entryModule] entryPath
  where
    fixture = "review-prelude-ambient-data-dependency"
    entryPath = (fixtureModulePath fixture)
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        ["Prelude"]
        [0, 0]
        preludeAmbientDependencyName
        []
    className =
      resolved TypedCurrentModule TypedCapabilityNamespace "ProvidesPayload"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "payload"
    methodOwner = binder ["Prelude"] [1, 0] methodName
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedDataType preludeAmbientDependencyName [])
        (TypedManagedVariantRecipe preludeAmbientDependencyName [])
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 methodScheme]
    preludeModule =
      typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ProvidesPayload"]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration
        ]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo

duplicateInterfaceModulePath :: [Text]
duplicateInterfaceModulePath =
  (fixtureModulePath "review-duplicate-interface-entries")

duplicateInterfaceValueName :: TypedCoreName
duplicateInterfaceValueName =
  resolved TypedCurrentModule TypedValueNamespace "published"

duplicateInterfaceDataName :: TypedCoreName
duplicateInterfaceDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Flag"

duplicateInterfaceClassName :: TypedCoreName
duplicateInterfaceClassName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Render"

duplicateInterfaceImplId :: TypedImplId
duplicateInterfaceImplId =
  TypedImplId
    duplicateInterfaceModulePath
    duplicateInterfaceClassName
    [TypedBoolType]

duplicateModuleInterfaceEntriesProgram :: TypedProgram
duplicateModuleInterfaceEntriesProgram =
  TypedProgram Nothing [moduleValue] duplicateInterfaceModulePath
  where
    valueOwner =
      binder duplicateInterfaceModulePath [0] duplicateInterfaceValueName
    valueScheme = monoScheme valueOwner
    valueInterface =
      TypedValueInterface duplicateInterfaceValueName valueScheme
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        duplicateInterfaceModulePath
        [1, 0]
        duplicateInterfaceDataName
        []
    dataInterface = TypedDataInterface dataDeclaration
    classDeclaration =
      TypedClassDeclaration
        span1
        duplicateInterfaceClassName
        [TypedTypeParameterId 0]
        []
    classInterface = TypedClassInterface classDeclaration
    implInterface = TypedImplInterface duplicateInterfaceImplId
    moduleValue =
      typedModule
        duplicateInterfaceModulePath
        relativeSource
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [valueInterface, valueInterface]
            [dataInterface, dataInterface]
            [classInterface, classInterface]
            [implInterface, implInterface]
        )
        [ TypedLetStatement valueOwner duplicateInterfaceValueName span1 valueScheme trueExpr,
          TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 duplicateInterfaceImplId [])
        ]
        unitInfo

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
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] bareSignatureValueName
    statements =
      [ TypedSignatureStatement owner bareSignatureValueName span1 (monoScheme owner),
        expressionStatement 1 (fixtureVariableExpr boolInfo bareSignatureValueName)
      ]

activeRebindingExportName :: TypedCoreName
activeRebindingExportName =
  resolved TypedCurrentModule TypedValueNamespace "item"

activeRebindingExportProgram :: TypedProgram
activeRebindingExportProgram =
  singleModuleProgram fixture relativeSource exports statements interface unitInfo modulePath
  where
    fixture = "review-active-rebinding-export"
    modulePath = (fixtureModulePath fixture)
    firstOwner = binder modulePath [0] activeRebindingExportName
    secondOwner = binder modulePath [1] activeRebindingExportName
    firstScheme = monoScheme firstOwner
    secondScheme =
      fixtureScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    exports = [TypedModuleExport TypedValueNamespace "item"]
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
    modulePath = (fixtureModulePath fixture)
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
        expressionStatement 1 (fixtureBoundVariableExpr constructorOwner constructorInfo constructorName)
      ]

missingConstructorInstantiationOwner :: TypedBinderId
missingConstructorInstantiationOwner =
  binder
    (fixtureModulePath "review-missing-constructor-instantiation")
    [0, 0]
    (resolved TypedCurrentModule TypedConstructorNamespace "Some")

missingConstructorInstantiationProgram :: TypedProgram
missingConstructorInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface constructorInfo modulePath
  where
    fixture = "review-missing-constructor-instantiation"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameterId]
        [ TypedConstructorDeclaration
            missingConstructorInstantiationOwner
            constructorName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType dataName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe dataName [TypedBoolType]))
        []
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (fixtureBoundVariableExpr missingConstructorInstantiationOwner constructorInfo constructorName)
      ]

retainedCapabilityEvidenceProgram :: TypedProgram
retainedCapabilityEvidenceProgram =
  TypedProgram Nothing [providerModule, facadeModule, entryModule] entryPath
  where
    providerPath = (fixtureLibraryPath "RetainedCapabilityProvider")
    facadePath = (fixtureLibraryPath "RetainedCapabilityFacade")
    entryPath = (fixtureModulePath "review-retained-capability-evidence")
    parameter = TypedTypeParameterId 0
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq"
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "ForeignEq"
    localCapability =
      TypedClassDeclaration span1 localCapabilityName [parameter] []
    retainedCapability =
      TypedClassDeclaration span1 importedCapabilityName [parameter] []
    localImplId =
      TypedImplId providerPath localCapabilityName [TypedBoolType]
    retainedImplId =
      TypedImplId providerPath importedCapabilityName [TypedBoolType]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedCapabilityProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ForeignEq"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface localCapability]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement localCapability,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [])
        ]
        unitInfo
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner = binder facadePath [0] publishedName
    constraint = TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/RetainedCapabilityFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["ForeignEq"])]
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface retainedCapability]
            [TypedImplInterface retainedImplId]
        )
        [TypedLetStatement publishedOwner publishedName span1 publishedScheme trueExpr]
        unitInfo
    importedPublishedName =
      resolved
        (TypedImportedModule facadePath)
        TypedValueNamespace
        "published"
    instantiation = TypedInstantiation publishedOwner [] Nothing
    evidenceUse =
      TypedEvidenceUse
        ( Just
            ( TypedEvidenceParameterRef
                publishedOwner
                (TypedEvidenceParameterId 0)
            )
        )
        constraint
        retainedImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiation]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr entryInfo importedPublishedName)]
        entryInfo

retainedCapabilityWrongImplName :: TypedCoreName
retainedCapabilityWrongImplName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Visible"

retainedCapabilityWrongImplProgram :: TypedProgram
retainedCapabilityWrongImplProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = fixtureLibraryPath "RetainedCapabilityWrongImpl"
    entryPath = fixtureModulePath "review-retained-capability-wrong-impl"
    parameter = TypedTypeParameterId 0
    providerCapability =
      resolved TypedCurrentModule TypedCapabilityNamespace "Hidden"
    providerMethod =
      resolved TypedCurrentModule TypedValueNamespace "render"
    providerMethodOwner = binder providerPath [0, 0] providerMethod
    providerMethodScheme =
      fixtureScheme
        providerMethodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        (TypedClosureRecipe [TypedRepresentationParameterRecipe parameter] TypedManagedTextRecipe)
    providerClass =
      TypedClassDeclaration
        span1
        providerCapability
        [parameter]
        [TypedMethodSignature providerMethod span1 providerMethodScheme]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedCapabilityWrongImpl.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Hidden",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface providerClass] [])
        [TypedClassStatement providerClass]
        unitInfo
    importedCapability =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Hidden"
    visibleClass =
      TypedClassDeclaration
        span1
        retainedCapabilityWrongImplName
        [parameter]
        []
    wrongImplId =
      TypedImplId entryPath retainedCapabilityWrongImplName [TypedBoolType]
    localName =
      resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder entryPath [2] localName
    evidenceParameter = TypedEvidenceParameterId 0
    constraint =
      TypedCapabilityConstraint importedCapability Nothing TypedBoolType
    localScheme =
      fixtureScheme
        localOwner
        []
        [TypedEvidenceParameter evidenceParameter constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    evidenceUse =
      TypedEvidenceUse
        (Just (TypedEvidenceParameterRef localOwner evidenceParameter))
        constraint
        wrongImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation localOwner [] Nothing]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["render"])]
        []
        emptyInterface
        [ TypedClassStatement visibleClass,
          TypedImplStatement (TypedImplDeclaration span1 wrongImplId []),
          TypedLetStatement localOwner localName span1 localScheme trueExpr,
          expressionStatement 4 (fixtureVariableExpr entryInfo localName)
        ]
        entryInfo

missingPublishedImplId :: TypedImplId
missingPublishedImplId =
  TypedImplId
    (fixtureModulePath "review-missing-published-impl")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Comparable")
    [TypedBoolType]

missingPublishedImplProgram :: TypedProgram
missingPublishedImplProgram =
  singleModuleProgram fixture relativeSource exports statements interface unitInfo modulePath
  where
    fixture = "review-missing-published-impl"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Comparable"
    capability =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    exports = [TypedModuleExport TypedCapabilityNamespace "Comparable"]
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 missingPublishedImplId [])
      ]
    interface =
      TypedModuleInterface [] [] [TypedClassInterface capability] []

expandingRecursiveEqualityProgram :: TypedProgram
expandingRecursiveEqualityProgram =
  recursiveEqualityProgram "review-expanding-recursive-equality" False

recursiveEqualityCallableType :: TypedType
recursiveEqualityCallableType =
  TypedDataType
    (resolved TypedCurrentModule TypedTypeNamespace "Nest")
    [TypedBoolType]

recursiveEqualityCallableFieldProgram :: TypedProgram
recursiveEqualityCallableFieldProgram =
  recursiveEqualityProgram "review-recursive-equality-callable-field" True

recursiveEqualityProgram :: Text -> Bool -> TypedProgram
recursiveEqualityProgram fixture includeCallableField =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nest"
    recursiveConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Nest"
    callableConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Callable"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    recursiveArgument = TypedListType parameterType
    recursiveField = TypedDataType dataName [recursiveArgument]
    recursiveConstructor =
      TypedConstructorDeclaration
        (binder modulePath [0, 0] recursiveConstructorName)
        recursiveConstructorName
        [recursiveField]
        [TypedManagedVariantRecipe dataName [recursiveArgument]]
    callableConstructor =
      TypedConstructorDeclaration
        (binder modulePath [0, 1] callableConstructorName)
        callableConstructorName
        [boolToBoolType]
        [boolToBoolRecipe]
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        ( recursiveConstructor
            : [callableConstructor | includeCallableField]
        )
    valueName = resolved TypedCurrentModule TypedValueNamespace "equality"
    valueOwner = binder modulePath [1] valueName
    targetType = TypedDataType dataName [TypedBoolType]
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

orphanSignatureName :: TypedCoreName
orphanSignatureName =
  resolved TypedCurrentModule TypedValueNamespace "orphan"

orphanSignatureProgram :: TypedProgram
orphanSignatureProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-orphan-signature"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] orphanSignatureName
    statements =
      [TypedSignatureStatement owner orphanSignatureName span1 (monoScheme owner)]

recursiveEqualityNestedCallableType :: TypedType
recursiveEqualityNestedCallableType =
  TypedDataType
    (resolved TypedCurrentModule TypedTypeNamespace "Nest")
    [TypedBoolType]

recursiveEqualityNestedCallableProgram :: TypedProgram
recursiveEqualityNestedCallableProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-recursive-equality-nested-callable"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nest"
    stepName = resolved TypedCurrentModule TypedConstructorNamespace "Step"
    baseName = resolved TypedCurrentModule TypedConstructorNamespace "Base"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    callableParameter = TypedFunctionType parameterType parameterType
    recursiveField = TypedDataType dataName [callableParameter]
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] stepName)
            stepName
            [recursiveField]
            [TypedManagedVariantRecipe dataName [callableParameter]],
          TypedConstructorDeclaration
            (binder modulePath [0, 1] baseName)
            baseName
            [parameterType]
            [TypedRepresentationParameterRecipe parameter]
        ]
    valueName = resolved TypedCurrentModule TypedValueNamespace "equality"
    valueOwner = binder modulePath [1] valueName
    scheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint recursiveEqualityNestedCallableType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

importedCurrentOriginName :: TypedCoreName
importedCurrentOriginName =
  resolved
    (TypedImportedModule (fixtureModulePath "review-imported-current-origin"))
    TypedValueNamespace
    "item"

importedCurrentOriginProgram :: TypedProgram
importedCurrentOriginProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-imported-current-origin"
    modulePath = (fixtureModulePath fixture)
    localName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = binder modulePath [0] localName
    statements =
      [ TypedLetStatement owner localName span1 (monoScheme owner) trueExpr,
        expressionStatement
          2
          (fixtureBoundVariableExpr owner boolInfo importedCurrentOriginName)
      ]

retainedCapabilityExportProgram :: TypedProgram
retainedCapabilityExportProgram =
  case retainedCapabilityEvidenceProgram of
    TypedProgram prelude modules entryPath ->
      TypedProgram prelude (map addCapabilityExport modules) entryPath
  where
    facadePath = (fixtureLibraryPath "RetainedCapabilityFacade")
    addCapabilityExport moduleValue@(TypedModule modulePath sourcePath imports exports interface statements moduleInfo)
      | modulePath == facadePath =
          TypedModule
            modulePath
            sourcePath
            imports
            (TypedModuleExport TypedCapabilityNamespace "ForeignEq" : exports)
            interface
            statements
            moduleInfo
      | otherwise = moduleValue

importAliasCollisionProgram :: TypedProgram
importAliasCollisionProgram =
  TypedProgram Nothing [leftModule, rightModule, entryModule] entryPath
  where
    leftPath = ["Alias", "Left"]
    rightPath = ["Alias", "Right"]
    entryPath = (fixtureModulePath "review-import-alias-collision")
    dependency path sourcePath =
      typedModule path (TypedSourcePath sourcePath) [] [] emptyInterface [] unitInfo
    leftModule = dependency leftPath "src/Alias/Left.jz"
    rightModule = dependency rightPath "src/Alias/Right.jz"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 leftPath (Just "Ops") Nothing,
          TypedResolvedImport span1 rightPath (Just "Ops") Nothing
        ]
        []
        emptyInterface
        []
        unitInfo

implBeforeClassCapabilityName :: TypedCoreName
implBeforeClassCapabilityName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Deferred"

implBeforeClassProgram :: TypedProgram
implBeforeClassProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-impl-before-class"
    modulePath = (fixtureModulePath fixture)
    implId =
      TypedImplId modulePath implBeforeClassCapabilityName [TypedBoolType]
    declaration =
      TypedClassDeclaration
        span1
        implBeforeClassCapabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        TypedClassStatement declaration
      ]

evidenceCapabilityWrongName :: TypedCoreName
evidenceCapabilityWrongName =
  resolved
    (TypedImportedModule ["Evidence", "Right"])
    TypedCapabilityNamespace
    "Shared"

evidenceCapabilityOriginProgram :: TypedProgram
evidenceCapabilityOriginProgram =
  TypedProgram Nothing [leftModule, rightModule, entryModule] entryPath
  where
    leftPath = ["Evidence", "Left"]
    rightPath = ["Evidence", "Right"]
    entryPath = (fixtureModulePath "review-evidence-capability-origin")
    provider modulePath sourcePath publishedIdentifier =
      typedModule
        modulePath
        (TypedSourcePath sourcePath)
        []
        [TypedModuleExport TypedValueNamespace publishedIdentifier]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 localImplId []),
          TypedLetStatement
            publishedOwner
            publishedName
            span1
            publishedScheme
            trueExpr
        ]
        unitInfo
      where
        capabilityName =
          resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
        constraint =
          TypedCapabilityConstraint capabilityName Nothing TypedBoolType
        classDeclaration =
          TypedClassDeclaration
            span1
            capabilityName
            [TypedTypeParameterId 0]
            []
        localImplId =
          TypedImplId modulePath capabilityName [TypedBoolType]
        publishedName =
          resolved TypedCurrentModule TypedValueNamespace publishedIdentifier
        publishedOwner = binder modulePath [2] publishedName
        publishedScheme =
          fixtureScheme
            publishedOwner
            []
            [ TypedEvidenceParameter
                (TypedEvidenceParameterId 0)
                constraint
            ]
            []
            TypedBoolType
            TypedBoolRecipe
    leftModule = provider leftPath "src/Evidence/Left.jz" "left"
    rightModule = provider rightPath "src/Evidence/Right.jz" "right"
    leftName =
      resolved (TypedImportedModule leftPath) TypedValueNamespace "left"
    leftCapabilityName =
      resolved (TypedImportedModule leftPath) TypedCapabilityNamespace "Shared"
    leftConstraint =
      TypedCapabilityConstraint leftCapabilityName Nothing TypedBoolType
    leftOwner = binder leftPath [2] (resolved TypedCurrentModule TypedValueNamespace "left")
    wrongImplId =
      TypedImplId rightPath evidenceCapabilityWrongName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        ( Just
            ( TypedEvidenceParameterRef
                leftOwner
                (TypedEvidenceParameterId 0)
            )
        )
        leftConstraint
        wrongImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation leftOwner [] Nothing]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 leftPath Nothing (Just ["left"]),
          TypedResolvedImport span1 rightPath Nothing (Just ["right"])
        ]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr entryInfo leftName)]
        entryInfo

malformedGeneratedNamesProgram :: TypedProgram
malformedGeneratedNamesProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-generated-names"
    modulePath = (fixtureModulePath fixture)
    invalidLambdaName =
      TypedGeneratedName (TypedLambdaPatternArgument 0)
    invalidLambda =
      TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath [0, 0] invalidLambdaName)
        invalidLambdaName
        trueExpr
    emptyOperatorName = TypedGeneratedName (TypedOperatorBinding "")
    malformedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "operator:%2B")
    unencodedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:garbage")
    invalidHexOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%GG")
    builtinOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%2B")
    reservedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E")
    emptyOperatorOwner = binder modulePath [1] emptyOperatorName
    malformedOperatorOwner = binder modulePath [2] malformedOperatorName
    unencodedOperatorOwner = binder modulePath [3] unencodedOperatorName
    invalidHexOperatorOwner = binder modulePath [4] invalidHexOperatorName
    builtinOperatorOwner = binder modulePath [5] builtinOperatorName
    reservedOperatorOwner = binder modulePath [6] reservedOperatorName
    statements =
      [ expressionStatement 1 invalidLambda,
        TypedLetStatement
          emptyOperatorOwner
          emptyOperatorName
          span1
          (monoScheme emptyOperatorOwner)
          trueExpr,
        TypedLetStatement
          malformedOperatorOwner
          malformedOperatorName
          span1
          (monoScheme malformedOperatorOwner)
          trueExpr,
        TypedLetStatement
          unencodedOperatorOwner
          unencodedOperatorName
          span1
          (monoScheme unencodedOperatorOwner)
          trueExpr,
        TypedLetStatement
          invalidHexOperatorOwner
          invalidHexOperatorName
          span1
          (monoScheme invalidHexOperatorOwner)
          trueExpr,
        TypedLetStatement
          builtinOperatorOwner
          builtinOperatorName
          span1
          (monoScheme builtinOperatorOwner)
          trueExpr,
        TypedLetStatement
          reservedOperatorOwner
          reservedOperatorName
          span1
          (monoScheme reservedOperatorOwner)
          trueExpr
      ]

regularPreludeModuleProgram :: TypedProgram
regularPreludeModuleProgram =
  TypedProgram
    Nothing
    [ typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    ["Prelude"]

retainedClassMethodExportProgram :: TypedProgram
retainedClassMethodExportProgram =
  TypedProgram Nothing [providerModule, facadeModule] facadePath
  where
    providerPath = (fixtureLibraryPath "RetainedMethodProvider")
    facadePath = (fixtureLibraryPath "RetainedMethodFacade")
    parameter = TypedTypeParameterId 0
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder providerPath [0, 0] localMethodName
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        ( TypedClosureRecipe
            [TypedRepresentationParameterRecipe parameter]
            TypedManagedTextRecipe
        )
    localClass =
      TypedClassDeclaration
        span1
        localClassName
        [parameter]
        [TypedMethodSignature localMethodName span1 methodScheme]
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner = binder providerPath [1] publishedName
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint localClassName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedMethodProvider.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface localClass]
            []
        )
        [ TypedClassStatement localClass,
          TypedLetStatement publishedOwner publishedName span1 publishedScheme trueExpr
        ]
        unitInfo
    retainedClassName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Display"
    retainedMethodName =
      resolved
        (TypedImportedModule providerPath)
        TypedValueNamespace
        "display"
    retainedClass =
      TypedClassDeclaration
        span1
        retainedClassName
        [parameter]
        [TypedMethodSignature retainedMethodName span1 methodScheme]
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/RetainedMethodFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["published"])]
        [TypedModuleExport TypedValueNamespace "display"]
        (TypedModuleInterface [] [] [TypedClassInterface retainedClass] [])
        []
        unitInfo

malformedWhitespaceName :: TypedCoreName
malformedWhitespaceName =
  resolved TypedCurrentModule TypedValueNamespace "bad name"

malformedReservedName :: TypedCoreName
malformedReservedName =
  resolved TypedCurrentModule TypedValueNamespace "if"

malformedQualifiedName :: TypedCoreName
malformedQualifiedName =
  resolved TypedCurrentModule TypedValueNamespace "Other::render"

malformedResolvedIdentifiersProgram :: TypedProgram
malformedResolvedIdentifiersProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-resolved-identifiers"
    modulePath = (fixtureModulePath fixture)
    binding statementIndex name =
      let owner = binder modulePath [statementIndex] name
       in TypedLetStatement owner name span1 (monoScheme owner) trueExpr
    statements =
      [ binding 0 malformedWhitespaceName,
        binding 1 malformedReservedName,
        binding 2 malformedQualifiedName
      ]

normalizedPreludeAmbientImpl :: TypedImplId
normalizedPreludeAmbientImpl =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedBoolType]

normalizedPreludeImplDuplicatesProgram :: TypedProgram
normalizedPreludeImplDuplicatesProgram =
  TypedProgram (Just preludeModule) [] ["Prelude"]
  where
    preludePath = ["Prelude"]
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    localClass =
      TypedClassDeclaration
        span1
        localCapabilityName
        [TypedTypeParameterId 0]
        []
    localImpl =
      TypedImplId preludePath localCapabilityName [TypedBoolType]
    preludeModule =
      typedModule
        preludePath
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Equal"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface localClass]
            [ TypedImplInterface localImpl,
              TypedImplInterface normalizedPreludeAmbientImpl
            ]
        )
        [ TypedClassStatement localClass,
          TypedImplStatement (TypedImplDeclaration span1 localImpl []),
          TypedImplStatement
            (TypedImplDeclaration span1 normalizedPreludeAmbientImpl [])
        ]
        unitInfo

malformedImportAliasProgram :: TypedProgram
malformedImportAliasProgram =
  TypedProgram Nothing [dependencyModule, entryModule] entryPath
  where
    dependencyPath = ["Alias", "Dependency"]
    entryPath = (fixtureModulePath "review-malformed-import-alias")
    dependencyModule =
      typedModule
        dependencyPath
        (TypedSourcePath "src/Alias/Dependency.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 dependencyPath (Just "True") Nothing]
        []
        emptyInterface
        []
        unitInfo

duplicateModuleExportsProgram :: TypedProgram
duplicateModuleExportsProgram =
  singleModuleProgram
    fixture
    relativeSource
    [ duplicateExport,
      duplicateExport
    ]
    [TypedLetStatement owner name span1 scheme trueExpr]
    (TypedModuleInterface [TypedValueInterface name scheme] [] [] [])
    unitInfo
    modulePath
  where
    fixture = "review-duplicate-module-exports"
    modulePath = (fixtureModulePath fixture)
    name = resolved TypedCurrentModule TypedValueNamespace "answer"
    owner = binder modulePath [0] name
    scheme = monoScheme owner
    duplicateExport = TypedModuleExport TypedValueNamespace "answer"

invalidSpan :: TypedSpan
invalidSpan = TypedSpan 0 (-1)

invalidImportSpanProgram :: TypedProgram
invalidImportSpanProgram =
  TypedProgram Nothing [dependencyModule, entryModule] entryPath
  where
    dependencyPath = ["Span", "Dependency"]
    entryPath = (fixtureModulePath "review-invalid-import-span")
    dependencyModule =
      typedModule
        dependencyPath
        (TypedSourcePath "src/Span/Dependency.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport invalidSpan dependencyPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo

invalidStatementSpansProgram :: TypedProgram
invalidStatementSpansProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-invalid-statement-spans"
    modulePath = (fixtureModulePath fixture)
    name = resolved TypedCurrentModule TypedValueNamespace "answer"
    signatureOwner = binder modulePath [0] name
    bindingOwner = binder modulePath [1] name
    signatureScheme = monoScheme signatureOwner
    bindingScheme = monoScheme bindingOwner
    statements =
      [ TypedSignatureStatement signatureOwner name invalidSpan signatureScheme,
        TypedLetStatement bindingOwner name invalidSpan bindingScheme trueExpr,
        TypedExpressionStatement invalidSpan trueExpr
      ]

invalidDeclarationSpansProgram :: TypedProgram
invalidDeclarationSpansProgram =
  TypedProgram (Just invalidPrelude) [] ["Prelude"]
  where
    invalidPrelude =
      case fixturePrelude of
        TypedModule modulePath sourcePath imports exports interface statements moduleInfo ->
          TypedModule
            modulePath
            sourcePath
            imports
            exports
            (invalidateInterface interface)
            (map invalidateStatement statements)
            moduleInfo
    invalidateInterface (TypedModuleInterface values datas classes impls) =
      TypedModuleInterface
        values
        datas
        [TypedClassInterface (invalidateClass declaration) | TypedClassInterface declaration <- classes]
        impls
    invalidateStatement statement =
      case statement of
        TypedClassStatement declaration ->
          TypedClassStatement (invalidateClass declaration)
        TypedImplStatement declaration ->
          TypedImplStatement (invalidateImpl declaration)
        other -> other
    invalidateClass (TypedClassDeclaration _ name parameters methods) =
      TypedClassDeclaration
        invalidSpan
        name
        parameters
        [TypedMethodSignature methodName invalidSpan scheme | TypedMethodSignature methodName _ scheme <- methods]
    invalidateImpl (TypedImplDeclaration _ implId methods) =
      TypedImplDeclaration
        invalidSpan
        implId
        [ TypedMethodDefinition methodId owner name invalidSpan expression
        | TypedMethodDefinition methodId owner name _ expression <- methods
        ]

invalidExpressionSpansProgram :: TypedProgram
invalidExpressionSpansProgram =
  instantiationProgram "review-invalid-expression-spans" (Just invalidSpan)

resolvedModuleOrderImporterPath :: [Text]
resolvedModuleOrderImporterPath =
  (fixtureModulePath "review-resolved-module-order")

resolvedModuleOrderProgram :: TypedProgram
resolvedModuleOrderProgram =
  TypedProgram
    Nothing
    [ typedModule
        resolvedModuleOrderImporterPath
        relativeSource
        [TypedResolvedImport span1 dependencyPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo,
      typedModule
        dependencyPath
        (TypedSourcePath "src/Dependency/Library.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    resolvedModuleOrderImporterPath
  where
    dependencyPath = ["Dependency", "Library"]

emptyResolvedIdentifierName :: TypedCoreName
emptyResolvedIdentifierName =
  resolved TypedCurrentModule TypedValueNamespace ""

emptyResolvedIdentifierProgram :: TypedProgram
emptyResolvedIdentifierProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-resolved-identifier"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] emptyResolvedIdentifierName
    statements =
      [ TypedLetStatement owner emptyResolvedIdentifierName span1 (monoScheme owner) trueExpr,
        expressionStatement 1 (fixtureBoundVariableExpr owner boolInfo emptyResolvedIdentifierName)
      ]

explicitSpanOnVariableOwner :: TypedBinderId
explicitSpanOnVariableOwner =
  binder
    (fixtureModulePath "review-explicit-span-on-variable")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "identity")

explicitSpanOnVariableProgram :: TypedProgram
explicitSpanOnVariableProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface instantiatedInfo modulePath
  where
    fixture = "review-explicit-span-on-variable"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    scheme =
      fixtureScheme
        explicitSpanOnVariableOwner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation =
      TypedInstantiation
        explicitSpanOnVariableOwner
        [TypedTypeArgument parameter TypedBoolType]
        (Just span1)
    instantiatedInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiation]
        []
    statements =
      [ TypedLetStatement
          explicitSpanOnVariableOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 (fixtureVariableExpr instantiatedInfo valueName)
      ]

singleEvidenceCandidateProgram :: TypedProgram
singleEvidenceCandidateProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-single-evidence-candidate"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    implId =
      TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    candidate =
      TypedEvidenceCandidate implId Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (TypedBooleanLiteral True)

emptyModulePathProgram :: TypedProgram
emptyModulePathProgram =
  TypedProgram
    Nothing
    [ typedModule
        []
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    []

wrongPreludeSlotProgram :: TypedProgram
wrongPreludeSlotProgram =
  TypedProgram
    ( Just
        ( typedModule
            (fixtureLibraryPath "WrongPrelude")
            (TypedSourcePath "src/Library/WrongPrelude.jz")
            []
            []
            emptyInterface
            []
            unitInfo
        )
    )
    [ typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    entryPath
  where
    entryPath = (fixtureModulePath "review-wrong-prelude-slot")

signatureBindingMismatchProgram :: TypedProgram
signatureBindingMismatchProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-signature-binding-mismatch"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "annotated"
    signatureOwner = binder modulePath [0] valueName
    bindingOwner = binder modulePath [1] valueName
    signatureScheme = monoScheme signatureOwner
    bindingScheme =
      fixtureScheme bindingOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedSignatureStatement signatureOwner valueName span1 signatureScheme,
        TypedLetStatement
          bindingOwner
          valueName
          span1
          bindingScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "value"))
      ]

signatureBindingShapeMismatchProgram :: TypedProgram
signatureBindingShapeMismatchProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-signature-binding-shape-mismatch"
    modulePath = fixtureModulePath fixture
    valueName = resolved TypedCurrentModule TypedValueNamespace "annotated"
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    signatureOwner = binder modulePath [0] valueName
    bindingOwner = shapeBindingOwner
    argumentOwner = binder modulePath [1, 0] argumentName
    signatureScheme =
      TypedScheme signatureOwner [] [] [] boolToBoolType boolToBoolRecipe (Just TypedDirectCallableShape)
    bindingScheme =
      TypedScheme bindingOwner [] [] [] boolToBoolType boolToBoolRecipe (Just TypedClosureCallableShape)
    statements =
      [ TypedSignatureStatement signatureOwner valueName span1 signatureScheme,
        TypedLetStatement
          bindingOwner
          valueName
          span1
          bindingScheme
          ( TypedLambdaExpr
              boolToBoolInfo
              argumentOwner
              argumentName
              (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
          )
      ]

shapeBindingOwner :: TypedBinderId
shapeBindingOwner =
  binder
    (fixtureModulePath "review-signature-binding-shape-mismatch")
    [1]
    (resolved TypedCurrentModule TypedValueNamespace "annotated")

qualifiedMethodTypeApplicationProgram :: TypedProgram
qualifiedMethodTypeApplicationProgram =
  TypedProgram (Just preludeModule) [entryModule] entryPath
  where
    preludePath = ["Prelude"]
    entryPath = (fixtureModulePath "review-qualified-method-type-application")
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Printable"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "print!"
    methodOwner = binder preludePath [0, 0] methodName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId preludePath capabilityName [TypedBoolType]
    methodArgument =
      resolved TypedCurrentModule TypedValueNamespace "printArgument"
    methodExpression =
      TypedLambdaExpr
        boolToBoolInfo
        (binder preludePath [1, 0, 0] methodArgument)
        methodArgument
        trueExpr
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "print!")
        (binder preludePath [1, 0] methodName)
        methodName
        span1
        methodExpression
    preludeModule =
      typedModule
        preludePath
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Printable"]
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
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Printable"
    importedImplId =
      TypedImplId preludePath importedCapabilityName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint (preludeCapability "Printable") (Just "Printable::print!") TypedBoolType)
        importedImplId
        (Just (TypedMethodId importedImplId "print!"))
    methodInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        []
        [TypedSelectedEvidence evidenceUse]
    expression =
      TypedTypeApplicationExpr
        methodInfo
        (fixtureVariableExpr methodInfo (TypedBuiltinName "Printable::print!"))
        span1
        TypedBoolType
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 expression]
        methodInfo

qualifiedMethodValueContractProgram :: TypedProgram
qualifiedMethodValueContractProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-qualified-method-value-contract"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId =
      TypedImplId ["Prelude"] capabilityName [TypedTextType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint (preludeCapability "Render") (Just "Render::map") TypedTextType)
        implId
        (Just (TypedMethodId implId "map"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName "Render::map")

aliasShapedSelfRecursionProgram :: TypedProgram
aliasShapedSelfRecursionProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-alias-shaped-self-recursion"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = binder modulePath [0] valueName
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm
            (TypedWildcardPattern boolInfo)
            Nothing
            (fixtureBoundVariableExpr owner boolInfo valueName)
        ]
    statement =
      TypedLetStatement owner valueName span1 (monoScheme owner) expression

eagerSelfReferenceName :: TypedCoreName
eagerSelfReferenceName =
  resolved TypedCurrentModule TypedValueNamespace "item"

eagerSelfReferenceProgram :: TypedProgram
eagerSelfReferenceProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-eager-self-reference"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] eagerSelfReferenceName
    expression =
      TypedIfExpr
        boolInfo
        (fixtureVariableExpr boolInfo eagerSelfReferenceName)
        trueExpr
        falseExpr
    statement =
      TypedLetStatement owner eagerSelfReferenceName span1 (monoScheme owner) expression

importNameCollisionProgram :: TypedProgram
importNameCollisionProgram =
  TypedProgram Nothing [firstLibrary, secondLibrary, entryModule] entryPath
  where
    fixture = "review-import-name-collision"
    firstPath = (fixtureLibraryPath "FirstCollision")
    secondPath = (fixtureLibraryPath "SecondCollision")
    entryPath = (fixtureModulePath fixture)
    collisionLibrary libraryPath constructorIdentifier =
      let dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
          constructorName = resolved TypedCurrentModule TypedConstructorNamespace constructorIdentifier
          constructorOwner = binder libraryPath [0, 0] constructorName
          declaration =
            TypedDataDeclaration
              span1
              dataName
              []
              [TypedConstructorDeclaration constructorOwner constructorName [] []]
          valueName = resolved TypedCurrentModule TypedValueNamespace "shared"
          valueOwner = binder libraryPath [1] valueName
          valueScheme = monoScheme valueOwner
       in typedModule
            libraryPath
            (TypedSourcePath ("src/" <> Text.intercalate "/" libraryPath <> ".jz"))
            []
            [ TypedModuleExport TypedValueNamespace "shared",
              TypedModuleExport TypedTypeNamespace "Box"
            ]
            ( TypedModuleInterface
                [TypedValueInterface valueName valueScheme]
                [TypedDataInterface declaration]
                []
                []
            )
            [ TypedDataStatement declaration,
              TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
            ]
            unitInfo
    firstLibrary = collisionLibrary firstPath "FirstBox"
    secondLibrary = collisionLibrary secondPath "SecondBox"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 firstPath Nothing Nothing,
          TypedResolvedImport span1 secondPath Nothing Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

localClassMethodName :: TypedCoreName
localClassMethodName =
  resolved TypedCurrentModule TypedValueNamespace "render"

localClassMethodVisibilityProgram :: TypedProgram
localClassMethodVisibilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-local-class-method-visibility"
    modulePath = (fixtureModulePath fixture)
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodOwner = binder modulePath [0, 0] localClassMethodName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [TypedTypeParameterId 0]
        [TypedMethodSignature localClassMethodName span1 methodScheme]
    statements =
      [ TypedClassStatement classDeclaration,
        expressionStatement 1 (fixtureVariableExpr boolToBoolInfo localClassMethodName)
      ]

syntheticBinderShadowingProgram :: TypedProgram
syntheticBinderShadowingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-synthetic-binder-shadowing"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    earlierOwner = binder modulePath [99] valueName
    laterOwner = binder modulePath [0] valueName
    earlierScheme = monoScheme earlierOwner
    laterScheme =
      fixtureScheme laterOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedLetStatement earlierOwner valueName span1 earlierScheme trueExpr,
        TypedLetStatement
          laterOwner
          valueName
          span1
          laterScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "later")),
        expressionStatement 1 (fixtureBoundVariableExpr laterOwner textInfo valueName)
      ]

implFreeClassParameterProgram :: TypedProgram
implFreeClassParameterProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] [statement] emptyInterface unitInfo modulePath)
  where
    fixture = "review-impl-free-class-parameter"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodOwner = binder modulePath [0, 0] methodName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    identityArgumentName =
      resolved TypedCurrentModule TypedValueNamespace "identityArgument"
    identityArgumentOwner =
      binder modulePath [0, 0, 0] identityArgumentName
    identityInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    identityExpression =
      TypedLambdaExpr
        identityInfo
        identityArgumentOwner
        identityArgumentName
        ( fixtureBoundVariableExpr
            identityArgumentOwner
            (info parameterType parameterRecipe)
            identityArgumentName
        )
    methodExpression =
      TypedBlockExpr
        boolInfo
        [ expressionStatement 1 identityExpression,
          expressionStatement 2 trueExpr
        ]
    method =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        methodOwner
        methodName
        span1
        methodExpression
    statement =
      TypedImplStatement
        ( TypedImplDeclaration
            span1
            implId
            [method, fixtureImplMethod modulePath [0, 1] implId "other"]
        )

duplicateQualifiedMethodCandidateImpl :: TypedImplId
duplicateQualifiedMethodCandidateImpl =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render")
    [TypedTextType]

duplicateQualifiedMethodCandidateProgram :: TypedProgram
duplicateQualifiedMethodCandidateProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-qualified-method-candidate"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.map") TypedTextType
    candidate =
      TypedEvidenceCandidate
        duplicateQualifiedMethodCandidateImpl
        (Just (TypedMethodId duplicateQualifiedMethodCandidateImpl "map"))
    expression =
      fixtureVariableExpr
        ( TypedNodeInfo
            builtinMapType
            builtinMapRecipe
            []
            [TypedEvidenceCandidates constraint [candidate, candidate]]
        )
        (TypedBuiltinName "map")

metadataOnlyImportedTypeName :: TypedCoreName
metadataOnlyImportedTypeName =
  resolved
    (TypedImportedModule (fixtureLibraryPath "MetadataProvider"))
    TypedTypeNamespace
    "Box"

metadataOnlySourceTypeProgram :: TypedProgram
metadataOnlySourceTypeProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    fixture = "review-metadata-only-source-type"
    providerPath = (fixtureLibraryPath "MetadataProvider")
    entryPath = (fixtureModulePath fixture)
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    localConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    constructorOwner = binder providerPath [0, 0] localConstructorName
    dataDeclaration =
      TypedDataDeclaration
        span1
        localDataName
        []
        [TypedConstructorDeclaration constructorOwner localConstructorName [] []]
    localDataType = TypedDataType localDataName []
    localDataRecipe = TypedManagedVariantRecipe localDataName []
    localDataInfo = info localDataType localDataRecipe
    providerValueName =
      resolved TypedCurrentModule TypedValueNamespace "make"
    providerValueOwner = binder providerPath [1] providerValueName
    providerValueScheme =
      fixtureScheme
        providerValueOwner
        []
        []
        []
        localDataType
        localDataRecipe
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/MetadataProvider.jz")
        []
        [TypedModuleExport TypedValueNamespace "make"]
        ( TypedModuleInterface
            [TypedValueInterface providerValueName providerValueScheme]
            [TypedDataInterface dataDeclaration]
            []
            []
        )
        [ TypedDataStatement dataDeclaration,
          TypedLetStatement
            providerValueOwner
            providerValueName
            span1
            providerValueScheme
            (fixtureBoundVariableExpr constructorOwner localDataInfo localConstructorName)
        ]
        unitInfo
    leakedValueName =
      resolved TypedCurrentModule TypedValueNamespace "leaked"
    leakedValueOwner = binder entryPath [0] leakedValueName
    leakedScheme =
      fixtureScheme
        leakedValueOwner
        []
        []
        []
        (TypedDataType metadataOnlyImportedTypeName [])
        (TypedManagedVariantRecipe metadataOnlyImportedTypeName [])
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["make"])]
        []
        emptyInterface
        [TypedSignatureStatement leakedValueOwner leakedValueName span1 leakedScheme]
        unitInfo

nonScalarCharacterProgram :: TypedProgram
nonScalarCharacterProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-scalar-character"
    modulePath = (fixtureModulePath fixture)
    charInfo = info TypedCharType TypedCharRecipe
    nonScalar = '\xD800'
    invalidLiteral =
      TypedLiteralExpr charInfo (TypedCharacterLiteral nonScalar)
    invalidPattern =
      TypedLiteralPattern charInfo (TypedCharacterLiteral nonScalar)
    patternExpression =
      TypedPatternCaseExpr
        boolInfo
        (TypedLiteralExpr charInfo (TypedCharacterLiteral 'x'))
        [TypedCaseArm invalidPattern Nothing trueExpr]
    statements =
      [ expressionStatement 1 invalidLiteral,
        expressionStatement 2 patternExpression
      ]

inferredMethodOnlyCapabilityVisibilityProgram :: TypedProgram
inferredMethodOnlyCapabilityVisibilityProgram =
  methodOnlyCapabilityVisibilityProgram False

explicitMethodOnlyCapabilityVisibilityProgram :: TypedProgram
explicitMethodOnlyCapabilityVisibilityProgram =
  methodOnlyCapabilityVisibilityProgram True

methodOnlyCapabilityVisibilityProgram :: Bool -> TypedProgram
methodOnlyCapabilityVisibilityProgram hasExplicitSignature =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    fixture = "review-method-only-capability-visibility"
    libraryPath = (fixtureLibraryPath "MethodOnlyCapability")
    entryPath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedCapabilityName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Render"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MethodOnlyCapability.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Render",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [TypedClassStatement classDeclaration]
        unitInfo
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    inferredOwner = binder entryPath [0] localName
    explicitOwner = binder entryPath [0] localName
    explicitBindingOwner = binder entryPath [1] localName
    localScheme owner =
      fixtureScheme
        owner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    localStatements
      | hasExplicitSignature =
          [ TypedSignatureStatement explicitOwner localName span1 (localScheme explicitOwner),
            TypedLetStatement explicitBindingOwner localName span1 (localScheme explicitBindingOwner) trueExpr
          ]
      | otherwise =
          [TypedLetStatement inferredOwner localName span1 (localScheme inferredOwner) trueExpr]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["render"])]
        []
        emptyInterface
        localStatements
        boolInfo

capabilityImportCollisionProgram :: TypedProgram
capabilityImportCollisionProgram =
  TypedProgram Nothing [valueModule, capabilityModule, entryModule] entryPath
  where
    fixture = "review-capability-import-collision"
    valuePath = (fixtureLibraryPath "SharedValue")
    capabilityPath = (fixtureLibraryPath "SharedCapability")
    entryPath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "Shared"
    valueOwner = binder valuePath [0] valueName
    valueScheme = monoScheme valueOwner
    valueModule =
      typedModule
        valuePath
        (TypedSourcePath "src/Library/SharedValue.jz")
        []
        [TypedModuleExport TypedValueNamespace "Shared"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        boolInfo
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    capabilityModule =
      typedModule
        capabilityPath
        (TypedSourcePath "src/Library/SharedCapability.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Shared"]
        (TypedModuleInterface [] [] [TypedClassInterface capabilityDeclaration] [])
        [TypedClassStatement capabilityDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 valuePath Nothing Nothing,
          TypedResolvedImport span1 capabilityPath Nothing Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

nestedTypeParameterShadowingProgram :: TypedProgram
nestedTypeParameterShadowingProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-type-parameter-shadowing"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerOwner = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentOwner = binder modulePath [0, 0] argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder modulePath [0, 0, 0] localName
    localScheme =
      fixtureScheme
        localOwner
        [parameter]
        []
        []
        parameterType
        parameterRecipe
    localBinding =
      TypedLetStatement
        localOwner
        localName
        span1
        localScheme
        (fixtureBoundVariableExpr argumentOwner parameterInfo argumentName)
    localUseInfo =
      TypedNodeInfo
        parameterType
        parameterRecipe
        [ TypedInstantiation
            localOwner
            [TypedTypeArgument parameter parameterType]
            Nothing
        ]
        []
    block =
      TypedBlockExpr
        parameterInfo
        [ localBinding,
          expressionStatement 2 (fixtureVariableExpr localUseInfo localName)
        ]
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe =
      TypedClosureRecipe [parameterRecipe] parameterRecipe
    outerExpression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        block
    outerScheme =
      fixtureScheme
        outerOwner
        [parameter]
        []
        []
        functionType
        functionRecipe
    topLevelBinding =
      TypedLetStatement
        outerOwner
        outerName
        span1
        outerScheme
        outerExpression

typeOnlyImportSelectorProgram :: TypedProgram
typeOnlyImportSelectorProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    fixture = "review-type-only-import-selector"
    libraryPath = (fixtureLibraryPath "TypeOnlySelector")
    entryPath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "BoxValue"
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
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TypeOnlySelector.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Box"]
        (TypedModuleInterface [] [TypedDataInterface dataDeclaration] [] [])
        [TypedDataStatement dataDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Box"])]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

ordinaryUnboundEvidenceProgram :: TypedProgram
ordinaryUnboundEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-ordinary-unbound-evidence"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse =
      TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression =
      TypedLiteralExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            []
            [TypedSelectedEvidence evidenceUse]
        )
        (TypedBooleanLiteral True)

nestedLocalGeneralizationProgram :: TypedProgram
nestedLocalGeneralizationProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-local-generalization"
    modulePath = (fixtureModulePath fixture)
    outerParameter = TypedTypeParameterId 0
    innerParameter = TypedTypeParameterId 1
    outerParameterType = TypedTypeParameterType outerParameter
    outerParameterRecipe = TypedRepresentationParameterRecipe outerParameter
    outerParameterInfo = info outerParameterType outerParameterRecipe
    innerParameterType = TypedTypeParameterType innerParameter
    innerParameterRecipe = TypedRepresentationParameterRecipe innerParameter
    innerParameterInfo = info innerParameterType innerParameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerOwner = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentOwner = binder modulePath [0, 0] argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder modulePath [0, 0, 0] localName
    localArgumentName =
      resolved TypedCurrentModule TypedValueNamespace "localArgument"
    localArgumentOwner = binder modulePath [0, 0, 0, 0] localArgumentName
    localFunctionType =
      TypedFunctionType innerParameterType innerParameterType
    localFunctionRecipe =
      TypedClosureRecipe [innerParameterRecipe] innerParameterRecipe
    localScheme =
      fixtureScheme
        localOwner
        [innerParameter]
        []
        []
        localFunctionType
        localFunctionRecipe
    localExpression =
      TypedLambdaExpr
        (info localFunctionType localFunctionRecipe)
        localArgumentOwner
        localArgumentName
        (fixtureBoundVariableExpr localArgumentOwner innerParameterInfo localArgumentName)
    localBinding =
      TypedLetStatement
        localOwner
        localName
        span1
        localScheme
        localExpression
    instantiatedLocalType =
      TypedFunctionType outerParameterType outerParameterType
    instantiatedLocalRecipe =
      TypedClosureRecipe [outerParameterRecipe] outerParameterRecipe
    localUseInfo =
      TypedNodeInfo
        instantiatedLocalType
        instantiatedLocalRecipe
        [ TypedInstantiation
            localOwner
            [TypedTypeArgument innerParameter outerParameterType]
            Nothing
        ]
        []
    localUse = fixtureVariableExpr localUseInfo localName
    localApplication =
      TypedApplyExpr
        outerParameterInfo
        localUse
        (fixtureBoundVariableExpr argumentOwner outerParameterInfo argumentName)
    block =
      TypedBlockExpr
        outerParameterInfo
        [ localBinding,
          expressionStatement 2 localApplication
        ]
    functionType =
      TypedFunctionType outerParameterType outerParameterType
    functionRecipe =
      TypedClosureRecipe [outerParameterRecipe] outerParameterRecipe
    outerExpression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        block
    outerScheme =
      fixtureScheme
        outerOwner
        [outerParameter]
        []
        []
        functionType
        functionRecipe
    topLevelBinding =
      TypedLetStatement
        outerOwner
        outerName
        span1
        outerScheme
        outerExpression

nonConcreteImplTargetId :: TypedImplId
nonConcreteImplTargetId =
  TypedImplId
    (fixtureModulePath "review-non-concrete-impl-target")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Concrete")
    [TypedFunctionType TypedBoolType TypedBoolType]

nonConcreteImplTargetProgram :: TypedProgram
nonConcreteImplTargetProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-concrete-impl-target"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Concrete"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedClassStatement capabilityDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 nonConcreteImplTargetId []),
        expressionStatement 3 trueExpr
      ]

blockDeclarationScopeProgram :: TypedProgram
blockDeclarationScopeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-block-declaration-scope"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nested"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Nested"
    dataDeclaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "NestedClass"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    block =
      TypedBlockExpr
        boolInfo
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement capabilityDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 implId []),
          expressionStatement 4 trueExpr
        ]

delimiterModulePathProgram :: TypedProgram
delimiterModulePathProgram =
  modulePathFixtureProgram ["A::B"]

slashModulePathProgram :: TypedProgram
slashModulePathProgram =
  modulePathFixtureProgram ["App/Main"]

reservedModulePathProgram :: TypedProgram
reservedModulePathProgram =
  modulePathFixtureProgram ["if"]

modulePathFixtureProgram :: [Text] -> TypedProgram
modulePathFixtureProgram modulePath =
  TypedProgram
    Nothing
    [ typedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo
    ]
    modulePath

moduleMetadataIdentityOwner :: TypedBinderId
moduleMetadataIdentityOwner =
  binder
    (fixtureModulePath "review-module-metadata-identity")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

moduleMetadataIdentityProgram :: TypedProgram
moduleMetadataIdentityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface moduleInfo modulePath
  where
    fixture = "review-module-metadata-identity"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = moduleMetadataIdentityOwner
    scheme = monoScheme owner
    moduleInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation owner [] Nothing]
        []
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 2 trueExpr
      ]

qualifiedTypeApplicationInstantiationOwner :: TypedBinderId
qualifiedTypeApplicationInstantiationOwner =
  binder
    (fixtureModulePath "review-qualified-type-application-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "ordinary")

qualifiedTypeApplicationInstantiationProgram :: TypedProgram
qualifiedTypeApplicationInstantiationProgram =
  case qualifiedMethodTypeApplicationProgram of
    TypedProgram
      prelude
      [TypedModule _ sourcePath imports exports interface [TypedExpressionStatement expressionSpan originalExpression] _]
      _ ->
        case originalExpression of
          TypedTypeApplicationExpr (TypedNodeInfo resultType resultRecipe [] evidence) function explicitSpan typeArgument ->
            let applicationInfo =
                  TypedNodeInfo
                    resultType
                    resultRecipe
                    [TypedInstantiation qualifiedTypeApplicationInstantiationOwner [] Nothing]
                    evidence
                expression =
                  TypedTypeApplicationExpr applicationInfo function explicitSpan typeArgument
                ordinaryName =
                  resolved TypedCurrentModule TypedValueNamespace "ordinary"
                ordinaryScheme =
                  monoScheme qualifiedTypeApplicationInstantiationOwner
                entryPath =
                  (fixtureModulePath "review-qualified-type-application-instantiation")
                entryModule =
                  TypedModule
                    entryPath
                    sourcePath
                    imports
                    exports
                    interface
                    [ TypedLetStatement
                        qualifiedTypeApplicationInstantiationOwner
                        ordinaryName
                        span1
                        ordinaryScheme
                        trueExpr,
                      TypedExpressionStatement expressionSpan expression
                    ]
                    applicationInfo
             in TypedProgram prelude [entryModule] entryPath
          _ -> error "qualified method type-application fixture changed shape"
    _ -> error "qualified method type-application program changed shape"

localClassMethodAfterValueProgram :: TypedProgram
localClassMethodAfterValueProgram =
  localClassMethodSchemeProgram "review-local-class-method-after-value" False

localClassMethodBeforeValueProgram :: TypedProgram
localClassMethodBeforeValueProgram =
  localClassMethodSchemeProgram "review-local-class-method-before-value" True

localClassMethodSchemeProgram :: Text -> Bool -> TypedProgram
localClassMethodSchemeProgram fixture classFirst =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "shared"
    valueOwner = binder modulePath [0] valueName
    valueStatement =
      TypedLetStatement valueOwner valueName span1 (monoScheme valueOwner) trueExpr
    className =
      resolved TypedCurrentModule TypedCapabilityNamespace "SharedClass"
    methodOwner = binder modulePath [1, 0] valueName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classStatement =
      TypedClassStatement
        ( TypedClassDeclaration
            span1
            className
            [TypedTypeParameterId 0]
            [TypedMethodSignature valueName span1 methodScheme]
        )
    declarations
      | classFirst = [classStatement, valueStatement]
      | otherwise = [valueStatement, classStatement]
    statements =
      declarations
        <> [expressionStatement 3 (fixtureBoundVariableExpr valueOwner boolInfo valueName)]

lexicalSchemeShadowingProgram :: TypedProgram
lexicalSchemeShadowingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-lexical-scheme-shadowing"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    outerOwner = binder modulePath [0] valueName
    innerOwner = binder modulePath [1, 0] valueName
    innerUse = fixtureBoundVariableExpr innerOwner textInfo valueName
    block =
      TypedBlockExpr
        textInfo
        [ TypedLetStatement
            innerOwner
            valueName
            span1
            (fixtureScheme innerOwner [] [] [] TypedTextType TypedManagedTextRecipe)
            (TypedLiteralExpr textInfo (TypedTextLiteral "inner")),
          expressionStatement 2 innerUse
        ]
    statements =
      [ TypedLetStatement outerOwner valueName span1 (monoScheme outerOwner) trueExpr,
        expressionStatement 1 block
      ]

fullyAppliedMethodCandidatesProgram :: TypedProgram
fullyAppliedMethodCandidatesProgram =
  qualifiedMapDispatchProgram
    fixture
    []
    [ TypedEvidenceCandidates
        fixtureRenderConstraint
        [ fixtureRenderCandidate (fixtureRenderImpl ["Prelude"]),
          fixtureRenderCandidate (fixtureRenderImpl (fixtureModulePath fixture))
        ]
    ]
  where
    fixture = "review-fully-applied-method-candidates"

fixtureRenderImpl :: [Text] -> TypedImplId
fixtureRenderImpl modulePath =
  TypedImplId
    modulePath
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render")
    [TypedTextType]

fixtureRenderConstraint :: TypedCapabilityConstraint
fixtureRenderConstraint =
  TypedCapabilityConstraint
    (preludeCapability "Render")
    (Just "Render.map")
    TypedTextType

fixtureRenderCandidate :: TypedImplId -> TypedEvidenceCandidate
fixtureRenderCandidate implId =
  TypedEvidenceCandidate implId (Just (TypedMethodId implId "map"))

qualifiedMapDispatchProgram :: Text -> [TypedEvidenceSelection] -> [TypedEvidenceSelection] -> TypedProgram
qualifiedMapDispatchProgram fixture intermediateEvidence resultEvidence =
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
        resultInfo
        modulePath
    )
  where
    modulePath = fixtureModulePath fixture
    secondImpl = fixtureRenderImpl modulePath
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
        (TypedNodeInfo intermediateType intermediateRecipe [] intermediateEvidence)
        (fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map"))
        mapper
    argument =
      TypedListExpr
        (info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe))
        [trueExpr]
    resultInfo =
      TypedNodeInfo
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
        []
        resultEvidence
    expression = TypedApplyExpr resultInfo intermediate argument

duplicateUnboundEvidenceProgram :: TypedProgram
duplicateUnboundEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-unbound-evidence"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    use =
      TypedEvidenceUse
        Nothing
        constraint
        implId
        (Just (TypedMethodId implId "equal"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (TypedBuiltinName "Equal::equal")

generalizedClassMethodImportProgram :: TypedProgram
generalizedClassMethodImportProgram =
  generalizedClassMethodImportProgramWith
    "review-generalized-class-method-import"
    True

missingImportedClassMethodDispatchProgram :: TypedProgram
missingImportedClassMethodDispatchProgram =
  generalizedClassMethodImportProgramWith
    "review-missing-imported-class-method-dispatch"
    False

generalizedClassMethodImportProgramWith :: Text -> Bool -> TypedProgram
generalizedClassMethodImportProgramWith fixture includeEvidence =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "GeneralizedClassMethod")
    entryPath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    methodName = resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder libraryPath [0, 0] methodName
    methodType =
      TypedFunctionType
        (TypedTypeParameterType parameter)
        (TypedTypeParameterType parameter)
    methodRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        (TypedRepresentationParameterRecipe parameter)
    methodScheme =
      fixtureScheme methodOwner [] [] [] methodType methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath className [TypedBoolType]
    methodDefinition =
      fixtureImplMethod libraryPath [1, 0] localImplId "display"
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/GeneralizedClassMethod.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Display",
          TypedModuleExport TypedValueNamespace "display"
        ]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 localImplId [methodDefinition])
        ]
        unitInfo
    importedMethodName =
      resolved (TypedImportedModule libraryPath) TypedValueNamespace "display"
    importedCapabilityName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Display"
    importedImplId =
      TypedImplId libraryPath importedCapabilityName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint
        importedCapabilityName
        (Just (Text.intercalate "::" (libraryPath <> ["Display", "display"])))
        TypedBoolType
    selectedEvidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            ( Just
                ( TypedEvidenceParameterRef
                    methodOwner
                    (TypedEvidenceParameterId 0)
                )
            )
            constraint
            importedImplId
            (Just (TypedMethodId importedImplId "display"))
        )
    evidence
      | includeEvidence = [selectedEvidence]
      | otherwise = []
    instantiatedInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [ TypedInstantiation
            methodOwner
            [TypedTypeArgument parameter TypedBoolType]
            Nothing
        ]
        evidence
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["display"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr instantiatedInfo importedMethodName)]
        instantiatedInfo

importedClassCollisionProgram :: TypedProgram
importedClassCollisionProgram =
  TypedProgram Nothing [firstLibrary, secondLibrary, entryModule] entryPath
  where
    firstPath = (fixtureLibraryPath "FirstClash")
    secondPath = (fixtureLibraryPath "SecondClash")
    entryPath = (fixtureModulePath "review-imported-class-collision")
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
    constraint =
      TypedCapabilityConstraint
        (resolved (TypedImportedModule firstPath) TypedCapabilityNamespace "Clash")
        Nothing
        TypedBoolType
    scheme =
      fixtureScheme
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
        [ TypedLetStatement valueOwner valueName span1 scheme trueExpr,
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
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0, 1] forwardBlockReferenceName
    block =
      TypedBlockExpr
        boolInfo
        [ expressionStatement 2 (fixtureVariableExpr boolInfo forwardBlockReferenceName),
          TypedLetStatement owner forwardBlockReferenceName span1 (monoScheme owner) trueExpr,
          expressionStatement 3 trueExpr
        ]

recursiveBlockPeerProgram :: TypedProgram
recursiveBlockPeerProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-recursive-block-peers"
    modulePath = (fixtureModulePath fixture)
    leftName = resolved TypedCurrentModule TypedValueNamespace "left"
    rightName = resolved TypedCurrentModule TypedValueNamespace "right"
    leftOwner = binder modulePath [0, 0] leftName
    rightOwner = binder modulePath [0, 1] rightName
    recursiveLambda ownerPath argumentName peerOwner peerName =
      let argumentOwner = binder modulePath ownerPath argumentName
       in TypedLambdaExpr
        boolToBoolInfo
        argumentOwner
        argumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureBoundVariableExpr peerOwner boolToBoolInfo peerName)
            (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
        )
    leftArgument = resolved TypedCurrentModule TypedValueNamespace "leftArgument"
    rightArgument = resolved TypedCurrentModule TypedValueNamespace "rightArgument"
    leftStatement =
      TypedLetStatement
        leftOwner
        leftName
        span1
        (fixtureScheme leftOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 0, 0] leftArgument rightOwner rightName)
    rightStatement =
      TypedLetStatement
        rightOwner
        rightName
        span1
        (fixtureScheme rightOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 1, 0] rightArgument leftOwner leftName)
    block =
      TypedBlockExpr
        boolToBoolInfo
        [ leftStatement,
          rightStatement,
          expressionStatement 3 (fixtureBoundVariableExpr leftOwner boolToBoolInfo leftName)
        ]

malformedLiteralConstraintBoundsProgram :: TypedProgram
malformedLiteralConstraintBoundsProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-literal-constraint-bounds"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    signature statementIndex suffix lower upper =
      let valueName = resolved TypedCurrentModule TypedValueNamespace suffix
          owner = binder modulePath [statementIndex] valueName
          scheme =
            fixtureScheme
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
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    owner = binder modulePath [0] valueName
    firstParameter = TypedEvidenceParameterId 0
    secondParameter = TypedEvidenceParameterId 1
    firstConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    secondConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType
    scheme =
      fixtureScheme
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
      fixtureVariableExpr
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
    libraryPath = (fixtureLibraryPath "PrivateCapabilityMetadata")
    entryPath = (fixtureModulePath "review-private-capability-metadata-visibility")
    parameter = TypedTypeParameterId 0
    libraryCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    libraryClass =
      TypedClassDeclaration span1 libraryCapabilityName [parameter] []
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder libraryPath [1] valueName
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint libraryCapabilityName Nothing TypedBoolType)
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
  importedModuleQualifiedMethodKeyProgram
    "review-module-qualified-method-key"
    "Lib::Api::Make::make"

forgedModuleQualifiedMethodKeyProgram :: TypedProgram
forgedModuleQualifiedMethodKeyProgram =
  importedModuleQualifiedMethodKeyProgram
    "review-forged-module-qualified-method-key"
    "Other::Make::make"

importedModuleQualifiedMethodKeyProgram :: Text -> Text -> TypedProgram
importedModuleQualifiedMethodKeyProgram fixture qualifiedMethod =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = ["Lib", "Api"]
    entryPath = fixtureModulePath fixture
    capabilityIdentifier = "Make"
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace capabilityIdentifier
    methodName = resolved TypedCurrentModule TypedValueNamespace "make"
    methodOwner = binder providerPath [0, 0] methodName
    methodScheme = monoScheme methodOwner
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId providerPath capabilityName [TypedBoolType]
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "make")
        (binder providerPath [1, 0] methodName)
        methodName
        span1
        trueExpr
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Lib/Api.jz")
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
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        capabilityIdentifier
    importedImplId =
      TypedImplId providerPath importedCapabilityName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint importedCapabilityName (Just qualifiedMethod) TypedBoolType)
        importedImplId
        (Just (TypedMethodId importedImplId "make"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName qualifiedMethod)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing Nothing]
        []
        emptyInterface
        [expressionStatement 1 expression]
        (expressionInfoForFixture expression)

importedDataDependencyProgram :: TypedProgram
importedDataDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule, entryModule] entryPath
  where
    providerPath = (fixtureLibraryPath "ImportedDataProvider")
    facadePath = (fixtureLibraryPath "ImportedDataFacade")
    entryPath = (fixtureModulePath "review-imported-data-dependency")
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
      fixtureScheme valueOwner [] [] [] boxType boxRecipe
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
            (fixtureBoundVariableExpr (binder providerPath [0, 0] (resolved TypedCurrentModule TypedConstructorNamespace "Box")) (info boxType boxRecipe) importedBoxConstructor)
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
        [expressionStatement 1 (fixtureBoundVariableExpr valueOwner entryInfo importedValueName)]
        entryInfo

transitiveDataContractDependencyProgram :: TypedProgram
transitiveDataContractDependencyProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "TransitiveDataContract")
    entryPath = (fixtureModulePath "review-transitive-data-contract-dependency")
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
      fixtureScheme
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
        [expressionStatement 1 (fixtureVariableExpr entryInfo importedValueName)]
        entryInfo

importedCapabilityFacadePath :: [Text]
importedCapabilityFacadePath = (fixtureLibraryPath "ImportedCapabilityFacade")

importedCapabilityDependencyProgram :: TypedProgram
importedCapabilityDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule] importedCapabilityFacadePath
  where
    providerPath = (fixtureLibraryPath "ImportedCapabilityProvider")
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
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "ForeignEq"
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
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
    (TypedImportedModule (fixtureLibraryPath "MetadataOnlyImpl"))
    TypedCapabilityNamespace
    "PrivateEq"

metadataOnlyImportedImpl :: TypedImplId
metadataOnlyImportedImpl =
  TypedImplId
    (fixtureLibraryPath "MetadataOnlyImpl")
    metadataOnlyImportedCapabilityName
    [TypedBoolType]

metadataOnlyImplVisibilityProgram :: TypedProgram
metadataOnlyImplVisibilityProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "MetadataOnlyImpl")
    entryPath = (fixtureModulePath "review-metadata-only-impl-visibility")
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
      TypedCapabilityConstraint capabilityName Nothing TypedBoolType
    importedConstraint =
      TypedCapabilityConstraint metadataOnlyImportedCapabilityName Nothing TypedBoolType
    valueScheme =
      fixtureScheme
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
      TypedEvidenceUse Nothing importedConstraint metadataOnlyImportedImpl Nothing
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
    modulePath = (fixtureModulePath fixture)
    genericName = fixtureValueName "generic"
    genericOwner = binder modulePath [0] genericName
    parameter = TypedTypeParameterId 0
    genericScheme =
      fixtureScheme genericOwner [parameter] [] [] TypedBoolType TypedBoolRecipe
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PatternMarker"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint capabilityName Nothing TypedBoolType
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
    modulePath = (fixtureModulePath fixture)
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
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint phantomFunctionType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

sameScopeValueRebindingProgram :: TypedProgram
sameScopeValueRebindingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-same-scope-value-rebinding"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "item"
    firstSignatureOwner = binder modulePath [0] valueName
    firstOwner = binder modulePath [1] valueName
    secondSignatureOwner = binder modulePath [2] valueName
    secondOwner = binder modulePath [3] valueName
    firstSignatureScheme =
      fixtureScheme firstSignatureOwner [] [] [] TypedBoolType TypedBoolRecipe
    firstScheme =
      fixtureScheme firstOwner [] [] [] TypedBoolType TypedBoolRecipe
    secondSignatureScheme =
      fixtureScheme secondSignatureOwner [] [] [] TypedTextType TypedManagedTextRecipe
    secondScheme =
      fixtureScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedSignatureStatement firstSignatureOwner valueName span1 firstSignatureScheme,
        TypedLetStatement firstOwner valueName span1 firstScheme trueExpr,
        TypedSignatureStatement secondSignatureOwner valueName span1 secondSignatureScheme,
        TypedLetStatement
          secondOwner
          valueName
          span1
          secondScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "latest")),
        expressionStatement 4 (fixtureBoundVariableExpr secondOwner textInfo valueName)
      ]

forwardModuleReferenceProgram :: TypedProgram
forwardModuleReferenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-forward-module-reference"
    modulePath = (fixtureModulePath fixture)
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
          (fixtureVariableExpr boolInfo laterName),
        TypedLetStatement laterOwner laterName span1 (monoScheme laterOwner) trueExpr,
        expressionStatement 3 (fixtureBoundVariableExpr firstOwner boolInfo firstName)
      ]

forwardSignedVisibilityPrograms :: [(Text, TypedProgram)]
forwardSignedVisibilityPrograms =
  [ ( "forward-signed-function-visibility",
      forwardVisibilityProgram "forward-signed-function-visibility" True True
    ),
    ( "forward-signed-scalar-invisibility",
      forwardVisibilityProgram "forward-signed-scalar-invisibility" True False
    ),
    ( "forward-unsigned-function-invisibility",
      forwardVisibilityProgram "forward-unsigned-function-invisibility" False True
    ),
    ( "forward-signed-function-hidden-from-unsigned-caller",
      unsignedForwardCallerProgram
    ),
    ( "forward-signed-function-hidden-from-scalar-expression",
      scalarForwardReferenceProgram
    )
  ]

forwardVisibilityProgram :: Text -> Bool -> Bool -> TypedProgram
forwardVisibilityProgram fixture laterIsSigned laterIsFunction =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstSignatureOwner = binder modulePath [0] firstName
    firstOwner = binder modulePath [1] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [1, 0] firstArgumentName
    firstSignatureScheme =
      fixtureScheme firstSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstScheme =
      fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstBody
      | laterIsFunction =
          TypedApplyExpr
            boolInfo
            laterFunctionReference
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
      | otherwise = fixtureVariableExpr boolInfo laterName
    laterFunctionReference
      | laterIsSigned = fixtureBoundVariableExpr laterOwner boolToBoolInfo laterName
      | otherwise = fixtureVariableExpr boolToBoolInfo laterName
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        firstBody
    laterStatementIndex = if laterIsSigned then 3 else 2
    laterOwner = binder modulePath [laterStatementIndex] laterName
    laterScheme
      | laterIsFunction =
          fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
      | otherwise = monoScheme laterOwner
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [laterStatementIndex, 0] laterArgumentName
    laterExpression
      | laterIsFunction =
          TypedLambdaExpr
            boolToBoolInfo
            laterArgumentOwner
            laterArgumentName
            (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
      | otherwise = trueExpr
    laterSignature =
      let signatureOwner = binder modulePath [2] laterName
          signatureScheme
            | laterIsFunction =
                fixtureScheme signatureOwner [] [] [] boolToBoolType boolToBoolRecipe
            | otherwise = monoScheme signatureOwner
       in TypedSignatureStatement signatureOwner laterName span1 signatureScheme
    terminalStatementIndex = laterStatementIndex + 1
    terminalExpression =
      TypedApplyExpr
        boolInfo
        (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName)
        trueExpr
    statements =
      [ TypedSignatureStatement firstSignatureOwner firstName span1 firstSignatureScheme,
        TypedLetStatement firstOwner firstName span1 firstScheme firstExpression
      ]
        <> [laterSignature | laterIsSigned]
        <> [ TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
             expressionStatement terminalStatementIndex terminalExpression
           ]

unsignedForwardCallerProgram :: TypedProgram
unsignedForwardCallerProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "forward-signed-function-hidden-from-unsigned-caller"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstOwner = binder modulePath [0] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [0, 0] firstArgumentName
    firstScheme = fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureVariableExpr boolToBoolInfo laterName)
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
        )
    laterSignatureOwner = binder modulePath [1] laterName
    laterOwner = binder modulePath [2] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [2, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    statements =
      [ TypedLetStatement firstOwner firstName span1 firstScheme firstExpression,
        TypedSignatureStatement
          laterSignatureOwner
          laterName
          span1
          (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
        TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
        expressionStatement
          3
          ( TypedApplyExpr
              boolInfo
              (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName)
              trueExpr
          )
      ]

scalarForwardReferenceProgram :: TypedProgram
scalarForwardReferenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "forward-signed-function-hidden-from-scalar-expression"
    modulePath = fixtureModulePath fixture
    laterName = fixtureValueName "later"
    laterSignatureOwner = binder modulePath [1] laterName
    laterOwner = binder modulePath [2] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [2, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    statements =
      [ expressionStatement
          1
          ( TypedApplyExpr
              boolInfo
              (fixtureVariableExpr boolToBoolInfo laterName)
              trueExpr
          ),
        TypedSignatureStatement
          laterSignatureOwner
          laterName
          span1
          (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
        TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
        expressionStatement 3 trueExpr
      ]

nestedForwardSignedFunctionProgram :: TypedProgram
nestedForwardSignedFunctionProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-forward-signed-function-invisibility"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstSignatureOwner = binder modulePath [0, 0, 0] firstName
    firstOwner = binder modulePath [0, 0, 1] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [0, 0, 1, 0] firstArgumentName
    firstScheme = fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureVariableExpr boolToBoolInfo laterName)
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
        )
    laterSignatureOwner = binder modulePath [0, 0, 2] laterName
    laterOwner = binder modulePath [0, 0, 3] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [0, 0, 3, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    block =
      TypedBlockExpr
        boolInfo
        [ TypedSignatureStatement
            firstSignatureOwner
            firstName
            span1
            (fixtureScheme firstSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedLetStatement firstOwner firstName span1 firstScheme firstExpression,
          TypedSignatureStatement
            laterSignatureOwner
            laterName
            span1
            (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
          expressionStatement 2 (TypedApplyExpr boolInfo (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName) trueExpr)
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureScheme
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
        expressionStatement 1 (fixtureBoundVariableExpr missingPolymorphicInstantiationOwner boolToBoolInfo valueName)
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
    modulePath = (fixtureModulePath fixture)
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
        (fixtureScheme owner [] [] [TypedStrictEqualityPrimitiveConstraint target] TypedBoolType TypedBoolRecipe)
    statements =
      [ TypedDataStatement dataDeclaration,
        constrained functionOwner boolToBoolType,
        constrained dataOwner unsupportedEqualityDataType
      ]

uncheckedSpecialNameProgram :: TypedProgram
uncheckedSpecialNameProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface builtinMapInfo (fixtureModulePath fixture)
  where
    fixture = "review-unchecked-special-name"
    statements =
      [ expressionStatement 1 (fixtureVariableExpr boolInfo (TypedBuiltinName "doesNotExist")),
        expressionStatement 2 (fixtureVariableExpr boolInfo (TypedGeneratedName TypedOperatorSectionFunction)),
        expressionStatement 3 (fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map"))
      ]

classMethodExportProgram :: TypedProgram
classMethodExportProgram =
  targetIndependentClassMethodImportProgramWith
    "review-class-method-export"
    True

missingTargetIndependentClassMethodDispatchProgram :: TypedProgram
missingTargetIndependentClassMethodDispatchProgram =
  targetIndependentClassMethodImportProgramWith
    "review-missing-target-independent-class-method-dispatch"
    False

targetIndependentClassMethodImportProgramWith :: Text -> Bool -> TypedProgram
targetIndependentClassMethodImportProgramWith fixture includeEvidence =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "ClassMethodExport")
    entryPath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    methodScheme = fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    declaration = TypedClassDeclaration span1 className [parameter] [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath className [TypedBoolType]
    methodDefinition = fixtureImplMethod libraryPath [1, 0] localImplId "render"
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/ClassMethodExport.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Render",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface declaration] [TypedImplInterface localImplId])
        [ TypedClassStatement declaration,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [methodDefinition])
        ]
        boolInfo
    importedMethod = resolved (TypedImportedModule libraryPath) TypedValueNamespace "render"
    importedClassName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Render"
    importedImplId =
      TypedImplId libraryPath importedClassName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint
        importedClassName
        (Just (Text.intercalate "::" (libraryPath <> ["Render", "render"])))
        TypedBoolType
    selectedEvidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            ( Just
                ( TypedEvidenceParameterRef
                    methodOwner
                    (TypedEvidenceParameterId 0)
                )
            )
            constraint
            importedImplId
            (Just (TypedMethodId importedImplId "render"))
        )
    evidenceSelections
      | includeEvidence = [selectedEvidence]
      | otherwise = []
    importedMethodInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [ TypedInstantiation
            methodOwner
            [TypedTypeArgument parameter TypedBoolType]
            Nothing
        ]
        evidenceSelections
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["render"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr importedMethodInfo importedMethod)]
        importedMethodInfo

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
    renderOwner = binder ["Prelude"] [1, 0] renderName
    mapOwner = binder ["Prelude"] [1, 1] mapName
    equalityClass = fixtureEqualClass TypedCurrentModule
    renderClass =
      TypedClassDeclaration
        span1
        renderClassName
        [parameter]
        [ TypedMethodSignature renderName span1 (fixtureScheme renderOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedMethodSignature mapName span1 (fixtureScheme mapOwner [] [] [] genericMapType genericMapRecipe)
        ]
    boolImpl = TypedImplId ["Prelude"] equalClassName [TypedBoolType]
    charImpl = TypedImplId ["Prelude"] equalClassName [TypedCharType]
    textRenderImpl = TypedImplId ["Prelude"] renderClassName [TypedTextType]
    genericMapType =
      TypedFunctionType
        (TypedFunctionType TypedBoolType (TypedTypeParameterType parameter))
        ( TypedFunctionType
            (TypedListType TypedBoolType)
            (TypedListType (TypedTypeParameterType parameter))
        )
    genericMapRecipe =
      TypedClosureRecipe
        [ TypedClosureRecipe
            [TypedBoolRecipe]
            (TypedRepresentationParameterRecipe parameter),
          TypedManagedListRecipe TypedBoolRecipe
        ]
        (TypedManagedListRecipe (TypedRepresentationParameterRecipe parameter))
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
    mapExpression = fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map")
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
      | methodKey == "map" =
          fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map")
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
    modulePath = (fixtureModulePath fixture)
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedTextType
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "generic"
    owner = binder modulePath [0] valueName
    parameterType = TypedTypeParameterType evidenceTypeScopeParameter
    scheme = fixtureScheme owner [evidenceTypeScopeParameter] [] [] TypedBoolType TypedBoolRecipe
    implId =
      TypedImplId
        ["Prelude"]
        (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
        [TypedBoolType]
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing parameterType
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    statements =
      [ TypedClassStatement (TypedClassDeclaration span1 className [TypedTypeParameterId 0] []),
        TypedImplStatement (TypedImplDeclaration span1 foreignOwnedLocalImplId [])
      ]

importedTypeCapabilityMetadataProgram :: TypedProgram
importedTypeCapabilityMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "TypeCapability")
    entryPath = (fixtureModulePath "review-imported-type-capability-metadata")
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
    modulePath = (fixtureModulePath "review-callable-builtin-equality")
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureScheme
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
        (fixtureVariableExpr calleeInfo valueName)
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
          (TypedCapabilityConstraint (preludeCapability "Missing") (Just "Missing.m") TypedBoolType),
        TypedEvidenceParameter
          (TypedEvidenceParameterId 1)
          (TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.missing") TypedBoolType)
      ]
    scheme = fixtureScheme owner [] evidence [] TypedBoolType TypedBoolRecipe

unconstrainedNumericParameterProgram :: TypedProgram
unconstrainedNumericParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-numeric-parameter"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "numeric"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentOwner = binder modulePath [0, 0] argumentName
    argument = fixtureBoundVariableExpr argumentOwner parameterInfo argumentName
    body = TypedBinaryExpr parameterInfo (TypedBuiltinOperator "+") argument argument
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        body
    scheme = fixtureScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

unconstrainedEqualityParameterProgram :: TypedProgram
unconstrainedEqualityParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-equality-parameter"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "equal"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterInfo =
      info
        (TypedTypeParameterType parameter)
        (TypedRepresentationParameterRecipe parameter)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentOwner = binder modulePath [0, 0] argumentName
    argument = fixtureBoundVariableExpr argumentOwner parameterInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    functionType = TypedFunctionType (TypedTypeParameterType parameter) TypedBoolType
    functionRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        TypedBoolRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        body
    scheme = fixtureScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

duplicatePatternNameSecondBinder :: TypedBinderId
duplicatePatternNameSecondBinder =
  binder
    (fixtureModulePath "review-duplicate-pattern-name")
    [0, 1]
    (fixtureValueName "duplicate")

duplicatePatternNameProgram :: TypedProgram
duplicatePatternNameProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-duplicate-pattern-name"
    modulePath = (fixtureModulePath fixture)
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

duplicateOrPatternContractProgram :: TypedProgram
duplicateOrPatternContractProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-duplicate-or-pattern-contract"
    modulePath = (fixtureModulePath fixture)
    duplicateName = fixtureValueName "duplicate"
    mixedTupleInfo =
      info
        (TypedTupleType [TypedBoolType, TypedTextType])
        (TypedManagedProductRecipe [TypedBoolRecipe, TypedManagedTextRecipe])
    variable lexicalPath valueInfo =
      TypedVariablePattern
        valueInfo
        (binder modulePath lexicalPath duplicateName)
        duplicateName
    firstAlternative =
      TypedTuplePattern
        mixedTupleInfo
        [variable [0, 0] boolInfo, variable [0, 1] textInfo]
    secondAlternative =
      TypedTuplePattern
        mixedTupleInfo
        [variable [1, 0] boolInfo, variable [1, 1] boolInfo]
    patternValue =
      TypedOrPattern mixedTupleInfo [firstAlternative, secondAlternative]
    scrutinee =
      TypedTupleExpr
        mixedTupleInfo
        [trueExpr, literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "value")]
    expression =
      TypedPatternCaseExpr
        boolInfo
        scrutinee
        [TypedCaseArm patternValue Nothing trueExpr]

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
    modulePath = (fixtureModulePath fixture)
    firstName = fixtureValueName "first"
    secondName = fixtureValueName "second"
    firstOwner = fixtureBinder fixture 0 firstName
    secondOwner = ownerAmbiguousSecondOwner
    parameter = TypedTypeParameterId 0
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme owner =
      fixtureScheme
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
    expression = fixtureVariableExpr expressionInfo firstName
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
    modulePath = (fixtureModulePath fixture)
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

reorderedOrPatternMismatchBinder :: TypedBinderId
reorderedOrPatternMismatchBinder =
  binder
    (fixtureModulePath "review-reordered-or-pattern")
    [1, 0]
    (fixtureValueName "right")

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
    libraryPath = (fixtureLibraryPath fixture)
    entryPath = (fixtureModulePath fixture)
    localClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedClassName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    parameter = TypedTypeParameterId 0
    methodScheme = fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
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
    constraint = TypedCapabilityConstraint importedClassName Nothing TypedBoolType
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
      fixtureScheme
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
    modulePath = (fixtureModulePath fixture)
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
    argument = fixtureBoundVariableExpr argumentBinder operandInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    lambdaType = TypedFunctionType operandType TypedBoolType
    lambdaRecipe = TypedClosureRecipe [operandRecipe] TypedBoolRecipe
    expression = TypedLambdaExpr (info lambdaType lambdaRecipe) argumentBinder argumentName body
    scheme =
      fixtureScheme
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
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just methodKey) TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression
      | methodKey == "Equal::equal" =
          fixtureVariableExpr
            (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
            (TypedBuiltinName "Equal::equal")
      | otherwise =
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
    (fixtureVariableExpr boolInfo (TypedBuiltinName "__kernel_textLength"))

missingInterfaceMetadataDataName :: TypedCoreName
missingInterfaceMetadataDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Box"

missingInterfaceMetadataProgram :: TypedProgram
missingInterfaceMetadataProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = (fixtureLibraryPath "MissingMetadata")
    valueName = resolved TypedCurrentModule TypedValueNamespace "boxed"
    valueBinder = binder libraryPath [0] valueName
    dataType = TypedDataType missingInterfaceMetadataDataName []
    dataRecipe = TypedManagedVariantRecipe missingInterfaceMetadataDataName []
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Box"
    valueScheme = fixtureScheme valueBinder [] [] [] dataType dataRecipe
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
            (fixtureBoundVariableExpr (binder libraryPath [1, 0] constructorName) (info dataType dataRecipe) constructorName),
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "same"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme =
      fixtureScheme
        constrainedMonomorphicOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    expression = fixtureBoundVariableExpr constrainedMonomorphicOwner boolInfo valueName
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "choose"
    firstParameter = TypedTypeParameterId 0
    secondParameter = TypedTypeParameterId 1
    parameterType = TypedTypeParameterType firstParameter
    parameterRecipe = TypedRepresentationParameterRecipe firstParameter
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      fixtureScheme
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
        (fixtureVariableExpr instantiatedInfo valueName)
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
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
      fixtureScheme
        (methodBinder 0 localName)
        [parameter]
        []
        []
        parameterType
        parameterRecipe
    evidenceScheme =
      fixtureScheme
        (methodBinder 1 evidenceName)
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint capabilityName Nothing parameterType)
        ]
        []
        parameterType
        parameterRecipe
    primitiveScheme =
      fixtureScheme
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
    (fixtureModulePath "review-duplicate-impl-declaration")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

duplicateImplDeclarationProgram :: TypedProgram
duplicateImplDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-impl-declaration"
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureScheme
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
        (fixtureVariableExpr calleeInfo valueName)
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
    modulePath = (fixtureModulePath fixture)
    operatorName =
      TypedGeneratedName
        (TypedOperatorBinding "$operator:%7E")
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
      fixtureScheme
        owner
        []
        []
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator operatorName "^")
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
    libraryPath = (fixtureLibraryPath "DataDependency")
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
    libraryPath = (fixtureLibraryPath "ClassMethodDependency")
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
      fixtureScheme
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
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    numericName = fixtureValueName "numeric"
    numericOwner = binder modulePath [0] numericName
    equalityName = fixtureValueName "equality"
    equalityOwner = binder modulePath [1] equalityName
    constrainedScheme owner primitiveConstraint =
      fixtureScheme
        owner
        [parameter]
        []
        [primitiveConstraint]
        TypedBoolType
        TypedBoolRecipe
    instantiatedUse owner name typeArgument =
      fixtureVariableExpr
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
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    functionName = fixtureValueName "function"
    functionOwner = binder modulePath [0] functionName
    otherName = fixtureValueName "other"
    scheme owner =
      fixtureScheme owner [parameter] [] [] TypedBoolType TypedBoolRecipe
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
        (fixtureVariableExpr functionInfo functionName)
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
    (fixtureModulePath "review-constrained-resolved-operator")
    [0]
    constrainedResolvedOperatorName

constrainedResolvedOperatorName :: TypedCoreName
constrainedResolvedOperatorName =
  TypedGeneratedName (TypedOperatorBinding "$operator:%7E")

constrainedResolvedOperatorProgram :: TypedProgram
constrainedResolvedOperatorProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath)
  where
    fixture = "review-constrained-resolved-operator"
    modulePath = (fixtureModulePath fixture)
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme =
      fixtureScheme
        constrainedResolvedOperatorOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator constrainedResolvedOperatorName "~")
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
        (fixtureModulePath fixture)
        relativeSource
        []
        []
        emptyInterface
        [TypedLetStatement owner name span1 (monoScheme owner) trueExpr]
        boolInfo
    ]
    (fixtureModulePath fixture)
  where
    fixture = "review-missing-module-result"
    name = fixtureValueName "item"
    owner = fixtureBinder fixture 0 name

emptyDataDeclarationProgram :: TypedProgram
emptyDataDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-data-declaration"
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
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
      [ TypedLetStatement
          laterOrPatternCollidingBinder
          valueName
          span1
          (monoScheme laterOrPatternCollidingBinder)
          trueExpr,
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
    (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    renderName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    mapName =
      resolved TypedCurrentModule TypedValueNamespace "map"
    parameter = TypedTypeParameterId 0
    methodScheme methodOwner =
      fixtureScheme
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
    (fixtureValueName "item")

duplicateInstantiationProgram :: TypedProgram
duplicateInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-instantiation"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "item"
    parameter = TypedTypeParameterId 0
    scheme =
      fixtureScheme
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
      fixtureVariableExpr
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
    (fixtureModulePath fixture)
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
    libraryPath = (fixtureLibraryPath "VisibleClass")
    entryPath = (fixtureModulePath "review-visible-class-collision")
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

selectedClassDataDependencyCapabilityName :: TypedCoreName
selectedClassDataDependencyCapabilityName =
  resolved
    (TypedImportedModule (fixtureLibraryPath "SelectedClassData"))
    TypedCapabilityNamespace
    "RoundTrip"

selectedClassDataDependencyProgram :: TypedProgram
selectedClassDataDependencyProgram =
  TypedProgram Nothing [libraryModule, facadeModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "SelectedClassData")
    facadePath = (fixtureLibraryPath "SelectedClassDataFacade")
    entryPath = (fixtureModulePath "review-selected-class-data-dependency")
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
      fixtureScheme
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
    importedCapabilityName = selectedClassDataDependencyCapabilityName
    importedMethodName =
      resolved
        (TypedImportedModule libraryPath)
        TypedValueNamespace
        "roundTrip"
    retainedClass =
      TypedClassDeclaration
        span1
        importedCapabilityName
        [classParameter]
        [TypedMethodSignature importedMethodName span1 methodScheme]
    forwardedName =
      resolved TypedCurrentModule TypedValueNamespace "forwarded"
    forwardedOwner = binder facadePath [0] forwardedName
    forwardedScheme =
      fixtureScheme
        forwardedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/SelectedClassDataFacade.jz")
        [TypedResolvedImport span1 libraryPath Nothing (Just ["RoundTrip"])]
        [TypedModuleExport TypedValueNamespace "forwarded"]
        ( TypedModuleInterface
            [TypedValueInterface forwardedName forwardedScheme]
            []
            [TypedClassInterface retainedClass]
            []
        )
        [TypedLetStatement forwardedOwner forwardedName span1 forwardedScheme trueExpr]
        unitInfo
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
      resolved TypedCurrentModule TypedValueNamespace "item"
    parameterBinder = binder entryPath [0, 0, 0] parameterName
    body = fixtureBoundVariableExpr parameterBinder (info importedBoxType importedBoxRecipe) parameterName
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
        [TypedResolvedImport span1 facadePath Nothing (Just ["forwarded"])]
        []
        emptyInterface
        [ TypedImplStatement
            (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo

selectedValueDataMetadataProgram :: TypedProgram
selectedValueDataMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "SelectedValueData")
    entryPath = (fixtureModulePath "review-selected-value-data-metadata")
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
    valueScheme = fixtureScheme valueBinder [] [] [] dataType dataRecipe
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
            (fixtureBoundVariableExpr (binder libraryPath [1, 0] localConstructorName) (info dataType dataRecipe) localConstructorName),
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
        [expressionStatement 1 (fixtureBoundVariableExpr valueBinder entryInfo importedValueName)]
        entryInfo

selectiveImportLeakedImpl :: TypedImplId
selectiveImportLeakedImpl =
  TypedImplId
    (fixtureLibraryPath "PrivateImpl")
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedBoolType]

selectiveImportImplLeakProgram :: TypedProgram
selectiveImportImplLeakProgram = TypedProgram (Just fixturePrelude) [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "PrivateImpl")
    entryPath = (fixtureModulePath "review-selective-import-impl-leak")
    localValueName = resolved TypedCurrentModule TypedValueNamespace "published"
    valueBinder = binder libraryPath [0] localValueName
    valueScheme = monoScheme valueBinder
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/PrivateImpl.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface localValueName valueScheme]
            []
            [TypedClassInterface retainedPreludeEqualClass]
            [TypedImplInterface selectiveImportLeakedImpl]
        )
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
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
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
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    capability = TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint = TypedCapabilityConstraint capabilityName (Just "Equal.equal") TypedBoolType
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerBinder = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    argumentUse = fixtureBoundVariableExpr argumentBinder parameterInfo argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localBinder = binder modulePath [0, 0, 0] localName
    localScheme = fixtureScheme localBinder [] [] [] parameterType parameterRecipe
    localBinding =
      TypedLetStatement
        localBinder
        localName
        span1
        localScheme
        argumentUse
    localUse = expressionStatement 2 (fixtureBoundVariableExpr localBinder parameterInfo localName)
    block = TypedBlockExpr parameterInfo [localBinding, localUse]
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentBinder
        argumentName
        block
    outerScheme = fixtureScheme outerBinder [parameter] [] [] functionType functionRecipe
    topLevelBinding = TypedLetStatement outerBinder outerName span1 outerScheme expression

implMethodVisibleName :: TypedCoreName
implMethodVisibleName = resolved TypedCurrentModule TypedValueNamespace "equal"

implMethodValueVisibilityProgram :: TypedProgram
implMethodValueVisibilityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-value-visibility"
    modulePath = (fixtureModulePath fixture)
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
        expressionStatement 2 (fixtureVariableExpr boolInfo implMethodVisibleName)
      ]

builtinOperatorContractProgram :: TypedProgram
builtinOperatorContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface intInfo modulePath
  where
    fixture = "review-builtin-operator-contract"
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    constraint = TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.render") TypedTextType
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
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme =
      fixtureScheme
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
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "phantom"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId (TypedDataType missingInstantiationDataName [])] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
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
invisibleOperatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")

invisibleOperatorProgram :: TypedProgram
invisibleOperatorProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo entryPath
  where
    fixture = "review-invisible-operator"
    entryPath = (fixtureModulePath "review-invisible-operator")
    expression = TypedOperatorValueExpr boolInfo (TypedResolvedOperator invisibleOperatorName "~")

expressionDuplicateBinder :: TypedBinderId
expressionDuplicateBinder =
  binder
    (fixtureModulePath "review-expression-duplicate-binder")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

expressionDuplicateBinderProgram :: TypedProgram
expressionDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-expression-duplicate-binder"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    scheme = monoScheme expressionDuplicateBinder
    lambda = TypedLambdaExpr boolToBoolInfo expressionDuplicateBinder valueName (fixtureBoundVariableExpr expressionDuplicateBinder boolInfo valueName)
    statements = [TypedLetStatement expressionDuplicateBinder valueName span1 scheme trueExpr, expressionStatement 2 lambda]

privateInterfaceLibraryPath :: [Text]
privateInterfaceLibraryPath = ["Private", "Library"]

privateInterfaceEntryPath :: [Text]
privateInterfaceEntryPath = (fixtureModulePath "review-private-interface")

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
        [expressionStatement 1 (fixtureVariableExpr boolInfo privateInterfaceImportedName)]
        boolInfo

constructorPatternContractProgram :: TypedProgram
constructorPatternContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-constructor-pattern-contract"
    modulePath = (fixtureModulePath fixture)
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    constructorOwner = binder modulePath [0, 0] someName
    declaration =
      TypedDataDeclaration
        span1
        optionName
        [parameterId]
        [ TypedConstructorDeclaration
            constructorOwner
            someName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
        [TypedInstantiation constructorOwner [TypedTypeArgument parameterId TypedBoolType] Nothing]
        []
    scrutinee = TypedApplyExpr optionInfo (fixtureVariableExpr constructorInfo someName) trueExpr
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
    (fixtureModulePath "review-explicit-type-application-contract")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

explicitTypeApplicationContractProgram :: TypedProgram
explicitTypeApplicationContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-explicit-type-application-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    scheme = monoScheme explicitTypeApplicationOwner
    expression = TypedTypeApplicationExpr boolInfo (fixtureBoundVariableExpr explicitTypeApplicationOwner boolInfo valueName) span1 TypedBoolType
    statements = [TypedLetStatement explicitTypeApplicationOwner valueName span1 scheme trueExpr, expressionStatement 2 expression]

variableSchemeContractProgram :: TypedProgram
variableSchemeContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-variable-scheme-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    statements =
      [ TypedLetStatement valueBinder valueName span1 (monoScheme valueBinder) trueExpr,
        expressionStatement 2 (fixtureBoundVariableExpr valueBinder textInfo valueName)
      ]

missingImportProgram :: TypedProgram
missingImportProgram =
  typedProgram
  where
    fixture = "review-missing-import"
    modulePath = (fixtureModulePath fixture)
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
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface builtinMapInfo modulePath)
  where
    fixture = "review-candidate-constraint"
    modulePath = (fixtureModulePath fixture)
    renderName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    equalName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    renderImpl = TypedImplId ["Prelude"] renderName [TypedTextType]
    equalImpl = TypedImplId ["Prelude"] equalName [TypedBoolType]
    constraint = TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.map") TypedTextType
    equalCandidate = TypedEvidenceCandidate equalImpl (Just (TypedMethodId equalImpl "equal"))
    wrongMethodCandidate = TypedEvidenceCandidate renderImpl (Just (TypedMethodId renderImpl "render"))
    candidateExpression candidate =
      fixtureVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (TypedBuiltinName "map")
    statements =
      [ expressionStatement 1 (candidateExpression equalCandidate),
        expressionStatement 2 (candidateExpression wrongMethodCandidate)
      ]

invalidVariableNamespaceName :: TypedCoreName
invalidVariableNamespaceName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

invalidVariableNamespaceProgram :: TypedProgram
invalidVariableNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-variable-namespace"
    modulePath = (fixtureModulePath fixture)
    declaration =
      dataDeclarationWithNullaryConstructor
        modulePath
        [0, 0]
        invalidVariableNamespaceName
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 2 (fixtureVariableExpr boolInfo invalidVariableNamespaceName)
      ]

binderNameContractBinder :: TypedBinderId
binderNameContractBinder =
  binder
    (fixtureModulePath "review-binder-name-contract")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "embedded")

binderNameContractProgram :: TypedProgram
binderNameContractProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-binder-name-contract"
    modulePath = (fixtureModulePath fixture)
    publishedName = resolved TypedCurrentModule TypedValueNamespace "published"
    scheme = monoScheme binderNameContractBinder
    statement = TypedLetStatement binderNameContractBinder publishedName span1 scheme trueExpr

blockLocalGeneralizedSchemeProgram :: TypedProgram
blockLocalGeneralizedSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-block-local-generalized-scheme"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    use = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    block = TypedBlockExpr boolInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

blockLocalMonomorphicSchemeProgram :: TypedProgram
blockLocalMonomorphicSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface textInfo modulePath
  where
    fixture = "review-block-local-monomorphic-scheme"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    scheme = monoScheme owner
    use = fixtureBoundVariableExpr owner textInfo valueName
    block = TypedBlockExpr textInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

implMethodNameProgram :: TypedProgram
implMethodNameProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-name"
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    owner = binder modulePath [0] operatorName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    scheme = fixtureScheme owner [] [] [] operatorType operatorRecipe
    operator = TypedResolvedOperator operatorName "~"
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
      TypedCoreValidationFailure (TypedExpressionPath (fixtureModulePath "review-operator-scheme") [statementIndex] [0])

selectiveImportProgram :: TypedProgram
selectiveImportProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "Selective")
    entryPath = (fixtureModulePath "review-selective-import")
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
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodBinder = binder modulePath [0, 0] methodName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodScheme = fixtureScheme methodBinder [] [] [] methodType methodRecipe
    declaration = TypedClassDeclaration span1 capabilityName [parameterId] [TypedMethodSignature methodName span1 methodScheme]

evidenceParameterContractProgram :: TypedProgram
evidenceParameterContractProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-evidence-parameter-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    evidenceId = TypedEvidenceParameterId 0
    generalizedConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing (TypedTypeParameterType parameterId)
    scheme = fixtureScheme owner [parameterId] [TypedEvidenceParameter evidenceId generalizedConstraint] [] TypedBoolType TypedBoolRecipe
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
    expression selection = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [selection]) valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 2 (expression (selected (TypedEvidenceParameterId 7) (TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType) TypedBoolType)),
        expressionStatement 3 (expression (selected evidenceId (TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType) TypedCharType))
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    evidenceId = TypedEvidenceParameterId 0
    laterEvidenceId = TypedEvidenceParameterId 1
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    laterConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType
    scheme = fixtureScheme owner [] [TypedEvidenceParameter evidenceId constraint, TypedEvidenceParameter laterEvidenceId laterConstraint] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
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
    modulePath = (fixtureModulePath fixture)
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
    statements = [TypedDataStatement declaration, expressionStatement 2 (fixtureBoundVariableExpr (binder modulePath [0, 0] constructorName) boolInfo constructorName)]

unrelatedTypeApplicationProgram :: TypedProgram
unrelatedTypeApplicationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface applicationInfo modulePath
  where
    fixture = "review-unrelated-type-application"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "unrelated"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      fixtureScheme
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
    modulePath = (fixtureModulePath fixture)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0] argumentName
    lambdaInfo =
      info
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    expression = TypedLambdaExpr lambdaInfo argumentBinder argumentName (fixtureBoundVariableExpr argumentBinder textInfo argumentName)

generalizedVariableContractProgram :: TypedProgram
generalizedVariableContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface badUseInfo modulePath
  where
    fixture = "review-generalized-variable-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    scheme =
      fixtureScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    badUseInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression = fixtureBoundVariableExpr owner badUseInfo valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameterId),
        expressionStatement 2 expression
      ]

enclosingInstantiationScopeProgram :: TypedProgram
enclosingInstantiationScopeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-enclosing-instantiation-scope"
    modulePath = (fixtureModulePath fixture)
    identityName = resolved TypedCurrentModule TypedValueNamespace "identity"
    identityOwner = binder modulePath [0] identityName
    identityParameter = TypedTypeParameterId 0
    identityParameterType = TypedTypeParameterType identityParameter
    identityParameterRecipe = TypedRepresentationParameterRecipe identityParameter
    identityScheme =
      fixtureScheme
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
    wrapperScheme = fixtureScheme wrapperOwner [wrapperParameter] [] [] wrapperType wrapperRecipe
    instantiation = TypedInstantiation identityOwner [TypedTypeArgument identityParameter wrapperParameterType] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo wrapperType wrapperRecipe [instantiation] []) identityName
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
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodOwner = binder modulePath [0, 0] methodName
    methodScheme = fixtureScheme methodOwner [] [] [] methodType methodRecipe
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
    modulePath = (fixtureModulePath fixture)
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
    modulePath = (fixtureModulePath fixture)
    firstOwner = binder modulePath [0] duplicateDeclarationName
    secondOwner = binder modulePath [1] duplicateDeclarationName
    statements =
      [ TypedSignatureStatement firstOwner duplicateDeclarationName span1 (monoScheme firstOwner),
        TypedSignatureStatement secondOwner duplicateDeclarationName span1 (monoScheme secondOwner)
      ]

importedImplQualificationProgram :: TypedProgram
importedImplQualificationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "QualifiedImpl")
    entryPath = (fixtureModulePath "review-imported-impl-qualification")
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
    constraint = TypedCapabilityConstraint importedCapabilityName Nothing importedTargetType
    valueName = resolved TypedCurrentModule TypedValueNamespace "usesMark"
    valueOwner = binder entryPath [0] valueName
    evidenceParameter = TypedEvidenceParameterId 0
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [TypedEvidenceParameter evidenceParameter constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    instantiation = TypedInstantiation valueOwner [] Nothing
    evidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef valueOwner evidenceParameter))
            constraint
            importedImplId
            Nothing
        )
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [evidence])
        valueName
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing Nothing]
        []
        emptyInterface
        [ TypedLetStatement valueOwner valueName span1 valueScheme trueExpr,
          expressionStatement 1 expression
        ]
        (expressionInfoForFixture expression)

implTargetArityProgram :: TypedProgram
implTargetArityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-target-arity"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType, TypedCharType]
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 2 expression
      ]

localDeclarationOriginBinder :: TypedBinderId
localDeclarationOriginBinder =
  binder
    (fixtureModulePath "review-local-declaration-origin")
    [0]
    localDeclarationOriginName

localDeclarationOriginName :: TypedCoreName
localDeclarationOriginName = resolved (TypedImportedModule ["Other", "Module"]) TypedValueNamespace "foreign"

localDeclarationOriginProgram :: TypedProgram
localDeclarationOriginProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-local-declaration-origin"
    modulePath = (fixtureModulePath fixture)
    scheme = monoScheme localDeclarationOriginBinder
    statements =
      [ TypedLetStatement
          localDeclarationOriginBinder
          localDeclarationOriginName
          span1
          scheme
          trueExpr
      ]

reservedValueIdentifierName :: TypedCoreName
reservedValueIdentifierName =
  resolved TypedCurrentModule TypedValueNamespace "value"

reservedValueIdentifierProgram :: TypedProgram
reservedValueIdentifierProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-reserved-value-identifier"
    modulePath = fixtureModulePath fixture
    valueBinder = binder modulePath [0] reservedValueIdentifierName
    statements =
      [ TypedLetStatement
          valueBinder
          reservedValueIdentifierName
          span1
          (monoScheme valueBinder)
          trueExpr
      ]

reservedValueModulePath :: [Text]
reservedValueModulePath = ["Fixture", "value"]

reservedValueModulePathProgram :: TypedProgram
reservedValueModulePathProgram =
  TypedProgram
    Nothing
    [ typedModule
        reservedValueModulePath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo
    ]
    reservedValueModulePath

expressionFixtureProgram :: Text -> TypedExpr -> TypedProgram
expressionFixtureProgram fixture expression =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface (expressionInfoForFixture expression) (fixtureModulePath fixture)

expressionInfoForFixture :: TypedExpr -> TypedNodeInfo
expressionInfoForFixture expression =
  case expression of
    TypedLiteralExpr valueInfo _ -> valueInfo
    TypedVariableExpr valueInfo _ _ -> valueInfo
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

assertJazzStructure :: Text -> Either Text CanonicalTypedCoreStructure -> RunResult -> IO ()
assertJazzStructure label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " structure") expected (checkedRunStructure result)

checkedRunStructure :: RunResult -> Either Text CanonicalTypedCoreStructure
checkedRunStructure result =
  case runRuntimeValue result of
    Just value -> decodeCanonicalTypedCoreStructure value
    Nothing -> Left "run completed without a runtime value"

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
    _ -> error "unknown valid typed-core fixture"

programWith :: Text -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> TypedProgram
programWith fixtureName statements interface moduleInfo =
  TypedProgram
    Nothing
    [ typedModule
        (fixtureModulePath fixtureName)
        (TypedSourcePath ("src/Fixture/" <> fixtureName <> ".jz"))
        []
        []
        interface
        statements
        moduleInfo
    ]
    (fixtureModulePath fixtureName)

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
    directInfo
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
        expressionStatement 1 (TypedVariableExpr directInfo directName (Just directOwner))
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

instantiationProgram :: Text -> Maybe TypedSpan -> TypedProgram
instantiationProgram fixture explicitSpan =
  programWith
    fixture
    [ TypedLetStatement
        owner
        name
        span1
        scheme
        (polymorphicIdentityExpression (fixtureModulePath fixture) [0] parameterId),
      expressionStatement 2 expression
    ]
    emptyInterface
    instantiatedInfo
  where
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder (fixtureModulePath fixture) [0] name
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument (TypedTypeParameterId 0) TypedBoolType]
        explicitSpan
    parameterId = TypedTypeParameterId 0
    scheme =
      fixtureScheme
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
        Nothing -> fixtureVariableExpr instantiatedInfo name
        Just explicitApplicationSpan ->
          TypedTypeApplicationExpr
            instantiatedInfo
            (fixtureVariableExpr instantiatedInfo name)
            explicitApplicationSpan
            TypedBoolType

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
    methodExpression = fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map")
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
    "flattened-callable-data-field",
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
    flattenedCallableDataFieldFixture,
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

applicationArgumentRecipeStagingFixture :: InvalidFixture
applicationArgumentRecipeStagingFixture =
  InvalidFixture
    fixture
    program
    [expressionFailureAt fixture 1 TypedApplicationArgumentMismatch (TypedRecipeDetail stagedCallableRecipe directCallableRecipe)]
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
    [expressionFailureAt fixture 0 TypedLambdaResultMismatch (TypedRecipeDetail expectedTailRecipe actualTailRecipe)]
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
    [expressionFailureAt fixture 2 TypedConditionalBranchMismatch (TypedRecipeDetail directRecipe closureRecipe)]
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
        (TypedRecipeDetail closureRecipe directRecipe)
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
    expression = TypedVariableExpr (TypedNodeInfo builtinMapType builtinMapRecipe [instantiation] []) (TypedBuiltinName "map") Nothing

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

signatureProgram :: Text -> TypedBinderId -> TypedCoreName -> TypedScheme -> TypedProgram
signatureProgram fixture valueBinder valueName scheme =
  singleModuleProgram
    fixture
    relativeSource
    []
    [TypedSignatureStatement valueBinder valueName span1 scheme]
    emptyInterface
    boolInfo
    (fixtureModulePath fixture)

singleModuleProgram :: Text -> TypedSourcePath -> [TypedModuleExport] -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> [Text] -> TypedProgram
singleModuleProgram fixture sourcePath exports statements interface moduleInfo entryModule =
  TypedProgram
    Nothing
    [typedModule (fixtureModulePath fixture) sourcePath [] exports interface statements moduleInfo]
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
    (fixtureBoundVariableExpr argumentBinder parameterInfo argumentName)
  where
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath (lexicalPath <> [0]) argumentName
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

preludeCapability :: Text -> TypedCoreName
preludeCapability = resolved TypedAmbientPrelude TypedCapabilityNamespace

fixtureBinder :: Text -> Int -> TypedCoreName -> TypedBinderId
fixtureBinder fixture lexicalIndex = binder (fixtureModulePath fixture) [lexicalIndex]

fixtureModulePath :: Text -> [Text]
fixtureModulePath fixture = ["Fixture", fixtureModuleSegment fixture]

fixtureLibraryPath :: Text -> [Text]
fixtureLibraryPath fixture = ["Library", fixtureModuleSegment fixture]

fixtureModuleSegment :: Text -> Text
fixtureModuleSegment = Text.replace "-" "_"

monoScheme :: TypedBinderId -> TypedScheme
monoScheme valueBinder = fixtureScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

fixtureScheme :: TypedBinderId -> [TypedTypeParameterId] -> [TypedEvidenceParameter] -> [TypedPrimitiveConstraint] -> TypedType -> TypedRepresentationRecipe -> TypedScheme
fixtureScheme owner parameters evidence primitive typeValue recipe =
  TypedScheme owner parameters evidence primitive typeValue recipe callableShape
  where
    callableShape =
      case typeValue of
        TypedFunctionType {} -> Just TypedDirectCallableShape
        _ -> Nothing

fixtureVariableExpr :: TypedNodeInfo -> TypedCoreName -> TypedExpr
fixtureVariableExpr nodeInfo name = TypedVariableExpr nodeInfo name binderReference
  where
    binderReference =
      case nodeInstantiationsForFixture nodeInfo of
        TypedInstantiation owner _ _ : _ -> Just owner
        [] -> Nothing

fixtureBoundVariableExpr :: TypedBinderId -> TypedNodeInfo -> TypedCoreName -> TypedExpr
fixtureBoundVariableExpr owner nodeInfo name = TypedVariableExpr nodeInfo name (Just owner)

nodeInstantiationsForFixture :: TypedNodeInfo -> [TypedInstantiation]
nodeInstantiationsForFixture (TypedNodeInfo _ _ instantiations _) = instantiations

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
moduleFailure fixture = TypedCoreValidationFailure (TypedModulePath (fixtureModulePath fixture))

statementFailure :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
statementFailure fixture statementIndex = statementFailureAtPath fixture [statementIndex]

statementFailureAtPath :: Text -> [Int] -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
statementFailureAtPath fixture statementPath = TypedCoreValidationFailure (TypedStatementPath (fixtureModulePath fixture) statementPath)

expressionFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailure fixture = expressionFailureAt fixture 0

expressionFailureAt :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailureAt fixture statementIndex = expressionFailureAtPath fixture [statementIndex]

expressionFailureAtPath :: Text -> [Int] -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailureAtPath fixture statementPath =
  TypedCoreValidationFailure (TypedExpressionPath (fixtureModulePath fixture) statementPath [0])

patternFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
patternFailure fixture =
  TypedCoreValidationFailure (TypedPatternPath (fixtureModulePath fixture) [0] [0, 0])
