{-# LANGUAGE OverloadedStrings #-}

-- | Domain and review-regression validator contract tests.
module Jazz.Compiler.Bootstrap.TypedCoreContract.RegressionTests
  ( tests,
    reviewRegressionPrograms,
  )
where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreContract.Fixtures
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )
import System.Timeout (timeout)

tests :: [NamedTest]
tests = map fst reviewRegressionGroups <> [("uses nearest-prior dependencies through rebinding", testNearestPriorBindingDependencies), ("preserves source-ordered recursive visibility", testSourceOrderedRecursiveVisibility)]

testRecursiveGroupContracts :: IO ()
testRecursiveGroupContracts =
  mapM_
    ( \(fixture, program, failures) -> do
        assertEqual (fixture <> " Haskell validation first run") failures (validateTypedProgram program)
        assertEqual (fixture <> " Haskell validation second run") failures (validateTypedProgram program)
    )
    recursiveGroupContractCases

testRecursiveGroupFixContracts :: IO ()
testRecursiveGroupFixContracts = do
  let expected = [(fixture, failures, failures) | (fixture, _, failures) <- recursiveGroupFixCases]
      actual =
        [ (fixture, validateTypedProgram program, validateTypedProgram program)
        | (fixture, program, _) <- recursiveGroupFixCases
        ]
  assertEqual "recursive-group review regressions on both Haskell runs" expected actual

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
    (("enforces typed-core value-shape contracts", testValueShapeRegressions), [bindingValueProgram, lambdaResultProgram, literalTypeProgram, collectionShapeProgram, dataTypeArityProgram, tuplePatternShapeProgram, moduleResultProgram, stagedModuleResultProgram, stagedBlockResultProgram, schemeDataTypeProgram, driveAbsoluteProgram]),
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
    (("preserves proven recursive block peers", testRecursiveBlockPeers), [recursiveBlockPeerProgram, sourceOrderedRecursiveVisibilityProgram]),
    (("validates declared root recursive groups exactly twice", testRecursiveGroupContracts), [program | (_, program, _) <- recursiveGroupContractCases]),
    (("preserves earliest-member ordering and malformed root visibility parity", testRecursiveGroupFixContracts), [program | (_, program, _) <- recursiveGroupFixCases]),
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
    (("preserves same-scope value rebinding", testSameScopeValueRebinding), [sameScopeValueRebindingProgram, nearestPriorBindingDependencyProgram]),
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
    (("stages callable representation substitutions", testCallableRepresentationSubstitution), [callableRepresentationSubstitutionProgram]),
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
    (("rejects duplicate constructors within one declaration", testDuplicateConstructorDeclaration), [duplicateConstructorDeclarationProgram]),
    (("covers every builtin catalog contract in hosted parity", testBuiltinCatalogParity), [builtinCatalogProgram, builtinDirectCallProgram]),
    (("stages constructor values outside complete calls", testConstructorValueRecipeRole), [constructorValueRecipeProgram]),
    (("stages polymorphic builtin values outside complete calls", testPolymorphicBuiltinRecipeRole), [polymorphicBuiltinRecipeProgram]),
    (("stages builtin operator values outside complete calls", testBuiltinOperatorValueRecipeRole), [builtinOperatorValueRecipeProgram]),
    (("rejects direct qualified methods in value position", testQualifiedDirectMethodValue), [qualifiedDirectMethodValueProgram]),
    (("stages callable results after a direct ABI root", testDirectCallableResultValue), [directCallableResultValueProgram]),
    (("checks implementation method callable shapes", testImplMethodCallableShape), [implMethodCallableShapeProgram]),
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

testCallableRepresentationSubstitution :: IO ()
testCallableRepresentationSubstitution =
  assertEqual
    "callable type arguments use staged value recipes"
    []
    (validateTypedProgram callableRepresentationSubstitutionProgram)

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
      operatorShapeFailure 4 0 "$operator:%7E",
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
      operatorShapeFailure 5 1 "$operator:%61",
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
      operatorShapeFailure 6 2 "$operator:%2B",
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
        (TypedTextDetail "->"),
      operatorShapeFailure 7 3 "$operator:%2D%3E"
    ]
    (validateTypedProgram invalidResolvedOperatorSymbolsProgram)
  where
    operatorShapeFailure statementIndex lexicalIndex encodedName =
      expressionFailureAt
        "review-invalid-resolved-operator-symbols"
        statementIndex
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            ( binder
                (fixtureModulePath "review-invalid-resolved-operator-symbols")
                [lexicalIndex]
                (TypedGeneratedName (TypedOperatorBinding encodedName))
            )
        )

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
        2
        TypedDuplicateDeclaration
        (TypedImplDetail duplicateOrderingImplId)
    ]
    (validateTypedProgram duplicateDeclarationOrderingProgram)

testDuplicateConstructorDeclaration :: IO ()
testDuplicateConstructorDeclaration =
  assertEqual
    "constructors may be rebound by later data declarations but not repeated within one declaration"
    [ statementFailure
        "review-duplicate-constructor-declaration"
        0
        TypedDuplicateDeclaration
        (TypedNameDetail (resolved TypedCurrentModule TypedConstructorNamespace "Choice"))
    ]
    (validateTypedProgram duplicateConstructorDeclarationProgram)

testBuiltinCatalogParity :: IO ()
testBuiltinCatalogParity = do
  assertEqual
    "every catalog builtin name and value contract validates"
    []
    (validateTypedProgram builtinCatalogProgram)
  assertEqual
    "a complete builtin call keeps its flattened direct recipe"
    []
    (validateTypedProgram builtinDirectCallProgram)

testConstructorValueRecipeRole :: IO ()
testConstructorValueRecipeRole =
  assertEqual
    "constructor values require staged recipes outside a complete call"
    [ expressionFailureAt
        "review-constructor-value-recipe"
        1
        TypedBindingValueMismatch
        (TypedRecipeDetail pairConstructorValueRecipe pairConstructorDirectRecipe)
    ]
    (validateTypedProgram constructorValueRecipeProgram)

testPolymorphicBuiltinRecipeRole :: IO ()
testPolymorphicBuiltinRecipeRole =
  assertEqual
    "polymorphic builtin values require staged recipes outside a complete call"
    [ expressionFailureAt
        "review-polymorphic-builtin-value-recipe"
        0
        TypedBindingValueMismatch
        (TypedRecipeDetail builtinMapValueRecipe builtinMapRecipe)
    ]
    (validateTypedProgram polymorphicBuiltinRecipeProgram)

testBuiltinOperatorValueRecipeRole :: IO ()
testBuiltinOperatorValueRecipeRole =
  assertEqual
    "builtin operator values require staged recipes outside a complete call"
    [ expressionFailureAt
        "review-builtin-operator-value-recipe"
        0
        TypedBindingValueMismatch
        (TypedRecipeDetail builtinIntOperatorValueRecipe builtinIntOperatorDirectRecipe)
    ]
    (validateTypedProgram builtinOperatorValueRecipeProgram)

testQualifiedDirectMethodValue :: IO ()
testQualifiedDirectMethodValue =
  assertEqual
    "direct-qualified methods cannot escape as values"
    [ expressionFailureAt
        "review-qualified-direct-method-value"
        0
        TypedCallableShapeMismatch
        (TypedBinderDetail qualifiedDirectMethodOwner)
    ]
    (validateTypedProgram qualifiedDirectMethodValueProgram)

testDirectCallableResultValue :: IO ()
testDirectCallableResultValue =
  assertEqual
    "callable results after a direct ABI root require unary staging"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-direct-callable-result-value") [0] [0, 0])
        TypedBindingValueMismatch
        (TypedRecipeDetail textAppendValueRecipe textAppendDirectRecipe)
    ]
    (validateTypedProgram directCallableResultValueProgram)

testImplMethodCallableShape :: IO ()
testImplMethodCallableShape =
  assertEqual
    "impl method bodies match their direct callable ABI width"
    [ statementFailure
        "review-impl-method-callable-shape"
        1
        TypedCallableShapeMismatch
        (TypedBinderDetail implMethodCallableShapeOwner)
    ]
    (validateTypedProgram implMethodCallableShapeProgram)

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
    "block result compares staged callable recipes"
    [ expressionFailure
        "review-staged-block-result"
        TypedBlockResultMismatch
        (TypedRecipeDetail flattenedCallableResultRecipe stagedCallableResultRecipe)
    ]
    (validateTypedProgram stagedBlockResultProgram)
  assertEqual
    "module result compares staged callable recipes"
    [ moduleFailure
        "review-staged-module-result"
        TypedModuleResultMismatch
        (TypedRecipeDetail flattenedCallableResultRecipe stagedCallableResultRecipe)
    ]
    (validateTypedProgram stagedModuleResultProgram)
  assertEqual
    "scheme data types require visible declarations"
    [statementFailure "review-scheme-data-type" 0 TypedDataTypeMismatch (TypedNameDetail missingSchemeDataName)]
    (validateTypedProgram schemeDataTypeProgram)
  assertEqual
    "drive-absolute source paths are rejected consistently"
    [moduleFailure "review-drive-absolute" TypedInvalidSourcePath (TypedTextDetail "C:/Fixture/Main.jz")]
    (validateTypedProgram driveAbsoluteProgram)

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
        ),
      statementFailure
        "review-impl-method-contract"
        1
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            ( binder
                (fixtureModulePath "review-impl-method-contract")
                [0, 0]
                (fixtureValueName "equal")
            )
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
        (TypedTextDetail "^"),
      expressionFailureAt
        "review-mismatched-resolved-operator"
        1
        TypedCallableShapeMismatch
        ( TypedBinderDetail
            ( binder
                (fixtureModulePath "review-mismatched-resolved-operator")
                [0]
                (TypedGeneratedName (TypedOperatorBinding "$operator:%7E"))
            )
        )
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
        (TypedEvidenceParameterDetail (TypedEvidenceParameterId 0)),
      expressionFailureAt
        "review-constrained-resolved-operator"
        1
        TypedCallableShapeMismatch
        (TypedBinderDetail constrainedResolvedOperatorOwner)
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

testNearestPriorBindingDependencies :: IO ()
testNearestPriorBindingDependencies =
  assertEqual
    "acyclic binding chains resolve a repeated name to its nearest prior declaration"
    []
    (validateTypedProgram nearestPriorBindingDependencyProgram)

testSourceOrderedRecursiveVisibility :: IO ()
testSourceOrderedRecursiveVisibility =
  assertEqual
    "recursive groups retain source order without leaking future peers to interleaved statements"
    [ expressionFailureAt
        "review-source-ordered-recursive-visibility"
        1
        TypedInvisibleName
        (TypedNameDetail (fixtureValueName "tail")),
      expressionFailureAt
        "review-source-ordered-recursive-visibility"
        3
        TypedInvisibleName
        (TypedNameDetail (fixtureValueName "tail")),
      expressionFailureAt
        "review-source-ordered-recursive-visibility"
        5
        TypedInvisibleName
        (TypedNameDetail (fixtureValueName "tail"))
    ]
    (validateTypedProgram sourceOrderedRecursiveVisibilityProgram)

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
    "alias-shaped root self references require callable recursive metadata"
    [ TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-alias-shaped-self-recursion") [0] [0, 1])
        TypedInvisibleName
        (TypedNameDetail (fixtureValueName "item")),
      TypedCoreValidationFailure
        (TypedExpressionPath (fixtureModulePath "review-alias-shaped-self-recursion") [0] [0, 1])
        TypedBinderReferenceMismatch
        (TypedBinderDetail (recursiveGroupOwnerAt "review-alias-shaped-self-recursion" 0 "item"))
    ]
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
