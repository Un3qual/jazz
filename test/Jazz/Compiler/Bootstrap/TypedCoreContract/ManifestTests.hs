{-# LANGUAGE OverloadedStrings #-}

-- | Fixed-manifest, adapter, and coverage contract tests.
module Jazz.Compiler.Bootstrap.TypedCoreContract.ManifestTests
  ( tests,
  )
where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedProgramRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    decodeCanonicalTypedValidationFailuresRuntimeValue,
  )
import Jazz.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
  )
import Jazz.Compiler.Bootstrap.TypedCoreContract.Fixtures
import Jazz.Compiler.Bootstrap.TypedCoreContract.RegressionTests (reviewRegressionPrograms)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( closureRecursionExpectedPrograms,
    directRecursionExpectedPrograms,
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
  )

tests :: [NamedTest]
tests = coreTests

coreTests :: [NamedTest]
coreTests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("accepts exact producer recursive artifacts", testProducerRecursionArtifacts),
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
  assertEqual "valid fixture count" 21 (length validFixtures)

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

testProducerRecursionArtifacts :: IO ()
testProducerRecursionArtifacts = do
  assertEqual
    "producer direct-recursion fixture names"
    ["self-recursive-function", "mutually-recursive-functions"]
    (map fst directRecursionExpectedPrograms)
  assertEqual
    "producer closure-recursion fixture names"
    [ "closure-value-mutual-recursion",
      "closure-value-self-recursion",
      "capturing-self-recursion",
      "capturing-mutual-recursion"
    ]
    (map fst closureRecursionExpectedPrograms)
  mapM_
    ( \(fixture, program) -> do
        assertEqual (fixture <> " producer artifact first validation") [] (validateTypedProgram program)
        assertEqual (fixture <> " producer artifact second validation") [] (validateTypedProgram program)
    )
    (directRecursionExpectedPrograms <> closureRecursionExpectedPrograms)

testInvalidFixtureManifest :: IO ()
testInvalidFixtureManifest = do
  assertEqual "invalid fixture names" expectedInvalidFixtureNames (map invalidFixtureName invalidFixtures)
  assertEqual "invalid fixture count" 56 (length invalidFixtures)

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
  assertEqual "combined fixture count" 77 (length validFixtures + length invalidFixtures)

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
