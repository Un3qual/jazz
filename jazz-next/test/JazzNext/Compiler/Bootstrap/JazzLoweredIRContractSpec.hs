{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLoweredIRComparison
  ( canonicalLoweredProgramRuntimeValue,
    canonicalLoweredProgramsRuntimeValue,
    canonicalLoweredValidationFailuresRuntimeValue,
    decodeCanonicalLoweredValidationFailuresRuntimeValue
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors
  )
import JazzNext.Compiler.LoweredIR
import JazzNext.Compiler.LoweredIR.Validate (validateLoweredProgram)
import JazzNext.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import JazzNext.Compiler.Name (identifierText)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue
  )
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
  )
import JazzNext.TestSource (readCheckedInJazzProjectModuleSource)

main :: IO ()
main = runTestSuite "JazzLoweredIRContract" tests

tests :: [NamedTest]
tests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("renders the scalar contract deterministically", testScalarContractRendering),
    ("renders the complete valid contract deterministically", testValidContractRendering),
    ("accepts every fixed valid program", testValidPrograms),
    ("audits the fixed invalid fixture manifest", testInvalidFixtureManifest),
    ("reports every fixed invalid program exactly", testInvalidPrograms),
    ("reports every validator hardening regression exactly", testHardeningPrograms),
    ("validates the exact UInt64 immediate domain", testUInt64ImmediateRange),
    ("matches Jazz UInt64 immediate boundaries twice", testJazzUInt64ImmediateRange),
    ("rejects malformed Jazz unsigned immediate text twice", testJazzMalformedUnsignedImmediate),
    ("rejects non-scalar character immediates", testUnicodeScalarImmediate),
    ("rejects variant tags outside the shared Haskell/Jazz carrier", testSharedTagCarrierRange),
    ("scopes temporary identifiers to their blocks", testBlockLocalTemporaryScope),
    ("preserves every duplicate variant tag in order", testDuplicateVariantTagOrder),
    ("preserves complete program failure order", testCompleteFailureOrder),
    ("round-trips canonical validation failures through the checked adapter", testCheckedValidationAdapterRoundTrip),
    ("rejects unknown validation constructors", testCheckedValidationAdapterUnknownConstructor),
    ("rejects wrong validation constructor arity", testCheckedValidationAdapterWrongArity),
    ("rejects wrong validation field categories", testCheckedValidationAdapterWrongFieldCategory),
    ("rejects malformed nested validation values", testCheckedValidationAdapterMalformedNestedValue),
    ("validates the minimal contract through real Jazz modules", testJazzMinimalValidation),
    ("matches Haskell validation for all 41 Jazz fixtures twice", testJazzValidationParity),
    ("matches Haskell validation for every hardening regression twice", testJazzHardeningParity)
  ]

testCheckedValidationAdapterRoundTrip :: IO ()
testCheckedValidationAdapterRoundTrip =
  mapM_
    ( \(name, failures) ->
        assertEqual
          (name <> " checked validation round-trip")
          (Right failures)
          (decodeCanonicalLoweredValidationFailuresRuntimeValue (canonicalLoweredValidationFailuresRuntimeValue failures))
    )
    ( [(invalidFixtureName fixture, invalidFixtureFailures fixture) | fixture <- invalidFixtures]
        <> [(hardeningFixtureName fixture, hardeningFixtureFailures fixture) | fixture <- hardeningFixtures]
    )

testCheckedValidationAdapterUnknownConstructor :: IO ()
testCheckedValidationAdapterUnknownConstructor =
  assertTextLeftContains
    "unknown validation constructor"
    "unknown validation failure constructor 'UnexpectedFailure'"
    (decodeCanonicalLoweredValidationFailuresRuntimeValue (VList [canonicalNullaryConstructor "UnexpectedFailure"] Nothing))

testCheckedValidationAdapterWrongArity :: IO ()
testCheckedValidationAdapterWrongArity =
  assertTextLeftContains
    "wrong validation failure arity"
    "LoweredIRValidationFailure expected 3 field(s), got 2"
    ( decodeCanonicalLoweredValidationFailuresRuntimeValue
        ( VList
            [ canonicalConstructor
                "LoweredIRValidationFailure"
                [canonicalNullaryConstructor "LoweredProgramPath", canonicalNullaryConstructor "LoweredMissingEntryFunction"]
            ]
            Nothing
        )
    )

testCheckedValidationAdapterWrongFieldCategory :: IO ()
testCheckedValidationAdapterWrongFieldCategory =
  assertTextLeftContains
    "wrong validation path field category"
    "validation path expected a constructor, got Text"
    ( decodeCanonicalLoweredValidationFailuresRuntimeValue
        ( VList
            [ canonicalConstructor
                "LoweredIRValidationFailure"
                [VText "not-a-path", canonicalNullaryConstructor "LoweredMissingEntryFunction", canonicalNullaryConstructor "LoweredNoValidationDetail"]
            ]
            Nothing
        )
    )

testCheckedValidationAdapterMalformedNestedValue :: IO ()
testCheckedValidationAdapterMalformedNestedValue =
  assertTextLeftContains
    "malformed nested layout identifier"
    "LoweredLayoutId expected 1 field(s), got 0"
    ( decodeCanonicalLoweredValidationFailuresRuntimeValue
        ( VList
            [ canonicalConstructor
                "LoweredIRValidationFailure"
                [ canonicalConstructor "LoweredLayoutPath" [canonicalNullaryConstructor "LoweredLayoutId"],
                  canonicalNullaryConstructor "LoweredUnknownLayout",
                  canonicalConstructor "LoweredIdentifierDetail" [VText "missing"]
                ]
            ]
            Nothing
        )
    )

assertTextLeftContains :: Show value => Text -> Text -> Either Text value -> IO ()
assertTextLeftContains label expected result =
  case result of
    Left actual -> assertContains label expected actual
    Right value -> failTest (label <> ": expected Left, got Right " <> Text.pack (show value))

testJazzValidationParity :: IO ()
testJazzValidationParity = do
  let programs = map validFixtureProgram validFixtures <> map invalidFixtureProgram invalidFixtures
      expected =
        renderRuntimeValue
          ( VList
              [ VTuple
                  [ canonicalLoweredProgramRuntimeValue programValue,
                    canonicalLoweredValidationFailuresRuntimeValue (validateLoweredProgram programValue)
                  ]
                | programValue <- programs
              ]
              Nothing
          )
  first <- runJazzValidationBatch programs
  second <- runJazzValidationBatch programs
  assertJazzOutput "Jazz validation first run" expected first
  assertJazzOutput "Jazz validation second run" expected second
  assertEqual "Jazz validation deterministic output" (runOutput first) (runOutput second)

testJazzHardeningParity :: IO ()
testJazzHardeningParity = do
  let programs = map hardeningFixtureProgram hardeningFixtures
      expected =
        renderRuntimeValue
          ( VList
              [ VTuple
                  [ canonicalLoweredProgramRuntimeValue programValue,
                    canonicalLoweredValidationFailuresRuntimeValue (validateLoweredProgram programValue)
                  ]
                | programValue <- programs
              ]
              Nothing
          )
  first <- runJazzValidationBatch programs
  second <- runJazzValidationBatch programs
  assertJazzOutput "Jazz hardening validation first run" expected first
  assertJazzOutput "Jazz hardening validation second run" expected second
  assertEqual "Jazz hardening validation deterministic output" (runOutput first) (runOutput second)

testJazzMinimalValidation :: IO ()
testJazzMinimalValidation = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "Jazz minimal compile errors" [] (runCompileErrors result)
  assertEqual "Jazz minimal runtime errors" [] (runRuntimeErrors result)
  assertEqual "Jazz minimal validation" (Just "[]") (runOutput result)
  where
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just jazzMinimalProgramSource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

jazzMinimalProgramSource :: Text
jazzMinimalProgramSource =
  """
  module App::Main {
    import LoweredIRTypes.
    import LoweredIRValidate (validateProgram).
    import Maybe.
    validateProgram
      (LoweredProgram
        (LoweredIRVersion 1)
        []
        []
        [LoweredFunction
          (LoweredFunctionId "main")
          Nothing
          []
          LoweredUnitRepresentation
          [LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            (Just (LoweredReturn (LoweredImmediateOperand LoweredUnitImmediate)))]
          (LoweredBlockId "entry")]
        (LoweredFunctionId "main")).
  }

  """

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

runJazzValidationBatch :: [LoweredProgram] -> IO RunResult
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

jazzValidationBatchSource :: [LoweredProgram] -> Text
jazzValidationBatchSource programs =
  Text.unlines
    [ "module App::Main {",
      "  import List (listMap).",
      "  import LoweredIRTypes.",
      "  import LoweredIRValidate (validateProgram).",
      "  import Maybe.",
      "  listMap",
      "    (\\(program) -> (program, validateProgram program))",
      "    [" <> Text.intercalate ", " (map (renderJazzRuntimeValue . canonicalLoweredProgramRuntimeValue) programs) <> "].",
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
    _ -> error "unsupported runtime value in generated lowered-IR fixture"

testValidPrograms :: IO ()
testValidPrograms =
  mapM_
    (\fixture -> assertEqual (validFixtureName fixture <> " validation") [] (validateLoweredProgram (validFixtureProgram fixture)))
    validFixtures

testInvalidFixtureManifest :: IO ()
testInvalidFixtureManifest = do
  assertEqual "invalid fixture names" expectedInvalidFixtureNames (map invalidFixtureName invalidFixtures)
  assertEqual "invalid fixture count" 31 (length invalidFixtures)
  assertEqual "complete fixture count" 41 (length validFixtures + length invalidFixtures)

testInvalidPrograms :: IO ()
testInvalidPrograms =
  mapM_
    ( \fixture ->
        assertEqual
          (invalidFixtureName fixture <> " failures")
          (invalidFixtureFailures fixture)
          (validateLoweredProgram (invalidFixtureProgram fixture))
    )
    invalidFixtures

testHardeningPrograms :: IO ()
testHardeningPrograms =
  mapM_
    ( \fixture ->
        assertEqual
          (hardeningFixtureName fixture <> " failures")
          (hardeningFixtureFailures fixture)
          (validateLoweredProgram (hardeningFixtureProgram fixture))
    )
    hardeningFixtures

testUInt64ImmediateRange :: IO ()
testUInt64ImmediateRange =
  mapM_
    ( \(name, programValue, expectedFailures) ->
        assertEqual
          (name <> " Haskell validation")
          expectedFailures
          (validateLoweredProgram programValue)
    )
    uint64ImmediateBoundaryCases

testJazzUInt64ImmediateRange :: IO ()
testJazzUInt64ImmediateRange = do
  let programs = [programValue | (_, programValue, _) <- uint64ImmediateBoundaryCases]
      expected =
        renderRuntimeValue
          ( VList
              [ VTuple
                  [ canonicalLoweredProgramRuntimeValue programValue,
                    canonicalLoweredValidationFailuresRuntimeValue expectedFailures
                  ]
                | (_, programValue, expectedFailures) <- uint64ImmediateBoundaryCases
              ]
              Nothing
          )
  first <- runJazzValidationBatch programs
  second <- runJazzValidationBatch programs
  assertJazzOutput "Jazz UInt64 boundary first run" expected first
  assertJazzOutput "Jazz UInt64 boundary second run" expected second
  assertEqual "Jazz UInt64 boundary deterministic output" (runOutput first) (runOutput second)

testJazzMalformedUnsignedImmediate :: IO ()
testJazzMalformedUnsignedImmediate = do
  let expected =
        renderRuntimeValue
          ( canonicalLoweredValidationFailuresRuntimeValue
              [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (unsigned LoweredIntegerWidth64))]
          )
  first <- runJazzMalformedUnsignedImmediate
  second <- runJazzMalformedUnsignedImmediate
  assertJazzOutput "Jazz malformed UInt64 first run" expected first
  assertJazzOutput "Jazz malformed UInt64 second run" expected second
  assertEqual "Jazz malformed UInt64 deterministic output" (runOutput first) (runOutput second)

runJazzMalformedUnsignedImmediate :: IO RunResult
runJazzMalformedUnsignedImmediate =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    ["App", "Main"]
    lookupSource
  where
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just jazzMalformedUnsignedImmediateSource)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

jazzMalformedUnsignedImmediateSource :: Text
jazzMalformedUnsignedImmediateSource =
  """
  module App::Main {
    import LoweredIRTypes.
    import LoweredIRValidate (validateProgram).
    import Maybe.
    validateProgram
      (LoweredProgram
        (LoweredIRVersion 1)
        []
        []
        [LoweredFunction
          (LoweredFunctionId "main")
          Nothing
          []
          (LoweredUnsignedIntegerRepresentation LoweredIntegerWidth64)
          [LoweredBlock
            (LoweredBlockId "entry")
            []
            []
            (Just
              (LoweredReturn
                (LoweredImmediateOperand
                  (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 "not-decimal"))))]
          (LoweredBlockId "entry")]
        (LoweredFunctionId "main")).
  }

  """

uint64ImmediateBoundaryCases :: [(Text, LoweredProgram, [LoweredIRValidationFailure])]
uint64ImmediateBoundaryCases =
  [ ( "first upper-half UInt64 immediate",
      immediateReturnProgram (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 9223372036854775808),
      []
    ),
    ( "maximum UInt64 immediate",
      immediateReturnProgram (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 18446744073709551615),
      []
    ),
    ( "first overflowing UInt64 immediate",
      immediateReturnProgram (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 18446744073709551616),
      [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (unsigned LoweredIntegerWidth64))]
    )
  ]

testUnicodeScalarImmediate :: IO ()
testUnicodeScalarImmediate =
  assertEqual
    "surrogate character immediate"
    [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail LoweredCharRepresentation)]
    (validateLoweredProgram (immediateReturnProgram (LoweredCharImmediate '\xD800')))

testSharedTagCarrierRange :: IO ()
testSharedTagCarrierRange = do
  assertEqual
    "upper-half UInt64 variant layout tag"
    [layoutFailure "option" LoweredTagOutOfRange (LoweredTagDetail 9223372036854775808)]
    (validateLoweredProgram upperHalfVariantLayoutTagProgram)
  assertEqual
    "upper-half UInt64 switch case tag"
    [terminatorFailure "main" "entry" LoweredTagOutOfRange (LoweredTagDetail 9223372036854775808)]
    (validateLoweredProgram upperHalfSwitchCaseTagProgram)

testBlockLocalTemporaryScope :: IO ()
testBlockLocalTemporaryScope =
  assertEqual "block-local temporary validation" [] (validateLoweredProgram blockLocalTemporaryProgram)

testDuplicateVariantTagOrder :: IO ()
testDuplicateVariantTagOrder =
  assertEqual
    "duplicate variant tag order"
    [ layoutFailure "choice" LoweredDuplicateVariantTag (LoweredTagDetail 1),
      layoutFailure "choice" LoweredDuplicateVariantTag (LoweredTagDetail 2)
    ]
    (validateLoweredProgram duplicateVariantTagsProgram)

testCompleteFailureOrder :: IO ()
testCompleteFailureOrder =
  assertEqual
    "complete failure order"
    [ layoutFailure "duplicate" LoweredDuplicateLayout (identifierDetail "duplicate"),
      programFailure LoweredMissingEntryFunction (identifierDetail "missing"),
      blockFailure "main" "entry" LoweredMissingTerminator LoweredNoValidationDetail
    ]
    (validateLoweredProgram completeFailureOrderProgram)

testValidFixtureManifest :: IO ()
testValidFixtureManifest = do
  assertEqual "valid fixture names" expectedValidFixtureNames (map validFixtureName validFixtures)
  assertEqual "valid fixture count" 10 (length validFixtures)

testValidContractRendering :: IO ()
testValidContractRendering = do
  let programs = map validFixtureProgram validFixtures
      first = renderRuntimeValue (canonicalLoweredProgramsRuntimeValue programs)
      second = renderRuntimeValue (canonicalLoweredProgramsRuntimeValue programs)
  mapM_ (\constructorName -> assertContains constructorName constructorName first) validConstructorInventory
  assertEqual "valid batch deterministic rendering" first second

testScalarContractRendering :: IO ()
testScalarContractRendering = do
  let first = renderRuntimeValue (canonicalLoweredProgramRuntimeValue minimalScalarProgram)
      second = renderRuntimeValue (canonicalLoweredProgramRuntimeValue minimalScalarProgram)
  assertEqual "scalar canonical rendering" expectedScalarRendering first
  assertEqual "scalar deterministic rendering" first second

scalarProgram :: LoweredProgram
scalarProgram =
  program
    []
    []
    ( scalarFunction "main" LoweredUnitRepresentation LoweredUnitImmediate
        : [ scalarFunction "bool" LoweredBoolRepresentation (LoweredBoolImmediate True),
            scalarFunction "i8" (signed LoweredIntegerWidth8) (LoweredSignedIntegerImmediate LoweredIntegerWidth8 (-8)),
            scalarFunction "i16" (signed LoweredIntegerWidth16) (LoweredSignedIntegerImmediate LoweredIntegerWidth16 (-16)),
            scalarFunction "i32" (signed LoweredIntegerWidth32) (LoweredSignedIntegerImmediate LoweredIntegerWidth32 (-32)),
            scalarFunction "i64" (signed LoweredIntegerWidth64) (LoweredSignedIntegerImmediate LoweredIntegerWidth64 (-64)),
            scalarFunction "u8" (unsigned LoweredIntegerWidth8) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 8),
            scalarFunction "u16" (unsigned LoweredIntegerWidth16) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth16 16),
            scalarFunction "u32" (unsigned LoweredIntegerWidth32) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth32 32),
            scalarFunction "u64" (unsigned LoweredIntegerWidth64) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 64),
            scalarFunction "f16" (float LoweredFloatWidth16) (LoweredFloatImmediate LoweredFloatWidth16 "1.5"),
            scalarFunction "f32" (float LoweredFloatWidth32) (LoweredFloatImmediate LoweredFloatWidth32 "2.5"),
            scalarFunction "f64" (float LoweredFloatWidth64) (LoweredFloatImmediate LoweredFloatWidth64 "3.5"),
            scalarFunction "char" LoweredCharRepresentation (LoweredCharImmediate 'λ')
          ]
    )
    "main"

minimalScalarProgram :: LoweredProgram
minimalScalarProgram =
  program
    []
    []
    [scalarFunction "main" LoweredUnitRepresentation LoweredUnitImmediate]
    "main"

branchJoinProgram :: LoweredProgram
branchJoinProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            []
            ( LoweredBranch
                (immediate (LoweredBoolImmediate True))
                (blockId "left")
                []
                (blockId "right")
                []
            ),
          block "left" [] [] (LoweredJump (blockId "join") [int64 1]),
          block "right" [] [] (LoweredJump (blockId "join") [int64 2]),
          block
            "join"
            [parameter "value" i64]
            []
            (LoweredReturn (blockParameter "value" i64))
        ]
        "entry"
    ]
    "main"

productProjectionProgram :: LoweredProgram
productProjectionProgram =
  program
    [LoweredLayout (layoutId "pair") (LoweredProductLayout [i64, LoweredBoolRepresentation])]
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            [ instruction "pair" (managed "pair") (LoweredConstructProduct (layoutId "pair") [int64 7, immediate (LoweredBoolImmediate True)]),
              instruction "value" i64 (LoweredProjectField (layoutId "pair") 0 (temporary "pair" (managed "pair")))
            ]
            (LoweredReturn (temporary "value" i64))
        ]
        "entry"
    ]
    "main"

variantSwitchProgram :: LoweredProgram
variantSwitchProgram =
  program
    [ LoweredLayout
        (layoutId "option")
        (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [i64]])
    ]
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            [instruction "option" (managed "option") (LoweredConstructVariant (layoutId "option") 1 [int64 9])]
            ( LoweredSwitch
                (temporary "option" (managed "option"))
                [ LoweredSwitchCase 0 (blockId "none") [],
                  LoweredSwitchCase 1 (blockId "some") [temporary "option" (managed "option")]
                ]
                (Just (LoweredSwitchDefault (blockId "default") []))
            ),
          block "none" [] [] (LoweredReturn (int64 0)),
          block
            "some"
            [parameter "option" (managed "option")]
            [ instruction
                "payload"
                i64
                (LoweredProjectVariantField (layoutId "option") 1 0 (blockParameter "option" (managed "option")))
            ]
            (LoweredReturn (temporary "payload" i64)),
          block "default" [] [] (LoweredReturn (int64 (-1)))
        ]
        "entry"
    ]
    "main"

directCallProgram :: LoweredProgram
directCallProgram =
  program [] [] [identityFunction, directCaller LoweredDirectCall LoweredReturn] "main"

directTailCallProgram :: LoweredProgram
directTailCallProgram =
  program
    []
    []
    [ identityFunction,
      function "main" Nothing [] i64 [block "entry" [] [] (LoweredDirectTailCall (functionId "identity") [int64 12])] "entry"
    ]
    "main"

closureCallProgram :: LoweredProgram
closureCallProgram = closureProgram False

closureTailCallProgram :: LoweredProgram
closureTailCallProgram = closureProgram True

runtimeServiceCallProgram :: LoweredProgram
runtimeServiceCallProgram =
  program
    [LoweredLayout (layoutId "text") LoweredTextLayout]
    [LoweredRuntimeService (serviceId "write-text") (LoweredCallSignature [managed "text"] LoweredUnitRepresentation)]
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [ instruction "text" (managed "text") (LoweredConstructText (layoutId "text") "hello"),
              instruction "result" LoweredUnitRepresentation (LoweredRuntimeCall (serviceId "write-text") [temporary "text" (managed "text")])
            ]
            (LoweredReturn (temporary "result" LoweredUnitRepresentation))
        ]
        "entry"
    ]
    "main"

textListLayoutsProgram :: LoweredProgram
textListLayoutsProgram =
  program
    [ LoweredLayout (layoutId "text") LoweredTextLayout,
      LoweredLayout (layoutId "texts") (LoweredListLayout (managed "text"))
    ]
    []
    [ function
        "main"
        Nothing
        []
        (managed "texts")
        [ block
            "entry"
            []
            [ instruction "text" (managed "text") (LoweredConstructText (layoutId "text") "item"),
              instruction "texts" (managed "texts") (LoweredConstructList (layoutId "texts") [temporary "text" (managed "text")])
            ]
            (LoweredReturn (temporary "texts" (managed "texts")))
        ]
        "entry"
    ]
    "main"

data ValidFixture = ValidFixture
  { validFixtureName :: Text,
    validFixtureProgram :: LoweredProgram
  }

validFixtures :: [ValidFixture]
validFixtures =
  [ ValidFixture "scalar-representations" scalarProgram,
    ValidFixture "branch-join" branchJoinProgram,
    ValidFixture "product-projection" productProjectionProgram,
    ValidFixture "variant-switch" variantSwitchProgram,
    ValidFixture "direct-call" directCallProgram,
    ValidFixture "direct-tail-call" directTailCallProgram,
    ValidFixture "closure-call" closureCallProgram,
    ValidFixture "closure-tail-call" closureTailCallProgram,
    ValidFixture "runtime-service-call" runtimeServiceCallProgram,
    ValidFixture "text-list-layouts" textListLayoutsProgram
  ]

expectedValidFixtureNames :: [Text]
expectedValidFixtureNames =
  [ "scalar-representations",
    "branch-join",
    "product-projection",
    "variant-switch",
    "direct-call",
    "direct-tail-call",
    "closure-call",
    "closure-tail-call",
    "runtime-service-call",
    "text-list-layouts"
  ]

validConstructorInventory :: [Text]
validConstructorInventory =
  [ "LoweredProductLayout",
    "LoweredVariantLayouts",
    "LoweredClosureEnvironmentLayout",
    "LoweredTextLayout",
    "LoweredListLayout",
    "LoweredFunctionParameterOperand",
    "LoweredBlockParameterOperand",
    "LoweredTemporaryOperand",
    "LoweredImmediateOperand",
    "LoweredConstructProduct",
    "LoweredConstructVariant",
    "LoweredConstructList",
    "LoweredConstructText",
    "LoweredConstructClosure",
    "LoweredProjectField",
    "LoweredProjectVariantField",
    "LoweredDirectCall",
    "LoweredClosureCall",
    "LoweredRuntimeCall",
    "LoweredReturn",
    "LoweredJump",
    "LoweredBranch",
    "LoweredSwitch",
    "LoweredDirectTailCall",
    "LoweredClosureTailCall"
  ]

data InvalidFixture = InvalidFixture
  { invalidFixtureName :: Text,
    invalidFixtureProgram :: LoweredProgram,
    invalidFixtureFailures :: [LoweredIRValidationFailure]
  }

invalidFixtures :: [InvalidFixture]
invalidFixtures =
  [ InvalidFixture
      "duplicate-layout"
      (unitProgram [productLayout "duplicate" [], productLayout "duplicate" []] [])
      [layoutFailure "duplicate" LoweredDuplicateLayout (identifierDetail "duplicate")],
    InvalidFixture
      "unknown-layout"
      (unitProgram [productLayout "holder" [managed "missing"]] [])
      [layoutFailure "holder" LoweredUnknownLayout (identifierDetail "missing")],
    InvalidFixture
      "duplicate-variant-tag"
      (unitProgram [LoweredLayout (layoutId "option") (LoweredVariantLayouts [LoweredVariantLayout 1 [], LoweredVariantLayout 1 [i64]])] [])
      [layoutFailure "option" LoweredDuplicateVariantTag (LoweredTagDetail 1)],
    InvalidFixture
      "duplicate-runtime-service"
      (unitProgram [] [unitService "duplicate", unitService "duplicate"])
      [serviceFailure "duplicate" LoweredDuplicateRuntimeService (identifierDetail "duplicate")],
    InvalidFixture
      "duplicate-function"
      (program [] [] [unitMain, unitMain] "main")
      [functionFailure "main" LoweredDuplicateFunction (identifierDetail "main")],
    InvalidFixture
      "missing-entry-function"
      (program [] [] [unitMain] "missing")
      [programFailure LoweredMissingEntryFunction (identifierDetail "missing")],
    InvalidFixture
      "duplicate-block"
      ( program
          []
          []
          [function "main" Nothing [] LoweredUnitRepresentation [unitBlock "entry", unitBlock "entry"] "entry"]
          "main"
      )
      [blockFailure "main" "entry" LoweredDuplicateBlock (identifierDetail "entry")],
    InvalidFixture
      "missing-entry-block"
      (program [] [] [function "main" Nothing [] LoweredUnitRepresentation [unitBlock "body"] "missing"] "main")
      [functionFailure "main" LoweredMissingEntryBlock (identifierDetail "missing")],
    InvalidFixture
      "missing-terminator"
      ( program
          []
          []
          [LoweredFunction (functionId "main") Nothing [] LoweredUnitRepresentation [LoweredBlock (blockId "entry") [] [] Nothing] (blockId "entry")]
          "main"
      )
      [blockFailure "main" "entry" LoweredMissingTerminator LoweredNoValidationDetail],
    InvalidFixture
      "duplicate-temporary"
      (instructionUnitProgram [addInstruction "value" 1 2, addInstruction "value" 3 4])
      [instructionFailure "main" "entry" 1 LoweredDuplicateTemporary (identifierDetail "value")],
    InvalidFixture
      "use-before-definition"
      ( instructionUnitProgram
          [ instruction
              "first"
              i64
              (LoweredPrimitiveOperation (LoweredArithmeticPrimitive LoweredAdd) [temporary "later" i64, int64 1]),
            addInstruction "later" 2 3
          ]
      )
      [instructionFailure "main" "entry" 0 LoweredUseBeforeDefinition (identifierDetail "later")],
    InvalidFixture
      "cross-block-temporary"
      ( program
          []
          []
          [ function
              "main"
              Nothing
              []
              i64
              [ block "entry" [] [addInstruction "value" 1 2] (LoweredJump (blockId "next") []),
                block "next" [] [] (LoweredReturn (temporary "value" i64))
              ]
              "entry"
          ]
          "main"
      )
      [terminatorFailure "main" "next" LoweredCrossBlockTemporary (identifierDetail "value")],
    InvalidFixture
      "unknown-parameter"
      (program [] [] [function "main" Nothing [] LoweredUnitRepresentation [block "entry" [] [] (LoweredReturn (functionParameter "missing" LoweredUnitRepresentation))] "entry"] "main")
      [terminatorFailure "main" "entry" LoweredUnknownParameter (identifierDetail "missing")],
    InvalidFixture
      "unknown-function-call"
      ( callInstructionProgram
          LoweredUnitRepresentation
          (LoweredDirectCall (functionId "missing") [])
          []
      )
      [instructionFailure "main" "entry" 0 LoweredUnknownFunction (identifierDetail "missing")],
    InvalidFixture
      "unknown-block-target"
      (program [] [] [function "main" Nothing [] LoweredUnitRepresentation [block "entry" [] [] (LoweredJump (blockId "missing") [])] "entry"] "main")
      [terminatorFailure "main" "entry" LoweredUnknownBlock (identifierDetail "missing")],
    InvalidFixture
      "instruction-result-representation"
      ( instructionReturnProgram
          LoweredBoolRepresentation
          [instruction "value" LoweredBoolRepresentation (LoweredPrimitiveOperation (LoweredArithmeticPrimitive LoweredAdd) [int64 1, int64 2])]
          (temporary "value" LoweredBoolRepresentation)
      )
      [instructionFailure "main" "entry" 0 LoweredInstructionResultRepresentationMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "invalid-field-projection"
      ( projectionProgram
          (productLayout "pair" [i64])
          i64
          (LoweredProjectField (layoutId "pair") 1 (temporary "value" (managed "pair")))
      )
      [instructionFailure "main" "entry" 1 LoweredInvalidFieldProjection (LoweredIndexDetail 1)],
    InvalidFixture
      "invalid-tag-projection"
      ( projectionProgram
          (productLayout "pair" [i64])
          (unsigned LoweredIntegerWidth64)
          (LoweredProjectVariantTag (layoutId "pair") (temporary "value" (managed "pair")))
      )
      [instructionFailure "main" "entry" 1 LoweredInvalidTagProjection LoweredNoValidationDetail],
    InvalidFixture
      "closure-environment-layout"
      closureEnvironmentMismatchProgram
      [instructionFailure "main" "entry" 1 LoweredClosureEnvironmentMismatch (representationDetail (managed "environment") (managed "wrong-environment"))],
    InvalidFixture
      "jump-argument-arity"
      (edgeProgram (LoweredJump (blockId "target") []) i64)
      [terminatorFailure "main" "entry" LoweredEdgeArityMismatch (LoweredArityDetail 1 0)],
    InvalidFixture
      "jump-argument-representation"
      (edgeProgram (LoweredJump (blockId "target") [immediate (LoweredBoolImmediate True)]) i64)
      [terminatorFailure "main" "entry" LoweredEdgeRepresentationMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "branch-non-boolean"
      ( branchProgram
          (int64 1)
          (block "yes" [] [] (LoweredReturn (immediate LoweredUnitImmediate)))
          (block "no" [] [] (LoweredReturn (immediate LoweredUnitImmediate)))
      )
      [terminatorFailure "main" "entry" LoweredBranchConditionMismatch (representationDetail LoweredBoolRepresentation i64)],
    InvalidFixture
      "switch-duplicate-case-tag"
      (switchProgram [LoweredSwitchCase 0 (blockId "target") [], LoweredSwitchCase 0 (blockId "target") []] (unitBlock "target"))
      [terminatorFailure "main" "entry" LoweredDuplicateSwitchCaseTag (LoweredTagDetail 0)],
    InvalidFixture
      "switch-target-arguments"
      ( switchProgram
          [LoweredSwitchCase 0 (blockId "target") []]
          (block "target" [parameter "value" i64] [] (LoweredReturn (immediate LoweredUnitImmediate)))
      )
      [terminatorFailure "main" "entry" LoweredEdgeArityMismatch (LoweredArityDetail 1 0)],
    InvalidFixture
      "return-representation"
      (program [] [] [function "main" Nothing [] i64 [block "entry" [] [] (LoweredReturn (immediate (LoweredBoolImmediate True)))] "entry"] "main")
      [terminatorFailure "main" "entry" LoweredReturnRepresentationMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "direct-call-signature"
      ( callInstructionProgram
          i64
          (LoweredDirectCall (functionId "identity") [immediate (LoweredBoolImmediate True)])
          [identityFunction]
      )
      [instructionFailure "main" "entry" 0 LoweredDirectCallSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "closure-call-signature"
      (invalidClosureCallProgram False)
      [instructionFailure "main" "entry" 2 LoweredClosureCallSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "runtime-call-signature"
      ( callInstructionProgram
          LoweredUnitRepresentation
          (LoweredRuntimeCall (serviceId "consume") [immediate (LoweredBoolImmediate True)])
          []
      )
      [instructionFailure "main" "entry" 0 LoweredRuntimeCallSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "direct-tail-signature"
      ( program
          []
          []
          [ identityFunction,
            function "main" Nothing [] i64 [block "entry" [] [] (LoweredDirectTailCall (functionId "identity") [immediate (LoweredBoolImmediate True)])] "entry"
          ]
          "main"
      )
      [terminatorFailure "main" "entry" LoweredDirectTailCallSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "closure-tail-signature"
      (invalidClosureCallProgram True)
      [terminatorFailure "main" "entry" LoweredClosureTailCallSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    InvalidFixture
      "unknown-runtime-service"
      (callInstructionProgram LoweredUnitRepresentation (LoweredRuntimeCall (serviceId "missing") []) [])
      [instructionFailure "main" "entry" 0 LoweredUnknownRuntimeService (identifierDetail "missing")]
  ]

expectedInvalidFixtureNames :: [Text]
expectedInvalidFixtureNames =
  [ "duplicate-layout",
    "unknown-layout",
    "duplicate-variant-tag",
    "duplicate-runtime-service",
    "duplicate-function",
    "missing-entry-function",
    "duplicate-block",
    "missing-entry-block",
    "missing-terminator",
    "duplicate-temporary",
    "use-before-definition",
    "cross-block-temporary",
    "unknown-parameter",
    "unknown-function-call",
    "unknown-block-target",
    "instruction-result-representation",
    "invalid-field-projection",
    "invalid-tag-projection",
    "closure-environment-layout",
    "jump-argument-arity",
    "jump-argument-representation",
    "branch-non-boolean",
    "switch-duplicate-case-tag",
    "switch-target-arguments",
    "return-representation",
    "direct-call-signature",
    "closure-call-signature",
    "runtime-call-signature",
    "direct-tail-signature",
    "closure-tail-signature",
    "unknown-runtime-service"
  ]

blockLocalTemporaryProgram :: LoweredProgram
blockLocalTemporaryProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block "entry" [] [addInstruction "value" 1 2] (LoweredReturn (temporary "value" i64)),
          block "other" [] [addInstruction "value" 3 4] (LoweredReturn (temporary "value" i64))
        ]
        "entry"
    ]
    "main"

duplicateVariantTagsProgram :: LoweredProgram
duplicateVariantTagsProgram =
  unitProgram
    [ LoweredLayout
        (layoutId "choice")
        ( LoweredVariantLayouts
            [ LoweredVariantLayout 1 [],
              LoweredVariantLayout 1 [i64],
              LoweredVariantLayout 2 [],
              LoweredVariantLayout 2 [LoweredBoolRepresentation]
            ]
        )
    ]
    []

completeFailureOrderProgram :: LoweredProgram
completeFailureOrderProgram =
  LoweredProgram
    (LoweredIRVersion 1)
    [productLayout "duplicate" [], productLayout "duplicate" []]
    []
    [LoweredFunction (functionId "main") Nothing [] LoweredUnitRepresentation [LoweredBlock (blockId "entry") [] [] Nothing] (blockId "entry")]
    (functionId "missing")

data HardeningFixture = HardeningFixture
  { hardeningFixtureName :: Text,
    hardeningFixtureProgram :: LoweredProgram,
    hardeningFixtureFailures :: [LoweredIRValidationFailure]
  }

hardeningFixtures :: [HardeningFixture]
hardeningFixtures =
  [ HardeningFixture
      "unsupported lowered IR version"
      unsupportedVersionProgram
      [programFailure LoweredUnsupportedVersion (LoweredVersionDetail supportedLoweredIRVersion (LoweredIRVersion 2))],
    HardeningFixture
      "negative unsigned 8-bit immediate"
      (immediateReturnProgram (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 (-1)))
      [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (unsigned LoweredIntegerWidth8))],
    HardeningFixture
      "overflowing unsigned 8-bit immediate"
      (immediateReturnProgram (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 256))
      [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (unsigned LoweredIntegerWidth8))],
    HardeningFixture
      "underflowing signed 8-bit immediate"
      (immediateReturnProgram (LoweredSignedIntegerImmediate LoweredIntegerWidth8 (-129)))
      [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (signed LoweredIntegerWidth8))],
    HardeningFixture
      "overflowing signed 8-bit immediate"
      (immediateReturnProgram (LoweredSignedIntegerImmediate LoweredIntegerWidth8 128))
      [terminatorFailure "main" "entry" LoweredImmediateOutOfRange (LoweredImmediateRangeDetail (signed LoweredIntegerWidth8))],
    HardeningFixture
      "negative variant layout tag"
      negativeVariantLayoutTagProgram
      [layoutFailure "option" LoweredTagOutOfRange (LoweredTagDetail (-1))],
    HardeningFixture
      "negative switch case tag"
      negativeSwitchCaseTagProgram
      [terminatorFailure "main" "entry" LoweredTagOutOfRange (LoweredTagDetail (-1))],
    HardeningFixture
      "missing variant field tag"
      missingVariantFieldTagProgram
      [instructionFailure "main" "entry" 1 LoweredInvalidTagProjection (LoweredTagDetail 1)],
    HardeningFixture
      "zero-operand arithmetic"
      (primitiveInstructionProgram i64 (LoweredArithmeticPrimitive LoweredAdd) [])
      [instructionFailure "main" "entry" 0 LoweredPrimitiveSignatureMismatch (LoweredArityDetail 2 0)],
    HardeningFixture
      "mixed-representation arithmetic"
      (primitiveInstructionProgram i64 (LoweredArithmeticPrimitive LoweredAdd) [int64 1, immediate (LoweredBoolImmediate True)])
      [instructionFailure "main" "entry" 0 LoweredPrimitiveSignatureMismatch (representationDetail i64 LoweredBoolRepresentation)],
    HardeningFixture
      "integer boolean operation"
      (primitiveInstructionProgram LoweredBoolRepresentation (LoweredBooleanPrimitive LoweredBooleanAnd) [int64 1, int64 0])
      [instructionFailure "main" "entry" 0 LoweredPrimitiveSignatureMismatch (representationDetail LoweredBoolRepresentation i64)],
    HardeningFixture
      "scalar switch"
      scalarSwitchProgram
      [terminatorFailure "main" "entry" LoweredInvalidTagProjection LoweredNoValidationDetail],
    HardeningFixture
      "non-variant managed switch"
      productSwitchProgram
      [terminatorFailure "main" "entry" LoweredInvalidTagProjection LoweredNoValidationDetail],
    HardeningFixture
      "unknown variant switch tag"
      (switchProgram [LoweredSwitchCase 1 (blockId "target") []] (unitBlock "target"))
      [ terminatorFailure "main" "entry" LoweredInvalidTagProjection (LoweredTagDetail 1),
        terminatorFailure "main" "entry" LoweredMissingSwitchCaseTag (LoweredTagDetail 0)
      ],
    HardeningFixture
      "switch without default misses variant tag"
      partialSwitchCoverageProgram
      [terminatorFailure "main" "entry" LoweredMissingSwitchCaseTag (LoweredTagDetail 1)],
    HardeningFixture
      "capturing direct call"
      (capturingDirectCallProgram False)
      [instructionFailure "main" "entry" 0 LoweredDirectCallSignatureMismatch LoweredNoValidationDetail],
    HardeningFixture
      "capturing direct tail call"
      (capturingDirectCallProgram True)
      [terminatorFailure "main" "entry" LoweredDirectTailCallSignatureMismatch LoweredNoValidationDetail],
    HardeningFixture
      "scalar closure environment"
      (invalidClosureEnvironmentKindProgram False)
      [ functionFailure "captured" LoweredClosureEnvironmentMismatch LoweredNoValidationDetail,
        instructionFailure "main" "entry" 0 LoweredClosureEnvironmentMismatch LoweredNoValidationDetail
      ],
    HardeningFixture
      "product closure environment"
      (invalidClosureEnvironmentKindProgram True)
      [ functionFailure "captured" LoweredClosureEnvironmentMismatch (identifierDetail "environment"),
        instructionFailure "main" "entry" 1 LoweredClosureEnvironmentMismatch (identifierDetail "environment")
      ],
    HardeningFixture
      "duplicate function parameter"
      duplicateFunctionParameterProgram
      [functionFailure "duplicate" LoweredDuplicateParameter (identifierDetail "value")],
    HardeningFixture
      "duplicate block parameter"
      duplicateBlockParameterProgram
      [blockFailure "main" "join" LoweredDuplicateParameter (identifierDetail "value")],
    HardeningFixture
      "parameterized entry block"
      parameterizedEntryBlockProgram
      [blockFailure "main" "entry" LoweredEntryBlockParameters (LoweredArityDetail 0 1)],
    HardeningFixture
      "parameterized entry function"
      parameterizedEntryFunctionProgram
      [functionFailure "main" LoweredEntryFunctionParameters (LoweredArityDetail 0 1)],
    HardeningFixture
      "capturing entry function"
      capturingEntryFunctionProgram
      [functionFailure "main" LoweredEntryFunctionParameters (LoweredArityDetail 0 1)],
    HardeningFixture
      "jump to entry block"
      entryIncomingJumpProgram
      [terminatorFailure "main" "loop" LoweredEntryBlockIncomingEdge (identifierDetail "entry")],
    HardeningFixture
      "branch to entry block"
      entryIncomingBranchProgram
      [terminatorFailure "main" "loop" LoweredEntryBlockIncomingEdge (identifierDetail "entry")],
    HardeningFixture
      "switch to entry block"
      entryIncomingSwitchProgram
      [terminatorFailure "main" "loop" LoweredEntryBlockIncomingEdge (identifierDetail "entry")],
    HardeningFixture
      "unknown managed result"
      (unknownInstructionResultProgram (managed "missing"))
      [ instructionFailure "main" "entry" 0 LoweredUnknownFunction (identifierDetail "missing"),
        instructionFailure "main" "entry" 0 LoweredUnknownLayout (identifierDetail "missing")
      ],
    HardeningFixture
      "unknown nested closure result"
      (unknownInstructionResultProgram (LoweredClosureRepresentation (LoweredCallSignature [managed "missing"] LoweredUnitRepresentation)))
      [ instructionFailure "main" "entry" 0 LoweredUnknownFunction (identifierDetail "missing"),
        instructionFailure "main" "entry" 0 LoweredUnknownLayout (identifierDetail "missing")
      ]
  ]

primitiveInstructionProgram :: LoweredRepresentation -> LoweredPrimitive -> [LoweredOperand] -> LoweredProgram
primitiveInstructionProgram resultRepresentation primitive operands =
  instructionUnitProgram
    [instruction "value" resultRepresentation (LoweredPrimitiveOperation primitive operands)]

scalarSwitchProgram :: LoweredProgram
scalarSwitchProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            []
            (LoweredSwitch (immediate (LoweredBoolImmediate True)) [LoweredSwitchCase 0 (blockId "target") []] Nothing),
          unitBlock "target"
        ]
        "entry"
    ]
    "main"

productSwitchProgram :: LoweredProgram
productSwitchProgram =
  program
    [productLayout "product" []]
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [instruction "product" (managed "product") (LoweredConstructProduct (layoutId "product") [])]
            (LoweredSwitch (temporary "product" (managed "product")) [LoweredSwitchCase 0 (blockId "target") []] Nothing),
          unitBlock "target"
        ]
        "entry"
    ]
    "main"

capturingDirectCallProgram :: Bool -> LoweredProgram
capturingDirectCallProgram useTailCall =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [])]
    []
    [capturedUnitFunction, mainFunction]
    "main"
  where
    capturedUnitFunction =
      function
        "captured"
        (Just (parameter "environment" (managed "environment")))
        []
        LoweredUnitRepresentation
        [unitBlock "entry"]
        "entry"
    mainFunction
      | useTailCall =
          function
            "main"
            Nothing
            []
            LoweredUnitRepresentation
            [block "entry" [] [] (LoweredDirectTailCall (functionId "captured") [])]
            "entry"
      | otherwise =
          function
            "main"
            Nothing
            []
            LoweredUnitRepresentation
            [ block
                "entry"
                []
                [instruction "result" LoweredUnitRepresentation (LoweredDirectCall (functionId "captured") [])]
                (LoweredReturn (temporary "result" LoweredUnitRepresentation))
            ]
            "entry"

invalidClosureEnvironmentKindProgram :: Bool -> LoweredProgram
invalidClosureEnvironmentKindProgram useProductLayout =
  program
    layouts
    []
    [ capturedFunction,
      function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [block "entry" [] setup (LoweredReturn (immediate LoweredUnitImmediate))]
        "entry"
    ]
    "main"
  where
    environmentRepresentation
      | useProductLayout = managed "environment"
      | otherwise = i64
    environmentOperand
      | useProductLayout = temporary "environment" (managed "environment")
      | otherwise = int64 0
    layouts
      | useProductLayout = [productLayout "environment" []]
      | otherwise = []
    capturedFunction =
      function
        "captured"
        (Just (parameter "environment" environmentRepresentation))
        []
        LoweredUnitRepresentation
        [unitBlock "entry"]
        "entry"
    setup =
      ( if useProductLayout
          then [instruction "environment" (managed "environment") (LoweredConstructProduct (layoutId "environment") [])]
          else []
      )
        <> [ instruction
               "closure"
               (LoweredClosureRepresentation (LoweredCallSignature [] LoweredUnitRepresentation))
               (LoweredConstructClosure (functionId "captured") environmentOperand)
           ]

duplicateFunctionParameterProgram :: LoweredProgram
duplicateFunctionParameterProgram =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [])]
    []
    [ function
        "duplicate"
        (Just (parameter "value" (managed "environment")))
        [parameter "value" i64]
        LoweredUnitRepresentation
        [unitBlock "entry"]
        "entry",
      unitMain
    ]
    "main"

duplicateBlockParameterProgram :: LoweredProgram
duplicateBlockParameterProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block "entry" [] [] (LoweredJump (blockId "join") [int64 1, int64 2]),
          block
            "join"
            [parameter "value" i64, parameter "value" i64]
            []
            (LoweredReturn (immediate LoweredUnitImmediate))
        ]
        "entry"
    ]
    "main"

parameterizedEntryBlockProgram :: LoweredProgram
parameterizedEntryBlockProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        i64
        [block "entry" [parameter "value" i64] [] (LoweredReturn (blockParameter "value" i64))]
        "entry"
    ]
    "main"

parameterizedEntryFunctionProgram :: LoweredProgram
parameterizedEntryFunctionProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        [parameter "value" i64]
        i64
        [block "entry" [] [] (LoweredReturn (functionParameter "value" i64))]
        "entry"
    ]
    "main"

capturingEntryFunctionProgram :: LoweredProgram
capturingEntryFunctionProgram =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [])]
    []
    [ function
        "main"
        (Just (parameter "environment" (managed "environment")))
        []
        LoweredUnitRepresentation
        [unitBlock "entry"]
        "entry"
    ]
    "main"

entryIncomingJumpProgram :: LoweredProgram
entryIncomingJumpProgram =
  entryIncomingEdgeProgram
    []
    []
    (LoweredJump (blockId "entry") [])

entryIncomingBranchProgram :: LoweredProgram
entryIncomingBranchProgram =
  entryIncomingEdgeProgram
    []
    []
    ( LoweredBranch
        (immediate (LoweredBoolImmediate True))
        (blockId "entry")
        []
        (blockId "exit")
        []
    )

entryIncomingSwitchProgram :: LoweredProgram
entryIncomingSwitchProgram =
  entryIncomingEdgeProgram
    [LoweredLayout (layoutId "option") (LoweredVariantLayouts [LoweredVariantLayout 0 []])]
    [instruction "option" (managed "option") (LoweredConstructVariant (layoutId "option") 0 [])]
    ( LoweredSwitch
        (temporary "option" (managed "option"))
        [LoweredSwitchCase 0 (blockId "entry") []]
        (Just (LoweredSwitchDefault (blockId "exit") []))
    )

entryIncomingEdgeProgram :: [LoweredLayout] -> [LoweredInstruction] -> LoweredTerminator -> LoweredProgram
entryIncomingEdgeProgram layouts loopInstructions loopTerminator =
  program
    layouts
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ unitBlock "entry",
          block "loop" [] loopInstructions loopTerminator,
          unitBlock "exit"
        ]
        "entry"
    ]
    "main"

unknownInstructionResultProgram :: LoweredRepresentation -> LoweredProgram
unknownInstructionResultProgram resultRepresentation =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [instruction "result" resultRepresentation (LoweredDirectCall (functionId "missing") [])]
            (LoweredReturn (immediate LoweredUnitImmediate))
        ]
        "entry"
    ]
    "main"

unsupportedVersionProgram :: LoweredProgram
unsupportedVersionProgram =
  case minimalScalarProgram of
    LoweredProgram _ layouts services functions entryFunction ->
      LoweredProgram (LoweredIRVersion 2) layouts services functions entryFunction

immediateReturnProgram :: LoweredImmediate -> LoweredProgram
immediateReturnProgram immediateValue =
  let resultRepresentation = loweredImmediateRepresentation immediateValue
   in program
        []
        []
        [ function
            "main"
            Nothing
            []
            resultRepresentation
            [block "entry" [] [] (LoweredReturn (immediate immediateValue))]
            "entry"
        ]
        "main"

negativeVariantLayoutTagProgram :: LoweredProgram
negativeVariantLayoutTagProgram =
  unitProgram
    [LoweredLayout (layoutId "option") (LoweredVariantLayouts [LoweredVariantLayout (-1) []])]
    []

upperHalfVariantLayoutTagProgram :: LoweredProgram
upperHalfVariantLayoutTagProgram =
  unitProgram
    [LoweredLayout (layoutId "option") (LoweredVariantLayouts [LoweredVariantLayout 9223372036854775808 []])]
    []

negativeSwitchCaseTagProgram :: LoweredProgram
negativeSwitchCaseTagProgram =
  switchProgramWith
    [LoweredVariantLayout 0 []]
    [LoweredSwitchCase (-1) (blockId "target") []]
    (Just (LoweredSwitchDefault (blockId "target") []))
    (unitBlock "target")

upperHalfSwitchCaseTagProgram :: LoweredProgram
upperHalfSwitchCaseTagProgram =
  switchProgramWith
    [LoweredVariantLayout 0 []]
    [LoweredSwitchCase 9223372036854775808 (blockId "target") []]
    (Just (LoweredSwitchDefault (blockId "target") []))
    (unitBlock "target")

partialSwitchCoverageProgram :: LoweredProgram
partialSwitchCoverageProgram =
  switchProgramWith
    [LoweredVariantLayout 0 [], LoweredVariantLayout 1 []]
    [LoweredSwitchCase 0 (blockId "target") []]
    Nothing
    (unitBlock "target")

missingVariantFieldTagProgram :: LoweredProgram
missingVariantFieldTagProgram =
  program
    [LoweredLayout (layoutId "option") (LoweredVariantLayouts [LoweredVariantLayout 0 []])]
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [ instruction "option" (managed "option") (LoweredConstructVariant (layoutId "option") 0 []),
              instruction
                "payload"
                i64
                (LoweredProjectVariantField (layoutId "option") 1 0 (temporary "option" (managed "option")))
            ]
            (LoweredReturn (immediate LoweredUnitImmediate))
        ]
        "entry"
    ]
    "main"

unitProgram :: [LoweredLayout] -> [LoweredRuntimeService] -> LoweredProgram
unitProgram layouts services = program layouts services [unitMain] "main"

unitMain :: LoweredFunction
unitMain = scalarFunction "main" LoweredUnitRepresentation LoweredUnitImmediate

unitBlock :: Text -> LoweredBlock
unitBlock name = block name [] [] (LoweredReturn (immediate LoweredUnitImmediate))

unitService :: Text -> LoweredRuntimeService
unitService name = LoweredRuntimeService (serviceId name) (LoweredCallSignature [] LoweredUnitRepresentation)

productLayout :: Text -> [LoweredRepresentation] -> LoweredLayout
productLayout name = LoweredLayout (layoutId name) . LoweredProductLayout

addInstruction :: Text -> Integer -> Integer -> LoweredInstruction
addInstruction name left right =
  instruction name i64 (LoweredPrimitiveOperation (LoweredArithmeticPrimitive LoweredAdd) [int64 left, int64 right])

instructionUnitProgram :: [LoweredInstruction] -> LoweredProgram
instructionUnitProgram instructions =
  program [] [] [function "main" Nothing [] LoweredUnitRepresentation [block "entry" [] instructions (LoweredReturn (immediate LoweredUnitImmediate))] "entry"] "main"

instructionReturnProgram :: LoweredRepresentation -> [LoweredInstruction] -> LoweredOperand -> LoweredProgram
instructionReturnProgram resultRepresentation instructions resultOperand =
  program [] [] [function "main" Nothing [] resultRepresentation [block "entry" [] instructions (LoweredReturn resultOperand)] "entry"] "main"

callInstructionProgram :: LoweredRepresentation -> LoweredOperation -> [LoweredFunction] -> LoweredProgram
callInstructionProgram resultRepresentation operation additionalFunctions =
  program
    []
    runtimeServices
    ( additionalFunctions
        <> [ function
               "main"
               Nothing
               []
               resultRepresentation
               [block "entry" [] [instruction "result" resultRepresentation operation] (LoweredReturn (temporary "result" resultRepresentation))]
               "entry"
           ]
    )
    "main"
  where
    runtimeServices =
      case operation of
        LoweredRuntimeCall service _
          | service == serviceId "consume" ->
              [LoweredRuntimeService service (LoweredCallSignature [i64] LoweredUnitRepresentation)]
        _ -> []

projectionProgram :: LoweredLayout -> LoweredRepresentation -> LoweredOperation -> LoweredProgram
projectionProgram layout resultRepresentation projection =
  program
    [layout]
    []
    [ function
        "main"
        Nothing
        []
        resultRepresentation
        [ block
            "entry"
            []
            [ instruction "value" (managed "pair") (LoweredConstructProduct (layoutId "pair") [int64 1]),
              instruction "projection" resultRepresentation projection
            ]
            (LoweredReturn (temporary "projection" resultRepresentation))
        ]
        "entry"
    ]
    "main"

closureEnvironmentMismatchProgram :: LoweredProgram
closureEnvironmentMismatchProgram =
  program
    [ LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [i64]),
      LoweredLayout (layoutId "wrong-environment") (LoweredClosureEnvironmentLayout [i64])
    ]
    []
    [ function
        "captured"
        (Just (parameter "environment" (managed "environment")))
        []
        LoweredUnitRepresentation
        [unitBlock "entry"]
        "entry",
      function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [ instruction "environment" (managed "wrong-environment") (LoweredConstructProduct (layoutId "wrong-environment") [int64 1]),
              instruction
                "closure"
                (LoweredClosureRepresentation (LoweredCallSignature [] LoweredUnitRepresentation))
                (LoweredConstructClosure (functionId "captured") (temporary "environment" (managed "wrong-environment")))
            ]
            (LoweredReturn (immediate LoweredUnitImmediate))
        ]
        "entry"
    ]
    "main"

edgeProgram :: LoweredTerminator -> LoweredRepresentation -> LoweredProgram
edgeProgram edge targetRepresentation =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block "entry" [] [] edge,
          block "target" [parameter "value" targetRepresentation] [] (LoweredReturn (immediate LoweredUnitImmediate))
        ]
        "entry"
    ]
    "main"

branchProgram :: LoweredOperand -> LoweredBlock -> LoweredBlock -> LoweredProgram
branchProgram condition yesBlock noBlock =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        (block "entry" [] [] (LoweredBranch condition (blockId "yes") [] (blockId "no") []) : [yesBlock, noBlock])
        "entry"
    ]
    "main"

switchProgram :: [LoweredSwitchCase] -> LoweredBlock -> LoweredProgram
switchProgram cases targetBlock =
  switchProgramWith
    [LoweredVariantLayout 0 []]
    cases
    Nothing
    targetBlock

switchProgramWith :: [LoweredVariantLayout] -> [LoweredSwitchCase] -> Maybe LoweredSwitchDefault -> LoweredBlock -> LoweredProgram
switchProgramWith variants cases maybeDefault targetBlock =
  program
    [LoweredLayout (layoutId "option") (LoweredVariantLayouts variants)]
    []
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [instruction "option" (managed "option") (LoweredConstructVariant (layoutId "option") 0 [])]
            (LoweredSwitch (temporary "option" (managed "option")) cases maybeDefault),
          targetBlock
        ]
        "entry"
    ]
    "main"

invalidClosureCallProgram :: Bool -> LoweredProgram
invalidClosureCallProgram useTailCall =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [i64])]
    []
    [ function
        "captured"
        (Just (parameter "environment" (managed "environment")))
        [parameter "value" i64]
        i64
        [block "entry" [] [] (LoweredReturn (functionParameter "value" i64))]
        "entry",
      mainFunction
    ]
    "main"
  where
    signature = LoweredCallSignature [i64] i64
    setup =
      [ instruction "environment" (managed "environment") (LoweredConstructProduct (layoutId "environment") [int64 1]),
        instruction "closure" (LoweredClosureRepresentation signature) (LoweredConstructClosure (functionId "captured") (temporary "environment" (managed "environment")))
      ]
    closureOperand = temporary "closure" (LoweredClosureRepresentation signature)
    invalidArgument = immediate (LoweredBoolImmediate True)
    mainFunction
      | useTailCall =
          function "main" Nothing [] i64 [block "entry" [] setup (LoweredClosureTailCall closureOperand [invalidArgument])] "entry"
      | otherwise =
          function
            "main"
            Nothing
            []
            i64
            [ block
                "entry"
                []
                (setup <> [instruction "result" i64 (LoweredClosureCall closureOperand [invalidArgument])])
                (LoweredReturn (temporary "result" i64))
            ]
            "entry"

programFailure :: LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
programFailure = LoweredIRValidationFailure LoweredProgramPath

layoutFailure :: Text -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
layoutFailure name = LoweredIRValidationFailure (LoweredLayoutPath (layoutId name))

serviceFailure :: Text -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
serviceFailure name = LoweredIRValidationFailure (LoweredRuntimeServicePath (serviceId name))

functionFailure :: Text -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
functionFailure name = LoweredIRValidationFailure (LoweredFunctionPath (functionId name))

blockFailure :: Text -> Text -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
blockFailure functionName blockName = LoweredIRValidationFailure (LoweredBlockPath (functionId functionName) (blockId blockName))

instructionFailure :: Text -> Text -> Int -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
instructionFailure functionName blockName instructionIndex =
  LoweredIRValidationFailure (LoweredInstructionPath (functionId functionName) (blockId blockName) instructionIndex)

terminatorFailure :: Text -> Text -> LoweredIRValidationKind -> LoweredIRValidationDetail -> LoweredIRValidationFailure
terminatorFailure functionName blockName = LoweredIRValidationFailure (LoweredTerminatorPath (functionId functionName) (blockId blockName))

identifierDetail :: Text -> LoweredIRValidationDetail
identifierDetail = LoweredIdentifierDetail

representationDetail :: LoweredRepresentation -> LoweredRepresentation -> LoweredIRValidationDetail
representationDetail = LoweredRepresentationDetail

scalarFunction :: Text -> LoweredRepresentation -> LoweredImmediate -> LoweredFunction
scalarFunction name representation value =
  function name Nothing [] representation [block "entry" [] [] (LoweredReturn (immediate value))] "entry"

identityFunction :: LoweredFunction
identityFunction =
  function
    "identity"
    Nothing
    [parameter "value" i64]
    i64
    [block "entry" [] [] (LoweredReturn (functionParameter "value" i64))]
    "entry"

directCaller :: (LoweredFunctionId -> [LoweredOperand] -> LoweredOperation) -> (LoweredOperand -> LoweredTerminator) -> LoweredFunction
directCaller callOperation returnTerminator =
  function
    "main"
    Nothing
    []
    i64
    [ block
        "entry"
        []
        [instruction "result" i64 (callOperation (functionId "identity") [int64 11])]
        (returnTerminator (temporary "result" i64))
    ]
    "entry"

closureProgram :: Bool -> LoweredProgram
closureProgram useTailCall =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [i64])]
    []
    [ function
        "captured"
        (Just (parameter "environment" (managed "environment")))
        [parameter "ignored" i64]
        i64
        [ block
            "entry"
            []
            [ instruction
                "captured-value"
                i64
                (LoweredProjectField (layoutId "environment") 0 (functionParameter "environment" (managed "environment")))
            ]
            (LoweredReturn (temporary "captured-value" i64))
        ]
        "entry",
      closureMain
    ]
    "main"
  where
    signature = LoweredCallSignature [i64] i64
    commonInstructions =
      [ instruction "environment" (managed "environment") (LoweredConstructProduct (layoutId "environment") [int64 42]),
        instruction
          "closure"
          (LoweredClosureRepresentation signature)
          (LoweredConstructClosure (functionId "captured") (temporary "environment" (managed "environment")))
      ]
    closureMain
      | useTailCall =
          function
            "main"
            Nothing
            []
            i64
            [ block
                "entry"
                []
                commonInstructions
                (LoweredClosureTailCall (temporary "closure" (LoweredClosureRepresentation signature)) [int64 0])
            ]
            "entry"
      | otherwise =
          function
            "main"
            Nothing
            []
            i64
            [ block
                "entry"
                []
                ( commonInstructions
                    <> [ instruction
                           "result"
                           i64
                           (LoweredClosureCall (temporary "closure" (LoweredClosureRepresentation signature)) [int64 0])
                       ]
                )
                (LoweredReturn (temporary "result" i64))
            ]
            "entry"

program :: [LoweredLayout] -> [LoweredRuntimeService] -> [LoweredFunction] -> Text -> LoweredProgram
program layouts services functions entryFunction =
  LoweredProgram (LoweredIRVersion 1) layouts services functions (functionId entryFunction)

function :: Text -> Maybe LoweredParameter -> [LoweredParameter] -> LoweredRepresentation -> [LoweredBlock] -> Text -> LoweredFunction
function name environment parameters resultRepresentation blocks entryBlock =
  LoweredFunction (functionId name) environment parameters resultRepresentation blocks (blockId entryBlock)

block :: Text -> [LoweredParameter] -> [LoweredInstruction] -> LoweredTerminator -> LoweredBlock
block name parameters instructions terminator =
  LoweredBlock (blockId name) parameters instructions (Just terminator)

instruction :: Text -> LoweredRepresentation -> LoweredOperation -> LoweredInstruction
instruction name = LoweredInstruction (temporaryId name)

parameter :: Text -> LoweredRepresentation -> LoweredParameter
parameter name = LoweredParameter (parameterId name)

functionParameter :: Text -> LoweredRepresentation -> LoweredOperand
functionParameter name = LoweredFunctionParameterOperand (parameterId name)

blockParameter :: Text -> LoweredRepresentation -> LoweredOperand
blockParameter name = LoweredBlockParameterOperand (parameterId name)

temporary :: Text -> LoweredRepresentation -> LoweredOperand
temporary name = LoweredTemporaryOperand (temporaryId name)

immediate :: LoweredImmediate -> LoweredOperand
immediate = LoweredImmediateOperand

int64 :: Integer -> LoweredOperand
int64 = immediate . LoweredSignedIntegerImmediate LoweredIntegerWidth64

signed :: LoweredIntegerWidth -> LoweredRepresentation
signed = LoweredSignedIntegerRepresentation

unsigned :: LoweredIntegerWidth -> LoweredRepresentation
unsigned = LoweredUnsignedIntegerRepresentation

float :: LoweredFloatWidth -> LoweredRepresentation
float = LoweredFloatRepresentation

i64 :: LoweredRepresentation
i64 = signed LoweredIntegerWidth64

managed :: Text -> LoweredRepresentation
managed = LoweredManagedReferenceRepresentation . layoutId

functionId :: Text -> LoweredFunctionId
functionId = LoweredFunctionId

blockId :: Text -> LoweredBlockId
blockId = LoweredBlockId

temporaryId :: Text -> LoweredTemporaryId
temporaryId = LoweredTemporaryId

layoutId :: Text -> LoweredLayoutId
layoutId = LoweredLayoutId

serviceId :: Text -> LoweredRuntimeServiceId
serviceId = LoweredRuntimeServiceId

parameterId :: Text -> LoweredParameterId
parameterId = LoweredParameterId

expectedScalarRendering :: Text
expectedScalarRendering =
  "LoweredProgram(LoweredIRVersion(1), [], [], [LoweredFunction(LoweredFunctionId(\"main\"), Nothing, [], LoweredUnitRepresentation, [LoweredBlock(LoweredBlockId(\"entry\"), [], [], Just(LoweredReturn(LoweredImmediateOperand(LoweredUnitImmediate))))], LoweredBlockId(\"entry\"))], LoweredFunctionId(\"main\"))"
