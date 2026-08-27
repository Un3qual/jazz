{-# LANGUAGE OverloadedStrings #-}

-- | Hosted Jazz and Haskell validator parity tests.
module Jazz.Compiler.Bootstrap.TypedCoreContract.ParityTests
  ( tests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( CanonicalTypedCoreStructure,
    canonicalTypedProgramRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    decodeCanonicalTypedCoreStructure,
  )
import Jazz.Compiler.Bootstrap.TypedCoreContract.Fixtures
import Jazz.Compiler.Bootstrap.TypedCoreContract.RegressionTests (reviewRegressionPrograms)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( closureRecursionExpectedPrograms,
    directRecursionExpectedPrograms,
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
    assertEqual,
  )
import Jazz.TestSource (readCheckedInJazzProjectModuleSource)

tests :: [NamedTest]
tests = [("matches Haskell validation for every fixed and review fixture twice", testJazzValidationParity)]

testJazzValidationParity :: IO ()
testJazzValidationParity = do
  let programs =
        map validFixtureProgram validFixtures
          <> map invalidFixtureProgram invalidFixtures
          <> reviewRegressionPrograms
          <> map snd directRecursionExpectedPrograms
          <> map snd closureRecursionExpectedPrograms
      expectedRuntimeValue =
        VList
          [ VTuple
              [ canonicalTypedProgramRuntimeValue program,
                canonicalTypedValidationFailuresRuntimeValue (expectedContractFailures program)
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

expectedContractFailures :: TypedProgram -> [TypedCoreValidationFailure]
expectedContractFailures program =
  case [failures | (_, candidate, failures) <- recursiveGroupContractCases <> recursiveGroupFixCases, candidate == program] of
    failures : _ -> failures
    [] -> validateTypedProgram program

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
