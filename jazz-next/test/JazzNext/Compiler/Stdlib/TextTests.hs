{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.TextTests
  ( textTests,
  )
where

import Data.Text (Text)
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixture,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
  )

textTests :: [NamedTest]
textTests =
  [ ("Text indexes, slices, and constructs by Unicode scalar", fixtureTest ["Stdlib", "Text", "Core"] "stdlib/text/Core.jz" expectedCore),
    ("Text search and replacement are left-to-right and non-overlapping", fixtureTest ["Stdlib", "Text", "Search"] "stdlib/text/Search.jz" expectedSearch),
    ("Text splitting and cleanup define whitespace boundaries", fixtureTest ["Stdlib", "Text", "SplitCleanup"] "stdlib/text/SplitCleanup.jz" expectedSplitCleanup),
    ("Char classification and simple case mapping handle Unicode", fixtureTest ["Stdlib", "Text", "CharCase"] "stdlib/text/CharCase.jz" expectedCharCase),
    ("bulk Text construction handles large deterministic inputs", fixtureTest ["Stdlib", "Text", "LargeConcat"] "stdlib/text/LargeConcat.jz" expectedLargeConcat)
  ]

fixtureTest :: [Text] -> FilePath -> Text -> IO ()
fixtureTest modulePath fixturePath expectedOutput = do
  result <- runStdlibFixture modulePath fixturePath
  assertSuccessfulOutput expectedOutput result

assertSuccessfulOutput :: Text -> RunResult -> IO ()
assertSuccessfulOutput expectedOutput result = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just expectedOutput) (runOutput result)

expectedCore, expectedSearch, expectedSplitCleanup, expectedCharCase, expectedLargeConcat :: Text
expectedCore = "(Nothing, Just('a'), Just('🙂'), Just('é'), Nothing, \"\", \"a🙂\", \"a🙂é\", \"a🙂é\", \"é\", \"\", \"a🙂\", \"🙂é\", \"\", ['a', '🙂', 'é'], \"é🙂a\", \"\", \"JaJaJa\", \"Jazz🙂\", \"\", \"a🙂é\", \"a-🙂-é\", \"....猫\", \"猫.\", \"猫\")"
expectedSearch = "(True, True, False, True, True, False, True, False, Just(1), Just(0), Nothing, \"bba\", \"unchanged\", \"🙂x🙂x\")"
expectedSplitCleanup = "([\"\", \"a\", \"\", \"b\", \"\"], [\"a\", \"🙂\", \"é\"], [], [], [\"a\", \"b\", \"c\", \"d\"], [\"Jazz\", \"🙂\", \"rocks\"], \"Jazz\", \"Jazz  \\t\", \"  Jazz\")"
expectedCharCase = "(True, True, False, False, 'É', 'é', '1', '1', 'i')"
expectedLargeConcat = "(20000, 4999, True)"
