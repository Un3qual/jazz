{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.TextTests
  ( textTests,
  )
where

import Data.Text (Text)
import JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixtureExpecting,
  )
import JazzNext.TestHarness
  ( NamedTest,
  )

textTests :: [NamedTest]
textTests =
  [ ("Text indexes, slices, and constructs by Unicode scalar", runStdlibFixtureExpecting ["Stdlib", "Text", "Core"] "stdlib/text/Core.jz" expectedCore),
    ("Text search and replacement are left-to-right and non-overlapping", runStdlibFixtureExpecting ["Stdlib", "Text", "Search"] "stdlib/text/Search.jz" expectedSearch),
    ("Text splitting and cleanup define whitespace boundaries", runStdlibFixtureExpecting ["Stdlib", "Text", "SplitCleanup"] "stdlib/text/SplitCleanup.jz" expectedSplitCleanup),
    ("Char classification and simple case mapping handle Unicode", runStdlibFixtureExpecting ["Stdlib", "Text", "CharCase"] "stdlib/text/CharCase.jz" expectedCharCase),
    ("bulk Text construction handles large deterministic inputs", runStdlibFixtureExpecting ["Stdlib", "Text", "LargeConcat"] "stdlib/text/LargeConcat.jz" expectedLargeConcat)
  ]

expectedCore, expectedSearch, expectedSplitCleanup, expectedCharCase, expectedLargeConcat :: Text
expectedCore = "(Nothing, Just('a'), Just('🙂'), Just('é'), Nothing, \"\", \"a🙂\", \"a🙂é\", \"a🙂é\", \"é\", \"\", \"a🙂\", \"🙂é\", \"\", ['a', '🙂', 'é'], \"é🙂a\", \"\", \"JaJaJa\", \"Jazz🙂\", \"\", \"a🙂é\", \"a-🙂-é\", \"....猫\", \"猫.\", \"猫\")"
expectedSearch = "(True, True, False, True, True, False, True, False, Just(1), Just(0), Nothing, \"bba\", \"unchanged\", \"🙂x🙂x\")"
expectedSplitCleanup = "([\"\", \"a\", \"\", \"b\", \"\"], [\"a\", \"🙂\", \"é\"], [], [], [\"a\", \"b\", \"c\", \"d\"], [\"Jazz\", \"🙂\", \"rocks\"], \"Jazz\", \"Jazz  \\t\", \"  Jazz\")"
expectedCharCase = "(True, True, True, True, False, False, 'É', 'é', '1', '1', 'i')"
expectedLargeConcat = "(20000, 4999, True)"
