{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Stdlib.GenericCapabilitiesTests (genericCapabilityTests) where

import Data.Text (Text)
import Jazz.Compiler.Driver (runCompileErrors)
import Jazz.Compiler.Stdlib.Shared (runStdlibFixtureExpecting, runStdlibSource)
import Jazz.TestHarness (NamedTest, assertSingleDiagnosticCode)

genericCapabilityTests :: [NamedTest]
genericCapabilityTests =
  [ ("generic collection methods preserve constructors and element evidence", runStdlibFixtureExpecting ["GenericLibrary"] "stdlib/generic/GenericLibrary.jz" expectedGenericLibrary),
    ("generic collection methods preserve mapping laws and fold order", runStdlibFixtureExpecting ["GenericLaws"] "stdlib/generic/GenericLaws.jz" expectedGenericLaws)
  ]
    <> map rejectsCompilation negativeCases

rejectsCompilation :: (Text, Text, Text) -> NamedTest
rejectsCompilation (label, expectedCode, source) =
  ( label,
    do
      result <- runStdlibSource ["InvalidGenericLibrary"] source
      assertSingleDiagnosticCode label expectedCode (runCompileErrors result)
  )

expectedGenericLibrary :: Text
expectedGenericLibrary =
  "(([True, False], [True, False], Just(True), Ok(True), Err(\"bad\"), [True, False], [(\"a\", True), (\"b\", False)], [(\"b\", False), (\"a\", True)], Crate(True)), (True, True, True, True, True, True, True, False, False, True, False), (Nothing, Just(9), Just(5), Just(5), 1), \"a😀é\", [1, 1, 1], [0], [1, 2, 3], \"a😀\", [1, 2], [1, 2], [1, 2, 3])"

expectedGenericLaws :: Text
expectedGenericLaws =
  "((True, True, True, True, True, True), (True, True, True, True), ((0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)), ((123, 321), (123, 321), (3, 3), (1, 1), (123, 321), (54, 45), (45, 54), (123, 321)), [True, False], True, True, False, False, [])"

negativeCases :: [(Text, Text, Text)]
negativeCases =
  [ ("Text mapping requires a Char callback", "E2006", "module InvalidGenericLibrary { import Text as Text. Text::map (\\(character) -> 1) \"abc\". }"),
    ("Set mapping requires output ordering", "E2009", "module InvalidGenericLibrary { import Set as Set. data Token = Token Int. Set::map (Set::singleton 1) (\\(item) -> Token item). }"),
    ("Text has no generic mapping instance", "E2006", "module InvalidGenericLibrary {  map (\\(character) -> character) \"abc\". }"),
    ("Set has no generic mapping instance", "E2009", "module InvalidGenericLibrary { import Set as Set. map (\\(item) -> item) (Set::singleton 1). }"),
    ("mapping preserves the input constructor", "E2005", "module InvalidGenericLibrary { import Queue as Queue. result :: Queue::Queue(Bool). result = map (\\(item) -> item == 1) [1, 2]. result. }")
  ]
