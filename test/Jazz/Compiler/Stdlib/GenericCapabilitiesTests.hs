{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Stdlib.GenericCapabilitiesTests (genericCapabilityTests) where

import Data.Text (Text)
import Jazz.Compiler.Driver (runCompileErrors)
import Jazz.Compiler.Stdlib.Shared (runStdlibFixtureExpecting, runStdlibSource)
import Jazz.TestHarness (NamedTest, assertSingleDiagnosticCode)

genericCapabilityTests :: [NamedTest]
genericCapabilityTests =
  [ ("generic collection methods preserve constructors and element evidence", runStdlibFixtureExpecting ["GenericLibrary"] "stdlib/generic/GenericLibrary.jz" "[]"),
    ("generic collection methods preserve mapping laws and fold order", runStdlibFixtureExpecting ["GenericLaws"] "stdlib/generic/GenericLaws.jz" "[]")
  ]
    <> map rejectsCompilation negativeCases

rejectsCompilation :: (Text, Text, Text) -> NamedTest
rejectsCompilation (label, expectedCode, source) =
  ( label,
    do
      result <- runStdlibSource ["InvalidGenericLibrary"] source
      assertSingleDiagnosticCode label expectedCode (runCompileErrors result)
  )

negativeCases :: [(Text, Text, Text)]
negativeCases =
  [ ("Text mapping requires a Char callback", "E2006", "module InvalidGenericLibrary { import Text as Text. Text::map (\\(character) -> 1) \"abc\". }"),
    ("Set mapping requires output ordering", "E2009", "module InvalidGenericLibrary { import Set as Set. data Token = Token Int. Set::map (Set::singleton 1) (\\(item) -> Token item). }"),
    ("Text has no generic mapping instance", "E2006", "module InvalidGenericLibrary {  map (\\(character) -> character) \"abc\". }"),
    ("Set has no generic mapping instance", "E2009", "module InvalidGenericLibrary { import Set as Set. map (\\(item) -> item) (Set::singleton 1). }"),
    ("mapping preserves the input constructor", "E2005", "module InvalidGenericLibrary { import Queue as Queue. result :: Queue::Queue(Bool). result = map (\\(item) -> item == 1) [1, 2]. result. }")
  ]
