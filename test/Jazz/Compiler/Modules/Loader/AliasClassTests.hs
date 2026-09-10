{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.AliasClassTests (aliasClassTests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runModuleGraphWithPrelude,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.Modules.Loader.Shared (lookupSourceIn, resolverConfig)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness (NamedTest, assertEqual, assertSingleDiagnosticCode, assertSingleDiagnosticContains, assertSingleDiagnosticPrimaryStart)

aliasClassTests :: [NamedTest]
aliasClassTests =
  [ (label, assertProgram body expected)
  | (label, body, expected) <-
      [ ( "aliased classes dispatch through the original implementation",
          "Facts::Eq::equals 1 1.",
          "True"
        ),
        ( "aliased class methods remain first-class values",
          "equal = Facts::Eq::equals. equal 1 2.",
          "False"
        ),
        ( "aliased class methods retain partial application evidence",
          "equalOne = Facts::Eq::equals 1. equalOne 1.",
          "True"
        ),
        ( "aliased class methods support explicit instantiation",
          "Facts::Eq::equals @Int 1 1.",
          "True"
        ),
        ( "qualified class constraints select the aliased class",
          "same :: @{Facts::Eq(a)}: a -> a -> Bool. same = \\(left, right) -> Facts::Eq::equals left right. same 1 1.",
          "True"
        ),
        ( "qualified impl heads connect a local type to the imported class",
          "data Local = Local. impl Facts::Eq(Local) { equals = \\(left, right) -> True. }. Facts::Eq::equals Local Local.",
          "True"
        ),
        ( "two aliases share one implementation identity",
          "import Lib::Facts as Other. (Facts::Eq::equals 1 1, Other::Eq::equals 1 2).",
          "(True, False)"
        ),
        ( "aliased and unqualified imports share implementation identity",
          "import Lib::Facts. (Facts::Eq::equals 1 1, Eq::equals 1 2).",
          "(True, False)"
        ),
        ( "a local same-spelled class remains distinct from an aliased class",
          "class Eq(a) { equals :: a -> a -> Bool. }. impl Eq(Int) { equals = \\(x, y) -> False. }. (Facts::Eq::equals 1 1, Eq::equals 1 1).",
          "(True, False)"
        )
      ]
  ]
    ++ [ (label, assertRejected body code)
       | (label, body, code) <-
           [ ("qualified class lookup rejects unknown aliases", "Missing::Eq::equals 1 1.", "E4013"),
             ("qualified class lookup rejects missing public classes", "Facts::Missing::equals 1 1.", "E4014"),
             ("qualified class lookup rejects private classes", "Facts::Hidden::hidden 1.", "E4014"),
             ("a same-spelled exported type cannot satisfy class lookup", "Facts::OnlyType::equals 1 1.", "E4014"),
             ("aliased imports do not expose unqualified classes", "Eq::equals 1 1.", "E4013"),
             ("qualified constraint lookup rejects private classes", "same :: @{Facts::Hidden(Int)}: Int. same = 1.", "E4014"),
             ("qualified impl lookup rejects private classes", "impl Facts::Hidden(Int) { hidden = \\(x) -> x. }.", "E4014")
           ]
       ]
    ++ [ ("qualified class diagnostics identify the failing source component", testDiagnosticComponents),
         ("qualified class constraints reject spaced qualification", assertRejected "same :: @{Facts :: Eq(Int)}: Int. same = 1." "E4004"),
         ("qualified class constraints reject overlong qualification", assertRejected "same :: @{Facts::Eq::Extra(Int)}: Int. same = 1." "E4004")
       ]
    ++ [ ( "qualified method lookup diagnoses a missing method",
           assertFailure "Facts::Eq::absent 1 1." "missing class method"
         ),
         ( "qualified impl heads retain duplicate implementation rejection",
           assertFailure "data Local = Local. impl Facts::Eq(Local) { equals = \\(x, y) -> True. }. impl Facts::Eq(Local) { equals = \\(x, y) -> True. }." "duplicate"
         ),
         ("imported constrained values retain private evidence under aliased import", testHiddenEvidence),
         ("same-spelled classes from different modules remain distinct", testDifferentOrigins)
       ]

runProgram :: Text -> [(FilePath, Text)] -> IO RunResult
runProgram body additional =
  runModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    (lookupSourceIn (Map.fromList sources))
  where
    sources =
      [ ("src/App/Main.jz", "module App::Main { import Lib::Facts as Facts. " <> body <> " }"),
        ( "src/Lib/Facts.jz",
          """
          module Lib::Facts (class Eq, type OnlyType) {
            class Eq(a) { equals :: a -> a -> Bool. }.
            impl Eq(Int) { equals = \\(left, right) -> left == right. }.
            class Hidden(a) { hidden :: a -> a. }.
            impl Hidden(Int) { hidden = \\(item) -> item. }.
            data OnlyType = OnlyType.
          }
          """
        )
      ]
        ++ additional

assertOutput :: RunResult -> Text -> IO ()
assertOutput result expected = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "result" (Just expected) (runOutput result)

assertProgram :: Text -> Text -> IO ()
assertProgram body expected = runProgram body [] >>= (`assertOutput` expected)

assertRejected :: Text -> Text -> IO ()
assertRejected body code = do
  result <- runProgram body []
  assertSingleDiagnosticCode "compile failure" code (runCompileErrors result)

assertFailure :: Text -> Text -> IO ()
assertFailure body message = do
  result <- runProgram body []
  assertSingleDiagnosticContains "compile failure" message (runCompileErrors result)

testDifferentOrigins :: IO ()
testDifferentOrigins = do
  result <-
    runProgram
      "import Lib::Other as Other. (Facts::Eq::equals 1 1, Other::Eq::equals 1 1)."
      [("src/Lib/Other.jz", "class Eq(a) { equals :: a -> a -> Bool. }. impl Eq(Int) { equals = \\(x, y) -> False. }.")]
  assertOutput result "(True, False)"

testHiddenEvidence :: IO ()
testHiddenEvidence = do
  result <-
    runProgram
      "import Lib::Wrapped as Wrapped. Wrapped::same 1 1."
      [ ( "src/Lib/Wrapped.jz",
          """
          module Lib::Wrapped (value same) {
            import Lib::Facts as Facts.
            same :: @{Facts::Eq(a)}: a -> a -> Bool.
            same = \\(x, y) -> Facts::Eq::equals x y.
          }
          """
        )
      ]
  assertOutput result "True"

testDiagnosticComponents :: IO ()
testDiagnosticComponents =
  mapM_
    check
    [ ("Missing::Eq::equals 1 1.", 1),
      ("Facts::Hidden::hidden 1.", 8),
      ("Facts::Eq::absent 1 1.", 12),
      ("stored = Facts::Eq::absent.", 21),
      ("same :: @{Facts::Hidden(Int)}: Int. same = 1.", 18),
      ("impl Facts::Hidden(Int) { }.", 13)
    ]
  where
    check (body, column) = do
      result <- runProgram ("\n" <> body) []
      assertSingleDiagnosticPrimaryStart
        "qualified reference location"
        (SourceSpanIn "src/App/Main.jz" 2 column)
        (runCompileErrors result)
