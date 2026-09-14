{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.AliasClassTests (aliasClassTests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics (SourceSpan (..), diagnosticCode)
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
          "Facts::Equatable::equals 1 1.",
          "True"
        ),
        ( "aliased class methods remain first-class values",
          "equal = Facts::Equatable::equals. equal 1 2.",
          "False"
        ),
        ( "aliased class methods retain partial application evidence",
          "equalOne = Facts::Equatable::equals 1. equalOne 1.",
          "True"
        ),
        ( "aliased class methods support explicit instantiation",
          "Facts::Equatable::equals @Int 1 1.",
          "True"
        ),
        ( "qualified class constraints select the aliased class",
          "same :: @{Facts::Equatable(a)}: a -> a -> Bool. same = \\(left, right) -> Facts::Equatable::equals left right. same 1 1.",
          "True"
        ),
        ( "qualified impl heads connect a local type to the imported class",
          "data Local = Local. impl Facts::Equatable(Local) { equals = \\(left, right) -> True. }. Facts::Equatable::equals Local Local.",
          "True"
        ),
        ( "two aliases share one implementation identity",
          "import Lib::Facts as Other. (Facts::Equatable::equals 1 1, Other::Equatable::equals 1 2).",
          "(True, False)"
        ),
        ( "aliased and unqualified imports share implementation identity",
          "import Lib::Facts. (Facts::Equatable::equals 1 1, Equatable::equals 1 2).",
          "(True, False)"
        ),
        ( "a local same-spelled class remains distinct from an aliased class",
          "class Equatable(a) { equals :: a -> a -> Bool. }. impl Equatable(Int) { equals = \\(x, y) -> False. }. (Facts::Equatable::equals 1 1, Equatable::equals 1 1).",
          "(True, False)"
        )
      ]
  ]
    ++ [ (label, assertRejected body code)
       | (label, body, code) <-
           [ ("qualified class lookup rejects unknown aliases", "Missing::Equatable::equals 1 1.", "E4013"),
             ("qualified class lookup rejects missing public classes", "Facts::Missing::equals 1 1.", "E4014"),
             ("qualified class lookup rejects private classes", "Facts::Hidden::hidden 1.", "E4014"),
             ("a same-spelled exported type cannot satisfy class lookup", "Facts::OnlyType::equals 1 1.", "E4014"),
             ("aliased imports do not expose unqualified classes", "Equatable::equals 1 1.", "E4013"),
             ("qualified constraint lookup rejects private classes", "same :: @{Facts::Hidden(Int)}: Int. same = 1.", "E4014"),
             ("qualified impl lookup rejects private classes", "impl Facts::Hidden(Int) { hidden = \\(x) -> x. }.", "E4014")
           ]
       ]
    ++ [ ("imported type constructors preserve inferred parameter kinds", testImportedConstructorKinds),
         ("qualified class diagnostics identify the failing source component", testDiagnosticComponents),
         ("qualified method argument errors retain the argument location", testArgumentDiagnostic),
         ("qualified class constraints reject spaced qualification", assertRejected "same :: @{Facts :: Equatable(Int)}: Int. same = 1." "E4004"),
         ("qualified result types reject spacing before the separator", assertRejected "same :: Int -> Facts :: OnlyType. same = \\(x) -> x." "E4004"),
         ("qualified result types reject spacing after the separator", assertRejected "same :: Int -> Facts:: OnlyType. same = \\(x) -> x." "E4004"),
         ("bare qualified types reject internal spacing", assertRejected "same :: Facts :: OnlyType. same = 1." "E2009"),
         ("unfinished signatures reject their terminator", assertRejected "broken :: @{Equatable(Int. missing = Facts::Hidden::hidden 1." "E4004"),
         ("parenthesized class constraints reject spaced qualification", assertRejected "same :: @{((Facts :: Equatable(Int)))}: Int. same = 1." "E4004"),
         ("parenthesized class constraints reject overlong qualification", assertRejected "same :: @{(Facts::Equatable::Extra(Int))}: Int. same = 1." "E4004"),
         ("later parenthesized constraints reject spaced qualification", assertRejected "same :: @{((Facts::Equatable(Int))), (Facts :: Equatable(Int))}: Int. same = 1." "E4004"),
         ("parenthesized class constraints preserve valid qualification", assertProgram "same :: @{((Facts::Equatable(Int)))}: Int. same = 1. same." "1"),
         ("qualified class constraints reject overlong qualification", assertRejected "same :: @{Facts::Equatable::Extra(Int)}: Int. same = 1." "E4004")
       ]
    ++ [ ( "qualified method lookup diagnoses a missing method",
           assertFailure "Facts::Equatable::absent 1 1." "missing class method"
         ),
         ( "qualified impl heads retain duplicate implementation rejection",
           assertFailure "data Local = Local. impl Facts::Equatable(Local) { equals = \\(x, y) -> True. }. impl Facts::Equatable(Local) { equals = \\(x, y) -> True. }." "duplicate"
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
          module Lib::Facts (class Equatable, type OnlyType) {
            class Equatable(a) { equals :: a -> a -> Bool. }.
            impl Equatable(Int) { equals = \\(left, right) -> left == right. }.
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

testImportedConstructorKinds :: IO ()
testImportedConstructorKinds = do
  let definitions = [("src/Lib/Kinds.jz", "data Wrapped f a = Wrapped f(a). unwrap = \\(wrapped) -> case wrapped { | Wrapped xs -> xs }.")]
  accepted <-
    runProgram
      "import Lib::Kinds as K. keep :: K::Wrapped(List, Int) -> K::Wrapped(List, Int). keep = \\(x) -> x. K::unwrap (keep (K::Wrapped [1, 2]))."
      definitions
  assertOutput accepted "[1, 2]"
  rejected <-
    runProgram
      "import Lib::Kinds as K. keep :: K::Wrapped(Int, Int) -> K::Wrapped(Int, Int). keep = \\(x) -> x."
      definitions
  assertSingleDiagnosticContains "imported kind mismatch" "kind mismatch" (runCompileErrors rejected)

testDifferentOrigins :: IO ()
testDifferentOrigins = do
  result <-
    runProgram
      "import Lib::Other as Other. (Facts::Equatable::equals 1 1, Other::Equatable::equals 1 1)."
      [("src/Lib/Other.jz", "class Equatable(a) { equals :: a -> a -> Bool. }. impl Equatable(Int) { equals = \\(x, y) -> False. }.")]
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
            same :: @{Facts::Equatable(a)}: a -> a -> Bool.
            same = \\(x, y) -> Facts::Equatable::equals x y.
          }
          """
        )
      ]
  assertOutput result "True"

testDiagnosticComponents :: IO ()
testDiagnosticComponents =
  mapM_
    check
    [ ("Missing::Equatable::equals 1 1.", 1),
      ("Facts::Hidden::hidden 1.", 8),
      ("Facts::Equatable::absent 1 1.", 19),
      ("stored = Facts::Equatable::absent.", 28),
      ("stored = Facts::Equatable::absent @Int.", 28),
      ("Facts::Equatable::absent @Int 1 1.", 19),
      ("same :: @{Facts::Hidden(Int)}: Int. same = 1.", 18),
      ("same :: @{Equatable(Facts::OnlyType), Facts::OnlyType(Int)}: Int. same = 1.", 46),
      ("same :: @{Equatable(Facts::OnlyType), ((Facts::OnlyType(Int)))}: Int. same = 1.", 48),
      ("impl Facts::Hidden(Int) { }.", 13)
    ]
  where
    check (body, column) = do
      result <- runProgram ("\n" <> body) []
      assertSingleDiagnosticPrimaryStart
        "qualified reference location"
        (SourceSpanIn "src/App/Main.jz" 2 column)
        (runCompileErrors result)

testArgumentDiagnostic :: IO ()
testArgumentDiagnostic = do
  result <- runProgram "\nFacts::Equatable::equals [1, \"a\"] 1." []
  assertSingleDiagnosticPrimaryStart
    "list argument location"
    (SourceSpanIn "src/App/Main.jz" 2 26)
    (filter ((== "E2007") . diagnosticCodeText . diagnosticCode) (runCompileErrors result))
