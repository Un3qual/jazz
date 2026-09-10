{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.ReexportTests (reexportTests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Driver
  ( runCompileErrors,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.Modules.Loader.Shared (lookupSourceIn, resolverConfig)
import Jazz.Compiler.RuntimeHost (RuntimeHost (..), disabledRuntimeHost)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness (NamedTest, assertEqual, assertSingleDiagnosticCode, assertSingleDiagnosticPrimaryStart)

reexportTests :: [NamedTest]
reexportTests =
  [ ("re-exported closures retain their private environment through two facades", testClosure),
    ("direct and facade ADTs share nominal identity in a diamond", testNominalDiamond),
    ("re-exported classes retain original evidence across aliases and diamonds", testClassDiamond),
    ("facades publish their own impls alongside a re-exported class", testFacadeImpl),
    ("re-exported values carry hidden nominal types without exporting them", testHiddenType),
    ("constructor-only facades retain ownership when reunited with an abstract type", testConstructorOnly),
    ("facade diamonds reuse dependency effects and suppress dependency expressions", testEffects),
    ("invalid typed exports point at the selected name", testExportLocation)
  ]
    ++ [ (label, assertRejected selector imports "E4015")
       | (label, selector, imports) <-
           [ ("private dependency values cannot be re-exported", "value secret", "import Lib::Source."),
             ("filtered dependency values cannot be re-exported", "value answer", "import Lib::Source (Visible)."),
             ("alias-only imports cannot satisfy export selectors", "value answer", "import Lib::Source as Source."),
             ("bare export selectors remain owned-only", "answer", "import Lib::Source."),
             ("typed export selectors do not cross namespaces", "class Visible", "import Lib::Source."),
             ("hidden constructors cannot be recovered by explicit groups", "type Choice(Hidden)", "import Lib::Source."),
             ("wrong-owner constructors cannot be re-exported with a type", "type Choice(Other)", "import Lib::Source."),
             ("a shadowing constructor cannot stand in for an imported type", "type Choice(Visible)", "import Lib::Source. data Local = Visible.")
           ]
       ]
    ++ [ ("facades preserve genuine import collisions", testCollision),
         ("owned declarations win typed export selection", testLocalShadow),
         ("all-constructor re-exports forward only visible constructors", testVisibleConstructors),
         ("a later facade cannot recover a hidden constructor", testHiddenConstructorChain),
         ("ambient declarations cannot satisfy typed exports", testAmbientExport),
         ("distinct facade-owned implementations remain ambiguous", testConflictingImpls),
         ("a shared class cannot hide a value-constructor import collision", testExpressionNamespaceCollision)
       ]

assertGraph :: [(FilePath, Text)] -> Text -> IO ()
assertGraph sources expected = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] (lookupSourceIn (Map.fromList sources))
  assertEqual "re-export compile errors" [] (runCompileErrors result)
  assertEqual "re-export runtime errors" [] (runRuntimeErrors result)
  assertEqual "re-export output" (Just expected) (runOutput result)

testClosure :: IO ()
testClosure =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (value answer) { hidden = 40. answer = \\(n) -> hidden + n. 999. }"),
      ("src/Lib/First.jz", "module Lib::First (value answer) { import Lib::Source. }"),
      ("src/Lib/API.jz", "module Lib::API (value answer) { import Lib::First. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API. answer 2. }")
    ]
    "42"

testNominalDiamond :: IO ()
testNominalDiamond =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (type Box(..), value make) { data Box a = Box a. make = \\(n) -> Box n. }"),
      ("src/Lib/Left.jz", "module Lib::Left (type Box(..), value make) { import Lib::Source. }"),
      ("src/Lib/Right.jz", "module Lib::Right (type Box(..), value make) { import Lib::Source. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::Left. import Lib::Right. import Lib::Source as Original. boxed :: Original::Box(Int). boxed = make 42. case boxed { | Box n -> n }. }")
    ]
    "42"

testClassDiamond :: IO ()
testClassDiamond =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (class Equal) { class Equal(a) { equal :: a -> a -> Bool. }. impl Equal(Int) { equal = \\(a, b) -> a == b. }. }"),
      ("src/Lib/Left.jz", "module Lib::Left (class Equal) { import Lib::Source. }"),
      ("src/Lib/Right.jz", "module Lib::Right (class Equal) { import Lib::Source. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::Left. import Lib::Right. import Lib::Source as Original. stored = Equal::equal. partial = Original::Equal::equal 1. (stored 1 1, partial 2, Equal::equal @Int 2 2). }")
    ]
    "(True, False, True)"

testFacadeImpl :: IO ()
testFacadeImpl =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (class Equal) { class Equal(a) { equal :: a -> a -> Bool. }. impl Equal(Int) { equal = \\(a, b) -> a == b. }. }"),
      ("src/Lib/API.jz", "module Lib::API (class Equal, type Marker(..), value same) { import Lib::Source. data Marker = Marker. impl Equal(Marker) { equal = \\(a, b) -> True. }. same :: @{Equal(a)}: a -> a -> Bool. same = \\(a, b) -> Equal::equal a b. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API as API. (API::Equal::equal API::Marker API::Marker, API::same 1 1, API::same API::Marker API::Marker). }")
    ]
    "(True, True, True)"

testHiddenType :: IO ()
testHiddenType =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (value make, value unwrap) { data Hidden = Hidden Int. make = \\(n) -> Hidden n. unwrap = \\(h) -> case h { | Hidden n -> n }. }"),
      ("src/Lib/API.jz", "module Lib::API (value make, value unwrap) { import Lib::Source. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API. unwrap (make 42). }")
    ]
    "42"

testConstructorOnly :: IO ()
testConstructorOnly =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source (type Choice, constructor Visible) { data Choice = Visible Int | Hidden. }"),
      ("src/Lib/Constructors.jz", "module Lib::Constructors (constructor Visible) { import Lib::Source. }"),
      ("src/Lib/Types.jz", "module Lib::Types (type Choice) { import Lib::Source. }"),
      ("src/Lib/API.jz", "module Lib::API (type Choice(Visible)) { import Lib::Constructors. import Lib::Types. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API as API. import Lib::API. chosen :: API::Choice. chosen = API::Visible 42. case chosen { | Visible n -> n | _ -> 0 }. }")
    ]
    "42"

testEffects :: IO ()
testEffects = do
  calls <- newIORef []
  let host = disabledRuntimeHost {runtimeHostWriteStdout = \text -> modifyIORef' calls (<> [text]) >> pure (Right ())}
      sources =
        Map.fromList
          [ ("src/Lib/Source.jz", "module Lib::Source (value answer!) { answer! = { write! \"once\". 42. }. write! \"suppressed\". }"),
            ("src/Lib/Left.jz", "module Lib::Left (value answer!) { import Lib::Source. }"),
            ("src/Lib/Right.jz", "module Lib::Right (value answer!) { import Lib::Source. }"),
            ("src/App/Main.jz", "module App::Main { import Lib::Left as L. import Lib::Right as R. (L::answer!, R::answer!). }")
          ]
  result <- runModuleGraphWithPreludeAndHost host defaultWarningSettings (Just "write! = __kernel_writeStdoutRaw!.") resolverConfig ["App", "Main"] (lookupSourceIn sources)
  assertEqual "effect compile errors" [] (runCompileErrors result)
  assertEqual "effect runtime errors" [] (runRuntimeErrors result)
  assertEqual "effect output" (Just "(42, 42)") (runOutput result)
  recorded <- readIORef calls
  assertEqual "dependency initialization runs once; expressions stay suppressed" ["once"] recorded

visibilitySource :: Text
visibilitySource = "module Lib::Source (value answer, type Choice(Visible), type Other(..)) { answer = 42. secret = 99. data Choice = Visible Int | Hidden. data Other = Other. }"

assertRejected :: Text -> Text -> Text -> IO ()
assertRejected selector imports code = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      ( lookupSourceIn
          ( Map.fromList
              [ ("src/Lib/Source.jz", visibilitySource),
                ("src/App/Main.jz", "module App::Main (" <> selector <> ") { " <> imports <> " }")
              ]
          )
      )
  assertSingleDiagnosticCode "rejected export" code (runCompileErrors result)

testExportLocation :: IO ()
testExportLocation = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (lookupSourceIn (Map.singleton "src/App/Main.jz" "module App::Main (value missing) { }"))
  assertSingleDiagnosticCode "missing export" "E4015" (runCompileErrors result)
  assertSingleDiagnosticPrimaryStart "missing export name" (SourceSpanIn "src/App/Main.jz" 1 25) (runCompileErrors result)

testCollision :: IO ()
testCollision = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      ( lookupSourceIn
          ( Map.fromList
              [ ("src/Lib/Source.jz", "module Lib::Source { answer = 1. }"),
                ("src/Lib/API.jz", "module Lib::API (value answer) { import Lib::Source. }"),
                ("src/Lib/Other.jz", "module Lib::Other { answer = 2. }"),
                ("src/App/Main.jz", "module App::Main { import Lib::API. import Lib::Other. answer. }")
              ]
          )
      )
  assertSingleDiagnosticCode "different declarations collide" "E4008" (runCompileErrors result)

testLocalShadow :: IO ()
testLocalShadow =
  assertGraph
    [ ("src/Lib/Source.jz", "module Lib::Source { answer = 1. }"),
      ("src/Lib/API.jz", "module Lib::API (value answer) { import Lib::Source. answer = 42. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API. answer. }")
    ]
    "42"

testVisibleConstructors :: IO ()
testVisibleConstructors =
  assertGraph
    [ ("src/Lib/Source.jz", visibilitySource),
      ("src/Lib/API.jz", "module Lib::API (type Choice(..)) { import Lib::Source. }"),
      ("src/App/Main.jz", "module App::Main { import Lib::API. case Visible 42 { | Visible n -> n | _ -> 0 }. }")
    ]
    "42"

testHiddenConstructorChain :: IO ()
testHiddenConstructorChain = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      ( lookupSourceIn
          ( Map.fromList
              [ ("src/Lib/Source.jz", visibilitySource),
                ("src/Lib/API.jz", "module Lib::API (type Choice(..)) { import Lib::Source. }"),
                ("src/App/Main.jz", "module App::Main (type Choice(Hidden)) { import Lib::API. }")
              ]
          )
      )
  assertSingleDiagnosticCode "hidden constructor remains private" "E4015" (runCompileErrors result)

testAmbientExport :: IO ()
testAmbientExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just "ambient = 42.")
      resolverConfig
      ["App", "Main"]
      (lookupSourceIn (Map.singleton "src/App/Main.jz" "module App::Main (value ambient) { }"))
  assertSingleDiagnosticCode "ambient export needs an explicit import" "E4015" (runCompileErrors result)

testConflictingImpls :: IO ()
testConflictingImpls = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      ( lookupSourceIn
          ( Map.fromList
              [ ("src/Lib/Source.jz", "module Lib::Source (class Choose) { class Choose(a) { choose :: a -> Int. }. }"),
                ("src/Lib/Left.jz", "module Lib::Left (class Choose) { import Lib::Source. impl Choose(Int) { choose = \\(n) -> 1. }. }"),
                ("src/Lib/Right.jz", "module Lib::Right (class Choose) { import Lib::Source. impl Choose(Int) { choose = \\(n) -> 2. }. }"),
                ("src/App/Main.jz", "module App::Main { import Lib::Left. import Lib::Right. Choose::choose 0. }")
              ]
          )
      )
  assertSingleDiagnosticCode "distinct implementations remain ambiguous" "E2015" (runCompileErrors result)

testExpressionNamespaceCollision :: IO ()
testExpressionNamespaceCollision = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      ( lookupSourceIn
          ( Map.fromList
              [ ("src/Lib/Source.jz", "module Lib::Source (class Token) { class Token(a) { }. }"),
                ("src/Lib/Left.jz", "module Lib::Left (class Token, value Token) { import Lib::Source. Token = 1. }"),
                ("src/Lib/Right.jz", "module Lib::Right (class Token, constructor Token) { import Lib::Source. data Other = Token. }"),
                ("src/App/Main.jz", "module App::Main { import Lib::Left. import Lib::Right. Token. }")
              ]
          )
      )
  assertSingleDiagnosticCode "shared class does not mask conflicting expressions" "E4008" (runCompileErrors result)
