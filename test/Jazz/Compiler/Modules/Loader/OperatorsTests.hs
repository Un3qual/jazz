{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Modules.Loader.OperatorsTests
  ( operatorTests,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BundledPrelude (bundledPreludeSource)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( runCompileErrors,
    runModuleGraphWithPrelude,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.Modules.Loader.Shared
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticRelatedStart,
    failTest,
  )

operatorTests :: [NamedTest]
operatorTests =
  [ ("facade operator sections capture their operands on creation", testFacadeSectionCapture),
    ("operator imports retain fixity, visibility and generic aliases", testOperatorImportCases),
    ("operator imports reject conflicting declarations and visibility", testOperatorImportFailures),
    ("facades transport all operator forms and defining fixity", testFacadeOperators),
    ("operator notation retains imported function dependencies", testImportedOperatorFunction),
    ("run module graph retains local operator binding needed by exported binding", testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding),
    ("run module graph retains local operator signature needed by exported binding", testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding),
    ("run module graph retains local operator binding needed by explicit imported export", testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport),
    ("run module graph does not leak retained operator binding into importer", testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter),
    ("run module graph imported right operator section captures right operand", testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand),
    ("run module graph ignores hidden operator binding collisions", testRunModuleGraphIgnoresHiddenOperatorBindingCollisions)
  ]

testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding :: IO ()
testRunModuleGraphRetainsLocalOperatorBindingNeededByExportedBinding = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Ops.
            plus.
            }
            """
          ),
          ( "src/Lib/Ops.jz",
            """
            module Lib::Ops {
            operator %% tier 2.
            (%%) = \\(left, right) -> left + right.
            plus = 1 %% 2.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding :: IO ()
testRunModuleGraphRetainsLocalOperatorSignatureNeededByExportedBinding = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Ops.
            plus.
            }
            """
          ),
          ( "src/Lib/Ops.jz",
            """
            module Lib::Ops {
            operator %% tier 2.
            (%%) :: Int -> Int -> Int.
            (%%) = \\(left, right) -> left + right.
            plus = 1 %% 2.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport :: IO ()
testRunModuleGraphRetainsLocalOperatorBindingNeededByExplicitImportedExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Ops (plus).
            plus.
            }
            """
          ),
          ( "src/Lib/Ops.jz",
            """
            module Lib::Ops {
            operator %% tier 2.
            (%%) = \\(left, right) -> left + right.
            plus = 1 %% 2.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter :: IO ()
testRunModuleGraphDoesNotLeakRetainedOperatorBindingIntoImporter = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" Nothing (runOutput result)
  case runCompileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "missing binding code" "E2010" rendered
      assertContains "missing binding operator" "operator '%%' has no executable binding" rendered
    _ -> failTest "expected exactly one missing operator binding error"
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Ops (plus).
            operator %% tier 2.
            result = (10 %% 3) + plus.
            result.
            }
            """
          ),
          ( "src/Lib/Ops.jz",
            """
            module Lib::Ops {
            operator %% tier 2.
            (%%) = \\(left, right) -> left + right.
            plus = 1 %% 2.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand :: IO ()
testRunModuleGraphImportedRightOperatorSectionCapturesRightOperand = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime output is suppressed on right operand failure" Nothing (runOutput result)
  case runRuntimeErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "right section capture runtime code" "E3001" rendered
      assertContains "right section capture runtime text" "division by zero" rendered
    _ -> failTest "expected exactly one imported right section runtime error"
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Ops (section).
            section.
            }
            """
          ),
          ( "src/Lib/Ops.jz",
            """
            module Lib::Ops {
            operator %% tier 2.
            (%%) = \\(left, right) -> left - right.
            section = (%% (1 / 0)).
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphIgnoresHiddenOperatorBindingCollisions :: IO ()
testRunModuleGraphIgnoresHiddenOperatorBindingCollisions = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      (Just bundledPreludeSource)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(3, 7)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A.
            import Lib::B.
            (a, b).
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            module Lib::A {
            operator %% tier 2.
            (%%) = \\(left, right) -> left + right.
            a = 1 %% 2.
            }
            """
          ),
          ( "src/Lib/B.jz",
            """
            module Lib::B {
            operator %% tier 2.
            (%%) = \\(left, right) -> left * right.
            b = 1 %% 7.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

-- The wrapper exports only its callers, so retaining add depends on the
-- operator reference inventory, including first-class values and sections.
testImportedOperatorFunction :: IO ()
testImportedOperatorFunction = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] (lookupSourceIn sources)
  assertEqual "imported operator compile errors" [] (runCompileErrors result)
  assertEqual "imported operator runtime errors" [] (runRuntimeErrors result)
  assertEqual "imported operator output" (Just "(5, 5, 5, 5)") (runOutput result)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
              import Lib::Wrapper (direct, aliased, left, right).
              (direct 9 4, aliased 9 4, left 4, right 9).
            }
            """
          ),
          ( "src/Lib/Wrapper.jz",
            """
            module Lib::Wrapper (direct, aliased, left, right) {
              import Lib::Operations (add).
              direct = \\(a, b) -> a + b.
              aliased = (+).
              left = (9 +).
              right = (+ 4).
            }
            """
          ),
          ("src/Lib/Operations.jz", "module Lib::Operations (add) { add = __kernel_subtract. }")
        ]

testFacadeOperators :: IO ()
testFacadeOperators = do
  result <- runModuleGraphWithPrelude defaultWarningSettings (Just bundledPreludeSource) resolverConfig ["App", "Main"] (lookupSourceIn sources)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "facade operator output" (Just "(7, 7, 7, 7, 7, 7, 7, 7, 14, 42)") (runOutput result)
  where
    sources =
      Map.fromList
        [ ("src/Lib/Operations.jz", "module Lib::Operations (value (%%), value answer) { operator %% precedence 6 left. (%%) :: Int -> Int -> Int. (%%) = \\(left, right) -> left - right. answer = 42. }"),
          ("src/Lib/API.jz", "module Lib::API (value (Ops::%%), value Ops::answer) { import Lib::Operations as Ops. }"),
          ("src/App/Main.jz", "module App::Main { import Lib::API ((%%), answer). import Lib::API as API. (10 %% 3, (%%) 10 3, (10 %%) 3, (%% 3) 10, 10 API::%% 3, (API::%%) 10 3, (10 API::%%) 3, (API::%% 3) 10, 2 * 10 %% 3, answer). }")
        ]

testOperatorImportCases :: IO ()
testOperatorImportCases =
  mapM_
    check
    [ ("import Lib::Left as L. import Lib::Right as R. (10 L::%% 3 L::%% 1, 10 R::%% 3 R::%% 1).", "(6, 8)"),
      ("10 L::%% 3. import Lib::Left as L.", "7"),
      ("import Lib::Left as L. operator %% precedence 6 left. (%%) = __kernel_add. (10 L::%% 3, 10 %% 3).", "(7, 13)"),
      ("import Lib::Generic as G. ((G::%%) @Int 7 True, (7 G::%%) False).", "(7, 7)"),
      ("import Lib::PrivateAPI as P. 1 P::%% 1.", "True"),
      ("import Lib::Constrained as C. (1 C::%% 1, (C::%%) @Int 1 2).", "(True, False)")
    ]
  where
    check (body, expected) = do
      result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] (lookupSourceIn (Map.insert "src/App/Main.jz" ("module App::Main { " <> body <> " }") operatorImportSources))
      assertEqual "imported operator compile errors" [] (runCompileErrors result)
      assertEqual "imported operator runtime errors" [] (runRuntimeErrors result)
      assertEqual body (Just expected) (runOutput result)

testOperatorImportFailures :: IO ()
testOperatorImportFailures = do
  mapM_
    check
    [ ("import Lib::Left. import Lib::Right.", "E4008"),
      ("import Lib::Left as L. 1 %% 2.", "E4004"),
      ("import Lib::NonAssoc. 1 %% 2 %% 3.", "E4004"),
      ("import Lib::DeclarationOnly.", "E4015"),
      ("import Lib::Left.\n(%%) = __kernel_add.", "E4004"),
      ("import Lib::Left.\n(%%) :: Int -> Int -> Int.", "E4004"),
      ("import Lib::Left.\noperator %% precedence 7 right.", "E4004")
    ]
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (lookupSourceIn (Map.insert "src/App/Main.jz" "module App::Main {\nimport Lib::Left.\n(%%) = __kernel_add.\n}" operatorImportSources))
  assertSingleDiagnosticRelatedStart "imported operator original import" (SourceSpanIn "src/App/Main.jz" 2 1) (runCompileErrors result)
  where
    check (body, code) = do
      result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] (lookupSourceIn (Map.insert "src/App/Main.jz" ("module App::Main { " <> body <> " }") operatorImportSources))
      assertSingleDiagnosticCode body code (runCompileErrors result)
      mapM_ (\diagnostic -> assertEqual "operator diagnostics avoid storage names" False ("$operator:" `Text.isInfixOf` renderDiagnostic diagnostic)) (runCompileErrors result)

operatorImportSources :: Map.Map FilePath Text
operatorImportSources =
  Map.fromList
    [ ("src/Lib/Private.jz", "module Lib::Private (value (%%)) { operator %% precedence 6 left. data Hidden = Hidden Int. class Same(a) { same :: a -> a -> Bool. }. impl Same(Hidden) { same = \\(x, y) -> case (x, y) { | (Hidden(a), Hidden(b)) -> __kernel_equals a b }. }. helper = \\(x) -> Hidden x. (%%) = \\(x, y) -> same (helper x) (helper y). }"),
      ("src/Lib/PrivateAPI.jz", "module Lib::PrivateAPI (value (P::%%)) { import Lib::Private as P. }"),
      ("src/Lib/Left.jz", "module Lib::Left (value (%%)) { operator %% precedence 6 left. (%%) = __kernel_subtract. }"),
      ("src/Lib/Right.jz", "module Lib::Right (value (%%)) { operator %% precedence 6 right. (%%) = __kernel_subtract. }"),
      ("src/Lib/NonAssoc.jz", "module Lib::NonAssoc (value (%%)) { operator %% precedence 6 nonassoc. (%%) = __kernel_subtract. }"),
      ("src/Lib/DeclarationOnly.jz", "module Lib::DeclarationOnly (value (%%)) { operator %% precedence 6 left. }"),
      ("src/Lib/Generic.jz", "module Lib::Generic (value (%%)) { operator %% precedence 6 left. (%%) :: a -> b -> a. (%%) = \\(left, right) -> left. }"),
      ("src/Lib/Constrained.jz", "module Lib::Constrained (value (%%)) { operator %% precedence 6 left. class Match(a) { match :: a -> a -> Bool. }. impl Match(Int) { match = __kernel_equals. }. (%%) :: @{Match(a)}: a -> a -> Bool. (%%) = match. }")
    ]

testFacadeSectionCapture :: IO ()
testFacadeSectionCapture = mapM_ check ["(API::%% (__kernel_divide 1 0))", "((__kernel_divide 1 0) API::%%)"]
  where
    check section = do
      let sources =
            Map.insert "src/Lib/API.jz" "module Lib::API (value (L::%%)) { import Lib::Left as L. }" $
              Map.insert "src/App/Main.jz" ("module App::Main { import Lib::API as API. " <> section <> ". }") operatorImportSources
      result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] (lookupSourceIn sources)
      assertEqual "section compile errors" [] (runCompileErrors result)
      assertEqual "failed section has no output" Nothing (runOutput result)
      assertSingleDiagnosticCode "operand fails before section invocation" "E3001" (runRuntimeErrors result)
