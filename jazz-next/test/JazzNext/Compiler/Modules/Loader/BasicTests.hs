{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.BasicTests
  ( basicTests
  ) where

import qualified Data.Map.Strict as Map
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( ResolvedPrelude (..),
    RunResult (..),
    compileErrors,
    compileModuleGraph,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude,
    runRuntimeErrors,
    runWarnings
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest
  )
import JazzNext.Compiler.Modules.Loader.Shared

basicTests :: [NamedTest]
basicTests =
  [ ("compile module graph succeeds for resolvable entry module", testCompileModuleGraphSuccess)
    , ("run module graph produces runtime output from entry module", testRunModuleGraphSuccess)
    , ("compile module graph default helper loads bundled prelude", testCompileModuleGraphDefaultLoadsBundledPrelude)
    , ("run module graph explicit prelude exposes public helpers across files", testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles)
    , ("run module graph ignores dependency expression statements", testRunModuleGraphIgnoresDependencyExpressions)
    , ("compile module graph validates dependency expression statements", testCompileModuleGraphValidatesDependencyExpressions)
    , ("run module graph validates dependency expression statements before runtime", testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime)
    , ("compile module graph qualifies semantic diagnostic spans with source paths", testCompileModuleGraphQualifiesSemanticDiagnosticSpans)
    , ("compile module graph qualifies explicit type application diagnostic spans", testCompileModuleGraphQualifiesExplicitTypeApplicationDiagnosticSpans)
    , ("compile module graph reports module source parse diagnostics", testCompileModuleGraphParseFailure)
    , ("run module graph skips unused dependency bindings during module evaluation", testRunModuleGraphSkipsUnusedDependencyBindingsDuringEvaluation)
    , ("run module graph qualifies sibling data fields across modules", testRunModuleGraphQualifiesSiblingDataFieldsAcrossModules)
    , ("loader reuses memoized source lookup across resolution and compilation", testMemoizedLookupReuse)
  ]

testCompileModuleGraphSuccess :: IO ()
testCompileModuleGraphSuccess = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Util.
        util.
        }
        """),
          ("src/Lib/Util.jz", """
          module Lib::Util {
          util = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphSuccess :: IO ()
testRunModuleGraphSuccess = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Util.
        util.
        }
        """),
          ("src/Lib/Util.jz", """
          module Lib::Util {
          util = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphDefaultLoadsBundledPrelude :: IO ()
testCompileModuleGraphDefaultLoadsBundledPrelude = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Data.
        map hd values.
        }
        """),
          ("src/Lib/Data.jz", """
          module Lib::Data {
          values = [[1, 2], [3], [4, 5]].
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles :: IO ()
testRunModuleGraphExplicitPreludeExposesPublicHelpersAcrossFiles = do
  result <-
    runModuleGraphWithResolvedPrelude
      defaultWarningSettings
      (PreludeExplicit """
      __kernel_map = __kernel_map.
      __kernel_hd = __kernel_hd.
      map = __kernel_map.
      hd = __kernel_hd.
      """)
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "[1, 3]") (runOutput result)
  where
    sourceMap = moduleGraphProjectedSources "map hd values"
    lookupSource = lookupSourceIn sourceMap

testRunModuleGraphIgnoresDependencyExpressions :: IO ()
testRunModuleGraphIgnoresDependencyExpressions = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Util.
        util.
        }
        """),
          ("src/Lib/Util.jz", """
          module Lib::Util {
          util = 1.
          1 / 0.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphValidatesDependencyExpressions :: IO ()
testCompileModuleGraphValidatesDependencyExpressions = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] ->
      assertContains
        "signature adjacency"
        "must be immediately followed by a matching binding"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one dependency signature adjacency error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Util.
        util.
        }
        """),
          ("src/Lib/Util.jz", """
          module Lib::Util {
          util :: Int.
          True.
          util = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime :: IO ()
testRunModuleGraphValidatesDependencyExpressionsBeforeRuntime = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output is suppressed" Nothing (runOutput result)
  case runCompileErrors result of
    [err] ->
      assertContains
        "dependency validation error"
        "must be immediately followed by a matching binding"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one dependency validation compile error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Util.
        util.
        }
        """),
          ("src/Lib/Util.jz", """
          module Lib::Util {
          util :: Int.
          True.
          util = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphQualifiesSemanticDiagnosticSpans :: IO ()
testCompileModuleGraphQualifiesSemanticDiagnosticSpans = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] -> do
      assertContains "semantic error code" "E2005" (renderDiagnostic err)
      assertContains "semantic primary source" "src/Lib/Bad.jz:1:1" (renderDiagnostic err)
      assertContains "semantic related source" "related src/Lib/Bad.jz:2:1" (renderDiagnostic err)
    _ -> failTest "expected exactly one source-qualified dependency semantic error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Bad (x).
        x.
        """),
          ("src/Lib/Bad.jz", """
          x :: Int.
          x = True.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphQualifiesExplicitTypeApplicationDiagnosticSpans :: IO ()
testCompileModuleGraphQualifiesExplicitTypeApplicationDiagnosticSpans = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] -> do
      assertContains "explicit type application error code" "E2009" (renderDiagnostic err)
      assertContains "explicit type application source" "src/App/Main.jz:3:" (renderDiagnostic err)
    _ -> failTest "expected exactly one source-qualified explicit type application error"
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            id = \\(value) -> value.
            result = id @Unknown 1.
            result.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphParseFailure :: IO ()
testCompileModuleGraphParseFailure = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "warnings" [] (compileWarnings result)
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "module parse code" "E4004" rendered
      assertContains "module parse path" "src/App/Main.jz" rendered
      assertContains "fail-fast module syntax" "expected '{'" rendered
    _ -> failTest "expected exactly one module parse error"
  where
    sourceMap =
      Map.fromList
        [("src/App/Main.jz", "module App::Main.")]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphSkipsUnusedDependencyBindingsDuringEvaluation :: IO ()
testRunModuleGraphSkipsUnusedDependencyBindingsDuringEvaluation = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Util.
            util.
            }
            """
          ),
          ( "src/Lib/Util.jz",
            """
            module Lib::Util {
            util = 1.
            bomb = 1 / 0.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphQualifiesSiblingDataFieldsAcrossModules :: IO ()
testRunModuleGraphQualifiesSiblingDataFieldsAcrossModules = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "Outer(Inner)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (makeOuter).
            makeOuter.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            data Inner = Inner.
            data Outer = Outer Inner.
            makeOuter = Outer Inner.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testMemoizedLookupReuse :: IO ()
testMemoizedLookupReuse = do
  readCountsRef <- newIORef (Map.empty :: Map.Map FilePath Int)
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (lookupSource readCountsRef)
  readCounts <- readIORef readCountsRef
  assertEqual "run succeeds" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  assertEqual
    "entry module read exactly once"
    (Just 1)
    (Map.lookup "src/App/Main.jz" readCounts)
  assertEqual
    "dependency module read exactly once"
    (Just 1)
    (Map.lookup "src/Lib/Util.jz" readCounts)
  where
    lookupSource readCountsRef path = do
      readCounts <- readIORef readCountsRef
      let previousReads = Map.findWithDefault 0 path readCounts
          nextReadCount = previousReads + 1
      writeIORef readCountsRef (Map.insert path nextReadCount readCounts)
      pure (lookupByReadCount path nextReadCount)

    lookupByReadCount :: FilePath -> Int -> Maybe Text
    lookupByReadCount path readCount =
      case path of
        -- Without memoization this second read would replace the resolver-accepted
        -- source and fail compilation. Memoized lookup should keep first-read content.
        "src/App/Main.jz"
          | readCount == 1 -> Just """
          module App::Main {
          import Lib::Util.
          util.
          }
          """
          | otherwise -> Just "broken = ."
        "src/Lib/Util.jz" -> Just """
        module Lib::Util {
        util = 1.
        }
        """
        _ -> Nothing
