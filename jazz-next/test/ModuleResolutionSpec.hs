{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    resolveModuleGraph
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModuleResolution" tests

tests :: [NamedTest]
tests =
  [ ("maps module path to relative .jz file", testModulePathMapping),
    ("resolves dependency graph in deterministic order", testResolveDependencyGraph),
    ("reports unresolved import with importer context", testReportsUnresolvedImport),
    ("reports ambiguous module candidates across roots", testReportsAmbiguousImport),
    ("reports import cycles with minimal trace", testReportsCycle),
    ("reports parse failures while loading imported modules", testReportsImportedModuleParseFailure)
  ]

testModulePathMapping :: IO ()
testModulePathMapping =
  assertEqual
    "relative file path"
    "App/Core.jz"
    (modulePathToRelativeFile ["App", "Core"])

testResolveDependencyGraph :: IO ()
testResolveDependencyGraph =
  assertRight
    "resolve graph"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nmain = util."),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Lib", "Util"],
            resolvedSourcePath = "src/Lib/Util.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Lib", "Util"]]
          }
      ]

testReportsUnresolvedImport :: IO ()
testReportsUnresolvedImport = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "unresolved code" "E4001" result
  assertLeftContains "unresolved module" "Missing::Thing" result
  assertLeftContains "importer context" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "import Missing::Thing.\nmain = 1.")]

testReportsAmbiguousImport :: IO ()
testReportsAmbiguousImport = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "ambiguous code" "E4002" result
  assertLeftContains "ambiguous first candidate" "rootA/Lib/Util.jz" result
  assertLeftContains "ambiguous second candidate" "rootB/Lib/Util.jz" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["rootA", "rootB"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("rootA/App/Main.jz", "import Lib::Util.\nmain = util."),
          ("rootA/Lib/Util.jz", "util = 1."),
          ("rootB/Lib/Util.jz", "util = 2.")
        ]

testReportsCycle :: IO ()
testReportsCycle = do
  let result = resolveModuleGraph config sourceFiles ["A", "One"]
  assertLeftContains "cycle code" "E4003" result
  assertLeftContains "cycle trace" "A::One -> B::Two -> A::One" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/A/One.jz", "import B::Two.\na = 1."),
          ("src/B/Two.jz", "import A::One.\nb = 2.")
        ]

testReportsImportedModuleParseFailure :: IO ()
testReportsImportedModuleParseFailure = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "parse failure code" "E4004" result
  assertLeftContains "parse failure path" "src/Lib/Util.jz" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nmain = util."),
          ("src/Lib/Util.jz", "broken = .")
        ]
