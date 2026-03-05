{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    parseModulePathText,
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
  [ ("rejects empty entry module path before traversal", testRejectsEmptyEntryModulePath),
    ("accepts lexer-compatible continuation characters in CLI module paths", testParseModulePathContinuations),
    ("maps module path to relative .jz file", testModulePathMapping),
    ("accepts matching module declaration in resolved file", testAcceptsMatchingModuleDeclaration),
    ("resolves dependency graph in deterministic order", testResolveDependencyGraph),
    ("deduplicates duplicate module roots before ambiguity checks", testDeduplicatesDuplicateRoots),
    ("reports unresolved import with importer context", testReportsUnresolvedImport),
    ("reports ambiguous module candidates across roots", testReportsAmbiguousImport),
    ("reports import cycles with minimal trace", testReportsCycle),
    ("reports parse failures while loading imported modules", testReportsImportedModuleParseFailure),
    ("reports module declaration mismatch for resolved file path", testReportsModuleDeclarationMismatch),
    ("reports duplicate module declarations in a module file", testReportsDuplicateModuleDeclaration)
  ]

testRejectsEmptyEntryModulePath :: IO ()
testRejectsEmptyEntryModulePath =
  assertLeftContains
    "empty entry path"
    "empty entry module path"
    (resolveModuleGraph config sourceFiles [])
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "util = 1.")
        ]

testModulePathMapping :: IO ()
testModulePathMapping =
  assertEqual
    "relative file path"
    "App/Core.jz"
    (modulePathToRelativeFile ["App", "Core"])

testParseModulePathContinuations :: IO ()
testParseModulePathContinuations =
  assertEqual
    "continuation chars"
    (Right ["App", "Main'", "Build!"])
    (parseModulePathText "App::Main'::Build!")

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

testAcceptsMatchingModuleDeclaration :: IO ()
testAcceptsMatchingModuleDeclaration =
  assertRight
    "matching declaration is accepted"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main.\nimport Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "module Lib::Util.\nutil = 1.")
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

testDeduplicatesDuplicateRoots :: IO ()
testDeduplicatesDuplicateRoots =
  assertRight
    "duplicate roots are not treated as ambiguity"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config =
      ModuleResolutionConfig
        { moduleRoots = ["src", "src"],
          moduleExtension = ".jz"
        }
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
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

testReportsModuleDeclarationMismatch :: IO ()
testReportsModuleDeclarationMismatch = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "mismatch code" "E4006" result
  assertLeftContains "declared module name" "Wrong::Name" result
  assertLeftContains "expected module name" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "module Wrong::Name.\nmain = 1.")]

testReportsDuplicateModuleDeclaration :: IO ()
testReportsDuplicateModuleDeclaration = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "duplicate declaration code" "E4005" result
  assertLeftContains "duplicate declaration text" "multiple module declarations" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main.\nmodule App::Main.\nmain = 1.")
        ]
