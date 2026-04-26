{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSubject
  )
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
    failTest,
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
    ("reports nested module declaration parse failure in a module file", testReportsNestedModuleDeclarationParseFailure),
    ("accepts symbol-list imports when requested symbols are exported", testAcceptsValidImportSymbolList),
    ("reports non-exported import symbols with module context", testReportsMissingImportSymbol),
    ("reports import symbol collisions across imported modules", testReportsImportSymbolCollision),
    ("reports import alias collisions across imported modules", testReportsImportAliasCollision),
    ("reports unqualified references to bindings imported only by alias", testReportsUnqualifiedAliasImportReference)
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Util.\nutil.\n}"),
          ("src/Lib/Util.jz", "module Lib::Util {\nutil = 1.\n}")
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
        [("src/App/Main.jz", "module Wrong::Name {\nmain = 1.\n}")]

testReportsNestedModuleDeclarationParseFailure :: IO ()
testReportsNestedModuleDeclarationParseFailure = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "nested module parse failure code" "E4004" result
  assertLeftContains "nested module parse failure path" "src/App/Main.jz" result
  assertLeftContains "nested module parse failure text" "top-level" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nmodule App::Main {\nmain = 1.\n}\n}")
        ]

testAcceptsValidImportSymbolList :: IO ()
testAcceptsValidImportSymbolList =
  assertRight
    "valid import symbol list resolves"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nmain = add."),
          ("src/Lib/Math.jz", "add = 1.\nsub = 2.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Lib", "Math"],
            resolvedSourcePath = "src/Lib/Math.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Lib", "Math"]]
          }
      ]

testReportsMissingImportSymbol :: IO ()
testReportsMissingImportSymbol = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "missing symbol code" "E4007" result
  assertLeftContains "missing symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "missing symbol metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (subtract).\nmain = 1."),
          ("src/Lib/Math.jz", "add = 1.")
        ]

testReportsImportSymbolCollision :: IO ()
testReportsImportSymbolCollision = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "symbol collision code" "E4008" result
  assertLeftContains "symbol collision text" "symbol 'map'" result
  assertLeftContains "first module context" "A::Ops" result
  assertLeftContains "second module context" "B::Ops" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "symbol collision metadata"
    (Just (SourceSpan 2 1))
    (Just (SourceSpan 1 1))
    (Just "map")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import A::Ops (map).\nimport B::Ops (map).\nmain = map."),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsImportAliasCollision :: IO ()
testReportsImportAliasCollision = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias collision code" "E4009" result
  assertLeftContains "alias collision text" "alias collision" result
  assertLeftContains "first module context" "A::Ops" result
  assertLeftContains "second module context" "B::Ops" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias collision metadata"
    (Just (SourceSpan 2 1))
    (Just (SourceSpan 1 1))
    (Just "Ops")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import A::Ops as Ops.\nimport B::Ops as Ops.\nmain = 1."),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsUnqualifiedAliasImportReference :: IO ()
testReportsUnqualifiedAliasImportReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias visibility code" "E4012" result
  assertLeftContains "hidden symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "import alias context" "Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias visibility metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nmain = subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
        ]

assertLeftDiagnosticMetadata ::
  Show a =>
  Text ->
  Maybe SourceSpan ->
  Maybe SourceSpan ->
  Maybe Text ->
  Either Diagnostic a ->
  IO ()
assertLeftDiagnosticMetadata label expectedPrimary expectedRelated expectedSubject value =
  case value of
    Left diagnostic -> do
      assertEqual (label <> " primary span") expectedPrimary (diagnosticPrimarySpan diagnostic)
      assertEqual (label <> " related span") expectedRelated (diagnosticRelatedSpan diagnostic)
      assertEqual (label <> " subject") expectedSubject (diagnosticSubject diagnostic)
    Right ok ->
      failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))
