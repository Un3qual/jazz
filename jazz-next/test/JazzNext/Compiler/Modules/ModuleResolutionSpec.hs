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
    diagnosticSubject,
    renderDiagnostic
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
    ("preserves exact module path segments while resolving", testPreservesExactModulePathSegments),
    ("maps module path to relative .jz file", testModulePathMapping),
    ("maps nested module paths to canonical .jz files", testNestedModulePathMapping),
    ("accepts omitted module declaration from resolved source path", testAcceptsOmittedModuleDeclaration),
    ("accepts matching module declaration in resolved file", testAcceptsMatchingModuleDeclaration),
    ("resolves dependency graph in deterministic order", testResolveDependencyGraph),
    ("resolves imports in lexical rendered-path order", testResolveImportsInLexicalRenderedPathOrder),
    ("collapses duplicate imports to one dependency edge", testCollapsesDuplicateImports),
    ("reuses already-resolved modules across branches", testReusesAlreadyResolvedModuleAcrossBranches),
    ("deduplicates duplicate module roots before ambiguity checks", testDeduplicatesDuplicateRoots),
    ("reports unresolved import with importer context", testReportsUnresolvedImport),
    ("reports ambiguous module candidates across roots", testReportsAmbiguousImport),
    ("reports import cycles with minimal trace", testReportsCycle),
    ("reports nested import cycles with minimal trace", testReportsNestedCycleMinimalTrace),
    ("reports parse failures while loading imported modules", testReportsImportedModuleParseFailure),
    ("reports module declaration mismatch for resolved file path", testReportsModuleDeclarationMismatch),
    ("reports nested module declaration parse failure in a module file", testReportsNestedModuleDeclarationParseFailure),
    ("accepts symbol-list imports when requested symbols are exported", testAcceptsValidImportSymbolList),
    ("accepts symbol-list imports for data constructors", testAcceptsDataConstructorImportSymbolList),
    ("accepts type applications while collecting module references", testAcceptsTypeApplicationsWhileCollectingModuleReferences),
    ("accepts bare imports as unqualified visible exports", testAcceptsBareImportUnqualifiedExport),
    ("accepts local bindings over hidden symbol-list exports", testAcceptsLocalBindingOverHiddenExplicitImport),
    ("reports non-exported import symbols with module context", testReportsMissingImportSymbol),
    ("reports unqualified references hidden by explicit symbol lists", testReportsHiddenExplicitImportValueReference),
    ("reports import symbol collisions across imported modules", testReportsImportSymbolCollision),
    ("reports import alias collisions across imported modules", testReportsImportAliasCollision),
    ("reports pattern references to constructors hidden by explicit imports", testReportsHiddenExplicitImportConstructorPatternReference),
    ("reports unqualified references to bindings imported only by alias", testReportsUnqualifiedAliasImportReference),
    ("reports pattern references to constructors hidden by alias imports", testReportsHiddenAliasImportConstructorPatternReference),
    ("accepts qualified alias references before alias declaration", testAcceptsQualifiedAliasReferenceBeforeImport),
    ("accepts local bindings sharing qualified alias names", testAcceptsLocalBindingSharingAliasName),
    ("accepts qualified references through alias imports", testAcceptsQualifiedAliasImportReference),
    ("accepts qualified references to data constructors through alias imports", testAcceptsQualifiedAliasDataConstructorReference),
    ("reports qualified references through unknown aliases", testReportsUnknownQualifiedAliasReference),
    ("reports standalone qualified references through unknown aliases", testReportsStandaloneUnknownQualifiedAliasReference),
    ("reports qualified alias references to missing exports", testReportsMissingQualifiedAliasExport)
  ]

sharedCycleSourceFiles :: Map.Map FilePath Text
sharedCycleSourceFiles =
  Map.fromList
    [ ("src/A/One.jz", "import B::Two.\na = 1."),
      ("src/B/Two.jz", "import A::One.\nb = 2.")
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

testNestedModulePathMapping :: IO ()
testNestedModulePathMapping = do
  assertEqual
    "nested relative file path"
    "App/Core/Parser.jz"
    (modulePathToRelativeFile ["App", "Core", "Parser"])
  assertEqual
    "punctuated relative file path"
    "Lib/Build!.jz"
    (modulePathToRelativeFile ["Lib", "Build!"])

testParseModulePathContinuations :: IO ()
testParseModulePathContinuations =
  assertEqual
    "continuation chars"
    (Right ["App", "Main'", "Build!"])
    (parseModulePathText "App::Main'::Build!")

testPreservesExactModulePathSegments :: IO ()
testPreservesExactModulePathSegments =
  assertRight
    "case-distinct module paths resolve independently"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nimport lib::Util.\nmain = upperValue."),
          ("src/Lib/Util.jz", "upperValue = 1."),
          ("src/lib/Util.jz", "lowerValue = 2.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Lib", "Util"],
            resolvedSourcePath = "src/Lib/Util.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["lib", "Util"],
            resolvedSourcePath = "src/lib/Util.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Lib", "Util"], ["lib", "Util"]]
          }
      ]

testAcceptsOmittedModuleDeclaration :: IO ()
testAcceptsOmittedModuleDeclaration =
  assertRight
    "omitted declaration uses resolved source path"
    (resolveModuleGraph config sourceFiles ["App", "Nested", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Nested/Main.jz", "main = 1.")]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["App", "Nested", "Main"],
            resolvedSourcePath = "src/App/Nested/Main.jz",
            resolvedImports = []
          }
      ]

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

testResolveImportsInLexicalRenderedPathOrder :: IO ()
testResolveImportsInLexicalRenderedPathOrder =
  assertRight
    "reverse source imports resolve lexically"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Zoo::Dep.\nimport Alpha::Dep.\nmain = alpha."),
          ("src/Alpha/Dep.jz", "alpha = 1."),
          ("src/Zoo/Dep.jz", "zoo = 2.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Alpha", "Dep"],
            resolvedSourcePath = "src/Alpha/Dep.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["Zoo", "Dep"],
            resolvedSourcePath = "src/Zoo/Dep.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Alpha", "Dep"], ["Zoo", "Dep"]]
          }
      ]

testCollapsesDuplicateImports :: IO ()
testCollapsesDuplicateImports =
  assertRight
    "duplicate imports collapse"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nimport Lib::Util.\nmain = util."),
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

testReusesAlreadyResolvedModuleAcrossBranches :: IO ()
testReusesAlreadyResolvedModuleAcrossBranches =
  assertRight
    "shared dependency is reused"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import A::One.\nimport B::Two.\nmain = a."),
          ("src/A/One.jz", "import Shared::Util.\na = shared."),
          ("src/B/Two.jz", "import Shared::Util.\nb = shared."),
          ("src/Shared/Util.jz", "shared = 1.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Shared", "Util"],
            resolvedSourcePath = "src/Shared/Util.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["A", "One"],
            resolvedSourcePath = "src/A/One.jz",
            resolvedImports = [["Shared", "Util"]]
          },
        ResolvedModule
          { resolvedModulePath = ["B", "Two"],
            resolvedSourcePath = "src/B/Two.jz",
            resolvedImports = [["Shared", "Util"]]
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["A", "One"], ["B", "Two"]]
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
  assertLeftContains "ambiguous candidate order" "matched rootA/Lib/Util.jz, rootB/Lib/Util.jz" result
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
    sourceFiles = sharedCycleSourceFiles

testReportsNestedCycleMinimalTrace :: IO ()
testReportsNestedCycleMinimalTrace = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "nested cycle code" "E4003" result
  assertLeftContains "nested cycle trace" "A::One -> B::Two -> A::One" result
  assertLeftDiagnosticNotContains "nested cycle excludes entry" "App::Main" result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.insert
        "src/App/Main.jz"
        "import A::One.\nmain = a."
        sharedCycleSourceFiles

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

testAcceptsDataConstructorImportSymbolList :: IO ()
testAcceptsDataConstructorImportSymbolList =
  assertRight
    "data constructor import symbol list resolves"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe (Just).\nmain = Just 1."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Lib", "Maybe"],
            resolvedSourcePath = "src/Lib/Maybe.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Lib", "Maybe"]]
          }
      ]

testAcceptsTypeApplicationsWhileCollectingModuleReferences :: IO ()
testAcceptsTypeApplicationsWhileCollectingModuleReferences =
  assertRight
    "type applications in module reference collection"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util as Util.\nmain = Util::id @Int 1."),
          ("src/Lib/Util.jz", "id = \\(value) -> value.\nvalue = id @Int 1.")
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

testAcceptsBareImportUnqualifiedExport :: IO ()
testAcceptsBareImportUnqualifiedExport =
  assertRight
    "bare import makes exports visible"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math.\nmain = subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testAcceptsLocalBindingOverHiddenExplicitImport :: IO ()
testAcceptsLocalBindingOverHiddenExplicitImport =
  assertRight
    "local binding shadows hidden import export"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nsubtract = 0.\nmain = subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testReportsHiddenExplicitImportValueReference :: IO ()
testReportsHiddenExplicitImportValueReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "explicit hidden value code" "E4011" result
  assertLeftContains "hidden value text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "explicit hidden value metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math (add).\nmain = subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testReportsHiddenExplicitImportConstructorPatternReference :: IO ()
testReportsHiddenExplicitImportConstructorPatternReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "explicit hidden constructor code" "E4011" result
  assertLeftContains "hidden constructor text" "Just" result
  assertLeftContains "imported module context" "Lib::Maybe" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "explicit hidden constructor metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "Just")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe (Nothing).\nmain = case Nothing { | Just value -> value | _ -> 0 }."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.")
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

testReportsHiddenAliasImportConstructorPatternReference :: IO ()
testReportsHiddenAliasImportConstructorPatternReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "alias hidden constructor code" "E4012" result
  assertLeftContains "hidden constructor text" "Just" result
  assertLeftContains "imported module context" "Lib::Maybe" result
  assertLeftContains "import alias context" "Maybe" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "alias hidden constructor metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "Just")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\nmain = case Maybe::Nothing { | Just value -> value | _ -> 0 }."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.")
        ]

testAcceptsQualifiedAliasReferenceBeforeImport :: IO ()
testAcceptsQualifiedAliasReferenceBeforeImport =
  assertRight
    "qualified alias reference before import resolves"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "main = Math::subtract.\nimport Lib::Math as Math."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testAcceptsLocalBindingSharingAliasName :: IO ()
testAcceptsLocalBindingSharingAliasName =
  assertRight
    "local binding does not shadow qualified alias"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as math.\nmath = 0.\nmain = math::subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testAcceptsQualifiedAliasImportReference :: IO ()
testAcceptsQualifiedAliasImportReference =
  assertRight
    "qualified alias import reference resolves"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nmain = Math::subtract."),
          ("src/Lib/Math.jz", "add = 1.\nsubtract = 2.")
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

testAcceptsQualifiedAliasDataConstructorReference :: IO ()
testAcceptsQualifiedAliasDataConstructorReference =
  assertRight
    "qualified alias data constructor reference resolves"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Maybe as Maybe.\nmain = Maybe::Just 1."),
          ("src/Lib/Maybe.jz", "data Maybe = Just value | Nothing.")
        ]
    expectedModules =
      [ ResolvedModule
          { resolvedModulePath = ["Lib", "Maybe"],
            resolvedSourcePath = "src/Lib/Maybe.jz",
            resolvedImports = []
          },
        ResolvedModule
          { resolvedModulePath = ["App", "Main"],
            resolvedSourcePath = "src/App/Main.jz",
            resolvedImports = [["Lib", "Maybe"]]
          }
      ]

testReportsUnknownQualifiedAliasReference :: IO ()
testReportsUnknownQualifiedAliasReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "unknown alias code" "E4013" result
  assertLeftContains "unknown alias text" "Math" result
  assertLeftContains "referenced symbol text" "subtract" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "unknown alias metadata"
    Nothing
    Nothing
    (Just "Math")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "main = Math::subtract.")]

testReportsStandaloneUnknownQualifiedAliasReference :: IO ()
testReportsStandaloneUnknownQualifiedAliasReference = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "standalone unknown alias code" "E4013" result
  assertLeftContains "standalone unknown alias text" "Math" result
  assertLeftContains "standalone referenced symbol text" "subtract" result
  assertLeftContains "standalone importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "standalone unknown alias metadata"
    Nothing
    Nothing
    (Just "Math")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [("src/App/Main.jz", "Math::subtract.")]

testReportsMissingQualifiedAliasExport :: IO ()
testReportsMissingQualifiedAliasExport = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "missing qualified alias code" "E4014" result
  assertLeftContains "missing symbol text" "subtract" result
  assertLeftContains "imported module context" "Lib::Math" result
  assertLeftContains "alias context" "Math" result
  assertLeftContains "importer context" "App::Main" result
  assertLeftDiagnosticMetadata
    "missing qualified alias metadata"
    (Just (SourceSpan 1 1))
    Nothing
    (Just "subtract")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nmain = Math::subtract."),
          ("src/Lib/Math.jz", "add = 1.")
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

assertLeftDiagnosticNotContains ::
  Show a =>
  Text ->
  Text ->
  Either Diagnostic a ->
  IO ()
assertLeftDiagnosticNotContains label needle value =
  case value of
    Left diagnostic ->
      let rendered = renderDiagnostic diagnostic
       in
        if needle `Text.isInfixOf` rendered
          then failTest (label <> ": expected not to find '" <> needle <> "' in '" <> rendered <> "'")
          else pure ()
    Right ok ->
      failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))
