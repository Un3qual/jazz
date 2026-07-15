{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSubject,
    renderDiagnostic
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( diagnosticCodeText
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    ResolvedModule (..),
    modulePathToRelativeFile,
    parseModulePathText,
    resolveModuleGraph,
    resolveProgram
  )
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    exportInventoryEntries
  )
import qualified JazzNext.Compiler.ModuleGraph as ModuleGraph
import JazzNext.Compiler.Name
  ( NameNamespace (TypeNamespace, ValueNamespace)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertLeftDiagnosticCodeAndContains,
    assertRight,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModuleResolution" tests

tests :: [NamedTest]
tests =
  [ ("rejects empty entry module path before traversal", testRejectsEmptyEntryModulePath),
    ("resolved program retains lowered modules", testResolvedProgramRetainsLoweredModules),
    ("resolved module audit ignores generic type variables", testResolvedModuleAuditIgnoresGenericTypeVariables),
    ("resolved module carries explicit public inventory", testResolvedModuleCarriesExplicitPublicInventory),
    ("empty export list produces empty inventory", testEmptyExportListProducesEmptyInventory),
    ("namespace-aware exports select exact public entries", testNamespaceAwareExportsSelectExactEntries),
    ("namespace-aware exports reject same-name wrong namespace", testNamespaceAwareExportsRejectWrongNamespace),
    ("namespace-aware export diagnostics render an empty inventory", testNamespaceAwareExportDiagnosticRendersEmptyInventory),
    ("explicit exports keep private local bindings resolvable", testExplicitExportsKeepPrivateLocalsUsable),
    ("rejects unknown module export names", testRejectsUnknownModuleExport),
    ("rejects imported-only module export names", testRejectsImportedOnlyModuleExport),
    ("explicit imports reject private module bindings", testExplicitImportRejectsPrivateModuleBinding),
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
    ("deduplicates lexically equivalent module roots before ambiguity checks", testDeduplicatesEquivalentRoots),
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
    ("reports import symbol collisions across bare imports", testReportsBareImportSymbolCollision),
    ("reports import symbol collisions across bare and symbol-list imports", testReportsMixedImportSymbolCollision),
    ("reports import alias collisions across imported modules", testReportsImportAliasCollision),
    ("reports pattern references to constructors hidden by explicit imports", testReportsHiddenExplicitImportConstructorPatternReference),
    ("reports unqualified references to bindings imported only by alias", testReportsUnqualifiedAliasImportReference),
    ("reports pattern references to constructors hidden by alias imports", testReportsHiddenAliasImportConstructorPatternReference),
    ("accepts qualified alias references before alias declaration", testAcceptsQualifiedAliasReferenceBeforeImport),
    ("accepts local bindings sharing qualified alias names", testAcceptsLocalBindingSharingAliasName),
    ("accepts qualified references through alias imports", testAcceptsQualifiedAliasImportReference),
    ("accepts qualified references to data constructors through alias imports", testAcceptsQualifiedAliasDataConstructorReference),
    ("accepts explicit class import symbols", testAcceptsExplicitClassImportSymbol),
    ("rejects type-only explicit import symbols", testRejectsTypeOnlyImportSymbol),
    ("reports class import collisions", testReportsClassImportCollision),
    ("reports type import collisions", testReportsTypeImportCollision),
    ("keeps repeated class imports idempotent", testKeepsRepeatedClassImportsIdempotent),
    ("reports qualified references through unknown aliases", testReportsUnknownQualifiedAliasReference),
    ("reports standalone qualified references through unknown aliases", testReportsStandaloneUnknownQualifiedAliasReference),
    ("reports qualified alias references to missing exports", testReportsMissingQualifiedAliasExport),
    ("implementation methods inventory hidden unqualified references", testImplMethodRejectsHiddenUnqualifiedReference),
    ("implementation methods inventory hidden qualified references", testImplMethodRejectsHiddenQualifiedReference),
    ("module lexer failures retain source-qualified structured detail", testModuleLexerFailureRetainsStructuredDetail)
  ]

testResolvedProgramRetainsLoweredModules :: IO ()
testResolvedProgramRetainsLoweredModules = do
  result <-
    resolveProgram
      resolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["App", "Main"]
  assertRight "resolved program" result $ \program -> do
    assertEqual
      "module order"
      [["Lib", "Value"], ["App", "Main"]]
      (map ModuleGraph.resolvedModulePath (ModuleGraph.resolvedProgramModules program))
    assertEqual "entry path" ["App", "Main"] (ModuleGraph.resolvedProgramEntryPath program)
    assertEqual "module count" 2 (length (ModuleGraph.resolvedProgramModules program))
    assertEqual
      "unresolved core names"
      []
      (concatMap ModuleGraph.unresolvedResolvedModuleNames (ModuleGraph.resolvedProgramModules program))
  where
    resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testResolvedModuleAuditIgnoresGenericTypeVariables :: IO ()
testResolvedModuleAuditIgnoresGenericTypeVariables = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "resolved generic module" result $ \program ->
    assertEqual
      "unresolved generic module names"
      []
      (concatMap ModuleGraph.unresolvedResolvedModuleNames (ModuleGraph.resolvedProgramModules program))
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            id :: a -> a.
            id = \\(value) -> value.
            id 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sources)

testResolvedModuleCarriesExplicitPublicInventory :: IO ()
testResolvedModuleCarriesExplicitPublicInventory = do
  result <-
    resolveProgram
      testResolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["App", "Main"]
  assertRight "resolved explicit public inventory" result $ \program ->
    case
        [ resolvedModule
          | resolvedModule <- ModuleGraph.resolvedProgramModules program,
            ModuleGraph.resolvedModulePath resolvedModule == ["Lib", "Value"]
        ] of
      [resolvedModule] ->
        assertEqual
          "public inventory contains only answer"
          (Set.singleton (ModuleExport ValueNamespace "answer"))
          ( exportInventoryEntries
              (ModuleGraph.resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Value module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value (answer).
        answer.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value (answer) {
          helper = 1.
          answer = helper.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

testEmptyExportListProducesEmptyInventory :: IO ()
testEmptyExportListProducesEmptyInventory = do
  result <-
    resolveProgram
      testResolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["App", "Main"]
  assertRight "resolved empty public inventory" result $ \program ->
    case
        [ resolvedModule
          | resolvedModule <- ModuleGraph.resolvedProgramModules program,
            ModuleGraph.resolvedModulePath resolvedModule == ["Lib", "Value"]
        ] of
      [resolvedModule] ->
        assertEqual
          "public inventory is empty"
          Set.empty
          ( exportInventoryEntries
              (ModuleGraph.resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Value module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value.
        0.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value () {
          hidden = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportsSelectExactEntries :: IO ()
testNamespaceAwareExportsSelectExactEntries = do
  result <-
    resolveProgram
      testResolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["Lib", "Box"]
  assertRight "resolved namespace-aware public inventory" result $ \program ->
    case ModuleGraph.resolvedProgramModules program of
      [resolvedModule] ->
        assertEqual
          "public inventory contains exact type and value exports"
          ( Set.fromList
              [ ModuleExport TypeNamespace "Box",
                ModuleExport ValueNamespace "Box"
              ]
          )
          ( exportInventoryEntries
              (ModuleGraph.resolvedModuleExportInventory resolvedModule)
          )
      modules -> failTest ("expected one resolved Lib::Box module, got " <> Text.pack (show (length modules)))
  where
    sources =
      Map.singleton
        "src/Lib/Box.jz"
        """
        module Lib::Box (type Box, value Box) {
        data Box = Box payload.
        Box = 1.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportsRejectWrongNamespace :: IO ()
testNamespaceAwareExportsRejectWrongNamespace = do
  result <-
    resolveProgram
      testResolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["Lib", "Token"]
  assertLeftDiagnosticCodeAndContains
    "wrong namespace module export"
    "E4015"
    "module export type 'Token' is not declared by module 'Lib::Token'"
    result
  where
    sources =
      Map.singleton
        "src/Lib/Token.jz"
        """
        module Lib::Token (type Token) {
        data Box = Token.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testNamespaceAwareExportDiagnosticRendersEmptyInventory :: IO ()
testNamespaceAwareExportDiagnosticRendersEmptyInventory = do
  result <-
    resolveProgram
      testResolverConfig
      ResolveKernelOnly
      Set.empty
      Set.empty
      lookupSource
      ["Lib", "Empty"]
  assertLeftDiagnosticCodeAndContains
    "empty namespace-aware module export inventory"
    "E4015"
    "available declarations: <none>"
    result
  where
    sources =
      Map.singleton
        "src/Lib/Empty.jz"
        """
        module Lib::Empty (type Missing) {
        0.
        }
        """
    lookupSource path = pure (Map.lookup path sources)

testExplicitExportsKeepPrivateLocalsUsable :: IO ()
testExplicitExportsKeepPrivateLocalsUsable = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "private local remains resolvable" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value (answer).
        answer.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value (answer) {
          helper = 1.
          answer = helper.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsUnknownModuleExport :: IO ()
testRejectsUnknownModuleExport = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["Lib", "Value"]
  assertLeftDiagnosticCodeAndContains
    "unknown module export"
    "E4015"
    "module export 'missing' is not declared by module 'Lib::Value'"
    result
  assertLeftDiagnosticMetadata
    "unknown module export metadata"
    (Just (SourceSpanIn "src/Lib/Value.jz" 1 1))
    Nothing
    (Just "missing")
    result
  where
    sources = Map.singleton "src/Lib/Value.jz" """
    module Lib::Value (missing) {
    answer = 1.
    }
    """
    lookupSource path = pure (Map.lookup path sources)

testRejectsImportedOnlyModuleExport :: IO ()
testRejectsImportedOnlyModuleExport = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["Lib", "Wrapper"]
  assertLeftDiagnosticCodeAndContains
    "imported-only module export"
    "E4015"
    "module export 'answer' is not declared by module 'Lib::Wrapper'"
    result
  where
    sources =
      Map.fromList
        [ ("src/Lib/Wrapper.jz", """
        module Lib::Wrapper (answer) {
        import Lib::Origin (answer).
        wrapper = answer.
        }
        """),
          ("src/Lib/Origin.jz", """
          module Lib::Origin {
          answer = 1.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

testExplicitImportRejectsPrivateModuleBinding :: IO ()
testExplicitImportRejectsPrivateModuleBinding = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "private explicit import"
    "E4007"
    "import symbol 'helper' is not exported by module 'Lib::Value'"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Value (helper).
        helper.
        }
        """),
          ("src/Lib/Value.jz", """
          module Lib::Value (answer) {
          helper = 1.
          answer = helper.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

sharedCycleSourceFiles :: Map.Map FilePath Text
sharedCycleSourceFiles =
  Map.fromList
    [ ("src/A/One.jz", """
    import B::Two.
    a = 1.
    """),
      ("src/B/Two.jz", """
      import A::One.
      b = 2.
      """)
    ]

testRejectsEmptyEntryModulePath :: IO ()
testRejectsEmptyEntryModulePath =
  assertLeftDiagnosticCodeAndContains
    "empty entry path"
    "E4016"
    "empty entry module path"
    (resolveModuleGraph config sourceFiles [])
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Util.
        util.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Util.
        import lib::Util.
        main = upperValue.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Util.
        main = util.
        """),
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
        [ ("src/App/Main.jz", """
        import Zoo::Dep.
        import Alpha::Dep.
        main = alpha.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Util.
        import Lib::Util.
        main = util.
        """),
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
        [ ("src/App/Main.jz", """
        import A::One.
        import B::Two.
        main = a.
        """),
          ("src/A/One.jz", """
          import Shared::Util.
          a = shared.
          """),
          ("src/B/Two.jz", """
          import Shared::Util.
          b = shared.
          """),
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
        [ ("src/App/Main.jz", """
        import Lib::Util.
        util.
        """),
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

testDeduplicatesEquivalentRoots :: IO ()
testDeduplicatesEquivalentRoots =
  assertRight
    "equivalent roots are not treated as ambiguity"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config =
      ModuleResolutionConfig
        { moduleRoots = ["src", "src/."],
          moduleExtension = ".jz"
        }
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Util.
        util.
        """),
          ("src/./App/Main.jz", """
          import Lib::Util.
          util.
          """),
          ("src/Lib/Util.jz", "util = 1."),
          ("src/./Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModule ["Lib", "Util"] "src/Lib/Util.jz" [],
        ResolvedModule ["App", "Main"] "src/App/Main.jz" [["Lib", "Util"]]
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
        [("src/App/Main.jz", """
        import Missing::Thing.
        main = 1.
        """)]

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
        [ ("rootA/App/Main.jz", """
        import Lib::Util.
        main = util.
        """),
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
        """
        import A::One.
        main = a.
        """
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
        [ ("src/App/Main.jz", """
        import Lib::Util.
        main = util.
        """),
          ("src/Lib/Util.jz", "broken = .")
        ]

testModuleLexerFailureRetainsStructuredDetail :: IO ()
testModuleLexerFailureRetainsStructuredDetail = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  case result of
    Left diagnostic -> do
      assertEqual "module lexer failure code" "E4004" (diagnosticCodeText (diagnosticCode diagnostic))
      assertEqual
        "module lexer failure qualified primary span"
        (Just (SourceSpanIn "src/Lib/Bad.jz" 1 10))
        (diagnosticPrimarySpan diagnostic)
      assertLeftContains "module lexer original detail" "unterminated text literal" result
    Right modules -> failTest ("expected module lexer failure, got " <> Text.pack (show modules))
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Bad.
        main = 1.
        """),
          ("src/Lib/Bad.jz", "broken = \"unterminated")
        ]

testImplMethodRejectsHiddenUnqualifiedReference :: IO ()
testImplMethodRejectsHiddenUnqualifiedReference = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "implementation method hidden unqualified reference"
    "E4011"
    "helper"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Value (answer).
            class Use(a) { use :: a -> Int. }.
            impl Use(Int) { use = \\(value) -> helper. }.
            main = answer.
            """
          ),
          ("src/Lib/Value.jz", "module Lib::Value { helper = 41. answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testImplMethodRejectsHiddenQualifiedReference :: IO ()
testImplMethodRejectsHiddenQualifiedReference = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "implementation method hidden qualified reference"
    "E4014"
    "helper"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Value as Value.
            class Use(a) { use :: a -> Int. }.
            impl Use(Int) { use = \\(value) -> Value::helper. }.
            main = 1.
            """
          ),
          ("src/Lib/Value.jz", "module Lib::Value (answer) { helper = 41. answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

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
        [("src/App/Main.jz", """
        module Wrong::Name {
        main = 1.
        }
        """)]

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
        [ ("src/App/Main.jz", """
        module App::Main {
        module App::Main {
        main = 1.
        }
        }
        """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        main = add.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          sub = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe (Just).
        main = Just 1.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Util as Util.
        main = Util::id @Int 1.
        """),
          ("src/Lib/Util.jz", """
          id = \\(value) -> value.
          value = id @Int 1.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math.
        main = subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        subtract = 0.
        main = subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math (subtract).
        main = 1.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Math (add).
        main = subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import A::Ops (map).
        import B::Ops (map).
        main = map.
        """),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsBareImportSymbolCollision :: IO ()
testReportsBareImportSymbolCollision = do
  assertCollision "A then B" """
  import A::Ops.
  import B::Ops.
  main = map.
  """
  assertCollision "B then A" """
  import B::Ops.
  import A::Ops.
  main = map.
  """
  where
    assertCollision label importerSource = do
      let result = resolveModuleGraph config (sourceFiles importerSource) ["App", "Main"]
      assertLeftContains (label <> " collision code") "E4008" result
      assertLeftContains (label <> " collision symbol") "symbol 'map'" result
      assertLeftDiagnosticMetadata
        (label <> " collision metadata")
        (Just (SourceSpan 2 1))
        (Just (SourceSpan 1 1))
        (Just "map")
        result

    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles importerSource =
      Map.fromList
        [ ("src/App/Main.jz", importerSource),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsMixedImportSymbolCollision :: IO ()
testReportsMixedImportSymbolCollision = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "mixed collision code" "E4008" result
  assertLeftContains "mixed collision symbol" "symbol 'map'" result
  assertLeftDiagnosticMetadata
    "mixed collision metadata"
    (Just (SourceSpan 2 1))
    (Just (SourceSpan 1 1))
    (Just "map")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", """
        import A::Ops.
        import B::Ops (map).
        main = map.
        """),
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
        [ ("src/App/Main.jz", """
        import A::Ops as Ops.
        import B::Ops as Ops.
        main = 1.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe (Nothing).
        main = case Nothing { | Just value -> value | _ -> 0 }.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        main = subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        main = case Maybe::Nothing { | Just value -> value | _ -> 0 }.
        """),
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
        [ ("src/App/Main.jz", """
        main = Math::subtract.
        import Lib::Math as Math.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as math.
        math = 0.
        main = math::subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        main = Math::subtract.
        """),
          ("src/Lib/Math.jz", """
          add = 1.
          subtract = 2.
          """)
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
        [ ("src/App/Main.jz", """
        import Lib::Maybe as Maybe.
        main = Maybe::Just 1.
        """),
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
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        main = Math::subtract.
        """),
          ("src/Lib/Math.jz", "add = 1.")
        ]

testResolverConfig :: ModuleResolutionConfig
testResolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

testAcceptsExplicitClassImportSymbol :: IO ()
testAcceptsExplicitClassImportSymbol = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "explicit class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Facts (Eq).
        x :: @{Eq(Int)}: Int.
        x = 1.
        """),
          ("src/Lib/Facts.jz", """
          class Eq(a) { }.
          impl Eq(Int) { }.
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsTypeOnlyImportSymbol :: IO ()
testRejectsTypeOnlyImportSymbol = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "type-only import"
    "E4007"
    "import symbol 'Optional' is not exported"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Types (Optional).
        x = 1.
        """),
          ("src/Lib/Types.jz", "data Optional a = Some a | None.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsClassImportCollision :: IO ()
testReportsClassImportCollision = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "class import collision"
    "E4008"
    "import binding collision for symbol 'Eq'"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        import A::Facts.
        import B::Facts.
        x = 1.
        """),
          ("src/A/Facts.jz", "class Eq(a) { }."),
          ("src/B/Facts.jz", "class Eq(a) { }.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testReportsTypeImportCollision :: IO ()
testReportsTypeImportCollision = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "type import collision"
    "E4008"
    "import type collision for 'Box'"
    result
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import A::Types.
            import B::Types.
            value :: Box(Int).
            value = ABox 1.
            """
          ),
          ("src/A/Types.jz", "data Box a = ABox a."),
          ("src/B/Types.jz", "data Box a = BBox a.")
        ]
    lookupSource path = pure (Map.lookup path sources)

testKeepsRepeatedClassImportsIdempotent :: IO ()
testKeepsRepeatedClassImportsIdempotent = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "repeated class import" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Facts.
        import Lib::Facts.
        x :: @{Eq(Int)}: Int.
        x = 1.
        """),
          ("src/Lib/Facts.jz", """
          class Eq(a) { }.
          impl Eq(Int) { }.
          """)
        ]
    lookupSource path = pure (Map.lookup path sources)

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
