{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.CapabilitiesTests
  ( capabilitiesTests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileModuleGraph,
    compileModuleGraphWithPrelude,
    runModuleGraph,
    runModuleGraphWithPrelude
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

capabilitiesTests :: [NamedTest]
capabilitiesTests =
  [ ("compile module graph default helper exposes bundled capability facts in modules", testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules)
    , ("compile module graph hides capability facts excluded by explicit import list", testCompileModuleGraphExplicitImportListHidesCapabilityFacts)
    , ("compile module graph keeps alias-qualified ADT equality distinct from local ADT", testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct)
    , ("compile module graph resolves alias-qualified impl method references", testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences)
    , ("compile module graph rewrites hidden impl method references", testCompileModuleGraphRewritesHiddenImplMethodReferences)
    , ("compile module graph keeps module ADT impl facts distinct", testCompileModuleGraphKeepsModuleAdtImplFactsDistinct)
    , ("compile module graph preserves constrained schemes through export bridges", testCompileModuleGraphPreservesConstrainedSchemesThroughExportBridges)
    , ("run module graph retains local capabilities needed by inferred equality export", testRunModuleGraphRetainsLocalCapabilitiesNeededByInferredEqualityExport)
    , ("run module graph allows structural equality through hidden inferred equality export", testRunModuleGraphAllowsStructuralEqualityThroughHiddenInferredEqualityExport)
    , ("run module graph keeps inferred equality export facts scoped to hidden capability", testRunModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability)
    , ("run module graph keeps helper-only inferred equality hidden despite direct sibling import", testRunModuleGraphKeepsHelperOnlyInferredEqualityHiddenDespiteDirectSiblingImport)
    , ("compile module graph keeps inferred equality export facts scoped to hidden capability", testCompileModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability)
    , ("run module graph retains imported capability facts referenced by inferred export", testCompileModuleGraphRetainsImportedCapabilityFactsReferencedByInferredExport)
    , ("run module graph keeps imported-class impl visible when helper is selected", testRunModuleGraphKeepsImportedClassImplVisibleWhenHelperIsSelected)
    , ("compile module graph keeps sibling capability facts isolated", testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated)
    , ("compile module graph exposes capability facts through visible imports", testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports)
    , ("run module graph allows bundled class-qualified method lookup", testRunModuleGraphAllowsBundledClassQualifiedMethodLookup)
    , ("run module graph keeps Char and Text primitive impl targets unqualified", testRunModuleGraphKeepsCharAndTextPrimitiveImplTargetsUnqualified)
    , ("run module graph allows imported class-qualified method lookup", testRunModuleGraphAllowsImportedClassQualifiedMethodLookup)
    , ("compile module graph rejects alias-only imported class-qualified method lookup", testCompileModuleGraphRejectsAliasOnlyImportedClassQualifiedMethodLookup)
    , ("run module graph allows imported pre-module class-qualified method lookup", testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup)
    , ("run module graph keeps hidden impls out of runtime dispatch", testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch)
    , ("run module graph retains local capabilities needed by exported bindings", testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings)
    , ("run module graph retains local capabilities needed by imported capability bodies", testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies)
    , ("run module graph namespaces capabilities needed by directly imported capability bodies", testRunModuleGraphNamespacesCapabilitiesNeededByDirectlyImportedCapabilityBodies)
    , ("run module graph retains value dependencies needed by imported capability bodies", testRunModuleGraphRetainsValueDependenciesNeededByImportedCapabilityBodies)
    , ("run module graph keeps inferred runtime hints module scoped", testRunModuleGraphKeepsInferredRuntimeHintsModuleScoped)
    , ("run module graph keeps nested inferred runtime hints module scoped", testRunModuleGraphKeepsNestedInferredRuntimeHintsModuleScoped)
    , ("run module graph keeps pre-module inferred runtime hints module scoped", testRunModuleGraphKeepsPreModuleInferredRuntimeHintsModuleScoped)
    , ("run module graph rebases explicit generic ADT application hints", testRunModuleGraphRebasesExplicitGenericAdtApplicationHints)
    , ("run module graph rebases fallback explicit generic ADT hints", testRunModuleGraphRebasesFallbackExplicitGenericAdtHints)
    , ("run module graph rebases class method argument signatures", testRunModuleGraphRebasesClassMethodArgumentSignatures)
    , ("run module graph retains local capabilities needed by imported signatures", testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures)
    , ("run module graph namespaces hidden retained local capabilities", testRunModuleGraphNamespacesHiddenRetainedLocalCapabilities)
    , ("run module graph namespaces alias-retained local capabilities", testRunModuleGraphNamespacesAliasRetainedLocalCapabilities)
    , ("run module graph rewrites hidden capability references despite value shadowing", testRunModuleGraphRewritesHiddenCapabilityReferencesDespiteValueShadowing)
    , ("run module graph exposes data referenced by imported class methods", testRunModuleGraphExposesDataReferencedByImportedClassMethods)
    , ("run module graph preserves imported generic constructor payload dispatch", testRunModuleGraphPreservesImportedGenericConstructorPayloadDispatch)
    , ("run module graph keeps imported ADT names in type positions", testRunModuleGraphKeepsImportedAdtNamesInTypePositions)
    , ("run module graph rebases dependency class method result hints", testRunModuleGraphRebasesDependencyClassMethodResultHints)
    , ("run module graph rebases imported class method result hints from the class origin", testRunModuleGraphRebasesImportedClassMethodResultHintsFromClassOrigin)
    , ("compile module graph rejects classes that collide with the ambient prelude", testCompileModuleGraphRejectsAmbientClassCollision)
    , ("compile module graph rejects classes that collide with visible imported classes", testCompileModuleGraphRejectsImportedClassCollision)
    , ("compile module graph does not re-export imported classes", testCompileModuleGraphDoesNotReexportImportedClasses)
    , ("run module graph publishes explicitly exported class", testRunModuleGraphPublishesExplicitlyExportedClass)
    , ("compile module graph rejects private explicit class import", testCompileModuleGraphRejectsPrivateExplicitClassImport)
    , ("compile module graph allows local class matching private dependency class", testCompileModuleGraphAllowsLocalClassMatchingPrivateDependencyClass)
  ]

testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules :: IO ()
testCompileModuleGraphDefaultExposesBundledCapabilityFactsInModules = do
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
        x :: @{Eq(Int)}: Int.
        x = 1.
        }
        """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExplicitImportListHidesCapabilityFacts :: IO ()
testCompileModuleGraphExplicitImportListHidesCapabilityFacts = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "explicit import capability fact isolation"
        "missing class declaration 'Hidden'"
        (renderDiagnostic err)
    errors ->
      failTest
        ( "expected exactly one hidden capability fact error, got "
            <> Text.pack (show (map renderDiagnostic errors))
        )
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Facts (facts).
        use :: @{Hidden(Int)}: Int.
        use = 1.
        """),
          ("src/Lib/Facts.jz", """
          facts = 0.
          class Hidden(a) { }.
          impl Hidden(Int) { }.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct :: IO ()
testCompileModuleGraphKeepsAliasQualifiedAdtEqualityDistinct = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "alias-qualified ADT equality mismatch"
        "E2004"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one alias-qualified ADT equality mismatch"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Box as L.
        data Box a = Box a.
        left = L::Box 1.
        right = Box 1.
        same = left == right.
        """),
          ("src/Lib/Box.jz", "data Box a = Box a.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences :: IO ()
testCompileModuleGraphResolvesAliasQualifiedImplMethodReferences = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Math as Math.
        class Sample(a) {
        method :: Int.
        }.
        impl Sample(Int) {
        method = Math::one.
        }.
        x = 1.
        """),
          ("src/Lib/Math.jz", "one = 1.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRewritesHiddenImplMethodReferences :: IO ()
testCompileModuleGraphRewritesHiddenImplMethodReferences = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Thing as Thing.
        x = 0.
        """),
          ("src/Lib/Thing.jz", """
          helper = 1.
          class Sample(a) {
          method :: Int.
          }.
          impl Sample(Int) {
          method = helper.
          }.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsModuleAdtImplFactsDistinct :: IO ()
testCompileModuleGraphKeepsModuleAdtImplFactsDistinct = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "module ADT impl fact isolation"
        "missing impl fact 'Eq(Box(Int))'"
        (renderDiagnostic err)
    errors ->
      failTest
        ( "expected exactly one missing local ADT impl fact, got "
            <> Text.pack (show (map renderDiagnostic errors))
        )
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Box (Box).
        data Box a = Box a.
        class Eq(a) { }.
        use :: @{Eq(Box(Int))}: Int.
        use = 1.
        """),
          ("src/Lib/Box.jz", """
          data Box a = Box a.
          class Eq(a) { }.
          impl Eq(Box(Int)) { }.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphPreservesConstrainedSchemesThroughExportBridges :: IO ()
testCompileModuleGraphPreservesConstrainedSchemesThroughExportBridges = do
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
        import Lib::Poly.
        intValue :: Int.
        intValue = id 1.
        boolValue :: Bool.
        boolValue = id True.
        boolValue.
        }
        """),
          ("src/Lib/Poly.jz", """
          module Lib::Poly {
          class Eq(a) { }.
          impl Eq(Int) { }.
          impl Eq(Bool) { }.
          id :: @{Eq(a)}: a -> a.
          id = \\(x) -> x.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByInferredEqualityExport :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByInferredEqualityExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Poly (same).
        result = same 1.
        result.
        }
        """),
          ("src/Lib/Poly.jz", """
          module Lib::Poly {
          class Eq(a) { }.
          impl Eq(Int) { }.
          same = \\(x) -> x == x.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsStructuralEqualityThroughHiddenInferredEqualityExport :: IO ()
testRunModuleGraphAllowsStructuralEqualityThroughHiddenInferredEqualityExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Poly (same).
        result = same [1].
        result.
        }
        """),
          ("src/Lib/Poly.jz", """
          module Lib::Poly {
          class Eq(a) { }.
          same = \\(xs) -> xs == xs.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability :: IO ()
testRunModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability =
  mapM_
    assertHiddenEqualityScope
    [ ( "direct equality",
        "result = same True.",
        "same = \\(x) -> x == x."
      ),
      ( "operator equality",
        "result = same True False.",
        "same = (==)."
      ),
      ( "section equality",
        "result = same True False.",
        "same = \\(right) -> (== right)."
      )
    ]
  where
    assertHiddenEqualityScope (label, appUse, sameDefinition) = do
      result <-
        runModuleGraphWithPrelude
          defaultWarningSettings
          Nothing
          resolverConfig
          ["App", "Main"]
          (lookupSource sameDefinition appUse)
      case runCompileErrors result of
        [err] -> do
          assertContains
            (label <> " hidden Eq impl error")
            "missing impl fact"
            (renderDiagnostic err)
          assertContains
            (label <> " hidden Eq fact name")
            "Lib::Poly::Eq(Bool)"
            (renderDiagnostic err)
        _ -> failTest (label <> ": expected exactly one hidden Eq compile error")

    lookupSource sameDefinition appUse path =
      pure (Map.lookup path (sourceMap sameDefinition appUse))

    sourceMap sameDefinition appUse =
      -- Explicit fragments are intentional: these programs embed test-specific declarations.
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main {\nimport Lib::Poly (same).\nclass Eq(a) { }.\nimpl Eq(Bool) { }.\n" <> appUse <> "\n}"
          ),
          ( "src/Lib/Poly.jz",
            "module Lib::Poly {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\n" <> sameDefinition <> "\n}"
          )
        ]

testRunModuleGraphKeepsHelperOnlyInferredEqualityHiddenDespiteDirectSiblingImport :: IO ()
testRunModuleGraphKeepsHelperOnlyInferredEqualityHiddenDespiteDirectSiblingImport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case runCompileErrors result of
    [err] -> do
      assertContains
        "helper-only import hidden Eq impl error"
        "missing impl fact"
        (renderDiagnostic err)
      assertContains
        "helper-only import hidden Eq fact name"
        "Lib::Poly::Eq(Bool)"
        (renderDiagnostic err)
    errors ->
      failTest
        ( "expected exactly one hidden Eq compile error, got "
            <> Text.pack (show (map renderDiagnostic errors))
        )
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import App::Direct.
            import App::HelperOnly.
            result = helperResult.
            result.
            }
            """
          ),
          ( "src/App/Direct.jz",
            """
            module App::Direct {
            import Lib::Poly (Eq).
            direct = 0.
            }
            """
          ),
          ( "src/App/HelperOnly.jz",
            """
            module App::HelperOnly {
            import Lib::Poly (same).
            class Eq(a) { }.
            impl Eq(Bool) { }.
            helperResult = same True.
            }
            """
          ),
          ( "src/Lib/Poly.jz",
            """
            module Lib::Poly {
            class Eq(a) { }.
            impl Eq(Int) { }.
            same = \\(x) -> x == x.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability :: IO ()
testCompileModuleGraphKeepsInferredEqualityExportFactsScopedToHiddenCapability = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] -> do
      assertContains
        "compile hidden Eq impl error"
        "missing impl fact"
        (renderDiagnostic err)
      assertContains
        "compile hidden Eq fact name"
        "Lib::Poly::Eq(Bool)"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one compile-time hidden Eq error"
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Poly (same).
            class Eq(a) { }.
            impl Eq(Bool) { }.
            result = same True.
            }
            """
          ),
          ( "src/Lib/Poly.jz",
            """
            module Lib::Poly {
            class Eq(a) { }.
            impl Eq(Int) { }.
            same = \\(x) -> x == x.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRetainsImportedCapabilityFactsReferencedByInferredExport :: IO ()
testCompileModuleGraphRetainsImportedCapabilityFactsReferencedByInferredExport = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Wrapper (same).
        result = same 1.
        result.
        }
        """),
          ("src/Lib/Facts.jz", """
          module Lib::Facts {
          class Eq(a) { }.
          impl Eq(Int) { }.
          }
          """),
          ("src/Lib/Wrapper.jz", """
          module Lib::Wrapper {
          import Lib::Facts.
          same = \\(x) -> x == x.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsImportedClassImplVisibleWhenHelperIsSelected :: IO ()
testRunModuleGraphKeepsImportedClassImplVisibleWhenHelperIsSelected = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Wrapper (same).
            result = same 1.
            result.
            }
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts {
            class Eq(a) { }.
            }
            """
          ),
          ( "src/Lib/Wrapper.jz",
            """
            module Lib::Wrapper {
            import Lib::Facts (Eq).
            impl Eq(Int) { }.
            same = \\(x) -> x == x.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated :: IO ()
testCompileModuleGraphKeepsSiblingCapabilityFactsIsolated = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] ->
      assertContains
        "sibling capability fact isolation error"
        "missing class declaration 'Eq'"
        (renderDiagnostic err)
    _ -> failTest "expected exactly one sibling capability fact isolation error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        import Lib::Facts.
        import Lib::UsesEq.
        uses.
        """),
          ("src/Lib/Facts.jz", """
          class Eq(a) { }.
          impl Eq(Int) { }.
          facts = 0.
          """),
          ("src/Lib/UsesEq.jz", """
          uses :: @{Eq(Int)}: Int.
          uses = 1.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports :: IO ()
testCompileModuleGraphExposesCapabilityFactsThroughVisibleImports = do
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
        import Lib::Facts.
        use :: @{Eq(Int)}: Int.
        use = 1.
        """),
          ("src/Lib/Facts.jz", """
          class Eq(a) { }.
          impl Eq(Int) { }.
          facts = 0.
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsBundledClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsBundledClassQualifiedMethodLookup = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        Eq::equals 1 1.
        }
        """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsCharAndTextPrimitiveImplTargetsUnqualified :: IO ()
testRunModuleGraphKeepsCharAndTextPrimitiveImplTargetsUnqualified = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            class RuntimeEq(a) {
            equals :: a -> a -> Bool.
            }.
            impl RuntimeEq(Char) {
            equals = \\(left, right) -> True.
            }.
            impl RuntimeEq(Text) {
            equals = \\(left, right) -> False.
            }.
            (RuntimeEq::equals 'a' 'b', RuntimeEq::equals \"a\" \"b\").
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsImportedClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsImportedClassQualifiedMethodLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Facts.
        Eq::equals 1 1.
        }
        """),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            impl Eq(Int) {
            equals = \\(left, right) -> left == right.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsAliasOnlyImportedClassQualifiedMethodLookup :: IO ()
testCompileModuleGraphRejectsAliasOnlyImportedClassQualifiedMethodLookup = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] -> do
      let rendered = renderDiagnostic err
      assertContains "alias-only class-qualified import code" "E4013" rendered
      assertContains "hidden capability class name" "Eq" rendered
      assertContains "method name" "equals" rendered
    _ -> failTest "expected exactly one alias-only class-qualified import error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Facts as Facts.
        Eq::equals 1 1.
        }
        """),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            impl Eq(Int) {
            equals = \\(left, right) -> left == right.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup :: IO ()
testRunModuleGraphAllowsImportedPreModuleClassQualifiedMethodLookup = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Facts.
        Eq::equals 1 1.
        }
        """),
          ( "src/Lib/Facts.jz",
            """
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            impl Eq(Int) {
            equals = \\(left, right) -> left == right.
            }.
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch :: IO ()
testRunModuleGraphKeepsHiddenImplsOutOfRuntimeDispatch = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (Choice).
            import Lib::Hidden (val).
            Choice::pick 1.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> True.
            }.
            }
            """
          ),
          ( "src/Lib/Hidden.jz",
            """
            module Lib::Hidden {
            import Lib::Api (Choice).
            val = 0.
            impl Choice(UInt8) {
            pick = \\(value) -> False.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByExportedBindings = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (foo).
            foo.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> True.
            }.
            foo = Choice::pick 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedCapabilityBodies = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (Choice).
            Choice::pick 1.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            class Flag(a) {
            enabled :: Bool.
            }.
            impl Flag(Int) {
            enabled = True.
            }.
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> Flag::enabled.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphNamespacesCapabilitiesNeededByDirectlyImportedCapabilityBodies :: IO ()
testRunModuleGraphNamespacesCapabilitiesNeededByDirectlyImportedCapabilityBodies = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (Choice).
            class Flag(a) {
            enabled :: Bool.
            }.
            impl Flag(Int) {
            enabled = False.
            }.
            (Choice::pick 1, Flag::enabled).
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            class Flag(a) {
            enabled :: Bool.
            }.
            impl Flag(Int) {
            enabled = True.
            }.
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> Flag::enabled.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsValueDependenciesNeededByImportedCapabilityBodies :: IO ()
testRunModuleGraphRetainsValueDependenciesNeededByImportedCapabilityBodies = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (Choice).
            Choice::pick 1.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            helper = True.
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> helper.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsInferredRuntimeHintsModuleScoped :: IO ()
testRunModuleGraphKeepsInferredRuntimeHintsModuleScoped = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A as A.
            import Lib::B as B.
            result = (A::picked, B::picked).
            result.
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            module Lib::A {
            data Box a = Box a.
            class RuntimePick(a) {
            pick :: a -> Bool.
            }.
            impl RuntimePick(Box(Int)) {
            pick = \\(box) -> True.
            }.
            impl RuntimePick(Box(UInt8)) {
            pick = \\(box) -> False.
            }.
            box = Box 1.
            picked = RuntimePick::pick box.
            }
            """
          ),
          ( "src/Lib/B.jz",
            """
            module Lib::B {
            data Box a = Box a.
            class RuntimePick(a) {
            pick :: a -> Bool.
            }.
            impl RuntimePick(Box(Int)) {
            pick = \\(box) -> True.
            }.
            impl RuntimePick(Box(UInt8)) {
            pick = \\(box) -> False.
            }.
            box = Box (__kernel_toUInt8 1).
            picked = RuntimePick::pick box.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsNestedInferredRuntimeHintsModuleScoped :: IO ()
testRunModuleGraphKeepsNestedInferredRuntimeHintsModuleScoped = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "False") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Pick (picked).
            picked.
            }
            """
          ),
          ( "src/Lib/Pick.jz",
            """
            module Lib::Pick {
            class RuntimePick(a) {
            pick :: a -> Bool.
            }.
            impl RuntimePick(Int) {
            pick = \\(value) -> True.
            }.
            impl RuntimePick(UInt8) {
            pick = \\(value) -> False.
            }.
            picked = {
            x = if True then 1 else __kernel_toUInt8 2.
            RuntimePick::pick x.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsPreModuleInferredRuntimeHintsModuleScoped :: IO ()
testRunModuleGraphKeepsPreModuleInferredRuntimeHintsModuleScoped = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(False, True)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A as A.
            import Lib::B as B.
            (A::picked, B::picked).
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            class RuntimePick(a) {
            pick :: a -> Bool.
            }.
            impl RuntimePick(Int) {
            pick = \\(value) -> True.
            }.
            impl RuntimePick(UInt8) {
            pick = \\(value) -> False.
            }.
            picked = {
            x = if True then 1 else __kernel_toUInt8 2.
            RuntimePick::pick x.
            }.
            """
          ),
          ( "src/Lib/B.jz",
            """
            class RuntimePick(a) {
            pick :: a -> Bool.
            }.
            impl RuntimePick(Int) {
            pick = \\(value) -> True.
            }.
            impl RuntimePick(UInt8) {
            pick = \\(value) -> False.
            }.
            picked = {
            x = 1.
            RuntimePick::pick x.
            }.
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRebasesExplicitGenericAdtApplicationHints :: IO ()
testRunModuleGraphRebasesExplicitGenericAdtApplicationHints = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            data Box a = Box a.
            class RuntimeFlag(a) {
            flag :: a -> Bool.
            }.
            impl RuntimeFlag(Box([Int])) {
            flag = \\(box) -> True.
            }.
            impl RuntimeFlag(Box([Bool])) {
            flag = \\(box) -> False.
            }.
            identity = \\(value) -> value.
            result = RuntimeFlag::flag (identity @Box([Int]) (Box [])).
            result.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRebasesFallbackExplicitGenericAdtHints :: IO ()
testRunModuleGraphRebasesFallbackExplicitGenericAdtHints = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "fallback explicit hint compile errors" [] (runCompileErrors result)
  assertEqual "fallback explicit hint runtime errors" [] (runRuntimeErrors result)
  assertEqual "fallback explicit hint output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            data Box a = Box a.
            class Flag(a) {
            flag :: a -> Bool.
            }.
            impl Flag(Box([Int])) {
            flag = \\(box) -> True.
            }.
            impl Flag(Box([Bool])) {
            flag = \\(box) -> False.
            }.
            use :: a -> b -> a.
            use = \\(value, ignored) -> value.
            result = Flag::flag (use @Box([Int]) (Box []) True).
            result.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRebasesClassMethodArgumentSignatures :: IO ()
testRunModuleGraphRebasesClassMethodArgumentSignatures = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "class method argument compile errors" [] (runCompileErrors result)
  assertEqual "class method argument runtime errors" [] (runRuntimeErrors result)
  assertEqual "class method argument output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            data Token a = Token a.
            class Check(a) {
            check :: Token(Int) -> a -> Bool.
            }.
            impl Check(Int) {
            check = \\(token, value) -> True.
            }.
            result = Check::check (Token 1) 1.
            result.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures :: IO ()
testRunModuleGraphRetainsLocalCapabilitiesNeededByImportedSignatures = do
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
            import Lib::Api (foo).
            foo.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            class Need(a) {
            }.
            impl Need(Int) {
            }.
            foo :: @{Need(Int)}: Int.
            foo = 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphNamespacesHiddenRetainedLocalCapabilities :: IO ()
testRunModuleGraphNamespacesHiddenRetainedLocalCapabilities = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A (pickedA).
            import Lib::B (pickedB).
            (pickedA, pickedB).
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            module Lib::A {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> True.
            }.
            pickedA = Choice::pick 1.
            }
            """
          ),
          ( "src/Lib/B.jz",
            """
            module Lib::B {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> False.
            }.
            pickedB = Choice::pick 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphNamespacesAliasRetainedLocalCapabilities :: IO ()
testRunModuleGraphNamespacesAliasRetainedLocalCapabilities = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A as A.
            import Lib::B as B.
            (A::pickedA, B::pickedB).
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            module Lib::A {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> True.
            }.
            pickedA = Choice::pick 1.
            }
            """
          ),
          ( "src/Lib/B.jz",
            """
            module Lib::B {
            class Choice(a) {
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            pick = \\(value) -> False.
            }.
            pickedB = Choice::pick 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRewritesHiddenCapabilityReferencesDespiteValueShadowing :: IO ()
testRunModuleGraphRewritesHiddenCapabilityReferencesDespiteValueShadowing = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::A as A.
            A::picked.
            }
            """
          ),
          ( "src/Lib/A.jz",
            """
            module Lib::A {
            data Marker = Choice.
            class Choice(a) {
            flag :: a -> Bool.
            pick :: a -> Bool.
            }.
            impl Choice(Int) {
            flag = \\(value) -> True.
            pick = \\(value) -> Choice::flag value.
            }.
            picked = Choice::pick 1.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphExposesDataReferencedByImportedClassMethods :: IO ()
testRunModuleGraphExposesDataReferencedByImportedClassMethods = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "Box") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api (Make).
            Make::make.
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            data Box = Box.
            class Make(a) {
            make :: Box.
            }.
            impl Make(Int) {
            make = Box.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphPreservesImportedGenericConstructorPayloadDispatch :: IO ()
testRunModuleGraphPreservesImportedGenericConstructorPayloadDispatch = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Box.
        (Pick::pick intBox, Pick::pick byteBox).
        }
        """),
          ( "src/Lib/Box.jz",
            """
            module Lib::Box {
            data Box a = Box a.
            class Pick(a) {
            pick :: a -> Bool.
            }.
            impl Pick(Box(Int)) {
            pick = \\(box) -> True.
            }.
            impl Pick(Box(UInt8)) {
            pick = \\(box) -> False.
            }.
            intBox = Box 1.
            byteBox = Box (__kernel_toUInt8 1).
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsImportedAdtNamesInTypePositions :: IO ()
testRunModuleGraphKeepsImportedAdtNamesInTypePositions = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Box.
            class Pick(a) {
            pick :: a -> Bool.
            }.
            impl Pick(Box(UInt8)) {
            pick = \\(box) -> True.
            }.
            box = Box (__kernel_toUInt8 1).
            Pick::pick box.
            }
            """
          ),
          ("src/Lib/Box.jz", """
          module Lib::Box {
          data Box a = Box a.
          }
          """)
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRebasesDependencyClassMethodResultHints :: IO ()
testRunModuleGraphRebasesDependencyClassMethodResultHints = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  case runRuntimeErrors result of
    [diagnostic] -> do
      assertContains "rebased result hint overflow code" "E3025" (renderDiagnostic diagnostic)
      assertContains "rebased result hint overflow target" "outside UInt8 range" (renderDiagnostic diagnostic)
    diagnostics ->
      failTest
        ( "expected one UInt8 overflow after applying the method result hint, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  assertEqual "runtime output" Nothing (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Factory.
            (\\(Box value) -> value + 255) (Make::make 0).
            }
            """
          ),
          ( "src/Lib/Factory.jz",
            """
            module Lib::Factory {
            data Box = Box UInt8.
            class Make(a) {
            make :: a -> Box.
            }.
            impl Make(Int) {
            make = \\(value) -> Box 1.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphRebasesImportedClassMethodResultHintsFromClassOrigin :: IO ()
testRunModuleGraphRebasesImportedClassMethodResultHintsFromClassOrigin = do
  result <-
    runModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  case runRuntimeErrors result of
    [diagnostic] -> do
      assertContains "imported class result hint overflow code" "E3025" (renderDiagnostic diagnostic)
      assertContains "imported class result hint overflow target" "outside UInt8 range" (renderDiagnostic diagnostic)
    diagnostics ->
      failTest
        ( "expected one UInt8 overflow after applying the imported class method result hint, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  assertEqual "runtime output" Nothing (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Api.
            impl Make(Int) {
            make = \\(value) -> Box 1.
            }.
            (\\(Box value) -> value + 255) (Make::make 0).
            }
            """
          ),
          ( "src/Lib/Api.jz",
            """
            module Lib::Api {
            data Box = Box UInt8.
            class Make(a) {
            make :: a -> Box.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsAmbientClassCollision :: IO ()
testCompileModuleGraphRejectsAmbientClassCollision = do
  result <-
    compileModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] ->
      let rendered = renderDiagnostic diagnostic
       in do
            assertContains "ambient class collision code" "E1004" rendered
            assertContains "ambient class collision summary" "duplicate class declaration 'Eq'" rendered
    diagnostics ->
      failTest
        ( "expected one ambient class collision, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsImportedClassCollision :: IO ()
testCompileModuleGraphRejectsImportedClassCollision = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] ->
      let rendered = renderDiagnostic diagnostic
       in do
            assertContains "imported class collision code" "E1004" rendered
            assertContains "imported class collision summary" "duplicate class declaration 'Eq'" rendered
    diagnostics ->
      failTest
        ( "expected one imported class collision, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Facts (Eq).
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            }
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphDoesNotReexportImportedClasses :: IO ()
testCompileModuleGraphDoesNotReexportImportedClasses = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "non-transitive class code" "E4007" (renderDiagnostic diagnostic)
      assertContains
        "non-transitive class export"
        "import symbol 'Eq' is not exported by module 'Lib::Wrapper'"
        (renderDiagnostic diagnostic)
    diagnostics ->
      failTest
        ( "expected one non-transitive class export diagnostic, got "
            <> Text.pack (show (map renderDiagnostic diagnostics))
        )
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Wrapper (Eq).
            x = 1.
            }
            """
          ),
          ( "src/Lib/Wrapper.jz",
            """
            module Lib::Wrapper {
            import Lib::Facts (Eq).
            wrapper = 0.
            }
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts {
            class Eq(a) { }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphPublishesExplicitlyExportedClass :: IO ()
testRunModuleGraphPublishesExplicitlyExportedClass = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "public class compile errors" [] (runCompileErrors result)
  assertEqual "public class runtime errors" [] (runRuntimeErrors result)
  assertEqual "public class runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Facts (Eq).
        Eq::equals 1 1.
        }
        """),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts (Eq) {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            class Hidden(a) { }.
            impl Eq(Int) {
            equals = \\(left, right) -> left == right.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsPrivateExplicitClassImport :: IO ()
testCompileModuleGraphRejectsPrivateExplicitClassImport = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "private class import code" "E4007" (renderDiagnostic diagnostic)
      assertContains "private class import name" "Hidden" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one E4007 diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", """
        module App::Main {
        import Lib::Facts (Hidden).
        0.
        }
        """),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts (Eq) {
            class Eq(a) {
            equals :: a -> a -> Bool.
            }.
            class Hidden(a) { }.
            impl Eq(Int) {
            equals = \\(left, right) -> left == right.
            }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphAllowsLocalClassMatchingPrivateDependencyClass :: IO ()
testCompileModuleGraphAllowsLocalClassMatchingPrivateDependencyClass = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private dependency class collision errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
            import Lib::Facts.
            class Hidden(a) { }.
            0.
            }
            """
          ),
          ( "src/Lib/Facts.jz",
            """
            module Lib::Facts (Eq) {
            class Eq(a) { }.
            class Hidden(a) { }.
            }
            """
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
