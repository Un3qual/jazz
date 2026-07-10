{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Modules.Loader.CapabilitiesTests
  ( capabilitiesTests
  ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    ResolvedPrelude (..),
    RunResult (..),
    compileModuleGraph,
    compileModuleGraphWithResolvedPrelude,
    compileModuleGraphWithPrelude,
    runModuleGraph,
    runModuleGraphWithResolvedPrelude,
    runModuleGraphWithPrelude
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..)
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite
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
        [ ("src/App/Main.jz", "module App::Main {\nx :: @{Eq(Int)}: Int.\nx = 1.\n}")
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
        [ ("src/App/Main.jz", "import Lib::Facts (facts).\nuse :: @{Hidden(Int)}: Int.\nuse = 1."),
          ("src/Lib/Facts.jz", "facts = 0.\nclass Hidden(a) { }.\nimpl Hidden(Int) { }.")
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
        [ ("src/App/Main.jz", "import Lib::Box as L.\ndata Box a = Box a.\nleft = L::Box 1.\nright = Box 1.\nsame = left == right."),
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
        [ ("src/App/Main.jz", "import Lib::Math as Math.\nclass Sample(a) {\nmethod :: Int.\n}.\nimpl Sample(Int) {\nmethod = Math::one.\n}.\nx = 1."),
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
        [ ("src/App/Main.jz", "import Lib::Thing as Thing.\nx = 0."),
          ("src/Lib/Thing.jz", "helper = 1.\nclass Sample(a) {\nmethod :: Int.\n}.\nimpl Sample(Int) {\nmethod = helper.\n}.")
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
        [ ("src/App/Main.jz", "import Lib::Box (Box).\ndata Box a = Box a.\nclass Eq(a) { }.\nuse :: @{Eq(Box(Int))}: Int.\nuse = 1."),
          ("src/Lib/Box.jz", "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.")
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Poly.\nintValue :: Int.\nintValue = id 1.\nboolValue :: Bool.\nboolValue = id True.\nboolValue.\n}"),
          ("src/Lib/Poly.jz", "module Lib::Poly {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\n}")
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Poly (same).\nresult = same 1.\nresult.\n}"),
          ("src/Lib/Poly.jz", "module Lib::Poly {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\nsame = \\(x) -> x == x.\n}")
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Poly (same).\nresult = same [1].\nresult.\n}"),
          ("src/Lib/Poly.jz", "module Lib::Poly {\nclass Eq(a) { }.\nsame = \\(xs) -> xs == xs.\n}")
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
            "module App::Main {\nimport App::Direct.\nimport App::HelperOnly.\nresult = helperResult.\nresult.\n}"
          ),
          ( "src/App/Direct.jz",
            "module App::Direct {\nimport Lib::Poly (Eq).\ndirect = 0.\n}"
          ),
          ( "src/App/HelperOnly.jz",
            "module App::HelperOnly {\nimport Lib::Poly (same).\nclass Eq(a) { }.\nimpl Eq(Bool) { }.\nhelperResult = same True.\n}"
          ),
          ( "src/Lib/Poly.jz",
            "module Lib::Poly {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\nsame = \\(x) -> x == x.\n}"
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
            "module App::Main {\nimport Lib::Poly (same).\nclass Eq(a) { }.\nimpl Eq(Bool) { }.\nresult = same True.\n}"
          ),
          ( "src/Lib/Poly.jz",
            "module Lib::Poly {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\nsame = \\(x) -> x == x.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Wrapper (same).\nresult = same 1.\nresult.\n}"),
          ("src/Lib/Facts.jz", "module Lib::Facts {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\n}"),
          ("src/Lib/Wrapper.jz", "module Lib::Wrapper {\nimport Lib::Facts.\nsame = \\(x) -> x == x.\n}")
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
            "module App::Main {\nimport Lib::Wrapper (same).\nresult = same 1.\nresult.\n}"
          ),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) { }.\n}"
          ),
          ( "src/Lib/Wrapper.jz",
            "module Lib::Wrapper {\nimport Lib::Facts (Eq).\nimpl Eq(Int) { }.\nsame = \\(x) -> x == x.\n}"
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
        [ ("src/App/Main.jz", "import Lib::Facts.\nimport Lib::UsesEq.\nuses."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.\nfacts = 0."),
          ("src/Lib/UsesEq.jz", "uses :: @{Eq(Int)}: Int.\nuses = 1.")
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
        [ ("src/App/Main.jz", "import Lib::Facts.\nuse :: @{Eq(Int)}: Int.\nuse = 1."),
          ("src/Lib/Facts.jz", "class Eq(a) { }.\nimpl Eq(Int) { }.\nfacts = 0.")
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
        [ ("src/App/Main.jz", "module App::Main {\nEq::equals 1 1.\n}")
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts as Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts.\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}."
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
            "module App::Main {\nimport Lib::Api (Choice).\nimport Lib::Hidden (val).\nChoice::pick 1.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\n}"
          ),
          ( "src/Lib/Hidden.jz",
            "module Lib::Hidden {\nimport Lib::Api (Choice).\nval = 0.\nimpl Choice(UInt8) {\npick = \\(value) -> False.\n}.\n}"
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
            "module App::Main {\nimport Lib::Api (foo).\nfoo.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\nfoo = Choice::pick 1.\n}"
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
            "module App::Main {\nimport Lib::Api (Choice).\nChoice::pick 1.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Flag(a) {\nenabled :: Bool.\n}.\nimpl Flag(Int) {\nenabled = True.\n}.\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> Flag::enabled.\n}.\n}"
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
            "module App::Main {\nimport Lib::Api (Choice).\nclass Flag(a) {\nenabled :: Bool.\n}.\nimpl Flag(Int) {\nenabled = False.\n}.\n(Choice::pick 1, Flag::enabled).\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Flag(a) {\nenabled :: Bool.\n}.\nimpl Flag(Int) {\nenabled = True.\n}.\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> Flag::enabled.\n}.\n}"
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
            "module App::Main {\nimport Lib::Api (Choice).\nChoice::pick 1.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nhelper = True.\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> helper.\n}.\n}"
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
            "module App::Main {\nimport Lib::A as A.\nimport Lib::B as B.\nresult = (A::picked, B::picked).\nresult.\n}"
          ),
          ( "src/Lib/A.jz",
            "module Lib::A {\ndata Box a = Box a.\nclass RuntimePick(a) {\npick :: a -> Bool.\n}.\nimpl RuntimePick(Box(Int)) {\npick = \\(box) -> True.\n}.\nimpl RuntimePick(Box(UInt8)) {\npick = \\(box) -> False.\n}.\nbox = Box 1.\npicked = RuntimePick::pick box.\n}"
          ),
          ( "src/Lib/B.jz",
            "module Lib::B {\ndata Box a = Box a.\nclass RuntimePick(a) {\npick :: a -> Bool.\n}.\nimpl RuntimePick(Box(Int)) {\npick = \\(box) -> True.\n}.\nimpl RuntimePick(Box(UInt8)) {\npick = \\(box) -> False.\n}.\nbox = Box (__kernel_toUInt8 1).\npicked = RuntimePick::pick box.\n}"
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
            "module App::Main {\nimport Lib::Pick (picked).\npicked.\n}"
          ),
          ( "src/Lib/Pick.jz",
            "module Lib::Pick {\nclass RuntimePick(a) {\npick :: a -> Bool.\n}.\nimpl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\nimpl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\npicked = {\nx = if True 1 else __kernel_toUInt8 2.\nRuntimePick::pick x.\n}.\n}"
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
            "module App::Main {\nimport Lib::A as A.\nimport Lib::B as B.\n(A::picked, B::picked).\n}"
          ),
          ( "src/Lib/A.jz",
            "class RuntimePick(a) {\npick :: a -> Bool.\n}.\nimpl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\nimpl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\npicked = {\nx = if True 1 else __kernel_toUInt8 2.\nRuntimePick::pick x.\n}."
          ),
          ( "src/Lib/B.jz",
            "class RuntimePick(a) {\npick :: a -> Bool.\n}.\nimpl RuntimePick(Int) {\npick = \\(value) -> True.\n}.\nimpl RuntimePick(UInt8) {\npick = \\(value) -> False.\n}.\npicked = {\nx = 1.\nRuntimePick::pick x.\n}."
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
            "module App::Main {\nimport Lib::Api (foo).\nfoo.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\nclass Need(a) {\n}.\nimpl Need(Int) {\n}.\nfoo :: @{Need(Int)}: Int.\nfoo = 1.\n}"
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
            "module App::Main {\nimport Lib::A (pickedA).\nimport Lib::B (pickedB).\n(pickedA, pickedB).\n}"
          ),
          ( "src/Lib/A.jz",
            "module Lib::A {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\npickedA = Choice::pick 1.\n}"
          ),
          ( "src/Lib/B.jz",
            "module Lib::B {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> False.\n}.\npickedB = Choice::pick 1.\n}"
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
            "module App::Main {\nimport Lib::A as A.\nimport Lib::B as B.\n(A::pickedA, B::pickedB).\n}"
          ),
          ( "src/Lib/A.jz",
            "module Lib::A {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> True.\n}.\npickedA = Choice::pick 1.\n}"
          ),
          ( "src/Lib/B.jz",
            "module Lib::B {\nclass Choice(a) {\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\npick = \\(value) -> False.\n}.\npickedB = Choice::pick 1.\n}"
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
            "module App::Main {\nimport Lib::A as A.\nA::picked.\n}"
          ),
          ( "src/Lib/A.jz",
            "module Lib::A {\ndata Marker = Choice.\nclass Choice(a) {\nflag :: a -> Bool.\npick :: a -> Bool.\n}.\nimpl Choice(Int) {\nflag = \\(value) -> True.\npick = \\(value) -> Choice::flag value.\n}.\npicked = Choice::pick 1.\n}"
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
            "module App::Main {\nimport Lib::Api (Make).\nMake::make.\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\ndata Box = Box.\nclass Make(a) {\nmake :: Box.\n}.\nimpl Make(Int) {\nmake = Box.\n}.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Box.\n(Pick::pick intBox, Pick::pick byteBox).\n}"),
          ( "src/Lib/Box.jz",
            "module Lib::Box {\ndata Box a = Box a.\nclass Pick(a) {\npick :: a -> Bool.\n}.\nimpl Pick(Box(Int)) {\npick = \\(box) -> True.\n}.\nimpl Pick(Box(UInt8)) {\npick = \\(box) -> False.\n}.\nintBox = Box 1.\nbyteBox = Box (__kernel_toUInt8 1).\n}"
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
            "module App::Main {\nimport Lib::Box.\nclass Pick(a) {\npick :: a -> Bool.\n}.\nimpl Pick(Box(UInt8)) {\npick = \\(box) -> True.\n}.\nbox = Box (__kernel_toUInt8 1).\nPick::pick box.\n}"
          ),
          ("src/Lib/Box.jz", "module Lib::Box {\ndata Box a = Box a.\n}")
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
            "module App::Main {\nimport Lib::Factory.\n(\\(Box value) -> value + 255) (Make::make 0).\n}"
          ),
          ( "src/Lib/Factory.jz",
            "module Lib::Factory {\ndata Box = Box UInt8.\nclass Make(a) {\nmake :: a -> Box.\n}.\nimpl Make(Int) {\nmake = \\(value) -> Box 1.\n}.\n}"
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
            "module App::Main {\nimport Lib::Api.\nimpl Make(Int) {\nmake = \\(value) -> Box 1.\n}.\n(\\(Box value) -> value + 255) (Make::make 0).\n}"
          ),
          ( "src/Lib/Api.jz",
            "module Lib::Api {\ndata Box = Box UInt8.\nclass Make(a) {\nmake :: a -> Box.\n}.\n}"
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
            "module App::Main {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\n}"
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
            "module App::Main {\nimport Lib::Facts (Eq).\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\n}"
          ),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\n}"
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
            "module App::Main {\nimport Lib::Wrapper (Eq).\nx = 1.\n}"
          ),
          ( "src/Lib/Wrapper.jz",
            "module Lib::Wrapper {\nimport Lib::Facts (Eq).\nwrapper = 0.\n}"
          ),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts {\nclass Eq(a) { }.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts (Eq).\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts (Eq) {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nclass Hidden(a) { }.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
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
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts (Hidden).\n0.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts (Eq) {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nclass Hidden(a) { }.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
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
            "module App::Main {\nimport Lib::Facts.\nclass Hidden(a) { }.\n0.\n}"
          ),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts (Eq) {\nclass Eq(a) { }.\nclass Hidden(a) { }.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
