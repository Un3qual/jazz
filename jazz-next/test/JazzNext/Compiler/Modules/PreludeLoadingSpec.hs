{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    RunResult (..),
    compileSourceWithPrelude,
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleErrorContains,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PreludeLoading" tests

tests :: [NamedTest]
tests =
  [ ("compile source can reference prelude-defined bindings", testCompileWithPreludeBindingVisibility),
    ("run source can apply prelude-defined section functions", testRunWithPreludeSectionFunction),
    ("bundled default prelude preserves user diagnostic spans", testBundledPreludePreservesUserDiagnosticSpans),
    ("invalid prelude source produces prelude parse diagnostic", testPreludeParseDiagnostic),
    ("prelude bridge with unknown kernel symbol fails conformance checks", testPreludeUnknownBridgeSymbolDiagnostic),
    ("prelude bridge with missing kernel suffix fails conformance checks", testPreludeBridgeMissingSuffixDiagnostic),
    ("prelude bridge must be direct symbol reference", testPreludeMalformedBridgeDiagnostic),
    ("prelude bridge rejects canonical alias in bridge declaration", testPreludeBridgeRejectsCanonicalAlias),
    ("prelude bridge rebinding reports current and previous bridge spans", testPreludeBridgeRebindingDiagnostic),
    ("prelude bridge allows canonical alias after kernel self-bridge", testPreludeBridgeAllowsCanonicalAliasAfterBridge),
    ("bundled default prelude exposes capability classes and default impl facts", testBundledPreludeExposesCapabilityClassesAndDefaultImplFacts),
    ("bundled default prelude exposes width-specific numeric impl facts", testBundledPreludeExposesWidthSpecificNumericImplFacts),
    ("prelude exposes numeric conversion aliases", testPreludeExposesNumericConversionAliases),
    ("bundled default prelude exposes Eq Int equals method body", testBundledPreludeExposesEqIntEqualsMethodBody),
    ("bundled default prelude exposes Eq Float equals method body", testBundledPreludeExposesEqFloatEqualsMethodBody),
    ("bundled default prelude exposes Eq Bool equals method body", testBundledPreludeExposesEqBoolEqualsMethodBody),
    ("compile without prelude rejects numeric conversion aliases", testCompileWithoutPreludeRejectsNumericConversionAliases),
    ("compile without prelude does not inherit bundled Eq equals method bodies", testCompileWithoutPreludeRejectsBundledEqEqualsMethodBodies),
    ("compile without prelude rejects bundled capability facts", testCompileWithoutPreludeRejectsBundledCapabilityFacts),
    ("explicit prelude does not inherit bundled impl facts", testExplicitPreludeDoesNotInheritBundledImplFacts),
    ("explicit prelude does not inherit bundled Eq equals method bodies", testExplicitPreludeDoesNotInheritBundledEqEqualsMethodBodies),
    ("compile without prelude keeps numeric conversion kernel bridges available", testCompileWithoutPreludeKeepsNumericConversionKernelBridgesAvailable),
    ("compile without prelude rejects public prelude aliases", testCompileWithoutPreludeRejectsPreludeAliases),
    ("compile without prelude keeps kernel bridge names available", testCompileWithoutPreludeKeepsKernelBridgeNamesAvailable),
    ("compile without prelude keeps missing binding behavior unchanged", testCompileWithoutPreludeStillFailsMissingBinding)
  ]

testCompileWithPreludeBindingVisibility :: IO ()
testCompileWithPreludeBindingVisibility = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "seed = 41.") "seed + 1."
  assertEqual "compile errors" [] (compileErrors result)

testRunWithPreludeSectionFunction :: IO ()
testRunWithPreludeSectionFunction = do
  result <- runSourceWithPrelude defaultWarningSettings (Just "inc = (+ 1).") "inc 2."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testBundledPreludePreservesUserDiagnosticSpans :: IO ()
testBundledPreludePreservesUserDiagnosticSpans = do
  result <- compileSource defaultWarningSettings "x :: Int. y = 1."
  case map renderDiagnostic (compileErrors result) of
    [rendered] -> do
      assertContains "bundled default prelude keeps signature code" "E1003" rendered
      assertContains "bundled default prelude keeps user spans anchored to user source" "1:1:" rendered
    renderedErrors ->
      assertEqual "single rendered diagnostic" 1 (length renderedErrors)

testPreludeParseDiagnostic :: IO ()
testPreludeParseDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "broken = .") "1."
  assertSingleErrorContains
    "prelude parse error code"
    "E0002"
    (compileErrors result)

testPreludeUnknownBridgeSymbolDiagnostic :: IO ()
testPreludeUnknownBridgeSymbolDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_unknown = unknown.") "1."
  let diagnostics = compileErrors result
  assertSingleDiagnosticCode
    "unknown kernel bridge symbol code"
    "E0004"
    diagnostics
  assertSingleDiagnosticPrimarySpan
    "unknown kernel bridge primary span"
    (SourceSpan 1 1)
    diagnostics
  assertSingleDiagnosticSubject
    "unknown kernel bridge subject"
    "__kernel_unknown"
    diagnostics

testPreludeBridgeMissingSuffixDiagnostic :: IO ()
testPreludeBridgeMissingSuffixDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_ = __kernel_map.") "1."
  assertSingleErrorContains
    "missing kernel bridge suffix code"
    "E0005"
    (compileErrors result)

testPreludeMalformedBridgeDiagnostic :: IO ()
testPreludeMalformedBridgeDiagnostic = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_map = inc. inc = (+ 1).") "1."
  assertSingleErrorContains
    "malformed kernel bridge code"
    "E0005"
    (compileErrors result)

testPreludeBridgeRejectsCanonicalAlias :: IO ()
testPreludeBridgeRejectsCanonicalAlias = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "map = (+ 1). __kernel_map = map.") "1."
  assertSingleErrorContains
    "bridge cannot reference canonical alias name"
    "E0005"
    (compileErrors result)

testPreludeBridgeRebindingDiagnostic :: IO ()
testPreludeBridgeRebindingDiagnostic = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      (Just "__kernel_map = __kernel_map.\n__kernel_map = __kernel_map.")
      "1."
  let diagnostics = compileErrors result
  assertSingleDiagnosticCode
    "bridge rebinding code"
    "E0005"
    diagnostics
  assertSingleDiagnosticPrimarySpan
    "bridge rebinding primary span"
    (SourceSpan 2 1)
    diagnostics
  assertSingleDiagnosticRelatedSpan
    "bridge rebinding related span"
    (SourceSpan 1 1)
    diagnostics
  assertSingleDiagnosticSubject
    "bridge rebinding subject"
    "__kernel_map"
    diagnostics

testPreludeBridgeAllowsCanonicalAliasAfterBridge :: IO ()
testPreludeBridgeAllowsCanonicalAliasAfterBridge = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "__kernel_map = __kernel_map. map = __kernel_map.") "1."
  assertEqual
    "bridge validation accepts canonical alias after kernel self-bridge"
    []
    (compileErrors result)

testBundledPreludeExposesCapabilityClassesAndDefaultImplFacts :: IO ()
testBundledPreludeExposesCapabilityClassesAndDefaultImplFacts = do
  result <-
    compileSource
      defaultWarningSettings
      ( Text.unlines
          [ "eqInt :: @{Eq(Int)}: Int.",
            "eqInt = 1.",
            "eqFloat :: @{Eq(Float)}: Float.",
            "eqFloat = toFloat64 1.",
            "eqBool :: @{Eq(Bool)}: Bool.",
            "eqBool = True.",
            "ordInt :: @{Ord(Int)}: Int.",
            "ordInt = 1.",
            "ordFloat :: @{Ord(Float)}: Float.",
            "ordFloat = toFloat64 1.",
            "numInt :: @{Num(Int)}: Int.",
            "numInt = 1.",
            "numFloat :: @{Num(Float)}: Float.",
            "numFloat = toFloat64 1.",
            "integralInt :: @{Integral(Int)}: Int.",
            "integralInt = 1.",
            "fractionalFloat :: @{Fractional(Float)}: Float.",
            "fractionalFloat = toFloat64 1.",
            "defaultInt :: @{Default(Int)}: Int.",
            "defaultInt = 1.",
            "defaultFloat :: @{Default(Float)}: Float.",
            "defaultFloat = toFloat64 1.",
            "defaultBool :: @{Default(Bool)}: Bool.",
            "defaultBool = False.",
            "showableInt :: @{Showable(Int)}: Int.",
            "showableInt = 1.",
            "showableFloat :: @{Showable(Float)}: Float.",
            "showableFloat = toFloat64 1.",
            "showableBool :: @{Showable(Bool)}: Bool.",
            "showableBool = True."
          ]
      )
  assertEqual "bundled prelude default capability facts" [] (compileErrors result)

testBundledPreludeExposesWidthSpecificNumericImplFacts :: IO ()
testBundledPreludeExposesWidthSpecificNumericImplFacts = do
  result <-
    compileSource
      defaultWarningSettings
      (Text.unlines (concatMap widthSpecificNumericImplFactCases widthSpecificNumericImplTargets))
  assertEqual "bundled prelude width-specific numeric capability facts" [] (compileErrors result)
  where
    widthSpecificNumericImplTargets =
      [ ("Int8", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("Int16", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("Int32", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("Int64", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("UInt8", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("UInt16", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("UInt32", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("UInt64", "1", ["Eq", "Ord", "Num", "Integral", "Default", "Showable"]),
        ("Float16", "toFloat16 1", ["Eq", "Ord", "Num", "Fractional", "Default", "Showable"]),
        ("Float32", "toFloat32 1", ["Eq", "Ord", "Num", "Fractional", "Default", "Showable"]),
        ("Float64", "toFloat64 1", ["Eq", "Ord", "Num", "Fractional", "Default", "Showable"])
      ]

    widthSpecificNumericImplFactCases (targetType, expression, classNames) =
      concatMap (widthSpecificNumericImplFactCase targetType expression) classNames

    widthSpecificNumericImplFactCase targetType expression className =
      let bindingName =
            Text.toLower (className <> targetType)
       in
        [ bindingName <> " :: @{" <> className <> "(" <> targetType <> ")}: " <> targetType <> ".",
          bindingName <> " = " <> expression <> "."
        ]

testPreludeExposesNumericConversionAliases :: IO ()
testPreludeExposesNumericConversionAliases = do
  result <- compileSource defaultWarningSettings "x :: UInt8.\nx = toUInt8 1."
  assertEqual "bundled prelude exposes toUInt8" [] (compileErrors result)

testBundledPreludeExposesEqIntEqualsMethodBody :: IO ()
testBundledPreludeExposesEqIntEqualsMethodBody = do
  result <- runSource defaultWarningSettings "Eq::equals 1 1."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testBundledPreludeExposesEqFloatEqualsMethodBody :: IO ()
testBundledPreludeExposesEqFloatEqualsMethodBody = do
  result <- runSource defaultWarningSettings "(Eq::equals 1.5 1.5, Eq::equals 1.5 2.25)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testBundledPreludeExposesEqBoolEqualsMethodBody :: IO ()
testBundledPreludeExposesEqBoolEqualsMethodBody = do
  result <- runSource defaultWarningSettings "(Eq::equals True True, Eq::equals True False)."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, False)") (runOutput result)

testCompileWithoutPreludeRejectsNumericConversionAliases :: IO ()
testCompileWithoutPreludeRejectsNumericConversionAliases = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x = toUInt8 1."
  assertEqual
    "public numeric conversion aliases are unavailable without prelude"
    ["E1001: unbound variable 'toUInt8'"]
    (map renderDiagnostic (compileErrors result))

testCompileWithoutPreludeRejectsBundledEqEqualsMethodBodies :: IO ()
testCompileWithoutPreludeRejectsBundledEqEqualsMethodBodies = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( Text.unlines
          [ "class Eq(a) {",
            "equals :: a -> a -> Bool.",
            "}.",
            "impl Eq(Int) { }.",
            "result = Eq::equals 1 1.",
            "result."
          ]
      )
  assertSingleErrorContains
    "no-prelude compile has no bundled Eq(Int).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors result)
  boolResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( Text.unlines
          [ "class Eq(a) {",
            "equals :: a -> a -> Bool.",
            "}.",
            "impl Eq(Bool) { }.",
            "result = Eq::equals True True.",
            "result."
          ]
      )
  assertSingleErrorContains
    "no-prelude compile has no bundled Eq(Bool).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors boolResult)
  floatResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( Text.unlines
          [ "class Eq(a) {",
            "equals :: a -> a -> Bool.",
            "}.",
            "impl Eq(Float) { }.",
            "left :: Float.",
            "left = 1.5.",
            "right :: Float.",
            "right = 1.5.",
            "result = Eq::equals left right.",
            "result."
          ]
      )
  assertSingleErrorContains
    "no-prelude compile has no bundled Eq(Float).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors floatResult)

testCompileWithoutPreludeRejectsBundledCapabilityFacts :: IO ()
testCompileWithoutPreludeRejectsBundledCapabilityFacts = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x :: @{Eq(Int)}: Int.\nx = 1."
  assertSingleErrorContains
    "no-prelude compile has no bundled capability facts"
    "missing class declaration 'Eq'"
    (compileErrors result)
  widthResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x :: @{Num(UInt16)}: UInt16.\nx = 1."
  assertSingleErrorContains
    "no-prelude compile has no bundled width-specific capability facts"
    "missing class declaration 'Num'"
    (compileErrors widthResult)

testExplicitPreludeDoesNotInheritBundledImplFacts :: IO ()
testExplicitPreludeDoesNotInheritBundledImplFacts = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just "class Eq(a) { }.") "x :: @{Eq(Int)}: Int.\nx = 1."
  assertSingleErrorContains
    "explicit prelude uses only supplied impl facts"
    "missing impl fact 'Eq(Int)'"
    (compileErrors result)
  widthResult <- compileSourceWithPrelude defaultWarningSettings (Just "class Num(a) { }.") "x :: @{Num(UInt16)}: UInt16.\nx = 1."
  assertSingleErrorContains
    "explicit prelude uses only supplied width-specific impl facts"
    "missing impl fact 'Num(UInt16)'"
    (compileErrors widthResult)

testExplicitPreludeDoesNotInheritBundledEqEqualsMethodBodies :: IO ()
testExplicitPreludeDoesNotInheritBundledEqEqualsMethodBodies = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      ( Just
          ( Text.unlines
              [ "class Eq(a) {",
                "equals :: a -> a -> Bool.",
                "}.",
                "impl Eq(Int) { }."
              ]
          )
      )
      "result = Eq::equals 1 1.\nresult."
  assertSingleErrorContains
    "explicit prelude has no bundled Eq(Int).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors result)
  boolResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      ( Just
          ( Text.unlines
              [ "class Eq(a) {",
                "equals :: a -> a -> Bool.",
                "}.",
                "impl Eq(Bool) { }."
              ]
          )
      )
      "result = Eq::equals True True.\nresult."
  assertSingleErrorContains
    "explicit prelude has no bundled Eq(Bool).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors boolResult)
  floatResult <-
    compileSourceWithPrelude
      defaultWarningSettings
      ( Just
          ( Text.unlines
              [ "class Eq(a) {",
                "equals :: a -> a -> Bool.",
                "}.",
                "impl Eq(Float) { }."
              ]
          )
      )
      ( Text.unlines
          [ "left :: Float.",
            "left = 1.5.",
            "right :: Float.",
            "right = 1.5.",
            "result = Eq::equals left right.",
            "result."
          ]
      )
  assertSingleErrorContains
    "explicit prelude has no bundled Eq(Float).equals method body"
    "missing impl method body 'Eq::equals'"
    (compileErrors floatResult)

testCompileWithoutPreludeKeepsNumericConversionKernelBridgesAvailable :: IO ()
testCompileWithoutPreludeKeepsNumericConversionKernelBridgesAvailable = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x :: UInt8.\nx = __kernel_toUInt8 1."
  assertEqual
    "numeric conversion kernel bridge names remain available without prelude"
    []
    (compileErrors result)

testCompileWithoutPreludeRejectsPreludeAliases :: IO ()
testCompileWithoutPreludeRejectsPreludeAliases = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x = map hd [[1], [2]]."
  assertEqual
    "public aliases are unavailable without prelude"
    ["E1001: unbound variable 'map'", "E1001: unbound variable 'hd'"]
    (map renderDiagnostic (compileErrors result))

testCompileWithoutPreludeKeepsKernelBridgeNamesAvailable :: IO ()
testCompileWithoutPreludeKeepsKernelBridgeNamesAvailable = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "x = __kernel_map __kernel_hd [[1], [2]]."
  assertEqual
    "kernel bridge names remain available without prelude"
    []
    (compileErrors result)

testCompileWithoutPreludeStillFailsMissingBinding :: IO ()
testCompileWithoutPreludeStillFailsMissingBinding = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "seed + 1."
  assertSingleErrorContains
    "missing prelude binding still reports unbound variable"
    "E1001"
    (compileErrors result)
