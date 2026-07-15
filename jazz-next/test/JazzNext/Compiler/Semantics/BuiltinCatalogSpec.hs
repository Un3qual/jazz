{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolOwnership,
    builtinSymbolArity,
    builtinSymbolKernelName,
    builtinSymbolName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
    lookupBuiltinSymbol,
    lookupKernelBuiltinSymbol
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Diagnostics.Render
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( compileExpr,
    compileSource,
    compileSourceWithPrelude,
    compileErrors,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.Name
  ( mkIdentifier,
    sourceName
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertLeftDiagnosticContains,
    runTestSuite
  )
import JazzNext.TestSource
  ( JazzSourceRole (StandardLibrarySource),
    readCheckedInJazzSource
  )

main :: IO ()
main = runTestSuite "BuiltinCatalog" tests

tests :: [NamedTest]
tests =
  [ ("catalog round-trips builtin names", testCatalogRoundTripsBuiltinNames),
    ("catalog arity contract is stable", testCatalogArityContract),
    ("catalog ownership contract is stable", testCatalogOwnershipContract),
    ("kernel bridge names map to builtin targets", testKernelBridgeTargetName),
    ("kernel bridge prefix stays stable", testKernelBridgePrefix),
    ("default conversion aliases stay prelude-only", testDefaultConversionAliasesStayPreludeOnly),
    ("bundled prelude file stays reproducible from catalog", testBundledPreludeFileStaysReproducibleFromCatalog),
    ("bundled prelude comparison normalizes line endings", testBundledPreludeComparisonNormalizesLineEndings),
    ("bundled prelude keeps text traversal intrinsics private", testBundledPreludeKeepsTextTraversalIntrinsicsPrivate),
    ("bundled prelude keeps host IO intrinsics private", testBundledPreludeKeepsHostIOIntrinsicsPrivate),
    ("bundled prelude includes Eq Float64 equals method body", testBundledPreludeIncludesEqFloat64EqualsMethodBody),
    ("direct compile helper stays kernel-only", testDirectCompileHelperStaysKernelOnly),
    ("compile pipeline treats catalog builtins as bound names", testCompilePipelineTreatsCatalogBuiltinsAsBound),
    ("runtime exposes catalog builtins as callable values", testRuntimeExposesCatalogBuiltinsAsFunctions),
    ("no-prelude path rejects canonical builtin aliases", testNoPreludePathRejectsCanonicalAliases),
    ("no-prelude path keeps kernel bridge names available", testNoPreludePathKeepsKernelBridgeNames),
    ("builtin over-application reports runtime failure after saturation", testRuntimeBuiltinOverApplicationFails)
  ]

expectedBuiltins :: [(BuiltinSymbol, Text, Int, BuiltinOwnership)]
expectedBuiltins =
  [ (BuiltinMap, "map", 2, PreludeTarget),
    (BuiltinFilter, "filter", 2, PreludeTarget),
    (BuiltinHd, "hd", 1, PreludeTarget),
    (BuiltinTl, "tl", 1, PreludeTarget),
    (BuiltinPrint, "print!", 1, PreludeTarget),
    (BuiltinToInt8, "toInt8", 1, PreludeTarget),
    (BuiltinToInt16, "toInt16", 1, PreludeTarget),
    (BuiltinToInt32, "toInt32", 1, PreludeTarget),
    (BuiltinToInt64, "toInt64", 1, PreludeTarget),
    (BuiltinToUInt8, "toUInt8", 1, PreludeTarget),
    (BuiltinToUInt16, "toUInt16", 1, PreludeTarget),
    (BuiltinToUInt32, "toUInt32", 1, PreludeTarget),
    (BuiltinToUInt64, "toUInt64", 1, PreludeTarget),
    (BuiltinToFloat16, "toFloat16", 1, PreludeTarget),
    (BuiltinToFloat32, "toFloat32", 1, PreludeTarget),
    (BuiltinToFloat64, "toFloat64", 1, PreludeTarget),
    (BuiltinListPrependRaw, "listPrependRaw", 2, KernelIntrinsic),
    (BuiltinListReverseRaw, "listReverseRaw", 1, KernelIntrinsic),
    (BuiltinCharToUInt32, "charToUInt32", 1, KernelIntrinsic),
    (BuiltinCharFromUInt32Raw, "charFromUInt32Raw", 1, KernelIntrinsic),
    (BuiltinCharIsAlpha, "charIsAlpha", 1, KernelIntrinsic),
    (BuiltinCharIsAlphaNum, "charIsAlphaNum", 1, KernelIntrinsic),
    (BuiltinCharIsDigit, "charIsDigit", 1, KernelIntrinsic),
    (BuiltinCharIsSpace, "charIsSpace", 1, KernelIntrinsic),
    (BuiltinCharIsHexDigit, "charIsHexDigit", 1, KernelIntrinsic),
    (BuiltinTextLength, "textLength", 1, KernelIntrinsic),
    (BuiltinTextUnconsRaw, "textUnconsRaw", 1, KernelIntrinsic),
    (BuiltinTextAppend, "textAppend", 2, KernelIntrinsic),
    (BuiltinTextAppendChar, "textAppendChar", 2, KernelIntrinsic),
    (BuiltinTextFromChars, "textFromChars", 1, KernelIntrinsic),
    (BuiltinRenderValue, "renderValue", 1, KernelIntrinsic),
    (BuiltinReadTextRaw, "readTextRaw!", 1, KernelIntrinsic),
    (BuiltinWriteTextRaw, "writeTextRaw!", 2, KernelIntrinsic),
    (BuiltinReadStdinRaw, "readStdinRaw!", 1, KernelIntrinsic),
    (BuiltinWriteStdoutRaw, "writeStdoutRaw!", 1, KernelIntrinsic),
    (BuiltinWriteStderrRaw, "writeStderrRaw!", 1, KernelIntrinsic),
    (BuiltinArguments, "arguments!", 1, KernelIntrinsic),
    (BuiltinExit, "exit!", 1, KernelIntrinsic)
  ]

testCatalogRoundTripsBuiltinNames :: IO ()
testCatalogRoundTripsBuiltinNames =
  mapM_ assertRoundTrip expectedBuiltins
  where
    assertRoundTrip (symbol, name, _, ownership) = do
      assertEqual ("builtin name for " <> Text.pack (show symbol)) name (builtinSymbolName symbol)
      assertEqual
        ("public lookup for " <> name)
        ( case ownership of
            PreludeTarget -> Just symbol
            KernelIntrinsic -> Nothing
        )
        (lookupBuiltinSymbol name)
      assertEqual
        ("kernel lookup for " <> name)
        (Just symbol)
        (lookupKernelBuiltinSymbol (builtinSymbolKernelName symbol))

testCatalogArityContract :: IO ()
testCatalogArityContract = do
  assertEqual "symbol count" (length expectedBuiltins) (length allBuiltinSymbols)
  mapM_ assertArity expectedBuiltins
  where
    assertArity (symbol, _, expectedArity, _) =
      assertEqual
        ("arity for " <> Text.pack (show symbol))
        expectedArity
        (builtinSymbolArity symbol)

testCatalogOwnershipContract :: IO ()
testCatalogOwnershipContract =
  mapM_ assertOwnership expectedBuiltins
  where
    assertOwnership (symbol, _, _, expectedOwnership) =
      assertEqual
        ("ownership for " <> Text.pack (show symbol))
        expectedOwnership
        (builtinSymbolOwnership symbol)

testKernelBridgeTargetName :: IO ()
testKernelBridgeTargetName = do
  assertEqual "bridge target map" (Just "__kernel_map") (kernelBridgeTargetName "__kernel_map")
  assertEqual "bridge target filter" (Just "__kernel_filter") (kernelBridgeTargetName "__kernel_filter")
  assertEqual "bridge target hd" (Just "__kernel_hd") (kernelBridgeTargetName "__kernel_hd")
  assertEqual "bridge target tl" (Just "__kernel_tl") (kernelBridgeTargetName "__kernel_tl")
  assertEqual "bridge target print" (Just "__kernel_print!") (kernelBridgeTargetName "__kernel_print!")
  assertEqual "bridge target missing suffix" Nothing (kernelBridgeTargetName "__kernel_")
  assertEqual "bridge target unknown kernel symbol" Nothing (kernelBridgeTargetName "__kernel_unknown")
  assertEqual "non-bridge binding ignored" Nothing (kernelBridgeTargetName "map")

testKernelBridgePrefix :: IO ()
testKernelBridgePrefix =
  assertEqual "kernel bridge prefix" "__kernel_" kernelBridgeBindingPrefix

testDefaultConversionAliasesStayPreludeOnly :: IO ()
testDefaultConversionAliasesStayPreludeOnly = do
  assertEqual "toInt is not a catalog builtin" Nothing (lookupBuiltinSymbol "toInt")
  assertEqual "toFloat is not a catalog builtin" Nothing (lookupBuiltinSymbol "toFloat")
  assertEqual "toInt has no kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toInt")
  assertEqual "toFloat has no kernel bridge" Nothing (kernelBridgeTargetName "__kernel_toFloat")
  toIntResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x = __kernel_toInt 1."
  assertEqual
    "no-prelude compile rejects __kernel_toInt"
    ["error: E1001: unbound variable '__kernel_toInt'"]
    (map renderDiagnostic (compileErrors toIntResult))
  toFloatResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x = __kernel_toFloat 1."
  assertEqual
    "no-prelude compile rejects __kernel_toFloat"
    ["error: E1001: unbound variable '__kernel_toFloat'"]
    (map renderDiagnostic (compileErrors toFloatResult))

testBundledPreludeFileStaysReproducibleFromCatalog :: IO ()
testBundledPreludeFileStaysReproducibleFromCatalog = do
  checkedInPrelude <- readCheckedInJazzSource StandardLibrarySource "Prelude.jz"
  assertEqual
    "checked-in bundled prelude file matches catalog-generated prelude"
    bundledPreludeSource
    (normalizePreludeLineEndings checkedInPrelude)

testBundledPreludeComparisonNormalizesLineEndings :: IO ()
testBundledPreludeComparisonNormalizesLineEndings =
  assertEqual
    "CRLF checked-in prelude text normalizes to generated source"
    bundledPreludeSource
    (normalizePreludeLineEndings (Text.replace "\n" "\r\n" bundledPreludeSource))

testBundledPreludeKeepsTextTraversalIntrinsicsPrivate :: IO ()
testBundledPreludeKeepsTextTraversalIntrinsicsPrivate = do
  assertContains
    "bundled prelude contains text length kernel bridge"
    "__kernel_textLength = __kernel_textLength."
    bundledPreludeSource
  assertContains
    "bundled prelude contains raw text uncons kernel bridge"
    "__kernel_textUnconsRaw = __kernel_textUnconsRaw."
    bundledPreludeSource
  assertEqual
    "bundled prelude omits public textLength alias"
    False
    ("textLength = __kernel_textLength." `elem` Text.lines bundledPreludeSource)
  assertEqual
    "bundled prelude omits public textUnconsRaw alias"
    False
    ("textUnconsRaw = __kernel_textUnconsRaw." `elem` Text.lines bundledPreludeSource)

testBundledPreludeKeepsHostIOIntrinsicsPrivate :: IO ()
testBundledPreludeKeepsHostIOIntrinsicsPrivate =
  mapM_ assertPrivateHostIntrinsic hostIntrinsicNames
  where
    hostIntrinsicNames =
      [ "readTextRaw!",
        "writeTextRaw!",
        "readStdinRaw!",
        "writeStdoutRaw!",
        "writeStderrRaw!",
        "arguments!",
        "exit!"
      ]

    assertPrivateHostIntrinsic name = do
      let kernelName = kernelBridgeBindingPrefix <> name
      assertContains
        ("bundled prelude contains host kernel bridge " <> kernelName)
        (kernelName <> " = " <> kernelName <> ".")
        bundledPreludeSource
      assertEqual
        ("bundled prelude omits public host alias " <> name)
        False
        ((name <> " = " <> kernelName <> ".") `elem` Text.lines bundledPreludeSource)

testBundledPreludeIncludesEqFloat64EqualsMethodBody :: IO ()
testBundledPreludeIncludesEqFloat64EqualsMethodBody =
  assertContains
    "bundled prelude renders Eq(Float64).equals body"
    ( """
    impl Eq(Float64) {
    equals = \\(left, right) -> left == right.
    }.

    """
    )
    bundledPreludeSource

normalizePreludeLineEndings :: Text -> Text
normalizePreludeLineEndings text =
  let withoutCrLf = Text.replace "\r\n" "\n" text
   in Text.replace "\r" "\n" withoutCrLf

testDirectCompileHelperStaysKernelOnly :: IO ()
testDirectCompileHelperStaysKernelOnly = do
  kernelResult <- compileExpr defaultWarningSettings (runtimeExpr (EVar "__kernel_map"))
  assertEqual "direct compile helper accepts kernel bridge" [] (compileErrors kernelResult)
  canonicalResult <- compileExpr defaultWarningSettings (runtimeExpr (EVar "map"))
  assertEqual
    "direct compile helper rejects canonical alias"
    ["error: E1001: unbound variable 'map'"]
    (map renderDiagnostic (compileErrors canonicalResult))

testCompilePipelineTreatsCatalogBuiltinsAsBound :: IO ()
testCompilePipelineTreatsCatalogBuiltinsAsBound =
  mapM_ assertBuiltinCompiles expectedPreludeTargets
  where
    assertBuiltinCompiles (_, name, _, _) = do
      result <- compileSource defaultWarningSettings ("x = " <> name <> ".")
      assertEqual ("compile errors for " <> name) [] (compileErrors result)

testRuntimeExposesCatalogBuiltinsAsFunctions :: IO ()
testRuntimeExposesCatalogBuiltinsAsFunctions =
  mapM_ assertBuiltinRuns expectedPreludeTargets
  where
    assertBuiltinRuns (_, name, _, _) = do
      result <- runSource defaultWarningSettings (name <> ".")
      assertEqual ("compile errors for " <> name) [] (runCompileErrors result)
      assertEqual ("runtime errors for " <> name) [] (runRuntimeErrors result)
      assertEqual ("runtime output for " <> name) (Just "<function>") (runOutput result)

testNoPreludePathRejectsCanonicalAliases :: IO ()
testNoPreludePathRejectsCanonicalAliases =
  mapM_ assertBuiltinRejectedWithoutPrelude expectedPreludeTargets
  where
    assertBuiltinRejectedWithoutPrelude (_, name, _, _) = do
      compileResult <- compileSourceWithPrelude defaultWarningSettings Nothing ("x = " <> name <> ".")
      assertEqual
        ("no-prelude compile rejects canonical alias " <> name)
        ["error: E1001: unbound variable '" <> name <> "'"]
        (map renderDiagnostic (compileErrors compileResult))
      runResult <- runSourceWithPrelude defaultWarningSettings Nothing (name <> ".")
      assertEqual
        ("no-prelude runtime compile rejects canonical alias " <> name)
        ["error: E1001: unbound variable '" <> name <> "'"]
        (map renderDiagnostic (runCompileErrors runResult))
      assertEqual ("no-prelude runtime errors stay empty on compile failure for " <> name) [] (runRuntimeErrors runResult)
      assertEqual ("no-prelude runtime output is suppressed for " <> name) Nothing (runOutput runResult)

testNoPreludePathKeepsKernelBridgeNames :: IO ()
testNoPreludePathKeepsKernelBridgeNames =
  mapM_ assertKernelBuiltinRunsWithoutPrelude expectedBuiltins
  where
    assertKernelBuiltinRunsWithoutPrelude (symbol, _, _, _) = do
      let kernelName = builtinSymbolKernelName symbol
      compileResult <- compileSourceWithPrelude defaultWarningSettings Nothing ("x = " <> kernelName <> ".")
      assertEqual ("no-prelude compile errors for " <> kernelName) [] (compileErrors compileResult)
      runResult <- runSourceWithPrelude defaultWarningSettings Nothing (kernelName <> ".")
      assertEqual ("no-prelude runtime compile errors for " <> kernelName) [] (runCompileErrors runResult)
      assertEqual ("no-prelude runtime errors for " <> kernelName) [] (runRuntimeErrors runResult)
      assertEqual ("no-prelude runtime output for " <> kernelName) (Just "<function>") (runOutput runResult)

testRuntimeBuiltinOverApplicationFails :: IO ()
testRuntimeBuiltinOverApplicationFails =
  mapM_ assertOverApplicationFails expectedPreludeTargets
  where
    assertOverApplicationFails (_, name, _, _) = do
      let expr = overAppliedBuiltinExpr name
      assertLeftDiagnosticContains
        ("over-application runtime error for " <> name)
        "E3008"
        (evaluateRuntimeExpr expr)

-- Apply one extra argument after a builtin is fully saturated. Runtime should
-- reject application of the resulting non-function value.
overAppliedBuiltinExpr :: Text -> Expr
overAppliedBuiltinExpr name =
  runtimeExpr $
    case name of
      "map" ->
        EApply
          ( EApply
              (EApply (EVar "__kernel_map") (ESectionLeft (ELit (LInt 1)) "+"))
              (EList [ELit (LInt 2)])
          )
          (ELit (LInt 3))
      "filter" ->
        EApply
          ( EApply
              (EApply (EVar "__kernel_filter") (ESectionLeft (ELit (LInt 1)) "<"))
              (EList [ELit (LInt 2), ELit (LInt 3)])
          )
          (ELit (LInt 4))
      "hd" ->
        EApply
          (EApply (EVar "__kernel_hd") (EList [ELit (LInt 1)]))
          (ELit (LInt 2))
      "tl" ->
        EApply
          (EApply (EVar "__kernel_tl") (EList [ELit (LInt 1), ELit (LInt 2)]))
          (ELit (LInt 3))
      "print!" ->
        EApply
          (EApply (EVar "__kernel_print!") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toInt8" ->
        EApply
          (EApply (EVar "__kernel_toInt8") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toInt16" ->
        EApply
          (EApply (EVar "__kernel_toInt16") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toInt32" ->
        EApply
          (EApply (EVar "__kernel_toInt32") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toInt64" ->
        EApply
          (EApply (EVar "__kernel_toInt64") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toUInt8" ->
        EApply
          (EApply (EVar "__kernel_toUInt8") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toUInt16" ->
        EApply
          (EApply (EVar "__kernel_toUInt16") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toUInt32" ->
        EApply
          (EApply (EVar "__kernel_toUInt32") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toUInt64" ->
        EApply
          (EApply (EVar "__kernel_toUInt64") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toFloat16" ->
        EApply
          (EApply (EVar "__kernel_toFloat16") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toFloat32" ->
        EApply
          (EApply (EVar "__kernel_toFloat32") (ELit (LInt 1)))
          (ELit (LInt 2))
      "toFloat64" ->
        EApply
          (EApply (EVar "__kernel_toFloat64") (ELit (LInt 1)))
          (ELit (LInt 2))
      _ -> EApply (EVar (sourceName (mkIdentifier name))) (ELit (LInt 1))

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

expectedPreludeTargets :: [(BuiltinSymbol, Text, Int, BuiltinOwnership)]
expectedPreludeTargets =
  filter (\(_, _, _, ownership) -> ownership == PreludeTarget) expectedBuiltins
