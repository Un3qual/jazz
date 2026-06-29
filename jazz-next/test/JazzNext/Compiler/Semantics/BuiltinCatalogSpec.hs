{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
  ( doesFileExist,
    getCurrentDirectory
  )
import System.FilePath
  ( (</>),
    takeDirectory
  )
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludePath,
    bundledPreludeSource
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
    lookupBuiltinSymbol
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
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
import JazzNext.Compiler.Identifier
  ( mkIdentifier
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    runTestSuite
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
    (BuiltinToFloat64, "toFloat64", 1, PreludeTarget)
  ]

testCatalogRoundTripsBuiltinNames :: IO ()
testCatalogRoundTripsBuiltinNames =
  mapM_ assertRoundTrip expectedBuiltins
  where
    assertRoundTrip (symbol, name, _, _) = do
      assertEqual ("builtin name for " <> Text.pack (show symbol)) name (builtinSymbolName symbol)
      assertEqual ("lookup round-trip for " <> name) (Just symbol) (lookupBuiltinSymbol name)

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
    ["E1001: unbound variable '__kernel_toInt'"]
    (map renderDiagnostic (compileErrors toIntResult))
  toFloatResult <- compileSourceWithPrelude defaultWarningSettings Nothing "x = __kernel_toFloat 1."
  assertEqual
    "no-prelude compile rejects __kernel_toFloat"
    ["E1001: unbound variable '__kernel_toFloat'"]
    (map renderDiagnostic (compileErrors toFloatResult))

testBundledPreludeFileStaysReproducibleFromCatalog :: IO ()
testBundledPreludeFileStaysReproducibleFromCatalog = do
  checkedInPrelude <- readCheckedInBundledPrelude
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

normalizePreludeLineEndings :: Text -> Text
normalizePreludeLineEndings text =
  let withoutCrLf = Text.replace "\r\n" "\n" text
   in Text.replace "\r" "\n" withoutCrLf

readCheckedInBundledPrelude :: IO Text
readCheckedInBundledPrelude = do
  cwd <- getCurrentDirectory
  maybePath <- findPreludeFrom cwd
  case maybePath of
    Just path -> TextIO.readFile path
    Nothing ->
      ioError $
        userError
          ( "could not find checked-in bundled prelude mirror '"
              <> bundledPreludePath
              <> "' from current directory '"
              <> cwd
              <> "' or any parent"
          )
  where
    findPreludeFrom directory = do
      let candidate = directory </> bundledPreludePath
      exists <- doesFileExist candidate
      if exists
        then pure (Just candidate)
        else
          let parent = takeDirectory directory
           in
            if parent == directory
              then pure Nothing
              else findPreludeFrom parent

testDirectCompileHelperStaysKernelOnly :: IO ()
testDirectCompileHelperStaysKernelOnly = do
  kernelResult <- compileExpr defaultWarningSettings (runtimeExpr (EVar "__kernel_map"))
  assertEqual "direct compile helper accepts kernel bridge" [] (compileErrors kernelResult)
  canonicalResult <- compileExpr defaultWarningSettings (runtimeExpr (EVar "map"))
  assertEqual
    "direct compile helper rejects canonical alias"
    ["E1001: unbound variable 'map'"]
    (map renderDiagnostic (compileErrors canonicalResult))

testCompilePipelineTreatsCatalogBuiltinsAsBound :: IO ()
testCompilePipelineTreatsCatalogBuiltinsAsBound =
  mapM_ assertBuiltinCompiles expectedBuiltins
  where
    assertBuiltinCompiles (_, name, _, _) = do
      result <- compileSource defaultWarningSettings ("x = " <> name <> ".")
      assertEqual ("compile errors for " <> name) [] (compileErrors result)

testRuntimeExposesCatalogBuiltinsAsFunctions :: IO ()
testRuntimeExposesCatalogBuiltinsAsFunctions =
  mapM_ assertBuiltinRuns expectedBuiltins
  where
    assertBuiltinRuns (_, name, _, _) = do
      result <- runSource defaultWarningSettings (name <> ".")
      assertEqual ("compile errors for " <> name) [] (runCompileErrors result)
      assertEqual ("runtime errors for " <> name) [] (runRuntimeErrors result)
      assertEqual ("runtime output for " <> name) (Just "<function>") (runOutput result)

testNoPreludePathRejectsCanonicalAliases :: IO ()
testNoPreludePathRejectsCanonicalAliases =
  mapM_ assertBuiltinRejectedWithoutPrelude expectedBuiltins
  where
    assertBuiltinRejectedWithoutPrelude (_, name, _, _) = do
      compileResult <- compileSourceWithPrelude defaultWarningSettings Nothing ("x = " <> name <> ".")
      assertEqual
        ("no-prelude compile rejects canonical alias " <> name)
        ["E1001: unbound variable '" <> name <> "'"]
        (map renderDiagnostic (compileErrors compileResult))
      runResult <- runSourceWithPrelude defaultWarningSettings Nothing (name <> ".")
      assertEqual
        ("no-prelude runtime compile rejects canonical alias " <> name)
        ["E1001: unbound variable '" <> name <> "'"]
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
  mapM_ assertOverApplicationFails expectedBuiltins
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
      _ -> EApply (EVar (mkIdentifier name)) (ELit (LInt 1))

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]
