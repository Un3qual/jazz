{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolArity,
    builtinSymbolName,
    kernelBridgeBindingPrefix,
    kernelBridgeTargetName,
    lookupBuiltinSymbol
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( compileSource,
    compileErrors,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSource
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
    assertLeftContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "BuiltinCatalog" tests

tests :: [NamedTest]
tests =
  [ ("catalog round-trips builtin names", testCatalogRoundTripsBuiltinNames),
    ("catalog arity contract is stable", testCatalogArityContract),
    ("kernel bridge names map to builtin targets", testKernelBridgeTargetName),
    ("kernel bridge prefix stays stable", testKernelBridgePrefix),
    ("compile pipeline treats catalog builtins as bound names", testCompilePipelineTreatsCatalogBuiltinsAsBound),
    ("runtime exposes catalog builtins as callable values", testRuntimeExposesCatalogBuiltinsAsFunctions),
    ("builtin over-application reports runtime failure after saturation", testRuntimeBuiltinOverApplicationFails)
  ]

expectedBuiltins :: [(BuiltinSymbol, Text, Int)]
expectedBuiltins =
  [ (BuiltinMap, "map", 2),
    (BuiltinHd, "hd", 1),
    (BuiltinTl, "tl", 1),
    (BuiltinPrint, "print!", 1)
  ]

testCatalogRoundTripsBuiltinNames :: IO ()
testCatalogRoundTripsBuiltinNames =
  mapM_ assertRoundTrip expectedBuiltins
  where
    assertRoundTrip (symbol, name, _) = do
      assertEqual ("builtin name for " <> Text.pack (show symbol)) name (builtinSymbolName symbol)
      assertEqual ("lookup round-trip for " <> name) (Just symbol) (lookupBuiltinSymbol name)

testCatalogArityContract :: IO ()
testCatalogArityContract = do
  assertEqual "symbol count" (length expectedBuiltins) (length allBuiltinSymbols)
  mapM_ assertArity expectedBuiltins
  where
    assertArity (symbol, _, expectedArity) =
      assertEqual
        ("arity for " <> Text.pack (show symbol))
        expectedArity
        (builtinSymbolArity symbol)

testKernelBridgeTargetName :: IO ()
testKernelBridgeTargetName = do
  assertEqual "bridge target map" (Just "map") (kernelBridgeTargetName "__kernel_map")
  assertEqual "bridge target hd" (Just "hd") (kernelBridgeTargetName "__kernel_hd")
  assertEqual "bridge target tl" (Just "tl") (kernelBridgeTargetName "__kernel_tl")
  assertEqual "bridge target print" (Just "print!") (kernelBridgeTargetName "__kernel_print!")
  assertEqual "bridge target missing suffix" Nothing (kernelBridgeTargetName "__kernel_")
  assertEqual "non-bridge binding ignored" Nothing (kernelBridgeTargetName "map")

testKernelBridgePrefix :: IO ()
testKernelBridgePrefix =
  assertEqual "kernel bridge prefix" "__kernel_" kernelBridgeBindingPrefix

testCompilePipelineTreatsCatalogBuiltinsAsBound :: IO ()
testCompilePipelineTreatsCatalogBuiltinsAsBound =
  mapM_ assertBuiltinCompiles expectedBuiltins
  where
    assertBuiltinCompiles (_, name, _) = do
      result <- compileSource defaultWarningSettings ("x = " <> name <> ".")
      assertEqual ("compile errors for " <> name) [] (compileErrors result)

testRuntimeExposesCatalogBuiltinsAsFunctions :: IO ()
testRuntimeExposesCatalogBuiltinsAsFunctions =
  mapM_ assertBuiltinRuns expectedBuiltins
  where
    assertBuiltinRuns (_, name, _) = do
      result <- runSource defaultWarningSettings (name <> ".")
      assertEqual ("compile errors for " <> name) [] (runCompileErrors result)
      assertEqual ("runtime errors for " <> name) [] (runRuntimeErrors result)
      assertEqual ("runtime output for " <> name) (Just "<function>") (runOutput result)

testRuntimeBuiltinOverApplicationFails :: IO ()
testRuntimeBuiltinOverApplicationFails =
  mapM_ assertOverApplicationFails expectedBuiltins
  where
    assertOverApplicationFails (_, name, _) = do
      let expr = overAppliedBuiltinExpr name
      assertLeftContains
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
              (EApply (EVar "map") (ESectionLeft (EInt 1) "+"))
              (EList [EInt 2])
          )
          (EInt 3)
      "hd" ->
        EApply
          (EApply (EVar "hd") (EList [EInt 1]))
          (EInt 2)
      "tl" ->
        EApply
          (EApply (EVar "tl") (EList [EInt 1, EInt 2]))
          (EInt 3)
      "print!" ->
        EApply
          (EApply (EVar "print!") (EInt 1))
          (EInt 2)
      _ -> EApply (EVar name) (EInt 1)

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]
