{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbol
  )
import JazzNext.Compiler.Driver
  ( compileSource,
    runSource,
    compileErrors,
    runCompileErrors,
    runRuntimeErrors,
    runOutput
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "BuiltinCatalog" tests

tests :: [NamedTest]
tests =
  [ ("catalog round-trips builtin names", testCatalogRoundTripsBuiltinNames),
    ("catalog arity contract is stable", testCatalogArityContract),
    ("compile pipeline treats catalog builtins as bound names", testCompilePipelineTreatsCatalogBuiltinsAsBound),
    ("runtime exposes catalog builtins as callable values", testRuntimeExposesCatalogBuiltinsAsFunctions)
  ]

expectedBuiltins :: [(BuiltinSymbol, Text, Int)]
expectedBuiltins =
  [ (BuiltinMap, "map", 2),
    (BuiltinHd, "hd", 1),
    (BuiltinTl, "tl", 1)
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
