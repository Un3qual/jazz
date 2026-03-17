{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolOwnership,
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
    ("catalog ownership contract is stable", testCatalogOwnershipContract),
    ("kernel bridge names map to builtin targets", testKernelBridgeTargetName),
    ("kernel bridge prefix stays stable", testKernelBridgePrefix),
    ("compile pipeline rejects public builtin names without prelude", testCompilePipelineRejectsPublicBuiltinsWithoutPrelude),
    ("compile pipeline treats kernel bridge builtins as bound names", testCompilePipelineTreatsKernelBridgeBuiltinsAsBound),
    ("runtime exposes kernel bridge builtins as callable values", testRuntimeExposesKernelBridgeBuiltinsAsFunctions),
    ("builtin over-application reports runtime failure after saturation", testRuntimeBuiltinOverApplicationFails)
  ]

expectedBuiltins :: [(BuiltinSymbol, Text, Int, BuiltinOwnership)]
expectedBuiltins =
  [ (BuiltinMap, "map", 2, PreludeTarget),
    (BuiltinFilter, "filter", 2, PreludeTarget),
    (BuiltinHd, "hd", 1, PreludeTarget),
    (BuiltinTl, "tl", 1, PreludeTarget),
    (BuiltinPrint, "print!", 1, PreludeTarget)
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
  assertEqual "bridge target map" (Just "map") (kernelBridgeTargetName "__kernel_map")
  assertEqual "bridge target filter" (Just "filter") (kernelBridgeTargetName "__kernel_filter")
  assertEqual "bridge target hd" (Just "hd") (kernelBridgeTargetName "__kernel_hd")
  assertEqual "bridge target tl" (Just "tl") (kernelBridgeTargetName "__kernel_tl")
  assertEqual "bridge target print" (Just "print!") (kernelBridgeTargetName "__kernel_print!")
  assertEqual "bridge target missing suffix" Nothing (kernelBridgeTargetName "__kernel_")
  assertEqual "non-bridge binding ignored" Nothing (kernelBridgeTargetName "map")

testKernelBridgePrefix :: IO ()
testKernelBridgePrefix =
  assertEqual "kernel bridge prefix" "__kernel_" kernelBridgeBindingPrefix

testCompilePipelineRejectsPublicBuiltinsWithoutPrelude :: IO ()
testCompilePipelineRejectsPublicBuiltinsWithoutPrelude =
  mapM_ assertBuiltinRejected expectedBuiltins
  where
    assertBuiltinRejected (_, name, _, _) = do
      result <- compileSource defaultWarningSettings ("x = " <> name <> ".")
      assertLeftContains
        ("public builtin requires prelude for " <> name)
        "E1001"
        (case compileErrors result of
            [] -> Right ()
            errorsOut -> Left (Text.intercalate "\n" errorsOut))

testCompilePipelineTreatsKernelBridgeBuiltinsAsBound :: IO ()
testCompilePipelineTreatsKernelBridgeBuiltinsAsBound =
  mapM_ assertBuiltinCompiles expectedBuiltins
  where
    assertBuiltinCompiles (_, name, _, _) = do
      let bridgeName = bridgeBindingName name
      result <- compileSource defaultWarningSettings ("x = " <> bridgeName <> ".")
      assertEqual ("compile errors for " <> bridgeName) [] (compileErrors result)

testRuntimeExposesKernelBridgeBuiltinsAsFunctions :: IO ()
testRuntimeExposesKernelBridgeBuiltinsAsFunctions =
  mapM_ assertBuiltinRuns expectedBuiltins
  where
    assertBuiltinRuns (_, name, _, _) = do
      let bridgeName = bridgeBindingName name
      result <- runSource defaultWarningSettings (bridgeName <> ".")
      assertEqual ("compile errors for " <> name) [] (runCompileErrors result)
      assertEqual ("runtime errors for " <> name) [] (runRuntimeErrors result)
      assertEqual ("runtime output for " <> name) (Just "<function>") (runOutput result)

testRuntimeBuiltinOverApplicationFails :: IO ()
testRuntimeBuiltinOverApplicationFails =
  mapM_ assertOverApplicationFails expectedBuiltins
  where
    assertOverApplicationFails (_, name, _, _) = do
      let expr = overAppliedBuiltinExpr (bridgeBindingName name)
      assertLeftContains
        ("over-application runtime error for " <> name)
        "E3008"
        (evaluateRuntimeExpr expr)

bridgeBindingName :: Text -> Text
bridgeBindingName name = kernelBridgeBindingPrefix <> name

-- Apply one extra argument after a builtin is fully saturated. Runtime should
-- reject application of the resulting non-function value.
overAppliedBuiltinExpr :: Text -> Expr
overAppliedBuiltinExpr name =
  runtimeExpr $
    case publicName of
      "map" ->
        EApply
          ( EApply
              (EApply (EVar name) (ESectionLeft (EInt 1) "+"))
              (EList [EInt 2])
          )
          (EInt 3)
      "filter" ->
        EApply
          ( EApply
              (EApply (EVar name) (ESectionLeft (EInt 1) "<"))
              (EList [EInt 2, EInt 3])
          )
          (EInt 4)
      "hd" ->
        EApply
          (EApply (EVar name) (EList [EInt 1]))
          (EInt 2)
      "tl" ->
        EApply
          (EApply (EVar name) (EList [EInt 1, EInt 2]))
          (EInt 3)
      "print!" ->
        EApply
          (EApply (EVar name) (EInt 1))
          (EInt 2)
      _ -> EApply (EVar name) (EInt 1)
  where
    publicName =
      case kernelBridgeTargetName name of
        Just targetName -> targetName
        Nothing -> name

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EScope
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]
