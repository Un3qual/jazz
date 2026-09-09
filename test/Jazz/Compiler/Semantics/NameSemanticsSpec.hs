{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Jazz.Compiler.AST (Expr (..), Statement (..))
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (mkModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    UnresolvedName,
    generatedName,
    mkIdentifier,
    namePurity,
    qualifiedName,
    renderName,
    resolvedAmbientName,
    resolvedImportedName,
    resolvedLocalName,
    sourceName,
  )
import Jazz.Compiler.Purity (Purity (..))
import Jazz.Compiler.SourceProgram (parseAndLowerStandaloneSource)
import Jazz.TestHarness (NamedTest, assertEqual, assertRight, runTestSuite)

main :: IO ()
main = runTestSuite "NameSemantics" tests

tests :: [NamedTest]
tests =
  [ ("source and resolved names render from distinct payloads", testSourceAndResolvedNamesRender),
    ("every name origin renders its stable spelling", testEveryNameOriginRenders),
    ("generated names do not acquire user purity", testGeneratedNamesDoNotAcquireUserPurity),
    ("kernel bridge binders remain local while their targets resolve as builtins", testKernelBridgeTargetResolution)
  ]

testSourceAndResolvedNamesRender :: IO ()
testSourceAndResolvedNamesRender = do
  let modulePath = mkModulePath (mkIdentifier "Lib" :| [])
      source = sourceName (mkIdentifier "Lib::answer")
      imported = resolvedImportedName modulePath ValueNamespace (mkIdentifier "answer")
  assertEqual "rendered source" "Lib::answer" (renderName source)
  assertEqual "rendered imported" "Lib::answer" (renderName imported)
  assertEqual
    "imported origin retains nominal path"
    (UserName (ResolvedUserName (ImportedModule modulePath) ValueNamespace (mkIdentifier "answer")))
    imported

testEveryNameOriginRenders :: IO ()
testEveryNameOriginRenders = do
  let modulePath = mkModulePath (mkIdentifier "Lib" :| [])
      unqualified = sourceName (mkIdentifier "answer")
      qualified = qualifiedName (mkIdentifier "Lib") (mkIdentifier "answer")
      local = resolvedLocalName ValueNamespace (mkIdentifier "answer")
      imported = resolvedImportedName modulePath ValueNamespace (mkIdentifier "answer")
      ambient = resolvedAmbientName ValueNamespace (mkIdentifier "answer")
      builtin = BuiltinName (mkIdentifier "print") :: ResolvedName
      generated = generatedName OperatorSectionFunction :: UnresolvedName
  assertEqual "unqualified source" "answer" (renderName unqualified)
  assertEqual "qualified source" "Lib::answer" (renderName qualified)
  assertEqual "local resolved" "answer" (renderName local)
  assertEqual "imported resolved" "Lib::answer" (renderName imported)
  assertEqual "ambient resolved" "answer" (renderName ambient)
  assertEqual "builtin" "print" (renderName builtin)
  assertEqual "generated" "<generated:OperatorSectionFunction>" (renderName generated)

testGeneratedNamesDoNotAcquireUserPurity :: IO ()
testGeneratedNamesDoNotAcquireUserPurity = do
  let generated = generatedName (OperatorBinding "!") :: UnresolvedName
  assertEqual "generated constructor" (GeneratedName (OperatorBinding "!")) generated
  assertEqual "generated purity" Pure (namePurity generated)

testKernelBridgeTargetResolution :: IO ()
testKernelBridgeTargetResolution =
  assertRight "lower kernel bridge" (parseAndLowerStandaloneSource "__kernel_hd = __kernel_hd.") $ \lowered ->
    assertRight "resolve kernel bridge" (resolveStandaloneExprNames (exportInventory []) lowered) $ \resolved ->
      case resolved of
        EBlock _ [SLet _ binder (EVar _ target)] -> do
          assertEqual
            "kernel bridge binder"
            (resolvedLocalName ValueNamespace (mkIdentifier "__kernel_hd"))
            binder
          assertEqual
            "kernel bridge target"
            (BuiltinName (mkIdentifier "__kernel_hd"))
            target
        _ -> assertEqual "kernel bridge core shape" "single let block" (show resolved)
