{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Jazz.Compiler.AST (CoreNode (..), Expr (..), Statement (..))
import Jazz.Compiler.CoreIdentity (CoreBinderId (..), ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..), mkModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.ModuleResolver.Imports (emptyImportScope)
import Jazz.Compiler.ModuleResolver.Names (ResolutionContext (..), resolveExprNames)
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
    resolveDeclarationOwner,
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
    ("declaration views preserve source identity", testDeclarationViews),
    ("generated names do not acquire user purity", testGeneratedNamesDoNotAcquireUserPurity),
    ("kernel bridge binders remain local while their targets resolve as builtins", testKernelBridgeTargetResolution),
    ("references select source-owned declarations across rebinding and lambda shadowing", testDeclarationTargets)
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
      generated = generatedName (OperatorSectionFunction 3) :: UnresolvedName
  assertEqual "unqualified source" "answer" (renderName unqualified)
  assertEqual "qualified source" "Lib::answer" (renderName qualified)
  assertEqual "local resolved" "answer" (renderName local)
  assertEqual "imported resolved" "Lib::answer" (renderName imported)
  assertEqual "ambient resolved" "answer" (renderName ambient)
  assertEqual "builtin" "print" (renderName builtin)
  assertEqual "generated" "<generated:OperatorSectionFunction 3>" (renderName generated)

testDeclarationViews :: IO ()
testDeclarationViews = do
  let path = mkModulePath (mkIdentifier "Shared" :| [])
      declaration owner = resolveDeclarationOwner owner (resolvedLocalName TypeNamespace (mkIdentifier "Box"))
      local = declaration (NamedSourceUnit path)
      imported = resolvedImportedName path TypeNamespace (mkIdentifier "Box")
  assertEqual "local diagnostic spelling" "Box" (renderName local)
  assertEqual "imported diagnostic spelling" "Shared::Box" (renderName imported)
  assertEqual "import view retains nominal identity" local imported
  assertEqual "ordered keys agree with nominal equality" (Just True) (Map.lookup imported (Map.singleton local True))
  assertEqual
    "same path in distinct source units remains distinct"
    3
    (Set.size (Set.fromList (map declaration [NamedSourceUnit path, StandaloneSourceUnit path, PreludeSourceUnit path])))

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

testDeclarationTargets :: IO ()
testDeclarationTargets =
  assertRight "lower rebinding and shadowing" (parseAndLowerStandaloneSource "x = 1. x = x + 1. f = \\(x) -> x. x.") $ \lowered ->
    mapM_ (check lowered) [StandaloneSourceUnit path, NamedSourceUnit path, PreludeSourceUnit path]
  where
    path = mkModulePath (mkIdentifier "SamePath" :| [])
    check lowered owner =
      assertRight "resolve declarations" (resolveExprNames (context owner) lowered) $ \resolved ->
        case resolved of
          EBlock _ [SLet firstNode _ _, SLet secondNode _ (EBinary _ _ (EVar earlierUse _) _), SLet _ _ (ELambda parameterNode _ (EVar parameterUse _)), SExpr _ (EVar finalUse _)] -> do
            let firstBinder = CoreBinderId (owner, coreNodeId firstNode)
                secondBinder = CoreBinderId (owner, coreNodeId secondNode)
                parameterBinder = CoreBinderId (owner, coreNodeId parameterNode)
            assertEqual "declaration carries its source owner" (Just firstBinder) (resolvedNodeBinder (coreNodeFacts firstNode))
            assertEqual "rebind publishes the replaced declaration" (Just (LexicalReference firstBinder)) (resolvedNodeShadowedReference (coreNodeFacts secondNode))
            assertEqual "lambda publishes the shadowed declaration" (Just (LexicalReference secondBinder)) (resolvedNodeShadowedReference (coreNodeFacts parameterNode))
            assertEqual "rebind initializer selects earlier declaration" (Just (LexicalReference firstBinder)) (resolvedNodeReference (coreNodeFacts earlierUse))
            assertEqual "later use selects rebind" (Just (LexicalReference secondBinder)) (resolvedNodeReference (coreNodeFacts finalUse))
            assertEqual "lambda parameter has its own declaration identity" (Just parameterBinder) (resolvedNodeBinder (coreNodeFacts parameterNode))
            assertEqual "lambda use selects parameter" (Just (LexicalReference parameterBinder)) (resolvedNodeReference (coreNodeFacts parameterUse))
          _ -> fail ("unexpected rebinding core: " <> show resolved)
    context owner = ResolutionContext owner Map.empty (exportInventory []) (exportInventory []) emptyImportScope
