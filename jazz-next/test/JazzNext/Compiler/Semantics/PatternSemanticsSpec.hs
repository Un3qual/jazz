{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import JazzNext.Compiler.AST (Pattern (..))
import JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    patternBinderNames
  )
import JazzNext.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "PatternSemantics" tests

tests :: [NamedTest]
tests =
  [ ("tuple and as-pattern binders are collected", testNestedBinders),
    ("or-patterns expose only common binders", testOrPatternBinders),
    ("Unit binds no names", testUnitBinders)
  ]

testNestedBinders :: IO ()
testNestedBinders =
  assertEqual
    "nested binders"
    (Set.fromList ["whole", "left", "right"])
    (patternBinderNames (PAs "whole" (PTuple [PVariable "left", PVariable "right"])))

testOrPatternBinders :: IO ()
testOrPatternBinders =
  assertEqual
    "common binders"
    (Set.singleton "x")
    (commonPatternBinderNames [PTuple [PVariable "x", PWildcard], PTuple [PVariable "x", PVariable "y"]])

testUnitBinders :: IO ()
testUnitBinders = assertEqual "Unit binders" Set.empty (patternBinderNames (PTuple []))
