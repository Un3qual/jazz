{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import Jazz.Compiler.AST
  ( CoreNode (..),
    CoreNodeId (..),
    CorePhase (Lowered),
    CoreSort (PatternSort),
    Pattern (..),
  )
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Name (mkIdentifier, sourceName)
import Jazz.Compiler.Pattern
  ( commonPatternBinderNames,
    patternBinderNames,
  )
import Jazz.TestHarness (NamedTest, assertEqual, runTestSuite)

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
    ( patternBinderNames
        ( PAs
            (node 0)
            (sourceName (mkIdentifier "whole"))
            ( PTuple
                (node 1)
                [ PVariable (node 2) (sourceName (mkIdentifier "left")),
                  PVariable (node 3) (sourceName (mkIdentifier "right"))
                ]
            )
        )
    )

testOrPatternBinders :: IO ()
testOrPatternBinders =
  assertEqual
    "common binders"
    (Set.singleton "x")
    ( commonPatternBinderNames
        [ PTuple (node 0) [PVariable (node 1) (sourceName (mkIdentifier "x")), PWildcard (node 2)],
          PTuple (node 3) [PVariable (node 4) (sourceName (mkIdentifier "x")), PVariable (node 5) (sourceName (mkIdentifier "y"))]
        ]
    )

testUnitBinders :: IO ()
testUnitBinders = assertEqual "Unit binders" Set.empty (patternBinderNames (PTuple (node 0) []))

node :: Int -> CoreNode 'Lowered 'PatternSort
node nodeId = CoreNode (CoreNodeId nodeId) (SourceSpan 1 1) ()
