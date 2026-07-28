{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.Parser (parseSurfaceProgram)
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceFunctionClause (..),
    SurfacePattern (..),
    SurfaceStatement (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "FunctionEquationParser" tests

tests :: [NamedTest]
tests =
  [ ("parses ordinary function heads", testParsesOrdinaryFunctionHeads),
    ("groups contiguous ordered equations", testGroupsContiguousOrderedEquations),
    ("parses grouped tuple and constructor head patterns", testParsesGroupedHeadPatterns),
    ("parses list head patterns", testParsesListHeadPatterns),
    ("rejects mixed clause arity", testRejectsMixedClauseArity),
    ("rejects unterminated grouped head patterns", testRejectsUnterminatedGroupedHeadPattern)
  ]

testParsesOrdinaryFunctionHeads :: IO ()
testParsesOrdinaryFunctionHeads =
  assertEqual
    "ordinary function heads"
    ( Right
        ( SEBlock
            [ SSFunction
                "identity"
                (SourceSpan 1 1)
                (SurfaceFunctionClause (SourceSpan 1 1) [SPVariable "item"] (SEVar "item") :| []),
              SSFunction
                "constant"
                (SourceSpan 2 1)
                ( SurfaceFunctionClause
                    (SourceSpan 2 1)
                    [SPVariable "left", SPVariable "right"]
                    (SEVar "left")
                    :| []
                )
            ]
        )
    )
    ( parseSurfaceProgram
        """
        identity item = item.
        constant left right = left.
        """
    )

testGroupsContiguousOrderedEquations :: IO ()
testGroupsContiguousOrderedEquations =
  assertEqual
    "contiguous ordered equations"
    ( Right
        ( SEBlock
            [ SSFunction
                "mapMaybe"
                (SourceSpan 1 1)
                ( SurfaceFunctionClause
                    (SourceSpan 1 1)
                    [SPVariable "transform", SPConstructor "Nothing" []]
                    (SEVar "Nothing")
                    :| [ SurfaceFunctionClause
                           (SourceSpan 2 1)
                           [SPVariable "transform", SPConstructor "Just" [SPVariable "item"]]
                           (SEApply (SEVar "Just") (SEApply (SEVar "transform") (SEVar "item")))
                       ]
                )
            ]
        )
    )
    ( parseSurfaceProgram
        """
        mapMaybe transform Nothing = Nothing.
        mapMaybe transform (Just item) = Just (transform item).
        """
    )

testParsesGroupedHeadPatterns :: IO ()
testParsesGroupedHeadPatterns =
  assertEqual
    "grouped tuple and constructor patterns"
    ( Right
        ( SEBlock
            [ SSFunction
                "pair"
                (SourceSpan 1 1)
                ( SurfaceFunctionClause
                    (SourceSpan 1 1)
                    [SPTuple [SPVariable "left", SPVariable "right"]]
                    (SEVar "left")
                    :| []
                ),
              SSFunction
                "unwrap"
                (SourceSpan 2 1)
                ( SurfaceFunctionClause
                    (SourceSpan 2 1)
                    [SPConstructor "Just" [SPVariable "item"]]
                    (SEVar "item")
                    :| []
                )
            ]
        )
    )
    ( parseSurfaceProgram
        """
        pair (left, right) = left.
        unwrap (Just item) = item.
        """
    )

testParsesListHeadPatterns :: IO ()
testParsesListHeadPatterns =
  assertEqual
    "list head patterns"
    ( Right
        ( SEBlock
            [ SSFunction
                "headOr"
                (SourceSpan 1 1)
                ( SurfaceFunctionClause
                    (SourceSpan 1 1)
                    [SPVariable "fallback", SPConsList (SPVariable "first") SPWildcard]
                    (SEVar "first")
                    :| [ SurfaceFunctionClause
                           (SourceSpan 2 1)
                           [SPVariable "fallback", SPList []]
                           (SEVar "fallback")
                       ]
                )
            ]
        )
    )
    ( parseSurfaceProgram
        """
        headOr fallback [first | _] = first.
        headOr fallback [] = fallback.
        """
    )

testRejectsMixedClauseArity :: IO ()
testRejectsMixedClauseArity =
  assertLeftDiagnosticContains
    "mixed function equation arity"
    "function 'pick' clauses must all have 1 parameter(s), found 2"
    ( parseSurfaceProgram
        """
        pick Nothing = 0.
        pick (Just item) fallback = item.
        """
    )

testRejectsUnterminatedGroupedHeadPattern :: IO ()
testRejectsUnterminatedGroupedHeadPattern =
  assertLeftDiagnosticContains
    "unterminated grouped function-head pattern"
    "expected ')'"
    (parseSurfaceProgram "unwrap (Just item = item.")
