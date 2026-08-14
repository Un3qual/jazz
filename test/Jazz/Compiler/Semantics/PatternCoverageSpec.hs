{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.PatternCoverage
  ( ConstructorInventory,
    PatternCoverageFailure (..),
    analyzePatternCoverage,
    constructorInventoryFromBindings,
    emptyConstructorInventory,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    TypeBinding (..),
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "PatternCoverage" tests

tests :: [NamedTest]
tests =
  [ ("empty Bool match reports the first missing constructor", testEmptyBoolMatch),
    ("both Bool constructors are exhaustive", testCompleteBoolMatch),
    ("duplicate Bool arm is unreachable", testDuplicateBoolArm),
    ("open integer literals require a fallback", testOpenIntegerDomain),
    ("unguarded wildcard makes a later arm unreachable", testWildcardShadowing),
    ("guarded arms do not contribute coverage", testGuardedArmDoesNotCover),
    ("guarded arms do not shadow later arms", testGuardedArmDoesNotShadow),
    ("unit has one exhaustive tuple pattern", testUnitCoverage),
    ("tuple coverage decomposes element domains", testTupleCoverage),
    ("nil and cons cover every list", testListCoverage),
    ("missing cons reports a list witness", testMissingListCons),
    ("visible ADT constructors form a closed domain", testAdtCoverage),
    ("missing ADT constructor reports its fields", testMissingAdtConstructor),
    ("ADT payloads decompose nested constructor spaces", testNestedAdtCoverage),
    ("hidden ADT constructors keep the domain open", testHiddenAdtConstructor),
    ("exact lists specialize through cons cells", testExactListShadowing),
    ("as-patterns contribute their inner coverage", testAsPatternCoverage),
    ("or-pattern alternatives form a coverage union", testOrPatternCoverage),
    ("partly useful or-pattern arm stays reachable", testPartlyUsefulOrPattern),
    ("wholly covered or-pattern arm is unreachable", testCoveredOrPattern)
  ]

testEmptyBoolMatch :: IO ()
testEmptyBoolMatch =
  assertCoverage
    "empty Bool"
    TBoolType
    []
    [NonExhaustivePattern (PLiteral (LBool False))]

testCompleteBoolMatch :: IO ()
testCompleteBoolMatch =
  assertCoverage
    "complete Bool"
    TBoolType
    [arm (PLiteral (LBool False)), arm (PLiteral (LBool True))]
    []

testDuplicateBoolArm :: IO ()
testDuplicateBoolArm =
  assertCoverage
    "duplicate Bool"
    TBoolType
    [arm (PLiteral (LBool False)), arm (PLiteral (LBool False)), arm PWildcard]
    [UnreachablePatternArm 2]

testOpenIntegerDomain :: IO ()
testOpenIntegerDomain =
  assertCoverage
    "open integer"
    TIntType
    [arm (PLiteral (LInt 0))]
    [NonExhaustivePattern PWildcard]

testWildcardShadowing :: IO ()
testWildcardShadowing =
  assertCoverage
    "wildcard shadowing"
    TIntType
    [arm PWildcard, arm (PLiteral (LInt 1))]
    [UnreachablePatternArm 2]

testGuardedArmDoesNotCover :: IO ()
testGuardedArmDoesNotCover =
  assertCoverage
    "guarded coverage"
    TBoolType
    [guardedArm (PLiteral (LBool False)), arm (PLiteral (LBool True))]
    [NonExhaustivePattern (PLiteral (LBool False))]

testGuardedArmDoesNotShadow :: IO ()
testGuardedArmDoesNotShadow =
  assertCoverage
    "guarded shadowing"
    TBoolType
    [ guardedArm (PLiteral (LBool False)),
      arm (PLiteral (LBool False)),
      arm (PLiteral (LBool True))
    ]
    []

testUnitCoverage :: IO ()
testUnitCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "unit"
    (TTupleType [])
    [arm (PTuple [])]
    []

testTupleCoverage :: IO ()
testTupleCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "tuple"
    (TTupleType [TBoolType, TBoolType])
    [ arm (PTuple [PLiteral (LBool False), PWildcard]),
      arm (PTuple [PLiteral (LBool True), PWildcard])
    ]
    []

testListCoverage :: IO ()
testListCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "list"
    (TListType TIntType)
    [arm (PList []), arm (PConsList PWildcard PWildcard)]
    []

testMissingListCons :: IO ()
testMissingListCons =
  assertCoverageWith
    emptyConstructorInventory
    "missing list cons"
    (TListType TIntType)
    [arm (PList [])]
    [NonExhaustivePattern (PConsList PWildcard PWildcard)]

testAdtCoverage :: IO ()
testAdtCoverage =
  assertCoverageWith
    maybeInventory
    "ADT"
    maybeIntType
    [ arm (PConstructor "Nothing" []),
      arm (PConstructor "Just" [PWildcard])
    ]
    []

testMissingAdtConstructor :: IO ()
testMissingAdtConstructor =
  assertCoverageWith
    maybeInventory
    "missing ADT constructor"
    maybeIntType
    [arm (PConstructor "Nothing" [])]
    [NonExhaustivePattern (PConstructor "Just" [PWildcard])]

testNestedAdtCoverage :: IO ()
testNestedAdtCoverage =
  assertCoverageWith
    maybeInventory
    "nested ADT"
    (TDataType "Maybe" [TBoolType])
    [ arm (PConstructor "Nothing" []),
      arm (PConstructor "Just" [PLiteral (LBool False)])
    ]
    [NonExhaustivePattern (PConstructor "Just" [PLiteral (LBool True)])]

testHiddenAdtConstructor :: IO ()
testHiddenAdtConstructor =
  assertCoverageWith
    hiddenMaybeInventory
    "hidden ADT constructor"
    maybeIntType
    [arm (PConstructor "Nothing" [])]
    [NonExhaustivePattern PWildcard]

testExactListShadowing :: IO ()
testExactListShadowing =
  assertCoverageWith
    emptyConstructorInventory
    "exact list shadowing"
    (TListType TBoolType)
    [ arm (PList []),
      arm (PConsList PWildcard PWildcard),
      arm (PList [PLiteral (LBool True)])
    ]
    [UnreachablePatternArm 3]

testAsPatternCoverage :: IO ()
testAsPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "as-pattern"
    TBoolType
    [ arm (PAs "whole" (PLiteral (LBool False))),
      arm (PLiteral (LBool True))
    ]
    []

testOrPatternCoverage :: IO ()
testOrPatternCoverage =
  assertCoverageWith
    emptyConstructorInventory
    "or-pattern"
    TBoolType
    [arm (POr [PLiteral (LBool False), PLiteral (LBool True)])]
    []

testPartlyUsefulOrPattern :: IO ()
testPartlyUsefulOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "partly useful or-pattern"
    TBoolType
    [ arm (PLiteral (LBool False)),
      arm (POr [PLiteral (LBool False), PLiteral (LBool True)])
    ]
    []

testCoveredOrPattern :: IO ()
testCoveredOrPattern =
  assertCoverageWith
    emptyConstructorInventory
    "covered or-pattern"
    TBoolType
    [ arm (PLiteral (LBool False)),
      arm (PLiteral (LBool True)),
      arm (POr [PLiteral (LBool False), PLiteral (LBool True)])
    ]
    [UnreachablePatternArm 3]

assertCoverage :: Text -> ExpressionType -> [CaseArm] -> [PatternCoverageFailure] -> IO ()
assertCoverage label expressionType arms expected =
  assertCoverageWith emptyConstructorInventory label expressionType arms expected

assertCoverageWith :: ConstructorInventory -> Text -> ExpressionType -> [CaseArm] -> [PatternCoverageFailure] -> IO ()
assertCoverageWith inventory label expressionType arms expected =
  assertEqual label expected (analyzePatternCoverage inventory expressionType arms)

arm :: Pattern -> CaseArm
arm patternValue = CaseArm patternValue Nothing (ELit (LInt 0))

guardedArm :: Pattern -> CaseArm
guardedArm patternValue =
  CaseArm patternValue (Just (ELit (LBool True))) (ELit (LInt 0))

maybeIntType :: ExpressionType
maybeIntType = TDataType "Maybe" [TIntType]

maybeInventory :: ConstructorInventory
maybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            ["a"]
            [ [],
              [ConstructorArgumentParameter "a"]
            ]
        )
    )
    ( Map.fromList
        [ ("Nothing", ConstructorTypeBinding "Maybe" ["a"] []),
          ("Just", ConstructorTypeBinding "Maybe" ["a"] [ConstructorArgumentParameter "a"])
        ]
    )

hiddenMaybeInventory :: ConstructorInventory
hiddenMaybeInventory =
  constructorInventoryFromBindings
    ( Map.singleton
        "Maybe"
        ( DataTypeBinding
            ["a"]
            [ [],
              [ConstructorArgumentParameter "a"]
            ]
        )
    )
    (Map.singleton "Nothing" (ConstructorTypeBinding "Maybe" ["a"] []))
