{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    mkIdentifier
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "RecursiveBindings" tests

tests :: [NamedTest]
tests =
  [ ("collect binding names keeps let declaration indices", testCollectBindingNames),
    ("free vars treat lambda parameters as bound", testFreeVarsLambdaParameterBound),
    ("free vars treat bindings as visible in their own rhs", testFreeVarsScopeBindsSelfRecursion),
    ("recursive groups keep singleton self-recursive bindings", testRecursiveGroupsKeepSingletonSelfRecursion),
    ("recursive groups ignore same-name non-alias references", testRecursiveGroupsIgnoreSameNameNonAliasReference),
    ("recursive groups suppress singleton self edge when outer binding exists", testRecursiveGroupsPreferOuterBindingForSingletonName),
    ("nested block SCC free vars stay local to the block", testFreeVarsScopeKeepsNestedRecursivePeersLocal),
    ("recursive groups do not leak nested block SCC peers to outer scope", testRecursiveGroupsDoNotLeakNestedBlockPeers),
    ("recursive groups preserve declaration order through alias bridge", testRecursiveGroupsPreserveDeclarationOrder),
    ("recursive groups prefer nearest earlier rebinding over later declaration", testRecursiveGroupsPreferNearestEarlierRebinding),
    ("self-recursive binding detection is parameterized by caller predicate", testInferSelfRecursiveBindingsIsParameterized)
  ]

testCollectBindingNames :: IO ()
testCollectBindingNames =
  assertEqual
    "binding names"
    (Map.fromList [(0, "x"), (2, "y")])
    (collectBindingNames indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "x") span0 (ELit (LInt 1))),
        (1, SSignature (ident "x") span0 "Int"),
        (2, SLet (ident "y") span0 (EVar (ident "x")))
      ]

testFreeVarsLambdaParameterBound :: IO ()
testFreeVarsLambdaParameterBound =
  assertEqual
    "lambda free vars"
    (Set.singleton "y")
    (freeVarsExprWithBound Set.empty expr)
  where
    expr =
      ELambda
        (ident "x")
        (EApply (EVar (ident "x")) (EVar (ident "y")))

testFreeVarsScopeBindsSelfRecursion :: IO ()
testFreeVarsScopeBindsSelfRecursion =
  assertEqual
    "scope free vars"
    (Set.singleton "g")
    (freeVarsScopeWithBound Set.empty statements)
  where
    statements =
      [ SLet
          (ident "f")
          span0
          (EApply (EVar (ident "f")) (EVar (ident "g")))
      ]

testRecursiveGroupsKeepSingletonSelfRecursion :: IO ()
testRecursiveGroupsKeepSingletonSelfRecursion =
  assertEqual
    "singleton self-recursive group"
    (Map.fromList [(0, [0])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 (EVar (ident "f")))
      ]

testRecursiveGroupsIgnoreSameNameNonAliasReference :: IO ()
testRecursiveGroupsIgnoreSameNameNonAliasReference =
  assertEqual
    "same-name non-alias reference does not create self edge"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 (EApply (ELambda (ident "x") (EVar (ident "x"))) (EVar (ident "f"))))
      ]

testRecursiveGroupsPreferOuterBindingForSingletonName :: IO ()
testRecursiveGroupsPreferOuterBindingForSingletonName =
  assertEqual
    "outer singleton binding suppresses self edge"
    Map.empty
    (inferRecursiveGroupsOrdered (Set.singleton "f") indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 (EVar (ident "f")))
      ]

testFreeVarsScopeKeepsNestedRecursivePeersLocal :: IO ()
testFreeVarsScopeKeepsNestedRecursivePeersLocal =
  assertEqual
    "nested recursive peer names stay local to block"
    Set.empty
    (freeVarsScopeWithBound Set.empty nestedStatements)
  where
    nestedStatements =
      [ SLet (ident "y") span0 (EVar (ident "z")),
        SLet (ident "z") span0 (EVar (ident "y")),
        SExpr span0 (EVar (ident "y"))
      ]

testRecursiveGroupsDoNotLeakNestedBlockPeers :: IO ()
testRecursiveGroupsDoNotLeakNestedBlockPeers =
  assertEqual
    "nested block recursive peer names do not form outer SCC"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    nestedBlock =
      EBlock
        [ SLet (ident "y") span0 (EVar (ident "z")),
          SLet (ident "z") span0 (EVar (ident "y")),
          SExpr span0 (EVar (ident "y"))
        ]
    indexedStatements =
      [ (0, SLet (ident "x") span0 nestedBlock),
        (1, SLet (ident "z") span0 (EVar (ident "x")))
      ]

testRecursiveGroupsPreserveDeclarationOrder :: IO ()
testRecursiveGroupsPreserveDeclarationOrder =
  assertEqual
    "ordered recursive group"
    (Map.fromList [(0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 (EVar (ident "h"))),
        (1, SLet (ident "h") span0 (EVar (ident "g"))),
        (2, SLet (ident "g") span0 (EVar (ident "f")))
      ]

testRecursiveGroupsPreferNearestEarlierRebinding :: IO ()
testRecursiveGroupsPreferNearestEarlierRebinding =
  assertEqual
    "nearest earlier rebinding wins"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "x") span0 (ELit (LInt 0))),
        (1, SLet (ident "f") span0 (EVar (ident "x"))),
        (2, SLet (ident "x") span0 (EVar (ident "f")))
      ]

testInferSelfRecursiveBindingsIsParameterized :: IO ()
testInferSelfRecursiveBindingsIsParameterized = do
  assertEqual
    "wrapped lambda policy marks self recursion"
    (Set.singleton 0)
    (inferSelfRecursiveBindings hasWrappedLambdaBranch indexedStatements)
  assertEqual
    "bare lambda policy does not mark wrapped self recursion"
    Set.empty
    (inferSelfRecursiveBindings isBareLambda indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 wrappedSelfRecursiveExpr)
      ]

    wrappedSelfRecursiveExpr =
      EIf
        (ELit (LBool True))
        (ELambda (ident "x") (EApply (EVar (ident "f")) (EVar (ident "x"))))
        (ELit (LInt 0))

    hasWrappedLambdaBranch expr =
      case expr of
        EIf _ (ELambda _ _) _ -> True
        _ -> False

    isBareLambda expr =
      case expr of
        ELambda {} -> True
        _ -> False

ident :: Text -> Identifier
ident = mkIdentifier

span0 :: SourceSpan
span0 = SourceSpan 1 1
