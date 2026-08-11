{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Jazz.Compiler.Name
  ( Name,
    mkIdentifier,
    operatorBindingName,
    sourceName
  )
import Jazz.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import Jazz.TestHarness
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
    ("ordinary binding initializers keep their own name free", testFreeVarsScopeKeepsOrdinaryInitializerNameFree),
    ("ordinary binding initializers resolve an outer same-name binding", testFreeVarsScopeResolvesOuterInitializerName),
    ("ordinary rebinding initializers resolve the nearest prior local binding", testFreeVarsScopeResolvesPriorLocalInitializerName),
    ("recursive groups keep singleton self-recursive bindings", testRecursiveGroupsKeepSingletonSelfRecursion),
    ("recursive groups keep top-level self-recursive lambdas", testRecursiveGroupsKeepTopLevelSelfRecursiveLambda),
    ("recursive groups keep top-level mutually recursive lambdas", testRecursiveGroupsKeepTopLevelMutualLambdas),
    ("recursive groups ignore same-name non-alias references", testRecursiveGroupsIgnoreSameNameNonAliasReference),
    ("recursive groups ignore mixed alias and eager self wrapper branches", testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper),
    ("recursive groups ignore eager block statements before alias terminal", testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal),
    ("recursive groups ignore eager self use before an unrelated callable result", testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult),
    ("recursive groups keep pattern binders from resolving through prior callables", testRecursiveGroupsRespectPatternBinderFunctionShadowing),
    ("recursive groups resolve aliases in their definition-site pattern scope", testRecursiveGroupsRespectAliasDefinitionPatternScope),
    ("recursive groups keep a callable pattern case with a guarded self-reference", testRecursiveGroupsKeepCallablePatternGuardSelfReference),
    ("recursive groups follow a block alias to the nearest prior callable rebinding", testRecursiveGroupsFollowPriorBlockCallableRebinding),
    ("recursive groups use the latest callable block rebinding", testRecursiveGroupsUseLatestBlockCallableRebinding),
    ("recursive groups let a scalar block rebinding hide a prior callable", testRecursiveGroupsPreferLatestScalarBlockRebinding),
    ("recursive groups ignore eager self operator use in a conditional", testRecursiveGroupsIgnoreEagerOperatorConditional),
    ("recursive groups retain alias-only operator self cycles", testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle),
    ("recursive groups suppress singleton self edge when outer binding exists", testRecursiveGroupsPreferOuterBindingForSingletonName),
    ("nested local self recursion stays local to the block", testFreeVarsScopeKeepsNestedSelfRecursionLocal),
    ("nested block SCC free vars stay local to the block", testFreeVarsScopeKeepsNestedRecursivePeersLocal),
    ("nested ordinary initializers retain enclosing mutual recursion", testRecursiveGroupsKeepNestedInitializerMutualRecursion),
    ("nested aliases resolve a nearest prior outer declaration", testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion),
    ("nested conditional aliases resolve a nearest prior outer declaration", testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion),
    ("nested operator aliases resolve a nearest prior outer declaration", testRecursiveGroupsKeepNestedPriorOuterOperatorAliasMutualRecursion),
    ("nested aliases without an outer declaration remain local self cycles", testRecursiveGroupsKeepNoOuterNestedAliasLocal),
    ("nested aliases do not resolve to the current enclosing declaration", testRecursiveGroupsKeepCurrentNestedAliasLocal),
    ("nested self-recursive lambdas stay out of enclosing SCCs", testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal),
    ("nested conditional self-recursive lambdas stay out of enclosing SCCs", testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal),
    ("recursive groups do not leak nested block SCC peers to outer scope", testRecursiveGroupsDoNotLeakNestedBlockPeers),
    ("recursive groups preserve declaration order through alias bridge", testRecursiveGroupsPreserveDeclarationOrder),
    ("recursive groups prefer nearest earlier rebinding over later declaration", testRecursiveGroupsPreferNearestEarlierRebinding),
    ("self-recursive binding detection is parameterized by caller predicate", testInferSelfRecursiveBindingsIsParameterized),
    ("self-recursive binding detection respects outer names", testInferSelfRecursiveBindingsRespectsOuterNames)
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
        (1, SSignature (ident "x") span0 (SignatureType TypeInt)),
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

testFreeVarsScopeKeepsOrdinaryInitializerNameFree :: IO ()
testFreeVarsScopeKeepsOrdinaryInitializerNameFree =
  assertEqual
    "scope free vars"
    (Set.fromList ["f", "g"])
    (freeVarsScopeWithBound Set.empty statements)
  where
    statements =
      [ SLet
          (ident "f")
          span0
          (EApply (EVar (ident "f")) (EVar (ident "g")))
      ]

testFreeVarsScopeResolvesOuterInitializerName :: IO ()
testFreeVarsScopeResolvesOuterInitializerName =
  assertEqual
    "outer same-name binding is not free"
    (Set.singleton "g")
    (freeVarsScopeWithBound (Set.singleton "f") statements)
  where
    statements =
      [ SLet
          (ident "f")
          span0
          (EApply (EVar (ident "f")) (EVar (ident "g")))
      ]

testFreeVarsScopeResolvesPriorLocalInitializerName :: IO ()
testFreeVarsScopeResolvesPriorLocalInitializerName =
  assertEqual
    "nearest prior local binding is not free"
    (Set.singleton "g")
    (freeVarsScopeWithBound Set.empty statements)
  where
    statements =
      [ SLet (ident "f") span0 (ELit (LInt 0)),
        SLet
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

testRecursiveGroupsKeepTopLevelSelfRecursiveLambda :: IO ()
testRecursiveGroupsKeepTopLevelSelfRecursiveLambda =
  assertEqual
    "top-level lambda self recursion"
    (Map.fromList [(1, [1])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ ( 1,
          SLet
            (ident "loop")
            span0
            (ELambda (ident "item") (EApply (EVar (ident "loop")) (EVar (ident "item"))))
        )
      ]

testRecursiveGroupsKeepTopLevelMutualLambdas :: IO ()
testRecursiveGroupsKeepTopLevelMutualLambdas =
  assertEqual
    "top-level lambda mutual recursion"
    (Map.fromList [(1, [1, 3]), (3, [1, 3])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ ( 1,
          SLet
            (ident "left")
            span0
            (ELambda (ident "item") (EApply (EVar (ident "right")) (EVar (ident "item"))))
        ),
        ( 3,
          SLet
            (ident "right")
            span0
            (ELambda (ident "item") (EApply (EVar (ident "left")) (EVar (ident "item"))))
        )
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

testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper :: IO ()
testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper =
  assertEqual
    "mixed alias and eager self wrapper does not create self edge"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 mixedWrapperExpr)
      ]
    mixedWrapperExpr =
      EIf
        (ELit (LBool True))
        (EBinary "+" (EVar (ident "f")) (ELit (LInt 1)))
        (EVar (ident "f"))

testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal :: IO ()
testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal =
  assertEqual
    "eager block statement before alias terminal does not create self edge"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 blockExpr)
      ]
    blockExpr =
      EBlock
        [ SExpr span0 (EBinary "+" (EVar (ident "f")) (ELit (LInt 1))),
          SExpr span0 (EVar (ident "f"))
        ]

testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult :: IO ()
testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult =
  assertEqual
    "eager self use is not owned by an unrelated callable result"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ (0, SLet (ident "f") span0 blockExpr)
      ]
    blockExpr =
      EBlock
        [ SExpr span0 (EApply (EVar (ident "f")) (ELit (LBool True))),
          SExpr span0 (ELambda (ident "x") (EVar (ident "x")))
        ]

testRecursiveGroupsRespectPatternBinderFunctionShadowing :: IO ()
testRecursiveGroupsRespectPatternBinderFunctionShadowing =
  assertEqual
    "pattern-bound scalar does not manufacture a recursive function owner"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty [(0, SLet functionName span0 functionBody)])
  where
    functionName = ident "f"
    apparentName = ident "apparent"
    capturedName = ident "captured"
    functionBody =
      EBlock
        [ SLet apparentName span0 (ELambda (ident "x") (EVar (ident "x"))),
          SLet capturedName span0 (ELambda (ident "x") (EVar functionName)),
          SExpr
            span0
            ( EPatternCase
                (ELit (LBool True))
                [CaseArm (PVariable apparentName) Nothing (EVar apparentName)]
            )
        ]

testRecursiveGroupsRespectAliasDefinitionPatternScope :: IO ()
testRecursiveGroupsRespectAliasDefinitionPatternScope =
  assertEqual
    "pattern-bound use site does not hide an alias initializer's prior callable"
    (Map.fromList [(0, [0])])
    (inferRecursiveGroupsOrdered Set.empty [(0, SLet functionName span0 functionBody)])
  where
    functionName = ident "f"
    targetName = ident "target"
    aliasName = ident "alias"
    functionBody =
      EBlock
        [ SLet targetName span0 (ELambda (ident "x") (EVar functionName)),
          SLet aliasName span0 (EVar targetName),
          SExpr
            span0
            ( EPatternCase
                (ELit (LBool True))
                [CaseArm (PVariable targetName) Nothing (EVar aliasName)]
            )
        ]

testRecursiveGroupsKeepCallablePatternGuardSelfReference :: IO ()
testRecursiveGroupsKeepCallablePatternGuardSelfReference =
  assertEqual
    "callable pattern-case guard owns its self-reference"
    (Map.fromList [(0, [0])])
    (inferRecursiveGroupsOrdered Set.empty [(0, SLet functionName span0 patternCaseExpr)])
  where
    functionName = ident "f"
    identityLambda = ELambda (ident "x") (EVar (ident "x"))
    patternCaseExpr =
      EPatternCase
        (ELit (LInt 1))
        [ CaseArm
            (PLiteral (LInt 1))
            ( Just
                ( EBinary
                    "=="
                    (EApply (EVar functionName) (ELit (LInt 0)))
                    (ELit (LInt 0))
                )
            )
            identityLambda,
          CaseArm PWildcard Nothing identityLambda
        ]

testRecursiveGroupsFollowPriorBlockCallableRebinding :: IO ()
testRecursiveGroupsFollowPriorBlockCallableRebinding =
  assertEqual
    "same-name block alias follows the nearest prior callable declaration"
    (Map.fromList [(0, [0])])
    (inferRecursiveGroupsOrdered Set.empty (outerBlockStatements leadingStatements))
  where
    leadingStatements =
      [ SLet (ident "inner") span0 innerCallable,
        SLet (ident "inner") span0 (EVar (ident "inner"))
      ]

testRecursiveGroupsUseLatestBlockCallableRebinding :: IO ()
testRecursiveGroupsUseLatestBlockCallableRebinding =
  assertEqual
    "terminal block name uses the latest callable declaration"
    (Map.fromList [(0, [0])])
    (inferRecursiveGroupsOrdered Set.empty (outerBlockStatements leadingStatements))
  where
    leadingStatements =
      [ SLet (ident "inner") span0 (ELit (LBool True)),
        SLet (ident "inner") span0 innerCallable
      ]

testRecursiveGroupsPreferLatestScalarBlockRebinding :: IO ()
testRecursiveGroupsPreferLatestScalarBlockRebinding =
  assertEqual
    "terminal block name does not reach through the latest scalar declaration"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty (outerBlockStatements leadingStatements))
  where
    leadingStatements =
      [ SLet (ident "inner") span0 innerCallable,
        SLet (ident "inner") span0 (ELit (LBool True))
      ]

outerBlockStatements :: [Statement] -> [(Int, Statement)]
outerBlockStatements leadingStatements =
  [ ( 0,
      SLet
        (ident "f")
        span0
        (EBlock (leadingStatements <> [SExpr span0 (EVar (ident "inner"))]))
    )
  ]

innerCallable :: Expr
innerCallable =
  ELambda
    (ident "x")
    (EApply (EVar (ident "f")) (EVar (ident "x")))

testRecursiveGroupsIgnoreEagerOperatorConditional :: IO ()
testRecursiveGroupsIgnoreEagerOperatorConditional =
  assertEqual
    "eager operator condition prevents alias-only self ownership"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty [(0, SLet operatorName span0 conditionalExpr)])
  where
    operatorName = operatorBindingName "%%"
    conditionalExpr =
      EIf
        (EBinary "%%" (ELit (LBool True)) (ELit (LBool False)))
        (EOperatorValue "%%")
        (EOperatorValue "%%")

testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle :: IO ()
testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle =
  assertEqual
    "operator value alias retains self ownership"
    (Map.fromList [(0, [0])])
    ( inferRecursiveGroupsOrdered
        Set.empty
        [(0, SLet (operatorBindingName "%%") span0 (EOperatorValue "%%"))]
    )

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

testFreeVarsScopeKeepsNestedSelfRecursionLocal :: IO ()
testFreeVarsScopeKeepsNestedSelfRecursionLocal =
  assertEqual
    "nested self-recursive name stays local to block"
    Set.empty
    (freeVarsScopeWithBound Set.empty nestedStatements)
  where
    nestedStatements =
      [ SLet (ident "loop") span0 (EVar (ident "loop")),
        SExpr span0 (EVar (ident "loop"))
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

testRecursiveGroupsKeepNestedInitializerMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedInitializerMutualRecursion =
  assertEqual
    "nested ordinary initializers preserve enclosing owner edges"
    (Map.fromList [(1, [1, 3]), (3, [1, 3])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ ( 1,
          SLet
            (ident "left")
            span0
            ( ELambda
                (ident "item")
                ( EBlock
                    [ SLet
                        (ident "right")
                        span0
                        (EApply (EVar (ident "right")) (EVar (ident "item"))),
                      SExpr span0 (EVar (ident "item"))
                    ]
                )
            )
        ),
        ( 3,
          SLet
            (ident "right")
            span0
            ( ELambda
                (ident "item")
                ( EBlock
                    [ SLet
                        (ident "left")
                        span0
                        (EApply (EVar (ident "left")) (EVar (ident "item"))),
                      SExpr span0 (EVar (ident "item"))
                    ]
                )
            )
        )
      ]

testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion =
  assertEqual
    "nested alias preserves prior outer mutual edge"
    (Map.fromList [(1, [1, 3]), (3, [1, 3])])
    (inferRecursiveGroupsOrdered Set.empty (nestedPriorOuterMutualStatements (EVar (ident "left"))))

testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion =
  assertEqual
    "nested conditional alias preserves prior outer mutual edge"
    (Map.fromList [(1, [1, 3]), (3, [1, 3])])
    ( inferRecursiveGroupsOrdered
        Set.empty
        ( nestedPriorOuterMutualStatements
            (EIf (ELit (LBool True)) (EVar (ident "left")) (EVar (ident "left")))
        )
    )

testRecursiveGroupsKeepNestedPriorOuterOperatorAliasMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedPriorOuterOperatorAliasMutualRecursion =
  assertEqual
    "nested operator alias preserves prior outer mutual edge"
    (Map.fromList [(1, [1, 3]), (3, [1, 3])])
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    operatorName = operatorBindingName "%%"
    indexedStatements =
      [ (1, SLet operatorName span0 (EVar (ident "peer"))),
        ( 3,
          SLet
            (ident "peer")
            span0
            ( EBlock
                [ SLet operatorName span0 (EOperatorValue "%%"),
                  SExpr span0 (ELit (LInt 0))
                ]
            )
        )
      ]

testRecursiveGroupsKeepNoOuterNestedAliasLocal :: IO ()
testRecursiveGroupsKeepNoOuterNestedAliasLocal =
  assertEqual
    "nested alias keeps a local self cycle instead of resolving to a future outer peer"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ ( 1,
          SLet
            (ident "owner")
            span0
            ( EBlock
                [ SLet (ident "local") span0 (EVar (ident "local")),
                  SExpr span0 (ELit (LInt 0))
                ]
            )
        ),
        (3, SLet (ident "local") span0 (EVar (ident "owner")))
      ]

testRecursiveGroupsKeepCurrentNestedAliasLocal :: IO ()
testRecursiveGroupsKeepCurrentNestedAliasLocal =
  assertEqual
    "nested alias does not manufacture an enclosing self edge"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      [ ( 1,
          SLet
            (ident "owner")
            span0
            ( EBlock
                [ SLet (ident "owner") span0 (EVar (ident "owner")),
                  SExpr span0 (ELit (LInt 0))
                ]
            )
        )
      ]

testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal :: IO ()
testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal =
  assertEqual
    "nested self-recursive lambda does not form an enclosing mutual SCC"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty nestedSelfRecursiveLambdaStatements)

testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal :: IO ()
testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal =
  assertEqual
    "nested conditional self-recursive lambda does not form an enclosing mutual SCC"
    Map.empty
    (inferRecursiveGroupsOrdered Set.empty indexedStatements)
  where
    indexedStatements =
      nestedSelfRecursiveLambdaStatementsWith
        ( EIf
            (ELit (LBool True))
            (ELambda (ident "nested") (EApply (EVar (ident "loop")) (EVar (ident "nested"))))
            (ELambda (ident "nested") (EVar (ident "nested")))
        )

nestedSelfRecursiveLambdaStatements :: [(Int, Statement)]
nestedSelfRecursiveLambdaStatements =
  nestedSelfRecursiveLambdaStatementsWith
    (ELambda (ident "nested") (EApply (EVar (ident "loop")) (EVar (ident "nested"))))

nestedSelfRecursiveLambdaStatementsWith :: Expr -> [(Int, Statement)]
nestedSelfRecursiveLambdaStatementsWith localLoopExpr =
  [ ( 1,
      SLet
        (ident "owner")
        span0
        ( ELambda
            (ident "item")
            ( EBlock
                [ SLet (ident "loop") span0 localLoopExpr,
                  SExpr span0 (EVar (ident "item"))
                ]
            )
        )
    ),
    ( 3,
      SLet
        (ident "loop")
        span0
        (ELambda (ident "item") (EApply (EVar (ident "owner")) (EVar (ident "item"))))
    )
  ]

nestedPriorOuterMutualStatements :: Expr -> [(Int, Statement)]
nestedPriorOuterMutualStatements nestedAliasExpr =
  [ ( 1,
      SLet
        (ident "left")
        span0
        (ELambda (ident "item") (EApply (EVar (ident "right")) (EVar (ident "item"))))
    ),
    ( 3,
      SLet
        (ident "right")
        span0
        ( ELambda
            (ident "item")
            ( EBlock
                [ SLet (ident "left") span0 nestedAliasExpr,
                  SExpr span0 (EVar (ident "item"))
                ]
            )
        )
    )
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
    (inferSelfRecursiveBindings Set.empty hasWrappedLambdaBranch indexedStatements)
  assertEqual
    "bare lambda policy does not mark wrapped self recursion"
    Set.empty
    (inferSelfRecursiveBindings Set.empty isBareLambda indexedStatements)
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

testInferSelfRecursiveBindingsRespectsOuterNames :: IO ()
testInferSelfRecursiveBindingsRespectsOuterNames =
  assertEqual
    "an outer builtin-like name suppresses a self-recursive function cell"
    Set.empty
    (inferSelfRecursiveBindings (Set.singleton (ident "map")) isLambda indexedStatements)
  where
    indexedStatements =
      [ ( 0,
          SLet
            (ident "map")
            span0
            (ELambda (ident "items") (EApply (EVar (ident "map")) (EVar (ident "items"))))
        )
      ]

    isLambda expr =
      case expr of
        ELambda {} -> True
        _ -> False

ident :: Text -> Name
ident = sourceName . mkIdentifier

span0 :: SourceSpan
span0 = SourceSpan 1 1
