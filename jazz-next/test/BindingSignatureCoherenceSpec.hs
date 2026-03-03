module Main (main) where

import JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertJust,
    assertSingleErrorContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "BindingSignatureCoherence" tests

tests :: [NamedTest]
tests =
  [ ("signature directly above matching binding is accepted", testSignatureDirectlyAboveBinding),
    ("signature separated from binding by expression is rejected", testSignatureSeparatedFromBinding),
    ("signature must match immediate binding name", testSignatureNameMismatch),
    ("use-before-definition is rejected", testUseBeforeDefinition),
    ("nested scope resolves outer bindings", testNestedScopeResolvesOuterBinding),
    ("self-recursive binding is accepted", testSelfRecursiveBinding),
    ("mutual recursion group is accepted", testMutualRecursionGroup),
    ("three-node mutual recursion group is accepted", testThreeNodeMutualRecursionGroup),
    ("non-recursive forward reference in bindings is rejected", testNonRecursiveForwardReference),
    ("rebinding cannot retroactively create recursion group", testRebindingDoesNotCreateRetroactiveRecursion)
  ]

testSignatureDirectlyAboveBinding :: IO ()
testSignatureDirectlyAboveBinding = do
  result <- compileExpr defaultWarningSettings validSignatureProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSignatureSeparatedFromBinding :: IO ()
testSignatureSeparatedFromBinding = do
  result <- compileExpr defaultWarningSettings separatedSignatureProgram
  assertSingleErrorContains
    "error text"
    "must be immediately followed by a matching binding"
    (compileErrors result)

testSignatureNameMismatch :: IO ()
testSignatureNameMismatch = do
  result <- compileExpr defaultWarningSettings mismatchedSignatureProgram
  assertSingleErrorContains
    "error text"
    "must annotate the next binding with the same name"
    (compileErrors result)

testUseBeforeDefinition :: IO ()
testUseBeforeDefinition = do
  result <- compileExpr defaultWarningSettings useBeforeDefinitionProgram
  assertSingleErrorContains
    "error text"
    "unbound variable 'x'"
    (compileErrors result)

testNestedScopeResolvesOuterBinding :: IO ()
testNestedScopeResolvesOuterBinding = do
  result <- compileExpr defaultWarningSettings nestedScopeProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

validSignatureProgram :: Expr
validSignatureProgram =
  EScope
    [ SSignature "x" (SourceSpan 1 1) "Int",
      SLet "x" (SourceSpan 2 1) (EInt 1),
      SExpr (EVar "x")
    ]

separatedSignatureProgram :: Expr
separatedSignatureProgram =
  EScope
    [ SSignature "x" (SourceSpan 1 1) "Int",
      SExpr (EInt 1),
      SLet "x" (SourceSpan 3 1) (EInt 2)
    ]

mismatchedSignatureProgram :: Expr
mismatchedSignatureProgram =
  EScope
    [ SSignature "x" (SourceSpan 1 1) "Int",
      SLet "y" (SourceSpan 2 1) (EInt 2)
    ]

useBeforeDefinitionProgram :: Expr
useBeforeDefinitionProgram =
  EScope
    [ SExpr (EVar "x"),
      SLet "x" (SourceSpan 2 1) (EInt 1)
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SExpr
        ( EScope
            [ SExpr (EVar "x")
            ]
        )
    ]

testSelfRecursiveBinding :: IO ()
testSelfRecursiveBinding = do
  result <- compileExpr defaultWarningSettings selfRecursiveProgram
  assertEqual "compile errors" [] (compileErrors result)

selfRecursiveProgram :: Expr
selfRecursiveProgram =
  EScope
    [ SLet "f" (SourceSpan 1 1) (EVar "f")
    ]

testMutualRecursionGroup :: IO ()
testMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings mutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testThreeNodeMutualRecursionGroup :: IO ()
testThreeNodeMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings threeNodeMutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testNonRecursiveForwardReference :: IO ()
testNonRecursiveForwardReference = do
  result <- compileExpr defaultWarningSettings nonRecursiveForwardReferenceProgram
  assertSingleErrorContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

testRebindingDoesNotCreateRetroactiveRecursion :: IO ()
testRebindingDoesNotCreateRetroactiveRecursion = do
  result <- compileExpr defaultWarningSettings retroactiveRebindingProgram
  assertSingleErrorContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

mutualRecursionProgram :: Expr
mutualRecursionProgram =
  EScope
    [ SLet "even" (SourceSpan 1 1) (EVar "odd"),
      SLet "odd" (SourceSpan 2 1) (EVar "even"),
      SExpr (EVar "even")
    ]

threeNodeMutualRecursionProgram :: Expr
threeNodeMutualRecursionProgram =
  EScope
    [ SLet "a" (SourceSpan 1 1) (EVar "b"),
      SLet "b" (SourceSpan 2 1) (EVar "c"),
      SLet "c" (SourceSpan 3 1) (EVar "a"),
      SExpr (EVar "a")
    ]

nonRecursiveForwardReferenceProgram :: Expr
nonRecursiveForwardReferenceProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (EInt 1),
      SExpr (EVar "x")
    ]

retroactiveRebindingProgram :: Expr
retroactiveRebindingProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (EInt 1),
      SLet "y" (SourceSpan 3 1) (EVar "x"),
      SExpr (EVar "x")
    ]
