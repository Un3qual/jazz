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
    assertContains,
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
    ("self-recursive binding is accepted", testSelfRecursiveBinding)
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
