{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource
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
    ("signature type mismatch is rejected", testSignatureTypeMismatch),
    ("signature separated from binding by expression is rejected", testSignatureSeparatedFromBinding),
    ("signature must match immediate binding name", testSignatureNameMismatch),
    ("use-before-definition is rejected", testUseBeforeDefinition),
    ("nested scope resolves outer bindings", testNestedScopeResolvesOuterBinding),
    ("self-recursive binding is accepted", testSelfRecursiveBinding),
    ("mutual recursion group is accepted", testMutualRecursionGroup),
    ("three-node mutual recursion group is accepted", testThreeNodeMutualRecursionGroup),
    ("non-recursive forward reference in bindings is rejected", testNonRecursiveForwardReference),
    ("rebinding cannot retroactively create recursion group", testRebindingDoesNotCreateRetroactiveRecursion),
    ("source pipeline accepts adjacent signature and binding", testSourceAcceptsSignatureAdjacency),
    ("source pipeline rejects separated signature", testSourceRejectsSeparatedSignature),
    ("source pipeline rejects signature name mismatch", testSourceRejectsSignatureNameMismatch),
    ("source pipeline rejects non-recursive forward reference", testSourceRejectsNonRecursiveForwardReference),
    ("source pipeline rejects retroactive rebinding recursion", testSourceRejectsRetroactiveRebindingRecursion),
    ("source pipeline accepts mutual recursion group", testSourceAcceptsMutualRecursionGroup),
    ("source pipeline rejects signature type mismatch", testSourceRejectsSignatureTypeMismatch),
    ("source pipeline rejects unsupported signature surface", testSourceRejectsUnsupportedSignatureSurface),
    ("source pipeline reports signed recursive rhs type errors", testSourceReportsSignedRecursiveRhsTypeError),
    ("signature mismatch keeps declared type for downstream checks", testSignatureMismatchKeepsDeclaredTypeDownstream)
  ]

testSignatureDirectlyAboveBinding :: IO ()
testSignatureDirectlyAboveBinding = do
  result <- compileExpr defaultWarningSettings validSignatureProgram
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

testSignatureTypeMismatch :: IO ()
testSignatureTypeMismatch = do
  result <- compileExpr defaultWarningSettings signatureTypeMismatchProgram
  assertSingleErrorContains
    "signature type mismatch error"
    "E2005"
    (compileErrors result)

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
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

separatedSignatureProgram :: Expr
separatedSignatureProgram =
  EScope
    [ SSignature "x" (SourceSpan 1 1) "Int",
      SExpr (SourceSpan 2 1) (EInt 1),
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
    [ SExpr (SourceSpan 1 1) (EVar "x"),
      SLet "x" (SourceSpan 2 1) (EInt 1)
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SExpr
        (SourceSpan 2 1)
        ( EScope
            [ SExpr (SourceSpan 3 1) (EVar "x")
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
      SExpr (SourceSpan 3 1) (EVar "even")
    ]

threeNodeMutualRecursionProgram :: Expr
threeNodeMutualRecursionProgram =
  EScope
    [ SLet "a" (SourceSpan 1 1) (EVar "b"),
      SLet "b" (SourceSpan 2 1) (EVar "c"),
      SLet "c" (SourceSpan 3 1) (EVar "a"),
      SExpr (SourceSpan 4 1) (EVar "a")
    ]

nonRecursiveForwardReferenceProgram :: Expr
nonRecursiveForwardReferenceProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (EInt 1),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

retroactiveRebindingProgram :: Expr
retroactiveRebindingProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (EInt 1),
      SLet "y" (SourceSpan 3 1) (EVar "x"),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]

signatureTypeMismatchProgram :: Expr
signatureTypeMismatchProgram =
  EScope
    [ SSignature "x" (SourceSpan 1 1) "Int",
      SLet "x" (SourceSpan 2 1) (EBool True)
    ]

assertSourceOk :: Text.Text -> IO ()
assertSourceOk src = do
  result <- compileSource defaultWarningSettings src
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

assertSourceErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertContains "source error" needle (Text.unlines (compileErrors result))

assertSourceSingleErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertSingleErrorContains "source error" needle (compileErrors result)

testSourceAcceptsSignatureAdjacency :: IO ()
testSourceAcceptsSignatureAdjacency =
  assertSourceOk "x :: Int.\nx = 1.\nx."

testSourceRejectsSeparatedSignature :: IO ()
testSourceRejectsSeparatedSignature =
  assertSourceErrorContains "x :: Int.\n1.\nx = 2." "E1002"

testSourceRejectsSignatureNameMismatch :: IO ()
testSourceRejectsSignatureNameMismatch =
  assertSourceErrorContains "x :: Int.\ny = 2." "E1003"

testSourceRejectsNonRecursiveForwardReference :: IO ()
testSourceRejectsNonRecursiveForwardReference =
  assertSourceErrorContains "x = y.\ny = 1.\nx." "E1001"

testSourceRejectsRetroactiveRebindingRecursion :: IO ()
testSourceRejectsRetroactiveRebindingRecursion =
  assertSourceErrorContains "x = y.\ny = 1.\ny = x.\nx." "E1001"

testSourceAcceptsMutualRecursionGroup :: IO ()
testSourceAcceptsMutualRecursionGroup =
  assertSourceOk "even = odd.\nodd = even.\neven."

testSourceRejectsSignatureTypeMismatch :: IO ()
testSourceRejectsSignatureTypeMismatch =
  assertSourceSingleErrorContains "x :: Int.\nx = True." "E2005"

testSourceRejectsUnsupportedSignatureSurface :: IO ()
testSourceRejectsUnsupportedSignatureSurface =
  assertSourceSingleErrorContains "x :: [Int].\nx = [1]." "E2009"

testSourceReportsSignedRecursiveRhsTypeError :: IO ()
testSourceReportsSignedRecursiveRhsTypeError =
  assertSourceSingleErrorContains "x :: Bool.\nx = x + 1." "E2003"

testSignatureMismatchKeepsDeclaredTypeDownstream :: IO ()
testSignatureMismatchKeepsDeclaredTypeDownstream =
  assertSourceSingleErrorContains "x :: Int.\nx = True.\ny = x + 1." "E2005"
