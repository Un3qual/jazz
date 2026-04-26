{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
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
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
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
    ("source pipeline accepts concrete list signature", testSourceAcceptsConcreteListSignature),
    ("source pipeline accepts nested concrete list signature", testSourceAcceptsNestedConcreteListSignature),
    ("source pipeline accepts simple function signature", testSourceAcceptsSimpleFunctionSignature),
    ("source pipeline accepts list to list function signature", testSourceAcceptsListToListFunctionSignature),
    ("source pipeline accepts parenthesized function signature", testSourceAcceptsParenthesizedFunctionSignature),
    ("source pipeline accepts right-associated chained function signature", testSourceAcceptsChainedFunctionSignature),
    ("source pipeline accepts parenthesized function override signature", testSourceAcceptsParenthesizedFunctionOverrideSignature),
    ("source pipeline accepts list of parenthesized function types", testSourceAcceptsFunctionListSignature),
    ("source pipeline accepts empty constrained signature as monomorphic", testSourceAcceptsEmptyConstrainedSignature),
    ("source pipeline accepts concrete constrained signature as monomorphic", testSourceAcceptsConcreteConstrainedSignature),
    ("source pipeline rejects list signature mismatch", testSourceRejectsListSignatureMismatch),
    ("source pipeline rejects unsupported signature surface", testSourceRejectsUnsupportedSignatureSurface),
    ("source pipeline reports duplicate constrained signature constraints", testSourceRejectsDuplicateConstrainedSignatureConstraints),
    ("source pipeline rejects constrained signature surface with E2009", testSourceRejectsConstrainedSignatureSurface),
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
  assertSingleDiagnosticCode
    "signature type mismatch error"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature type mismatch subject"
    "x"
    (compileErrors result)

testSignatureSeparatedFromBinding :: IO ()
testSignatureSeparatedFromBinding = do
  result <- compileExpr defaultWarningSettings separatedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must be immediately followed by a matching binding"
    (compileErrors result)

testSignatureNameMismatch :: IO ()
testSignatureNameMismatch = do
  result <- compileExpr defaultWarningSettings mismatchedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must annotate the next binding with the same name"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature mismatch subject"
    "x"
    (compileErrors result)

testUseBeforeDefinition :: IO ()
testUseBeforeDefinition = do
  result <- compileExpr defaultWarningSettings useBeforeDefinitionProgram
  assertSingleDiagnosticContains
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
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

separatedSignatureProgram :: Expr
separatedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SExpr (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 3 1) (ELit (LInt 2))
    ]

mismatchedSignatureProgram :: Expr
mismatchedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 2))
    ]

useBeforeDefinitionProgram :: Expr
useBeforeDefinitionProgram =
  EBlock
    [ SExpr (SourceSpan 1 1) (EVar "x"),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1))
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr
        (SourceSpan 2 1)
        ( EBlock
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
  EBlock
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
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

testRebindingDoesNotCreateRetroactiveRecursion :: IO ()
testRebindingDoesNotCreateRetroactiveRecursion = do
  result <- compileExpr defaultWarningSettings retroactiveRebindingProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

mutualRecursionProgram :: Expr
mutualRecursionProgram =
  EBlock
    [ SLet "even" (SourceSpan 1 1) (EVar "odd"),
      SLet "odd" (SourceSpan 2 1) (EVar "even"),
      SExpr (SourceSpan 3 1) (EVar "even")
    ]

threeNodeMutualRecursionProgram :: Expr
threeNodeMutualRecursionProgram =
  EBlock
    [ SLet "a" (SourceSpan 1 1) (EVar "b"),
      SLet "b" (SourceSpan 2 1) (EVar "c"),
      SLet "c" (SourceSpan 3 1) (EVar "a"),
      SExpr (SourceSpan 4 1) (EVar "a")
    ]

nonRecursiveForwardReferenceProgram :: Expr
nonRecursiveForwardReferenceProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

retroactiveRebindingProgram :: Expr
retroactiveRebindingProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "y" (SourceSpan 3 1) (EVar "x"),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]

signatureTypeMismatchProgram :: Expr
signatureTypeMismatchProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LBool True))
    ]

assertSourceOk :: Text.Text -> IO ()
assertSourceOk src = do
  result <- compileSource defaultWarningSettings src
  assertEqual "compile errors" [] (compileErrors result)
  assertJust "generated JS is present" (generatedJs result)

assertSourceErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertContains "source error" needle (Text.unlines (map renderDiagnostic (compileErrors result)))

assertSourceSingleErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

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
testSourceRejectsSignatureTypeMismatch = do
  result <- compileSource defaultWarningSettings "x :: Int.\nx = True."
  assertSingleDiagnosticCode
    "source signature type mismatch code"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "source signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "source signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "source signature type mismatch subject"
    "x"
    (compileErrors result)

testSourceAcceptsConcreteListSignature :: IO ()
testSourceAcceptsConcreteListSignature =
  assertSourceOk "x :: [Int].\nx = [1]."

testSourceAcceptsNestedConcreteListSignature :: IO ()
testSourceAcceptsNestedConcreteListSignature =
  assertSourceOk "x :: [[Bool]].\nx = [[True], [False]]."

testSourceAcceptsSimpleFunctionSignature :: IO ()
testSourceAcceptsSimpleFunctionSignature =
  assertSourceOk "f :: Int -> Int.\nf = (+ 1)."

testSourceAcceptsListToListFunctionSignature :: IO ()
testSourceAcceptsListToListFunctionSignature =
  assertSourceOk "f :: [Int] -> [Int].\nf = filter (> 1)."

testSourceAcceptsParenthesizedFunctionSignature :: IO ()
testSourceAcceptsParenthesizedFunctionSignature =
  assertSourceOk "f :: ([Int]) -> ([Int]).\nf = filter (> 1)."

testSourceAcceptsChainedFunctionSignature :: IO ()
testSourceAcceptsChainedFunctionSignature =
  assertSourceOk "f :: Int -> Int -> Int.\nf = (+)."

testSourceAcceptsParenthesizedFunctionOverrideSignature :: IO ()
testSourceAcceptsParenthesizedFunctionOverrideSignature =
  assertSourceOk "applyToOne :: (Int -> Int) -> Int.\napplyToOne = \\(f) -> f 1."

testSourceAcceptsFunctionListSignature :: IO ()
testSourceAcceptsFunctionListSignature =
  assertSourceOk "fns :: [(Int -> Int)].\nfns = [(+ 1)]."

testSourceAcceptsEmptyConstrainedSignature :: IO ()
testSourceAcceptsEmptyConstrainedSignature =
  assertSourceOk "applyToOne :: @{}: (Int -> Int) -> Int.\napplyToOne = \\(f) -> f 1."

testSourceAcceptsConcreteConstrainedSignature :: IO ()
testSourceAcceptsConcreteConstrainedSignature =
  assertSourceOk "x :: @{Eq(Int)}: Int.\nx = 1."

testSourceRejectsListSignatureMismatch :: IO ()
testSourceRejectsListSignatureMismatch = do
  result <- compileSource defaultWarningSettings "x :: [Bool].\nx = [1]."
  assertSingleDiagnosticCode
    "source list signature mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsUnsupportedSignatureSurface :: IO ()
testSourceRejectsUnsupportedSignatureSurface =
  assertSourceSingleErrorContains "x :: [a].\nx = [1]." "E2009"

testSourceRejectsDuplicateConstrainedSignatureConstraints :: IO ()
testSourceRejectsDuplicateConstrainedSignatureConstraints = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a), Eq(a)}: a -> a.\nf = \\(x) -> x."
  assertSingleDiagnosticCode
    "source duplicate constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source duplicate constrained signature text"
    "duplicate constraint 'Eq'"
    (compileErrors result)

testSourceRejectsConstrainedSignatureSurface :: IO ()
testSourceRejectsConstrainedSignatureSurface = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a), Ord(b)}: a -> b -> c.\nf = 1."
  assertSingleDiagnosticCode
    "source constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source constrained signature payload"
    "@{Eq(a), Ord(b)}: a -> b -> c"
    (compileErrors result)

testSourceReportsSignedRecursiveRhsTypeError :: IO ()
testSourceReportsSignedRecursiveRhsTypeError =
  assertSourceSingleErrorContains "x :: Bool.\nx = x + 1." "E2003"

testSignatureMismatchKeepsDeclaredTypeDownstream :: IO ()
testSignatureMismatchKeepsDeclaredTypeDownstream =
  assertSourceSingleErrorContains "x :: Int.\nx = True.\ny = x + 1." "E2005"
