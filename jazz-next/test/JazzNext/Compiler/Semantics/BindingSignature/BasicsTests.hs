{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.BasicsTests
  ( basicTests
  ) where

import qualified Data.Set as Set
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (..),
    TypeScheme (..),
    emptyScopeCapabilityFacts
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileExpr,
    compileSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains
  )
import JazzNext.Compiler.Semantics.BindingSignature.Shared

basicTests :: [NamedTest]
basicTests =
  [ ("type scheme record preserves fields", testTypeSchemeRecordPreservesFields)
    , ("signature directly above matching binding is accepted", testSignatureDirectlyAboveBinding)
    , ("nested scope resolves outer bindings", testNestedScopeResolvesOuterBinding)
    , ("source pipeline accepts adjacent signature and binding", testSourceAcceptsSignatureAdjacency)
    , ("source pipeline accepts Char and Text signatures", testSourceAcceptsCharTextSignatures)
    , ("source pipeline preserves numeric defaults through final solver phase", testSourcePreservesNumericDefaultsThroughFinalSolverPhase)
    , ("source pipeline preserves Float alias hint across numeric operator dispatch", testSourcePreservesFloatAliasHintAcrossNumericOperatorDispatch)
    , ("source pipeline uses binding signatures to contextualize RHS lambdas", testSourceUsesBindingSignaturesToContextualizeRhsLambdas)
    , ("compiler keeps nested capability facts scoped", testSourceKeepsNestedCapabilityFactsScoped)
    , ("compiler hides alias-only imported capability facts in signatures", testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures)
    , ("source pipeline accepts concrete list signature", testSourceAcceptsConcreteListSignature)
    , ("source pipeline accepts nested concrete list signature", testSourceAcceptsNestedConcreteListSignature)
    , ("source pipeline accepts concrete tuple signature", testSourceAcceptsConcreteTupleSignature)
    , ("source pipeline accepts width-specific integer signatures", testSourceAcceptsWidthSpecificIntegerSignatures)
    , ("source pipeline accepts same-width integral operator signatures", testSourceAcceptsSameWidthIntegralOperatorSignatures)
    , ("source pipeline accepts same-width float numeric operator signatures", testSourceAcceptsSameWidthFloatNumericOperatorSignatures)
    , ("source pipeline accepts float fractional literal signatures", testSourceAcceptsFloatFractionalLiteralSignatures)
    , ("source pipeline accepts list to list function signature", testSourceAcceptsListToListFunctionSignature)
    , ("source pipeline accepts parenthesized function signature", testSourceAcceptsParenthesizedFunctionSignature)
    , ("source pipeline accepts right-associated chained function signature", testSourceAcceptsChainedFunctionSignature)
    , ("source pipeline accepts parenthesized function override signature", testSourceAcceptsParenthesizedFunctionOverrideSignature)
    , ("source pipeline accepts list of parenthesized function types", testSourceAcceptsFunctionListSignature)
    , ("source pipeline accepts empty constrained signature as monomorphic", testSourceAcceptsEmptyConstrainedSignature)
    , ("source pipeline accepts empty constrained tuple signature as monomorphic", testSourceAcceptsEmptyConstrainedTupleSignature)
    , ("source pipeline accepts concrete constrained signature as monomorphic", testSourceAcceptsConcreteConstrainedSignature)
    , ("source pipeline accepts bundled concrete constrained signature facts", testSourceAcceptsBundledConcreteConstrainedSignatureFacts)
    , ("source pipeline accepts bundled width-specific numeric constrained signature facts", testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts)
    , ("source pipeline accepts additional concrete constrained signatures", testSourceAcceptsAdditionalConcreteConstrainedSignatures)
    , ("source pipeline accepts concrete tuple constrained signature argument", testSourceAcceptsConcreteTupleConstrainedSignatureArgument)
    , ("source pipeline accepts ADT application constrained signature argument", testSourceAcceptsAdtApplicationConstrainedSignatureArgument)
    , ("source pipeline accepts variable constrained signature as monomorphic", testSourceAcceptsVariableConstrainedSignatureAsMonomorphic)
    , ("source pipeline honors visible facts for variable constrained signatures", testSourceHonorsVisibleFactsForVariableConstrainedSignatures)
    , ("source pipeline keeps generic constructor aliases monomorphic", testSourceKeepsGenericConstructorAliasesMonomorphic)
  ]

testTypeSchemeRecordPreservesFields :: IO ()
testTypeSchemeRecordPreservesFields =
  assertEqual
    "scheme result"
    (TFunctionType (TVarType 0) (TVarType 0))
    (schemeResultType scheme)
  where
    scheme =
      TypeScheme
        { schemeQuantifiedVariables = Set.singleton 0,
          schemeQuantifiedOrder = [0],
          schemeClassConstraints = [],
          schemePrimitiveConstraints = [],
          schemeDefiningCapabilities = emptyScopeCapabilityFacts,
          schemeResultType = TFunctionType (TVarType 0) (TVarType 0)
        }

testSignatureDirectlyAboveBinding :: IO ()
testSignatureDirectlyAboveBinding = do
  result <- compileExpr defaultWarningSettings validSignatureProgram
  assertEqual "compile errors" [] (compileErrors result)

testNestedScopeResolvesOuterBinding :: IO ()
testNestedScopeResolvesOuterBinding = do
  result <- compileExpr defaultWarningSettings nestedScopeProgram
  assertEqual "compile errors" [] (compileErrors result)

testSourceAcceptsSignatureAdjacency :: IO ()
testSourceAcceptsSignatureAdjacency =
  assertSourceOk "x :: Int.\nx = 1.\nx."

testSourceAcceptsCharTextSignatures :: IO ()
testSourceAcceptsCharTextSignatures =
  assertSourceOk
    "character :: Char.\ncharacter = 'a'.\nmessage :: Text.\nmessage = \"Jazz\".\n(message, character)."

testSourcePreservesNumericDefaultsThroughFinalSolverPhase :: IO ()
testSourcePreservesNumericDefaultsThroughFinalSolverPhase =
  assertSourceOk "numeric = \\(x) -> x + 1.\nresult = numeric 2.\nresult."

testSourcePreservesFloatAliasHintAcrossNumericOperatorDispatch :: IO ()
testSourcePreservesFloatAliasHintAcrossNumericOperatorDispatch = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
          <> "impl RuntimeFlag(Float) {\nflag = \\(value) -> True.\n}.\n"
          <> "impl RuntimeFlag(Float64) {\nflag = \\(value) -> False.\n}.\n"
          <> "left :: Float.\nleft = 1.5.\n"
          <> "right :: Float.\nright = 2.25.\n"
          <> "result :: Bool.\nresult = RuntimeFlag::flag (left + right).\nresult."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testSourceUsesBindingSignaturesToContextualizeRhsLambdas :: IO ()
testSourceUsesBindingSignaturesToContextualizeRhsLambdas =
  assertSourceOkWithoutPrelude
    ( "class D(a) {\nn :: a -> Bool.\n}.\n"
        <> "impl D(Int) {\nn = \\(value) -> True.\n}.\n"
        <> "impl D(Bool) {\nn = \\(value) -> False.\n}.\n"
        <> "f :: Int -> Bool.\n"
        <> "f = \\(x) -> D::n x.\n"
        <> "result :: Bool.\nresult = f 1.\nresult."
    )

testSourceKeepsNestedCapabilityFactsScoped :: IO ()
testSourceKeepsNestedCapabilityFactsScoped = do
  result <- compileExpr defaultWarningSettings program
  assertSingleDiagnosticContains
    "nested capability fact isolation"
    "missing class declaration 'Eq'"
    (compileErrors result)
  where
    spanValue = SourceSpan 1 1
    eqInt = TypeInt
    program =
      EBlock
        [ SLet
            "seed"
            spanValue
            ( EBlock
                [ SClass spanValue "Eq" ["a"] [],
                  SImpl spanValue "Eq" [eqInt] [],
                  SExpr spanValue (ELit (LInt 0))
                ]
            ),
          SSignature "x" spanValue (ConstrainedSignature [SignatureConstraint "Eq" [eqInt]] eqInt),
          SLet "x" spanValue (ELit (LInt 1))
        ]

testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures :: IO ()
testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures = do
  result <- compileExpr defaultWarningSettings aliasOnlyImportedCapabilityFactsProgram
  assertSingleDiagnosticContains
    "alias-only capability fact isolation"
    "missing class declaration 'RemoteEq'"
    (compileErrors result)

testSourceAcceptsConcreteListSignature :: IO ()
testSourceAcceptsConcreteListSignature =
  assertSourceOk "x :: [Int].\nx = [1]."

testSourceAcceptsNestedConcreteListSignature :: IO ()
testSourceAcceptsNestedConcreteListSignature =
  assertSourceOk "x :: [[Bool]].\nx = [[True], [False]]."

testSourceAcceptsConcreteTupleSignature :: IO ()
testSourceAcceptsConcreteTupleSignature =
  assertSourceOk "pair :: (Int, Bool).\npair = (1, True).\npair."

testSourceAcceptsWidthSpecificIntegerSignatures :: IO ()
testSourceAcceptsWidthSpecificIntegerSignatures = do
  assertSourceOk "x :: Int8.\nx = 1."
  assertSourceOk "x :: Int8.\nx = 127."
  assertSourceOk "x :: UInt8.\nx = 255."
  assertSourceOk "x :: UInt64.\nx = 1."
  assertSourceOk "x :: UInt64.\nx = 18446744073709551615."
  assertSourceOk "xs :: [Int32].\nxs = [1, 2, 3]."
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(UInt16) { }.\nx :: @{Num(UInt16)}: UInt16.\nx = 1."

testSourceAcceptsSameWidthIntegralOperatorSignatures :: IO ()
testSourceAcceptsSameWidthIntegralOperatorSignatures = do
  assertSourceOk "add :: Int8 -> Int8 -> Int8.\nadd = (+)."
  assertSourceOk "lt :: UInt32 -> UInt32 -> Bool.\nlt = (<)."

testSourceAcceptsSameWidthFloatNumericOperatorSignatures :: IO ()
testSourceAcceptsSameWidthFloatNumericOperatorSignatures = do
  assertSourceOk "fadd :: Float -> Float -> Float.\nfadd = (+)."
  assertSourceOk "fadd64 :: Float64 -> Float64 -> Float64.\nfadd64 = (+)."

testSourceAcceptsFloatFractionalLiteralSignatures :: IO ()
testSourceAcceptsFloatFractionalLiteralSignatures = do
  assertSourceOk "x :: Float.\nx = 1.5."
  assertSourceOk "x :: Float16.\nx = 1.5."
  assertSourceOk "x :: Float32.\nx = 1.5."
  assertSourceOk "x :: Float64.\nx = 1.5."
  assertSourceOk "xs :: [Float64].\nxs = [1.5, 2.25]."

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

testSourceAcceptsEmptyConstrainedTupleSignature :: IO ()
testSourceAcceptsEmptyConstrainedTupleSignature =
  assertSourceOk "pair :: @{}: (Int, Bool).\npair = (1, True).\npair."

testSourceAcceptsConcreteConstrainedSignature :: IO ()
testSourceAcceptsConcreteConstrainedSignature =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nx :: @{Eq(Int)}: Int.\nx = 1."

testSourceAcceptsBundledConcreteConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledConcreteConstrainedSignatureFacts =
  assertSourceOk "x :: @{Eq(Int)}: Int.\nx = 1."

testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts = do
  assertSourceOk "x :: @{Num(UInt16)}: UInt16.\nx = 1."
  assertSourceOk "x :: @{Integral(Int32)}: Int32.\nx = 1."
  assertSourceOk "x :: @{Fractional(Float32)}: Float32.\nx = toFloat32 1."
  assertSourceOk "x :: @{Showable(Float64)}: Float64.\nx = toFloat64 1."

testSourceAcceptsAdditionalConcreteConstrainedSignatures :: IO ()
testSourceAcceptsAdditionalConcreteConstrainedSignatures = do
  assertSourceOkWithoutPrelude "class Default(a) { }.\nimpl Default(Bool) { }.\nx :: @{Default(Bool)}: Bool.\nx = True."
  assertSourceOkWithoutPrelude "class Fractional(a) { }.\nimpl Fractional(Int) { }.\nx :: @{Fractional(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Integral(a) { }.\nimpl Integral(Int) { }.\nx :: @{Integral(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(Int) { }.\nx :: @{Num(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Ord(a) { }.\nimpl Ord(Int) { }.\nx :: @{Ord(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Showable(a) { }.\nimpl Showable([[Bool]]) { }.\nx :: @{Showable([[Bool]])}: [[Bool]].\nx = [[True], [False]]."

testSourceAcceptsConcreteTupleConstrainedSignatureArgument :: IO ()
testSourceAcceptsConcreteTupleConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq((Int, Bool)) { }.\npair :: @{Eq((Int, Bool))}: (Int, Bool).\npair = (1, True)."

testSourceAcceptsAdtApplicationConstrainedSignatureArgument :: IO ()
testSourceAcceptsAdtApplicationConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.\nx :: @{Eq(Box(Int))}: Int.\nx = 1."

testSourceAcceptsVariableConstrainedSignatureAsMonomorphic :: IO ()
testSourceAcceptsVariableConstrainedSignatureAsMonomorphic =
  assertSourceOk "id :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nid 1."

testSourceHonorsVisibleFactsForVariableConstrainedSignatures :: IO ()
testSourceHonorsVisibleFactsForVariableConstrainedSignatures =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nx = id 1.\ny = id True."

testSourceKeepsGenericConstructorAliasesMonomorphic :: IO ()
testSourceKeepsGenericConstructorAliasesMonomorphic = do
  result <- compileSource defaultWarningSettings "data Box a = Box a. make = Box. first = make 1. second = make True."
  assertSingleDiagnosticCode
    "generic constructor alias monomorphic code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "generic constructor alias monomorphic text"
    "cannot apply function of type Int -> Box"
    (compileErrors result)
