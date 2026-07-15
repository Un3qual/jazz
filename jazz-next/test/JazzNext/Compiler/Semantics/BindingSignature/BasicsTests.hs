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
  ( RunResult (..),
    compileErrors,
    compileExpr,
    compileSource,
    runCompileErrors,
    runRuntimeErrors,
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
  assertSourceOk """
  x :: Int.
  x = 1.
  x.
  """

testSourceAcceptsCharTextSignatures :: IO ()
testSourceAcceptsCharTextSignatures =
  assertSourceOk
    """
    character :: Char.
    character = 'a'.
    message :: Text.
    message = \"Jazz\".
    (message, character).
    """

testSourcePreservesNumericDefaultsThroughFinalSolverPhase :: IO ()
testSourcePreservesNumericDefaultsThroughFinalSolverPhase =
  assertSourceOk """
  numeric = \\(x) -> x + 1.
  result = numeric 2.
  result.
  """

testSourcePreservesFloatAliasHintAcrossNumericOperatorDispatch :: IO ()
testSourcePreservesFloatAliasHintAcrossNumericOperatorDispatch = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( """
      class RuntimeFlag(a) {
      flag :: a -> Bool.
      }.
      impl RuntimeFlag(Float) {
      flag = \\(value) -> True.
      }.
      impl RuntimeFlag(Float64) {
      flag = \\(value) -> False.
      }.
      left :: Float.
      left = 1.5.
      right :: Float.
      right = 2.25.
      result :: Bool.
      result = RuntimeFlag::flag (left + right).
      result.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "True") (runOutput result)

testSourceUsesBindingSignaturesToContextualizeRhsLambdas :: IO ()
testSourceUsesBindingSignaturesToContextualizeRhsLambdas =
  assertSourceOkWithoutPrelude
    ( """
    class D(a) {
    n :: a -> Bool.
    }.
    impl D(Int) {
    n = \\(value) -> True.
    }.
    impl D(Bool) {
    n = \\(value) -> False.
    }.
    f :: Int -> Bool.
    f = \\(x) -> D::n x.
    result :: Bool.
    result = f 1.
    result.
    """
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
  assertSourceOk """
  x :: [Int].
  x = [1].
  """

testSourceAcceptsNestedConcreteListSignature :: IO ()
testSourceAcceptsNestedConcreteListSignature =
  assertSourceOk """
  x :: [[Bool]].
  x = [[True], [False]].
  """

testSourceAcceptsConcreteTupleSignature :: IO ()
testSourceAcceptsConcreteTupleSignature =
  assertSourceOk """
  pair :: (Int, Bool).
  pair = (1, True).
  pair.
  """

testSourceAcceptsWidthSpecificIntegerSignatures :: IO ()
testSourceAcceptsWidthSpecificIntegerSignatures = do
  assertSourceOk """
  x :: Int8.
  x = 1.
  """
  assertSourceOk """
  x :: Int8.
  x = 127.
  """
  assertSourceOk """
  x :: UInt8.
  x = 255.
  """
  assertSourceOk """
  x :: UInt64.
  x = 1.
  """
  assertSourceOk """
  x :: UInt64.
  x = 18446744073709551615.
  """
  assertSourceOk """
  xs :: [Int32].
  xs = [1, 2, 3].
  """
  assertSourceOkWithoutPrelude """
  class Num(a) { }.
  impl Num(UInt16) { }.
  x :: @{Num(UInt16)}: UInt16.
  x = 1.
  """

testSourceAcceptsSameWidthIntegralOperatorSignatures :: IO ()
testSourceAcceptsSameWidthIntegralOperatorSignatures = do
  assertSourceOk """
  add :: Int8 -> Int8 -> Int8.
  add = (+).
  """
  assertSourceOk """
  lt :: UInt32 -> UInt32 -> Bool.
  lt = (<).
  """

testSourceAcceptsSameWidthFloatNumericOperatorSignatures :: IO ()
testSourceAcceptsSameWidthFloatNumericOperatorSignatures = do
  assertSourceOk """
  fadd :: Float -> Float -> Float.
  fadd = (+).
  """
  assertSourceOk """
  fadd64 :: Float64 -> Float64 -> Float64.
  fadd64 = (+).
  """

testSourceAcceptsFloatFractionalLiteralSignatures :: IO ()
testSourceAcceptsFloatFractionalLiteralSignatures = do
  assertSourceOk """
  x :: Float.
  x = 1.5.
  """
  assertSourceOk """
  x :: Float16.
  x = 1.5.
  """
  assertSourceOk """
  x :: Float32.
  x = 1.5.
  """
  assertSourceOk """
  x :: Float64.
  x = 1.5.
  """
  assertSourceOk """
  xs :: [Float64].
  xs = [1.5, 2.25].
  """

testSourceAcceptsListToListFunctionSignature :: IO ()
testSourceAcceptsListToListFunctionSignature =
  assertSourceOk """
  f :: [Int] -> [Int].
  f = filter (> 1).
  """

testSourceAcceptsParenthesizedFunctionSignature :: IO ()
testSourceAcceptsParenthesizedFunctionSignature =
  assertSourceOk """
  f :: ([Int]) -> ([Int]).
  f = filter (> 1).
  """

testSourceAcceptsChainedFunctionSignature :: IO ()
testSourceAcceptsChainedFunctionSignature =
  assertSourceOk """
  f :: Int -> Int -> Int.
  f = (+).
  """

testSourceAcceptsParenthesizedFunctionOverrideSignature :: IO ()
testSourceAcceptsParenthesizedFunctionOverrideSignature =
  assertSourceOk """
  applyToOne :: (Int -> Int) -> Int.
  applyToOne = \\(f) -> f 1.
  """

testSourceAcceptsFunctionListSignature :: IO ()
testSourceAcceptsFunctionListSignature =
  assertSourceOk """
  fns :: [(Int -> Int)].
  fns = [(+ 1)].
  """

testSourceAcceptsEmptyConstrainedSignature :: IO ()
testSourceAcceptsEmptyConstrainedSignature =
  assertSourceOk """
  applyToOne :: @{}: (Int -> Int) -> Int.
  applyToOne = \\(f) -> f 1.
  """

testSourceAcceptsEmptyConstrainedTupleSignature :: IO ()
testSourceAcceptsEmptyConstrainedTupleSignature =
  assertSourceOk """
  pair :: @{}: (Int, Bool).
  pair = (1, True).
  pair.
  """

testSourceAcceptsConcreteConstrainedSignature :: IO ()
testSourceAcceptsConcreteConstrainedSignature =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  x :: @{Eq(Int)}: Int.
  x = 1.
  """

testSourceAcceptsBundledConcreteConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledConcreteConstrainedSignatureFacts =
  assertSourceOk """
  x :: @{Eq(Int)}: Int.
  x = 1.
  """

testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts = do
  assertSourceOk """
  x :: @{Num(UInt16)}: UInt16.
  x = 1.
  """
  assertSourceOk """
  x :: @{Integral(Int32)}: Int32.
  x = 1.
  """
  assertSourceOk """
  x :: @{Fractional(Float32)}: Float32.
  x = toFloat32 1.
  """
  assertSourceOk """
  x :: @{Showable(Float64)}: Float64.
  x = toFloat64 1.
  """

testSourceAcceptsAdditionalConcreteConstrainedSignatures :: IO ()
testSourceAcceptsAdditionalConcreteConstrainedSignatures = do
  assertSourceOkWithoutPrelude """
  class Default(a) { }.
  impl Default(Bool) { }.
  x :: @{Default(Bool)}: Bool.
  x = True.
  """
  assertSourceOkWithoutPrelude """
  class Fractional(a) { }.
  impl Fractional(Int) { }.
  x :: @{Fractional(Int)}: Int.
  x = 1.
  """
  assertSourceOkWithoutPrelude """
  class Integral(a) { }.
  impl Integral(Int) { }.
  x :: @{Integral(Int)}: Int.
  x = 1.
  """
  assertSourceOkWithoutPrelude """
  class Num(a) { }.
  impl Num(Int) { }.
  x :: @{Num(Int)}: Int.
  x = 1.
  """
  assertSourceOkWithoutPrelude """
  class Ord(a) { }.
  impl Ord(Int) { }.
  x :: @{Ord(Int)}: Int.
  x = 1.
  """
  assertSourceOkWithoutPrelude """
  class Showable(a) { }.
  impl Showable([[Bool]]) { }.
  x :: @{Showable([[Bool]])}: [[Bool]].
  x = [[True], [False]].
  """

testSourceAcceptsConcreteTupleConstrainedSignatureArgument :: IO ()
testSourceAcceptsConcreteTupleConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq((Int, Bool)) { }.
  pair :: @{Eq((Int, Bool))}: (Int, Bool).
  pair = (1, True).
  """

testSourceAcceptsAdtApplicationConstrainedSignatureArgument :: IO ()
testSourceAcceptsAdtApplicationConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude """
  data Box a = Box a.
  class Eq(a) { }.
  impl Eq(Box(Int)) { }.
  x :: @{Eq(Box(Int))}: Int.
  x = 1.
  """

testSourceAcceptsVariableConstrainedSignatureAsMonomorphic :: IO ()
testSourceAcceptsVariableConstrainedSignatureAsMonomorphic =
  assertSourceOk """
  id :: @{Eq(a)}: a -> a.
  id = \\(x) -> x.
  id 1.
  """

testSourceHonorsVisibleFactsForVariableConstrainedSignatures :: IO ()
testSourceHonorsVisibleFactsForVariableConstrainedSignatures =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Bool) { }.
  id :: @{Eq(a)}: a -> a.
  id = \\(x) -> x.
  x = id 1.
  y = id True.
  """

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
