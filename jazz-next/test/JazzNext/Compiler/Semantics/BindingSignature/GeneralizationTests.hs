{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.GeneralizationTests
  ( generalizationTests
  ) where

import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    compileErrors,
    compileSourceWithPrelude,
    runCompileErrors,
    runRuntimeErrors,
    runSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual
  )
import JazzNext.Compiler.Semantics.BindingSignature.Shared

generalizationTests :: [NamedTest]
generalizationTests =
  [ ("source pipeline instantiates ordinary binding schemes per use", testSourceInstantiatesOrdinaryBindingSchemesPerUse)
    , ("source pipeline instantiates signed generic schemes per use", testSourceInstantiatesSignedGenericSchemesPerUse)
    , ("source pipeline instantiates signed generic ADT schemes per use", testSourceInstantiatesSignedGenericAdtSchemesPerUse)
    , ("source pipeline instantiates signed generic constructor aliases per use", testSourceInstantiatesSignedGenericConstructorAliasesPerUse)
    , ("source pipeline instantiates variable constrained signatures per use", testSourceInstantiatesVariableConstrainedSignaturePerUse)
    , ("source pipeline instantiates primitive constrained signatures per use", testSourceInstantiatesPrimitiveConstrainedSignaturePerUse)
    , ("source pipeline applies explicit type application to generalized signatures", testSourceAppliesExplicitTypeApplicationToGeneralizedSignature)
    , ("source pipeline applies explicit named type applications", testSourceAppliesExplicitNamedTypeApplication)
    , ("source pipeline applies nested explicit named type applications", testSourceAppliesNestedExplicitNamedTypeApplication)
    , ("source pipeline applies explicit type application to first source variable", testSourceAppliesExplicitTypeApplicationToFirstSourceVariable)
    , ("source pipeline applies explicit type application to inferred type order", testSourceAppliesExplicitTypeApplicationToInferredTypeOrder)
    , ("source pipeline rejects primitive-incompatible explicit type application", testSourceRejectsPrimitiveIncompatibleExplicitTypeApplication)
    , ("source pipeline rejects explicit type application on monomorphic bindings", testSourceRejectsExplicitTypeApplicationOnMonomorphicBinding)
    , ("source pipeline rejects extra explicit type application arguments", testSourceRejectsExtraExplicitTypeApplicationArgument)
    , ("source pipeline does not shift inference variables after rejected variable type application", testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState)
  ]

testSourceInstantiatesOrdinaryBindingSchemesPerUse :: IO ()
testSourceInstantiatesOrdinaryBindingSchemesPerUse =
  assertSourceOk """
  id = \\(x) -> x.
  intValue = id 1.
  boolValue = id True.
  """

testSourceInstantiatesSignedGenericSchemesPerUse :: IO ()
testSourceInstantiatesSignedGenericSchemesPerUse =
  assertSourceOkWithoutPrelude
    """
    id :: a -> a.
    id = \\(x) -> x.
    intValue = id 1.
    boolValue = id True.
    """

testSourceInstantiatesSignedGenericAdtSchemesPerUse :: IO ()
testSourceInstantiatesSignedGenericAdtSchemesPerUse =
  assertSourceOkWithoutPrelude
    ( """
    data Box a = Box a.
    keep :: Box(a) -> Box(a).
    keep = \\(value) -> value.
    intBox = keep (Box 1).
    boolBox = keep (Box True).
    """
    )

testSourceInstantiatesSignedGenericConstructorAliasesPerUse :: IO ()
testSourceInstantiatesSignedGenericConstructorAliasesPerUse =
  assertSourceOkWithoutPrelude
    ( """
    data Box a = Box a.
    make :: a -> Box(a).
    make = Box.
    first = make 1.
    second = make True.
    """
    )

testSourceInstantiatesVariableConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesVariableConstrainedSignaturePerUse =
  assertSourceOk """
  id :: @{Eq(a)}: a -> a.
  id = \\(x) -> x.
  x = id 1.
  y = id True.
  """

testSourceInstantiatesPrimitiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesPrimitiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude """
  class Num(a) { }.
  impl Num(Int32) { }.
  impl Num(Int64) { }.
  add :: @{Num(a)}: a -> a -> a.
  add = \\(x, y) -> x + y.
  a32 :: Int32.
  a32 = 1.
  b32 :: Int32.
  b32 = 2.
  small = add a32 b32.
  a64 :: Int64.
  a64 = 3.
  b64 :: Int64.
  b64 = 4.
  wide = add a64 b64.
  """

testSourceAppliesExplicitTypeApplicationToGeneralizedSignature :: IO ()
testSourceAppliesExplicitTypeApplicationToGeneralizedSignature =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  id :: @{Eq(a)}: a -> a.
  id = \\(x) -> x.
  value = id @Int 1.
  value.
  """

testSourceAppliesExplicitNamedTypeApplication :: IO ()
testSourceAppliesExplicitNamedTypeApplication =
  assertSourceOkWithoutPrelude
    """
    data Box a = Box a.
    identity :: a -> a.
    identity = \\(value) -> value.
    value = identity @Box(Char) (Box 'x').
    """

testSourceAppliesNestedExplicitNamedTypeApplication :: IO ()
testSourceAppliesNestedExplicitNamedTypeApplication =
  assertSourceOkWithoutPrelude
    ( """
    data IOError = IOError.
    data Result a b = Ok a | Err b.
    identity :: a -> a.
    identity = \\(value) -> value.
    value = identity @Result(IOError, Text) (Err \"bad\").
    """
    )

testSourceAppliesExplicitTypeApplicationToFirstSourceVariable :: IO ()
testSourceAppliesExplicitTypeApplicationToFirstSourceVariable =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  choose :: @{Eq(b)}: b -> a -> b.
  choose = \\(x, y) -> x.
  value = choose @Int 1 True.
  value.
  """

testSourceAppliesExplicitTypeApplicationToInferredTypeOrder :: IO ()
testSourceAppliesExplicitTypeApplicationToInferredTypeOrder = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( """
      flip = \\(f, x, y) -> f y x.
      value = flip @Int (\\(left, right) -> left + 1) True 2.
      value.
      """
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testSourceRejectsPrimitiveIncompatibleExplicitTypeApplication :: IO ()
testSourceRejectsPrimitiveIncompatibleExplicitTypeApplication =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Num(a) { }.
    impl Num(Bool) { }.
    addSelf :: @{Num(a)}: a -> a.
    addSelf = \\(x) -> x + x.
    bad = addSelf @Bool True.
    """
    "primitive numeric constraint"

testSourceRejectsExplicitTypeApplicationOnMonomorphicBinding :: IO ()
testSourceRejectsExplicitTypeApplicationOnMonomorphicBinding =
  assertSourceSingleErrorContains
    """
    inc :: Int -> Int.
    inc = \\(x) -> x + 1.
    value = inc @Int 1.
    """
    "explicit type application target must be a generalized binding"

testSourceRejectsExtraExplicitTypeApplicationArgument :: IO ()
testSourceRejectsExtraExplicitTypeApplicationArgument =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    id :: @{Eq(a)}: a -> a.
    id = \\(x) -> x.
    value = id @Int @Bool 1.
    """
    "explicit type application target must be a generalized binding"

testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState :: IO ()
testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing """
  bad :: @{Eq(f), Ord(a)}: f(a) -> a.
  bad = \\(x) -> x.
  use = [] 1.
  """
  assertContains
    "later diagnostic keeps deterministic type variable id"
    "cannot apply function of type [t3] to argument of type Int"
    (Text.unlines (map renderDiagnostic (compileErrors result)))
