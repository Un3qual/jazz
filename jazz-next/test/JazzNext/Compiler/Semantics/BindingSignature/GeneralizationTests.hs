{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.GeneralizationTests
  ( generalizationTests
  ) where

import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileSourceWithPrelude,
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
  assertSourceOk "id = \\(x) -> x.\nintValue = id 1.\nboolValue = id True."

testSourceInstantiatesSignedGenericSchemesPerUse :: IO ()
testSourceInstantiatesSignedGenericSchemesPerUse =
  assertSourceOkWithoutPrelude
    "id :: a -> a.\nid = \\(x) -> x.\nintValue = id 1.\nboolValue = id True."

testSourceInstantiatesSignedGenericAdtSchemesPerUse :: IO ()
testSourceInstantiatesSignedGenericAdtSchemesPerUse =
  assertSourceOkWithoutPrelude
    ( "data Box a = Box a.\n"
        <> "keep :: Box(a) -> Box(a).\n"
        <> "keep = \\(value) -> value.\n"
        <> "intBox = keep (Box 1).\n"
        <> "boolBox = keep (Box True)."
    )

testSourceInstantiatesSignedGenericConstructorAliasesPerUse :: IO ()
testSourceInstantiatesSignedGenericConstructorAliasesPerUse =
  assertSourceOkWithoutPrelude
    ( "data Box a = Box a.\n"
        <> "make :: a -> Box(a).\n"
        <> "make = Box.\n"
        <> "first = make 1.\n"
        <> "second = make True."
    )

testSourceInstantiatesVariableConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesVariableConstrainedSignaturePerUse =
  assertSourceOk "id :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nx = id 1.\ny = id True."

testSourceInstantiatesPrimitiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesPrimitiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(Int32) { }.\nimpl Num(Int64) { }.\nadd :: @{Num(a)}: a -> a -> a.\nadd = \\(x) -> \\(y) -> x + y.\na32 :: Int32.\na32 = 1.\nb32 :: Int32.\nb32 = 2.\nsmall = add a32 b32.\na64 :: Int64.\na64 = 3.\nb64 :: Int64.\nb64 = 4.\nwide = add a64 b64."

testSourceAppliesExplicitTypeApplicationToGeneralizedSignature :: IO ()
testSourceAppliesExplicitTypeApplicationToGeneralizedSignature =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nvalue = id @Int 1.\nvalue."

testSourceAppliesExplicitNamedTypeApplication :: IO ()
testSourceAppliesExplicitNamedTypeApplication =
  assertSourceOkWithoutPrelude
    "data Box a = Box a.\nidentity :: a -> a.\nidentity = \\(value) -> value.\nvalue = identity @Box(Char) (Box 'x')."

testSourceAppliesNestedExplicitNamedTypeApplication :: IO ()
testSourceAppliesNestedExplicitNamedTypeApplication =
  assertSourceOkWithoutPrelude
    ( "data IOError = IOError.\n"
        <> "data Result a b = Ok a | Err b.\n"
        <> "identity :: a -> a.\n"
        <> "identity = \\(value) -> value.\n"
        <> "value = identity @Result(IOError, Text) (Err \"bad\")."
    )

testSourceAppliesExplicitTypeApplicationToFirstSourceVariable :: IO ()
testSourceAppliesExplicitTypeApplicationToFirstSourceVariable =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nchoose :: @{Eq(b)}: b -> a -> b.\nchoose = \\(x) -> \\(y) -> x.\nvalue = choose @Int 1 True.\nvalue."

testSourceAppliesExplicitTypeApplicationToInferredTypeOrder :: IO ()
testSourceAppliesExplicitTypeApplicationToInferredTypeOrder = do
  result <-
    runSourceWithPrelude
      defaultWarningSettings
      Nothing
      ( "flip = \\(f) -> \\(x) -> \\(y) -> f y x.\n"
          <> "value = flip @Int (\\(left) -> \\(right) -> left + 1) True 2.\n"
          <> "value."
      )
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "3") (runOutput result)

testSourceRejectsPrimitiveIncompatibleExplicitTypeApplication :: IO ()
testSourceRejectsPrimitiveIncompatibleExplicitTypeApplication =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Num(a) { }.\nimpl Num(Bool) { }.\naddSelf :: @{Num(a)}: a -> a.\naddSelf = \\(x) -> x + x.\nbad = addSelf @Bool True."
    "primitive numeric constraint"

testSourceRejectsExplicitTypeApplicationOnMonomorphicBinding :: IO ()
testSourceRejectsExplicitTypeApplicationOnMonomorphicBinding =
  assertSourceSingleErrorContains
    "inc :: Int -> Int.\ninc = \\(x) -> x + 1.\nvalue = inc @Int 1."
    "explicit type application target must be a generalized binding"

testSourceRejectsExtraExplicitTypeApplicationArgument :: IO ()
testSourceRejectsExtraExplicitTypeApplicationArgument =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nvalue = id @Int @Bool 1."
    "explicit type application target must be a generalized binding"

testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState :: IO ()
testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "bad :: @{Eq(f), Ord(a)}: f(a) -> a.\nbad = \\(x) -> x.\nuse = [] 1."
  assertContains
    "later diagnostic keeps deterministic type variable id"
    "cannot apply function of type [t3] to argument of type Int"
    (Text.unlines (map renderDiagnostic (compileErrors result)))
