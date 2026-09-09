{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Jazz.Compiler.AST (CorePhase (Lowered))
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignatureType (..),
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "SignatureRendering" tests

tests :: [NamedTest]
tests =
  [ ("renders primitive and numeric types", testPrimitiveAndNumericTypes),
    ("renders named applications and nested containers", testNamedApplicationsAndContainers),
    ("renders function types right-associatively with required parentheses", testFunctionAssociativity)
  ]

renderLoweredSignatureType :: AST.SignatureType 'Lowered -> Text
renderLoweredSignatureType = renderSignatureType

testPrimitiveAndNumericTypes :: IO ()
testPrimitiveAndNumericTypes = do
  assertEqual "Int" "Int" (renderLoweredSignatureType TypeInt)
  assertEqual "Float" "Float" (renderLoweredSignatureType TypeFloat)
  assertEqual "UInt8" "UInt8" (renderLoweredSignatureType (TypeNumeric NumericUInt8))
  assertEqual "Bool" "Bool" (renderLoweredSignatureType TypeBool)
  assertEqual "Char" "Char" (renderLoweredSignatureType TypeChar)
  assertEqual "Text" "Text" (renderLoweredSignatureType TypeText)
  assertEqual "named type" "Point" (renderLoweredSignatureType (TypeName "Point"))
  assertEqual "type variable" "value" (renderLoweredSignatureType (TypeVariable "value"))

testNamedApplicationsAndContainers :: IO ()
testNamedApplicationsAndContainers = do
  assertEqual
    "named application"
    "Result([Int], (Bool, Text))"
    (renderLoweredSignatureType (TypeApplication "Result" [TypeList TypeInt, TypeTuple [TypeBool, TypeText]]))
  assertEqual
    "list containing a function"
    "[(Int -> Bool)]"
    (renderLoweredSignatureType (TypeList (TypeFunction TypeInt TypeBool)))
  assertEqual
    "nested tuple"
    "(Int, (Bool, Text))"
    (renderLoweredSignatureType (TypeTuple [TypeInt, TypeTuple [TypeBool, TypeText]]))

testFunctionAssociativity :: IO ()
testFunctionAssociativity = do
  assertEqual
    "right-associated result"
    "Int -> Bool -> Text"
    (renderLoweredSignatureType (TypeFunction TypeInt (TypeFunction TypeBool TypeText)))
  assertEqual
    "function argument"
    "(Int -> Bool) -> Text"
    (renderLoweredSignatureType (TypeFunction (TypeFunction TypeInt TypeBool) TypeText))
