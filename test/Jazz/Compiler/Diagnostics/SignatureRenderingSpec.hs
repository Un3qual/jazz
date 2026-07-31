{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.AST
  ( NumericType (..),
    SignatureType (..)
  )
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "SignatureRendering" tests

tests :: [NamedTest]
tests =
  [ ("renders primitive and numeric types", testPrimitiveAndNumericTypes),
    ("renders named applications and nested containers", testNamedApplicationsAndContainers),
    ("renders function types right-associatively with required parentheses", testFunctionAssociativity)
  ]

testPrimitiveAndNumericTypes :: IO ()
testPrimitiveAndNumericTypes = do
  assertEqual "Int" "Int" (renderSignatureType TypeInt)
  assertEqual "Float" "Float" (renderSignatureType TypeFloat)
  assertEqual "UInt8" "UInt8" (renderSignatureType (TypeNumeric NumericUInt8))
  assertEqual "Bool" "Bool" (renderSignatureType TypeBool)
  assertEqual "Char" "Char" (renderSignatureType TypeChar)
  assertEqual "Text" "Text" (renderSignatureType TypeText)
  assertEqual "named type" "Point" (renderSignatureType (TypeName "Point"))
  assertEqual "type variable" "value" (renderSignatureType (TypeVariable "value"))

testNamedApplicationsAndContainers :: IO ()
testNamedApplicationsAndContainers = do
  assertEqual
    "named application"
    "Result([Int], (Bool, Text))"
    (renderSignatureType (TypeApplication "Result" [TypeList TypeInt, TypeTuple [TypeBool, TypeText]]))
  assertEqual
    "list containing a function"
    "[(Int -> Bool)]"
    (renderSignatureType (TypeList (TypeFunction TypeInt TypeBool)))
  assertEqual
    "nested tuple"
    "(Int, (Bool, Text))"
    (renderSignatureType (TypeTuple [TypeInt, TypeTuple [TypeBool, TypeText]]))

testFunctionAssociativity :: IO ()
testFunctionAssociativity = do
  assertEqual
    "right-associated result"
    "Int -> Bool -> Text"
    (renderSignatureType (TypeFunction TypeInt (TypeFunction TypeBool TypeText)))
  assertEqual
    "function argument"
    "(Int -> Bool) -> Text"
    (renderSignatureType (TypeFunction (TypeFunction TypeInt TypeBool) TypeText))
