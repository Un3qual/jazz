{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Semantics.PrimitiveSemantics.EqualityOperator
  ( basicEqualityTests,
    operatorTests,
    primitiveMismatchTests,
    structuralEqualityTests
  )
import JazzNext.Compiler.Semantics.PrimitiveSemantics.NumericConversions
  ( integerWidthTests,
    numericConversionTests
  )
import JazzNext.Compiler.Semantics.PrimitiveSemantics.ScalarCollection
  ( arithmeticMismatchTests,
    arithmeticPrimitiveTests,
    collectionTests,
    mixedCollectionTests,
    scalarPrimitiveTests
  )
import JazzNext.TestHarness
  ( NamedTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "PrimitiveSemantics" tests

tests :: [NamedTest]
tests =
  concat
    [ arithmeticPrimitiveTests,
      basicEqualityTests,
      scalarPrimitiveTests,
      primitiveMismatchTests,
      arithmeticMismatchTests,
      collectionTests,
      structuralEqualityTests,
      integerWidthTests,
      operatorTests,
      mixedCollectionTests,
      numericConversionTests
    ]
