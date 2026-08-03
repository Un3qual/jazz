{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Semantics.PrimitiveSemantics.EqualityOperator
  ( basicEqualityTests,
    operatorTests,
    primitiveMismatchTests,
    structuralEqualityTests
  )
import Jazz.Compiler.Semantics.PrimitiveSemantics.NumericConversions
  ( integerWidthTests,
    numericConversionTests
  )
import Jazz.Compiler.Semantics.PrimitiveSemantics.ScalarCollection
  ( arithmeticMismatchTests,
    arithmeticPrimitiveTests,
    collectionTests,
    mixedCollectionTests,
    scalarPrimitiveTests
  )
import Jazz.TestHarness
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
