{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Bootstrap.TypedCoreContract.Tests (tests)
import Jazz.TestHarness (runTestSuite)

main :: IO ()
main = runTestSuite "JazzTypedCoreContract" tests
