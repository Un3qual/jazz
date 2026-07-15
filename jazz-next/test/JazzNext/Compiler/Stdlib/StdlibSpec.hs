{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Stdlib.FoundationsTests
  ( foundationTests,
  )
import JazzNext.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main = runTestSuite "Stdlib" foundationTests
