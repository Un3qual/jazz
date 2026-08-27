{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Jazz.Compiler.Runtime.Observation.ProfileTests as ProfileTests
import qualified Jazz.Compiler.Runtime.Observation.StatisticsTests as StatisticsTests
import qualified Jazz.Compiler.Runtime.OutcomeTests as OutcomeTests
import Jazz.TestHarness (runTestSuite)

main :: IO ()
main =
  runTestSuite
    "RuntimeObservation"
    (OutcomeTests.tests <> StatisticsTests.tests <> ProfileTests.tests)
