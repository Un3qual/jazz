{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Jazz.Compiler.Runtime.Observation.ProfileTests as ProfileTests
import qualified Jazz.Compiler.Runtime.Observation.StatisticsTests as StatisticsTests
import Jazz.TestHarness (runTestSuite)

main :: IO ()
main = runTestSuite "RuntimeObservation" (StatisticsTests.tests <> ProfileTests.tests)
