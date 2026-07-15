{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified JazzNext.Compiler.Runtime.Observation.ProfileTests as ProfileTests
import qualified JazzNext.Compiler.Runtime.Observation.StatisticsTests as StatisticsTests
import JazzNext.TestHarness (runTestSuite)

main :: IO ()
main = runTestSuite "RuntimeObservation" (StatisticsTests.tests <> ProfileTests.tests)
