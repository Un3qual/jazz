{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Stdlib.FoundationsTests
  ( foundationPerformanceTests,
    foundationTests,
  )
import Jazz.Compiler.Stdlib.LinearCollectionsTests
  ( linearCollectionScaleTests,
    linearCollectionTests,
  )
import Jazz.Compiler.Stdlib.OrderedCollectionsTests
  ( orderedCollectionTests,
  )
import Jazz.Compiler.Stdlib.TextTests
  ( textScaleTests,
    textTests,
  )
import Jazz.TestHarness
  ( runTestSuite,
  )
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let performanceTests =
        if "--skip-performance" `elem` args
          then []
          else foundationPerformanceTests <> linearCollectionScaleTests <> textScaleTests
  runTestSuite "Stdlib" (foundationTests <> linearCollectionTests <> orderedCollectionTests <> textTests <> performanceTests)
