{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Stdlib.FoundationsTests
  ( foundationTests,
  )
import JazzNext.Compiler.Stdlib.LinearCollectionsTests
  ( linearCollectionTests,
  )
import JazzNext.Compiler.Stdlib.OrderedCollectionsTests
  ( orderedCollectionTests,
  )
import JazzNext.Compiler.Stdlib.TextTests
  ( textTests,
  )
import JazzNext.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main = runTestSuite "Stdlib" (foundationTests <> linearCollectionTests <> orderedCollectionTests <> textTests)
