{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.Stdlib.FoundationsTests
  ( foundationTests,
  )
import Jazz.Compiler.Stdlib.LinearCollectionsTests
  ( linearCollectionTests,
  )
import Jazz.Compiler.Stdlib.OrderedCollectionsTests
  ( orderedCollectionTests,
  )
import Jazz.Compiler.Stdlib.TextTests
  ( textTests,
  )
import Jazz.TestHarness
  ( runTestSuite,
  )

main :: IO ()
main = runTestSuite "Stdlib" (foundationTests <> linearCollectionTests <> orderedCollectionTests <> textTests)
