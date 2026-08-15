module Jazz.Compiler.Bootstrap.TypedCoreContract.Tests
  ( tests,
  )
where

import qualified Jazz.Compiler.Bootstrap.TypedCoreContract.ManifestTests as ManifestTests
import qualified Jazz.Compiler.Bootstrap.TypedCoreContract.ParityTests as ParityTests
import qualified Jazz.Compiler.Bootstrap.TypedCoreContract.RegressionTests as RegressionTests
import Jazz.TestHarness (NamedTest)

tests :: [NamedTest]
tests = ManifestTests.tests <> RegressionTests.tests <> ParityTests.tests
