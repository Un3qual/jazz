{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, unless, when)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy
  )
import JazzNext.Repository.Root (findJazzNextPackageRoot)
import JazzNext.Repository.StdlibFormat
  ( StdlibFormatViolation (..),
    renderStdlibFormatViolation,
    validateStdlibModule
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite
  )
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

main :: IO ()
main = runTestSuite "RepositoryAudit" tests

tests :: [NamedTest]
tests =
  [ ("accepts a valid stdlib module", testValidStdlibModule),
    ("rejects a missing module header", testMissingModuleHeader),
    ("rejects a missing final closing brace", testMissingClosingBrace),
    ("rejects blank lines after the final closing brace", testTrailingBlankLines),
    ("rejects odd or shallow body indentation", testBodyIndentation),
    ("exempts the bundled Prelude source", testPreludeExemption),
    ("accepts only the named private Cabal library", testPrivatePackagePolicy),
    ("rejects an unnamed public Cabal library", testPublicLibraryPolicy),
    ("rejects a private library without private visibility", testMissingPrivateVisibility),
    ("locates the active jazz-next package root", testPackageRoot),
    ("validates all checked-in stdlib modules", testCheckedInStdlib),
    ("validates the checked-in Cabal package policy", testCheckedInPackagePolicy)
  ]

validStdlibSource :: Text
validStdlibSource =
  """
  module Good {
    value = 1.
  }
  """

validPrivatePackage :: Text
validPrivatePackage =
  """
  library jazz-next-internal
    visibility: private
  """

testValidStdlibModule :: IO ()
testValidStdlibModule =
  assertEqual "valid stdlib violations" [] (validateStdlibModule "stdlib/Good.jz" validStdlibSource)

testMissingModuleHeader :: IO ()
testMissingModuleHeader =
  assertEqual
    "missing module header"
    [InvalidModuleHeader "stdlib/Bad.jz"]
    ( validateStdlibModule
        "stdlib/Bad.jz"
        """
        value = 1.
        }
        """
    )

testMissingClosingBrace :: IO ()
testMissingClosingBrace =
  assertEqual
    "missing final closing brace"
    [MissingFinalClosingBrace "stdlib/Bad.jz"]
    ( validateStdlibModule
        "stdlib/Bad.jz"
        """
        module Bad {
          value = 1.
        """
    )

testTrailingBlankLines :: IO ()
testTrailingBlankLines =
  -- Explicit escapes are intentional: this case directly tests trailing whitespace.
  assertEqual
    "trailing blank lines"
    [MissingFinalClosingBrace "stdlib/Bad.jz"]
    (validateStdlibModule "stdlib/Bad.jz" "module Bad {\n  value = 1.\n}\n\n")

testBodyIndentation :: IO ()
testBodyIndentation =
  assertEqual
    "invalid body indentation"
    [ InvalidBodyIndentation "stdlib/Bad.jz" 2,
      InvalidBodyIndentation "stdlib/Bad.jz" 3
    ]
    ( validateStdlibModule
        "stdlib/Bad.jz"
        """
        module Bad {
         shallow = 1.
           odd = 2.
        }
        """
    )

testPreludeExemption :: IO ()
testPreludeExemption =
  assertEqual
    "Prelude exemption"
    []
    (validateStdlibModule "stdlib/Prelude.jz" "class Eq(a) { }.")

testPrivatePackagePolicy :: IO ()
testPrivatePackagePolicy =
  assertEqual "valid private library policy" [] (validatePackagePolicy validPrivatePackage)

testPublicLibraryPolicy :: IO ()
testPublicLibraryPolicy =
  assertEqual
    "public library policy"
    [PublicLibraryStanza, MissingPrivateLibraryStanza]
    ( validatePackagePolicy
        """
        library
          exposed-modules: Public
        """
    )

testMissingPrivateVisibility :: IO ()
testMissingPrivateVisibility =
  assertEqual
    "missing private visibility"
    [MissingPrivateLibraryVisibility]
    ( validatePackagePolicy
        """
        library jazz-next-internal
          exposed-modules: Internal
        """
    )

testPackageRoot :: IO ()
testPackageRoot =
  withPackageRoot $ \packageRoot -> do
    packageSource <- TextIO.readFile (packageRoot </> "jazz-next.cabal")
    unless ("name: jazz-next" `Text.isInfixOf` packageSource) $ do
      failTest "located package root does not contain the jazz-next package"

testCheckedInStdlib :: IO ()
testCheckedInStdlib =
  withPackageRoot $ \packageRoot -> do
    let stdlibDirectory = packageRoot </> "stdlib"
    stdlibExists <- doesDirectoryExist stdlibDirectory
    unless stdlibExists (failTest "stdlib audit could not find the stdlib directory")
    stdlibEntries <- sort <$> listDirectory stdlibDirectory
    let jazzFiles = filter ((== ".jz") . takeExtension) stdlibEntries
    when (null jazzFiles) (failTest "stdlib audit found no .jz files")
    sources <- forM jazzFiles $ \entry -> do
      source <- TextIO.readFile (stdlibDirectory </> entry)
      pure ("stdlib" </> entry, source)
    let violations = concatMap (uncurry validateStdlibModule) sources
    unless (null violations) $ do
      failTest (Text.intercalate "\n" (map renderStdlibFormatViolation violations))

testCheckedInPackagePolicy :: IO ()
testCheckedInPackagePolicy =
  withPackageRoot $ \packageRoot -> do
    packageSource <- TextIO.readFile (packageRoot </> "jazz-next.cabal")
    let violations = validatePackagePolicy packageSource
    unless (null violations) $ do
      failTest (Text.intercalate "\n" (map renderPackagePolicyViolation violations))

withPackageRoot :: (FilePath -> IO ()) -> IO ()
withPackageRoot action = do
  rootResult <- findJazzNextPackageRoot
  case rootResult of
    Left message -> failTest message
    Right packageRoot -> action packageRoot
